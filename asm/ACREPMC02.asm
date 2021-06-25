*          DATA SET ACREPMC02  AT LEVEL 003 AS OF 05/01/02                      
*PHASE AC0702A                                                                  
*INCLUDE GETLOGO                                                                
         TITLE 'ACREP0702 - AUTH/RECON/MANUAL CHEQUE LISTING'                   
AC0702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AUTH**,RA                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING AC07D,RC            RC=A(LOCAL W/S)                              
         CLI   MODE,PROCRCVR                                                    
         BE    CHQ10                                                            
         CLI   MODE,RUNLAST                                                     
         BE    CHQ30                                                            
         CLI   MODE,RUNFRST                                                     
         BE    CHQ00                                                            
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* RUN FIRST ROUTINE                                                             
*                                                                               
CHQ00    GOTO1 ADDICTAT,DMCB,C'LU  ',DICI,DICO                                  
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         MVI   CLEARHED,C'N'                                                    
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVI   BOXCOLS+00,C'L'                                                  
         MVI   BOXCOLS+16,C'C'                                                  
         MVI   BOXCOLS+31,C'C'                                                  
         MVI   BOXCOLS+41,C'C'                                                  
         MVI   BOXCOLS+48,C'C'                                                  
         MVI   BOXCOLS+58,C'C'                                                  
         MVI   BOXCOLS+73,C'R'                                                  
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+08,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         DROP  RF                                                               
         B     XIT                                                              
         EJECT                                                                  
* PROCESS A RECOVERY FILE RECORD                                                
*                                                                               
CHQ10    L     R2,ADTRANS                                                       
         USING RCVRECD,R2          R2=A(RECOVERY RECORD HEADER)                 
         LA    R3,RCVRECRD                                                      
         USING ACKEYD,R3           R3=A(RECOVERY RECORD)                        
         LA    R4,ACRECORD         R4=A(ACCOUNT RECORD)                         
         USING TRANSD,R4                                                        
*                                                                               
         CLI   RCVRECTY,RCVRCPYQ   ONLY WANT COPIES AND CHANGES                 
         BE    *+12                                                             
         CLI   RCVRECTY,RCVRCHAQ                                                
         BNE   XIT                                                              
*                                                                               
         CLI   TRNSEL,TRNSELQ      ONLY WANT TRANSACTIONS                       
         BNE   XIT                                                              
*                                                                               
         CLI   RCVPRGNO,X'13'      TEST INV/AUTH OR MANUAL CHEQUE               
         BNE   *+16                                                             
         TM    TRNSSTAT,X'80'      WANT CREDITS ONLY                            
         BZ    CHQ12                                                            
         B     XIT                                                              
*                                                                               
         CLI   RCVPRGNO,X'0D'      TEST CRD/RECON                               
         BNE   XIT                                                              
         CLC   ACKEYACC+1(2),=C'SC'                                             
         BNE   XIT                                                              
*                                                                               
CHQ12    CLI   RCVRECTY,RCVRCPYQ   SAVE COPY RECORD FOR NEXT MODE               
         BNE   CHQ14                                                            
         LA    R0,COPYREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,RCVLEN                                                      
         LA    RE,RCVRECD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
*                                                                               
CHQ14    LA    R5,COPYREC+(RCVRECRD-RCVRECD)                                    
         CLC   ACKEYD(ACLENGTH-ACKEYD),0(R5)                                    
         BNE   XIT                 COPY/CHANGE KEYS DON'T MATCH                 
         LA    R5,ACRECORD-ACKEYD(R5)                                           
*                                                                               
         CLI   RCVPRGNO,X'0D'      TEST CRD/RECON                               
         BNE   CHQ16                                                            
         TM    TRNSSTAT,X'02'      TEST RECONCILED BIT ON IN CHANGE             
         BZ    XIT                                                              
         TM    TRNSSTAT-TRANSD(R5),X'02'                                        
         BNZ   XIT                                                              
         MVI   TYPE,RECON                                                       
         BAS   RE,PUTSORT          THIS ITEM RECONCILED TODAY                   
         B     XIT                                                              
*                                                                               
CHQ16    TM    TRNSSTAT,X'08'      TEST CHANGE RECORD IS AUTHORISED             
         BZ    CHQ18                                                            
         TM    TRNSSTAT-TRANSD(R5),X'08'                                        
         BNZ   CHQ18                                                            
         MVI   TYPE,AUTHD                                                       
         BAS   RE,PUTSORT          THIS ITEM AUTHORISED TODAY                   
*                                                                               
CHQ18    XC    ACPYEL,ACPYEL                                                    
         XC    ACHAEL,ACHAEL                                                    
         SR    R0,R0                                                            
         LA    RF,TRANSD           LOCATE CHEQUE ELEMENT IN CHANGE REC          
         USING TRMAPD,RF                                                        
CHQ20    CLI   TRMAPEL,0                                                        
         BE    CHQ22                                                            
         CLI   TRMAPEL,TRMAPELQ                                                 
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     CHQ20                                                            
         ST    RF,ACHAEL                                                        
*                                                                               
CHQ22    LR    RF,R5               LOCATE CHEQUE ELEMENT IN COPY REC            
CHQ24    CLI   TRMAPEL,0                                                        
         BE    CHQ26                                                            
         CLI   TRMAPEL,TRMAPELQ                                                 
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     CHQ24                                                            
         ST    RF,ACPYEL                                                        
*                                                                               
CHQ26    LM    RE,RF,ACPYEL                                                     
         CR    RE,RF               TEST ANY MANUAL CHEQUE ELEMENTS              
         BE    XIT                                                              
         MVI   TYPE,MANCK                                                       
         CLC   TRMAPNO-TRMAPD(L'TRMAPNO,RE),TRMAPNO-TRMAPD(RF)                  
         BE    XIT                                                              
         OC    ACDTUSED,ACDTUSED   IF USED DATE SET MEANS CHEQUE                
         BNZ   *+10                                                             
         MVC   TRMAPEL-TRMAPD(TRMAPLNQ,RF),0(RE)                                
         MVC   ACKEYCON,SPACES                                                  
         MVC   ACKEYCON(L'TRMAPNO),TRMAPNO                                      
         MVI   ACKEYCON+L'TRMAPNO,C' '                                          
         OC    ACDTUSED,ACDTUSED   IF USED DATE NOT SET MEANS UNCHEQUE          
         BNZ   *+8                                                              
         MVI   ACKEYCON+L'TRMAPNO,C'*'                                          
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         EJECT                                                                  
* RUNLAST GET RECORDS AND PRINT REPORT                                          
*                                                                               
CHQ30    L     R9,LOGOC                                                         
         USING LOGOD,R9            R9=A(LOGOD)                                  
         CLI   SORTSW,0                                                         
         BE    XIT                                                              
         MVI   TYPE,0                                                           
         MVI   COMP,0                                                           
         LA    R2,COPYREC          R2=A(RECOVERY HEADER)                        
         LA    R3,RCVRECRD         R3=A(ACCOUNT KEY)                            
         LA    R4,ACRECORD         R4=A(FIRST ELEMENT)                          
*                                                                               
CHQ32    GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RE,15,4(R1)                                                      
         BNZ   CHQ34                                                            
         MVI   RCVFILTY,X'FF'      SET E-O-F INDICATOR                          
         B     CHQ36                                                            
*                                                                               
CHQ34    SR    RF,RF               MOVE SORTED RECORD TO MY W/S                 
         ICM   RF,3,0(RE)                                                       
         LA    R0,RCVRECD                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHQ36    CLC   RCVFILTY,TYPE       TEST CHANGE OF TYPE & COMPANY                
         BNE   *+14                                                             
         CLC   RCVRECRD,COMP                                                    
         BE    CHQ66                                                            
         CLI   TYPE,0              TEST FIRST RECORD                            
         BE    CHQ40                                                            
*                                                                               
         CLI   TYPE,AUTHD          DO ACCOUNT/COMPANY TOTALS                    
         BE    *+8                                                              
         BAS   RE,ACCEND                                                        
         BAS   RE,CPYEND                                                        
*&&UK                                                                           
         CLI   RCPOSTNG,C'N'       TEST POSTING=NO                              
         BE    CHQ38                                                            
         XC    WRKID,WRKID                                                      
         MVC   WRKID+0(2),ORIGINUM                                              
         MVC   WRKID+2(3),DESC                                                  
         MVC   WRKID+6(1),TODAYP+2                                              
         MVI   WRKID+7,C'O'                                                     
         XC    WORK,WORK                                                        
         MVC   WORK+0(2),=Y(8+L'LOGONAME)                                       
         MVC   WORK+4(L'COMP),COMP                                              
         MVC   WORK+5(L'ORIGINUM),ORIGINUM                                      
         MVC   WORK+7(L'LOGONAME),LOGONAME                                      
         GOTO1 DATAMGR,DMCB,WRKADD,WRKFIL,WRKID,WORK,WRKBUFF                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
CHQ38    CLI   REMOPT,C'Y'         DO END LOGOS IF NOT DIRECT                   
         BE    CHQ40                                                            
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,LOGOD                                                  
*                                                                               
CHQ40    CLI   RCVFILTY,X'FF'      TEST E-O-F ON SORT                           
         BE    CHQ88                                                            
         MVI   FORCECLR,C'Y'       CLEAR HEADINGS                               
         GOTO1 ACREPORT                                                         
         MVC   HEAD4(L'AC@CPY),AC@CPY                                           
         LA    R1,TOTS             CLEAR ACCUMULATORS                           
         LA    R0,TOTSN                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R0,*-10                                                          
         MVC   LINE,MAXLINES       SET UP FOR NEW REPORT                        
         MVC   PAGE,=H'1'                                                       
*                                                                               
CHQ46    MVC   TYPE,RCVFILTY       SET TYPE AND COMPANY                         
         MVC   COMP,RCVRECRD                                                    
         XC    ORIGINUM,ORIGINUM   CLEAR USER VALUES                            
         MVC   LOGONAME,SPACES                                                  
         MVC   LOGOJOB,SPACES                                                   
         MVC   LOGOADD,SPACES                                                   
         MVC   LOGOADD2,SPACES                                                  
         MVC   LOGO1,SPACES                                                     
         XC    AKEY,AKEY                                                        
*                                                                               
         MVC   WORK,SPACES         READ COMPANY RECORD                          
         MVC   WORK(1),RCVRECRD                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,WORK,IO                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IO+(ACRECORD-ACKEYD)                                          
         SR    R0,R0                                                            
*                                                                               
CHQ48    CLI   0(R1),0             TEST E-O-R                                   
         BE    CHQ56                                                            
         USING ACCOMPD,R1                                                       
         CLI   ACMPEL,ACMPELQ      TEST COMPANY ELEMENT                         
         BNE   CHQ50                                                            
         MVC   ORIGINUM,ACMPID                                                  
         MVC   LOGO1,ACMPABBR                                                   
         MVC   HEAD4+8(L'ACMPALFA),ACMPALFA                                     
         B     CHQ54                                                            
*                                                                               
         USING ACNAMED,R1                                                       
CHQ50    CLI   ACNMEL,ACNMELQ      TEST NAME ELEMENT                            
         BNE   CHQ52                                                            
         USING ACNAMED,R1                                                       
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=Y(ACNMNAME+1-ACNAMED)                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HEAD4+11(0),ACNMNAME                                             
         CH    RE,=Y(L'LOGONAME-1)                                              
         BNH   *+8                                                              
         LH    RE,=Y(L'LOGONAME-1)                                              
         EX    RE,*+8                                                           
         B     CHQ54                                                            
         MVC   LOGONAME(0),ACNMNAME                                             
*                                                                               
         USING ACADDD,R1                                                        
CHQ52    CLI   ACADEL,ACADELQ      TEST ADDRESS ELEMENT                         
         BNE   CHQ54                                                            
         USING ACADDD,R1                                                        
         MVC   LOGOADD,ACADADD                                                  
         MVC   LOGOADD2,ACADADD+L'ACADADD                                       
*                                                                               
CHQ54    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CHQ48                                                            
*                                                                               
CHQ56    OC    ORIGINUM,ORIGINUM                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETLOGO,DMCB,ORIGINUM,LOGOD,DATAMGR                             
         MVC   LOGOJOB+0(4),0(R1)                                               
*                                                                               
CHQ58    LA    R1,TYPETAB          LOCATE TYPE IN TABLE                         
         LA    R0,TYPETABN                                                      
CHQ60    CLC   TYPE,0(R1)                                                       
         BE    *+14                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         BCT   R0,CHQ60                                                         
         DC    H'0'                                                             
*                                                                               
*&&UK*&& MVC   LOGOJOB+4(3),1(R1)                                               
*&&US*&& MVC   LOGOJOB+4(4),=C'CHCK'                                            
         MVC   DESC,1(R1)                                                       
         MVC   RCPROG,2(R1)                                                     
         MVC   RCSUBPRG,4(R1)                                                   
*                                                                               
         BAS   RE,REMCHECK         TEST REMOTE PRINTING                         
         BE    CHQ66                                                            
         MVI   LOGOEND,C'X'                                                     
         MVI   LOGOTYPE,C'S'                                                    
         GOTO1 LOGO,DMCB,LOGOC     NO - PRINT START LOGOS                       
*                                                                               
CHQ66    CLI   TYPE,AUTHD                                                       
         BE    CHQ68                                                            
         CLC   AKEY,ACKEYACC       TEST CHANGE OF ACCOUNT                       
         BE    CHQ68                                                            
         OC    AKEY,AKEY           TEST FIRST TIME                              
         BZ    CHQ68                                                            
         BAS   RE,ACCEND                                                        
*                                                                               
CHQ68    MVC   AKEY,ACKEYACC                                                    
         ZAP   DUB,TRNSAMNT                                                     
         CLI   TYPE,MANCK                                                       
         BNE   CHQ70                                                            
         CLI   ACKEYCON+L'TRMAPNO,C'*'                                          
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         AP    TOT5,DUB                                                         
         AP    TOT2,DUB                                                         
         B     CHQ74                                                            
*                                                                               
CHQ70    TM    TRNSSTAT,X'80'      ADD DEBITS, SUBTRACT CREDITS                 
         BZ    *+14                                                             
         AP    TOT5,DUB                                                         
         B     *+10                                                             
         SP    TOT5,DUB                                                         
*                                                                               
         LA    RE,TOT1             ADD TO DEBIT/CREDIT TOTALS                   
         LA    RF,TOT3                                                          
         TM    TRNSSTAT,X'80'                                                   
         BNZ   *+12                                                             
         LA    RE,L'TOTS(RE)                                                    
         LA    RF,L'TOTS(RF)                                                    
         AP    0(L'TOTS,RE),DUB                                                 
         AP    0(L'TOTS,RF),DUB                                                 
*                                                                               
CHQ74    MVC   P+1(L'ACKEYACC-1),ACKEYACC+1                                     
         MVC   P+42(6),TRNSREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,P+32)                                
         CURED DUB,(12,P+59),2                                                  
         CLI   TYPE,RECON          TEST RECONCILIATION REPORT                   
         BNE   CHQ78                                                            
         MVC   P+71(2),=C'CR'                                                   
         TM    TRNSSTAT,X'80'                                                   
         BZ    *+8                                                              
         MVI   P+71,C'D'                                                        
         MVC   P+17(L'ACKEYCON-1),ACKEYCON+1                                    
*                                                                               
CHQ78    CLI   TYPE,AUTHD          TEST AUTHORISATION REPORT                    
         BNE   CHQ84                                                            
         SR    R0,R0                                                            
         LA    R1,TRANSD                                                        
CHQ80    CLI   0(R1),0                                                          
         BE    CHQ84                                                            
*                                                                               
         USING ACOTHERD,R1         OTHERS ELEMENT                               
         CLI   ACOTEL,ACOTELQ                                                   
         BNE   *+14                                                             
         MVC   P+50(6),ACOTNUM                                                  
         B     CHQ82                                                            
*                                                                               
         USING TRCPJD,R1           CLI/PRO/JOB ELEMENT                          
         CLI   TRCPEL,TRCPELQ                                                   
         BNE   CHQ82                                                            
         LA    RE,P+17                                                          
         MVC   0(L'TRCPCLI,RE),TRCPCLI                                          
         LA    RE,L'TRCPCLI-1(RE)                                               
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVC   2(L'TRCPPROD,RE),TRCPPROD                                        
         LA    RE,2+L'TRCPPROD-1(RE)                                            
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVC   2(L'TRCPJOB,RE),TRCPJOB                                          
*                                                                               
CHQ82    IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     CHQ80                                                            
*                                                                               
CHQ84    CLI   TYPE,MANCK          TEST MANUAL CHEQUE                           
         BNE   CHQ86                                                            
         MVC   P+17(L'TRMAPNO+1),ACKEYCON                                       
*                                                                               
CHQ86    GOTO1 ACREPORT            PRINT REPORT LINE                            
         B     CHQ32                                                            
*                                                                               
CHQ88    DS    0H                                                               
*&&UK                                                                           
         CLI   RCPOSTNG,C'N'       TEST POSTING=NO                              
         BE    CHQ90                                                            
         GOTO1 DATAMGR,DMCB,WRKCLO,WRKFIL,WRKID,WORK,WRKBUFF                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
CHQ90    B     XIT                                                              
         EJECT                                                                  
* PUT A RECORD TO SORTER                                                        
*                                                                               
PUTSORT  LR    R0,RE                                                            
         CLI   SORTSW,0                                                         
         BNE   PUTSORT2                                                         
         MVI   SORTSW,1                                                         
         GOTO1 ADSORTER,DMCB,SORTCRD1,SORTCRD2,0                                
*                                                                               
PUTSORT2 MVC   RCVFILTY,TYPE                                                    
         GOTO1 ADSORTER,DMCB,SORTPUT,RCVRECD                                    
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE FOR COMPANY TOTALS                                                    
*                                                                               
CPYEND   NTR1  ,                                                                
         MVC   PSECOND+1(L'AC@CPYTS),AC@CPYTS                                   
         CLI   TYPE,RECON                                                       
         BNE   *+16                                                             
         SP    TOT1,TOT2                                                        
         ZAP   TOT2,TOT1                                                        
         CURED TOT2,(12,PSECOND+59),2                                           
         GOTO1 ACREPORT                                                         
         CLI   REMOPT,C'Y'                                                      
         BNE   CPYENDX                                                          
         GOTO1 PRINT,DMCB,SPACES,BC01                                           
         GOTO1 (RF),(R1),(L'CLOSE,CLOSE)                                        
CPYENDX  B     XIT                                                              
         SPACE 2                                                                
* ROUTINE FOR ACCOUNT TOTALS                                                    
*                                                                               
ACCEND   NTR1  ,                                                                
         MVC   PSECOND+1(L'AC@ACCTS),AC@ACCTS                                   
         CURED TOT5,(12,PSECOND+59),2                                           
         MVI   PTHIRD+1,0                                                       
         GOTO1 ACREPORT                                                         
         ZAP   TOT5,=P'0'                                                       
ACCENDX  B     XIT                                                              
         EJECT                                                                  
* CHECK PROFILE RECORD FOR REMOTE PRINTING                                      
*                                                                               
REMCHECK NTR1  ,                                                                
         L     R3,REMOTEC                                                       
         USING REMOTED,R3                                                       
         XC    REMOTKEY,REMOTKEY                                                
         MVI   REMOPT,C'N'         DEFAULT IS NO                                
         LA    R2,IO                                                            
         USING CTPREC,R2                                                        
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,C'P'                                                     
         MVC   CTPKSYS(CTPKORIG-CTPKSYS),TYPETAB+1                              
         MVC   CTPKORIG,ORIGINUM                                                
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTPREC,CTPREC                         
         CLI   8(R1),0                                                          
         BNE   REMX                                                             
*                                                                               
         LA    R1,CTPDATA                                                       
         SR    R0,R0                                                            
REM2     CLI   0(R1),0                                                          
         BE    REMX                                                             
         CLI   0(R1),X'42'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     REM2                                                             
*                                                                               
         USING CTOCOD,R1                                                        
         CLC   CTOCODE(L'REMLIT),REMLIT                                         
         BNE   REMX                                                             
         MVI   REMOPT,C'Y'                                                      
         MVC   REMOTJID,DESC                                                    
         MVC   REMOTKEY(L'REPLIT),REPLIT                                        
         MVC   REMOTDST(2),ORIGINUM                                             
         MVI   REMOTCLS,C'Q'                                                    
         B     REMX                                                             
*                                                                               
REMX     CLI   REMOPT,C'Y'                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
VGETLOGO DC    V(GETLOGO)                                                       
         SPACE 2                                                                
DICI     DS    0X                                                               
         DCDDL AC#CPY,7                                                         
         DCDDL AC#CPYTS,20                                                      
         DCDDL AC#ACCTS,20                                                      
DICIX    DC    X'00'                                                            
         SPACE 2                                                                
BC01     DC    C'BC01'                                                          
CLOSE    DC    C'CLOSE'                                                         
WRKCLO   DC    C'CLO'                                                           
WRKADD   DC    C'ADD'                                                           
WRKFIL   DC    C'WKFILE '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
CONFIL   DC    C'CTFILE '                                                       
REMLIT   DC    C'REMOTE'                                                        
REPLIT   DC    C'AUTH/REC'                                                      
         SPACE 2                                                                
TYPETAB  DS    0XL5                                                             
         DC    C'A',C'A02',AL1(1)                                               
         DC    C'R',C'A03',AL1(2)                                               
         DC    C'C',C'A04',AL1(3)                                               
TYPETABN EQU   (*-TYPETAB)/L'TYPETAB                                            
         SPACE 2                                                                
SORTCRD1 DC    C'SORT FIELDS=(5,1,A,29,42,A,9,4,A),FORMAT=BI,WORK=1 '           
SORTCRD2 DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
         SPACE 2                                                                
WRKBUFF  DS    4500C                                                            
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
AC07D    DSECT                                                                  
ACPYEL   DS    A                                                                
ACHAEL   DS    A                                                                
REMOPT   DS    CL1                                                              
TYPE     DS    CL1                                                              
RECON    EQU   C'R'                                                             
AUTHD    EQU   C'A'                                                             
MANCK    EQU   C'C'                                                             
COMP     DS    XL1                                                              
SORTSW   DS    XL1                                                              
TODAYP   DS    PL3                                                              
DESC     DS    CL3                                                              
WRKID    DS    XL16                                                             
TOTS     DS    0PL8                                                             
TOT1     DS    PL8                                                              
TOT2     DS    PL8                                                              
TOT3     DS    PL8                                                              
TOT4     DS    PL8                                                              
TOT5     DS    PL8                                                              
TOTSN    EQU   (*-TOTS)/L'TOTS                                                  
AKEY     DS    CL(L'ACKEYACC)                                                   
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
IO       DS    2100X               I/O AREA                                     
COPYREC  DS    2100X               SAVED COPY RECORD                            
         EJECT                                                                  
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACLANGEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLANGEQU                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPMC02 05/01/02'                                      
         END                                                                    
