*          DATA SET PPREPIV02  AT LEVEL 006 AS OF 05/13/20                      
*PHASE PPIV02B                                                                  
*INCLUDE PUBEDIT                                                                
*INCLUDE SORTER                                                                 
         TITLE 'PPIV02 - Successful Autopay Report'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SCHT SEP/15  Report successful AutoPay transactions                           
*        This program reads the AutoPay records on PRTFILE.  If the             
*        AutoPay record has been processed and has a paid date, it is           
*        reported in the PIV report.                                            
*                                                                               
* SCHT MAY/20                                                                   
*                                                                               
* New filters in request card:                                                  
* QOPT2(col 63): F = Print full details (print RUN and PAID dates)              
* QOPT3(col 64): D = Use date @ Column 38 as the RUN DATE                       
*                                                                               
*                                                                               
* SCHT MAY/20 (SPEC-45619)                                                      
*                                                                               
* There are weekend processing issues.  Friday night's PIP run gets             
* picked up by the script Sat morning but the PIV and PIW don't run on          
* Sat.  Saturday night's PIP run does not get picked up till Monday as          
* scripts do not run on Sundays.  PIV runs on Sunday night but does             
* not pick up Friday night's PIP's.  Saturday night's PIP and Sunday            
* night's PIP get picked up by the script on Monday morning.  Monday's          
* PIV picks up Sunday night's PIP but not Saturday's.  What does this           
* mean?                                                                         
*                                                                               
* The Sunday night PIV run must pick up Friday night's PIP.                     
* The Monday morning PIV run must pick up Sat and Sun night's PIP.              
* Tue-Fri morning PIV runs pick up the prior days PIP.                          
*                                                                               
* The PIP run date is what's in the autopay key (PAPYKDAT)                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PPIV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPIV02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPPYWRKD,R8                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNF     DS    0H                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
*                                                                               
         L     RE,VCOMFACS                                                      
         MVC   HEXIN,CHEXIN-COMFACSD(RE)                                        
*                                                                               
         MVI   FCRDCLOS,C'Y'       Pass back close outs                         
*                                                                               
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROC     BAS   RE,CHKAGY           Validate agency                              
         JNE   EXIT                                                             
*                                                                               
         BAS   RE,INIT             Initialize values                            
*                                                                               
         LA    R2,KEY              Read autopay records                         
         USING PAPYREC,R2                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVC   PAPYKAGY,QAGENCY    Agency                                       
         GOTO1 HIGH                                                             
         J     PROC5_12                                                         
*                                                                               
PROC5_10 GOTO1 SEQ                                                              
*                                                                               
PROC5_12 LA    R2,KEY                                                           
         CLC   PAPYKAGY,QAGENCY                                                 
         JNE   PROC5_20                                                         
         CLI   PAPYKTYP,PAPYKRCD   Autopay record?                              
         JNE   PROC5_10                                                         
         CLC   DATE,PAPYKDAT       Same date?                                   
         JE    PROC5_13                                                         
*                                                                               
* PIV run on Sun: DATE=Sat PREVDATE=Fri                                         
* PIV run on Mon: DATE=Sun PREVDATE=Sat                                         
*                                                                               
         OC    PREVDATE,PREVDATE                                                
         JZ    PROC5_10                                                         
         CLC   PREVDATE,PAPYKDAT                                                
         JNE   PROC5_10                                                         
*&&DO                                                                           
*                                                                               
* Get latest autopay record on the file and process only that days              
* activity.                                                                     
*                                                                               
         OC    DATE,DATE           First time through?                          
         JNZ   *+10                                                             
         MVC   DATE,PAPYKDAT       Set it                                       
*                                                                               
         CLC   DATE,PAPYKDAT       Same date?                                   
         JNE   PROC5_10                                                         
*&&                                                                             
PROC5_13 TM    KEY+25,PAPYPRCQ     Processed?                                   
         JZ    PROC5_10                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC  '),=C'PRTFILE',        *        
               KEY+27,AREC,(0,DMWORK)                                           
*                                                                               
         L     R2,AREC                                                          
         AHI   R2,33                                                            
         USING PAP1D,R2                                                         
         OC    PAP1ADTE,PAP1ADTE   Autopaid succesfully?                        
         JZ    PROC5_10                                                         
*                                                                               
         MVC   SVVALS(SVVALSQ),SPACES                                           
         GOTO1 DATCON,DMCB,(3,PAP1ADTE),(8,SVADAT)  Autopaid date               
*                                                                               
         L     R2,AREC                                                          
         USING PAPYREC,R2                                                       
         MVC   SVMED,PAPYKMED      Media                                        
         MVC   SVCLT,PAPYKCLT      Client                                       
         EDIT  PAPYKSN,SVSERNUM,0,ALIGN=LEFT      Serial #                      
         MVC   HALF,PAPYKDAT                                                    
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(8,SVDAT)     Date                          
*                                                                               
         AHI   R2,33                                                            
         USING PAP1D,R2                                                         
*                                                                               
         MVC   SVPRD,PAP1PRD       Product                                      
         EDIT  PAP1EST,(3,SVEST),0,ALIGN=LEFT     Estimate                      
         GOTO1 DATCON,DMCB,(3,PAP1DATE),(8,SVPER)                               
*                                                                               
         MVI   SVPER+8,C'-'        Default to ref 1                             
         MVI   SVPER+9,C'1'                                                     
         CLI   PAP1REF,1                                                        
         JE    PROC5_18                                                         
         EDIT  PAP1REF,(3,SVPER+9),0,ALIGN=LEFT                                 
*                                                                               
         ZIC   R0,PAP1REF                                                       
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    PROC5_14                                                         
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  SVPER+10(1),DUB+7(1)                                             
         OI    SVPER+10,X'F0'                                                   
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   SVPER+9(1),0(RF)                                                 
         MVI   SVPER+11,C' '                                                    
         B     PROC5_18            DONE                                         
*                                                                               
PROC5_14 OI    DUB+7,X'0F'                                                      
         UNPK  SVPER+9(2),DUB                                                   
         CLI   SVPER+9,C'0'                                                     
         BNE   *+10                                                             
         MVC   SVPER+9(2),SVPER+10                                              
*                                                                               
PROC5_18 MVC   SVINV#,PAP1INV#     Invoice number                               
         MVC   SVINV$,PAP1INV$     Invoice $                                    
         MVC   SVPAYEE,PAP1PAYE+1  Payee (minus the 'S')                        
         GOTO1 DATCON,DMCB,(3,PAP1ADTE),(8,SVADAT)  Autopaid date               
         GOTO1 =V(PUBEDIT),DMCB,(8,PAP1PUB),(C'S',SVPUB)                        
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SVVALS                                   
         J     PROC5_10                                                         
         DROP  R2                                                               
*                                                                               
PROC5_20 GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BZ    PROCX                                                            
*                                                                               
         MVC   SVVALS(SVVALSQ),0(R5)                                            
*                                                                               
         OC    PREVMED,PREVMED     First time through?                          
         JNZ   *+10                                                             
         MVC   PREVMED,SVMED       Yes                                          
*                                                                               
         CLC   SVMED,PREVMED       New media?                                   
         JE    PROC5_22                                                         
         BAS   RE,PRINTTOT         Print totals for media                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVMED,SVMED                                                    
*                                                                               
PROC5_22 LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PRCLT,SVCLT                                                      
         MVC   PRPRD,SVPRD                                                      
         MVC   PREST,SVEST                                                      
         MVC   PRPUB,SVPUB                                                      
         CLI   QOPT2,Q2FULLQ       Print full details?                          
         JNE   PROC5_23                                                         
         MVC   PRRUN,SVDAT                                                      
         MVC   PRPAID,SVADAT                                                    
*                                                                               
PROC5_23 MVC   PRPER(8),SVPER                                                   
         CLC   =C'-1 ',SVPER+8                                                  
         JE    *+10                                                             
         MVC   PRPER+8(4),SVPER+8                                               
*                                                                               
         MVC   PRPAYEE,SVPAYEE                                                  
         MVC   PRINV#,SVINV#                                                    
         MVC   PRINV$,SVINV$                                                    
         GOTO1 REPORT                                                           
*                                                                               
         AP    MEDCOUNT,=P'1'                                                   
*                                                                               
         LA    RF,SVINV$           Move invoice $ to field for packing          
         XC    TEMP$,TEMP$                                                      
         LA    RE,TEMP$                                                         
         LA    R1,12                                                            
PROC5_30 CLI   0(RF),C' '                                                       
         JE    PROC5_36                                                         
         CLI   0(RF),C'.'          Femove decimal point                         
         JE    PROC5_34                                                         
PROC5_32 MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
PROC5_34 AHI   RF,1                                                             
         BCT   R1,PROC5_30                                                      
*                                                                               
PROC5_36 LA    RE,12                                                            
         SR    RE,R1               Length of input                              
         SHI   RE,1                Subtract off for decimal point               
*                                                                               
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),TEMP$(0)                                                  
*                                                                               
         AP    MEDTOTAL,DUB                                                     
         B     PROC5_20                                                         
         DROP  R2                                                               
*                                                                               
PROCX    BAS   RE,PRINTTOT         Print last media's totals                    
         J     EXIT                                                             
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNL     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
RUNLX    J     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
PRINTTOT NTR1                                                                   
         MVC   P(33),=C'Total number of insertions paid: '                      
         EDIT  MEDCOUNT,DUB,,ALIGN=LEFT                                         
         MVC   P+35(L'DUB),DUB                                                  
         GOTO1 REPORT                                                           
         MVC   P(33),=C'Total amount paid:               '                      
         EDIT  MEDTOTAL,TEMP$,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=$                 
         MVC   P+35(L'TEMP$),TEMP$                                              
         GOTO1 REPORT                                                           
         ZAP   MEDCOUNT,=P'0'                                                   
         ZAP   MEDTOTAL,=P'0'                                                   
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPRT     NTR1                                                                   
         MVI   RCSUBPRG,0                                                       
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INIT     NTR1                                                                   
         XC    PREVMED,PREVMED                                                  
         ZAP   MEDCOUNT,=P'0'                                                   
         ZAP   MEDTOTAL,=P'0'                                                   
         MVI   FLAGS,0                                                          
*                                                                               
* This report runs on the following date/times:                                 
* Mon-Fri: around 10am EST                                                      
* Sat: Does not run                                                             
* Sun: Runs Sunday night                                                        
*                                                                               
* PAPYKDAT is the date of the PIP run in the AUTOPAY key.                       
*                                                                               
* PIV run on Mon: DATE=Sun PREVDATE=Sat                                         
* PIV run on Tue-Fri: DATE=yesterday PREVDATE=null                              
* PIV run on Sun: DATE=Sat PREVDATE=Fri                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DATEC6)                                     
         CLI   QOPT3,Q3DATEQ       Use request card date?                       
         JNE   INIT_10                                                          
         GOTO1 DATCON,DMCB,(0,QSTART),(0,DATEC6)                                
*                                                                               
INIT_10  GOTO1 GETDAY,DMCB,DATEC6,FULL                                          
         CLI   DMCB,1              Test if this is being run on Monday          
         JNE   *+8                                                              
         OI    FLAGS,FLMONQ                                                     
         CLI   DMCB,7              Test if this is being run on Sunday          
         JNE   *+8                                                              
         OI    FLAGS,FLSUNQ                                                     
*                                                                               
         GOTO1 GETDAY,DMCB,DATEC6,FULL                                          
         GOTO1 ADDAY,DMCB,DATEC6,DUB,-1  Get prior day's records                
         GOTO1 DATCON,DMCB,(0,DUB),(21,DATEC10)                                 
         GOTO1 DATCON,DMCB,(0,DUB),(0,DATEC6)                                   
         GOTO1 DATCON,DMCB,(0,DUB),(2,DATEC2)                                   
         GOTO1 DATCON,DMCB,(0,DUB),(1,DATEC1)                                   
         MVC   DATE,DATEC2         Report this date's autopays                  
         XC    DATE,=X'FFFF'                                                    
         XC    PREVDATE,PREVDATE                                                
*                                                                               
         TM    FLAGS,FLMONQ        Is this being run on Monday?                 
         JZ    INIT_20                                                          
         GOTO1 ADDAY,DMCB,DATEC6,DUB,-1  Yes - Look up Saturday's too           
         GOTO1 DATCON,DMCB,(0,DUB),(2,PREVDATE)                                 
         XC    PREVDATE,=X'FFFF'                                                
         J     INITX                                                            
*                                                                               
INIT_20  TM    FLAGS,FLSUNQ        Is this being run on Sunday?                 
         JZ    INITX                                                            
         GOTO1 ADDAY,DMCB,DATEC6,DUB,-1  Yes - Look up Friday's too             
         GOTO1 DATCON,DMCB,(0,DUB),(2,PREVDATE)                                 
         XC    PREVDATE,=X'FFFF'                                                
*                                                                               
INITX    J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         JNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* Find entry in AGYTABLE                                                        
*                                                                               
CHKAGY   NTR1                                                                   
         USING AGYTABD,R4                                                       
         LA    R4,AGYTABLE                                                      
CA10     CLI   0(R4),X'FF'                                                      
         JE    NO                  Not in table                                 
         CLC   QAGENCY,ATALPHA     match on agy alpha                           
         JE    YES                                                              
         AHI   R4,ATLENQ                                                        
         J     CA10                                                             
         DROP  R4                                                               
*                                                                               
AGYTABLE DC    C'SJ'                                                            
         DC    C'*B'                                                            
         DC    C'H7'                                                            
         DC    C'FR'                                                            
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+40(20),=C'PAID INVOICE LISTING'                               
         MVC   H2+40(20),=C'--------------------'                               
         MVC   H3(12),=C'Media Code: '                                          
         MVC   H3+13(1),SVMED                                                   
*                                                                               
         LA    RF,H4                                                            
         USING PLINED,RF                                                        
         MVC   0(PLINEDQ,RF),SPACES                                             
*                                                                               
         MVC   PRCLT,=C'CLI'                                                    
         MVC   PRPRD,=C'PRD'                                                    
         MVC   PREST,=C'EST'                                                    
         MVC   PRPUB+4(6),=C'VENDOR'                                            
         MVC   PRPAYEE(4),=C'SREP'                                              
         MVC   PRPER+2(6),=C'PERIOD'                                            
         MVC   PRINV#(7),=C'INV NUM'                                            
         MVC   PRINV$,=C'AMOUNT PAID '                                          
         CLI   QOPT2,Q2FULLQ       Print full details?                          
         JNE   HDHK_10                                                          
         MVC   PRRUN,=C'RUN DATE'                                               
         MVC   PRPAID(9),=C'PAID DATE'                                          
*                                                                               
HDHK_10  LA    RF,H5                                                            
         USING PLINED,RF                                                        
         MVC   0(PLINEDQ,RF),SPACES                                             
*                                                                               
         MVC   PRCLT,=C'---'                                                    
         MVC   PRPRD,=C'---'                                                    
         MVC   PREST,=C'---'                                                    
         MVC   PRPUB,=C'---------------'                                        
         MVC   PRPAYEE,=C'----'                                                 
         MVC   PRPER,=C'------------'                                           
         MVC   PRINV#,=C'------------'                                          
         MVC   PRINV$,=C'------------'                                          
         CLI   QOPT2,Q2FULLQ       Print full details?                          
         JNE   HDHKX                                                            
         MVC   PRRUN,=C'--------'                                               
         MVC   PRPAID(9),=C'---------'                                          
*                                                                               
HDHKX    J     EXIT                                                             
         LTORG                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,18,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=96'                                    
*                                                                               
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
*                                                                               
SVKEY    DS    CL32                                                             
SKIPLNSW DS    CL1                                                              
DATEC10 DS     CL10                                                             
DATEC6   DS    CL6                                                              
DATEC2   DS    XL2                                                              
DATEC1   DS    XL3                 YYMMDD PWOS                                  
BSYSTIME DS    XL3                                                              
DATE     DS    XL2                 Run date for autopays                        
PREVDATE DS    XL2                 Run date for additional autopays             
*                                                                               
AIO2L    DS    F                                                                
AIO2     DS    F                                                                
AWKBUFF  DS    F                                                                
AWKBUFFX DS    F                                                                
SEQNUM   DS    F                                                                
FIXED    DS    CL1                                                              
WRKRSTAT DS    XL1                                                              
WSTATOPN EQU   X'80'               Worker file is open                          
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
*                                                                               
PREVMED  DS    CL1                 PREVIOUS MEDIA                               
MEDCOUNT DS    PL8                                                              
MEDTOTAL DS    PL8                                                              
TEMP$    DS    CL12                                                             
*                                                                               
Q2FULLQ  EQU   C'F'                Print full details                           
Q3DATEQ  EQU   C'D'                Use date from request for run date           
*                                                                               
FLAGS    DS    X                                                                
FLSUNQ   EQU   X'07'               Sunday date                                  
FLMONQ   EQU   X'01'               Monday date                                  
*                                                                               
SVVALS   DS    0X                                                               
SVMED    DS    CL1                 Media                                        
SVCLT    DS    CL3                 Client                                       
SVPRD    DS    CL3                 Product                                      
SVEST    DS    CL3                 Estimate                                     
SVDAT    DS    CL8                 Date                                         
SVKEYQ   EQU   *-SVVALS                                                         
SVSERNUM DS    CL10                Serial #                                     
SVPER    DS    CL17                Period (date-reference)                      
SVPUB    DS    CL15                Publication                                  
SVPAYEE  DS    CL4                 Payee                                        
SVINV#   DS    CL12                Invoice number                               
SVINV$   DS    CL12                Invoice $                                    
SVADAT   DS    CL8                 Autopaid date                                
SVVALSQ  EQU   *-SVVALS                                                         
*                                                                               
         DS    F                                                                
         DS    0D                                                               
         DC    C'*RCVREC*'                                                      
RCVREC   DC    F'0'                VAR REC LEN                                  
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL32                                                            
REC      DS    4000C                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPPYWRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL50                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
BADOPTSW DS    CL1                                                              
*                                                                               
AUTOPYSW DS    X                                                                
APYINV#Q EQU   X'80'               Have invoice number                          
APYINV$Q EQU   X'40'               Have invoice amount                          
APYPASSQ EQU   APYINV#Q+APYINV$Q                                                
*                                                                               
CURMEDCD DS    F                                                                
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
HEXIN    DS    V                                                                
WKKEY    DS    XL25                                                             
TMKEY    DS    XL25                                                             
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE PSERELEM                                                       
       ++INCLUDE PPGENAPY                                                       
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
*                                                                               
*        Agency Table DSECT                                                     
*                                                                               
AGYTABD  DSECT                                                                  
ATALPHA  DS    CL2                 Agency alpha id                              
ATLENQ   EQU   *-AGYTABD                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PLINED   DSECT                                                                  
PRCLT    DS    CL3                 Client                                       
         DS    CL2                                                              
PRPRD    DS    CL3                 Product                                      
         DS    CL2                                                              
PREST    DS    CL3                 Estimate                                     
         DS    CL2                                                              
PRPUB    DS    CL15                Publication                                  
         DS    CL1                                                              
PRPAYEE  DS    CL4                 Payee                                        
         DS    CL1                                                              
PRPER    DS    CL12                Period (date-reference)                      
         DS    CL1                                                              
PRINV#   DS    CL12                Invoice number                               
         DS    CL1                                                              
PRINV$   DS    CL12                Invoice $                                    
         DS    CL3                                                              
PRRUN    DS    CL8                 Run date                                     
         DS    CL3                                                              
PRPAID   DS    CL8                 Paid date                                    
PLINEDQ  EQU   *-PRCLT                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPIV02 05/13/20'                                      
         END                                                                    
