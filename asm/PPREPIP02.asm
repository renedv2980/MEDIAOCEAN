*          DATA SET PPREPIP02  AT LEVEL 011 AS OF 05/13/20                      
*PHASE PPIP02B                                                                  
*INCLUDE PUBEDIT                                                                
         TITLE 'PPIP02 - Read recovery and build AutoPay'                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SCHT SEP/15  Scan for autopay eligible buys                                   
*         This program reads the print recovery file for buys that              
*         are eligible for AutoPay.  If the buy qualifies, an AutoPay           
*         record is added to PRTFILE and viewable in Print/SFM.                 
*         After all the AutoPay records are added, they are then                
*         scanned to create worker files for each agency.                       
*                                                                               
* SCHT MAY/20                                                                   
*                                                                               
* Removed all special Saturday processing code.  The run date in the            
* AUTOPAY key (PAPYKDAT) will always be the date this is run or if              
* overriden using the new request option (QOPT3 = D).                           
*                                                                               
* Added new record length for the 01 element to save error messages             
* from PRINT/PAY system.                                                        
*                                                                               
* For TESTING:                                                                  
*                                                                               
* Reminder - when running this on TST/FQA/CSC, first run it to                  
* generate the AUTOPAY records only (QOPT4 = A).  Then after issuing            
* an SSB/POP on the system to make sure the records are added, run              
* this again to generate the worker file only (QOPT4 = W).                      
*                                                                               
* New options in request card:                                                  
* QOPT1(col 62):   Determines which agency table to use                         
*                  blank = ADV system                                           
*                  C=CSC System                                                 
*                  Q=FQA System                                                 
*                  T=TST System                                                 
*                                                                               
* QOPT3(col 64):   D=Use QSTART date (col 38)                                   
*                                                                               
* QOPT4(col 65):   A=Generate autopay records only                              
*                  W=Generate worker file only                                  
*                                                                               
* QOPT6(col 67):   Alpha Agency ID (2 characters)                               
* QOPT7(col 68)                                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PPIP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPIP02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPIPWRKD,R8                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     LARL  RE,REC                                                           
         ST    RE,AREC                                                          
         ZAP   BUYAPCNT,=P'0'      Buys eligible for autopay                    
         ZAP   APAYRCNT,=P'0'      Number of autopay records created            
         XC    SVCLTKEY,SVCLTKEY   Init save client record key                  
         MVI   SVCOFFCD,0          Init save client office code                 
*                                                                               
* Default to today's date                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DATEC0)                                     
         GOTO1 DATCON,DMCB,(5,0),(1,DATEC1)                                     
         GOTO1 DATCON,DMCB,(5,0),(2,DATEC2)                                     
         NI    FLAGS,X'FF'-FLRWFQ                                               
         MVI   FCRDCLOS,C'Y'       Pass back close outs                         
         B     EXIT                                                             
*                                                                               
* Read recovery file for eligible buys                                          
*                                                                               
*                                                                               
PROC     CLI   QOPT4,Q4WRKRQ       Generate worker file only?                   
         JE    PROC_100                                                         
         CLI   QOPT3,Q3DATEQ       Use QSTART date instead?                     
         JNE   PROC_02                                                          
         GOTO1 DATCON,DMCB,(0,QSTART),(0,DATEC0)                                
         GOTO1 DATCON,DMCB,(0,QSTART),(1,DATEC1)                                
         GOTO1 DATCON,DMCB,(0,QSTART),(2,DATEC2)                                
*                                                                               
PROC_02  OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
PROC_10  LARL  R5,RCVREC                                                        
         USING RECVHDR,R5                                                       
         GET   RECVIN,0(R5)                                                     
*                                                                               
         XC    SVVALS(SVVALSQ),SVVALS                                           
*                                                                               
         LARL  R5,RCVREC                                                        
         AHI   R5,4                                                             
*&&DO                                                                           
         GOTO1 DATCON,DMCB,(3,RDATE),(0,DATEC0)                                 
         GOTO1 GETDAY,DMCB,DATEC0,FULL                                          
         CLI   DMCB,6              Test Saturday                                
         JNE   PROC_12                                                          
         OI    FLAGS,FLRWFQ                                                     
         GOTO1 ADDAY,DMCB,DATEC0,DUB,2      Make it Monday                      
         GOTO1 DATCON,DMCB,(0,DUB),(2,DATEC2)                                   
*&&                                                                             
PROC_12  CLI   RRECTY,RRECTCPY     Copy?                                        
         JNE   *+8                                                              
         MVI   COPYFLAG,0                                                       
*                                                                               
         CLI   RFILTY,X'42'        Print file?                                  
         JNE   PROC_10                                                          
*                                                                               
         LARL  R2,REC                                                           
         USING PBUYREC,R2                                                       
         CLI   3(R2),X'20'         Buy record?                                  
         JNE   PROC_10                                                          
*                                                                               
         LR    RE,R2               Point to begining of buy record              
         SR    RF,RF                                                            
         ICM   RF,3,PBUYLEN                                                     
         AR    RE,RF               Point to end of buy record                   
         XC    0(2,RE),0(RE)       Set end-of-record marker                     
*                                                                               
         GOTOR CHKAGY,DMCB,AGYENTRY   Get agency                                
         JNE   PROC_10                                                          
*                                                                               
         GOTOR GETCLT              Get client                                   
*                                                                               
         XC    WORK,WORK                                                        
         XC    A0BPROF,A0BPROF                                                  
         MVC   WORK(4),=C'PA0B'    Get A0B profile                              
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),PBUYKAGY                                               
         MVC   WORK+6(1),PBUYKMED                                               
         MVC   WORK+7(3),PBUYKCLT                                               
         CLI   SVCOFFCD,C' '                                                    
         JNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFCD                                              
         GOTO1 GETPROF,DMCB,WORK,A0BPROF,DATAMGR                                
         CLI   A0BPROF,C'Y'        Use AutoPay?                                 
         JNE   PROC_10                                                          
*                                                                               
         BRAS  RE,TSTOK            BUY OK to pay?                               
         JE    PROC_20                                                          
         CLI   RRECTY,RRECTCPY     No - Is it Copy?                             
         JE    PROC_10             Yes - skip it                                
         TM    COPYFLAG,APYYES#Q   Ok to Pay turned off?                        
         JO    PROC_32             Yes - delete autopay record                  
         J     PROC_10             No - skip it                                 
*                                                                               
PROC_20  CLI   RRECTY,RRECTCPY     Yes - Is it Copy?                            
         JNE   *+14                                                             
         MVC   COPYFLAG,AUTOPYSW   Save flag for Copy                           
         J     PROC_10                                                          
*                                                                               
         CLI   RRECTY,RRECTCHG     Is it Change?                                
         JNE   *+8                                                              
         MVI   COPYFLAG,0          Ok to Pay not turned off                     
*                                                                               
PROC_30  AP    BUYAPCNT,=P'1'                                                   
*                                                                               
PROC_32  LARL  R2,REC              A(Buy record in recovery file)               
         USING PBUYREC,R2                                                       
         LARL  R3,AIO1             A(Autopay record)                            
         USING PAPYREC,R3                                                       
*                                                                               
         MVC   PAPYKAGY,PBUYKAGY   Agency                                       
         MVC   PAPYKMED,PBUYKMED   Media                                        
         MVI   PAPYKTYP,PAPYKRCD   Autopay record type                          
         MVC   PAPYKDAT,DATEC2     Date                                         
         XC    PAPYKDAT,=X'FFFF'                                                
         MVC   PAPYKCLT,PBUYKCLT   Client                                       
         MVC   PAPYKSN,SVSERNUM    Serial#                                      
         MVI   PAPYLEN+1,PAPYEL-PAPYKEY                                         
*        MVC   PAPYLEN,=X'0021'                                                 
*                                                                               
         LA    R4,33(R3)                                                        
         USING PAP1D,R4                                                         
         MVI   PAP1EL,PAP1ELQ                                                   
         MVI   PAP1LEN,PAP1LNQ2                                                 
         MVC   PAP1STAT,SVSTAT     Status                                       
         MVC   PAP1PRD,PBUYKPRD    Product                                      
         MVC   PAP1EST,PBUYKEST    Estimate                                     
         MVC   PAP1DATE,PBUYKDAT   Insertion date                               
         MVC   PAP1REF,PBUYKLIN    Insertion reference number                   
         MVC   PAP1PUB,PBUYKPUB    Publication                                  
         MVC   PAP1ZONE,PBUYKZON   Zone                                         
         MVC   PAP1EDIT,PBUYKEDT   Edition                                      
         MVC   PAP1PAYE,SVPAYE     Payee/Rep                                    
         MVC   PAP1INVD,PBUYKDAT   Invoice date = Insertion date                
         MVC   PAP1INV#,SVINV#     Invoice number                               
         MVC   PAP1INV$,SVINV$     Invoice dollar                               
         MVC   PAP1RATE,SVRATE     Rate                                         
*                                                                               
         SR    RF,RF               Update record length                         
         ICM   RF,3,PAPYLEN                                                     
         AHI   RF,PAP1LNQ2                                                      
         STCM  RF,3,PAPYLEN                                                     
         DROP  R3,R4                                                            
*                                                                               
         CLI   RRECTY,RRECTADD     Add?                                         
         JE    PROC_90                                                          
         TM    COPYFLAG,APYYES#Q   Ok to Pay turned off?                        
         JO    *+12                Yes - delete autopay record                  
         TM    PBUYCNTL,X'80'      Buy deleted?                                 
         JZ    PROC_90                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'PAPYKEY),0(R3)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PAPYKEY),0(R3)                                             
         JNE   PROC_10             Autopay doesn't exist - get next buy         
*                                                                               
PROC_85  LARL  RF,AIO2             A(Temporary I/O)                             
         ST    RF,AREC             Set AREC for autopay getrec/putrec           
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC  '),=C'PRTFILE',        *        
               KEY+27,AREC,(0,DMWORK)                                           
*                                                                               
         LA    RF,KEY                                                           
         USING PAPYREC,RF          Mark autopay key deleted                     
         OI    KEY+L'PAPYKEY,X'80'                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMWRT   '),=C'PRTDIR ',        *        
               KEY,KEY,(0,DMWORK)                                               
*                                                                               
         L     RF,AREC                                                          
         OI    PAPYCNTL,X'80'      Mark autopay rec deleted                     
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'PUTREC  '),=C'PRTFILE',        *        
               KEY,AREC,(0,DMWORK)                                              
         MVC   ACTION,=C'DEL'                                                   
         J     PROC_96                                                          
         DROP  RF                                                               
*                                                                               
PROC_90  OI    DMINBTS,X'08'       Check if autopay already exists              
         XC    KEY,KEY                                                          
         MVC   KEY(L'PAPYKEY),0(R3)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PAPYKEY),0(R3)                                             
         JNE   PROC_94             Autopay doesn't exist, add it                
*                                                                               
         LARL  RF,AIO2             Autopay does exist, get it                   
         ST    RF,AREC             for autopay getrec/putrec                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC  '),=C'PRTFILE',        *        
               KEY+27,AREC,(0,DMWORK)                                           
*                                                                               
         LA    RF,KEY                                                           
         USING PAPYREC,RF          Test if autopay is deleted                   
         TM    KEY+L'PAPYKEY,X'80'                                              
         JZ    PROC_92                                                          
         NI    KEY+L'PAPYKEY,X'FF'-X'80'   If so, restore                       
         GOTO1 WRT                                                              
         DROP  RF                                                               
*                                                                               
PROC_92  LARL  RF,AIO1             Rebuilt autopay                              
         ST    RF,AREC             Update file with rebuilt autopay             
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'PUTREC  '),=C'PRTFILE',        *        
               KEY,AREC,(0,DMWORK)                                              
         MVC   ACTION,=C'CHG'                                                   
         J     PROC_96                                                          
*                                                                               
PROC_94  LARL  RF,AIO1                                                          
         ST    RF,AREC             Set AREC to autopay record                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'PAPYKEY),0(RF)                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'ADDREC  '),=C'PRTFILE',        *        
               KEY,AREC,(0,DMWORK)                                              
         MVC   ACTION,=C'ADD'                                                   
         AP    APAYRCNT,=P'1'                                                   
         J     PROC_96                                                          
         DROP  R2                                                               
*                                                                               
PROC_96  LARL  R3,AIO1                                                          
         USING PAPYREC,R3                                                       
*                                                                               
         MVC   P(3),ACTION                                                      
         MVC   P+4(L'PAPYKAGY),PAPYKAGY                                         
         MVC   P+8(L'PAPYKMED),PAPYKMED                                         
         MVC   P+10(L'PAPYKCLT),PAPYKCLT                                        
*                                                                               
         MVC   HALF,PAPYKDAT                                                    
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(8,P+25)                                    
*                                                                               
         EDIT  PAPYKSN,(10,P+35),0,ALIGN=LEFT                                   
         GOTO1 HEXOUT,DMCB,PAPYKEY,P+50,25,=C'TOG'                              
*                                                                               
         LR    RF,R3                                                            
         AHI   RF,33                                                            
         USING PAP1D,RF                                                         
*                                                                               
         MVC   P+14(3),PAP1PRD                                                  
         EDIT  PAP1EST,(3,P+18),0,ALIGN=LEFT                                    
*                                                                               
         GOTO1 REPORT                                                           
         J     PROC_10                                                          
         DROP  R3,RF                                                            
*                                                                               
* Autopay records have been added to the file.  Now read them and               
* build worker files for each agency.                                           
*                                                                               
PROC_100 CLI   QOPT4,Q4APYRQ       Generate autopay records only?               
         JE    PROCX                                                            
         BRAS  RE,BLDWRKR          Build worker files from Autopay recs         
*                                                                               
PROCX    CLOSE RECVIN                                                           
         J     EXIT                                                             
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=6000,             X        
               MACRF=GM,EODAD=PROC_100                                          
*                                                                               
*                                                                               
RUNL     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P+01(COUNTSQ-8),8(R4)                                            
         OI    7(R4),X'0F'                                                      
         UNPK  P+26(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,COUNTSQ(R4)                                                   
         B     RUNL50                                                           
*                                                                               
RUNLX    J     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
NEXTEL   LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         JNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
COUNTS   DS    0C                                                               
BUYAPCNT DS    PL8                                                              
         DC    CL25'Buys okay for autopay   '                                   
COUNTSQ  EQU   *-BUYAPCNT                                                       
APAYRCNT DS    PL8                                                              
         DC    CL25'Autopay records created '                                   
         DC    X'FF'                                                            
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* TEST IF BUY IS OK TO PAY                                                      
*                                                                               
TSTOK    NTR1  BASE=*,LABEL=*                                                   
         MVI   AUTOPYSW,0                                                       
*                                                                               
         LARL  R2,REC                                                           
         AHI   R2,33                                                            
TST_15   CLI   0(R2),0             End of record?                               
         JE    TST_80                                                           
         CLI   0(R2),X'20'         First buy element?                           
         JE    TST_30                                                           
         CLI   0(R2),X'99'         Serial# element?                             
         JE    TST_40                                                           
         CLI   0(R2),X'80'         Special rep element?                         
         JE    TST_50                                                           
         CLI   0(R2),X'25'         Pay element?                                 
         JE    TST_60                                                           
         CLI   0(R2),X'CC'         Custom column element?                       
         JE    TST_70                                                           
*                                                                               
TST_20   ZIC   RF,1(R2)            Bump to next element                         
         AR    R2,RF                                                            
         J     TST_15                                                           
*                                                                               
         USING PBDELEM,R2                                                       
TST_30   OI    AUTOPYSW,APYX20#Q                                                
         CLI   PBDCOSIN,C' '       Net?                                         
         JNE   *+8                                                              
         OI    SVSTAT,PAP1SNET                                                  
         CLI   PBDCOSIN,C'S'       Gross = Net?                                 
         JNE   *+8                                                              
         OI    SVSTAT,PAP1SGEN                                                  
         CLI   PBDCOSIN,C'C'       Gross = Agycom?                              
         JNE   *+8                                                              
         OI    SVSTAT,PAP1SGEA                                                  
         ZAP   SVRATEP,PBDCOS(5)   Save rate                                    
*                                                                               
         BRAS  RE,PROCRATE         Process rate                                 
         J     TST_20                                                           
*                                                                               
         USING PSERELEM,R2                                                      
TST_40   OI    AUTOPYSW,APYX99#Q                                                
         MVC   SVSERNUM,PSERNUM    Save serial#                                 
         J     TST_20                                                           
*                                                                               
         USING PBSREPEL,R2                                                      
TST_50   MVI   SVPAYE,C'S'                                                      
         MVC   SVPAYE+1(L'PBSREP),PBSREP      Save special rep                  
         J     TST_20                                                           
*                                                                               
         USING PPAYELEM,R2                                                      
TST_60   OC    PPDDATE,PPDDATE                                                  
         JNZ   *+8                                                              
         OI    AUTOPYSW,APYBNP#Q                                                
         J     TST_20              Already paid, no need to autopay             
*                                                                               
         USING BYCCELD,R2                                                       
TST_70   CLC   BYCCSQN,=AL2(8213)  Custom column - invoice number?              
         JNE   TST_72                                                           
         OI    AUTOPYSW,APYINV#Q   Yes, found invoice number                    
*                                                                               
         ZIC   R1,BYCCLEN                                                       
         SHI   R1,BYCCHDRL                                                      
         CHI   R1,L'SVINV#                                                      
         JNH   *+8                                                              
         LHI   R1,L'SVINV#-1       Default to max invoice# length               
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVINV#(0),BYCCDATA  Save invoice #                               
         OC    SVINV#,SPACES                                                    
         J     TST_20                                                           
*                                                                               
TST_72   CLC   BYCCSQN,=AL2(8214)  Custom column - invoice amount?              
         JNE   TST_74                                                           
         OI    AUTOPYSW,APYINV$Q                                                
*                                                                               
         ZIC   R1,BYCCLEN                                                       
         SHI   R1,BYCCHDRL                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         ZAP   SVINV$P,BYCCDATA(0) Save invoice $                               
         EDIT  SVINV$P,SVINV$,2,ALIGN=LEFT                                      
         J     TST_20                                                           
*                                                                               
TST_74   CLC   BYCCSQN,=AL2(8208)  Custom column - OK to pay?                   
         JE    *+14                                                             
         CLC   BYCCSQN,=AL2(8209)                                               
         JNE   TST_20                                                           
         CLI   BYCCDATA,C'Y'                                                    
         JNE   TST_20                                                           
         OI    AUTOPYSW,APYOKP#Q                                                
         J     TST_20                                                           
         DROP  R2                                                               
*                                                                               
TST_80   CLI   AUTOPYSW,APYYES#Q   Eligible for autopay?                        
         JNE   TSTOKN                                                           
*                                                                               
TSTOKY   J     YES                                                              
TSTOKN   J     NO                                                               
TSTOKX   J     EXIT                                                             
*                                                                               
*                                                                               
* Find entry in AGYTABLE                                                        
*                                                                               
         USING PBUYREC,R2                                                       
         USING AGYTABD,R4                                                       
CHKAGY   NTR1  BASE=*,LABEL=*                                                   
         ICM   R5,15,0(R1)         A(AGYENTRY)                                  
         XC    0(L'AGYENTRY,R5),0(R5)                                           
         LA    R4,AGYTABLE                                                      
                                                                                
         CLI   QOPT1,Q1TSTQ        Option to generate on TST                    
         BNE   *+8                                                              
         LA    R4,AGYTABL2                                                      
         CLI   QOPT1,Q1FQAQ        Option to generate on FQA                    
         BNE   *+8                                                              
         LA    R4,AGYTABL2                                                      
                                                                                
CA10     CLI   0(R4),X'FF'                                                      
         JE    NO                  Not in table                                 
*                                                                               
         CLC   QOPT6(2),SPACES     Filter on agency?                            
         JZ    *+14                No - process every agency                    
         CLC   ATALPHA,QOPT6       Same agency?                                 
         JNE   *+14                No - skip it                                 
*                                                                               
         CLC   PBUYKAGY,ATALPHA    match on agy alpha                           
         JE    *+12                                                             
         AHI   R4,ATLENQ                                                        
         J     CA10                                                             
         MVC   0(L'AGYENTRY,R5),0(R4)      Save entry                           
         J     YES                                                              
         DROP  R2,R4                                                            
*                                                                               
* LIVE TABLE                                                                    
*                                                                               
AGYTABLE DC    C'SJ',CL16'SJB,DDS    ',XL2'0AB7',C'1'                           
         DC    C'*B',CL16'DDSB,DDS   ',XL2'0A4B',C'T'                           
         DC    C'H7',CL16'MSNYA,DDS  ',XL2'2192',C'2'                           
         DC    C'FR',CL16'FDMJW,DDS  ',XL2'1974',C'2'                           
         DC    X'FF'                                                            
*                                                                               
* TST/FQA/CSC TABLE                                                             
*                                                                               
AGYTABL2 DC    C'SJ',CL16'SJR,DDS    ',XL2'0011',C'T'                           
         DC    C'*B',CL16'DDSB,DDS   ',XL2'0A4B',C'T'                           
         DC    C'H7',CL16'MSNYA,DDS  ',XL2'2192',C'2'                           
         DC    C'FR',CL16'FDMJW,DDS  ',XL2'1974',C'2'                           
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
* Get client record                                                             
*                                                                               
         USING PBUYREC,R2                                                       
GETCLT   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVORIKEY,KEY        Save original AIO values                     
         MVC   SVORIKSV,KEYSAVE                                                 
         MVC   SVORIAIO,AREC                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+0(2),PBUYKAGY                                                
         MVC   KEY+2(1),PBUYKMED                                                
         MVI   KEY+3,X'02'         Client record code                           
         MVC   KEY+4(3),PBUYKCLT                                                
         CLC   SVCLTKEY,KEY        Already looked up this client?               
         JE    G_CLT80                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     Found client?                                
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCLTKEY,KEY        Save client key for next time                
         LARL  RF,AIO2             A(Temporary I/O)                             
         ST    RF,AREC             Set AREC for autopay getrec/putrec           
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC  '),=C'PRTFILE',        +        
               KEY+27,AREC,(0,DMWORK)                                           
         L     RF,AREC                                                          
         USING PCLTREC,RF                                                       
         MVC   SVCOFFCD,PCLTOFF    Save client office code                      
         DROP  RF                                                               
*                                                                               
G_CLT80  MVC   KEY,SVORIKEY        Restore original AIO values                  
         MVC   KEYSAVE,SVORIKSV                                                 
         MVC   AREC,SVORIAIO                                                    
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
* Autopay records have been added to the file.  Now read them and               
* build worker files for each agency.                                           
*                                                                               
BLDWRKR  NTR1  BASE=*,LABEL=*                                                   
         NI    DMINBTS,X'FF'-X'08'   Don't read deleted records                 
         LARL  R3,AGYTABLE                                                      
         USING AGYTABD,R3                                                       
*                                                                               
BW_10    CLI   0(R3),X'FF'         End of table?                                
         JE    BLDWRKRX                                                         
*                                                                               
         MVI   WRKRSTAT,0                                                       
*                                                                               
         LA    R2,KEY                                                           
         USING PAPYREC,R2                                                       
         XC    KEY,KEY                                                          
         MVC   PAPYKAGY,ATALPHA                                                 
         GOTO1 HIGH                                                             
         J     BW_12                                                            
BW_SEQ   GOTO1 SEQ                                                              
BW_12    LA    R2,KEY                                                           
         CLC   PAPYKAGY,ATALPHA    Same agency?                                 
         JNE   BW_100              No - get next agency in AGYTABLE             
         CLI   PAPYKTYP,PAPYKRCD   Autopay record?                              
         JNE   BW_SEQ                                                           
         TM    KEY+25,PAPYPRCQ     Already processed?                           
         JO    BW_SEQ                                                           
*                                                                               
         GOTOR CHKAGY,DMCB,AGYENTRY   Get agency                                
         JNE   BW_100                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC  '),=C'PRTFILE',        *        
               KEY+27,AREC,(0,DMWORK)                                           
*                                                                               
         L     R2,AREC                                                          
*                                                                               
         MVC   WFUSED,SPACES                                                    
         MVC   WFMSG,SPACES                                                     
         MVC   WFMED,SPACES                                                     
         MVC   WFCLT,SPACES                                                     
         MVC   WFPRDEST,SPACES                                                  
         MVC   WFPER,SPACES                                                     
         MVC   WFPUB,SPACES                                                     
         MVC   WFPAYEE,SPACES                                                   
         MVC   WFINVD,SPACES                                                    
         MVC   WFINV#,SPACES                                                    
         MVC   WFINV$,SPACES                                                    
         MVC   WFSERNUM,SPACES                                                  
*                                                                               
         MVC   WFMED,PAPYKMED      Media                                        
         MVC   WFCLT,PAPYKCLT      Client                                       
         EDIT  PAPYKSN,WFSERNUM,0,ALIGN=LEFT      Serial #                      
*                                                                               
         AHI   R2,33                                                            
         USING PAP1D,R2                                                         
*                                                                               
         MVC   WFPRDEST(L'PAP1PRD),PAP1PRD        Product                       
         LA    R4,WFPRDEST+2                                                    
         CLI   PAP1PRD+2,C' '                                                   
         JE    *+8                                                              
         AHI   R4,1                                                             
         MVI   0(R4),C'/'                                                       
         AHI   R4,1                                                             
         EDIT  PAP1EST,(3,0(R4)),0,ALIGN=LEFT         Estimate                  
         GOTO1 DATCON,DMCB,(3,PAP1DATE),(8,WFPER)                               
*                                                                               
         MVI   WFPER+8,C'-'        Default to ref 1                             
         MVI   WFPER+9,C'1'                                                     
         CLI   PAP1REF,1                                                        
         JE    BW_20                                                            
         EDIT  PAP1REF,(3,WFPER+9),0,ALIGN=LEFT                                 
*                                                                               
         ZIC   R0,PAP1REF                                                       
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    BW_14                                                            
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  WFPER+10(1),DUB+7(1)                                             
         OI    WFPER+10,X'F0'                                                   
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   WFPER+9(1),0(RF)                                                 
         MVI   WFPER+11,C' '                                                    
         B     BW_20               DONE                                         
*                                                                               
BW_14    OI    DUB+7,X'0F'                                                      
         UNPK  WFPER+9(2),DUB                                                   
         CLI   WFPER+9,C'0'                                                     
         BNE   *+10                                                             
         MVC   WFPER+9(2),WFPER+10                                              
*                                                                               
BW_20    GOTO1 DATCON,DMCB,(3,PAP1INVD),(8,WFINVD)                              
         MVC   WFINV#,PAP1INV#     Invoice number                               
         MVC   WFINV$,PAP1INV$     Invoice $                                    
         MVC   WFPAYEE,PAP1PAYE    Special rep                                  
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(8,PAP1PUB),(C'S',WFPUB)                        
*                                                                               
         TM    WRKRSTAT,WSTATOPN   Worker file already open?                    
         JO    *+8                 No                                           
         BAS   RE,OPENWRKR         Open/Get PAP wrkr file                       
*                                                                               
         LARL  RF,IO2L                                                          
         MVC   0(2,RF),=H'255'                                                  
*                                                                               
         LARL  RF,IO2                                                           
         XC    0(255,RF),0(RF)     Add autopay to worker file                   
         MVC   10(WFVALSQ,RF),WFVALS                                            
         BAS   RE,WRKR                                                          
*                                                                               
         J     BW_SEQ                                                           
*                                                                               
BW_100   BAS   RE,CLOSWRKR         Close wrkr file                              
         AHI   R3,ATLENQ           Get next agency in AGYTABLE                  
         J     BW_10                                                            
*                                                                               
BLDWRKRX J     EXIT                                                             
         DROP  R2,R3                                                            
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
         LTORG                                                                  
*                                                                               
* Open/Get worker file                                                          
*                                                                               
OPENWRKR NTR1                                                                   
         LARL  R0,WKBUFF           Clear worker file buffer                     
         LARL  R1,WKBUFFX                                                       
         SR    R1,R0               Length to clear                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,AGYENTRY         Agency table entry                           
         USING AGYTABD,R6                                                       
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LARL  R4,IO2                                                           
         XC    0(255,R4),0(R4)     Build worker file header                     
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,ATIDNUM     Agency user id number                        
         MVC   WLSYSPRG(3),=C'PAP'                                              
         MVC   WLSUBPRG,ATADV      Set facpak                                   
*                                                                               
         CLI   QOPT1,C'T'          Option to generate on TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       Set factst                                   
         CLI   QOPT1,C'C'          Option to generate on CSC                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'S'       Set faccsc                                   
         CLI   QOPT1,C'Q'          Option to generate on FQA                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'Q'       Set facfqa                                   
*                                                                               
         MVC   WLDAY,DATEC1+2                                                   
         MVI   WLCLASS,C'T'        Class T for worker file scripts              
         MVI   WLTYPE,C'A'         Type A for immediate execution               
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
*&&DO                                                                           
* Scripts do not run on Sunday mornings so retain Saturday worker               
* file 48 hours for Monday morning processing.                                  
*                                                                               
         TM    FLAGS,FLRWFQ        Retain worker file?                          
         JZ    *+10                                                             
         MVC   WLRETNL,=X'0030'    Yes - retain for 60 hours                    
*&&                                                                             
         LARL  R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,ATIDNUM     Agency id number                             
         MVC   UKSYSPRG(3),=C'PAP'                                              
         MVC   UKFILENO,WLREPRNO   Worker file number                           
         DROP  R5                                                               
*                                                                               
         MVI   FIXED,C'N'                                                       
*                                                                               
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
*                                                                               
         LARL  R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'PPAUTPAY'                                            
         MVI   18(R1),C'I'           Set type to insert                         
         MVC   30(5,R1),=C'00006'    Header length                              
         MVC   35(5,R1),=C'00100'    Input data length                          
         MVC   40(5,R1),=C'00020'    Output data length                         
         MVI   45(R1),C'Y'           Insert errors at file end                  
*                                                                               
         LARL  RF,IO2L                                                          
         MVC   0(2,RF),=H'76'      Bytes for QSAM                               
         BAS   RE,WRKR                                                          
*                                                                               
         LARL  R1,IO2                                                           
         XC    0(256,R1),0(R1)     Sign-on information                          
         MVC   10(90,R1),SPACES                                                 
         MVC   30(8,R1),=C'NEWSTYLE'                                            
         MVC   46(16,R1),ATSIGNON  Agency/Pid                                   
         MVC   90(3,R1),=C'DDS'    Password                                     
*                                                                               
         LARL  RF,IO2L                                                          
         MVC   0(2,RF),=H'100'     Bytes for QSAM                               
         BAS   RE,WRKR                                                          
*                                                                               
         OI    WRKRSTAT,WSTATOPN   File open                                    
         J     EXIT                                                             
*                                                                               
* Add line to worker file                                                       
*                                                                               
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         JZ    WRKR10                                                           
         LARL  R3,IO2                                                           
         MVC   0(4,R3),=F'2102'                                                 
         EDIT  SEQNUM,(6,4(R3)),0,FILL=0                                        
*                                                                               
WRKR10   LARL  R5,WKBUFF                                                        
         LARL  R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         JE    *+10                                                             
         LARL  R3,IO2L                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),(R5)                         
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM           Bump line number                             
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         LARL  RF,IO2                                                           
         MVC   P,0(RF)                                                          
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
* Close worker file                                                             
*                                                                               
CLOSWRKR NTR1                                                                   
         TM    WRKRSTAT,WSTATOPN     Worker file already open?                  
         JZ    EXIT                                                             
         USING WLHDRD,R4                                                        
         LARL  R4,IO2                                                           
         XC    0(255,R4),0(R4)       Build header                               
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         NI    WRKRSTAT,X'FF'-WSTATOPN                                          
         J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKFI  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
*                                                                               
WFVALS   DS    0X                                                               
WFUSED   DS    CL20                Used by script                               
WFMSG    DS    CL60                Message errors/success                       
WFMED    DS    CL1                 Media                                        
WFCLT    DS    CL3                 Client                                       
WFPRDEST DS    CL7                 Product/Estimate                             
WFPER    DS    CL17                Period (date-reference)                      
WFPUB    DS    CL15                Publication                                  
WFPAYEE  DS    CL5                 Payee                                        
WFINVD   DS    CL8                 Invoice date                                 
WFINV#   DS    CL12                Invoice number                               
WFINV$   DS    CL12                Invoice $                                    
WFSERNUM DS    CL10                Serial #                                     
WFVALSQ  EQU   *-WFVALS                                                         
*                                                                               
SVWLKEY  DS    CL8                 Worker file key                              
SVUKKEY  DS    XL56                                                             
AWRKNTRY DS    A                                                                
SEQNUM   DS    F                                                                
FIXED    DS    CL1                                                              
WRKRSTAT DS    XL1                                                              
WSTATOPN EQU   X'80'               Worker file is open                          
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
*                                                                               
         DS    F                                                                
         DS    0D                                                               
         DC    C'*RCVREC*'                                                      
RCVREC   DC    F'0'                VAR REC LEN                                  
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL32                                                            
REC      DS    4000C                                                            
AIO1     DS    XL4000                                                           
AIO2     DS    XL4000                                                           
IO2L     DS    F                                                                
IO2      DS    XL600               IO AREA                                      
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
*                                                                               
* Process rate                                                                  
*                                                                               
         USING PBDELEM,R2                                                       
PROCRATE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* Format rate/cost (same as that of BUY display)                                
*                                                                               
F        USING F_COST_D,WKTEMP1                                                 
         XC    F.F_COST_D(FC_BLKLQ),F.F_COST_D                                  
         MVC   F.FC_TMPWK,SPACES                                                
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_RTIND,PBDRLIND                                              
         MVC   F.FC_ACPCT,PBDACP                                                
         ZAP   F.FC_COST_,PBDCOS                                                
         MVC   F.FC_NETSW,PBDCTYP                                               
         MVC   F.FC_COTYP,PBDCOSTY                                              
         MVC   F.FC_COIND,PBDCOSIN                                              
         LARL  RE,REC                                                           
         CLI   2(RE),C'N'          Newspaper?                                   
         JNE   F_COS40                                                          
         ZAP   DUB,F.FC_COST_      Format cost for newspaper                    
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     entered as net?                              
         JNE   F_COS32                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = net percentage                             
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS32  CLI   F.FC_COTYP,C'U'     Unit rate?                                   
         BE    F_COS37                                                          
         C     R1,=F'99999999'     Total rate over 999,999.99?                  
         BNH   F_COS34                                                          
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          Have entered pennies when buying             
         LTR   R1,R1               (No room)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
F_COS33  EDITR (R1),(9,F.FC_TMPWK+5),0,FLOAT=-,ALIGN=LEFT                       
         J     F_COS38                                                          
*                                                                               
F_COS34  CHI   R1,0                Negative rate?                               
         BNL   F_COS36                                                          
         C     R1,=F'-99999999'    Total rate less than 99,999,999?             
         BH    F_COS35                                                          
         MHI   R1,-1               Drop pennies and dimes                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MHI   R1,-1                                                            
         B     F_COS33                                                          
*                                                                               
F_COS35  C     R1,=F'-999999'      Total rate less than 9,999.99?               
         BNL   F_COS36                                                          
         MHI   R1,-1               Drop pennies, leave dimes                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDITR (R1),(9,F.FC_TMPWK+5),1,FLOAT=-,ALIGN=LEFT                       
         B     F_COS38                                                          
*                                                                               
F_COS36  EDITR (R1),(9,F.FC_TMPWK+5),2,FLOAT=-,ALIGN=LEFT                       
         B     F_COS38                                                          
*                                                                               
F_COS37  EDITR (R1),(11,F.FC_TMPWK+5),5,FLOAT=-,ALIGN=LEFT                      
*                                                                               
         LA    R1,F.FC_TMPWK+5-3   Start of output                              
         AR    R1,R0               + length                                     
         SHI   R1,3                Back up to last 3 bytes                      
         CLC   =C'000',0(R1)                                                    
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES      Move some blanks                             
*                                                                               
F_COS38  LA    R1,F.FC_TMPWK+5                                                  
         CLI   F.FC_COTYP,C'U'     Unit rate?                                   
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COTYP                                               
         CLI   F.FC_COIND,C' '     Default cost type?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     Entered as net?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    Frozen rate?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         B     F_COS_X                                                          
*                                                                               
F_COS40  ZAP   DUB,F.FC_COST_      Format cost for non-newspaper                
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     Net input so display as net                  
         BNE   F_COS42                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =Net percentage                              
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS42  EDITR (R1),(10,F.FC_TMPWK+2),2,ALIGN=LEFT,FLOAT=-                      
         LA    R1,F.FC_TMPWK+2                                                  
         CLI   F.FC_COIND,C' '     Default cost type?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     Entered as net?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    Frozen rate?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST(11),SPACES                                            
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
*                                                                               
F_COS_X  MVC   SVRATE,F.FC_OCOST  Save rate output                              
         J     YES                                                              
         DROP  F                                                                
*                                                                               
WKTEMP1  DS    CL80                                                             
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPIPWRKD DSECT                                                                  
DATEC10  DS    CL10                                                             
DATEC2   DS    XL2                                                              
DATEC1   DS    XL3                                                              
DATEC0   DS    CL6                                                              
ACTION   DS    CL3                                                              
AGYENTRY DS    XL(ATLENQ)          Agency table entry                           
*                                                                               
SVVALS   DS    0X                                                               
SVSTAT   DS    XL1                 Status                                       
SVSERNUM DS    PL5                 Serial #                                     
SVINV#   DS    CL12                Invoice #                                    
SVINV$   DS    CL12                Invoice $                                    
SVINV$P  DS    PL8                 Invoice $ (packed)                           
SVRATE   DS    CL12                Rate                                         
SVRATEP  DS    PL8                 Rate (packed)                                
SVPAYE   DS    CL5                 Special rep                                  
SVVALSQ  EQU   *-SVVALS                                                         
*                                                                               
FLAGS    DS    XL1                                                              
FLRWFQ   EQU   X'80'               Retain worker file                           
*                                                                               
ELCODE   DS    X                                                                
A0BPROF  DS    CL16                                                             
*                                                                               
COPYFLAG DS    X                   FLAG FOR COPY RECORD                         
*                                                                               
AUTOPYSW DS    X                                                                
APYX20#Q EQU   X'80'               Has buy element                              
APYX99#Q EQU   X'40'               Has serial #                                 
APYBNP#Q EQU   X'20'               Buy not paid                                 
APYOKP#Q EQU   X'10'               Has OK to pay                                
APYINV#Q EQU   X'08'               Have invoice number                          
APYINV$Q EQU   X'04'               Have invoice amount                          
APYYES#Q EQU   APYX20#Q+APYX99#Q+APYBNP#Q+APYOKP#Q+APYINV#Q+APYINV$Q            
*                                                                               
SVCLTKEY DS    XL(L'PCLTKEY)       Save client record key                       
SVCOFFCD DS    XL(L'PCLTOFF)       Save client office code                      
SVORIKEY DS    XL(L'KEY)           Save original key                            
SVORIKSV DS    XL(L'KEYSAVE)       Save original keysave                        
SVORIAIO DS    A                   Save original AIO pointer                    
*                                                                               
Q1CSCQ   EQU   C'C'                CSC System                                   
Q1FQAQ   EQU   C'Q'                FQA System                                   
Q1TSTQ   EQU   C'T'                TST System                                   
Q3DATEQ  EQU   C'D'                Use date from request for run date           
Q4APYRQ  EQU   C'A'                Generate autopay records only                
Q4WRKRQ  EQU   C'W'                Generate worker file only                    
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE PSERELEM                                                       
       ++INCLUDE PPGENAPY                                                       
       ++INCLUDE PPGENBYCC                                                      
       ++INCLUDE PBSREPEL                                                       
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
*                                                                               
*        Agency Table DSECT                                                     
*                                                                               
AGYTABD  DSECT                                                                  
ATALPHA  DS    CL2                 Agency alpha id                              
ATSIGNON DS    CL16                Agency signon id,Pid                         
ATIDNUM  DS    XL2                 Agency user id number                        
ATADV    DS    CL1                 Agency's adv                                 
ATLENQ   EQU   *-AGYTABD                                                        
*                                                                               
*        Format cost/rate DSECT                                                 
*                                                                               
F_COST_D DSECT                     FOR FORMATTING COST                          
FC_COST_ DS    PL8                 COST                                         
FC_NETSW DS    CL(L'PBDCTYP)       NET COST SWITCH (ENTERED AS NET)             
FC_COTYP DS    CL(L'PBDCOSTY)      COST TYPE (U=UNIT COST)                      
FC_COIND DS    CL(L'PBDCOSIN)      COST INDICATOR                               
FC_RTIND DS    XL(L'PBDRLIND)      RATE LOOK-UP INDICATOR                       
FC_ACPCT DS    PL(L'PBDACP)        AGENCY COMMISSION PERCENT (3 DEC)            
FC_OCOST DS    CL20                FORMATTED COST (OUTPUT)                      
FC_TMPWK DS    CL20                TEMPORARY WORK AREA                          
FC_BLKLQ EQU   *-F_COST_D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREPIP02 05/13/20'                                      
         END                                                                    
