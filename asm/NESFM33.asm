*          DATA SET NESFM33    AT LEVEL 021 AS OF 05/19/11                      
*PHASE T31C33C                                                                  
         TITLE 'NESFM33 - NETWORK CLEARANCE STATUS LIST        '                
T31C33   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CLRST*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         OI    GLSTSTAT,NOSELFLD                                                
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1C'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VMSUNPK,DMCB         A(MSUNPK) to display network                
*                                                                               
         CLI   MODE,VALKEY                                                      
         JE    VK                                                               
         CLI   MODE,LISTRECS                                                    
         JE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
*********************************************************************           
* VALIDATE KEY                                                                  
*********************************************************************           
VK       XC    FLTVALS(FLTVALLN),FLTVALS                                        
         LA    R2,LSTMEDH          Media (BAGYMD)                               
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,LSTCLIH          Client (QCLT/BCLT)                           
         GOTO1 VALICLT                                                          
         MVC   FLTCLI,BCLT                                                      
*                                                                               
         LA    R2,LSTPAYH          Payee                                        
         CLI   5(R2),0                                                          
         JE    VK10                                                             
         CLC   =C'DIR',LSTPAY                                                   
         JNE   ERRINV                                                           
         MVC   FLTPAY,LSTPAY                                                    
*                                                                               
VK10     LA    R2,LSTNETH          Network (BMKT/BSTA)                          
         GOTO1 VALINTWK                                                         
         MVC   FLTMKT,BMKT                                                      
         MVC   FLTNET,BSTA                                                      
*                                                                               
         LA    R2,LSTPERH          Period                                       
         CLI   5(R2),0                                                          
         JNE   VK20                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,TEMP)                                       
         MVC   LSTPER(3),TEMP                                                   
         MVI   LSTPERH+5,3                                                      
*                                                                               
         GOTO1 PERVAL,DMCB,(LSTPERH+5,LSTPER),WORK                              
         LA    R5,WORK                                                          
         USING PERVALD,R5          Default is prior mon/yr to current           
         MVC   FLTSTD,PVALCSTA                                                  
         MVC   FLTEND,PVALCEND                                                  
         ZIC   RF,FLTSTD           Set start of period to prior year            
         SHI   RF,2                                                             
         STC   RF,FLTSTD                                                        
         J     VK30                                                             
         DROP  R5                                                               
*                                                                               
VK20     MVI   ERROR,INVDATE                                                    
         GOTO1 PERVAL,DMCB,(LSTPERH+5,LSTPER),WORK                              
         CLI   DMCB+4,PVRCINV1      Valid period?                               
         JE    ERRX                                                             
         CLI   DMCB+4,PVRCINV2                                                  
         JE    ERRX                                                             
*                                                                               
         LA    R5,WORK                                                          
         USING PERVALD,R5                                                       
         MVC   FLTSTD,PVALCSTA                                                  
         MVC   FLTEND,PVALCEND                                                  
         DROP  R5                                                               
*                                                                               
VK30     MVC   LSTPER,SPACES                                                    
         GOTO1 DATCON,DMCB,(2,FLTSTD),(8,LSTPER)                                
         MVI   LSTPER+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(2,FLTEND),(8,LSTPER+9)                              
         OI    LSTPERH+6,X'80'                                                  
*                                                                               
         LA    R2,LSTOPTH          Options                                      
         CLI   5(R2),0                                                          
         JE    VK40                                                             
         GOTOR VALOPT                                                           
*                                                                               
VK40     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLRSTATD,R4                                                      
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,BAGYMD                                                  
         MVC   CLSKCLT,FLTCLI                                                   
         MVC   CLSKMKT,FLTMKT                                                   
         MVC   CLSKSTA,FLTNET                                                   
         MVC   FLTKEY,KEY                                                       
         DROP  R4                                                               
*                                                                               
VKX      GOTO1 VSETSPT                                                          
         XC    PREVKEY,PREVKEY                                                  
         J     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* LIST THE CLRST RECORDS                                                        
*********************************************************************           
LR       MVI   NLISTS,13                                                        
         LA    R2,LSTLST1H                                                      
         ST    R2,ATHISLST                                                      
*                                                                               
         GOTOR CHKKEY              Test if key fields changed                   
*                                                                               
         MVC   LSTLHL2+20(7),=C'Chkdate'                                        
         OI    LSTLHL2H+6,X'80'                                                 
         TM    FLTFLG,FLTCLR        Return check clear date?                    
         JZ    *+10                                                             
         MVC   LSTLHL2+20(7),=C'Clrdate'                                        
*                                                                               
         CLI   KEY,0                First time through?                         
         JE    *+14                                                             
         MVC   PREVKEY,KEY                                                      
         J     LR10                                                             
*                                                                               
         MVC   PREVKEY,FLTKEY                                                   
         XC    CURRELEM,CURRELEM                                                
*                                                                               
LR10     OC    CURRELEM,CURRELEM    Test middle of record                       
         JZ    *+10                                                             
         MVC   PREVKEY,CLRSTKEY     Reset clrst key                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CLSKEY),PREVKEY                                            
         GOTO1 HIGH                                                             
         J     LR20                                                             
*                                                                               
LRSEQ    XC    CURRELEM,CURRELEM                                                
         GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(CLSKCLT-CLSKEY),KEYSAVE                                      
         JNE   LRX                                                              
         MVC   PREVKEY,KEY                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING CLRSTATD,R6                                                      
*                                                                               
         OC    FLTCLI,FLTCLI        Client filter?                              
         JZ    *+12                                                             
         CLC   CLSKCLT,FLTCLI                                                   
         JNE   LRSEQ                                                            
*                                                                               
         OC    FLTMKT,FLTMKT        Market filter?                              
         JZ    *+12                                                             
         CLC   CLSKMKT,FLTMKT                                                   
         JNE   LRSEQ                                                            
*                                                                               
         OC    FLTNET,FLTNET        Network filter?                             
         JZ    *+12                                                             
         CLC   CLSKSTA,FLTNET                                                   
         JNE   LRSEQ                                                            
*                                                                               
         MVC   CLRSTKEY,KEY         Save clrst key                              
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         OC    CURRELEM,CURRELEM    Test middle of record                       
         JZ    LR22                                                             
         L     R6,CURRELEM          Start at next element                       
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         ST    R6,CURRELEM                                                      
         J     LR30                                                             
*                                                                               
LR22     GOTOR PRNTHDR              Print header line                           
*                                                                               
         MVC   KEY,CLRSTKEY         Restore clrst sequence                      
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,CLSELEMS                                                      
         NI    FLAG,X'FF'-VAL01                                                 
         DROP  R6                                                               
*                                                                               
LR30     CLI   0(R6),0              End of record?                              
         JE    LRSEQ                                                            
         CLI   0(R6),CLS01ELQ       Test clearance element                      
         JE    LR40                                                             
         TM    FLAG,VAL01           Test if found valid clearance elem          
         JZ    LR100                                                            
         TM    FLTFLG,FLTOLD        Test to return header info only             
         JO    LR100                                                            
*                                                                               
         CLI   0(R6),CLS03ELQ       Test invoice number element                 
         JE    LR50                                                             
         CLI   0(R6),CLS05ELQ       Test invoice detail element                 
         JE    LR60                                                             
         J     LR100                                                            
*                                                                               
         USING CLSTEL01,R6                                                      
LR40     NI    FLAG,X'FF'-VAL01                                                 
         CLC   CLSTCLRD,FLTSTD      Test within period                          
         JL    LR100                                                            
         CLC   CLSTCLRD,FLTEND                                                  
         JH    LR100                                                            
*                                                                               
         CLI   SVMOSF,0            MOS FILTER?                                  
         BE    LR45                                                             
         GOTO1 DATCON,DMCB,(2,CLSTSTDT),(6,DUB)                                 
         OC    DMCB,DMCB                                                        
         BZ    ERRSYN                                                           
         CLC   SVMOSF,DUB                                                       
         JNE   LR100                                                            
*                                                                               
LR45     OI    FLAG,VAL01                                                       
*                                                                               
         ST    R6,CURRELEM                                                      
         GOTOR PRNTCLR              Print clearance                             
         J     LR100                                                            
*                                                                               
         USING CLSTEL03,R6                                                      
LR50     MVC   SVINV,CLS3INV                                                    
         ST    R6,CURRELEM                                                      
         J     LR100                                                            
*                                                                               
         USING CLSTEL05,R6                                                      
LR60     ST    R6,CURRELEM                                                      
         GOTOR PRNTDET              Print clearance details                     
         J     LR100                                                            
*                                                                               
LR100    ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         J     LR30                                                             
*                                                                               
         J     LRSEQ                                                            
*                                                                               
LRX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* CHECK IF KEY FIELDS CHANGED                                                   
*********************************************************************           
CHKKEY   NTR1  LABEL=NO                                                         
         TM    LSTMEDH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTCLIH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTNETH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTPERH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTPAYH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTOPTH+4,X'80'                                                  
         JZ    CHKKEYX                                                          
CHKK10   XC    KEY,KEY                                                          
CHKKEYX  J     EXIT                                                             
*********************************************************************           
* VALIDATE OPTIONS                                                              
*********************************************************************           
VALOPT   NTR1  LABEL=NO                                                         
         LA    R2,LSTOPTH                                                       
         XC    SCANBLK(160),SCANBLK                                             
         GOTO1 SCANNER,DMCB,(R2),SCANBLK                                        
         CLI   DMCB+4,0                                                         
         BE    ERRSYN                                                           
*                                                                               
         XC    SVMOSF,SVMOSF                                                    
         LA    R5,SCANBLK                                                       
         USING SCANBLKD,R5                                                      
         SR    R1,R1                                                            
*                                                                               
VOPT10   CLI   0(R5),0                                                          
         JE    EXIT                                                             
         AHI   R1,1                 BUMP OPTION COUNTER                         
*                                                                               
         ZIC   RE,SC1STLEN                                                      
         SHI   RE,1                                                             
*                                                                               
         EX    RE,TESTCLR                                                       
         JE    VOPT20                                                           
         EX    RE,TESTINV                                                       
         JE    VOPT30                                                           
         EX    RE,TESTNET                                                       
         JE    VOPT40                                                           
         EX    RE,TESTGRS                                                       
         JE    VOPT50                                                           
         EX    RE,TESTOLD                                                       
         JE    VOPT60                                                           
         EX    RE,TESTDA                                                        
         JE    VOPT70                                                           
         EX    RE,TESTPID                                                       
         JE    VOPT80                                                           
         EX    RE,TESTACC                                                       
         JE    VOPT90                                                           
         EX    RE,TESTMOS                                                       
         JE    VOPT100                                                          
         EX    RE,TESTMOSF                                                      
         JE    VOPT110                                                          
*                                                                               
ERROPTS  CHI   R1,1                                                             
         JE    ERROP1                                                           
         CHI   R1,2                                                             
         JE    ERROP2                                                           
         J     ERROP3                                                           
*                                                                               
TESTCLR  CLC   SC1STFLD(0),=C'CLEARED'                                          
TESTINV  CLC   SC1STFLD(0),=C'INV'                                              
TESTNET  CLC   SC1STFLD(0),=C'NET'                                              
TESTGRS  CLC   SC1STFLD(0),=C'GROSS'                                            
TESTOLD  CLC   SC1STFLD(0),=C'OLDEL'                                            
TESTDA   CLC   SC1STFLD(0),=C'DA'                                               
TESTPID  CLC   SC1STFLD(0),=C'PID'                                              
TESTACC  CLC   SC1STFLD(0),=C'ACCAGY'                                           
TESTMOS  CLC   SC1STFLD(0),=C'MOS'                                              
TESTMOSF CLC   SC1STFLD(0),=C'MOSF'                                             
*                                                                               
VOPT20   OI    FLTFLG,FLTCLR                                                    
         J     VOPTNXT                                                          
*                                                                               
VOPT30   CLI   SC2NDLEN,0                                                       
         JE    ERROPTS                                                          
         MVC   FLTINV,SC2NDFLD                                                  
         J     VOPTNXT                                                          
*                                                                               
VOPT40   TM    FLTFLG,FLTGRSD                                                   
         JO    ERROPTS                                                          
         OI    FLTFLG,FLTNETD                                                   
         J     VOPTNXT                                                          
*                                                                               
VOPT50   TM    FLTFLG,FLTNETD                                                   
         JO    ERROPTS                                                          
         OI    FLTFLG,FLTGRSD                                                   
         J     VOPTNXT                                                          
*                                                                               
VOPT60   TM    FLTFLG,FLTOLD                                                    
         JO    ERROPTS                                                          
         OI    FLTFLG,FLTOLD                                                    
         J     VOPTNXT                                                          
*                                                                               
VOPT70   OI    FLTFLG,FLTDA                                                     
         J     VOPTNXT                                                          
*                                                                               
VOPT80   OI    FLTFLG,FLTPID                                                    
         TM    FLTFLG,FLTACC                                                    
         JO    ERRSYN                                                           
         J     VOPTNXT                                                          
*                                                                               
*                                                                               
VOPT90   OI    FLTFLG,FLTACC                                                    
         TM    FLTFLG,FLTPID                                                    
         JO    ERRSYN                                                           
         J     VOPTNXT                                                          
*                                                                               
VOPT100  OI    FLTFLG,FLTMOS                                                    
         TM    FLTFLG,FLTPID+FLTACC                                             
         JNZ   ERRSYN                                                           
         J     VOPTNXT                                                          
*                                                                               
VOPT110  GOTO1 DATVAL,DMCB,(2,SC2NDFLD),(0,DUB)       mosf -> yymmdd            
         OC    DMCB,DMCB                                                        
         BZ    ERRSYN                                                           
         GOTO1 DATCON,DMCB,DUB,(6,SVMOSF)            yymmdd-> mmm/yy            
         J     VOPTNXT                                                          
*                                                                               
VOPTNXT  LA    R5,SCBLKLQ(R5)                                                   
         J     VOPT10                                                           
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* PRINT ROUTINES                                                                
*********************************************************************           
PRNTHDR  NTR1  LABEL=NO                                                         
         L     R6,AIO                                                           
         USING CLRSTATD,R6                                                      
*                                                                               
         MVC   LISTAR,SPACES                                                    
         GOTO1 VMSUNPK,DMCB,CLSKMKT,WORK,WORK+10                                
         MVC   LNET,WORK                                                        
         MVI   LNET+4,C'-'                                                      
*                                                                               
         MVC   SVNET,WORK+10                                                    
*                                                                               
         MVC   LCKNUM(4),=C'D/A:'                                               
         GOTO1 HEXOUT,DMCB,KEY+14,LCKNUM+4,4,0                                  
*                                                                               
PHDR10   XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(L'QMED),QMED                                               
         MVC   KEY+2(L'LNET),LNET                                               
         MVC   KEY+6(L'AGENCY),AGENCY                                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO2                 
         L     RF,AIO2                                                          
         MVC   LNET+5(20),18(RF)    Return network description                  
*                                                                               
         TM    FLTFLG,FLTDA         Display disk address                        
         JZ    PHDRX                                                            
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
PHDRX    J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
PRNTCLR  NTR1  LABEL=NO                                                         
         L     R6,CURRELEM                                                      
         USING CLSTEL01,R6                                                      
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LNET,SVNET                                                       
*                                                                               
         MVC   LREP,=C'DIR '                                                    
         MVC   LREP-1(L'CLSTREPT),CLSTREPT                                      
         CLC   CLSTPYEE,=C'000'     Test rep type                               
         JZ    *+10                                                             
         MVC   LREP(L'CLSTPYEE),CLSTPYEE                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLSTCLRD),(8,LCLRDT)                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLSTSTDT),(6,SVMOS)                               
*                                                                               
PCLR01   EDIT  CLSTCLSQ,(4,WORK)                                                
         OC    WORK(4),=4X'F0'                                                  
         MVC   LSEQ,WORK+1                                                      
         MVC   SVSEQ,LSEQ                                                       
*                                                                               
         CLI   CLSTEST,0            Test cleared by estimate                    
         JE    PCLR02                                                           
         EDIT  CLSTEST,(4,WORK)                                                 
         OC    WORK(4),=4X'F0'                                                  
         MVC   LEST,WORK+1                                                      
         MVC   SVEST,LEST                                                       
*                                                                               
         CLI   CLST01LN,CL01ELN2                                                
         JE    PCLR06                                                           
*                                                                               
PCLR02   LA    RF,SVCLIST           A(Table of products)                        
PCLR03   CLI   0(RF),0                                                          
         JE    PCLR04                                                           
         CLC   CLSTPRD,3(RF)                                                    
         JNE   *+14                                                             
         MVC   LPRD,0(RF)                                                       
         J     PCLR04                                                           
*                                                                               
         LA    RF,4(RF)                                                         
         J     PCLR03                                                           
*                                                                               
PCLR04   CLI   CLSTPRD2,0           Test if piggy back product                  
         JE    PCLR08                                                           
         MVI   LPRD+3,C'/'                                                      
*                                                                               
         LA    RF,SVCLIST           A(Table of products)                        
PCLR05   CLI   0(RF),0                                                          
         JE    PCLR08                                                           
         CLC   CLSTPRD2,3(RF)                                                   
         JNE   *+14                                                             
         MVC   LPRD2,0(RF)                                                      
         J     PCLR08                                                           
*                                                                               
         LA    RF,4(RF)                                                         
         J     PCLR05                                                           
*                                                                               
PCLR06   MVC   LPRD,CLST3PR                                                     
         CLI   CLST3PR2,0           Test piggy back product                     
         JE    PCLR08                                                           
         MVI   LPRD+3,C'/'                                                      
         MVC   LPRD2,CLST3PR2                                                   
*                                                                               
PCLR08   TM    FLTFLG,FLTNETD       Test show net dollars                       
         JZ    *+14                                                             
         MVC   WORK,CLSTNET                                                     
         J     PCLR10                                                           
         TM    FLTFLG,FLTGRSD       Test show gross dollars                     
         JZ    *+14                                                             
         MVC   WORK,CLSTNET                                                     
         J     PCLR10                                                           
*                                                                               
         MVC   WORK,CLSTGRS         Defualt to gross                            
         OC    CLSTNET,CLSTNET                                                  
         JZ    PCLR10                                                           
         MVC   WORK,CLSTNET                                                     
*                                                                               
PCLR10   EDIT  (4,WORK),(11,LAMT),2,MINUS=YES                                   
*                                                                               
         MVC   LCKNUM,CLSTCHK                                                   
         TM    CLSTSTAT,X'80'       Reconciled?                                 
         JZ    *+8                                                              
         MVI   LCKRCN,C'*'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLSTCHDT),(8,LCKDT)                               
         TM    FLTFLG,FLTCLR        Test show check clear date                  
         JZ    PCLR20                                                           
         GOTO1 DATCON,DMCB,(2,CLSTBKDT),(8,LCKDT)                               
*                                                                               
PCLR20   XC    SVPID,SVPID                                                      
         XC    SVACC,SVACC                                                      
         TM    FLTFLG,FLTPID+FLTACC       Need Pid/Acc Agy?                     
         JZ    PCLR30                                                           
         CLI   CLST01LN,CL01ELN3   Test PID elem                                
         JNE   PCLR30                                                           
         MVC   SVACC,CLSTACC       Set ACC agency                               
*                                                                               
         LA    R4,PIDKEY                                                        
         XC    PIDKEY,PIDKEY                                                    
         USING SA0REC,PIDKEY                                                    
         MVI   SA0KTYP,SA0KTYPQ    C'0' - Personal auth. record                 
         MVC   SA0KAGY,AGENCY      Alpha ID                                     
         MVC   SA0KNUM,CLSTPID                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',PIDKEY,AIO3                  
         L     R4,AIO3                                                          
         CLC   PIDKEY(L'SA0KEY),0(R4)                                           
         BNE   PCLR30                                                           
         TM    SA0STAT,X'20'       Test locked                                  
         BO    PCLR30                                                           
*                                                                               
         AHI   R4,28                                                            
PCLR22   CLI   0(R4),0                                                          
         BE    PCLR30                                                           
         CLI   0(R4),X'C3'                                                      
         BE    PCLR24                                                           
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     PCLR22                                                           
         USING SAPALD,R4                                                        
*                                                                               
PCLR24   MVC   SVPID,SAPALPID      Set PID                                      
         DROP  R4                                                               
*                                                                               
PCLR30   GOTO1 LISTMON                                                          
*                                                                               
PCLRX    J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
PRNTDET  NTR1  LABEL=NO                                                         
         L     R6,CURRELEM                                                      
         USING CLSTEL05,R6                                                      
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         TM    FLTFLG,FLTMOS       MOS                                          
         BZ    PDET00                                                           
         MVC   LREP(L'MOSEQU),MOSEQU                                            
         MVC   LREP+4(L'SVMOS),SVMOS                                            
         B     PDET02                                                           
*                                                                               
PDET00   CLI   SVPID,0             TEST ANY PID                                 
         JE    PDET02                                                           
         TM    FLTFLG,FLTPID       Do we want PID                               
         JNO   PDET01                                                           
         MVC   LREP(L'PIDEQU),PIDEQU                                            
         MVC   LREP+4(L'SVPID),SVPID                                            
         XC    SVPID,SVPID                                                      
         J     PDET02                                                           
PDET01   TM    FLTFLG,FLTACC                                                    
         JNO   PDET02                                                           
         CLI   SVACC,0                                                          
         JE    PDET02                                                           
         MVC   LREP(L'ACCEQU),ACCEQU                                            
         MVC   LREP+4(L'SVACC),SVACC                                            
*                                                                               
PDET02   MVC   LSEQ,SVSEQ                                                       
*                                                                               
         MVC   LPRD,CLS5PRD1                                                    
         CLC   CLS5PRD2,SPACES                                                  
         JE    PDET04               Test if piggy back product                  
         CLI   CLS5PRD2,0                                                       
         JE    PDET04                                                           
         MVI   LPRD+3,C'/'                                                      
         MVC   LPRD2,CLS5PRD2                                                   
*                                                                               
PDET04   CLI   CLS5EST,0            Test if estimate present                    
         JE    PDET10                                                           
         EDIT  CLS5EST,(4,WORK)                                                 
         OC    WORK(4),=4X'F0'                                                  
         MVC   LEST,WORK+1                                                      
*                                                                               
PDET10   MVC   LINV,SVINV                                                       
*                                                                               
         TM    FLTFLG,FLTNETD       Test show net dollars                       
         JZ    *+14                                                             
         MVC   WORK,CLS5NET                                                     
         J     PDET20                                                           
         TM    FLTFLG,FLTGRSD       Test show gross dollars                     
         JZ    *+14                                                             
         MVC   WORK,CLS5NET                                                     
         J     PDET20                                                           
*                                                                               
         MVC   WORK,CLS5GRS         Defualt to gross                            
         OC    CLS5NET,CLS5NET                                                  
         JZ    PDET20                                                           
         MVC   WORK,CLS5NET                                                     
*                                                                               
PDET20   EDIT  (4,WORK),(11,LAMT),2,MINUS=YES                                   
*                                                                               
         TM    CLS5STAT,X'80'       Reconciled?                                 
         JZ    *+8                                                              
         MVI   LCKRCN,C'*'                                                      
         TM    CLS5STAT,X'40'       Credit?                                     
         JZ    *+10                                                             
         MVC   LSTAT,=C'CR'                                                     
         TM    CLS5STAT,X'20'       Check?                                      
         JZ    *+10                                                             
         MVC   LSTAT,=C'CK'                                                     
*                                                                               
         MVC   LCKNUM,CLS5CHK                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLS5CHDT),(8,LCKDT)                               
         TM    FLTFLG,FLTCLR        Test show check clear date                  
         JZ    PDET30                                                           
         GOTO1 DATCON,DMCB,(2,CLS5BKDT),(8,LCKDT)                               
*                                                                               
PDET30   GOTO1 LISTMON                                                          
*                                                                               
PDETX    J     EXIT                                                             
         DROP  R6                                                               
*********************************************************************           
* ERROR MESSAGES                                                                
*********************************************************************           
ERRINV   MVI   ERROR,INVALID                                                    
         J     ERRX                                                             
*                                                                               
ERROP1   MVC   HALF,=AL2(871)       Option 1 is not valid                       
         J     SPERREX                                                          
ERROP2   MVC   HALF,=AL2(872)       Option 2 is not valid                       
         J     SPERREX                                                          
ERROP3   MVC   HALF,=AL2(873)       Option 3 is not valid                       
         J     SPERREX                                                          
ERRSYN   MVC   HALF,=AL2(870)       Invalid options syntax                      
         J     SPERREX                                                          
ERRDUP   MVC   HALF,=AL2(854)       Duplicate option                            
         J     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         J     ERRX                                                             
         DROP  RF                                                               
*                                                                               
ERRX     GOTO1 ERREX                                                            
         J     EXIT                                                             
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
CLS01ELQ EQU   X'01'                                                            
CLS03ELQ EQU   X'03'                                                            
CLS05ELQ EQU   X'05'                                                            
*                                                                               
ACCEQU   DC    CL4'Acc='                                                        
PIDEQU   DC    CL4'Pid='                                                        
MOSEQU   DC    CL4'Mos='                                                        
RELO     DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMEBD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE NESFMWORKD                                                     
*********************************************************************           
* LOCAL WORKING STORAGE                                                         
*********************************************************************           
         ORG   SYSSPARE+220                                                     
*                                                                               
FLTVALS  DS    0X                   Clrst Filters                               
FLTCLI   DS    XL2                  Client                                      
FLTPAY   DS    CL3                  Payee                                       
FLTMKT   DS    XL2                  Market                                      
FLTNET   DS    XL3                  Network (Packed)                            
FLTSTD   DS    XL2                  Start Date (Range)                          
FLTEND   DS    XL2                  End Date (Range)                            
FLTINV   DS    CL10                 Invoice                                     
*                                                                               
FLTFLG   DS    XL1                                                              
FLTCLR   EQU   X'01'                Return Check Clear Date                     
FLTNETD  EQU   X'02'                Return Net                                  
FLTGRSD  EQU   X'04'                Return Gross                                
FLTOLD   EQU   X'08'                Return only header info                     
FLTDA    EQU   X'10'                Return disk address                         
FLTPID   EQU   X'20'                Return Pid                                  
FLTACC   EQU   X'40'                Return Acc Agy                              
FLTMOS   EQU   X'80'                Return MOS                                  
FLTVALLN EQU   *-FLTVALS                                                        
*                                                                               
SVVALS   DS    0C                   Saved Values                                
SVNET    DS    CL4                  Network                                     
SVSEQ    DS    CL3                  Sequence #                                  
SVEST    DS    CL3                  Estimate #                                  
SVINV    DS    CL10                 Invoice #                                   
SVVALLN  EQU   *-SVVALS                                                         
*                                                                               
FLTKEY   DS    XL13                                                             
CLRSTKEY DS    XL13                                                             
PREVKEY  DS    XL13                                                             
PIDKEY   DS    XL25                                                             
*                                                                               
SVPID    DS    CL8                  Pid                                         
SVACC    DS    CL2                  Acc Agency                                  
SVMOS    DS    CL6                  MOS                                         
SVMOSF   DS    CL6                  MOS FILTER                                  
*                                                                               
FLAG     DS    XL1                                                              
VAL01    EQU   X'01'                Found valid clearance element               
*                                                                               
CURRELEM DS    F                                                                
SCANBLK  DS    5CL32                                                            
TEMP     DS    XL100                                                            
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LNET     DS    CL4                  Network                                     
         DS    CL2                                                              
LREP     DS    CL4                  Rep                                         
         DS    CL1                                                              
LCLRDT   DS    CL8                  Clearance Date                              
         DS    CL1                                                              
LSEQ     DS    CL3                  Sequence #                                  
         DS    CL1                                                              
LPRD     DS    CL3                  Product                                     
         DS    CL1                  /                                           
LPRD2    DS    CL3                  Piggy back product                          
         DS    CL1                                                              
LEST     DS    CL3                  Estimate                                    
         DS    CL1                                                              
LINV     DS    CL10                 Invoice                                     
         DS    CL2                                                              
LAMT     DS    CL10                 Amount                                      
         DS    CL2                                                              
LCKNUM   DS    CL6                  Check Number                                
LCKRCN   DS    CL1                  Reconcilliation Indicator                   
         DS    CL1                                                              
LCKDT    DS    CL8                  Check Date                                  
LSTAT    DS    CL2                  CR = credit, CK=check                       
*                                                                               
SPOOLD   DSECT                                                                  
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NESFM33   05/19/11'                                      
         END                                                                    
