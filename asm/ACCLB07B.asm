*          DATA SET ACCLB07B   AT LEVEL 099 AS OF 12/22/99                      
*PHASE T62107B                                                                  
CLB07    TITLE '- BILL PROGRAM - LIST'                                          
CLB07    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB7**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING LWORKD,RC                                                        
         LH    R8,=Y(BSDICT-TWAD)                                               
         LA    R8,TWAD(R8)                                                      
         USING BSDICT,R8                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         ST    RE,BORELO                                                        
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXITY               LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     VALLAST             LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     SETHEAD             SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LINE SELECTION                      
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
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  NI    BCAPINDS,FF-BCAPIBAU                                             
*                                                                               
         TM    BCINDS1,BCINACT     TEST FIRST TIME FOR LIST                     
         BO    *+12                                                             
         TM    BCINDS2,BCINTRS                                                  
         BZ    SFRST02                                                          
         CLI   BCJOBCOD,C' '       TEST HAVE  A JOB CODE                        
         BNH   SFRST02                                                          
         LH    RE,=Y(UC@JOB-TWAD)  INSERT JOB= OPTION                           
         LA    RE,TWAD(RE)                                                      
         MVC   BASOPT(L'UC@JOB),0(RE)                                           
         LA    RF,BASOPT+L'UC@JOB-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCEQUAL                                                  
         MVC   2(L'BCJOBCOD,RF),BCJOBCOD                                        
         MVI   BASOPTH+FHILD,L'UC@JOB+1+L'BCJOBCOD                              
*                                                                               
SFRST02  L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
         MVC   BOELEM(L'LSOPS),LSOPS                                            
         XC    LSOPS,LSOPS                                                      
         MVI   SVJOBMAX,FF                                                      
         MVC   SVJOBMAX+1(L'SVJOBMAX-1),SVJOBMAX                                
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDLST),GOCBDLST-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
*                                                                               
SFRST30  CLC   LSOPS,BOELEM        TEST CHANGE IN OPTIONS                       
         BE    EXITY                                                            
         CLC   SVLIST,BOELEM       TEST CHANGE IN LIST TYPE                     
         BE    *+8                                                              
         OI    BCINDS2,BCIREBLS                                                 
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  LA    R2,IOKEY            INITIALIZE THE KEY                           
         USING PBRRECD,R2                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN                                                   
         MVI   PBRKSUB,PBRKACTQ                                                 
         CLI   SVLIST,SVLBILLQ                                                  
         BNE   *+12                                                             
         MVI   PBRPSUB,PBRPPASQ                                                 
         B     EXITY                                                            
         MVC   PBRKJOB,SVJOBMIN                                                 
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         TM    LSINDS2,LSIFNXTP    TEST FIRST FOR NEXT PAGE                     
         BO    GETNEXT             YES - A NORMAL GETNEXT SUFFICES              
         CLI   SVLIST,SVLJOBQ                                                   
         BE    GETFACT                                                          
         B     GETFPAS                                                          
*                                                                               
GETNEXT  LA    R2,IOKEY                                                         
         CLI   SVLIST,SVLJOBQ                                                   
         BE    GETNACT                                                          
         B     GETNPAS                                                          
*                                                                               
GETNPAS  IC    RE,PBRKEY+L'PBRKEY-1 * READ THROUGH PASSIVE RECORDS *            
         LA    RE,1(RE)                                                         
         STC   RE,PBRKEY+L'PBRKEY-1                                             
GETFPAS  GOTO1 AIO,IOHID+IOACCDIR                                               
         CLC   PBRPAS(PBRPBLNO-PBRPAS),IOKEYSAV                                 
         BNE   EXITN                                                            
*                                                                               
         MVC   TLBPAS,IOKEY                                                     
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
         BAS   RE,FLTREC                                                        
         BNE   GETNEXT                                                          
*                                                                               
         MVC   IODAOVER,PBRKDA                                                  
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BAS   RE,FLTDAT                                                        
         BNE   GETNEXT                                                          
         BAS   RE,CLCAMTS                                                       
         BAS   RE,SETCUR                                                        
         MVC   IOKEY,BOWORK1                                                    
         B     EXITY                                                            
*                                                                               
GETNACT  ICM   RE,7,PBRKSEQ-1      * READ THROUGH ACTIVE RECORDS *              
         LA    RE,1(RE)                                                         
         STCM  RE,7,PBRKSEQ-1                                                   
GETFACT  GOTO1 AIO,IOHID+IOACCDIR                                               
         CLC   PBRKEY(PBRKJOB-PBRKEY),IOKEYSAV                                  
         BNE   EXITN                                                            
         CLC   PBRKJOB,SVJOBMAX                                                 
         BH    EXITN                                                            
         CLI   PBRKPARA,0          SKIP BAD RECORDS                             
         BNE   GETNACT                                                          
         CLI   PBRKLINE,0                                                       
         BNE   GETNACT                                                          
*                                                                               
         MVC   BOWORK1(ACCKLEN),IOKEY                                           
         MVC   IODAOVER,PBRKDA     GET ACCMST RECORD                            
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BAS   RE,SETPAS           SET UP KEY OF PASSIVE                        
*                                                                               
         BAS   RE,FLTREC                                                        
         BNE   GETNEXT                                                          
         BAS   RE,FLTDAT                                                        
         BNE   GETNEXT                                                          
*                                                                               
         BAS   RE,CLCAMTS                                                       
         BAS   RE,SETCUR                                                        
         MVC   IOKEY,BOWORK1                                                    
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER ON PASSIVE RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
FLTREC   NTR1  ,                                                                
         LA    R2,TLBPAS                                                        
         USING PBRRECD,R2                                                       
*                                                                               
         CLI   SVDRAFT,SVYESQ      TEST DRAFT FILTER                            
         BE    FREC10                                                           
         CLI   SVDRAFT,SVONLYQ                                                  
         BNE   FREC02                                                           
         CLI   PBRPIND,PBRPIDFT                                                 
         BE    FREC10                                                           
         B     EXITN                                                            
FREC02   CLI   PBRPIND,PBRPIDFT                                                 
         BE    EXITN                                                            
*                                                                               
FREC10   CLI   SVLIVE,SVYESQ       TEST LIVE FILTER                             
         BE    FREC20                                                           
         CLI   SVLIVE,SVONLYQ                                                   
         BNE   FREC12                                                           
         CLI   PBRPIND,PBRPILVE                                                 
         BE    FREC20                                                           
         B     EXITN                                                            
FREC12   CLI   PBRPIND,PBRPILVE                                                 
         BE    EXITN                                                            
*                                                                               
FREC20   CLI   SVDEL,SVYESQ        TEST DELETE FILTER                           
         BE    FREC30                                                           
         CLI   SVDEL,SVONLYQ                                                    
         BNE   FREC22                                                           
         TM    PBRKSTAT,PBRSDELT                                                
         BO    FREC30                                                           
         B     EXITN                                                            
FREC22   TM    PBRKSTAT,PBRSDELT                                                
         BO    EXITN                                                            
*                                                                               
FREC30   CLC   PBRPJOB,SVJOBMIN    TEST JOB=ABC-XYZ                             
         BL    EXITN                                                            
         CLC   PBRPJOB,SVJOBMAX                                                 
         BH    EXITN                                                            
*                                                                               
FREC40   CLI   SVREFLN,0           FILTER ON BILL REF# ?                        
         BE    EXITY                                                            
         IC    R1,SVREFLN          GET LENGTH OF # INPUT                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   EXITN                                                            
         CLC   SVREFNO(0),PBRPBLNO                                              
         B     EXITY                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER ON ACTIVE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
FLTDAT   NTR1  ,                                                                
         L     R2,AIO1                                                          
         LA    R2,PBRRFST-PBRRECD(R2)   GET BLHEL FOR BILL DATES                
         USING BLHELD,R2                                                        
         XR    RF,RF                                                            
FLTDAT02 CLI   BLHEL,BLHELQ                                                     
         BE    FLTDAT04                                                         
         IC    RF,BLHLN                                                         
         AR    R2,RF                                                            
         B     FLTDAT02                                                         
*                                                                               
FLTDAT04 OC    SVMDATES,SVMDATES   MOA FILTERING  ?                             
         BZ    FLTDAT06                                                         
         OC    BLHBILD,BLHBILD     IS THE BILL LIVE ?                           
         BZ    FLTDAT06                                                         
         CLC   BLHBMOS,SVMDATST                                                 
         BL    EXITN                                                            
         OC    SVMDATEN,SVMDATEN                                                
         BZ    FLTDAT06                                                         
         CLC   BLHBMOS,SVMDATEN                                                 
         BH    EXITN                                                            
*                                                                               
FLTDAT06 OC    SVBDATES,SVBDATES   BILL DATE FILTERING ?                        
         BZ    FLTDAT08                                                         
         CLC   BLHTRND,SVBDATST                                                 
         BL    EXITN                                                            
         OC    SVBDATEN,SVBDATEN                                                
         BZ    FLTDAT08                                                         
         CLC   BLHTRND,SVBDATEN                                                 
         BH    EXITN                                                            
*                                                                               
FLTDAT08 OC    SVLDATES,SVLDATES   LIVE DATE FILTERING ?                        
         BZ    FLTDATX                                                          
         CLC   BLHBILD,SVLDATST                                                 
         BL    EXITN                                                            
         OC    SVLDATEN,SVLDATEN                                                
         BZ    FLTDATX                                                          
         CLC   BLHBILD,SVLDATEN                                                 
         BH    EXITN                                                            
*                                                                               
FLTDATX  B     EXITY                                                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET CURRENCY ENTRY ON TSAR RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
SETCUR   NTR1  ,                                                                
*                                                                               
         L     R4,AIO1             EXTRACT CURRENCY CODE                        
         LA    R4,PBRRFST-PBRRECD(R4)                                           
         USING BLHELD,R4                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R4,RF,*-12                                                       
*                                                                               
         OC    BLHCUR,BLHCUR       TEST USING COMPANY CURRENCY                  
         BZ    *+14                                                             
         CLC   BLHCUR,CSCPYCUR                                                  
         BNE   SCUR02                                                           
         MVC   TLBCUR,CSCURCPY                                                  
         B     SETCURX                                                          
*                                                                               
SCUR02   CLC   BLHCUR,CSBILCUR     TEST USING BILLING CURRENCY                  
         BNE   *+14                                                             
         MVC   TLBCUR,CSCURBIL                                                  
         B     SETCURX                                                          
         GOTO1 VBLDCUR,BOPARM,BLHCUR,TLBCUR,ACOM                                
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETCURX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET THE HEADLINE (FOR THE KEY)                           *         
***********************************************************************         
         SPACE 1                                                                
SETHEAD  LM    R2,R3,4(R1)                                                      
         CLI   SVLIST,SVLBILLQ                                                  
         BE    SHED02                                                           
         MVCDD 0(L'BLHJOB,R2),AC#JOB                                            
         MVCDD 0(L'BLHJOB,R3),AC#JOB,LU                                         
         MVCDD L'BLHJOB+1(L'BLHBLNO,R2),AC#BILC                                 
         MVCDD L'BLHJOB+1(L'BLHBLNO,R3),AC#BILC,LU                              
         B     EXIT                                                             
SHED02   MVCDD 0(L'BLHBLNO,R2),AC#BILC                                          
         MVCDD 0(L'BLHBLNO,R3),AC#BILC,LU                                       
         MVCDD L'BLHBLNO+1(L'BLHJOB,R2),AC#JOB                                  
         MVCDD L'BLHBLNO+1(L'BLHJOB,R3),AC#JOB,LU                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING PBRRECD,R2          R2=A(PRODUCTION BILL RECORD)                 
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISKEY              KEY                                          
         B     DISBIL              DATE BILLED                                  
         B     DISNET              NET                                          
         B     DISCOM              COMMISSION                                   
         B     DISCRT              DATE CREATED                                 
         B     DISEXP              DATE EXPIRES                                 
         B     DISCUR              CURRENCY                                     
         B     DISFMT              FORMAT                                       
         B     DISLPR              DATE/TIME LAST PRINTED                       
         B     DISDAT              BILL DATE                                    
         SPACE 1                                                                
***********************************************************************         
* DISPLAY KEY (UPDATE AMOUNT TOTALS IF NECESSERY)                     *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   CLC   PBRRSTA,TLBPSTA     TEST STATUS CHANGED                          
         BE    DKEY02                                                           
         BAS   RE,SETPAS                                                        
*                                                                               
DKEY02   TM    BCAPINDS,BCAPIBAU   TEST BILL AMOUNT UPDATE REQUIRED             
         BZ    DKEY04                                                           
         NI    BCAPINDS,FF-BCAPIBAU                                             
         BAS   RE,CLCAMTS                                                       
*                                                                               
DKEY04   CLI   SVLIST,SVLJOBQ      TEST LISTING BY JOB OR BILL#                 
         BNE   DKEY06                                                           
         MVC   FVIFLD(L'BLHJOB),BLHJOB                                          
         MVC   FVIFLD+L'BLHJOB+1(L'BLHBLNO),BLHBLNO                             
         B     EXIT                                                             
*                                                                               
DKEY06   MVC   FVIFLD(L'BLHBLNO),BLHBLNO                                        
         MVC   FVIFLD+L'BLHBLNO+1(L'BLHJOB),BLHJOB                              
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FORMAT                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISFMT   EDIT  BLHFORM,(3,FVIFLD),0,WRK=BOWORK1,DUB=BODUB1                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET/COMMISSION                                              *         
***********************************************************************         
         SPACE 1                                                                
DISNET   LA    RF,TLBNET                                                        
         B     *+8                                                              
DISCOM   LA    RF,TLBCOM                                                        
         CURED (P8,(RF)),(13,FVIFLD),TLBCUR,MINUS=YES,ZERO=NOBLANK,    *        
               DMCB=BODMCB                                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DATE CREATED                                                *         
***********************************************************************         
         SPACE 1                                                                
DISCRT   GOTO1 VDATCON,BOPARM,(X'42',BLHCRED),(17,FVIFLD)                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DATE EXPIRES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISEXP   GOTO1 VDATCON,BOPARM,(X'42',BLHEXPD),(17,FVIFLD)                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DATE BILLED                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBIL   OC    BLHBILD,BLHBILD                                                  
         BNZ   DBIL02                                                           
         MVCDD FVIFLD(8),AC#DRAFT                                               
         TM    PBRRSTAT,PBRSDELT                                                
         BZ    EXIT                                                             
         MVCDD FVIFLD(8),AC#DELD                                                
         B     EXIT                                                             
DBIL02   GOTO1 VDATCON,BOPARM,(X'42',BLHBILD),(17,FVIFLD)                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DATE/TIME LAST PRINTED                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLPR   OC    BLHPDATE,BLHPDATE                                                
         BZ    EXIT                                                             
         GOTO1 VDATCON,BOPARM,(X'42',BLHPDATE),(17,FVIFLD)                      
         OC    BLHPTIME,BLHPTIME                                                
         BZ    EXIT                                                             
         LA    R2,FVIFLD+7                                                      
         CLI   0(R2),C' '                                                       
         BNH   *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(1,R2),BCSLASH                                                  
         XR    R0,R0                                                            
         GOTO1 DLPR,BLHPTHH                                                     
         GOTO1 (RF),BLHPTMM                                                     
         GOTO1 (RF),BLHPTSS                                                     
         MVI   0(R2),C' '                                                       
*                                                                               
         MVI   1(R2),C'('                                                       
         EDIT  (1,BLHPNUM),(3,2(R2)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
         AR    R2,R0                                                            
         MVI   2(R2),C')'                                                       
         B     EXIT                                                             
*                                                                               
DLPR     UNPK  1(1,R2),0(1,R1)                                                  
         OI    1(R2),C'0'                                                       
         MVC   2(1,R2),0(R1)                                                    
         OI    2(R2),C'0'                                                       
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'.'                                                       
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCUR   MVC   FVIFLD(L'BLHCUR),BLHCUR                                          
         CLC   BLHCUR,BCSPACES                                                  
         BH    EXIT                                                             
         MVC   FVIFLD(L'CSCPYCUR),CSCPYCUR                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY BILL DATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISDAT   GOTO1 VDATCON,BOPARM,(X'42',BLHTRND),(17,FVIFLD)                       
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   L     R2,AIO1                                                          
         USING PBRRECD,R2          R2=A(PRODUCTION BILL RECORD)                 
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         DC    2H'0'               KEY                                          
         DC    2H'0'               DATE BILLED                                  
         DC    2H'0'               NET                                          
         DC    2H'0'               COMMISSION                                   
         DC    2H'0'               DATE CREATED                                 
         B     VALEXP              DATE EXPIRES                                 
         DC    2H'0'               CURRENCY                                     
         DC    2H'0'               FORMAT                                       
         DC    2H'0'               DATE/TIME LAST PRINTED                       
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DATE EXPIRES                                               *         
***********************************************************************         
         SPACE 1                                                                
VALEXP   CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         TM    PBRRSTAT,PBRSDELT   TEST RECORD IS DELETED                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITN                                                            
         L     R1,FVADDR                                                        
         GOTO1 AVALDAT                                                          
         BNE   EXITN                                                            
         CLC   BCTODAYC,BCWORK                                                  
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DNGRT)                                           
         B     EXITN                                                            
         OI    LSINDS1,LSIUPREC                                                 
         MVC   BLHEXPD,BCWORK                                                   
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALLAST  TM    LSINDS1,LSIUPREC    TEST RECORD TO BE UPDATED                    
         BZ    EXITY                                                            
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   PBRKEY,TLBPAS                                                    
         GOTO1 AIO,IOREAD+IOLOCK+IOACCDIR                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PBRKEXPD,BLHEXPD    UPDATE EXPIRY DATE ON PASSIVE                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE SELECTION                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         LA    R2,TLBPAS                                                        
         USING PBRRECD,R2                                                       
         TM    PBRKSTAT,PBRSDELT                                                
         BZ    *+12                                                             
         CLM   R1,1,=AL1(VALRESQ)                                               
         BNE   EXITN                                                            
         B     *(R1)                                                            
VALSELR  DS    0XL4                                                             
         B     VALALL              ALLOCATE                                     
         B     VALSET              SETUP                                        
         B     VALLPAR             LISTPARA                                     
         B     VALEDIT             EDIT                                         
         B     VALUPD              UPDATE                                       
         B     VALDRA              DRAFT                                        
         B     VALREP              REPRINT                                      
         B     VALDEL              DELETE                                       
         B     VALRES              RESTORE                                      
VALRESQ  EQU   *-VALSELR                                                        
*                                                                               
VALALL   GOTO1 SETUP,0                                                          
         BNE   EXITN                                                            
         B     EXITY                                                            
*                                                                               
VALSET   GOTO1 SETUP,0                                                          
         OI    CSINDSG1,CSINDSET                                                
         B     EXITY                                                            
*                                                                               
VALLPAR  CLI   PBRPIND,PBRPIDFT    BILL MUST BE DRAFT                           
         BNE   EXITN                                                            
         GOTO1 SETUP,SETICUR                                                    
         BNE   EXITN                                                            
         MVC   CSBILNUM,PBRPBLNO   SET BILL NUMBER                              
         B     EXITY                                                            
*                                                                               
*ALEDIT  CLI   PBRPIND,PBRPIDFT    BILL MUST BE DRAFT                           
*        BNE   EXITN                                                            
VALEDIT  GOTO1 SETUP,SETICUR                                                    
         BNE   EXITN                                                            
         MVC   CSBILNUM,PBRPBLNO   SET BILL NUMBER                              
         B     EXITY                                                            
*                                                                               
VALUPD   CLI   PBRPIND,PBRPIDFT                                                 
         BNE   EXITN                                                            
         GOTO1 SETUP,SETICUR+SETIALL                                            
         BNE   EXITN                                                            
         MVC   CSBILNUM,PBRPBLNO                                                
         B     EXITY                                                            
*                                                                               
VALDRA   CLI   PBRPIND,PBRPIDFT                                                 
         BNE   EXITN                                                            
         GOTO1 SETUP,SETICUR+SETIALL                                            
         BNE   EXITN                                                            
         MVC   CSBILNUM,PBRPBLNO                                                
         B     EXITY                                                            
*                                                                               
VALREP   CLI   PBRPIND,PBRPILVE    TEST LIVE BILL                               
         BNE   EXITN                                                            
         GOTO1 SETUP,0                                                          
         BNE   EXITN                                                            
         MVC   CSBILNUM,PBRPBLNO                                                
         GOTO1 VCOLY,BOPARM,('O#BILPRT',0),0,0                                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),BOPARM,0                                                    
         B     EXITY                                                            
*                                                                               
VALDEL   CLI   PBRPIND,PBRPIDFT    TEST DRAFT BILL                              
         BNE   EXITN                                                            
         TM    PBRKSTAT,PBRSDELT   TEST NOT ALREADY DELETED                     
         BO    EXITN                                                            
         MVC   IOKEY,PBRKEY        DELETE PASSIVE POINTER                       
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PBRKSTAT,PBRSDELT                                                
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLBPAS,IOKEY                                                     
         MVC   IODAOVER,PBRKDA     GET FILE RECORD                              
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         L     R2,AIO1                                                          
         MVC   IOKEY,PBRKEY                                                     
         LA    R2,IOKEY            DELETE DIRECTORY RECORD                      
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         OI    PBRKSTAT,PBRSDELT                                                
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             DELETE FILE RECORD                           
         OI    PBRRSTAT,PBRSDELT                                                
         GOTO1 AIO,IOWRITE+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
*                                                                               
VALRES   CLI   PBRPIND,PBRPIDFT    TEST DRAFT BILL                              
         BNE   EXITN                                                            
         TM    PBRKSTAT,PBRSDELT   TEST IS DELETED                              
         BZ    EXITN                                                            
         MVC   IOKEY,PBRKEY        RESTORE PASSIVE POINTER                      
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         NI    PBRKSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLBPAS,IOKEY                                                     
         MVC   IODAOVER,PBRKDA     GET FILE RECORD                              
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         L     R2,AIO1                                                          
         MVC   IOKEY,PBRKEY                                                     
         LA    R2,IOKEY            RESTORE DIRECTORY RECORD                     
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         NI    PBRKSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             RESTORE FILE RECORD                          
         NI    PBRRSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOWRITE+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CALL SET UP FOR BILL                                     *         
*                                                                     *         
* NTRY: R1=SETINDS VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
SETUP    NTR1  ,                                                                
         STC   R1,SETINDS                                                       
         MVC   IODAOVER,TLDA                                                    
         MVC   IOADDR,AIO4                                                      
         GOTO1 AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO4                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         GOTO1 ASETUP,BOPARM,PBRPJOB,BLHCUR,BLHRVAL                             
         BNE   EXIT                                                             
*                                                                               
         TM    SETINDS,SETICUR     TEST CURRENCY/RATE MUST MATCH                
         BZ    SETUPX                                                           
         CLC   BLHCUR,CSBILCUR                                                  
         BE    SETUP02                                                          
         MVC   FVXTRA(L'BLHCUR),BLHCUR                                          
         MVC   FVMSGNO,=AL2(AE$BCDMA)                                           
         B     EXITN                                                            
SETUP02  CLC   CSBILCUR,CSCPYCUR                                                
         BE    SETUP04                                                          
         CLC   CSBILCUR,BCCPYSEC                                                
         BE    SETUP04                                                          
         CLC   BLHRATE,CSEXCRAT                                                 
         BE    SETUP04                                                          
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),BLHRATE,0                       
         MVC   FVMSGNO,=AL2(AE$BRDMA)                                           
         B     EXITN                                                            
*                                                                               
SETUP04  TM    SETINDS,SETIALL     TEST ALLOCATION MUST BE EQUAL                
         BZ    SETUPX                                                           
         L     R4,AIO1                                                          
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         USING SCIELD,R4           R4=A(SCIELD FOR ALLOCATED NET/COMM)          
         XR    RF,RF                                                            
SETUP06  CLI   SCIEL,0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     EXITN                                                            
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCBAP                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R4,RF,SETUP06                                                    
         CP    SCIAMNT,TLBNET      TEST NET THE SAME                            
         BNE   *+14                                                             
         CP    SCIADMN,TLBCOM      TEST COMMISSION THE SAME                     
         BE    SETUPX                                                           
         MVC   FVMSGNO,=AL2(AE$DBNEJ)                                           
         B     EXITN                                                            
         DROP  R4                                                               
*                                                                               
SETUPX   OI    CSINDSG1,CSINDSET                                                
         B     EXITY                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE NET/COMMISSION AMOUNTS                         *         
*                                                                     *         
* NTRY: IO1=BILL HEADER RECORD                                        *         
* EXIT: CSLSTCUR IS UPDATED                                           *         
***********************************************************************         
         SPACE 1                                                                
CLCAMTS  NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         ZAP   TLBNET,PZERO                                                     
         ZAP   TLBCOM,PZERO                                                     
*                                                                               
         LA    R1,PBRRFST                                                       
         USING NDXELD,R1           R1=A(INDEX ELEMENT)                          
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R1,RF,*-12                                                       
         XR    R0,R0                                                            
         ICM   R0,1,NDXACTV        R0=NO. OF ACTIVE ENTRIES                     
         BZ    EXIT                                                             
         LA    R6,NDXINDX          R6=A(LIST OF ACTIVE ENTRIES)                 
         DROP  R1                                                               
*                                                                               
CAMTS02  MVC   IOKEY,PBRKEY                                                     
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY+(PBRKPARA-PBRRECD)(L'PBRKPARA),0(R6)                       
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         XR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    TLBNET,PGHNET       UPDATE AMOUNTS                               
         AP    TLBCOM,PGHCOM                                                    
         DROP  R1                                                               
*                                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,CAMTS02                                                       
*                                                                               
CLCAMTSX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP PASSIVE KEY FROM RECORD                           *         
*                                                                     *         
* NTRY: AIO1=BILL HEADER RECORD                                       *         
* EXIT: TLBPAS=PASSIVE KEY/RECORD STATUS                              *         
***********************************************************************         
         SPACE 1                                                                
SETPAS   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         MVC   TLBPSTA,PBRRSTA                                                  
         LA    R1,PBRRFST                                                       
         USING BLHELD,R1           R1=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R1,RF,*-12                                                       
         LA    R2,TLBPAS                                                        
         XC    PBRPAS,PBRPAS       SET UP KEY OF PASSIVE                        
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,BLHBLNO                                                 
         MVI   PBRPIND,PBRPIDFT                                                 
         OC    BLHBILD,BLHBILD                                                  
         BZ    *+8                                                              
         MVI   PBRPIND,PBRPILVE                                                 
         MVC   PBRPUSER,BLHUSER                                                 
         MVC   PBRPJOB,BLHJOB                                                   
         MVC   PBRPCRED,BLHCRED                                                 
         MVC   PBRPBILD,BLHBILD                                                 
         MVC   PBRPFORM,BLHFORM                                                 
         MVC   PBRPPERS,BLHPERS                                                 
         DROP  R1,R2                                                            
*                                                                               
SETPASX  B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
PZERO    DC    P'0'                                                             
         SPACE 1                                                                
DEFCLM   DS    0XL1                DEFAULT COLUMN LIST                          
         DC    AL1(LST#BIL)                                                     
         DC    AL1(LST#NET)                                                     
         DC    AL1(LST#COM)                                                     
         DC    AL1(LST#CRT)                                                     
         DC    AL1(LST#EXPO)                                                    
*&&UK*&& DC    AL1(LST#CUR)                                                     
         DC    AL1(LST#FMT)                                                     
         DC    AL1(LST#LPR)                                                     
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
LWORKD   DSECT                     ** LOCAL WORKING STORAGE **                  
SETINDS  DS    XL1                 INDICATOR BYTE FOR SETUP ROUTINE             
SETICUR  EQU   X'80'               CURRENCY/RATE MUST BE AS BILL                
SETIALL  EQU   X'40'               ALLOCATION PENDING MUST BE AS BILL           
LWORKX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB07    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,LSCLMMAX,LSCLMMAX)                               
         DC    AL1(0)                                                           
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  LIST=JOB/BILL                                
         DC    AL2(UC@LIST-TWAD,UC@LIST-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,3,1,8,L'SVLIST)                                      
         DC    AL1(1)                                                           
         DC    AL2(1,SVLIST-LSVALSD)                                            
         DC    AL1(SVLJOBQ),AL3(0)                                              
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  DRAFT=YES/NO/ONLY                            
         DC    AL2(UC@DRAFT-TWAD,UC@DRAFT-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'SVDRAFT)                                     
         DC    AL1(2)                                                           
         DC    AL2(2,SVDRAFT-LSVALSD)                                           
         DC    AL1(SVYESQ),AL3(0)                                               
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  LIVE=YES/NO/ONLY                             
         DC    AL2(UC@LIVE-TWAD,UC@LIVE-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,3,1,4,L'SVLIVE)                                      
         DC    AL1(3)                                                           
         DC    AL2(3,SVLIVE-LSVALSD)                                            
         DC    AL1(SVYESQ),AL3(0)                                               
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  DELETE=YES/NO/ONLY                           
         DC    AL2(UC@DEL-TWAD,UC@DEL-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'SVDEL)                                       
         DC    AL1(4)                                                           
         DC    AL2(4,SVDEL-LSVALSD)                                             
         DC    AL1(SVNOQ),AL3(0)                                                
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  JOB=ABC-XYZ                                  
         DC    AL2(UC@JOB-TWAD,UC@JOB-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,25,L'SVJOB)                                      
         DC    AL1(5)                                                           
         DC    AL2(5,SVJOB-LSVALSD)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  BILLDATE=DATE RANGE                          
         DC    AL2(UC@BILDT-TWAD,UC@BILDT-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,25,L'SVBDATES)                                   
         DC    AL1(6)                                                           
         DC    AL2(6,SVBDATES-LSVALSD)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  LIVEDATE=DATE RANGE                          
         DC    AL2(UC@LDATE-TWAD,UC@LDATE-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,5,1,25,L'SVLDATES)                                   
         DC    AL1(7)                                                           
         DC    AL2(7,SVLDATES-LSVALSD)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  MOA=DATE RANGE                               
         DC    AL2(UC3MOA-TWAD,UC8MOA-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,20,L'SVMDATES)                                   
         DC    AL1(8)                                                           
         DC    AL2(8,SVMDATES-LSVALSD)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  REF=DATE RANGE                               
         DC    AL2(UC3REF-TWAD,UC8REF-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,6,L'SVREF)                                       
         DC    AL1(9)                                                           
         DC    AL2(9,SVREF-LSVALSD)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R8,RB                                                            
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALOLST             1 LIST=JOB/BILL                              
         B     VALOYNO             2 DRAFT=YES/NO/ONLY                          
         B     VALOYNO             3 LIVE=YES/NO/ONLY                           
         B     VALOYNO             4 DELETE=YES/NO/ONLY                         
         B     VALOJOB             5 JOB=ABC-XYZ                                
         B     VALODAT             6 BILLDATE=DATE RANGE                        
         B     VALODAT             7 LIVEDATE=DATE RANGE                        
         B     VALOMOA             8 MOA=DATE RANGE                             
         B     VALOREF             9 REF=XXXXXX                                 
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST=JOB/BILL                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOLST  XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,SVLJOBQ                                                   
         LH    RE,=Y(UC@JOB-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALX                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,SVLBILLQ                                                  
         LH    RE,=Y(UC@BIL-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALX                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE = YES/NO/ONLY                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOYNO  IC    RE,FVXLEN                                                        
*                                                                               
         MVI   BCWORK,SVYESQ       =YES                                         
         CLC   FVIFLD(0),BC@YES                                                 
         EX    RE,*-6                                                           
         BE    VALX                                                             
*                                                                               
         MVI   BCWORK,SVNOQ        =NO                                          
         CLC   FVIFLD(0),BC@NO                                                  
         EX    RE,*-6                                                           
         BE    VALX                                                             
*                                                                               
         MVI   BCWORK,SVONLYQ      =ONLY                                        
         LH    RF,=Y(UC@ONLY-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         CLC   FVIFLD(0),0(RF)                                                  
         EX    RE,*-6                                                           
         BE    VALX                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB=                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALOJOB  LA    R3,BCWORK                                                        
         USING SVJOB,R3                                                         
         MVI   SVJOBMAX,FF                                                      
         MVC   SVJOBMAX+1(L'SVJOBMAX-1),SVJOBMAX                                
*                                                                               
         LA    R2,FVIFLD           R2=A(INPUT)                                  
         XR    RE,RE                                                            
         IC    RE,FVILEN           RE=L(INPUT)                                  
         LR    RF,RE                                                            
         LR    R1,R2               R1=A(INPUT)                                  
         CLI   0(R1),C'-'                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
*                                                                               
         SR    RF,RE               COPY MINIMUM CODE                            
         BZ    VJOB02                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVJOBMIN(0),0(R2)                                                
         LTR   RE,RE               TEST PARTITION CHARACTER FOUND               
         BNZ   VJOB02                                                           
         EX    RF,*+4                                                           
         MVC   SVJOBMAX(0),0(R2)                                                
         B     VALOJOBX                                                         
*                                                                               
VJOB02   BCT   RE,*+8                                                           
         B     VALOJOBX                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SVJOBMAX(0),1(R1)                                                
*                                                                               
VALOJOBX B     VALX                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE RANGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALODAT  LA    R5,BCWORK           R5=A(OUTPUT DATE RANGE)                      
         LA    R2,FVIFLD           R2=A(DATE VALUE)                             
         XR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         GOTO1 VPERVAL,BOPARM,((R3),(R2)),('PVINSGLS',BOWORK1)                  
         L     RF,4(R1)            RF=A(PERVAL O/P BLOCK)                       
         USING PERVALD,RF                                                       
*                                                                               
         CLI   4(R1),PVRCOK        DATE RANGE FOUND?                            
         BE    *+12                                                             
         CLI   4(R1),PVRCONE       ONE DATE INPUT ?                             
         BNE   VALODERR            ERROR                                        
*                                                                               
         TM    PVALASSM,PVALASD+PVALASM+PVALASY START DATE ASSUMED?             
         BO    *+10                                                             
         MVC   0(L'PVALCSTA,R5),PVALCSTA   GET START DATE                       
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY END DATE ASSUMED?               
         BNO   VALODAT4                                                         
*                                                                               
         CLI   0(R2),C'-'          SEARCH FOR SEPERATOR                         
         BE    VALX                                                             
         LA    R2,1(R2)                                                         
         BCT   R3,*-12                                                          
*                                                                               
         CLM   RF,8,=X'4'          ONE DATE INPUT ?                             
         BE    VALODAT2                                                         
         TM    PVALASSM,PVALASD+PVALASM+PVALASY START DATE ASSUMED?             
         BO    *+14                                                             
         MVC   L'PVALCSTA(L'PVALCEND,R5),PVALCEND                               
         B     *+10                                                             
VALODAT2 MVC   L'PVALCSTA(L'PVALCSTA,R5),PVALCSTA                               
         B     VALX                                                             
*                                                                               
VALODAT4 CLI   4(R1),PVRCONE                                                    
         BE    *+14                                                             
         MVC   L'PVALCSTA(L'PVALCEND,R5),PVALCEND                               
         B     *+10                                                             
         MVC   L'PVALCSTA(L'PVALCSTA,R5),PVALCSTA                               
         B     VALX                                                             
*                                                                               
VALODERR MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALX                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOA  RANGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALOMOA  LA    R5,BCWORK           R5=A(OUTPUT DATE RANGE)                      
         LA    R2,FVIFLD           R2=A(DATE VALUE)                             
         XR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         GOTO1 VPERVAL,BOPARM,((R3),(R2)),('PVINSGLS',BOWORK1)                  
         L     RF,4(R1)            RF=A(PERVAL O/P BLOCK)                       
         USING PERVALD,RF                                                       
*                                                                               
         CLI   4(R1),PVRCOK        DATE RANGE FOUND?                            
         BE    *+12                                                             
         CLI   4(R1),PVRCONE       ONE DATE INPUT ?                             
         BNE   VALOMERR            ERROR                                        
*                                                                               
         TM    PVALASSM,PVALASD    CHECK NO START DAY INPUT                     
         BZ    VALOMERR                                                         
         TM    PVALASSM,PVALASM+PVALASY START DATE ASSUMED?                     
         BO    *+10                                                             
         MVC   0(L'SVMDATST,R5),PVALPSTA   GET START DATE                       
         TM    PVALASSM,PVALAED    CHECK NO END DAY INPUT                       
         BZ    VALOMERR                                                         
         TM    PVALASSM,PVALAEM+PVALAEY END DATE ASSUMED?                       
         BNO   VALOMOA4                                                         
*                                                                               
         CLI   0(R2),C'-'          SEARCH FOR SEPERATOR                         
         BE    VALX                                                             
         LA    R2,1(R2)                                                         
         BCT   R3,*-12                                                          
*                                                                               
         CLM   RF,8,=X'4'          ONE DATE INPUT ?                             
         BE    VALOMOA2                                                         
         TM    PVALASSM,PVALASM+PVALASY START DATE ASSUMED?                     
         BO    *+14                                                             
         MVC   L'SVMDATST(L'SVMDATEN,R5),PVALPEND                               
         B     *+10                                                             
VALOMOA2 MVC   L'SVMDATST(L'SVMDATST,R5),PVALPSTA                               
         B     VALX                                                             
*                                                                               
VALOMOA4 CLI   4(R1),PVRCONE                                                    
         BE    *+14                                                             
         MVC   L'SVMDATST(L'SVMDATEN,R5),PVALPEND                               
         B     *+10                                                             
         MVC   L'SVMDATST(L'SVMDATST,R5),PVALPSTA                               
         B     VALX                                                             
*                                                                               
VALOMERR MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALX                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REFERENCE NUMBER OPTION                                    *         
***********************************************************************         
         SPACE 1                                                                
VALOREF  LA    R3,BCWORK                                                        
         USING SVREF,R3                                                         
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    VALORERR                                                         
         MVC   SVREFLN,FVILEN                                                   
         MVC   SVREFNO,FVIFLD                                                   
         B     VALX                                                             
*                                                                               
VALORERR MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALX                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKB                                                     
         SPACE 1                                                                
***********************************************************************         
* BILL LIST OPTIONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
LSVALSD  DSECT                                                                  
         ORG   LSOPS                                                            
SVLIST   DS    XL1                 TYPE OF LIST                                 
SVLBILLQ EQU   C'B'                LIST BY BILL NUMBER                          
SVLJOBQ  EQU   C'J'                LIST BY JOB NUMBER                           
SVDRAFT  DS    XL1                 DRAFT=YES/NO/ONLY                            
SVYESQ   EQU   C'Y'                YES                                          
SVNOQ    EQU   C'N'                NO                                           
SVONLYQ  EQU   C'O'                ONLY                                         
SVLIVE   DS    XL1                 LIVE=YES/NO/ONLY                             
SVDEL    DS    XL1                 DELETE=YES/NO/ONLY                           
SVJOB    DS    0CL24                                                            
SVJOBMIN DS    CL12                MINIMUM JOB CODE                             
SVJOBMAX DS    CL12                MAXIMUM JOB CODE                             
SVREF    DS    0XL7                                                             
SVREFLN  DS    XL1                 LENGTH OF REF# INPUT                         
SVREFNO  DS    CL6                 BILL REFERENCE NUMBER                        
*                                                                               
SVBDATES DS    0XL(2*L'BLHTRND)                                                 
SVBDATST DS    XL(L'BLHTRND)       BILL DATE FILTER - START                     
SVBDATEN DS    XL(L'BLHTRND)       BILL DATE FILTER - END                       
SVLDATES DS    0XL(2*L'BLHBILD)                                                 
SVLDATST DS    XL(L'BLHBILD)       LIVE DATE FILTER - START                     
SVLDATEN DS    XL(L'BLHBILD)       LIVE DATE FILTER - END                       
SVMDATES DS    0PL(2*L'BLHBMOS)                                                 
SVMDATST DS    PL(L'BLHBMOS)       MOA FILTER - START                           
SVMDATEN DS    PL(L'BLHBMOS)       MOA FILTER - END                             
         ORG   LSOPS+L'LSOPS                                                    
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099ACCLB07B  12/22/99'                                      
         END                                                                    
