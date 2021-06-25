*          DATA SET ACREPEW02  AT LEVEL 010 AS OF 08/05/20                      
         TITLE 'JOB EMAIL ALERTS FOR ESTIMATES'                                 
*PHASE ACEW02A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE URIENC                                                                 
*SMAN 001 ORIGINAL VERSION                                                      
*MPEN 002 <PR000404> CONTROL WHAT UNAPPROVED ORDERS ARE SHOWN                   
*MPEN 005 <RD013417> NEW EMAIL ALERTS FOR AURA                                  
*MPEN 006 <ITMF-13676> BUG FIX FOR READING ASJPASDS                             
*MPEN 007 <DSRD-19564> Remove bcc                                               
*MPEN                COPY US CHANGES                                            
*MPEN 008 <DSRD-15233> URI ENCODE URLS                                          
*MPEN 009 <DSRD-21358> UPDATE GERMAN ADDRESS                                    
*NSHE 010 05Aug20 <DSRD-27173> Clean up URL logic                               
***********************************************************************         
ACEW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACEW02*                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE                                
         USING SORTRECD,SORTREC                                                 
         LH    RF,=Y(IOAREA1-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA1                                                      
         LH    RF,=Y(IOAREA2-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA2                                                      
         LH    RF,=Y(IOAREA3-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA3         =P.4                                         
*                                                                               
         USING PARMD,RCFFPARM      PARM EUROPE                                  
         MVI   FCRDTRNS,NOQ                                                     
         MVI   FCRDHIST,NOQ                                                     
         MVI   FCRDACC,NOQ                                                      
         MVI   FCRDTIME,NOQ                                                     
         MVI   FCRDORD,NOQ                                                      
         MVI   FCRDEST,NOQ                                                      
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
*                                                                               
EXITL    XR    RE,RE               SET CC LOW                                   
         J     EXITCC                                                           
*                                                                               
EXITE    LHI   RE,1                SET CC EQUAL                                 
*                                                                               
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALISING                                                        *         
***********************************************************************         
         SPACE 1                                                                
RUNL     MVI   LLANG,X'FF'                                                      
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
         MVC   CTRY,MCCTRY                                                      
         XR    R4,R4                                                            
         LA    R5,EMRECL           GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,EMLTBLN                                                       
         L     R0,EMLTBLN                                                       
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AEMLTAB          START OF AREA                                
*                                                                               
         MVC   DUB,=CL8'GETURL'    ACGETURL                                     
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   RUNL10                                                           
         MVC   DUB,=CL8'GETURL'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
RUNL10   MVC   VGETURL,4(R1)       A(ACGETURL)                                  
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
         DROP  R8                                                               
         LAY   R5,TESTMAIL                                                      
         MVC   0(L'TESTMAIL,R5),SPACES                                          
*                                                                               
         MVI   IOCOMP,0                                                         
         CLC   PARMDAG,SPACES      UNLESS SPECIFIC REQUEST                      
         BNH   RUNL15                                                           
         GOTO1 VHEXIN,DMCB,PARMDAG,IOCOMP,2                                     
*                                                                               
RUNL15   CLI   QCNTINUE,QCNTQ                                                   
         JNE   RUNL60                NONE SUPPLIED                              
         LAY   R5,TESTMAIL                                                      
         CLC   QCACCT,SPACES                                                    
         JNH   RUNL30                                                           
         MVC   0(L'TESTMAIL,R5),QCACCT                                          
         OC    0(L'TESTMAIL,R5),SPACES                                          
         LA    RF,L'TESTMAIL-17      L'SUFFIX+1 FOR FIRST INCREMENT             
RUNL20   DS    0H                                                               
         LA    R5,1(R5)                                                         
         CLI   0(R5),C' '                                                       
         JNH   *+8                                                              
         JCT   RF,RUNL20                                                        
         MVC   0(16,R5),=C'@MEDIAOCEAN.COM:'                                    
*                                                                               
RUNL30   LAY   R5,TARPID                                                        
         MVC   0(L'TARPID,R5),QDRAFT                                            
         OC    0(L'TARPID,R5),SPACES                                            
*                                                                               
         CLC   0(L'TARPID,R5),SPACES                                            
         JNH   RUNL60                                                           
         MVC   IOKEY1,SPACES                                                    
         LA    R2,IOKEY1                                                        
         USING PERRECD,R2                                                       
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,IOCOMP                                                   
         MVC   PERKCODE,0(R5)                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PERKEY,PERKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,PERKDA,AIOAREA1,DMWORK                 
         L     R2,AIOAREA1                                                      
         LA    RF,PERRFST                                                       
         USING PIDEL,RF                                                         
         SR    RE,RE                                                            
*                                                                               
RUNL40   CLI   PIDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PIDEL,PIDELQ                                                     
         JE    RUNL50                                                           
         IC    RE,PIDLN                                                         
         AR    RF,RE                                                            
         J     RUNL40                                                           
*                                                                               
RUNL50   LAY   R5,TARPIN                                                        
         MVC   0(L'TARPIN,R5),PIDNO                                             
         DROP  R2,RF                                                            
*                                                                               
RUNL60   LA    R1,ROUTS            ESTABLISH ROUT A-TYPES                       
         LA    R0,ROUTSN                                                        
         SR    RE,RE                                                            
         L     RF,=A(ROUT)                                                      
RUNL70   STCM  RE,1,0(R1)                                                       
         STCM  RF,7,1(R1)                                                       
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,RUNL70                                                        
                                                                                
         GOTO1 VDATCON,DMCB,(4,RCDATE),(1,TODAYD)  TODAY                        
         GOTO1 DATCON,DMCB,(4,RCDATE),(20,TODAYL)  TODAY                        
                                                                                
         XC    IOKEY1,IOKEY1                                                    
         LA    R2,IOKEY1                                                        
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVI   CPYKCPY,X'41'  COMPANY CODES RANGE FROM X'41' TO X'FD'           
         B     RUNL90                                                           
RUNL80   MVC   IOKEY1,CSVKEY1                                                   
         LA    R2,IOKEY1                                                        
         IC    RF,CPYKCPY                                                       
         LA    RF,1(,RF)                                                        
         STC   RF,CPYKCPY                                                       
RUNL90   GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CPYKEY,CPYKEY,0                       
         BE    RUNL100                                                          
         DC    H'0'                                                             
RUNL100  CLI   CPYKCPY,X'FE'       REACHED END OF COMPANY RECORDS?              
         BNE   RUNL110                                                          
         CLI   SORTSW,0            IS SORTER EMPTY?                             
         BE    EXIT                                                             
         GOTO1 AGETSORT                                                         
         B     EXIT                                                             
RUNL110  MVC   CSVKEY1,IOKEY1                                                   
RUNL120  CLC   CPYKEY+CPYKEND(L'CPYKEY-1),SPACES REST OF KEY NULL?              
         BNE   RUNL80              NO, NOT A COMPANY RECORD                     
         MVC   COCODE,CPYKCPY                                                   
         MVC   COMPDA,CPYKDA       SAVE DISK ADDRESS                            
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,COMPDA,AIOAREA1,DMWORK                 
         BE    RUNL130                                                          
         DC    H'0'                                                             
                                                                                
* CHECK IF EMAIL ALERTS FOR ESTIMATES WANTED                          *         
                                                                                
         SPACE 1                                                                
RUNL130  GOTO1 VHELLO,DMCB,(C'G',ACCMST),('CPXELQ',AIOAREA1),0,0                
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BNE   RUNL80              NO CPXEL FOUND; GO TO NEXT REC'D             
*                                                                               
         L     R3,12(,R1)          R3 CONTAINS ADDRESS OF CPXELD                
         USING CPXELD,R3                                                        
         TM    CPXSTAT1,CPXSJOEM   EMAIL ALERTS FOR ESTIMATES WANTED?           
         BZ    RUNL80              NO (DEFAULT): GO TO NEXT RECORD              
         MVC   CPXSTA,CPXSTATA                                                  
         DROP  R3                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('CPYELQ',AIOAREA1),0,0                
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BNE   RUNL80              NO CPYEL FOUND; GO TO NEXT REC'D             
         L     R3,12(,R1)          R3 CONTAINS ADDRESS OF CPXELD                
         USING CPYELD,R3                                                        
                                                                                
         MVC   SAVOFF,SPACES                                                    
         MVC   ALPCODE,CPYALPHA    EXTRACT AGENCY ALPHA CODE                    
         MVC   ORIGINUM,CPYUID     COMPANY PRINCIPAL ID NUMBER                  
         TM    CPYSTATC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   RUNL150                                                          
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
         CLI   MCS2ACCS,0                                                       
         BE    RUNL150                                                          
         TM    CPYSTAT4,CPYSOFF2   2 CHAR OFFICES                               
         BO    RUNL140                                                          
         CLI   MCS2ACCS,C'*'                                                    
         BNE   RUNL150                                                          
         MVC   SAVOFF,MCS2ACCS+1   CONNECTED USER-ID                            
         B     RUNL150                                                          
                                                                                
RUNL140  MVC   SAVOFF,MCS2ACCS+2                                                
         DROP  R8                                                               
                                                                                
RUNL150  L     R2,AIOAREA1                                                      
         LA    R3,CPYRFST                                                       
*                                                                               
         LA    R0,FEDURL                                                        
         LHI   R1,L'FEDURL                                                      
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING FFTELD,R3                                                        
RUNL160  CLI   FFTEL,0                                                          
         JE    RUNL190                                                          
         CLI   FFTEL,FFTELQ                                                     
         JE    RUNL180                                                          
RUNL170  LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         J     RUNL160                                                          
*                                                                               
RUNL180  CLI   FFTTYPE,FFTTFURL    CHECK FEDERATED URL ELEMENT                  
         JNE   RUNL170                                                          
         LLC   RF,FFTLN                                                         
         SHI   RF,1+FFTDATA-FFTELD                                              
         LA    RE,FEDURL                                                        
         MVC   0(0,RE),FFTDATA     EXTRACT FEDERATED URL                        
         EX    RF,*-6                                                           
*                                                                               
RUNL190  GOTO1 AGSECALP            GET SECURITY ALPHA ID                        
         GOTO1 AGETLANG            GET LANGUAGE                                 
         CLC   COMPLANG,LLANG      TEST LANGUAGE CHANGE                         
         BE    RUNL200             NO                                           
         GOTO1 ADDICTAT,DMCB,C'LL  ',DATI,DATO RESOLVE DATA DICT ITEMS          
         ORG   *-2                                                              
         MVC   3(L'COMPLANG,R1),COMPLANG SET LANGUAGE FOR DICTATE               
         BASR  RE,RF                                                            
         MVC   LLANG,COMPLANG      SAVE LANGUAGE CODE                           
         MVC   RCLANG,COMPLANG                                                  
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
         MVC   MCLANG,COMPLANG                                                  
         DROP  R3,R8                                                            
*                                                                               
         GOTO1 AGLDGSJ            GET SJ LEDGER LEVELS                          
         BNE   RUNL80                                                           
*                              ** READ ASSIGNED JOBS PASSIVES **                
         USING ASJPASD,R4                                                       
RUNL200  LA    R4,IOKEY2                                                        
         XC    ASJPAS,ASJPAS                                                    
         MVI   ASJPTYP,ASJPTYPQ                                                 
         MVI   ASJPSUB,ASJPSUBQ                                                 
         MVC   ASJPCPY,COCODE                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ASJPAS,ASJPAS,0                       
         BE    RUNL230                                                          
         DC    H'0'                                                             
*                                                                               
RUNL210  LA    R4,IOKEY2                                                        
         MVC   IOKEY2,CSVKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ASJPAS,ASJPAS,0                       
         BE    RUNL220                                                          
         DC    H'0'                                                             
*                                                                               
RUNL220  LA    R4,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,ASJPAS,ASJPAS,0                       
         BE    RUNL230                                                          
         DC    H'0'                                                             
*                                                                               
RUNL230  MVC   CSVKEY2,IOKEY2                                                   
         CLC   COCODE,ASJPCPY     FINISHED READING?                             
         BE    RUNL235                                                          
         CLI   SORTSW,0           IS SORTER EMPTY?                              
         BE    RUNL80                                                           
         GOTO1 AGETSORT                                                         
         B     RUNL80                                                           
*                                                                               
RUNL235  CLI   ASJPTYP,ASJPTYPQ                                                 
         BNE   RUNL220                                                          
         CLI   ASJPSUB,ASJPSUBQ                                                 
         BNE   RUNL220                                                          
         OC    ASJPPID,ASJPPID                                                  
         BZ    RUNL220                                                          
         MVC   SVACOD,ASJPACT                                                   
         MVC   SVBPID,ASJPPID                                                   
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ASJPDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSTELQ',AIOAREA1),0,0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'               NO RSTELD ELEMENT                             
                                                                                
         USING RSTELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   SVDATE,RSTTDATE                                                  
         DROP  R4                                                               
                                                                                
RUNL240  L     R0,ADGOBLOC                                                      
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*&&US                                                                           
         L     R0,AGOXBLCK                                                      
         LHI   R1,GOXBLKX-GOXBLOCK                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*&&                                                                             
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(L'COCODE),COCODE CPY                                    
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL UNIT+LDG          
         MVI   GOWHICH,GOWHALL                                                  
         LA    RF,SVACOD                                                        
         LLC   R1,PCLILEN                                                       
         BCTR  R1,0                                                             
         MVC   GOSELCLI(0),0(RF)   MOVE IN CLIENT                               
         EX    R1,*-6                                                           
         OC    GOSELCLI,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
                                                                                
         LLC   R1,PPROLEN                                                       
         BCTR  R1,0                                                             
         MVC   GOSELPRO(0),0(RF)   MOVE IN PRODUCT                              
         EX    R1,*-6                                                           
         OC    GOSELPRO,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
                                                                                
         LLC   R1,PJOBLEN                                                       
         BCTR  R1,0                                                             
         MVC   GOSELJOB(0),0(RF)   MOVE IN JOB                                  
         EX    R1,*-6                                                           
         OC    GOSELJOB,SPACES                                                  
         MVC   GOSELMED,GOSELJOB                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   GOABEXT,ACMAGOBB    PUT EXTENSION AND BILLING BLOCK              
*&&UK*&& MVC   GOAEXT,ACMAGOX                                                   
*&&US*&& MVC   GOAEXT,AGOXBLCK                                                  
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
*&&US                                                                           
         L     RF,AGOXBLCK                                                      
         USING GOXBLOCKD,RF                                                     
*&&                                                                             
         CP    GOCERCHD,PZERO      CURRENT ESTIMATE % THRESHOLD                 
         BNH   RUNL250                                                          
         GOTO1 AESPSORT                                                         
         DROP  RF                                                               
*                                                                               
RUNL250  XR    RF,RF                                                            
         ICM   RF,3,GOINACTV                                                    
         LTR   RF,RF               NUMBER OF DAYS INACTIVE THRESHOLD            
         BZ    RUNL210             NOT INTERESTED, GOTO NEXT JOB                
         GOTO1 ADAYSORT                                                         
         B     RUNL210                                                          
         DROP  R2,R6                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* SUBROUTINES - BRANCH INDEX IN H.O.B RF                              *         
***********************************************************************         
         DS    0D                                                               
ROUT     NMOD1 0,**ROUT**,R9,R8                                                 
         LA    RC,SPACEND                                                       
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     ESPSORT             ADD TO SORTER (ESTIMATE %)                   
         B     DAYSORT             ADD TO SORTER (DAYS INACTIVE)                
         B     PUTSRT              PUT RECORDS TO SORTER                        
         B     GETSORT             GET RECORDS FROM SORT                        
         B     PROETAB             PROCESS EMAIL TABLE                          
         B     PUTETAB             PUT RECORD TO EMAIL TABLE                    
         B     GETEML              GET EMAIL ADDRESS OF APPROVER                
         B     GLDGSJ              GET SJ LEDGER LENGTHS                        
         B     GSECALP             GET SECURITY ALPHA ID                        
         B     GETLANG             GET LANGUAGE                                 
         B     PRTDET              PRINT DETAILS OF EMAIL SENT                  
         B     ESTINA              CHECK % EST / DAYS INACTIVE                  
         B     CLPRJO              GET JOB NAME                                 
         B     FLTORD              FILTER ORDER STATUS                          
         B     DOEMADD             FILL OUT EMAIL ADDRESS                       
         B     SETURL              BUILD URL                                    
                                                                                
ROUTL    CLI   *,255               SET CC LOW                                   
         B     ROUTX                                                            
*                                                                               
ROUTH    CLI   *,0                 SET CC HIGH                                  
         B     ROUTX                                                            
*                                                                               
ROUTE    CLI   *+1,0               SET CC EQUAL                                 
*                                                                               
ROUTX    XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
URIENC   DC    V(URIENC)                                                        
MAXELEN  EQU   160                 MAXIMUM LENGTH OF EMAIL LINE                 
         SPACE 1                                                                
TBROPE   DC    C'<tr>'                                                          
TBRCLO   DC    C'</tr>'                                                         
TBDOPE   DC    C'<td>'                                                          
TBDCLO   DC    C'</td>'                                                         
TBHOPE   DC    C'<th>'                                                          
TBHCLO   DC    C'</th>'                                                         
LINKOP   DC    C'<a style="text-decoration:none;" href="'                       
LINKCB   DC    C'">'                                                            
LINKCL   DC    C'</a>'                                                          
PRODUL   DC    C'SJ'                                                            
ACCARC   DC    C'ACCARC '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
CTFILE   DC    C'CTFILE '                                                       
JESMAIL  DC    C'JESMAIL '                                                      
ACAPTEAM DC    A(CAPTEAM)                                                       
AELEMT   DC    A(ELEMT)                                                         
AELEMTML DC    A(ELEMTML)                                                       
AELEMSEC DC    A(ELEMSECT)                                                      
AELEMSEN DC    A(ELEMSEND)                                                      
AELE140E DC    A(ELEM140E)                                                      
AELE265E DC    A(ELEM265E)                                                      
AELE276E DC    A(ELEM276E)                                                      
AELE278E DC    A(ELEM278E)                                                      
AELE271E DC    A(ELEM271E)                                                      
AELE272E DC    A(ELEM272E)                                                      
AELE273E DC    A(ELEM273E)                                                      
AELE280E DC    A(ELEM280E)                                                      
AELE370E DC    A(ELEM370E)                                                      
AELE790E DC    A(ELEM790E)                                                      
AELE800E DC    A(ELEM800E)                                                      
AELE805E DC    A(ELEM805E)                                                      
AELE840E DC    A(ELEM840E)                                                      
AEMAIL1  DC    A(EMAILL1)                                                       
AEMAIL2  DC    A(EMAILL2)                                                       
AEMAIL3  DC    A(EMAILL3)                                                       
AEMAIL4  DC    A(EMAILL4)                                                       
AEMAIL5  DC    A(EMAILL5)                                                       
AEMAIL6  DC    A(EMAILL6)                                                       
AEMAIL7  DC    A(EMAILL7)                                                       
AEMAIL8  DC    A(EMAILL8)                                                       
AEMAIL9  DC    A(EMAILL9)                                                       
AEMAILA  DC    A(EMAILLA)                                                       
*                                                                               
AUFROM   DC    C'AuraNotifications<AuraNotifications@mediaocean.com>'           
AUADDR   DC    C'MEDIAOCEAN | 120 BROADWAY | NEW YORK, NY 10271'                
AUHTML   DC    C'www.mediaocean.com'                                            
HTTP     DC    C'http://'                                                       
*                                                                               
DISPURLJ DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-jobs'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-jobs'                                              
         DC    X'50'                                                            
         DC    C'route=jobs/display/jobDisplay'                                 
         DC    X'50'                                                            
DISPURJL EQU   *-DISPURLJ                                                       
*                                                                               
CLITXT   DC    C'client='                                                       
PROTXT   DC    C'product='                                                      
JOBTXT   DC    C'job='                                                          
*                                                                               
SVFED    DS    CL1                 Saved fed system flag                        
SVTST    DS    CL1                 Saved test system flag                       
SVENV    DS    CL(L'AGUENV)        URL prefix                                   
SVHTTP   DS    CL(L'AGUHTT)        http/https                                   
SVURL    DS    CL(L'AGUURL2)       remaining URL                                
SVFEDP   DS    CL(L'AGUFEDP)       Federated prefix                             
SVFEDS   DS    CL(L'AGUFEDS)       Federated suffix                             
FEDURL   DS    CL250                                                            
*                                                                               
SORTCARD DS    CL80                SORT RECORD FIELDS                           
SORTTYPE DS    CL80                                                             
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
SORTEND  DC    C'END'                                                           
*                                                                               
PZERO    DC    PL8'0'                                                           
*                                                                               
TEXTSPCS DC    CL(MAXELEN)' '                                                   
                                                                                
*&&US                                                                           
LOOKFLDH DC    AL1(8+L'LOOKFLD),4X'00',AL1(L'LOOKFLD),AL2(0)                    
LOOKFLD  DC    C'OE,CE,HR'                                                      
*OOKFLDH DC    AL2(AC#OEN,AC#OEG,AC#OEC,AC#CEN,AC#CEG,AC#CEC,0)                 
*&&                                                                             
*&&UK                                                                           
JOBFLDS  DC    AL2(AC#OE,AC#CE,0)                                               
VCASHVAL DC    V(CASHVAL)                                                       
VTOBACCO DC    V(TOBACCO)                                                       
*&&                                                                             
VSQUASH  DC    V(SQUASHER)                                                      
VHELLO   DC    V(HELLO)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VDATCON  DC    V(DATCON)                                                        
VPERVERT DC    V(PERVERT)                                                       
*                                                                               
ACOLIST  DC    A(COLIST)                                                        
ACOLTAB  DC    A(COLTAB)                                                        
*&&US                                                                           
AGOXBLCK DC    A(GOXBLCKA)         ADDRESS OF GOXBLOCK                          
*&&                                                                             
AELEAREA DC    A(ELEAREA)                                                       
                                                                                
DATI     DS    0X                                                               
         DCDDL AC#JOBC,8           JOB CODE                                     
         DCDDL AC#JOBN,8           JOB NAME                                     
         DCDDL AC#EST,8            ESTIMATE                                     
         DCDDL AC#CMITD,9          COMMITTED                                    
         DCDDL AC#UCMD,11          UNCOMMITTED                                  
         DCDDL AC#SCLPO,20         ESTIMATE % COMMITTED                         
         DCDDL AC#NDINA,23         NUMBER OF DAYS INACTIVE                      
         DCDDL AC#CEHDR,80         CE HEADER LINE                               
         DCDDL AC#INHDR,80         NO. OF INACTIVE DAYS HEADER LINE             
         DCDDL AC#OTJBS,68         YOU HAVE OTHER JOBS TO ACTION...             
         DCDDL AC#DJRQA,80         HERE ARE DETAILS OF JOBS...                  
         DCDDL AC#PCHJS,37         PLSE CLICK HERE TO ACCESS YOUR JOBS.         
         DCDDL AC#EANFF,30         EMAIL ADDRESS NOT FOUND FOR...               
         DCDDL AC#JEMAL,17         JOB ALERTS                                   
         DCDDL AC#EHBST,22         EMAIL HAS BEEN SENT TO                       
         DCDDL AC#EMLL1,80         STANDARD EMAIL LINE 1                        
         DCDDL AC#EMLL2,80         STANDARD EMAIL LINE 2                        
         DCDDL AC#EMLL3,80         STANDARD EMAIL LINE 3                        
         DCDDL AC#EMLL4,80         STANDARD EMAIL LINE 4                        
         DCDDL AC#EMLL5,80         STANDARD EMAIL LINE 5                        
         DCDDL AC#EMLL6,80         STANDARD EMAIL LINE 6                        
         DCDDL AC#EMLL7,80         STANDARD EMAIL LINE 7                        
         DCDDL AC#EMLL8,80         STANDARD EMAIL LINE 8                        
         DCDDL AC#EMLL9,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#MODE,65          MEDIAOCEAN DE                                
*&&UK*&& DCDDL AC#MOUK,72          MEDIAOCEAN UK                                
*&&US*&& DCDDL AC#MOCA,80          MEDIAOCEAN CA                                
         DCDDL AC#FUSAT,18         FIND US AT                                   
*&&UK*&& DCDDL AC#MUKHT,20         MEDIAOCEAN UK/GERMAN WEBSITE ADDRES          
*&&US*&& DCDDL AC#MCAHT,20         MEDIAOCEAN CANADA    WEBSITE ADDRES          
         DCDDL AC#EMNL1,80         STANDARD EMAIL LINE 1                        
         DCDDL AC#EMNL2,80         STANDARD EMAIL LINE 2                        
         DCDDL AC#EMNL3,80         STANDARD EMAIL LINE 3                        
         DCDDL AC#EMNL4,80         STANDARD EMAIL LINE 4                        
         DCDDL AC#EMNL5,80         STANDARD EMAIL LINE 5                        
         DCDDL AC#EMNL6,80         STANDARD EMAIL LINE 6                        
         DCDDL AC#EMNL7,80         STANDARD EMAIL LINE 7                        
         DCDDL AC#EMNL8,80         STANDARD EMAIL LINE 8                        
         DCDDL AC#EMNL9,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#EMNLA,80         STANDARD EMAIL LINE 9                        
DATIX    DC    X'00'                                                            
*                                                                               
TSTBL    DS    0X                  TEST FILE SE NUMBERS                         
         DC    X'3F'               ACCX                                         
         DC    X'66'               ACC0                                         
         DC    X'67'               ACCS                                         
         DC    X'71'               ACC@@                                        
TSTBLN   EQU   (*-TSTBL)/L'TSTBL                                                
*                                                                               
EMAILMAX EQU  20000                MAX NUMBER OF EMAILS                         
CAPMAX   EQU  140                  MAX NUMBER OF CAPTEAM ENTRIES                
YESQ     EQU  C'Y'                                                              
NOQ      EQU  C'N'                                                              
*                                  ((14X4)+14)X2                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD SORT RECORD FOR 'ESTIMATE % REACHED'               *         
***********************************************************************         
         SPACE 1                                                                
ESPSORT  XC    SORTREC,SORTREC                                                  
         MVC   SORTCPY,COCODE      COMPANY CODE                                 
         MVI   SORTDOE,SORTEST     'S' FOR ESTIMATES                            
         MVC   SORTBPID,SVBPID     PERSON BINARY PID                            
         MVC   SORTJOB,SVACOD      JOB CODE                                     
*&&US                                                                           
         L     RF,AGOXBLCK                                                      
         USING GOXBLOCKD,RF                                                     
*&&                                                                             
*&&UK                                                                           
         L     RF,ADGOBLOC                                                      
         USING GOBLOCKD,RF                                                      
*&&                                                                             
         ZAP   SORTCER,GOCERCHD    ERM OPT/MAINT SETTING                        
         GOTO1 APUTSRT                                                          
         B     ROUTE                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO BUILD SORT RECORD FOR 'DAYS INACTIVE'                    *         
***********************************************************************         
         SPACE 1                                                                
         USING GOBLOCKD,RF                                                      
DAYSORT  L     RF,ADGOBLOC                                                      
         XR    R5,R5                                                            
         ICM   R5,3,GOINACTV                                                    
         LTR   R5,R5                                                            
         BZ    ROUTE                                                            
         DROP  RF                                                               
         GOTO1 DATCON,DMCB,(1,SVDATE),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,(R5)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+6)                                
         CLC   WORK+6(L'TODAYD),TODAYD                                          
         BNL   ROUTE                                                            
         XC    SORTREC,SORTREC                                                  
         MVC   SORTCPY,COCODE                                                   
         MVI   SORTDOE,SORTINA     'I' FOR DAYS INACTIVE                        
         MVC   SORTBPID,SVBPID                                                  
         MVC   SORTJOB,SVACOD                                                   
         GOTO1 DATCON,DMCB,(1,TODAYD),(0,WORK+6)                                
         GOTO1 VPERVERT,DMCB,WORK,WORK+6                                        
         XR    RE,RE                                                            
         ICM   RE,3,8(R1)                                                       
         SHI   RE,2                                                             
         STH   RE,SORTNDA          NUMBER OF DAYS INACTIVE                      
         GOTO1 APUTSRT                                                          
         B     ROUTE                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PUT RECORDS TO SORTER                                    *         
***********************************************************************         
         SPACE 1                                                                
PUTSRT   CLI   SORTSW,0            TEST SORT INITIALISED                        
         BNE   PSRT02                                                           
         MVI   SORTSW,1            SET SORT INITIALISED                         
         LA    R5,SORTCARD                                                      
         LA    R6,SORTTYPE                                                      
         MVC   0(L'SORTCARD,R5),SPACES                                          
         MVC   0(L'SORTTYPE,R6),SPACES                                          
         MVC   0(13,R5),=C'SORT FIELDS=('                                       
         LA    RF,13(R5)           A(FIRST SORTCARD ENTRY)                      
         MVI   0(RF),C'1'          DISPLACEMENT OF 1                            
         AHI   RF,1                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         LHI   RE,SORTKEYL                                                      
         CVD   RE,DUB                                                           
         UNPK  0(2,RF),DUB                                                      
         LTR   RE,RE                                                            
         JZ    *+12                                                             
         OI    1(RF),X'F0'                                                      
         AHI   RF,2                                                             
         MVC   0(2,RF),=C',A'                                                   
         AHI   RF,2                                                             
         MVC   0(18,RF),=C'),FORMAT=BI,WORK=1'                                  
*                                                                               
         MVC   0(29,R6),=C'RECORD TYPE=F,LENGTH=(00,,,,)'                       
         LHI   R1,SORTRECL                                                      
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  22(2,R6),DUB+6(2)                                                
         MVI   SORTSW,1            SET SORT INITIALISED                         
         GOTO1 ADSORTER,DMCB,(R5),(R6)                                          
*                                                                               
PSRT02   GOTO1 ADSORTER,DMCB,SORTPUT,SORTREC                                    
         B     ROUTE                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* GET RECORDS BACK FROM PRELIMINARY SORTING                           *         
* CALLED FOR EACH COMPANY THAT IS PROCESSED                           *         
***********************************************************************         
         SPACE 1                                                                
GETSORT  L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMLOOP,NUMLOOP                                                  
         XC    NUMEMLS,NUMEMLS                                                  
         XC    NUMRECS,NUMRECS                                                  
         XC    RUNIND1,RUNIND1                                                  
         XC    SVBPID,SVBPID                                                    
*&&US                                                                           
         CLI   PARMDPQ,YESQ        SUPPRESS FROM GOING TO PRINT QUEUE?          
         JE    GET01               . YES                                        
*                                                                               
         CLI   RCWRITE,NOQ         DON'T WRITE TO COMPANY'S PRINT QUEUE         
         JE    GET01               IF WRITE=NO                                  
*&&                                                                             
         L     RF,REMOTEC          REPORTING ON CLIENT PQ                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   START A NEW REPORT                           
         MVI   REMOTSYS,C'A'       A=ACCOUNTING                                 
         MVC   REMOTPRG,=C'EW'     THE 'EW' FROM PHASE NAME                     
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDST,ORIGINUM   USE PRINCIPAL ID NUMBER FROM CO RECS         
         MVC   REMOTFRM,=C'AEW '                                                
         MVI   RCSUBPRG,0                                                       
         DROP  RF                                                               
*                                                                               
GET01    LA    RF,AAURURL                                                       
         GOTO1 VGETURL,DMCB,(RF)                                                
         L     RE,ADMASTC          GET THE UTL ADDRESS                          
         L     RE,MCUTL-MASTD(RE)                                               
         MVC   SVURL,TEXTSPCS                                                   
*                                                                               
         L     R1,AAURURL          PULL URL FROM TABLE                          
         USING AGYURLD,R1                                                       
GET02    CLI   AGUSE,0             TEST OR LIVE                                 
         JE    GET04               LIVE                                         
         CLC   AGUSE,4(RE)         FIND MATCH ON SE NUMBER FOR TEST             
         JE    GET08                                                            
         J     GET06                                                            
*                                                                               
GET04    CLI   AGUAA,0             END OF TABLE, USE DEFAULT                    
         JE    GET08                                                            
         CLC   AGUAA,ALPCODE       MATCH ON AGENCY ALPHA                        
         JE    GET08                                                            
GET06    AHI   R1,AGYURLQ          NO MATCH, NEXT ENTRY                         
         J     GET02                                                            
*                                                                               
GET08    MVC   SVURL,AGUURL2                                                    
         MVC   SVHTTP,AGUHTT                                                    
         MVC   SVENV,AGUENV                                                     
         MVC   SVFED,AGUFED                                                     
         MVC   SVFEDP,AGUFEDP                                                   
         MVC   SVFEDS,AGUFEDS                                                   
         MVC   SVTST,AGUTST                                                     
*&&UK                                UK/German web address                      
         L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
         L     RF,AELE840E                                                      
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
*&&                                                                             
         L     RF,AELE790E                                                      
*&&UK                                                                           
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RF),AC@MOUK                                          
*                                                                               
         CLI   COMPCTRY,CTRYGBR      UK address                                 
         JE    GET16                                                            
         CLI   COMPCTRY,CTRYDFL                                                 
         JE    GET16                                                            
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RF),AC@MODE                                          
         CLI   COMPCTRY,CTRYGER      German address                             
         JE    GET16                                                            
*&&                                                                             
*&&US                                                                           
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RF),AC@MOCA                                          
         CLI   COMPCTRY,CTRYCAN      Canadian address                           
         JE    GET10                                                            
*&&                                  Default is US address                      
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AUADDR,RF),AUADDR                                            
*                                                                               
GET10    L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         LA    RE,AUHTML             Assume its US                              
*&&US                                                                           
         CLI   COMPCTRY,CTRYCAN      Canadian address                           
         JNE   *+8                                                              
         LA    RE,AC@MCAHT                                                      
         MVC   0(L'AUHTML,RF),0(RE)                                             
*&&                                                                             
         L     RF,AELE840E                                                      
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         MVC   0(L'AUHTML,RF),0(RE)                                             
*                                                                               
GET16    L     RF,AELE805E          Find us here....                            
         MVC   0(L'ELEM805E,RF),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,RF),AC@FUSAT                                        
*                                                                               
         L     RF,AELE800E         Put year in copyright                        
         MVC   0(L'ELEM800E,RF),TODAYL                                          
*                                                                               
         L     RF,AELE265E                                                      
         MVC   0(L'ELEM265E,RF),TEXTSPCS                                        
         MVC   0(L'AC@DJRQA,RF),AC@DJRQA    Here are the details                
*                                  STORE URL FOR USE LATER (IN SVURL)           
         L     RF,AEMAIL1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
*                                                                               
         L     RF,AEMAIL2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
*                                                                               
         L     RF,AEMAIL3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
*                                                                               
         L     RF,AEMAIL4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
*                                                                               
         L     RF,AEMAIL5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
*                                                                               
         L     RF,AEMAIL6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
*                                                                               
         L     RF,AEMAIL7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
*                                                                               
         L     RF,AEMAIL8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
*                                                                               
         L     RF,AEMAIL9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
*                                                                               
         L     RF,AEMAILA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
*                                                                               
         CLI   RCWRITE,NOQ                                                      
         BE    GET18                                                            
         GOTO1 VSMTP,DMCB,('SMTPAINI',0) INITIALISE JESMAIL                     
         GOTO1 VSMTP,DMCB,('SMTPASLL',0)        SET LONG LINES                  
         GOTO1 VSMTP,DMCB,('SMTPHTMH',0) SET HTML HEADER                        
         GOTO1 VSMTP,DMCB,('SMTPAFR2',0),(L'AUFROM,AUFROM) Set from add         
*                                                                               
GET18    GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RF,15,4(R1)         4(R1) CONTAINS ADDRESS OF RECORD             
         BNZ   GET20               IF ZERO, THEN AT EOF                         
         NI    RUNIND1,X'FF'-RUNINOEM                                           
         GOTO1 APROETAB            PROCESS EMAIL TABLE                          
         GOTO1 ADSORTER,DMCB,SORTEND                                            
         MVI   SORTSW,0                                                         
         CLI   RCWRITE,NOQ                                                      
         BE    ROUTE                                                            
         GOTO1 VSMTP,DMCB,('SMTPAEND',0) DETACH FROM JESMAIL                    
         B     ROUTE                                                            
*                                                                               
GET20    MVC   SORTREC,0(RF)                                                    
                                                                                
GET22    CLC   SVBPID,SORTBPID                                                  
         BNE   GET26                                                            
         TM    RUNIND1,RUNINOEM                                                 
         BNO   GET28                                                            
         B     GET18                                                            
GET26    MVC   SVBPID,SORTBPID                                                  
         NI    RUNIND1,X'FF'-RUNINOEM                                           
GET28    GOTO1 AESTINA             CHECK IF EST % REACHED                       
         BL    GET18                                                            
         GOTO1 AGETEML             GET EMAIL ADDRESS                            
         OC    PEREMAIL,PEREMAIL                                                
         BNZ   GET30                                                            
         OI    RUNIND1,RUNINOEM                                                 
         MVC   P+1(L'AC@EANFF),AC@EANFF                                         
         MVC   P+32(L'PIDCHAR),PIDCHAR                                          
         MVI   P+47,C'('                                                        
         XOUT  SVBPID,P+48,2                                                    
         MVI   P+52,C')'                                                        
         GOTO1 ACREPORT                                                         
         B     GET18                                                            
*                                                                               
GET30    GOTO1 ACLPRJO             GET CLI/PRO/JOB NAMES                        
         GOTO1 APUTETAB            PUT DETAILS INTO EMAIL TABLE                 
         B     GET18               GO TO NEXT SORTED RECORD                     
         EJECT                                                                  
***********************************************************************         
* CHECK % ESTIMATE OR DAYS INACTIVE                                             
***********************************************************************         
         SPACE 1                                                                
ESTINA   CLI   SORTDOE,SORTINA                                                  
         BE    ESTIN80                                                          
         TM    PROFSW,PRAGCY       ALREADY READ AGENCY PROFILES?                
         BO    ESTIN01A            YES                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+00,C'A'-X'40'                                            
         MVC   ELEMENT+01(3),=C'PO2' PAGE 2                                     
         MVC   ELEMENT+12(2),ALPCODE                                            
         GOTO1 GETPROF,DMCB,ELEMENT,AGYPROF2,DATAMGR                            
*                                                                               
         MVC   ELEMENT+01(3),=C'PO3' PAGE 3                                     
         GOTO1 GETPROF,DMCB,ELEMENT,AGYPROF3,DATAMGR                            
*                                                                               
ESTIN01  TM    PROFSW,PRAGCY                                                    
         BO    ESTIN01A                                                         
         OI    PROFSW,PRAGCY                                                    
         B     ESTIN01B                                                         
ESTIN01A TM    PROFSW,PRUSID                                                    
         BNO   ESTIN01B                                                         
ESTIN01B XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+00,C'A'-X'40'                                            
         MVC   ELEMENT+01(3),=C'PO2' PAGE 2                                     
         MVC   ELEMENT+12(2),ORIGINUM                                           
         GOTO1 GETPROF,DMCB,ELEMENT,USRPROF2,DATAMGR                            
*                                                                               
         OC    USRPROF2,USRPROF2                                                
         BNZ   *+10                                                             
         MVC   USRPROF2,AGYPROF2   USE AGENCY PROFILES IF NONE FOR USER         
*                                                                               
         MVC   ELEMENT+01(3),=C'PO3' PAGE 3                                     
         GOTO1 GETPROF,DMCB,ELEMENT,USRPROF3,DATAMGR                            
         OI    PROFSW,PRUSID                                                    
*                                                                               
         OC    USRPROF3,USRPROF3                                                
         BNZ   *+10                                                             
         MVC   USRPROF3,AGYPROF3   USE AGENCY PROFILES IF NONE FOR USER         
*                                                                               
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(L'COCODE),SORTCPY COMPANY                               
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL UNIT+LDG          
         MVI   GOWHICH,GOWHALL                                                  
         LA    RF,SORTJOB                                                       
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         MVC   GOSELCLI(0),0(RF)   MOVE IN CLIENT                               
         EX    R1,*-6                                                           
         OC    GOSELCLI,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
                                                                                
         IC    R1,PPROLEN                                                       
         SHI   R1,1                                                             
         MVC   GOSELPRO(0),0(RF)   MOVE IN PRODUCT                              
         EX    R1,*-6                                                           
         OC    GOSELPRO,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
                                                                                
         IC    R1,PJOBLEN                                                       
         SHI   R1,1                                                             
         MVC   GOSELJOB(0),0(RF)   MOVE IN JOB                                  
         EX    R1,*-6                                                           
         MVC   GOSELMED,0(RF)      AND MEDIA                                    
         OC    GOSELJOB,SPACES                                                  
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   GOABEXT,ACMAGOBB    PUT EXTENSION AND BILLING BLOCK              
*&&US*&& MVC   GOAEXT,AGOXBLCK                                                  
*&&UK*&& MVC   GOAEXT,ACMAGOX                                                   
         DROP  RF                                                               
                                                                                
         GOTO1 GETOPT,DMCB,(R2)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ESTIN01C ZAP   XTOTCE,PZERO                                                     
         ZAP   XTOTOR,PZERO                                                     
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY2           READ FOR ORDER TRANSACTIONS                  
         MVC   SAVEORD,SPACES                                                   
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,SORTCPY                                                  
         MVC   TRNKUNT(2),PRODUL                                                
         MVC   TRNKACT,SORTJOB                                                  
         MVC   TRNKWORK,=C'**'                                                  
         MVC   CSVKEY2,TRNKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TRNKEY,TRNKEY,0                       
         BE    ESTIN04                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN02  LA    R2,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,TRNKEY,TRNKEY,0                       
         BE    ESTIN04                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN04  CLC   CSVKEY2(TRNKCULC-TRNRECD),TRNKEY                                 
         BNE   ESTIN08                                                          
         CLC   TRNKREF,SPACES      SKIP OTHER RECORDS                           
         BNH   ESTIN02                                                          
         MVC   CSVKEY3(L'TRNKEY),TRNKEY                                         
         LA    R4,ACCMST                                                        
*&&UK                                                                           
         TM    TRNKSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         LA    R4,ACCARC                                                        
*&&                                                                             
         GOTO1 DATAMGR,DMCB,DMGET,0(R4),TRNKDA,AIOAREA1,DMWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING OAMELD,R3                                                        
         L     R2,AIOAREA1                                                      
         LA    R3,TRNRFST                                                       
         XR    R0,R0                                                            
         CLI   OAMEL,TRNELQ                                                     
         BNE   ESTIN02                                                          
*                                                                               
ESTIN06  IC    R0,OAMLN                                                         
         AR    R3,R0                                                            
         CLI   OAMEL,0                                                          
         BE    ESTIN02                                                          
         CLI   OAMEL,OAMELQ                                                     
         BNE   ESTIN06                                                          
                                                                                
         AP    XTOTOR,OAMAMNT                                                   
         SP    XTOTOR,OAMIVAL                                                   
         MVC   SAVEORD,TRNKREF     SAVE OFF ORDER NO. TO STOP DOUBLING          
         B     ESTIN06                                                          
*                                                                               
ESTIN08  LA    R2,IOKEY2           READ FOR CHARGES TRANSACTIONS                
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,SORTCPY                                                  
         MVC   TRNKUNT(2),PRODUL                                                
         MVC   TRNKACT,SORTJOB                                                  
         MVC   TRNKWORK,=C'**'                                                  
         MVI   TRNKCCPY,X'FF'                                                   
         MVC   CSVKEY2,TRNKEY                                                   
*                                                                               
ESTIN10  GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TRNKEY,TRNKEY,0                       
         BE    ESTIN14                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN12  LA    R2,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,TRNKEY,TRNKEY,0                       
         BE    ESTIN14                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN14  CLC   TRNKCULA,CSVKEY2                                                 
         BNE   ESTIN20                                                          
         CLC   TRNKWORK,=C'99'                                                  
         BE    ESTIN20                                                          
*                                                                               
         CLC   TRNKREF,SPACES      SKIP OTHER RECORDS                           
         BNH   ESTIN12                                                          
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT                                       
         BNZ   ESTIN12                                                          
                                                                                
         TM    TRNKSTAT,TRNSARCH                                                
         BZ    ESTIN18A                                                         
         GOTO1 DATAMGR,DMCB,DMGET,ACCARC,TRNKDA,AIOAREA1,DMWORK                 
         BE    ESTIN18B                                                         
         DC    H'0'                                                             
ESTIN18A GOTO1 DATAMGR,DMCB,DMGET,ACCMST,TRNKDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,R3                                                        
ESTIN18B L     R2,AIOAREA1                                                      
         LA    R3,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         BNE   ESTIN12                                                          
         TM    TRNSTAT,TRNSDR                                                   
         BZ    ESTIN12                                                          
         AP    XTOTOR,TRNAMNT                                                   
         B     ESTIN12                                                          
         DROP  R2,R3                                                            
*                                                                               
         USING OSJPASD,R2                                                       
ESTIN20  LA    R2,IOKEY2           read for other orders to this job            
         XC    OSJPAS,OSJPAS       (doesn't take est # on order into            
         MVI   OSJPTYP,OSJPTYPQ    account)                                     
         MVI   OSJPSUB,OSJPSUBQ    and add amounts to ESTCHK table              
         MVC   OSJPCPY,SORTCPY                                                  
         MVC   OSJPACT,SORTJOB                                                  
         MVI   OSJPMEM,0                                                        
         MVC   CSVKEY2,OSJPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,OSJPAS,OSJPAS,0                       
         BE    ESTIN24                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN22  LA    R2,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,OSJPAS,OSJPAS,0                       
         BE    ESTIN24                                                          
         DC    H'0'                                                             
*                                                                               
ESTIN24  CLC   CSVKEY2(OSJPORD-OSJPASD),OSJPAS                                  
         BNE   ESTIN34                                                          
         TM    OSJPSTAT,ORDSLDEL+ORDSDEL                                        
         BNZ   ESTIN22                                                          
         MVC   CSVKEY3(L'OSJPAS),OSJPAS                                         
         GOTO1 AFLTORD,DMCB,OSJPAS  filter orders                               
         BNE   ESTIN22                                                          
         CLC   SAVEORD,SPACES       ANY SAVED ORDER NUMBER?                     
         BNH   *+14                                                             
         CLC   SAVEORD,OSJPORD      SAME ORDER AS READ BEFORE                   
         BE    ESTIN22              DON'T READ IT THEN!!!                       
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,OSJPDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ORDRECD,R2                                                       
         L     R2,AIOAREA1                                                      
         USING OAMELD,R4                                                        
         LA    R4,ORDRFST                                                       
         XR    R0,R0                                                            
ESTIN26  CLI   OAMEL,0                                                          
         BE    ESTIN22                                                          
         CLI   OAMEL,OAMELQ                                                     
         BNE   ESTIN32                                                          
                                                                                
         AP    XTOTOR,OAMAMNT                                                   
         SP    XTOTOR,OAMIVAL                                                   
ESTIN32  IC    R0,OAMLN                                                         
         AR    R4,R0                                                            
         B     ESTIN26                                                          
         DROP  R4,R2                                                            
*                                                                               
         USING ACTRECD,R6                                                       
ESTIN34  LA    R6,IOKEY2                                                        
         MVC   ACTKEY,SPACES       READ JOB INTO AIOAREA1                       
         MVC   ACTKCPY,SORTCPY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         MVC   ACTKACT,SORTJOB                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'NEWO',,AIOAREA1,AIOAREA1                         
         ORG   *-2                                                              
         LR    R2,R1               (ACCEMU requires R2=A(DMCB))                 
         O     R2,=X'FF000000'                                                  
         BASR  RE,RF               convert job record to emulated               
         DROP  R6                                                               
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
*&&UK*&& GOTO1 ACMAJOBL,DMCB,(X'FF',JOBFLDS),ACOLIST,ADCOMFAC                   
*&&US*&& GOTO1 ACMAJOBL,DMCB,LOOKFLDH,ACOLIST,ADCOMFAC                          
         CLI   4(R1),0                                                          
*&&UK*&& BE    *+6                                                              
*&&US*&& BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ACMAJOBB                                                      
         USING JBLOCKD,R3                                                       
         MVC   JBAJOB,AIOAREA1                                                  
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ADCOMFAC                                                  
*&&UK*&& MVC   JBCSHVAL,VCASHVAL                                                
*&&UK*&& MVC   JBTOBACO,VTOBACCO                                                
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,AIOAREA2                                                   
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBAOPVTB,AELEAREA   use ELEAREA here                             
         LHI   RE,L'ELEAREA                                                     
         ST    RE,JBLOPVTB                                                      
         MVC   JBACOLTB,ACOLTAB                                                 
         LA    RE,L'COLTAB                                                      
         ST    RE,JBLCOLTB                                                      
         MVI   JBSELFUN,JBGETDE    MCS: GET DETAILS AND SET ORIGINAL            
*&&UK*&& LA    RE,JOBFLDS          COLUMN LIST  ADDRESS (OE/CE)                 
*&&US*&& LA    RE,LOOKFLDH         COLUMN LIST  ADDRESS (OE/CE)                 
         ST    RE,JBORICLI                                                      
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         CLI   JBNEWEST,JBMCSQ     FOR MCS ESTIMATES                            
         BE    ESTIN54                                                          
*                                                                               
         L     R4,JBACOLTB         R4=A(COLUMN TABLE)                           
         USING JBCOLD,R4                                                        
         XR    R2,R2                                                            
         ICM   R2,3,JBNROWS        R2=(LOOP COUNTER)                            
         BZ    ESTIN52                                                          
         DS    0H                  PUT VALUES TO EST. CHECK TABLE               
*                                                                               
ESTIN36  CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORK CODE ENTRY                     
         BNE   ESTIN50             NO-SKIP IT                                   
*                                                                               
         AP    XTOTCE,JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL)                           
ESTIN50  AH    R4,JBLCOL           Next table entry                             
         BCT   R2,ESTIN36                                                       
*                                                                               
ESTIN52  DS    0H                                                               
         B     ESTIN70                                                          
         DROP  R4                                                               
*                                                                               
* MCS estimate checking here                                                    
*                                                                               
         USING MJETABD,R4                                                       
ESTIN54  L     R4,JBACOLTB         R4=A(COLUMN TABLE)                           
*                                                                               
ESTIN56  XR    R0,R0               first entry is totals                        
         IC    R0,MJETLEN                                                       
         AR    R4,R0                                                            
         CLI   MJETTYP,MJETTEQ     end of table?                                
         BE    ESTIN70                                                          
         CLI   MJETTYP,MJETTWQ     look for work code entries                   
         BNE   ESTIN56                                                          
                                                                                
         AP    XTOTCE,MJETVAL+L'MJETVAL(L'MJETVAL)                              
         B     ESTIN56                                                          
         DROP  R4                                                               
*                                                                               
ESTIN70  CP    XTOTCE,PZERO                                                     
         BE    ROUTL               NO CURRENT ESTIMATE                          
         CP    XTOTOR,PZERO                                                     
         BE    ROUTL                                                            
         ZAP   WORK(12),XTOTOR     XTOTOR=COSTS+PO'S                            
         MP    WORK(12),=P'10000'                                               
         DP    WORK(12),XTOTCE     XTOTCE=CURRENT ESTIMATE                      
         ZAP   XTOTPC,WORK(6)                                                   
         CP    WORK(6),SORTCER     HAVE REACHED CURRENT ESTIMATE %?             
         BL    ROUTL               NO                                           
                                                                                
ESTIN80  B     ROUTE                                                            
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* GET CLI/PRO/JOB NAMES                                               *         
***********************************************************************         
         SPACE 1                                                                
CLPRJO   MVC   IOKEY2,SPACES                                                    
         LA    R6,IOKEY2                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKCPY,SORTCPY                                                  
         MVC   ACTKUNT(2),PRODUL                                                
                                                                                
         LA    R2,CLILEN                                                        
         LA    R3,CLINAME                                                       
         LA    R4,3                                                             
         B     *+10                                                             
                                                                                
CLPR02   MVC   IOKEY2,CSVKEY2                                                   
         LA    RF,SORTJOB                                                       
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SHI   R1,1                                                             
         MVC   ACTKACT(0),0(RF)                                                 
         EX    R1,*-6                                                           
         MVC   CSVKEY2,IOKEY2                                                   
                                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('NAMELQ',AIOAREA1),0,0                
         CLI   12(R1),0                                                         
         BNE   CLPR04                                                           
                                                                                
         L     RE,12(R1)                                                        
         USING NAMELD,RE                                                        
         XR    R0,R0                                                            
         AHI   R0,L'CLINAME                                                     
         XR    RF,RF                                                            
         MVC   0(L'CLINAME,R3),SPACES                                           
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q                                                       
         CR    RF,R0                                                            
         BNH   *+6                                                              
         LR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,R3),NAMEREC                                                  
         EX    RF,*-6                                                           
         DROP  RE                                                               
CLPR04   AHI   R2,1                                                             
         AHI   R3,L'CLINAME                                                     
         BCT   R4,CLPR02                                                        
         B     ROUTE                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*  GET SECURITY ALPHA CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
GSECALP  MVC   SECCODE,ALPCODE                                                  
         XC    IOKEY2,IOKEY2                                                    
         LA    R6,IOKEY2                                                        
         USING CT5REC,R6           SYSTEM ACCESS RECORD                         
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,SECCODE                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA1                       
         L     R6,AIOAREA1                                                      
         CLC   IOKEY2(L'CT5KEY),0(R6)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CT5DATA                                                       
         USING CTSEAD,R6                                                        
         XR    R0,R0                                                            
GSALP02  CLI   CTSEAEL,0           TEST EOR                                     
         BE    GSECALPX                                                         
         CLI   CTSEAEL,CTSEAELQ    X'B8'                                        
         BE    GSALP08                                                          
         CLI   CTSEAEL,CTAGDELQ                                                 
         BE    GSALP10                                                          
GSALP06  IC    R0,CTSEALEN                                                      
         AR    R6,R0                                                            
         B     GSALP02                                                          
                                                                                
GSALP08  MVC   SECCODE,CTSEAAID    AGENCY ALPHA ID                              
         B     GSALP06                                                          
*                                                                               
         USING CTAGDD,R6                                                        
GSALP10  MVC   COMPCTRY,CTRY       DEFAULT IS DDS COUNTRY CODE                  
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    GSALP06                                                          
         MVC   COMPCTRY,CTAGDCTY   AGENCY COUNTRY CODE                          
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         MVC   MCCTRY,COMPCTRY                                                  
         MVC   RCCTRY,COMPCTRY                                                  
         MVC   CTRY,COMPCTRY                                                    
         B     GSALP06                                                          
         DROP  R1                                                               
*                                                                               
                                                                                
GSECALPX B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  GET LANGUAGE CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETLANG  LA    R6,IOKEY2                                                        
         USING CTIREC,R6           SYSTEM ACCESS RECORD                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,ORIGINUM                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA1                       
         L     R6,AIOAREA1                                                      
         CLC   IOKEY2(L'CTIKEY),0(R6)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CTIDATA                                                       
         USING CTAGYD,R6                                                        
         XR    R0,R0                                                            
GLANG02  CLI   CTAGYEL,0           TEST EOR                                     
         BE    GETLANGX                                                         
GLANG04  CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   *+10                                                             
         MVC   COMPLANG,CTAGYLNG                                                
*                                                                               
         IC    R0,CTAGYLEN                                                      
         AR    R6,R0                                                            
         B     GLANG02                                                          
*                                                                               
GETLANGX B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET LEDGER SJ LENGTHS                                               *         
***********************************************************************         
         SPACE 2                                                                
GLDGSJ   XC    IOKEY2,IOKEY2                                                    
         LA    R6,IOKEY2                                                        
         USING LDGRECD,R6          READ SJ LEDGER TO GET CLI,PRO,JOB            
         MVC   LDGKEY,SPACES       LENGTHS                                      
         MVC   LDGKCPY,COCODE                                                   
         MVC   LDGKUNT(L'PRODUL),PRODUL                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,LDGKEY,LDGKEY,0                       
         BNE   ROUTL               CANNOT FIND SJ LEDGER                        
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,LDGKDA,AIOAREA1,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('ACLELQ',AIOAREA1),0,0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R7,12(R1)                                                        
         USING ACLELD,R7                                                        
         MVC   CLILEN(L'CLILEN),ACLELLVA                                        
         MVC   PROLEN(L'PROLEN),ACLELLVB                                        
         MVC   JOBLEN(L'JOBLEN),ACLELLVC                                        
         MVC   PCLILEN(L'CLILEN),ACLELLVA                                       
         XR    RE,RE                                                            
         IC    RE,ACLELLVB                                                      
         XR    RF,RF                                                            
         IC    RF,ACLELLVC                                                      
         SR    RF,RE                                                            
         STC   RF,PJOBLEN                                                       
         IC    RF,ACLELLVA                                                      
         SR    RE,RF                                                            
         STC   RE,PPROLEN                                                       
         DROP  R6,R7                                                            
GLDGSJX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET EMAIL ADDRESS                                                   *         
* EXIT CC EQUAL IF EMAIL ADDRESS FOUND                                          
* EXIT CC LOW IF EMAIL ADDRESS NOT FOUND                                        
***********************************************************************         
         SPACE 1                                                                
GETEML   XC    IOKEY2,IOKEY2                                                    
         XC    PIDCHAR,PIDCHAR                                                  
         XC    PEREMAIL,PEREMAIL                                                
*                                                                               
         LA    R6,IOKEY2                                                        
         USING SA0REC,R6                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECCODE     AGENCY ALPHA CODE                            
         MVC   SA0KNUM,SVBPID      PERSON BINARY PID                            
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA1                       
         L     R6,AIOAREA1                                                      
         CLC   IOKEY2(L'SA0KEY),0(R6)                                           
         BNE   ROUTL                                                            
GETEM11  TM    SA0STAT,X'20'       LOCKED                                       
         BO    ROUTL                                                            
*                                                                               
GETEM12  LA    R6,SA0DATA                                                       
         USING SAPALD,R6                                                        
         XR    R0,R0                                                            
GETEM13  CLI   SAPALEL,0           TEST EOR                                     
         BNE   GETEM14                                                          
         DC    H'0'                                                             
GETEM14  CLI   SAPALEL,SAPALELQ                                                 
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R6,R0                                                            
         B     GETEM13                                                          
         MVC   PIDCHAR,SAPALPID    PERSON CHARACTER CODE                        
*                                                                               
GETEM16  XC    IOKEY2,IOKEY2                                                    
         USING SAPEREC,R6                                                       
         LA    R6,IOKEY2                                                        
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECCODE     SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDCHAR                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA1                       
         L     R6,AIOAREA1                                                      
         CLC   IOKEY2(SAPEDEF-SAPEKEY),0(R6)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,SAPEDATA                                                      
         USING SAPEED,R7                                                        
         XC    PEREMAIL,PEREMAIL                                                
         MVC   PERFSTNM,SPACES                                                  
         MVC   PERMIDNM,SPACES                                                  
         MVC   PERLSTNM,SPACES                                                  
         XR    R0,R0                                                            
GETEM18  CLI   SAPEEEL,0           TEST EOR                                     
         BE    ROUTE                                                            
         CLI   SAPEEEL,SAPEEELQ    X'E5' - PERSON EMAIL ELEM                    
         BE    GETEM22                                                          
         CLI   SAPEEEL,SANAMELQ    X'C5'                                        
         BE    GETEM32                                                          
GETEM20  IC    R0,SAPEELN                                                       
         AR    R7,R0                                                            
         B     GETEM18                                                          
*                                                                               
GETEM22  IC    RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ+1                                                    
         MVC   PEREMAIL(0),SAPEEID SAVE EMAIL ADDRESS                           
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,ADDRLEN                                                       
         B     GETEM20                                                          
         DROP  R7                                                               
*                                                                               
         USING SANAMD,R7                                                        
GETEM32  LA    R1,SANAMELN         LENGTH OF NAME                               
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         BZ    GETEM34                                                          
         XR    RF,RF                                                            
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,PERFSTNM,SANAME                                               
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM34  TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT                     
         BZ    GETEM36                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,PERMIDNM,SANAME                                               
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM36  TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         BZ    GETEM20                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,58               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,58               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,PERLSTNM,SANAME                                               
         B     GETEM20                                                          
         DROP  R1,R6                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER                                 *         
***********************************************************************         
         SPACE 1                                                                
PUTETAB  L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         MVC   EMADDR,PEREMAIL     EMAIL ADDRESS                                
         MVC   EMADLN,ADDRLEN                                                   
         MVC   EMULC,PRODUL        UNIT+LEDGER                                  
         MVC   EMACC,SORTJOB       JOB CODE                                     
         MVC   EMJNM,JOBNAME       JOB NAME                                     
PUTE02   MVC   EMAPID,PIDCHAR      PERSON CHARACTER CODE                        
         MVC   EMAFSTNM,PERFSTNM   FIRST NAME                                   
         MVC   EMAMDLNM,PERMIDNM   MIDDLE NAME                                  
         MVC   EMALSTNM,PERLSTNM   LAST NAME                                    
         MVC   EMADOE,SORTDOE      SORT TYPE                                    
         CLI   SORTDOE,SORTINA                                                  
         BNE   PUTE04                                                           
         XR    RF,RF                                                            
         ICM   RF,3,SORTNDA        NUMBER OF DAYS INACTIVE                      
         LCR   RF,RF                                                            
         ST    RF,EMINDA                                                        
         B     PUTE06                                                           
PUTE04   ZAP   EMCEAMT,XTOTCE      CURRENT ESTIMATE AMOUNT                      
         ZAP   EMCOPOS,XTOTOR      COSTS+PO'S                                   
         ZAP   EMAPERC,XTOTPC      PERCENTAGE COMMITTED                         
         ZAP   EMUNCOM,EMCEAMT                                                  
         SP    EMUNCOM,EMCOPOS     UNCOMMITTED AMOUNT                           
PUTE06   LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
*                                                                               
         L     R7,NUMRECS                                                       
         LA    R7,1(R7)            INCREMENT # OF RECORDS                       
         ST    R7,NUMRECS          STORED IN EMAILTAB                           
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R7,RE                                                            
         BL    ROUTE                                                            
         L     R7,NUMLOOP                                                       
         LA    R7,1(R7)                                                         
         ST    R7,NUMLOOP                                                       
         GOTO1 APROETAB                                                         
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         MVI   FORCEHED,YESQ                                                    
         MVI   RCSUBPRG,0                                                       
         B     ROUTE                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* SORTING AND EMAIL BATCHING                                          *         
***********************************************************************         
         SPACE 1                                                                
PROETAB  L     R2,AEMLTAB                                                       
         USING EMRECD,R2                                                        
         MVI   RUNIND2,0                                                        
         OC    NUMRECS,NUMRECS                                                  
         BZ    ROUTE                                                            
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,YESQ                                                    
         L     R3,NUMRECS                                                       
         GOTO1 XSORT,DMCB,(R2),(R3),EMRECL,EMLENG,0                             
         XC    NUMIJOB,NUMIJOB                                                  
         XC    NUMEJOB,NUMEJOB                                                  
*                                                                               
PETAB05  ST    R2,ANXTETB                                                       
         MVC   PEREMAIL,EMADDR                                                  
         GOTO1 ADOEMADD,0(R2)      Output email address and subject             
         OC    EMADDR,EMADDR       Any email address?                           
         BNZ   PETAB10                                                          
         L     R2,ANXTETB          If not bump to next entry                    
         LA    R2,EMRECL(R2)       R2=A(NEXT RECORD)                            
         BCT   R3,PETAB90                                                       
         B     ROUTE                                                            
*                               ** Starting a section of the email **           
PETAB10  L     R6,AELEMT                                                        
*                                                                               
PETAB15  CLM   R6,15,AELEMTML      List section?                                
         BNL   PETAB25                                                          
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,*-6                                                           
*                                                                               
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB20                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB20  LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         B     PETAB15                                                          
*                                                                               
         USING EWJBHD,R4                                                        
PETAB25  LA    R4,TEXT1                                                         
         CLI   EMADOE,SORTEST     'E' FOR ESTIMATES                             
         BNE   PETAB40                                                          
*                                                                               
         TM    RUNIND2,RUNIEST     ALREADY DONE ESTIMATE HEADINGS?              
         BNZ   PETAB30                                                          
         OI    RUNIND2,RUNIEST     SET DONE ESTIMATE HEADINGS                   
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   EWJBH01,TBROPE                                                   
         MVC   EWJBH02,TBHOPE                                                   
         MVC   EWJBHBC,AC@JOBC                                                  
         MVC   EWJBH03,TBHCLO                                                   
         MVC   EWJBH04,TBHOPE                                                   
         MVC   EWJBHJN,AC@JOBN                                                  
         MVC   EWJBH05,TBHCLO                                                   
         MVC   EWJBH06,TBHOPE                                                   
         MVC   EWJBHES,AC@EST                                                   
         MVC   EWJBH07,TBHCLO                                                   
         MVC   EWJBH08,TBHOPE                                                   
         MVC   EWJBHCM,AC@CMITD                                                 
         MVC   EWJBH09,TBHCLO                                                   
         MVC   EWJBH10,TBHOPE                                                   
         MVC   EWJBHUC,AC@UCMD                                                  
         MVC   EWJBH11,TBHCLO                                                   
         MVC   EWJBH12,TBHOPE                                                   
         MVC   EWJBHPC,AC@SCLPO                                                 
         MVC   EWJBH13,TBHCLO                                                   
         MVC   EWJBH14,TBRCLO                                                   
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB30                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
         USING EWJOBD,R4                                                        
PETAB30  MVC   TEXT1,TEXTSPCS                                                   
         MVC   EWJOB01,TBROPE                                                   
         MVC   EWJOB02,TBDOPE                                                   
         MVC   EWJOB03,LINKOP                                                   
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB32                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB32  MVC   TEXT1,TEXTSPCS                                                   
         GOTO1 ASETURL,DMCB,EMACC,EWJJURL                                       
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB35                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB35  MVC   TEXT1,TEXTSPCS                                                   
         MVC   EWJOB04,LINKCL                                                   
         MVC   EWJOB05,TBDCLO                                                   
         MVC   EWJOB06,TBDOPE                                                   
         MVC   EWJTJBN(L'EMJNM),EMJNM JOB NAME                                  
         MVC   EWJOB07,TBDCLO                                                   
         MVC   EWJOB08,TBDOPE                                                   
         EDIT  (P6,EMCEAMT),(12,EWJTEST),2                                      
         MVC   EWJOB09,TBDCLO                                                   
         MVC   EWJOB10,TBDOPE                                                   
         EDIT  (P6,EMCOPOS),(12,EWJCOMM),2                                      
         MVC   EWJOB11,TBDCLO                                                   
         MVC   EWJOB12,TBDOPE                                                   
         EDIT  (P6,EMUNCOM),(12,EWJUNCM),2                                      
         MVC   EWJOB13,TBDCLO                                                   
         MVC   EWJOB14,TBDOPE                                                   
         EDIT  (P6,EMAPERC),(12,EWJESPC),2,ALIGN=RIGHT                          
         MVC   EWJOB15,TBDCLO                                                   
         MVC   EWJOB16,TBRCLO                                                   
*                                                                               
         L     RF,NUMEJOB                                                       
         AHI   RF,1                                                             
         ST    RF,NUMEJOB                                                       
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB55                                                          
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
         B     PETAB55                                                          
*                                                                               
         USING EWINHD,R4                                                        
PETAB40  CLI   EMADOE,SORTINA                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    EMINDA,EMINDA       ANY INACTIVITY?                              
         BZ    PETAB55                                                          
         MVC   TEXT1,TEXTSPCS                                                   
*                                                                               
         TM    RUNIND2,RUNINDAY    ALREADY DONE NO. OF DAYS INACTIVE            
         BNZ   PETAB45                                                          
         OI    RUNIND2,RUNINDAY    SET DONE NO. OF DAYS INACTIVE                
         MVC   EWIH01,TBROPE                                                    
         MVC   EWIH02,TBHOPE                                                    
         MVC   EWIHJB,AC@JOBC                                                   
         MVC   EWIH03,TBHCLO                                                    
         MVC   EWIH04,TBHOPE                                                    
         MVC   EWIHJN,AC@JOBN                                                   
         MVC   EWIH05,TBHCLO                                                    
         MVC   EWIH06,TBHOPE                                                    
         MVC   EWIHND,AC@NDINA                                                  
         MVC   EWIH07,TBHCLO                                                    
         MVC   EWIH08,TBRCLO                                                    
         CLI   RCWRITE,NOQ                                                      
         BE    PETAB45                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
         USING EWINAD,R4                                                        
PETAB45  MVC   TEXT1,TEXTSPCS                                                   
         MVC   EWIN01,TBROPE                                                    
         MVC   EWIN02,TBDOPE                                                    
         MVC   EWIN03,LINKOP                                                    
         CLI   RCWRITE,NOQ                                                      
         BE    PETAB48                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB48  MVC   TEXT1,TEXTSPCS                                                   
         GOTO1 ASETURL,DMCB,EMACC,EWINJU                                        
         CLI   RCWRITE,NOQ                                                      
         BE    PETAB50                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB50  MVC   TEXT1,TEXTSPCS                                                   
         MVC   EWINJB,EMACC                                                     
         MVC   EWIN04,LINKCL                                                    
         MVC   EWIN05,TBDCLO                                                    
         MVC   EWIN06,TBDOPE                                                    
         MVC   EWINJN(L'EMJNM),EMJNM JOB NAME                                   
         MVC   EWIN07,TBDCLO                                                    
         MVC   EWIN08,TBDOPE                                                    
         ICM   RE,15,EMINDA                                                     
         LCR   RE,RE                                                            
         EDIT  (RE),(8,EWJNDI),ALIGN=RIGHT                                      
         MVC   EWIN09,TBDCLO                                                    
         MVC   EWIN10,TBRCLO                                                    
*                                                                               
         L     RF,NUMIJOB                                                       
         AHI   RF,1                                                             
         ST    RF,NUMIJOB                                                       
*                                                                               
         CLI   RCWRITE,NOQ                                                      
         BE    PETAB55                                                          
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB55  LR    R5,R2                                                            
         BCT   R3,PETAB60          ANY MORE RECORD TO PROCESS                   
         B     PETAB65             NO                                           
*                                                                               
PETAB60  LA    R2,EMRECL(R2)       R2=A(NEXT RECORD)                            
         CLC   L'EMADDR(L'EMAPID,R5),EMAPID  SAME PERSON                        
         BE    PETAB25                                                          
*                                                                               
PETAB65  L     R6,AELEMTML                                                      
*                                                                               
PETAB70  CLI   0(R6),X'FF'                                                      
         BE    PETAB80                                                          
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,*-6                                                           
*                                                                               
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PETAB75                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB75  LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         B     PETAB70                                                          
*                                                                               
PETAB80  CLI   RCWRITE,NOQ                                                      
         BE    PETAB85                                                          
         GOTO1 VSMTP,DMCB,('SMTPASND',0)   SEND EMAIL                           
PETAB85  MVI   RUNIND2,0                                                        
         L     R7,NUMEMLS                                                       
         LA    R7,1(R7)                                                         
         ST    R7,NUMEMLS                                                       
*                                                                               
PETAB90  GOTO1 APRTDET               PRINT REPORT                               
         CHI   R3,1                                                             
         BNH   ROUTE                                                            
         B     PETAB05                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT DETAILS OF EMAIL SENT                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING EMRECD,R5                                                        
PRTDET   MVC   P+1(L'EMAPID),EMAPID                                             
         MVC   P+10(L'EMAFSTNM),EMAFSTNM                                        
         MVC   P+26(L'EMALSTNM),EMALSTNM                                        
         MVC   P+74(L'AC@EHBST),AC@EHBST  EMAIL HAS BEEN SENT TO                
         LLC   RF,EMADLN                                                        
         BCTR  RF,0                                                             
         MVC   PSECOND+74(0),EMADDR                                             
         EX    RF,*-6                                                           
         EDIT  (B4,NUMIJOB),(10,P+40)                                           
         EDIT  (B4,NUMEJOB),(10,P+52)                                           
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     ROUTE                                                            
         DROP  R5                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER ORDER PASSIVES ACCORDING TO GODIOS AND GODEOS                *         
* ON NTRY P1=A(ORDER PASSIVE)                                         *         
***********************************************************************         
         SPACE 1                                                                
FLTORD   L     R2,0(R1)                                                         
         USING OSJPASD,R2                                                       
         TM    OSJPSTAT,ORDCLOSE+ORDSFMCH+ORDSLDEL                              
         BNZ   ROUTL                                                            
*                                                                               
*&&UK                                                                           
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         L     R3,ACMAGOX                                                       
*&&                                                                             
*&&US*&& L     R3,AGOXBLCK                                                      
         USING GOXBLOCKD,R3                                                     
         LA    RF,GODIOS           order option                                 
         CLI   OSJPMEM,0           is it memo?                                  
         BE    FLTORD02                                                         
         LA    RF,GODIEO           yes-use memo order option                    
*                                                                               
FLTORD02 CLI   0(RF),GONONE        none                                         
         BE    ROUTL                                                            
         CLI   0(RF),GOINPR        in progress                                  
         BE    ROUTE                                                            
         TM    OSJPSTA2,ORDSDRFT   ignore in progress                           
         BNZ   ROUTL                                                            
         CLI   0(RF),GOSUB         submitted                                    
         BE    ROUTE                                                            
         TM    OSJPSTA2,ORDSSUBM   ignore submitted                             
         BNZ   ROUTL                                                            
         TM    OSJPSTA2,ORDSOREJ   ignore rejected                              
         BNZ   ROUTL                                                            
         CLI   0(RF),GOPAP         part approved                                
         BE    ROUTE                                                            
         TM    OSJPSTA2,ORDSPAPP   ignore part approved                         
         BNZ   ROUTL                                                            
         CLI   0(RF),GOAPPR        approved                                     
         BE    ROUTE                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET THE EMAIL ADDRESS AND SUBJECT                                   *         
* ON NTRY P1=EMRECD                                                   *         
***********************************************************************         
         USING EMRECD,R2                                                        
DOEMADD  LR    R2,R1                                                            
         LA    RE,L'EMADDR                                                      
         LA    RF,EMADDR+L'EMADDR-1                                             
DOEMA02  CLI   0(RF),C' '                                                       
         BH    DOEMA04                                                          
         AHI   RF,-1                                                            
         BCT   RE,DOEMA02                                                       
         DC    H'0'                                                             
*                                                                               
DOEMA04  MVI   1(RF),C':' EMAIL ADDRESS HAS TO BE TERMINATED WITH ':'           
         XC    SVEMAD,SVEMAD                                                    
         CLI   PARMDOE,YESQ        OVERRIDE TO TEST                             
         JNE   DOEMA06                                                          
         MVC   SVEMAD,EMADDR                                                    
         XC    EMADDR,EMADDR                                                    
*        MVC   EMADDR(17),=C'JIM.SHEA@DDS.NET:'                                 
         LAY   R5,TESTMAIL         OVERRIDE EMAIL PATCHED IN?                   
         CLC   0(L'TESTMAIL,R5),SPACES                                          
         JH    *+6                                                              
         DC    H'0'                WHERE IS IT?                                 
         MVC   EMADDR,0(R5)                                                     
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'SVEMAD),SVEMAD  PUT IN EMAIL ADDRESS                     
         MVC   TEXT1+L'SVEMAD+1(L'AC@JEMAL),AC@JEMAL  JOB ALERTS                
         CLI   PARMPID,YESQ                                                     
         BNE   DOEMA08                                                          
         MVC   TEXT1+L'SVEMAD+2+L'AC@JEMAL(L'EMAPID),EMAPID                     
         B     DOEMA08                                                          
*                                                                               
DOEMA06  MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'AC@JEMAL),AC@JEMAL  JOB ALERTS                           
*                                                                               
DOEMA08  MVC   WORK,SPACES                                                      
         MVC   WORK,EMADDR         store in 24 bit address                      
         XC    DMCB,DMCB                                                        
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'TEXT1,TEXT1)                     
*                                                                               
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'SVEMAD),SVEMAD                                           
*                                                                               
         L     RF,AELE271E         Clear out previous information               
         MVC   0(L'ELEM271E,RF),TEXTSPCS                                        
         L     RF,AELE272E                                                      
         MVC   0(L'ELEM272E,RF),TEXTSPCS                                        
         L     RF,AELE273E                                                      
         MVC   0(L'ELEM273E,RF),TEXTSPCS                                        
*                                                                               
         L     RF,AELE271E                                                      
         GOTO1 VSQUASH,DMCB,0(RF),MAXELEN                                       
         L     RF,AELE272E                                                      
         GOTO1 VSQUASH,DMCB,0(RF),80                                            
         L     RF,AELE273E                                                      
         GOTO1 VSQUASH,DMCB,0(RF),80                                            
*                                                                               
DOEMAX   B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Build the job link url                                              *         
* On ntry P1=Job code                                                 *         
*         P2=Output destination                                       *         
***********************************************************************         
         SPACE 1                                                                
SETURL   L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         LA    RF,SVHTTP                                                        
         LHI   RE,L'SVHTTP-1                                                    
         CLI   SVFED,YESQ          Is this a system with a url prefix?          
         JNE   BLDURL02                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    BLDURL02                                                         
         LA    RF,HTTP             Always http for federated urls               
         LHI   RE,L'HTTP-1                                                      
*                                                                               
BLDURL02 BASR  R1,0                                                             
         MVC   0(0,R3),0(RF)                                                    
         EX    RE,0(R1)                                                         
         AHI   R3,L'SVHTTP                                                      
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JNZ   BLDURL04                                                         
         CLC   SVENV,SPACES        Any environment?                             
         JNH   BLDURL04                                                         
         MVC   0(L'AGUENV,R3),SVENV                                             
         AHI   R3,L'SVENV                                                       
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
*                                                                               
BLDURL04 CLI   SVFED,YESQ          Is this a system with a url prefix?          
         JNE   BLDURL06                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    BLDURL06                                                         
         MVC   0(L'AGUFEDP,R3),SVFEDP                                           
         AHI   R3,L'SVFEDP                                                      
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
         MVC   0(L'FEDURL,R3),FEDURL                                            
         AHI   R3,L'FEDURL                                                      
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
         MVC   0(L'AGUFEDS,R3),SVFEDS                                           
         AHI   R3,L'SVFEDS                                                      
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
*                                  Squash and save new url length               
BLDURL06 MVC   0(L'SVURL,R3),SVURL                                              
         AHI   R3,L'SVURL                                                       
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
         MVC   0(DISPURJL,R3),DISPURLJ                                          
         AHI   R3,DISPURJL                                                      
         CLI   0(R3),C' '                                                       
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         AHI   R3,1                                                             
         MVC   0(L'CLITXT,R3),CLITXT                                            
         AHI   R3,L'CLITXT                                                      
*                                                                               
         LLC   RF,PCLILEN                                                       
         GOTOR URIENC,DMCB,((RF),0(R2)),0(R3)                                   
         LLC   RF,8(R1)                                                         
         AR    R3,RF                                                            
         MVI   0(R3),X'50'                                                      
         AHI   R3,1                                                             
         MVC   0(L'PROTXT,R3),PROTXT                                            
         AHI   R3,L'PROTXT                                                      
*                                                                               
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         LA    R4,0(RE,R2)                                                      
         GOTOR URIENC,DMCB,((RF),0(R4)),0(R3)                                   
         LLC   RF,8(R1)                                                         
         AR    R3,RF                                                            
         MVI   0(R3),X'50'                                                      
         AHI   R3,1                                                             
         MVC   0(L'JOBTXT,R3),JOBTXT                                            
         AHI   R3,L'JOBTXT                                                      
*                                                                               
         LLC   RF,PJOBLEN                                                       
         LLC   RE,PPROLEN                                                       
         LA    R4,0(RE,R4)                                                      
         GOTOR URIENC,DMCB,((RF),0(R4)),(X'80',0(R3))                           
         LLC   RF,8(R1)                                                         
         AR    R3,RF                                                            
         MVC   0(L'LINKCB,R3),LINKCB                                            
*                                                                               
         J     EXITE                                                            
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
AIOAREA1 DS    A                   ADDRESS OF IO AREA 1                         
AIOAREA2 DS    A                   ADDRESS OF IO AREA 2                         
AIOAREA3 DS    A                   ADDRESS OF IO AREA 3                         
EMLTBLN  DS    F                   LENGTH OF EMAIL TAB                          
AEMLTAB  DS    A                   ADDRESS OF EMAIL TAB                         
ANXTETB  DS    A                   ADDRESS OF NEXT ENTRY IN EMAIL TAB           
AAURURL  DS    A                   Address of AURTAB                            
VGETURL  DS    A                   Address of GETURL module                     
*                                                                               
ROUTS    DS    0A                                                               
AESPSORT DS    A                                                                
ADAYSORT DS    A                                                                
APUTSRT  DS    A                                                                
AGETSORT DS    A                                                                
APROETAB DS    A                                                                
APUTETAB DS    A                                                                
AGETEML  DS    A                                                                
AGLDGSJ  DS    A                                                                
AGSECALP DS    A                                                                
AGETLANG DS    A                                                                
APRTDET  DS    A                                                                
AESTINA  DS    A                                                                
ACLPRJO  DS    A                                                                
AFLTORD  DS    A                                                                
ADOEMADD DS    A                                                                
ASETURL  DS    A                                                                
ROUTSL   EQU   *-ROUTS                                                          
ROUTSN   EQU   ROUTSL/L'ROUTS                                                   
*                                                                               
COCODE   DS    XL1                 COMPANY CODE                                 
COMPLANG DS    XL1                 COMPANY LANGUAGE                             
CPXSTA   DS    XL1                 SAVED COMPANY EXTRA STATUS A                 
LLANG    DS    XL1                 LAST LANGUAGE CODE                           
CTRY     DS    XL1                 COUNTRY                                      
TODAYD   DS    PL3                 TODAY'S DATE                                 
TODAYL   DS    CL8                 TODAY'S DATE                                 
COMPCTRY DS    XL1                 COMPANY COUNTRY                              
COMPDA   DS    XL4                 COMPANY DISK ADDRESS                         
SECCODE  DS    CL2                 AGENCY ALPHA CODE FROM CTFILE                
SAVOFF   DS    XL2                 SAVED OFFICE                                 
ALPCODE  DS    CL2                 ALPHA ID CODE                                
SVBPID   DS    XL2                 SAVED PERSON BINARY PID                      
SVDATE   DS    PL3                 SAVED DATE                                   
SVACOD   DS    CL12                SAVED ACCOUNT CODE                           
SVEMAD   DS    CL50                SAVED EMAIL ADDRESS                          
SVADDR   DS    F                   SAVED ADDRESS                                
RUNIND1  DS    XL1                 RUN INDICATOR                                
RUNINOEM EQU   X'80'               NO EMAIL ADDRESS                             
*                                                                               
RUNIND2  DS    XL1                                                              
RUNINDAY EQU   X'80'               NO. DAYS INACTIVE DONE                       
RUNIEST  EQU   X'40'               ESTIMATES INACTIVE SECTION DONE              
*                                                                               
CLILEN   DS    XL1                 CLIENT LENGTH                                
PROLEN   DS    XL1                 PRODUCT LENGTH                               
JOBLEN   DS    XL1                 JOB LENGTH                                   
*                                                                               
PCLILEN  DS    XL1                 CLIENT LENGTH   (CLILEN)                     
PPROLEN  DS    XL1                 PRODUCT LENGTH  (PROLEN-CLILEN)              
PJOBLEN  DS    XL1                 JOB LENGTH      (JOBLEN-PROLEN)              
*                                                                               
CLINAME  DS    CL20                CLIENT NAME                                  
PRONAME  DS    CL20                PRODUCT NAME                                 
JOBNAME  DS    CL20                JOB NAME                                     
SAVEORD  DS    CL6                 SAVED ORDER NUMBER                           
*                                                                               
TARPID   DS    CL8             APPROVER FILTER                                  
TARPIN   DS    XL2             PIN DERIVED FROM TARPID, NEEDS PARMDAG           
TESTMAIL DS    CL50     override email address to use (QCNT=C)                  
*                                                                               
SORTSW   DS    XL1                 SORTER INDICATOR BYTE                        
PROFSW   DS    XL1                 PROFILE INDICATOR BYTE                       
PRAGCY   EQU   X'80'               READ AGENCY LEVEL PROFILES                   
PRUSID   EQU   X'40'               READ USERID PROFILES                         
NUMRECS  DS    F                   NUMBER OF RECORDS IN EMAILBUF                
NUMIJOB  DS    F                   NUMBER OF INACTIVE JOBS                      
NUMEJOB  DS    F                   NUMBER OF JOBS REACHING ESTIMATE %           
NUMEMLS  DS    F                   NUMBER OF EMAILS                             
NUMLOOP  DS    F                   NUMBER OF LOOPS WITH EMAILBUF                
*                                                                               
TEXT1    DS    CL(MAXELEN)                                                      
TEXT2    DS    CL80                                                             
TEXT3    DS    CL80                                                             
*                                                                               
PERFSTNM DS    CL15                PERSON FIRST NAME                            
PERMIDNM DS    CL15                PERSON MIDDLE NAME                           
PERLSTNM DS    CL58                PERSON LAST NAME                             
*                                                                               
PIDCHAR  DS    CL8                 PERSONAL-ID CHAR                             
PEREMAIL DS    CL50                PERSON EMAIL ADDRESS                         
ADDRLEN  DS    XL1                 ACTUAL EMAIL ADDRESS LENGTH                  
*                                                                               
XTOTCE   DS    PL6                 CURRENT ESTIMATE                             
XTOTOR   DS    PL6                 COSTS+PO'S                                   
XTOTPC   DS    PL6                 % COMMITTED                                  
*                                                                               
ELEMENT  DS    XL256                                                            
*                                                                               
AGYPROF2 DS    XL16                AGENCY PROFILE PAGE 2                        
USRPROF2 DS    0XL16               USER-ID PROFILE PAGE 2                       
PROFO201 DS    XL1                 Enable 'All order Items' mode                
         DS    XL1                 **NO LONGER IN USE                           
PROFO203 DS    XL1                 PRODUCTION Auto approval limit               
PROFO204 DS    XL1                 Multiplier PROD Auto Appr Lim                
PROFO205 DS    XL1                 Requisition number EXPENSE                   
PROFO206 DS    XL1                 Allow self approval EXPENSE                  
PROFO207 DS    XL1                 EXPENSE Auto approval limit                  
PROFO208 DS    XL1                 Multiplier EXP Auto Appr Lim                 
PROFO209 DS    XL1                 Requisition number INTERNAL                  
PROFO210 DS    XL1                 Allow self approval INTERNAL                 
PROFO211 DS    XL1                 INTERNAL Auto approval limit                 
PROFO212 DS    XL1                 Multiplier INT Auto Appr Lim                 
PROFO213 DS    XL1                 Requisition number ARTISTS                   
PROFO214 DS    XL1                 Allow self approval ARTISTS                  
PROFO215 DS    XL1                 ARTISTS Auto approval limit                  
PROFO216 DS    XL1                 Multiplier ART Auto Appr Lim                 
*                                                                               
AGYPROF3 DS    XL16                AGENCY PROFILE PAGE 3                        
USRPROF3 DS    0XL16               USER-ID PROFILE PAGE 3                       
PROFO301 DS    XL1                 Prevent zero order amount                    
         DS    XL1                 **NO LONGER IN USE**                         
         DS    XL14                N/D                                          
*                                                                               
SORTREC  DS    XL(SORTRECL)        SORT RECORD                                  
*                                                                               
DISP     DS    F                   DISPLACEMENT FOR EMAILTAB                    
DATO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
DATOX    DS    0C                                                               
IOCOMP   DS    XL1                 SPECIFIC COMPANY CODE                        
         DS    XL1                 n/d                                          
IOKEY1   DS    CL64                I/O KEY                                      
IOKEY2   DS    CL64                I/O KEY                                      
CSVKEY1  DS    CL64                SAVED I/O KEY                                
CSVKEY2  DS    CL64                SAVED I/O KEY                                
CSVKEY3  DS    CL64                SAVED I/O KEY                                
IOAREA1  DS    XL2000              I/O AREA                                     
IOAREA2  DS    XL2000              I/O AREA 2                                   
IOAREA3  DS    XL2000              I/O AREA 3                                   
*                                                                               
WORKX    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* EMAIL DSECT FOR EMAIL BUFFER                                        *         
***********************************************************************         
EMRECD   DSECT                                                                  
EMADDR   DS    CL50                EMAIL ADDRESS                                
EMAPID   DS    CL8                 PERSON CHARACTER PID                         
EMADOE   DS    CL1                 SAME  AS SORTDOE                             
EMUNCOM  DS    PL6                 UNCOMMITTED AMOUNT                           
EMCEAMT  DS    PL6                 CURRENT ESTIMATE AMOUNT                      
EMCOPOS  DS    PL6                 COSTS+PO'S                                   
EMAPERC  DS    PL6                 ESTIMATE PERCENTAGE COMMITTED                
EMINDA   DS    F                   NUMBER OF DAYS INACTIVE (COMPLEMENT)         
EMLENG   EQU   *-EMADDR            RECORD LENGTH                                
EMULA    DS    0CL14               U/L/ACC                                      
EMULC    DS    CL2                 UNIT AND LEDGER                              
EMACC    DS    CL12                ACCOUNT CODE (JOB)                           
EMJNM    DS    CL20                JOBNAME                                      
EMAFSTNM DS    CL15                PERSON FIRST NAME                            
EMAMDLNM DS    CL15                PERSON MIDDLE NAME                           
EMALSTNM DS    CL58                PERSON LAST NAME                             
EMADLN   DS    XL1                 ACTUAL ADDRESS LENGTH                        
EMRECL   EQU   *-EMRECD                                                         
         SPACE 1                                                                
***********************************************************************         
* SORT RECORD FOR SORTER                                              *         
***********************************************************************         
SORTRECD DSECT                                                                  
SORTKEY  DS    0XL15               SORT KEY                                     
SORTCPY  DS    XL1                 COMPANY CODE                                 
SORTDOE  DS    CL1                 I FOR DAYS INACTIVE OR S FOR EST             
SORTINA  EQU   C'I'                DAYS INACTIVE                                
SORTEST  EQU   C'S'                ESTIMATE                                     
SORTBPID DS    XL2                 PERSON BINARY PID                            
SORTJOB  DS    XL12                JOB CODE                                     
SORTKEYL EQU   *-SORTRECD          KEY LENGTH                                   
SORTCER  DS    PL3                 ESTIMATE % OPT/MAIN SETTING (ERM)            
         ORG   SORTCER                                                          
SORTNDA  DS    XL2                 NUMBER OF DAYS INACTIVE                      
         DS    XL1                 N/D                                          
SORTRECL EQU   *-SORTRECD                                                       
         SPACE 1                                                                
***********************************************************************         
* PARM OPTION DSECT EUROPE                                            *         
***********************************************************************         
         SPACE 1                                                                
PARMD    DSECT                                                                  
PARMDPQ  DS    CL1   RCFFPARM+0    SUPPRESS REPORT FROM PQ                      
PARMDOE  DS    CL1   RCFFPARM+1    OVERRIDE EMAILS                              
PARMDAG  DS    CL2   RCFFPARM+2/3  THIS AGENCY ONLY                             
PARMPID  DS    CL1   RCFFPAMR+4    SHOW PID INFO ON EMAILS                      
         SPACE 1                                                                
***********************************************************************         
* LIST OF JOBS THAT ARE UNCOMMITTED OR OVER ESTIMATE                  *         
***********************************************************************         
         SPACE 1                                                                
EWJBHD   DSECT                                                                  
EWJBH01  DS    CL4   <tr>                                                       
EWJBH02  DS    CL4   <th>                                                       
EWJBHBC  DS    CL8   Job code                                                   
EWJBH03  DS    CL5   </th>                                                      
EWJBH04  DS    CL4   <th>                                                       
EWJBHJN  DS    CL8   Job name                                                   
EWJBH05  DS    CL5   </th>                                                      
EWJBH06  DS    CL4   <th>                                                       
EWJBHES  DS    CL8   Estimate                                                   
EWJBH07  DS    CL5   </th>                                                      
EWJBH08  DS    CL4   <th>                                                       
EWJBHCM  DS    CL9   Committed                                                  
EWJBH09  DS    CL5   </th>                                                      
EWJBH10  DS    CL4   <th>                                                       
EWJBHUC  DS    CL11  Uncommitted                                                
EWJBH11  DS    CL5   </th>                                                      
EWJBH12  DS    CL4   <th>                                                       
EWJBHPC  DS    CL20  Estimate percentage committed                              
EWJBH13  DS    CL5   </th>                                                      
EWJBH14  DS    CL5   </tr>                                                      
                                                                                
EWJOBD   DSECT                                                                  
EWJOB01  DS    CL4   <tr>                                                       
EWJOB02  DS    CL4   <td>                                                       
EWJOB03  DS    CL(L'LINKOP)                                                     
         ORG   EWJOB01                                                          
EWJJURL  DS    CL(MAXELEN-(*-EWJOB01)) Job code url                             
         ORG   EWJOB01                                                          
EWJJOBC  DS    CL40  Job code (40 chars to allow encoding of specials)          
         ORG   EWJOB01                                                          
EWJOB04  DS    CL4   </a>                                                       
EWJOB05  DS    CL5   </td>                                                      
EWJOB06  DS    CL4   <td>                                                       
EWJTJBN  DS    CL36  Job name                                                   
EWJOB07  DS    CL5   </td>                                                      
EWJOB08  DS    CL4   <td>                                                       
EWJTEST  DS    CL12  Estimate                                                   
EWJOB09  DS    CL5   </td>                                                      
EWJOB10  DS    CL4   <td>                                                       
EWJCOMM  DS    CL12  Committed                                                  
EWJOB11  DS    CL5   </td>                                                      
EWJOB12  DS    CL4   <td>                                                       
EWJUNCM  DS    CL12  Uncommitted                                                
EWJOB13  DS    CL5   </td>                                                      
EWJOB14  DS    CL4   <td>                                                       
EWJESPC  DS    CL12  Estimate percentage committed                              
EWJOB15  DS    CL5   </td>                                                      
EWJOB16  DS    CL5   </tr>                                                      
         EJECT                                                                  
***********************************************************************         
* LIST OF JOBS THAT ARE INACTIVE                                      *         
***********************************************************************         
         SPACE 1                                                                
EWINHD   DSECT                                                                  
EWIH01   DS    CL4   <tr>                                                       
EWIH02   DS    CL4   <th>                                                       
EWIHJB   DS    CL8   Job code                                                   
EWIH03   DS    CL5   </th>                                                      
EWIH04   DS    CL4   <th>                                                       
EWIHJN   DS    CL8   Job name                                                   
EWIH05   DS    CL5   </th>                                                      
EWIH06   DS    CL4   <th>                                                       
EWIHND   DS    CL23  Number of days inactive                                    
EWIH07   DS    CL5   </th>                                                      
EWIH08   DS    CL5   </tr>                                                      
*                                                                               
EWINAD   DSECT                                                                  
EWIN01   DS    CL4   <tr>                                                       
EWIN02   DS    CL4   <th>                                                       
EWIN03   DS    CL(L'LINKOP)                                                     
         ORG   EWIN01                                                           
EWINJU   DS    CL(MAXELEN-(*-EWIN01)) Job code                                  
         ORG   EWIN01                                                           
EWINJB   DS    CL12  Job code                                                   
EWIN04   DS    CL4   </a>                                                       
EWIN05   DS    CL5   </th>                                                      
EWIN06   DS    CL4   <th>                                                       
EWINJN   DS    CL36  Job name                                                   
EWIN07   DS    CL5   </th>                                                      
EWIN08   DS    CL4   <th>                                                       
EWJNDI   DS    CL23  Number of days inactive                                    
EWIN09   DS    CL5   </th>                                                      
EWIN10   DS    CL5   </tr>                                                      
         EJECT                                                                  
*                                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
GOBBLOCKD DSECT                                                                 
* ACGOBBLOCK                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
         SPACE 1                                                                
GOXBLOCKD DSECT                                                                 
* ACGOXBLOCK                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACJOBBLOCK                                                                    
         PRINT OFF                                                              
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACLDGTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGETURLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGETURLD                                                      
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
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS AND TABLES                                                 *         
***********************************************************************         
ACEW02   CSECT                                                                  
         SPACE 1                                                                
*                                ** Summary Email **                            
EMLHTML  DS    0H                                                               
*                                                                               
ELEMT    DS    0H                   Top Section                                 
*                                                                               
ELEM5    DC    AL1(ELEM5L)                                                      
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
ELEM5L   EQU   *-ELEM5                                                          
*                                                                               
ELEM10   DC    AL1(ELEM10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
ELEM10L  EQU   *-ELEM10                                                         
*                                                                               
ELEM15   DC    AL1(ELEM15L)                                                     
         DC    C'<head>'                                                        
ELEM15L  EQU   *-ELEM15                                                         
*                                                                               
ELEM20   DC    AL1(ELEM20L)                                                     
         DC    C'<title>'                                                       
ELEM20L  EQU   *-ELEM20                                                         
*                                                                               
ELEM25   DC    AL1(ELEM25L)                                                     
         DC    C'</title>'                                                      
ELEM25L  EQU   *-ELEM25                                                         
*                                                                               
ELEM30   DC    AL1(ELEM30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
ELEM30L  EQU   *-ELEM30                                                         
*                                                                               
ELEM35   DC    AL1(ELEM35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
ELEM35L  EQU   *-ELEM35                                                         
*                                                                               
ELEM40   DC    AL1(ELEM40L)                                                     
         DC    C'</head>'                                                       
ELEM40L  EQU   *-ELEM40                                                         
*                                                                               
ELEM45   DC    AL1(ELEM45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
ELEM45L  EQU   *-ELEM45                                                         
*                                                                               
ELEM50   DC    AL1(ELEM50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
ELEM50L  EQU   *-ELEM50                                                         
*                                                                               
ELEM55   DC    AL1(ELEM55L)                                                     
         DC    C'<table width="800" border="0" cellspacing="0" '                
         DC    C'cellpadding="0" bgcolor="#ffffff" '                            
         DC    C'style="table-layout:fixed;">'                                  
ELEM55L  EQU   *-ELEM55                                                         
*                                                                               
ELEM60   DC    AL1(ELEM60L)                                                     
         DC    C'<tr>'                                                          
ELEM60L  EQU   *-ELEM60                                                         
*                                                                               
ELEM65   DC    AL1(ELEM65L)                                                     
         DC    C'<td>'                                                          
ELEM65L  EQU   *-ELEM65                                                         
*                                                                               
ELEM70   DC    AL1(ELEM70L)                                                     
         DC    C'<!-- Inner Wrap -->'                                           
ELEM70L  EQU   *-ELEM70                                                         
*                                                                               
ELEM75   DC    AL1(ELEM75L)                                                     
         DC    C'<table style="width:800px;" border="0" '                       
         DC    C'cellspacing="0" cellpadding="0" align="center">'               
ELEM75L  EQU   *-ELEM75                                                         
*                                                                               
ELEM80   DC    AL1(ELEM80L)                                                     
         DC    C'<tr>'                                                          
ELEM80L  EQU   *-ELEM80                                                         
*                                                                               
ELEM85   DC    AL1(ELEM85L)                                                     
         DC    C'<td>'                                                          
ELEM85L  EQU   *-ELEM85                                                         
*                                                                               
ELEM90   DC    AL1(ELEM90L)                                                     
         DC    C'<!-- top shadow -->'                                           
ELEM90L  EQU   *-ELEM90                                                         
*                                                                               
ELEM95   DC    AL1(ELEM95L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0"'                
         DC    C'cellpadding="0">'                                              
ELEM95L  EQU   *-ELEM95                                                         
*                                                                               
ELEM100  DC    AL1(ELEM100L)                                                    
         DC    C'<tr>'                                                          
ELEM100L EQU   *-ELEM100                                                        
*                                                                               
ELEM105  DC    AL1(ELEM105L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM105L EQU   *-ELEM105                                                        
*                                                                               
ELEM110  DC    AL1(ELEM110L)                                                    
         DC    C'</tr>'                                                         
ELEM110L EQU   *-ELEM110                                                        
*                                                                               
ELEM115  DC    AL1(ELEM115L)                                                    
         DC    C'</table>'                                                      
ELEM115L EQU   *-ELEM115                                                        
*                                                                               
ELEM120  DC    AL1(ELEM120L)                                                    
         DC    C'<!-- header-->'                                                
ELEM120L EQU   *-ELEM120                                                        
*                                                                               
ELEM125  DC    AL1(ELEM125L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0">'                              
ELEM125L EQU   *-ELEM125                                            '           
*                                                                               
ELEM130  DC    AL1(ELEM130L)                                                    
         DC    C'<tr>'                                                          
ELEM130L EQU   *-ELEM130                                                        
*                                                                               
ELEM135  DC    AL1(ELEM135L)                                                    
         DC    C'<td>'                                                          
ELEM135L EQU   *-ELEM135                                                        
*                                                                               
ELEM140  DC    AL1(ELEM140L)                                                    
         DC    C'<a href="'                                                     
ELEM140E DS    CL20                                                             
         DC    C'">'                                                            
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
ELEM140L EQU   *-ELEM140                                                        
*                                                                               
ELEM145  DC    AL1(ELEM145L)                                                    
         DC    C'</a>'                                                          
ELEM145L EQU   *-ELEM145                                                        
*                                                                               
ELEM150  DC    AL1(ELEM150L)                                                    
         DC    C'</td>'                                                         
ELEM150L EQU   *-ELEM150                                                        
*                                                                               
ELEM155  DC    AL1(ELEM155L)                                                    
         DC    C'</tr>'                                                         
ELEM155L EQU   *-ELEM155                                                        
*                                                                               
ELEM160  DC    AL1(ELEM160L)                                                    
         DC    C'</table>'                                                      
ELEM160L EQU   *-ELEM160                                                        
*                                                                               
ELEM165  DC    AL1(ELEM165L)                                                    
         DC    C'<!-- end: header -->'                                          
ELEM165L EQU   *-ELEM165                                                        
*                                                                               
ELEM170  DC    AL1(ELEM170L)                                                    
         DC    C'<!-- body -->'                                                 
ELEM170L EQU   *-ELEM170                                                        
*                                                                               
ELEM175  DC    AL1(ELEM175L)                                                    
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
ELEM175L EQU   *-ELEM175                                                        
*                                                                               
ELEM180  DC    AL1(ELEM180L)                                                    
         DC    C'<tr>'                                                          
ELEM180L EQU   *-ELEM180                                                        
*                                                                               
ELEM185  DC    AL1(ELEM185L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM185L EQU   *-ELEM185                                                        
*                                                                               
ELEM190  DC    AL1(ELEM190L)                                                    
         DC    C'</tr>'                                                         
ELEM190L EQU   *-ELEM190                                                        
*                                                                               
ELEM195  DC    AL1(ELEM195L)                                                    
         DC    C'</table>'                                                      
ELEM195L EQU   *-ELEM195                                                        
*                                                                               
ELEM200  DC    AL1(ELEM200L)                                                    
         DC    C'<!-- tape check module -->'                                    
ELEM200L EQU   *-ELEM200                                                        
*                                                                               
ELEMTL   EQU   *-ELEMT                                                          
*                                     ** Email section **                       
ELEMSECT DS    0X                                                               
*                                                                               
ELEM205  DC    AL1(ELEM205L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0" style="font-family:'           
ELEM205L EQU   *-ELEM205                                                        
*                                                                               
ELEM210  DC    AL1(ELEM210L)                                                    
         DC    C'Calibri, sans-serif; font-size:13px; color:#4E4D4C; '          
         DC    C'background-color:#D9D9CD; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM210L EQU   *-ELEM210                                                        
*                                                                               
ELEM215  DC    AL1(ELEM215L)                                                    
         DC    C'<tr>'                                                          
ELEM215L EQU   *-ELEM215                                                        
*                                                                               
ELEM220  DC    AL1(ELEM220L)                                                    
         DC    C'<td height="20" colspan="5" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM220L EQU   *-ELEM220                                                        
*                                                                               
ELEM225  DC    AL1(ELEM225L)                                                    
         DC    C'<img height="20" alt="spacer" src="http://info.'               
         DC    C'mediaocean.com/rs/331-XPM-231/images/spacer_icon.png"'         
         DC    C'/>'                                                            
ELEM225L EQU   *-ELEM225                                                        
*                                                                               
ELEM230  DC    AL1(ELEM230L)                                                    
         DC    C'</td>'                                                         
ELEM230L EQU   *-ELEM230                                                        
*                                                                               
ELEM235  DC    AL1(ELEM235L)                                                    
         DC    C'</tr>'                                                         
ELEM235L EQU   *-ELEM235                                                        
*                                                                               
ELEM240  DC    AL1(ELEM240L)                                                    
         DC    C'<tr>'                                                          
ELEM240L EQU   *-ELEM240                                                        
*                                                                               
ELEM245  DC    AL1(ELEM245L)                                                    
         DC    C'<td width="20">'                                               
ELEM245L EQU   *-ELEM245                                                        
*                                                                               
ELEM250  DC    AL1(ELEM250L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231'                
         DC    C'/images/spacer_icon.png"/>'                                    
ELEM250L EQU   *-ELEM250                                                        
*                                                                               
ELEM255  DC    AL1(ELEM255L)                                                    
         DC    C'</td>'                                                         
ELEM255L EQU   *-ELEM255                                                        
*                                                                               
ELEM260  DC    AL1(ELEM260L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:36px;">'            
ELEM260L EQU   *-ELEM260                                                        
*                                                                               
ELEM265  DC    AL1(ELEM265L)                                                    
         DC    C'<div>'                                                         
ELEM265E DS    CL80                Here are the details.....                    
         DC    C'</div>'                                                        
ELEM265L EQU   *-ELEM265                                                        
*                                                                               
ELEM270  DC    AL1(ELEM270L)                                                    
         DC    C'<p>'                                                           
ELEM270L EQU   *-ELEM270                                                        
*                                                                               
ELEM271  DC    AL1(ELEM271L)                                                    
ELEM271E DS    CL(MAXELEN)                                                      
ELEM271L EQU   *-ELEM271                                                        
*                                                                               
ELEM272  DC    AL1(ELEM272L)                                                    
ELEM272E DS    CL(MAXELEN)                                                      
ELEM272L EQU   *-ELEM272                                                        
*                                                                               
ELEM273  DC    AL1(ELEM273L)                                                    
ELEM273E DS    CL(MAXELEN)                                                      
ELEM273L EQU   *-ELEM273                                                        
*                                                                               
ELEM275  DC    AL1(ELEM275L)                                                    
         DC    C'<a style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; font-weight:bold;" href="'               
ELEM275L EQU   *-ELEM275                                                        
*                                                                               
ELEM276  DC    AL1(ELEM276L)                                                    
ELEM276E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM276L EQU   *-ELEM276                                                        
*                                                                               
ELEM278  DC    AL1(ELEM276L)                                                    
ELEM278E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM278L EQU   *-ELEM278                                                        
*                                                                               
ELEM280  DC    AL1(ELEM280L)                                                    
ELEM280E DS    CL80               Please click here to access....               
ELEM280L EQU   *-ELEM280                                                        
*                                                                               
ELEM285  DC    AL1(ELEM285L)                                                    
         DC    C'</a>'                                                          
ELEM285L EQU   *-ELEM285                                                        
*                                                                               
ELEM290  DC    AL1(ELEM290L)                                                    
         DC    C'</p>'                                                          
ELEM290L EQU   *-ELEM290                                                        
*                                                                               
ELEM295  DC    AL1(ELEM295L)                                                    
         DC    C'</td>'                                                         
ELEM295L EQU   *-ELEM295                                                        
*                                                                               
ELEM300  DC    AL1(ELEM300L)                                                    
         DC    C'<td width="20">'                                               
ELEM300L EQU   *-ELEM300                                                        
*                                                                               
ELEM305  DC    AL1(ELEM305L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" src="'                 
         DC    C'http://info.mediaocean.com/rs/331-XPM-231/images/'             
         DC    C'spacer_icon.png"/>'                                            
ELEM305L EQU   *-ELEM305                                                        
*                                                                               
ELEM310  DC    AL1(ELEM310L)                                                    
         DC    C'</td>'                                                         
ELEM310L EQU   *-ELEM310                                                        
*                                                                               
ELEM315  DC    AL1(ELEM315L)                                                    
         DC    C'<td style="vertical-align:top; font-family: Calibri, '         
         DC    C'sans-serif; font-size:13px; color:#4E4D4C; '                   
         DC    C'line-height:20px;">'                                           
ELEM315L EQU   *-ELEM315                                                        
*                                                                               
ELEM320  DC    AL1(ELEM320L)                                                    
         DC    C'<table border="0" style="background-color:#ffffff;">'          
ELEM320L EQU   *-ELEM320                                                        
*                                                                               
ELEM325  DC    AL1(ELEM325L)                                                    
         DC    C'<tr>'                                                          
ELEM325L EQU   *-ELEM325                                                        
*                                                                               
ELEM330  DC    AL1(ELEM330L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM330L EQU   *-ELEM330                                                        
*                                                                               
ELEM335  DC    AL1(ELEM335L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer" '         
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM335L EQU   *-ELEM335                                                        
*                                                                               
ELEM340  DC    AL1(ELEM340L)                                                    
         DC    C'</td>'                                                         
ELEM340L EQU   *-ELEM340                                                        
*                                                                               
ELEM345  DC    AL1(ELEM345L)                                                    
         DC    C'</tr>'                                                         
ELEM345L EQU   *-ELEM345                                                        
*                                                                               
ELEMOFF  DS    0X                                                               
*                                                                               
ELEM346  DC    AL1(ELEM346L)                                                    
         DC    C'<tr>'                                                          
ELEM346L EQU   *-ELEM346                                                        
*                                                                               
ELEM350  DC    AL1(ELEM350L)                                                    
         DC    C'<td width="20">'                                               
ELEM350L EQU   *-ELEM350                                                        
*                                                                               
ELEM355  DC    AL1(ELEM355L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM355L EQU   *-ELEM355                                                        
*                                                                               
ELEM360  DC    AL1(ELEM360L)                                                    
         DC    C'</td>'                                                         
ELEM360L EQU   *-ELEM360                                                        
*                                                                               
ELEM365  DC    AL1(ELEM365L)                                                    
         DC    C'<td width="340">'                                              
ELEM365L EQU   *-ELEM365                                                        
*                                                                               
ELEM366  DC    AL1(ELEM366L)                                                    
         DC    C'<div style="width:360px; background-color:#ffffff; '           
         DC    C'font-family: Calibri, sans-serif; font-size:13px;">'           
ELEM366L EQU   *-ELEM366                                                        
*                                                                               
ELEM370  DC    AL1(ELEM370L)                                                    
ELEM370E DS    CL120                for the period ending                       
         DC    C'</div>'                                                        
         DC    C'</td>'                                                         
ELEM370L EQU   *-ELEM370                                                        
*                                                                               
ELEM375  DC    AL1(ELEM375L)                                                    
         DC    C'</tr>'                                                         
ELEM375L EQU   *-ELEM375                                                        
*                                                                               
ELEM380  DC    AL1(ELEM380L)                                                    
         DC    C'<tr>'                                                          
ELEM380L EQU   *-ELEM380                                                        
*                                                                               
ELEM385  DC    AL1(ELEM385L)                                                    
         DC    C'<td width="20">'                                               
ELEM385L EQU   *-ELEM385                                                        
*                                                                               
ELEM390  DC    AL1(ELEM390L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM390L EQU   *-ELEM390                                                        
*                                                                               
ELEM395  DC    AL1(ELEM395L)                                                    
         DC    C'</td>'                                                         
ELEM395L EQU   *-ELEM395                                                        
*                                                                               
ELEM400  DC    AL1(ELEM400L)                                                    
         DC    C'<td>'                                                          
ELEM400L EQU   *-ELEM400                                                        
*                                                                               
ELEM405  DC    AL1(ELEM405L)                                                    
         DC    C'<div style="width:340px; height:380px; '                       
         DC    C'overflow-y:scroll; background-color:#ffffff;">'                
ELEM405L EQU   *-ELEM405                                                        
*                                                                               
ELEM410  DC    AL1(ELEM410L)                                                    
         DC    C'<table cellpadding="5" style="font-family: Calibri, '          
         DC    C'sans-serif; font-size:13px;">'                                 
ELEM410L EQU   *-ELEM410                                                        
*                                                                               
** list of timesheets goes here see TABROD DSECT **                             
                                                                                
ELEMTML  DS    0X                                                               
*                                                                               
ELEM415  DC    AL1(ELEM415L)                                                    
         DC    C'</table>'                                                      
ELEM415L EQU   *-ELEM415                                                        
*                                                                               
ELEM420  DC    AL1(ELEM420L)                                                    
         DC    C'</div>'                                                        
ELEM420L EQU   *-ELEM420                                                        
*                                                                               
ELEM425  DC    AL1(ELEM425L)                                                    
         DC    C'</td>'                                                         
ELEM425L EQU   *-ELEM425                                                        
*                                                                               
ELEM430  DC    AL1(ELEM430L)                                                    
         DC    C'</tr>'                                                         
ELEM430L EQU   *-ELEM430                                                        
*                                                                               
ELEM440  DC    AL1(ELEM440L)                                                    
         DC    C'<tr>'                                                          
ELEM440L EQU   *-ELEM440                                                        
*                                                                               
ELEM445  DC    AL1(ELEM445L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM445L EQU   *-ELEM445                                                        
*                                                                               
ELEM450  DC    AL1(ELEM450L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer" '         
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM450L EQU   *-ELEM450                                                        
*                                                                               
ELEM455  DC    AL1(ELEM455L)                                                    
         DC    C'</td>'                                                         
ELEM455L EQU   *-ELEM455                                                        
*                                                                               
ELEM460  DC    AL1(ELEM460L)                                                    
         DC    C'</tr>'                                                         
ELEM460L EQU   *-ELEM460                                                        
*                                                                               
ELEMOEN  DS    0X                                                               
*                                                                               
ELEM465  DC    AL1(ELEM465L)                                                    
         DC    C'</table>'                                                      
ELEM465L EQU   *-ELEM465                                                        
*                                                                               
ELEM470  DC    AL1(ELEM470L)                                                    
         DC    C'</td>'                                                         
ELEM470L EQU   *-ELEM470                                                        
*                                                                               
ELEM475  DC    AL1(ELEM475L)                                                    
         DC    C'<td width="20">'                                               
ELEM475L EQU   *-ELEM475                                                        
*                                                                               
ELEM480  DC    AL1(ELEM480L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM480L EQU   *-ELEM480                                                        
*                                                                               
ELEM485  DC    AL1(ELEM485L)                                                    
         DC    C'</td>'                                                         
ELEM485L EQU   *-ELEM485                                                        
*                                                                               
ELEM490  DC    AL1(ELEM490L)                                                    
         DC    C'</tr>'                                                         
ELEM490L EQU   *-ELEM490                                                        
*                                                                               
ELEM495  DC    AL1(ELEM495L)                                                    
         DC    C'<tr>'                                                          
ELEM495L EQU   *-ELEM495                                                        
*                                                                               
ELEM500  DC    AL1(ELEM500L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM500L EQU   *-ELEM500                                                        
*                                                                               
ELEM505  DC    AL1(ELEM505L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM505L EQU   *-ELEM505                                                        
*                                                                               
ELEM510  DC    AL1(ELEM510L)                                                    
         DC    C'</td>'                                                         
ELEM510L EQU   *-ELEM510                                                        
*                                                                               
ELEM515  DC    AL1(ELEM515L)                                                    
         DC    C'</tr>'                                                         
ELEM515L EQU   *-ELEM515                                                        
*                                                                               
ELEM520  DC    AL1(ELEM520L)                                                    
         DC    C'</table>'                                                      
ELEM520L EQU   *-ELEM520                                                        
*                                      ** end of email section **               
ELEMSEND DS    0X                      ** end of email **                       
*                                                                               
ELEM525  DC    AL1(ELEM525L)                                                    
         DC    C'<!-- no background module -->'                                 
ELEM525L EQU   *-ELEM525                                                        
*                                                                               
ELEM530  DC    AL1(ELEM530L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellspadding="0" style="font-family: '         
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
ELEM530L EQU   *-ELEM530                                                        
*                                                                               
ELEM535  DC    AL1(ELEM535L)                                                    
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM535L EQU   *-ELEM535                                                        
*                                                                               
ELEM540  DC    AL1(ELEM540L)                                                    
         DC    C'<tr>'                                                          
ELEM540L EQU   *-ELEM540                                                        
*                                                                               
ELEM545  DC    AL1(ELEM545L)                                                    
         DC    C'<td rowspan="3" width="30"><img height="1" width="30"'         
         DC    C' border="0" alt="spacer" src="http://info.mediaocean'          
         DC    C'.com/rs/331-XPM-231/images/spacer_icon.png"/>'                 
         DC    C'</td>'                                                         
ELEM545L EQU   *-ELEM545                                                        
*                                                                               
ELEM550  DC    AL1(ELEM550L)                                                    
         DC    C'<td>'                                                          
         DC    X'50'                                                            
         DC    C'nbsp;</td>'                                                    
ELEM550L EQU   *-ELEM550                                                        
*                                                                               
ELEM555  DC    AL1(ELEM555L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM555L EQU   *-ELEM555                                                        
*                                                                               
ELEM560  DC    AL1(ELEM560L)                                                    
         DC    C'</td>'                                                         
ELEM560L EQU   *-ELEM560                                                        
*                                                                               
ELEM565  DC    AL1(ELEM565L)                                                    
         DC    C'</tr>'                                                         
ELEM565L EQU   *-ELEM565                                                        
*                                                                               
ELEM570  DC    AL1(ELEM570L)                                                    
         DC    C'<tr>'                                                          
ELEM570L EQU   *-ELEM570                                                        
*                                                                               
ELEM575  DC    AL1(ELEM575L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
ELEM575L EQU   *-ELEM575                                                        
*                                                                               
ELEM580  DC    AL1(ELEM580L)                                                    
         DC    C'<p>'                                                           
ELEM580L EQU   *-ELEM580                                                        
*                                                                               
ELEM585  DC    AL1(ELEM585L)                                                    
EMAILL1  DS    CL80                                                             
EMAILL2  DS    CL80                                                             
ELEM585L EQU   *-ELEM585                                                        
*                                                                               
ELEM600  DC    AL1(ELEM600L)                                                    
EMAILL3  DS    CL80                                                             
EMAILL4  DS    CL80                                                             
ELEM600L EQU   *-ELEM600                                                        
*                                                                               
ELEM605  DC    AL1(ELEM605L)                                                    
EMAILL5  DS    CL80                                                             
         DC    C'</p>'                                                          
ELEM605L EQU   *-ELEM605                                                        
*                                                                               
ELEM610  DC    AL1(ELEM610L)                                                    
         DC    C'<p>'                                                           
ELEM610L EQU   *-ELEM610                                                        
*                                                                               
ELEM615  DC    AL1(ELEM615L)                                                    
EMAILL6  DS    CL80                                                             
EMAILL7  DS    CL80                                                             
ELEM615L EQU   *-ELEM615                                                        
*                                                                               
ELEM630  DC    AL1(ELEM630L)                                                    
EMAILL8  DS    CL80                                                             
EMAILL9  DS    CL80                                                             
ELEM630L EQU   *-ELEM630                                                        
*                                                                               
ELEM632  DC    AL1(ELEM632L)                                                    
EMAILLA  DS    CL80                                                             
ELEM632L EQU   *-ELEM632                                                        
*                                                                               
ELEM635  DC    AL1(ELEM635L)                                                    
         DC    C'</p>'                                                          
ELEM635L EQU   *-ELEM635                                                        
*                                                                               
ELEM655  DC    AL1(ELEM655L)                                                    
         DC    C'</td>'                                                         
ELEM655L EQU   *-ELEM655                                                        
*                                                                               
ELEM660  DC    AL1(ELEM660L)                                                    
         DC    C'</tr>'                                                         
ELEM660L EQU   *-ELEM660                                                        
*                                                                               
ELEM665  DC    AL1(ELEM665L)                                                    
         DC    C'<tr>'                                                          
ELEM665L EQU   *-ELEM665                                                        
*                                                                               
ELEM670  DC    AL1(ELEM670L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM670L EQU   *-ELEM670                                                        
*                                                                               
ELEM675  DC    AL1(ELEM675L)                                                    
         DC    C'<img height="20" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM675L EQU   *-ELEM675                                                        
*                                                                               
ELEM680  DC    AL1(ELEM680L)                                                    
         DC    C'</td>'                                                         
ELEM680L EQU   *-ELEM680                                                        
*                                                                               
ELEM685  DC    AL1(ELEM685L)                                                    
         DC    C'</tr>'                                                         
ELEM685L EQU   *-ELEM685                                                        
*                                                                               
ELEM690  DC    AL1(ELEM690L)                                                    
         DC    C'</table>'                                                      
ELEM690L EQU   *-ELEM690                                                        
*                                                                               
ELEM695  DC    AL1(ELEM695L)                                                    
         DC    C'<!-- end: body -->'                                            
ELEM695L EQU   *-ELEM695                                                        
*                                                                               
ELEM700  DC    AL1(ELEM700L)                                                    
         DC    C'<!-- Footer -->'                                               
ELEM700L EQU   *-ELEM700                                                        
*                                                                               
ELEM705  DC    AL1(ELEM705L)                                                    
         DC    C'<table width="100%" cellspacing="0" cellpadding="0" '          
         DC    C'style="height:78px; font-family: Calibri, sans-serif;'         
ELEM705L EQU   *-ELEM705                                                        
*                                                                               
ELEM710  DC    AL1(ELEM710L)                                                    
         DC    C' font-size:10px; color:#ffffff; '                              
         DC    C'background-color:#4E4D4C; text-align:center; '                 
         DC    C'border-bottom:1px solid #4E4D4C; '                             
ELEM710L EQU   *-ELEM710                                                        
*                                                                               
ELEM715  DC    AL1(ELEM715L)                                                    
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
ELEM715L EQU   *-ELEM715                                                        
*                                                                               
ELEM720  DC    AL1(ELEM720L)                                                    
         DC    C'<tr>'                                                          
ELEM720L EQU   *-ELEM720                                                        
*                                                                               
ELEM725  DC    AL1(ELEM725L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM725L EQU   *-ELEM725                                                        
*                                                                               
ELEM730  DC    AL1(ELEM730L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM730L EQU   *-ELEM730                                                        
*                                                                               
ELEM735  DC    AL1(ELEM735L)                                                    
         DC    C'</td>'                                                         
ELEM735L EQU   *-ELEM735                                                        
*                                                                               
ELEM740  DC    AL1(ELEM740L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM740L EQU   *-ELEM740                                                        
*                                                                               
ELEM745  DC    AL1(ELEM745L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM745L EQU   *-ELEM745                                                        
*                                                                               
ELEM750  DC    AL1(ELEM750L)                                                    
         DC    C'</td>'                                                         
ELEM750L EQU   *-ELEM750                                                        
*                                                                               
ELEM755  DC    AL1(ELEM755L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM755L EQU   *-ELEM755                                                        
*                                                                               
ELEM760  DC    AL1(ELEM760L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM760L EQU   *-ELEM760                                                        
*                                                                               
ELEM765  DC    AL1(ELEM765L)                                                    
         DC    C'</td>'                                                         
ELEM765L EQU   *-ELEM765                                                        
*                                                                               
ELEM770  DC    AL1(ELEM770L)                                                    
         DC    C'</tr>'                                                         
ELEM770L EQU   *-ELEM770                                                        
*                                                                               
ELEM775  DC    AL1(ELEM775L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
ELEM775L EQU   *-ELEM775                                                        
*                                                                               
ELEM780  DC    AL1(ELEM780L)                                                    
         DC    C'<td style="text-align:left;">'                                 
ELEM780L EQU   *-ELEM780                                                        
*                                                                               
ELEM785  DC    AL1(ELEM785L)                                                    
         DC    C'<div>'                                                         
ELEM785L EQU   *-ELEM785                                                        
*                                                                               
ELEM790  DC    AL1(ELEM790L)                                                    
ELEM790E DS    CL80                                                             
ELEM790L EQU   *-ELEM790                                                        
*                                                                               
ELEM795  DC    AL1(ELEM795L)                                                    
         DC    C'<br />'                                                        
ELEM795L EQU   *-ELEM795                                                        
*                                                                               
ELEM800  DC    AL1(ELEM800L)                                                    
         DC    X'50'                                                            
         DC    C'copy '                                                         
ELEM800E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
ELEM800L EQU   *-ELEM800                                                        
*                                                                               
ELEM805  DC    AL1(ELEM805L)                                                    
         DC    C'| '                                                            
ELEM805E DS    CL18                                                             
         DC    C':'                                                             
ELEM805L EQU   *-ELEM805                                                        
*                                                                               
ELEM810  DC    AL1(ELEM810L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM810L EQU   *-ELEM810                                                        
*                                                                               
ELEM815  DC    AL1(ELEM815L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM815L EQU   *-ELEM815                                                        
*                                                                               
ELEM820  DC    AL1(ELEM820L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM820L EQU   *-ELEM820                                                        
*                                                                               
ELEM822  DC    AL1(ELEM822L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
ELEM822L EQU   *-ELEM822                                                        
*                                                                               
ELEM825  DC    AL1(ELEM825L)                                                    
         DC    C'</div>'                                                        
ELEM825L EQU   *-ELEM825                                                        
*                                                                               
ELEM830  DC    AL1(ELEM830L)                                                    
         DC    C'</td>'                                                         
ELEM830L EQU   *-ELEM830                                                        
*                                                                               
ELEM835  DC    AL1(ELEM835L)                                                    
         DC    C'<td style="text-align:right;">'                                
ELEM835L EQU   *-ELEM835                                                        
*                                                                               
ELEM840  DC    AL1(ELEM840L)                                                    
         DC    C'<a href="'                                                     
ELEM840E DS    CL20                                                             
         DC    C'">'                                                            
ELEM840L EQU   *-ELEM840                                                        
*                                                                               
ELEM845  DC    AL1(ELEM845L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-Aura-footer-logo.png" alt="MediaOcean" />'           
ELEM845L EQU   *-ELEM845                                                        
*                                                                               
ELEM850  DC    AL1(ELEM850L)                                                    
         DC    C'</a>'                                                          
ELEM850L EQU   *-ELEM850                                                        
*                                                                               
ELEM855  DC    AL1(ELEM855L)                                                    
         DC    C'</td>'                                                         
ELEM855L EQU   *-ELEM855                                                        
*                                                                               
ELEM860  DC    AL1(ELEM860L)                                                    
         DC    C'</tr>'                                                         
ELEM860L EQU   *-ELEM860                                                        
*                                                                               
ELEM865  DC    AL1(ELEM865L)                                                    
         DC    C'<tr>'                                                          
ELEM865L EQU   *-ELEM865                                                        
*                                                                               
ELEM870  DC    AL1(ELEM870L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM870L EQU   *-ELEM870                                                        
*                                                                               
ELEM875  DC    AL1(ELEM875L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM875L EQU   *-ELEM875                                                        
*                                                                               
ELEM880  DC    AL1(ELEM880L)                                                    
         DC    C'</td>'                                                         
ELEM880L EQU   *-ELEM880                                                        
*                                                                               
ELEM885  DC    AL1(ELEM885L)                                                    
         DC    C'</tr>'                                                         
ELEM885L EQU   *-ELEM885                                                        
*                                                                               
ELEM890  DC    AL1(ELEM890L)                                                    
         DC    C'</table>'                                                      
ELEM890L EQU   *-ELEM890                                                        
*                                                                               
ELEM895  DC    AL1(ELEM895L)                                                    
         DC    C'<!-- end: Footer -->'                                          
ELEM895L EQU   *-ELEM895                                                        
*                                                                               
ELEM900  DC    AL1(ELEM900L)                                                    
         DC    C'</td>'                                                         
ELEM900L EQU   *-ELEM900                                                        
*                                                                               
ELEM905  DC    AL1(ELEM905L)                                                    
         DC    C'</tr>'                                                         
ELEM905L EQU   *-ELEM905                                                        
*                                                                               
ELEM910  DC    AL1(ELEM910L)                                                    
         DC    C'</table>'                                                      
ELEM910L EQU   *-ELEM910                                                        
*                                                                               
ELEM915  DC    AL1(ELEM915L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
ELEM915L EQU   *-ELEM915                                                        
*                                                                               
ELEM920  DC    AL1(ELEM920L)                                                    
         DC    C'</td>'                                                         
ELEM920L EQU   *-ELEM920                                                        
*                                                                               
ELEM925  DC    AL1(ELEM925L)                                                    
         DC    C'</tr>'                                                         
ELEM925L EQU   *-ELEM925                                                        
*                                                                               
ELEM930  DC    AL1(ELEM930L)                                                    
         DC    C'</table>'                                                      
ELEM930L EQU   *-ELEM930                                                        
*                                                                               
ELEM935  DC    AL1(ELEM935L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
ELEM935L EQU   *-ELEM935                                                        
*                                                                               
ELEM940  DC    AL1(ELEM940L)                                                    
         DC    C'</body>'                                                       
ELEM940L EQU   *-ELEM940                                                        
*                                                                               
ELEM945  DC    AL1(ELEM945L)                                                    
         DC    C'</html>'                                                       
ELEM945L EQU   *-ELEM945                                                        
*                                                                               
EMLHTMLL EQU   *-EMLHTML                                                        
ELEMSENX DC    X'FF'                                                            
         DS    0D                                                               
CAPTEAM  DS    XL200               CURRENT CLI+PRO LIDTTEAJ ENTRIES             
ELEAREA  DS    XL6144                                                           
COLIST   DS    XL200                                                            
COLTAB   DS    XL3600                                                           
*                                                                               
*&&US                                                                           
GOXBLCKA DS    XL(GOXBLKX-GOXBLOCK)         (400)                               
*&&                                                                             
         SPACE 1                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPEW02 08/05/20'                                      
         END                                                                    
