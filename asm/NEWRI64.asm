*          DATA SET NEWRI64    AT LEVEL 022 AS OF 03/14/18                      
*          DATA SET NEWRI64    AT LEVEL 020 AS OF 07/30/98                      
*PHASE T32064A,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'T32064 - CPM SEED REPORT'                                       
T32064   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE64**,RR=R2                                                 
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING T32064,RB,R5       NOTE R5 = 2ND BASE REGISTER                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          ANETWS1/ANETWS2/ANETWS3                      
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         MVC   AMYIO,ANETWS4       ANETWS4 = MY I/O AREA                        
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,XDDEMBLK                                                      
         ST    R1,NBADEM                                                        
*        BAS   RE,TRAPIT2                                                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   SKIP10                                                           
         L     R1,=A(MYNEBUFF)     LOAD NETWORK BUFF OFFLINE                    
         A     R1,RELO                                                          
         ST    R1,NBANBUFF                                                      
         OI    NBINDS7,NBI7NTIO                                                 
*                                                                               
SKIP10   CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,EDITMOD                                                       
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   FTERMFLG,0          REQUIRED FIELDS                              
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLCLIH          *CLIENT                                      
         NETGO NVGETFLD,DMCB                                                    
         NETGO NVCLI,DMCB                                                       
*        BAS   RE,TRAPIT                                                        
*                                                                               
         XC    MYWORK(20),MYWORK   *PRODUCT (FUDGE)                             
         MVI   MYWORK,20                                                        
         MVI   MYWORK+5,3                                                       
         MVC   MYWORK+8(3),=C'ALL'                                              
         LA    R2,MYWORK                                                        
         NETGO NVPRDALL,DMCB                                                    
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLESTH                        *ESTIMATE                      
         NETGO NVGETFLD,DMCB                                                    
         OI    NBINDS6,NBI6XDEM                                                 
         NETGO NVESTRNG,DMCB,0,XDDEMBLK                                         
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLNETH                         *NETWORK                      
         NETGO NVGETFLD,DMCB                                                    
****     NETGO NVNET,DMCB,SPLNETN                                               
         NETGO NVNET,DMCB,0                                                     
*        BAS   RE,TRAPIT                                                        
         L     R2,NBAIO                                                         
         USING STAREC,R2                                                        
         CLI   SPTYPE,C'N'         IF NOT NET OR SYND                           
         BE    ED05                                                             
         CLI   SPTYPE,C'S'                                                      
         BE    ED05                                                             
         MVI   DOHUNS,C'Y'         SET HUNOPT FLAG                              
         DROP  R2                                                               
*                                                                               
ED05     MVI   FTERMFLG,1                         (OPTIONAL)                    
         LA    R2,SPLPAKH                         *PACKAGE                      
         MVI   NBDATA,C'P'                                                      
         NETGO NVGETFLD,DMCB                                                    
         NETGO NVPAKALL,DMCB                                                    
*        BAS   RE,TRAPIT                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         LA    R2,SPLDPTH                  *DAYPART                             
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLSTDH                  *START DATE                          
         NETGO NVSTRDAT,DMCB                                                    
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLENDH                  *END DATE                            
         NETGO NVENDDAT,DMCB                                                    
*        BAS   RE,TRAPIT                                                        
*                                                                               
         LA    R2,SPLCSTH                  *COST (ACTUAL/PACKAGE)               
         MVI   COSTYP,C'A'         ACTUAL COST IS DEFAULT                       
         CLI   5(R2),0                                                          
         BE    ED12                                                             
         MVC   COSTYP,SPLCST                                                    
*        BAS   RE,TRAPIT                                                        
         CLI   COSTYP,C'P'         PACKAGE                                      
         BE    ED12                                                             
         CLI   COSTYP,C'A'                                                      
         BE    ED12                                                             
         CLI   COSTYP,C'M'         MAX COST                                     
         BNE   EDINV                                                            
*                                                                               
ED12     DS    0H                   *PKG GUARANTEE                              
         MVI   DOPAKG,0                                                         
         LA    R2,SPLPDMH           PKG GUARANTEE                               
         CLC   =C'DELETE',SPLPDM   DELETE PKG GUARANTEE?                        
         BNE   ED12D                                                            
         MVI   DOPAKG,C'P'         YES-DELETE PKG GUAR                          
         B     ED14B                                                            
*                                                                               
ED12D    CLI   5(R2),0                                                          
         BE    ED13                                                             
         BAS   RE,DEMVALID                                                      
*        BAS   RE,TRAPIT                                                        
         MVC   PDEMVAL,WORK                                                     
         LA    R2,SPLPCPH                                                       
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         B     ED13                                                             
*                                                                               
*                                                                               
                                                                                
ED13     LA    R2,SPLPCPH                                                       
         CLI   5(R2),0                                                          
         BE    ED14                                                             
         MVI   DOPAKG,C'Y'                                                      
         ZIC   R3,5(R2)                                                         
* - VALIDATE FOR 2 DECIMAL PLACES                                               
         GOTO1 CASHVAL,DMCB,(2,SPLPCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
         MVC   PKPKGCPM,4(R1)      SET TO PASS TO PKGREC                        
         OC    PDEMVAL,PDEMVAL     DID WE SET PKG TARGET DEMO?                  
         BNZ   *+10                                                             
         MVC   PDEMVAL,XDDEMOS       NO/USE 1ST DEMO IN EST                     
* - BUT GET 4 DECIMALS FOR CALCULATION PURPOSES                                 
         GOTO1 CASHVAL,DMCB,(4,SPLPCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
         CLC   =F'0',4(R1)                                                      
         BE    EDINV                                                            
         MVC   TPAKCPM,4(R1)        SET TARGET CPM                              
*        BAS   RE,TRAPIT                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
ED14     LA    R2,SPLDEMH            *DEMO GUARANTEE                            
         CLC   =C'DELETE',SPLDEM   DELETE?                                      
         BNE   ED14C                                                            
         MVI   DOPAKG,C'D'         YES DELETE DEMO GUARANTEE                    
*                                                                               
*********************************  DELETE PKG/DEMO GUARANTEE                    
ED14B    CLI   NBSELESE,0          EST RANGE NOT ALLOWED                        
         BE    *+12                                                             
         LA    R2,SPLESTH          ELSE GIVE ERROR                              
         B     EDINV                                                            
         CLI   NBSELPAK,0          MUST BE SPECIFIC PKG                         
         BNE   *+12                                                             
         LA    R2,SPLPAKH                                                       
         B     EDINV                                                            
         B     ED40                                                             
***********************************                                             
                                                                                
*                                                                               
ED14C    MVI   DODEMG,C'Y'                                                      
         NETGO NVGETFLD,DMCB                                                    
         BNZ   ED20                                                             
*        BAS   RE,TRAPIT                                                        
         CLI   DOPAKG,C'Y'         ..IF NO DEMO GUA NEED PAK GUA                
         BE    ED15                                                             
         LA    R2,SPLPCPH          ..POINT CURSOR TO PKG FIELD                  
         BNE   EDMIS               ..NO PAG GUA = ERROR                         
ED15     MVI   DODEMG,0                                                         
         XC    SPLDCP,SPLDCP       CLEAR DEMO CPM FIELD IN CASE                 
         OI    SPLDCPH+6,X'80'                                                  
         B     ED40                                                             
         SPACE                                                                  
ED20     DS    0H                           VALIDATE DEMO GUARANTEE             
         BAS   RE,DEMVALID         RETURNS 3 BYTE DEMO CODE                     
*        BAS   RE,TRAPIT                                                        
         MVC   TDEMVAL,WORK        NO/SAVE 3 BYTE DEMO CODE                     
*                                                                               
         DS    0H                                                               
ED30     LA    R2,SPLDCPH             *TARGET CPM FOR DEMO                      
         CLI   5(R2),0                                                          
         BE    EDMIS                                                            
         ZIC   R3,5(R2)                                                         
* - VALIDATE FOR 2 DECIMAL PLACES                                               
         GOTO1 CASHVAL,DMCB,(2,SPLDCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
* - BUT GET 4 DECIMALS FOR CALCULATION PURPOSES                                 
         GOTO1 CASHVAL,DMCB,(4,SPLDCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
         CLC   =F'0',4(R1)                                                      
         BE    EDINV                                                            
         MVC   TDEMCPM,4(R1)             SAVE TARGET CPM FOR DEMO               
*        BAS   RE,TRAPIT                                                        
*                                                                               
ED40     MVI   UPDATE,0                                                         
         LA    R2,SPLTSTH         *TEST RUN                                     
         CLI   5(R2),0                                                          
         BE    EDMIS                                                            
         CLI   SPLTST,C'Y'         YES=TEST                                     
         BE    EDTX                                                             
         CLI   SPLTST,C'*'         *=TEST AND PRINT EACH UNIT                   
         BE    EDTX                                                             
         CLI   SPLTST,C'N'         NO=MARK FILE                                 
         BNE   EDINV                                                            
         MVI   UPDATE,C'Y'         MARK UNITS                                   
         MVI   HALF,C'L'           SET TO LOCK                                  
         BAS   RE,LOCKEM                                                        
*                                                                               
EDTX     DS    0H                                                               
*        BAS   RE,TRAPIT                                                        
* - NEED TO HAVE COPIES WRITTERN TO RECOVERY FILE                               
         ICM   R1,15,TWAMASTC                                                   
         BZ    EDTXX                                                            
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
         SPACE                                                                  
EDTXX    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1                                                                   
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
         TM    NBINDS6,NBI6SOX     READ ONLY SYS?                               
         BNO   NOSOX                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'*** SYSTEM NOT FOR UPDATE'                        
         GOTO1 ERREX2                                                           
NOSOX    CLI   NBSELCLI,X'40'                                                   
         BE    *+14                                                             
         CLC   =C'ALL',NBSELCLI                                                 
         BNE   LOCK0                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** UPDATIVE SOON FOR ONE CLIENT'                 
         GOTO1 ERREX2                                                           
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
         DROP  R1                                                               
* - LOCK / UNLOCK                                                               
         CLI   HALF,C'L'           IF LOCKING                                   
         BNE   *+8                                                              
         MVI   TWAWHEN,5           SET FOR UPDATIVE SOON                        
         LA    R3,MYWORK                                                        
         USING LKKEYD,R3                                                        
         XC    MYWORK(L'LOCKEY),MYWORK                                          
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(HALF,MYWORK),(R6)                                     
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    XIT1                                                                   
         SPACE                                                                  
         EJECT                                                                  
                                                                                
* - VALIDATES DEMO- EXPECTS R2 POINT TO FIELD HEADER                            
DEMVALID NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 NBCALLOV,DMCB,0,X'D9000AD9'           (DEMOVAL)                  
         L     RF,DMCB                                                          
         L     R1,AMYIO                                                         
         USING DBLOCK,R1                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'T'                                                    
         ST    R1,DMCB+8                                                        
         DROP  R1                                                               
         GOTO1 (RF),DMCB,(1,(R2)),(1,WORK)                                      
         CLI   DMCB+4,0            ERROR                                        
         BE    EDINV               YES                                          
         XIT1                                                                   
*                                                                               
EDINV    DS    0H                                                               
*        BAS   RE,TRAPIT                                                        
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
EDMIS    DS    0H                                                               
*        BAS   RE,TRAPIT                                                        
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
* REPORT MODE / PROCESS UNITS                                                   
*                                                                               
REPMOD   NTR1                                                                   
*                                                                               
         CLI   DOPAKG,C'D'         DELETE PKG/DEMO GUARANTEE ???                
         BE    *+12                NO                                           
         CLI   DOPAKG,C'P'         DELETE PKG/DEMO GUARANTEE ???                
         BNE   REPMOD0             NO                                           
         MVI   NBDATA,C'B'         YES         UNITS AND PACKAGES               
         MVI   NBRESUME,NBPROCPK               RESTART PKG READ                 
         MVI   NBUSER+13,C'N'                  DON'T FILT PREEMPTS              
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,0                                                       
         B     RP08                                                             
******************************************************************              
*                                                                               
REPMOD0  GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         XC    SORTD(SRTRECLE),SORTD              CLEAR SORT WORK AREA          
         B     RP00                                                             
         SPACE                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=100'                                   
*                                                                               
RP00     ZAP   PKGCOST,=P'0'       PREPARE PACKED FIELDS                        
         ZAP   PKGIMP,=P'0'                                                     
         ZAP   DEMIMP,=P'0'                                                     
         ZAP   PACKWRK,=P'0'                                                    
         MVI   ERRFLG,0            AND ERROR FLAG                               
         SPACE                                                                  
* - SET UP NETIO PARAMETERS                                                     
         MVI   NBDATA,C'B'                        UNITS AND PACKAGES            
         MVI   NBRESUME,NBPROCPK                  RESTART PKG READ              
         MVI   NBESTOPT,C'A'                      ESTIMATED DEMOS               
         MVI   NBSELUOP,C'A'                      ACTUAL SCHEDULE               
         CLI   DOHUNS,C'Y'                                                      
         BNE   *+8                                                              
         MVI   NBHUNOPT,C'Y'                                                    
         OI    NBINDS,X'40'                       SET INCREASE PREC             
         OI    NBINDS,X'20'                       SKIP ANY PKG/DEM GUAR         
         OI    NBINDS,X'80'                       EQUIVALENCE OVERRIDES         
RP08     LA    R1,UNTHOOK                         UNIT HOOK                     
         ST    R1,NBHOOK                                                        
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   RP10                                                             
*                                                                               
         CLI   DOPAKG,C'D'         IF DELETING DEMO GUAR                        
         BE    *+12                                                             
         CLI   DOPAKG,C'P'         IF DELETING PACKAGE GUAR                     
         BNE   RP11                                                             
         BAS   RE,SPOOLIT          PUT OUT NUMBERS CHANGED                      
         MVC   P(17),=C'PACKAGES CHANGED='                                      
         EDIT  (B4,PKGDELT),(5,P+18)                                            
         BAS   RE,SPOOLIT                                                       
         MVC   P(14),=C'UNITS CHANGED='                                         
         EDIT  (B4,UNTDELT),(5,P+18)                                            
         BAS   RE,SPOOLIT                                                       
         B     RP12                                                             
*                                                                               
RP11     BAS   RE,RP30             CALCULATES CPM                               
         BAS   RE,WRTREP           WRITES REPORT                                
*                                                                               
RP12     CLI   SPLTST,C'N'         DID WE WRITE TO FILE                         
         BNE   XIT                 NO                                           
         MVI   HALF,C'U'           YES/SET TO UNLOCK CLIENT                     
         BAS   RE,LOCKEM                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
UNTHOOK  NTR1                             * UNIT HOOK                           
*                                                                               
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         CLI   DOPAKG,C'P'         >>IF DELETING PKG GUARANTEE                  
         BE    DELETE05                                                         
         CLI   DOPAKG,C'D'         >>IF DELETING DEMO GUARANTEE                 
         BE    DELETE10                                                         
         BNE   UNTHOOK0              ELSE DO CPM PROCESSING                     
*                                                                               
DELETE05 CLI   NBMODE,NBPROCPK                                                  
         BNE   DELETE10                                                         
         L     R2,NBAIO           PACKAGE REC PROCESSING                        
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DCHO                                                                   
         USING NPAKEL,R2                                                        
         XC    NPAKGCPM,NPAKGCPM   CLEAR CPM FIELD                              
*                                                                               
         L     R1,PKGDELT          ADD TO COUNTER OF PKG RECS CHANGED           
         LA    R1,1(R1)                                                         
         ST    R1,PKGDELT                                                       
*                                                                               
         B     DELETE20                                                         
         DROP  R2                                                               
*                                                                               
DELETE10 CLI   NBMODE,NBPROCUN     DELETING PKG/DEMO GUARANTEE                  
         BNE   XIT                                                              
         CLI   DOPAKG,C'P'         DELETING PKG GUARANTEE ON UNIT               
         BNE   DELETE15                                                         
* B3 = PKG GUARANTEE FACTOR                                                     
         GOTO1 HELLO,DMCB,(C'D',=CL8'UNTFILE'),(X'B3',NBAIO),0                  
         B     DELETE17                                                         
*                                                                               
DELETE15 CLI   DOPAKG,C'D'         DELETING DEMO GUARANTEE ON UNIT              
         BE    *+6                                                              
         DCHO                                                                   
* B4 = DEMO GUARANTEE FACTOR                                                    
         GOTO1 HELLO,DMCB,(C'D',=CL8'UNTFILE'),(X'B4',NBAIO),0                  
*                                                                               
DELETE17 L     R1,UNTDELT          ADD TO COUNTER OF UNT RECS CHANGED           
         LA    R1,1(R1)                                                         
         ST    R1,UNTDELT                                                       
         B     DELETE20                                                         
                                                                                
*                                                                               
DELETE20 B     DELETE21            LEAVE THIS FOR TESTING/CHECKING              
         GOTO1 =V(PRNTBL),DMCB,=C'NBACT',NBAIO,C'DUMP',20,=C'1D'                
         BAS   RE,SPOOLIT                                                       
DELETE21 CLI   UPDATE,C'Y'                                                      
         BNE   XIT                                                              
         MVI   NBUPUNIT,C'Y'                                                    
         MVI   NBNOWRIT,C'Y'                                                    
         B     XIT                                                              
                                                                                
******************************************************************              
* NON DELETING OF PKG/DEMO GUARANTEE PROCESSING                                 
UNTHOOK0 MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
         CLI   NBMODE,NBPROCUN                                                  
         BE    UNT10                                                            
*                                        * PACKAGE HANDLING                     
         CLI   NBMODE,NBPROCPK                                                  
         BNE   UNTX                                                             
*                                                                               
         CLI   COSTYP,C'P'         .ARE WE USING PACKAGE COST                   
         BNE   PKG10                NO                                          
         ICM   R1,15,NBPAKCST      .YES                                         
         M     R0,=F'100'          .PKG COST IN DOLLARS/GET PENNIES             
         CVD   R1,DUB                                                           
         AP    PKGCOST,DUB                                                      
PKG10    CLI   DOPAKG,C'Y'          IF PKG GUARANTEE                            
         BNE   UNTX                                                             
         XC    SRTEST,SRTEST        PASS TO SORTER                              
         MVC   SRTPROG(7),=C'PACKAGE'                                           
         B     UNT30               PUT TO SORTER                                
         SPACE                                                                  
*                                                                               
UNT10    DS    0H                         * UNIT HANDLING                       
*                                                                               
         MVC   SRTEST,NBACTEST              SET SORTREC KEY                     
         MVC   SRTDPT,NBACTDP                                                   
         MVC   SRTPKG,NBPACK                                                    
         MVC   SRTPROG,NBACTPRG                                                 
         MVC   SRTDATE,NBACTDAT                                                 
         MVC   SRTDSUB,NBACTSUB                                                 
*                                                                               
         CLI   COSTYP,C'A'                    IF USING ACTUAL COST              
         BE    UNT11                                                            
         CLI   COSTYP,C'M'                    IF USING MAX COST                 
         BNE   UNT12                                                            
         L     R1,NBMAXCST                                                      
         B     *+8                                                              
UNT11    L     R1,NBACTUAL                    GET DOLLARS AND CENTS             
         CVD   R1,DUB                                                           
         AP    PKGCOST,DUB                                                      
         MVC   SRTACT,NBACTUAL                SET IN SORT REC                   
*****    GOTO1 =V(PRNTBL),DMCB,=C'NBACT',DUB,C'DUMP',8,=C'1D'                   
*                                                                               
UNT12    OC    PDEMVAL,PDEMVAL     DO WE HAVE SPECIFIC PKG DEMO                 
         BZ    UNT20               NO                                           
         LA    R1,XDDEMOS          YES/FIND MATCHING DEMO IN LIST               
         LA    RE,XDESTDEM                                                      
         ZIC   R0,XDNDEMOS                                                      
UNT14    CLC   2(1,R1),PDEMVAL+2                                                
         BNE   UNT15                                                            
         CLC   0(1,R1),PDEMVAL                                                  
         BE    UNT17                                                            
UNT15    LA    R1,3(R1)            BUMP DEMO CODE LIST                          
         LA    RE,8(RE)            BUMP DEMO VALUES LIST                        
         BCT   R0,UNT14                                                         
         MVI   ERRFLG,1            SET ERROR FLAG                               
         B     UNT30               NO CAN FIND                                  
UNT17    ICM   R1,15,4(RE)         GET DEMO IMPS                                
         MVC   SRTIMP1,4(RE)                                                    
         CVD   R1,DUB                                                           
         AP    PKGIMP,DUB                                                       
         B     UNT20B                                                           
UNT20    ICM   R1,15,XDESTDEM+4       GET DEMO IMPS FOR PACK GUARANTEE          
         MVC   SRTIMP1,XDESTDEM+4                                               
         CVD   R1,DUB                                                           
         AP    PKGIMP,DUB                                                       
                                                                                
*                                                                               
UNT20B   OC    TDEMVAL,TDEMVAL     ARE WE DOING DEMO GUARANTEE                  
         BZ    UNT30               NO                                           
         LA    R1,XDDEMOS          YES/FIND MATCHING DEMO IN LIST               
         LA    RE,XDESTDEM                                                      
         ZIC   R0,XDNDEMOS                                                      
UNT21    CLC   2(1,R1),TDEMVAL+2                                                
         BNE   UNT22                                                            
         CLC   0(1,R1),TDEMVAL                                                  
         BE    UNT23                                                            
UNT22    LA    R1,3(R1)            BUMP DEMO CODE LIST                          
         LA    RE,8(RE)            BUMP DEMO VALUES LIST                        
         BCT   R0,UNT21                                                         
         MVI   ERRFLG,1            SET ERROR FLAG                               
         B     UNT30               NO CAN FIND                                  
UNT23    ICM   R1,15,4(RE)         GET DEMO IMPS                                
         MVC   SRTIMP2,4(RE)                                                    
         CVD   R1,DUB                                                           
         AP    DEMIMP,DUB                                                       
         SPACE                                                                  
UNT30    DS    0H                                                               
         MVC   SRTDSKAD,NBKEY+21           SET DISK ADDRESS                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTD                                        
         XC    SORTD(SRTRECLE),SORTD       CLEAR SORT AREA                      
*                                                                               
UNTX     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* - CALCULATE PACKAGE / DEMO CPM                                                
*                                                                               
RP30     NTR1                                                                   
         CLI   ERRFLG,1            IF ERROR                                     
         BE    XIT                                                              
         CP    PKGCOST,=PL8'0'     ARE THERE ACTUAL/PKG DOLLARS                 
         BE    XIT                 NO/THAT'S ALL FOLKS                          
*                                                                               
         CLI   DOPAKG,C'Y'         DO PKG GUA                                   
         BNE   RP50                NO                                           
         CP    PKGIMP,=PL8'0'      YES/ARE THERE PKG IMPS                       
         BE    RP50                NO                                           
         LA    R1,PKGIMP           YES/PASS IMPS                                
         LA    R2,PKGCPM               AND CPM OUT AREA                         
         LA    R3,TPAKCPM              AND TARGET CPM                           
         BAS   RE,CALCUL                                                        
*        B     RP50                                                             
         CLI   SPLTST,C'*'                                                      
         BNE   RP50                                                             
         SPACE                                                                  
* - TESTING                                                                     
         EDIT  (P6,PKGIMP+2),(12,P+1)                                           
         LA    R3,PKGCOST+2                                                     
         EDIT  (P6,0(R3)),(12,P+20)                                             
         BAS   RE,SPOOLIT                                                       
*                                                                               
         SPACE                                                                  
RP50     CLI   DODEMG,C'Y'         DO DEMO GUA                                  
         BNE   RPX                 NO                                           
         CP    DEMIMP,=PL8'0'      YES/ARE THERE DEMO IMPS                      
         BE    RPX                 NO                                           
         LA    R1,DEMIMP           YES/PASS IMPS                                
         LA    R2,DEMCPM               AND CPM OUT AREA                         
         LA    R3,TDEMCPM              AND TARGET CPM                           
         BAS   RE,CALCUL                                                        
RPX      B     XIT                                                              
         SPACE 2                                                                
* - CALCULATES CPM                                                              
* IN: R1 POINTS TO PL8 IMPS, R2 POINTS TO 4CL OUT CPM AREA                      
*                                                                               
CALCUL   NTR1                                                                   
         ZAP   PACKWRK,0(8,R1)     IMPS COME IN 2 DEC TOO LARGE/ROUND           
         AP    PACKWRK,=P'50'                                                   
         DP    PACKWRK,=PL8'100'                                                
         ZAP   0(8,R1),PACKWRK(8)  SAVE  BACK TO IMPS AREA                      
         SPACE                                                                  
         ZAP   PACKWRK,PKGCOST     GET PACKAGE COST (PENNIES)                   
         MP    PACKWRK,=P'100'     X 100 FOR 4 DECIMAL PLACES                   
         DP    PACKWRK,0(8,R1)     COST/IMPS=AGENCY CPM                         
         ZAP   WORK(8),PACKWRK(8)  (AND SET AGY CPM IN PACKWRK)                 
         ZAP   PACKWRK,WORK(8)                                                  
         SPACE                                                                  
         CLI   DOHUNS,C'Y'        IF HUNOPT                                     
         BNE   *+10                                                             
         MP    PACKWRK,=P'10'     GIVE AN EXTRA DECIMAL                         
         TM    NBINDS3,NBI3A2DC                                                 
         BNO   NOT2DC                                                           
         CLI   DOHUNS,C'Y'         IF HUNOPT MEANS CABLE PRECISION              
         BNE   CLNTWRK                                                          
         CLI   NBPOSTYP,C'C'                                                    
         BNE   CALCUL10                                                         
         MP    PACKWRK,=P'1000000'     CABLE                                    
         B     OK2DC                                                            
CALCUL10 MP    PACKWRK,=P'100000'                                               
         B     OK2DC                                                            
CLNTWRK  MP    PACKWRK,=P'10000000'    NETWORK                                  
         B     OK2DC                                                            
NOT2DC   MP    PACKWRK,=P'1000000'     AGY CPM/TARG CPM=FACTOR(4DEC)            
OK2DC    DS    0H                                                               
         ICM   R1,15,0(R3)             GET PKG TARGET CPM(4 DECIMALS)           
         CVD   R1,DUB                                                           
         DP    PACKWRK,DUB             AGY CPM / TARG CPM                       
         ZAP   DUB,PACKWRK(8)                                                   
         CVB   R1,DUB                                                           
         STCM  R1,15,0(R2)                                                      
CLX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE WRITES REPORT / UPDATES UNITS                                    
         SPACE                                                                  
WRTREP   NTR1                                                                   
         CP    PKGCOST,=PL8'0'     ARE THERE ACTUAL/PKG DOLLARS                 
         BE    COSTERR             NO/SET NOTICE AND EXIT                       
         SPACE                                                                  
WR10     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    WR100                                                            
         SPACE                                                                  
         LA    R2,P                                                             
         USING PLINE,R2                                                         
         MVC   SORTD(SRTRECLE),0(R3)        PUT TO SORT WORK AREA               
*        B     WR10A                                                            
         CLI   SPLTST,C'*'                                                      
         BNE   WR10A                                                            
         SPACE                                                                  
* - TESTING                                                                     
         EDIT  (B1,SRTEST),(3,P+1)                                              
         EDIT  (B1,SRTPKG),(3,P+5)                                              
         MVC   P+10(1),SRTDPT                                                   
         MVC   P+12(6),SRTPROG                                                  
         GOTO1 DATCON,DMCB,(2,SRTDATE),(5,P+20)                                 
         EDIT  (B4,SRTIMP1),(9,P+30)                                            
         EDIT  (B4,SRTIMP2),(9,P+40)                                            
         EDIT  (B4,SRTACT),(9,P+50)                                             
         BAS   RE,SPOOLIT                                                       
*                                                                               
WR10A    CLC   =C'PACKAGE',SRTPROG          IS IT PKG REC DATA                  
         BE    WR80                         YES/GO PKG PROCESSING               
         CLC   SRTEST,ESTSV                 NO                                  
         BE    WR12                                                             
         EDIT  (B1,SRTEST),(3,PLEST+1)                                          
         MVI   DPTSV,0                                                          
         MVI   PKGSV,0                                                          
         MVC   ESTSV,SRTEST                                                     
WR12     CLC   SRTPKG,PKGSV                                                     
         BE    WR14                                                             
         EDIT  (B1,SRTPKG),(3,PLPKG+1)                                          
         MVC   PKGSV,SRTPKG                                                     
         MVI   DPTSV,0                                                          
WR14     CLC   SRTDPT,DPTSV                                                     
         BE    WR50                                                             
         MVC   PLDPT+1(1),SRTDPT                                                
         MVC   DPTSV,SRTDPT                                                     
         BAS   RE,SPOOLIT                                                       
*                                                                               
WR50     CLI   UPDATE,C'Y'         UPDATE UNITS                                 
         BNE   WR10                                                             
         CLI   ERRFLG,1            ERROR CONDITION                              
         BE    WR10                NO UPDATE                                    
         LA    R3,SRTDSKAD                                                      
         L     R2,NBAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',(R3),(R2),MYDMWRK           
         CLI   DOPAKG,C'Y'                                                      
         BNE   WR60                                                             
* - DELETE PKG GUARANTEE                                                        
         GOTO1 HELLO,DMCB,(C'D',=CL8'UNTFILE'),(X'B3',NBAIO),0                  
         XC    MYWORK(20),MYWORK                                                
         MVI   MYWORK,X'B3'                                                     
         MVI   MYWORK+1,8                                                       
         MVC   MYWORK+2(4),PKGCPM       PKG GUARANTEE FACTOR                    
* - ADD PACKAGE GUARANTEE                                                       
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),NBAIO,MYWORK,0                   
*                                                                               
WR60     CLI   DODEMG,C'Y'         DEMO GUARANTEE                               
         BNE   WR70                                                             
         MVI   ELCODE,X'B4'                                                     
         GOTO1 REMELEM             REMOVE DEMO GUARANTEE                        
         XC    MYWORK(20),MYWORK   SET UP NEW DEMO GUAR FACTOR                  
         MVI   MYWORK,X'B4'                                                     
         MVI   MYWORK+1,11                                                      
         MVC   MYWORK+2(3),TDEMVAL                                              
         MVC   MYWORK+5(4),DEMCPM                                               
* - ADD DEMO GUARANTEE                                                          
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),NBAIO,MYWORK,0                   
*                                                                               
WR70     L     R2,NBAIO                WRITE RECORD BACK                        
         LA    R3,SRTDSKAD                                                      
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'UNTFILE ',(R3),(R2),MYDMWRK           
         B     WR10                                                             
*                                                                               
WR80     DS    0H                  HANDLE PACKAGE REC HERE                      
         CLI   UPDATE,C'Y'         FOR UPDAT EONLY                              
         BNE   WR10                                                             
         L     R2,NBAIO                                                         
         LA    R3,SRTDSKAD                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',(R3),(R2),MYDMWRK           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPAKEL,R2                                                        
         MVC   NPAKGCPM,PKPKGCPM       SET PKG GUAR                             
*                                                                               
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),X'02'         DO WE HAVE ANX'02'?                          
         BE    WR95                                                             
***      DC    H'0'                USED TO DUMP/ADD ELEM INSTEAD                
         XC    ELEM,ELEM           NO-ADD IT                                    
         LA    R1,ELEM                                                          
         USING NPK2D,R1                                                         
         MVI   NPK2EL,X'02'                                                     
***      MVC   NPK2LEN,NPK2ELN                                                  
         MVI   NPK2LEN,NPK2ELN                                                  
         MVC   NPK2PDEM,PDEMVAL    SET PKG GUAR DEMO                            
         L     R2,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),(R2),ELEM,0                      
         DROP  R1                                                               
         B     WR95B                                                            
         USING NPK2D,R2                                                         
WR95     MVC   NPK2PDEM,PDEMVAL    SET PKG GUAR DEMO                            
*                                                                               
WR95B    MVC   NPAKGDEM,NPK2PDEM   SET GUAR DEMO                                
         L     R2,NBAIO                WRITE RECORD BACK                        
         LA    R3,SRTDSKAD                                                      
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'UNTFILE ',(R3),(R2),MYDMWRK           
         B     WR10                                                             
         EJECT                                                                  
*                                                                               
WR100    DS    0H                                                               
         BAS   RE,SPOOLIT          SKIP LINE                                    
         CLI   ERRFLG,1                                                         
         BE    DEMERR                                                           
         LA    R2,P                                                             
         USING PLINE,R2                                                         
         MVC   PLEST(14),=C'*** TOTALS ***'                                     
         BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         OC    PKGCPM,PKGCPM                                                    
         BZ    WR135                                                            
         MVC   PLEST(11),=C'PKG FACTOR='                                        
         EDIT  (B4,PKGCPM),(8,MYWORK),ALIGN=LEFT                                
         BAS   RE,DECIT                                                         
         LA    R2,132(R2)                                                       
WR135    OC    DEMCPM,DEMCPM                                                    
         BZ    WR140                                                            
         MVC   PLEST(12),=C'DEMO FACTOR='                                       
         EDIT  (B4,DEMCPM),(8,MYWORK),ALIGN=LEFT                                
         BAS   RE,DECIT                                                         
WR140    BAS   RE,SPOOLIT                                                       
*                                                                               
WRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
DECIT    NTR1                                                                   
* - 4 DECIMAL PLACES                                                            
         LA    R3,PLEST+12        PRINT LINE                                    
         LA    R2,MYWORK          EDITED OUTPUT                                 
         C     R0,=F'4'           R0=SIGNIFICANT CHARS                          
         BNL   DEC10                                                            
         MVI   0(R3),C'.'         LASS THEN 4 - ADD LEADING 0'S                 
         LA    R3,1(R3)            BUMP OVER DECIMAL                            
         LA    R1,4                                                             
         SR    R1,R0          4-SIGNIFICANT CHARACTERS=LEN FOR MVC              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=C'0000'                                                 
         LA    R1,1(R1)            RESTORE BCTR OF MVC                          
         AR    R3,R1               BUMP OVER 0'S                                
         MVC   0(3,R3),MYWORK      SET EDITED NUMBER                            
         B     DECITX                                                           
*                                                                               
DEC10    C     R0,=F'4'                                                         
         BE    DEC15                                                            
         MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         BCTR  R0,0                                                             
         B     DEC10                                                            
DEC15    MVI   0(R3),C'.'          SET IN DECIMAL                               
         MVC   1(4,R3),0(R2)       AND MOVE 4 DEC OF DATA                       
DECITX   XIT1                                                                   
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE                                                                  
COSTERR  DS    0H                  NO PACKAGE COST                              
         MVC   P+10(24),=C'*** NO DOLLARS FOUND ***'                            
         BAS   RE,SPOOLIT                                                       
         B     XIT                                                              
         SPACE                                                                  
DEMERR   DS    0H                  NO DEMO MATCH                                
         LA    R2,P                                                             
         USING PLINE,R2                                                         
         MVC   PLEST(31),=C'*** NO DEMO GUARANTEE MATCH ***'                    
         BAS   RE,SPOOLIT                                                       
         B     XIT                                                              
         SPACE                                                                  
* RETURNS 3 BYTE PROD CODE IN WORK/ EXPECTS 1 BYTE PRD IN BYTE                  
GETPRD   NTR1                                                                   
         CLI   BYTE,0              IS IT UNA                                    
         BE    GP15                                                             
         L     R2,NBACLI                                                        
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         LA    R1,220                                                           
GP10     CLC   BYTE,3(R2)                                                       
         BE    GP20                                                             
         CLI   3(R2),0                                                          
         BE    GP15                                                             
         LA    R2,4(R2)                                                         
         BCT   R1,GP10                                                          
GP15     MVC   WORK(3),=C'UNA'                                                  
         B     *+10                                                             
GP20     MVC   WORK(3),0(R2)                                                    
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEADSPC  SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,CLIENT                                                      
         SSPEC H1,46,C'NETWORK CPM REPORT'                                      
         SSPEC H2,46,C'__________________'                                      
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,46,PERIOD                                                     
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         EJECT                                                                  
* MY DATAMGR INTERFACE FOR SPOT FILE                                            
*                                                                               
RDHI     NTR1                                                                   
         MVC   COMAND,=CL8'DMRDHI'                                              
         LA    R2,MYKEY                                                         
         MVC   MYKEYSV,MYKEY                                                    
         MVC   FILE,=C'SPTDIR  '                                                
         B     DIRALL                                                           
RDSEQ    NTR1                                                                   
         LA    R2,MYKEY                                                         
         MVC   MYKEYSV,MYKEY                                                    
         MVC   COMAND,=CL8'DMRSEQ'                                              
         MVC   FILE,=C'SPTDIR  '                                                
         B     DIRALL                                                           
*                                                                               
DIRALL   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMAND,FILE,MYKEY,MYKEY,0                           
         B     XIT                                                              
*                                                                               
GTREC    NTR1                                                                   
         LA    R2,MYKEY+14                                                      
         L     R3,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',(R2),(R3),MYDMWRK           
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
MYHEAD   NTR1                                                                   
         CLI   DOPAKG,C'D'         IF DELETING PKG/DEMO GUAR                    
         BE    XIT                 SKIP HEAD/BOXES                              
         CLI   DOPAKG,C'P'                                                      
         BE    XIT                                                              
         MVC   H3(7),=C'NETWORK'                                                
         MVC   H3+10(4),SPLNET                                                  
         MVC   H3+20(20),SPLNETN                                                
         CLI   UPDATE,C'Y'         MARK FILE                                    
         BE    MYH10                                                            
         MVC   H5+49(16),=C'*** TEST RUN ***'                                   
MYH10    DS    0H                                                               
         LA    R2,H10                                                           
         USING PLINE,R2                                                         
         MVC   PLEST+1(3),=C'EST'                                               
         MVC   PLPKG+1(3),=C'PKG'                                               
         MVC   PLDPT+1(3),=C'DPT'                                               
*                                                                               
MYH20    L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    XIT                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         DROP  R2                                                               
         LA    R2,BOXCOLS                                                       
         USING PLINE,R2                                                         
         MVI   PL1,C'L'                                                         
         MVI   PLDPT-1,C'C'                                                     
         MVI   PLPKG-1,C'C'                                                     
         MVI   PLEND,C'R'                                                       
         SPACE                                                                  
         LA    R1,BOXROWS                                                       
         LA    R1,8(R1)                                                         
         MVI   0(R1),C'T'                                                       
         LA    R1,2(R1)                                                         
         MVI   0(R1),C'M'                                                       
         LA    R1,45(R1)                                                        
         MVI   0(R1),C'B'                                                       
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
TRAPIT   NTR1                                                                   
         L     R1,DATVAL                                                        
         CLC   =C'XXDTVLXX',22(R1)                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   X'2F4'(R1),X'58'                                                 
         BE    TRAPX                                                            
*        LR    RF,R1                                                            
*        L     R1,=F'1000'                                                      
*        L     RE,=A(TRAPBUFF)                                                  
*        A     RE,RELO                                                          
*        MOVE  ((RF),(R1)),(RE)                                                 
         DC    H'0'                                                             
TRAPX    B     XIT                                                              
*                                                                               
TRAPIT2  NTR1                                                                   
         L     R1,DATVAL                                                        
         CLC   =C'XXDTVLXX',22(R1)                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   X'2F4'(R1),X'58'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R1                                                            
         L     R1,=F'1000'                                                      
         L     RF,=A(TRAPBUFF)                                                  
         A     RF,RELO                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
TRAPX2   B     XIT                                                              
*                                                                               
PKGDELT  DS    F                   PKG RECORDS CHANGED                          
UNTDELT  DS    F                   UNITRECORDS CHANGED                          
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
TRAPBUFF DS    CL1000                                                           
MYNEBUFF DS    CL6000                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
       ++INCLUDE NETDEMON                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*                                                                               
RELO     DS    F                                                                
AMYIO    DS    A                                                                
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
UPDATE   DS    CL1                                                              
COSTYP   DS    CL1                                                              
DOPAKG   DS    CL1                 PKG GUA FLAG                                 
DODEMG   DS    CL1                 DEMO GUA FLAG                                
TPAKCPM  DS    XL4                 TARGET PACKAGE CPM VALUE 4 DEC               
TDEMCPM  DS    XL4                 TARGET DEMO CPM VALUE                        
TDEMVAL  DS    CL3                 3 BYTE TARGET DEMO CODE                      
PDEMVAL  DS    CL3                 3 BYTE PACKAGE DEMO CODE                     
MYWORK   DS    CL400                                                            
ERRFLG   DS    CL1                                                              
DOHUNS   DS    CL1                                                              
PKPKGCPM DS    XL4                 2 DECIMAL PKG CPM TO PASS TO PKGREC          
*                                                                               
*                                                                               
MYKEY    DS    CL40                                                             
MYKEYSV  DS    CL40                                                             
PACKWRK  DS    PL16                PACKED WORK AREA                             
COMAND   DS    CL8                                                              
FILE     DS    CL8                                                              
*                                                                               
*                                                                               
         DS    0D                                                               
PKGCOST  DS    PL8                 PACKAGE COST                                 
PKGIMP   DS    PL8                 TOTAL PACKAGE IMPRESSIONS                    
DEMIMP   DS    PL8                 TOTAL DEMO GUARANTEE IMPRESSIONS             
PKGCPM   DS    XL4                 PACKAGE GUARANTEE FACTOR                     
DEMCPM   DS    XL4                 DEMO GUARANTEE FACTOR                        
ESTSV    DS    CL1                                                              
PKGSV    DS    CL1                                                              
DPTSV    DS    CL1                                                              
SCLEN    DS    CL1                 SAVE AREA FOR SCREEN FIELD LEN               
SCDATA   DS    CL40                SAVE AREA FOR SCREEN INPUT FILED             
*                                                                               
SORTD    DS    0H                  SORT WORK AREA                               
SRTEST   DS    CL1                 ESTIMATE                                     
SRTPKG   DS    CL1                 PACKAGE                                      
SRTDPT   DS    CL1                 DAYPART                                      
SRTPROG  DS    CL6                 PROGRAM                                      
SRTDATE  DS    CL2                 DATE                                         
SRTDSUB  DS    CL1                 SUB-LINE                                     
*  SRTKEY LEN = 12                                                              
SRTKEYLE EQU   *-SORTD                                                          
SRTIMP1  DS    CL4                 PKG TARGET DEMO IMPRESSION                   
SRTIMP2  DS    CL4                 DEMO IMPRESSION                              
SRTACT   DS    CL4                 ACTUAL COST                                  
SRTDSKAD DS    CL4                 DISKADDRESS                                  
SRTPKCST DS    CL4                 PACKAGE COST                                 
         DS    CL4                 SPARE                                        
SRTRECLE EQU   *-SORTD                                                          
* RECCARD SET UP FOR SORT REC = 100                                             
         SPACE                                                                  
*                                                                               
MYWORKLE EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
PLINE    DSECT              PRINT LINE DSECT                                    
         DS    CL45                                                             
PL1      DS    CL1                                                              
         DS    CL1                                                              
PLEST    DS    CL5                 ESTIMATE                                     
         DS    CL2                                                              
PLPKG    DS    CL5                 PACKAGE                                      
         DS    CL2                                                              
PLDPT    DS    CL5                 DAYPART                                      
         DS    CL2                                                              
PLEND    DS    CL1                                                              
         SPACE                                                                  
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NETINCLN                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDDD                                                       
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEWRI64   03/14/18'                                      
         END                                                                    
