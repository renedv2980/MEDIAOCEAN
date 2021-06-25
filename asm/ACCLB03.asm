*          DATA SET ACCLB03    AT LEVEL 037 AS OF 08/16/00                      
*PHASE T62103A                                                                  
CLB03    TITLE '- BILL PROGRAM - ZOOMS'                                         
CLB03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB3**,R8,R7,R6                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING ZWORKD,RC           RC=LOCAL W/S                                 
Z        USING PTAELD,ZPTAEL                                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
S        USING PRORATAD,LSPRATAS                                                
*                                                                               
         LA    R2,ZWORKD                                                        
         LA    R3,ZWORKL                                                        
         XR    R1,R1                                                            
         MVCL  R2,R0               CLEAR W/S                                    
         STCM  RF,8,ZNTRSES                                                     
         CLC   CSCPYCUR,CSBILCUR   TEST BILLING IN AGENCY CURRENCY              
         BNE   *+8                                                              
         OI    ZINDS,ZIAGYCUR                                                   
         CLI   CSACT,ACTZWR        TEST RECOVERY                                
         BE    RECOV                                                            
*                                                                               
         CLI   ZNTRSES,0           TEST NTRSES RETURN                           
         BNE   INIT02                                                           
         TM    BCINDS2,BCINTRS     TEST FIRST TIME                              
         BZ    INIT10                                                           
         BAS   RE,SAVFLD           SAVE ANY UNVALIDATED FIELDS                  
         GOTO1 AOVRSCR,BOPARM,('S#BILZOO',BASOLAYH)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(CSSCRN,REGOLAYH)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT02   CLI   ZISLST,0                                                         
         BNE   *+8                                                              
         OI    ZINDS,ZIDIS                                                      
         BAS   RE,READ                                                          
         BAS   RE,PROSCR                                                        
         BAS   RE,DISSCR                                                        
         CLI   ZISLST,0                                                         
         BE    INIT04                                                           
         BAS   RE,RESFLD                                                        
         B     INIT30                                                           
*                                                                               
INIT04   LA    RF,TWAD                                                          
         AH    RF,SINP1                                                         
         ST    RF,FVADDR                                                        
         TM    TLXSTAT,TLXSRVAL                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IMREF)                                           
         B     INITX                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         CLI   ZNTRSES,0                                                        
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         B     INITX                                                            
*                                                                               
INIT10   LA    R1,REGOLAYH         TEST FOR ANY INPUT TO SCREEN                 
         USING FHD,R1                                                           
         XR    RF,RF                                                            
INIT12   ICM   RF,1,FHLN                                                        
         BZ    INIT16                                                           
         TM    FHAT,FHATPR                                                      
         BO    *+12                                                             
         TM    FHII,FHIIVA         TEST FOR UNVALIDATED FIELD                   
         BZ    INIT20                                                           
         BXH   R1,RF,INIT12                                                     
         DROP  R1                                                               
*                                                                               
INIT16   OI    BCINDS2,BCIACTCP    ACTION HAS FINISHED                          
         B     EXIT                                                             
*                                                                               
INIT20   BAS   RE,READ                                                          
*                                                                               
INIT30   BAS   RE,VALSCR                                                        
         BNE   INITX                                                            
         BAS   RE,WRITE                                                         
         BL    INITX                                                            
*                                                                               
         LA    R1,REGOLAYH         SET VALID ALL FIELDS                         
         XR    RF,RF                                                            
         ICM   RF,1,FHLND(R1)                                                   
         BZ    *+12                                                             
         OI    FHIID(R1),FHIIVA                                                 
         BXH   R1,RF,*-12                                                       
*                                                                               
         TM    TLXSTAT,TLXSRVAL                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IMREF)                                           
         B     INITX                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RCENX)                                           
         CLI   ZUPDINDS,0                                                       
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         LA    R1,TWAD                                                          
         AH    R1,SINP1                                                         
         ST    R1,FVADDR                                                        
*                                                                               
INITX    BAS   RE,DISAVL                                                        
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE-OFF RECOVERY CONTROLLER                                       *         
***********************************************************************         
         SPACE 1                                                                
RECOV    TM    BCINDS2,BCINTRS     TEST FIRST TIME                              
         BO    *+12                                                             
         CLI   ZNTRSES,0           OR NTRSES RETURN                             
         BE    RECOV02                                                          
         GOTO1 AOVRSCR,BOPARM,('S#BILZOO',BASOLAYH)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(CSSCRN,REGOLAYH)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVCDD WOFSEQW,AC#WRTFN                                                 
         OI    WOFSEQWH+FHATD,FHATHI                                            
         OI    WOFSEQH+FHATD,FHATHI                                             
         OI    ZINDS,ZIDIS                                                      
         BAS   RE,READ                                                          
         BAS   RE,PROSCR                                                        
         BAS   RE,DISSCR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NPEWO)                                           
         LA    RF,WOFSEQH                                                       
         TM    ZINDS,ZIWORPEN                                                   
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AI$WOPEC)                                           
         LA    RF,WOFNAR1H                                                      
         ST    RF,FVADDR                                                        
         B     RECOVX                                                           
*                                                                               
RECOV02  CLI   BCPFKEY,PFK01       TEST FOR CLEAR PFKEY                         
         BNE   RECOV10                                                          
         BAS   RE,READ                                                          
         TM    ZINDS,ZIWORPEN      TEST ANYTHING TO CLEAR                       
         BO    RECOV04                                                          
         LA    RF,WOFSEQH                                                       
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NPEWO)                                           
         B     RECOVX                                                           
*                                                                               
RECOV04  GOTO1 FINDPTA,=AL1(PTATWOFR)                                           
         L     RF,ZAPTAEL          DELETE RECOVERY ELEMENT                      
         MVI   0(RF),FF                                                         
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',ATRNREC),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0               UPDATE PRORATA BLOCK                         
         TM    ZINDS,ZIAGYCUR                                                   
         BO    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BOPARM,ATRNREC,AGOPBLK,ACOM,(R0),LSPRATA,0              
         NI    ZINDS,FF-ZIWORPEN                                                
         CP    PM$ANVBL,BCPZERO    TEST AMOUNT STILL AVAILABLE                  
         BNL   RECOV06                                                          
         LA    RF,WOFNAR1H                                                      
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$CCRAA)                                           
         B     RECOVX                                                           
*                                                                               
RECOV06  MVI   ZUPDINDS,ZUPDITRN+ZUPDIJOB                                       
         CP    PP$AWOFF,BCPZERO    RECOVERY CLEARED BUT THERE MAY               
         BNE   *+12                STILL BE A WRITE-OFF PENDING                 
         L     RE,ATRNREC          ELSE CLEAR DIRECTORY STATUS                  
         NI    TRNRSTA2-TRNRECD(RE),FF-TRNSWOFP                                 
         BAS   RE,WRITE                                                         
         BAS   RE,DISSCR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$PWORC)                                           
         LA    RF,WOFSEQH                                                       
         ST    RF,FVADDR                                                        
         B     RECOVX                                                           
*                                                                               
RECOV10  LA    R1,REGOLAYH         TEST FOR ANY INPUT TO SCREEN                 
         USING FHD,R1                                                           
         XR    RF,RF                                                            
RECOV12  ICM   RF,1,FHLN                                                        
         BZ    RECOV14                                                          
         TM    FHAT,FHATPR                                                      
         BO    *+12                                                             
         TM    FHII,FHIIVA         TEST FOR UNVALIDATED FIELD                   
         BZ    RECOV20                                                          
         BXH   R1,RF,RECOV12                                                    
         DROP  R1                                                               
RECOV14  OI    BCINDS2,BCIACTCP    ACTION HAS FINISHED                          
         B     EXIT                                                             
*                                                                               
RECOV20  BAS   RE,READ                                                          
         BAS   RE,VALSCR                                                        
         BNE   RECOVX                                                           
         OI    ZINDS,ZIWORPEN                                                   
         L     RE,ATRNREC          SET DIRECTORY STATUS W/O PENDING             
         OI    TRNRSTA2-TRNRECD(RE),TRNSWOFP                                    
         OI    ZUPDINDS,ZUPDIJOB   ENSURE JOB SCIEL UPDATED                     
         BAS   RE,WRITE                                                         
         BL    RECOVX                                                           
*                                                                               
         LA    R1,REGOLAYH         SET VALID ALL FIELDS                         
         XR    RF,RF                                                            
         ICM   RF,1,FHLND(R1)                                                   
         BZ    *+12                                                             
         OI    FHIID(R1),FHIIVA                                                 
         BXH   R1,RF,*-12                                                       
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$WORDC)                                           
         CP    S.PP$AWOFR,BCPZERO                                               
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$WOPEC)                                           
         LA    RF,WOFNAR1H                                                      
         ST    RF,FVADDR                                                        
*                                                                               
RECOVX   BAS   RE,DISAVL                                                        
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
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
         EJECT                                                                  
***********************************************************************         
* READ RECORDS AND INITIALIZE                                         *         
***********************************************************************         
         SPACE 1                                                                
READ     NTR1  ,                                                                
*                                                                               
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         MVCDD BOWORK2(3),AC#TODAY                                              
         GOTO1 VDICTAT,BOPARM,C'SU  ',BOWORK2,0                                 
         LA    RF,POSTWOFC         SET LOCK FOR COST/TIME WRITE-OFFS            
         TM    TLXSTAT,TLXSHOUR                                                 
         BZ    *+8                                                              
         LA    RF,POSTWOFT                                                      
         GOTO1 VBMONVAL,(R1),(3,BOWORK2),((RF),ACOM),(CULANG,BOWORK1), *        
               (CUABIN,0)                                                       
         MVC   CSBSECL,0(R1)                                                    
*                                                                               
         MVC   IODAOVER,TLDA                                                    
         LH    R1,=Y(IOTRN)                                                     
         TM    ZINDS,ZIDIS                                                      
         BO    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO,IOGET+IOACCMST(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,ATRNREC          SAVE INITIAL STATUS AREA                     
         MVC   ZTRNRSTA,TRNRSTA-TRNRECD(RF)                                     
*                                                                               
         L     RE,AGOPBLK          FOR BILLING EXTENSION                        
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         GOTO1 AGETOPT,BOPARM,ATRNREC                                           
         XR    R0,R0                                                            
         TM    ZINDS,ZIAGYCUR      TEST AGENCY CURRENCY                         
         BO    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BOPARM,ATRNREC,AGOPBLK,ACOM,(R0),LSPRATA,0              
         LA    RE,LSPRATAS                                                      
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE ORIGINAL PROROTA BLOCK                  
         CP    PP$AWOFR,BCPZERO    TEST WRITE-OFF RECOVERY PENDING              
         BE    *+8                                                              
         OI    ZINDS,ZIWORPEN                                                   
*                                                                               
         MVC   ZBILCUR,CSBILCUR    COPY BILLING CURRENCY CODE                   
         MVC   ZCURBIL,CSCURBIL    COPY BILLING CURRENCY DATA                   
         CLC   PG$BLCUR,BCSPACES                                                
         BNH   READ02                                                           
         CLC   PG$BLCUR,ZBILCUR                                                 
         BE    READ02                                                           
         CP    PA$NETBL,BCPZERO    IF PRIOR BILLING COMES TO ZERO OK            
         BNE   *+14                                                             
         CP    PA$COMBL,BCPZERO                                                 
         BE    READ02                                                           
         MVC   ZBILCUR,PG$BLCUR                                                 
         GOTO1 AGETCUR,BOPARM,(X'80',ZBILCUR),ZCURBIL                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
READ02   CLI   CSACT,ACTZOO                                                     
         BNE   READ04                                                           
         CP    PP$AWOFF,BCPZERO                                                 
         BE    READX                                                            
*                                                                               
READ04   XR    RF,RF                                                            
         CLI   CSACT,ACTZWR                                                     
         BNE   *+8                                                              
         LA    RF,PTATWOFR                                                      
         XR    R0,R0                                                            
         TM    ZINDS,ZIDIS                                                      
         BZ    *+8                                                              
         LA    R0,1                                                             
         GOTO1 AGETPTA,BOPARM,((R0),ATRNREC),APTAREC,LSPRATA,((RF),0)           
         BNH   READ06                                                           
         CLI   CSACT,ACTZWO                                                     
         BNE   READ06                                                           
         NI    WOFEXPCH+FHIID,FF-FHIIVA                                         
*                                                                               
READ06   OI    ZINDS,ZIGOTPTA                                                   
         MVC   SDPTCODE,BOWORK1                                                 
         MVC   SSTFCODE,BOWORK1+12                                              
         MVC   SANALOFF,BOWORK1+24                                              
*                                                                               
READX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE BACK RECORDS                                                  *         
*                                                                     *         
* EXIT: CC=LOW IF ERROR WAS FOUND                                     *         
*       CC=HIGH IF NOTHING WAS WRITTEN                                *         
***********************************************************************         
         SPACE 1                                                                
WRITE    NTR1  ,                                                                
         TM    ZUPDINDS,ZUPDITRN+ZUPDIJOB+ZUPDIPTA                              
         BZ    EXITH                                                            
*                                                                               
         TM    ZUPDINDS,ZUPDIPTA   TEST UPDATE PTA RECORD/WRITE OFFS            
         BZ    WRITE20                                                          
         CLI   CSACT,ACTZWR        TEST FOR WRITE-OFF RECOVERY                  
         BNE   WRITE10                                                          
         L     RF,APTAREC                                                       
         MVC   IOKEY,0(RF)                                                      
         GOTO1 AIO,IORDUP+IOACCMST+IO1                                          
         MVC   IOADDR,APTAREC                                                   
         GOTO1 AIO,IOWRITE+IOACCMST                                             
         BE    WRITE20                                                          
         DC    H'0'                                                             
*                                                                               
WRITE10  TM    ZUPDINDS,ZUPDIWOP   TEST CHANGE TO WRITE-OFF POSTINGS            
         BZ    WRITE12                                                          
         GOTO1 AWOPPTA,BOPARM,ATRNREC,APTAREC,SDPTCODE,SSTFCODE                 
         BNE   WRITE14                                                          
WRITE12  GOTO1 APUTPTA,BOPARM,(C'T',ATRNREC),APTAREC                            
         BL    WRITE14                                                          
         OI    ZUPDINDS,ZUPDITRN                                                
         B     WRITE20                                                          
*                                                                               
WRITE14  LA    RE,REGWOACH         POSITION CURSOR FOR ERROR                    
         CLI   CSACT,ACTZOO        TEST ZOOM SCREEN                             
         BNE   WRITE15                                                          
         CLC   FVMSGNO,=AL2(AE$ISTFQ)                                           
         BE    *+14                                                             
         CLC   FVMSGNO,=AL2(AE$IDPTQ)                                           
         BNE   WRITE18             IF DEPT/STAFF REQUIRED                       
         LH    RF,=Y(LC@PF7ZW-TWAD)  REQUEST USER PF7S TO ZOOMWO                
         LA    RF,TWAD(RF)                                                      
         MVC   FVXTRA(L'LC@PF7ZW),0(RF)                                         
         B     WRITE18                                                          
WRITE15  LA    RE,WOFEXPCH                                                      
         CLI   0(R1),0                                                          
         BE    WRITE18                                                          
         CLI   0(R1),SPATW2DA                                                   
         BE    WRITE16                                                          
         CLI   0(R1),SPATW1PA                                                   
         BNE   *+16                                                             
         TM    SEXBSTAT,ACBSDEPT                                                
         BZ    *+8                                                              
WRITE16  LA    RE,WOFDPTCH                                                      
         CLI   0(R1),SPATW2PA                                                   
         BNE   *+8                                                              
         LA    RE,WOFSTFCH                                                      
WRITE18  ST    RE,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
WRITE20  TM    ZUPDINDS,ZUPDITRN   TEST UPDATE TRANSACTION RECORD               
         BZ    WRITE40                                                          
*                                                                               
         TM    ZUPDINDS,ZUPDIJOB   TEST UPDATE JOB RECORD                       
         BZ    WRITE30                                                          
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',ATRNREC),AIO3,LSPRATAS,LSPRATA              
         BE    WRITE24                                                          
         LA    RF,REGALNTH         TEST ERROR ON REGULAR ALLOCATION             
         CLI   0(R1),SCITCBAP                                                   
         BE    WRITE22                                                          
         LA    RF,REGXFNTH         TEST ERROR ON TRANSFER                       
         CLI   0(R1),SCITCBTP                                                   
         BE    WRITE22                                                          
         LA    RF,WOFSEQH          TEST ERROR ON RECOVERY                       
         CLI   0(R1),SCITCBRP                                                   
         BE    WRITE22                                                          
         CLI   0(R1),SCITCBWP      TEST ERROR ON WRITE-OFFS                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,REGWONTH                                                      
         CLI   CSACT,ACTZWO        TEST WRITE-OFF SCREEN                        
         BNE   WRITE22                                                          
         LA    RF,WOFNETH                                                       
WRITE22  ST    RF,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
WRITE24  GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRITE30  TM    ZUPDINDS,ZUPDIPTA   TEST UPDATE PTA RECORD                       
         BZ    WRITE32                                                          
         GOTO1 APUTPTA,BOPARM,ATRNREC,APTAREC                                   
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRITE32  L     R3,ATRNREC          TEST FOR CHANGE IN STATUS AREA               
         CLC   ZTRNRSTA,TRNRSTA-TRNRECD(R3)                                     
         BE    WRITE38                                                          
         LA    R2,IOKEY            UPDATE DIRECTORY RECORD                      
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,0(R3)                                                     
         GOTO1 AIO,IOACCDIR+IOREAD+IOLOCK                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNKSTA,TRNRSTA-TRNRECD(R3)                                      
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING TRXELD,R3           UPDATE EXTRA STATUS ELEMENT                  
         XR    RF,RF                                                            
WRITE34  CLI   TRXEL,TRXELQ                                                     
         BE    WRITE36                                                          
         CLI   TRXEL,0                                                          
         BE    WRITE38                                                          
         IC    RF,TRXLN                                                         
         BXH   R3,RF,WRITE34                                                    
WRITE36  NI    TRXSTA2,FF-(TRXSXFRP+TRXSWOFP+TRXSBILP)                          
         MVI   BOBYTE1,TRXSXFRP+TRXSWOFP+TRXSBILP                               
         NC    BOBYTE1,TRNKSTA2                                                 
         OC    TRXSTA2,BOBYTE1     ENSURE PENDING CONSISTENCY                   
         DROP  R3,R2                                                            
*                                                                               
WRITE38  LH    R1,=Y(IOTRN)                                                     
         GOTO1 AIO,IOPUT+IOACCMST(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRITE40  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROTECT EXTRA INPUT FIELDS IF NECESSERY                  *         
***********************************************************************         
         SPACE 1                                                                
PROSCR   NTR1  ,                                                                
         XC    BOHALF1,BOHALF1                                                  
         TM    TLXPEND,TLXPREV     TEST REVERSAL PENDING                        
         BZ    *+10                                                             
         OC    BOHALF1,=AL2(PROTREVP)                                           
         CLI   CSACT,ACTZWR        TEST WRITE-OFF RECOVERY                      
         BNE   *+10                                                             
         OC    BOHALF1,=AL2(PROTREC)                                            
         CP    PA$HOURS,BCPZERO    TEST ANY HOURS TO ALLOCATE                   
         BNE   *+10                                                             
         OC    BOHALF1,=AL2(PROTNOHR)                                           
         L     RF,ATRNREC                                                       
         USING TRNRECD,RF                                                       
         USING TRNELD,TRNRFST                                                   
         CLC   TRNKWORK,ORDWC      TEST AN ORDER                                
         BNE   *+16                                                             
         OC    BOHALF1,=AL2(PROTORD)                                            
         OC    CSMASK,=AL2(CSMORD)                                              
         CLI   TRNTYPE,99          TEST EXTRA BILLING                           
         BNE   *+16                                                             
         OC    BOHALF1,=AL2(PROTEXT)                                            
         OC    CSMASK,=AL2(CSMEXT)                                              
*&&UK                                                                           
         CLI   P#UNAUTH,C'Y'       TEST PREVENT ALLOC. OF UNUATH ITEMS          
         BNE   PSCR01                                                           
         TM    TRNSTAT,TRNSAUTH                                                 
         BO    PSCR01                                                           
         OC    BOHALF1,=AL2(PROTUNA)                                            
PSCR01   DS    0H                                                               
*&&                                                                             
         DROP  RF                                                               
*        GOTO1 AFLDSEC,=AL1(FIELD NUMBER)                                       
*        BNL   *+10                TEST FCONTROL SECURITY                       
*        OC    BOHALF1,=AL2(PROTFCON)                                           
         TM    TLXSTAT,TLXSNOCM    TEST NON-COMMISSIONABLE                      
         BZ    *+10                                                             
         OC    BOHALF1,=AL2(PROTNOCM)                                           
         TM    TLXSTAT,TLXSHOUR    TEST ON TIME                                 
         BZ    *+10                                                             
         OC    BOHALF1,=AL2(PROTHOUR)                                           
         L     RF,AGOPBLK          TEST BILLING TYPE IS NON-CLIENT              
         CLI   GOBILTYP-GOBLOCK(RF),C'C'                                        
         BE    *+10                                                             
         OC    BOHALF1,=AL2(PROTNONC)                                           
         CLC   ZBILCUR,CSBILCUR    TEST ITEM IS AS BILLING CURRENCY             
         BE    *+10                                                             
         OC    BOHALF1,=AL2(PROTCUR)                                            
*                                                                               
         CLI   TLACT,ACTMT1        PROTECT FIELDS IF CAME FROM                  
         BE    *+12                  MATCH 1/2 LISTS                            
         CLI   TLACT,ACTMT2                                                     
         BNE   *+10                                                             
         OC    BOHALF1,=AL2(PROTWLST)                                           
*                                                                               
         TM    TLXSTAT,TLXSRVAL    PROTECT IF REVALUE REQUIRED                  
         BZ    *+10                                                             
         OC    BOHALF1,=AL2(PROTRVAL)                                           
*                                                                               
         LA    R2,PROTZOO                                                       
         CLI   CSACT,ACTZWO                                                     
         BNE   *+8                                                              
         LA    R2,PROTZWO                                                       
         CLI   CSACT,ACTZWR                                                     
         BNE   *+8                                                              
         LA    R2,PROTZWR                                                       
         USING PROTTABD,R2         R2=A(PROTECT TABLE)                          
*                                                                               
PSCR02   OC    PROTCOND,PROTCOND                                                
         BZ    EXIT                                                             
         MVC   BOHALF2,BOHALF1     MATCH ON CONDITIONS                          
         NC    BOHALF2,PROTCOND                                                 
         BZ    PSCR08                                                           
*                                                                               
         XR    R1,R1               PROTECT THOSE FIELDS                         
         ICM   R1,3,PROTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1                                                           
         XR    RF,RF                                                            
         ICM   RF,3,PROTLAST                                                    
         LA    RF,TWAD(RF)                                                      
         XR    RE,RE                                                            
PSCR04   TM    FHAT,FHATPR                                                      
         BO    PSCR06                                                           
         OI    FHAT,FHATPR         NEWLY PROTECTED FIELDS                       
         NI    FHAT,FF-FHATHI        SHOULD NOT BE HIGHLIGHTED                  
PSCR06   IC    RE,FHLN                                                          
         BXLE  R1,RE,PSCR04                                                     
         DROP  R1                                                               
*                                                                               
PSCR08   LA    R2,PROTTABL(R2)                                                  
         B     PSCR02                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE NET AVAILABE FOR PENDING                       *         
***********************************************************************         
         SPACE 1                                                                
SETAVL   NTR1  ,                                                                
*                                                                               
         CLI   CSACT,ACTZOO        TEST REGULAR ZOOM SCREEN                     
         BNE   SAVL02                                                           
         GOTO1 ADJAVL,BOPARM,PTATRAL,REGALNTH,REGALHRH                          
         GOTO1 (RF),(R1),PTATWOF,REGWONTH,REGWOHRH                              
         GOTO1 (RF),(R1),PTATTRFT,REGXFNTH,REGXFHRH                             
*                                                                               
         TM    REGALNTH+FHIID,FHIIVA                                            
         BZ    SETAVLX                                                          
         TM    REGALCMH+FHIID,FHIIVA                                            
         BZ    SETAVLX                                                          
         TM    REGALHRH+FHIID,FHIIVA                                            
         BZ    SETAVLX             ENSURE COMMISSION SHOWN IS CORRECT           
         GOTO1 DISAMT,(R1),REGALCM,PP$ACOMM,PP$FCOMM                            
         OI    REGALCMH+FHOID,FHOITR                                            
         B     SETAVLX                                                          
*                                                                               
SAVL02   CLI   CSACT,ACTZWO        TEST ZOOM WRITE-OFF SCREEN                   
         BNE   EXIT                                                             
         GOTO1 ADJAVL,BOPARM,PTATWOF,WOFNETH,WOFHRSH                            
         BNE   EXITN                                                            
         B     SETAVLX                                                          
*                                                                               
SETAVLX  MVC   PG$STAT,S.PG$STAT                                                
         B     EXIT                                                             
         SPACE 1                                                                
ADJAVL   NTR1  ,                   ** ADJUST AVAILABLE IF ANY INPUT **          
         LM    R2,R4,0(R1)                                                      
         STC   R2,BOBYTE1                                                       
         CLI   BOBYTE1,PTATRAL                                                  
         BNE   AAVL02                                                           
         MVC   BODUB1,PP$AALLO                                                  
         MVC   BODUB2,PP$FALLO                                                  
         MVC   BODUB3,PP$HRSB                                                   
         BAS   RE,INPAVL                                                        
         BZ    EXIT                                                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',ATRNREC),LSPRATA,(1,PZERO),0           
         BE    EXITY                                                            
         ST    R3,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
AAVL02   CLI   BOBYTE1,PTATWOF                                                  
         BNE   AAVL04                                                           
         MVC   BODUB1,PP$AWOFF                                                  
         MVC   BODUB2,PP$FWOFF                                                  
         MVC   BODUB3,PP$HRSW                                                   
         BAS   RE,INPAVL                                                        
         BZ    EXIT                                                             
         GOTO1 AALLTRN,BOPARM,('PTATWOF',ATRNREC),LSPRATA,(1,PZERO),0           
         BE    EXITY                                                            
         ST    R3,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
AAVL04   CLI   BOBYTE1,PTATTRFT                                                 
         BNE   AAVL06                                                           
         MVC   BODUB1,PP$AXFER                                                  
         MVC   BODUB2,PP$FXFER                                                  
         MVC   BODUB3,PP$HRSX                                                   
         BAS   RE,INPAVL                                                        
         BZ    EXIT                                                             
         GOTO1 AALLTRN,BOPARM,('PTATTRFT',ATRNREC),LSPRATA,(1,PZERO),0          
         BE    EXITY                                                            
         ST    R3,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
AAVL06   DC    H'0'                                                             
*                                                                               
INPAVL   NTR1  ,                   ** TEST INPUT TO NET/HOURS FIELDS **         
         XR    R0,R0                                                            
         TM    FHIID(R3),FHIIVA    TEST NET FIELD CHANGED                       
         BO    IAVL02                                                           
         CLI   FHILD(R3),0         TEST NET FIELD CLEARED                       
         BE    *+10                                                             
         BCTR  R0,0                                                             
         B     IAVL04                                                           
         GOTO1 DISAMT,BOPARM,FHDAD(R3),BODUB1,BODUB2                            
         OI    FHIID(R3),FHIIVA                                                 
         OI    FHOID(R3),FHOITR                                                 
         TM    FHIID(R4),FHIIVA    ENSURE HOUR FIELD VALID                      
         BZ    IAVL02                                                           
         GOTO1 DISHRS,BOPARM,FHDAD(R4),BODUB3                                   
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
IAVL02   TM    FHIID(R4),FHIIVA    TEST HOUR FIELD CHANGED                      
         BO    INPAVLX                                                          
         CLI   FHILD(R4),0         TEST HOUR FIELD CLEARED                      
         BE    *+10                                                             
         BCTR  R0,0                                                             
         B     INPAVLX                                                          
IAVL04   GOTO1 DISHRS,BOPARM,FHDAD(R4),BODUB3                                   
         OI    FHIID(R4),FHIIVA                                                 
         OI    FHOID(R4),FHOITR                                                 
         TM    FHIID(R3),FHIIVA    ENSURE NET FIELD VALID                       
         BZ    IAVL02                                                           
         GOTO1 DISAMT,BOPARM,FHDAD(R3),BODUB1,BODUB2                            
         OI    FHOID(R3),FHOITR                                                 
*                                                                               
INPAVLX  LTR   R0,R0               CC=ZERO IF NO CHANGES                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY AVAILABLE TOTALS FOR PENDING                                *         
***********************************************************************         
         SPACE 1                                                                
DISAVL   NTR1  ,                                                                
         CLI   CSACT,ACTZOO        TEST REGULAR ZOOM SCREEN                     
         BNE   DAVL02                                                           
         GOTO1 DISAMT,BOPARM,REGAVNT,PM$ANVBL,PM$FNVBL                          
         OI    REGAVNTH+FHOID,FHOITR                                            
         GOTO1 (RF),(R1),REGAVCM,PM$ACVBL,PM$FCVBL                              
         OI    REGAVCMH+FHOID,FHOITR                                            
         GOTO1 DISHRS,(R1),REGAVHR,PM$HRVBL                                     
         OI    REGAVHRH+FHOID,FHOITR                                            
*&&US                                                                           
         LH    RF,=Y(LC@NRTV-TWAD)                                              
         CP    PP$AWOFF,BCPZERO                                                 
         BE    *+8                                                              
         LH    RF,=Y(LC@WRTN-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         MVC   REGNARW,0(RF)                                                    
         OI    REGNARWH+FHOID,FHOITR                                            
*&&                                                                             
         B     EXIT                                                             
*                                                                               
DAVL02   GOTO1 DISAMT,BOPARM,WOFAVNT,PM$ANVBL,PM$FNVBL                          
         OI    WOFAVNTH+FHOID,FHOITR                                            
         GOTO1 DISHRS,(R1),WOFAVHR,PM$HRVBL                                     
         OI    WOFAVHRH+FHOID,FHOITR                                            
         CLI   CSACT,ACTZWR                                                     
         BE    DAVL04                                                           
*                                                                               
         MVC   WOFSEQW,BCSPACES                                                 
         OI    WOFSEQWH+FHOID,FHOITR                                            
         MVC   WOFSEQ,BCSPACES                                                  
         OI    WOFSEQH+FHOID,FHOITR                                             
         CP    PP$AWOFF,BCPZERO                                                 
         BE    EXIT                                                             
         L     R2,APTAREC                                                       
         USING PTARECD,R2                                                       
         OC    PTAKSEQN,PTAKSEQN                                                
         BZ    EXIT                                                             
         MVCDD WOFSEQW,AC#WRTFN                                                 
         GOTO1 DISSEQ,BOPARM,WOFSEQ,PTAKSEQN                                    
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
DAVL04   GOTO1 DISAMT,BOPARM,WOFNET,PP$AWOFR,PP$FWOFR                           
         OI    WOFNETH+FHOID,FHOITR                                             
         GOTO1 DISHRS,(R1),WOFHRS,PP$HRSR                                       
         OI    WOFHRSH+FHOID,FHOITR                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   NTR1  ,                                                                
         LA    R4,SCRZOO                                                        
         CLI   CSACT,ACTZWO                                                     
         BNE   *+8                                                              
         LA    R4,SCRZWO                                                        
         CLI   CSACT,ACTZWR                                                     
         BNE   *+8                                                              
         LA    R4,SCRZWR                                                        
         USING SCRTABD,R4                                                       
*                                                                               
DSCR02   CLI   SCRTABD,EOT                                                      
         BE    DSCR10                                                           
         GOTO1 DISPLAY,SCRTABD                                                  
         TM    SCRTINDS,SCRTIVDS   TEST DISPLAY SHOULD BE VALIDATED             
         BZ    DSCR04                                                           
         OI    ZINDS,ZIVALDIS                                                   
         GOTO1 VALIDATE                                                         
         NI    ZINDS,FF-ZIVALDIS                                                
*                                                                               
DSCR04   TM    SCRTINDS,SCRTIDIS                                                
         BO    DSCR08                                                           
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
DSCR06   TM    FHAT,FHATPR                                                      
         BO    DSCR07                                                           
         OI    FHII,FHIIVA         SET FIELDS VALIDATED                         
         OC    SINP1,SINP1                                                      
         BNZ   DSCR07                                                           
         LR    R0,R1                                                            
         S     R0,ATWA                                                          
         STH   R0,SINP1                                                         
DSCR07   IC    R2,FHLN                                                          
         BXLE  R1,R2,DSCR06                                                     
         DROP  R1                                                               
*                                                                               
DSCR08   LA    R4,SCRTABL(R4)                                                   
         B     DSCR02                                                           
         DROP  R4                                                               
*                                                                               
DSCR10   OC    SINP1,SINP1                                                      
         BNZ   *+12                                                             
         LA    RE,BASACTH-TWAD                                                  
         STH   RE,SINP1                                                         
*        BAS   RE,DISAVL                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   NTR1  ,                                                                
         BAS   RE,SETAVL           ADJUST AMOUNTS AVAILABLE                     
*                                                                               
VSCR00   LA    R4,SCRZOO                                                        
         CLI   CSACT,ACTZWO                                                     
         BNE   *+8                                                              
         LA    R4,SCRZWO                                                        
         CLI   CSACT,ACTZWR                                                     
         BNE   *+8                                                              
         LA    R4,SCRZWR                                                        
         USING SCRTABD,R4                                                       
*                                                                               
VSCR02   CLI   SCRTABD,EOT                                                      
         BE    VALSCRY                                                          
         TM    ZINDS,ZIREDIS       TEST RE-DISPLAYING REST OF SCREEN            
         BZ    VSCR04                                                           
         GOTO1 DISPLAY,SCRTABD                                                  
         TM    SCRTINDS,SCRTIVDS   TEST DISPLAY SHOULD BE VALIDATED             
         BZ    VSCR18                                                           
         OI    ZINDS,ZIVALDIS                                                   
         GOTO1 VALIDATE                                                         
         NI    ZINDS,FF-ZIVALDIS                                                
         B     VSCR18                                                           
*                                                                               
VSCR04   TM    SCRTINDS,SCRTIDIS                                                
         BO    VSCR18                                                           
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
VSCR06   TM    SCRTINDS,SCRTIVPR                                                
         BO    *+12                                                             
         TM    FHAT,FHATPR         FIND UNVALIDATED INPUT FIELD                 
         BO    VSCR08                                                           
         TM    FHII,FHIIVA                                                      
         BZ    VSCR10                                                           
VSCR08   IC    R2,FHLN                                                          
         BXLE  R1,R2,VSCR06                                                     
         B     VSCR18                                                           
         DROP  R1                                                               
VSCR10   GOTO1 VALIDATE,SCRTABD                                                 
         BNE   VALSCRN                                                          
         NI    ZINDS,FF-ZIVALDIS                                                
         GOTO1 DISPLAY                                                          
*                                                                               
VSCR18   LA    R4,SCRTABL(R4)                                                   
         B     VSCR02                                                           
         DROP  R4                                                               
*                                                                               
*ALSCRY  BAS   RE,DISAVL                                                        
VALSCRY  B     EXITY                                                            
*ALSCRN  BAS   RE,DISAVL                                                        
VALSCRN  B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY ROUTINE                                                     *         
*                                                                     *         
* R1=A(SCRTABD ENTRY)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPLAY  NTR1  ,                                                                
         LR    R4,R1                                                            
         USING SCRTABD,R4                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         BZ    DIS04                                                            
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         TM    FHOI,FHOITR                                                      
         BO    DIS04                                                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
DIS02    OI    FHOI,FHOITR         TRANSMIT FIELDS                              
         IC    R2,FHLN                                                          
         BXLE  R1,R2,DIS02                                                      
         DROP  R1                                                               
*                                                                               
DIS04    TM    SCRTINDS,SCRTIVWO+SCRTIVXF+SCRTIVWR                              
         BZ    DIS20                                                            
         LA    RF,PP$AWOFF                                                      
         TM    SCRTINDS,SCRTIVXF                                                
         BZ    *+8                                                              
         LA    RF,PP$AXFER                                                      
         TM    SCRTINDS,SCRTIVWR                                                
         BZ    *+8                                                              
         LA    RF,PP$AWOFR                                                      
         CP    0(L'PP$AWOFF,RF),BCPZERO                                         
         BNE   DIS20                                                            
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
DIS12    IC    R2,FHLN                                                          
         TM    FHAT,FHATHI+FHATPR                                               
         BO    DIS14                                                            
         LR    RE,R2               CLEAR FIELDS                                 
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    RE,FHDAD(RE)                                                     
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
DIS14    BXLE  R1,R2,DIS12                                                      
         B     EXIT                                                             
         DROP  R1                                                               
*                                                                               
DIS20    CLI   SCRTPTA,0                                                        
         BE    DIS22                                                            
         GOTO1 FINDPTA,SCRTPTA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DIS22    XR    RF,RF                                                            
         ICM   RF,3,SCRTDIS                                                     
         B     CLB03(RF)                                                        
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TRANSACTION RECORD INFO                                     *         
***********************************************************************         
         SPACE 1                                                                
DISTRN   L     R2,ATRNREC                                                       
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         USING TRNELD,TRNRFST                                                   
*                                                                               
         MVC   REGJOBC,BCJOBCOD    JOB CODE/NAME                                
         MVC   REGJOBN,BCJOBNAM                                                 
*                                                                               
         MVC   REGSUPC,TRNKULC     SUPPLIER CODE/NAME                           
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'TRNKCULC),TRNKCULC                                       
         GOTO1 AGETACT,0                                                        
         MVC   REGSUPN,ACNAME                                                   
*                                                                               
         MVC   REGWC,TRNKWORK      WORK-CODE/NAME                               
         LA    RF,IOKEY                                                         
         USING WCORECD,RF                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY(WCOKWRK-WCOKCPY),TRNKCPY                                 
         MVC   WCOKWRK,TRNKWORK                                                 
         DROP  RF                                                               
         GOTO1 AIO,IOACCMST+IOREAD+IO1                                          
         BNE   DTRN10                                                           
         L     R1,AIO1                                                          
         LA    R1,WCORFST-WCORECD(R1)                                           
         XR    RF,RF                                                            
DTRN02   CLI   0(R1),0                                                          
         BE    DTRN10                                                           
         IC    RF,1(R1)                                                         
         CLI   0(R1),NAMELQ                                                     
         BNE   DTRN04                                                           
         MVC   REGWCN,BCSPACES                                                  
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   REGWCN(0),NAMEREC-NAMELD(R1)                                     
         B     DTRN10                                                           
DTRN04   CLI   0(R1),WCOELQ                                                     
         BNE   DTRN08                                                           
         MVC   REGWCN(L'WCODESC),WCODESC-WCOELD(R1)                             
DTRN08   BXH   R1,RF,DTRN02                                                     
*                                                                               
DTRN10   LA    R3,TRNELD                                                        
         USING PXDELD,R3                                                        
         XR    RF,RF                                                            
DTRN12   CLI   PXDEL,0             LOOK FOR TRANSFER                            
         BE    DTRN20                                                           
         CLI   PXDEL,PXDELQ                                                     
         BNE   DTRN14                                                           
         CLI   PXDTYPE,PXDTFROM                                                 
         BE    DTRN16                                                           
DTRN14   IC    RF,PXDLN                                                         
         BXH   R3,RF,DTRN12                                                     
DTRN16   MVCDD REGFRMW,AC#FROMJ                                                 
         MVC   REGFRMC,PXDFRTOA                                                 
         LA    R1,IOKEY                                                         
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,PXDFRTOA                                                 
         GOTO1 AGETACT,0                                                        
         MVC   REGFRMN,ACNAME                                                   
         DROP  R1,R3                                                            
*                                                                               
DTRN20   MVC   REGREF,TRNKREF                                                   
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,REGDAT)                          
         MVC   REGBAT,TRNMOS                                                    
         MVC   REGCOM,BC@NO                                                     
         TM    TLXSTAT,TLXSNOCM                                                 
         BO    *+10                                                             
         MVC   REGCOM,BC@YES                                                    
*&&UK                                                                           
         MVC   REGAUT,BC@NO                                                     
         TM    TRNSTAT,TRNSAUTH                                                 
         BZ    *+10                                                             
         MVC   REGAUT,BC@YES                                                    
*&&                                                                             
*                                                                               
DTRN30   LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         LA    R4,REGWRD1H                                                      
         LA    R0,5                                                             
DTRN32   CLI   TYPTABD,EOT                                                      
         BE    DTRN40                                                           
         GOTO1 DISTYP,BOPARM,TYPTABD,(R4)                                       
         BNE   DTRN38                                                           
         MVI   FHDAD(R4),AC#ESCL                                                
         MVC   FHDAD+1(L'TYPDICT,R4),TYPDICT                                    
         MVI   FHDAD+3(R4),L'REGWRD1                                            
         GOTO1 VDICTAT,BOPARM,C'SL  ',FHDAD(R4)                                 
         LA    R4,REGWRD2H-REGWRD1H(R4)                                         
         BCT   R0,DTRN38                                                        
         B     DTRN40                                                           
DTRN38   LA    R3,TYPTABL(R3)                                                   
         B     DTRN32                                                           
         DROP  R3                                                               
*                                                                               
DTRN40   B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BATCH TYPE ROUTINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISTYP   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING TYPTABD,R3                                                       
         USING REGWRD1H,R4                                                      
         CLI   TYPINDS,0                                                        
         BE    DISTYP02            TEST TYPINDS FILTER                          
         MVI   BOBYTE1,0                                                        
         TM    ZINDS,ZIAGYCUR                                                   
         BO    *+8                                                              
         OI    BOBYTE1,TYPIFC                                                   
         NC    BOBYTE1,TYPINDS                                                  
         BZ    EXITN                                                            
         B     DISTYP04                                                         
*                                                                               
DISTYP02 MVC   BOBYTE1,TYPSTAT1    TEST TYPSTAT1 FILTER                         
         NC    BOBYTE1,TLXSTAT                                                  
         CLC   BOBYTE1,TYPSTAT1                                                 
         BNE   EXITN                                                            
*                                                                               
DISTYP04 XR    RF,RF                                                            
         ICM   RF,3,TYPRTN                                                      
         B     CLB03(RF)                                                        
         SPACE 1                                                                
TYPTYP   XR    R0,R0                                                            
         IC    R0,TRNTYPE                                                       
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  REGDSC1(2),BODUB1                                                
         B     EXITY                                                            
*                                                                               
TYPNET   LA    R2,PA$NET           * NET *                                      
         TM    ZINDS,ZIAGYCUR                                                   
         BO    *+8                                                              
         LA    R2,PB$NET                                                        
         CURED (P8,(R2)),(L'REGDSC1,REGDSC1),ZCURBIL,MINUS=YES,        *        
               ZERO=NOBLANK,ALIGN=LEFT                                          
         B     EXITY                                                            
*                                                                               
TYPORD   DS    0H                  * ORDER *                                    
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('FFNELQ',ATRNREC),0                 
         CLI   12(R1),0                                                         
         BNE   EXITN                                                            
         L     RF,12(R1)                                                        
         USING FFNELD,RF                                                        
         LA    RE,REGDSC1                                                       
         CLI   FFNSTAT,FFNSPRTQ                                                 
         BNE   *+12                                                             
         MVI   0(RE),C'P'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(L'FFNONUM,RE),FFNONUM                                          
         B     EXITY                                                            
         DROP  RF                                                               
*                                                                               
TYPHRS   DS    0H                  * HOURS *                                    
         CURED (P8,PA$HOURS),(L'REGDSC1,REGDSC1),2,ALIGN=LEFT                   
         B     EXITY                                                            
*                                                                               
TYPCOST  CP    PA$HOURS,BCPZERO    * COST *                                     
         BE    EXITN                                                            
         TM    BCAPINDS,BCAPICHR   TEST COST=SCIEL AMOUNT                       
         BZ    TYPSCI                                                           
         B     TYPNDH                OR NET / HOURS                             
*                                                                               
TYPCHRG  CP    PA$HOURS,BCPZERO    * CHARGE *                                   
         BE    EXITN                                                            
         TM    BCAPINDS,BCAPICHR   TEST CHARGE=SCIEL AMOUNT                     
         BZ    TYPNDH                OR NET / HOURS                             
*                                                                               
TYPSCI   ZAP   BOPL82,BCPZERO                                                   
         LA    R1,TRNELD                                                        
         USING SCIELD,R1           R1=A(SCIEL ELEMENT)                          
         XR    RF,RF                                                            
TYPSCI02 CLI   SCIEL,0                                                          
         BE    TYPSCI10                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCOMM                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R1,RF,TYPSCI02                                                   
         ZAP   BOPL82,SCIAMNT                                                   
         DROP  R1                                                               
TYPSCI10 MVC   BOPL81,BOPL82                                                    
         TM    ZINDS,ZIAGYCUR                                                   
         BO    TYPSCI12                                                         
         EXCHP BOPL82,CSEXCVAL,DUB=BOPL81                                       
TYPSCI12 CURED (P8,BOPL81),(L'REGDSC1,REGDSC1),ZCURBIL,MINUS=YES,      *        
               ZERO=NOBLANK,ALIGN=LEFT                                          
         B     EXITY                                                            
*                                                                               
TYPNDH   XC    BOPL81,BOPL81                                                    
         MVC   BOPL82,PA$NET                                                    
         TM    ZINDS,ZIAGYCUR                                                   
         BO    *+10                                                             
         MVC   BOPL82,PB$NET                                                    
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),PA$HOURS                                              
         SRP   BOPL81,64-2,5                                                    
         CURED (P8,BOPL81),(L'REGDSC1,REGDSC1),ZCURBIL,MINUS=YES,      *        
               ZERO=NOBLANK,ALIGN=LEFT                                          
         B     EXITY                                                            
*                                                                               
TYPOWC   XR    RF,RF               ** ORDER W/C **                              
         LA    R1,TRNELD                                                        
         USING OAMELD,R1                                                        
         IC    RF,OAMLN                                                         
         AR    R1,RF                                                            
         CLI   OAMEL,OAMELQ                                                     
         BNE   *-10                                                             
         MVC   REGDSC1(L'OAMWORK),OAMWORK                                       
         B     EXITY                                                            
         DROP  R1                                                               
*                                                                               
TYPCUR   DS    0H                  * CURRENCY *                                 
         MVC   REGDSC1(L'CSBILCUR),CSBILCUR                                     
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3,R4                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY UPDATED INFO FOR THE REGULAR SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
DISUPDRG GOTO1 LSTUPB                                                           
         GOTO1 DISUDLST,BOPARM,(L'UPB,REGUPD1)                                  
         GOTO1 LSTUWO                                                           
         GOTO1 DISUDLST,(R1),(L'UPW,REGUPD1+L'UPB)                              
         GOTO1 LSTUXF                                                           
         GOTO1 DISUDLST,(R1),(L'UPX,REGUPD1+L'UPB+L'UPW)                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY UPDATED INFO FOR THE WRITE-OFF SCREEN                       *         
***********************************************************************         
         SPACE 1                                                                
DISUPDWO GOTO1 LSTUWO                                                           
         GOTO1 DISUDLST,BOPARM,(L'UPW,WOFUPD1)                                  
         GOTO1 LSTUPB                                                           
         GOTO1 DISUDLST,(R1),(L'UPB,WOFUPD1+L'UPW)                              
         GOTO1 LSTUXF                                                           
         GOTO1 DISUDLST,(R1),(L'UPX,WOFUPD1+L'UPW+L'UPB)                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO DISPLAY UPDATED LIST ENTRIES                             *         
*                                                                     *         
* NTRY: P1 BYTE 0 = L'PRINT                                           *         
*             1-3 = A(1ST PRINT POSITION)                             *         
***********************************************************************         
         SPACE 1                                                                
DISUDLST NTR1  ,                                                                
         XR    RF,RF                                                            
         IC    RF,0(R1)            RF=L(TO PRINT)                               
         BCTR  RF,0                RF=EXECUTABLE LENGTH OF PRINT                
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)          R2=A(PRINT POSITION)                         
         LH    R0,UDLST#                                                        
         CLC   UDLST#,=AL2(3)                                                   
         BNH   *+8                                                              
         LA    R0,3                                                             
         LTR   R0,R0                                                            
         BZ    DUDLSTX                                                          
         L     R3,AIO1                                                          
         USING UD,R3                                                            
DUDLST02 EX    RF,*+4                                                           
         MVC   0(0,R2),UPRT                                                     
         LA    R3,UDL(R3)                                                       
         LA    R2,REGUPD2H-REGUPD1H(R2)                                         
         BCT   R0,DUDLST02                                                      
*                                                                               
DUDLSTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE LIST OF UPDATED PRIOR BILLS                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LSTUPB   NTR1  ,                                                                
         XC    UDLST#,UDLST#                                                    
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
*&&UK                                                                           
         CLC   TRNKWORK,ORDWC      TEST ORDER TRANSACTION                       
         BNE   LUPB02              YES - 'REMOVE' MATCHED PTAELS                
         GOTO1 AMCHORD,BOPARM,(C'I',TRNRFST),AIO2                               
         L     R3,AIO2                                                          
*&&                                                                             
         USING PTAELD,R3                                                        
         USING UD,BOWORK1                                                       
LUPB02   CLI   PTAEL,0                                                          
         BE    LUPB10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   LUPB08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   LUPB08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    LUPB08                                                           
*                                                                               
         XC    UD(UDL),UD                                                       
         MVC   UKBDAT,PTARBLDT                                                  
         MVC   UPBNO,PTARBLNO                                                   
         GOTO1 VDATCON,BOPARM,(2,PTARBLDT),(17,UPBDAT)                          
         GOTO1 DISNET,(R1),UPBNET,PTAELD                                        
         GOTO1 ADDUD,UD                                                         
*                                                                               
LUPB08   XR    RF,RF                                                            
         IC    RF,PTALN                                                         
         BXH   R3,RF,LUPB02                                                     
*                                                                               
LUPB10   B     EXIT                                                             
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* CREATE LIST OF UPDATED WRITE-OFFS                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LSTUWO   NTR1  ,                                                                
         XC    UDLST#,UDLST#                                                    
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         USING UD,BOWORK1                                                       
LUWO02   CLI   PTAEL,0                                                          
         BE    LUWO10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   LUWO08                                                           
         CLI   PTATYPE,PTATWOF                                                  
         BE    *+12                                                             
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   LUWO08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    LUWO08                                                           
*                                                                               
         XC    UD(UDL),UD                                                       
         MVC   UKWSEQ,PTASEQN                                                   
         XC    UKWSEQ,BCEFFS                                                    
         MVC   UKWTYP,PTATYPE                                                   
         GOTO1 DISSEQ,BOPARM,UPWSEQ,PTASEQN                                     
         GOTO1 VDATCON,(R1),(2,PTADATE),(17,UPWDAT)                             
         GOTO1 DISNET,(R1),UPWNET,PTAELD                                        
         GOTO1 ADDUD,UD                                                         
*                                                                               
LUWO08   XR    RF,RF                                                            
         IC    RF,PTALN                                                         
         BXH   R3,RF,LUWO02                                                     
*                                                                               
LUWO10   B     EXIT                                                             
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* CREATE LIST OF UPDATED TRANSFERS                                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LSTUXF   NTR1  ,                                                                
         XC    UDLST#,UDLST#                                                    
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         USING UD,BOWORK1                                                       
LUXF02   CLI   PTAEL,0                                                          
         BE    LUXF10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   LUXF08                                                           
         CLI   PTATYPE,PTATTRFT                                                 
         BNE   LUXF08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    LUXF08                                                           
*                                                                               
         XC    UD(UDL),UD                                                       
         MVC   UKXDAT,PTADATE                                                   
         MVC   UPXJOB,PTATJOB                                                   
         GOTO1 VDATCON,BOPARM,(2,PTADATE),(17,UPXDAT)                           
         GOTO1 DISNET,(R1),UPXNET,PTAELD                                        
         GOTO1 ADDUD,UD                                                         
*                                                                               
LUXF08   XR    RF,RF                                                            
         IC    RF,PTALN                                                         
         BXH   R3,RF,LUXF02                                                     
*                                                                               
LUXF10   B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD TO UPDATED LIST                                      *         
*                                                                     *         
* NTRY: R1=A(UD)                                                      *         
* EXIT: UD ADDED TO LIST IN IO1                                       *         
***********************************************************************         
         SPACE 1                                                                
ADDUD    NTR1  ,                                                                
         LR    R2,R1                                                            
NEW      USING UD,R2               R2=A(NEW ENTRY FOR LIST)                     
         LH    RF,UDLST#                                                        
         BCTR  RF,0                                                             
         MH    RF,=Y(UDL)                                                       
         A     RF,AIO1                                                          
OLD      USING UD,RF               RF=A(LAST ENTRY IN LIST)                     
         LH    RE,UDLST#                                                        
         LA    R0,1(RE)                                                         
         STH   R0,UDLST#                                                        
         LTR   RE,RE                                                            
         BZ    ADDUD04                                                          
*                                                                               
ADDUD02  CLC   NEW.UKEY,OLD.UKEY                                                
         BL    ADDUD04                                                          
         MVC   OLD.UD+UDL(UDL),OLD.UD                                           
         SH    RF,=Y(UDL)                                                       
         BCT   RE,ADDUD02                                                       
*                                                                               
ADDUD04  MVC   OLD.UD+UDL(UDL),NEW.UD                                           
*                                                                               
ADDUDX   B     EXIT                                                             
         DROP  NEW,OLD                                                          
         SPACE 1                                                                
***********************************************************************         
* UPDATE LIST ENTRY                                                   *         
***********************************************************************         
         SPACE 1                                                                
UD       DSECT                                                                  
UKEY     DS    XL4                 KEY OF ENTRY                                 
UPRT     DS    CL32                PRINT PART OF LINE                           
UDL      EQU   *-UD                                                             
*                                                                               
         ORG   UKEY                * PRIOR BILLING *                            
UKBDAT   DS    XL2                 BILL DATE                                    
         ORG   UPRT                                                             
UPB      DS    0CL25                                                            
UPBNO    DS    CL6,CL1             BILL NUMBER                                  
UPBDAT   DS    CL8,CL1             BILL DATE                                    
UPBNET   DS    CL9                 NET AMOUNT                                   
*                                                                               
         ORG   UKEY                * WRITE-OFFS *                               
UKWSEQ   DS    XL2                 SEQUENCE NUMBER                              
UKWTYP   DS    XL1                 PTA TYPRE (WRITE-OFF/RECOVERY)               
         ORG   UPRT                                                             
UPW      DS    0CL23                                                            
UPWSEQ   DS    CL4,CL1             SEQUENCE NUMBER                              
UPWDAT   DS    CL8,CL1             TRANSACTION DATE                             
UPWNET   DS    CL9                 NET AMOUNT                                   
*                                                                               
         ORG   UKEY                * TRANSFERS *                                
UKXDAT   DS    XL2                 TRANSFER DATE                                
         ORG   UPRT                                                             
UPX      DS    0CL31                                                            
UPXJOB   DS    CL12,CL1            JPB CODE                                     
UPXDAT   DS    CL8,CL1             TRANSFER DATE                                
UPXNET   DS    CL9                 NET AMOUNT                                   
         ORG   UPRT                                                             
CLB03    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY ALLOCATION (PENDING)                                        *         
***********************************************************************         
         SPACE 1                                                                
DISALL   GOTO1 DISAMT,BOPARM,REGALNT,PP$AALLO,PP$FALLO                          
         GOTO1 (RF),(R1),REGALCM,PP$ACOMM,PP$FCOMM                              
         GOTO1 DISHRS,(R1),REGALHR,PP$HRSB                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF (PENDING)                                         *         
***********************************************************************         
         SPACE 1                                                                
DISWOF   GOTO1 DISAMT,BOPARM,REGWONT,PP$AWOFF,PP$FWOFF                          
         GOTO1 DISHRS,(R1),REGWOHR,PP$HRSW                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF EXPENSE ACCOUNT                                   *         
***********************************************************************         
         SPACE 1                                                                
DISWOAC  LA    RF,EXPUL                                                         
         TM    TLXSTAT,TLXSULSI                                                 
         BZ    *+8                                                              
         LA    RF,INCUL                                                         
         GOTO1 DISACC,BOPARM,(X'80',REGWOAC),Z.PTAWEXPA,(RF)                    
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TRANSFERS (PENDING)                                         *         
***********************************************************************         
         SPACE 1                                                                
DISXFR   GOTO1 DISAMT,BOPARM,REGXFNT,PP$AXFER,PP$FXFER                          
         GOTO1 DISHRS,(R1),REGXFHR,PP$HRSX                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TRANSFERS PENDING WORK-CODE                                 *         
***********************************************************************         
         SPACE 1                                                                
DISXFWC  MVC   REGXFWC,Z.PTATWRK                                                
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TRANSFERS PENDING ACCOUNT CODE                              *         
***********************************************************************         
         SPACE 1                                                                
DISXFAC  MVC   REGXFAC,Z.PTATJOB                                                
         B     EXIT                                                             
         SPACE 1                                                                
*&&US                                                                           
***********************************************************************         
* DISPLAY TRANSFERS COMMISSIONABLE STATUS                             *         
***********************************************************************         
         SPACE 1                                                                
DISXFCM  MVC   REGXFCM,BC@NO                                                    
         TM    Z.PTASTAT2,PTASXCOM                                              
         BZ    EXIT                                                             
         MVC   REGXFCM,BC@YES                                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TRANSFERS OVERRIDE SK ACCOUNT                               *         
***********************************************************************         
         SPACE 1                                                                
DISXFSK  MVC   REGXFSK,BCSPACES                                                 
         MVI   REGXFSK,C'*'                                                     
         CLI   Z.PTALN,PTATLN1Q                                                 
         BNH   EXIT                                                             
         MVC   REGXFSK,Z.PTATSKAC                                               
         B     EXIT                                                             
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NARRATIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISNAR   L     R2,ATRNREC          TRANSACTION RECORD NARRATIVE                 
         LA    R4,REGNAR1H                                                      
*&&UK*&& B     DNAR                                                             
*&&US                                                                           
         CP    PP$AWOFF,BCPZERO    TEST WRITE-OFF PENDING                       
         BE    DNAR                                                             
         L     R2,APTAREC                                                       
         B     DNAR                                                             
*&&                                                                             
*                                                                               
DISWNAR  L     R2,APTAREC          PTA RECORD NARRATIVE                         
         LA    R4,WOFNAR1H                                                      
*                                                                               
         USING REGNAR1H,R4                                                      
         USING TRNRECD,R2          R2=A(RECORD)                                 
DNAR     XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING TRNELD,R3                                                        
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
         MVC   REGNAR1,TRNNARR                                                  
         MVC   REGNAR2,TRNNARR+L'REGNAR1                                        
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF AMOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
DISWAMT  GOTO1 DISAMT,BOPARM,WOFNET,PP$AWOFF,PP$FWOFF                           
         GOTO1 DISHRS,(R1),WOFHRS,PP$HRSW                                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF REFERENCE NUMBER                                  *         
***********************************************************************         
         SPACE 1                                                                
DISWREF  MVC   WOFREF,Z.PTAWREF                                                 
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF DATE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISWDAT  GOTO1 VDATCON,BOPARM,(1,Z.PTAWDAT),(17,WOFDAT)                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY EXPENSE ACCOUNT DETAILS                                     *         
***********************************************************************         
         SPACE 1                                                                
DISEXP   LA    RF,EXPUL                                                         
         TM    TLXSTAT,TLXSULSI                                                 
         BZ    *+8                                                              
         LA    RF,INCUL                                                         
         GOTO1 DISACC,BOPARM,(X'80',WOFEXPC),Z.PTAWEXPA,(RF)                    
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DEPARTMENT DETAILS                                          *         
***********************************************************************         
         SPACE 1                                                                
DISDPT   MVC   WOFDPTC,SDPTCODE                                                 
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STAFF DETAILS                                               *         
***********************************************************************         
         SPACE 1                                                                
DISSTF   MVC   WOFSTFC,SSTFCODE                                                 
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFF ACCOUNT DETAILS                                   *         
***********************************************************************         
         SPACE 1                                                                
DISWOA   MVC   WOFWOAC,BCSPACES                                                 
         MVI   WOFWOAC,C'*'                                                     
         CLI   Z.PTALN,PTAWLN1Q                                                 
         BNH   EXIT                                                             
         GOTO1 DISACC,BOPARM,WOFWOAC,Z.PTAWWOFA,EXPUL                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OFFICE DETAILS                                              *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   L     R3,APTAREC                                                       
         USING PTARECD,R3                                                       
         USING TRNELD,PTARFST                                                   
         MVC   WOFOFFC,TRNOFFC                                                  
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
*&&US                                                                           
***********************************************************************         
* DISPLAY ANALYSIS OFFICE                                             *         
***********************************************************************         
         SPACE 1                                                                
DISANAL  CLC   SANALOFF,BCSPACES   TEST OFFICE DEFINED                          
         BH    *+10                                                             
         MVC   SANALOFF,WOFOFFC                                                 
         MVC   WOFANAL,SANALOFF                                                 
         B     EXIT                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY W/O RECOVERY SEQUENCE #                                     *         
***********************************************************************         
         SPACE 1                                                                
DISRSEQ  CP    PP$AWOFR,BCPZERO                                                 
         BE    DRSEQ02                                                          
         GOTO1 FINDPTA,=AL1(PTATWOFR)                                           
         GOTO1 DISSEQ,BOPARM,WOFSEQ,Z.PTASEQN                                   
         OI    WOFSEQH+FHATD,FHATPR                                             
         B     EXIT                                                             
DRSEQ02  NI    WOFSEQH+FHATD,FF-FHATPR                                          
         MVC   WOFSEQ,BCSPACES                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY AN AMOUNT                                                   *         
*                                                                     *         
* NTRY: P1=A(14 BYTE OUTPUT FIELD)                                    *         
*       P2=A(AMOUNT FOR AGENCY CURRENCY)                              *         
*       P3=A(AMOUNT FOR BILLING CURRENCY)                             *         
***********************************************************************         
         SPACE 1                                                                
DISAMT   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         TM    ZINDS,ZIAGYCUR      TEST BILLING IN AGENCY CURRENCY              
         BO    *+6                                                              
         LR    R3,R4                                                            
         CURED (P8,(R3)),(14,(R2)),ZCURBIL,MINUS=YES,ZERO=NOBLANK,     *        
               ALIGN=LEFT                                                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET AMOUNT FROM PTA ELEMENT                                 *         
*                                                                     *         
* NTRY: P1=A(9 BYTE OUTPUT FIELD)                                     *         
*       P2=A(PTA ELEMENT)                                             *         
***********************************************************************         
         SPACE 1                                                                
DISNET   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING PTAELD,R3                                                        
         CLC   PTACUR,BCSPACES                                                  
         BH    *+10                                                             
         MVC   PTACUR,CSCPYCUR                                                  
         CLC   PTACUR,CSBILCUR     TEST CURRENCY AS BILLING                     
         BE    *+14                                                             
         MVC   5(L'PTACUR,R2),PTACUR NO - JUST DISPLAY CODE                     
         B     EXIT                                                             
         LA    R4,PTANET                                                        
         TM    ZINDS,ZIAGYCUR      TEST BILLING IN AGENCY CURRENCY              
         BO    *+8                                                              
         LA    R4,PTANETF                                                       
         CURED (P6,(R4)),(9,(R2)),ZCURBIL,ZERO=NOBLANK,MINUS=YES                
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A NUMBER OF HOURS                                           *         
*                                                                     *         
* NTRY: P1=A(7 BYTE OUTPUT FIELD)                                     *         
*       P2=A(PL8 NO. OF HOURS)                                        *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R3)),(7,(R2)),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT CODE                                             *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF DUTCH TAX TYPE REQUIRED               *         
*                 = A(15 BYTE OUTPUT FIELD)                           *         
*       P2        = A(ACCOUNT CODE)                                   *         
*       P3        = A(DEFAULT UNIT/LEDGER)                            *         
***********************************************************************         
         SPACE 1                                                                
DISACC   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING ACTKULA,R3          R3=A(ACCOUNT CODE)                           
         MVC   0(15,R2),BCSPACES                                                
         CLI   ACTKUNT,C' '        TEST NO ACCOUNT CODE                         
         BNH   EXIT                                                             
         MVC   0(L'ACTKACT,R2),ACTKACT                                          
         CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),0(R4)                               
         BE    DISACC02            FINISHED IF DEFAULT UNIT/LEDGER              
         MVI   0(R2),C'*'                                                       
         MVC   1(L'ACTKULA,R2),ACTKULA                                          
*                                                                               
DISACC02 CLI   CUCTRY,CTRYHOL      TEST HOLLAMD                                 
         BNE   DISACCX                                                          
         TM    0(R1),X'80'         TEST TAX TYPE REQUIRED                       
         BZ    DISACCX                                                          
         CLC   EXPUL,ACTKUNT       TEST EXPENSE ACCOUNT                         
         BNE   DISACCX                                                          
         L     R1,APTAREC          EXTRACT TAX TYPE FROM PTA RECORD             
         LA    R1,PTARFST-PTARECD(R1)                                           
         USING FFTELD,R1                                                        
         XR    RF,RF                                                            
DISACC04 CLI   FFTEL,0                                                          
         BE    DISACCX                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTTAXI                                                 
         BE    *+12                                                             
         IC    RF,FFTLN                                                         
         BXH   R1,RF,DISACC04                                                   
         LA    RF,14(R2)                                                        
         CLI   0(RF),C' '          APPEND TAX TYPE TO ACCOUNT CODE              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCCOMMA                                                  
         MVC   2(1,RF),FFTDATA                                                  
         DROP   R1                                                              
DISACCX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY PTA ELEMENT/RECORD SEQUENCE NUMBER                          *         
*                                                                     *         
* NTRY: P1=A(4 BYTE OUTPUT FIELD)                                     *         
*       P2=A(SEQUENCE NUMBER)                                         *         
***********************************************************************         
         SPACE 1                                                                
DISSEQ   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         MVC   BOHALF1,0(R3)                                                    
         LH    RF,BOHALF1                                                       
         LCR   RF,RF                                                            
         CVD   RF,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(4,R2),BODUB1                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE ROUTINE                                                    *         
*                                                                     *         
* R1=A(SCRTABD ENTRY)                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1  ,                                                                
         LR    R4,R1                                                            
         USING SCRTABD,R4                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         TM    FHOI,FHOITR                                                      
         BO    VAL04                                                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
VAL02    OI    FHOI,FHOITR         TRANSMIT FIELDS                              
         IC    R2,FHLN                                                          
         BXLE  R1,R2,VAL02                                                      
         DROP  R1                                                               
*                                                                               
VAL04    TM    SCRTINDS,SCRTIVWO+SCRTIVXF+SCRTIVWR                              
         BZ    VAL20                                                            
         LA    RF,PP$AWOFF                                                      
         TM    SCRTINDS,SCRTIVXF                                                
         BZ    *+8                                                              
         LA    RF,PP$AXFER                                                      
         TM    SCRTINDS,SCRTIVWR                                                
         BZ    *+8                                                              
         LA    RF,PP$AWOFR                                                      
         CP    0(L'PP$AWOFF,RF),BCPZERO                                         
         BNE   VAL20                                                            
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,SCRTFRST                                                    
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIRST FIELD)                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTLAST                                                    
         LA    R3,TWAD(R3)         R3=A(LAST FIELD)                             
         XR    R2,R2                                                            
VAL12    TM    FHAT,FHATPR                                                      
         BO    VAL18                                                            
         CLI   FHIL,0                                                           
         BE    VAL18                                                            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$ININP)                                           
         B     EXITN                                                            
VAL18    IC    R2,FHLN                                                          
         BXLE  R1,R2,VAL12                                                      
         B     EXITY                                                            
         DROP  R1                                                               
*                                                                               
VAL20    CLI   SCRTPTA,0                                                        
         BE    VAL22                                                            
         GOTO1 FINDPTA,SCRTPTA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VAL22    TM    SCRTINDS,SCRTIFVL   TEST FVAL CALL REQUIRED                      
         BZ    VAL24                                                            
         XR    R3,R3                                                            
         ICM   R3,3,SCRTFRST                                                    
         LA    R3,TWAD(R3)                                                      
         GOTO1 AFVAL,(R3)                                                       
         TM    SCRTINDS,SCRTI0DS   TEST RE-DISPLAY IF NO INPUT                  
         BZ    VAL24                                                            
         CLI   FVILEN,0                                                         
         BH    VAL24                                                            
         GOTO1 DISPLAY,SCRTABD                                                  
         GOTO1 AFVAL,(R3)                                                       
         OI    ZINDS,ZIVALDIS                                                   
*                                                                               
VAL24    BAS   RE,VAL                                                           
         BH    EXITY               CC=HIGH = VALID BUT NO UPDATING              
         BE    VAL25                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               CC=LOW = ERROR                               
*                                                                               
VAL25    TM    ZINDS,ZIVALDIS                                                   
         BO    EXITY                                                            
*                                                                               
         CLI   SCRTPTA,0                                                        
         BE    VAL30                                                            
         CLC   ZPTAEL,ZPTAELSV                                                  
         BE    EXITY                                                            
         L     R2,ZAPTAEL                                                       
         CLC   Z.PTALN,ZPTAELSV+(PTALN-PTAELD)                                  
         BNE   VAL26                                                            
         IC    RE,Z.PTALN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),Z.PTAELD                                                 
         B     VAL30                                                            
VAL26    MVI   0(R2),FF                                                         
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',ATRNREC),0                     
         GOTO1 (RF),(R1),(C'P',ACCMST),ATRNREC,Z.PTAELD                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ZAPTAEL,16(R1)                                                   
*                                                                               
VAL30    OC    ZUPDINDS,SCRTUPD                                                 
         B     EXITY                                                            
*                                                                               
VAL      NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,3,SCRTVAL                                                     
         B     CLB03(RF)                                                        
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALLOCATION (PENDING)                                       *         
***********************************************************************         
         SPACE 1                                                                
VALALL   TM    REGALNTH+FHIID,FHIIVA                                            
         BO    VALL10                                                           
         GOTO1 VALAMT,BOPARM,REGALNTH,BODUB1                                    
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATRAL',ATRNREC),                     *        
               ('PTASCASH',LSPRATA),(1,BODUB1),0                                
         BNE   EXITN                                                            
*                                                                               
VALL10   NI    ZINDS,FF-ZICOMPER                                                
         TM    REGALCMH+FHIID,FHIIVA                                            
         BO    VALL20                                                           
         CLI   REGALCMH+FHILD,0                                                 
         BNE   *+12                                                             
         OI    REGALCMH+FHIID,FHIIVA                                            
         B     VALL20                                                           
         GOTO1 VALAMT,BOPARM,REGALCMH,BODUB2                                    
         BL    EXITN                                                            
         BE    *+8                                                              
         OI    ZINDS,ZICOMPER      SET COMMISSION ENTERED AS %                  
*                                                                               
VALL20   TM    REGALHRH+FHIID,FHIIVA                                            
         BO    VALL30                                                           
         GOTO1 AVALHRS,BOPARM,REGALHRH,LSPRATA                                  
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATRAL',ATRNREC),                     *        
               ('PTASHOUR',LSPRATA),(1,BODUB1),0                                
         BNE   EXITN                                                            
*                                                                               
VALL30   TM    TLXSTAT,TLXSNOCM    TEST NON-COMMISSIONABLE                      
         BZ    VALL32                                                           
         CP    PP$AALLO,BCPZERO                                                 
         BNE   VALALLX                                                          
         ZAP   BODUB2,BCPZERO                                                   
         B     VALL38                                                           
*                                                                               
VALL32   TM    ZINDS,ZICOMPER      TEST COMMISSION WAS PERCENTAGE               
         BO    VALL36                                                           
         TM    REGALCMH+FHIID,FHIIVA  TEST COMMISSION ENTERED                   
         BZ    VALL38                                                           
         L     RF,AGOPBLK                                                       
         ZAP   BODUB2,GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                          
*                                                                               
VALL36   XC    BOPL81,BOPL81                                                    
         MVC   BOPL82,PP$AALLO                                                  
         TM    ZINDS,ZIAGYCUR                                                   
         BO    *+10                                                             
         MVC   BOPL82,PP$FALLO                                                  
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),BODUB2                                                
         SRP   BOPL81(16),64-8,5                                                
         ZAP   BODUB2,BOPL81(16)                                                
*                                                                               
VALL38   XR    RF,RF                                                            
         TM    REGALCMH+FHIID,FHIIVA                                            
         BZ    *+8                                                              
         LA    RF,PTASCOVR                                                      
         GOTO1 AALLTRN,BOPARM,('PTATRAL',ATRNREC),((RF),LSPRATA),0,    *        
               (1,BODUB2)                                                       
         BNE   EXITN                                                            
*                                                                               
VALALLX  CLC   PP$AALLO,S.PP$AALLO                                              
         BNE   EXITY                                                            
         CLC   PP$ACOMM,S.PP$ACOMM                                              
         BNE   EXITY                                                            
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF (PENDING)                                        *         
***********************************************************************         
         SPACE 1                                                                
VALWOF   TM    ZINDS,ZIGOTPTA                                                   
         BO    VWOF02                                                           
         GOTO1 AGETPTA,BOPARM,ATRNREC,APTAREC,LSPRATA,0                         
         BNH   *+8                                                              
         NI    REGWOACH+FHIID,FF-FHIIVA                                         
*                                                                               
VWOF02   NI    REGNAR1H+FHIID,FF-FHIIVA                                         
*                                                                               
         TM    REGWONTH+FHIID,FHIIVA                                            
         BO    VWOF10                                                           
         GOTO1 VALAMT,BOPARM,REGWONTH,BODUB1                                    
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATWOF',ATRNREC),                     *        
               ('PTASCASH',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
*                                                                               
VWOF10   TM    REGWOHRH+FHIID,FHIIVA                                            
         BO    VWOF20                                                           
         GOTO1 AVALHRS,BOPARM,REGWOHRH,LSPRATA                                  
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATWOF',ATRNREC),                     *        
               ('PTASHOUR',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
*                                                                               
VWOF20   CP    PP$AWOFF,BCPZERO    TEST AMOUNT CHANGED TO ZERO                  
         BNE   VALWOFX                                                          
         MVC   REGWOAC,BCSPACES    CLEAR ACCOUNT CODE                           
         MVI   REGWOACH+FHILD,0                                                 
         NI    REGWOACH+FHIID,FF-FHIIVA                                         
*                                                                               
VALWOFX  CLC   PP$AWOFF,S.PP$AWOFF                                              
         BNE   EXITY                                                            
         CP    PP$AWOFF,BCPZERO    IF ZERO & UNCHANGED                          
         BNE   EXITH                 ENSURE PTAEL IS DELETED                    
         GOTO1 APUTPTA,BOPARM,ATRNREC,APTAREC                                   
         B     EXITH                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE WRITE-OFF (PENDING) ACCOUNT                                *         
***********************************************************************         
         SPACE 1                                                                
VALWOAC  BAS   RE,VALWEX                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TRANSFERS (PENDING)                                        *         
***********************************************************************         
         SPACE 1                                                                
VALXFR   TM    REGXFNTH+FHIID,FHIIVA                                            
         BO    VXFR10                                                           
         GOTO1 VALAMT,BOPARM,REGXFNTH,BODUB1                                    
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATTRFT',ATRNREC),                    *        
               ('PTASCASH',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
         L     R2,0(R1)                                                         
*                                                                               
VXFR10   TM    REGXFHRH+FHIID,FHIIVA                                            
         BO    VXFR20                                                           
         GOTO1 AVALHRS,BOPARM,REGXFHRH,LSPRATA                                  
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATTRFT',ATRNREC),                    *        
               ('PTASHOUR',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
         L     R2,0(R1)                                                         
*                                                                               
VXFR20   CP    PP$AXFER,BCPZERO    TEST AMOUNT CHANGED TO ZERO                  
         BNE   VXFR30                                                           
         MVI   0(R2),FF            DELETE THE PTA ELEMENT                       
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',ATRNREC),0                     
         MVC   REGXFWC,BCSPACES    CLEAR W/C & ACCOUNT                          
         NI    REGXFWCH+FHIID,FF-FHIIVA                                         
         MVI   REGXFWCH+FHILD,0                                                 
         MVC   REGXFAC,BCSPACES                                                 
         NI    REGXFACH+FHIID,FF-FHIIVA                                         
         MVI   REGXFACH+FHILD,0                                                 
*&&US                                                                           
         MVC   REGXFCM,BCSPACES                                                 
         NI    REGXFCMH+FHIID,FF-FHIIVA                                         
         MVI   REGXFCMH+FHILD,0                                                 
         MVC   REGXFSK,BCSPACES                                                 
         NI    REGXFSKH+FHIID,FF-FHIIVA                                         
         MVI   REGXFSKH+FHILD,0                                                 
*&&                                                                             
         B     VALXFRX                                                          
*                                                                               
*XFR30   CP    S.PP$AXFER,BCPZERO                                               
*        BNE   VALXFRX             TEST CHANGED FROM ZERO                       
VXFR30   NI    REGXFWCH+FHIID,FF-FHIIVA                                         
         NI    REGXFACH+FHIID,FF-FHIIVA                                         
*&&US                                                                           
         NI    REGXFCMH+FHIID,FF-FHIIVA                                         
         NI    REGXFSKH+FHIID,FF-FHIIVA                                         
*&&                                                                             
*                                                                               
VALXFRX  CLC   PP$AXFER,S.PP$AXFER                                              
         BNE   EXITY                                                            
         B     EXITH                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TRANSFERS (PENDING) WORK-CODE                              *         
***********************************************************************         
         SPACE 1                                                                
VALXFWC  LA    RF,IOKEY                                                         
         USING WCORECD,RF                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,FVIFLD                                                   
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
         MVC   Z.PTATWRK,FVIFLD                                                 
*                                                                               
         NI    REGXFACH+FHIID,FF-FHIIVA                                         
*&&US*&& NI    REGXFCMH+FHIID,FF-FHIIVA                                         
*&&US*&& NI    REGXFSKH+FHIID,FF-FHIIVA                                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TRANSFERS (PENDING) ACCOUNT CODE                           *         
***********************************************************************         
         SPACE 1                                                                
VALXFAC  MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         MVCDD BOWORK2(3),AC#TODAY SET LOCK FOR TRANSFERS                       
         GOTO1 VDICTAT,BOPARM,C'SU  ',BOWORK2,0                                 
         GOTO1 VBMONVAL,(R1),(3,BOWORK2),('POSTTRNF',ACOM),            *        
               (CULANG,BOWORK1),(CUABIN,0)                                      
         MVC   CSBSECL,0(R1)                                                    
*                                                                               
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,FVIFLD                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('JOBELQ',AIO1),0                    
         CLI   12(R1),0                                                         
         BNE   VXFAC05                                                          
         L     RE,12(R1)                                                        
         TM    JOBSTA1-JOBELD(RE),JOBSXJOB                                      
         BNO   VXFAC05                                                          
         MVC   FVMSGNO,=AL2(AE$CUEXJ)                                           
         B     EXITN                                                            
VXFAC05  TM    ACBSTAT,ACBSCLSE                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSLOCK                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXITN                                                            
         TM    ACINDS1,ACIPRJOB                                                 
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         MVC   Z.PTATJOB,FVIFLD                                                 
*                                                                               
VXFAC10  DS    0H                                                               
*&&UK                                                                           
         L     RF,ATRNREC                                                       
         USING TRNRECD,RF                                                       
         CLC   TRNKWORK,Z.PTATWRK  TEST CHANGE IN WORK-CODE                     
         BNE   VXFAC12                                                          
         CLC   TRNKACT,Z.PTATJOB   TEST CHANGE IN JOB                           
         BNE   VXFAC12                                                          
         MVC   FVMSGNO,=AL2(AE$WCJBD)                                           
         B     EXITN                                                            
         DROP  RF                                                               
*&&                                                                             
*&&US*&& NI    REGXFCMH+FHIID,FF-FHIIVA                                         
*&&US*&& NI    REGXFSKH+FHIID,FF-FHIIVA                                         
VXFAC12  B     EXITY                                                            
         SPACE 1                                                                
*&&US                                                                           
***********************************************************************         
* VALIDATE TRANSFERS COMMISSIONABLE STATUS                            *         
***********************************************************************         
         SPACE 1                                                                
VALXFCM  IC    RE,FVXLEN                                                        
*                                                                               
         NI    Z.PTASTAT2,FF-PTASXCOM                                           
         EX    RE,*+8                                                           
         BE    VALXFCMY                                                         
         CLC   FVIFLD(0),BC@NO                                                  
*                                                                               
         OI    Z.PTASTAT2,PTASXCOM                                              
         EX    RE,*+8                                                           
         BE    VALXFCMY                                                         
         CLC   FVIFLD(0),BC@YES                                                 
*                                                                               
         B     EXITN                                                            
*                                                                               
VALXFCMY NI    REGXFSKH+FHIID,FF-FHIIVA                                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TRANSFERS OVERRIDE SK ACCOUNT                              *         
***********************************************************************         
         SPACE 1                                                                
VALXFSK  MVI   Z.PTALN,PTATLN1Q                                                 
         CLI   FVILEN,1            TEST FOR NO OVERRIDE SK ACCOUNT              
         BNE   VXFSK02                                                          
         CLI   FVIFLD,C'*'                                                      
         BNE   VXFSK02                                                          
*                                                                               
         L     RF,ATRNREC                                                       
         USING TRNRECD,RF                                                       
         USING TRNELD,TRNRFST                                                   
         CLC   TRNKWORK,Z.PTATWRK  TEST CHANGE IN WORK-CODE                     
         BNE   EXITY                                                            
         CLC   TRNKACT,Z.PTATJOB   TEST CHANGE IN JOB                           
         BNE   EXITY                                                            
         LA    RE,BZQ                                                           
         TM    Z.PTASTAT2,PTASXCOM TEST CHANGE IN COMM. STATUS                  
         BZ    *+8                                                              
         LA    RE,BNZQ                                                          
         TM    TRNSTAT,TRNSNOCM                                                 
         EX    RE,*+4                                                           
         NOP   EXITY                                                            
*        CP    TRNAMNT,Z.PTANET    TEST CHANGE TO AMOUNT                        
*        BNE   EXITY                                                            
         LA    RE,REGXFWCH                                                      
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$WJSSK)                                           
         B     EXITN                                                            
         DROP  RF                                                               
*                                                                               
VXFSK02  MVI   Z.PTALN,PTATLN2Q                                                 
         MVC   Z.PTATSKAC,FVIFLD                                                
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'SKUL),SKUL                                             
         MVC   ACTKACT,FVIFLD                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE NARRATIVE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALNAR   L     R2,ATRNREC          TRANSACTION RECORD NARRATIVE                 
         LA    R4,REGNAR1H                                                      
*&&UK*&& B     VNAR                                                             
*&&US                                                                           
         L     R2,APTAREC                                                       
         CP    PP$AWOFF,BCPZERO    TEST WRITE-OFF PENDING                       
         BE    EXITY                                                            
         OI    ZUPDINDS,ZUPDIPTA                                                
         B     VNAR                                                             
*&&                                                                             
*                                                                               
VALWNAR  L     R2,APTAREC          PTA RECORD NARRATIVE                         
         LA    R4,WOFNAR1H                                                      
*                                                                               
         USING REGNAR1H,R4                                                      
         USING TRNRECD,R2                                                       
VNAR     XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING TRNELD,R3                                                        
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
*                                                                               
         MVC   TRNNARR(L'REGNAR1),REGNAR1                                       
         MVC   TRNNARR+L'REGNAR1(L'REGNAR2),REGNAR2                             
*                                                                               
         XR    RF,RF               FIND LENGTH OF NARRATIVE                     
         ICM   RF,1,REGNAR2H+FHILD                                              
         BNZ   *+12                                                             
         IC    RF,REGNAR1H+FHILD                                                
         B     *+8                                                              
         LA    RF,L'REGNAR1(RF)                                                 
         LA    RF,TRNLN1Q(RF)      SET ELEMENT LENGTH                           
         STC   RF,TRNLN                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ADD ELEMENT AT START OF RECORD               
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
*                                                                               
         B     EXITY                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF DETAIL INFORATION                                *         
***********************************************************************         
         SPACE 1                                                                
VALWAMT  TM    WOFNETH+FHIID,FHIIVA                                             
         BO    VWAMT02                                                          
         GOTO1 VALAMT,BOPARM,WOFNETH,BODUB1                                     
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATWOF',ATRNREC),                     *        
               ('PTASCASH',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
*                                                                               
VWAMT02  TM    WOFHRSH+FHIID,FHIIVA                                             
         BO    VWAMT04                                                          
         GOTO1 AVALHRS,BOPARM,WOFHRSH,LSPRATA                                   
         BNE   EXITN                                                            
         GOTO1 AALLTRN,BOPARM,('PTATWOF',ATRNREC),                     *        
               ('PTASHOUR',LSPRATA),(1,BODUB1),0                                
         BL    EXITN                                                            
*                                                                               
VWAMT04  CP    PP$AWOFF,BCPZERO    TEST CHANGED TO ZERO                         
         BNE   *+12                                                             
         OI    ZINDS,ZIREDIS                                                    
         B     VALWAMTX                                                         
         CP    S.PP$AWOFF,BCPZERO                                               
         BNE   VALWAMTX            TEST CHANGED FROM ZERO                       
         NI    WOFDATH+FHIID,FF-FHIIVA                                          
         NI    WOFREFH+FHIID,FF-FHIIVA                                          
         NI    WOFEXPCH+FHIID,FF-FHIIVA                                         
         NI    WOFWOACH+FHIID,FF-FHIIVA                                         
         NI    WOFOFFCH+FHIID,FF-FHIIVA                                         
*                                                                               
VALWAMTX CP    PP$AWOFF,S.PP$AWOFF                                              
         BNE   EXITY                                                            
         B     EXITH                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALWREF  MVC   Z.PTAWREF,FVIFLD                                                 
* TEST THAT W/O REF# DOES NOT EQUAL ANY UPDATED W/O S OR WORIES                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALWDAT  GOTO1 AVALDAT,WOFDATH                                                  
         BNE   EXITN                                                            
         MVC   Z.PTAWDAT,BCWORK+2                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE EXPENSE ACCOUNT DETAILS                                    *         
***********************************************************************         
         SPACE 1                                                                
VALEXP   MVC   WOFEXPN,BCSPACES                                                 
*                                                                               
         BAS   RE,VALWEX                                                        
         BNE   EXITN                                                            
         MVC   WOFEXPN,ACNAME                                                   
         MVC   SEXBSTAT,ACBSTAT    SAVE ACBSTAT                                 
*&&US                                                                           
         PUSH  USING                                                            
         TM    SEXBSTAT,ACBSPERS   TEST STAFF=Y                                 
         BZ    VALEXPX                                                          
         TM    SEXBSTAT,ACBSDEPT   TEST DEPT=N                                  
         BO    VALEXPX                                                          
         USING LDGRECD,IOKEY                                                    
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'STFUL),STFUL                                           
         GOTO1 AGETLDG                                                          
         BNE   VALEXPX                                                          
         ICM   RF,15,ACALDG                                                     
         USING LDGTABD,RF                                                       
         CLI   LDGTLVA,L'ACTKACT   TEST > 2 LEVELS                              
         BE    VALEXPX                                                          
         CLI   LDGTLVB,L'ACTKACT                                                
         BE    VALEXPX                                                          
         OI    SEXBSTAT,ACBSDEPT   YES - DEPARTMENT IS REQUIRED                 
         DROP  RF                                                               
*&&                                                                             
VALEXPX  NI    WOFDPTCH+FHIID,FF-FHIIVA INVALIDATE DEPT. FIELD                  
         NI    WOFSTFCH+FHIID,FF-FHIIVA INVALIDATE STAFF FIELD                  
         NI    WOFOFFCH+FHIID,FF-FHIIVA INVALIDATE OFFICE FIELD                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DEPARTMENT DETAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALDPT   MVC   WOFDPTN,BCSPACES                                                 
*                                                                               
         MVC   BOWORK1(L'SDPTCODE),SDPTCODE                                     
         MVC   SDPTCODE,FVIFLD                                                  
         TM    SEXBSTAT,ACBSDEPT   TEST DEPARTMENT REQUIRED                     
         BO    VDPT02                                                           
         CLI   FVILEN,0                                                         
         BE    VALDPTY                                                          
         MVC   FVMSGNO,=AL2(AE$ANFDP)                                           
         B     EXITN                                                            
*                                                                               
VDPT02   MVC   FVMSGNO,=AL2(AE$IDPTQ)                                           
         CLI   FVILEN,0                                                         
         BE    EXITN                                                            
*                                                                               
VALDPTY  CLC   SDPTCODE,BOWORK1                                                 
         BE    EXITH                                                            
         NI    WOFOFFCH+FHIID,FF-FHIIVA                                         
         NI    WOFSTFCH+FHIID,FF-FHIIVA                                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE STAFF DETAILS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSTF   MVC   WOFSTFN,BCSPACES                                                 
*                                                                               
         MVC   BOWORK1(L'SSTFCODE),SSTFCODE                                     
         MVC   SSTFCODE,FVIFLD                                                  
         TM    SEXBSTAT,ACBSPERS   TEST STAFF REQUIRED                          
         BO    VSTF02                                                           
         CLI   FVILEN,0                                                         
         BE    VALSTFY                                                          
         MVC   FVMSGNO,=AL2(AE$ANFST)                                           
         B     EXITN                                                            
*                                                                               
VSTF02   MVC   FVMSGNO,=AL2(AE$ISTFQ)                                           
         CLI   FVILEN,0                                                         
         BE    EXITN                                                            
*&&UK                                                                           
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'STFUL),STFUL                                           
         MVC   ACTKACT,SSTFCODE                                                 
         DROP  RF                                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         MVC   WOFSTFN,ACNAME                                                   
*&&                                                                             
VALSTFY  CLC   SSTFCODE,BOWORK1                                                 
         BE    EXITH                                                            
*&&US*&& NI    WOFOFFCH+FHIID,FF-FHIIVA                                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE WRITE-OFF ACCOUNT DETAILS                                  *         
***********************************************************************         
         SPACE 1                                                                
VALWOA   MVC   WOFWOAN,BCSPACES                                                 
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITN                                                            
         CLI   FVILEN,1                                                         
         BNE   VWOA02                                                           
         CLI   FVIFLD,C'*'                                                      
         MVI   Z.PTALN,PTAWLN1Q                                                 
         BE    VALWOAY                                                          
VWOA02   XR    RF,RF                                                            
         TM    TLXSTAT,TLXSHOUR                                                 
         BZ    *+8                                                              
         LA    RF,X'40'(RF)                                                     
         GOTO1 AVALWAN,BOPARM,((RF),0)                                          
         BNE   EXITN                                                            
         MVC   WOFWOAN,ACNAME                                                   
         MVI   Z.PTALN,PTAWLN2Q                                                 
         MVC   Z.PTAWWOFA,ACCODE+(ACTKUNT-ACTKCPY)                              
*                                                                               
VALWOAY  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OFFICE CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   TM    BCCPYST1,CPYSOROE   TEST ON OFFICES                              
         BZ    VALOFF02                                                         
         MVI   FVMINL,1                                                         
         TM    BCCPYST4,CPYSOFF2   TEST NEW OFFICE SYSTEM                       
         BO    *+8                                                              
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,WOFOFFCH                                                   
         BNE   EXITN                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'OFFUL),OFFUL                                           
         MVC   ACTKACT(L'TRNOFFC),FVIFLD                                        
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         DROP  R2                                                               
*&&UK                                                                           
         MVC   WOFOFFN,ACNAME                                                   
         BAS   RE,VALDPSTF                                                      
         BNE   EXITN                                                            
*&&                                                                             
*                                                                               
VALOFF02 L     RF,APTAREC                                                       
         USING PTARECD,RF                                                       
         USING TRNELD,PTARFST                                                   
         MVC   TRNOFFC,FVIFLD                                                   
         DROP  RF                                                               
*&&US*&& NI    WOFANALH+FHIID,FF-FHIIVA                                         
         B     EXITY                                                            
         SPACE 1                                                                
*&&US                                                                           
***********************************************************************         
* VALIDATE ANALYSIS OFFICE DETAILS                                    *         
***********************************************************************         
         SPACE 1                                                                
VALANAL  TM    BCCPYST1,CPYSOROE   TEST ON OFFICES                              
         BZ    VALANALX                                                         
         MVI   FVMINL,1                                                         
         TM    BCCPYST4,CPYSOFF2   TEST NEW OFFICE SYSTEM                       
         BO    *+8                                                              
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,WOFANALH                                                   
         BNE   EXITN                                                            
         MVC   SANALOFF,FVIFLD                                                  
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'OFFUL),OFFUL                                           
         MVC   ACTKACT(L'SANALOFF),SANALOFF                                     
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         DROP  R2                                                               
*&&US                                                                           
         BAS   RE,VALDPSTF                                                      
         BNE   EXITN                                                            
*&&                                                                             
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('ANOELQ',APTAREC),0                 
         L     RF,APTAREC                                                       
         USING PTARECD,RF                                                       
         USING TRNELD,PTARFST                                                   
         CLC   SANALOFF,TRNOFFC                                                 
         BE    VALANALX                                                         
         DROP  RF                                                               
         LA    R2,BOELEM                                                        
         USING ANOELD,R2                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER                                                  
         MVC   ANOOFFC,SANALOFF                                                 
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APTAREC,ANOELD                       
         DROP  R2                                                               
*                                                                               
VALANALX B     EXITY                                                            
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DEPARTMENT/STAFF                                           *         
*                                                                     *         
* NTRY: IOKEY = KEY FOR OFFICE RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDPSTF NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
*                                                                               
         MVC   WOFDPTN,BCSPACES                                                 
         OI    WOFDPTNH+FHOID,FHOITR                                            
         TM    SEXBSTAT,ACBSDEPT                                                
         BZ    VDPSTF02                                                         
         LA    RF,ACTKACT+1                                                     
         TM    BCCPYST4,CPYSOFF2   TEST NEW OFFICE SYSTEM                       
         BZ    *+8                                                              
         LA    RF,ACTKACT+L'TRNOFFC                                             
         MVC   0(L'SDPTCODE,RF),SDPTCODE                                        
         GOTO1 AGETACT,0                                                        
         BE    *+16                                                             
         LA    RE,WOFDPTCH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
         MVC   WOFDPTN,ACNAME                                                   
*                                                                               
VDPSTF02 DS    0H                                                               
*&&US                                                                           
         MVC   WOFSTFN,BCSPACES                                                 
         OI    WOFSTFNH+FHOID,FHOITR                                            
         TM    SEXBSTAT,ACBSPERS                                                
         BZ    VDPSTF04                                                         
         MVC   ACTKUNT(L'STFUL),STFUL                                           
         LA    RF,ACTKACT+L'ACTKACT-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(L'SSTFCODE,RF),SSTFCODE                                        
         GOTO1 AGETACT,0                                                        
         BE    *+16                                                             
         LA    RE,WOFSTFCH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
         MVC   WOFSTFN,ACNAME                                                   
*&&                                                                             
VDPSTF04 B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE W/O RECOVERY SEQUENCE #                                    *         
***********************************************************************         
         SPACE 1                                                                
VALRSEQ  XR    R0,R0                                                            
         ICM   R0,1,FVILEN                                                      
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITN                                                            
         XR    R2,R2               CALCULATE NUMBER ENTERED                     
         LA    R1,FVIFLD                                                        
VRSEQ02  CLI   0(R1),C'0'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITN                                                            
         IC    RE,0(R1)                                                         
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         MH    R2,=Y(10)                                                        
         AR    R2,RE                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VRSEQ02                                                       
         LNR   R2,R2                                                            
         STH   R2,BOHALF1                                                       
         GOTO1 AGETPTA,BOPARM,ATRNREC,APTAREC,LSPRATA,                 *        
               ('PTATWOFR',BOHALF1)                                             
         BNE   EXITN                                                            
         MVC   SDPTCODE,BOWORK1                                                 
         MVC   SSTFCODE,BOWORK1+12                                              
         MVC   SANALOFF,BOWORK1+24                                              
         OI    ZINDS,ZIREDIS+ZIWORPEN                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AMOUNT                                          *         
*                                                                     *         
* NTRY: P1=A(FIELD HEADER)                                            *         
*       P2=A(PL8 OUTPUT)                                              *         
* EXIT: CC=EQUAL IF VALID INPUT                                       *         
*       CC=HIGH, OUTPUT IS A VALID PERCENTAGE, P2=% TO 4DP            *         
*       CC=LOW, IF INVALID INPUT                                      *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   NTR1  ,                                                                
         LM    R1,R2,0(R1)                                                      
         GOTO1 AFVAL                                                            
         BNE   EXITN                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,ZCURBIL+(CURTDECP-CURTABD)                               
         OI    BOBYTE1,X'80'                                                    
         XR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(R3))                    
         CLI   0(R1),FF                                                         
         BE    VALAMT2             INPUT MUST BE STRAIGHT CASH                  
         ZAP   0(8,R2),4(8,R1)                                                  
         B     EXITY                                                            
*                                                                               
VALAMT2  BCTR  R3,0                TEST INPUT IS STRAIGHT PERCENTAGE            
         LA    RE,FVIFLD(R3)                                                    
         CLI   0(RE),C'%'                                                       
         BNE   EXITL                                                            
         GOTO1 VCASHVAL,BODMCB,(X'84',FVIFLD),(R3)                              
         CLI   0(R1),FF                                                         
         BE    EXITL                                                            
         ZAP   0(8,R2),4(8,R1)                                                  
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND PENDING PTA ELEMENT                                 *         
*                                                                     *         
* NTRY: R1=A(PTA ELEMENT TYPE)                                        *         
* EXIT: ZAPTAEL=A(ELEMENT)                                            *         
***********************************************************************         
         SPACE 1                                                                
FINDPTA  NTR1  ,                                                                
         ICM   R2,15,ZAPTAEL       TEST ALREADY HAVE ELEMENT                    
         BZ    FPTA02                                                           
         USING PTAELD,R2                                                        
         CLI   PTAEL,PTAELQ                                                     
         BNE   FPTA02                                                           
         TM    PTASTAT1,PTASPEND                                                
         BZ    FPTA02                                                           
         CLC   PTATYPE,0(R1)                                                    
         BE    FINDPTAY                                                         
*                                                                               
FPTA02   L     R2,ATRNREC                                                       
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING PTAELD,R2                                                        
         XR    RF,RF                                                            
*                                                                               
FPTA04   CLI   PTAEL,0                                                          
         BE    FINDPTAN                                                         
         CLI   PTAEL,PTAELQ                                                     
         BNE   FPTA08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BZ    FPTA08                                                           
         CLC   PTATYPE,0(R1)                                                    
         BE    FPTA10                                                           
FPTA08   IC    RF,PTALN                                                         
         BXH   R2,RF,FPTA04                                                     
*                                                                               
FPTA10   ST    R2,ZAPTAEL                                                       
         XC    ZPTAEL,ZPTAEL                                                    
         IC    RE,PTALN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ZPTAEL(0),PTAELD                                                 
*                                                                               
FINDPTAY MVC   ZPTAELSV,ZPTAEL                                                  
         B     EXITY                                                            
*                                                                               
FINDPTAN XC    ZAPTAEL,ZAPTAEL                                                  
         XC    ZPTAEL,ZPTAEL                                                    
         XC    ZPTAELSV,ZPTAELSV                                                
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE WRITE-OFF EXPENSE ACCOUNT                       *         
***********************************************************************         
         SPACE 1                                                                
VALWEX   NTR1  ,                                                                
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITN                                                            
         LA    RF,X'80'                                                         
         TM    TLXSTAT,TLXSULSI                                                 
         BZ    *+8                                                              
         LA    RF,X'40'(RF)                                                     
         GOTO1 AVALWEX,BOPARM,((RF),0)                                          
         BNE   EXITN                                                            
         MVC   Z.PTAWEXPA,ACCODE+(ACTKUNT-ACTKCPY)                              
*                                                                               
         CLI   CUCTRY,CTRYHOL      TEST HOLLAND                                 
         BNE   VALWEXX                                                          
         CLI   BCBYTE1,0           TEST HAVE TAX TYPE                           
         BNE   VALWEX02                                                         
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',APTAREC),         *        
               (1,=AL1(FFTTTAXI))                                               
         CLI   12(R1),0                                                         
         BE    VALWEXX                                                          
         DC    H'0'                                                             
*                                                                               
VALWEX02 GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('FFTELQ',APTAREC),         *        
               (1,=AL1(FFTTTAXI))                                               
         CLI   12(R1),0            TEST FFTELD ON PTA RECORD                    
         BNE   VALWEX04                                                         
         L     R3,12(R1)                                                        
         USING FFTELD,R3                                                        
         CLC   BCBYTE1,FFTDATA     TEST CHANGE OF CODE                          
         BE    VALWEXX                                                          
         MVC   FFTDATA(1),BCBYTE1                                               
         OI    ZUPDINDS,ZUPDIPTA   ENSURE PTA RECORD IS UPDATED                 
         B     VALWEXX                                                          
*                                                                               
VALWEX04 LA    R3,BOELEM           ADD FFT ELEMENT                              
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA+1-FFTELD                                           
         MVI   FFTTYPE,FFTTTAXI                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,1                                                        
         MVC   FFTDATA(1),BCBYTE1                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APTAREC,FFTELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    ZUPDINDS,ZUPDIPTA   ENSURE PTA RECORD IS UPDATED                 
         DROP  R3                                                               
*                                                                               
VALWEXX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE FIELDS FROM PREVIOUS LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
SAVFLD   NTR1  ,                                                                
         LA    R2,ISTAB                                                         
         USING ISTABD,R2           R2=A(INTER/SCREEN TABLE)                     
         LA    R3,ZISLST                                                        
         USING ISLSTD,R3           R3=A(INTER/SCREEN LIST)                      
*                                                                               
SAVFLD02 CLI   ISTABD,EOT          TEST END-OF-TABLE                            
         BE    SAVFLD10                                                         
         CLC   ISTPSCR,TWASCRN     MATCH ON PREVIOUS/NEXT SCREEN                
         BNE   SAVFLD08                                                         
         CLC   ISTCSCR,CSSCRN                                                   
         BNE   SAVFLD08                                                         
         XR    R1,R1                                                            
         ICM   R1,3,ISTPDSP                                                     
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIELD ON PREVIOUS SCREEN)               
         TM    FHAT,FHATPR         TEST OPEN FIELD                              
         BO    SAVFLD08                                                         
         TM    FHII,FHIIVA         TEST FIELD UNVALIDATED                       
         BO    SAVFLD08                                                         
         MVC   ISLDSP,ISTCDSP      YES - ADD FIELD TO LIST                      
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RF,FHDAD+1                                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LA    RF,FHDAD(RF)                                                     
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   ISLFLD(0),FHDA                                                   
         LA    RE,ISLLNQ+1(RE)                                                  
         STC   RE,ISLLN                                                         
         AR    R3,RE               BUMP R3 TO NEXT LIST ENTRY                   
         DROP  R1                                                               
*                                                                               
SAVFLD08 LA    R2,ISTABL(R2)       BUMP R2 TO NEXT TABLE ENTRY                  
         B     SAVFLD02                                                         
*                                                                               
SAVFLD10 MVI   ISLSTD,EOT          SET END OF LIST                              
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE SAVED FIELDS TO CURRENT SCREEN                              *         
***********************************************************************         
         SPACE 1                                                                
RESFLD   NTR1  ,                                                                
         LA    R3,ZISLST                                                        
         USING ISLSTD,R3           R3=A(INTER-SCREEN LIST)                      
         XR    RF,RF                                                            
*                                                                               
RESFLD02 ICM   RF,1,ISLLN          TEST LAST FIELD DONE                         
         BZ    EXIT                                                             
         XR    R1,R1                                                            
         ICM   R1,3,ISLDSP                                                      
         LA    R1,TWAD(R1)                                                      
         USING FHD,R1              R1=A(FIELD HEADER)                           
         LR    RE,RF                                                            
         SH    RE,=Y(ISLLNQ+1)                                                  
         EX    RE,*+4              COPY FIELD                                   
         MVC   FHDA(0),ISLFLD                                                   
         LA    R2,FHDA(RE)                                                      
         LA    RE,1(RE)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,FHIL                                                          
         NI    FHII,FF-FHIIVA      SET FIELD UNVALIDATED                        
         BXH   R3,RF,RESFLD02      BUMP R3 TO NEXT LIST ENTRY                   
*                                                                               
         DROP  R1,R3                                                            
         EJECT                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
ACCMST   DC    C'ACCMST '                                                       
PZERO    DC    PL8'0'                                                           
EXPUL    DC    C'SE'               EXPENSE UNIT/LEDGER                          
INCUL    DC    C'SI'               INCOME UNIT/LEDGER                           
OFFUL    DC    C'2D'               OFFICE/DEPT. UNIT/LEDGER                     
STFUL    DC    C'2P'               STAFF UNIT/LEDGER                            
SKUL     DC    C'SK'               SK UNIT/LEDGER                               
ORDWC    DC    C'**'               ORDER WORK-CODE                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCREEN TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
SCRTABD  DSECT                                                                  
SCRTDIS  DS    AL2                 DISPLACEMENT TO DISPLAY ROUTINE              
SCRTVAL  DS    AL2                 DISPLACEMENT TO VALIDATE ROUTINE             
SCRTFRST DS    AL2                 DISPLACEMENT TO FIRST FIELD                  
SCRTLAST DS    AL2                 DISPLACEMENT TO LAST FIELD                   
SCRTINDS DS    XL1                 INDICATOR BYTE                               
SCRTIDIS EQU   X'80'               DISPLAY ONLY, NOT FOR VALIDATION             
SCRTIVWO EQU   X'40'               ONLY VALID IF WRITE-OFF PENDING              
SCRTIVXF EQU   X'20'               ONLY VALID IF TRANSFER PENDING               
SCRTIVWR EQU   X'10'               ONLY VALID IF WRITE-OFF RECOVERY             
SCRTIVDS EQU   X'08'               VALIDATE AFTER DISPLAYING                    
SCRTIFVL EQU   X'04'               CALL FVAL BEFORE VALIDATING                  
SCRTI0DS EQU   X'02'               IF FIELD EMPTY RE-DISPLAY                    
SCRTIVPR EQU   X'01'               VALIDATE PROTECTED FIELD                     
SCRTUPD  DS    XL1                 UPDATE INDICATORS (SEE ZUPDINDS)             
SCRTPTA  DS    XL1                 PENDING PTA ELEMENT TYPE REQUIRED            
         DS    XL1                 N/D                                          
SCRTABL  EQU   *-SCRTABD                                                        
         SPACE 1                                                                
CLB03    CSECT                                                                  
SCRZOO   DS    0X                                                               
*                                                                               
         DC    AL2(DISTRN-CLB03,0)                                              
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISUPDRG-CLB03,0)                                            
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISALL-CLB03,VALALL-CLB03)                                   
         DC    AL2(REGALNTH-TWAD,REGALHRH-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(ZUPDITRN+ZUPDIJOB)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWOF-CLB03,VALWOF-CLB03)                                   
         DC    AL2(REGWONTH-TWAD,REGWOHRH-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(ZUPDITRN+ZUPDIJOB+ZUPDIPTA)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWOAC-CLB03,VALWOAC-CLB03)                                 
         DC    AL2(REGWOACH-TWAD,REGWOACH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN+ZUPDIPTA+ZUPDIWOP)                                  
         DC    AL1(PTATWOF)                                                     
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISXFR-CLB03,VALXFR-CLB03)                                   
         DC    AL2(REGXFNTH-TWAD,REGXFHRH-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(ZUPDITRN+ZUPDIJOB)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISXFWC-CLB03,VALXFWC-CLB03)                                 
         DC    AL2(REGXFWCH-TWAD,REGXFWCH-TWAD)                                 
         DC    AL1(SCRTIVXF+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(PTATTRFT)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISXFAC-CLB03,VALXFAC-CLB03)                                 
         DC    AL2(REGXFACH-TWAD,REGXFACH-TWAD)                                 
         DC    AL1(SCRTIVXF+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(PTATTRFT)                                                    
         DC    AL1(0)                                                           
*&&US                                                                           
         DC    AL2(DISXFCM-CLB03,VALXFCM-CLB03)                                 
         DC    AL2(REGXFCMH-TWAD,REGXFCMH-TWAD)                                 
         DC    AL1(SCRTIVXF+SCRTIFVL+SCRTI0DS+SCRTIVPR)                         
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(PTATTRFT)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISXFSK-CLB03,VALXFSK-CLB03)                                 
         DC    AL2(REGXFSKH-TWAD,REGXFSKH-TWAD)                                 
         DC    AL1(SCRTIVXF+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(PTATTRFT)                                                    
         DC    AL1(0)                                                           
*&&                                                                             
*&&UK                                                                           
         DC    AL2(DISNAR-CLB03,VALNAR-CLB03)                                   
         DC    AL2(REGNAR1H-TWAD,REGNAR2H-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    AL2(DISNAR-CLB03,VALNAR-CLB03)                                   
         DC    AL2(REGNAR1H-TWAD,REGNAR2H-TWAD)                                 
         DC    AL1(0) ?? AL1(SCRTIVWO)                                          
         DC    AL1(0) ?? AL1(ZUPDIPTA)                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
SCRZOOX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
SCRZWO   DS    0X                                                               
*                                                                               
         DC    AL2(DISTRN-CLB03,0)                                              
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISUPDWO-CLB03,0)                                            
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWAMT-CLB03,VALWAMT-CLB03)                                 
         DC    AL2(WOFNETH-TWAD,WOFHRSH-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(ZUPDITRN+ZUPDIJOB+ZUPDIPTA)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWREF-CLB03,VALWREF-CLB03)                                 
         DC    AL2(WOFREFH-TWAD,WOFREFH-TWAD)                                   
         DC    AL1(SCRTIVWO+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN+ZUPDIPTA)                                           
         DC    AL1(PTATWOF)                                                     
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWDAT-CLB03,VALWDAT-CLB03)                                 
         DC    AL2(WOFDATH-TWAD,WOFDATH-TWAD)                                   
         DC    AL1(SCRTIVWO+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDITRN+ZUPDIPTA)                                           
         DC    AL1(PTATWOF)                                                     
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISEXP-CLB03,VALEXP-CLB03)                                   
         DC    AL2(WOFEXPCH-TWAD,WOFEXPNH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(ZUPDITRN+ZUPDIPTA+ZUPDIWOP)                                  
         DC    AL1(PTATWOF)                                                     
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISDPT-CLB03,VALDPT-CLB03)                                   
         DC    AL2(WOFDPTCH-TWAD,WOFDPTNH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIFVL)                                           
         DC    AL1(ZUPDIPTA+ZUPDIWOP)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISSTF-CLB03,VALSTF-CLB03)                                   
         DC    AL2(WOFSTFCH-TWAD,WOFSTFNH-TWAD)                                 
*&&US*&& DC    AL1(SCRTIVWO+SCRTIVDS+SCRTIFVL)                                  
*&&UK*&& DC    AL1(SCRTIVWO+SCRTIFVL)                                           
         DC    AL1(ZUPDIPTA+ZUPDIWOP)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWOA-CLB03,VALWOA-CLB03)                                   
         DC    AL2(WOFWOACH-TWAD,WOFWOANH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(ZUPDITRN+ZUPDIPTA)                                           
         DC    AL1(PTATWOF)                                                     
         DC    AL1(0)                                                           
*&&UK                                                                           
         DC    AL2(DISOFF-CLB03,VALOFF-CLB03)                                   
         DC    AL2(WOFOFFCH-TWAD,WOFOFFNH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIVDS+SCRTIFVL+SCRTI0DS+SCRTIVPR)                
         DC    AL1(ZUPDIPTA+ZUPDIWOP)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    AL2(DISOFF-CLB03,VALOFF-CLB03)                                   
         DC    AL2(WOFOFFCH-TWAD,WOFOFFCH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(ZUPDIPTA+ZUPDIWOP)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISANAL-CLB03,VALANAL-CLB03)                                 
         DC    AL2(WOFANALH-TWAD,WOFANALH-TWAD)                                 
         DC    AL1(SCRTIVWO+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(ZUPDIPTA+ZUPDIWOP)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
         DC    AL2(DISWNAR-CLB03,VALWNAR-CLB03)                                 
         DC    AL2(WOFNAR1H-TWAD,WOFNAR2H-TWAD)                                 
         DC    AL1(SCRTIVWO)                                                    
         DC    AL1(ZUPDIPTA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
SCRZWOX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
SCRZWR   DS    0X                                                               
*                                                                               
         DC    AL2(DISTRN-CLB03,0)                                              
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISUPDWO-CLB03,0)                                            
         DC    AL2(0,0)                                                         
         DC    AL1(SCRTIDIS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISRSEQ-CLB03,VALRSEQ-CLB03)                                 
         DC    AL2(WOFSEQH-TWAD,WOFSEQH-TWAD)                                   
         DC    AL1(SCRTIFVL)                                                    
         DC    AL1(ZUPDITRN)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWREF-CLB03,0)                                             
         DC    AL2(WOFREFH-TWAD,WOFREFH-TWAD)                                   
         DC    AL1(SCRTIVWR+SCRTIFVL)                                           
         DC    AL1(0)                                                           
         DC    AL1(PTATWOFR)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWDAT-CLB03,0)                                             
         DC    AL2(WOFDATH-TWAD,WOFDATH-TWAD)                                   
         DC    AL1(SCRTIVWR+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(0)                                                           
         DC    AL1(PTATWOFR)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISEXP-CLB03,VALEXP-CLB03)                                   
         DC    AL2(WOFEXPCH-TWAD,WOFEXPNH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(0)                                                           
         DC    AL1(PTATWOFR)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISDPT-CLB03,VALDPT-CLB03)                                   
         DC    AL2(WOFDPTCH-TWAD,WOFDPTNH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIFVL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISSTF-CLB03,VALSTF-CLB03)                                   
         DC    AL2(WOFSTFCH-TWAD,WOFSTFNH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIVDS+SCRTIFVL)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISWOA-CLB03,VALWOA-CLB03)                                   
         DC    AL2(WOFWOACH-TWAD,WOFWOANH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(ZUPDITRN+ZUPDIPTA)                                           
         DC    AL1(PTATWOFR)                                                    
         DC    AL1(0)                                                           
*&&UK                                                                           
         DC    AL2(DISOFF-CLB03,VALOFF-CLB03)                                   
         DC    AL2(WOFOFFCH-TWAD,WOFOFFNH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIVDS+SCRTIFVL+SCRTI0DS+SCRTIVPR)                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    AL2(DISOFF-CLB03,0)                                              
         DC    AL2(WOFOFFCH-TWAD,WOFOFFCH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIFVL+SCRTI0DS)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(DISANAL-CLB03,VALANAL-CLB03)                                 
         DC    AL2(WOFANALH-TWAD,WOFANALH-TWAD)                                 
         DC    AL1(SCRTIVWR+SCRTIVDS+SCRTIFVL+SCRTI0DS)                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
         DC    AL2(DISWNAR-CLB03,VALWNAR-CLB03)                                 
         DC    AL2(WOFNAR1H-TWAD,WOFNAR2H-TWAD)                                 
         DC    AL1(SCRTIVWR)                                                    
         DC    AL1(ZUPDIPTA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
SCRZWRX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PROTECTION TABLE                                                    *         
***********************************************************************         
         SPACE 1                                                                
PROTTABD DSECT                                                                  
PROTCOND DS    XL2                 PROTECT FIELDS BECAUSE ....                  
PROTORD  EQU   X'8000'             ..... IS AN ORDER                            
PROTEXT  EQU   X'4000'             ..... IS AN EXTRA BILLING ITEM               
PROTREC  EQU   X'2000'             ..... IS THE ZOOMWR SCREEN                   
PROTREVP EQU   X'1000'             ..... REVERSAL IS PENDING                    
PROTNOHR EQU   X'0800'             ..... NO HOURS TO ALLOCATE                   
PROTFCON EQU   X'0400'             ..... FCONTROL RECORD NOT SET TO W           
PROTNOCM EQU   X'0200'             ..... ITEM IS NON-COMMISSIONABLE             
PROTHOUR EQU   X'0100'             ..... ITEM IS ON TIME                        
PROTNONC EQU   X'0080'             ..... BILLING TYPE IS NON-CLIENT             
PROTCUR  EQU   X'0040'             ..... BILLING CURRENCY NOT AS ITEM           
PROTUNA  EQU   X'0020'             ..... CANNOT MARK UNAUTH ITEM                
PROTWLST EQU   X'0010'             ..... ZOOMED IN FROM WRONG LIST              
PROTRVAL EQU   X'0008'             ..... ITEM REQUIRES REVALUING                
PROTFRST DS    XL2                 DISPLACEMENT TO FIRST FIELD                  
PROTLAST DS    XL2                 DISPLACEMENT TO LAST FIELD                   
PROTTABL EQU   *-PROTTABD                                                       
         SPACE 1                                                                
CLB03    CSECT                                                                  
PROTZOO  DS    0X                  * REGULAR ZOOM *                             
         DC    AL2(PROTREVP+PROTCUR+PROTWLST+PROTRVAL)                          
         DC    AL2(REGALNTH-TWAD,REGNARWH-TWAD)                                 
         DC    AL2(PROTNOCM)                                                    
         DC    AL2(REGALCMH-TWAD,REGALCMH-TWAD)                                 
         DC    AL2(PROTORD+PROTEXT)                                             
         DC    AL2(REGWONTH-TWAD,REGXFACH-TWAD)                                 
         DC    AL2(PROTNOHR)                                                    
         DC    AL2(REGALHRH-TWAD,REGALHRH-TWAD)                                 
         DC    AL2(PROTNOHR)                                                    
         DC    AL2(REGWOHRH-TWAD,REGWOHRH-TWAD)                                 
         DC    AL2(PROTNOHR)                                                    
         DC    AL2(REGXFHRH-TWAD,REGXFHRH-TWAD)                                 
*&&US*&& DC    AL2(PROTHOUR)                                                    
*&&US*&& DC    AL2(REGXFCMH-TWAD,REGXFCMH-TWAD)                                 
         DC    AL2(PROTFCON)                                                    
         DC    AL2(REGNAR1H-TWAD,REGNAR2H-TWAD)                                 
         DC    AL2(PROTNONC+PROTUNA)                                            
         DC    AL2(REGALNTH-TWAD,REGALHRH-TWAD)                                 
PRTZOOX  DS    AL2(EOT)                                                         
*                                                                               
PROTZWO  DS    0X                  * ZOOM WRITE-OFF *                           
         DC    AL2(PROTNOHR)                                                    
         DC    AL2(WOFHRSH-TWAD,WOFHRSH-TWAD)                                   
         DC    AL2(PROTORD+PROTEXT+PROTREVP+PROTCUR+PROTWLST+PROTRVAL)          
         DC    AL2(WOFNETH-TWAD,WOFNAR2H-TWAD)                                  
PROTZWOX DS    AL2(EOT)                                                         
*                                                                               
PROTZWR  DS    0X                  * ZOOM WRITE-OFF RECOVERY *                  
         DC    AL2(PROTREC)                                                     
*&&UK*&& DC    AL2(WOFNETH-TWAD,WOFOFFCH-TWAD)                                  
*&&US*&& DC    AL2(WOFNETH-TWAD,WOFANALH-TWAD)                                  
         DC    AL2(PROTNOHR)                                                    
         DC    AL2(WOFHRSH-TWAD,WOFHRSH-TWAD)                                   
         DC    AL2(PROTORD+PROTEXT+PROTREVP+PROTCUR+PROTWLST)                   
         DC    AL2(WOFNETH-TWAD,WOFNAR2H-TWAD)                                  
PROTZWRX DS    AL2(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INTER-SCREEN TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
ISTABD   DSECT                                                                  
ISTPSCR  DS    XL1                 PREVIOUS SCREEN                              
ISTPDSP  DS    XL2                 PREVIOUS DISPLACEMENT TO FIELD               
ISTCSCR  DS    XL1                 CURRENT SCREEN                               
ISTCDSP  DS    XL2                 CURRENT DISPLACEMENT TO FIELD                
ISTABL   EQU   *-ISTABD                                                         
         SPACE 1                                                                
ISLSTD   DSECT                     SAVED FIELD LIST                             
ISLLN    DS    XL1                 LENGTH                                       
ISLDSP   DS    XL2                 DISPLACEMNET TO FIELD                        
ISLLNQ   EQU   *-ISLSTD            LENGTH OF FIXED PART                         
ISLFLD   DS    0C                  FIELD                                        
         SPACE 1                                                                
CLB03    CSECT                                                                  
ISTAB    DS    0X                                                               
*                                  WRITE-OFF NET AMOUNT                         
         DC    AL1(S#BILZRG),AL2(REGWONTH-TWAD)                                 
         DC    AL1(S#BILZWO),AL2(WOFNETH-TWAD)                                  
*                                  WRITE-OFF HOURS                              
         DC    AL1(S#BILZRG),AL2(REGWOHRH-TWAD)                                 
         DC    AL1(S#BILZWO),AL2(WOFHRSH-TWAD)                                  
*                                  WRITE-OFF DETAIL                             
         DC    AL1(S#BILZRG),AL2(REGWOACH-TWAD)                                 
         DC    AL1(S#BILZWO),AL2(WOFEXPCH-TWAD)                                 
*                                                                               
ISTABX   DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TYPE ROUTINE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPRTN   DS    XL2                 DISPLACEMENT TO ROUTNE                       
TYPDICT  DS    XL2                 DICTIONARY REFERENCE NUMBER                  
TYPINDS  DS    XL1                 TYPINDS FILTER                               
TYPIFC   EQU   X'80'               FOREIGN CURRENCY BILLING                     
TYPSTAT1 DS    XL1                 TLXSTAT FILTER                               
TYPTABL  EQU   *-TYPTABD                                                        
         SPACE 1                                                                
CLB03    CSECT                                                                  
TYPTAB   DS    0X                                                               
*                                                                               
         DC    AL2(TYPTYP-CLB03,AC#TYPE),AL1(0,0)                               
         DC    AL2(TYPNET-CLB03,AC#NET),AL1(0,0)                                
         DC    AL2(TYPORD-CLB03,AC#ORDER),AL1(0,TLXSCOST)                       
         DC    AL2(TYPHRS-CLB03,AC#HOURS),AL1(0,TLXSHOUR)                       
*&&UK*&& DC    AL2(TYPCOST-CLB03,AC#COST),AL1(0,TLXSHOUR)                       
*&&UK*&& DC    AL2(TYPCHRG-CLB03,AC#CRG),AL1(0,TLXSHOUR)                        
*&&US*&& DC    AL2(TYPCHRG-CLB03,AC#RATE),AL1(0,TLXSHOUR)                       
*&&UK*&& DC    AL2(TYPOWC-CLB03,AC#ORDWC),AL1(0,TLXSORD)                        
         DC    AL2(TYPCUR-CLB03,AC#CURRY),AL1(TYPIFC,0)                         
*                                                                               
TYPTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ZOOM OVERLAY W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
ZWORKD   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6A                                                               
WORK     DS    XL100                                                            
*                                                                               
ZAPTAEL  DS    A                   A(PENDING PTA ELEMENT) (FINDPTA)             
ZPTAEL   DS    XL255               PTA ELEMENT (FINDPTA)                        
ZPTAELSV DS    XL255               SAVED PTA ELEMENT (FINDPTA)                  
*                                                                               
ZNTRSES  DS    XL1                 NTRSES RETURN CODE                           
ZINDS    DS    XL1                 * GENERAL INDICATORS *                       
ZIDIS    EQU   X'80'               DISPLAY ONLY MODE                            
ZIAGYCUR EQU   X'40'               BILLING IN AGENCY CURRENCY                   
ZICOMPER EQU   X'20'               COMMISION ENTERED AS A PERCENTAGE            
ZIREDIS  EQU   X'10'               RE-DISPLAY, NOT VAL. REST OF SCREEN          
ZIVALDIS EQU   X'08'               VALIDATING FIELD AFTER DISPLAYING IT         
ZIJOBACT EQU   X'04'               ACTIVITY FOUND ON JOB RECORD                 
ZIWORPEN EQU   X'02'               WRITE-OFF RECOVERY IS PENDING                
ZIGOTPTA EQU   X'01'               GOT PTA RECORD                               
*                                                                               
ZUPDINDS DS    XL1                 * UPDATE INDICATORS *                        
ZUPDITRN EQU   X'80'               UPDATE TRANSACTION RECORD                    
ZUPDIPTA EQU   X'40'               UPDATE PTA RECORD                            
ZUPDIWOP EQU   X'20'               UPDATE W/O POSTINGS ON PTA RECORD            
ZUPDIJOB EQU   X'10'               UPDATE JOB RECORD                            
*                                                                               
ZTRNRSTA DS    XL(L'TRNRSTA)       SAVED RECORD STATUS                          
*                                                                               
ZBILCUR  DS    XL(L'CSBILCUR)      CURRENCY CODE                                
ZCURBIL  DS    XL(CURTABL)         CURRENCY TABLE ENTRY                         
*                                                                               
ZISLST   DS    XL512               INTER-SCREEN FIELD LIST                      
*                                                                               
UDLST#   DS    H                   NO. OF UDLST ENTRIES (SAVED IN IO1)          
*                                                                               
ZWORKL   EQU   *-ZWORKD                                                         
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
         PRINT OFF                                                              
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                                                                  
IOTRN    EQU   IO4                                                              
         ORG   AIO4                                                             
ATRNREC  DS    A                   A(TRANSACTION RECORD)                        
         ORG   AIO5                                                             
APTAREC  DS    A                   A(PTA RECORD)                                
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBFCD                                                       
         ORG   REGOLAYH                                                         
       ++INCLUDE ACCLBF6D                                                       
         ORG   REGOLAYH                                                         
       ++INCLUDE ACCLBF8D                                                       
         SPACE 1                                                                
         ORG   OSVALS              ** SAVED STORAGE **                          
SINP1    DS    H                   DISPLACEMENT TO FIRST INPUT FIELD            
SEXBSTAT DS    XL1                 ACBSTAT FOR EXPENSE ACCOUNT                  
SDPTCODE DS    CL12                DEPARTMENT CODE                              
SSTFCODE DS    CL12                STAFF CODE                                   
SANALOFF DS    CL2                 ANALYSIS OFFICE CODE (US ONLY)               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACCLB03   08/16/00'                                      
         END                                                                    
