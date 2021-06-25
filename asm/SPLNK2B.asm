*          DATA SET SPLNK2B    AT LEVEL 010 AS OF 09/12/17                      
*&&      SET   NOP=N,WT=N                                                       
*PHASE T21E2BA                                                                  
SPLNK2B  TITLE 'CANADIAN PINERGY UPLOAD'                                        
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,CODE=CODE,RLEN=2000,REQUEST=*,WORKERKEY=SPCP,   +        
               LINKIO=Y,SYSTEM=SPTSYSQ,                                +        
               FILES=FILES,SERVERTYPE=TSTSPUP,SYSPHASE=SYSPHASE,IDF=Y, +        
               BLOCKS=(B#SAVED,SAVED,B#WORKD,WORKD,B#BDEL,$BDD),       +        
               BLOCKS2=(B#CLTREC,CLTHDR)                                        
*                                                                               
SE#MAXLN EQU   1276                                                             
SE#CKSUM EQU   1277                                                             
SE#STLCK EQU   1372                STATION IS LOCKED                            
*                                                                               
CODE     NMOD1 0,**SL2B**,RR=RE                                                 
         LR    R5,R1               R5=A(LP_D)                                   
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFF-LINE                                
         BNZ   CODE02                                                           
         L     R9,LP_ABLK1                                                      
         ICM   R8,15,RSVRSAVE      R8=A(SAVE AREA)                              
         B     CODE04                                                           
*                                                                               
CODE02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(SAVE W/S)                               
         MVC   MASTC,RMASTC        SET A(MASTC)                                 
*                                                                               
CODE04   ST    R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
*                                                                               
         MVC   MAPNUM,LP_QMAPN     EXTRACT MAP NUMBER                           
         MVC   AGY,LP_AGY          EXTRACT AGENCY                               
         MVC   VERSION,LP_VRSN     EXTRACT PC VERSION                           
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         CLI   RUNPMODE,RRUNSTRQ   TEST 'FIRST' MODE                            
         JE    FIRST                                                            
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JE    COMMIT                                                           
         B     EXITY                                                            
***********************************************************************         
* Handle RUNNNER's 'First for run' (once only off-line) mode                    
***********************************************************************         
FIRST    DS    0H                                                               
*&&WT*&& WTO   '*TEST* SL2B FIRST mode'                                         
         LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEDX-SAVED                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,LP_ACOM                                                       
         MVC   LINKIO,CLINKIO-COMFACSD(RE)                                      
         MVC   RECUP,CRECUP-COMFACSD(RE)                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFF-LINE                                
         JZ    FIRSTX                                                           
         L     RF,LP_ACOM          LOAD SUBSIDIARY PHASES & INITIALISE          
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
*****                                                                           
         GOTOR (RF),DMCB,0,X'D9000A2A'  Get address of BUYVAL here              
         MVC   SPBYVAL,0(R1)            eliminates msgs loading BUYVAL          
*****                                                                           
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
*                                                                               
         LA    R0,LP_D                                                          
         ST    R0,ALP              SET A(LP_D) IN GLOBAL W/S                    
         MVC   ACOMFACS,LP_ACOM    SET A(COMFACS)                               
         MVC   ATWA,LP_ATWA        SET A(TWA)                                   
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
*                                                                               
         MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
*                                                                               
FIRSTX   DS    0H                                                               
         J     EXITY                                                            
*                                                                               
FIRSTDIE DC    H'0'                                                             
***********************************************************************         
* Handle 'First for request' (before first upload record) mode                  
***********************************************************************         
INIT     DS    0H                                                               
*&&WT*&& WTO   '*TEST* SL2B INIT mode'                                          
         CLC   STAMP,STAMPLIT      TEST CORRECT SAVE STORAGE STAMP              
         BE    INIT10                                                           
         MVC   DUB(4),SVMED        SAVE MEDIA/CLIENT ALPHA                      
*                                                                               
         MVC   SVMED(4),DUB        RESTORE MEDIA/CLIENT ALPHA                   
         MVC   STAMP,STAMPLIT                                                   
*                                                                               
INIT10   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RE,ALIOB                                                         
         USING LIOBD,RE                                                         
         OI    LIOBINDS,LIOBIMLT   SET MULTIPLE OUTPUT RECORDS                  
         DROP  RE                                                               
*                                                                               
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVI   ERRORFLG,0                                                       
                                                                                
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,LP_AGY                                                    
         MVC   GBYCOMF,ACOMFACS                                                 
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         MVC   SAVE1OR2,GBY1OR2                                                 
         MVC   SV1OR2,GBY1OR2                                                   
*                                                                               
         GOTOR BUFFER,DMCB,('TOKPTRQ',TSAINI),('TPBKEYL',TPBRECL)               
         JNE   FIRSTDIE                                                         
         MVI   MISCFLG1,MF1TSRVP   TSAR SETUP TO VALIDATE POINTERS              
         MVI   VACTEFLG,C'N'         NO ACTION ERRORS                           
         XC    SVCLT,SVCLT           NO SAVED CLIENT YET                        
         XC    PDDSPTR,PDDSPTR       No previous DDSPTR                         
         XC    CPYCOUNT,CPYCOUNT     Clear copy action counter                  
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFF-LINE                                
         JZ    INIT20                                                           
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0    CLEAR XBUFFS               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
*                                                                               
INIT20   DS    0H                                                               
*                                                                               
         B     EXITY                                                            
         DROP  R6,R7                                                            
         EJECT                                                                  
*==========================================================                     
* PROCESS INPUT OF AN UPLOAD RECORD                                             
*==========================================================                     
INPUT    DS    0H                                                               
*&&WT*&& WTO   '*TEST* SL2B INPUT mode'                                         
         LA    RE,RECTAB                                                        
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   0(2,RE),MAPNUM     LOOK UP RECORD MAP CODE IN TABLE              
         BE    INPUT20                                                          
         AHI   RE,L'RECTAB                                                      
         BCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  DS    0H                                                               
         CLI   5(RE),X'FF'          CLEAR ALL ERRORS?                           
         BE    INPUT30               YES                                        
*                                                                               
         TM    5(RE),X'80'          CLEAR LOW LEVEL ERRORS?                     
         BZ    INPUT40                                                          
         CLI   ERRORFLG,FATALERR    FATAL ERROR, DON'T CLEAR                    
         BE    INPUT40                                                          
INPUT30  MVI   ERRORFLG,0                                                       
*                                                                               
INPUT40  CLI   ERRORFLG,0          HAVE ANY ERRORS?                             
         BNE   EXITY               YES                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,2(RE)                                                       
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR          SET A(PROCESSING ROUTINE)                    
*                                                                               
         MVC   XTRATEXT,SPACES     INITIALIZE EXTRA MESSAGE TEXT                
         GOTOR RECADDR                                                          
         B     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
*===============================================================                
* PINERGY CHANGE ACTION       RECORD MAP 2A3                                    
*===============================================================                
PNGCHG   NTR1  BASE=*,LABEL=*                                                   
         CLI   VPTREFLG,C'Y'         WE HAVE A DDSPTR ERROR?                    
         JE    PCHGVPER                                                         
*                                                                               
PNGCHG10 TM    MISCFLG1,MF1TSRVP     TSAR SETUP TO VALIDATE POINTERS?           
         JNZ   PCHGTSRA              YES, SET IT UP TO VALIDATE ACTIONS         
*                                                                               
PNGCHG20 L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         TM    MISCFLG1,MF1NDTLR     Action in process? Need a trailer          
         JNZ   PCHGEP01              Can't start another action                 
         OI    MISCFLG1,MF1NDTLR     Now we need a trailer                      
*                                                                               
         XC    ASPCOUNT,ASPCOUNT     Clear add spot counter                     
         L     R6,AIO8                                                          
         USING TACKEY,R6                                                        
         MVC   TACRTOKN,QTOKEN                                                  
         GOTOR REFRMPTR              MODIFY input DDSPTR                        
*                                                                               
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
         GOTOR VHEXIN,DMCB,DDSPMFKY,TACKMFKY,L'DDSPMFKY                         
         DROP  R2                                                               
         MVI   TACKCHCP,C'B'         Buy change                                 
         MVI   SVBUYACT,C'B'                                                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'BUYKEY),TACKMFKY                                         
         MVC   HALF,IOKEY+BUYKBUY-BUYKEY    ADJUST BYLN # FOR SPTD              
         MVI   IOKEY+BUYKBUY-BUYKEY,0                                           
         MVC   IOKEY+BUYKBUY-BUYKEY+1(2),HALF                                   
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         JNE   PCHGEB01                                                         
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   PCHGEB01                                                         
*                                                                               
         L     R3,AIO5                                                          
         USING BUYKEY,R3                                                        
         XC    IOBRDLST,IOBRDLST                                                
         BRAS  RE,BLDXPRD                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,7,BDCOST                                                      
         TM    BDCIND2,BDCNBRDQ      x'10' - buy in dollars?                    
         JZ    PNGCHG30              No                                         
         MHI   R1,100                Adjust to be in pennies                    
PNGCHG30 STCM  R1,15,SVBUCOST        Keep this to valid cost ovr                
         DROP  R3                                                               
*                                                                               
PNGCHG90 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PNGCHGH0                                                         
*                                                                               
         MVC   WTOTXT1,=70C' '                                                  
         MVC   WTOTXT1(19),=CL19'*TEST* SL2B CHG ERROR '                        
         EDIT  (B2,TACERRCD),(4,WTOTXT1+19),FILL=0                              
         MVC   WTOTXT1+40(L'QTOKEN),QTOKEN                                      
         MVC   WTOTXT2,=70C' '                                                  
         MVC   WTOTXT2(70),QDDSPTR                                              
*&&WT*&& WTO   TEXT=((MSGSTARL,D),(WTOTXT1L,D),(WTOTXT2L,D),(0,E)),    +        
               DESC=2                                                           
         B     EXIT                                                             
*                                                                               
PCHGVPER BRAS  RE,OUTVPER            OUTPUT VALPTR ERRORS                       
         J     PNGCHG10              NO GO BACK AND VALIDATE ACTIONS            
*                                                                               
PCHGTSRA BRAS  RE,SETTSARA           SETUP TSAR TO VALIDATE ACTIONS             
         J     PNGCHG20                                                         
*                                                                               
PCHGEB01 MVC   TACERRCD,=AL2(01)     BUY DOES NOT EXIST                         
         MVI   TACERRCT,C'B'                                                    
         J     PCHGERR                                                          
*                                                                               
PCHGEP01 MVC   TACERRCD,=AL2(01)     NEED A TLR FOR PREVIOUS ACTION             
         MVI   TACERRCT,C'P'                                                    
*                                                                               
PCHGERR  MVI   VACTEFLG,C'Y'              WE GOT AN ERROR FOR DDSPTR            
         J     PNGCHG90                                                         
*                                                                               
PNGCHGX  B     EXIT                                                             
PNGCHGH0 DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* Set flags in XPRDLIST for all existing products                               
*                                                                               
* ON EXIT:                                                                      
* XPRDLIST+0 will contain the count of unique products in this buyline          
***********************************************************************         
                                                                                
BLDXPRD  NTR1  BASE=*,LABEL=*                                                   
         XC    XPRDLIST,XPRDLIST                                                
         L     R3,AIO5                                                          
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
BLDX10   BRAS  RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         CLI   1(R3),10            Unallocated?                                 
         BNH   BLDX10              Yes, no product codes then                   
*                                                                               
         LLC   RF,10(R3)           Get 1st product allocation                   
         LA    RF,XPRDLIST(RF)                                                  
         CLI   0(RF),0             We got this one already?                     
         BNE   BLDX20              Yes, maybe we don't have the piggy           
         MVC   0(1,RF),10(R3)      No, store the binary in its location         
         LLC   RE,XPRDLIST         Bump # of unique products in buyline         
         AHI   RE,1                                                             
         STC   RE,XPRDLIST                                                      
*                                                                               
BLDX20   CLI   1(R3),14            Do we have a piggyback?                      
         BNH   BLDX10              No                                           
*                                                                               
         LLC   RF,14(R3)           Get 2nd allocation                           
         LA    RF,XPRDLIST(RF)                                                  
         CLI   0(RF),0             We got this one already?                     
         BNE   BLDX10              Yes, next spot element                       
         MVC   0(1,RF),14(R3)      No, store the binary in its location         
         LLC   RE,XPRDLIST         Bump # of unique products in buyline         
         AHI   RE,1                                                             
         STC   RE,XPRDLIST                                                      
         B     BLDX10                                                           
         EJECT                                                                  
***********************************************************************         
* ADD PRODUCT TO PRODUCT LIST FOR BUY                                           
* NTRY:- R1=A(PRODUCT NUMBER)                                                   
***********************************************************************         
ADDPRD   NTR1  BASE=*,LABEL=*                                                   
         LLC   RF,0(R1)                                                         
         LA    RF,XPRDLIST(RF)                                                  
         CLI   0(RF),0                                                          
         JNE   ADDPRDY                                                          
         MVC   0(1,RF),0(R1)       Put it in XPRDLIST now!                      
         LLC   RF,XPRDLIST         Bump count of unique prds in buyline         
         AHI   RF,1                                                             
         STC   RF,XPRDLIST                                                      
*                                                                               
         LA    RF,IOBRDLST                                                      
         LHI   R0,L'IOBRDLST-1                                                  
*                                                                               
ADDPRD10 CLI   0(RF),0                                                          
         JE    ADDPRD20                                                         
         CLC   0(1,RF),0(R1)                                                    
         JE    ADDPRDY                                                          
         AHI   RF,1                                                             
         BRCT  R0,ADDPRD10                                                      
         J     ADDPRDN             Too many products in one go!!                
*                                                                               
ADDPRD20 MVC   0(1,RF),0(R1)                                                    
ADDPRDY  J     EXITY                                                            
*                                                                               
ADDPRDN  J     EXITN                                                            
         EJECT                                                                  
*===============================================================                
* COMMON SECTION WHERE WE HAVE A DDSPTR ERROR                                   
* THIS IS WHERE IT'LL GO THROUGH THE TSAR RECORDS AND                           
* CALL LINKIO TO BUILD ERROR RETURN BASED ON THOSE TSAR RECORDS MARKED          
* AS IN ERROR                                                                   
*===============================================================                
OUTVPER  NTR1  BASE=*,LABEL=*                                                   
*&&WT*&& WTO   '*TEST* SL2B OUTVPER'                                            
*                                                                               
         LARL  R1,VPTERR                                                        
         ST    R1,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY                                                            
*                                                                               
VPTERR   LKOUT H                                                                
VPTER1   LKOUT R,X'0001'                                                        
Array    LKOUT C,X'0001',(A,ARYVPE)                                             
         LKOUT E                                                                
         LKOUT X                                                                
***********************************************************************         
* Array defintion for ValPtr error response                                     
***********************************************************************         
                                                                                
ARYVPE   LKOUT A,(R,NXTVPE),MULTIROW=Y,ROWNAME=TOKPTRD                          
Token    LKOUT C,1,TPBTOKN,CHAR                                                 
ErrCod   LKOUT C,2,TPBERRCD,UBIN                                                
ErrCat   LKOUT C,3,TPBERRCT,CHAR                                                
         LKOUT E                                                                
**********************************************************************          
* Goes through all the TSAR records that were marked with errors and            
* sends them to output                                                          
**********************************************************************          
NXTVPE   DS    0H                                                               
         L     R4,AIO8                                                          
         ST    R4,LP_ADATA                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST     Test first time?                           
         JE    NXVPE1ST              Yes, it is                                 
NXVPENXT GOTOR BUFFER,DMCB,('TOKPTRQ',TSANXT),0                                 
NXVPE010 TM    BUFFRET,TSEEOF        Anymore entries?                           
         JNZ   NXVPEEOF              No more                                    
NXTVPEX  J     EXITY                                                            
*                                                                               
NXVPEEOF MVI   VPTREFLG,C'N'         DON'T NEED TO OUTPUT THESE NOW             
         OI    MISCFLG1,MF1VPTER     We had VALPTR errors                       
         J     NOMORE                                                           
*                                                                               
NXVPE1ST GOTOR BUFFER,DMCB,('TOKPTRQ',TSAGET),1                                 
         J     NXVPE010                                                         
         EJECT                                                                  
*===============================================================                
* CLOSES THE 1ST TSAR BUFFER USED TO VALIDATE POINTERS                          
* STARTS A NEW BUFFER TO VALIDATE ACTIONS                                       
*===============================================================                
SETTSARA NTR1  BASE=*,LABEL=*                                                   
         GOTOR BUFFER,DMCB,('TOKACTQ',TSAINI),('TACKEYL',TACRECL)               
         JNE   STSRDIE                                                          
*                                                                               
STSRAX   NI    MISCFLG1,X'FF'-MF1TSRVP  TSAR NO LONGER VALIDATING PTRS          
         OI    MISCFLG1,MF1TSRVA     TSAR SETUP TO VALIDATE ACTIONS NOW         
         MVI   VACTEFLG,C'N'         NO ACTION ERRORS                           
         B     EXIT                                                             
STSRDIE  DC    H'0'                                                             
         EJECT                                                                  
*===============================================================                
* PINERGY COPY ACTION         RECORD MAP 2A4                                    
*===============================================================                
PNGCPY   NTR1  BASE=*,LABEL=*                                                   
         CLI   VPTREFLG,C'Y'         WE HAVE A DDSPTR ERROR?                    
         JE    PCPYVPER                                                         
PNGCPY10 TM    MISCFLG1,MF1TSRVP     TSAR SETUP TO VALIDATE POINTERS?           
         JNZ   PCPYTSRA              YES, SET IT UP TO VALIDATE ACTIONS         
*                                                                               
PNGCPY20 L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         TM    MISCFLG1,MF1NDTLR     Action in progress, need a trailer         
         JNZ   PCPYEP01              Yes, Can't start another action            
         OI    MISCFLG1,MF1NDTLR     No, now we need a trailer                  
*                                                                               
         XC    ASPCOUNT,ASPCOUNT     Clear add spot counter                     
         L     R6,AIO8                                                          
         USING TACKEY,R6                                                        
         MVC   TACRTOKN,QTOKEN                                                  
         GOTOR REFRMPTR              MODIFY input DDSPTR                        
*                                                                               
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
         GOTO1 VHEXIN,DMCB,DDSPMFKY,TACKMFKY,L'DDSPMFKY                         
         DROP  R2                                                               
         MVI   TACKCHCP,C'C'         Copy action                                
         MVI   SVBUYACT,C'C'                                                    
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,CPYCOUNT         Bump number of Copy Actions                
         LA    RE,1(RE)                                                         
         STCM  RE,3,CPYCOUNT                                                    
         MVC   TACKCPSQ,CPYCOUNT     New copy action                            
*                                                                               
         MVI   TACKIND,0             Indicate this is not used                  
         MVC   TACKNSLN,BUSEC        New spot length                            
         MVC   SVBUSEC,BUSEC         Save new spot length                       
         MVC   TACKNCST,BUCOST       New buyline rate                           
         MVC   SVBUCOST,BUCOST       Save new buyline rate                      
*                                                                               
         BRAS  RE,VALSLN             Validate the spot length                   
         JNE   PCPYEB16                                                         
*                                                                               
         XC    IOBRDLST,IOBRDLST                                                
         XC    XPRDLIST,XPRDLIST                                                
*                                                                               
         CLC   PRVMCPSE,TACKMFKY     SAME MED,CLT,PRD,STA,EST?                  
         BNE   PCPYNXBL              NO, FIND NEXT POSSIBLE BUYLINE #           
PNGCPY50 LLH   R1,MCPSEBLN                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,MCPSEBLN                                                    
         CHI   R1,MXBUYLIN           MAX BUY LINE # SURPASSED?                  
         BH    PCPYEB13              YES                                        
*                                                                               
PNGCPY90 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PCPYDCH0                                                         
*                                                                               
         MVC   WTOTXT1,=70C' '                                                  
         MVC   WTOTXT1(19),=CL19'*TEST* SL2B CPY ERROR '                        
         EDIT  (B2,TACERRCD),(4,WTOTXT1+19),FILL=0                              
         MVC   WTOTXT1+40(L'QTOKEN),QTOKEN                                      
         MVC   WTOTXT2,=70C' '                                                  
         MVC   WTOTXT2(70),QDDSPTR                                              
*&&WT*&& WTO   TEXT=((MSGSTARL,D),(WTOTXT1L,D),(WTOTXT2L,D),(0,E)),    +        
               DESC=2                                                           
         J     EXIT                                                             
*                                                                               
PCPYVPER BRAS  RE,OUTVPER            OUTPUT VALPTR ERRORS                       
         J     PNGCPY10              NO GO BACK AND VALIDATE ACTIONS            
*                                                                               
PCPYTSRA BRAS  RE,SETTSARA           SETUP TSAR TO VALIDATE ACTIONS             
         XC    PRVMCPSE,PRVMCPSE     CLEAR PREV MED,CLT,PRD,STA,EST             
         J     PNGCPY20                                                         
*                                                                               
PCPYNXBL MVC   PRVMCPSE,TACKMFKY     Same med,clt,prd,sta,est                   
         MVC   MCPSEBLN,TACKMFKY+BUYKBUY-BUYKEY  Orig line # as start           
         BRAS  RE,NXBUYLIN           Find the next buyline for mcpse            
         J     PNGCPY50                                                         
*                                                                               
PCPYEB16 MVC   TACERRCD,=AL2(16)     Spot length is not valid                   
         MVI   TACERRCT,C'B'                                                    
         J     PCPYERR                                                          
*                                                                               
MXBUYLIN EQU   499                                                              
PCPYEB13 MVC   TACERRCD,=AL2(13)     Max buy line # surpassed                   
         MVI   TACERRCT,C'B'                                                    
         J     PCPYERR                                                          
*                                                                               
PCPYEP01 MVC   TACERRCD,=AL2(01)     Need a trailer for prev action             
         MVI   TACERRCT,C'P'                                                    
*                                                                               
PCPYERR  MVI   VACTEFLG,C'Y'         Got an action error                        
         J     PNGCPY90                                                         
PCPYDCH0 DC    H'0'                                                             
PNGCPYX  B     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* Validate spot length                                                          
*===============================================================                
VALSLN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,VSLNTAB          Check against the standards                  
         XR    R3,R3                                                            
         ICM   R3,3,0(R2)          Get length of each Alpha/Med table           
         XR    R4,R4                                                            
         ICM   R4,15,2(R2)          Get displ to end of all tables              
         AR    R4,R2                                                            
*                                                                               
         LA    R2,6(R2)            R2 = A(1st agency/media) table               
VALSLN10 CLC   LP_AGY,0(R2)        Match on agency?                             
         JE    VALSLN20            Yes                                          
         CLC   =C'00',0(R2)        No, on default agency?                       
         JE    VALSLN20            Yes                                          
VALSLN15 AR    R2,R3               Bump to next agency/media table              
         CR    R2,R4               Are we past all the agency/medias?           
         JNL   VALSLND0            Yes, die as this should not happen           
         J     VALSLN10            No, check this new agency/media              
*                                                                               
VALSLN20 CLI   2(R2),C'T'          We only care about media T                   
         JNE   VALSLN15            Not media T                                  
         LA    R4,4(R2)            Start from 0                                 
         LLC   RF,BUSEC                                                         
         SLL   RF,1                Mulitple by 2                                
         AR    R4,RF                                                            
         CLI   1(R4),0             Valid spot length?                           
         JNE   VALSLNYS            Non-zero is a valid spot length              
VALSLNNO J     EXITN                                                            
VALSLNYS J     EXITY                                                            
VALSLND0 DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Find the buyline number for a med,clt,prd,sta,est                             
***********************************************************************         
NXBUYLIN NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(BUYKBUY-BUYKEY),PRVMCPSE                                   
         MVC   IOKEY+BUYKBUY+1-BUYKEY(2),MCPSEBLN                               
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IODIR+IO5'                            
         JE    NXBLNSEQ                                                         
         TM    IOERR,IOEDEL        Deleted key?                                 
         JZ    NXBLNH0             NO, some dreadful error                      
NXBLNSEQ MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IODIR+IO5'                            
         JE    NXBLN020                                                         
         TM    IOERR,IOEDEL        Deleted key?                                 
         JZ    NXBLNH0             NO, some dreadful error                      
*                                                                               
NXBLN020 CLC   IOKEY(BUYKBUY-BUYKEY),IOKEYSAV    Same upto line#?               
         JNE   NXBLNX                                                           
         MVC   MCPSEBLN,IOKEY+BUYKBUY+1-BUYKEY   yes, new last line #           
         J     NXBLNSEQ                                                         
*                                                                               
NXBLNX   J     EXIT                                                             
NXBLNH0  DC    H'0'                                                             
         EJECT                                                                  
*===============================================================                
* PINERGY DELETE SPOTS ACTION  RECORD MAP 2A5                                   
*===============================================================                
PNGDSP   NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     R6,AIO8                                                          
         USING TACKEY,R6                                                        
         MVC   TACRTOKN,QTOKEN                                                  
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
         GOTO1 VHEXIN,DMCB,DDSPMFKY,TACKMFKY,L'DDSPMFKY                         
         DROP  R2                                                               
*                                                                               
         MVC   TACKCHCP,SVBUYACT     Used whatever action we had                
         MVI   TACKIND,C'0'          Indicate this is for delete spot           
*                                                                               
         CLI   SVBUYACT,0            Action in progress?                        
         JE    PDSPEP02              No, need a buy action for DSP              
         CLI   SVBUYACT,C'C'         Copy buyline action?                       
         JE    PDSPEP03              Yes, not allowed                           
*                                                                               
         ICM   RF,15,I$DSPDTE        Any dates to delete?                       
         BZ    PDSPEP07              None                                       
*                                                                               
         LA    R2,LW_DATA2-LW_D(RF)  R2=A(1st date to delete spots)             
         LLH   R3,LW_NUMN-LW_D(RF)   R3 = # of dates to delete                  
*                                                                               
PNGDSP70 MVC   TACKDATE,0(R2)                                                   
         XC    TACKSPSQ,TACKSPSQ     No need for seq # on delete                
*                                                                               
         L     RE,AIO5               AIO5 = A(BUY Record), see PNGCHG           
         CLI   0(RE),X'10'           Make sure record is a Buy record           
         JNH   PDSPEB01              If not, buyline does not exist             
         CLC   0(L'BUYKEY,RE),TACKMFKY                                          
         JNE   PDSPEB01              If not, buyline does not exist             
         LA    RE,BDELEM-BUYKEY(RE)                                             
*                                                                               
PNGDSP75 LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0               Did not find any spots for date            
         JE    PNGDSP80              Who cares then, nothing to delete          
*                                                                               
         CLI   0(RE),RCPOLOQ         X'0B'                                      
         JL    PNGDSP75                                                         
         CLI   0(RE),RCPOTOQ         X'0C'                                      
         JH    PNGDSP75                                                         
         CLC   TACKDATE,RDATE-REGELEM(RE)  Match on date?                       
         JNE   PNGDSP75                                                         
         CLC   =X'0000',RPAY-REGELEM(RE)   Spot is paid?                        
         JNE   PDSPEW03                                                         
         J     PNGDSP75              Go thru all spots with this date           
*                                                                               
PNGDSP80 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PDSPDCH0                                                         
         AHI   R2,L'RDATE                                                       
         JCT   R3,PNGDSP70                                                      
         J     PNGDSP95                                                         
*                                                                               
PNGDSP90 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PDSPDCH0                                                         
*                                                                               
PNGDSP95 MVC   WTOTXT1,=70C' '                                                  
         MVC   WTOTXT1(19),=CL19'*TEST* SL2B DSP ERROR '                        
         EDIT  (B2,TACERRCD),(4,WTOTXT1+19),FILL=0                              
         MVC   WTOTXT1+40(L'QTOKEN),QTOKEN                                      
         MVC   WTOTXT2,=70C' '                                                  
         MVC   WTOTXT2(70),QDDSPTR                                              
*&&WT*&& WTO   TEXT=((MSGSTARL,D),(WTOTXT1L,D),(WTOTXT2L,D),(0,E)),    +        
               DESC=2                                                           
         J     EXIT                                                             
*                                                                               
PDSPEB01 MVC   TACERRCD,=AL2(01)     Buy does not exist                         
         MVI   TACERRCT,C'B'                                                    
         J     PDSPERR                                                          
*                                                                               
PDSPEW03 MVC   TACERRCD,=AL2(03)     Can't modify weeks with paid spots         
         MVI   TACERRCT,C'W'                                                    
         J     PDSPERR                                                          
*                                                                               
PDSPEP02 MVC   TACERRCD,=AL2(02)     No previous action for this rec            
         J     PDSPPERR                                                         
*                                                                               
PDSPEP03 MVC   TACERRCD,=AL2(03)     DeleteSpots not allowed for Copy           
         J     PDSPPERR                 Buyline Request                         
*                                                                               
PDSPEP07 MVC   TACERRCD,=AL2(07)     DeleteSpots needs at least one             
         J     PDSPPERR                 date                                    
*                                                                               
PDSPPERR MVI   TACERRCT,C'P'                                                    
*                                                                               
PDSPERR  MVI   VACTEFLG,C'Y'         Got an action error                        
         J     PNGDSP90                                                         
PDSPDCH0 DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* PINERGY ADD SPOT ACTION     RECORD MAP 2A6                                    
*===============================================================                
PNGASP   NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     R6,AIO8                                                          
         USING TACKEY,R6                                                        
         MVC   TACRTOKN,QTOKEN                                                  
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPMFKY,TACKMFKY,L'DDSPMFKY                         
         DROP  R2                                                               
         MVC   TACKCHCP,SVBUYACT     Used whatever action we had                
*                                                                               
         CLI   SVBUYACT,C'C'         Add spot for Buy Copy request?             
         JNE   *+10                                                             
         MVC   TACKCPSQ,CPYCOUNT     Yes, identify which copy action            
*                                                                               
         MVC   TACKNSLN,SVBUSEC      Saved new spot length                      
         MVC   TACKNCST,SVBUCOST     Saved new buyline rate                     
         MVI   TACKIND,C'1'          Indicate this is for add spot              
         MVC   TACKDATE,QSPTDATE                                                
*                                                                               
         XR    RE,RE                 ADD SPOT COUNT FOR USE WITH SEQ            
         ICM   RE,3,ASPCOUNT            IN TSAR KEY                             
         LA    RE,1(RE)                                                         
         STCM  RE,3,ASPCOUNT                                                    
         MVC   TACKSPSQ,ASPCOUNT     USE ADD SPOT COUNT FOR UNIQUENESS          
*                                                                               
         CHI   RE,208                Do we have more than 208 spots?            
         JH    PASPEB12              Yes, hit the max for a buyline             
*                                                                               
         OC    QSPCOST,QSPCOST       Any spot cost override?                    
         JZ    PNGASP10              None                                       
         CP    =P'0',QSPCOST         Cost override of $0???                     
         JE    PNGASPZ0                                                         
         CVB   R1,QSPCOST                                                       
         STCM  R1,15,TACRCSOV                                                   
*                                                                               
PNGASP10 MVI   TACRBPRD,0                                                       
         CLC   QSPPRD,SPACES         Any product code provided?                 
         JNH   PNGASP20              None                                       
         BRAS  RE,FINDPRD                                                       
         JNE   PASPES04                                                         
         STC   R1,TACRBPRD                                                      
*                                                                               
         BRAS  RE,ADDPRD                                                        
         JNE   PASPEB11              Happens if IOBRDLST is full                
         CLI   XPRDLIST,128          Surpassed max # of unique prds             
         BNL   PASPEB11                per buyline?  Yes                        
*                                                                               
PNGASP20 CLI   SVBUYACT,0            Action in progress?                        
         JE    PASPEP02              No, need a buy action for ASP              
****     CLI   SVBUYACT,C'B'         Add spot for Buy Change request?           
****     JNE   PNGASP90                                                         
         CLC   TACRCSOV,SVBUCOST     Override cost same as byln rate?           
         JE    PNGASP90              Yes, no problem                            
         CLI   TACRCSOV,0            Override > 167772.15??                     
         JNE   PASPES05              Yes, invalid override                      
*                                                                               
PNGASP90 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PASPDCH0                                                         
*                                                                               
         MVC   WTOTXT1,=70C' '                                                  
         MVC   WTOTXT1(19),=CL19'*TEST* SL2B ASP ERROR '                        
         EDIT  (B2,TACERRCD),(4,WTOTXT1+19),FILL=0                              
         MVC   WTOTXT1+40(L'QTOKEN),QTOKEN                                      
         MVC   WTOTXT2,=70C' '                                                  
         MVC   WTOTXT2(70),QDDSPTR                                              
*&&WT*&& WTO   TEXT=((MSGSTARL,D),(WTOTXT1L,D),(WTOTXT2L,D),(0,E)),    +        
               DESC=2                                                           
         J     EXIT                                                             
*                                                                               
PNGASPZ0 OI    TACRFLAG,TACRZERO     Cost override of $0                        
         J     PNGASP10                                                         
*                                                                               
PASPES04 MVC   TACERRCD,=AL2(04)     Product code is not valid                  
         MVI   TACERRCT,C'S'                                                    
         J     PASPERR                                                          
*                                                                               
PASPES05 MVC   TACERRCD,=AL2(05)     Invalid spot cost override                 
         MVI   TACERRCT,C'S'                                                    
         J     PASPERR                                                          
*                                                                               
PASPEB11 MVC   TACERRCD,=AL2(11)     Max # of prds in buyline is 128            
         MVI   TACERRCT,C'B'                                                    
         J     PASPERR                                                          
*                                                                               
PASPEB12 MVC   TACERRCD,=AL2(12)     Max # of spots per buyline is 208          
         MVI   TACERRCT,C'B'                                                    
         J     PASPERR                                                          
*                                                                               
PASPEP02 MVC   TACERRCD,=AL2(02)     No previous action for this rec            
         MVI   TACERRCT,C'P'                                                    
         J     PASPERR                                                          
*                                                                               
PASPERR  MVI   VACTEFLG,C'Y'         Got an action error                        
         J     PNGASP90                                                         
PASPDCH0 DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* PINERGY END OF ACTION       RECORD MAP 2A7                                    
*===============================================================                
PNGEOR   NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     R6,AIO8                                                          
         USING TACKEY,R6                                                        
         MVC   TACRTOKN,QTOKEN                                                  
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPMFKY,TACKMFKY,L'DDSPMFKY                         
         DROP  R2                                                               
         MVC   TACKCHCP,SVBUYACT     Used whatever action we had                
         MVC   TACKNSLN,SVBUSEC      Saved new spot length                      
         MVC   TACKNCST,SVBUCOST     Saved new buyline rate                     
         MVI   TACKIND,C'9'          INDICATE THIS A TRAILER                    
*                                                                               
         CLI   SVBUYACT,C'C'         Add spot for Buy Copy request?             
         JNE   *+10                                                             
         MVC   TACKCPSQ,CPYCOUNT     Yes, identify which copy action            
*                                                                               
         TM    MISCFLG1,MF1NDTLR         Need a trailer?                        
         JZ    PEOREP02                                                         
         NI    MISCFLG1,X'FF'-MF1NDTLR   Got our trailer now                    
*                                                                               
PNGEOR90 GOTOR BUFFER,DMCB,('TOKACTQ',TSAADD),('TACKEYL',TACRECL)               
         JNE   PEORDCH0                                                         
*                                                                               
         MVC   WTOTXT1,=70C' '                                                  
         MVC   WTOTXT1(19),=CL19'*TEST* SL2B EOR ERROR '                        
         EDIT  (B2,TACERRCD),(4,WTOTXT1+19),FILL=0                              
         MVC   WTOTXT1+40(L'QTOKEN),QTOKEN                                      
         MVC   WTOTXT2,=70C' '                                                  
         MVC   WTOTXT2(70),QDDSPTR                                              
*&&WT*&& WTO   TEXT=((MSGSTARL,D),(WTOTXT1L,D),(WTOTXT2L,D),(0,E)),    +        
               DESC=2                                                           
*                                                                               
         XC    QDDSPTR,QDDSPTR       Clear these as we're done with             
         MVI   SVBUYACT,0              the previous action                      
         MVI   SVBUSEC,0                                                        
         XC    SVBUCOST,SVBUCOST                                                
         J     PNGEORX                                                          
*                                                                               
PEOREP02 MVC   TACERRCD,=AL2(02)     No previous action for this rec            
         MVI   TACERRCT,C'P'                                                    
*                                                                               
PEORERR  MVI   VACTEFLG,C'Y'         We got an error for an action              
         J     PNGEOR90                                                         
PNGEORX  B     EXIT                                                             
PEORDCH0 DC    H'0'                                                             
         EJECT                                                                  
*===============================================================                
* PINERGY VALIDATE POINTER    RECORD MAP 2A8                                    
*===============================================================                
PNGVPT   NTR1  BASE=*,LABEL=*                                                   
*&&WT*&& WTO   '*TEST* SL2B PNGVPT'                                             
*                                                                               
         L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     R4,AIO8                                                          
         USING TOKPTRD,R4                                                       
         MVC   TPBTOKN,QTOKEN                                                   
         GOTOR REFRMPTR                                                         
****     MVC   TPBDDSP,QDDSPTR                                                  
*                                                                               
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
*                                                                               
         CLC   0(DDSPNSPT-DDSPTRD,R2),PDDSPTR  Same byln as previous?           
         JNE   PVPTR010                                                         
*  Same line checksum as previous line?  (Not incl spot cksm)                   
         CLC   DDSPMCKS(LINCKSML*2),PDDSPTR+DDSPMCKS-DDSPTRD                    
         JNE   PVPTEB14                        No, we have a problem            
         GOTO1 VHEXIN,DMCB,DDSPMCKS,LINCKSM,L'LINCKSM*2                         
         OC    PPTRLLER,PPTRLLER            Have a prev line level err?         
         JZ    PVPTR040                     No, check spot level                
         MVC   TPBERRCD,PPTRLLER            Same error then                     
         MVC   TPBERRCT,PPTRLLEC                                                
         J     PVPTTSAD                     put it out too                      
*                                                                               
PVPTR010 MVC   PDDSPTR,QDDSPTR              We now have a previous ptr          
         XC    PPTRLLER,PPTRLLER            But no line error yet               
         MVI   PPTRLLEC,0                                                       
*                                                                               
         OC    SVCLT,SVCLT              WE HAVE A SAVED CLIENT?                 
         JZ    PVPTVCLT                 NO VALIDATE IT                          
PVPTR015 CLC   DDSPQCLT,SVCLT           YES, ONLY 1 CLIENT ALLOWED              
         JNE   PVPTEP04                                                         
*                                                                               
         CLI   DDSPMFKY+1,C'1'             Only media T for import              
         JNE   PVPTEP06                    Not a media T buyline                
         XC    IOKEY,IOKEY                                                      
         GOTO1 VHEXIN,DMCB,DDSPMFKY,IOKEY,L'BUYKEY*2                            
         CLC   SVBCLT,IOKEY+1           Hex client does not match               
         JNE   PVPTEB15                   buylines's hex client code            
*                                                                               
         MVC   HALF,IOKEY+BUYKBUY-BUYKEY    ADJUST BYLN # FOR SPTD              
         MVI   IOKEY+BUYKBUY-BUYKEY,0                                           
         MVC   IOKEY+BUYKBUY-BUYKEY+1(2),HALF                                   
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         JNE   PVPTEB01                                                         
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   PVPTEB01                                                         
*                                                                               
         L     R6,AIO5                                                          
         USING BUYKEY,R6                                                        
*                                                                               
         CLI   DDSPTMUN,C'S'              WE ONLY USE SECONDS NOW               
         JNE   PVPTEB02                                                         
         GOTO1 VHEXIN,DMCB,DDSPSLN,BYTE,L'DDSPSLN                               
*&&DO                                                                           
         PACK  DUB,DDSPSLN                ASSUME DECIMAL LENGTH FOR NOW         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BYTE                                                          
*&&                                                                             
         CLC   BDSEC,BYTE                 SPOT LENGTH MATCHES?                  
         JNE   PVPTEB02                                                         
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPCIND,HALF,L'DDSPCIND                             
         MVC   HALF2(1),BDCIND            HANDLED BY INDICATORS                 
         MVC   HALF2+1(1),BDCIND2                                               
         CLC   HALF,HALF2                                                       
         JNE   PVPTEB03                   COST INDICATORS NOT THE SAME          
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPGRSD,FULL,L'DDSPGRSD                             
         MVI   FULL2,0                                                          
         MVC   FULL2+1(L'BDCOST),BDCOST   WHAT ABOUT NEG RATES???               
         CLC   FULL,FULL2                 COST INDICATORS HANDLED IT            
         JNE   PVPTEB04                   COST INDICATORS NOT THE SAME          
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPMCKS,LINCKSM,L'LINCKSM*2                         
         LA    R3,LINCKSM                                                       
         USING LINCKSMD,R3                                                      
         LA    RE,BDPROGRM                PROGRAM NAME                          
         LA    RF,L'BDPROGRM                                                    
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,12,HALF2         CHECKSM HAS FOR EXAMPLE 1234ABCD             
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF2          ADD HOB OF TOP TO LOB OF LOWER               
         ICM   RE,2,HALF2+1        ADD LOB OF TOP TO HOB OF LOWER               
         AR    R0,RE               SO  3412 + ABCD = DFDF                       
         STCM  R0,3,HALF2                                                       
         CLC   LCKSMPRG,HALF2                                                   
         JNE   PVPTEB05                                                         
*                                                                               
         CLC   LCKSMADJ,BDPROGT           ADJ CODE                              
         BNE   PVPTEB06                                                         
*                                                                               
         CLC   LCKSMDYS,BDDAY             DAYS                                  
         JNE   PVPTEB07                                                         
*                                                                               
         CLC   LCKSMDPT,BDDAYPT           DAYPART                               
         JNE   PVPTEB08                                                         
*                                                                               
         XR    R0,R0                     4 BYTES SO CKSM IS NOT GOOD            
         ICM   R0,3,BDTIMST                                                     
         OC    BDTIMEND,BDTIMEND                                                
         JNZ   *+10                                                             
         MVC   BDTIMEND,BDTIMST          SO SAME AS SPLNK22:FLTBYRY             
         XR    RE,RE                                                            
         ICM   RE,1,BDTIMEND             ADD HOB OF END TO LOB OF START         
         ICM   RE,2,BDTIMEND+1           ADD LOB OF END TO HOB OF START         
         AR    R0,RE                                                            
         STCM  R0,3,HALF2                                                       
         CLC   LCKSMTIM,HALF2                                                   
         JNE   PVPTEB09                                                         
*                                                                               
         XR    R0,R0                     IN CASE WE DON'T HAVE BUYID            
         LA    RE,BDELEM                                                        
PVPTR020 CLI   0(RE),0                                                          
         JE    PVPTR030                                                         
         CLI   0(RE),IDELCODQ            X'70' - ID ELEMENT?                    
         JE    PVPTR025                                                         
         LLC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         J     PVPTR020                                                         
*                                                                               
PVPTR025 DS    0H                  RE already pointing to ID elem               
         LLC   RF,1(RE)            L'ID elem                                    
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,12,HALF2         CHECKSM HAS FOR EXAMPLE 1234ABCD             
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF2          ADD HOB OF TOP TO LOB OF LOWER               
         ICM   RE,2,HALF2+1        ADD LOB OF TOP TO HOB OF LOWER               
         AR    R0,RE               SO  3412 + ABCD = DFDF                       
*                                                                               
PVPTR030 STCM  R0,3,HALF2                                                       
         CLC   LCKSMIDL,HALF2             ID DOES NOT MATCH                     
         JNE   PVPTEB10                                                         
***************                                                                 
* Check the spot level checksum                                                 
***************                                                                 
PVPTR040 GOTO1 VHEXIN,DMCB,DDSPNSPT,BYTE2,L'DDSPNSPT  Want Nth spot             
         CLI   BYTE2,0                                                          
         JE    PVPTEW01                   We can't find our spot                
         GOTO1 VDATCON,DMCB,(0,DDSPSDAT),(2,HALF2) MATCH OUR SPOT DATE?         
*                                                                               
         XR    R0,R0                 Haven't found any spots for date           
         L     R6,AIO5               Locate the desired spot in Buy rec         
         XR    RE,RE                                                            
         ICM   RE,7,BDCOST                                                      
*&&DO                                                                           
* This block is commented out because cost overrides are always in              
*   pennies.  Eugene builds the DDSPtr with the spot cost but doesn't           
*   mulitply by 100 to set the amounts into pennies.                            
         TM    BDCIND2,BDCNBRDQ           X'10' - RATE IN DOLLARS?              
         JZ    PVPTR043                                                         
         MHI   RE,100                Yes, default in pennies                    
*&&                                                                             
PVPTR043 STCM  RE,15,FULL2           Rate if spot iS not cost override          
         LA    R6,BDELEM                                                        
*                                                                               
PVPTR045 LLC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0                    EOR?                                  
         JE    PVPTEW01                   Spot prob was deleted on MF           
*                                                                               
         CLI   0(R6),RCORGQ               We have a spot that counts?           
         JL    PVPTR045                   Want the x'06'-X'0C' spots            
         CLI   0(R6),RCPOTOQ                                                    
         JH    PVPTR045                                                         
*                                                                               
         USING REGELEM,R6                                                       
PVPTR050 TM    RSTATUS,RSMINUSQ+RSMINSDQ   Minused or Missed?                   
         JNZ   PVPTR045                Yes, skip this spot for Pinergy          
         CLC   RDATE,HALF2                                                      
         JNE   PVPTR045                   SPOT DATE DOES NOT MATCH              
*                                                                               
         AHI   R0,1                                                             
         CLM   R0,1,BYTE2                 Found the Nth spot?                   
         JNE   PVPTR045                   No, keep looking                      
*                                                                               
         GOTO1 VHEXIN,DMCB,DDSPSCST,FULL,L'DDSPSCST  COST IN DDSPTR             
         TM    RSTATUS,RSRATOVQ           Rate override on spot?                
         JZ    PVPTR055                                                         
         XC    FULL2,FULL2                Yes, get that value                   
         MVC   FULL2+1(3),RPCOST                                                
*                                                                               
PVPTR055 CLC   FULL,FULL2                 SPOT COST NOT THE SAME?               
         JNE   PVPTES02                                                         
*                                                                               
         OC    RPAY,RPAY           WHY IS THIS SPOT PAID??                      
         BNZ   PVPTR057            SKIP CHKSUM AS PAID IS CHECKED LATER         
*                                                                               
         LA    RE,REGELEM          LET'S SEE ABOUT THE CHECKSUM                 
         LLC   RF,RLEN                                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,12,HALF2         CHECKSM HAS FOR EXAMPLE 1234ABCD             
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF2          ADD HOB OF TOP TO LOB OF LOWER               
         ICM   RE,2,HALF2+1        ADD LOB OF TOP TO HOB OF LOWER               
         AR    R0,RE               SO  3412 + ABCD = DFDF                       
         STCM  R0,3,HALF2                                                       
         LA    R3,LINCKSM                                                       
         CLC   SPTECKSM,HALF2             CHECKSUM FOR SPOT ELEM SAME?          
         JNE   PVPTES03                   NO, SOMETHING ELSE CHANGED            
******************************************                                      
* We have to make sure total # of spots for the date did not change on          
* the actual buy record since we don't use a real checksum                      
******************************************                                      
PVPTR057 CLC   DDSPNSPT,DDSPTSPT   ONLY CHECK THIS WHEN N=TOTAL                 
         JNE   PVPTTSAD            It is not                                    
         MVC   BYTE,BYTE2          BYTE is now N which is also total            
*                                                                               
*                                  BYTE2 = total # of spots for date            
*                                  HALF2 = date of interest                     
         GOTO1 VHEXIN,DMCB,DDSPTSPT,BYTE2,L'DDSPTSPT                            
         GOTO1 VDATCON,DMCB,(0,DDSPSDAT),(2,HALF2) MATCH OUR SPOT DATE?         
         LLC   RF,BYTE             RF=current # of spots we've seen             
*                                                                               
PVPTR060 LLC   R0,1(R6)            Let's count total # for this                 
         AR    R6,R0                                                            
         CLI   0(R6),0             EOR?                                         
         JE    PVPTR070            Done looking for spots for date              
*                                                                               
         CLI   0(R6),RCORGQ        We have a spot that counts?                  
         JL    PVPTR060            Want the x'06'-X'0C' spots                   
         CLI   0(R6),RCPOTOQ                                                    
         JH    PVPTR060                                                         
*                                                                               
         USING REGELEM,R6                                                       
         TM    RSTATUS,RSMINUSQ+RSMINSDQ   Minused or Missed?                   
         JNZ   PVPTR060            Yes, skip this spot for Pinergy              
*                                                                               
         CLC   RDATE,HALF2         Spot date matches date in DDSPtr?            
         JNE   PVPTR060            No, next spot please                         
         AHI   RF,1                Yes, bump count and count til EOR            
         J     PVPTR060                                                         
*                                                                               
PVPTR070 CLM   RF,1,BYTE2          Match total # of spots for date?             
         JH    PVPTEW02            No, means there are more spots on MF         
         J     PVPTTSAD            No problem with this ptr                     
*                                                                               
PVPTVCLT BRAS  RE,VALCLT                                                        
         JNE   PVPTEP05                                                         
         J     PVPTR015                                                         
***************                                                                 
PVPTEB01 MVC   TPBERRCD,=AL2(1)           BUYLINE DOES NOT EXIST                
         J     PVPTBERR                                                         
*                                                                               
PVPTEB02 MVC   TPBERRCD,=AL2(2)           SPOT LENGTH NOT THE SAME              
         J     PVPTBERR                                                         
*                                                                               
PVPTEB03 MVC   TPBERRCD,=AL2(3)           COST INDICATORS NOT THE SAME          
         J     PVPTBERR                                                         
*                                                                               
PVPTEB04 MVC   TPBERRCD,=AL2(4)           BUYLINE RATE NOT THE SAME             
         J     PVPTBERR                                                         
*                                                                               
PVPTEB05 MVC   TPBERRCD,=AL2(5)           PROGRAM NAME NOT THE SAME             
         J     PVPTBERR                                                         
*                                                                               
PVPTEB06 MVC   TPBERRCD,=AL2(6)           ADJ CODE  NOT THE SAME                
         J     PVPTBERR                                                         
*                                                                               
PVPTEB07 MVC   TPBERRCD,=AL2(7)           DAYS  NOT THE SAME                    
         J     PVPTBERR                                                         
*                                                                               
PVPTEB08 MVC   TPBERRCD,=AL2(8)           DPT   NOT THE SAME                    
         J     PVPTBERR                                                         
*                                                                               
PVPTEB09 MVC   TPBERRCD,=AL2(9)           START/END TIMES NOT THE SAME          
         J     PVPTBERR                                                         
*                                                                               
PVPTEB10 MVC   TPBERRCD,=AL2(10)          ID NOT THE SAME                       
         J     PVPTBERR                                                         
*                                                                               
PVPTEB14 MVC   TPBERRCD,=AL2(14)    CHECKSUM OF SAME BUYLINE DOES               
         J     PVPTBERR               NOT MATCH, THIS CAN'T BE                  
*                                                                               
PVPTEB15 MVC   TPBERRCD,=AL2(15)    Client code doesn't match buy's             
         J     PVPTBERR                                                         
***************                                                                 
PVPTEW01 MVC   TPBERRCD,=AL2(01)    Spot was deleted from this week             
         J     PVPTWERR                                                         
*                                                                               
PVPTEW02 MVC   TPBERRCD,=AL2(02)    Spot was added to this week                 
         J     PVPTWERR                                                         
***************                                                                 
PVPTES01 MVC   TPBERRCD,=AL2(01)    Spot date changed                           
         J     PVPTSERR                                                         
*                                                                               
PVPTES02 MVC   TPBERRCD,=AL2(02)    Spot cost override changed                  
         J     PVPTSERR                                                         
*                                                                               
PVPTES03 MVC   TPBERRCD,=AL2(03)    Spot level checksum changed                 
         J     PVPTSERR                                                         
***************                                                                 
PVPTEP04 MVC   TPBERRCD,=AL2(04)    ONLY ONE CLIENT CODE PER                    
         J     PVPTPERR               IMPORT FILE                               
*                                                                               
PVPTEP05 MVC   TPBERRCD,=AL2(05)    INVALID CLIENT CODE                         
         J     PVPTPERR                                                         
*                                                                               
PVPTEP06 MVC   TPBERRCD,=AL2(06)    Not a media T buyline                       
         J     PVPTPERR                                                         
***************                                                                 
PVPTBERR MVI   TPBERRCT,C'B'        Buyline level category                      
         MVC   PPTRLLER,TPBERRCD    Save as previous line level err             
         MVC   PPTRLLEC,TPBERRCT                                                
         J     PVPTEERR                                                         
*                                                                               
PVPTPERR MVI   TPBERRCT,C'P'        Pinergy level category                      
         MVC   PPTRLLER,TPBERRCD    Save as previous line level err             
         MVC   PPTRLLEC,TPBERRCT                                                
         J     PVPTEERR                                                         
*                                                                               
PVPTWERR MVI   TPBERRCT,C'W'        Week    level category                      
         J     PVPTEERR                                                         
*                                                                               
PVPTSERR MVI   TPBERRCT,C'S'        Spot    level category                      
*                                                                               
PVPTEERR MVI   VPTREFLG,C'Y'        We got an error for DDSPTR                  
         OI    MISCFLG1,MF1VPTER                                                
*                                                                               
PVPTTSAD OC    TPBERRCD,TPBERRCD    Any error?                                  
         JZ    EXIT                 None, no need to add to TSAR                
*                                                                               
         GOTOR BUFFER,DMCB,('TOKPTRQ',TSAADD),('TPBKEYL',TPBRECL)               
         JNE   PVPTDCH0                                                         
         B     EXIT                                                             
*                                                                               
PVPTDCH0 DC    H'0'                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*===============================================================                
* COMMIT ACTIONS TO MF RECORDS                                                  
*===============================================================                
COMMIT   DS    0H                                                               
*&&WT*&& WTO   '*TEST* SL2B COMMIT'                                             
*                                                                               
         TM    MISCFLG1,MF1TSRVP     TSAR still setup to VALPTR?                
         JZ    CMMT005               No                                         
         TM    MISCFLG1,MF1VPTER     Had errors from ValPtr???                  
         JZ    CMMTDONE              No actions processed                       
*                                                                               
         BRAS  RE,OUTVPER            Output VALPTR errors                       
         J     CMMTDONE              Issue FREEMAIN                             
*                                                                               
CMMT005  CLI   VACTEFLG,C'Y'         Any action errors?                         
         JE    CMTVACT               Put them out with LINKIO                   
*                                                                               
         TM    MISCFLG1,MF1VPTER     Errors from ValPtr???                      
         JNZ   CMTOERRS              Yes, output error codes & text             
*                                                                               
         GOTOR BUFFER,DMCB,('TOKACTQ',TSAGET),1  No, process actions            
CMMT010  TM    BUFFRET,TSEEOF        No more entries?                           
         JNZ   CMMTDONE              None, we're done                           
*                                                                               
         L     R4,AIO8                                                          
         USING TOKACTD,R4                                                       
         OC    TACERRCD,TACERRCD     ANY ERROR IN THIS TSAR RECORD?             
         JNZ   CMMTDCH0              Why shoud this be??                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY,TACKMFKY                                                   
         MVC   HALF,IOKEY+BUYKBUY-BUYKEY   Adjust buyline # for SPTD            
         MVI   IOKEY+BUYKBUY-BUYKEY,0                                           
         MVC   IOKEY+BUYKBUY-BUYKEY+1(2),HALF                                   
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         JNE   CMMTDCH0              WTF?                                       
*                                                                               
         CLI   TACKCHCP,C'B'         Buy change request?                        
         JNE   CMMTCPY                                                          
         BRAS  RE,BUYCHG             Yes                                        
         J     CMMTNXT                                                          
*                                                                               
CMMTCPY  BRAS  RE,BUYCPY             No, copy buyline                           
*                                                                               
CMMTNXT  GOTOR VDATAMGR,DMCB,(0,=C'COMMIT'),0  Commit my changes                
*                                                                               
         GOTOR BUFFER,DMCB,('TOKACTQ',TSANXT),0                                 
         J     CMMT010                                                          
*                                                                               
CMTVACT  BRAS  RE,OUTVACT            Output VALACT errors                       
CMTOERRS BRAS  RE,OUTPERR            Output errors codes & text                 
         GOTOR VDATAMGR,DMCB,=C'DMUNLK',0                                       
*&&WT*&& WTO   '*TEST* SL2B  UNLOCKED!!'                                        
         J     EXITN                                                            
*                                                                               
CMMTDONE GOTOR VDATAMGR,DMCB,=C'DMUNLK',0                                       
*&&WT*&& WTO   '*TEST* SL2B  UNLOCKED!!'                                        
*                                                                               
         BRAS  RE,OUTSUCC            Output Success                             
         B     EXITY                                                            
CMMTDCH0 DC    H'0'                  Shouldn't happen, but ...                  
         EJECT                                                                  
***********************************************************************         
* PROCESS BUY CHANGE REQUEST                                                    
*                                                                               
* ON ENTRY:    (R4)                  AIO8 - TSAR record                         
***********************************************************************         
BUYCHG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO5'                           
         JNE   BCHGDCH0                                                         
*                                                                               
         L     R6,AIO5                                                          
         USING BUYKEY,R6                                                        
         MVC   AIOBUY,AIO5                                                      
         XC    IOBRDLST,IOBRDLST                                                
         BRAS  RE,BLDXPRD                                                       
*                                                                               
BCHG010  GOTOR BUFFER,DMCB,('TOKACTQ',TSANXT),0                                 
         JNE   BCHGDCH0                                                         
*                                                                               
         CLI   TACKIND,C'9'          Trailer for this Change Request?           
         JE    BCHG090               Yes, save this buyline                     
*********                                                                       
* Delete spots matching the date                                                
*********                                                                       
         CLI   TACKIND,C'0'          Delete Spot for this change?               
         JNE   BCHGAS00              No, it is an Add Spot                      
         LA    R3,BDELEM             Locate all spots with this date            
BCHGDS10 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
BCHGDS15 CLI   0(R3),0               End of record                              
         JE    BCHG010               delete done for this date                  
         CLI   0(R3),RCPOLOQ         X'0B' - Regular spot                       
         JL    BCHGDS10                                                         
         CLI   0(R3),RCPOTOQ         X'0C' - OTO spot?                          
         JH    BCHGDS10              only want spots                            
         CLC   TACKDATE,RDATE-REGELEM(R3) Want exact match on date              
         JNE   BCHGDS10                Not exact, next spot                     
         CLC   =X'0000',RPAY-REGELEM(R3)  Spot is already paid?                 
         JNE   BCHGDCH0                   Yes, die as we should be here         
*                                                                               
BCHGDS40 GOTOR RECUP,DMCB,(C'S',AIOBUY),(R3),(R3)  delete it from here          
         J     BCHGDS15                   see if more spot action               
*********                                                                       
* Add spot                                                                      
*********                                                                       
BCHGAS00 LA    R2,ELEM               No, add a spot to this line                
         XC    ELEM,ELEM                                                        
         USING REGELEM,R2                                                       
         MVI   REGELEM,RCPOLOQ       X'0B' element                              
         MVI   RLEN,RLREGLNQ         default length without allocation          
         MVC   RDATE,TACKDATE        Spot date                                  
         CLI   TACRBPRD,0            Do we have any allocation?                 
         JE    BCHGAS10              No, none                                   
         MVI   RLEN,RLPOL1LQ         Yes, only one brand                        
         MVC   RPPRD,TACRBPRD                                                   
*                                                                               
         LA    R1,RPPRD                                                         
         BRAS  RE,ADDPRD                                                        
*                                                                               
         MVC   RPTIME,BDSEC          same spot length as buyline                
BCHGAS10 OC    TACRCSOV,TACRCSOV     Any cost override?                         
         JZ    BCHGZERO              None, cost override might be $0            
         CLC   TACRCSOV,TACKNCST     same rate as buyline rate?                 
         JE    BCHGAS20              then not an override                       
         MVC   RPCOST,TACRCSOV+1     No, we have a cost override                
BCHGAS15 OI    RSTATUS,RSRATOVQ      x'20'-rate override for spot               
*                                                                               
BCHGAS20 LA    R3,BDELEM                                                        
BCHGAS25 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0               End of record                              
         JE    BCHGAS40              then okay to add here                      
         CLI   0(R3),RCPOLOQ         X'0B' - Regular spot                       
         JE    BCHGAS30                                                         
         CLI   0(R3),RCPOTOQ         X'0C' - OTO spot?                          
         JNE   BCHGAS25              NOT ONE OF THESE? NEXT ELEM THEN           
BCHGAS30 CLC   RDATE,RDATE-REGELEM(R3) DATE TO BE ADDED > THIS DATE?            
         JH    BCHGAS25                                                         
*                                                                               
BCHGAS40 GOTOR RECUP,DMCB,(C'S',AIOBUY),ELEM,(R3)  add it here                  
         J     BCHG010                    see if more spot action               
*                                                                               
BCHGZERO TM    TACRFLAG,TACRZERO     Cost override of $0                        
         JNZ   BCHGAS15              Yes                                        
         J     BCHGAS20              No, don't set override flag                
*                                                                               
BCHG090  BRAS  RE,ADDDTXEL                                                      
         BRAS  RE,BUYVAL                                                        
*                                                                               
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO5'                           
         JNE   BCHGDCH0                                                         
*                                                                               
BCHGX    J     EXITY                                                            
BCHGDCH0 DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS BUY COPY REQUEST                                                      
***********************************************************************         
BUYCPY   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO8                                                          
         USING TOKACTD,R4                                                       
*                                                                               
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   BCPYDCH0                                                         
         L     RE,AIO5               AIO5 contains original buyline             
         LHI   RF,IO5LQ              Both IO areas are 6K                       
         L     R0,AIO6                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                 make a copy to AIO6                        
*                                                                               
         MVC   AIOBUY,AIO6           Working with the copy                      
         L     R6,AIO6                                                          
         USING BUYRECD,R6                                                       
         MVC   BDSEC,TACKNSLN                                                   
         CLI   TACKNCST,0            Is the HighOrderByte used?                 
         JNE   BCPY010               No, new rate < 167772.15                   
         MVC   BDCOST,TACKNCST+1     No, just the last 3 bytes                  
         J     BCPY015                                                          
*                                                                               
BCPY010  ICM   RF,15,TACKNCST        NOTE: Original has to be in $$             
         CVD   RF,DUB                                                           
         SRP   DUB,64-2,5            Divide by 100 so pennies to $$             
         CVB   RF,DUB                                                           
         STCM  RF,7,BDCOST                                                      
*                                                                               
BCPY015  LA    R6,BDELEM             Get rid of all spots on the copy           
BCPY020  CLI   0(R6),0               End of record                              
         BE    BCPY029                                                          
         CLI   0(R6),RCPOLOQ         X'0B' - normal spot?                       
         BL    BCPY025                                                          
         CLI   0(R6),RCPOTOQ         X'0C' - OTO spot?                          
         BH    BCPY025                                                          
         LR    R2,R6                 R2=A(where to start adding spots)          
         GOTOR RECUP,DMCB,(C'S',AIOBUY),(R6),(R6)  Delete elem                  
         B     BCPY020                                                          
*                                                                               
BCPY025  LLC   R0,1(R6)              Point R6 to the next element               
         AR    R6,R0                                                            
         J     BCPY020                                                          
         DROP  R6                                                               
*                                                                               
BCPY029  XC    IOBRDLST,IOBRDLST                                                
         XC    XPRDLIST,XPRDLIST                                                
*                                                                               
BCPY030  GOTOR BUFFER,DMCB,('TOKACTQ',TSANXT),0                                 
         JNE   BCPYDCH0                                                         
*                                                                               
         CLI   TACKIND,0             No indicator?                              
         JE    BCPY030               None, skip to next record                  
         CLI   TACKIND,C'9'          Trailer for this Copy Request?             
         JE    BCPYADDB              Yes, add this buyline                      
*                                                                               
         LA    R2,ELEM               No, add a spot to this line                
         XC    ELEM,ELEM                                                        
         USING REGELEM,R2                                                       
         MVI   REGELEM,RCPOLOQ       X'0B' - normal spot                        
         MVI   RLEN,RLREGLNQ         default length without allocation          
         MVC   RDATE,TACKDATE        Spot date                                  
         CLI   TACRBPRD,0            Do we have any allocation?                 
         JE    BCPY033               No, none                                   
         MVI   RLEN,RLPOL1LQ         Yes, only one brand                        
         MVC   RPPRD,TACRBPRD                                                   
         MVC   RPTIME,TACKNSLN       same spot length as buyline                
*                                                                               
         LA    R1,RPPRD                                                         
         BRAS  RE,ADDPRD                                                        
*                                                                               
BCPY033  OC    TACRCSOV,TACRCSOV     Any rate override?                         
         JZ    BCPYZERO              None, zero is not a concern here           
         CLC   TACRCSOV,TACKNCST     same rate as buyline rate?                 
         JE    BCPY040                                                          
         MVC   RPCOST,TACRCSOV+1     No, we have a cost override                
BCPY036  OI    RSTATUS,RSRATOVQ      x'20'-rate override for spot               
*                                                                               
BCPY040  L     R6,AIO6               Locate where to put the spot               
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
BCPY045  LLC   R0,1(R6)              Point R6 to the next element               
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0               End of record                              
         JE    BCPY050                                                          
         CLI   0(R6),RCPOLOQ         X'0B' - normal spot?                       
         JL    BCPY045                                                          
         CLI   0(R6),RCPOTOQ         X'0C' - OTO spot?                          
         JH    BCPY050                                                          
         CLC   RDATE,RDATE-REGELEM(R6)  If date to add is greater               
         JH    BCPY045                  Then get next elem                      
BCPY050  GOTOR RECUP,DMCB,(C'S',AIOBUY),ELEM,(R6)                               
         J     BCPY030                                                          
         DROP  R2                                                               
*                                                                               
BCPYZERO TM    TACRFLAG,TACRZERO     Cost override of $0                        
         JNZ   BCPY036               Yes                                        
         J     BCPY040               No, don't set override flag                
*                                                                               
BCPYADDB L     R6,AIO6               LOCATE WHERE TO PUT THE SPOT               
         OI    BDSTAT3,BDST3_DSKADD  SET ADDED BY DESKTOP FOR DMDAPTRS          
         BRAS  RE,ADDACTEL                                                      
         BRAS  RE,ADDDTXEL                                                      
         BRAS  RE,BUYVAL                                                        
*                                                                               
         BRAS  RE,ADDBUYLN                                                      
*                                                                               
BCPYX    J     EXITY                                                            
BCPYDCH0 DC    H'0'                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD buyline that is in AIO6                                                   
***********************************************************************         
ADDBUYLN NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY           We'll start looking for next #             
         L     R6,AIO6                 starting from the line # in AIO6         
         USING BUYKEY,R6                                                        
         MVC   IOKEY(L'BUYKEY),0(R6)                                            
         MVC   HALF,IOKEY+BUYKBUY-BUYKEY   Adjust buyline # for SPTD            
         MVI   IOKEY+BUYKBUY-BUYKEY,0                                           
         MVC   IOKEY+BUYKBUY-BUYKEY+1(2),HALF   half has the line#              
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IODIR+IO5'  Use IO5 for this          
         BNE   ABYLNH0                                                          
ABYLNSEQ MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IODIR+IO5'  CHECK NEXT LINE           
         BE    ABYLN040                                                         
         TM    IOERR,IOEDEL        Test deleted                                 
         JNZ   ABYLN040            That is okay                                 
         DC    H'0'                                                             
ABYLN040 CLC   IOKEY(BUYKBUY-BUYKEY),IOKEYSAV  same upto line#?                 
         JNE   ABYLN050                        no, we have the line #           
         MVC   HALF,IOKEY+BUYKBUY-BUYKEY+1     Yes, new last line #             
         J     ABYLNSEQ                                                         
*                                                                               
ABYLN050 XR    RE,RE                 Next buyline #                             
         ICM   RE,3,HALF                                                        
         LA    RE,1(RE)                                                         
         CHI   RE,499                Greater than 499?                          
         BH    ABYLNH0                                                          
         STCM  RE,3,BUYKBUY                                                     
         DROP  R6                                                               
*                                                                               
         MVI   GBYACT,GBYADD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOFIL+IO6'                           
         BNE   ABYLNH0                                                          
*                                                                               
ABYLNX   J     EXITY                                                            
ABYLNH0  DC    H'0'                  Can't handle this gracefully yet           
         EJECT                                                                  
*===============================================================                
* Output success message                                                        
*===============================================================                
OUTSUCC  NTR1  BASE=*,LABEL=*                                                   
*&&WT*&& WTO   '*TEST* SL2B OUTSUCC!!'                                          
         MVI   SUCCCODE,C'1'       1=Success                                    
         LARL  R1,SUCRPL                                                        
         ST    R1,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY                                                            
*                                                                               
SUCRPL   LKOUT H                                                                
SUCRP1   LKOUT R,X'0010'                                                        
SucCod   LKOUT C,1,(D,B#WORKD,SUCCCODE),CHAR                                    
         LKOUT E                                                                
         LKOUT X                                                                
*===============================================================                
* COMMON SECTION WHERE WE HAVE A ACTION ERROR                                   
* THIS IS WHERE IT'LL GO THROUGH THE TSAR RECORDS AND                           
* CALL LINKIO TO BUILD ERROR RETURN BASED ON THOSE TSAR RECORDS MARKED          
* AS IN ERROR                                                                   
*===============================================================                
OUTVACT  NTR1  BASE=*,LABEL=*                                                   
*&&WT*&& WTO   '*TEST* SL2B OUTVACT we have action errors'                      
*                                                                               
         LARL  R1,VACERR                                                        
         ST    R1,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY                                                            
*                                                                               
VACERR   LKOUT H                                                                
VACER1   LKOUT R,X'0001'                                                        
Array    LKOUT C,X'0001',(A,ARYVAC)                                             
         LKOUT E                                                                
         LKOUT X                                                                
***********************************************************************         
* Array defintion for Action error response                                     
***********************************************************************         
ARYVAC   LKOUT A,(R,NXTVAC),MULTIROW=Y,ROWNAME=TOKACTD                          
Token    LKOUT C,1,TACRTOKN,CHAR                                                
ErrCod   LKOUT C,2,TACERRCD,UBIN                                                
ErrCat   LKOUT C,3,TACERRCT,CHAR                                                
         LKOUT E                                                                
***********************************************************************         
* THIS IS WHERE IT'LL GO THROUGH THE TSAR RECORDS AND                           
* CALL LINKIO TO BUILD ERROR RETURN BASED ON THOSE TSAR RECORDS MARKED          
* AS IN ERROR                                                                   
***********************************************************************         
NXTVAC   DS    0H                                                               
         L     R4,AIO8                                                          
         ST    R4,LP_ADATA                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST     Test first time?                           
         JE    NXVAC1ST              Yes, it is                                 
NXVACNXT GOTOR BUFFER,DMCB,('TOKACTQ',TSANXT),0                                 
NXVAC010 TM    BUFFRET,TSEEOF        Anymore entries?                           
         JNZ   NXVACEOF              No more                                    
         USING TOKACTD,R4                                                       
         OC    TACERRCD,TACERRCD     Any error in this TSAR record?             
         JZ    NXVACNXT                                                         
NXTVACX  J     EXITY                                                            
*                                                                               
NXVACEOF MVI   VPTREFLG,C'N'         DON'T NEED TO OUTPUT THESE NOW             
         OI    MISCFLG1,MF1VPTER     We had VALPTR errors                       
         J     NOMORE                                                           
*                                                                               
NXVAC1ST GOTOR BUFFER,DMCB,('TOKACTQ',TSAGET),1                                 
         J     NXVAC010                                                         
         EJECT                                                                  
*===============================================================                
* OUTPUT ALL THE POSSIBLE ERROR CODES AND THEIR TEXT                            
*===============================================================                
OUTPERR  NTR1  BASE=*,LABEL=*                                                   
*&&WT*&& WTO   '*TEST* SL2B OUTPERR all possible errors and text'               
         LARL  R1,OUTERR                                                        
         ST    R1,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY                                                            
*                                                                               
OUTERR   LKOUT H                                                                
OUTER1   LKOUT R,X'0002'                                                        
Array    LKOUT C,X'0002',(A,ARYERR)                                             
         LKOUT E                                                                
         LKOUT X                                                                
***********************************************************************         
* Array defintion for error codes and their text                                
***********************************************************************         
ARYERR   LKOUT A,(R,NXTERR),MULTIROW=Y,ROWNAME=ERRTABD                          
ErrCod   LKOUT C,1,ERTBCODE,UBIN                                                
ErrTxt   LKOUT C,2,ERTBTEXT,CHAR                                                
ErrCat   LKOUT C,3,(D,B#SAVED,ERRORCAT),CHAR                                    
         LKOUT E                                                                
***********************************************************************         
NXTERR   DS    0H                                                               
         L     R4,AIO8                                                          
         ST    R4,LP_ADATA                                                      
NXTERRD  USING ERRTABD,R4                                                       
*                                                                               
         CLI   LP_RMODE,LP_RFRST     Test first time?                           
         JE    NXERR1ST              Yes, it is                                 
*                                                                               
NXERR010 LARL  R6,ERRTABLE                                                      
         LR    R2,R6                                                            
         XR    RE,RE                                                            
         ICM   RE,3,DSPNXERR                                                    
         AR    R6,RE                                                            
*                                                                               
         AHI   R2,ERRTABX-ERRTABLE                                              
         CR    R6,R2                                                            
         JNL   NOMORE                                                           
*                                                                               
         OC    0(2,R6),0(R6)         End of a category?                         
         JZ    NXERRCAT              Yes                                        
*                                                                               
         USING ERRTABD,R6                                                       
         MVC   NXTERRD.ERTBCODE,ERTBCODE                                        
         MVC   NXTERRD.ERTBLTXT,ERTBLTXT                                        
         XC    NXTERRD.ERTBTEXT,NXTERRD.ERTBTEXT                                
         LLC   R3,ERTBLTXT           L'LENGTH + L'ERROR TEXT                    
         LR    R2,R3                                                            
         BCTR  R2,0                  -1 FOR L'LENGTH                            
         BCTR  R2,0                  -1 FOR EX INSTR                            
*                                                                               
         BASR  RE,0                                                             
         EX    R2,8(RE)                                                         
         J     *+10                                                             
         MVC   NXTERRD.ERTBTEXT(0),ERTBTEXT                                     
*                                                                               
         LA    R6,2(R3,R6)           R6 = A(NEXT ERROR) IN TABLE                
         LARL  R2,ERRTABLE                                                      
         SR    R6,R2                                                            
         STCM  R6,3,DSPNXERR         Calc disp to next for loop                 
         J     EXITY                                                            
*                                                                               
NXERRCAT OC    2(2,R6),2(R6)         End of all categories?                     
         JZ    NOMORE                Yes, no more errors                        
         MVC   ERRORCAT,2(R6)                                                   
         LA    R6,3(R6)                                                         
         J     NXERR030                                                         
*                                                                               
NXERR1ST LARL  R6,ERRTABLE                                                      
         MVC   ERRORCAT,0(R6)        Grab the first error category              
         LA    R6,1(R6)                                                         
*                                                                               
NXERR030 LR    R0,R6                                                            
         LARL  R2,ERRTABLE                                                      
         SR    R0,R2                                                            
         STCM  R0,3,DSPNXERR         1st error in category is 1 off             
         J     NXERR010                                                         
         DROP  R6,NXTERRD                                                       
         EJECT                                                                  
ERRTABD  DSECT                                                                  
ERTBCODE DS    XL2                   Error code                                 
ERTBLTXT DS    XL1                   L'(Error text)                             
ERTBTEXT DS    CL128                 Text of error                              
*                                                                               
SVRDEF   CSECT                                                                  
*                                                                               
ERRTABLE DS    0X                                                               
*************************************                                           
* Buyline level errors                                                          
*************************************                                           
         DC    C'B'                                                             
         DC    AL2(01),AL1(ERR001X-*)                                           
         DC    C'Buyline has been deleted'                                      
ERR001X  DS    0X                                                               
         DC    AL2(02),AL1(ERR002X-*)                                           
         DC    C'Spot length has changed'                                       
ERR002X  DS    0X                                                               
         DC    AL2(03),AL1(ERR003X-*)                                           
         DC    C'Cost indicators have changed'                                  
ERR003X  DS    0X                                                               
         DC    AL2(04),AL1(ERR004X-*)                                           
         DC    C'Buyline cost has changed'                                      
ERR004X  DS    0X                                                               
         DC    AL2(05),AL1(ERR005X-*)                                           
         DC    C'Program has changed'                                           
ERR005X  DS    0X                                                               
         DC    AL2(06),AL1(ERR006X-*)                                           
         DC    C'Adjacency code has changed'                                    
ERR006X  DS    0X                                                               
         DC    AL2(07),AL1(ERR007X-*)                                           
         DC    C'Days have changed'                                             
ERR007X  DS    0X                                                               
         DC    AL2(08),AL1(ERR008X-*)                                           
         DC    C'Daypart has changed'                                           
ERR008X  DS    0X                                                               
         DC    AL2(09),AL1(ERR009X-*)                                           
         DC    C'Start/end times have changed'                                  
ERR009X  DS    0X                                                               
         DC    AL2(10),AL1(ERR010X-*)                                           
         DC    C'Buy ID has changed'                                            
ERR010X  DS    0X                                                               
         DC    AL2(11),AL1(ERB011X-*)                                           
         DC    C'Maximum of 128 products per buyline has been exceeded'         
ERB011X  DS    0X                                                               
         DC    AL2(12),AL1(ERB012X-*)                                           
         DC    C'Maximum of 208 spots per buyline has been exceeded'            
ERB012X  DS    0X                                                               
         DC    AL2(13),AL1(ERB013X-*)                                           
         DC    C'Maximum of 499 buylines per station/estimate has '             
         DC    C'been exceeded'                                                 
ERB013X  DS    0X                                                               
         DC    AL2(14),AL1(ERB014X-*)                                           
         DC    C'Buyline values don''t match original'                          
ERB014X  DS    0X                                                               
         DC    AL2(15),AL1(ERB015X-*)                                           
         DC    C'Client doesn''t match for this buyline'                        
ERB015X  DS    0X                                                               
         DC    AL2(16),AL1(ERB016X-*)                                           
         DC    C'Invalid spot length'                                           
ERB016X  DS    0X                                                               
         DC    AL2(0)                   EOT for buyline level                   
*************************************                                           
* Week level errors                                                             
*************************************                                           
         DC    C'W'                                                             
         DC    AL2(01),AL1(ERW001X-*)                                           
         DC    C'A spot has been deleted from this week'                        
ERW001X  DS    0X                                                               
         DC    AL2(02),AL1(ERW002X-*)                                           
         DC    C'A spot has been added to this week'                            
ERW002X  DS    0X                                                               
         DC    AL2(03),AL1(ERW003X-*)                                           
         DC    C'This week includes paid spots'                                 
ERW003X  DS    0X                                                               
***************                                                                 
         DC    AL2(0)                   EOT for week level                      
*************************************                                           
* Spot level errors                                                             
*************************************                                           
         DC    C'S'                                                             
         DC    AL2(01),AL1(ERS001X-*)                                           
         DC    C'Spot date has changed'                                         
ERS001X  DS    0X                                                               
         DC    AL2(02),AL1(ERS002X-*)                                           
         DC    C'Spot cost override has changed'                                
ERS002X  DS    0X                                                               
         DC    AL2(03),AL1(ERS003X-*)                                           
         DC    C'Spot checksum has changed'                                     
ERS003X  DS    0X                                                               
         DC    AL2(04),AL1(ERS004X-*)                                           
         DC    C'Invalid product'                                               
ERS004X  DS    0X                                                               
         DC    AL2(05),AL1(ERS005X-*)                                           
         DC    C'Invalid spot cost override'                                    
ERS005X  DS    0X                                                               
         DC    AL2(0)                   EOT for spot level                      
*************************************                                           
* Pinergy level errors                                                          
*************************************                                           
         DC    C'P'                                                             
         DC    AL2(01),AL1(ERP001X-*)                                           
         DC    C'Need a trailer before starting another action'                 
ERP001X  DS    0X                                                               
         DC    AL2(02),AL1(ERP002X-*)                                           
         DC    C'No previous action for this record'                            
ERP002X  DS    0X                                                               
         DC    AL2(03),AL1(ERP003X-*),C'Delete spots action isn''t'             
         DC    C'allowed for a Copy Buyline request'                            
ERP003X  DS    0X                                                               
         DC    AL2(04),AL1(ERP004X-*)                                           
         DC    C'Only one client code is allowed per import file'               
ERP004X  DS    0X                                                               
         DC    AL2(05),AL1(ERP005X-*)                                           
         DC    C'Invalid client code'                                           
ERP005X  DS    0X                                                               
         DC    AL2(06),AL1(ERP006X-*)                                           
         DC    C'Not a media T buyline'                                         
ERP006X  DS    0X                                                               
         DC    AL2(07),AL1(ERP007X-*)                                           
         DC    C'Delete spots action needs at least one date'                   
ERP007X  DS    0X                                                               
         DC    AL2(0)                   EOT for Pinergy level                   
*********************************                                               
ERRTABX  DS    0X                                                               
         DC    AL2(0)                   EOT of all error levels                 
         EJECT                                                                  
*===============================================================                
* VALIDATE CLIENT CODE                                                          
*===============================================================                
                                                                                
VALCLT   NTR1  BASE=*,LABEL=*                                                   
         XC    SVBCLT(SVBVALSX-SVBCLT),SVBCLT                                   
         LA    R2,QDDSPTR                                                       
         USING DDSPTRD,R2                                                       
         GOTO1 VHEXIN,DMCB,DDSPMFKY,QMEDX,2                                     
         MVI   QMEDA,C'T'                                                       
         MVC   SVCLT,DDSPQCLT                                                   
         CLI   SVCLT+2,C'*'        SINCE EUGENE * PADS                          
         JNE   *+8                                                              
         MVI   SVCLT+2,C' '                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,SVCLT,3,SVBCLT                            
         BNE   EXIT                                                             
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Interface to TSAR (on-line) or BUFFERIN (off-line)                  *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/0   - Buffer number                                       *         
*          /1-3 - TSAR action code                                    *         
*        P2/0   - Key length (initialization call)                    *         
*          /2-3 - Record length (initialization call)                 *         
***********************************************************************         
* These 2 equates are now the same as we're going to use the same               
TOKPTRQ  EQU   1                   TOKEN AND DDSPTR                             
TOKACTQ  EQU   1                   TOKEN AND ACTION                             
                                                                                
BUFFER   NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1               R2=A(parameter list)                         
*                                                                               
         LA    R3,TSARBLK                                                       
         USING TSARD,R3            R3=A(TSAR control block)                     
         MVC   TSACTN,3(R2)        Set action code                              
         L     R1,AIO8                                                          
         ST    R1,TSAREC           Set A(record)                                
                                                                                
         CLI   TSACTN,TSAINI       Test initialization call                     
         JNE   BUFFER10                                                         
         XC    TSPRECN,TSPRECN     Reset # of TSAR records                      
         XC    TSPRECNX,TSPRECNX                                                
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
         OI    TSIND2,TSI2MANY     Going to get more than 64K records           
*                                  NOTE: ONLINE, 4MB is the Size Limit          
*                                                                               
         L     RE,LP_ATWA                                                       
         MVC   TSBUFFA,SVTSARB-TWAD(RE)                                         
         OC    TSBUFFA,TSBUFFA     If we already requested an area              
         JNZ   BUFFER10            Then we'll still use it                      
*                                                                               
*               TSBUFFA is zero so we can get as much as specified              
*                                      Not really using MINIO block             
*                                      R0 is the size in Megabytes              
         MVI   TSRECI,TSRMINB1+TSRXTN+TSRMGB                                    
         LHI   R0,40                in offline TSAR, 40MB                       
         STCM  R0,3,TSBUFFL                                                     
                                                                                
BUFFER10 CLI   TSACTN,TSAGET       Get a record?                                
         JNE   BUFFER20                                                         
         ICM   RE,15,4(R2)         Yes, caller set the nth rec to get           
         STCM  RE,7,TSRNUM3                                                     
*                                                                               
BUFFER20 GOTOR VTSAR,TSARD                                                      
         MVC   BUFFRET,TSERRS                                                   
         TM    BUFFRET,TSEINIF     Test initialization failure                  
         JNZ   BFFRDCH0                                                         
         L     RE,LP_ATWA                                                       
         MVC   SVTSARB-TWAD(L'SVTSARB,RE),TSBUFFA                               
         CLI   BUFFRET,0           Set condition code for caller                
         J     EXIT                                                             
BFFRDCH0 DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=================================================================              
* LOOK UP PRODUCT CODE IN CLIENT RECORD                                         
* NTRY:- R1=A(PRODUCT CODE)                                                     
* EXIT:- CC=LOW IF PRODUCT CODE IS NOT PRESENT                                  
*        CC=EQUAL IF PRODUCT CODE IS PRESENT AND CORRECT AND                    
*           R1=A(PRODUCT NUMBER)                                                
*        CC=HIGH IF PRODUCT CODE IS INVALID                                     
*=================================================================              
                                                                                
FINDPRD  CLI   QSPPRD,C'A'         TEST THERE IS A PRODUCT                      
         BLR   RE                  NO - EXIT WITH CC LOW                        
*                                                                               
         L     RF,ACLTREC          POINT TO CLIENT RECORD                       
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
FINDPRD2 CLC   QSPPRD,0(RF)        MATCH PRODUCT CODE                           
         JNE   FINDPRD4                                                         
         LLC   R1,3(RF)            FOUND - EXIT WITH CC EQUAL                   
         BR    RE                                                               
*                                                                               
FINDPRD4 AHI   RF,4                                                             
         CLI   0(RF),C' '                                                       
         JH    FINDPRD2                                                         
         CLI   QSPPRD,0            NOT FOUND - EXIT WITH NEQ                    
         BR    RE                                                               
         EJECT                                                                  
*================================================================               
* CALL BUYVAL TO VALIDATE BUY RECORD                                            
*================================================================               
                                                                                
BUYVAL   NTR1  BASE=*,LABEL=*                                                   
*****    GOTOR VCALLOV,DMCB,0,X'D9000A2A'  GET ADDRESS OF BUYVAL                
*****    L     RF,0(R1)                                                         
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING SPBUYVLD,R3                                                      
         MVC   SPBYAREC,AIOBUY                                                  
         MVC   SPBYAFAC,ACOMFACS                                                
*                                                                               
         L     RE,ACLTREC          POINT TO CLIENT RECORD                       
         LLC   R0,CPROF-CLTHDR(RE)                                              
*                                                                               
*****    GOTO1 (RF),(R1),SPBYLEN                                                
         GOTO1 SPBYVAL,DMCB,((R0),SPBYLEN)                                      
         CLI   SPBYERR,0                                                        
         JE    EXIT                                                             
         DCHO                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*===============================================================                
* GET THE DATE AND TIME FOR ACTIVITY                                            
*  OUPUT : WORK(2)   - BINARY DATE                                              
*          WORK+2(3) - TIME                                                     
*===============================================================                
                                                                                
GETDTTM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,WORK)                                      
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    GDT10                                                            
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTO1 VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTO1 VDATCON,DMCB,(0,DUB),(2,WORK)                                    
*                                                                               
GDT10    ICM   R1,15,PACKOF4B                                                   
         SRL   R1,4                GET RID OF SECONDS AND SIGN                  
         STCM  R1,7,WORK+2                                                      
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
*================================================                               
* INSERT ACTIVITY ELEMENT                                                       
*================================================                               
                                                                                
ADDACTEL NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ELEM                                                          
         USING ACTVELEM,R3                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'99'                                                       
         MVI   ELEM+1,12                                                        
         L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVADD,SECOPASS-SECD(R1)                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVADD+2)                                 
*                                                                               
         CLI   SVACTION,QACTADD    NO CHANGE DATE ON ADD                        
         BE    ADDACT10                                                         
         MVC   ACTVCHG,ACTVADD                                                  
         DROP  R3                                                               
*                                                                               
ADDACT10 L     R3,AIOBUY           POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
*================================================                               
* UPDATE DESKTOP TRANSFER ELEM                                                  
*================================================                               
ADDDTXEL NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOBUY                                                        
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,DTXCODEQ     REMOVE OLD TRANSFER ELEM                     
         MVI   ELCDHI,DTXCODEQ                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
                                                                                
         USING DTXELEM,R3                                                       
         XC    ELEM,ELEM           BUILD NEW TRANSFER ELEM                      
         LA    R3,ELEM                                                          
         MVI   DTXCODE,DTXCODEQ    x'9B'                                        
         MVI   DTXLEN,DTXSLNQ                                                   
                                                                                
         MVI   DTXSTAT,DTCX_PNG    x'04' CAME FROM CANADIAN PINERGY             
         MVC   DTXVER,VERSION      SAVE PC VERSION NUMBER                       
         BRAS  RE,GETDTTM                                                       
         MVC   DTXTIME,WORK+2      TIME LAST CHANGED                            
         DROP  R3                                                               
*                                                                               
ADDDT10  L     R3,AIOBUY           POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* ELEMENT MAINTENANCE ROUTINES                                                  
*=================================================================              
                                                                                
ADDEL    NTR1  ,                   ADD AN ELEMENT                               
         GOTOR RECUP,DMCB,(C'S',AIOBUY),ELEM,(RCUPRTRN,(R3))                    
         J     EXIT                                                             
                                                                                
DELEL    NTR1  ,                   DELETE AN ELEMENT                            
         GOTOR RECUP,DMCB,(C'S',AIOBUY),(R3),(R3)                               
         J     EXIT                                                             
                                                                                
NEXTEL   CLI   0(R3),0             LOCATE AN ELEMENT                            
         JE    NEXTELN                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R3)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
NEXTEL2  CLC   ELCDLO,0(R3)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         JL    NEXTEL                                                           
NEXTELY  CR    RE,RE                                                            
         BR    RE                                                               
NEXTELN  LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBALLY ADDRESSABLE THINGS **            
*                                                                               
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
EXITN    SR    RE,RE                                                            
         B     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
*                                                                               
MSGSTARL DC    H'70'                                                            
         DC    70C'*'                                                           
WTOTXT1L DC    H'70'                                                            
WTOTXT1  DC    70C' '                                                           
WTOTXT2L DC    H'70'                                                            
WTOTXT2  DC    70C' '                                                           
         EJECT                                                                  
***********************************************************************         
* REFORMULATE DDSPTR TO BE OUR ORIGINALLY SPEC'D FORMAT                         
***********************************************************************         
REFRMPTR NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TDDSPTR            R2 = A(INPUT DDSPTR)                       
*                                                                               
         XC    QDDSPTR,QDDSPTR       R3 = A(DDSPTR) HOW WE WANT IT              
         LA    R3,QDDSPTR                                                       
         USING DDSPTRD,R3                                                       
         MVC   DDSPQCLT,0(R2)        COPY CLIENT INTO OUR SPACE                 
         LA    R2,L'DDSPQCLT(R2)                                                
         CLI   DDSPQCLT+2,C'*'       DID WE GET A 2 CHAR CLIENT?                
         JNE   RFPTR010                                                         
         MVI   DDSPQCLT+2,C' '       FOR DDS, SPACEPAD THE 2 CHAR CLT           
         BCTR  R2,0                                                             
********                                                                        
RFPTR010 LA    R2,1(R2)              0(R2) POINTS INPUT ESTIMATE                
         CLI   1(R2),C'*'            ONLY A SINGLE DIGIT?                       
         JNE   RFPTR013                                                         
         MVC   DDSPQEST(2),=C'00'    ZERO FILL FROM LEFT FOR EST                
         MVC   DDSPQEST+2(1),0(R2)   COPY THE SINGLE DIGIT                      
         LA    R2,1(R2)              R2 POINTS TO THE * AFTER EST               
         J     RFPTR020                                                         
*                                                                               
RFPTR013 CLI   2(R2),C'*'            PERHAPS A 2 DIGIT ESTIMATE?                
         JNE   RFPTR016                                                         
         MVI   DDSPQEST,C'0'                                                    
         MVC   DDSPQEST+1(2),0(R2)                                              
         LA    R2,2(R2)              R2 POINTS TO THE * AFTER EST               
         J     RFPTR020                                                         
*                                                                               
RFPTR016 MVC   DDSPQEST,0(R2)                                                   
         LA    R2,3(R2)                                                         
********                                                                        
RFPTR020 LA    R2,2(R2)              WE HAVE ** AS SEPARATOR AFTER EST          
         MVC   DDSPQSTA,0(R2)                                                   
         LA    R2,L'DDSPQSTA(R2)                                                
         CLI   DDSPQSTA+3,C'*'                                                  
         JNE   RFPTR030                                                         
         MVI   DDSPQSTA+3,C' '                                                  
         BCTR  R2,0                                                             
*                                                                               
RFPTR030 LA    R2,1(R2)              SKIP THE LAST * SEPARATOR                  
*                                    COPY REST TO REST OF OURS                  
         MVC   DDSPMFKY(DDSPTRX-DDSPMFKY),0(R2)                                 
*                                                                               
RFPTRX   J     EXIT                                                             
*                                                                               
WTOTXT3L DC    H'132'                                                           
WTOTXT3  DC    132C' '                                                          
WTOTXT4L DC    H'132'                                                           
WTOTXT4  DC    132C' '                                                          
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(WORKL)                                                       
STAMPLIT DC    C'**SL2B**'                                                      
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
FILES    DS    0C                  ** System/File list **                       
         DC    C'SPOT   '                                                       
         DC    C'U'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'U'                                                             
SPTFIL   DC    C'SPTFIL '                                                       
         DC    C'U'                                                             
RECV1    DC    C'RECV1  '                                                       
         DC    C'U'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'U'                                                             
XSPFIL   DC    C'XSPFIL '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'X'                                                             
                                                                                
RECTAB   DS    0XL6                ** RECORD TABLE **                           
         DC    AL2(I#PNGCHG),AL3(PNGCHG),X'80'   PINERGY CHANGE ACTION          
         DC    AL2(I#PNGCPY),AL3(PNGCPY),X'80'   PINERGY COPY ACTION            
         DC    AL2(I#PNGDSP),AL3(PNGDSP),X'00'   PINERGY DELETE SPOTS           
         DC    AL2(I#PNGASP),AL3(PNGASP),X'00'   PINERGY ADD SPOTS              
         DC    AL2(I#PNGEOR),AL3(PNGEOR),X'00'   PINERGY END REQUEST            
         DC    AL2(I#PNGVPT),AL3(PNGVPT),X'00'   PINERGY ValidateDDSPtr         
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
*                                                                               
         EJECT                                                                  
TTOKEN   DC    C'TOKEN'                                                         
TBDSTART DC    C'BDSTART'                                                       
TBDEND   DC    C'BDEND'                                                         
TBDWKS   DC    C'BDWKS'                                                         
TBDINPUT DC    C'BDINPUT'                                                       
TBDWKIND DC    C'BDWKIND'                                                       
TBDDAY   DC    C'BDDAY'                                                         
TBDNOWK  DC    C'BDNOWK'                                                        
TBDSEC   DC    C'BDSEC'                                                         
TBDTIME  DC    C'BDTIME'                                                        
TBDCOSTP DC    C'BDCOSTP'                                                       
TBDDAYPT DC    C'BDDAYPT'                                                       
TBDTIMST DC    C'BDTIMST'                                                       
TBDTIMND DC    C'BDTIMEND'                                                      
TBDPROCD DC    C'BDPROG CODE'                                                   
TBDPROGM DC    C'BDPROGRM'                                                      
TBDPROGT DC    C'BDPROGT'                                                       
TBDCOST  DC    C'BDCOST'                                                        
TBDCIND  DC    C'BDCIND'                                                        
TBDNTAX  DC    C'BDNTAX'                                                        
TBDWHY3  DC    C'BDWHY3'                                                        
TBDREP   DC    C'BDREP'                                                         
TBDCHG   DC    C'BDCHG'                                                         
TBDWHY   DC    C'BDWHY'                                                         
TBDPURP  DC    C'BDPURP'                                                        
TBDSEDAY DC    C'BDSEDAY'                                                       
TBDCIND2 DC    C'BDCIND2'                                                       
TBCANAD  DC    C'BDCANAD'                                                       
TBDWHY2  DC    C'BDWHY2'                                                        
TBDSTAT  DC    C'BDSTAT'                                                        
TBDMGCOD DC    C'BDMGCODE'                                                      
TBDSTAT3 DC    C'BDSTAT3'                                                       
TBDNRGN  DC    C'BDNRGN'                                                        
TBDADVAG DC    C'BDADVAGY'                                                      
TBDMAST1 DC    C'BDMASPR1'                                                      
TBDMAST2 DC    C'BDMASPR2'                                                      
TBDSTAT2 DC    C'BDSTAT2'                                                       
TBDCON   DC    C'CONTRACT NUM'                                                  
TCMDATA1 DC    C'COM LINE 1'                                                    
TCMDATA2 DC    C'COM LINE 2'                                                    
TCMDATA3 DC    C'COM LINE 3'                                                    
TCMDATA4 DC    C'COM LINE 4'                                                    
TCMDATA5 DC    C'COM LINE 5'                                                    
*                                                                               
TBOOK    DC    C'BOOK'                                                          
TBKTYPE  DC    C'BOOKTYPE'                                                      
TPROGRAM DC    C'PROGRAM'                                                       
TDEMOVAL DC    C'DEMOVAL'                                                       
TAGYMKT  DC    C'AGENCY MKT'                                                    
TRETURN  DC    C'RETURN BUYS'                                                   
TRSMKT   DC    C'RTGSVC MKT'                                                    
TDEMOVER DC    C'FLAGS'                                                         
TLKUPMKT DC    C'LKUP MKT'                                                      
TLKUPSTA DC    C'LKUP STA'                                                      
TLKUPRSV DC    C'LKUP RTG SVC'                                                  
TPBDEM   DC    C'PBDEM'                                                         
TPBDEMOV DC    C'PBDEM FLAG'                                                    
TPBSDEM  DC    C'PBSDEM'                                                        
TPBSDMOV DC    C'PBSDEM FLAG'                                                   
TBSPLACT DC    C'BUY SPILL ACTION'                                              
TPSPLACT DC    C'POST BUY SPILL ACTION'                                         
                                                                                
TPROVCD  DC    C'PROV CODE'                                                     
TSTAPCT  DC    C'STAPCT'                                                        
TSTACOST DC    C'STACOST'                                                       
TSTACIN2 DC    C'STACOST IND2'                                                  
TSTAFLAG DC    C'STA FLAGS'                                                     
TSTAGST  DC    C'GST'                                                           
TSTAPST  DC    C'PST'                                                           
TENDBUY  DC    C'END BUY ?'                                                     
                                                                                
TBUWEEK  DC    C'WEEK START'                                                    
TSDDATE  DC    C'SPOT DATE'                                                     
TSDSTAT  DC    C'SPOT STATUS'                                                   
TSDPRD1  DC    C'PRD1'                                                          
TSDLEN1  DC    C'SLN1'                                                          
TSDPRD2  DC    C'PRD2'                                                          
TSDLEN2  DC    C'SLN2'                                                          
TSDCOST  DC    C'SPOT COST'                                                     
TSDMGCD  DC    C'MKGD CODE'                                                     
TSDCDAT  DC    C'CLEAR DATE'                                                    
TSDCSEQ  DC    C'CLEAR SEQNUM'                                                  
TSDADAT  DC    C'AFFID DATE'                                                    
TSDATIM  DC    C'AFFID TIME'                                                    
TSDADAY  DC    C'AFFID DAY'                                                     
TSDAFL1  DC    C'AFFID FILM 1'                                                  
TSDAFL2  DC    C'AFFID FILM 2'                                                  
TSDDFLM  DC    C'DEALER TAG SEQNUM'                                             
TSDDTAG  DC    C'DEALER TAG NUMBER'                                             
TSDDDATE DC    C'TRAFFIC INST DATE'                                             
TSDTFL1  DC    C'TRAFFIC FILM 1'                                                
TSDTFL2  DC    C'TRAFFIC FILM 2'                                                
TSDTPAT  DC    C'TRAFFIC PTTN REF'                                              
TSPOTS   DC    C'NUMBER OF SPOTS'                                               
         EJECT                                                                  
B#AGYREC EQU   2                   IO1 1K - AGENCY RECORD                       
B#CLTREC EQU   3                   IO2 2K - CLIENT RECORD                       
B#MKTREC EQU   4                   IO3 6K - MARKET RECORD                       
B#ESTREC EQU   5                   IO4 4K - ESTIMATE RECORD                     
B#BUYREC EQU   6                   IO5 6K - BUY RECORD                          
*                                  IO6 6K - EQUATE OF 7                         
*                                  IO7 2K - EQUATE OF 8                         
*                                  IO8 2K - EQUATE OF 9                         
*                                                                               
*============================================================                   
* PINERGY CHANGE Buyline Request                                                
*============================================================                   
                                                                                
PNGCHGR  LKREQ H,I#PNGCHG,NEWREC=Y             MAPCODE 2A3                      
                                                                                
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
DDSPtr   LKREQ F,2,(D,B#WORKD,TDDSPTR),CHAR,TEXT=SP#PNTR                        
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* PINERGY COPY Buyline Request                                                  
*============================================================                   
                                                                                
PNGCPYR  LKREQ H,I#PNGCPY,NEWREC=Y             MAPCODE 2A4                      
                                                                                
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
DDSPtr   LKREQ F,2,(D,B#WORKD,TDDSPTR),CHAR,TEXT=SP#PNTR                        
SpotLen  LKREQ F,3,(D,B#SAVED,BUSEC),LBIN,TEXT=(*,TBDSEC)                       
Rate     LKREQ F,4,(D,B#SAVED,BUCOST),CBIN,TEXT=(*,TBDCOST)                     
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* PINERGY Delete Spots Action                                                   
*============================================================                   
                                                                                
PNGDSPR  LKREQ H,I#PNGDSP,NEWREC=Y             MAPCODE 2A5                      
                                                                                
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
SpDate   LKREQ F,2,(I,B#SAVED,I$DSPDTE),CDAT,OLEN=L'$SDDATE,           X        
               TEXT=(*,TSDDATE),ARRAY=*                                         
         LKREQ E                                                                
                                                                                
*============================================================                   
* PINERGY Add Spot Action                                                       
*============================================================                   
                                                                                
PNGASPR  LKREQ H,I#PNGASP,NEWREC=Y             MAPCODE 2A6                      
                                                                                
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
NthSpot  LKREQ F,2,(D,B#SAVED,QNSPOT),UBIN,TEXT=SP#SPREF                        
SpDate   LKREQ F,3,(D,B#SAVED,QSPTDATE),CDAT,TEXT=(*,TSDDATE)                   
CostOvrd LKREQ F,4,(D,B#SAVED,QSPCOST),CPAK,TEXT=(*,TBDCOST)                    
Brand    LKREQ F,5,(D,B#SAVED,QSPPRD),CHAR,TEXT=(*,TSDPRD1)                     
         LKREQ E                                                                
                                                                                
*============================================================                   
* END OF PINERGY REQUEST                                                        
*============================================================                   
                                                                                
PNGEORR  LKREQ H,I#PNGEOR,NEWREC=Y             MAPCODE 2A7                      
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
         LKREQ E                                                                
                                                                                
*============================================================                   
* PINERGY VALIDATE DDSPTR                                                       
*============================================================                   
                                                                                
PNGVPTR  LKREQ H,I#PNGVPT,NEWREC=Y             MAPCODE 2A8                      
                                                                                
Token    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
DDSPtr   LKREQ F,2,(D,B#WORKD,TDDSPTR),CHAR,TEXT=SP#PNTR                        
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
*&&DO                                                                           
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGY                                                    
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         MVC   L.LOCKMED,SVMED                                                  
         MVC   L.LOCKCLT,SVCLT                                                  
         MVC   L.LOCKSTA,SVSTA                                                  
         MVC   L.LOCKSTA+4(1),SVMED  SET MEDIA TO MATCH LOCK DATA               
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGY                                                    
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,SVMED                                                  
         MVC   L.LOCKCLT,SVCLT                                                  
         SR    R0,R0                                                            
         IC    R0,SVBEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVMED,C'T'          THIS ONLY WORKS FOR CANADA!                  
         BE    *+12                                                             
         CLI   SVMED,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
*&&                                                                             
         LTORG                                                                  
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPLNKRECS                                                      
         ORG   $SDAV2                                                           
$SDSPOTS DS    XL1                                                              
$SDDX2   EQU   *                                                                
                                                                                
MAXRECLN EQU   3972                SPTFIL MAXIMUM RECORD LENGTH                 
                                                                                
SAVED    DSECT                                                                  
                                                                                
STAMP    DS    CL(L'STAMPLIT)      OVERLAY STAMP                                
                                                                                
SPBYVAL  DS    A                   A(SPBUYVAL)                                  
LINKIO   DS    A                   A(LINKIO)                                    
MASTC    DS    A                   A(MASTC)                                     
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
RECUP    DS    A                   A(RECUP)                                     
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
CKSUMBFR DS    F                                                                
*                                                                               
AGY      DS    CL(L'LP_AGY)        AGENCY CODE                                  
MAPNUM   DS    XL(L'LP_QMAPN)      RECORD MAP NUMBER                            
VERSION  DS    XL(L'LP_VRSN)       PC VERSION NUMBER                            
DSPNXERR DS    XL2                 Disp tp next error                           
*                                                                               
ERRORCAT DS    C                                                                
ERCATBUY EQU   C'B'                Buyline level error category                 
ERCATPIN EQU   C'P'                Pinergy level error category                 
ERCATSPT EQU   C'S'                   Spot level error category                 
ERCATWK  EQU   C'W'                   Week level error category                 
*                                                                               
ERRORFLG DS    X                                                                
FATALERR EQU   C'F'                FATAL ERROR, EXIT                            
ERRRGULR EQU   C'R'                REGULAR ERROR                                
ERRSTLCK EQU   C'L'                STATION IS LOCKED                            
*                                                                               
BUFFRET  DS    XL1                                                              
*                                                                               
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVACTION DS    C                   SAVED THE ORIGINAL REQUEST ACTION            
*                                                                               
VPTREFLG DS    XL1                 ERROR IN VALIDATE POINTERS?                  
VACTEFLG DS    XL1                 ERROR IN VALIDATE ACTIONS?                   
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS 1                        
MF1TSRVP EQU   X'80'                - TSAR SETUP TO VALIDATE POINTERS           
MF1TSRVA EQU   X'40'                - TSAR SETUP TO VALIDATE ACTIONS            
MF1NDTLR EQU   X'02'                - STARTED AN ACTION, NEED A TLR             
MF1VPTER EQU   X'01'                - We had VALPTR errors                      
*                                                                               
SVMED    DS    CL1                 MEDIA LETTER                                 
SVCLT    DS    CL3                 CLIENT CODE                                  
*                                                                               
SVBVALS  EQU   *                                                                
SVBAGYMD DS    XL1                 MEDIA CODE                                   
SVBCLT   DS    XL2                 CLIENT PACKED                                
SVBEST   DS    X                   ESTIMATE NUMBER                              
SVBLIN   DS    XL2                 BUY LINE NUMBER                              
SVBMKT   DS    XL2                 MARKET NUMBER                                
SVBSTA   DS    XL3                 STATION PACKED                               
SVSTA    DS    CL8                 STATION CALL LETTERS                         
SVKEY    DS    XL20                BUY KEY                                      
SVSPDATE DS    XL2                 SPOT DATE                                    
SVNETWK  DS    CL4                 NETWORK CALL LETTERS                         
SVNETBTS DS    XL1                 NETWORK BINARY                               
SVMKTSTA DS    XL5                                                              
SVSFLAG1 DS    XL1                 SAVED STATION FLAG1                          
*                                                                               
SVBVALSX EQU   *                                                                
                                                                                
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
                                                                                
         DS    0D                                                               
XPRDLIST DS    XL256               PRODUCT CODES IN EXISTING BUYREC             
*                                  NEW PRODUCTS GO IN IOBRDLST                  
                                                                                
BUWEEK   DS    XL2                 SPOT WEEK START DATE                         
BUWEEKX  DS    XL2                 SPOT WEEK END DATE                           
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
SAVE1OR2 DS    X                                                                
*                                                                               
BUMASPR1 DS    CL3                                                              
BUMASPR2 DS    CL3                                                              
*                                                                               
DEMQMKT  DS    CL(L'NDMKTALF)                                                   
DEMQSTA  DS    CL5                                                              
DEMQRSV  DS    CL1                                                              
*                                                                               
BUSPLAMK DS    XL2                                                              
BUSPLRMK DS    XL2                                                              
BUSPLBTY DS    CL1                                                              
*                                                                               
PACKOF4B DS    PL4                                                              
*                                                                               
TTLCLCST DS    F                                                                
*                                                                               
BUCOST   DS    F                                                                
BUSEC    DS    X                                                                
*                                                                               
I$DSPDTE DS    0A                  A(Delete spot dates)                         
DDTEIND  DS    AL1                                                              
ADSPDTE  DS    AL3                                                              
*                                                                               
         DS    0D                  RE-USABLE FIELDS FOR LKREQS                  
MYDUB    DS    D                                                                
MYDUB2   DS    D                                                                
MYFULL   DS    F                                                                
MYFULL2  DS    F                                                                
MYHALF   DS    H                                                                
MYHALF2  DS    H                                                                
MYBYTE   DS    X                                                                
MYBYTE2  DS    X                                                                
RCUPRTRN DS    X                   RECUP RETURN FLAG                            
                                                                                
QACTION  DS    C                   ** ACTION CODE **                            
QACTADD  EQU   C'A'                ADD A NEW BUY                                
QACTOVER EQU   C'O'                OVERWRITE A DELETED BUY                      
QACTCHA  EQU   C'C'                CHANGE AN EXISTING BUY                       
QACTDEL  EQU   C'D'                DELETE AN EXISTING BUY                       
QBEST    DS    X                   ESTIMATE NUMBER                              
QLIN     DS    XL2                 BUY LINE NUMBER                              
QMKT     DS    XL2                                                              
QSTA     DS    CL8                                                              
ASPCOUNT DS    XL2                 Add spot counter                             
CPYCOUNT DS    XL2                 Copy action counter                          
QDSKADDR DS    F                                                                
QTOKEN   DS    CL32                                                             
QDDSPTR  DS    CL128                                                            
PDDSPTR  DS    CL128               TO SAVE ON IOS                               
PPTRLLER DS    XL2                 Prev pointer line error code                 
PPTRLLEC DS    CL1                              line error category             
PRVMCPSE DS    XL10                Previous med,clt,prd,sta,est                 
MCPSEBLN DS    XL2                                                              
SVBUYACT DS    CL1                 Saved buy action, C'B' or C'C'               
SVBUSEC  DS    XL1                 Saved new buyline spot length                
SVBUCOST DS    XL4                 Saved new buyline cost                       
QNSPOT   DS    X                   nth spot                                     
QSPTDATE DS    XL2                 Spot date                                    
QSPCOST  DS    PL8                 Spot cost override                           
QSPPRD   DS    CL3                 Product allocation                           
QENDBUY  DS    C                                                                
*                                                                               
         DS    0D                                                               
SAVEDX   EQU   *                                                                
         EJECT                                                                  
* INCLUDED DSECTS FOLLOW                                                        
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
***************                                                                 
* FOR x02A8 - DDSPtr validation                                                 
***************                                                                 
TOKPTRD  DSECT ,                   ** Token and DDSPtr **                       
TPBKEY   DS    0X                  ** Buffer key **                             
TPBTOKN  DS    XL(L'QTOKEN)        PC Token                                     
TPBKEYL  EQU   *-TPBKEY                                                         
TPBERRCT DS    CL1                 Error category                               
TPBERRCD DS    XL2                 Error code                                   
TPBRECL  EQU   *-TOKPTRD                                                        
***********************************                                             
TOKACTD  DSECT ,                   ** Token and Actions **  TNUM based          
TACKEY   DS    0X                  ** BUFFER KEY **                             
TACKMFKY DS    XL13                MFKEY OF BUYLINE                             
TACKCHCP DS    CL1                 CHANGE OR COPY                               
TACKCPSQ DS    XL2                 Seq # used to identify which copy            
***************                                                                 
* NEW SPOT LENGTH AND NEW COST ONLY USED FOR COPY, OTHERWISE NULLS              
***************                                                                 
TACKNSLN DS    XL1                 NEW SPOT LENGTH                              
TACKNCST DS    XL4                 NEW COST                                     
***************                                                                 
* INDICATOR                                                                     
* 0 - Delete spot for Change action                                             
*   - NULL        for Copy action                                               
* 1 - Add spot for both change and copy actions                                 
* 9 - Trailer record for action                                                 
***************                                                                 
TACKIND  DS    XL1                 INDICATOR                                    
TACKDATE DS    XL2                 DATE OF SPOT(S)                              
TACKSPSQ DS    XL2                 Seq # used to identify spots                 
TACKEYL  EQU   *-TACKEY                                                         
*                                                                               
TACREC   DS    0X                                                               
TACRCSOV DS    XL4                 COST OVERRIDE (NULLS FOR DELETE)             
TACRFLAG DS    XL1                   FLAG                                       
TACRZERO EQU   X'80'                 - zero cost override                       
TACRBPRD DS    XL1                 PRODUCT ALLOC (NULLS FOR DELETE)             
*                                                                               
TACRTOKN DS    XL(L'QTOKEN)        PC TOKEN (ALL HAVE A TOKEN)                  
TACERRCT DS    CL1                 Error category                               
TACERRCD DS    XL2                 Error code                                   
TACRECL  EQU   *-TACKEY                                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMSGEQUS                                                      
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
*                                                                               
SLNTABD  DSECT                     ** SPOT LENGTH TABLE **                      
STABLEN  DS    X                   SPOT LENGTH                                  
STABEQV  DS    X                   SPOT LENGTH EQUIVALENT                       
SLNTABQ  EQU   *-SLNTABD                                                        
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPBUYVALD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
DDSPTRD  DSECT                                                                  
DDSPQCLT DS    CL3                 EBCDIC CLIENT                                
         DS    CL1                     * SEPARATOR                              
DDSPQEST DS    CL3                 EBCDIC ESTIMATE                              
         DS    CL2                    ** SEPARATOR                              
DDSPQSTA DS    CL4                 EBCDIC STATION                               
         DS    CL1                     * SEPARATOR                              
DDSPMFKY DS    CL26                MF KEY IN HEX CHAR                           
         DS    CL4                     SPARE                                    
DDSPSLN  DS    CL2                 BYLN SPOT LENGTH IN HEX CHAR                 
DDSPTMUN DS    CL1                 TIME UNITS (DEFAULT S FOR SECONDS)           
DDSPGRSD DS    CL8                 GROSS DOLLARS IN HEX CHAR                    
DDSPCIND DS    CL4                 2 BYTES OF COST IND IN HEX CHAR              
         DS    CL12                TOTAL GROSS IN HEX CHAR                      
         DS    CL2                 TOTAL # OF SPOTS IN HEX CHAR                 
*                                                                               
DDSPNSPT DS    CL2                 Nth spot for date in HEX char                
DDSPTSPT DS    CL2                 Total # spots for date in HEX char           
DDSPTDAT DS    CL10                Total cost for date in HEX char              
DDSPSDAT DS    CL6                 DATE OF SPOT IN YYMMDD FORMAT                
DDSPSCST DS    CL8                 COST OF SPOT IN HEX CHAR                     
DDSPMCKS DS    CL27                MULITPLE CHECKSUMS                           
DDSPTRX  DS    0X                                                               
DDSPTRL  EQU   *-DDSPTRD                                                        
*                                                                               
LINCKSMD DSECT                     A CALCULATED BUYLINE CHECKSUM                
LCKSMPRG DS    XL2                 USING PROGRAM NAME                           
LCKSMADJ DS    XL1                       ADJACENCY CODE                         
LCKSMDYS DS    XL1                       DAYS                                   
LCKSMDPT DS    XL1                       DAYPART                                
LCKSMTIM DS    XL2                       START AND END TIMES                    
LCKSMIDL DS    XL2                       ID ELEMENT                             
LINCKSML EQU   *-LINCKSMD          LENGTH OF LINE LEVEL CHECKSUM                
SPTECKSM DS    XL2                 SPOT ELEMENT CHECKSUM                        
LINCKSMX DS    0X                                                               
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVERWORK            REDEFINE 1K OVERLAY WORKING STORAGE          
AIOBUY   DS    A                                                                
TDDSPTR  DS    CL128               Temporary DDSPtr                             
LINCKSM  DS    XL(LINCKSMX-LINCKSMD)                                            
TSARBLK  DS    XL(TSPXTNL)         TSAR BUFFER 1                                
SUCCCODE DS    CL1                 Success code                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPLNK2B   09/12/17'                                      
         END                                                                    
