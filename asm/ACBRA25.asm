*          DATA SET ACBRA25    AT LEVEL 008 AS OF 04/24/15                      
*PHASE T62425A                                                                  
                                                                                
ACBRA25  TITLE '- BrandOcean Resources Upload Server'                           
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 12FEB07 New application server                                       
* NSHE 005 05AUG10 Move PTCERR routine into overlay                             
* SMAN 006 28SEP11 BR44108L Bug fix to prevent bad resource KSTELDs             
* NSHE 007 03SEP12 Change IO routine for auto switching system                  
* NSHE 008 24Apr15 Remove SETFAC                                                
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=1000,REQUEST=*,WORKERKEY=ACBO,   *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ                   
                                                                                
ENTRY    NMOD1 0,**BO25**,RR=RE                                                 
         LR    RC,R1                                                            
         USING LP_D,RC                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  R5,GLOBALS                                                       
         USING GLOBALS,R5          RA=A(GLOBAL LITERALS)                        
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         ST    R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         B     INITUL                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* Initialise Upload                                                   *         
***********************************************************************         
                                                                                
INITUL   CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
         GOTOR GOIO                inactive - for pending action                
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   GIND2,GI2RSRC                                                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
         MVC   AGENCY,CUXCPY                                                    
                                                                                
INIT02   L     R0,AIO4                                                          
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR I/O AREA 4 (RECORD ID TABLE)           
                                                                                
         L     R1,ALP              RESTORE A(LP_D)                              
         MVC   AALIOB,LP_ALIOB     EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Process and Upload Record                                           *         
***********************************************************************         
                                                                                
INPUT    BASR  RF,0                                                             
         AHI   RF,RECTAB-*                                                      
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         BE    INPUT02                                                          
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                -> unknown record type                       
                                                                                
INPUT02  MVC   RECTYPE,RECTTYPE    -> set known record type                     
                                                                                
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR UPDREC              PROCESS THE INPUT RECORD                     
                                                                                
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Go to Upload Record handling routine                                *         
***********************************************************************         
                                                                                
UPDREC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR (#SYSCHK,ASYSCHK)                                                
         BE    UPDREC2                                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$FLRD                                                   
         J     EXITN                                                            
                                                                                
UPDREC2  XR    RF,RF                                                            
         IC    RF,RECTYPE                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         CHI   RF,UPDTABL                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
         B     UPDTAB(RF)                                                       
                                                                                
UPDTAB   DS    0XL4                                                             
         J     UPDRES              Resources                                    
         J     UPDOHD              Out of office - header                       
         J     UPDOIT              Out of office - item                         
         J     UPDOTR              Out of office - trailer                      
UPDTABL  EQU   *-UPDTAB                                                         
                                                                                
UPDRECY  J     EXITY                                                            
                                                                                
UPDRECN  J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Resources Upload Module                                             *         
***********************************************************************         
                                                                                
UPDRES   BASE  ,                                                                
         MVI   TWAMODE,0                                                        
         LA    R0,DR_VALS          Clear derived values                         
         LHI   R1,DR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    XERRTAB,XERRTAB     clear error table                            
         LA    R1,XERRTAB                                                       
         ST    R1,XANXTER                                                       
*                                                                               
         CLC   QR_SJCLI,SPACES     Test client given                            
         JH    URES010                                                          
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
*                                                                               
URES010  MVC   TEMP2(13),QR_SJCLI                                               
         GOTOR VTSCPJ                                                           
         BE    URES012                                                          
         GOTOR SAVERR,DMCB,ROUERRV,(13,TEMP2)                                   
*                                                                               
URES012  TM    RS#ICPJ,RS#IJOB     ONLY GET JOB STATUS IF JOB                   
         BZ    URES020                                                          
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         MVI   DR_JBST,C'1'        DEFAULT OPEN                                 
         TM    ACTRSTAT,ACTSLOCK                                                
         BZ    URES014                                                          
         MVI   DR_JBST,C'4'        LOCKED                                       
URES014  TM    ACTRSTAT,ACTSCLOS                                                
         BZ    URES016                                                          
         MVI   DR_JBST,C'5'        CLOSED                                       
URES016  TM    ACTRSTAT,ACTSDRFT                                                
         BZ    URES020                                                          
         MVI   DR_JBST,C'2'        DRAFT                                        
         LA    R3,ACTRFST                                                       
         USING JOBELD,R3                                                        
         SR    R0,R0                                                            
URES018  CLI   JOBEL,0                                                          
         BE    URES020                                                          
         CLI   JOBEL,JOBELQ                                                     
         BE    URES019                                                          
         IC    R0,JOBLN                                                         
         AR    R3,R0                                                            
         B     URES018                                                          
*                                                                               
URES019  TM    JOBSTA2,JOBSREJ                                                  
         BZ    URES020                                                          
         MVI   DR_JBST,C'3'        REJECTED                                     
                                                                                
URES020  MVI   DR_NAML,FF                                                       
         CLC   QR_NAME,SPACES                                                   
         BNH   URES026                                                          
         LA    RE,QR_NAME+L'QR_NAME-1                                           
         LA    RF,L'QR_NAME                                                     
URES022  CLI   0(RE),C' '                                                       
         BH    URES024                                                          
         SHI   RE,1                                                             
         BCT   RF,URES022                                                       
         DC    H'0'                                                             
URES024  SHI   RF,1                                                             
         STC   RF,DR_NAML                                                       
         OC    QR_CAMP,QR_CAMP                                                  
         BNZ   URES030                                                          
         GOTOR BLDCAMP                                                          
         B     URES050                                                          
*                                                                               
URES026  OC    QR_CAMP,QR_CAMP     have we a campaign                           
         BZ    URES052             no - skip validation and job update          
*        MVC   ROUERRV,=AL2(AE$ICAMN)                                           
*        GOTOR SAVERR,DMCB,ROUERRV,0                                            
*                                                                               
URES030  GOTOR VALCAMP                                                          
         BE    URES050                                                          
         GOTOR SAVERR,DMCB,ROUERRV,(13,TEMP2)                                   
         B     URES050                                                          
URES050  GOTOR UPDJOB                                                           
         BE    URES052                                                          
         GOTOR SAVERR,DMCB,ROUERRV,(10,TEMP2)                                   
URES052  MVI   DR_NXTL,FF                                                       
         CLC   QR_NXT,SPACES                                                    
         BNH   URES060                                                          
         LA    RE,QR_NXT+L'QR_NXT-1                                             
         LA    RF,L'QR_NXT                                                      
URES056  CLI   0(RE),C' '                                                       
         BH    URES058                                                          
         SHI   RE,1                                                             
         BCT   RF,URES056                                                       
         DC    H'0'                                                             
URES058  STC   RF,DR_NXTL                                                       
*                                                                               
URES060  L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BLDELS              BUILD ELEMENTS IN GENAREA                    
*                                                                               
         GOTOR GETWRK              GET RESOURCES WORK RECORD                    
                                                                                
         TM    TWAMODE,TWAMERP                                                  
         BNZ   UPDRESX                                                          
         CLI   QR_STAT,QR_FINSQ                                                 
         BNE   URES062                                                          
         MVI   NEWSTAT,RWKSENDD                                                 
         B     URES066                                                          
URES062  CLI   QR_STAT,QR_CURRQ                                                 
         BNE   URES064                                                          
         MVI   NEWSTAT,0                                                        
         B     URES066                                                          
URES064  CLI   QR_STAT,QR_OBSOQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NEWSTAT,RWKSDEAD                                                 
URES066  GOTOR DOSTAT              CREATE STATUS CHANGE ELEMENTS                
         BE    URES068                                                          
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         OI    TWAMODE,TWAMUWD                                                  
                                                                                
URES068  LH    R1,QR_INDX                                                       
         AHI   R1,1                                                             
         STH   R1,DR_INDX                                                       
                                                                                
         GOTOR UPDAUD              USES STCELS HELD IN AIO4 AND AIO5            
         BE    URES070                                                          
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         OI    TWAMODE,TWAMUWD                                                  
         B     URES050                                                          
*                                                                               
URES070  GOTOR UPDRWK              USES NEW ELEMENTS IN GENAREA TO              
*                                  BUILD RESOURCES WORK RECORD                  
                                                                                
*                                                                               
*** Global exists for Resources Upload Server *************************         
                                                                                
         USING LIOBD,R4                                                         
UPDRESX  L     R4,AALIOB           after updates pass return data               
         TM    TWAMODE,TWAMERP+TWAMEDP+TWAMUWD exit on all errors               
         BNZ   UPDRSX10                                                         
                                                                                
         CURED (4,DR_CPN#),(8,DUB),0,ALIGN=LEFT                                 
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CAMP#),    +        
               ('LD_CHARQ',DUB),(L'DUB,0)                                       
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#JBSTA),    +        
               ('LD_CHARQ',DR_JBST),(L'DR_JBST,0)                               
         J     UPDRSX20                                                         
*                                                                               
UPDRSX10 GOTOR PRCERR,DMCB,XERRTAB,ELEMENT                                      
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERR),      +        
               ('LD_CHARQ',ELEMENT),(BYTE1,0)                                   
         TM    TWAMODE,TWAMUWD exit on all errors                               
         BZ    UPDRSX20                                                         
         OI    GIND1,GIUNWIND  DDLINK will unwind/abend                         
         J     UPDRESN                                                          
                                                                                
UPDRSX20 LA    R0,QR_VALS                                                       
         LA    R1,QR_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECY                                                          
                                                                                
UPDRESN  LA    R0,QR_VALS                                                       
         LA    R1,QR_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITN                                                            
         DROP  RB                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* OUT OF OFFICE - HEADER                                              *         
***********************************************************************         
UPDOHD   BASE  ,                                                                
         MVI   TWAMODE,0                                                        
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RF,AGENAREA                                                      
         ST    RF,SVGENADR                                                      
         J     UPDOUTX                                                          
                                                                                
***********************************************************************         
* OUT OF OFFICE - ITEM                                                *         
***********************************************************************         
UPDOIT   BASE  ,                                                                
         MVI   TWAMODE,0                                                        
         LA    R0,DR_VALS          Clear derived values                         
         LHI   R1,DR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    XERRTAB,XERRTAB     clear error table                            
         LA    R1,XERRTAB                                                       
         ST    R1,XANXTER                                                       
         MVC   TEMP2(L'SA0KNUM),QR_PIDB                                         
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         BE    UOIT002                                                          
         MVC   ROUERRV,=AL2(AE$INPID)  INVALID PID                              
         XOUT  TEMP2,XERRTXT,2                                                  
         GOTOR SAVERR,DMCB,ROUERRV,(4,XERRTXT)                                  
*                                                                               
UOIT002  MVI   DR_NXTL,FF                                                       
         CLC   QR_NXT,SPACES                                                    
         BNH   UOIT008                                                          
         LA    RE,QR_NXT+L'QR_NXT-1                                             
         LA    RF,L'QR_NXT                                                      
UOIT004  CLI   0(RE),C' '                                                       
         BH    UOIT006                                                          
         SHI   RE,1                                                             
         BCT   RF,UOIT004                                                       
         DC    H'0'                                                             
UOIT006  STC   RF,DR_NXTL                                                       
                                                                                
UOIT008  L     R4,SVGENADR                                                      
         USING OUTTABD,R4                                                       
         MVC   OUTACT,QR_ACTN                                                   
         MVC   OUTPID,QR_PIDB                                                   
         MVC   OUTSTRDT,QR_STDT                                                 
         MVC   OUTSTRAM,QR_STAM                                                 
         MVC   OUTENDDT,QR_ENDT                                                 
         MVC   OUTENDAM,QR_ENAM                                                 
         MVC   OUTTYPE,QR_TYPE                                                  
         XR    RE,RE                                                            
         CLI   DR_NXTL,FF                                                       
         JE    UOIT010                                                          
         IC    RE,DR_NXTL                                                       
         SHI   RE,1                                                             
         MVC   OUTCOMNT(0),QR_NXT                                               
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
UOIT010  AHI   RE,OUTTLN1Q                                                      
         STC   RE,OUTLEN                                                        
         AR    R4,RE                                                            
         ST    R4,SVGENADR                                                      
         J     UPDOUTX                                                          
                                                                                
***********************************************************************         
* OUT OF OFFICE - TRAILER                                             *         
***********************************************************************         
UPDOTR   BASE  ,                                                                
         L     R4,AGENAREA                                                      
         USING OUTTABD,R4                                                       
UOTR002  CLI   OUTLEN,0                                                         
         JE    UOTR100                                                          
         CLI   OUTACT,OUTAADD                                                   
         JNE   UOTR010                                                          
         GOTOR ADDOUT,DMCB,(R4)                                                 
         JE    UOTR020                                                          
         DC    H'0'                                                             
                                                                                
UOTR010  CLI   OUTACT,OUTADEL                                                   
         JE    UOTR012                                                          
         DC    H'0'                                                             
UOTR012  GOTOR DELOUT,DMCB,(R4)                                                 
         JE    UOTR020                                                          
         DC    H'0'                                                             
                                                                                
UOTR020  XR    RE,RE                                                            
         IC    RE,OUTLEN                                                        
         AR    R4,RE                                                            
         J     UOTR002                                                          
                                                                                
UOTR100  J     UPDOUTX                                                          
                                                                                
                                                                                
*** Global exists for Out of Office Upload Server ***                           
                                                                                
         USING LIOBD,R4                                                         
UPDOUTX  L     R4,AALIOB           after updates pass return data               
         TM    TWAMODE,TWAMERP+TWAMEDP+TWAMUWD exit on all errors               
         BNZ   UPDOUX10                                                         
                                                                                
         CURED (4,DR_CPN#),(8,DUB),0                                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CAMP#),    +        
               ('LD_CHARQ',DUB),(L'DUB,0)                                       
         J     UPDOUX20                                                         
*                                                                               
UPDOUX10 GOTOR PRCERR,DMCB,XERRTAB,ELEMENT                                      
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERR),      +        
               ('LD_CHARQ',ELEMENT),(BYTE1,0)                                   
         TM    TWAMODE,TWAMUWD exit on all errors                               
         BZ    UPDOUX20                                                         
         OI    GIND1,GIUNWIND  DDLINK will unwind/abend                         
         J     UPDOUTN                                                          
                                                                                
UPDOUX20 LA    R0,QR_VALS                                                       
         LA    R1,QR_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECY                                                          
                                                                                
UPDOUTN  LA    R0,QR_VALS                                                       
         LA    R1,QR_VALSL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITN                                                            
         DROP  RB                                                               
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ISSUE PENDING I/O      I N A C T I V E                              *         
***********************************************************************         
                                                                                
GOIO     DS    0H                                                               
         BR    RE                                                               
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
                                                                                
         CLC   XERRTXT,SPACES                                                   
         JNH   PUTERR2                                                          
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,(L'XERRTXT,XERRTXT)                                         
         J     PUTERR4                                                          
                                                                                
PUTERR2  GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,0                                                           
                                                                                
PUTERR4  MVC   XERRTXT,SPACES                                                   
                                                                                
PUTERRX  J     EXITY                                                            
                                                                                
         EJECT                                                                  
STAMLIT  DC    C'Start Date AM/PM'                                              
ENAMLIT  DC    C'End Date AM/PM'                                                
KSDELIT  DC    C'Keystage Date To/End'                                          
KSDSLIT  DC    C'Keystage Date From/Start'                                      
KSTGLIT  DC    C'Keystage number'                                               
PHRSLIT  DC    C'Person hours'                                                  
ACTNLIT  DC    C'Action'                                                        
EMPTLIT  DC    C'Empty - available for future use'                              
                                                                                
D#CAMP#  EQU   1                   CAMPAIGN NUMBER IF NEW                       
D#ERR    EQU   2                   ERROR                                        
D#JBSTA  EQU   3                   JOB STATUS                                   
         EJECT                                                                  
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* Routines within this server                                         *         
***********************************************************************         
                                                                                
* Dummy routine                                                                 
DUMROU   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DUMROU*'                                                      
                                                                                
         J     DUMROUY                                                          
                                                                                
DUMROUY  J     EXITY                                                            
                                                                                
DUMROUN  J     EXITN                                                            
                                                                                
***********************************************************************         
* Process error table passed in parm 1, area to put text to in parm 2 *         
***********************************************************************         
         SPACE 1                                                                
PRCERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PRCERR*'                                                      
         LM    R2,R3,0(R1)                                                      
         USING ERRTABD,R2                                                       
         ST    R3,SAVERE                                                        
         XC    BYTE1,BYTE1                                                      
PRER02   OC    ERRMESNM,ERRMESNM   HAVE WE A MESSAGE NUMBER                     
         JZ    PRER04                                                           
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ERRMESNM                                                 
         L     R0,SAVERE                                                        
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVI   GTLTXT,L'ERRMESDT                                                
         LA    R0,ERRMESDT                                                      
         STCM  R0,7,GTATXT                                                      
         GOTO1 VGETTXT,(R1)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,4(R1)                                                         
         IC    RF,BYTE1            EXISTING TEXT LENGTH                         
         AR    RF,RE               NEW TEXT LENGTH                              
         CHI   RF,L'ELEMENT        HAVE GONE OVER MAX LENGTH                    
         JH    PRER04              YES                                          
         STC   RF,BYTE1            NO - STORE NEW LENGTH                        
         L     RF,SAVERE           CURRENT ADDRESS OF TEXT STRING               
         AR    RF,RE               ADD NEW LENGTH                               
         ST    RF,SAVERE           NEXT ADDRESS OF TEXT STRING                  
         LA    R2,ERRTABL(R2)      BUMP ALONG TO NEXT MESSAGE NUMBER            
         J     PRER02              REPEAT                                       
*                                                                               
PRER04   J     EXITY                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Build campaign record                                               *         
*                                                                     *         
***********************************************************************         
                                                                                
BLDCAMP  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDCAMP'                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CPNPASD,R2                                                       
         MVI   CPNPTYP,CPNPTYPQ                                                 
         MVI   CPNPSUB,CPNPSUBQ                                                 
         MVC   CPNPCPY,CUXCPY                                                   
         MVC   CSVKEY1,CPNPAS                                                   
BCAMP02  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(CPNPCODE-CPNPASD),CSVKEY1                                  
         JNE   BCAMP04                                                          
         MVC   DR_CPN#,CPNPCODE                                                 
         XC    DR_CPN#,FFS4                                                     
         L     R2,DR_CPN#                                                       
         LA    R2,1(R2)                                                         
         ST    R2,DR_CPN#          NEXT CAMPAIGN NUMBER                         
         J     BCAMP06                                                          
         DROP  R2                                                               
                                                                                
BCAMP04  MVC   DR_CPN#,=X'0000001'                                              
                                                                                
BCAMP06  L     R3,AIO3                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         USING RWKRECD,R3                                                       
         MVI   RWKKTYP,RWKKTYPQ                                                 
         MVI   RWKKSUB,RWKKSUBQ                                                 
         MVC   RWKKCPY,CUXCPY                                                   
         MVC   RWKKOFF,RS#SJOFF                                                 
         MVC   RWKKCCDE,DR_CPN#                                                 
         MVC   QR_CAMP,DR_CPN#                                                  
         MVC   RWKRPID,CCTPID                                                   
         MVC   RWKRDTE,TODAYC                                                   
         LA    R2,RWKRFST                                                       
         USING NAMELD,R2                                                        
         XR    RE,RE                                                            
         IC    RE,DR_NAML                                                       
         BASR  RF,0                                                             
         MVC   NAMEREC(0),QR_NAME                                               
         EX    RE,0(RF)                                                         
         MVI   NAMEL,NAMELQ                                                     
         AHI   RE,1+NAMLN1Q                                                     
         STC   RE,NAMLN                                                         
         AR    R2,RE                                                            
         USING CPJELD,R2                                                        
         MVI   CPJEL,CPJELQ                                                     
         MVI   CPJLN,CPJLNQ                                                     
         MVI   CPJTYPE,CPJTJOB                                                  
         MVC   CPJCLI,SPACES                                                    
         MVC   CPJCLI(L'QR_SJCLI),QR_SJCLI                                      
         MVC   CPJPRO,SPACES                                                    
         MVC   CPJJOB,SPACES                                                    
         MVC   CPJWRK,SPACES                                                    
         AHI   R2,CPJLNQ                                                        
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         SR    R2,R3                                                            
         STH   R2,RWKRLEN                                                       
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    BCAMP08                                                          
         DC    H'0'                                                             
BCAMP08  L     R4,ABLOCK                                                        
         USING CPTRD,R4                                                         
         MVC   LDGLVALN(L'LDGLVALN*4),ONERL1L                                   
         GOTO1 VPADDLE,DMCB,(C'A',(R3)),CPTRBLK,IODA,0,ACOMFACS                 
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* validate campaign record                                            *         
*                                                                     *         
***********************************************************************         
                                                                                
VALCAMP  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALCAM*'                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CPNPASD,R2                                                       
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         MVI   CPNPTYP,CPNPTYPQ                                                 
         MVI   CPNPSUB,CPNPSUBQ                                                 
         MVC   CPNPCPY,CUXCPY                                                   
         MVC   CPNPCODE,QR_CAMP                                                 
         XC    CPNPCODE,FFS4                                                    
         MVC   CSVKEY1,CPNPAS                                                   
VCAMP02  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(CPNPACT-CPNPASD),CSVKEY1                                   
         JE    VCAMPY                                                           
         MVC   ROUERRV,=AL2(AE$INCAM)                                           
         J     VCAMPN                                                           
*                                                                               
VCAMPY   J     EXITY                                                            
                                                                                
VCAMPN   J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* validate keystage record                                            *         
*                                                                     *         
***********************************************************************         
                                                                                
VALKYST  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALKYST'                                                      
         XC    IOKEY,IOKEY                                                      
         L     R3,0(R1)                                                         
         LA    R2,IOKEY                                                         
         USING KSTRECD,R2                                                       
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,CUXCPY                                                   
         MVC   KSTKOFF,SPACES                                                   
         TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF  Are offices used in           
         JZ    VKYST12                                     resources            
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA         Yes - read office block                      
         SR    R0,R0               so we can read all valid offices             
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   VKYST02                                                          
         LA    R0,L'CTPVALUE*2                                                  
         LA    R4,OFFAWORK                                                      
         J     VKYST04                                                          
VKYST02  ICM   R0,3,OFFAWORK                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R4,OFFAWORK+2                                                    
VKYST04  MVC   KSTKOFF,0(R4)                                                    
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   VKYST12                                                          
         CLI   0(R4),C'*'                                                       
         JE    VKYST14                                                          
         MVI   KSTKOFF+1,C' '                                                   
VKYST12  MVC   KSTKNUM,0(R3)                                                    
         MVC   CSVKEY1,KSTKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VCAMPY                                                           
         TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF                                
         JZ    VKYST20                                                          
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   VKYST16                                                          
VKYST14  LA    R4,1(R4)            1 Character offices                          
         J     VKYST18                                                          
VKYST16  LA    R4,L'KSTKOFF(R4)    2 Character offices                          
VKYST18  MVC   KSTKEY,CSVKEY1                                                   
         JCT   R0,VKYST04                                                       
VKYST20  MVC   ROUERRV,=AL2(AE$IKEYS)                                           
         J     VKYSTN                                                           
*                                                                               
VKYSTY   J     EXITY                                                            
                                                                                
VKYSTN   J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Get resources work record                                           *         
* Put data to genarea after new record                                *         
***********************************************************************         
         SPACE 1                                                                
GETWRK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETWRK*'                                                      
         L     R4,SVOLDREC                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO4                                        
         USING RWKRECD,R2                                                       
         MVI   RWKKTYP,RWKKTYPQ                                                 
         MVI   RWKKSUB,RWKKSUBQ                                                 
         MVC   RWKKCPY,CUXCPY                                                   
         MVC   RWKKOFF,RS#SJOFF                                                 
         MVC   RWKKCCDE,QR_CAMP                                                 
         MVC   RWKKACT,ANYACCD                                                  
         MVI   RWKKSEQ,0                                                        
         MVC   CSVKEY1,RWKKEY                                                   
GWRK002  GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    GWRK008                                                          
         MVI   NEWREC,C'Y'                                                      
         J     GETWRKY                                                          
*                                                                               
GWRK004  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
         CLC   IOKEY(RWKKSEQ-RWKRECD),CSVKEY1                                   
         JNE   GETWRKY                                                          
         J     GWRK010                                                          
*                                                                               
GWRK008  MVC   PREVSTAT,RWKKSTAT                                                
GWRK010  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    GWRK012                                                          
         DC    H'0'                                                             
GWRK012  L     R2,AIO4                                                          
         LA    R3,RWKRFST                                                       
         USING KSTELD,R3                                                        
         XR    RE,RE                                                            
GWRK014  CLI   KSTEL,0                                                          
         JE    GWRK004                                                          
         IC    RE,KSTLN                                                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),KSTEL                                                    
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         AR    R3,RE                                                            
         AR    R4,RE                                                            
         J     GWRK014                                                          
*                                                                               
GETWRKY  J     EXITY                                                            
                                                                                
GETWRKN  J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Build elements for resource record                                  *         
* Put data to genarea                                                 *         
***********************************************************************         
         SPACE 1                                                                
BLDELS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDELS*'                                                      
         L     R6,AGENAREA                                                      
         ST    R6,SVGENADR         SAVE NEXT ELEMENT ADDRESS                    
         OC    QR_APID,QR_APID                                                  
         JZ    BELS010                                                          
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,QR_APID                                                     
         JZ    BELS010                                                          
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
                                                                                
         USING LIDELD,R6                                                        
         MVI   LIDEL,LIDELQ        BUILD LIDEL ELEMENT FOR PIDS                 
         MVI   LIDTYPE,LIDTRSPD    SET LIST TYPE AS RESOURCES PID               
         MVI   LIDITLN,L'LIDRPID+L'LIDRHRS                                      
         LA    R2,LIDDATA          R2=A(OF WHERE LIST DATA)                     
BELS002  OC    0(L'SA0KNUM,R4),0(R4)                                            
         JZ    BELS006                                                          
         MVC   TEMP2(L'SA0KNUM),0(R4)                                           
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         JE    BELS004                                                          
         MVC   ROUERRV,=AL2(AE$INPID)  INVALID PID                              
         GOTOR VHEXOUT,DMCB,TEMP2,XERRTXT,2                                     
         GOTOR SAVERR,DMCB,ROUERRV,(4,XERRTXT)                                  
         J     BELS006                                                          
*                                                                               
BELS004  MVC   0(L'LIDRPID+L'LIDRHRS,R2),0(R4)   EXTRACT FROM WMP               
         LA    R2,L'LIDRPID+L'LIDRHRS(R2)    BUMP ALONG GENAREA                 
BELS006  LA    R4,L'LIDRPID+L'LIDRHRS(R4)    BUMP ALONG WMP                     
         JCT   R3,BELS002          GET NET ENTRY IF PRESENT                     
         LA    R4,LIDDATA                                                       
         CR    R2,R4               HAVE WE ADDED ANY DATA                       
         JE    BELS010             NO                                           
         SR    R2,R6               R2=ELEMENT LENGTH                            
         STC   R2,LIDLN            SAVE ELEMENT LENGTH                          
         AR    R6,R2               R6=A(NEXT ELEMENT)                           
         ST    R6,SVGENADR         SAVE NEXT ELEMENT ADDRESS                    
*                                                                               
BELS010  CLI   DR_NXTL,FF          IS THERE ANY NEXT ACTION                     
         JE    BELS020             NO                                           
         XR    RE,RE               YES - BUILD FREE FORM TXT ELEMENT            
         IC    RE,DR_NXTL                                                       
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTFREE    SET TYPE AS FREE FORM TEXT                   
         STC   RE,FFTDLEN          SET DATA LENGTH                              
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   FFTDATA(0),QR_NXT   EXTRACT NEXT ACTION TO GENAREA               
         EX    RE,0(RF)                                                         
         AHI   RE,FFTLN1Q+2                                                     
         STC   RE,FFTLN            SET ELEMENT LENGTH                           
         AR    R6,RE                                                            
         ST    R6,SVGENADR         SAVE NEXT ELEMENT ADDRESS                    
*                                                                               
BELS020  OC    QR_AKYST,QR_AKYST                                                
         JZ    BELS040                                                          
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,QR_AKYST                                                    
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
         USING KEYSTGD,R4                                                       
         USING KSTELD,R6                                                        
BELS022  GOTOR VALKYST,DMCB,KEYSNUM                                             
         JE    BELS024                                                          
         CURED (B2,KEYSNUM),(4,XERRTXT),0                                       
         GOTOR SAVERR,DMCB,ROUERRV,(4,XERRTXT)                                  
         J     BELS036                                                          
                                                                                
BELS024  MVI   KSTEL,KSTELQ                                                     
         MVI   KSTTYPE,KSTTDATE                                                 
         MVC   KSTCODE,KEYSNUM                                                  
         MVC   KSTSTDT,KEYSSTR                                                  
         MVC   KSTENDT,KEYSEND                                                  
         CLI   KEYSSAPM,C'A'                                                    
         JNE   BELS024A                                                         
         OI    KSTSTAT,KSTSSAM                                                  
BELS024A CLI   KEYSSAPM,C'P'                                                    
         JNE   BELS024B                                                         
         OI    KSTSTAT,KSTSSPM                                                  
BELS024B CLI   KEYSEAPM,C'A'                                                    
         JNE   BELS024C                                                         
         OI    KSTSTAT,KSTSEAM                                                  
BELS024C CLI   KEYSEAPM,C'P'                                                    
         JNE   BELS026                                                          
         OI    KSTSTAT,KSTSEPM                                                  
*                                                                               
BELS026  XR    R2,R2                                                            
         LA    R6,KSTPID                                                        
         ICM   R2,1,KEYSPIDN       NUMBER OF PID ENTRIES                        
         JZ    BELS034                                                          
         LA    R7,KEYSPID                                                       
BELS028  OC    0(L'SA0KNUM,R7),0(R7)                                            
         JZ    BELS032                                                          
         MVC   TEMP2(L'SA0KNUM),0(R7)                                           
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         JE    BELS030                                                          
         MVC   ROUERRV,=AL2(AE$INPID)  INVALID PID                              
         GOTOR VHEXOUT,DMCB,TEMP2,XERRTXT,2                                     
         GOTOR SAVERR,DMCB,ROUERRV,(4,XERRTXT)                                  
         J     BELS032                                                          
*                                                                               
BELS030  MVC   0(L'SA0KNUM,R6),0(R7)                                            
         LA    R6,L'SA0KNUM(R6)                                                 
BELS032  LA    R7,L'SA0KNUM(R7)                                                 
         JCT   R2,BELS028                                                       
*                                                                               
BELS034  L     R2,SVGENADR                                                      
         SR    R6,R2                                                            
         LR    R2,R6                                                            
         L     R6,SVGENADR                                                      
         STC   R2,KSTLN            SET ELEMENT LENGTH                           
         AR    R6,R2               R6=A(NEXT FREE ELEMENT)                      
         ST    R6,SVGENADR                                                      
BELS036  LA    R4,KEYSTGL(R4)      R4=A(NEXT KEYSTAGE TABLE ENTRY)              
         JCT   R3,BELS022          GET NEXT ENTRY IF MORE                       
BELS040  MVI   0(R6),FF                                                         
         AHI   R6,1                                                             
         ST    R6,SVOLDREC         SAVE ADDRESS OF WHERE OLD REC                
*                                                                               
BLDELSY  J     EXITY                                                            
                                                                                
BLDELSN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Create status change elements                                       *         
* exit new STCELS in IO4 and IO5                                      *         
***********************************************************************         
                                                                                
DOSTAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOSTAT*'                                                      
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO4                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO5                                        
         XR    RE,RE                                                            
         L     R4,AIO4                                                          
         CLI   NEWREC,C'Y'                                                      
         JE    DSTAT100                                                         
         L     R2,AGENAREA         ADDRESS OF NEW ELEMENTS                      
         L     R3,SVOLDREC                                                      
         USING STCELD,R4                                                        
OLD      USING FFTELD,R3                                                        
NEW      USING FFTELD,R2                                                        
DSTAT010 CLI   NEW.FFTEL,FF        EOR FOR NEW ELEMENTS IN GENAREA              
         JE    DSTAT012                                                         
         CLI   NEW.FFTEL,FFTELQ    FIND FREE FORM TEXT ELEMENT                  
         JE    DSTAT014                                                         
         IC    RE,NEW.FFTLN                                                     
         AR    R2,RE                                                            
         J     DSTAT010                                                         
*                                                                               
DSTAT012 OI    RESIND,RESINNCM     NO NEW COMMENT                               
DSTAT014 CLI   OLD.FFTEL,0                                                      
         JE    DSTAT016                                                         
         CLI   OLD.FFTEL,FFTELQ                                                 
         JE    DSTAT020                                                         
         IC    RE,OLD.FFTLN                                                     
         AR    R3,RE                                                            
         J     DSTAT014                                                         
*                                                                               
DSTAT016 TM    RESIND,RESINNCM     DO WE HAVE NO COMMENT NOW                    
         JNZ   DSTAT040            YES - THEN NOTHING HAS CHANGED               
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRCMMT    SET ADDED COMMENTS                           
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         XR    RE,RE                                                            
         IC    RE,NEW.FFTDLEN                                                   
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   STCRCMTS(0),NEW.FFTDATA                                          
         EX    RE,0(RF)                                                         
         AHI   RE,1+STCLN5Q                                                     
         STC   RE,STCLN                                                         
         AR    R4,RE                                                            
         J     DSTAT040                                                         
*                                                                               
DSTAT020 TM    RESIND,RESINNCM     DO WE HAVE NO COMMENT NOW                    
         JZ    DSTAT022            NO                                           
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRCMDL    SET DELETED COMMENTS                         
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVI   STCLN,STCLN5Q                                                    
         AHI   R4,STCLN5Q                                                       
         J     DSTAT040                                                         
*                                                                               
DSTAT022 CLC   NEW.FFTDLEN,OLD.FFTDLEN                                          
         JNE   DSTAT024                                                         
         XR    RE,RE                                                            
         IC    RE,NEW.FFTDLEN                                                   
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   NEW.FFTDATA(0),OLD.FFTDATA                                       
         EX    RE,0(RF)                                                         
         JE    DSTAT040            COMMENTS ARE THE SAME                        
DSTAT024 MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRCMMT    SET ADDED COMMENTS                           
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         XR    RE,RE                                                            
         IC    RE,NEW.FFTDLEN                                                   
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   STCRCMTS(0),NEW.FFTDATA                                          
         EX    RE,0(RF)                                                         
         AHI   RE,1+STCLN5Q                                                     
         STC   RE,STCLN                                                         
         AR    R4,RE                                                            
         DROP  NEW,OLD                                                          
*                                                                               
DSTAT040 L     R2,AGENAREA         ADDRESS OF NEW ELEMENTS                      
         XR    RE,RE                                                            
         USING STCELD,R4                                                        
NEW      USING KSTELD,R2                                                        
DSTAT042 CLI   NEW.KSTEL,FF        EOR FOR NEW ELEMENTS IN GENAREA              
         JE    DSTAT064                                                         
         CLI   NEW.KSTEL,KSTELQ    FIND KEYSTAGE ELEMENT                        
         JE    DSTAT046                                                         
DSTAT044 IC    RE,NEW.KSTLN                                                     
         AR    R2,RE                                                            
         J     DSTAT042                                                         
*                                                                               
DSTAT046 L     R3,SVOLDREC                                                      
OLD      USING KSTELD,R3                                                        
DSTAT048 CLI   OLD.KSTEL,0                                                      
         JE    DSTAT052                                                         
         CLI   OLD.KSTEL,KSTELQ                                                 
         JNE   DSTAT050                                                         
         CLC   NEW.KSTCODE,OLD.KSTCODE                                          
         JE    DSTAT058                                                         
DSTAT050 IC    RE,OLD.KSTLN                                                     
         AR    R3,RE                                                            
         J     DSTAT048                                                         
*                                                                               
DSTAT052 MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRKSAD    SET ADDED KEYSTAGE                           
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVC   STCRKSTG,NEW.KSTCODE                                             
         OC    NEW.KSTSTDT,NEW.KSTSTDT                                          
         JNZ   DSTAT054                                                         
         MVI   STCLN,STCLN5Q                                                    
         AHI   R4,STCLN5Q                                                       
         J     DSTAT044                                                         
*                                                                               
DSTAT054 MVI   STCLN,STCLN6Q                                                    
         MVC   STCRSTDT,NEW.KSTSTDT                                             
         MVC   STCRENDT,NEW.KSTENDT                                             
         AHI   R4,STCLN6Q                                                       
         J     DSTAT044                                                         
                                                                                
DSTAT058 CLC   OLD.KSTSTDT,NEW.KSTSTDT    HAVE THE DATES CHANGED                
         JNE   DSTAT060            YES                                          
         CLC   OLD.KSTENDT,NEW.KSTENDT                                          
         JE    DSTAT044            NO                                           
DSTAT060 MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRDTST    SET DATE SET FOR KEYSTAGE                    
         OC    OLD.KSTENDT,OLD.KSTENDT  DID WE HAVE A PREVIOUS DATE             
         JZ    DSTAT062            NO                                           
         MVI   STCRTYP,STCRDTAM    YES - SET DATE AMENDED FOR KEYSTAGE          
DSTAT062 MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVC   STCRKSTG,NEW.KSTCODE                                             
         MVI   STCLN,STCLN6Q                                                    
         MVC   STCRSTDT,NEW.KSTSTDT                                             
         MVC   STCRENDT,NEW.KSTENDT                                             
         AHI   R4,STCLN6Q                                                       
         J     DSTAT044                                                         
*                                                                               
DSTAT064 L     R3,SVOLDREC                                                      
         XR    RE,RE                                                            
         USING STCELD,R4                                                        
DSTAT066 CLI   OLD.KSTEL,0         EOR FOR OLD ELEMENTS IN GENAREA              
         JE    DSTAT080                                                         
         CLI   OLD.KSTEL,KSTELQ    FIND KEYSTAGE ELEMENT                        
         JE    DSTAT072                                                         
DSTAT068 IC    RE,OLD.KSTLN                                                     
         AR    R3,RE                                                            
         J     DSTAT066                                                         
*                                                                               
DSTAT072 L     R2,AGENAREA         ADDRESS OF NEW ELEMENTS                      
DSTAT074 CLI   NEW.KSTEL,FF                                                     
         JE    DSTAT078                                                         
         CLI   NEW.KSTEL,KSTELQ                                                 
         JNE   DSTAT076                                                         
         CLC   NEW.KSTCODE,OLD.KSTCODE                                          
         JE    DSTAT068                                                         
DSTAT076 IC    RE,NEW.KSTLN                                                     
         AR    R2,RE                                                            
         J     DSTAT074                                                         
*                                                                               
DSTAT078 MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRKSDL    SET DELETED KEYSTAGE                         
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVC   STCRKSTG,OLD.KSTCODE                                             
         MVI   STCLN,STCLN5Q                                                    
         AHI   R4,STCLN5Q                                                       
         J     DSTAT068                                                         
*                                                                               
DSTAT080 CLC   PREVSTAT,NEWSTAT                                                 
         JE    DOSTATY                                                          
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRSTAT    SET STATUS CHANGED                           
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVC   STCRSTA,NEWSTAT                                                  
         MVI   STCLN,STCLN5Q                                                    
         AHI   R4,STCLN5Q                                                       
         J     DOSTATY                                                          
*                                                                               
DSTAT100 MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIRES      SET RESOURCES AS TYPE                        
         MVI   STCRTYP,STCRADD     SET ADDED RESOURCE RECORD                    
         MVC   STCRPID,CCTPID                                                   
         MVC   STCRDTE,TODAYP                                                   
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCRTIM,DUB                                                      
         MVC   STCRUSR,CUUSER                                                   
         MVI   STCLN,STCLN5Q                                                    
         AHI   R4,STCLN5Q                                                       
*                                                                               
DOSTATY  J     EXITY                                                            
                                                                                
DOSTATN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Update or add new audit record for resources                        *         
* entry - STCELS in IO4 and IO5                                       *         
***********************************************************************         
                                                                                
UPDAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDAUD*'                                                      
         XR    RE,RE                                                            
         L     R4,AIO4                                                          
NEW      USING STCELD,R4                                                        
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         USING AUDRECD,R2                                                       
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKRES                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVC   AUDKCCDE,QR_CAMP                                                 
         MVC   AUDKACT,ANYACCD                                                  
         MVI   AUDKSEQ,0                                                        
         MVC   CSVKEY1,AUDKEY                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   UPAUD56                                                          
UPAUD10  MVC   BYTE1,AUDKSEQ                                                    
         CLC   AUDKINDX,QR_INDX                                                 
         JE    UPAUD20                                                          
         OC    QR_INDX,QR_INDX                                                  
         JNZ   UPAUD12                                                          
         MVC   ROUERRV,=AL2(AE$RECAE)                                           
         J     UPDAUDN                                                          
UPAUD12  MVC   ROUERRV,=AL2(AE$FATAL)                                           
         J     UPDAUDN                                                          
                                                                                
UPAUD20  MVC   CSVKEY2,AUDKEY                                                   
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPAUD22                                                          
         DC    H'0'                                                             
                                                                                
UPAUD22  L     R2,AIO2                                                          
         MVC   AUDRINDX,DR_INDX                                                 
*                                                                               
UPAUD24  MVC   AUDRSTAT,NEWSTAT                                                 
UPAUD26  CLI   NEW.STCEL,STCELQ                                                 
         JE    UPAUD28                                                          
         CLI   NEW.STCEL,0                                                      
         JE    UPAUD40                                                          
UPAUD28  SR    RE,RE                                                            
         ICM   RE,3,AUDRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,NEW.STCLN                                                     
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    UPAUD40                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,NEW.STCELD,=C'ADD=END'         
         CLI   12(R1),0                                                         
         JE    UPAUD30                                                          
         CLI   12(R1),5                                                         
         JE    UPAUD40                                                          
         DC    H'0'                                                             
UPAUD30  SR    RE,RE                                                            
         IC    RE,NEW.STCLN                                                     
         AR    R4,RE                                                            
         J     UPAUD26                                                          
*                                                                               
UPAUD40  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPAUD42                                                          
         DC    H'0'                                                             
UPAUD42  LA    R2,IOKEY                                                         
         MVC   AUDKSTAT,NEWSTAT                                                 
         MVC   AUDKINDX,DR_INDX                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    UPAUD44                                                          
         DC    H'0'                                                             
UPAUD44  MVC   IOKEY,CSVKEY2                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPAUD46                                                          
         DC    H'0'                                                             
UPAUD46  L     R1,=AL4(IOSQUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   CSVKEY1(AUDKSEQ-AUDKEY),AUDKEY DO ANY RECORDS EXIST              
         JE    UPAUD10             YES                                          
UPAUD54  SR    RE,RE               NO - BUILD NEW RECORD                        
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
UPAUD56  CLI   NEW.STCEL,0                                                      
         JE    UPDAUDY                                                          
         L     R2,AIO2                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO2                                        
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKRES                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVC   AUDKCCDE,QR_CAMP                                                 
         MVC   AUDKACT,ANYACCD                                                  
         MVC   AUDKSEQ,BYTE1                                                    
         MVC   AUDRLEN,=Y(AUDRFST-AUDKEY)                                       
         XC    AUDRSTA,AUDRSTA                                                  
         MVC   AUDRSTAT,NEWSTAT                                                 
         MVC   AUDRINDX,DR_INDX                                                 
UPAUD58  CLI   NEW.STCEL,0                                                      
         JE    UPAUD62                                                          
         SR    RE,RE                                                            
         ICM   RE,3,AUDRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,NEW.STCLN                                                     
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    UPAUD62                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,NEW.STCELD,=C'ADD=END'         
         CLI   12(R1),0                                                         
         JE    UPAUD60                                                          
         CLI   12(R1),5                                                         
         JE    UPAUD62                                                          
         DC    H'0'                                                             
*                                                                               
UPAUD60  SR    RE,RE                                                            
         IC    RE,NEW.STCLN                                                     
         AR    R4,RE                                                            
         J     UPAUD58                                                          
*                                                                               
UPAUD62  MVC   IOKEY,AUDKEY                                                     
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPAUD64                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    UPAUD64                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    UPAUD54                                                          
         DC    H'0'                                                             
*                                                                               
UPAUD64  DS    0H                  update existing records                      
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
         L     R0,AIO8             copy new record to IO8                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPAUD66                                                          
         DC    H'0'                                                             
*                                                                               
UPAUD66  L     R0,AIO2             copy new record to IO8                       
         LA    R1,2000                                                          
         L     RE,AIO8                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPAUD68                                                          
         DC    H'0'                                                             
UPAUD68  LA    RE,IOKEY            pass new status                              
         MVC   AUDKSTA-AUDRECD(8,RE),AUDRSTA                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    UPAUD54                                                          
         DC    H'0'                                                             
         DROP  NEW,R2                                                           
*                                                                               
UPDAUDY  J     EXITY                                                            
                                                                                
UPDAUDN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Update job with campaign number                                     *         
* entry - STCELS in IO4 and IO5                                       *         
***********************************************************************         
                                                                                
UPDJOB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDJOB*'                                                      
         TM    RS#ICPJ,RS#IJOB                                                  
         JZ    UPDJOBY                                                          
         XR    RE,RE                                                            
         MVC   IOKEY,SPACES                                                     
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         MVC   ACTKACT,ANYACCD                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    UPJOB02                                                          
         DC    H'0'                                                             
*                                                                               
UPJOB02  L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPJOB04                                                          
         DC    H'0'                                                             
*                                                                               
UPJOB04  L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         USING FFTELD,R3                                                        
         XR    RE,RE                                                            
UPJOB10  CLI   FFTEL,0                                                          
         JE    UPJOB18                                                          
         CLI   FFTEL,FFTELQ                                                     
         JE    UPJOB14                                                          
UPJOB12  IC    RE,FFTLN                                                         
         AR    R3,RE                                                            
         J     UPJOB10                                                          
*                                                                               
UPJOB14  CLI   FFTTYPE,FFTTCAMP                                                 
         JNE   UPJOB12                                                          
         CLI   QR_STAT,QR_OBSOQ    if obsolete remove campaign                  
         JE    UPJOB16                        from job                          
         CLC   QR_CAMP,FFTDATA     any other status check it matches            
         JE    UPDJOBY                                                          
         CURED (B4,FFTDATA),(10,TEMP2),0,ALIGN=LEFT                             
         MVC   ROUERRV,=AL2(AE$JBCMP)  otherwise error                          
         J     UPDJOBN                                                          
*                                                                               
UPJOB16  MVI   FFTEL,X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ACTRECD),0                      
         CLI   12(R1),0                                                         
         JE    UPJOB20                                                          
         DC    H'0'                                                             
*                                                                               
UPJOB18  CLI   QR_STAT,QR_OBSOQ    if status obsolete don't add campgn          
         JE    UPDJOBY                                       to job             
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTCAMP                                                 
         MVC   FFTDATA(L'QR_CAMP),QR_CAMP                                       
         MVI   FFTDLEN,L'QR_CAMP                                                
         MVI   FFTLN,L'FFTDLEN+L'QR_CAMP+FFTLN1Q                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,FFTELD                         
         CLI   12(R1),0                                                         
         JE    UPJOB20                                                          
         DC    H'0'                                                             
UPJOB20  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPDJOBY                                                          
         DC    H'0'                                                             
UPDJOBY  J     EXITY                                                            
                                                                                
UPDJOBN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Update or add new resources work record                             *         
* entry - STCELS in IO4 and IO5                                       *         
***********************************************************************         
                                                                                
UPDRWK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDRWK*'                                                      
         XR    RE,RE                                                            
         XC    BYTE1,BYTE1                                                      
         L     R3,AIO2                                                          
         L     R4,AGENAREA                                                      
NEW      USING KSTELD,R4                                                        
         USING RWKRECD,R3                                                       
         J     UPRWK04                                                          
UPRWK02  SR    RE,RE               NO - BUILD NEW RECORD                        
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
         CLI   NEW.KSTEL,FF                                                     
         JE    UPDRWKY                                                          
UPRWK04  GOTOR (#CLRIO,ACLRIO),DMCB,AIO2                                        
         XC    RWKKEY,RWKKEY                                                    
         MVI   RWKKTYP,RWKKTYPQ                                                 
         MVI   RWKKSUB,RWKKSUBQ                                                 
         MVC   RWKKCPY,CUXCPY                                                   
         MVC   RWKKOFF,RS#SJOFF                                                 
         MVC   RWKKCCDE,QR_CAMP                                                 
         MVC   RWKKACT,ANYACCD                                                  
         MVC   RWKKSEQ,BYTE1                                                    
         MVC   RWKRLEN,=Y(AUDRFST-AUDKEY)                                       
         XC    RWKRSTA,RWKRSTA                                                  
         MVC   RWKRSTAT,NEWSTAT                                                 
         MVI   RWKRSTA2,0                                                       
         MVC   RWKRPID,CCTPID                                                   
         MVC   RWKRDTE,TODAYC                                                   
UPRWK10  CLI   NEW.KSTEL,FF                                                     
         JE    UPRWK14                                                          
         SR    RE,RE                                                            
         ICM   RE,3,RWKRLEN                                                     
         SR    RF,RF                                                            
         IC    RF,NEW.KSTLN                                                     
         AR    RE,RF                                                            
         CH    RE,=H'2000'                                                      
         JH    UPRWK14                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),RWKRECD,NEW.KSTELD,=C'ADD=END'         
         CLI   12(R1),0                                                         
         JE    UPRWK12                                                          
         CLI   12(R1),5                                                         
         JE    UPRWK14                                                          
         DC    H'0'                                                             
*                                                                               
UPRWK12  SR    RE,RE                                                            
         IC    RE,NEW.KSTLN                                                     
         AR    R4,RE                                                            
         J     UPRWK10                                                          
*                                                                               
UPRWK14  MVC   IOKEY,RWKKEY                                                     
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPRWK18                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    UPRWK18                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    UPRWK16                                                          
         DC    H'0'                                                             
UPRWK16  L     R2,ABLOCK                                                        
         USING CPTRD,R2                                                         
         MVC   LDGLVALN(L'LDGLVALN*4),ONERL1L                                   
         GOTO1 VPADDLE,DMCB,(C'A',AIO2),CPTRBLK,IODA,0,ACOMFACS                 
         J     UPRWK02                                                          
*                                                                               
UPRWK18  DS    0H                  update existing records                      
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
         L     R0,AIO8             copy new record to IO8                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPRWK20                                                          
         DC    H'0'                                                             
*                                                                               
UPRWK20  L     R2,ABLOCK                                                        
         USING CPTRD,R2                                                         
         MVC   LDGLVALN(L'LDGLVALN*4),ONERL1L                                   
         GOTO1 VPADDLE,DMCB,(C'D',AIO2),(C'K',CPTRBLK),IODA,0,ACOMFACS          
                                                                                
         L     R0,AIO2             copy new record to IO8                       
         LA    R1,2000                                                          
         L     RE,AIO8                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPRWK22                                                          
         DC    H'0'                                                             
UPRWK22  L     R2,ABLOCK                                                        
         USING CPTRD,R2                                                         
         MVC   LDGLVALN(L'LDGLVALN*4),ONERL1L                                   
         GOTO1 VPADDLE,DMCB,(C'A',AIO2),CPTRBLK,IODA,0,ACOMFACS                 
                                                                                
         LA    RE,IOKEY            pass new status                              
         MVC   RWKKSTA-RWKRECD(8,RE),RWKRSTA                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    UPRWK02                                                          
         DC    H'0'                                                             
         DROP  NEW,R3                                                           
*                                                                               
UPDRWKY  J     EXITY                                                            
                                                                                
UPDRWKN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* add out of the office records                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ADDOUT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDOUT*'                                                      
         L     R4,0(R1)                                                         
         USING OUTTABD,R4                                                       
         L     R3,AIO3                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3                                        
         USING PKSRECD,R3                                                       
         MVI   PKSKTYP,PKSKTYPQ                                                 
         MVI   PKSKSUB,PKSKSUBQ                                                 
         MVC   PKSKCPY,CUXCPY                                                   
         MVC   PKSKPIDB,OUTPID                                                  
         XR    RE,RE                                                            
         ICM   RE,7,OUTENDDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,PKSKEDT                                                     
         XR    RE,RE                                                            
         ICM   RE,7,OUTSTRDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,PKSKSDT                                                     
         MVC   PKSKOUT,OUTTYPE                                                  
         CLI   OUTSTRAM,C'A'                                                    
         JNE   ADDO002                                                          
         OI    PKSKAMPM,PKSKSAM                                                 
ADDO002  CLI   OUTSTRAM,C'P'                                                    
         JNE   ADDO004                                                          
         OI    PKSKAMPM,PKSKSPM                                                 
ADDO004  CLI   OUTENDAM,C'A'                                                    
         JNE   ADDO006                                                          
         OI    PKSKAMPM,PKSKEAM                                                 
ADDO006  CLI   OUTENDAM,C'P'                                                    
         JNE   ADDO008                                                          
         OI    PKSKAMPM,PKSKEPM                                                 
ADDO008  MVC   PKSROFF,CUUSER                                                   
         MVC   PKSRPID,CCTPID                                                   
         MVC   PKSRDTE,TODAYC                                                   
         LA    R2,PKSRFST                                                       
         USING FFTELD,R2                                                        
         XR    RE,RE                                                            
         IC    RE,OUTLEN                                                        
         SHI   RE,OUTTLN1Q                                                      
         LTR   RE,RE                                                            
         JZ    ADDO010                                                          
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   FFTDATA(0),OUTCOMNT                                              
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
ADDO010  MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTFREE                                                 
         AHI   RE,FFTLN1Q+L'FFTDLEN                                             
         STC   RE,FFTLN                                                         
         AR    R2,RE                                                            
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         SR    R2,R3                                                            
         STH   R2,PKSRLEN                                                       
                                                                                
         MVC   IOKEY,PKSKEY                                                     
         L     R1,=AL4(IORDUPD+IODIR+IO3)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    ADDO020                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    ADDO020                                                          
         J     ADDO040                                                          
                                                                                
ADDO020  GOTOR (#CLRIO,ACLRIO),DMCB,AIO8  UPDATE EXISTING RECORD                
         L     R0,AIO8             copy new record to IO8                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    ADDO022                                                          
         DC    H'0'                                                             
ADDO022  L     R0,AIO3             copy new record to IO3                       
         LA    R1,2000                                                          
         L     RE,AIO8                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,=AL4(IOPUTREC+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    ADDO024                                                          
         DC    H'0'                                                             
                                                                                
ADDO024  MVC   IOKEY+L'PKSKEY(L'PKSKSTA),PKSRSTA                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
ADDO040  GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Delete out of office records                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DELOUT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DELOUT*'                                                      
         L     R4,0(R1)                                                         
         USING OUTTABD,R4                                                       
         LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING PKSRECD,R3                                                       
         MVI   PKSKTYP,PKSKTYPQ                                                 
         MVI   PKSKSUB,PKSKSUBQ                                                 
         MVC   PKSKCPY,CUXCPY                                                   
         MVC   PKSKPIDB,OUTPID                                                  
         XR    RE,RE                                                            
         ICM   RE,7,OUTENDDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,PKSKEDT                                                     
         XR    RE,RE                                                            
         ICM   RE,7,OUTSTRDT                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,PKSKSDT                                                     
         MVC   PKSKOUT,OUTTYPE                                                  
         CLI   OUTSTRAM,C'A'                                                    
         JNE   DELO002                                                          
         OI    PKSKAMPM,PKSKSAM                                                 
DELO002  CLI   OUTSTRAM,C'P'                                                    
         JNE   DELO004                                                          
         OI    PKSKAMPM,PKSKSPM                                                 
DELO004  CLI   OUTENDAM,C'A'                                                    
         JNE   DELO006                                                          
         OI    PKSKAMPM,PKSKEAM                                                 
DELO006  CLI   OUTENDAM,C'P'                                                    
         JNE   DELO008                                                          
         OI    PKSKAMPM,PKSKEPM                                                 
                                                                                
DELO008  MVC   CSVKEY1,IOKEY                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO3)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    DELO010                                                          
         CLI   IOERR,IOEDEL                                                     
         JE    EXITY                                                            
         DC    H'0'                DIE IF WE THINK ADD IS OK                    
                                                                                
DELO010  L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    DELO012                                                          
         DC    H'0'                                                             
DELO012  L     R3,AIO3                                                          
         OI    PKSRSTAT,RWKSDELT                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JE    DELO014                                                          
         DC    H'0'                                                             
                                                                                
DELO014  MVC   IOKEY+L'PKSKEY(L'PKSKSTA),PKSRSTA                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TIMESHEET VALIDATION MODULE: CLI/PRO/JOB ACCOUNT                    *         
* - DATA RECEIVED IN TEMP2                                            *         
* - DATA RETURNED IN ANYACCNT (ERROR IN ROUERRV)                      *         
* - SETS CC                                                           *         
***********************************************************************         
         DS    0H                                                               
VTSCPJ   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    CL8'*VTSCPJ*'                                                    
                                                                                
         OC    TEMP2(13),SPACES   CLI/PRO/JOB -> SJ ACCOUNT CODE                
         CLC   TEMP2(13),SPACES                                                 
         JNH   EXITY                                                            
                                                                                
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         MVC   ANYACCNT+2(0),TEMP2                                              
         EX    R1,*-6                                                           
         LA    R1,ANYACCNT+3(R1)                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
*&&UK*&& MVC   0(0,R1),TEMP2+5                                                  
*&&US*&& MVC   0(0,R1),TEMP2+3                                                  
         EX    RF,*-6                                                           
         LA    R1,1(RF,R1)                                                      
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
*&&UK*&& MVC   0(0,R1),TEMP2+7                                                  
*&&US*&& MVC   0(0,R1),TEMP2+6                                                  
         EX    RF,*-6                                                           
         OC    ANYACCNT,SPACES                                                  
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ANYACCNT(2),PRODUL                                               
         XR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         SHI   RF,1                                                             
         MVC   ACTKACT(0),ANYACCNT+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,*-6                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VTSCPJ10                                                         
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,ACTRFST                                                       
         OI    RS#ICPJ,RS#ICLI     CLIENT LEVEL                                 
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ16 CLI   PPREL,0                                                          
         JE    VTSCPJ26                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VTSCPJ22                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VTSCPJ20                                                         
                                                                                
VTSCPJ18 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VTSCPJ16                                                         
                                                                                
VTSCPJ20 MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         J     VTSCPJ18                                                         
                                                                                
         USING RSTELD,R2                                                        
VTSCPJ22 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VTSCPJ18                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
VTSCPJ26 DS    0H                                                               
*&&UK*&& CLC   TEMP2+5(2),SPACES  PROCESS PRODUCT                               
*&&US*&& CLC   TEMP2+3(3),SPACES                                                
         JNH   VTSCPJ68                                                         
         USING ACTRECD,R2                                                       
VTSCPJ28 LA    R2,IOKEY                                                         
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SHI   RF,1                                                             
         MVC   ACTKACT(0),ANYACCNT+2                                            
         EX    RF,*-6                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VTSCPJ32                                                         
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ32 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         OI    RS#ICPJ,RS#IPRD     PRODUCT LEVEL                                
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ38 CLI   PPREL,0                                                          
         JE    VTSCPJ46                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VTSCPJ44                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VTSCPJ44                                                         
                                                                                
VTSCPJ40 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VTSCPJ38                                                         
                                                                                
VTSCPJ42 CLI   PPRGAOFF,X'40'                                                   
         JNH   VTSCPJ40                                                         
         MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         J     VTSCPJ40                                                         
*                                                                               
         USING RSTELD,R2                                                        
VTSCPJ44 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VTSCPJ40                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
VTSCPJ46 DS    0H                                                               
*&&UK*&& CLC   TEMP2+7(6),SPACES  PROCESS JOB                                   
*&&US*&& CLC   TEMP2+6(6),SPACES                                                
         JNH   VTSCPJ68                                                         
         USING ACTRECD,R2                                                       
VTSCPJ48 LA    R2,IOKEY                                                         
         MVC   ACTKACT,ANYACCNT+2                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VTSCPJ50                                                         
         MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ50 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         OI    RS#ICPJ,RS#IJOB     JOB LEVEL                                    
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ60 CLI   PPREL,0                                                          
         JE    VTSCPJ68                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VTSCPJ66                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VTSCPJ64                                                         
                                                                                
VTSCPJ62 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VTSCPJ60                                                         
                                                                                
VTSCPJ64 CLI   PPRGAOFF,X'40'                                                   
         JNH   VTSCPJ62                                                         
         MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         J     VTSCPJ62                                                         
*                                                                               
         USING RSTELD,R2                                                        
VTSCPJ66 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VTSCPJ62                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
VTSCPJ68 CLI   OFFIND,FULLYQ       OFFICE CHECKING                              
         JNE   VTSCPJ98                                                         
         CLC   RS#SJOFF,SPACES                                                  
         JNE   VTSCPJ72                                                         
         MVC   ROUERRV,=AL2(AE$INVPO)                                           
         J     EXITN                                                            
                                                                                
         USING OFFALD,R1                                                        
VTSCPJ72 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,RS#SJOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    VTSCPJ98                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
         DROP  R1,R2                                                            
*                                                                               
VTSCPJ98 DS    0H                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE ERROR MESSAGES IN AN AREA SO WE CAN DISPLAY ALL ERRORS         *         
***********************************************************************         
         SPACE 1                                                                
SAVERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
         L     R4,XANXTER          address of next error in table               
         USING ERRTABD,R4                                                       
         OI    TWAMODE,TWAMERP                                                  
         LM    R2,R3,0(R1)                                                      
         MVC   ERRMESNM,0(R2)                                                   
         MVC   BYTE1,4(R1)                                                      
         SR    R1,R1                                                            
         IC    R1,BYTE1                                                         
         OR    R1,R1               have we any data for the message             
         JZ    SVER04                                                           
         MVC   ERRMESDT,SPACES     space fill error message data                
         CHI   R1,L'ERRMESDT       make sure data fits in data field            
         JNH   SVER02                                                           
         LA    R1,L'ERRMESDT                                                    
SVER02   SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   ERRMESDT(0),0(R3)                                                
         EX    R1,0(RF)                                                         
SVER04   LA    R4,ERRTABL(R4)                                                   
         ST    R4,XANXTER                                                       
         MVC   XERRTXT,SPACES                                                   
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
SVRDEF   CSECT                                                                  
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR RESOURCES WORK UPLOAD                               *         
***********************************************************************         
                                                                                
RESUPL   LKREQ H,A#RWKUP,NEWREC=Y                                               
Client   LKREQ F,01,(D,B#SAVED,QR_SJCLI),CHAR,TEXT=AC#CLIC                      
Prod     LKREQ F,02,(D,B#SAVED,QR_SJPRO),CHAR,TEXT=AC#PROC                      
Job      LKREQ F,03,(D,B#SAVED,QR_SJJOB),CHAR,TEXT=AC#JOBC                      
Camp     LKREQ F,04,(D,B#SAVED,QR_CAMP),LBIN,TEXT=AC#CAMC                       
CampNam  LKREQ F,05,(D,B#SAVED,QR_NAME),CHAR,TEXT=AC#RSCMN,LOWERCASE=Y          
Keystg   LKREQ F,06,(I,B#SAVED,QR_KYSTI),LBIN,TEXT=(*,KSTGLIT),        +        
               LIST=NOSORT,OLEN=2,ARRAY=S                                       
KstSDte  LKREQ F,07,,PDAT,OLEN=L'KSTSTDT,                              +        
               TEXT=(*,KSDSLIT)                                                 
KstSAPM  LKREQ F,08,,CHAR,OLEN=1,                                      +        
               TEXT=(*,STAMLIT)                                                 
KstEDte  LKREQ F,09,,PDAT,OLEN=L'KSTENDT,                              +        
               TEXT=(*,KSDELIT)                                                 
KstEAPM  LKREQ F,10,,CHAR,OLEN=1,                                      +        
               TEXT=(*,ENAMLIT)                                                 
Kpid     LKREQ F,11,,HEXD,TEXT=AC#CPIDN,                               +        
               OLEN=L'QR_PIDB,ENTRIES=20,ECOUNT=Y,ARRAY=E                       
Pid      LKREQ F,12,(I,B#SAVED,QR_PIDI),HEXD,TEXT=AC#CPIDN,            +        
               OLEN=L'QR_PIDB,LIST=NOSORT,ARRAY=S                               
Hrs      LKREQ F,13,,SPAK,TEXT=(*,PHRSLIT),                            +        
               OLEN=L'LIDRHRS,ARRAY=E                                           
NxtActn  LKREQ F,14,(D,B#SAVED,QR_NXT),VSTR,TEXT=AC#ACT,LOWERCASE=Y             
Status   LKREQ F,15,(D,B#SAVED,QR_STAT),CHAR,TEXT=AC#STT                        
Index    LKREQ F,16,(D,B#SAVED,QR_INDX),LBIN,TEXT=AC#INDEX                      
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR OUT OF OFFICE UPLOAD - HEADER                       *         
***********************************************************************         
                                                                                
OUTHDR   LKREQ H,A#OUTHD,NEWREC=Y                                               
Empty    LKREQ F,01,(D,B#SAVED,QR_ACTN),CHAR,TEXT=(*,EMPTLIT)                   
                                                                                
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR OUT OF OFFICE UPLOAD                                *         
***********************************************************************         
                                                                                
OUTITM   LKREQ H,A#OUTUP,NEWREC=Y                                               
Actn     LKREQ F,01,(D,B#SAVED,QR_ACTN),CHAR,TEXT=(*,ACTNLIT)                   
PidB     LKREQ F,02,(D,B#SAVED,QR_PIDB),HEXD,TEXT=AC#CPIDN                      
StrDte   LKREQ F,03,(D,B#SAVED,QR_STDT),PDAT,TEXT=AC#STRDT                      
StrAM    LKREQ F,04,(D,B#SAVED,QR_STAM),CHAR,TEXT=(*,STAMLIT)                   
EndDte   LKREQ F,05,(D,B#SAVED,QR_ENDT),PDAT,TEXT=AC#ENDDT                      
EndAM    LKREQ F,06,(D,B#SAVED,QR_ENAM),CHAR,TEXT=(*,ENAMLIT)                   
Type     LKREQ F,07,(D,B#SAVED,QR_TYPE),CHAR,TEXT=AC#TYPE1                      
Cmmts    LKREQ F,08,(D,B#SAVED,QR_NXT),VSTR,TEXT=AC#RSCMN,LOWERCASE=Y           
                                                                                
         LKREQ E                                                                
***********************************************************************         
* REQUEST MAP FOR OUT OF OFFICE UPLOAD - TRAILER                      *         
***********************************************************************         
                                                                                
OUTTRL   LKREQ H,A#OUTTR,NEWREC=Y                                               
Empty    LKREQ F,01,(D,B#SAVED,QR_ACTN),CHAR,TEXT=(*,EMPTLIT)                   
                                                                                
         LKREQ E                                                                
***********************************************************************         
* END OF REQUEST MAP TABLES                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
*** GENERAL UPLOAD RESPONSE DATA MAP NUMBERS                                    
                                                                                
* general equates *                                                             
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
ACCMST   DC    C'ACCMST  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
LSPACES  DC    255C' '                                                          
FFS4     DC    X'FFFFFFFF'                                                      
ADDEND   DC    C'ADD=END'                                                       
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#RWKUP),AL1(RECTRSUP)                                       
         DC    AL2(A#OUTHD),AL1(RECTOTHD)                                       
         DC    AL2(A#OUTUP),AL1(RECTOTIT)                                       
         DC    AL2(A#OUTTR),AL1(RECTOTTR)                                       
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTRSUP EQU   1                   Resources upload                             
RECTOTHD EQU   2                   Out of office upload - header                
RECTOTIT EQU   3                   Out of office upload - item                  
RECTOTTR EQU   4                   Out of office upload - trailer               
RECTABL  EQU   *-RECTABD                                                        
                                                                                
                                                                                
         EJECT                                                                  
                                                                                
SAVED    DSECT                                                                  
SVGENADR DS    A                   CURRENT ADDESSS OF RES ELEMENTS              
SVOLDREC DS    A                   START OF OLD RESOURCE ELEMENTS               
XANXTER  DS    A                   ADDRESS OF ERRORS                            
                                                                                
                                                                                
XERRTAB  DS    XL256               saved error messages and data                
NEWSTAT  DS    XL1                 STATUS OF NEW/CURRENT RECORD                 
PREVSTAT DS    XL1                 STATUS OF PREVIOUS RECORD                    
AGENCY   DS    XL1                                                              
NEWREC   DS    CL1                 NEW RECORD                                   
                                                                                
RESIND   DS    XL1                 RESOURCES INDICATOR                          
RESINNCM EQU   X'80'                                                            
SAVEVAR  DS    0F                  ** Variables **                              
RS#ICPJ  DS    XL1                                                              
RS#ICLI  EQU   X'80'                                                            
RS#IPRD  EQU   X'40'                                                            
RS#IJOB  EQU   X'20'                                                            
RS#SJOFF DS    CL(L'TRNOFFC)                                                    
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
*                                                                               
QR_VALS  DS    0F                                                               
*&&UK                                                                           
QR_SJCLI DS    CL5                 CLIENT                                       
QR_SJPRO DS    CL2                 PRODUCT                                      
QR_SJJOB DS    CL7                 JOB                                          
*&&                                                                             
*&&US                                                                           
QR_SJCLI DS    CL3                 CLIENT                                       
QR_SJPRO DS    CL3                 PRODUCT                                      
QR_SJJOB DS    CL6                 JOB                                          
*&&                                                                             
QR_CAMP  DS    XL4                 CAMPAIGN                                     
QR_NAME  DS    CL36                CAMPAIGN NAME IF NEW                         
QR_ACTN  DS    CL1                 ACTION                                       
QR_PIDB  DS    XL2                 BINARY PID CODE                              
QR_STDT  DS    PL3                 START DATE                                   
QR_STAM  DS    XL1                 START DATE AM/PM                             
QR_ENDT  DS    PL3                 END DATE                                     
QR_ENAM  DS    XL1                 END DATE AM/PM                               
QR_TYPE  DS    CL1                 TYPE OF OUT OF OFFICE                        
QR_THOL  EQU   C'1'                HOLIDAY                                      
QR_TAPT  EQU   C'2'                APPOINTMENT                                  
QR_TOTH  EQU   C'3'                OTHER                                        
                                                                                
QR_KYSTI DS    X                   KEY STAGE INDICATOR                          
QR_AKYST DS    AL3                 ADDRESS KEY STAGE LIST                       
                                                                                
QR_PIDI  DS    X                   PID INDICATOR                                
QR_APID  DS    AL3                 ADDRESS PID LIST                             
                                                                                
QR_NXT   DS    CL220               NEXT ACTION COMMENTS                         
QR_STAT  DS    CL1                 STATUS OF WORK                               
QR_FINSQ EQU   C'1'                FINISHED                                     
QR_OBSOQ EQU   C'2'                OBSOLETE                                     
QR_CURRQ EQU   C'3'                CURRENT                                      
                                                                                
         DS    0D                                                               
QR_INDX  DS    XL2                 INDEX                                        
                                                                                
QR_VALSL EQU   *-QR_VALS                                                        
                                                                                
DR_VALS  DS    0F                  DERIVED VARIABLES                            
DR_NXTL  DS    XL1                 LENGTH OF TEXT FOR NEXT ACTION               
DR_NAML  DS    XL1                 LENGTH OF NAME FOR CAMPAIGN                  
                                                                                
DR_INDX  DS    XL2                 NEXT INDEX NUM TO STOP CON-CURRENT           
DR_CPN#  DS    XL4                 CAMPAIGN NUMBER                              
DR_JBST  DS    CL1                 JOB STATUS                                   
                                                                                
                                                                                
DR_VALSL EQU   *-DR_VALS                                                        
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
SAVEL    EQU   *-SAVED                                                          
***********************************************************************         
* KEYSTAGE LIST AREA DSECT                                            *         
***********************************************************************         
        SPACE  1                                                                
KEYSTGD  DSECT                                                                  
KEYSNUM  DS    XL2                 KEYSTAGE NUMBER                              
KEYSSTR  DS    PL3                 START DATE                                   
KEYSSAPM DS    CL1                 AM/PM                                        
KEYSEND  DS    PL3                 END DATE                                     
KEYSEAPM DS    CL1                 AM/PM                                        
KEYSPIDN DS    XL1                 NUMBER OF PID ENTRIES                        
KEYSPID  DS    20XL2               PID ENTRIES                                  
KEYSTGL  EQU   *-KEYSTGD           LENGTH                                       
                                                                                
***********************************************************************         
* PID AND HOURS LIST AREA DSECT                                       *         
***********************************************************************         
PHRTABD  DSECT                                                                  
PHRSPID  DS    XL2                 BINARY PID                                   
PHRSHRS  DS    PL6                 HOURS FOR PID                                
PHRTABL  EQU   *-PHRTABD           LENGTH                                       
                                                                                
***********************************************************************         
* OUT OF OFFICE RECORD DSECT                                          *         
***********************************************************************         
OUTTABD  DSECT                                                                  
OUTLEN   DS    XL1                 LENGTH OF RECORD ENTRY                       
OUTACT   DS    CL1                 ACTION                                       
OUTAADD  EQU   C'A'                ADD                                          
OUTADEL  EQU   C'D'                DELETION                                     
OUTPID   DS    XL2                 BINARY PID                                   
OUTSTRDT DS    PL3                 START DATE                                   
OUTSTRAM DS    CL1                 START AM/PM                                  
OUTENDDT DS    PL3                 END DATE                                     
OUTENDAM DS    CL1                 END AM/PM                                    
OUTTYPE  DS    CL1                 TYPE                                         
OUTTHOL  EQU   PKSKOHOL            HOLIDAY                                      
OUTTAPT  EQU   PKSKOAPT            APPOINTMENT                                  
OUTTOTH  EQU   PKSKOOTH            OTHER                                        
OUTTLN1Q EQU   *-OUTTABD           LENGTH OF DATA MINUS TEXT                    
OUTCOMNT DS    0CL220              TEXT COMMENTS                                
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
                                                                                
         EJECT                                                                  
                                                                                
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVVALL   EQU   *-SVVALS                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBRA25   04/24/15'                                      
         END                                                                    
