*          DATA SET FAPROGSPC  AT LEVEL 008 AS OF 09/10/15                      
*PHASE PROGSPCA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATTIM                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOADER                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE ARREDIT                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SMTP                                                                   
*INCLUDE SORTER                                                                 
         TITLE 'FAPROGSPC - REBUILD PROGRAMS TABLES IN DATASPACE'               
***********************************************************************         
* Program consructs the inards of the programs dataspace                        
*                                                                               
* This is run mostly in two modes.                                              
* First time as a full build.                                                   
* Second to do reload of phases to go live based on reload date                 
*                                                                               
* NOTE: This does not use DDLOCKSPC to get the ALET because                     
*       each FACPAK can have a different programs dataspace then                
*       the DATAMGR or TABS dataspace. Can not be driven by the                 
*       DSPACE=N card.                                                          
*                                                                               
* CARDS                                                                         
*                                                                               
* MODE=INIT        Builds table from scratch             (Default)              
* MODE=RELOAD      Loads based on reload date                                   
*                                                                               
* RUN=DSP          Build tables in dataspace             (Default)              
* RUN=TEST         Build tables in local core memeory                           
*                                                                               
* FAPARMS=FAPARMn  This is the same as the FACPAK parameters.                   
*                  This drives the system and dataspace to use                  
*                  Programs dataspace and the programs dataspace                
*                  NOTE: DSPACE= is the DATAMGR dataspace                       
*                  There is no longer DSPACE= card support since the            
*                  FAPARMS drives it.                                           
*                                                                               
* DDSIO=           Over-ride defaut version of DDSIO                            
*                                                                               
***************************                                                     
* Table of DATASPACE info *                                                     
**************************************************************                  
* SYSTEM   PROGRAM            DATAMANAGER    COUNRTY  DSPACE *                  
**************************************************************                  
* ADV      PRGAXXXXXXXX       DMGADATAMGRX   US/UK    A/P                       
* REP      PRGRXXXXXXXX       DMGRDATAMGRX   US       R                         
* CSC      PRGCXXXXXXXX       DMGCDATAMGRX   US/UK    C                         
* FQA      PRGQXXXXXXXX       DMGQDATAMGRX   US/UK    Q                         
* TST      PRGTXXXXXXXX       DMGTDATAMGRX   US/UK    T                         
* MEL      TST2XXXXXXXX       DMGTDATAMGRX   US       T                         
* NEW      PGMNXXXXXXXX       DMGTDATAMGRX   UK       T                         
* TTS      PGMSXXXXXXXX       DMGTDATAMGRX   UK       T                         
* BAR  ?   PRGBXXXXXXXX       DMGBDATAMGRX   UK       B                         
***********************************************************************         
         PRINT NOGEN                                                            
PROGSPC  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,*PROGSPC,AWRKAREA,RA,R9                                    
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         USING DMSPACED,DSPHD                                                   
         USING PROGSPCD,PSREC                                                   
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
         CLC   =C'INIT',MODE                                                    
         BNE   MAIN02                                                           
         BRAS  RE,CHKPT            WRITE CHECKPOINTS                            
         BRAS  RE,GETRLD                                                        
         BRAS  RE,PGMFILE          BUILD PROGRAMS TABLE ENTRIES                 
         B     XBASE                                                            
*                                                                               
MAIN02   CLC   =C'RELOAD',MODE                                                  
         BNE   MAIN04                                                           
         BRAS  RE,DORELOAD         ADD DAILY ENTRIES TO PROGRAMS TABLE          
         B     XBASE                                                            
*                                                                               
MAIN04   DC    H'0'                WHY ARE YOU HERE?                            
*                                                                               
AWRKAREA DC    A(WORKAREA)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         MVC   IOH,=CL8'IO1*IO1*'                                               
         MVC   PTITLE,SPACES                                                    
         MVC   TITLED(L'CTITLE),CTITLE   SET UP TITLE                           
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PLINED(L'CTITLEU),CTITLEU SET UP TITLE UNDERLINE                 
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         GOTO1 VDATTIM,DMCB,(X'01',DATETIME),0                                  
*                                                                               
         LHI   R1,1                BEGINNING INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R2,CARD                                                          
INIT02   GOTO1 VCARDS,DMCB,(R2),=C'RE00'                                        
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    INIT04              YES                                          
*                                                                               
         MVC   PLINE+20(L'CARD),CARD                                            
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BRAS  RE,CARDVAL                                                       
         BE    INIT02                                                           
*                                                                               
         LHI   R1,3                CARD VALIDATION ERROR                        
         BRAS  RE,DOMSG                                                         
         B     XBASE                                                            
*                                                                               
INIT04   CLC   FAPARM,SPACES       Must have FAPARMS=                           
         BH    INIT06                                                           
         LHI   R1,15               Error, missing FAPARMS=                      
         BRAS  RE,DOMSG                                                         
         B     XBASE                                                            
*                                                                               
INIT06   GOTO1 ALOADER,DMCB,FAPARM,0                                            
         LT    R2,DMCB+4                                                        
         BNZ   INIT08                                                           
         LHI   R1,16               Phase not found                              
         BRAS  RE,DOMSG                                                         
         B     XBASE                                                            
*                                                                               
         USING FAPARMD,R2                                                       
         USING SSBD,RE                                                          
INIT08   L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,FACSPACE+3 Set from DATAMGR                             
         MVC   DSPACE,PGMSDSP      Programs dataspace                           
         MVC   SYSCODE,VTAMAPL     System Name                                  
         DROP  R2,RE                                                            
*                                                                               
         LHI   R1,2                COMPLETED INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
         BRAS  RE,GETSPC           GET ADDRESS OF PGMS DSPACE                   
*                                                                               
         LHI   R1,8                BEGAN OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         GOTO1 VDMGR,DMCB,DMOPEN,SERVICE,FILELIST                               
         LHI   R1,9                ENDED OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD PROGRAMS FILE IN THE PGMS DATASPACE                           *         
***********************************************************************         
         USING LIBUFFD,LIBUFFR                                                  
PGMFILE  NTR1  ,                                                                
         LHI   R1,10               BEGAN CLEARING PROGRAMS FILE                 
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         TIME  DEC                                                              
         ST    R0,TIME1            SAVE TIME OF START OF THIS CLEAR             
*                                                                               
         GOTO1 VDMGR,DMCB,DTFAD,CTFILE,0,0                                      
         L     RF,DMCB+12                                                       
         ST    RF,ACTDTF           GET A(CTFILE DTF)                            
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
*                                                                               
         BRAS  RE,FORCEIT          FORCE FREE DATASPACE                         
         MVI   OKFREE,C'Y'                                                      
         XC    LIBUFFR,LIBUFFR                                                  
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,PGMALET                                                  
         L     R2,PGMOFFS                                                       
         SAC   512                                                              
         ICM   RF,15,PGMSLAST-FAPGMSD(R2)                                       
         BCTR  RF,0                                                             
         ICM   R2,15,PGMSPGM-FAPGMSD(R2)                                        
         USING PGAREAD,R2                                                       
         ST    RF,PGALAST          SET A(END OF PGMS BUILD BLOCK)               
         LR    RF,R2                                                            
         AHI   RF,PGAREAL                                                       
         ST    RF,PGAFRST          SET A(START OF PGMS BUILD BLOCK)             
         ST    RF,PGANOW           SET A(CURRENT)                               
         XC    PGNCORE,PGNCORE                                                  
         MVC   PGAEYE,EYECATCH                                                  
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         BRAS  RE,LOCKIT           LOCK DATASPACE                               
         ICM   RF,15,4(R1)                                                      
         BZ    PSERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         MVC   PSBUFADR,DSPTFRST                                                
*                                                                               
         ICM   RF,15,DSPTEND       GET A(END-1)                                 
         AHI   RF,1                                                             
         STCM  RF,15,PSBUFEND      SAVE A(END)                                  
         S     RF,PSBUFADR                                                      
         AHI   RF,-64                                                           
         ST    RF,PSBUFLEN         SAVE BUFFER SIZE                             
*                                                                               
         L     RF,PSBUFADR                                                      
         AHI   RF,64                                                            
         STCM  RF,15,LIABUFF       RESERVE FIRST 64 BYTES OF BUFFER             
*                                                                               
         MVC   LILBUFF,PSBUFLEN    INITIALISE ARREDIT                           
         MVI   LIACTN,LIAINI                                                    
         OI    LIFLAG1,LIF1ARS                                                  
         MVC   LIALET,PGMALET                                                   
         MVC   LIKEYL,=AL2(PSKEYL)                                              
         MVC   LIRECLMX,=AL2(PROGSPCL)                                          
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLI   LIRTN,LIROK                                                      
         BNE   PSERR                                                            
*                                                                               
         TIME  DEC                                                              
         ST    R0,TIME2            SAVE TIME OF INIT                            
*                                                                               
         XC    KEY,KEY             SET FOR PHASE RECORDS                        
         MVC   KEY(2),=X'0501'                                                  
*                                                                               
         XC    P1(24),P1                                                        
         MVC   P1,ISREAD                                                        
         LA    R6,IO               SET IO AREA ADDRESS                          
         ST    R6,P2                                                            
         MVC   P4,ACTDTF           SET CONTROL FILE                             
         LA    RE,KEY                                                           
         ST    RE,P5               SET A(KEYARG)                                
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BNZ   PSERR                                                            
*                                                                               
         USING CTPHRECD,R6                                                      
         CLC   =X'0501',CTPHPKEY                                                
         BE    BPHS04              AT LEAST 1 PHASE LIST REC FOUND              
         B     PSERR                                                            
*                                                                               
BPHS02   MVC   P1,ISRDSEQ          NEXT PHASE RECORD                            
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BNZ   PSERR                                                            
*                                                                               
         CLC   =X'0501',CTPHPKEY                                                
         BNE   BPHS28              END OF PHASE RECORDS                         
*                                                                               
BPHS04   TM    27(R6),X'80'        IGNORE DELETED PHASE RECORDS                 
         BO    BPHS02                                                           
*                                                                               
         XC    PSREC,PSREC                                                      
         MVC   PSNAME,CTPHHEXN     NAME                                         
         MVC   PSLVL,CTPHLVL       LEVEL                                        
*                                                                               
         LA    RF,LANGTAB          GET LANGUAGE CHARACTERS                      
         XR    R0,R0                                                            
         IC    R0,CTPHLANG                                                      
         CHI   R0,2                                                             
         BH    *+8                                                              
         LHI   R0,0                                                             
         MH    R0,0(RF)                                                         
         AHI   RF,6                                                             
         USING LANGTABD,RF                                                      
         AR    RF,R0                                                            
         MVC   PSLANG,LANGOVL1     USER LANGUAGE CHARACTER                      
         DROP  RF                                                               
*                                                                               
         LA    R3,CTPHFRST         FIND SYSTEM ELEMENT                          
         USING CTPHSYSD,R3                                                      
         XR    RF,RF                                                            
BPHS06   CLI   CTPHSCDE,0          EOR                                          
         BE    NOSYS                                                            
         CLI   CTPHSCDE,CTPHSCEQ                                                
         BE    BPHS08                                                           
         ICM   RF,1,CTPHSYLN                                                    
         BZ    NOSYS                                                            
         BXH   R3,RF,BPHS06                                                     
*                                                                               
NOSYS    DC    H'0'                NEED TO DO SOMETHING ABOUT THIS              
*                                                                               
BPHS08   TM    CTPHSFL1,CTPHSOFQ   OFFLINE?                                     
         BO    BPHS02              YES - IGNORE                                 
*                                                                               
         MVC   PSNODES,CTPHSNDE    SET INFO FROM SYSTEM ELEMENT                 
         MVC   PSFLAG1,CTPHSFL1                                                 
         MVC   PSFLAG2,CTPHSFL2                                                 
         MVC   PSTIME,DATETIME                                                  
*                                                                               
         TM    PSFLAG1,CTPHSDMQ    DUMMY PHASE?                                 
         BZ    BPHS10              NO                                           
         OI    PSFLAG1,CTPHSCRQ    DUMMY ALWAYS CORE RESIDENT                   
*                                                                               
         ICM   R0,15,CTPHSSPR      GET LENGTH REQUESTED                         
         STCM  R0,15,PSLEN                                                      
         AHI   R0,63                                                            
         SRL   R0,6                                                             
         SLL   R0,6                ROUND UP TO NEXT 64 BYTES                    
         STCM  R0,15,PSLENH                                                     
         MVC   PSADR,=AL4(PSADRD)                                               
         B     BPHS20                                                           
*                                                                               
BPHS10   MVC   MODID,SPACES          OUTPUT PHASE NAME                          
         GOTO1 VHEXOUT,DMCB,PSNAME,MODID,3,0                                    
         MVC   MODID(1),PSLANG       SET LANGUAGE                               
         CLI   PSLVL,0             SET LEVEL                                    
         BE    *+10                                                             
         MVC   MODID+6(1),PSLVL                                                 
         MVC   XDUB,MODID                                                       
*                                                                               
         L     RF,ARLLIST          SEE IF RELOAD ENTRY                          
BPHS12   CLI   0(RF),255                                                        
         BE    BPHS14                                                           
         CLC   MODID,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,16                                                            
         B     BPHS12                                                           
*                                                                               
         MVC   XDUB,8(RF)                                                       
         BRAS  RE,FNDSUB                                                        
         MVC   MODID,XDUB                                                       
*                                                                               
BPHS14   LA    R0,MODID            MANUALLY LOAD PHASE SOMEWHERE                
         XC    MODADDR,MODADDR                                                  
         XC    MODLEN,MODLEN                                                    
*                                                                               
         LOAD  EPLOC=(0),ERRET=BPHS16                                           
*                                                                               
         ST    R0,MODADDR          R0 = A(LOADED PHASE)                         
         STCM  R1,7,MODLEN+1       R1 = LENGTH ON DBLWORDS                      
         L     R1,MODLEN                                                        
         SLL   R1,3                                                             
         ST    R1,MODLEN           SET REAL LENGTH                              
*                                                                               
         CLC   MODLEN,MAXLEN       TOO BIG?                                     
         BNH   BPHS20              NO                                           
*                                                                               
         LA    R0,MODID            DELETE LOCAL COPY OF PHASE                   
         DELETE EPLOC=(0)                                                       
*                                                                               
         BRAS  RE,TOOBIG           OUTPUT WARNING AND IGNORE                    
         B     BPHS02                                                           
*                                                                               
BPHS16   CLI   MODID+6,C' '        LOADING LIVE PHASE?                          
         BE    BPHS18              YES                                          
         MVI   MODID+6,C' '        LOOK FOR LIVE PHASE INSTEAD                  
         MVI   ISTEST,C'Y'                                                      
         B     BPHS14                                                           
*                                                                               
BPHS18   LA    R0,NOGOOD           NOT FOUND - SET 07FE + MESSAGE               
         ST    R0,MODADDR                                                       
         LHI   R0,NOGOODL                                                       
         ST    R0,MODLEN           DUMMY LENGTH FOR LATER                       
         BRAS  RE,NOTTHERE                                                      
*                                                                               
BPHS20   CLI   ISTEST,C'Y'         FLAG BAD TEST VERSIONS                       
         BNE   *+12                                                             
         BRAS  RE,FINDUSER                                                      
         BRAS  RE,NOTEST                                                        
         MVI   ISTEST,C'N'                                                      
*                                                                               
         SAM31                     GET INTO XA                                  
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,PGMALET                                                  
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,PGMSPGM-FAPGMSD(R2)                                        
         USING PGAREAD,R2                                                       
*                                                                               
         TM    PSFLAG1,(CTPHSCRQ+CTPHSDMQ+CTPHSCSQ)                             
         BZ    BPHS22                                                           
*                                                                               
         OI    PSFLAG1,CTPHSCRQ                                                 
         ICM   RF,15,PGNCORE       UPDATE COUNT OF CORERES PHASES               
         AHI   RF,1                                                             
         STCM  RF,15,PGNCORE                                                    
*                                                                               
BPHS22   TM    PSFLAG1,CTPHSDMQ    DUMMY PHASE - USES NO PGMS AREA              
         BNZ   BPHS24                                                           
*                                                                               
         ICM   RE,15,PGANOW                                                     
         L     RF,MODLEN           GET LENGTH OF THIS PHASE                     
         AHI   RF,63                                                            
         TML   RF,32                                                            
         BO    *+8                                                              
         AHI   RF,32                                                            
         SRL   RF,6                                                             
         SLL   RF,6                ROUND UP TO NEXT 64 BYTES                    
         AR    RF,RE                                                            
         C     RF,PGALAST                                                       
         BNL   PSERR               PHASE WILL NOT FIT                           
*                                                                               
         STCM  RE,15,PSADR         SET A(PHASE)                                 
         STCM  RF,15,PGANOW        SET NEW ADDRESS                              
         SR    RF,RE                                                            
         STCM  RF,15,PSLENH        SET L'SLOT FOR PHASE                         
*                                                                               
         L     R0,MODADDR          COPY PHASE FROM MODADDR                      
         L     R1,MODLEN                                                        
         STCM  R1,15,PSLEN         SET REAL LENGTH OF PHASE                     
*                                                                               
         CPYA  ARE,AR2             MOVE IN PHASE                                
         MVCL  RE,R0                                                            
*                                                                               
BPHS24   BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
*                                                                               
         LA    R0,MODID            DELETE LOCAL COPY OF PHASE                   
         DELETE EPLOC=(0)                                                       
*                                                                               
         MVI   LIACTN,LIAADD       ADD RECORD TO BUFFER                         
         OI    LIFLAG1,(LIF1INS+LIF1ARS)                                        
         LA    RF,PSREC            GET A(RECORD)                                
         ST    RF,LIAREC                                                        
         MVC   LIRECL,=AL2(PROGSPCL)                                            
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLI   LIRTN,0                                                          
         BE    BPHS26                                                           
*                                                                               
         CLI   LIRTN,7             DUP ON ADD FOR SOME REASON?                  
         BNE   PSERR                                                            
         BRAS  RE,DUPNTRY                                                       
*                                                                               
BPHS26   L     R1,PSBUFACT         APPROXIMATE USAGE OF BUFFER                  
         AHI   R1,PROGSPCL+4                                                    
         STCM  R1,15,PSBUFACT                                                   
         B     BPHS02                                                           
*                                                                               
BPHS28   MVI   LIACTN,LIAFIX       FIX ARRAY (TURN TO LINKED LIST)              
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
*                                                                               
         L     R2,PSBUFADR         SAVE PARAMETER LIST                          
         LAM   AR2,AR2,LIALET                                                   
         SAC   512                                                              
         XC    0(64,R2),0(R2)                                                   
         MVC   0(LIBUFFL,R2),LIBUFFD                                            
*                                                                               
         MVI   LIACTN,LIAREP       REPORT ON SIZES                              
         LA    RF,WORK                                                          
         ST    RF,LIAREC                                                        
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         L     RF,LIAREC                                                        
         MVC   SZNOW,WORK+(LIBCURL-LIBBUFFD)                                    
*                                                                               
         SAC   0                   CLEAR EVERYTHING                             
         LAM   AR0,ARF,ARZERO                                                   
         BRAS  RE,FREEIT                                                        
         DROP  R6                                                               
*                                                                               
         LAM   AR0,ARF,ARZERO        GET PROGRAM AREA INFO                      
         LAM   AR2,AR2,PGMALET                                                  
         L     R2,PGMOFFS                                                       
         SAC   512                                                              
         ICM   R2,15,PGMSPGM-FAPGMSD(R2)                                        
         MVC   AREAD,0(R2)                                                      
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         TIME  DEC                                                              
         ST    R0,TIME3            SAVE TIME TABLE REBUILD                      
         BRAS  RE,DOTIME           OUTPUT TIMING INFORMATION                    
*                                                                               
         CLI   OKFREE,C'Y'                                                      
         BE    BLDCTBX                                                          
         CLI   OKFREE,C'F'                                                      
         BE    PSERR                                                            
         BRAS  RE,NOTEOUTC                                                      
*                                                                               
BLDCTBX  LHI   R1,11               ENDED BUILDING PROGRAMS TABLE                
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL                                                        
         BRAS  RE,NFREPORT                                                      
         B     EXITOK                                                           
*                                                                               
PSERR    SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         GOTO1 VSORTER,DMCB,END,0                                               
         BRAS  RE,FREEIT                                                        
         MVC   TBLID,=CL16'Program File'                                        
         B     FATALITY                                                         
         EJECT                                                                  
***********************************************************************         
* GET RELOAD PHLIST FROM RELOAD FILE                                  *         
***********************************************************************         
GETRLD   NTR1  ,                                                                
         L     R2,ARLLIST                                                       
         OPEN  (RELOAD,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRLD02   GET   RELOAD,PLINE+20                                                  
         CLI   PLINE+20,C'*'                                                    
         BE    GRLD02                                                           
         MVC   0(16,R2),PLINE+20                                                
         AHI   R2,16                                                            
         MVC   PLINE+1(19),=CL19'RELOAD ENTRY'                                  
         BRAS  RE,PRINTL                                                        
         B     GRLD02                                                           
*                                                                               
GETRLDX  MVI   0(R2),255                                                        
         CLOSE RELOAD                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RELOAD DAILY ENTRIES FROM FAC.XXX.RELOAD FILE                       *         
***********************************************************************         
DORELOAD NTR1  ,                                                                
         LHI   R1,12               BEGAN DAILY PROGRAMS FILE RELOAD             
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(19,RLDATE)                                   
*                                                                               
         BRAS  RE,ASKIT            GET TABLE INFO                               
         ICM   RF,15,4(R1)                                                      
         BZ    PSERR                                                            
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         SAM31                     GET INTO XA                                  
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,PGMALET                                                  
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         MVC   LIBUFFR,0(R2)       COPY DSPACE PARAMETER LIST                   
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         SAM24                                                                  
         MVC   LIALET,PGMALET      SET CORRECT ALET                             
         OI    LIFLAG1,LIF1ARS                                                  
         NI    LIFLAG1,255-LIF1INS SET LINKED LIST REQUIRED FOR ANY ADD         
*                                                                               
         GOTO1 VDMGR,DMCB,DTFAD,CTFILE,0,0                                      
         L     RF,DMCB+12                                                       
         ST    RF,ACTDTF           GET A(CTFILE DTF)                            
*                                                                               
         XC    KEY,KEY             SET FOR PHASE RECORDS                        
         MVC   KEY(2),=X'0501'                                                  
*                                                                               
         XC    P1(24),P1                                                        
         MVC   P1,ISREAD                                                        
         LA    R6,IO               SET IO AREA ADDRESS                          
         ST    R6,P2                                                            
         MVC   P4,ACTDTF           SET CONTROL FILE                             
         LA    RE,KEY                                                           
         ST    RE,P5               SET A(KEYARG)                                
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BNZ   PSERR                                                            
*                                                                               
         USING CTPHRECD,R6                                                      
         CLC   =X'0501',CTPHPKEY                                                
         BE    RPHS03              AT LEAST 1 PHASE LIST REC FOUND              
         B     PSERR                                                            
*                                                                               
RPHS02   MVC   P1,ISRDSEQ          NEXT PHASE RECORD                            
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BNZ   PSERR                                                            
*                                                                               
         CLC   =X'0501',CTPHPKEY                                                
         BNE   RPHSX               END OF PHASE RECORDS                         
*                                                                               
RPHS03   TM    CTPHSTAT,X'80'                                                   
         BO    RPHS02                                                           
*                                                                               
         LA    R3,CTPHFRST         FIND SYSTEM ELEMENT                          
         USING CTPHSYSD,R3                                                      
         XR    RF,RF                                                            
RPHS04   CLI   CTPHSCDE,0          EOR                                          
         BE    NORSYS                                                           
         CLI   CTPHSCDE,CTPHSCEQ                                                
         BE    RPHS06                                                           
         ICM   RF,1,CTPHSYLN                                                    
         BZ    NORSYS                                                           
         BXH   R3,RF,RPHS04                                                     
*                                                                               
NORSYS   DC    H'0'                NEED TO DO SOMETHING ABOUT THIS              
*                                                                               
RPHS06   ST    R3,FULL             SAVE POINTER TO SYSTEM ELEMENT               
         CLI   CTPHHEXN+2,0        BASE PROGRAM?                                
         BNE   RPHS18              NO - NO RELOAD LIST THEN                     
*                                                                               
         CLC   RLPGM,CTPHHEXN      SAME BASE AS BEFORE?                         
         BE    RPHS08              YES                                          
         MVC   RLPGM,CTPHHEXN                                                   
         XC    RLLIST,RLLIST                                                    
*                                                                               
RPHS08   CLC   RLDATE,CTPHSRDT     RELOAD TODAY?                                
         BNE   RPHS18              YES                                          
*                                                                               
         USING CTPHLSTD,R3                                                      
RPHS09   CLI   CTPHLCDE,0          EOR                                          
         BE    RPHS10                                                           
         CLI   CTPHLCDE,CTPHLCEQ                                                
         BE    RPHS12                                                           
         ICM   RF,1,CTPHLLEN                                                    
         BZ    NORSYS                                                           
         BXH   R3,RF,RPHS09                                                     
*                                                                               
RPHS10   MVI   RLLIST,255          NO RELOAD LIST - ONLY RELOAD BASE            
         B     RPHS18                                                           
*                                                                               
RPHS12   TM    CTPHLFLG,CTPHLALQ   RELOAD ALL?                                  
         BZ    RPHS14              NO                                           
         MVI   RLLIST,255                                                       
         MVC   RLLIST+1(255),RLLIST                                             
         B     RPHS18                                                           
*                                                                               
RPHS14   LA    R1,CTPHLLST         POINT TO OVERLAY LIST                        
         XR    R0,R0                                                            
         IC    R0,CTPHLLEN         GET ELEMENT LENGTH                           
         AHI   R0,-3               ADJUST FOR OVERHEAD                          
         BNP   RPHS18              IGNORE BAD ELEMENTS                          
*                                                                               
RPHS16   XR    RE,RE                                                            
         IC    RE,0(R1)            GET OVERLAY NUMBER                           
         LA    RE,RLLIST(RE)                                                    
         MVI   0(RE),255           SET RELOAD FLAG IN TABLE                     
         AHI   R1,1                                                             
         BCT   R0,RPHS16                                                        
         DROP  R3                                                               
*                                                                               
RPHS18   L     R3,FULL             RESTORE 05 ELEMENT POINTER                   
         USING CTPHSYSD,R3                                                      
         TM    CTPHSFL1,CTPHSOFQ   TEST OFFLINE ONLY                            
         BO    RPHS02              YES - IGNORE                                 
*                                                                               
         CLC   RLDATE,CTPHSRDT     RELOAD TODAY?                                
         BE    RPHS20              YES                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,CTPHHEXN+2       RELOAD FROM LIST?                            
         LA    RE,RLLIST(RE)                                                    
         CLI   0(RE),255                                                        
         BE    RPHS20              YES                                          
*                                                                               
         XC    PSREC,PSREC         SEE IF RECORD ALREADY EXISTS                 
         MVC   PSNAME,CTPHHEXN     NAME                                         
         MVC   PSLVL,CTPHLVL       LEVEL                                        
*                                                                               
         LA    RF,LANGTAB          GET LANGUAGE CHARACTERS                      
         XR    R0,R0                                                            
         IC    R0,CTPHLANG                                                      
         CHI   R0,2                                                             
         BH    *+8                                                              
         LHI   R0,0                                                             
         MH    R0,0(RF)                                                         
         AHI   RF,6                                                             
         USING LANGTABD,RF                                                      
         AR    RF,R0                                                            
         MVC   PSLANG,LANGOVL1     USER LANGUAGE CHARACTER                      
         DROP  RF                                                               
*                                                                               
         MVC   TPSKEY,PSREC                                                     
         MVI   LIACTN,LIAHIGH      TRY TO READ THE RECORD                       
         LA    RF,PROGSPCD                                                      
         ST    RF,LIAREC                                                        
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLC   TPSKEY,PROGSPCD     FOUND IT?                                    
         BE    RPHS02              YES                                          
         CLI   CTPHLVL,C' '        WAS THERE A LEVEL?                           
         BH    RPHS02              YES, SO DON'T LOAD PREMUTURELY               
*                                                                               
RPHS20   BRAS  RE,NLOCKIT          LOCK TABLE                                   
                                                                                
         XC    PSREC,PSREC         TRY TO GET THE RECORD                        
         MVC   PSNAME,CTPHHEXN     NAME                                         
         MVC   PSLVL,CTPHLVL       LEVEL                                        
*                                                                               
         LA    RF,LANGTAB          GET LANGUAGE CHARACTERS                      
         XR    R0,R0                                                            
         IC    R0,CTPHLANG                                                      
         CHI   R0,2                                                             
         BH    *+8                                                              
         LHI   R0,0                                                             
         MH    R0,0(RF)                                                         
         AHI   RF,6                                                             
         USING LANGTABD,RF                                                      
         AR    RF,R0                                                            
         MVC   PSLANG,LANGOVL1     USER LANGUAGE CHARACTER                      
         DROP  RF                                                               
*                                                                               
         MVC   TPSKEY,PSREC                                                     
         MVI   LIACTN,LIAHIGH      TRY TO READ THE RECORD                       
         LA    RF,PROGSPCD                                                      
         ST    RF,LIAREC                                                        
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         MVI   LIACTN,LIAWRT                                                    
         CLC   TPSKEY,PROGSPCD     FOUND IT?                                    
         BE    RPHS22              YES                                          
*                                                                               
         MVI   LIACTN,LIAADD       ADD RECORD                                   
         XC    PSREC,PSREC                                                      
         MVC   PSREC(PSKEYL),TPSKEY                                             
*                                                                               
RPHS22   TM    PSFLAG1,(CTPHSCRQ+CTPHSDMQ+CTPHSCSQ)                             
         BNZ   RPHS24              THIS WAS ALREADY CORE RESIDENT               
*                                                                               
         SAM31                                                                  
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,PGMALET                                                  
         L     R2,PGMOFFS                                                       
         SAC   512                                                              
         ICM   R2,15,PGMSPGM-FAPGMSD(R2)                                        
         USING PGAREAD,R2                                                       
         ICM   RF,15,PGNCORE       UPDATE COUNT OF CORERES PHASES               
         AHI   RF,1                                                             
         STCM  RF,15,PGNCORE                                                    
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         SAM24                                                                  
         DROP  R2                                                               
*                                                                               
RPHS24   MVC   PSNODES,CTPHSNDE    SET INFO FROM SYSTEM ELEMENT                 
         MVC   PSFLAG1,CTPHSFL1                                                 
         MVC   PSFLAG2,CTPHSFL2                                                 
         MVC   PSTIME,DATETIME     SET RELOAD DATE AND TIME                     
*                                                                               
         TM    PSFLAG1,CTPHSDMQ    DUMMY PHASE?                                 
         BZ    RPHS26              NO                                           
         OI    PSFLAG1,CTPHSCRQ    DUMMY ALWAYS CORE RESIDENT                   
*                                                                               
         ICM   R0,15,CTPHSSPR      GET LENGTH REQUESTED                         
         STCM  R0,15,PSLEN                                                      
         AHI   R0,63                                                            
         SRL   R0,6                                                             
         SLL   R0,6                ROUND UP TO NEXT 64 BYTES                    
         TML   R0,32                                                            
         BO    *+8                                                              
         AHI   R0,32                                                            
         STCM  R0,15,PSLENH                                                     
         MVC   PSADR,=AL4(PSADRD)                                               
         B     RPHS30                                                           
*                                                                               
RPHS26   MVC   DUB,SPACES          OUTPUT PHASE NAME                            
         GOTO1 VHEXOUT,DMCB,PSNAME,DUB,3,0                                      
         MVC   DUB(1),PSLANG       SET LANGUAGE                                 
         CLI   PSLVL,0             SET LEVEL                                    
         BE    *+10                                                             
         MVC   DUB+6(1),PSLVL                                                   
         MVC   XDUB,DUB                                                         
*                                                                               
RPHS28   L     RF,ALOAD            PUT PHASE INTO ALOAD                         
         GOTO1 ALOADER,DMCB,DUB,(RF),(C'M',(RF))                                
         ICM   R0,15,4(R1)                                                      
         BNZ   RPHS30                                                           
*                                                                               
         CLI   DUB+6,C' '          LOADING LIVE PHASE?                          
         BE    *+12                YES                                          
         MVI   DUB+6,C' '          LOOK FOR LIVE PHASE INSTEAD                  
         B     RPHS28                                                           
*                                                                               
         L     RF,ALOAD            NOT FOUND - SET 07FE                         
         MVC   0(2,RF),=X'07FE'                                                 
         ST    RF,DMCB+4                                                        
         LHI   RF,2                DUMMY LENGTH FOR LATER                       
         ST    RF,DMCB                                                          
         MVC   MODLEN,DMCB                                                      
         BRAS  RE,NOTTHERE                                                      
*                                                                               
RPHS30   BRAS  RE,RLDED            OUTPUT RELOADED MESSAGE                      
*                                                                               
         SAM31                                                                  
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,PGMALET                                                  
         L     R2,PGMOFFS                                                       
         SAC   512                                                              
         ICM   R2,15,PGMSPGM-FAPGMSD(R2)                                        
         USING PGAREAD,R2                                                       
         TM    PSFLAG1,(CTPHSCRQ+CTPHSDMQ+CTPHSCSQ)                             
         BZ    *+8                                                              
         OI    PSFLAG1,CTPHSCRQ                                                 
*                                                                               
         TM    PSFLAG1,CTPHSDMQ    DUMMY PHASE - USES NO PGMS AREA              
         BNZ   RPHS34                                                           
         CLC   PSLENH,DMCB         WILL IT FIT IN CURRENT SLOT?                 
         BH    RPHS32              YES                                          
*                                                                               
         ICM   RE,15,PGANOW                                                     
         L     RF,DMCB             GET LENGTH OF THIS PHASE                     
         AHI   RF,63                                                            
         SRL   RF,6                                                             
         SLL   RF,6                ROUND UP TO NEXT 64 BYTES                    
         TML   RF,32                                                            
         BO    *+8                                                              
         AHI   RF,32                                                            
         AR    RF,RE                                                            
         C     RF,PGALAST                                                       
         BNL   PSERR               PHASE WILL NOT FIT                           
*                                                                               
         STCM  RE,15,PSADR         SET A(PHASE)                                 
         STCM  RF,15,PGANOW        SET NEW ADDRESS                              
         SR    RF,RE                                                            
         STCM  RF,15,PSLENH        SET L'SLOT FOR PHASE                         
*                                                                               
RPHS32   L     R0,ALOAD            COPY PHASE FROM ALOAD                        
         L     R1,DMCB                                                          
         STCM  R1,15,PSLEN         SET REAL LENGTH OF PHASE                     
         ICM   RE,15,PSADR         GET A(PHASE)                                 
         ICM   RF,15,PSLENH        GET L'SLOT FOR PHASE                         
         CPYA  ARE,AR2                                                          
         MVCL  RE,R0               MOVE IN PHASE                                
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         SAM24                                                                  
*                                                                               
RPHS34   LA    RF,PSREC            SET A(RECORD)                                
         ST    RF,LIAREC                                                        
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLI   LIRTN,0                                                          
         BNE   PSERR                                                            
         BRAS  RE,FREEIT           FREE TABLE                                   
         B     RPHS02                                                           
*                                                                               
RPHSX    LHI   R1,13               ENDED DAILY PROGRAMS FILE RELOAD             
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* LOCKSPC ROUTINES                                                    *         
***********************************************************************         
ASKIT    LR    R0,RE               ENQUIRE ON TABLE                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DPDPGMS)                                             
         MVI   DUB,X'20'                                                        
         GOTO1 VLOCKSPC,DUB        CALL LOCKSPC                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
FORCEIT  LR    R0,RE               FORCE FREE TABLE                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DPDPGMS)                                             
         MVI   DUB,X'11'           FORCE FREE                                   
         GOTO1 VLOCKSPC,DUB        CALL LOCKSPC                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LOCKIT   LR    R0,RE               LOCK TABLE                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DPDPGMS)                                             
         MVI   DUB,X'80'           SET LONG ALLOCATE                            
         GOTO1 VLOCKSPC,DUB        CALL LOCKSPC                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
NLOCKIT  LR    R0,RE               LOCK TABLE                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DPDPGMS)                                             
         MVI   DUB,X'00'           SET NORMAL ALLOCATE                          
         GOTO1 VLOCKSPC,DUB        CALL LOCKSPC                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
FREEIT   LR    R0,RE               FREE TABLE                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DPDPGMS)                                             
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        CALL LOCKSPC                                 
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE REQUESTED ENTRIES WITH NO TEST PHASE              *         
***********************************************************************         
FNDSUB   NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(12),=C'Reload phase'                                    
         MVC   PLINE+26(7),XDUB                                                 
         MVC   PLINE+34(15),=CL15'substituted for'                              
         MVC   PLINE+50(6),MODID                                                
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE REQUESTED ENTRIES WITH NO TEST PHASE              *         
***********************************************************************         
NOTEST   NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(10),=C'Test phase'                                      
         MVC   PLINE+24(7),XDUB                                                 
         MVC   PLINE+32(24),=C'not found - substituting'                        
         MVC   PLINE+57(6),MODID                                                
         MVC   PLINE+64(7),=CL7'instead'                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         CLC   =C'INIT',MODE                                                    
         BNE   EXITOK                                                           
*                                                                               
         XC    NFFLIN,NFFLIN                                                    
         MVC   NFFNAME,XDUB                                                     
         MVI   NFFSTAT,NFFNT                                                    
         TM    PSFLAG1,(CTPHSCRQ+CTPHSDMQ+CTPHSCSQ)                             
         BZ    *+8                                                              
         MVI   NFFCORE,C'Y'                                                     
         MVC   NFFLEN,MODLEN                                                    
         OC    MODUSER,MODUSER                                                  
         BNZ   *+10                                                             
         MVC   MODUSER,=XL4'FFFFFFFF'                                           
         MVC   NFFUSER,MODUSER                                                  
         MVC   NFFBOOK,MODBOOK                                                  
         MVC   NFFLKDAT,MODLKDAT                                                
         GOTO1 VSORTER,DMCB,PUT,NFFLIN                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE REQUESTED ENTRIES THAT ARE NOT FOUND              *         
***********************************************************************         
NOTTHERE NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(5),=C'Phase'                                            
         MVC   PLINE+19(8),XDUB                                                 
         MVC   PLINE+27(20),=CL20'not found in CIL'                             
         BRAS  RE,PRINTL           PUT NOT FOUND MESSAGE TO SYSPRINT            
*                                                                               
         CLC   =C'INIT',MODE                                                    
         BNE   EXITOK                                                           
*                                                                               
         XC    NFFLIN,NFFLIN                                                    
         MVC   NFFNAME,XDUB                                                     
         MVI   NFFSTAT,NFFNF                                                    
         MVC   NFFLEN,MODLEN                                                    
         GOTO1 VSORTER,DMCB,PUT,NFFLIN                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE DUPLICATE ENTRIES IN DATASPACE                    *         
***********************************************************************         
DUPNTRY  NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(5),=C'Phase'                                            
         MVC   PLINE+19(8),XDUB                                                 
         MVC   PLINE+27(30),=CL30'already in DSPACE - RC=7'                     
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE ENTRIES THAT ARE TOO BIG TO LOAD                  *         
***********************************************************************         
TOOBIG   NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(5),=C'Phase'                                            
         MVC   PLINE+19(8),XDUB                                                 
         MVC   PLINE+27(30),=CL30'Is too big to be loaded'                      
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE ENTRIES THAT ARE RELOADED                         *         
***********************************************************************         
RLDED    NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+01(L'TIME),TIME                                            
         MVC   PLINE+13(5),=C'Phase'                                            
         MVC   PLINE+19(L'XDUB),XDUB                                            
         MVC   PLINE+27(20),=CL20'reloaded'                                     
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TIMING AND SIZE MESSAGE OUTPUT                                      *         
* NTRY: TIME1   START TIME                                            *         
*       TIME2   CLEAR TIME                                            *         
*       TIME3   BUILT TIME                                            *         
*       SZNOW   CURRENT TABLE SIZE                                    *         
*       DSPHD   DATASPACE INFORMATION                                 *         
*       AREAD   DATASPACE INFORMATION                                 *         
***********************************************************************         
         PUSH  USING                                                            
DOTIME   NTR1  ,                                                                
         L     R0,TIME1            START TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   DMSGT1,TIME                                                      
         L     R0,TIME2            CLEAR TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   DMSGT2,TIME                                                      
         L     R0,TIME3            REBUILT TIME                                 
         BRAS  RE,TIMER                                                         
         MVC   DMSGT3,TIME                                                      
*                                                                               
         ICM   R0,15,DSPTEND       MAX SIZE OF TABLE                            
         AHI   R0,1                                                             
         ICM   RF,15,DSPTFRST                                                   
         N     RF,=XL4'0FFFFFFF'                                                
         SR    R0,RF               R0 = MAX SIZE                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGMAX,DUB                                                      
*                                                                               
         ICM   RF,15,SZNOW         CURRENT SIZE OF TABLE                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGNOW,DUB                                                      
*                                                                               
         MVC   DMSG5PCT,=CL3'???'                                               
         LR    RE,R0               (MAX-NOW)/MAX*100                            
         SR    RE,RF                                                            
         BP    *+12                                                             
         MVI   OKFREE,C'F'         TABLE HAS OVERFLOWED                         
         B     DOTM02                                                           
*                                                                               
         SRDL  RE,32                                                            
         M     RE,=F'100'                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)            ROUND UP                                     
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSG5PCT,DUB                                                     
*                                                                               
         CHI   RF,10               MORE THAN 10% STILL UNUSED?                  
         BH    *+8                 YES                                          
         MVI   OKFREE,C'N'         OTHER TABLE TYPE NEEDS CHANGING              
*                                                                               
         USING PGAREAD,AREAD                                                    
DOTM02   ICM   R0,15,PGALAST       MAX SIZE OF TABLE                            
         AHI   R0,1                                                             
         ICM   RF,15,PGAFRST                                                    
         SR    R0,RF               R0 = MAX SIZE                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGTMAX,DUB                                                     
*                                                                               
         ICM   R0,15,PGANOW        CURRENT SIZE OF TABLE                        
         SR    R0,RF               R0 = CURRENT SIZE                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGTNOW,DUB                                                     
*                                                                               
         MVC   DMSGTPCT,=CL3'???'                                               
         ICM   R0,15,PGALAST       MAX SIZE OF TABLE                            
         AHI   R0,1                                                             
         LR    RE,R0                                                            
         ICM   RF,15,PGANOW                                                     
         SR    RE,RF               (MAX-NOW)/MAX*100                            
         BP    *+12                                                             
         MVI   OKFREE,C'F'         TABLE HAS OVERFLOWED                         
         B     DOTM10                                                           
*                                                                               
         SRDL  RE,32                                                            
         M     RE,=F'100'                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)            ROUND UP                                     
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGTPCT,DUB                                                     
*                                                                               
         CHI   RF,10               MORE THAN 10% STILL UNUSED?                  
         BH    *+8                 YES                                          
         MVI   OKFREE,C'N'         OTHER TABLE TYPE NEEDS CHANGING              
*                                                                               
DOTM10   XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+1(L'TIME),TIME                                             
         MVC   PLINE+L'TIME+2(L'DMSG1),DMSG1                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG2),DMSG2                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG3),DMSG3                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG4),DMSG4                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG5),DMSG5                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG6),DMSG6                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG7),DMSG7                                    
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+L'TIME+2(L'DMSG8),DMSG8                                    
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTL           PRINT CLEAR LINE UNDERNEATH                  
         B     EXITOK                                                           
*                                                                               
DMSG1    DC    CL60'Programs file rebuild - Timing/Size Information'            
         ORG   DMSG1+L'DMSG1                                                    
*                                                                               
DMSG2    DC    CL60' '                                                          
         ORG   DMSG2                                                            
         DC    C'Routine started at '                                           
DMSGT1   DC    CL11' '                                                          
         DC    C'  '                                                            
         ORG   DMSG2+L'DMSG2                                                    
*                                                                               
DMSG3    DC    CL110' '                                                         
         ORG   DMSG3                                                            
         DC    C'Table cleared at   '                                           
DMSGT2   DC    CL11' '                                                          
         DC    C'     '                                                         
         DC    C'Table rebuilt at   '                                           
DMSGT3   DC    CL11' '                                                          
         ORG   DMSG3+L'DMSG3                                                    
*                                                                               
DMSG4    DC    CL110' '                                                         
         ORG   DMSG4                                                            
         DC    C'Allocated size     '                                           
DMSGMAX  DC    CL8' '                                                           
         DC    C' bytes  '                                                      
         DC    C'Amount in use      '                                           
DMSGNOW  DC    CL8' '                                                           
         DC    C' bytes'                                                        
         ORG   DMSG4+L'DMSG4                                                    
*                                                                               
DMSG5    DC    CL60' '                                                          
         ORG   DMSG5                                                            
         DC    C'Percentage free    '                                           
DMSG5PCT DC    CL3' '                                                           
         DC    C'%'                                                             
         ORG   DMSG5+L'DMSG5                                                    
*                                                                               
DMSG6    DC    CL60'Programs area rebuild - Size Information'                   
         ORG   DMSG6+L'DMSG6                                                    
*                                                                               
DMSG7    DC    CL110' '                                                         
         ORG   DMSG7                                                            
         DC    C'Allocated size     '                                           
DMSGTMAX DC    CL8' '                                                           
         DC    C' bytes  '                                                      
         DC    C'Amount in use      '                                           
DMSGTNOW DC    CL8' '                                                           
         DC    C' bytes'                                                        
         ORG   DMSG7+L'DMSG7                                                    
*                                                                               
DMSG8    DC    CL60' '                                                          
         ORG   DMSG8                                                            
         DC    C'Percentage free    '                                           
DMSGTPCT DC    CL3' '                                                           
         DC    C'%'                                                             
         ORG   DMSG8+L'DMSG8                                                    
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME FROM MVS TIME MACRO                                     *         
* NTRY R0 = ZERO USE CURRENT TIME (FROM MVS TIME MACRO)               *         
*      R0 = NZ   USE TIME IN HHMMSSXX IN R0                           *         
* EXIT TIME HOLDS HH:MM:SS:XX WHERE XX IS 1/100 SECS                  *         
***********************************************************************         
TIMER    NTR1  ,                                                                
         LTR   R0,R0                                                            
         BNZ   TIME02                                                           
*                                                                               
         TIME  DEC                 R0=TIME                                      
*                                                                               
TIME02   STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           XX PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'                                                      
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C':'                                                      
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
GETSPC   NTR1  ,                                                                
         LHI   R1,4                                                             
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    GETSPC2                                                          
*                                                                               
         LHI   R1,6                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 911,DUMP                                                         
*                                                                               
GETSPC2  MVC   PGMOFFS,WORK+20     EXTRACT VALUES                               
         MVC   PGMALET,WORK+24                                                  
*                                                                               
         USING SSBD,RE                                                          
         LARL  RE,SSB                                                           
*        MVC   SSOTBLET,PGMALET                                                 
         MVC   SSOPGMTO,PGMOFFS                                                 
         MVC   SSOPGMTA,PGMALET                                                 
         DROP  RE                                                               
*                                                                               
         OC    PGMALET,PGMALET                                                  
         BNZ   GETSPC4                                                          
*                                                                               
         LHI   R1,7                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 911,DUMP                                                         
*                                                                               
GETSPC4  LHI   R1,5                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CLEAR PROGRAMS CHECKPOINT AREAS                                     *         
***********************************************************************         
CHKPT    NTR1  ,                                                                
         GOTO1 VDMGR,DMCB,DTFAD,PRGMS,0,0                                       
         L     RF,12(R1)                                                        
         ST    RF,VPRGMS                                                        
*                                                                               
         XC    P1(24),P1           WRITE CHKPNT 1 TO 1ST TRK OF PRGMS           
         MVC   P1,VWTCKD                                                        
         L     RE,VCHKPT1                                                       
         ST    RE,P2                                                            
         L     RF,VCHKPT1X                                                      
         SR    RF,RE               SUBT TO GET LENGTH                           
         ST    RF,P3               SET LEN                                      
         L     RE,VPRGMS                                                        
         ST    RE,P4                                                            
         USING DTFPHD,RE                                                        
         MVC   DNEXT,=X'00010000'  SET TRK 1/BLK 0                              
         LA    R0,P6                                                            
         ST    R0,P5               SET A(DA) ON RETURN                          
         LA    R1,P1                                                            
         LHI   R8,PHLSTRK-1        WRITE UP TO PHLIST TRACK                     
*                                                                               
CHKPT02  L     RF,VDADDS                                                        
         BASR  RE,RF                                                            
         OC    P3(2),P3                                                         
         BZ    CHKPT04                                                          
         ABEND 311,DUMP                                                         
*                                                                               
CHKPT04  L     RE,VPRGMS                                                        
         LH    RF,DNEXT            WRITE FIRST PHLSTTK TRACKS                   
         USING DTFPHD,RE                                                        
         LA    RF,1(RF)                                                         
         SLL   RF,16                                                            
         ST    RF,DNEXT                                                         
         BRCT  R8,CHKPT02                                                       
         B     EXITOK                                                           
*                                                                               
PHLSTRK  EQU   36                  PHASE LIST TRACK NUMBER                      
VWTCKD   DC    A(5)                                                             
VCHKPT1  DC    V(CHKPT1)                                                        
VCHKPT1X DC    V(CHKPT1X)                                                       
VPRGMS   DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE REPORT ON PHASES THAT WERE NOT FOUND                          *         
***********************************************************************         
NFREPORT NTR1  ,                                                                
         XC    TOTLEN,TOTLEN                                                    
         XC    TOTCORE,TOTCORE                                                  
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
*                                                                               
         MVC   SUB(3),SYSCODE                                                   
*                                                                               
NFR04    GOTO1 VSMTP,DMCB,('SMTPAPRS',TO),(L'SUB,SUB)                           
*                                                                               
         XR    R0,R0                                                            
NFR06    GOTO1 VSORTER,DMCB,GET,0                                               
         ICM   R2,15,DMCB+4                                                     
         BZ    NFR30                                                            
*                                                                               
         MVC   NFFLIN,0(R2)                                                     
*                                                                               
* IF NFFSTAT CHANGES, FORCE PRINTING OF CORRESPONDING HEADER                    
*                                                                               
         CLC   SAVENFF,NFFSTAT                                                  
         BE    *+14                                                             
         MVC   SAVENFF,NFFSTAT                                                  
         MVI   DONEHDR,C'N'                                                     
*                                                                               
* CHECK IF WE NEED TO PRINT HEADER                                              
*                                                                               
         CLI   DONEHDR,C'Y'                                                     
         BE    NFR10                                                            
*                                                                               
         MVI   DONEHDR,C'Y'                                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
         CLI   SAVENFF,NFFNF                                                    
         BNE   *+10                                                             
         MVC   DATA(L'DATA1),DATA1                                              
*                                                                               
         CLI   SAVENFF,NFFNT                                                    
         BNE   *+10                                                             
         MVC   DATA(L'DATA2),DATA2                                              
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
NFR10    DS    0H                                                               
         CLI   SAVENFF,NFFNF                                                    
         BNE   NFR12                                                            
*                                                                               
         LR    RF,R0                                                            
         MHI   RF,20                                                            
         LA    RF,DATA(RF)                                                      
         MVC   0(L'NFFNAME,RF),NFFNAME                                          
*                                                                               
         CLI   NFFCORE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   L'NFFNAME+2(RF),C'+'                                             
*                                                                               
         AHI   R0,1                WE CAN FIT 4 ON A LINE HERE                  
         CHI   R0,4                                                             
         BL    NFR06               NEXT SORT RECORD                             
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
         XR    R0,R0                                                            
         B     NFR06                                                            
*                                                                               
NFR12    DS    0H                                                               
         CLI   SAVENFF,NFFNT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEUSER,NFFUSER    USER CHANGED?                                
         BE    NFR15                                                            
*                                                                               
         MVC   SAVEUSER,NFFUSER                                                 
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)      BLANK LINE                     
         MVC   DATA,SPACES                                                      
*                                                                               
         CLI   NFFUSER,X'FF'       USER UNKNOWN?                                
         BNE   *+14                                                             
         MVC   DATA(10),=CL10'UNKNOWN:'                                         
         B     *+14                                                             
         MVC   DATA(L'NFFUSER),NFFUSER                                          
         MVI   DATA+L'NFFUSER,C':'                                              
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
NFR15    DS    0H                                                               
DT       USING NEXISTD,DATA                                                     
*                                                                               
         MVC   DT.NEXNAME,NFFNAME  PHASE NAME                                   
         CLI   NFFCORE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   DT.NEXCORE,C'+'                                                  
*                                                                               
         OC    NFFBOOK,NFFBOOK     PANBOOK NAME                                 
         BZ    *+10                                                             
         MVC   DT.NEXBOOK,NFFBOOK                                               
*                                                                               
         OC    NFFLEN,NFFLEN                                                    
         BZ    NFR20                                                            
*                                                                               
         EDIT  NFFLEN,DT.NEXLEN,ALIGN=RIGHT                                     
*                                                                               
         ICM   RE,15,NFFLEN                                                     
         A     RE,TOTLEN                                                        
         ST    RE,TOTLEN                                                        
*                                                                               
         CLI   NFFCORE,C'Y'                                                     
         BNE   NFR20                                                            
*                                                                               
         ICM   RE,15,NFFLEN                                                     
         A     RE,TOTCORE                                                       
         ST    RE,TOTCORE                                                       
*                                                                               
NFR20    DS    0H                                                               
         OC    NFFLKDAT,NFFLKDAT   DATE LAST LINKED                             
         BZ    *+10                                                             
         MVC   DT.NEXLKDAT,NFFLKDAT                                             
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
         B     NFR06                                                            
*                                                                               
NFR30    DS    0H                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
         OC    TOTLEN,TOTLEN                                                    
         BZ    NFR40                                                            
*                                                                               
         MVC   DATA(L'DATA3),DATA3                                              
         EDIT  TOTLEN,(8,DATA+L'DATA3+2),ALIGN=LEFT                             
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
NFR40    OC    TOTCORE,TOTCORE                                                  
         BZ    NFR50                                                            
*                                                                               
         MVC   DATA(L'DATA4),DATA4                                              
         EDIT  TOTLEN,(8,DATA+L'DATA4+2),ALIGN=LEFT                             
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',DATA)                                     
         MVC   DATA,SPACES                                                      
*                                                                               
NFR50    GOTO1 VSMTP,DMCB,('SMTPASND',0)                                        
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)                                        
         GOTO1 VSORTER,DMCB,END,0                                               
         B     EXITOK                                                           
*                                                                               
DATA     DS    CL80' '                                                          
SAVENFF  DC    X'00'                                                            
DONEHDR  DC    C'N'                                                             
SAVEUSER DC    CL4'    '                                                        
*                                                                               
TO       DC    CL60'USMFPROG:'                                                  
*TO       DC    CL60'TZIH:'                                                     
*                                                                               
SUB      DC    CL60'??? PROGRAMS DATASPACE BUILD REPORT'                        
DATA1    DC    CL80'The following phases are not in loadlib'                    
DATA2    DC    CL80'These test phases DO NOT EXIST - Live substituted'          
DATA3    DC    C'AMOUNT OF DATASPACE STORAGE WASTED '                           
DATA4    DC    C'AMOUNT OF FACPAK CORE WASTED       '                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEND A LOTUS NOTE IF PROGRAM TABLE GETS TOO BIG          *         
***********************************************************************         
NOTEOUTC NTR1  ,                                                                
         WTO   'PGMTAB size problem found - sending note'                       
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         MVC   SUBC(3),SYSCODE                                                  
*                                                                               
NOC04    GOTO1 VSMTP,DMCB,('SMTPAPRS',TO),(L'SUBC,SUBC)                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',LINE1)                                    
         GOTO1 VSMTP,DMCB,('SMTPASND',0)                                        
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)                                        
         GOTO1 VSORTER,DMCB,END,0                                               
         J     EXITOK                                                           
*&&US                                                                           
TOC      DC    CL60'US-MF_FAC_NOTIFY'                                           
*&&                                                                             
*&&UK                                                                           
TOC      DC    CL60'RMOR,TCLE:'                                                 
*&&                                                                             
SUBC     DC    CL60'??? PROGRAMS DATASPACE SIZE PROBLEM'                        
LINE1    DC    CL80'Check last night PROGSPC Job output for details'            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCAN31),DMCB,(R2),SCNBLK,0,(1,SCICARD),20                     
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         USING SCANBLKD,R2                                                      
         LA    R2,SCNBLK                                                        
*                                                                               
         USING CARDTABD,R3                                                      
         LA    R3,CARDTAB                                                       
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     EXITOK                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'INVALID LINE FORMAT'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'INVALID KEYWORD'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'VALUE NOT A VALID NUMBER'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'VALUE NOT A VALID CHARACTER STRING'                     
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'LENGTH OF INPUT STRING TOO SHORT'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'LENGTH OF INPUT STRING TOO LONG'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'NUMERIC VALUE TOO SMALL'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'NUMERIC VALUE TOO LARGE'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'INVALID/MISSING VALUE'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+1(15),=CL15' *** ERROR ***'                                
         MVC   PLINE+16(40),0(R1)                                               
         BAS   RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         PUT   SYSPRINT,PTITLE     PRINT TITLES                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
PRINTL   NTR1  ,                                                                
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXIT                EXIT                                         
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FATAL ERROR ON BUILD - HAVE TO PUT OUT WTOR AND ABEND               *         
***********************************************************************         
FATALITY SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         MVC   ERR1TB,TBLID                                                     
*                                                                               
         XR    R0,R0               MUST BE CLEAR FOR WTOR                       
         WTOR  TEXT=(ERR1L,RESPND,12,REPLECB)                                   
         WAIT  ECB=REPLECB                                                      
         ABEND 911,DUMP                                                         
*                                                                               
ERR1L    DC    AL2(65)                                                          
         DC    CL25'PROGSPC - Error building '                                  
ERR1TB   DC    CL16' '                                                          
         DC    CL25'Recycle PGMS dataspace'                                     
*                                                                               
REPLECB  DC    F'0'                                                             
RESPND   DC    CL12' '                                                          
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
DOMSG    NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
*                                                                               
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+1(L'TIME),TIME                                             
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   PLINE+L'TIME+2(L'MESSTAB),0(R1)                                  
*                                                                               
DOMSG02  BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS AND OTHER USEFUL ROUTINES                                     *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    BRAS  RE,PRINTX           CLOSE PRINTING AND EXIT TO MVS               
         L     RD,SAVERD                                                        
         XBASE                                                                  
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'CARDTAB*CARDTAB*'                                           
*                                                                               
CARDTAB  DC    CL8'MODE    ',F'001',F'0000010'                                  
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'DDSIO   ',F'001',F'0000012'                                  
         DC    AL1(04,CTCHR,8,0),V(DDSIO)                                       
         DC    CL8'FAPARMS ',F'001',F'0000008'                                  
         DC    AL1(06,CTCHR,L'FAPARM,0),A(FAPARM)                               
CARDTABX DC    AL1(CARDEOT)                                                     
         DS    0L                                                               
         DC    CL16'CARDTABXCARDTABX'                                           
         EJECT                                                                  
***********************************************************************         
* FIND LAST USER TO LINK THE PHASE LOADED AT MODADDR                  *         
***********************************************************************         
FINDUSER NTR1                                                                   
         XC    MODUSER,MODUSER                                                  
         XC    MODBOOK,MODBOOK                                                  
         XC    MODLKDAT,MODLKDAT                                                
*                                                                               
         ICM   RE,15,MODADDR                                                    
         JZ    EXITOK                                                           
*                                                                               
         L     RE,MODADDR          PHASE START                                  
         LR    RF,RE                                                            
         A     RF,MODLEN           PHASE END                                    
         SHI   RF,15           SO THE CLC DOESN'T RUN PAST END OF PHASE         
*                                                                               
FUSER10  DS    0H                                                               
         CR    RE,RF               REACHED END OF PHASE?                        
         JNL   EXITOK              YES - EXIT                                   
*                                                                               
         OC    MODUSER,MODUSER     FOUND "COMPILED ON" ALREADY?                 
         BNZ   FUSER25             YES - NOW LOOK FOR "LNKSTAMP"                
*                                                                               
         CLC   =C' COMPILED ON',0(RE)                                           
         BNE   FUSER30             NEXT BYTE                                    
*                                                                               
* FOUND AN INSTANCE OF "COMPILED ON" HERE                                       
*                                                                               
         MVC   MODUSER,LVLUSER-LVLCMLAB(RE)                                     
         MVC   MODLKDAT,LVLCDATE-LVLCMLAB(RE)                                   
*                                                                               
         LHI   R0,LVLCMLAB-LVLSTMPD   BYTES BACK TO START OF LVL STAMP          
         CLC   =C'BOOK=',0(RE)                                                  
         BE    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-12                                                          
*                                                                               
         MVC   MODBOOK,LVLBOOK-LVLBKLAB(RE)       SAVE PANBOOK                  
         B     FUSER30             NEXT BYTE                                    
         DROP  RE                                                               
*                                                                               
FUSER25  DS    0H                                                               
         CLC   =C'LNKSTAMP',0(RE)                                               
         BNE   FUSER30             NEXT BYTE                                    
         XC    MODUSER,MODUSER     NEED TO LOOK FOR NEXT "COMPILED ON"          
         XC    MODLKDAT,MODLKDAT                                                
*                                                                               
FUSER30  DS    0H                                                               
         LA    RE,1(RE)                                                         
         B     FUSER10                                                          
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
PROGSPC  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
K        EQU   1024                                                             
EOT      EQU   X'FF'                                                            
CTMAXLEN EQU   1024                                                             
CTMAXID  EQU   1000                                                             
IOL      EQU   2*K                                                              
MAXLEN   DC    A(132*K)            MAX PHASE LENGTH SUPPORTED                   
ARZERO   DC    16F'0'                                                           
*                                                                               
DSTAB    DS    0CL4                                                             
*&&US*&& DC    C'AADVRREPCCSC2MELTTSTQFQA'                                      
*&&UK*&& DC    C'AADVNNEWSTTSTTSTQFQACCSC'                                      
         DC    X'FF'                                                            
*                                                                               
ISREAD   DC    F'1'                                                             
ISRDSEQ  DC    F'2'                                                             
*                                                                               
VSORTER  DC    V(SORTER)                                                        
VSMTP    DC    V(SMTP)                                                          
VHEXOUT  DC    V(HEXOUT)                                                        
VISDDS   DC    V(ISDDS)                                                         
VCARDS   DC    V(CARDS)                                                         
VARREDIT DC    V(ARREDIT)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
VPQOPEN  DC    V(PQOPEN)                                                        
VPQBUFF  DC    V(PQBUFF)                                                        
VDATCON  DC    V(DATCON)                                                        
VDATTIM  DC    V(DATTIM)                                                        
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VPRINT   DC    V(PRINT)                                                         
VLOADER  DC    V(LOADER)                                                        
VPRINTER DC    V(PRINTER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VHELLO   DC    V(HELLO)                                                         
VSCANNER DC    V(SCANNER)                                                       
ALOADER  DC    V(LOADER)                                                        
*                                                                               
ALOAD    DC    A(LOADAREA)                                                      
AMESSTAB DC    A(MESSTAB)                                                       
ARLLIST  DC    A(RLLST)                                                         
*                                                                               
MODID    DS    D                                                                
MODADDR  DS    A                                                                
MODLEN   DS    F                                                                
MODUSER  DS    CL4                                                              
MODBOOK  DS    CL10                                                             
MODLKDAT DS    CL8                                                              
*                                                                               
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
END      DC    CL4'END '                                                        
*                                                                               
JESMAIL  DC    CL8'JESMAIL'                                                     
DMOPEN   DC    CL8'OPEN'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
DTFAD    DC    CL8'DTFAD '                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
PRGMS    DC    CL8'PRGMS '                                                      
*                                                                               
EYECATCH DC    CL16'****PROGRAMS****'                                           
*                                                                               
NOGOOD   DC    XL2'07FE',C'NOT FOUND IN LOADLIB'                                
NOGOODL  EQU   *-NOGOOD                                                         
*                                                                               
SERVICE  DC    CL8'SERVICE'                                                     
FILELIST DC    C'NCTFILE UPRGMS  X'                                             
*                                                                               
CTITLE   DC    C'Programs file rebuild information'                             
CTITLEU  DC    C'---------------------------------'                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,1,A,2,4,A,5,8,A),FORMAT=BI,WORK=1'           
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(38,,,,) '                             
SPACES   DC    132C' '                                                          
         EJECT                                                                  
***********************************************************************         
* FAPGMSDEQU                                                          *         
***********************************************************************         
       ++INCLUDE FAPGMSDEQU                                                     
         EJECT                                                                  
***********************************************************************         
* FALANGTAB                                                           *         
***********************************************************************         
       ++INCLUDE FALANGTAB                                                      
         EJECT                                                                  
***********************************************************************         
* UTL AND SSB FOR DDSIO                                               *         
***********************************************************************         
         DS    0L                                                               
         DC    CL8'PSBUFADR'                                                    
PSBUFADR DC    A(0)                A(BUFFER)                                    
         DC    A(0)                                                             
         DC    CL8'PSBUFEND'                                                    
PSBUFEND DC    A(0)                A(BUFFER END-1)                              
         DC    A(0)                                                             
         DC    CL8'PSBUFLEN'                                                    
PSBUFLEN DC    F'0'                LENGTH OF BUFFER                             
         DC    A(0)                                                             
         DC    CL8'PSBUFACT'                                                    
PSBUFACT DC    F'0'                USED LENGTH OF BUFFER                        
         DC    A(0)                                                             
         DC    CL8'PSBUFCNT'                                                    
PSBUFCNT DC    F'0'                NUMBER OF RECORDS IN BUFFER                  
         DC    A(0)                                                             
*                                                                               
         DC    CL8'KEY*****'                                                    
KEY      DS    XL25                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'LIBUFFR*'                                                    
LIBUFFR  DS    XL(LIBUFFL)                                                      
*                                                                               
         DS    0D                                                               
         DC    CL8'PSREC***'                                                    
PSREC    DS    XL(PROGSPCL)                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'PGAREAD*'                                                    
AREAD    DS    XL(PGAREAL)                                                      
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
         DC    X'01'                                                            
         DC    XL251'00'                                                        
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(132)          
*                                                                               
RELOAD   DCB   DSORG=PS,MACRF=GM,DDNAME=RELOAD,RECFM=FB,LRECL=80,      *        
               BLKSIZE=400,EODAD=GETRLDX                                        
RLDEOF   DC    H'0'                                                             
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
*                                                                               
RUN      DC    CL04'DSP'                                                        
MODE     DC    CL10'INIT'                                                       
TEST     DC    C'N'                                                             
DSPACE   DC    CL12' '                                                          
SYSCODE  DC    CL3' '                                                           
FAPARM   DC    CL8' '              Must be set by parms                         
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
MESSTAB  DS    0CL60                                                            
  DC CL60'Began processing input parameters from cards                '         
  DC CL60'Ended processing input parameters from cards                '         
  DC CL60'Card validation error - Application terminating             '         
  DC CL60'Attempting Dataspace binds                                  '         
  DC CL60'Completed  Dataspace binds                                  '         
  DC CL60'Unable to bind to PGMS dataspace - Application terminating  '         
  DC CL60'No ALET for PGMS dataspace- Application terminating         '         
  DC CL60'Began opening files                                         '         
  DC CL60'Ended opening files                                         '         
  DC CL60'Began clearing Programs file entries                        '         
  DC CL60'Ended clearing Programs file entries                        '         
  DC CL60'Began rebuilding Programs file entries                      '         
  DC CL60'Ended rebuilding Programs file entries                      '         
  DC CL60'RC=7 FROM ARREDIT                                           '         
  DC CL60'Missing required card FAPARMS=, terminating progam          '         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'RLLIST**RLLIST**'                                           
RLLST    DC    500XL16'00'                                                      
         DC    X'FF'                                                            
*                                                                               
         DS    0L                                                               
         DC    CL16'BUFFER**BUFFER**'                                           
BUFFER   DS    60000C                                                           
*                                                                               
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    100000X'00'                                                      
*                                                                               
         DS    0L                                                               
         DC    CL16'LOADAREALOADAREA'                                           
LOADAREA DS    250000C                                                          
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
XDUB     DS    D                                                                
DSPHD    DS    XL64                                                             
TOTLEN   DS    F                                                                
TOTCORE  DS    F                                                                
*                                                                               
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
DATETIME DS    F                                                                
*                                                                               
FULL     DS    F                                                                
*                                                                               
ACTDTF   DS    A                                                                
*                                                                               
TIME1    DS    F                                                                
TIME2    DS    F                                                                
TIME3    DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
WAITER   DS    A                                                                
*                                                                               
PGMOFFS  DS    A                   PROGRAMS DATASPACE OFFSET                    
PGMALET  DS    A                   PROGRAMS DATASPACE ALET                      
*                                                                               
SZNOW    DS    F                                                                
TABLE    DS    CL8                                                              
TBLID    DS    CL16                                                             
HALF     DS    H                                                                
BYTE     DS    X                                                                
OKFREE   DS    X                                                                
*                                                                               
RLDATE   DS    XL3                                                              
RLPGM    DS    XL2                                                              
RLLIST   DS    XL256                                                            
*                                                                               
TIME     DS    CL11                                                             
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
ISTEST   DS    C                                                                
*                                                                               
PLINE    DS    0CL132                                                           
         DS    XL1                                                              
PLINED   DS    CL131                                                            
*                                                                               
PTITLE   DS    0CL132                                                           
         DS    XL1                                                              
TITLED   DS    CL131                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
BLDNUM   DS    CL5                 BUILD ONE TABLE AT A TIME                    
TPSKEY   DS    XL(PSKEYL)                                                       
NFFLIN   DS    0XL(NFFLINQ)                                                     
NFFLINS  DS    0X                                                               
NFFSTAT  DS    X                                                                
NFFNF    EQU   C'N'                                                             
NFFNT    EQU   C'T'                                                             
NFFUSER  DS    CL4                                                              
NFFNAME  DS    CL8                                                              
NFFCORE  DS    X                                                                
         DS    XL2                                                              
NFFBOOK  DS    CL10                                                             
NFFLEN   DS    XL4                                                              
NFFLKDAT DS    CL8                                                              
NFFLINQ  EQU   (*-NFFLINS)                                                      
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
IOH      DS    CL8                                                              
IO       DS    (IOL)C                                                           
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
MASTC    CSECT                                                                  
         PRINT  OFF                                                             
       ++INCLUDE DDMASTC                                                        
         PRINT  ON                                                              
         EJECT                                                                  
***********************************************************************         
* DUMMY AREA FOR CHKPT WRITE                                          *         
* 56664 IS THE MAX TRACK CAPACITY FOR THE 3390                        *         
***********************************************************************         
CHKPT1   CSECT                                                                  
         ENTRY CHKPT1X                                                          
         DC    56664X'00'                                                       
         ORG   CHKPT1                                                           
         DC    C'THUNDERBAYTHUNDERBAY' <=== JUST AN INDICATOR ;-)               
         ORG                                                                    
CHKPT1X  DS    0X                                                               
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* EDICT CHUNKY DSECT                                                            
***********************************************************************         
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* PANACEA LEVEL STAMP DSECT                                           *         
***********************************************************************         
LVLSTMPD DSECT                                                                  
LVLBKLAB DS    CL5                 C'BOOK='                                     
LVLBOOK  DS    CL10                BOOK NAME                                    
         DS    C                   BYTE, MAY OR MAY NOT BE C' '                 
LVLLVLAB DS    CL6                 C'LEVEL='                                    
LVLLEVEL DS    CL3                 NNN - LEVEL NUMBER                           
LVLDTLAB DS    CL6                 C' DATE='                                    
LVLDATE  DS    CL8                 MMMDD/YY DATE                                
LVLTMLAB DS    CL6                 C' TIME='                                    
LLVTIME  DS    CL8                 HH:MM:SS - TIME'                             
LVLCMLAB DS    CL13                C' COMPILED ON '                             
LVLCDATE DS    CL8                 MMMDD/YY                                     
LVLATLAB DS    CL4                 C' AT '                                      
LVLCTIME DS    CL5                 HH:MM                                        
LVLBYLAB DS    CL4                 C' BY '                                      
LVLUSER  DS    CL8                 C'XXXXXXXX'                                  
LVLASLAB DS    CL4                 C' AS '                                      
LVLRMNAM DS    0CL10               RELO BOOKNAME                                
LVLRMRM  DS    CL2                 C'RM'                                        
LVLRMBK  DS    CL8                 RELO MODULE NAME (WITHOUT "RM")              
         EJECT                                                                  
***********************************************************************         
* REPORT LINE LAYOUT                                                  *         
***********************************************************************         
NEXISTD  DSECT                                                                  
NEXNAME  DS    CL8                                                              
NEXCORE  DS    C                                                                
         DS    CL3                                                              
NEXBOOK  DS    CL10                                                             
         DS    CL3                                                              
NEXLEN   DS    CL6                                                              
         DS    CL3                                                              
NEXLKDAT DS    CL8                                                              
NEXSTDLQ EQU   *-NEXISTD                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         EJECT                                                                  
* FAPARMDEF                                                                     
FAPARMD  DSECT                                                                  
       ++INCLUDE FAPARMDEF                                                      
         EJECT                                                                  
* FASSBOFF                                                                      
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FAPGMSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPGMSD                                                        
         PRINT ON                                                               
* FAPROGSPCD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDARREDITD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDARREDITD                                                     
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008FAPROGSPC 09/10/15'                                      
         END                                                                    
