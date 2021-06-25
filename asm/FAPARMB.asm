*          DATA SET FAPARMB    AT LEVEL 018 AS OF 04/27/18                      
*PHASE FAPARMBA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - REPB VTAM SYSTEM'                    
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(14)                                                          
FACORELN DC    A(125*K)                                                         
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'5'                                                             
TIMLOOP  DC    F'20'                                                            
TIMAUTOQ DC    F'120'                                                           
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS ***          
TSKPGM   DC    A(128*K)            WAS 114                                      
TSKWRK   DC    A(128*K)            WAS 120                                      
TSKTIA   DC    A(18*K)                                                          
TSKTWA   DC    A(18*K)                                                          
TSKMAP   DC    A(K+16)                                                          
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'DM'                                                            
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                NOP ALL                                      
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'REP1'           READ-ONLY                                    
*JUN5/17 DC    CL8'REP3'                                                        
         DC    CL8'REP4'                                                        
         DC    CL8'REP5'           READ-ONLY                                    
*JUN5/17 DC    CL8'REP7'                                                        
         DC    CL8'REP8'           READ-ONLY                                    
         DC    CL8'SPOTS'                                                       
         DC    CL8'SPOTV'                                                       
         DC    CL8'SPOTY'          READ-ONLY                                    
         DC    CL8'STRS'                                                        
         DC    CL8'STRU'                                                        
         DC    CL8'STRV'                                                        
         DC    CL8'DEMO'                                                        
         DC    CL8'GAMES'                                                       
         DC    CL8'CONTROL'                                                     
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
*                                                                               
         DC    X'030124'           INCLUDE NET/NPOD (RESEARCH WRITER)           
         DC    X'0326FF'                                                        
*                                                                               
         DC    X'0401FF'           EXCLUDE PRINT (ALL)                          
*                                                                               
         DC    X'0502FF'           INCLUDE MPL/PFM                              
*                                                                               
         DC    X'0701FF'           EXCLUDE TALENT (ALL)                         
*                                                                               
         DC    X'0901FF'           EXCLUDE MBASE (ALL)                          
*                                                                               
         DC    X'0C01FF'           EXCLUDE CPP (ALL)                            
*                                                                               
         ORG                                                                    
*                                                                               
FACMAXIO DC    H'20000'                                                         
FACPTLST DC    CL6'REPPT '                                                      
FACPOPLN DC    H'4'                TIMER POP DURATION (SEC/100)                 
FACPOPMX DC    H'200'              TIMER POP MAXIMUM COUNTER                    
FACPRIO  DC    H'50'               PRIORITY I/O THRESHOLD                       
FACPRCPU DC    H'80'               PRIORITY CPU THRESHOLD                       
FACPRMIN DC    H'5'                PRIORITY MIN VALUE                           
FACDAROK DC    AL1(YES)            PROCESS DARE FROM THIS FACPAK                
FAC31BIT DC    AL1(YES)            BON MORT                                     
*                                                                               
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    C'N'                UPDATE PRGMS FILE                            
*                                                                               
VTAMAPL  DC    CL8'REPB'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'800'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'1800'             VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'128'              VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'10'               TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    Y(18*K)             TEMPSTR RECORD LENGTH                        
FACTMSL  DC    Y(18*K)             TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(24*18*K)          TSAR BUFFER SIZE                             
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(2*14*K)           TRACE BUFFER LENGTH                          
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'7'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                SCRUNCH XA STORAGE ALLOC IN 4K'S             
FACSPACE DC    C'DMGRDATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    AL1(YES)            TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    AL1(YES)            NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABPXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    AL1(YES)            DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    AL1(YES)            UPDATE TABS TABLES FROM THIS FACPAK          
FACUDATE DC    X'00'                                                            
FACXAUTL DC    AL1(YES)            Y = BUILD UTLS IN XA                         
FACSSMXP DC    H'8'                MAXIMUM PHYSICAL SESSIONS                    
FACXALEN DC    AL2(K)              WSSVR - # OF 1K BLOCKS (1 MEG)               
*                                                                               
PHLIST   DC    50XL16'00'                                                       
         ORG   PHLIST                                                           
         ORG                                                                    
         DC    X'00'                                                            
*                                                                               
FAJESIO  DC    CL8' '              *** MONSOON *** WAS CL8'FAJESIO'             
FACJOBMX DC    AL2(500)            *** MONSOON *** WAS 10                       
FACXA9MB DC    AL1(1)              NUMBER OF MB OF XA9 WORK PER TASK            
         DC    XL2'00'             N/D                                          
*                                                                               
FACMQION DC    F'1'                MQ SERIES IO CONTROL NUMBER 2=TRACE          
FACMQUTL DC    H'500'              N/U (FOR NUM ENTRIES IN UTL)                 
FACMQBUF DC    H'100'              N/U (FOR NUM BUFFERS)                        
AFACMQM  DC    A(FACMQM)           A(MQ SERIES MANAGER NAME)                    
AFACMQIN DC    A(FACMQIN)          A(MQ SERIES FACPAK INPUT Q NAME)             
AFACMQOU DC    A(FACMQOU)          A(MQ SERIES FACPAK OUTPUT Q NAME)            
AFACMQWK DC    A(FACMQWK)          A(MQ SERIES FACPAK WORK Q NAME)              
AFACMQCT DC    A(FACMQCT)          A(MQ SERIES FACPAK CONTROL Q NAME)           
DDICTLEN DC    A(0)                OVERRIDE DATA DICTIONARY LENGTH              
UKMEDDSP DC    XL12'00'            UK MEDIA DATASPACE NAME                      
PGMSDSP  DC    CL12'PRGRXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((4*K*K)+32)     MINIO (4 MEGS)                               
*                                                                               
FACSMTP  DC    CL8'JESMAIL '                                                    
FACMQMAX DC    F'10'               MAX MQ                                       
FAMOFA   DC    AL1(YES)            FORMOSA ON - MO MQ /OBSOLETE BIT?            
FACMQIO  DC    CL08'FAMQIO  '                                                   
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK POLLING CODE                          
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'REPB.INPUT.QUEUE'                                           
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'REPB.WORK.QUEUE'                                            
FACMQCT  DC    CL48'REPB.CONTROL.QUEUE'                                         
*                                                                               
DMFLIST  DS    0CL20                                                            
         DC    C'PRGMS     ',C'PRGMS     '                                      
         DC    C'TEMPSTR   ',C'TEMPSTR   '                                      
         DC    C'TEMPEST   ',C'TEMPEST   '                                      
         DC    C'DMPFILE   ',C'DMPFILE   '                                      
         DC    C'TSTRCVR   ',C'TSTRCVR   '                                      
         DC    C'PRTQ1     ',C'PRTQ1     '                                      
         DC    C'PRTQ2     ',C'PRTQ2     '                                      
         DC    C'PRTQ3     ',C'PRTQ3     '                                      
         DC    C'PRTQ4     ',C'PRTQ4     '                                      
         DC    C'PRTQ5     ',C'PRTQ5     '                                      
         DC    C'PRTQ6     ',C'PRTQ6     '                                      
         DC    C'PRTQ7     ',C'PRTQ7     '                                      
         DC    C'PRTQ8     ',C'PRTQ8     '                                      
         DC    C'PRTQ9     ',C'PRTQ9     '                                      
         DC    C'PRTQA     ',C'PRTQA     '                                      
         DC    C'PRTQB     ',C'PRTQB     '                                      
         DC    C'PRTQC     ',C'PRTQC     '                                      
         DC    C'PRTQD     ',C'PRTQD     '                                      
         DC    C'PRTQE     ',C'PRTQE     '                                      
         DC    C'PRTQF     ',C'PRTQF     '                                      
         DC    C'PRTQG     ',C'PRTQG     '                                      
         DC    C'EASIWK    ',C'EASIWK    '                                      
         DC    C'WRKF1     ',C'WRKF1     '                                      
         DC    C'WRKF2     ',C'WRKF2     '                                      
         DC    C'WRKF3     ',C'WRKF3     '                                      
         DC    C'WRKF4     ',C'WRKF4     '                                      
         DC    C'WRKF5     ',C'WRKF5     '                                      
         DC    C'WRKF6     ',C'WRKF6     '                                      
         DC    C'WRKF7     ',C'WRKF7     '                                      
         DC    C'WRKF8     ',C'WRKF8     '                                      
         DC    C'WRKF9     ',C'WRKF9     '                                      
         DC    C'WRKFA     ',C'WRKFA     '                                      
         DC    C'WRKFB     ',C'WRKFB     '                                      
         DC    C'WRKFC     ',C'WRKFC     '                                      
         DC    C'WRKFD     ',C'WRKFD     '                                      
         DC    C'WRKFE     ',C'WRKFE     '                                      
         DC    C'WRKFF     ',C'WRKFF     '                                      
         DC    C'WRKFG     ',C'WRKFG     '                                      
         DC    C'WRKZ1     ',C'WRKZ1     '                                      
         DC    C'WRKZ2     ',C'WRKZ2     '                                      
         DC    C'WRKZ3     ',C'WRKZ3     '                                      
         DC    C'WRKZ4     ',C'WRKZ4     '                                      
         DC    C'WRKZ5     ',C'WRKZ5     '                                      
         DC    C'WRKZ6     ',C'WRKZ6     '                                      
         DC    C'WRKZ7     ',C'WRKZ7     '                                      
         DC    C'WRKZ8     ',C'WRKZ8     '                                      
         DC    C'WRKZ9     ',C'WRKZ9     '                                      
         DC    C'WRKZA     ',C'WRKZA     '                                      
         DC    C'WRKZB     ',C'WRKZB     '                                      
         DC    C'WRKZC     ',C'WRKZC     '                                      
         DC    C'WRKZD     ',C'WRKZD     '                                      
         DC    C'WRKZE     ',C'WRKZE     '                                      
         DC    C'WRKZF     ',C'WRKZF     '                                      
         DC    C'ADRFILE   ',C'ADRFILE   '                                      
         DC    C'STATS     ',C'STATS     '                                      
         DC    C'KWXFILE   ',C'KWXFILE   '                                      
         DC    C'EDCTA     ',C'EDCTA     '                                      
         DC    C'EDCTZ     ',C'EDCTZ     '                                      
         DC    C'EDCTR     ',C'EDCTR     '                                      
         DC    C'WKFILE    ',C'WKFILE    '                                      
         DC    C'FACWRK    ',C'FACWRK    '                                      
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
*                                                                               
         DC    C'REPDIR3   ',C'REPDIR3   '                                      
         DC    C'REPFIL3   ',C'REPFIL3   '                                      
         DC    C'RREQ3     ',C'RREQ3     '                                      
         DC    C'REPRCV3   ',C'REPRCV3   '                                      
         DC    C'REPWRK3   ',C'REPWRK3   '                                      
         DC    C'RRGNEW3   ',C'RRGNEW3   '                                      
*                                                                               
         DC    C'REPDIR4   ',C'REPDIR4   '                                      
         DC    C'REPFIL4   ',C'REPFIL4   '                                      
         DC    C'RREQ4     ',C'RREQ4     '                                      
         DC    C'REPRCV4   ',C'REPRCV4   '                                      
         DC    C'REPWRK4   ',C'REPWRK4   '                                      
         DC    C'RRGNEW4   ',C'RRGNEW4   '                                      
*                                                                               
*        DC    C'REPDIRK   ',C'REPDIRK   ' REMOVED APR12/2007                   
*        DC    C'REPFILK   ',C'REPFILK   '                                      
*        DC    C'RREQK     ',C'RREQK     '                                      
*        DC    C'REPRCVK   ',C'REPRCVK   '                                      
*        DC    C'REPWRKK   ',C'REPWRKK   '                                      
*        DC    C'RRGNEWK   ',C'RRGNEWK   '                                      
*                                                                               
*        DC    C'REPDIR9   ',C'REPDIR9   '                                      
*        DC    C'REPFIL9   ',C'REPFIL9   '                                      
*        DC    C'RREQ9     ',C'RREQ9     '                                      
*        DC    C'REPRCV9   ',C'REPRCV9   '                                      
*        DC    C'REPWRK9   ',C'REPWRK9   '                                      
*        DC    C'RRGNEW9   ',C'RRGNEW9   '                                      
*SET                                                                            
*R/O FOR DC    C'REPDIR6   ',C'REPDIR6   '                                      
*BUHR    DC    C'REPFIL6   ',C'REPFIL6   '                                      
*MAR20   DC    C'RREQ6     ',C'RREQ6     '                                      
*2001    DC    C'REPRCV6   ',C'REPRCV6   '                                      
*-AATK   DC    C'REPWRK6   ',C'REPWRK6   '                                      
*        DC    C'RRGNEW6   ',C'RRGNEW6   '                                      
*                                                                               
         DC    C'REPDIR7   ',C'REPDIR7   '                                      
         DC    C'REPFIL7   ',C'REPFIL7   '                                      
         DC    C'RREQ7     ',C'RREQ7     '                                      
         DC    C'REPRCV7   ',C'REPRCV7   '                                      
         DC    C'REPWRK7   ',C'REPWRK7   '                                      
         DC    C'RRGNEW7   ',C'RRGNEW7   '                                      
         DC    C'SPTDIRS   ',C'SPTDIRS   '                                      
         DC    C'SPTFILS   ',C'SPTFILS   '                                      
         DC    C'XSPDIRS   ',C'XSPDIRS   '                                      
         DC    C'XSPFILS   ',C'XSPFILS   '                                      
         DC    C'REQS      ',C'REQS      '                                      
         DC    C'RECVS     ',C'RECVS     '                                      
         DC    C'STAFILS   ',C'STAFILS   '                                      
*                                                                               
*        DC    C'SPTDIRU   ',C'SPTDIRU   '                                      
*        DC    C'SPTFILU   ',C'SPTFILU   '                                      
*        DC    C'XSPDIRU   ',C'XSPDIRU   '                                      
*        DC    C'XSPFILU   ',C'XSPFILU   '                                      
*        DC    C'REQU      ',C'REQU      '                                      
*        DC    C'RECVU     ',C'RECVU     '                                      
*        DC    C'STAFILU   ',C'STAFILU   '                                      
*                                                                               
         DC    C'SPTDIRV   ',C'SPTDIRV   '                                      
         DC    C'SPTFILV   ',C'SPTFILV   '                                      
         DC    C'XSPDIRV   ',C'XSPDIRV   '                                      
         DC    C'XSPFILV   ',C'XSPFILV   '                                      
         DC    C'REQV      ',C'REQV      '                                      
         DC    C'RECVV     ',C'RECVV     '                                      
         DC    C'STAFILV   ',C'STAFILV   '                                      
         DC    C'TRFDIRS   ',C'TRFDIRS   '                                      
         DC    C'TRFFILS   ',C'TRFFILS   '                                      
         DC    C'TRFRCVS   ',C'TRFRCVS   '                                      
         DC    C'TRFREQS   ',C'TRFREQS   '                                      
         DC    C'TRFDIRU   ',C'TRFDIRU   '                                      
         DC    C'TRFFILU   ',C'TRFFILU   '                                      
         DC    C'TRFRCVU   ',C'TRFRCVU   '                                      
         DC    C'TRFREQU   ',C'TRFREQU   '                                      
         DC    C'TRFDIRV   ',C'TRFDIRV   '                                      
         DC    C'TRFFILV   ',C'TRFFILV   '                                      
         DC    C'TRFRCVV   ',C'TRFRCVV   '                                      
         DC    C'TRFREQV   ',C'TRFREQV   '                                      
*                                                                               
*        DC    C'ACCFILV   ',C'ACCFILV   '                                      
*        DC    C'ACCREQV   ',C'ACCREQV   '                                      
*        DC    C'ACCRCVV   ',C'ACCRCVV   '                                      
*        DC    C'ACCDAYV   ',C'ACCDAYV   '                                      
*        DC    C'ACCWRKV   ',C'ACCWRKV   '                                      
*        DC    C'ACCDIRV   ',C'ACCDIRV   '                                      
*        DC    C'ACCMSTV   ',C'ACCMSTV   '                                      
*        DC    C'ACCARCV   ',C'ACCARCV   '                                      
*                                                                               
*&&DO                                       REMOVED AS OF APR15/09              
         DC    C'MPLDIRV   ',C'MPLDIRV   '                                      
         DC    C'MPLFILV   ',C'MPLFILV   '                                      
         DC    C'MPLREQV   ',C'MPLREQV   '                                      
         DC    C'MPLRCVV   ',C'MPLRCVV   '                                      
*        DC    C'MPQDRAV   ',C'MPQDRAV   '                                      
*        DC    C'MPQFLAV   ',C'MPQFLAV   '                                      
*&&                                                                             
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018FAPARMB   04/27/18'                                      
         END                                                                    
