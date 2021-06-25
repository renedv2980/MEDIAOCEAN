*          DATA SET FAPARMQ    AT LEVEL 021 AS OF 12/23/20                      
*PHASE FAPARMQA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - V T A M   TEST SYSTEM'               
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(15)                                                          
FACORELN DC    A(1000*K)                                                        
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'20'                                                            
TIMLOOP  DC    F'20'                                                            
TIMAUTOQ DC    F'90'                                                            
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS ***          
TSKPGM   DC    A(128*K)            TASK AREAS MUST BE DBLWD ALIGNED             
TSKWRK   DC    A(128*K)                                                         
TSKTIA   DC    A(18*K)                                                          
TSKTWA   DC    A(18*K)                                                          
TSKMAP   DC    A(K+16)                                                          
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'FQ'                                                            
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                ALL SYSTEMS NOP                              
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'SPOT0   '                                                    
         DC    CL8'SPOTOV  '                                                    
         DC    CL8'SPOTTT  '                                                    
         DC    CL8'SPOTTU  '                                                    
         DC    CL8'STR0    '                                                    
         DC    CL8'STROV   '                                                    
         DC    CL8'STRTT   '                                                    
         DC    CL8'STRTU   '                                                    
         DC    CL8'NETOV   '                                                    
         DC    CL8'NETTT   '                                                    
         DC    CL8'NETTU   '                                                    
         DC    CL8'NETT    '                                                    
         DC    CL8'PRNTOV  '                                                    
         DC    CL8'PRNTTT  '                                                    
         DC    CL8'PRNTTU  '                                                    
         DC    CL8'PRNTT   '                                                    
         DC    CL8'REP4    '                                                    
         DC    CL8'REP6    '                                                    
         DC    CL8'ACCOV   '                                                    
         DC    CL8'ACCTT   '                                                    
         DC    CL8'ACCTU   '                                                    
         DC    CL8'ACC0    '                                                    
         DC    CL8'ACC1    '                                                    
         DC    CL8'ACC2    '                                                    
         DC    CL8'ACC3    '                                                    
         DC    CL8'ACC4    '                                                    
         DC    CL8'ACC5    '                                                    
*        DC    CL8'TAL1    '                                                    
*        DC    CL8'TAL2    '                                                    
*        DC    CL8'TAL3    '                                                    
*        DC    CL8'TAL4    '                                                    
         DC    CL8'CONTROL '                                                    
         DC    CL8'DEMO    '                                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                ALL PROGRAMS OP                              
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             NOW 35000 WAS 10000                          
FACPTLST DC    XL6'00'                                                          
FACPOPLN DC    H'3'                TIMER POP DURATION (SEC/100)                 
FACPOPMX DC    H'128'              TIMER POP MAXIMUM COUNTER                    
FACPRIO  DC    H'50'               PRIORITY I/O THRESHOLD                       
FACPRCPU DC    H'80'               PRIORITY CPU THRESHOLD                       
FACPRMIN DC    H'5'                PRIORITY MIN VALUE                           
FACDAROK DC    AL1(YES)            PROCESS DARE FROM THIS FACPAK                
FAC31BIT DC    AL1(YES)            USE 31-BIT I/O ADDRESSES                     
*                                                                               
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    AL1(YES)            UPDATE PRGMS FILE FROM THIS SYS              
*                                                                               
VTAMAPL  DC    CL8'FQA '           VTAM APPLICATION ID                          
VTAMUTL  DC    H'512'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'200'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'70'               VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'1'                TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    Y(18*K)             TEMPSTR RECORD LENGTH                        
FACTMSL  DC    Y(18*K)             TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(18*24*K)          TSAR BUFFER SIZE                             
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(2*14*K)           TRACE BUFFER SIZE                            
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'7'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                SCRUNCH XA STORAGE ALLOC IN 4K'S             
FACSPACE DC    C'DMGQDATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    AL1(YES)            TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    AL1(YES)            NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABQXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    AL1(YES)            DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    AL1(YES)            UPDATE TABS TABLES FROM THIS FACPAK          
FACUDATE DC    X'00'                                                            
         DC    X'00'                                                            
FACSSMXP DC    H'8'                MAXIMUM PHYSICAL SESSIONS                    
FACXALEN DC    AL2(K)              WSSVR - # OF 1K BLOCKS (1 MEG)               
*                                                                               
PHLIST   DC    50XL16'00'                                                       
         ORG   PHLIST                                                           
         ORG                                                                    
         DC    X'00'                                                            
*                                                                               
FAJESIO  DC    CL8' '                                                           
FACJOBMX DC    AL2(500)                                                         
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
PGMSDSP  DC    CL12'PRGQXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((K*K*4)+32)     MINIO BLOCK (4 MEG)                          
*                                                                               
FACSMTP  DC    CL08'JESMAIL '                                                   
FACMQMAX DC    F'08'                                                            
FAMOFA   DC    AL1(NO)                                                          
FACMQIO  DC    CL08'FAMQIO  '                                                   
*                                                                               
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK POLLING                               
FAALLC   DC    AL1(NO)             ALL CHARACTERS ARE VALID                     
         DC    XL22'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48'MQ7Q'                                                       
FACMQIN  DC    CL48'QA.INPUT.ALIASQ'                                            
FACMQOU  DC    CL48'DDS.BROKER.FQA.LOCALQ'                                      
FACMQWK  DC    CL48'QA.WORK.ALIASQ'                                             
FACMQCT  DC    CL48'QA.CONTROL.ALIASQ'                                          
*                                                                               
DMFLIST  DS    0CL20                                                            
         DC    C'PRGMS     ',C'PRGMS     '  SERVICE                             
         DC    C'TEMPSTR   ',C'TEMPSTR   '                                      
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
         DC    C'ADRFILE   ',C'ADRFILE   '                                      
         DC    C'STATS     ',C'STATS     '                                      
         DC    C'DMPFILE   ',C'DMPFILE   '                                      
         DC    C'TSTRCVR   ',C'TSTRCVR   '                                      
         DC    C'WKFILE    ',C'WKFILE    '                                      
         DC    C'FACWRK    ',C'FACWRK    '                                      
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
         DC    C'TEMPEST   ',C'TEMPEST   '                                      
         DC    C'KWXFILE   ',C'KWXFILE   '                                      
         DC    C'EDCTA     ',C'EDCTA     '                                      
         DC    C'EDCTR     ',C'EDCTR     '                                      
         DC    C'EDCTZ     ',C'EDCTZ     '                                      
*                                                                               
         DC    C'SPTDIR0   ',C'SPTDIR0   '  SPOT0                               
         DC    C'SPTFIL0   ',C'SPTFIL0   '                                      
         DC    C'XSPDIR0   ',C'XSPDIR0   '                                      
         DC    C'XSPFIL0   ',C'XSPFIL0   '                                      
         DC    C'STAFIL0   ',C'STAFIL0   '                                      
         DC    C'RECV0     ',C'RECV0     '                                      
         DC    C'REQ0      ',C'REQ0      '                                      
*                                                                               
         DC    C'SPTDIROV  ',C'SPTDIROV  '  SPOTOV                              
         DC    C'SPTFILOV  ',C'SPTFILOV  '                                      
         DC    C'XSPDIROV  ',C'XSPDIROV  '                                      
         DC    C'XSPFILOV  ',C'XSPFILOV  '                                      
         DC    C'STAFILOV  ',C'STAFILOV  '                                      
         DC    C'RECVOV    ',C'RECVOV    '                                      
         DC    C'REQOV     ',C'REQOV     '                                      
*                                                                               
         DC    C'SPTDIRTT  ',C'SPTDIRTT  '  SPOTTT                              
         DC    C'SPTFILTT  ',C'SPTFILTT  '                                      
         DC    C'XSPDIRTT  ',C'XSPDIRTT  '                                      
         DC    C'XSPFILTT  ',C'XSPFILTT  '                                      
         DC    C'STAFILTT  ',C'STAFILTT  '                                      
         DC    C'RECVTT    ',C'RECVTT    '                                      
         DC    C'REQTT     ',C'REQTT     '                                      
*                                                                               
         DC    C'SPTDIRTU  ',C'SPTDIRTU  '  SPOTTU                              
         DC    C'SPTFILTU  ',C'SPTFILTU  '                                      
         DC    C'XSPDIRTU  ',C'XSPDIRTU  '                                      
         DC    C'XSPFILTU  ',C'XSPFILTU  '                                      
         DC    C'STAFILTU  ',C'STAFILTU  '                                      
         DC    C'RECVTU    ',C'RECVTU    '                                      
         DC    C'REQTU     ',C'REQTU     '                                      
*                                                                               
         DC    C'TRFDIR0   ',C'TRFDIR0   '  STR0                                
         DC    C'TRFFIL0   ',C'TRFFIL0   '                                      
         DC    C'TRFRCV0   ',C'TRFRCV0   '                                      
         DC    C'TRFREQ0   ',C'TRFREQ0   '                                      
*                                                                               
         DC    C'TRFDIROV  ',C'TRFDIROV  '  STROV                               
         DC    C'TRFFILOV  ',C'TRFFILOV  '                                      
         DC    C'TRFRCVOV  ',C'TRFRCVOV  '                                      
         DC    C'TRFREQOV  ',C'TRFREQOV  '                                      
*                                                                               
         DC    C'TRFDIRTT  ',C'TRFDIRTT  '  STRTT                               
         DC    C'TRFFILTT  ',C'TRFFILTT  '                                      
         DC    C'TRFRCVTT  ',C'TRFRCVTT  '                                      
         DC    C'TRFREQTT  ',C'TRFREQTT  '                                      
*                                                                               
         DC    C'TRFDIRTU  ',C'TRFDIRTU  '  STRTU                               
         DC    C'TRFFILTU  ',C'TRFFILTU  '                                      
         DC    C'TRFRCVTU  ',C'TRFRCVTU  '                                      
         DC    C'TRFREQTU  ',C'TRFREQTU  '                                      
*                                                                               
         DC    C'SPTDIRT   ',C'SPTDIRT   '  NETT                                
         DC    C'SPTFILT   ',C'SPTFILT   '                                      
         DC    C'STAFILT   ',C'STAFILT   '                                      
         DC    C'UNTDIRT   ',C'UNTDIRT   '                                      
         DC    C'UNTFILT   ',C'UNTFILT   '                                      
         DC    C'XSPFILT   ',C'XSPFILT   '                                      
         DC    C'XSPDIRT   ',C'XSPDIRT   '                                      
         DC    C'RECVT     ',C'RECVT     '                                      
         DC    C'REQT      ',C'REQT      '                                      
*                                                                               
         DC    C'SPTDIROW  ',C'SPTDIROW  '  NETOV                               
         DC    C'SPTFILOW  ',C'SPTFILOW  '                                      
         DC    C'STAFILOW  ',C'STAFILOW  '                                      
         DC    C'UNTDIROW  ',C'UNTDIROW  '                                      
         DC    C'UNTFILOW  ',C'UNTFILOW  '                                      
         DC    C'XSPDIROW  ',C'XSPDIROW  '                                      
         DC    C'XSPFILOW  ',C'XSPFILOW  '                                      
         DC    C'RECVOW    ',C'RECVOW    '                                      
         DC    C'REQOW     ',C'REQOW     '                                      
*                                                                               
         DC    C'SPTDIRTN  ',C'SPTDIRTN  '  NETTT                               
         DC    C'SPTFILTN  ',C'SPTFILTN  '                                      
         DC    C'STAFILTN  ',C'STAFILTN  '                                      
         DC    C'UNTDIRTN  ',C'UNTDIRTN  '                                      
         DC    C'UNTFILTN  ',C'UNTFILTN  '                                      
         DC    C'XSPDIRTN  ',C'XSPDIRTN  '                                      
         DC    C'XSPFILTN  ',C'XSPFILTN  '                                      
         DC    C'RECVTN    ',C'RECVTN    '                                      
         DC    C'REQTN     ',C'REQTN     '                                      
*                                                                               
         DC    C'SPTDIRTV  ',C'SPTDIRTV  '  NETTU                               
         DC    C'SPTFILTV  ',C'SPTFILTV  '                                      
         DC    C'STAFILTV  ',C'STAFILTV  '                                      
         DC    C'UNTDIRTV  ',C'UNTDIRTV  '                                      
         DC    C'UNTFILTV  ',C'UNTFILTV  '                                      
         DC    C'XSPDIRTV  ',C'XSPDIRTV  '                                      
         DC    C'XSPFILTV  ',C'XSPFILTV  '                                      
         DC    C'RECVTV    ',C'RECVTV    '                                      
         DC    C'REQTV     ',C'REQTV     '                                      
*                                                                               
         DC    C'PRTDIROV  ',C'PRTDIROV  '  PRNTOV                              
         DC    C'PRTFILOV  ',C'PRTFILOV  '                                      
         DC    C'PUBDIROV  ',C'PUBDIROV  '                                      
         DC    C'PUBFILOV  ',C'PUBFILOV  '                                      
         DC    C'PRECVOV   ',C'PRECVOV   '                                      
         DC    C'PREQOV    ',C'PREQOV    '                                      
*                                                                               
         DC    C'PRTDIRTT  ',C'PRTDIRTT  '  PRNTTT                              
         DC    C'PRTFILTT  ',C'PRTFILTT  '                                      
         DC    C'PUBDIRTT  ',C'PUBDIRTT  '                                      
         DC    C'PUBFILTT  ',C'PUBFILTT  '                                      
         DC    C'PRECVTT   ',C'PRECVTT   '                                      
         DC    C'PREQTT    ',C'PREQTT    '                                      
*                                                                               
         DC    C'PRTDIRTU  ',C'PRTDIRTU  '  PRNTTU                              
         DC    C'PRTFILTU  ',C'PRTFILTU  '                                      
         DC    C'PUBDIRTU  ',C'PUBDIRTU  '                                      
         DC    C'PUBFILTU  ',C'PUBFILTU  '                                      
         DC    C'PRECVTU   ',C'PRECVTU   '                                      
         DC    C'PREQTU    ',C'PREQTU    '                                      
*                                                                               
         DC    C'PRTDIRT   ',C'PRTDIRT   '  PRNTT                               
         DC    C'PRTFILT   ',C'PRTFILT   '                                      
         DC    C'PUBDIRT   ',C'PUBDIRT   '                                      
         DC    C'PUBFILT   ',C'PUBFILT   '                                      
         DC    C'PRECVT    ',C'PRECVT    '                                      
         DC    C'PREQT     ',C'PREQT     '                                      
*                                                                               
         DC    C'REPDIR4   ',C'REPDIR4   '  REP4                                
         DC    C'REPFIL4   ',C'REPFIL4   '                                      
         DC    C'RREQ4     ',C'RREQ4     '                                      
         DC    C'REPRCV4   ',C'REPRCV4   '                                      
         DC    C'REPWRK4   ',C'REPWRK4   '                                      
         DC    C'RRGNEW4   ',C'RRGNEW4   '                                      
*                                                                               
         DC    C'REPDIR6   ',C'REPDIR6   '  REP6                                
         DC    C'REPFIL6   ',C'REPFIL6   '                                      
         DC    C'RREQ6     ',C'RREQ6     '                                      
         DC    C'REPRCV6   ',C'REPRCV6   '                                      
         DC    C'REPWRK6   ',C'REPWRK6   '                                      
         DC    C'RRGNEW6   ',C'RRGNEW6   '                                      
*                                                                               
         DC    C'ACCDIROV  ',C'ACCDIROV  '  ACCOV                               
         DC    C'ACCMSTOV  ',C'ACCMSTOV  '                                      
         DC    C'ACCARCOV  ',C'ACCARCOV  '                                      
         DC    C'ACCREQOV  ',C'ACCREQOV  '                                      
         DC    C'ACCRCVOV  ',C'ACCRCVOV  '                                      
         DC    C'ACCDAYOV  ',C'ACCDAYOV  '                                      
         DC    C'ACCWRKOV  ',C'ACCWRKOV  '                                      
         DC    C'ACCHSTOV  ',C'ACCHSTOV  '                                      
*                                                                               
         DC    C'ACCDIRTT  ',C'ACCDIRTT  '  ACCTT                               
         DC    C'ACCMSTTT  ',C'ACCMSTTT  '                                      
         DC    C'ACCARCTT  ',C'ACCARCTT  '                                      
         DC    C'ACCREQTT  ',C'ACCREQTT  '                                      
         DC    C'ACCRCVTT  ',C'ACCRCVTT  '                                      
         DC    C'ACCDAYTT  ',C'ACCDAYTT  '                                      
         DC    C'ACCWRKTT  ',C'ACCWRKTT  '                                      
         DC    C'ACCHSTTT  ',C'ACCHSTTT  '                                      
*                                                                               
         DC    C'ACCDIRTU  ',C'ACCDIRTU  '  ACCTU                               
         DC    C'ACCMSTTU  ',C'ACCMSTTU  '                                      
         DC    C'ACCARCTU  ',C'ACCARCTU  '                                      
         DC    C'ACCREQTU  ',C'ACCREQTU  '                                      
         DC    C'ACCRCVTU  ',C'ACCRCVTU  '                                      
         DC    C'ACCDAYTU  ',C'ACCDAYTU  '                                      
         DC    C'ACCWRKTU  ',C'ACCWRKTU  '                                      
         DC    C'ACCHSTTU  ',C'ACCHSTTU  '                                      
*                                                                               
         DC    C'ACCDIR0   ',C'ACCDIR0   '  ACC0                                
         DC    C'ACCMST0   ',C'ACCMST0   '                                      
         DC    C'ACCARC0   ',C'ACCARC0   '                                      
         DC    C'ACCREQ0   ',C'ACCREQ0   '                                      
         DC    C'ACCRCV0   ',C'ACCRCV0   '                                      
         DC    C'ACCDAY0   ',C'ACCDAY0   '                                      
         DC    C'ACCWRK0   ',C'ACCWRK0   '                                      
         DC    C'ACCHST0   ',C'ACCHST0   '                                      
*                                                                               
         DC    C'ACCDIR1   ',C'ACCDIR1   '  ACC1                                
         DC    C'ACCMST1   ',C'ACCMST1   '                                      
         DC    C'ACCARC1   ',C'ACCARC1   '                                      
         DC    C'ACCREQ1   ',C'ACCREQ1   '                                      
         DC    C'ACCRCV1   ',C'ACCRCV1   '                                      
         DC    C'ACCDAY1   ',C'ACCDAY1   '                                      
         DC    C'ACCWRK1   ',C'ACCWRK1   '                                      
         DC    C'ACCHST1   ',C'ACCHST1   '                                      
*                                                                               
         DC    C'ACCDIR2   ',C'ACCDIR2   '  ACC2                                
         DC    C'ACCMST2   ',C'ACCMST2   '                                      
         DC    C'ACCARC2   ',C'ACCARC2   '                                      
         DC    C'ACCREQ2   ',C'ACCREQ2   '                                      
         DC    C'ACCRCV2   ',C'ACCRCV2   '                                      
         DC    C'ACCDAY2   ',C'ACCDAY2   '                                      
         DC    C'ACCWRK2   ',C'ACCWRK2   '                                      
         DC    C'ACCHST2   ',C'ACCHST2   '                                      
*                                                                               
         DC    C'ACCDIR3   ',C'ACCDIR3   '  ACC3                                
         DC    C'ACCMST3   ',C'ACCMST3   '                                      
         DC    C'ACCARC3   ',C'ACCARC3   '                                      
         DC    C'ACCREQ3   ',C'ACCREQ3   '                                      
         DC    C'ACCRCV3   ',C'ACCRCV3   '                                      
         DC    C'ACCDAY3   ',C'ACCDAY3   '                                      
         DC    C'ACCWRK3   ',C'ACCWRK3   '                                      
         DC    C'ACCHST3   ',C'ACCHST3   '                                      
*                                                                               
         DC    C'ACCDIR4   ',C'ACCDIR4   '  ACC4                                
         DC    C'ACCMST4   ',C'ACCMST4   '                                      
         DC    C'ACCARC4   ',C'ACCARC4   '                                      
         DC    C'ACCREQ4   ',C'ACCREQ4   '                                      
         DC    C'ACCRCV4   ',C'ACCRCV4   '                                      
         DC    C'ACCDAY4   ',C'ACCDAY4   '                                      
         DC    C'ACCWRK4   ',C'ACCWRK4   '                                      
         DC    C'ACCHST4   ',C'ACCHST4   '                                      
*                                                                               
         DC    C'ACCDIR5   ',C'ACCDIR5   '  ACC5                                
         DC    C'ACCMST5   ',C'ACCMST5   '                                      
         DC    C'ACCARC5   ',C'ACCARC5   '                                      
         DC    C'ACCREQ5   ',C'ACCREQ5   '                                      
         DC    C'ACCRCV5   ',C'ACCRCV5   '                                      
         DC    C'ACCDAY5   ',C'ACCDAY5   '                                      
         DC    C'ACCWRK5   ',C'ACCWRK5   '                                      
         DC    C'ACCHST5   ',C'ACCHST5   '                                      
*                                                                               
         DC    C'TALDIR1   ',C'TALDIR1   '  TAL1                                
         DC    C'TALFIL1   ',C'TALFIL1   '                                      
         DC    C'CHKDIR1   ',C'CHKDIR1   '                                      
         DC    C'CHKFIL1   ',C'CHKFIL1   '                                      
         DC    C'TALREQ1   ',C'TALREQ1   '                                      
         DC    C'TALRCV1   ',C'TALRCV1   '                                      
*                                                                               
         DC    C'TALDIR2   ',C'TALDIR2   '  TAL2                                
         DC    C'TALFIL2   ',C'TALFIL2   '                                      
         DC    C'CHKDIR2   ',C'CHKDIR2   '                                      
         DC    C'CHKFIL2   ',C'CHKFIL2   '                                      
         DC    C'TALREQ2   ',C'TALREQ2   '                                      
         DC    C'TALRCV2   ',C'TALRCV2   '                                      
*                                                                               
         DC    C'TALDIR3   ',C'TALDIR3   '  TAL3                                
         DC    C'TALFIL3   ',C'TALFIL3   '                                      
         DC    C'CHKDIR3   ',C'CHKDIR3   '                                      
         DC    C'CHKFIL3   ',C'CHKFIL3   '                                      
         DC    C'TALREQ3   ',C'TALREQ3   '                                      
         DC    C'TALRCV3   ',C'TALRCV3   '                                      
*                                                                               
         DC    C'TALDIR4   ',C'TALDIR4   '  TAL4                                
         DC    C'TALFIL4   ',C'TALFIL4   '                                      
         DC    C'CHKDIR4   ',C'CHKDIR4   '                                      
         DC    C'CHKFIL4   ',C'CHKFIL4   '                                      
         DC    C'TALREQ4   ',C'TALREQ4   '                                      
         DC    C'TALRCV4   ',C'TALRCV4   '                                      
*                                                                               
         DC    C'CTFILE    ',C'CTFILE   V'  CON                                 
         DC    C'CTRCVR    ',C'CTRCVR   V'                                      
         DC    C'CTREQ     ',C'CTREQ    V'                                      
         DC    C'GENDIR    ',C'GENDIR   V'                                      
         DC    C'GENFIL    ',C'GENFIL   V'                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021FAPARMQ   12/23/20'                                      
         END                                                                    
