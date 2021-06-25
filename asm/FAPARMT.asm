*          DATA SET FAPARMT    AT LEVEL 036 AS OF 12/23/20                      
*PHASE FAPARMTA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - V T A M   TEST SYSTEM'               
***********************************************************************         
* AHYD - OCT01/12  REMOVED ACCC R/W FOR COMPANY UB RESTORE                      
* AHYD - RPTEST (NETW) DEC/2010                                                 
* AHYD - FMNY   (NETW) OCT/2012 (PAYING FOR THIS)                               
* AHYD - FMNY   (NETW) NOV/2012 @DR SITE NO NETW YET                            
* AHYD - ZENITH (NETR) FEB/2012 (PAYING FOR THIS)                               
***********************************************************************         
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(1)                                                           
FACORELN DC    A(1000*K)                                                        
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
*                                                                               
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
DMID     DC    C'FT'                                                            
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLST  DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                ALL SYSTEMS NOP                              
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
*AH3     DC    CL8'SPOT1   '       GLOBAL: MEL & TST                            
         DC    CL8'SPOTTT  '       GLOBAL: MEL & TST                            
         DC    CL8'SPOT0   '       GLOBAL: DDSION & MEL & TST                   
         DC    CL8'STRTT   '       GLOBAL: MEL & TST                            
         DC    CL8'STR0    '       GLOBAL: MEL & TST                            
         DC    CL8'NETTT   '       GLOBAL: MEL & TST                            
         DC    CL8'NET6    '       GLOBAL: OPTIMIZER MEL & TST DDSION           
         DC    CL8'NETT    '       GLOBAL: TST                                  
         DC    CL8'PRNTTT  '       GLOBAL: TST & MEL                            
         DC    CL8'PRNTT   '       GLOBAL: TST & MEL                            
         DC    CL8'ACC0    '       GLOBAL: DDSION & TST & MEL                   
         DC    CL8'ACCTT   '       GLOBAL: DDSION & TST & MEL                   
         DC    CL8'ACC1    '                                                    
         DC    CL8'ACC2    '                                                    
         DC    CL8'ACC3    '                                                    
*        DC    CL8'TAL2    '       GLOBAL: DDSION & TST                         
*        DC    CL8'TAL3    '       GLOBAL: DDSION & TST                         
         DC    CL8'REP2    '       GLOBAL: TST & MEL                            
         DC    CL8'REP6    '       GLOBAL: TST & MEL                            
         DC    CL8'DEMO    '       DEMOS                                        
         DC    CL8'PER1    '       GLOBAL: TST & MEL                            
         DC    CL8'GAMES   '                                                    
         DC    CL8'CONTROL '                                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
         DC    X'00'                                                            
*                                                                               
PGOPNOP  DC    C'+'                NOP PROGRAM(S)                               
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
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    AL1(YES)            UPDATE PRGMS FILE FROM THIS SYS              
*                                                                               
VTAMAPL  DC    CL8'TST'            VTAM APPLICATION ID                          
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
FACSPACE DC    C'DMGTDATAMGRX'     DATAMGR DATASPACE NAME                       
*                                                                               
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    AL1(YES)            TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    AL1(YES)            NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABTXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    AL1(YES)            DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    AL1(YES)            UPDATE TABS TABLES FROM THIS FACPAK          
FACUDATE DC    X'00'                                                            
FACXAUTL DC    AL1(YES)            BUILD UTLS IN XA(=Y)                         
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
         DC    XL2'00'             SPARE                                        
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
PGMSDSP  DC    CL12'PRGTXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((4*K*K)+32)     MINIO (4 MEGS)                               
FACSMTP  DC    CL8'JESMAIL'        JES3/SMTP                                    
FACMQMAX DC    F'10'               MAX MQ                                       
FAMOFA   DC    AL1(NO)             MQ FOR REP                                   
FACMQIO  DC    CL08'FAMQIO'                                                     
*                                                                               
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK POLLING                               
FAALLC   DC    AL1(NO)             ALL CHARACTERS ARE VALID                     
         DC    XL22'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48'MQ7T'                                                       
FACMQIN  DC    CL48'TST.INPUT.ALIASQ'                                           
FACMQOU  DC    CL48'DDS.BROKER.TEST.LOCALQ'                                     
FACMQWK  DC    CL48'TST.WORK.ALIASQ'                                            
FACMQCT  DC    CL48'TST.CONTROL.ALIASQ'                                         
*                                                                               
DMFLIST  DS    0CL20                                                            
         DC    C'PRGMS     ',C'PRGMS     '                                      
         DC    C'TEMPSTR   ',C'TEMPSTR   '                                      
         DC    C'TEMPEST   ',C'TEMPEST   '                                      
         DC    C'ADRFILE   ',C'ADRFILE   '                                      
         DC    C'STATS     ',C'STATS     '                                      
         DC    C'DMPFILE   ',C'DMPFILE   '                                      
         DC    C'TSTRCVR   ',C'TSTRCVR   '                                      
         DC    C'WKFILE    ',C'WKFILE    '                                      
         DC    C'FACWRK    ',C'FACWRK    '                                      
         DC    C'EASIWK    ',C'EASIWK    '                                      
         DC    C'KWXFILE   ',C'KWXFILE   '                                      
*                                                                               
         DC    C'EDCTA     ',C'EDCTA     '                                      
         DC    C'EDCTR     ',C'EDCTR     '                                      
*NOP*    DC    C'EDCTZ     ',C'EDCTZ     '                                      
*                                                                               
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
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    C'CTFILE    ',C'CTFILE   V'                                      
         DC    C'CTRCVR    ',C'CTRCVR   V'                                      
         DC    C'CTREQ     ',C'CTREQ    V'                                      
         DC    C'GENDIR    ',C'GENDIR   V'  AS OF JUN5TH - 2011                 
         DC    C'GENFIL    ',C'GENFIL   V'                                      
*&&DO                                                                           
* SPOT1                                                                         
         DC    C'SPTDIR1   ',C'SPTDIR1   '                                      
         DC    C'SPTFIL1   ',C'SPTFIL1   '                                      
         DC    C'XSPDIR1   ',C'XSPDIR1   '                                      
         DC    C'XSPFIL1   ',C'XSPFIL1   '                                      
         DC    C'RECV1     ',C'RECV1     '                                      
         DC    C'REQ1      ',C'REQ1      '                                      
         DC    C'STAFIL1   ',C'STAFIL1   '                                      
*                                                                               
* TRAFFIC 1 (STR1)                                                              
*                                                                               
         DC    C'TRFDIR1   ',C'TRFDIR1   '                                      
         DC    C'TRFFIL1   ',C'TRFFIL1   '                                      
         DC    C'TRFRCV1   ',C'TRFRCV1   '                                      
         DC    C'TRFREQ1   ',C'TRFREQ1   '                                      
*&&                                                                             
* SPOTTT                                                                        
         DC    C'SPTDIRTT  ',C'SPTDIRTT  '                                      
         DC    C'SPTFILTT  ',C'SPTFILTT  '                                      
         DC    C'XSPDIRTT  ',C'XSPDIRTT  '                                      
         DC    C'XSPFILTT  ',C'XSPFILTT  '                                      
         DC    C'RECVTT    ',C'RECVTT    '                                      
         DC    C'REQTT     ',C'REQTT     '                                      
         DC    C'STAFILTT  ',C'STAFILTT  '                                      
*                                                                               
* TRAFFIC TT (STRTT)                                                            
*                                                                               
         DC    C'TRFDIRTT  ',C'TRFDIRTT  '                                      
         DC    C'TRFFILTT  ',C'TRFFILTT  '                                      
         DC    C'TRFRCVTT  ',C'TRFRCVTT  '                                      
         DC    C'TRFREQTT  ',C'TRFREQTT  '                                      
*                                                                               
*        DC    C'SPTDIR3   ',C'SPTDIR3   ' OMDTST                               
*        DC    C'SPTFIL3   ',C'SPTFIL3   '                                      
*        DC    C'XSPDIR3   ',C'XSPDIR3   '                                      
*        DC    C'XSPFIL3   ',C'XSPFIL3   '                                      
*        DC    C'RECV3     ',C'RECV3     '                                      
*        DC    C'REQ3      ',C'REQ3      '                                      
*        DC    C'STAFIL3   ',C'STAFIL3   '                                      
*                                                                               
*        DC    C'TRFDIR3   ',C'TRFDIR3   ' OMDTST                               
*        DC    C'TRFFIL3   ',C'TRFFIL3   '                                      
*        DC    C'TRFRCV3   ',C'TRFRCV3   '                                      
*        DC    C'TRFREQ3   ',C'TRFREQ3   '                                      
*                                                                               
         DC    C'SPTDIR0   ',C'SPTDIR0   '                                      
         DC    C'SPTFIL0   ',C'SPTFIL0   '                                      
         DC    C'XSPDIR0   ',C'XSPDIR0   '                                      
         DC    C'XSPFIL0   ',C'XSPFIL0   '                                      
         DC    C'STAFIL0   ',C'STAFIL0   '                                      
         DC    C'RECV0     ',C'RECV0     '                                      
         DC    C'REQ0      ',C'REQ0      '                                      
*                                                                               
*        DC    C'SPTDIRG   ',C'SPTDIRG  G' RPTEST (SPOT)                        
*        DC    C'SPTFILG   ',C'SPTFILG  G'                                      
*        DC    C'XSPDIRG   ',C'XSPDIRG  G'                                      
*        DC    C'XSPFILG   ',C'XSPFILG  G'                                      
*        DC    C'RECVG     ',C'RECVG    G'                                      
*        DC    C'REQG      ',C'REQG     G'                                      
*        DC    C'STAFILG   ',C'STAFILG  G'                                      
* NETT                                                                          
         DC    C'SPTDIRT   ',C'SPTDIRT  G'                                      
         DC    C'SPTFILT   ',C'SPTFILT  G'                                      
         DC    C'XSPDIRT   ',C'XSPDIRT  G'                                      
         DC    C'XSPFILT   ',C'XSPFILT  G'                                      
         DC    C'STAFILT   ',C'STAFILT  G'                                      
         DC    C'UNTDIRT   ',C'UNTDIRT  G'                                      
         DC    C'UNTFILT   ',C'UNTFILT  G'                                      
         DC    C'RECVT     ',C'RECVT    G'                                      
         DC    C'REQT      ',C'REQT     G'                                      
* NETR                                                                          
*        DC    C'SPTDIRR   ',C'SPTDIRR  G'                                      
*        DC    C'SPTFILR   ',C'SPTFILR  G'                                      
*        DC    C'XSPDIRR   ',C'XSPDIRR  G'                                      
*        DC    C'XSPFILR   ',C'XSPFILR  G'                                      
*        DC    C'STAFILR   ',C'STAFILR  G'                                      
*        DC    C'UNTDIRR   ',C'UNTDIRR  G'                                      
*        DC    C'UNTFILR   ',C'UNTFILR  G'                                      
*        DC    C'RECVR     ',C'RECVR    G'                                      
*        DC    C'REQR      ',C'REQR     G'                                      
*                                                                               
*        DC    C'RECVN     ',C'WSTRCVN   ' FOR WESTERN                          
*        DC    C'REQN      ',C'WSTREQN   ' FOR WESTERN                          
*                                                                               
*        DC    C'RECVJ     ',C'WSTRCVJ   ' FOR WESTERN                          
*        DC    C'REQJ      ',C'WSTREQJ   ' FOR WESTERN                          
*                                                                               
         DC    C'TRFDIR0   ',C'TRFDIR0   '                                      
         DC    C'TRFFIL0   ',C'TRFFIL0   '                                      
         DC    C'TRFRCV0   ',C'TRFRCV0   '                                      
         DC    C'TRFREQ0   ',C'TRFREQ0   '                                      
*                                                                               
*        DC    C'TRFDIR7   ',C'TRFDIR7  G'                                      
*        DC    C'TRFFIL7   ',C'TRFFIL7  G'                                      
*        DC    C'TRFRCV7   ',C'TRFRCV7  G'                                      
*        DC    C'TRFREQ7   ',C'TRFREQ7  G'                                      
*&&DO                                                                           
         DC    C'PRTDIR2   ',C'PRTDIR2   '                                      
         DC    C'PRTFIL2   ',C'PRTFIL2   '                                      
         DC    C'PUBDIR2   ',C'PUBDIR2   '                                      
         DC    C'PUBFIL2   ',C'PUBFIL2   '                                      
         DC    C'PRECV2    ',C'PRECV2    '                                      
         DC    C'PREQ2     ',C'PREQ2     '                                      
*&&                                                                             
*        DC    C'PRTDIR8   ',C'PRTDIR8  G' RPTEST                               
*        DC    C'PRTFIL8   ',C'PRTFIL8  G'                                      
*        DC    C'PUBDIR8   ',C'PUBDIR8  G'                                      
*        DC    C'PUBFIL8   ',C'PUBFIL8  G'                                      
*        DC    C'PRECV8    ',C'PRECV8   G'                                      
*        DC    C'PREQ8     ',C'PREQ8    G'                                      
*                                                                               
         DC    C'PRTDIRTT  ',C'PRTDIRTT  '                                      
         DC    C'PRTFILTT  ',C'PRTFILTT  '                                      
         DC    C'PUBDIRTT  ',C'PUBDIRTT  '                                      
         DC    C'PUBFILTT  ',C'PUBFILTT  '                                      
         DC    C'PRECVTT   ',C'PRECVTT   '                                      
         DC    C'PREQTT    ',C'PREQTT    '                                      
*                                                                               
         DC    C'PRTDIRT   ',C'PRTDIRT   '                                      
         DC    C'PRTFILT   ',C'PRTFILT   '                                      
         DC    C'PUBDIRT   ',C'PUBDIRT   '                                      
         DC    C'PUBFILT   ',C'PUBFILT   '                                      
         DC    C'PRECVT    ',C'PRECVT    '                                      
         DC    C'PREQT     ',C'PREQT     '                                      
* NETTT                                                                         
         DC    C'UNTDIRTN  ',C'UNTDIRTN  '                                      
         DC    C'UNTFILTN  ',C'UNTFILTN  '                                      
         DC    C'RECVTN    ',C'RECVTN    '                                      
         DC    C'REQTN     ',C'REQTN     '                                      
         DC    C'XSPDIRTN  ',C'XSPDIRTN  '                                      
         DC    C'XSPFILTN  ',C'XSPFILTN  '                                      
         DC    C'SPTDIRTN  ',C'SPTDIRTN  '                                      
         DC    C'SPTFILTN  ',C'SPTFILTN  '                                      
         DC    C'STAFILTN  ',C'STAFILTN  '                                      
*&&DO                                                                           
         DC    C'UNTDIR8   ',C'UNTDIR8   '                                      
         DC    C'UNTFIL8   ',C'UNTFIL8   '                                      
         DC    C'RECV8     ',C'RECV8     '                                      
         DC    C'REQ8      ',C'REQ8      '                                      
         DC    C'XSPDIR8   ',C'XSPDIR8   '                                      
         DC    C'XSPFIL8   ',C'XSPFIL8   '                                      
         DC    C'SPTDIR8   ',C'SPTDIR8   '                                      
         DC    C'SPTFIL8   ',C'SPTFIL8   '                                      
         DC    C'STAFIL8   ',C'STAFIL8   '                                      
*&&                                                                             
*&&DO                                                                           
         DC    C'UNTDIRK   ',C'UNTDIRK   '                                      
         DC    C'UNTFILK   ',C'UNTFILK   '                                      
         DC    C'RECVK     ',C'RECVK     '                                      
         DC    C'REQK      ',C'REQK      '                                      
         DC    C'XSPDIRK   ',C'XSPDIRK   '                                      
         DC    C'XSPFILK   ',C'XSPFILK   '                                      
         DC    C'SPTDIRK   ',C'SPTDIRK   '                                      
         DC    C'SPTFILK   ',C'SPTFILK   '                                      
         DC    C'STAFILK   ',C'STAFILK   '                                      
*&&                                                                             
*        DC    C'UNTDIRW   ',C'UNTDIRW  G' FMNY                                 
*        DC    C'UNTFILW   ',C'UNTFILW  G'                                      
*        DC    C'RECVW     ',C'RECVW    G'                                      
*        DC    C'REQW      ',C'REQW     G'                                      
*        DC    C'XSPDIRW   ',C'XSPDIRW  G'                                      
*        DC    C'XSPFILW   ',C'XSPFILW  G'                                      
*        DC    C'SPTDIRW   ',C'SPTDIRW  G'                                      
*        DC    C'SPTFILW   ',C'SPTFILW  G'                                      
*        DC    C'STAFILW   ',C'STAFILW  G'                                      
*                                                                               
         DC    C'REPDIR2   ',C'REPDIR2   '                                      
         DC    C'REPFIL2   ',C'REPFIL2   '                                      
         DC    C'RREQ2     ',C'RREQ2     '                                      
         DC    C'REPRCV2   ',C'REPRCV2   '                                      
         DC    C'REPWRK2   ',C'REPWRK2   '                                      
         DC    C'RRGNEW2   ',C'RRGNEW2   '                                      
*                                                                               
         DC    C'REPDIR6   ',C'REPDIR6   '                                      
         DC    C'REPFIL6   ',C'REPFIL6   '                                      
         DC    C'RREQ6     ',C'RREQ6     '                                      
         DC    C'REPRCV6   ',C'REPRCV6   '                                      
         DC    C'REPWRK6   ',C'REPWRK6   '                                      
         DC    C'RRGNEW6   ',C'RRGNEW6   '                                      
*                                                                               
         DC    C'TALDIR2   ',C'TALDIR2   '                                      
         DC    C'TALFIL2   ',C'TALFIL2   '                                      
         DC    C'CHKDIR2   ',C'CHKDIR2   '                                      
         DC    C'CHKFIL2   ',C'CHKFIL2   '                                      
         DC    C'TALREQ2   ',C'TALREQ2   '                                      
         DC    C'TALRCV2   ',C'TALRCV2   '                                      
*                                                                               
         DC    C'TALDIR3   ',C'TALDIR3   '                                      
         DC    C'TALFIL3   ',C'TALFIL3   '                                      
         DC    C'CHKDIR3   ',C'CHKDIR3   '                                      
         DC    C'CHKFIL3   ',C'CHKFIL3   '                                      
         DC    C'TALREQ3   ',C'TALREQ3   '                                      
         DC    C'TALRCV3   ',C'TALRCV3   '                                      
*                                                                               
         DC    C'ACCREQ0   ',C'ACCREQ0   '                                      
         DC    C'ACCRCV0   ',C'ACCRCV0   '                                      
         DC    C'ACCDAY0   ',C'ACCDAY0   '                                      
         DC    C'ACCWRK0   ',C'ACCWRK0   '                                      
         DC    C'ACCDIR0   ',C'ACCDIR0   '                                      
         DC    C'ACCMST0   ',C'ACCMST0   '                                      
         DC    C'ACCARC0   ',C'ACCARC0   '                                      
*                                                                               
         DC    C'ACCREQ1   ',C'ACCREQ1   '                                      
         DC    C'ACCRCV1   ',C'ACCRCV1   '                                      
         DC    C'ACCDAY1   ',C'ACCDAY1   '                                      
         DC    C'ACCWRK1   ',C'ACCWRK1   '                                      
         DC    C'ACCDIR1   ',C'ACCDIR1   '                                      
         DC    C'ACCMST1   ',C'ACCMST1   '                                      
         DC    C'ACCARC1   ',C'ACCARC1   '                                      
*                                                                               
         DC    C'ACCREQ2   ',C'ACCREQ2   '                                      
         DC    C'ACCRCV2   ',C'ACCRCV2   '                                      
         DC    C'ACCDAY2   ',C'ACCDAY2   '                                      
         DC    C'ACCWRK2   ',C'ACCWRK2   '                                      
         DC    C'ACCDIR2   ',C'ACCDIR2   '                                      
         DC    C'ACCMST2   ',C'ACCMST2   '                                      
         DC    C'ACCARC2   ',C'ACCARC2   '                                      
*                                                                               
         DC    C'ACCREQ3   ',C'ACCREQ3   '                                      
         DC    C'ACCRCV3   ',C'ACCRCV3   '                                      
         DC    C'ACCDAY3   ',C'ACCDAY3   '                                      
         DC    C'ACCWRK3   ',C'ACCWRK3   '                                      
         DC    C'ACCDIR3   ',C'ACCDIR3   '                                      
         DC    C'ACCMST3   ',C'ACCMST3   '                                      
         DC    C'ACCARC3   ',C'ACCARC3   '                                      
*                                                                               
* TEMP FOR COMPANY ALPHA UB (AHYD MARCH 21ST)                                   
* REMOVED                                                                       
*        DC    C'ACCREQC   ',C'ACCREQC   '                                      
*        DC    C'ACCRCVC   ',C'ACCRCVC   '                                      
*        DC    C'ACCDAYC   ',C'ACCDAYC   '                                      
*        DC    C'ACCWRKC   ',C'ACCWRKC   '                                      
*        DC    C'ACCDIRC   ',C'ACCDIRC   '                                      
*        DC    C'ACCMSTC   ',C'ACCMSTC   '                                      
*        DC    C'ACCARCC   ',C'ACCARCC   '                                      
*                                                                               
         DC    C'ACCREQTT  ',C'ACCREQTT  '                                      
         DC    C'ACCRCVTT  ',C'ACCRCVTT  '                                      
         DC    C'ACCDAYTT  ',C'ACCDAYTT  '                                      
         DC    C'ACCWRKTT  ',C'ACCWRKTT  '                                      
         DC    C'ACCDIRTT  ',C'ACCDIRTT  '                                      
         DC    C'ACCMSTTT  ',C'ACCMSTTT  '                                      
         DC    C'ACCARCTT  ',C'ACCARCTT  '                                      
*&&DO                                                                           
         DC    C'ACCREQT   ',C'ACCREQT   '                                      
         DC    C'ACCRCVT   ',C'ACCRCVT   '                                      
         DC    C'ACCDAYT   ',C'ACCDAYT   '                                      
         DC    C'ACCWRKT   ',C'ACCWRKT   '                                      
         DC    C'ACCDIRT   ',C'ACCDIRT   '                                      
         DC    C'ACCMSTT   ',C'ACCMSTT   '                                      
         DC    C'ACCARCT   ',C'ACCARCT   '                                      
*&&                                                                             
         DC    C'REPDIR9   ',C'REPDIR9   '                                      
         DC    C'REPFIL9   ',C'REPFIL9   '                                      
         DC    C'RREQ9     ',C'RREQ9     '                                      
         DC    C'REPRCV9   ',C'REPRCV9   '                                      
         DC    C'REPWRK9   ',C'REPWRK9   '                                      
         DC    C'RRGNEW9   ',C'RRGNEW9   '                                      
*                                                                               
         DC    C'REPDIRK   ',C'REPDIRK   '   ADDED APR12/2007                   
         DC    C'REPFILK   ',C'REPFILK   '                                      
         DC    C'RREQK     ',C'RREQK     '                                      
         DC    C'REPRCVK   ',C'REPRCVK   '                                      
         DC    C'REPWRKK   ',C'REPWRKK   '                                      
         DC    C'RRGNEWK   ',C'RRGNEWK   '                                      
*                                                                               
         DC    C'PERDIR1   ',C'PERDIR1  G'                                      
         DC    C'PERFIL1   ',C'PERFIL1  G'                                      
         DC    C'PERREQ1   ',C'PERREQ1  G'                                      
         DC    C'PERRCV1   ',C'PERRCV1  G'                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036FAPARMT   12/23/20'                                      
         END                                                                    
