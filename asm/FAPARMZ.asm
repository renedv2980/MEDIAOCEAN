*          DATA SET FAPARMZ    AT LEVEL 020 AS OF 12/23/20                      
*PHASE FAPARMZA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - CSC SYSTEM'                          
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(11)                                                          
FACORELN DC    A(160*K)                                                         
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'5'                                                             
TIMLOOP  DC    F'30'                                                            
TIMAUTOQ DC    F'120'                                                           
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS ***          
TSKPGM   DC    A(128*K)            PROGRAMS AREA                                
TSKWRK   DC    A(128*K)            WORKING STORAGE AREA                         
TSKTIA   DC    A(18*K)             TIA AREA                                     
TSKTWA   DC    A(18*K)             TWA AREA                                     
TSKMAP   DC    A(K+16)             MAP AREA FOR PHASE / NODES                   
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'FC'                                                            
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLIST DC    AL1(20)             LENGTH OF ENTRY / 2 (OLD TO NEW)             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
*                                                                               
SEOPNOP  DC    C'!'                ALL SYSTEMS EXCEPT THESE STARTED             
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'TAL2    '                                                    
         DC    CL8'TAL4    '                                                    
         DC    CL8'PER1    '                                                    
         DC    CL8'PER2    '                                                    
         DC    CL8'PRNTOV  '                                                    
         DC    CL8'PRNTT   '                                                    
         DC    CL8'NETOV   '                                                    
         DC    CL8'NETT    '                                                    
         DC    CL8'SPOTOV  '                                                    
         DC    CL8'SPOT0   '                                                    
         DC    CL8'STROV   '                                                    
         DC    CL8'STR0    '                                                    
         DC    CL8'ACCOV   '                                                    
         DC    CL8'ACCTT   '                                                    
         DC    CL8'ACCT    '                                                    
         DC    CL8'ACCY    '                                                    
         DC    CL8'MBA1    '                                                    
         DC    CL8'REP2    '                                                    
         DC    CL8'REP6    '                                                    
         DC    CL8'REP9    '                                                    
         DC    CL8'REPK    '                                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             NOW 35000 WAS 10000                          
FACPTLST DC    CL6'ADVPT '                                                      
FACPOPLN DC    H'2'                TIMER POP DURATION (SEC/100)                 
FACPOPMX DC    H'192'              TIMER POP MAXIMUM COUNTER                    
FACPRIO  DC    H'50'               PRIORITY I/O THRESHOLD                       
FACPRCPU DC    H'80'               PRIORITY CPU THRESHOLD                       
FACPRMIN DC    H'5'                PRIORITY MIN VALUE                           
FACDAROK DC    AL1(YES)            PROCESS DARE FROM THIS FACPAK                
FAC31BIT DC    AL1(YES)            USE 31-BIT I/O ADDRESSES                     
*                                                                               
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    AL1(NO)             CAN'T UPDATE PRGMS FILE                      
*                                                                               
VTAMAPL  DC    CL8'CSC'            VTAM APPLICATION ID                          
VTAMUTL  DC    H'300'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'300'              VTAM NUM ENTRIES IN PRQ                      
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
FACSPACE DC    C'DMGCDATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    AL1(YES)            TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    AL1(YES)            NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABCXXXXXXXX'  NAME OF TABS DATASPACE                       
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
FAJESIO  DC    CL8'       '        *** MONSOON *** (WAS FAJESIO)                
FACJOBMX DC    AL2(500)            *** MONSOON *** (WAS 50)                     
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
PGMSDSP  DC    CL12'PRGCXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((4*K*K)+32)     MINIO 4 MEGS + EYE CATCHER                   
*                                                                               
FACSMTP  DC    CL8'JESMAIL'                                                     
FACMQMAX DC    F'10'                                                            
FAMOFA   DC    AL1(NO)                                                          
FACMQIO  DC    CL8'FAMQIO  '                                                    
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK POLLING                               
FAALLC   DC    AL1(NO)             ALL CHARACTERS ARE VALID                     
         DC    XL22'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48'MQ7C'                                                       
FACMQIN  DC    CL48'CSC.INPUT.ALIASQ'                                           
FACMQOU  DC    CL48'DDS.BROKER.CSC.LOCALQ'                                      
FACMQWK  DC    CL48'CSC.WORK.ALIASQ'                                            
FACMQCT  DC    CL48'CSC.CONTROL.LOCALQ'                                         
*FACMQCT  DC    CL48'CSC.CONTROL.ALIASQ'                                        
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
         DC    C'WKFILE    ',C'WKFILE    '                                      
         DC    C'FACWRK    ',C'FACWRK    '                                      
         DC    C'KWXFILE   ',C'KWXFILE   '                                      
         DC    C'EDCTA     ',C'EDCTA     '                                      
         DC    C'EDCTR     ',C'EDCTR     '                                      
         DC    C'EDCTZ     ',C'EDCTZ     '                                      
*                                                                               
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
* ACC0                                                                          
         DC    C'ACCREQ0   ',C'ACCREQ0   '                                      
         DC    C'ACCRCV0   ',C'ACCRCV0   '                                      
         DC    C'ACCDAY0   ',C'ACCDAY0   '                                      
         DC    C'ACCWRK0   ',C'ACCWRK0   '                                      
         DC    C'ACCDIR0   ',C'ACCDIR0   '                                      
         DC    C'ACCMST0   ',C'ACCMST0   '                                      
         DC    C'ACCARC0   ',C'ACCARC0   '                                      
* ACC1                                                                          
         DC    C'ACCREQ1   ',C'ACCREQ1   '                                      
         DC    C'ACCRCV1   ',C'ACCRCV1   '                                      
         DC    C'ACCDAY1   ',C'ACCDAY1   '                                      
         DC    C'ACCWRK1   ',C'ACCWRK1   '                                      
         DC    C'ACCDIR1   ',C'ACCDIR1   '                                      
         DC    C'ACCMST1   ',C'ACCMST1   '                                      
         DC    C'ACCARC1   ',C'ACCARC1   '                                      
* ACC2                                                                          
         DC    C'ACCREQ2   ',C'ACCREQ2   '                                      
         DC    C'ACCRCV2   ',C'ACCRCV2   '                                      
         DC    C'ACCDAY2   ',C'ACCDAY2   '                                      
         DC    C'ACCWRK2   ',C'ACCWRK2   '                                      
         DC    C'ACCDIR2   ',C'ACCDIR2   '                                      
         DC    C'ACCMST2   ',C'ACCMST2   '                                      
         DC    C'ACCARC2   ',C'ACCARC2   '                                      
* ACC3                                                                          
         DC    C'ACCREQ3   ',C'ACCREQ3   '                                      
         DC    C'ACCRCV3   ',C'ACCRCV3   '                                      
         DC    C'ACCDAY3   ',C'ACCDAY3   '                                      
         DC    C'ACCWRK3   ',C'ACCWRK3   '                                      
         DC    C'ACCDIR3   ',C'ACCDIR3   '                                      
         DC    C'ACCMST3   ',C'ACCMST3   '                                      
         DC    C'ACCARC3   ',C'ACCARC3   '                                      
* ACC4                                                                          
         DC    C'ACCREQ4   ',C'ACCREQ4   '                                      
         DC    C'ACCRCV4   ',C'ACCRCV4   '                                      
         DC    C'ACCDAY4   ',C'ACCDAY4   '                                      
         DC    C'ACCWRK4   ',C'ACCWRK4   '                                      
         DC    C'ACCDIR4   ',C'ACCDIR4   '                                      
         DC    C'ACCMST4   ',C'ACCMST4   '                                      
         DC    C'ACCARC4   ',C'ACCARC4   '                                      
* ACC5                                                                          
         DC    C'ACCREQ5   ',C'ACCREQ5   '                                      
         DC    C'ACCRCV5   ',C'ACCRCV5   '                                      
         DC    C'ACCDAY5   ',C'ACCDAY5   '                                      
         DC    C'ACCWRK5   ',C'ACCWRK5   '                                      
         DC    C'ACCDIR5   ',C'ACCDIR5   '                                      
         DC    C'ACCMST5   ',C'ACCMST5   '                                      
         DC    C'ACCARC5   ',C'ACCARC5   '                                      
* ACC6                                                                          
         DC    C'ACCREQ6   ',C'ACCREQ6   '                                      
         DC    C'ACCRCV6   ',C'ACCRCV6   '                                      
         DC    C'ACCDAY6   ',C'ACCDAY6   '                                      
         DC    C'ACCWRK6   ',C'ACCWRK6   '                                      
         DC    C'ACCDIR6   ',C'ACCDIR6   '                                      
         DC    C'ACCMST6   ',C'ACCMST6   '                                      
         DC    C'ACCARC6   ',C'ACCARC6   '                                      
* ACC7                                                                          
         DC    C'ACCREQ7   ',C'ACCREQ7   '                                      
         DC    C'ACCRCV7   ',C'ACCRCV7   '                                      
         DC    C'ACCDAY7   ',C'ACCDAY7   '                                      
         DC    C'ACCWRK7   ',C'ACCWRK7   '                                      
         DC    C'ACCDIR7   ',C'ACCDIR7   '                                      
         DC    C'ACCMST7   ',C'ACCMST7   '                                      
         DC    C'ACCARC7   ',C'ACCARC7   '                                      
* ACC8                                                                          
         DC    C'ACCREQ8   ',C'ACCREQ8   '                                      
         DC    C'ACCRCV8   ',C'ACCRCV8   '                                      
         DC    C'ACCDAY8   ',C'ACCDAY8   '                                      
         DC    C'ACCWRK8   ',C'ACCWRK8   '                                      
         DC    C'ACCDIR8   ',C'ACCDIR8   '                                      
         DC    C'ACCMST8   ',C'ACCMST8   '                                      
         DC    C'ACCARC8   ',C'ACCARC8   '                                      
* ACC9                                                                          
         DC    C'ACCREQ9   ',C'ACCREQ9   '                                      
         DC    C'ACCRCV9   ',C'ACCRCV9   '                                      
         DC    C'ACCDAY9   ',C'ACCDAY9   '                                      
         DC    C'ACCWRK9   ',C'ACCWRK9   '                                      
         DC    C'ACCDIR9   ',C'ACCDIR9   '                                      
         DC    C'ACCMST9   ',C'ACCMST9   '                                      
         DC    C'ACCARC9   ',C'ACCARC9   '                                      
* ACCA                                                                          
         DC    C'ACCREQA   ',C'ACCREQA   '                                      
         DC    C'ACCRCVA   ',C'ACCRCVA   '                                      
         DC    C'ACCDAYA   ',C'ACCDAYA   '                                      
         DC    C'ACCWRKA   ',C'ACCWRKA   '                                      
         DC    C'ACCDIRA   ',C'ACCDIRA   '                                      
         DC    C'ACCMSTA   ',C'ACCMSTA   '                                      
         DC    C'ACCARCA   ',C'ACCARCA   '                                      
* ACCB                                                                          
         DC    C'ACCREQB   ',C'ACCREQB   '                                      
         DC    C'ACCRCVB   ',C'ACCRCVB   '                                      
         DC    C'ACCDAYB   ',C'ACCDAYB   '                                      
         DC    C'ACCWRKB   ',C'ACCWRKB   '                                      
         DC    C'ACCDIRB   ',C'ACCDIRB   '                                      
         DC    C'ACCMSTB   ',C'ACCMSTB   '                                      
         DC    C'ACCARCB   ',C'ACCARCB   '                                      
* ACCC                                                                          
         DC    C'ACCREQC   ',C'ACCREQC   '                                      
         DC    C'ACCRCVC   ',C'ACCRCVC   '                                      
         DC    C'ACCDAYC   ',C'ACCDAYC   '                                      
         DC    C'ACCWRKC   ',C'ACCWRKC   '                                      
         DC    C'ACCDIRC   ',C'ACCDIRC   '                                      
         DC    C'ACCMSTC   ',C'ACCMSTC   '                                      
         DC    C'ACCARCC   ',C'ACCARCC   '                                      
* ACCD                                                                          
         DC    C'ACCREQD   ',C'ACCREQD   '                                      
         DC    C'ACCRCVD   ',C'ACCRCVD   '                                      
         DC    C'ACCDAYD   ',C'ACCDAYD   '                                      
         DC    C'ACCWRKD   ',C'ACCWRKD   '                                      
         DC    C'ACCDIRD   ',C'ACCDIRD   '                                      
         DC    C'ACCMSTD   ',C'ACCMSTD   '                                      
         DC    C'ACCARCD   ',C'ACCARCD   '                                      
* ACCQ                                                                          
         DC    C'ACCREQQ   ',C'ACCREQQ   '                                      
         DC    C'ACCRCVQ   ',C'ACCRCVQ   '                                      
         DC    C'ACCDAYQ   ',C'ACCDAYQ   '                                      
         DC    C'ACCWRKQ   ',C'ACCWRKQ   '                                      
         DC    C'ACCDIRQ   ',C'ACCDIRQ   '                                      
         DC    C'ACCMSTQ   ',C'ACCMSTQ   '                                      
         DC    C'ACCARCQ   ',C'ACCARCQ   '                                      
* ACCV                                                                          
         DC    C'ACCREQV   ',C'ACCREQV   '                                      
         DC    C'ACCRCVV   ',C'ACCRCVV   '                                      
         DC    C'ACCDAYV   ',C'ACCDAYV   '                                      
         DC    C'ACCWRKV   ',C'ACCWRKV   '                                      
         DC    C'ACCDIRV   ',C'ACCDIRV   '                                      
         DC    C'ACCMSTV   ',C'ACCMSTV   '                                      
         DC    C'ACCARCV   ',C'ACCARCV   '                                      
* ACCY                                                                          
         DC    C'ACCREQY   ',C'ACCREQY   '                                      
         DC    C'ACCRCVY   ',C'ACCRCVY   '                                      
         DC    C'ACCDAYY   ',C'ACCDAYY   '                                      
         DC    C'ACCWRKY   ',C'ACCWRKY   '                                      
         DC    C'ACCDIRY   ',C'ACCDIRY   '                                      
         DC    C'ACCMSTY   ',C'ACCMSTY   '                                      
         DC    C'ACCARCY   ',C'ACCARCY   '                                      
* ACCZ                                                                          
         DC    C'ACCREQZ   ',C'ACCREQZ   '                                      
         DC    C'ACCRCVZ   ',C'ACCRCVZ   '                                      
         DC    C'ACCDAYZ   ',C'ACCDAYZ   '                                      
         DC    C'ACCWRKZ   ',C'ACCWRKZ   '                                      
         DC    C'ACCDIRZ   ',C'ACCDIRZ   '                                      
         DC    C'ACCMSTZ   ',C'ACCMSTZ   '                                      
         DC    C'ACCARCZ   ',C'ACCARCZ   '                                      
* ACCIT                                                                         
         DC    C'ACCREQIT  ',C'ACCREQIT  '                                      
         DC    C'ACCRCVIT  ',C'ACCRCVIT  '                                      
         DC    C'ACCDAYIT  ',C'ACCDAYIT  '                                      
         DC    C'ACCWRKIT  ',C'ACCWRKIT  '                                      
         DC    C'ACCDIRIT  ',C'ACCDIRIT  '                                      
         DC    C'ACCMSTIT  ',C'ACCMSTIT  '                                      
         DC    C'ACCARCIT  ',C'ACCARCIT  '                                      
* ACCI1                                                                         
         DC    C'ACCREQI1  ',C'ACCREQI1  '                                      
         DC    C'ACCRCVI1  ',C'ACCRCVI1  '                                      
         DC    C'ACCDAYI1  ',C'ACCDAYI1  '                                      
         DC    C'ACCWRKI1  ',C'ACCWRKI1  '                                      
         DC    C'ACCDIRI1  ',C'ACCDIRI1  '                                      
         DC    C'ACCMSTI1  ',C'ACCMSTI1  '                                      
         DC    C'ACCARCI1  ',C'ACCARCI1  '                                      
* ACCI3                                                                         
         DC    C'ACCREQI3  ',C'ACCREQI3  '                                      
         DC    C'ACCRCVI3  ',C'ACCRCVI3  '                                      
         DC    C'ACCDAYI3  ',C'ACCDAYI3  '                                      
         DC    C'ACCWRKI3  ',C'ACCWRKI3  '                                      
         DC    C'ACCDIRI3  ',C'ACCDIRI3  '                                      
         DC    C'ACCMSTI3  ',C'ACCMSTI3  '                                      
         DC    C'ACCARCI3  ',C'ACCARCI3  '                                      
* ACCO1                                                                         
         DC    C'ACCREQO1  ',C'ACCREQO1  '                                      
         DC    C'ACCRCVO1  ',C'ACCRCVO1  '                                      
         DC    C'ACCDAYO1  ',C'ACCDAYO1  '                                      
         DC    C'ACCWRKO1  ',C'ACCWRKO1  '                                      
         DC    C'ACCDIRO1  ',C'ACCDIRO1  '                                      
         DC    C'ACCMSTO1  ',C'ACCMSTO1  '                                      
         DC    C'ACCARCO1  ',C'ACCARCO1  '                                      
* ACCTU                                                                         
         DC    C'ACCREQTU  ',C'ACCREQTU  '                                      
         DC    C'ACCRCVTU  ',C'ACCRCVTU  '                                      
         DC    C'ACCDAYTU  ',C'ACCDAYTU  '                                      
         DC    C'ACCWRKTU  ',C'ACCWRKTU  '                                      
         DC    C'ACCDIRTU  ',C'ACCDIRTU  '                                      
         DC    C'ACCMSTTU  ',C'ACCMSTTU  '                                      
         DC    C'ACCARCTU  ',C'ACCARCTU  '                                      
* ACCU1                                                                         
         DC    C'ACCREQU1  ',C'ACCREQU1  '                                      
         DC    C'ACCRCVU1  ',C'ACCRCVU1  '                                      
         DC    C'ACCDAYU1  ',C'ACCDAYU1  '                                      
         DC    C'ACCWRKU1  ',C'ACCWRKU1  '                                      
         DC    C'ACCDIRU1  ',C'ACCDIRU1  '                                      
         DC    C'ACCMSTU1  ',C'ACCMSTU1  '                                      
         DC    C'ACCARCU1  ',C'ACCARCU1  '                                      
* ACCU2                                                                         
         DC    C'ACCREQU2  ',C'ACCREQU2  '                                      
         DC    C'ACCRCVU2  ',C'ACCRCVU2  '                                      
         DC    C'ACCDAYU2  ',C'ACCDAYU2  '                                      
         DC    C'ACCWRKU2  ',C'ACCWRKU2  '                                      
         DC    C'ACCDIRU2  ',C'ACCDIRU2  '                                      
         DC    C'ACCMSTU2  ',C'ACCMSTU2  '                                      
         DC    C'ACCARCU2  ',C'ACCARCU2  '                                      
* ACCY1                                                                         
         DC    C'ACCREQY1  ',C'ACCREQY1  '                                      
         DC    C'ACCRCVY1  ',C'ACCRCVY1  '                                      
         DC    C'ACCDAYY1  ',C'ACCDAYY1  '                                      
         DC    C'ACCWRKY1  ',C'ACCWRKY1  '                                      
         DC    C'ACCDIRY1  ',C'ACCDIRY1  '                                      
         DC    C'ACCMSTY1  ',C'ACCMSTY1  '                                      
         DC    C'ACCARCY1  ',C'ACCARCY1  '                                      
* ACCN1                                                                         
         DC    C'ACCREQN1  ',C'ACCREQN1  '                                      
         DC    C'ACCRCVN1  ',C'ACCRCVN1  '                                      
         DC    C'ACCDAYN1  ',C'ACCDAYN1  '                                      
         DC    C'ACCWRKN1  ',C'ACCWRKN1  '                                      
         DC    C'ACCDIRN1  ',C'ACCDIRN1  '                                      
         DC    C'ACCMSTN1  ',C'ACCMSTN1  '                                      
         DC    C'ACCARCN1  ',C'ACCARCN1  '                                      
* ACCN2                                                                         
         DC    C'ACCREQN2  ',C'ACCREQN2  '                                      
         DC    C'ACCRCVN2  ',C'ACCRCVN2  '                                      
         DC    C'ACCDAYN2  ',C'ACCDAYN2  '                                      
         DC    C'ACCWRKN2  ',C'ACCWRKN2  '                                      
         DC    C'ACCDIRN2  ',C'ACCDIRN2  '                                      
         DC    C'ACCMSTN2  ',C'ACCMSTN2  '                                      
         DC    C'ACCARCN2  ',C'ACCARCN2  '                                      
* SPOT1                                                                         
         DC    C'SPTDIR1   ',C'SPTDIR1   '                                      
         DC    C'SPTFIL1   ',C'SPTFIL1   '                                      
         DC    C'XSPDIR1   ',C'XSPDIR1   '                                      
         DC    C'XSPFIL1   ',C'XSPFIL1   '                                      
         DC    C'STAFIL1   ',C'STAFIL1   '                                      
         DC    C'RECV1     ',C'RECV1     '                                      
         DC    C'REQ1      ',C'REQ1      '                                      
* SPOT2                                                                         
         DC    C'SPTDIR2   ',C'SPTDIR2   '                                      
         DC    C'SPTFIL2   ',C'SPTFIL2   '                                      
         DC    C'XSPDIR2   ',C'XSPDIR2   '                                      
         DC    C'XSPFIL2   ',C'XSPFIL2   '                                      
         DC    C'STAFIL2   ',C'STAFIL2   '                                      
         DC    C'RECV2     ',C'RECV2     '                                      
         DC    C'REQ2      ',C'REQ2      '                                      
* SPOT3                                                                         
         DC    C'SPTDIR3   ',C'SPTDIR3   '                                      
         DC    C'SPTFIL3   ',C'SPTFIL3   '                                      
         DC    C'XSPDIR3   ',C'XSPDIR3   '                                      
         DC    C'XSPFIL3   ',C'XSPFIL3   '                                      
         DC    C'STAFIL3   ',C'STAFIL3   '                                      
         DC    C'RECV3     ',C'RECV3     '                                      
         DC    C'REQ3      ',C'REQ3      '                                      
* SPOT4                                                                         
         DC    C'SPTDIR4   ',C'SPTDIR4   '                                      
         DC    C'SPTFIL4   ',C'SPTFIL4   '                                      
         DC    C'XSPDIR4   ',C'XSPDIR4   '                                      
         DC    C'XSPFIL4   ',C'XSPFIL4   '                                      
         DC    C'STAFIL4   ',C'STAFIL4   '                                      
         DC    C'RECV4     ',C'RECV4     '                                      
         DC    C'REQ4      ',C'REQ4      '                                      
* SPOT5                                                                         
         DC    C'SPTDIR5   ',C'SPTDIR5   '                                      
         DC    C'SPTFIL5   ',C'SPTFIL5   '                                      
         DC    C'XSPDIR5   ',C'XSPDIR5   '                                      
         DC    C'XSPFIL5   ',C'XSPFIL5   '                                      
         DC    C'STAFIL5   ',C'STAFIL5   '                                      
         DC    C'RECV5     ',C'RECV5     '                                      
         DC    C'REQ5      ',C'REQ5      '                                      
* SPOT6                                                                         
         DC    C'SPTDIR6   ',C'SPTDIR6   '                                      
         DC    C'SPTFIL6   ',C'SPTFIL6   '                                      
         DC    C'XSPDIR6   ',C'XSPDIR6   '                                      
         DC    C'XSPFIL6   ',C'XSPFIL6   '                                      
         DC    C'STAFIL6   ',C'STAFIL6   '                                      
         DC    C'RECV6     ',C'RECV6     '                                      
         DC    C'REQ6      ',C'REQ6      '                                      
* SPOT7                                                                         
         DC    C'SPTDIR7   ',C'SPTDIR7   '                                      
         DC    C'SPTFIL7   ',C'SPTFIL7   '                                      
         DC    C'XSPDIR7   ',C'XSPDIR7   '                                      
         DC    C'XSPFIL7   ',C'XSPFIL7   '                                      
         DC    C'STAFIL7   ',C'STAFIL7   '                                      
         DC    C'RECV7     ',C'RECV7     '                                      
         DC    C'REQ7      ',C'REQ7      '                                      
* SPOTB                                                                         
         DC    C'SPTDIRB   ',C'SPTDIRB   '                                      
         DC    C'SPTFILB   ',C'SPTFILB   '                                      
         DC    C'XSPDIRB   ',C'XSPDIRB   '                                      
         DC    C'XSPFILB   ',C'XSPFILB   '                                      
         DC    C'STAFILB   ',C'STAFILB   '                                      
         DC    C'RECVB     ',C'RECVB     '                                      
         DC    C'REQB      ',C'REQB      '                                      
* SPOTE                                                                         
         DC    C'SPTDIRE   ',C'SPTDIRE   '                                      
         DC    C'SPTFILE   ',C'SPTFILE   '                                      
         DC    C'XSPDIRE   ',C'XSPDIRE   '                                      
         DC    C'XSPFILE   ',C'XSPFILE   '                                      
         DC    C'STAFILE   ',C'STAFILE   '                                      
         DC    C'RECVE     ',C'RECVE     '                                      
         DC    C'REQE      ',C'REQE      '                                      
* SPOTF                                                                         
         DC    C'SPTDIRF   ',C'SPTDIRF   '                                      
         DC    C'SPTFILF   ',C'SPTFILF   '                                      
         DC    C'XSPDIRF   ',C'XSPDIRF   '                                      
         DC    C'XSPFILF   ',C'XSPFILF   '                                      
         DC    C'STAFILF   ',C'STAFILF   '                                      
         DC    C'RECVF     ',C'RECVF     '                                      
         DC    C'REQF      ',C'REQF      '                                      
* SPOTG                                                                         
         DC    C'SPTDIRG   ',C'SPTDIRG   '                                      
         DC    C'SPTFILG   ',C'SPTFILG   '                                      
         DC    C'XSPDIRG   ',C'XSPDIRG   '                                      
         DC    C'XSPFILG   ',C'XSPFILG   '                                      
         DC    C'STAFILG   ',C'STAFILG   '                                      
         DC    C'RECVG     ',C'RECVG     '                                      
         DC    C'REQG      ',C'REQG      '                                      
* SPOTH                                                                         
         DC    C'SPTDIRH   ',C'SPTDIRH   '                                      
         DC    C'SPTFILH   ',C'SPTFILH   '                                      
         DC    C'XSPDIRH   ',C'XSPDIRH   '                                      
         DC    C'XSPFILH   ',C'XSPFILH   '                                      
         DC    C'STAFILH   ',C'STAFILH   '                                      
         DC    C'RECVH     ',C'RECVH     '                                      
         DC    C'REQH      ',C'REQH      '                                      
* SPOTL                                                                         
         DC    C'SPTDIRL   ',C'SPTDIRL   '                                      
         DC    C'SPTFILL   ',C'SPTFILL   '                                      
         DC    C'XSPDIRL   ',C'XSPDIRL   '                                      
         DC    C'XSPFILL   ',C'XSPFILL   '                                      
         DC    C'STAFILL   ',C'STAFILL   '                                      
         DC    C'RECVL     ',C'RECVL     '                                      
         DC    C'REQL      ',C'REQL      '                                      
* SPOTM                                                                         
         DC    C'SPTDIRM   ',C'SPTDIRM   '                                      
         DC    C'SPTFILM   ',C'SPTFILM   '                                      
         DC    C'XSPDIRM   ',C'XSPDIRM   '                                      
         DC    C'XSPFILM   ',C'XSPFILM   '                                      
         DC    C'STAFILM   ',C'STAFILM   '                                      
         DC    C'RECVM     ',C'RECVM     '                                      
         DC    C'REQM      ',C'REQM      '                                      
* SPOTN                                                                         
         DC    C'SPTDIRN   ',C'SPTDIRN   '                                      
         DC    C'SPTFILN   ',C'SPTFILN   '                                      
         DC    C'XSPDIRN   ',C'XSPDIRN   '                                      
         DC    C'XSPFILN   ',C'XSPFILN   '                                      
         DC    C'STAFILN   ',C'STAFILN   '                                      
         DC    C'RECVN     ',C'RECVN     '                                      
         DC    C'REQN      ',C'REQN      '                                      
* SPOTQ                                                                         
         DC    C'SPTDIRQ   ',C'SPTDIRQ   '                                      
         DC    C'SPTFILQ   ',C'SPTFILQ   '                                      
         DC    C'XSPDIRQ   ',C'XSPDIRQ   '                                      
         DC    C'XSPFILQ   ',C'XSPFILQ   '                                      
         DC    C'STAFILQ   ',C'STAFILQ   '                                      
         DC    C'RECVQ     ',C'RECVQ     '                                      
         DC    C'REQQ      ',C'REQQ      '                                      
* SPOTS                                                                         
         DC    C'SPTDIRS   ',C'SPTDIRS   '                                      
         DC    C'SPTFILS   ',C'SPTFILS   '                                      
         DC    C'XSPDIRS   ',C'XSPDIRS   '                                      
         DC    C'XSPFILS   ',C'XSPFILS   '                                      
         DC    C'STAFILS   ',C'STAFILS   '                                      
         DC    C'RECVS     ',C'RECVS     '                                      
         DC    C'REQS      ',C'REQS      '                                      
* SPOTU                                                                         
         DC    C'SPTDIRU   ',C'SPTDIRU   '                                      
         DC    C'SPTFILU   ',C'SPTFILU   '                                      
         DC    C'XSPDIRU   ',C'XSPDIRU   '                                      
         DC    C'XSPFILU   ',C'XSPFILU   '                                      
         DC    C'STAFILU   ',C'STAFILU   '                                      
         DC    C'RECVU     ',C'RECVU     '                                      
         DC    C'REQU      ',C'REQU      '                                      
* SPOTV                                                                         
         DC    C'SPTDIRV   ',C'SPTDIRV   '                                      
         DC    C'SPTFILV   ',C'SPTFILV   '                                      
         DC    C'XSPDIRV   ',C'XSPDIRV   '                                      
         DC    C'XSPFILV   ',C'XSPFILV   '                                      
         DC    C'STAFILV   ',C'STAFILV   '                                      
         DC    C'RECVV     ',C'RECVV     '                                      
         DC    C'REQV      ',C'REQV      '                                      
* SPOTY                                                                         
         DC    C'SPTDIRY   ',C'SPTDIRY   '                                      
         DC    C'SPTFILY   ',C'SPTFILY   '                                      
         DC    C'XSPDIRY   ',C'XSPDIRY   '                                      
         DC    C'XSPFILY   ',C'XSPFILY   '                                      
         DC    C'STAFILY   ',C'STAFILY   '                                      
         DC    C'RECVY     ',C'RECVY     '                                      
         DC    C'REQY      ',C'REQY      '                                      
* SPOTZ                                                                         
         DC    C'SPTDIRZ   ',C'SPTDIRZ   '                                      
         DC    C'SPTFILZ   ',C'SPTFILZ   '                                      
         DC    C'XSPDIRZ   ',C'XSPDIRZ   '                                      
         DC    C'XSPFILZ   ',C'XSPFILZ   '                                      
         DC    C'STAFILZ   ',C'STAFILZ   '                                      
         DC    C'RECVZ     ',C'RECVZ     '                                      
         DC    C'REQZ      ',C'REQZ      '                                      
* SPOTIT                                                                        
         DC    C'SPTDIRIT  ',C'SPTDIRIT  '                                      
         DC    C'SPTFILIT  ',C'SPTFILIT  '                                      
         DC    C'XSPDIRIT  ',C'XSPDIRIT  '                                      
         DC    C'XSPFILIT  ',C'XSPFILIT  '                                      
         DC    C'STAFILIT  ',C'STAFILIT  '                                      
         DC    C'RECVIT    ',C'RECVIT    '                                      
         DC    C'REQIT     ',C'REQIT     '                                      
* SPOTI1                                                                        
         DC    C'SPTDIRI1  ',C'SPTDIRI1  '                                      
         DC    C'SPTFILI1  ',C'SPTFILI1  '                                      
         DC    C'XSPDIRI1  ',C'XSPDIRI1  '                                      
         DC    C'XSPFILI1  ',C'XSPFILI1  '                                      
         DC    C'STAFILI1  ',C'STAFILI1  '                                      
         DC    C'RECVI1    ',C'RECVI1    '                                      
         DC    C'REQI1     ',C'REQI1     '                                      
* SPOTTT                                                                        
         DC    C'SPTDIRTT  ',C'SPTDIRTT  '                                      
         DC    C'SPTFILTT  ',C'SPTFILTT  '                                      
         DC    C'XSPDIRTT  ',C'XSPDIRTT  '                                      
         DC    C'XSPFILTT  ',C'XSPFILTT  '                                      
         DC    C'STAFILTT  ',C'STAFILTT  '                                      
         DC    C'RECVTT    ',C'RECVTT    '                                      
         DC    C'REQTT     ',C'REQTT     '                                      
* SPOTTU                                                                        
         DC    C'SPTDIRTU  ',C'SPTDIRTU  '                                      
         DC    C'SPTFILTU  ',C'SPTFILTU  '                                      
         DC    C'XSPDIRTU  ',C'XSPDIRTU  '                                      
         DC    C'XSPFILTU  ',C'XSPFILTU  '                                      
         DC    C'STAFILTU  ',C'STAFILTU  '                                      
         DC    C'RECVTU    ',C'RECVTU    '                                      
         DC    C'REQTU     ',C'REQTU     '                                      
* SPOTU1                                                                        
         DC    C'SPTDIRU1  ',C'SPTDIRU1  '                                      
         DC    C'SPTFILU1  ',C'SPTFILU1  '                                      
         DC    C'XSPDIRU1  ',C'XSPDIRU1  '                                      
         DC    C'XSPFILU1  ',C'XSPFILU1  '                                      
         DC    C'STAFILU1  ',C'STAFILU1  '                                      
         DC    C'RECVU1    ',C'RECVU1    '                                      
         DC    C'REQU1     ',C'REQU1     '                                      
* SPOTU2                                                                        
         DC    C'SPTDIRU2  ',C'SPTDIRU2  '                                      
         DC    C'SPTFILU2  ',C'SPTFILU2  '                                      
         DC    C'XSPDIRU2  ',C'XSPDIRU2  '                                      
         DC    C'XSPFILU2  ',C'XSPFILU2  '                                      
         DC    C'STAFILU2  ',C'STAFILU2  '                                      
         DC    C'RECVU2    ',C'RECVU2    '                                      
         DC    C'REQU2     ',C'REQU2     '                                      
* SPOTO1                                                                        
         DC    C'SPTDIRO1  ',C'SPTDIRO1  '                                      
         DC    C'SPTFILO1  ',C'SPTFILO1  '                                      
         DC    C'XSPDIRO1  ',C'XSPDIRO1  '                                      
         DC    C'XSPFILO1  ',C'XSPFILO1  '                                      
         DC    C'STAFILO1  ',C'STAFILO1  '                                      
         DC    C'RECVO1    ',C'RECVO1    '                                      
         DC    C'REQO1     ',C'REQO1     '                                      
* SPOTY1                                                                        
         DC    C'SPTDIRY1  ',C'SPTDIRY1  '                                      
         DC    C'SPTFILY1  ',C'SPTFILY1  '                                      
         DC    C'XSPDIRY1  ',C'XSPDIRY1  '                                      
         DC    C'XSPFILY1  ',C'XSPFILY1  '                                      
         DC    C'STAFILY1  ',C'STAFILY1  '                                      
         DC    C'RECVY1    ',C'RECVY1    '                                      
         DC    C'REQY1     ',C'REQY1     '                                      
* SPOTN1                                                                        
         DC    C'SPTDIRN1  ',C'SPTDIRN1  '                                      
         DC    C'SPTFILN1  ',C'SPTFILN1  '                                      
         DC    C'XSPDIRN1  ',C'XSPDIRN1  '                                      
         DC    C'XSPFILN1  ',C'XSPFILN1  '                                      
         DC    C'STAFILN1  ',C'STAFILN1  '                                      
         DC    C'RECVN1    ',C'RECVN1    '                                      
         DC    C'REQN1     ',C'REQN1     '                                      
* SPOTN2                                                                        
         DC    C'SPTDIRN2  ',C'SPTDIRN2  '                                      
         DC    C'SPTFILN2  ',C'SPTFILN2  '                                      
         DC    C'XSPDIRN2  ',C'XSPDIRN2  '                                      
         DC    C'XSPFILN2  ',C'XSPFILN2  '                                      
         DC    C'STAFILN2  ',C'STAFILN2  '                                      
         DC    C'RECVN2    ',C'RECVN2    '                                      
         DC    C'REQN2     ',C'REQN2     '                                      
* NET1                                                                          
         DC    C'SPTDIR8   ',C'SPTDIR8   '                                      
         DC    C'SPTFIL8   ',C'SPTFIL8   '                                      
         DC    C'XSPDIR8   ',C'XSPDIR8   '                                      
         DC    C'XSPFIL8   ',C'XSPFIL8   '                                      
         DC    C'UNTDIR8   ',C'UNTDIR8   '                                      
         DC    C'UNTFIL8   ',C'UNTFIL8   '                                      
         DC    C'STAFIL8   ',C'STAFIL8   '                                      
         DC    C'RECV8     ',C'RECV8     '                                      
         DC    C'REQ8      ',C'REQ8      '                                      
* NET2                                                                          
         DC    C'SPTDIR9   ',C'SPTDIR9   '                                      
         DC    C'SPTFIL9   ',C'SPTFIL9   '                                      
         DC    C'XSPDIR9   ',C'XSPDIR9   '                                      
         DC    C'XSPFIL9   ',C'XSPFIL9   '                                      
         DC    C'UNTDIR9   ',C'UNTDIR9   '                                      
         DC    C'UNTFIL9   ',C'UNTFIL9   '                                      
         DC    C'STAFIL9   ',C'STAFIL9   '                                      
         DC    C'RECV9     ',C'RECV9     '                                      
         DC    C'REQ9      ',C'REQ9      '                                      
* NET3                                                                          
         DC    C'SPTDIRA   ',C'SPTDIRA   '                                      
         DC    C'SPTFILA   ',C'SPTFILA   '                                      
         DC    C'XSPDIRA   ',C'XSPDIRA   '                                      
         DC    C'XSPFILA   ',C'XSPFILA   '                                      
         DC    C'UNTDIRA   ',C'UNTDIRA   '                                      
         DC    C'UNTFILA   ',C'UNTFILA   '                                      
         DC    C'STAFILA   ',C'STAFILA   '                                      
         DC    C'RECVA     ',C'RECVA     '                                      
         DC    C'REQA      ',C'REQA      '                                      
* NET4                                                                          
         DC    C'SPTDIRC   ',C'SPTDIRC   '                                      
         DC    C'SPTFILC   ',C'SPTFILC   '                                      
         DC    C'XSPDIRC   ',C'XSPDIRC   '                                      
         DC    C'XSPFILC   ',C'XSPFILC   '                                      
         DC    C'UNTDIRC   ',C'UNTDIRC   '                                      
         DC    C'UNTFILC   ',C'UNTFILC   '                                      
         DC    C'STAFILC   ',C'STAFILC   '                                      
         DC    C'RECVC     ',C'RECVC     '                                      
         DC    C'REQC      ',C'REQC      '                                      
* NET5                                                                          
         DC    C'SPTDIRJ   ',C'SPTDIRJ   '                                      
         DC    C'SPTFILJ   ',C'SPTFILJ   '                                      
         DC    C'XSPDIRJ   ',C'XSPDIRJ   '                                      
         DC    C'XSPFILJ   ',C'XSPFILJ   '                                      
         DC    C'UNTDIRJ   ',C'UNTDIRJ   '                                      
         DC    C'UNTFILJ   ',C'UNTFILJ   '                                      
         DC    C'STAFILJ   ',C'STAFILJ   '                                      
         DC    C'RECVJ     ',C'RECVJ     '                                      
         DC    C'REQJ      ',C'REQJ      '                                      
* NET6                                                                          
         DC    C'SPTDIRK   ',C'SPTDIRK   '                                      
         DC    C'SPTFILK   ',C'SPTFILK   '                                      
         DC    C'XSPDIRK   ',C'XSPDIRK   '                                      
         DC    C'XSPFILK   ',C'XSPFILK   '                                      
         DC    C'UNTDIRK   ',C'UNTDIRK   '                                      
         DC    C'UNTFILK   ',C'UNTFILK   '                                      
         DC    C'STAFILK   ',C'STAFILK   '                                      
         DC    C'RECVK     ',C'RECVK     '                                      
         DC    C'REQK      ',C'REQK      '                                      
* NET7                                                                          
         DC    C'SPTDIRP   ',C'SPTDIRP   '                                      
         DC    C'SPTFILP   ',C'SPTFILP   '                                      
         DC    C'XSPDIRP   ',C'XSPDIRP   '                                      
         DC    C'XSPFILP   ',C'XSPFILP   '                                      
         DC    C'UNTDIRP   ',C'UNTDIRP   '                                      
         DC    C'UNTFILP   ',C'UNTFILP   '                                      
         DC    C'STAFILP   ',C'STAFILP   '                                      
         DC    C'RECVP     ',C'RECVP     '                                      
         DC    C'REQP      ',C'REQP      '                                      
* NETW                                                                          
         DC    C'SPTDIRW   ',C'SPTDIRW   '                                      
         DC    C'SPTFILW   ',C'SPTFILW   '                                      
         DC    C'XSPDIRW   ',C'XSPDIRW   '                                      
         DC    C'XSPFILW   ',C'XSPFILW   '                                      
         DC    C'UNTDIRW   ',C'UNTDIRW   '                                      
         DC    C'UNTFILW   ',C'UNTFILW   '                                      
         DC    C'STAFILW   ',C'STAFILW   '                                      
         DC    C'RECVW     ',C'RECVW     '                                      
         DC    C'REQW      ',C'REQW      '                                      
* NETZ                                                                          
         DC    C'SPTDIRD   ',C'SPTDIRD   '                                      
         DC    C'SPTFILD   ',C'SPTFILD   '                                      
         DC    C'XSPDIRD   ',C'XSPDIRD   '                                      
         DC    C'XSPFILD   ',C'XSPFILD   '                                      
         DC    C'UNTDIRD   ',C'UNTDIRD   '                                      
         DC    C'UNTFILD   ',C'UNTFILD   '                                      
         DC    C'STAFILD   ',C'STAFILD   '                                      
         DC    C'RECVD     ',C'RECVD     '                                      
         DC    C'REQD      ',C'REQD      '                                      
* NETIN                                                                         
         DC    C'SPTDIRIN  ',C'SPTDIRIN  '                                      
         DC    C'SPTFILIN  ',C'SPTFILIN  '                                      
         DC    C'XSPDIRIN  ',C'XSPDIRIN  '                                      
         DC    C'XSPFILIN  ',C'XSPFILIN  '                                      
         DC    C'UNTDIRIN  ',C'UNTDIRIN  '                                      
         DC    C'UNTFILIN  ',C'UNTFILIN  '                                      
         DC    C'STAFILIN  ',C'STAFILIN  '                                      
         DC    C'RECVIN    ',C'RECVIN    '                                      
         DC    C'REQIN     ',C'REQIN     '                                      
* NETTT                                                                         
         DC    C'SPTDIRTN  ',C'SPTDIRTN  '                                      
         DC    C'SPTFILTN  ',C'SPTFILTN  '                                      
         DC    C'XSPDIRTN  ',C'XSPDIRTN  '                                      
         DC    C'XSPFILTN  ',C'XSPFILTN  '                                      
         DC    C'UNTDIRTN  ',C'UNTDIRTN  '                                      
         DC    C'UNTFILTN  ',C'UNTFILTN  '                                      
         DC    C'STAFILTN  ',C'STAFILTN  '                                      
         DC    C'RECVTN    ',C'RECVTN    '                                      
         DC    C'REQTN     ',C'REQTN     '                                      
* NETTU                                                                         
         DC    C'SPTDIRTV  ',C'SPTDIRTV  '                                      
         DC    C'SPTFILTV  ',C'SPTFILTV  '                                      
         DC    C'XSPDIRTV  ',C'XSPDIRTV  '                                      
         DC    C'XSPFILTV  ',C'XSPFILTV  '                                      
         DC    C'UNTDIRTV  ',C'UNTDIRTV  '                                      
         DC    C'UNTFILTV  ',C'UNTFILTV  '                                      
         DC    C'STAFILTV  ',C'STAFILTV  '                                      
         DC    C'RECVTV    ',C'RECVTV    '                                      
         DC    C'REQTV     ',C'REQTV     '                                      
* NETI1                                                                         
         DC    C'SPTDIRIA  ',C'SPTDIRIA  '                                      
         DC    C'SPTFILIA  ',C'SPTFILIA  '                                      
         DC    C'XSPDIRIA  ',C'XSPDIRIA  '                                      
         DC    C'XSPFILIA  ',C'XSPFILIA  '                                      
         DC    C'UNTDIRIA  ',C'UNTDIRIA  '                                      
         DC    C'UNTFILIA  ',C'UNTFILIA  '                                      
         DC    C'STAFILIA  ',C'STAFILIA  '                                      
         DC    C'RECVIA    ',C'RECVIA    '                                      
         DC    C'REQIA     ',C'REQIA     '                                      
* NETO1 (SPOTOA)                                                                
         DC    C'SPTDIROA  ',C'SPTDIROA  '                                      
         DC    C'SPTFILOA  ',C'SPTFILOA  '                                      
         DC    C'XSPDIROA  ',C'XSPDIROA  '                                      
         DC    C'XSPFILOA  ',C'XSPFILOA  '                                      
         DC    C'UNTDIROA  ',C'UNTDIROA  '                                      
         DC    C'UNTFILOA  ',C'UNTFILOA  '                                      
         DC    C'STAFILOA  ',C'STAFILOA  '                                      
         DC    C'RECVOA    ',C'RECVOA    '                                      
         DC    C'REQOA     ',C'REQOA     '                                      
* NETY1 (SPOTYA)                                                                
         DC    C'SPTDIRYA  ',C'SPTDIRYA  '                                      
         DC    C'SPTFILYA  ',C'SPTFILYA  '                                      
         DC    C'XSPDIRYA  ',C'XSPDIRYA  '                                      
         DC    C'XSPFILYA  ',C'XSPFILYA  '                                      
         DC    C'UNTDIRYA  ',C'UNTDIRYA  '                                      
         DC    C'UNTFILYA  ',C'UNTFILYA  '                                      
         DC    C'STAFILYA  ',C'STAFILYA  '                                      
         DC    C'RECVYA    ',C'RECVYA    '                                      
         DC    C'REQYA     ',C'REQYA     '                                      
* NETU1 (SPOTUA)                                                                
         DC    C'SPTDIRUA  ',C'SPTDIRUA  '                                      
         DC    C'SPTFILUA  ',C'SPTFILUA  '                                      
         DC    C'XSPDIRUA  ',C'XSPDIRUA  '                                      
         DC    C'XSPFILUA  ',C'XSPFILUA  '                                      
         DC    C'UNTDIRUA  ',C'UNTDIRUA  '                                      
         DC    C'UNTFILUA  ',C'UNTFILUA  '                                      
         DC    C'STAFILUA  ',C'STAFILUA  '                                      
         DC    C'RECVUA    ',C'RECVUA    '                                      
         DC    C'REQUA     ',C'REQUA     '                                      
* NETU2 (SPOTUB)                                                                
         DC    C'SPTDIRUB  ',C'SPTDIRUB  '                                      
         DC    C'SPTFILUB  ',C'SPTFILUB  '                                      
         DC    C'XSPDIRUB  ',C'XSPDIRUB  '                                      
         DC    C'XSPFILUB  ',C'XSPFILUB  '                                      
         DC    C'UNTDIRUB  ',C'UNTDIRUB  '                                      
         DC    C'UNTFILUB  ',C'UNTFILUB  '                                      
         DC    C'STAFILUB  ',C'STAFILUB  '                                      
         DC    C'RECVUB    ',C'RECVUB    '                                      
         DC    C'REQUB     ',C'REQUB     '                                      
* NETN1 (SPOTNA)                                                                
         DC    C'SPTDIRNA  ',C'SPTDIRNA  '                                      
         DC    C'SPTFILNA  ',C'SPTFILNA  '                                      
         DC    C'XSPDIRNA  ',C'XSPDIRNA  '                                      
         DC    C'XSPFILNA  ',C'XSPFILNA  '                                      
         DC    C'UNTDIRNA  ',C'UNTDIRNA  '                                      
         DC    C'UNTFILNA  ',C'UNTFILNA  '                                      
         DC    C'STAFILNA  ',C'STAFILNA  '                                      
         DC    C'RECVNA    ',C'RECVNA    '                                      
         DC    C'REQNA     ',C'REQNA     '                                      
* NETN2 (SPOTNB)                                                                
         DC    C'SPTDIRNB  ',C'SPTDIRNB  '                                      
         DC    C'SPTFILNB  ',C'SPTFILNB  '                                      
         DC    C'XSPDIRNB  ',C'XSPDIRNB  '                                      
         DC    C'XSPFILNB  ',C'XSPFILNB  '                                      
         DC    C'UNTDIRNB  ',C'UNTDIRNB  '                                      
         DC    C'UNTFILNB  ',C'UNTFILNB  '                                      
         DC    C'STAFILNB  ',C'STAFILNB  '                                      
         DC    C'RECVNB    ',C'RECVNB    '                                      
         DC    C'REQNB     ',C'REQNB     '                                      
* NETR                                                                          
         DC    C'SPTDIRR   ',C'SPTDIRR   '                                      
         DC    C'SPTFILR   ',C'SPTFILR   '                                      
         DC    C'XSPDIRR   ',C'XSPDIRR   '                                      
         DC    C'XSPFILR   ',C'XSPFILR   '                                      
         DC    C'UNTDIRR   ',C'UNTDIRR   '                                      
         DC    C'UNTFILR   ',C'UNTFILR   '                                      
         DC    C'STAFILR   ',C'STAFILR   '                                      
         DC    C'RECVR     ',C'RECVR     '                                      
         DC    C'REQR      ',C'REQR      '                                      
* PRNT1                                                                         
         DC    C'PRTDIR1   ',C'PRTDIR1   '                                      
         DC    C'PRTFIL1   ',C'PRTFIL1   '                                      
         DC    C'PUBDIR1   ',C'PUBDIR1   '                                      
         DC    C'PUBFIL1   ',C'PUBFIL1   '                                      
         DC    C'PRECV1    ',C'PRECV1    '                                      
         DC    C'PREQ1     ',C'PREQ1     '                                      
* PRNT2                                                                         
         DC    C'PRTDIR2   ',C'PRTDIR2   '                                      
         DC    C'PRTFIL2   ',C'PRTFIL2   '                                      
         DC    C'PUBDIR2   ',C'PUBDIR2   '                                      
         DC    C'PUBFIL2   ',C'PUBFIL2   '                                      
         DC    C'PRECV2    ',C'PRECV2    '                                      
         DC    C'PREQ2     ',C'PREQ2     '                                      
* PRNT3                                                                         
         DC    C'PRTDIR3   ',C'PRTDIR3   '                                      
         DC    C'PRTFIL3   ',C'PRTFIL3   '                                      
         DC    C'PUBDIR3   ',C'PUBDIR3   '                                      
         DC    C'PUBFIL3   ',C'PUBFIL3   '                                      
         DC    C'PRECV3    ',C'PRECV3    '                                      
         DC    C'PREQ3     ',C'PREQ3     '                                      
* PRNT4                                                                         
         DC    C'PRTDIR4   ',C'PRTDIR4   '                                      
         DC    C'PRTFIL4   ',C'PRTFIL4   '                                      
         DC    C'PUBDIR4   ',C'PUBDIR4   '                                      
         DC    C'PUBFIL4   ',C'PUBFIL4   '                                      
         DC    C'PRECV4    ',C'PRECV4    '                                      
         DC    C'PREQ4     ',C'PREQ4     '                                      
* PRNT5                                                                         
         DC    C'PRTDIR5   ',C'PRTDIR5   '                                      
         DC    C'PRTFIL5   ',C'PRTFIL5   '                                      
         DC    C'PUBDIR5   ',C'PUBDIR5   '                                      
         DC    C'PUBFIL5   ',C'PUBFIL5   '                                      
         DC    C'PRECV5    ',C'PRECV5    '                                      
         DC    C'PREQ5     ',C'PREQ5     '                                      
* PRNT6                                                                         
         DC    C'PRTDIR6   ',C'PRTDIR6   '                                      
         DC    C'PRTFIL6   ',C'PRTFIL6   '                                      
         DC    C'PUBDIR6   ',C'PUBDIR6   '                                      
         DC    C'PUBFIL6   ',C'PUBFIL6   '                                      
         DC    C'PRECV6    ',C'PRECV6    '                                      
         DC    C'PREQ6     ',C'PREQ6     '                                      
* PRNT7                                                                         
         DC    C'PRTDIR7   ',C'PRTDIR7   '                                      
         DC    C'PRTFIL7   ',C'PRTFIL7   '                                      
         DC    C'PUBDIR7   ',C'PUBDIR7   '                                      
         DC    C'PUBFIL7   ',C'PUBFIL7   '                                      
         DC    C'PRECV7    ',C'PRECV7    '                                      
         DC    C'PREQ7     ',C'PREQ7     '                                      
* PRNT8                                                                         
         DC    C'PRTDIR8   ',C'PRTDIR8   '                                      
         DC    C'PRTFIL8   ',C'PRTFIL8   '                                      
         DC    C'PUBDIR8   ',C'PUBDIR8   '                                      
         DC    C'PUBFIL8   ',C'PUBFIL8   '                                      
         DC    C'PRECV8    ',C'PRECV8    '                                      
         DC    C'PREQ8     ',C'PREQ8     '                                      
* PRNT9                                                                         
         DC    C'PRTDIR9   ',C'PRTDIR9   '                                      
         DC    C'PRTFIL9   ',C'PRTFIL9   '                                      
         DC    C'PUBDIR9   ',C'PUBDIR9   '                                      
         DC    C'PUBFIL9   ',C'PUBFIL9   '                                      
         DC    C'PRECV9    ',C'PRECV9    '                                      
         DC    C'PREQ9     ',C'PREQ9     '                                      
* PRNTZ                                                                         
         DC    C'PRTDIRZ   ',C'PRTDIRZ   '                                      
         DC    C'PRTFILZ   ',C'PRTFILZ   '                                      
         DC    C'PUBDIRZ   ',C'PUBDIRZ   '                                      
         DC    C'PUBFILZ   ',C'PUBFILZ   '                                      
         DC    C'PRECVZ    ',C'PRECVZ    '                                      
         DC    C'PREQZ     ',C'PREQZ     '                                      
* PRNTIT                                                                        
         DC    C'PRTDIRIT  ',C'PRTDIRIT  '                                      
         DC    C'PRTFILIT  ',C'PRTFILIT  '                                      
         DC    C'PUBDIRIT  ',C'PUBDIRIT  '                                      
         DC    C'PUBFILIT  ',C'PUBFILIT  '                                      
         DC    C'PRECVIT   ',C'PRECVIT   '                                      
         DC    C'PREQIT    ',C'PREQIT    '                                      
* PRNTI1                                                                        
         DC    C'PRTDIRI1  ',C'PRTDIRI1  '                                      
         DC    C'PRTFILI1  ',C'PRTFILI1  '                                      
         DC    C'PUBDIRI1  ',C'PUBDIRI1  '                                      
         DC    C'PUBFILI1  ',C'PUBFILI1  '                                      
         DC    C'PRECVI1   ',C'PRECVI1   '                                      
         DC    C'PREQI1    ',C'PREQI1    '                                      
* PRNTTT                                                                        
         DC    C'PRTDIRTT  ',C'PRTDIRTT  '                                      
         DC    C'PRTFILTT  ',C'PRTFILTT  '                                      
         DC    C'PUBDIRTT  ',C'PUBDIRTT  '                                      
         DC    C'PUBFILTT  ',C'PUBFILTT  '                                      
         DC    C'PRECVTT   ',C'PRECVTT   '                                      
         DC    C'PREQTT    ',C'PREQTT    '                                      
* PRNTTU                                                                        
         DC    C'PRTDIRTU  ',C'PRTDIRTU  '                                      
         DC    C'PRTFILTU  ',C'PRTFILTU  '                                      
         DC    C'PUBDIRTU  ',C'PUBDIRTU  '                                      
         DC    C'PUBFILTU  ',C'PUBFILTU  '                                      
         DC    C'PRECVTU   ',C'PRECVTU   '                                      
         DC    C'PREQTU    ',C'PREQTU    '                                      
* PRNTU1                                                                        
         DC    C'PRTDIRU1  ',C'PRTDIRU1  '                                      
         DC    C'PRTFILU1  ',C'PRTFILU1  '                                      
         DC    C'PUBDIRU1  ',C'PUBDIRU1  '                                      
         DC    C'PUBFILU1  ',C'PUBFILU1  '                                      
         DC    C'PRECVU1   ',C'PRECVU1   '                                      
         DC    C'PREQU1    ',C'PREQU1    '                                      
* PRNTU2                                                                        
         DC    C'PRTDIRU2  ',C'PRTDIRU2  '                                      
         DC    C'PRTFILU2  ',C'PRTFILU2  '                                      
         DC    C'PUBDIRU2  ',C'PUBDIRU2  '                                      
         DC    C'PUBFILU2  ',C'PUBFILU2  '                                      
         DC    C'PRECVU2   ',C'PRECVU2   '                                      
         DC    C'PREQU2    ',C'PREQU2    '                                      
* PRNTO1                                                                        
         DC    C'PRTDIRO1  ',C'PRTDIRO1  '                                      
         DC    C'PRTFILO1  ',C'PRTFILO1  '                                      
         DC    C'PUBDIRO1  ',C'PUBDIRO1  '                                      
         DC    C'PUBFILO1  ',C'PUBFILO1  '                                      
         DC    C'PRECVO1   ',C'PRECVO1   '                                      
         DC    C'PREQO1    ',C'PREQO1    '                                      
* PRNTY1                                                                        
         DC    C'PRTDIRY1  ',C'PRTDIRY1  '                                      
         DC    C'PRTFILY1  ',C'PRTFILY1  '                                      
         DC    C'PUBDIRY1  ',C'PUBDIRY1  '                                      
         DC    C'PUBFILY1  ',C'PUBFILY1  '                                      
         DC    C'PRECVY1   ',C'PRECVY1   '                                      
         DC    C'PREQY1    ',C'PREQY1    '                                      
* PRNTN1                                                                        
         DC    C'PRTDIRN1  ',C'PRTDIRN1  '                                      
         DC    C'PRTFILN1  ',C'PRTFILN1  '                                      
         DC    C'PUBDIRN1  ',C'PUBDIRN1  '                                      
         DC    C'PUBFILN1  ',C'PUBFILN1  '                                      
         DC    C'PRECVN1   ',C'PRECVN1   '                                      
         DC    C'PREQN1    ',C'PREQN1    '                                      
* PRNTN2                                                                        
         DC    C'PRTDIRN2  ',C'PRTDIRN2  '                                      
         DC    C'PRTFILN2  ',C'PRTFILN2  '                                      
         DC    C'PUBDIRN2  ',C'PUBDIRN2  '                                      
         DC    C'PUBFILN2  ',C'PUBFILN2  '                                      
         DC    C'PRECVN2   ',C'PRECVN2   '                                      
         DC    C'PREQN2    ',C'PREQN2    '                                      
* STR1                                                                          
         DC    C'TRFDIR1   ',C'TRFDIR1   '                                      
         DC    C'TRFFIL1   ',C'TRFFIL1   '                                      
         DC    C'TRFRCV1   ',C'TRFRCV1   '                                      
         DC    C'TRFREQ1   ',C'TRFREQ1   '                                      
* STR2                                                                          
         DC    C'TRFDIR2   ',C'TRFDIR2   '                                      
         DC    C'TRFFIL2   ',C'TRFFIL2   '                                      
         DC    C'TRFRCV2   ',C'TRFRCV2   '                                      
         DC    C'TRFREQ2   ',C'TRFREQ2   '                                      
* STR3                                                                          
         DC    C'TRFDIR3   ',C'TRFDIR3   '                                      
         DC    C'TRFFIL3   ',C'TRFFIL3   '                                      
         DC    C'TRFRCV3   ',C'TRFRCV3   '                                      
         DC    C'TRFREQ3   ',C'TRFREQ3   '                                      
* STR4                                                                          
         DC    C'TRFDIR4   ',C'TRFDIR4   '                                      
         DC    C'TRFFIL4   ',C'TRFFIL4   '                                      
         DC    C'TRFRCV4   ',C'TRFRCV4   '                                      
         DC    C'TRFREQ4   ',C'TRFREQ4   '                                      
* STR5                                                                          
         DC    C'TRFDIR5   ',C'TRFDIR5   '                                      
         DC    C'TRFFIL5   ',C'TRFFIL5   '                                      
         DC    C'TRFRCV5   ',C'TRFRCV5   '                                      
         DC    C'TRFREQ5   ',C'TRFREQ5   '                                      
* STR6                                                                          
         DC    C'TRFDIR6   ',C'TRFDIR6   '                                      
         DC    C'TRFFIL6   ',C'TRFFIL6   '                                      
         DC    C'TRFRCV6   ',C'TRFRCV6   '                                      
         DC    C'TRFREQ6   ',C'TRFREQ6   '                                      
* STR7                                                                          
         DC    C'TRFDIR7   ',C'TRFDIR7   '                                      
         DC    C'TRFFIL7   ',C'TRFFIL7   '                                      
         DC    C'TRFRCV7   ',C'TRFRCV7   '                                      
         DC    C'TRFREQ7   ',C'TRFREQ7   '                                      
* STRB                                                                          
         DC    C'TRFDIRB   ',C'TRFDIRB   '                                      
         DC    C'TRFFILB   ',C'TRFFILB   '                                      
         DC    C'TRFRCVB   ',C'TRFRCVB   '                                      
         DC    C'TRFREQB   ',C'TRFREQB   '                                      
* STRE                                                                          
         DC    C'TRFDIRE   ',C'TRFDIRE   '                                      
         DC    C'TRFFILE   ',C'TRFFILE   '                                      
         DC    C'TRFRCVE   ',C'TRFRCVE   '                                      
         DC    C'TRFREQE   ',C'TRFREQE   '                                      
* STRF                                                                          
         DC    C'TRFDIRF   ',C'TRFDIRF   '                                      
         DC    C'TRFFILF   ',C'TRFFILF   '                                      
         DC    C'TRFRCVF   ',C'TRFRCVF   '                                      
         DC    C'TRFREQF   ',C'TRFREQF   '                                      
* STRG                                                                          
         DC    C'TRFDIRG   ',C'TRFDIRG   '                                      
         DC    C'TRFFILG   ',C'TRFFILG   '                                      
         DC    C'TRFRCVG   ',C'TRFRCVG   '                                      
         DC    C'TRFREQG   ',C'TRFREQG   '                                      
* STRH                                                                          
         DC    C'TRFDIRH   ',C'TRFDIRH   '                                      
         DC    C'TRFFILH   ',C'TRFFILH   '                                      
         DC    C'TRFRCVH   ',C'TRFRCVH   '                                      
         DC    C'TRFREQH   ',C'TRFREQH   '                                      
* STRL                                                                          
         DC    C'TRFDIRL   ',C'TRFDIRL   '                                      
         DC    C'TRFFILL   ',C'TRFFILL   '                                      
         DC    C'TRFRCVL   ',C'TRFRCVL   '                                      
         DC    C'TRFREQL   ',C'TRFREQL   '                                      
* STRM                                                                          
         DC    C'TRFDIRM   ',C'TRFDIRM   '                                      
         DC    C'TRFFILM   ',C'TRFFILM   '                                      
         DC    C'TRFRCVM   ',C'TRFRCVM   '                                      
         DC    C'TRFREQM   ',C'TRFREQM   '                                      
* STRN                                                                          
         DC    C'TRFDIRN   ',C'TRFDIRN   '                                      
         DC    C'TRFFILN   ',C'TRFFILN   '                                      
         DC    C'TRFRCVN   ',C'TRFRCVN   '                                      
         DC    C'TRFREQN   ',C'TRFREQN   '                                      
* STRQ                                                                          
         DC    C'TRFDIRQ   ',C'TRFDIRQ   '                                      
         DC    C'TRFFILQ   ',C'TRFFILQ   '                                      
         DC    C'TRFRCVQ   ',C'TRFRCVQ   '                                      
         DC    C'TRFREQQ   ',C'TRFREQQ   '                                      
* STRS                                                                          
         DC    C'TRFDIRS   ',C'TRFDIRS   '                                      
         DC    C'TRFFILS   ',C'TRFFILS   '                                      
         DC    C'TRFRCVS   ',C'TRFRCVS   '                                      
         DC    C'TRFREQS   ',C'TRFREQS   '                                      
* STRU                                                                          
         DC    C'TRFDIRU   ',C'TRFDIRU   '                                      
         DC    C'TRFFILU   ',C'TRFFILU   '                                      
         DC    C'TRFRCVU   ',C'TRFRCVU   '                                      
         DC    C'TRFREQU   ',C'TRFREQU   '                                      
* STRV                                                                          
         DC    C'TRFDIRV   ',C'TRFDIRV   '                                      
         DC    C'TRFFILV   ',C'TRFFILV   '                                      
         DC    C'TRFRCVV   ',C'TRFRCVV   '                                      
         DC    C'TRFREQV   ',C'TRFREQV   '                                      
* STRT                                                                          
         DC    C'TRFDIRY   ',C'TRFDIRY   '                                      
         DC    C'TRFFILY   ',C'TRFFILY   '                                      
         DC    C'TRFRCVY   ',C'TRFRCVY   '                                      
         DC    C'TRFREQY   ',C'TRFREQY   '                                      
* STRZ                                                                          
         DC    C'TRFDIRZ   ',C'TRFDIRZ   '                                      
         DC    C'TRFFILZ   ',C'TRFFILZ   '                                      
         DC    C'TRFRCVZ   ',C'TRFRCVZ   '                                      
         DC    C'TRFREQZ   ',C'TRFREQZ   '                                      
* STRIT                                                                         
         DC    C'TRFDIRIT  ',C'TRFDIRIT  '                                      
         DC    C'TRFFILIT  ',C'TRFFILIT  '                                      
         DC    C'TRFRCVIT  ',C'TRFRCVIT  '                                      
         DC    C'TRFREQIT  ',C'TRFREQIT  '                                      
* STRI1                                                                         
         DC    C'TRFDIRI1  ',C'TRFDIRI1  '                                      
         DC    C'TRFFILI1  ',C'TRFFILI1  '                                      
         DC    C'TRFRCVI1  ',C'TRFRCVI1  '                                      
         DC    C'TRFREQI1  ',C'TRFREQI1  '                                      
* STRTU                                                                         
         DC    C'TRFDIRTU  ',C'TRFDIRTU  '                                      
         DC    C'TRFFILTU  ',C'TRFFILTU  '                                      
         DC    C'TRFRCVTU  ',C'TRFRCVTU  '                                      
         DC    C'TRFREQTU  ',C'TRFREQTU  '                                      
* STRTT                                                                         
         DC    C'TRFDIRTT  ',C'TRFDIRTT  '                                      
         DC    C'TRFFILTT  ',C'TRFFILTT  '                                      
         DC    C'TRFRCVTT  ',C'TRFRCVTT  '                                      
         DC    C'TRFREQTT  ',C'TRFREQTT  '                                      
* STRU1                                                                         
         DC    C'TRFDIRU1  ',C'TRFDIRU1  '                                      
         DC    C'TRFFILU1  ',C'TRFFILU1  '                                      
         DC    C'TRFRCVU1  ',C'TRFRCVU1  '                                      
         DC    C'TRFREQU1  ',C'TRFREQU1  '                                      
* STRU2                                                                         
         DC    C'TRFDIRU2  ',C'TRFDIRU2  '                                      
         DC    C'TRFFILU2  ',C'TRFFILU2  '                                      
         DC    C'TRFRCVU2  ',C'TRFRCVU2  '                                      
         DC    C'TRFREQU2  ',C'TRFREQU2  '                                      
* STRO1                                                                         
         DC    C'TRFDIRO1  ',C'TRFDIRO1  '                                      
         DC    C'TRFFILO1  ',C'TRFFILO1  '                                      
         DC    C'TRFRCVO1  ',C'TRFRCVO1  '                                      
         DC    C'TRFREQO1  ',C'TRFREQO1  '                                      
* STRY1                                                                         
         DC    C'TRFDIRY1  ',C'TRFDIRY1  '                                      
         DC    C'TRFFILY1  ',C'TRFFILY1  '                                      
         DC    C'TRFRCVY1  ',C'TRFRCVY1  '                                      
         DC    C'TRFREQY1  ',C'TRFREQY1  '                                      
* STRN1                                                                         
         DC    C'TRFDIRN1  ',C'TRFDIRN1  '                                      
         DC    C'TRFFILN1  ',C'TRFFILN1  '                                      
         DC    C'TRFRCVN1  ',C'TRFRCVN1  '                                      
         DC    C'TRFREQN1  ',C'TRFREQN1  '                                      
* STRN2                                                                         
         DC    C'TRFDIRN2  ',C'TRFDIRN2  '                                      
         DC    C'TRFFILN2  ',C'TRFFILN2  '                                      
         DC    C'TRFRCVN2  ',C'TRFRCVN2  '                                      
         DC    C'TRFREQN2  ',C'TRFREQN2  '                                      
*                                                                               
* ALL OF TALENT IS FULL GLOBAL                                                  
* TAL1                                                                          
         DC    C'TALDIR1   ',C'TALDIR1   '                                      
         DC    C'TALFIL1   ',C'TALFIL1   '                                      
         DC    C'CHKDIR1   ',C'CHKDIR1   '                                      
         DC    C'CHKFIL1   ',C'CHKFIL1   '                                      
         DC    C'TALREQ1   ',C'TALREQ1   '                                      
         DC    C'TALRCV1   ',C'TALRCV1   '                                      
* TAL2                                                                          
         DC    C'TALDIR2   ',C'TALDIR2   '                                      
         DC    C'TALFIL2   ',C'TALFIL2   '                                      
         DC    C'CHKDIR2   ',C'CHKDIR2   '                                      
         DC    C'CHKFIL2   ',C'CHKFIL2   '                                      
         DC    C'TALREQ2   ',C'TALREQ2   '                                      
         DC    C'TALRCV2   ',C'TALRCV2   '                                      
* TAL3                                                                          
         DC    C'TALDIR3   ',C'TALDIR3   '                                      
         DC    C'TALFIL3   ',C'TALFIL3   '                                      
         DC    C'CHKDIR3   ',C'CHKDIR3   '                                      
         DC    C'CHKFIL3   ',C'CHKFIL3   '                                      
         DC    C'TALREQ3   ',C'TALREQ3   '                                      
         DC    C'TALRCV3   ',C'TALRCV3   '                                      
*                                                                               
* DON'T MAKE REP GLOBAL BECAUSE OF FORMOSA                                      
* REP1                                                                          
         DC    C'REPDIR1   ',C'REPDIR1   '                                      
         DC    C'REPFIL1   ',C'REPFIL1   '                                      
         DC    C'RREQ1     ',C'RREQ1     '                                      
         DC    C'REPRCV1   ',C'REPRCV1   '                                      
         DC    C'REPWRK1   ',C'REPWRK1   '                                      
         DC    C'RRGNEW1   ',C'RRGNEW1   '                                      
* REP3                                                                          
         DC    C'REPDIR3   ',C'REPDIR3   '                                      
         DC    C'REPFIL3   ',C'REPFIL3   '                                      
         DC    C'RREQ3     ',C'RREQ3     '                                      
         DC    C'REPRCV3   ',C'REPRCV3   '                                      
         DC    C'REPWRK3   ',C'REPWRK3   '                                      
         DC    C'RRGNEW3   ',C'RRGNEW3   '                                      
* REP4                                                                          
         DC    C'REPDIR4   ',C'REPDIR4   '                                      
         DC    C'REPFIL4   ',C'REPFIL4   '                                      
         DC    C'RREQ4     ',C'RREQ4     '                                      
         DC    C'REPRCV4   ',C'REPRCV4   '                                      
         DC    C'REPWRK4   ',C'REPWRK4   '                                      
         DC    C'RRGNEW4   ',C'RRGNEW4   '                                      
* REP5                                                                          
         DC    C'REPDIR5   ',C'REPDIR5   '                                      
         DC    C'REPFIL5   ',C'REPFIL5   '                                      
         DC    C'RREQ5     ',C'RREQ5     '                                      
         DC    C'REPRCV5   ',C'REPRCV5   '                                      
         DC    C'REPWRK5   ',C'REPWRK5   '                                      
         DC    C'RRGNEW5   ',C'RRGNEW5   '                                      
* REP7                                                                          
         DC    C'REPDIR7   ',C'REPDIR7   '                                      
         DC    C'REPFIL7   ',C'REPFIL7   '                                      
         DC    C'RREQ7     ',C'RREQ7     '                                      
         DC    C'REPRCV7   ',C'REPRCV7   '                                      
         DC    C'REPWRK7   ',C'REPWRK7   '                                      
         DC    C'RRGNEW7   ',C'RRGNEW7   '                                      
* REP8                                                                          
         DC    C'REPDIR8   ',C'REPDIR8   '                                      
         DC    C'REPFIL8   ',C'REPFIL8   '                                      
         DC    C'RREQ8     ',C'RREQ8     '                                      
         DC    C'REPRCV8   ',C'REPRCV8   '                                      
         DC    C'REPWRK8   ',C'REPWRK8   '                                      
         DC    C'RRGNEW8   ',C'RRGNEW8   '                                      
* REPY                                                                          
         DC    C'REPDIRY   ',C'REPDIRY   '                                      
         DC    C'REPFILY   ',C'REPFILY   '                                      
         DC    C'RREQY     ',C'RREQY     '                                      
         DC    C'REPRCVY   ',C'REPRCVY   '                                      
         DC    C'REPWRKY   ',C'REPWRKY   '                                      
         DC    C'RRGNEWY   ',C'RRGNEWY   '                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020FAPARMZ   12/23/20'                                      
         END                                                                    
