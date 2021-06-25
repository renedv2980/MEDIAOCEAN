*          DATA SET FAPARM4    AT LEVEL 025 AS OF 01/07/21                      
*PHASE FAPARM4A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADVERTISER IV SYSTEM'                
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(8)                                                           
FACORELN DC    A(125*K)                                                         
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'5'                                                             
TIMLOOP  DC    F'30'                                                            
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
SMFRECS  DC    AL1(YES)            SMF reporting (see SSB)                      
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                                                             
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'SPOT7  '                                                     
         DC    CL8'SPOTH  '                                                     
         DC    CL8'STR7   '                                                     
         DC    CL8'STRH   '                                                     
         DC    CL8'ACC1   '        READ-ONLY                                    
         DC    CL8'ACC2   '        READ-ONLY                                    
         DC    CL8'ACC3   '        READ-ONLY                                    
         DC    CL8'ACC4   '        READ-ONLY                                    
         DC    CL8'ACC5   '        READ-ONLY                                    
         DC    CL8'ACC6   '        READ-ONLY                                    
         DC    CL8'ACC7   '        READ-ONLY                                    
         DC    CL8'ACC8   '        READ-ONLY                                    
         DC    CL8'ACC9   '                                                     
         DC    CL8'ACCA   '                                                     
         DC    CL8'ACCB   '        READ-ONLY                                    
         DC    CL8'ACCC   '        READ-ONLY                                    
         DC    CL8'ACCD   '        READ-ONLY                                    
         DC    CL8'ACCQ   '        READ-ONLY                                    
         DC    CL8'ACCZ   '        READ-ONLY                                    
         DC    CL8'ACCI3  '        READ-ONLY                                    
         DC    CL8'ACCN1  '        READ-ONLY                                    
         DC    CL8'ACCN2  '        READ-ONLY                                    
         DC    CL8'ACCO1  '        READ-ONLY                                    
         DC    CL8'ACCY1  '        READ-ONLY                                    
         DC    CL8'GAMES  '                                                     
         DC    CL8'PRNT1  '        READ-ONLY                                    
         DC    CL8'PRNT2  '        READ-ONLY                                    
         DC    CL8'PRNT3  '        READ-ONLY                                    
         DC    CL8'PRNT4  '        READ-ONLY                                    
         DC    CL8'PRNT5  '        READ-ONLY                                    
         DC    CL8'PRNT6  '                                                     
         DC    CL8'PRNT7  '        READ-ONLY                                    
         DC    CL8'PRNT8  '        READ-ONLY                                    
         DC    CL8'PRNT9  '        READ-ONLY                                    
         DC    CL8'PRNTI1 '        READ-ONLY                                    
         DC    CL8'PRNTN1 '        READ-ONLY                                    
         DC    CL8'PRNTN2 '        READ-ONLY                                    
         DC    CL8'PRNTO1 '        READ-ONLY                                    
         DC    CL8'PRNTY1 '        READ-ONLY                                    
         DC    CL8'CONTROL'                                                     
         DC    CL8'NET1   '        READ ONLY                                    
         DC    CL8'NET2   '        READ ONLY                                    
         DC    CL8'NET3   '        READ ONLY                                    
         DC    CL8'NET4   '        READ ONLY                                    
         DC    CL8'NETR   '        READ ONLY                                    
         DC    CL8'NET5   '        READ ONLY                                    
         DC    CL8'NET6   '        READ ONLY                                    
         DC    CL8'NET7   '        READ ONLY                                    
         DC    CL8'NETW   '        READ ONLY                                    
         DC    CL8'NETN1  '        READ ONLY                                    
         DC    CL8'NETN2  '        READ ONLY                                    
         DC    CL8'NETO1  '        READ ONLY                                    
         DC    CL8'NETY1  '        READ ONLY                                    
         DC    CL8'TAL1   '                                                     
         DC    CL8'DEMO   '        Read-only                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             WAS 10000, NOW 35000                         
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
VTAMAPL  DC    CL8'ADV4'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'2250'    WAS 2000 VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'500'              VTAM NUM ENTRIES IN PRQ                      
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
FACSPACE DC    C'DMGADATAMGRX'     DATAMGR DATASPACE NAME                       
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
FAJESIO  DC    CL8'       '       *** MONSOON *** (WAS FAJESIO)                 
FACJOBMX DC    AL2(500)           *** MONSOON *** (WAS 50)                      
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
PGMSDSP  DC    CL12'PRGAXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((4*K*K)+32)     MINIO 4 MEGS + EYE CATCHER                   
*                                                                               
FACSMTP  DC    CL08'JESMAIL '      JES3/SMTP NAME                               
FACMQMAX DC    F'10'                                                            
FAMOFA   DC    AL1(NO)                                                          
FACMQIO  DC    CL08'FAMQIO  '                                                   
*                                                                               
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK polling                               
         DC    XL23'00'                                                         
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV4.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV4.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV4.CONTROL.ALIASQ'                                        
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
         DC    C'SPTDIR7   ',C'SPTDIR7  G'                                      
         DC    C'SPTFIL7   ',C'SPTFIL7  G'                                      
         DC    C'XSPDIR7   ',C'XSPDIR7  G'                                      
         DC    C'XSPFIL7   ',C'XSPFIL7  G'                                      
         DC    C'STAFIL7   ',C'STAFIL7  G'                                      
         DC    C'RECV7     ',C'RECV7    G'                                      
         DC    C'REQ7      ',C'REQ7     G'                                      
*                                                                               
         DC    C'TRFDIR7   ',C'TRFDIR7  G'                                      
         DC    C'TRFFIL7   ',C'TRFFIL7  G'                                      
         DC    C'TRFRCV7   ',C'TRFRCV7  G'                                      
         DC    C'TRFREQ7   ',C'TRFREQ7  G'                                      
*                                                                               
         DC    C'SPTDIRH   ',C'SPTDIRH  G'                                      
         DC    C'SPTFILH   ',C'SPTFILH  G'                                      
         DC    C'XSPDIRH   ',C'XSPDIRH  G'                                      
         DC    C'XSPFILH   ',C'XSPFILH  G'                                      
         DC    C'STAFILH   ',C'STAFILH  G'                                      
         DC    C'RECVH     ',C'RECVH    G'                                      
         DC    C'REQH      ',C'REQH     G'                                      
*                                                                               
         DC    C'TRFDIRH   ',C'TRFDIRH  G'                                      
         DC    C'TRFFILH   ',C'TRFFILH  G'                                      
         DC    C'TRFRCVH   ',C'TRFRCVH  G'                                      
         DC    C'TRFREQH   ',C'TRFREQH  G'                                      
*                                                                               
         DC    C'ACCFIL9   ',C'ACCFIL9   '                                      
         DC    C'ACCREQ9   ',C'ACCREQ9   '                                      
         DC    C'ACCRCV9   ',C'ACCRCV9   '                                      
         DC    C'ACCDAY9   ',C'ACCDAY9   '                                      
         DC    C'ACCWRK9   ',C'ACCWRK9   '                                      
         DC    C'ACCDIR9   ',C'ACCDIR9   '                                      
         DC    C'ACCMST9   ',C'ACCMST9   '                                      
         DC    C'ACCARC9   ',C'ACCARC9   '                                      
*                                                                               
         DC    C'ACCFILA   ',C'ACCFILA   '                                      
         DC    C'ACCREQA   ',C'ACCREQA   '                                      
         DC    C'ACCRCVA   ',C'ACCRCVA   '                                      
         DC    C'ACCDAYA   ',C'ACCDAYA   '                                      
         DC    C'ACCWRKA   ',C'ACCWRKA   '                                      
         DC    C'ACCDIRA   ',C'ACCDIRA   '                                      
         DC    C'ACCMSTA   ',C'ACCMSTA   '                                      
         DC    C'ACCARCA   ',C'ACCARCA   '                                      
*                                                                               
         DC    C'TALDIR1   ',C'TALDIR1   '                                      
         DC    C'TALFIL1   ',C'TALFIL1   '                                      
         DC    C'CHKDIR1   ',C'CHKDIR1   '                                      
         DC    C'CHKFIL1   ',C'CHKFIL1   '                                      
         DC    C'TALREQ1   ',C'TALREQ1   '                                      
         DC    C'TALRCV1   ',C'TALRCV1   '                                      
*                                                                               
         DC    C'PRTDIR6   ',C'PRTDIR6  G'                                      
         DC    C'PRTFIL6   ',C'PRTFIL6  G'                                      
         DC    C'PUBDIR6   ',C'PUBDIR6  G'                                      
         DC    C'PUBFIL6   ',C'PUBFIL6  G'                                      
         DC    C'PRECV6    ',C'PRECV6   G'                                      
         DC    C'PREQ6     ',C'PREQ6    G'                                      
*                                                                               
*&&DO                                       REMOVED AS OF APR15/09              
         DC    C'CPFILE4   ',C'CPFILE   *'                                      
         DC    C'CPREQ4    ',C'CPREQ4    '                                      
         DC    C'CPRCVR4   ',C'CPRCVR4   '                                      
*                                                                               
         DC    C'MPLDIR4   ',C'MPLDIR4   '                                      
         DC    C'MPLFIL4   ',C'MPLFIL4   '                                      
         DC    C'MPLREQ4   ',C'MPLREQ4   '                                      
         DC    C'MPLRCV4   ',C'MPLRCV4   '                                      
*        DC    C'MPQDRA4   ',C'MPQDRA4   '                                      
*        DC    C'MPQFLA4   ',C'MPQFLA4   '                                      
         DC    C'BUDDIR4   ',C'BUDDIR4   '                                      
         DC    C'BUDFIL4   ',C'BUDFIL4   '                                      
*&&                                                                             
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025FAPARM4   01/07/21'                                      
         END                                                                    
