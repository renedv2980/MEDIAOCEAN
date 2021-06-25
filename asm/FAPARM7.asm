*          DATA SET FAPARM7    AT LEVEL 028 AS OF 01/07/21                      
*PHASE FAPARM7A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADVERTISER VII SYSTEM'               
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(12)                                                          
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
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                                                             
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'SPOT2  '                                                     
         DC    CL8'SPOTE  '        READ-ONLY (Jan13/11)                         
         DC    CL8'SPOTM  '        READ-ONLY                                    
         DC    CL8'SPOTU1 '                                                     
         DC    CL8'SPOTU2 '                                                     
         DC    CL8'SPOTN2 '        READ-ONLY                                    
         DC    CL8'STR2   '                                                     
         DC    CL8'STRU1  '                                                     
         DC    CL8'STRU2  '                                                     
         DC    CL8'ACC4   '                                                     
         DC    CL8'ACC8   '        READ-ONLY                                    
         DC    CL8'ACC9   '        READ-ONLY                                    
         DC    CL8'ACCB   '        READ-ONLY                                    
         DC    CL8'ACCD   '                                                     
         DC    CL8'ACCI3  '                                                     
         DC    CL8'ACCU1  '                                                     
         DC    CL8'ACCU2  '                                                     
         DC    CL8'PRNT1  '                                                     
         DC    CL8'PRNT2  '        READ-ONLY                                    
         DC    CL8'PRNT3  '        READ-ONLY                                    
         DC    CL8'PRNT4  '        READ-ONLY                                    
         DC    CL8'PRNT5  '        READ-ONLY                                    
         DC    CL8'PRNT6  '        READ-ONLY                                    
         DC    CL8'PRNT7  '        READ-ONLY                                    
         DC    CL8'PRNT8  '        READ-ONLY                                    
         DC    CL8'PRNT9  '        READ-ONLY                                    
         DC    CL8'PRNTI1 '        READ-ONLY                                    
         DC    CL8'PRNTN1 '        READ-ONLY                                    
         DC    CL8'PRNTN2 '        READ-ONLY                                    
         DC    CL8'PRNTU1 '                                                     
         DC    CL8'PRNTU2 '                                                     
         DC    CL8'PRNTY1 '        READ-ONLY                                    
         DC    CL8'NET1   '        READ-ONLY                                    
         DC    CL8'NET2   '        READ-ONLY                                    
         DC    CL8'NET3   '        READ-ONLY                                    
         DC    CL8'NET4   '        READ-ONLY                                    
         DC    CL8'NETR   '        READ-ONLY                                    
         DC    CL8'NET5   '        READ-ONLY                                    
         DC    CL8'NET6   '        READ-ONLY (Jan13/11)                         
         DC    CL8'NET7   '        READ-ONLY                                    
         DC    CL8'NETW   '        READ-ONLY                                    
         DC    CL8'NETN1  '        READ-ONLY                                    
         DC    CL8'NETN2  '        READ-ONLY                                    
         DC    CL8'NETU1  '                                                     
         DC    CL8'NETU2  '                                                     
         DC    CL8'NETY1  '        READ-ONLY                                    
         DC    CL8'TAL1   '        READ-ONLY                                    
         DC    CL8'DEMO   '        Read-only                                    
         DC    CL8'CONTROL'                                                     
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
VTAMAPL  DC    CL8'ADV7'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'1500'             VTAM NUM ENTRIES IN UTL                      
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
FACXAUTL DC    AL1(YES)            BUILD UTLS IN XA (=Y)                        
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
FACSMTP  DC    CL08'JESMAIL '                                                   
FACMQMAX DC    F'08'                                                            
FAMOFA   DC    AL1(NO)                                                          
FACMQIO  DC    CL08'FAMQIO  '                                                   
*                                                                               
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK Polling                               
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV7.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV7.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV7.CONTROL.ALIASQ'                                        
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
*-----------------------------------------                                      
* SPOT2                                                                         
*-----------------------------------------                                      
         DC    C'SPTDIR2   ',C'SPTDIR2  G'                                      
         DC    C'SPTFIL2   ',C'SPTFIL2  G'                                      
         DC    C'XSPDIR2   ',C'XSPDIR2  G'                                      
         DC    C'XSPFIL2   ',C'XSPFIL2  G'                                      
         DC    C'STAFIL2   ',C'STAFIL2  G'                                      
         DC    C'RECV2     ',C'RECV2    G'                                      
         DC    C'REQ2      ',C'REQ2     G'                                      
* STR2                                                                          
         DC    C'TRFDIR2   ',C'TRFDIR2  G'                                      
         DC    C'TRFFIL2   ',C'TRFFIL2  G'                                      
         DC    C'TRFRCV2   ',C'TRFRCV2  G'                                      
         DC    C'TRFREQ2   ',C'TRFREQ2  G'                                      
*-----------------------------------------                                      
* SPOTU1                                                                        
*-----------------------------------------                                      
         DC    C'SPTDIRU1  ',C'SPTDIRU1  '                                      
         DC    C'SPTFILU1  ',C'SPTFILU1  '                                      
         DC    C'XSPDIRU1  ',C'XSPDIRU1  '                                      
         DC    C'XSPFILU1  ',C'XSPFILU1  '                                      
         DC    C'STAFILU1  ',C'STAFILU1  '                                      
         DC    C'RECVU1    ',C'RECVU1    '                                      
         DC    C'REQU1     ',C'REQU1     '                                      
* STRU1                                                                         
         DC    C'TRFDIRU1  ',C'TRFDIRU1  '                                      
         DC    C'TRFFILU1  ',C'TRFFILU1  '                                      
         DC    C'TRFRCVU1  ',C'TRFRCVU1  '                                      
         DC    C'TRFREQU1  ',C'TRFREQU1  '                                      
*-----------------------------------------                                      
* SPOTU2                                                                        
*-----------------------------------------                                      
         DC    C'SPTDIRU2  ',C'SPTDIRU2  '                                      
         DC    C'SPTFILU2  ',C'SPTFILU2  '                                      
         DC    C'XSPDIRU2  ',C'XSPDIRU2  '                                      
         DC    C'XSPFILU2  ',C'XSPFILU2  '                                      
         DC    C'STAFILU2  ',C'STAFILU2  '                                      
         DC    C'RECVU2    ',C'RECVU2    '                                      
         DC    C'REQU2     ',C'REQU2     '                                      
* STRU2                                                                         
         DC    C'TRFDIRU2  ',C'TRFDIRU2  '                                      
         DC    C'TRFFILU2  ',C'TRFFILU2  '                                      
         DC    C'TRFRCVU2  ',C'TRFRCVU2  '                                      
         DC    C'TRFREQU2  ',C'TRFREQU2  '                                      
*-----------------------------------------                                      
* NETU1 (SPOTUA)                                                                
*-----------------------------------------                                      
         DC    C'UNTDIRUA  ',C'UNTDIRUA  '                                      
         DC    C'UNTFILUA  ',C'UNTFILUA  '                                      
         DC    C'RECVUA    ',C'RECVUA    '                                      
         DC    C'REQUA     ',C'REQUA     '                                      
         DC    C'XSPDIRUA  ',C'XSPDIRUA  '                                      
         DC    C'XSPFILUA  ',C'XSPFILUA  '                                      
         DC    C'SPTDIRUA  ',C'SPTDIRUA  '                                      
         DC    C'SPTFILUA  ',C'SPTFILUA  '                                      
         DC    C'STAFILUA  ',C'STAFILUA  '                                      
*-----------------------------------------                                      
* NETU2 (SPOTUB)                                                                
*-----------------------------------------                                      
         DC    C'UNTDIRUB  ',C'UNTDIRUB  '                                      
         DC    C'UNTFILUB  ',C'UNTFILUB  '                                      
         DC    C'RECVUB    ',C'RECVUB    '                                      
         DC    C'REQUB     ',C'REQUB     '                                      
         DC    C'XSPDIRUB  ',C'XSPDIRUB  '                                      
         DC    C'XSPFILUB  ',C'XSPFILUB  '                                      
         DC    C'SPTDIRUB  ',C'SPTDIRUB  '                                      
         DC    C'SPTFILUB  ',C'SPTFILUB  '                                      
         DC    C'STAFILUB  ',C'STAFILUB  '                                      
*-----------------------------------------                                      
* PRNT1                                                                         
*-----------------------------------------                                      
         DC    C'PRTDIR1   ',C'PRTDIR1  G'                                      
         DC    C'PRTFIL1   ',C'PRTFIL1  G'                                      
         DC    C'PUBDIR1   ',C'PUBDIR1  G'                                      
         DC    C'PUBFIL1   ',C'PUBFIL1  G'                                      
         DC    C'PRECV1    ',C'PRECV1   G'                                      
         DC    C'PREQ1     ',C'PREQ1    G'                                      
*-----------------------------------------                                      
* PRNTU1                                                                        
*-----------------------------------------                                      
         DC    C'PRTDIRU1  ',C'PRTDIRU1  '                                      
         DC    C'PRTFILU1  ',C'PRTFILU1  '                                      
         DC    C'PUBDIRU1  ',C'PUBDIRU1  '                                      
         DC    C'PUBFILU1  ',C'PUBFILU1  '                                      
         DC    C'PRECVU1   ',C'PRECVU1   '                                      
         DC    C'PREQU1    ',C'PREQU1    '                                      
*-----------------------------------------                                      
* PRNTU2                                                                        
*-----------------------------------------                                      
         DC    C'PRTDIRU2  ',C'PRTDIRU2  '                                      
         DC    C'PRTFILU2  ',C'PRTFILU2  '                                      
         DC    C'PUBDIRU2  ',C'PUBDIRU2  '                                      
         DC    C'PUBFILU2  ',C'PUBFILU2  '                                      
         DC    C'PRECVU2   ',C'PRECVU2   '                                      
         DC    C'PREQU2    ',C'PREQU2    '                                      
*-----------------------------------------                                      
* ACC4                                                                          
*-----------------------------------------                                      
         DC    C'ACCFIL4   ',C'ACCFIL4   '                                      
         DC    C'ACCREQ4   ',C'ACCREQ4   '                                      
         DC    C'ACCRCV4   ',C'ACCRCV4   '                                      
         DC    C'ACCDAY4   ',C'ACCDAY4   '                                      
         DC    C'ACCWRK4   ',C'ACCWRK4   '                                      
         DC    C'ACCDIR4   ',C'ACCDIR4   '                                      
         DC    C'ACCMST4   ',C'ACCMST4   '                                      
         DC    C'ACCARC4   ',C'ACCARC4   '                                      
*-----------------------------------------                                      
* ACCD                                                                          
*-----------------------------------------                                      
         DC    C'ACCFILD   ',C'ACCFILD   '                                      
         DC    C'ACCREQD   ',C'ACCREQD   '                                      
         DC    C'ACCRCVD   ',C'ACCRCVD   '                                      
         DC    C'ACCDAYD   ',C'ACCDAYD   '                                      
         DC    C'ACCWRKD   ',C'ACCWRKD   '                                      
         DC    C'ACCDIRD   ',C'ACCDIRD   '                                      
         DC    C'ACCMSTD   ',C'ACCMSTD   '                                      
         DC    C'ACCARCD   ',C'ACCARCD   '                                      
*-----------------------------------------                                      
* ACCI3                                                                         
*-----------------------------------------                                      
         DC    C'ACCFILI3  ',C'ACCFILI3  '                                      
         DC    C'ACCREQI3  ',C'ACCREQI3  '                                      
         DC    C'ACCRCVI3  ',C'ACCRCVI3  '                                      
         DC    C'ACCDAYI3  ',C'ACCDAYI3  '                                      
         DC    C'ACCWRKI3  ',C'ACCWRKI3  '                                      
         DC    C'ACCDIRI3  ',C'ACCDIRI3  '                                      
         DC    C'ACCMSTI3  ',C'ACCMSTI3  '                                      
         DC    C'ACCARCI3  ',C'ACCARCI3  '                                      
*-----------------------------------------                                      
* ACCU1                                                                         
*-----------------------------------------                                      
         DC    C'ACCFILU1  ',C'ACCFILU1  '                                      
         DC    C'ACCREQU1  ',C'ACCREQU1  '                                      
         DC    C'ACCRCVU1  ',C'ACCRCVU1  '                                      
         DC    C'ACCDAYU1  ',C'ACCDAYU1  '                                      
         DC    C'ACCWRKU1  ',C'ACCWRKU1  '                                      
         DC    C'ACCDIRU1  ',C'ACCDIRU1  '                                      
         DC    C'ACCMSTU1  ',C'ACCMSTU1  '                                      
         DC    C'ACCARCU1  ',C'ACCARCU1  '                                      
*-----------------------------------------                                      
* ACCU2                                                                         
*-----------------------------------------                                      
         DC    C'ACCFILU2  ',C'ACCFILU2  '                                      
         DC    C'ACCREQU2  ',C'ACCREQU2  '                                      
         DC    C'ACCRCVU2  ',C'ACCRCVU2  '                                      
         DC    C'ACCDAYU2  ',C'ACCDAYU2  '                                      
         DC    C'ACCWRKU2  ',C'ACCWRKU2  '                                      
         DC    C'ACCDIRU2  ',C'ACCDIRU2  '                                      
         DC    C'ACCMSTU2  ',C'ACCMSTU2  '                                      
         DC    C'ACCARCU2  ',C'ACCARCU2  '                                      
*-----------------------------------------                                      
* CONTROL                                                                       
*-----------------------------------------                                      
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028FAPARM7   01/07/21'                                      
         END                                                                    
