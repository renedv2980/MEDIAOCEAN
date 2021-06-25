*          DATA SET FAPARM2    AT LEVEL 027 AS OF 12/23/20                      
*PHASE FAPARM2A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADVERTISER II SYSTEM'                
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(5)                                                           
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
TSKPGM   DC    A(128*K)            WAS 150                                      
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
SMFRECS  DC    AL1(YES)            SMF on (See SSB)                             
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLIST DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                                                             
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'SPOT5  '                                                     
         DC    CL8'SPOTE  '                                                     
         DC    CL8'SPOTF  '                                                     
         DC    CL8'SPOTI1 '                                                     
         DC    CL8'STR5   '                                                     
         DC    CL8'STRE   '                                                     
         DC    CL8'STRF   '                                                     
         DC    CL8'STRI1  '                                                     
         DC    CL8'PRNT1  '        READ-ONLY                                    
         DC    CL8'PRNT2  '        READ-ONLY                                    
         DC    CL8'PRNT3  '        READ-ONLY                                    
         DC    CL8'PRNT4  '                                                     
         DC    CL8'PRNT5  '                                                     
         DC    CL8'PRNT6  '        READ-ONLY                                    
         DC    CL8'PRNT7  '        READ-ONLY                                    
         DC    CL8'PRNT8  '        READ-ONLY                                    
         DC    CL8'PRNT9  '        READ-ONLY                                    
         DC    CL8'PRNTI1 '                                                     
         DC    CL8'PRNTN1 '        READ-ONLY                                    
         DC    CL8'PRNTN2 '        READ-ONLY                                    
         DC    CL8'PRNTY1 '        READ-ONLY                                    
         DC    CL8'ACC1   '                                                     
         DC    CL8'ACC5   '                                                     
         DC    CL8'ACC6   '        READ-ONLY                                    
         DC    CL8'ACC7   '        READ-ONLY                                    
         DC    CL8'ACC8   '        READ-ONLY                                    
         DC    CL8'ACCB   '        READ-ONLY - OCT9/05                          
         DC    CL8'ACCC   '        READ-ONLY                                    
         DC    CL8'ACCD   '        READ-ONLY                                    
         DC    CL8'ACCV   '                                                     
         DC    CL8'ACCI1  '                                                     
         DC    CL8'ACCI3  '        READ-ONLY                                    
         DC    CL8'ACCN1  '        READ-ONLY                                    
         DC    CL8'ACCY1  '        READ-ONLY                                    
         DC    CL8'CONTROL'                                                     
         DC    CL8'NET1   '        READ-ONLY                                    
         DC    CL8'NET2   '        READ-ONLY                                    
         DC    CL8'NET3   '        READ-ONLY                                    
         DC    CL8'NET4   '        READ-ONLY                                    
         DC    CL8'NETR   '        READ-ONLY                                    
         DC    CL8'NET5   '        READ-ONLY                                    
         DC    CL8'NET6   '        READ-ONLY                                    
         DC    CL8'NET7   '        READ-ONLY                                    
         DC    CL8'NETW   '        READ-ONLY                                    
         DC    CL8'NETN1  '        READ-ONLY                                    
         DC    CL8'NETN2  '        READ-ONLY                                    
         DC    CL8'NETY1  '        READ-ONLY                                    
         DC    CL8'TAL1   '        READ-ONLY                                    
         DC    CL8'DEMO   '        Read-only                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         DC    X'0801FF'                                                        
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             Now 35000 was 10000                          
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
VTAMAPL  DC    CL8'ADV2'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'2000'             VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'700'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'192'              VTAM NUM BUFFERS  (was 128)                  
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
FAPOLING DC    AL1(YES)            DDLINK polling                               
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV2.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV2.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV2.CONTROL.ALIASQ'                                        
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
         DC    C'SPTDIR5   ',C'SPTDIR5  G'                                      
         DC    C'SPTFIL5   ',C'SPTFIL5  G'                                      
         DC    C'XSPDIR5   ',C'XSPDIR5  G'                                      
         DC    C'XSPFIL5   ',C'XSPFIL5  G'                                      
         DC    C'STAFIL5   ',C'STAFIL5  G'                                      
         DC    C'RECV5     ',C'RECV5    G'                                      
         DC    C'REQ5      ',C'REQ5     G'                                      
*                                                                               
         DC    C'SPTDIRE   ',C'SPTDIRE  G'                                      
         DC    C'SPTFILE   ',C'SPTFILE  G'                                      
         DC    C'XSPDIRE   ',C'XSPDIRE  G'                                      
         DC    C'XSPFILE   ',C'XSPFILE  G'                                      
         DC    C'STAFILE   ',C'STAFILE  G'                                      
         DC    C'RECVE     ',C'RECVE    G'                                      
         DC    C'REQE      ',C'REQE     G'                                      
*                                                                               
         DC    C'SPTDIRF   ',C'SPTDIRF   '                                      
         DC    C'SPTFILF   ',C'SPTFILF   '                                      
         DC    C'XSPDIRF   ',C'XSPDIRF   '                                      
         DC    C'XSPFILF   ',C'XSPFILF   '                                      
         DC    C'STAFILF   ',C'STAFILF   '                                      
         DC    C'RECVF     ',C'RECVF     '                                      
         DC    C'REQF      ',C'REQF      '                                      
*                                                                               
         DC    C'SPTDIRI1  ',C'SPTDIRI1  '                                      
         DC    C'SPTFILI1  ',C'SPTFILI1  '                                      
         DC    C'XSPDIRI1  ',C'XSPDIRI1  '                                      
         DC    C'XSPFILI1  ',C'XSPFILI1  '                                      
         DC    C'STAFILI1  ',C'STAFILI1  '                                      
         DC    C'RECVI1    ',C'RECVI1    '                                      
         DC    C'REQI1     ',C'REQI1     '                                      
*                                                                               
         DC    C'TRFDIR5   ',C'TRFDIR5   '                                      
         DC    C'TRFFIL5   ',C'TRFFIL5   '                                      
         DC    C'TRFRCV5   ',C'TRFRCV5   '                                      
         DC    C'TRFREQ5   ',C'TRFREQ5   '                                      
*                                                                               
         DC    C'TRFDIRE   ',C'TRFDIRE   '                                      
         DC    C'TRFFILE   ',C'TRFFILE   '                                      
         DC    C'TRFRCVE   ',C'TRFRCVE   '                                      
         DC    C'TRFREQE   ',C'TRFREQE   '                                      
*                                                                               
         DC    C'TRFDIRF   ',C'TRFDIRF   '                                      
         DC    C'TRFFILF   ',C'TRFFILF   '                                      
         DC    C'TRFRCVF   ',C'TRFRCVF   '                                      
         DC    C'TRFREQF   ',C'TRFREQF   '                                      
*                                                                               
         DC    C'TRFDIRI1  ',C'TRFDIRI1  '                                      
         DC    C'TRFFILI1  ',C'TRFFILI1  '                                      
         DC    C'TRFRCVI1  ',C'TRFRCVI1  '                                      
         DC    C'TRFREQI1  ',C'TRFREQI1  '                                      
*                                                                               
         DC    C'PRTDIR4   ',C'PRTDIR4   '                                      
         DC    C'PRTFIL4   ',C'PRTFIL4   '                                      
         DC    C'PUBDIR4   ',C'PUBDIR4   '                                      
         DC    C'PUBFIL4   ',C'PUBFIL4   '                                      
         DC    C'PRECV4    ',C'PRECV4    '                                      
         DC    C'PREQ4     ',C'PREQ4     '                                      
*                                                                               
         DC    C'PRTDIR5   ',C'PRTDIR5   '                                      
         DC    C'PRTFIL5   ',C'PRTFIL5   '                                      
         DC    C'PUBDIR5   ',C'PUBDIR5   '                                      
         DC    C'PUBFIL5   ',C'PUBFIL5   '                                      
         DC    C'PRECV5    ',C'PRECV5    '                                      
         DC    C'PREQ5     ',C'PREQ5     '                                      
*                                                                               
         DC    C'PRTDIRI1  ',C'PRTDIRI1  '                                      
         DC    C'PRTFILI1  ',C'PRTFILI1  '                                      
         DC    C'PUBDIRI1  ',C'PUBDIRI1  '                                      
         DC    C'PUBFILI1  ',C'PUBFILI1  '                                      
         DC    C'PRECVI1   ',C'PRECVI1   '                                      
         DC    C'PREQI1    ',C'PREQI1    '                                      
*                                                                               
         DC    C'ACCFIL1   ',C'ACCFIL1   '                                      
         DC    C'ACCREQ1   ',C'ACCREQ1   '                                      
         DC    C'ACCRCV1   ',C'ACCRCV1   '                                      
         DC    C'ACCDAY1   ',C'ACCDAY1   '                                      
         DC    C'ACCWRK1   ',C'ACCWRK1   '                                      
         DC    C'ACCDIR1   ',C'ACCDIR1   '                                      
         DC    C'ACCMST1   ',C'ACCMST1   '                                      
         DC    C'ACCARC1   ',C'ACCARC1   '                                      
*                                                                               
         DC    C'ACCFIL5   ',C'ACCFIL5   '                                      
         DC    C'ACCREQ5   ',C'ACCREQ5   '                                      
         DC    C'ACCRCV5   ',C'ACCRCV5   '                                      
         DC    C'ACCDAY5   ',C'ACCDAY5   '                                      
         DC    C'ACCWRK5   ',C'ACCWRK5   '                                      
         DC    C'ACCDIR5   ',C'ACCDIR5   '                                      
         DC    C'ACCMST5   ',C'ACCMST5   '                                      
         DC    C'ACCARC5   ',C'ACCARC5   '                                      
*                                                                               
         DC    C'ACCFILV   ',C'ACCFILV   '                                      
         DC    C'ACCREQV   ',C'ACCREQV   '                                      
         DC    C'ACCRCVV   ',C'ACCRCVV   '                                      
         DC    C'ACCDAYV   ',C'ACCDAYV   '                                      
         DC    C'ACCWRKV   ',C'ACCWRKV   '                                      
         DC    C'ACCDIRV   ',C'ACCDIRV   '                                      
         DC    C'ACCMSTV   ',C'ACCMSTV   '                                      
         DC    C'ACCARCV   ',C'ACCARCV   '                                      
*                                                                               
         DC    C'ACCFILI1  ',C'ACCFILI1  '                                      
         DC    C'ACCREQI1  ',C'ACCREQI1  '                                      
         DC    C'ACCRCVI1  ',C'ACCRCVI1  '                                      
         DC    C'ACCDAYI1  ',C'ACCDAYI1  '                                      
         DC    C'ACCWRKI1  ',C'ACCWRKI1  '                                      
         DC    C'ACCDIRI1  ',C'ACCDIRI1  '                                      
         DC    C'ACCMSTI1  ',C'ACCMSTI1  '                                      
         DC    C'ACCARCI1  ',C'ACCARCI1  '                                      
*                                                                               
*&&DO                                       REMOVED AS OF APR15/09              
         DC    C'CPFILE2   ',C'CPFILE   *'                                      
         DC    C'CPREQ2    ',C'CPREQ2    '                                      
         DC    C'CPRCVR2   ',C'CPRCVR2   '                                      
*                                                                               
         DC    C'MPLDIR2   ',C'MPLDIR2   '                                      
         DC    C'MPLFIL2   ',C'MPLFIL2   '                                      
         DC    C'MPLREQ2   ',C'MPLREQ2   '                                      
         DC    C'MPLRCV2   ',C'MPLRCV2   '                                      
*        DC    C'MPQDRA2   ',C'MPQDRA2   '                                      
*        DC    C'MPQFLA2   ',C'MPQFLA2   '                                      
         DC    C'BUDDIR2   ',C'BUDDIR2   '                                      
         DC    C'BUDFIL2   ',C'BUDFIL2   '                                      
*&&                                                                             
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027FAPARM2   12/23/20'                                      
         END                                                                    
