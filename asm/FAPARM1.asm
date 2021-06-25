*          DATA SET FAPARM1    AT LEVEL 028 AS OF 01/07/21                      
*PHASE FAPARM1A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADVERTISER I SYSTEM'                 
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                ENTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(2)                                                           
FACORELN DC    A(125*K)            WAS 16*4000                                  
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
         DC    CL8'SPOT1'                                                       
         DC    CL8'SPOT3'                                                       
         DC    CL8'SPOT4'                                                       
         DC    CL8'SPOT6'                                                       
         DC    CL8'SPOTB'                                                       
         DC    CL8'SPOTO1'                                                      
         DC    CL8'SPOTM'          READ-ONLY                                    
         DC    CL8'SPOTN1'                                                      
         DC    CL8'SPOTN2'         READ-ONLY                                    
         DC    CL8'SPOTTU'                                                      
         DC    CL8'STR1'                                                        
         DC    CL8'STR3'                                                        
         DC    CL8'STR4'                                                        
         DC    CL8'STR6'                                                        
         DC    CL8'STRB'                                                        
         DC    CL8'STRTU'                                                       
         DC    CL8'STRO1'                                                       
         DC    CL8'STRM'           READ-ONLY                                    
         DC    CL8'STRN1'                                                       
         DC    CL8'STRN2'          READ-ONLY                                    
         DC    CL8'PRNT2'                                                       
         DC    CL8'PRNT3'                                                       
         DC    CL8'PRNT4'          READ-ONLY                                    
         DC    CL8'PRNT5'          READ-ONLY                                    
         DC    CL8'PRNT6'          READ-ONLY                                    
         DC    CL8'PRNT7'          READ-ONLY                                    
         DC    CL8'PRNT8'          READ-ONLY                                    
         DC    CL8'PRNT9'          READ-ONLY                                    
         DC    CL8'PRNTI1'         READ-ONLY                                    
         DC    CL8'PRNTN1'                                                      
         DC    CL8'PRNTN2'         READ-ONLY                                    
         DC    CL8'PRNTO1'                                                      
         DC    CL8'PRNTTU'                                                      
         DC    CL8'PRNTY1'         READ-ONLY                                    
         DC    CL8'ACC2'                                                        
         DC    CL8'ACC5'           READ-ONLY                                    
         DC    CL8'ACC6'                                                        
         DC    CL8'ACC3'           READ-ONLY                                    
         DC    CL8'ACC8'           READ-ONLY                                    
         DC    CL8'ACC9'           READ-ONLY                                    
         DC    CL8'ACCA'           READ-ONLY                                    
         DC    CL8'ACCB'                                                        
         DC    CL8'ACCO1'                                                       
         DC    CL8'ACCN1'                                                       
         DC    CL8'ACCN2'          READ-ONLY                                    
         DC    CL8'ACCTU'                                                       
         DC    CL8'CONTROL'                                                     
         DC    CL8'GAMES'                                                       
         DC    CL8'NET1'           READ-ONLY                                    
         DC    CL8'NET2'           READ-ONLY                                    
         DC    CL8'NET3'           READ-ONLY                                    
         DC    CL8'NET4'           READ-ONLY                                    
         DC    CL8'NETR'           READ-ONLY                                    
         DC    CL8'NET5'           READ-ONLY                                    
         DC    CL8'NET6'           READ-ONLY                                    
         DC    CL8'NET7'           READ-ONLY                                    
         DC    CL8'NETN1'          READ-ONLY                                    
         DC    CL8'NETN2'          READ-ONLY                                    
         DC    CL8'NETO1'          READ-ONLY                                    
         DC    CL8'NETTU'                                                       
         DC    CL8'NETW'           READ-ONLY                                    
         DC    CL8'NETY1'          READ-ONLY                                    
         DC    CL8'DEMO'           Read-only                                    
         DC    CL8'TAL1'           READ-ONLY                                    
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         DC    X'080107'           REP - INCLUDE T808/T810/T811                 
         DC    X'08090D'                                                        
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             Was 10000, now 35000                         
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
FACPGMUP DC    AL1(YES)            UPDATE PRGMS FILE FROM THIS SYS              
*                                                                               
VTAMAPL  DC    CL8'ADV1'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'2400'             VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'700'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'128'              VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'10'               TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    Y(18*K)             TEMPSTR RECORD LENGTH                        
FACTMSL  DC    Y(18*K)             TEMPEST RECORD LENGTH - WAS 14336            
FACTSAR  DC    A(24*18*K)          TSAR BUFFER SIZE - WAS 12*14336              
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(2*14*K)   WAS 0   TRACE BUFFER LENGTH                          
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'7'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                SCRUNCH XA STORAGE ALLOC IN 4K'S             
FACSPACE DC    C'DMGADATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(200000)   WAS 0   SCRIPT TRACE BUFFER SIZE                     
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
FACSMTP  DC    CL08'JESMAIL '                                                   
FACMQMAX DC    F'08'                                                            
FAMOFA   DC    AL1(NO)                                                          
FACMQIO  DC    CL08'FAMQIO  '                                                   
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK polling code                          
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV1.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV1.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV1.CONTROL.ALIASQ'                                        
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
******************************************                                      
* SPOT1 - STR1                                                                  
******************************************                                      
         DC    C'SPTDIR1   ',C'SPTDIR1   '                                      
         DC    C'SPTFIL1   ',C'SPTFIL1   '                                      
         DC    C'XSPDIR1   ',C'XSPDIR1   '                                      
         DC    C'XSPFIL1   ',C'XSPFIL1   '                                      
         DC    C'STAFIL1   ',C'STAFIL1   '                                      
         DC    C'RECV1     ',C'RECV1     '                                      
         DC    C'REQ1      ',C'REQ1      '                                      
*                                                                               
         DC    C'TRFDIR1   ',C'TRFDIR1   '                                      
         DC    C'TRFFIL1   ',C'TRFFIL1   '                                      
         DC    C'TRFRCV1   ',C'TRFRCV1   '                                      
         DC    C'TRFREQ1   ',C'TRFREQ1   '                                      
******************************************                                      
* SPOT3 - STR3                                                                  
******************************************                                      
         DC    C'SPTDIR3   ',C'SPTDIR3   '                                      
         DC    C'SPTFIL3   ',C'SPTFIL3   '                                      
         DC    C'XSPDIR3   ',C'XSPDIR3   '                                      
         DC    C'XSPFIL3   ',C'XSPFIL3   '                                      
         DC    C'STAFIL3   ',C'STAFIL3   '                                      
         DC    C'RECV3     ',C'RECV3     '                                      
         DC    C'REQ3      ',C'REQ3      '                                      
*                                                                               
         DC    C'TRFDIR3   ',C'TRFDIR3   '                                      
         DC    C'TRFFIL3   ',C'TRFFIL3   '                                      
         DC    C'TRFRCV3   ',C'TRFRCV3   '                                      
         DC    C'TRFREQ3   ',C'TRFREQ3   '                                      
******************************************                                      
* SPOT4 - STR4                                                                  
******************************************                                      
         DC    C'SPTDIR4   ',C'SPTDIR4   '                                      
         DC    C'SPTFIL4   ',C'SPTFIL4   '                                      
         DC    C'XSPDIR4   ',C'XSPDIR4   '                                      
         DC    C'XSPFIL4   ',C'XSPFIL4   '                                      
         DC    C'STAFIL4   ',C'STAFIL4   '                                      
         DC    C'RECV4     ',C'RECV4     '                                      
         DC    C'REQ4      ',C'REQ4      '                                      
*                                                                               
         DC    C'TRFDIR4   ',C'TRFDIR4   '                                      
         DC    C'TRFFIL4   ',C'TRFFIL4   '                                      
         DC    C'TRFRCV4   ',C'TRFRCV4   '                                      
         DC    C'TRFREQ4   ',C'TRFREQ4   '                                      
******************************************                                      
* SPOT6 - STR6                                                                  
******************************************                                      
         DC    C'SPTDIR6   ',C'SPTDIR6   '                                      
         DC    C'SPTFIL6   ',C'SPTFIL6   '                                      
         DC    C'XSPDIR6   ',C'XSPDIR6   '                                      
         DC    C'XSPFIL6   ',C'XSPFIL6   '                                      
         DC    C'STAFIL6   ',C'STAFIL6   '                                      
         DC    C'RECV6     ',C'RECV6     '                                      
         DC    C'REQ6      ',C'REQ6      '                                      
*                                                                               
         DC    C'TRFDIR6   ',C'TRFDIR6   '                                      
         DC    C'TRFFIL6   ',C'TRFFIL6   '                                      
         DC    C'TRFRCV6   ',C'TRFRCV6   '                                      
         DC    C'TRFREQ6   ',C'TRFREQ6   '                                      
******************************************                                      
* SPOTB - STRB                                                                  
******************************************                                      
         DC    C'SPTDIRB   ',C'SPTDIRB   '                                      
         DC    C'SPTFILB   ',C'SPTFILB   '                                      
         DC    C'XSPDIRB   ',C'XSPDIRB   '                                      
         DC    C'XSPFILB   ',C'XSPFILB   '                                      
         DC    C'STAFILB   ',C'STAFILB   '                                      
         DC    C'RECVB     ',C'RECVB     '                                      
         DC    C'REQB      ',C'REQB      '                                      
*                                                                               
         DC    C'TRFDIRB   ',C'TRFDIRB   '                                      
         DC    C'TRFFILB   ',C'TRFFILB   '                                      
         DC    C'TRFRCVB   ',C'TRFRCVB   '                                      
         DC    C'TRFREQB   ',C'TRFREQB   '                                      
******************************************                                      
* SPOTTU - STRTU                                                                
******************************************                                      
         DC    C'SPTDIRTU  ',C'SPTDIRTU  '                                      
         DC    C'SPTFILTU  ',C'SPTFILTU  '                                      
         DC    C'XSPDIRTU  ',C'XSPDIRTU  '                                      
         DC    C'XSPFILTU  ',C'XSPFILTU  '                                      
         DC    C'STAFILTU  ',C'STAFILTU  '                                      
         DC    C'RECVTU    ',C'RECVTU    '                                      
         DC    C'REQTU     ',C'REQTU     '                                      
*                                                                               
         DC    C'TRFDIRTU  ',C'TRFDIRTU  '                                      
         DC    C'TRFFILTU  ',C'TRFFILTU  '                                      
         DC    C'TRFRCVTU  ',C'TRFRCVTU  '                                      
         DC    C'TRFREQTU  ',C'TRFREQTU  '                                      
******************************************                                      
* SPOTO1 - STRO1                                                                
******************************************                                      
         DC    C'SPTDIRO1  ',C'SPTDIRO1  '                                      
         DC    C'SPTFILO1  ',C'SPTFILO1  '                                      
         DC    C'XSPDIRO1  ',C'XSPDIRO1  '                                      
         DC    C'XSPFILO1  ',C'XSPFILO1  '                                      
         DC    C'STAFILO1  ',C'STAFILO1  '                                      
         DC    C'RECVO1    ',C'RECVO1    '                                      
         DC    C'REQO1     ',C'REQO1     '                                      
*                                                                               
         DC    C'TRFDIRO1  ',C'TRFDIRO1  '                                      
         DC    C'TRFFILO1  ',C'TRFFILO1  '                                      
         DC    C'TRFRCVO1  ',C'TRFRCVO1  '                                      
         DC    C'TRFREQO1  ',C'TRFREQO1  '                                      
******************************************                                      
* SPOTN1 - STRN1                                                                
******************************************                                      
         DC    C'SPTDIRN1  ',C'SPTDIRN1  '                                      
         DC    C'SPTFILN1  ',C'SPTFILN1  '                                      
         DC    C'XSPDIRN1  ',C'XSPDIRN1  '                                      
         DC    C'XSPFILN1  ',C'XSPFILN1  '                                      
         DC    C'STAFILN1  ',C'STAFILN1  '                                      
         DC    C'RECVN1    ',C'RECVN1    '                                      
         DC    C'REQN1     ',C'REQN1     '                                      
*                                                                               
         DC    C'TRFDIRN1  ',C'TRFDIRN1  '                                      
         DC    C'TRFFILN1  ',C'TRFFILN1  '                                      
         DC    C'TRFRCVN1  ',C'TRFRCVN1  '                                      
         DC    C'TRFREQN1  ',C'TRFREQN1  '                                      
******************************************                                      
* PRNT2                                                                         
******************************************                                      
         DC    C'PRTDIR2   ',C'PRTDIR2   '                                      
         DC    C'PRTFIL2   ',C'PRTFIL2   '                                      
         DC    C'PUBDIR2   ',C'PUBDIR2   '                                      
         DC    C'PUBFIL2   ',C'PUBFIL2   '                                      
         DC    C'PRECV2    ',C'PRECV2    '                                      
         DC    C'PREQ2     ',C'PREQ2     '                                      
******************************************                                      
* PRNT3                                                                         
******************************************                                      
         DC    C'PRTDIR3   ',C'PRTDIR3   '                                      
         DC    C'PRTFIL3   ',C'PRTFIL3   '                                      
         DC    C'PUBDIR3   ',C'PUBDIR3   '                                      
         DC    C'PUBFIL3   ',C'PUBFIL3   '                                      
         DC    C'PRECV3    ',C'PRECV3    '                                      
         DC    C'PREQ3     ',C'PREQ3     '                                      
******************************************                                      
* PRNTTU                                                                        
******************************************                                      
         DC    C'PRTDIRTU  ',C'PRTDIRTU  '                                      
         DC    C'PRTFILTU  ',C'PRTFILTU  '                                      
         DC    C'PUBDIRTU  ',C'PUBDIRTU  '                                      
         DC    C'PUBFILTU  ',C'PUBFILTU  '                                      
         DC    C'PRECVTU   ',C'PRECVTU   '                                      
         DC    C'PREQTU    ',C'PREQTU    '                                      
******************************************                                      
* PRNTO1                                                                        
******************************************                                      
         DC    C'PRTDIRO1  ',C'PRTDIRO1  '                                      
         DC    C'PRTFILO1  ',C'PRTFILO1  '                                      
         DC    C'PUBDIRO1  ',C'PUBDIRO1  '                                      
         DC    C'PUBFILO1  ',C'PUBFILO1  '                                      
         DC    C'PRECVO1   ',C'PRECVO1   '                                      
         DC    C'PREQO1    ',C'PREQO1    '                                      
******************************************                                      
* PRNTN1                                                                        
******************************************                                      
         DC    C'PRTDIRN1  ',C'PRTDIRN1  '                                      
         DC    C'PRTFILN1  ',C'PRTFILN1  '                                      
         DC    C'PUBDIRN1  ',C'PUBDIRN1  '                                      
         DC    C'PUBFILN1  ',C'PUBFILN1  '                                      
         DC    C'PRECVN1   ',C'PRECVN1   '                                      
         DC    C'PREQN1    ',C'PREQN1    '                                      
******************************************                                      
* ACC2                                                                          
******************************************                                      
         DC    C'ACCFIL2   ',C'ACCFIL2   '                                      
         DC    C'ACCREQ2   ',C'ACCREQ2   '                                      
         DC    C'ACCRCV2   ',C'ACCRCV2   '                                      
         DC    C'ACCDAY2   ',C'ACCDAY2   '                                      
         DC    C'ACCWRK2   ',C'ACCWRK2   '                                      
         DC    C'ACCDIR2   ',C'ACCDIR2   '                                      
         DC    C'ACCMST2   ',C'ACCMST2   '                                      
         DC    C'ACCARC2   ',C'ACCARC2   '                                      
******************************************                                      
* ACC6                                                                          
******************************************                                      
         DC    C'ACCFIL6   ',C'ACCFIL6   '                                      
         DC    C'ACCREQ6   ',C'ACCREQ6   '                                      
         DC    C'ACCRCV6   ',C'ACCRCV6   '                                      
         DC    C'ACCDAY6   ',C'ACCDAY6   '                                      
         DC    C'ACCWRK6   ',C'ACCWRK6   '                                      
         DC    C'ACCDIR6   ',C'ACCDIR6   '                                      
         DC    C'ACCMST6   ',C'ACCMST6   '                                      
         DC    C'ACCARC6   ',C'ACCARC6   '                                      
******************************************                                      
* ACCB                                                                          
******************************************                                      
         DC    C'ACCFILB   ',C'ACCFILB   '                                      
         DC    C'ACCREQB   ',C'ACCREQB   '                                      
         DC    C'ACCRCVB   ',C'ACCRCVB   '                                      
         DC    C'ACCDAYB   ',C'ACCDAYB   '                                      
         DC    C'ACCWRKB   ',C'ACCWRKB   '                                      
         DC    C'ACCDIRB   ',C'ACCDIRB   '                                      
         DC    C'ACCMSTB   ',C'ACCMSTB   '                                      
         DC    C'ACCARCB   ',C'ACCARCB   '                                      
******************************************                                      
* ACCTU                                                                         
******************************************                                      
         DC    C'ACCFILTU  ',C'ACCFILTU  '                                      
         DC    C'ACCREQTU  ',C'ACCREQTU  '                                      
         DC    C'ACCRCVTU  ',C'ACCRCVTU  '                                      
         DC    C'ACCDAYTU  ',C'ACCDAYTU  '                                      
         DC    C'ACCWRKTU  ',C'ACCWRKTU  '                                      
         DC    C'ACCDIRTU  ',C'ACCDIRTU  '                                      
         DC    C'ACCMSTTU  ',C'ACCMSTTU  '                                      
         DC    C'ACCARCTU  ',C'ACCARCTU  '                                      
******************************************                                      
* ACCO1                                                                         
******************************************                                      
         DC    C'ACCFILO1  ',C'ACCFILO1  '                                      
         DC    C'ACCREQO1  ',C'ACCREQO1  '                                      
         DC    C'ACCRCVO1  ',C'ACCRCVO1  '                                      
         DC    C'ACCDAYO1  ',C'ACCDAYO1  '                                      
         DC    C'ACCWRKO1  ',C'ACCWRKO1  '                                      
         DC    C'ACCDIRO1  ',C'ACCDIRO1  '                                      
         DC    C'ACCMSTO1  ',C'ACCMSTO1  '                                      
         DC    C'ACCARCO1  ',C'ACCARCO1  '                                      
******************************************                                      
* ACCN1                                                                         
******************************************                                      
         DC    C'ACCFILN1  ',C'ACCFILN1  '                                      
         DC    C'ACCREQN1  ',C'ACCREQN1  '                                      
         DC    C'ACCRCVN1  ',C'ACCRCVN1  '                                      
         DC    C'ACCDAYN1  ',C'ACCDAYN1  '                                      
         DC    C'ACCWRKN1  ',C'ACCWRKN1  '                                      
         DC    C'ACCDIRN1  ',C'ACCDIRN1  '                                      
         DC    C'ACCMSTN1  ',C'ACCMSTN1  '                                      
         DC    C'ACCARCN1  ',C'ACCARCN1  '                                      
******************************************                                      
* NETTU (SPOTTV)                                                                
******************************************                                      
         DC    C'SPTDIRTV  ',C'SPTDIRTV  '                                      
         DC    C'SPTFILTV  ',C'SPTFILTV  '                                      
         DC    C'XSPDIRTV  ',C'XSPDIRTV  '                                      
         DC    C'XSPFILTV  ',C'XSPFILTV  '                                      
         DC    C'UNTDIRTV  ',C'UNTDIRTV  '                                      
         DC    C'UNTFILTV  ',C'UNTFILTV  '                                      
         DC    C'STAFILTV  ',C'STAFILTV  '                                      
         DC    C'RECVTV    ',C'RECVTV    '                                      
         DC    C'REQTV     ',C'REQTV     '                                      
******************************************                                      
* PER1                                                                          
******************************************                                      
         DC    C'PERDIR1   ',C'PERDIR1   '                                      
         DC    C'PERFIL1   ',C'PERFIL1   '                                      
         DC    C'PERREQ1   ',C'PERREQ1   '                                      
         DC    C'PERRCV1   ',C'PERRCV1   '                                      
******************************************                                      
* CONTROL                                                                       
******************************************                                      
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028FAPARM1   01/07/21'                                      
         END                                                                    
