*          DATA SET FAPARMM    AT LEVEL 016 AS OF 12/23/20                      
*PHASE FAPARMMA   <===                                                          
         TITLE 'FACPAK STARTUP VARIABLES - TEST SYSTEM (FACMEL)'                
***********************************************************************         
* AHYD - OCT01/12  REMOVED ACCC R/W FOR COMPANY UB RESTORE                      
***********************************************************************         
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(6)                                                           
FACORELN DC    A(160*K)            WAS A(1000*K)                                
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    AL1(YES)                                                         
FACAUTOQ DC    AL1(NO)                                                          
FACTWAS  DC    H'12'                                                            
*                                                                               
TIMTEST  DC    F'20'                                                            
TIMLOOP  DC    F'01'                                                            
TIMAUTOQ DC    F'60'                                                            
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS ***          
TSKPGM   DC    A(128*K)                                                         
TSKWRK   DC    A(128*K)                                                         
TSKTIA   DC    A(18*K)                                                          
TSKTWA   DC    A(18*K)                                                          
TSKMAP   DC    A(K+16)                                                          
*TSKTIA   DC    A(20*K)            WAS 18432                                    
*TSKTWA   DC    A(20*K)            WAS 18432                                    
*TSKMAP   DC    A(4*K)                                                          
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'FM'                                                            
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLST  DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                JUST THESE SYSTEMS STARTED                   
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'SPOT0'                                                       
         DC    CL8'STR0'                                                        
         DC    CL8'SPOTTT'                                                      
         DC    CL8'STRTT'                                                       
         DC    CL8'PRNTTT'                                                      
         DC    CL8'PRNTT'                                                       
         DC    CL8'PER1'                                                        
         DC    CL8'PER2'                                                        
         DC    CL8'ACCTT'                                                       
         DC    CL8'ACC0'                                                        
         DC    CL8'NET6'           (SPOTK)                                      
         DC    CL8'NETT'           (SPOTT)                                      
         DC    CL8'NETTT'          (SPOTTN)                                     
         DC    CL8'CONTROL'                                                     
         DC    CL8'DEMO'                                                        
SELISTQ  DC    ((80*L'SELIST)-(*-SELIST))C' '                                   
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                ALL PROGRAMS OP                              
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
         DS    0H                                                               
FACMAXIO DC    X'8888'             WAS 30000 NOW 35000                          
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
VTAMAPL  DC    CL8'MEL'            VTAM APPLICATION ID                          
VTAMUTL  DC    H'256'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'32'               VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'20'               VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'01'               TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    Y(18*K)             TEMPSTR RECORD LENGTH                        
FACTMSL  DC    Y(18*K)             TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(18*24*K)          TSAR BUFFER SIZE                             
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(2*14*K)           TRACE BUFFER LENGTH                          
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (USE NUM TSKS)             
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'7'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'16'               16K SCRUNCH XA STORAGE                       
FACSPACE DC    C'DMGTDATAMGRX'     DATAMGR DATASPACE NAME                       
*                                                                               
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    AL1(YES)            TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    AL1(YES)            NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABTXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    AL1(YES)            DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    AL1(YES)            UPDATE TABS TABLES FROM THIS FACPAK          
FACUDATE DC    X'00'                                                            
FACXAUTL DC    AL1(YES)            BUILD UTLS IN XA (=Y)                        
FACSSMXP DC    H'8'            ==> MAXIMUM PHYSICAL SESSIONS                    
FACXALEN DC    AL2(K)                                                           
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
FACMQION DC    F'2'                MQ SERIES IO CONTROL NUMBER 2=TRACE          
FACMQUTL DC    H'500'              N/U (FOR NUM ENTRIES IN UTL)                 
FACMQBUF DC    H'100'              N/U (FOR NUM BUFFERS)                        
AFACMQM  DC    A(FACMQM)           A(MQ SERIES MANAGER NAME)                    
AFACMQIN DC    A(FACMQIN)          A(MQ SERIES FACPAK INPUT Q NAME)             
AFACMQOU DC    A(FACMQOU)          A(MQ SERIES FACPAK OUTPUT Q NAME)            
AFACMQWK DC    A(FACMQWK)          A(MQ SERIES FACPAK WORK Q NAME)              
AFACMQCT DC    A(FACMQCT)          A(MQ SERIES FACPAK CONTROL Q NAME)           
DDICTLEN DC    A(0)                OVERRIDE DATA DICTIONARY LENGTH              
UKMEDDSP DC    XL12'00'            UK MEDIA DATASPACE NAME                      
PGMSDSP  DC    CL12'TST2XXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
MINAREA  DC    AL4((4*K*K)+32)     MINIO (4 MEGS)                               
FACSMTP  DC    CL8'JESMAIL'        JES3/SMTP                                    
FACMQMAX DC    F'02'               MAX MQ                                       
FAMOFA   DC    AL1(YES)            MQ FOR REP                                   
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
FACMQIN  DC    CL48'MEL.INPUT.ALIASQ'                                           
FACMQOU  DC    CL48'DDS.BROKER.TEST.LOCALQ'                                     
FACMQWK  DC    CL48'MEL.WORK.ALIASQ'                                            
FACMQCT  DC    CL48'MEL.CONTROL.ALIASQ'                                         
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
         DC    C'GENDIR    ',C'GENDIR   V'                                      
         DC    C'GENFIL    ',C'GENFIL   V'                                      
*                                                                               
         DC    C'ACCREQ0   ',C'ACCREQ0   '                                      
         DC    C'ACCRCV0   ',C'ACCRCV0   '                                      
         DC    C'ACCDAY0   ',C'ACCDAY0   '                                      
         DC    C'ACCWRK0   ',C'ACCWRK0   '                                      
         DC    C'ACCDIR0   ',C'ACCDIR0   '                                      
         DC    C'ACCMST0   ',C'ACCMST0   '                                      
         DC    C'ACCARC0   ',C'ACCARC0   '                                      
*                                                                               
         DC    C'ACCREQTT  ',C'ACCREQTT  '                                      
         DC    C'ACCRCVTT  ',C'ACCRCVTT  '                                      
         DC    C'ACCDAYTT  ',C'ACCDAYTT  '                                      
         DC    C'ACCWRKTT  ',C'ACCWRKTT  '                                      
         DC    C'ACCDIRTT  ',C'ACCDIRTT  '                                      
         DC    C'ACCMSTTT  ',C'ACCMSTTT  '                                      
         DC    C'ACCARCTT  ',C'ACCARCTT  '                                      
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
*        DC    C'TALDIR2   ',C'TALDIR2   '                                      
*        DC    C'TALFIL2   ',C'TALFIL2   '                                      
*        DC    C'CHKDIR2   ',C'CHKDIR2   '                                      
*        DC    C'CHKFIL2   ',C'CHKFIL2   '                                      
*        DC    C'TALREQ2   ',C'TALREQ2   '                                      
*        DC    C'TALRCV2   ',C'TALRCV2   '                                      
*                                                                               
* SPOT1                                                                         
*        DC    C'SPTDIR1   ',C'SPTDIR1   '                                      
*        DC    C'SPTFIL1   ',C'SPTFIL1   '                                      
*        DC    C'XSPDIR1   ',C'XSPDIR1   '                                      
*        DC    C'XSPFIL1   ',C'XSPFIL1   '                                      
*        DC    C'RECV1     ',C'RECV1     '                                      
*        DC    C'REQ1      ',C'REQ1      '                                      
*        DC    C'STAFIL1   ',C'STAFIL1   '                                      
*                                                                               
* TRAFFIC 1 (STR1)                                                              
*                                                                               
*        DC    C'TRFDIR1   ',C'TRFDIR1   '                                      
*        DC    C'TRFFIL1   ',C'TRFFIL1   '                                      
*        DC    C'TRFRCV1   ',C'TRFRCV1   '                                      
*        DC    C'TRFREQ1   ',C'TRFREQ1   '                                      
*                                                                               
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
* NETT                                                                          
*                                                                               
         DC    C'UNTDIRT   ',C'UNTDIRT  G'                                      
         DC    C'UNTFILT   ',C'UNTFILT  G'                                      
         DC    C'RECVT     ',C'RECVT    G'                                      
         DC    C'REQT      ',C'REQT     G'                                      
         DC    C'XSPDIRT   ',C'XSPDIRT  G'                                      
         DC    C'XSPFILT   ',C'XSPFILT  G'                                      
         DC    C'SPTDIRT   ',C'SPTDIRT  G'                                      
         DC    C'SPTFILT   ',C'SPTFILT  G'                                      
         DC    C'STAFILT   ',C'STAFILT  G'                                      
*                                                                               
* NETTT                                                                         
*                                                                               
         DC    C'UNTDIRTN  ',C'UNTDIRTN  '                                      
         DC    C'UNTFILTN  ',C'UNTFILTN  '                                      
         DC    C'RECVTN    ',C'RECVTN    '                                      
         DC    C'REQTN     ',C'REQTN     '                                      
         DC    C'XSPDIRTN  ',C'XSPDIRTN  '                                      
         DC    C'XSPFILTN  ',C'XSPFILTN  '                                      
         DC    C'SPTDIRTN  ',C'SPTDIRTN  '                                      
         DC    C'SPTFILTN  ',C'SPTFILTN  '                                      
         DC    C'STAFILTN  ',C'STAFILTN  '                                      
*                                                                               
* NET1                                                                          
*                                                                               
*        DC    C'UNTDIR8   ',C'UNTDIR8  G'                                      
*        DC    C'UNTFIL8   ',C'UNTFIL8  G'                                      
*        DC    C'RECV8     ',C'RECV8    G'                                      
*        DC    C'REQ8      ',C'REQ8     G'                                      
*        DC    C'XSPDIR8   ',C'XSPDIR8  G'                                      
*        DC    C'XSPFIL8   ',C'XSPFIL8  G'                                      
*        DC    C'SPTDIR8   ',C'SPTDIR8  G'                                      
*        DC    C'SPTFIL8   ',C'SPTFIL8  G'                                      
*        DC    C'STAFIL8   ',C'STAFIL8  G'                                      
*                                                                               
*                                                                               
* NET6 (FULL GLOBAL)                                                            
*                                                                               
         DC    C'UNTDIRK   ',C'UNTDIRK   '                                      
         DC    C'UNTFILK   ',C'UNTFILK   '                                      
         DC    C'RECVK     ',C'RECVK     '                                      
         DC    C'REQK      ',C'REQK      '                                      
         DC    C'XSPDIRK   ',C'XSPDIRK   '                                      
         DC    C'XSPFILK   ',C'XSPFILK   '                                      
         DC    C'SPTDIRK   ',C'SPTDIRK   '                                      
         DC    C'SPTFILK   ',C'SPTFILK   '                                      
         DC    C'STAFILK   ',C'STAFILK   '                                      
*                                                                               
         DC    C'SPTDIR0   ',C'SPTDIR0  G'                                      
         DC    C'SPTFIL0   ',C'SPTFIL0  G'                                      
         DC    C'XSPDIR0   ',C'XSPDIR0  G'                                      
         DC    C'XSPFIL0   ',C'XSPFIL0  G'                                      
         DC    C'STAFIL0   ',C'STAFIL0  G'                                      
         DC    C'RECV0     ',C'RECV0    G'                                      
         DC    C'REQ0      ',C'REQ0     G'                                      
*                                                                               
         DC    C'TRFDIR0   ',C'TRFDIR0  G'                                      
         DC    C'TRFFIL0   ',C'TRFFIL0  G'                                      
         DC    C'TRFRCV0   ',C'TRFRCV0  G'                                      
         DC    C'TRFREQ0   ',C'TRFREQ0  G'                                      
*                                                                               
*        DC    C'PRTDIR2   ',C'PRTDIR2  G'                                      
*        DC    C'PRTFIL2   ',C'PRTFIL2  G'                                      
*        DC    C'PUBDIR2   ',C'PUBDIR2  G'                                      
*        DC    C'PUBFIL2   ',C'PUBFIL2  G'                                      
*        DC    C'PRECV2    ',C'PRECV2   G'                                      
*        DC    C'PREQ2     ',C'PREQ2    G'                                      
*                                                                               
         DC    C'PRTDIRTT  ',C'PRTDIRTT  '                                      
         DC    C'PRTFILTT  ',C'PRTFILTT  '                                      
         DC    C'PUBDIRTT  ',C'PUBDIRTT  '                                      
         DC    C'PUBFILTT  ',C'PUBFILTT  '                                      
         DC    C'PRECVTT   ',C'PRECVTT   '                                      
         DC    C'PREQTT    ',C'PREQTT    '                                      
*                                                                               
         DC    C'PRTDIRT   ',C'PRTDIRT  G'                                      
         DC    C'PRTFILT   ',C'PRTFILT  G'                                      
         DC    C'PUBDIRT   ',C'PUBDIRT  G'                                      
         DC    C'PUBFILT   ',C'PUBFILT  G'                                      
         DC    C'PRECVT    ',C'PRECVT   G'                                      
         DC    C'PREQT     ',C'PREQT    G'                                      
*                                                                               
         DC    C'PERDIR1   ',C'PERDIR1  G'                                      
         DC    C'PERFIL1   ',C'PERFIL1  G'                                      
         DC    C'PERREQ1   ',C'PERREQ1  G'                                      
         DC    C'PERRCV1   ',C'PERRCV1  G'                                      
*                                                                               
         DC    C'PERDIR2   ',C'PERDIR2  G'                                      
         DC    C'PERFIL2   ',C'PERFIL2  G'                                      
         DC    C'PERREQ2   ',C'PERREQ2  G'                                      
         DC    C'PERRCV2   ',C'PERRCV2  G'                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016FAPARMM   12/23/20'                                      
         END                                                                    
