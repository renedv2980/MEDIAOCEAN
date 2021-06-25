*          DATA SET FAPARM3    AT LEVEL 117 AS OF 01/07/21                      
*PHASE FAPARM3A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADV III VTAM SYSTEM'                 
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(7)                                                           
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
DMID     DC    C'DM'               SMF - SEE SSB                                
SMFRECS  DC    AL1(YES)                                                         
*                                                                               
WDMFLST  DC    AL1(20)             WIDTH OF FILE RENAME TABLE ENTRY             
ADMFLST  DC    AL3(DMFLIST)        A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'!'                ALL SYSTEMS EXCEPT THESE STARTED             
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'ACC0    '                                                    
         DC    CL8'ACCT    '                                                    
         DC    CL8'ACCY    '                                                    
         DC    CL8'ACCOV   '                                                    
         DC    CL8'ACCTT   '                                                    
         DC    CL8'ACCTU   '                                                    
         DC    CL8'ACCU1   '                                                    
         DC    CL8'ACCU2   '                                                    
         DC    CL8'ACCZ    '                                                    
         DC    CL8'SPOT0   '                                                    
         DC    CL8'SPOTOV  '                                                    
         DC    CL8'SPOTTT  '                                                    
         DC    CL8'SPOTTU  '                                                    
         DC    CL8'SPOTU1  '                                                    
         DC    CL8'SPOTU2  '                                                    
         DC    CL8'SPOTZ   '                                                    
         DC    CL8'STR0    '                                                    
         DC    CL8'STROV   '                                                    
         DC    CL8'STRTT   '                                                    
         DC    CL8'STRTU   '                                                    
         DC    CL8'STRU1   '                                                    
         DC    CL8'STRU2   '                                                    
         DC    CL8'STRZ    '                                                    
         DC    CL8'NETT    '                                                    
         DC    CL8'NETOV   '                                                    
         DC    CL8'NETTT   '                                                    
         DC    CL8'NETTU   '                                                    
         DC    CL8'NETU1   '                                                    
         DC    CL8'NETU2   '                                                    
         DC    CL8'NETZ    '                                                    
         DC    CL8'TAL2    '                                                    
         DC    CL8'TAL3    '                                                    
         DC    CL8'PRNTT   '                                                    
         DC    CL8'PRNTOV  '                                                    
         DC    CL8'PRNTTT  '                                                    
         DC    CL8'PRNTTU  '                                                    
         DC    CL8'PRNTU1  '                                                    
         DC    CL8'PRNTU2  '                                                    
         DC    CL8'PRNTZ   '                                                    
         DC    CL8'PER1    '                                                    
         DC    CL8'PER2    '                                                    
         DC    CL8'MBA1    '                                                    
         DC    CL8'REP2    '                                                    
         DC    CL8'REP6    '                                                    
         DC    CL8'REP9    '                                                    
         DC    CL8'REPK    '       AHYD MAR15, 2012                             
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
VTAMAPL  DC    CL8'ADV3'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'1750'    WAS 1500 VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'700'              VTAM NUM ENTRIES IN PRQ                      
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
FAMOFA   DC    C'N'                                                             
FACMQIO  DC    CL08'FAMQIO  '                                                   
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK POLLING                               
*                                                                               
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV3.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV3.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV3.CONTROL.ALIASQ'                                        
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
* NET1 (SPOT8)                                                                  
******************************************                                      
         DC    C'SPTDIR8   ',C'SPTDIR8   '                                      
         DC    C'SPTFIL8   ',C'SPTFIL8   '                                      
         DC    C'XSPDIR8   ',C'XSPDIR8   '                                      
         DC    C'XSPFIL8   ',C'XSPFIL8   '                                      
         DC    C'UNTDIR8   ',C'UNTDIR8   '                                      
         DC    C'UNTFIL8   ',C'UNTFIL8   '                                      
         DC    C'STAFIL8   ',C'STAFIL8   '                                      
         DC    C'RECV8     ',C'RECV8     '                                      
         DC    C'REQ8      ',C'REQ8      '                                      
******************************************                                      
* NET2 (SPOT9)                                                                  
******************************************                                      
         DC    C'SPTDIR9   ',C'SPTDIR9   '                                      
         DC    C'SPTFIL9   ',C'SPTFIL9   '                                      
         DC    C'XSPDIR9   ',C'XSPDIR9   '                                      
         DC    C'XSPFIL9   ',C'XSPFIL9   '                                      
         DC    C'UNTDIR9   ',C'UNTDIR9   '                                      
         DC    C'UNTFIL9   ',C'UNTFIL9   '                                      
         DC    C'STAFIL9   ',C'STAFIL9   '                                      
         DC    C'RECV9     ',C'RECV9     '                                      
         DC    C'REQ9      ',C'REQ9      '                                      
******************************************                                      
* NET3 (SPOTA)                                                                  
******************************************                                      
         DC    C'SPTDIRA   ',C'SPTDIRA   '                                      
         DC    C'SPTFILA   ',C'SPTFILA   '                                      
         DC    C'XSPDIRA   ',C'XSPDIRA   '                                      
         DC    C'XSPFILA   ',C'XSPFILA   '                                      
         DC    C'UNTDIRA   ',C'UNTDIRA   '                                      
         DC    C'UNTFILA   ',C'UNTFILA   '                                      
         DC    C'STAFILA   ',C'STAFILA   '                                      
         DC    C'RECVA     ',C'RECVA     '                                      
         DC    C'REQA      ',C'REQA      '                                      
******************************************                                      
* NET7 (SPOTP)                                                                  
******************************************                                      
         DC    C'SPTDIRP   ',C'SPTDIRP   '                                      
         DC    C'SPTFILP   ',C'SPTFILP   '                                      
         DC    C'XSPDIRP   ',C'XSPDIRP   '                                      
         DC    C'XSPFILP   ',C'XSPFILP   '                                      
         DC    C'UNTDIRP   ',C'UNTDIRP   '                                      
         DC    C'UNTFILP   ',C'UNTFILP   '                                      
         DC    C'STAFILP   ',C'STAFILP   '                                      
         DC    C'RECVP     ',C'RECVP     '                                      
         DC    C'REQP      ',C'REQP      '                                      
******************************************                                      
* NET4 (SPOTC)                                                                  
******************************************                                      
         DC    C'SPTDIRC   ',C'SPTDIRC   '                                      
         DC    C'SPTFILC   ',C'SPTFILC   '                                      
         DC    C'XSPDIRC   ',C'XSPDIRC   '                                      
         DC    C'XSPFILC   ',C'XSPFILC   '                                      
         DC    C'UNTDIRC   ',C'UNTDIRC   '                                      
         DC    C'UNTFILC   ',C'UNTFILC   '                                      
         DC    C'STAFILC   ',C'STAFILC   '                                      
         DC    C'RECVC     ',C'RECVC     '                                      
         DC    C'REQC      ',C'REQC      '                                      
******************************************                                      
* NET6 (SPOTK)                                                                  
******************************************                                      
         DC    C'SPTDIRK   ',C'SPTDIRK   '                                      
         DC    C'SPTFILK   ',C'SPTFILK   '                                      
         DC    C'XSPDIRK   ',C'XSPDIRK   '                                      
         DC    C'XSPFILK   ',C'XSPFILK   '                                      
         DC    C'UNTDIRK   ',C'UNTDIRK   '                                      
         DC    C'UNTFILK   ',C'UNTFILK   '                                      
         DC    C'STAFILK   ',C'STAFILK   '                                      
         DC    C'RECVK     ',C'RECVK     '                                      
         DC    C'REQK      ',C'REQK      '                                      
******************************************                                      
* NETR (SPOTR)                                                                  
******************************************                                      
         DC    C'SPTDIRR   ',C'SPTDIRR   '                                      
         DC    C'SPTFILR   ',C'SPTFILR   '                                      
         DC    C'XSPDIRR   ',C'XSPDIRR   '                                      
         DC    C'XSPFILR   ',C'XSPFILR   '                                      
         DC    C'UNTDIRR   ',C'UNTDIRR   '                                      
         DC    C'UNTFILR   ',C'UNTFILR   '                                      
         DC    C'STAFILR   ',C'STAFILR   '                                      
         DC    C'RECVR     ',C'RECVR     '                                      
         DC    C'REQR      ',C'REQR      '                                      
******************************************                                      
* NETN1 (SPOTNA)                                                                
******************************************                                      
         DC    C'SPTDIRNA  ',C'SPTDIRNA  '                                      
         DC    C'SPTFILNA  ',C'SPTFILNA  '                                      
         DC    C'XSPDIRNA  ',C'XSPDIRNA  '                                      
         DC    C'XSPFILNA  ',C'XSPFILNA  '                                      
         DC    C'UNTDIRNA  ',C'UNTDIRNA  '                                      
         DC    C'UNTFILNA  ',C'UNTFILNA  '                                      
         DC    C'STAFILNA  ',C'STAFILNA  '                                      
         DC    C'RECVNA    ',C'RECVNA    '                                      
         DC    C'REQNA     ',C'REQNA     '                                      
******************************************                                      
* NETN2 (SPOTNB)                                                                
******************************************                                      
         DC    C'SPTDIRNB  ',C'SPTDIRNB  '                                      
         DC    C'SPTFILNB  ',C'SPTFILNB  '                                      
         DC    C'XSPDIRNB  ',C'XSPDIRNB  '                                      
         DC    C'XSPFILNB  ',C'XSPFILNB  '                                      
         DC    C'UNTDIRNB  ',C'UNTDIRNB  '                                      
         DC    C'UNTFILNB  ',C'UNTFILNB  '                                      
         DC    C'STAFILNB  ',C'STAFILNB  '                                      
         DC    C'RECVNB    ',C'RECVNB    '                                      
         DC    C'REQNB     ',C'REQNB     '                                      
******************************************                                      
* SPOTIT - STRIT                                                                
******************************************                                      
         DC    C'SPTDIRIT  ',C'SPTDIRIT  '                                      
         DC    C'SPTFILIT  ',C'SPTFILIT  '                                      
         DC    C'XSPDIRIT  ',C'XSPDIRIT  '                                      
         DC    C'XSPFILIT  ',C'XSPFILIT  '                                      
         DC    C'STAFILIT  ',C'STAFILIT  '                                      
         DC    C'RECVIT    ',C'RECVIT    '                                      
         DC    C'REQIT     ',C'REQIT     '                                      
*                                                                               
         DC    C'TRFDIRIT  ',C'TRFDIRIT  '                                      
         DC    C'TRFFILIT  ',C'TRFFILIT  '                                      
         DC    C'TRFRCVIT  ',C'TRFRCVIT  '                                      
         DC    C'TRFREQIT  ',C'TRFREQIT  '                                      
******************************************                                      
* NETIT (SPOTIN)                                                                
******************************************                                      
         DC    C'UNTDIRIN  ',C'UNTDIRIN  '                                      
         DC    C'UNTFILIN  ',C'UNTFILIN  '                                      
         DC    C'RECVIN    ',C'RECVIN    '                                      
         DC    C'REQIN     ',C'REQIN     '                                      
         DC    C'XSPDIRIN  ',C'XSPDIRIN  '                                      
         DC    C'XSPFILIN  ',C'XSPFILIN  '                                      
         DC    C'SPTDIRIN  ',C'SPTDIRIN  '                                      
         DC    C'SPTFILIN  ',C'SPTFILIN  '                                      
         DC    C'STAFILIN  ',C'STAFILIN  '                                      
******************************************                                      
* NETI1 (SPOTIA)                                                                
******************************************                                      
         DC    C'UNTDIRIA  ',C'UNTDIRIA  '                                      
         DC    C'UNTFILIA  ',C'UNTFILIA  '                                      
         DC    C'RECVIA    ',C'RECVIA    '                                      
         DC    C'REQIA     ',C'REQIA     '                                      
         DC    C'XSPDIRIA  ',C'XSPDIRIA  '                                      
         DC    C'XSPFILIA  ',C'XSPFILIA  '                                      
         DC    C'SPTDIRIA  ',C'SPTDIRIA  '                                      
         DC    C'SPTFILIA  ',C'SPTFILIA  '                                      
         DC    C'STAFILIA  ',C'STAFILIA  '                                      
******************************************                                      
* NETO1 (SPOTOA)                                                                
******************************************                                      
         DC    C'UNTDIROA  ',C'UNTDIROA  '                                      
         DC    C'UNTFILOA  ',C'UNTFILOA  '                                      
         DC    C'RECVOA    ',C'RECVOA    '                                      
         DC    C'REQOA     ',C'REQOA     '                                      
         DC    C'XSPDIROA  ',C'XSPDIROA  '                                      
         DC    C'XSPFILOA  ',C'XSPFILOA  '                                      
         DC    C'SPTDIROA  ',C'SPTDIROA  '                                      
         DC    C'SPTFILOA  ',C'SPTFILOA  '                                      
         DC    C'STAFILOA  ',C'STAFILOA  '                                      
******************************************                                      
* NETY1 (SPOTYA)                                                                
******************************************                                      
         DC    C'UNTDIRYA  ',C'UNTDIRYA  '                                      
         DC    C'UNTFILYA  ',C'UNTFILYA  '                                      
         DC    C'RECVYA    ',C'RECVYA    '                                      
         DC    C'REQYA     ',C'REQYA     '                                      
         DC    C'XSPDIRYA  ',C'XSPDIRYA  '                                      
         DC    C'XSPFILYA  ',C'XSPFILYA  '                                      
         DC    C'SPTDIRYA  ',C'SPTDIRYA  '                                      
         DC    C'SPTFILYA  ',C'SPTFILYA  '                                      
         DC    C'STAFILYA  ',C'STAFILYA  '                                      
******************************************                                      
* PRNTIT                                                                        
******************************************                                      
         DC    C'PRTDIRIT  ',C'PRTDIRIT  '                                      
         DC    C'PRTFILIT  ',C'PRTFILIT  '                                      
         DC    C'PUBDIRIT  ',C'PUBDIRIT  '                                      
         DC    C'PUBFILIT  ',C'PUBFILIT  '                                      
         DC    C'PRECVIT   ',C'PRECVIT   '                                      
         DC    C'PREQIT    ',C'PREQIT    '                                      
******************************************                                      
* ACCIT                                                                         
******************************************                                      
         DC    C'ACCREQIT  ',C'ACCREQIT  '                                      
         DC    C'ACCRCVIT  ',C'ACCRCVIT  '                                      
         DC    C'ACCDAYIT  ',C'ACCDAYIT  '                                      
         DC    C'ACCWRKIT  ',C'ACCWRKIT  '                                      
         DC    C'ACCDIRIT  ',C'ACCDIRIT  '                                      
         DC    C'ACCMSTIT  ',C'ACCMSTIT  '                                      
         DC    C'ACCARCIT  ',C'ACCARCIT  '                                      
******************************************                                      
* CONTROL                                                                       
******************************************                                      
         DC    C'CTFILE    ',C'CTFILE    '                                      
         DC    C'CTRCVR    ',C'CTRCVR    '                                      
         DC    C'CTREQ     ',C'CTREQ     '                                      
         DC    C'GENDIR    ',C'GENDIR    '                                      
         DC    C'GENFIL    ',C'GENFIL    '                                      
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117FAPARM3   01/07/21'                                      
         END                                                                    
