*          DATA SET FAPARM5    AT LEVEL 025 AS OF 12/23/20                      
*PHASE FAPARM5A                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - ADVERTISER V SYSTEM'                 
FAPARMS  CSECT                                                                  
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(3)                                                           
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
         DC    CL8'ACC1   '        READ-ONLY                                    
         DC    CL8'ACC2   '        READ-ONLY                                    
         DC    CL8'ACC3   '        READ-ONLY                                    
         DC    CL8'ACC4   '        READ-ONLY                                    
         DC    CL8'ACC5   '        READ-ONLY                                    
         DC    CL8'ACC6   '        READ-ONLY                                    
         DC    CL8'ACC7   '                                                     
         DC    CL8'ACC8   '                                                     
         DC    CL8'ACC9   '        READ-ONLY                                    
         DC    CL8'ACCA   '        READ-ONLY                                    
         DC    CL8'ACCB   '        READ-ONLY                                    
         DC    CL8'ACCC   '        READ-ONLY                                    
         DC    CL8'ACCQ   '        R/W FROM JULY 08/2006                        
         DC    CL8'ACCZ   '        R/W as of Feb14,2013                         
         DC    CL8'ACCY1  '                                                     
         DC    CL8'ACCN1  '        READ-ONLY                                    
         DC    CL8'ACCN2  '        READ-ONLY                                    
         DC    CL8'TAL1   '        READ-ONLY                                    
         DC    CL8'GAMES  '                                                     
         DC    CL8'SPOTG  '                                                     
         DC    CL8'SPOTL  '                                                     
         DC    CL8'SPOTZ  '        R/W as of Feb14,2013                         
         DC    CL8'SPOTY1 '                                                     
         DC    CL8'STRG   '                                                     
         DC    CL8'STRL   '                                                     
         DC    CL8'STRZ   '        R/W as of Feb14,2013                         
         DC    CL8'STRY1  '                                                     
         DC    CL8'NET2   '        READ-ONLY                                    
         DC    CL8'NET3   '        READ-ONLY                                    
         DC    CL8'NET7   '        READ-ONLY                                    
         DC    CL8'NETW   '                                                     
         DC    CL8'NETZ   '                                                     
         DC    CL8'NETY1  '        READ-ONLY                                    
         DC    CL8'DEMO   '        Read-only                                    
         DC    CL8'PRNT1  '        READ-ONLY                                    
         DC    CL8'PRNT2  '        READ-ONLY                                    
         DC    CL8'PRNT3  '        READ-ONLY                                    
         DC    CL8'PRNT4  '        READ-ONLY                                    
         DC    CL8'PRNT5  '        READ-ONLY                                    
         DC    CL8'PRNT6  '        READ-ONLY                                    
         DC    CL8'PRNT7  '        READ-ONLY                                    
         DC    CL8'PRNT8  '                                                     
         DC    CL8'PRNT9  '        READ-ONLY                                    
         DC    CL8'PRNTZ  '        R/W as of Feb14,2013                         
         DC    CL8'PRNTI1 '        READ-ONLY                                    
         DC    CL8'PRNTN1 '        READ-ONLY                                    
         DC    CL8'PRNTN2 '        READ-ONLY                                    
         DC    CL8'PRNTY1 '                                                     
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
VTAMAPL  DC    CL8'ADV5'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'2500'             VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'500'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'200'              VTAM NUM BUFFERS                             
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
FAGLOBAL DC    AL1(NO)             GET DSN NAMES FROM DATASPACE                 
FABULKUP DC    AL1(YES)            FALINK BULK UPLOAD                           
FAPOLING DC    AL1(YES)            DDLINK polling code                          
         DC    XL23'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48' '                                                          
FACMQIN  DC    CL48'ADV5.INPUT.ALIASQ'                                          
FACMQOU  DC    CL48'DDS.BROKER.LOCALQ'                                          
FACMQWK  DC    CL48'ADV5.WORK.ALIASQ'                                           
FACMQCT  DC    CL48'ADV5.CONTROL.ALIASQ'                                        
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
* ACC7                                                                          
******************************************                                      
         DC    C'ACCFIL7   ',C'ACCFIL7   '                                      
         DC    C'ACCREQ7   ',C'ACCREQ7   '                                      
         DC    C'ACCRCV7   ',C'ACCRCV7   '                                      
         DC    C'ACCDAY7   ',C'ACCDAY7   '                                      
         DC    C'ACCWRK7   ',C'ACCWRK7   '                                      
         DC    C'ACCDIR7   ',C'ACCDIR7   '                                      
         DC    C'ACCMST7   ',C'ACCMST7   '                                      
         DC    C'ACCARC7   ',C'ACCARC7   '                                      
******************************************                                      
* ACCQ                                                                          
******************************************                                      
         DC    C'ACCFILQ   ',C'ACCFILQ   '                                      
         DC    C'ACCREQQ   ',C'ACCREQQ   '                                      
         DC    C'ACCRCVQ   ',C'ACCRCVQ   '                                      
         DC    C'ACCDAYQ   ',C'ACCDAYQ   '                                      
         DC    C'ACCWRKQ   ',C'ACCWRKQ   '                                      
         DC    C'ACCDIRQ   ',C'ACCDIRQ   '                                      
         DC    C'ACCMSTQ   ',C'ACCMSTQ   '                                      
         DC    C'ACCARCQ   ',C'ACCARCQ   '                                      
******************************************                                      
* ACCY                                                                          
******************************************                                      
         DC    C'ACCFILY   ',C'ACCFILY   '                                      
         DC    C'ACCREQY   ',C'ACCREQY   '                                      
         DC    C'ACCRCVY   ',C'ACCRCVY   '                                      
         DC    C'ACCDAYY   ',C'ACCDAYY   '                                      
         DC    C'ACCWRKY   ',C'ACCWRKY   '                                      
         DC    C'ACCDIRY   ',C'ACCDIRY   '                                      
         DC    C'ACCMSTY   ',C'ACCMSTY   '                                      
         DC    C'ACCARCY   ',C'ACCARCY   '                                      
******************************************                                      
* ACCZ                                                                          
******************************************                                      
         DC    C'ACCFILZ   ',C'ACCFILZ   '                                      
         DC    C'ACCREQZ   ',C'ACCREQZ   '                                      
         DC    C'ACCRCVZ   ',C'ACCRCVZ   '                                      
         DC    C'ACCDAYZ   ',C'ACCDAYZ   '                                      
         DC    C'ACCWRKZ   ',C'ACCWRKZ   '                                      
         DC    C'ACCDIRZ   ',C'ACCDIRZ   '                                      
         DC    C'ACCMSTZ   ',C'ACCMSTZ   '                                      
         DC    C'ACCARCZ   ',C'ACCARCZ   '                                      
******************************************                                      
* ACC8                                                                          
******************************************                                      
         DC    C'ACCFIL8   ',C'ACCFIL8   '                                      
         DC    C'ACCREQ8   ',C'ACCREQ8   '                                      
         DC    C'ACCRCV8   ',C'ACCRCV8   '                                      
         DC    C'ACCDAY8   ',C'ACCDAY8   '                                      
         DC    C'ACCWRK8   ',C'ACCWRK8   '                                      
         DC    C'ACCDIR8   ',C'ACCDIR8   '                                      
         DC    C'ACCMST8   ',C'ACCMST8   '                                      
         DC    C'ACCARC8   ',C'ACCARC8   '                                      
******************************************                                      
* ACCY1                                                                         
******************************************                                      
         DC    C'ACCFILY1  ',C'ACCFILY1  '                                      
         DC    C'ACCREQY1  ',C'ACCREQY1  '                                      
         DC    C'ACCRCVY1  ',C'ACCRCVY1  '                                      
         DC    C'ACCDAYY1  ',C'ACCDAYY1  '                                      
         DC    C'ACCWRKY1  ',C'ACCWRKY1  '                                      
         DC    C'ACCDIRY1  ',C'ACCDIRY1  '                                      
         DC    C'ACCMSTY1  ',C'ACCMSTY1  '                                      
         DC    C'ACCARCY1  ',C'ACCARCY1  '                                      
******************************************                                      
* SPOTG                                                                         
******************************************                                      
         DC    C'SPTDIRG   ',C'SPTDIRG   '                                      
         DC    C'SPTFILG   ',C'SPTFILG   '                                      
         DC    C'XSPDIRG   ',C'XSPDIRG   '                                      
         DC    C'XSPFILG   ',C'XSPFILG   '                                      
         DC    C'STAFILG   ',C'STAFILG   '                                      
         DC    C'RECVG     ',C'RECVG     '                                      
         DC    C'REQG      ',C'REQG      '                                      
*                                                                               
         DC    C'TRFDIRG   ',C'TRFDIRG   '                                      
         DC    C'TRFFILG   ',C'TRFFILG   '                                      
         DC    C'TRFRCVG   ',C'TRFRCVG   '                                      
         DC    C'TRFREQG   ',C'TRFREQG   '                                      
******************************************                                      
* SPOTL                                                                         
******************************************                                      
         DC    C'SPTDIRL   ',C'SPTDIRL   '                                      
         DC    C'SPTFILL   ',C'SPTFILL   '                                      
         DC    C'XSPDIRL   ',C'XSPDIRL   '                                      
         DC    C'XSPFILL   ',C'XSPFILL   '                                      
         DC    C'STAFILL   ',C'STAFILL   '                                      
         DC    C'RECVL     ',C'RECVL     '                                      
         DC    C'REQL      ',C'REQL      '                                      
*                                                                               
         DC    C'TRFDIRL   ',C'TRFDIRL   '                                      
         DC    C'TRFFILL   ',C'TRFFILL   '                                      
         DC    C'TRFRCVL   ',C'TRFRCVL   '                                      
         DC    C'TRFREQL   ',C'TRFREQL   '                                      
******************************************                                      
* SPOTY1                                                                        
******************************************                                      
         DC    C'SPTDIRY1  ',C'SPTDIRY1  '                                      
         DC    C'SPTFILY1  ',C'SPTFILY1  '                                      
         DC    C'XSPDIRY1  ',C'XSPDIRY1  '                                      
         DC    C'XSPFILY1  ',C'XSPFILY1  '                                      
         DC    C'STAFILY1  ',C'STAFILY1  '                                      
         DC    C'RECVY1    ',C'RECVY1    '                                      
         DC    C'REQY1     ',C'REQY1     '                                      
*                                                                               
         DC    C'TRFDIRY1  ',C'TRFDIRY1  '                                      
         DC    C'TRFFILY1  ',C'TRFFILY1  '                                      
         DC    C'TRFRCVY1  ',C'TRFRCVY1  '                                      
         DC    C'TRFREQY1  ',C'TRFREQY1  '                                      
******************************************                                      
* NETW                                                                          
******************************************                                      
         DC    C'SPTDIRW   ',C'SPTDIRW   '                                      
         DC    C'SPTFILW   ',C'SPTFILW   '                                      
         DC    C'XSPDIRW   ',C'XSPDIRW   '                                      
         DC    C'XSPFILW   ',C'XSPFILW   '                                      
         DC    C'UNTDIRW   ',C'UNTDIRW   '                                      
         DC    C'UNTFILW   ',C'UNTFILW   '                                      
         DC    C'STAFILW   ',C'STAFILW   '                                      
         DC    C'RECVW     ',C'RECVW     '                                      
         DC    C'REQW      ',C'REQW      '                                      
******************************************                                      
* NETZ                                                                          
******************************************                                      
         DC    C'SPTDIRD   ',C'SPTDIRD   '                                      
         DC    C'SPTFILD   ',C'SPTFILD   '                                      
         DC    C'XSPDIRD   ',C'XSPDIRD   '                                      
         DC    C'XSPFILD   ',C'XSPFILD   '                                      
         DC    C'UNTDIRD   ',C'UNTDIRD   '                                      
         DC    C'UNTFILD   ',C'UNTFILD   '                                      
         DC    C'STAFILD   ',C'STAFILD   '                                      
         DC    C'RECVD     ',C'RECVD     '                                      
         DC    C'REQD      ',C'REQD      '                                      
******************************************                                      
* SPOTZ                                                                         
******************************************                                      
         DC    C'SPTDIRZ   ',C'SPTDIRZ   '                                      
         DC    C'SPTFILZ   ',C'SPTFILZ   '                                      
         DC    C'XSPDIRZ   ',C'XSPDIRZ   '                                      
         DC    C'XSPFILZ   ',C'XSPFILZ   '                                      
         DC    C'STAFILZ   ',C'STAFILZ   '                                      
         DC    C'RECVZ     ',C'RECVZ     '                                      
         DC    C'REQZ      ',C'REQZ      '                                      
*                                                                               
         DC    C'TRFDIRZ   ',C'TRFDIRZ   '                                      
         DC    C'TRFFILZ   ',C'TRFFILZ   '                                      
         DC    C'TRFRCVZ   ',C'TRFRCVZ   '                                      
         DC    C'TRFREQZ   ',C'TRFREQZ   '                                      
******************************************                                      
* PRNT8                                                                         
******************************************                                      
         DC    C'PRTDIR8   ',C'PRTDIR8   '                                      
         DC    C'PRTFIL8   ',C'PRTFIL8   '                                      
         DC    C'PUBDIR8   ',C'PUBDIR8   '                                      
         DC    C'PUBFIL8   ',C'PUBFIL8   '                                      
         DC    C'PRECV8    ',C'PRECV8    '                                      
         DC    C'PREQ8     ',C'PREQ8     '                                      
******************************************                                      
* PRNTZ                                                                         
******************************************                                      
         DC    C'PRTDIRZ   ',C'PRTDIRZ   '                                      
         DC    C'PRTFILZ   ',C'PRTFILZ   '                                      
         DC    C'PUBDIRZ   ',C'PUBDIRZ   '                                      
         DC    C'PUBFILZ   ',C'PUBFILZ   '                                      
         DC    C'PRECVZ    ',C'PRECVZ    '                                      
         DC    C'PREQZ     ',C'PREQZ     '                                      
******************************************                                      
* PRNTY1                                                                        
******************************************                                      
         DC    C'PRTDIRY1  ',C'PRTDIRY1  '                                      
         DC    C'PRTFILY1  ',C'PRTFILY1  '                                      
         DC    C'PUBDIRY1  ',C'PUBDIRY1  '                                      
         DC    C'PUBFILY1  ',C'PUBFILY1  '                                      
         DC    C'PRECVY1   ',C'PRECVY1   '                                      
         DC    C'PREQY1    ',C'PREQY1    '                                      
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
**PAN#1  DC    CL21'025FAPARM5   12/23/20'                                      
         END                                                                    
