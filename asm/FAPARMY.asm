*          DATA SET FAPARMY    AT LEVEL 062 AS OF 09/28/00                      
*PHASE FAPARMYA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - Y2K SYSTEM'                          
         SPACE 2                                                                
FAPARMS  CSECT                                                                  
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                EXTENDED ADRFILE RECORDS                     
FACSYSID DC    AL1(15)                                                          
FACORELN DC    A(32*4000)                                                       
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    C'Y'                                                             
FACAUTOQ DC    C'N'                                                             
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'5'                                                             
TIMLOOP  DC    F'30'                                                            
TIMAUTOQ DC    F'120'                                                           
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS ***          
TSKPGM   DC    A(57000)            WAS 56000                                    
TSKWRK   DC    A(84000)            WAS 80000                                    
TSKTIA   DC    A(18432)                                                         
TSKTWA   DC    A(18432)                                                         
TSKMAP   DC    A(1040)                                                          
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'DM'                                                            
         DS    C                                                                
*                                                                               
ADMFLIST DC    A(DMFLIST)          A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                                                             
SELIST   DC    60CL8' '                                                         
         ORG   SELIST                                                           
*********DC    CL8'SPOT1'                                                       
*********DC    CL8'STR1'                                                        
*********DC    CL8'SPOTH'                                                       
*********DC    CL8'STRH'                                                        
*********DC    CL8'PRNT6'                                                       
*********DC    CL8'ACCA'                                                        
*********DC    CL8'ACCT'                                                        
*********DC    CL8'NET1'                                                        
*********DC    CL8'NET2'                                                        
*********DC    CL8'MPL1'                                                        
*********DC    CL8'MPL4'                                                        
*********DC    CL8'REP7'                                                        
         DC    CL8'CONTROL'                                                     
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                                                             
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
FACMAXIO DC    H'10000'                                                         
FACPTLST DC    XL6'00'                                                          
FACPOPLN DC    H'2'                TIMER POP DURATION (SEC/100)                 
FACPOPMX DC    H'192'              TIMER POP MAXIMUM COUNTER                    
FACPRIO  DC    H'50'               PRIORITY I/O THRESHOLD                       
FACPRCPU DC    H'80'               PRIORITY CPU THRESHOLD                       
FACPRMIN DC    H'5'                PRIORITY MIN VALUE                           
FACDAROK DC    C'N'                PROCESS DARE FROM THIS FACPAK                
FAC31BIT DC    C'Y'                USE 31-BIT I/O ADDRESSES                     
*                                                                               
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    C'Y'                UPDATE PRGMS FILE FROM THIS SYS              
*                                                                               
VTAMAPL  DC    CL8'Y2K'            VTAM APPLICATION ID                          
VTAMUTL  DC    H'500'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'100'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'128'              VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'10'               TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    H'18432'            TEMPSTR RECORD LENGTH                        
FACTMSL  DC    H'14336'            TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(12*14336)         TSAR BUFFER SIZE                             
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(0)                TRACE BUFFER LENGTH                          
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'4'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                SCRUNCH XA STORAGE ALLOC IN 4K'S             
FACSPACE DC    C'DMGADATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(0)                SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    CL1'Y'              TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    CL1'Y'              NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABPXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    XL1'00'             DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    CL1'Y'              UPDATE TABS TABLES FROM THIS FACPAK          
         DC    XL2'00'                                                          
FACSSMXP DC    H'4'                MAXIMUM PHYSICAL SESSIONS                    
         DC    XL2'00'                                                          
*                                                                               
PHLIST   DC    50XL16'00'                                                       
         ORG   PHLIST                                                           
         ORG                                                                    
         DC    X'00'                                                            
*                                                                               
FAJESIO  DC    CL8'       '        *** MONSOON *** (WAS FAJESIO)                
FACJOBMX DC    AL2(500)            *** MONSOON *** (WAS 50)                     
         DC    98X'00'             SPARE                                        
*                                                                               
FAPARMX  DS    0C                                                               
*                                                                               
DMFLIST  DS    0CL16                                                            
         DC    C'PRGMS   ',C'YPRGMS  '                                          
         DC    C'TEMPSTR ',C'YTMPSTR '                                          
         DC    C'TEMPEST ',C'YTMPEST '                                          
         DC    C'DMPFILE ',C'YDMPFIL '                                          
         DC    C'TSTRCVR ',C'YTSTRCV '                                          
         DC    C'PRTQ1   ',C'PRTQ1Y  '                                          
         DC    C'PRTQ2   ',C'PRTQ2Y  '                                          
         DC    C'PRTQ3   ',C'PRTQ3Y  '                                          
         DC    C'PRTQ4   ',C'PRTQ4Y  '                                          
         DC    C'PRTQ5   ',C'PRTQ5Y  '                                          
         DC    C'PRTQ6   ',C'PRTQ6Y  '                                          
         DC    C'PRTQ7   ',C'PRTQ7Y  '                                          
         DC    C'PRTQ8   ',C'PRTQ8Y  '                                          
         DC    C'PRTQ9   ',C'PRTQ9Y  '                                          
         DC    C'PRTQA   ',C'PRTQAY  '                                          
         DC    C'PRTQB   ',C'PRTQBY  '                                          
         DC    C'PRTQC   ',C'PRTQCY  '                                          
         DC    C'PRTQD   ',C'PRTQDY  '                                          
         DC    C'PRTQE   ',C'PRTQEY  '                                          
         DC    C'PRTQF   ',C'PRTQFY  '                                          
         DC    C'PRTQG   ',C'PRTQGY  '                                          
         DC    C'EASIWK  ',C'EASIWKY '                                          
         DC    C'WRKF1   ',C'WRKF1Y  '                                          
         DC    C'WRKF2   ',C'WRKF2Y  '                                          
         DC    C'WRKF3   ',C'WRKF3Y  '                                          
         DC    C'WRKF4   ',C'WRKF4Y  '                                          
         DC    C'WRKF5   ',C'WRKF5Y  '                                          
         DC    C'WRKF6   ',C'WRKF6Y  '                                          
         DC    C'WRKF7   ',C'WRKF7Y  '                                          
         DC    C'WRKF8   ',C'WRKF8Y  '                                          
         DC    C'WRKF9   ',C'WRKF9Y  '                                          
         DC    C'ADRFILE ',C'YADRFIL '                                          
         DC    C'STATS   ',C'YSTATS  '                                          
         DC    C'WKFILE  ',C'WKFILEY '                                          
         DC    C'FACWRK  ',C'FACWRKY '                                          
         DC    C'KWXFILE ',C'KWXFILY '                                          
         DC    C'EDCTA   ',C'EDCTAY  '                                          
         DC    C'EDCTR   ',C'EDCTRY  '                                          
*                                                                               
         DC    C'CTFILE  ',C'CTFILEY '                                          
         DC    C'CTRCVR  ',C'CTRCVRY '                                          
         DC    C'CTREQ   ',C'CTREQY  '                                          
         DC    C'GENDIR  ',C'GENDIR *'                                          
         DC    C'GENFIL  ',C'GENFIL *'                                          
*                                                                               
         DC    C'SPTDIR1 ',C'YSPTDR1 '                                          
         DC    C'STRFDR1 ',C'YTRDIR1 '  TRFDIR1 IN SPOT                         
         DC    C'SPTFIL1 ',C'YSPTFL1 '                                          
         DC    C'STRFFL1 ',C'YTRFIL1 '  TRFFIL1 IN SPOT                         
         DC    C'XSPDIR1 ',C'YXSPDR1 '                                          
         DC    C'XSPFIL1 ',C'YXSPFL1 '                                          
         DC    C'STAFIL1 ',C'YSTAFL1 '                                          
         DC    C'RECV1   ',C'YRECV1  '                                          
         DC    C'REQ1    ',C'YREQ1   '                                          
*                                                                               
         DC    C'TRFDIR1 ',C'YTRDIR1 '                                          
         DC    C'TRFFIL1 ',C'YTRFIL1 '                                          
         DC    C'TRFRCV1 ',C'YTRRCV1 '                                          
         DC    C'TRFREQ1 ',C'YTRREQ1 '                                          
*                                                                               
         DC    C'SPTDIRH ',C'YSPTDRH '                                          
         DC    C'STRFDRH ',C'YTRDIRH '  TRFDIRH IN SPOT                         
         DC    C'SPTFILH ',C'YSPTFLH '                                          
         DC    C'STRFFLH ',C'YTRFILH '  TRFFILH IN SPOT                         
         DC    C'XSPDIRH ',C'YXSPDRH '                                          
         DC    C'XSPFILH ',C'YXSPFLH '                                          
         DC    C'STAFILH ',C'YSTAFLH '                                          
         DC    C'RECVH   ',C'YRECVH  '                                          
         DC    C'REQH    ',C'YREQH   '                                          
*                                                                               
         DC    C'TRFDIRH ',C'YTRDIRH '                                          
         DC    C'TRFFILH ',C'YTRFILH '                                          
         DC    C'TRFRCVH ',C'YTRRCVH '                                          
         DC    C'TRFREQH ',C'YTRREQH '                                          
*                                                                               
         DC    C'SPTDIR8 ',C'YSPTDR8 '                                          
         DC    C'SPTFIL8 ',C'YSPTFL8 '                                          
         DC    C'XSPDIR8 ',C'YXSPDR8 '                                          
         DC    C'XSPFIL8 ',C'YXSPFL8 '                                          
         DC    C'UNTDIR8 ',C'YUNTDR8 '                                          
         DC    C'UNTFIL8 ',C'YUNTFL8 '                                          
         DC    C'STAFIL8 ',C'YSTAFL8 '                                          
         DC    C'RECV8   ',C'YRECV8  '                                          
         DC    C'REQ8    ',C'YREQ8   '                                          
*                                                                               
         DC    C'SPTDIR9 ',C'YSPTDR9 '                                          
         DC    C'SPTFIL9 ',C'YSPTFL9 '                                          
         DC    C'XSPDIR9 ',C'YXSPDR9 '                                          
         DC    C'XSPFIL9 ',C'YXSPFL9 '                                          
         DC    C'UNTDIR9 ',C'YUNTDR9 '                                          
         DC    C'UNTFIL9 ',C'YUNTFL9 '                                          
         DC    C'STAFIL9 ',C'YSTAFL9 '                                          
         DC    C'RECV9   ',C'YRECV9  '                                          
         DC    C'REQ9    ',C'YREQ9   '                                          
*                                                                               
         DC    C'ACCDIRA ',C'YACDIRA '                                          
         DC    C'ACCFILA ',C'YACFILA '                                          
         DC    C'ACCARCA ',C'YACARCA '                                          
         DC    C'ACCDAYA ',C'YACDAYA '                                          
         DC    C'ACCMSTA ',C'YACMSTA '                                          
         DC    C'ACCRCVA ',C'YACRCVA '                                          
         DC    C'ACCREQA ',C'YACREQA '                                          
         DC    C'ACCWRKA ',C'YACWRKA '                                          
*                                                                               
         DC    C'ACCDIRT ',C'YACDIRT '                                          
         DC    C'ACCFILT ',C'YACFILT '                                          
         DC    C'ACCARCT ',C'YACARCT '                                          
         DC    C'ACCDAYT ',C'YACDAYT '                                          
         DC    C'ACCMSTT ',C'YACMSTT '                                          
         DC    C'ACCRCVT ',C'YACRCVT '                                          
         DC    C'ACCREQT ',C'YACREQT '                                          
         DC    C'ACCWRKT ',C'YACWRKT '                                          
*                                                                               
         DC    C'BUDDIR1 ',C'YBDDIR1 '                                          
         DC    C'BUDFIL1 ',C'YBDFIL1 '                                          
         DC    C'MPLDIR1 ',C'YMPDIR1 '                                          
         DC    C'MPLFIL1 ',C'YMPFIL1 '                                          
         DC    C'MPLRCV1 ',C'YMPRCV1 '                                          
         DC    C'MPLREQ1 ',C'YMPREQ1 '                                          
         DC    C'MPQDRA1 ',C'YMPDRA1 '                                          
         DC    C'MPQFLA1 ',C'YMPFLA1 '                                          
*                                                                               
         DC    C'BUDDIR4 ',C'YBDDIR4 '                                          
         DC    C'BUDFIL4 ',C'YBDFIL4 '                                          
         DC    C'MPLDIR4 ',C'YMPDIR4 '                                          
         DC    C'MPLFIL4 ',C'YMPFIL4 '                                          
         DC    C'MPLRCV4 ',C'YMPRCV4 '                                          
         DC    C'MPLREQ4 ',C'YMPREQ4 '                                          
         DC    C'MPQDRA4 ',C'YMPDRA4 '                                          
         DC    C'MPQFLA4 ',C'YMPFLA4 '                                          
*                                                                               
         DC    C'PRTDIR6 ',C'YPRTDR6 '                                          
         DC    C'PRTFIL6 ',C'YPRTFL6 '                                          
         DC    C'PUBDIR6 ',C'YPUBDR6 '                                          
         DC    C'PUBFIL6 ',C'YPUBFL6 '                                          
         DC    C'PRECV6  ',C'YPRECV6 '                                          
         DC    C'PREQ6   ',C'YPREQ6  '                                          
*                                                                               
         DC    C'REPDIR7 ',C'YRPDIR7 '                                          
         DC    C'REPFIL7 ',C'YRPFIL7 '                                          
         DC    C'RREQ7   ',C'YREQ7   '                                          
         DC    C'REPRCV7 ',C'YRPRCV7 '                                          
         DC    C'REPWRK7 ',C'YRPWRK7 '                                          
         DC    C'RRGNEW7 ',C'YRGNEW7 '                                          
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062FAPARMY   09/28/00'                                      
         END                                                                    
