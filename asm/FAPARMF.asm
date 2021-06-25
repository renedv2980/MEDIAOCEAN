*          DATA SET FAPARMF    AT LEVEL 024 AS OF 02/28/01                      
*PHASE FAPARMF                                                                  
         TITLE 'FACPAK STARTUP VARIABLES - TEST SYSTEM (FACFRED)'               
FAPARMS  CSECT                                                                  
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'X'                                                             
FACSYSID DC    AL1(11)                                                          
FACORELN DC    A(32*4000)                                                       
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    C'N'                                                             
FACAUTOQ DC    C'N'                                                             
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'20'                                                            
TIMLOOP  DC    F'20'                                                            
TIMAUTOQ DC    F'90'                                                            
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS **           
TSKPGM   DC    A(72000)                                                         
TSKWRK   DC    A(84000)                                                         
TSKTIA   DC    A(18432)                                                         
TSKTWA   DC    A(18432)                                                         
TSKMAP   DC    A(4096)                                                          
*                                                                               
OPTLOAD  DC    C'I'                                                             
OPTRSTRT DC    C'I'                                                             
OPTVALDT DC    C'N'                                                             
OPTSYS   DC    C'L'                                                             
OPTLINES DC    C'L'                                                             
DMID     DC    C'FT'                                                            
         DS    C                                                                
*                                                                               
ADMFLIST DC    A(DMFLIST)          A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                ALL SYSTEMS NOP                              
SELIST   DC    60CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'CONTROL'                                                     
         ORG                                                                    
*                                                                               
         DS    C                                                                
*                                                                               
PGOPNOP  DC    C'+'                ALL PROGRAMS OP                              
PGLIST   DC    40XL3'00'                                                        
         ORG   PGLIST                                                           
         ORG                                                                    
*                                                                               
FACMAXIO DC    H'10000'                                                         
FACPTLST DC    XL6'00'                                                          
FACPOPLN DC    H'3'                TIMER POP DURATION (SEC/100)                 
FACPOPMX DC    H'128'              TIMER POP MAXIMUM COUNTER                    
FACPRIO  DC    H'50'               PRIORITY I/O THRESHOLD                       
FACPRCPU DC    H'80'               PRIORITY CPU THRESHOLD                       
FACPRMIN DC    H'5'                PRIORITY MIN VALUE                           
FACDAROK DC    C'N'                PROCESS DARE FROM THIS FACPAK                
FAC31BIT DC    C'Y'                USE 31-BIT I/O ADDRESSES                     
*                                                                               
FACPRVID DC    AL1(0)              NO PREVIOUS FACPAK                           
FACPGMUP DC    C'N'                UPDATE PRGMS FILE FROM THIS SYS              
*                                                                               
VTAMAPL  DC    CL8'FRED'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'256'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'32'               VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'50'               VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'1'                TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    H'18432'            TEMPSTR RECORD LENGTH                        
FACTMSL  DC    H'14336'            TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(14336*12-32)      TSAR BUFFER SIZE                             
FACTRACE DC    A(14336)            TRACE BUFFER LENGTH                          
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'4'                MAXIMUM LOGICAL SESSIONS                     
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                16K SCRUNCH XA STORAGE                       
FACSPACE DC    C'DMGTDATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(200000)           SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    CL1'Y'              TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    CL1'Y'              NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABTXXXXXXXX'  NAME OF TABS DATASPACE                       
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
FAJESIO  DC    CL8'       '                                                     
FACJOBMX DC    AL2(500)                                                         
*                                                                               
FACMQION DC    F'2'                MQ SERIES IO CONTROL NUMBER 2=TRACE          
FACMQUTL DC    H'500'              N/U (FOR NUM ENTRIES IN UTL)                 
FACMQBUF DC    H'100'              N/U (FOR NUM BUFFERS                         
AFACMQM  DC    A(FACMQM)           A(MQ SERIES MANAGER NAME)                    
AFACMQIN DC    A(FACMQIN)          A(MQ SERIES FACPAK INPUT Q NAME)             
AFACMQOU DC    A(FACMQOU)          A(MQ SERIES FACPAK OUTPUT Q NAME)            
AFACMQWK DC    A(FACMQWK)          A(MQ SERIES FACPAK WORK Q NAME)              
AFACMQCT DC    A(FACMQCT)          A(MQ SERIES FACPAK CONTROL Q NAME)           
*                                                                               
         DC    XL67'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                  END OF FIXED PART                            
*                                  MQ SERIES MANAGER NAME                       
FACMQM   DC    CL48'MQ5T'                                                       
FACMQIN  DC    CL48'MQ5T.MEL.INPUT.LOCALQ'                                      
FACMQOU  DC    CL48'MQ5T.MEL.OUTPUT.LOCALQ'                                     
FACMQWK  DC    CL48'MQ5T.MEL.WORK.LOCALQ'                                       
FACMQCT  DC    CL48'MQ5T.MEL.CONTROL.LOCALQ'                                    
         SPACE 1                                                                
*                                                                               
DMFLIST  DS    0CL16                                                            
         DC    C'PRGMS   ',C'SRPGMF  '                                          
         DC    C'TEMPSTR ',C'SRTWAF  '                                          
         DC    C'TEMPEST ',C'TEMPES0 '                                          
         DC    C'PRTQ1   ',C'PRTQ10  '                                          
         DC    C'PRTQ2   ',C'PRTQ20  '                                          
         DC    C'PRTQ3   ',C'PRTQ30  '                                          
         DC    C'PRTQ4   ',C'PRTQ40  '                                          
         DC    C'PRTQ5   ',C'PRTQ50  '                                          
         DC    C'PRTQ6   ',C'PRTQ60  '                                          
         DC    C'PRTQ7   ',C'PRTQ70  '                                          
         DC    C'PRTQ8   ',C'PRTQ80  '                                          
         DC    C'PRTQ9   ',C'PRTQ90  '                                          
         DC    C'PRTQA   ',C'PRTQA0  '                                          
         DC    C'PRTQB   ',C'PRTQB0  '                                          
         DC    C'PRTQC   ',C'PRTQC0  '                                          
         DC    C'PRTQD   ',C'PRTQD0  '                                          
         DC    C'PRTQE   ',C'PRTQE0  '                                          
         DC    C'PRTQF   ',C'PRTQF0  '                                          
         DC    C'PRTQG   ',C'PRTQG0  '                                          
         DC    C'EASIWK  ',C'EASIWK0 '                                          
         DC    C'WRKF1   ',C'WRKF10  '                                          
         DC    C'WRKF2   ',C'WRKF20  '                                          
         DC    C'WRKF3   ',C'WRKF30  '                                          
         DC    C'WRKF4   ',C'WRKF40  '                                          
         DC    C'WRKF5   ',C'WRKF50  '                                          
         DC    C'WRKF6   ',C'WRKF60  '                                          
         DC    C'WRKF7   ',C'WRKF70  '                                          
         DC    C'WRKF8   ',C'WRKF80  '                                          
         DC    C'WRKF9   ',C'WRKF90  '                                          
         DC    C'ADRFILE ',C'SRADRF  '                                          
         DC    C'STATS   ',C'SRSTAF  '                                          
         DC    C'DMPFILE ',C'SRDMPF  '                                          
         DC    C'TSTRCVR ',C'SRTSTF  '                                          
         DC    C'WKFILE  ',C'WKFILE  '                                          
         DC    C'FACWRK  ',C'FACWRK0 '                                          
         DC    C'KWXFILE ',C'SRKWXF  '                                          
         DC    C'EDCTA   ',C'EDCTA   '                                          
         DC    C'EDCTR   ',C'EDCTR   '                                          
         DC    C'CTFILE  ',C'CTFILE V'                                          
         DC    C'CTREQ   ',C'CTREQ  V'                                          
         DC    C'CTRCVR  ',C'CTRCVR V'                                          
         DC    C'GENDIR  ',C'GENDIR0V'                                          
         DC    C'GENFIL  ',C'GENFIL0V'                                          
*                                                                               
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024FAPARMF   02/28/01'                                      
         END                                                                    
