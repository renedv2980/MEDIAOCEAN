*          DATA SET FAPARMD    AT LEVEL 050 AS OF 09/11/15                      
*PHASE FAPARMDA                                                                 
         TITLE 'FACPAK STARTUP VARIABLES - V T A M   DARE SYSTEM'               
FAPARMS  CSECT                                                                  
FAPARMLN DC    AL2(FAPARMX-FAPARMS)                                             
FACRECRD DC    C'N'                                                             
FACSYSID DC    AL1(13)                                                          
FACORELN DC    A(32*4000)                                                       
FAPHLST  DC    CL8'FAPHLS'                                                      
FACORES  DC    C'Y'                                                             
FACAUTOQ DC    C'N'                                                             
FACTWAS  DC    H'12'                                                            
TIMTEST  DC    F'20'                                                            
TIMLOOP  DC    F'20'                                                            
TIMAUTOQ DC    F'90'                                                            
*                                                                               
TSKNUM   DC    F'0'                ** THIS IS NOW SET IN DDS.PARMS **           
TSKPGM   DC    A(57000)            TASK AREAS MUST BE DBLWD ALIGNED             
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
DMID     DC    C'FA'                                                            
         DS    C                                                                
*                                                                               
ADMFLIST DC    A(DMFLIST)          A(FILE RENAME TABLE)                         
*                                                                               
SEOPNOP  DC    C'-'                ALL SYSTEMS NOP                              
SELIST   DC    80CL8' '                                                         
         ORG   SELIST                                                           
         DC    CL8'CONTROL'                                                     
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
FACMAXIO DC    H'2000'                                                          
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
VTAMAPL  DC    CL8'DARE'           VTAM APPLICATION ID                          
VTAMUTL  DC    H'512'              VTAM NUM ENTRIES IN UTL                      
VTAMPRQ  DC    H'200'              VTAM NUM ENTRIES IN PRQ                      
VTAMBUF  DC    H'70'               VTAM NUM BUFFERS                             
         DC    H'0'                                                             
*                                                                               
FACTWAM  DC    H'1'                TWA TEMPSTR MODULUS FOR DISK ADDR            
FACTWAL  DC    H'18432'            TEMPSTR RECORD LENGTH                        
FACTMSL  DC    H'14336'            TEMPEST RECORD LENGTH                        
FACTSAR  DC    A(12*14336)         TSAR BUFFER SIZE                             
         ORG   FACTSAR                                                          
         DC    X'80'               USE TWO TSAR BUFFERS PER TASK                
         ORG                                                                    
FACTRACE DC    A(2*14336)          TRACE BUFFER SIZE                            
FACDUTL  DC    H'1'                DUMMY UTL ENTRIES (1=USE NUM TSKS)           
FACMXSCR DC    H'1'                MAXIMUM NUM SCRIPTS                          
FACMXDUM DC    H'1'                MAXIMUM NUM DUMMY USERS (S/R)                
FACSSMAX DC    H'4'            ==> MAXIMUM SESSIONS PER TERMINAL                
FACSSPGS DC    H'5'                PAGES PER SESSION                            
FACSCRXA DC    H'0'                SCRUNCH XA STORAGE ALLOC IN 4K'S             
FACSPACE DC    C'DMGADATAMGRX'     DATAMGR DATASPACE NAME                       
FACSCT   DC    A(0)                SCRIPT TRACE BUFFER SIZE                     
FACTAPRG DC    CL1'Y'              TAPRG IS A DISPLACEMENT     (=Y)             
FACTMPST DC    CL1'Y'              NEW TEMPEST (NO DEALLOCATE) (=Y)             
FACTBDSP DC    CL12'TABPXXXXXXXX'  NAME OF TABS DATASPACE                       
FACPROTO DC    XL1'00'             DUMP ON PROTECTION ERROR    (=Y)             
FACUTAB  DC    CL1'Y'              DON'T UPDATE TABS TABLES                     
         DC    XL6'00'                                                          
*                                                                               
PHLIST   DC    50XL16'00'                                                       
         ORG   PHLIST                                                           
         ORG                                                                    
         DC    X'00'                                                            
*                                                                               
FAJESIO  DC    CL8' '                                                           
FACJOBMX DC    AL2(500)                                                         
*                                                                               
FACMQION DC    F'0'                MQ SERIES IO CONTROL NUMBER 2=TRACE          
FACMQUTL DC    H'500'              N/U (FOR NUM ENTRIES IN UTL)                 
FACMQBUF DC    H'100'              N/U (FOR NUM BUFFERS)                        
AFACMQM  DC    A(0)                A(MQ SERIES MANAGER NAME)                    
AFACMQIN DC    A(0)                A(MQ SERIES FACPAK INPUT Q NAME)             
AFACMQOU DC    A(0)                A(MQ SERIES FACPAK OUTPUT Q NAME)            
AFACMQWK DC    A(0)                A(MQ SERIES FACPAK WORK Q NAME)              
AFACMQCT DC    A(0)                A(MQ SERIES FACPAK CONTROL Q NAME)           
DDICTLEN DC    A(0)                OVERRIDE DATA DICTIONARY LENGTH              
UKMEDDSP DC    XL12'00'            UK MEDIA DATASPACE NAME                      
PGMSDSP  DC    CL12'PRGAXXXXXXXX'  PROGRAMS FILE DATASAPCE NAME                 
*                                                                               
         DC    XL39'00'            SPARE                                        
*                                                                               
FAPARMX  DS    0C                                                               
*                                                                               
DMFLIST  DS    0CL16                                                            
         DC    C'PRGMS   ',C'GPRGMS  '                                          
         DC    C'TEMPSTR ',C'GTMPSTR '                                          
         DC    C'TEMPEST ',C'GTMPEST '                                          
         DC    C'ADRFILE ',C'GADRFIL '                                          
         DC    C'STATS   ',C'GSTATS  '                                          
         DC    C'DMPFILE ',C'GDMPFIL '                                          
         DC    C'TSTRCVR ',C'GTSTRCV '                                          
         DC    C'PRTQ1   ',C'PRTQ1   '                                          
         DC    C'PRTQ2   ',C'PRTQ2   '                                          
         DC    C'PRTQ3   ',C'PRTQ3   '                                          
         DC    C'PRTQ4   ',C'PRTQ4   '                                          
         DC    C'PRTQ5   ',C'PRTQ5   '                                          
         DC    C'PRTQ6   ',C'PRTQ6   '                                          
         DC    C'PRTQ7   ',C'PRTQ7   '                                          
         DC    C'PRTQ8   ',C'PRTQ8   '                                          
         DC    C'PRTQ9   ',C'PRTQ9   '                                          
         DC    C'PRTQA   ',C'PRTQA   '                                          
         DC    C'PRTQB   ',C'PRTQB   '                                          
         DC    C'PRTQC   ',C'PRTQC   '                                          
         DC    C'PRTQD   ',C'PRTQD   '                                          
         DC    C'PRTQE   ',C'PRTQE   '                                          
         DC    C'PRTQF   ',C'PRTQF   '                                          
         DC    C'PRTQG   ',C'PRTQG   '                                          
         DC    C'WRKF1   ',C'WRKF10  '                                          
         DC    C'WRKF2   ',C'WRKF20  '                                          
         DC    C'WRKF3   ',C'WRKF30  '                                          
         DC    C'WRKF4   ',C'WRKF40  '                                          
         DC    C'WRKF5   ',C'WRKF50  '                                          
         DC    C'WRKF6   ',C'WRKF60  '                                          
         DC    C'WRKF7   ',C'WRKF70  '                                          
         DC    C'WRKF8   ',C'WRKF80  '                                          
         DC    C'WRKF9   ',C'WRKF90  '                                          
         DC    C'WRKFA   ',C'WRKFA0  '                                          
         DC    C'WRKFB   ',C'WRKFB0  '                                          
         DC    C'WRKFC   ',C'WRKFC0  '                                          
         DC    C'WRKFD   ',C'WRKFD0  '                                          
         DC    C'WRKFE   ',C'WRKFE0  '                                          
         DC    C'WRKFF   ',C'WRKFF0  '                                          
         DC    C'WRKFG   ',C'WRKFG0  '                                          
         DC    C'WKFILE  ',C'NOP     '                                          
         DC    C'FACWRK  ',C'NOP     '                                          
         DC    C'EASIWK  ',C'NOP     '                                          
         DC    C'KWXFILE ',C'NOP     '                                          
         DC    C'EDCTA   ',C'EDCTA  R'                                          
         DC    C'EDCTR   ',C'NOP     '                                          
         DC    C'EDCTZ   ',C'NOP     '                                          
         DC    C'GENDIR  ',C'GENDIR *'                                          
         DC    C'GENFIL  ',C'GENFIL *'                                          
         DC    C'CTFILE  ',C'CTFILE *'                                          
*                                                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050FAPARMD   09/11/15'                                      
         END                                                                    
