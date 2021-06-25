*          DATA SET MPDITAB    AT LEVEL 004 AS OF 09/25/00                      
*PHASE T5103FA                                                                  
*                                                                               
         TITLE 'MPDITAB - BASE/QUAL/CTL TABLES'                                 
*                                                                               
MPDITAB  CSECT                                                                  
*                                                                               
         DC    A(BASETAB-MPDITAB)  A(BASE IDENTIFIER TABLE)                     
         DC    A(QUALTAB-MPDITAB)  A(QUALIFIER IDENTIFIER TABLE)                
         DC    A(CONTAB-MPDITAB)   A(BASE/QUALIFIER CONTROL TABLE)              
         DC    A(KEYTAB-MPDITAB)   A(OPTION KEYWORD TABLE)                      
         DC    A(ANSTAB-MPDITAB)   A(OPTION ARGUMENT/ANSWER TABLE)              
         DC    A(COMBTAB-MPDITAB)  A(OPTION KEYWORD=ARGUMENT TABLE)             
         DC    A(FXDTAB-MPDITAB)   A(FIXED OFFSETS TABLE)                       
         DC    A(GBLTAB-MPDITAB)   A(GLOBAL OFFSETS TABLE)                      
         SPACE 3                                                                
*                                                                               
*              BASE TRANSLATION TABLE                                           
*              ----------------------                                           
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL CODE                                       
*              BYTE  4    - MINIMUM LENGTH FOR COMPARE                          
*              BYTES 5-12 - EXTERNAL NAME                                       
*                                                                               
BASETAB  DS    0H                                                               
         DC    AL1(BLEN),C'B',AL1(MPBIMP,01),CL8'IMP'                           
BLEN     EQU   *-BASETAB                                                        
         DC    AL1(BLEN),C'B',AL1(MPBAUD,02),CL8'AUD'                           
         DC    AL1(BLEN),C'B',AL1(MPBRCH,02),CL8'RCH'                           
         DC    AL1(BLEN),C'B',AL1(MPBCOV,02),CL8'COV'                           
         DC    AL1(BLEN),C'B',AL1(MPBVRDR,02),CL8'VRDR'                         
         DC    AL1(BLEN),C'B',AL1(MPBDUPE,02),CL8'DUPE'                         
         DC    AL1(BLEN),C'B',AL1(MPBNET,02),CL8'NET'                           
         DC    AL1(BLEN),C'B',AL1(MPBSUM,02),CL8'SUM'                           
         DC    AL1(BLEN),C'B',AL1(MPBUNIQ,02),CL8'UNIQ'                         
*                                                                               
         DC    AL1(BLEN),C'B',AL1(MPB$,01),CL8'$'                               
         DC    AL1(BLEN),C'B',AL1(MPBCOST,02),CL8'COST'                         
         DC    AL1(BLEN),C'B',AL1(MPBCIRC,04),CL8'CIRC'                         
         DC    AL1(BLEN),C'B',AL1(MPBINS,03),CL8'INS'                           
*                                                                               
         DC    AL1(BLEN),C'B',AL1(MPBGRP,01),CL8'GRP'                           
         DC    AL1(BLEN),C'B',AL1(MPBAF,02),CL8'AF'                             
         DC    AL1(BLEN),C'B',AL1(MPBCOMP,03),CL8'COMP'                         
         DC    AL1(BLEN),C'B',AL1(MPBCPM,03),CL8'CPM'                           
         DC    AL1(BLEN),C'B',AL1(MPBCPT,03),CL8'CPT'                           
         DC    AL1(BLEN),C'B',AL1(MPBCPP,03),CL8'CPP'                           
         DC    AL1(BLEN),C'B',AL1(MPBTURN,03),CL8'TURN'                         
         DC    AL1(BLEN),C'B',AL1(MPBRPC,03),CL8'RPC'                           
         DC    AL1(BLEN),C'B',AL1(MPBVRPC,04),CL8'VRPC'                         
         DC    AL1(BLEN),C'B',AL1(MPBCOUNT,03),CL8'COUNT'                       
*                                                                               
         DC    AL1(BLEN),C'B',AL1(MPBTARG,99),CL8'TARG'                         
         EJECT                                                                  
*                                                                               
*              QUALIFIER TRANSLATION TABLE                                      
*              ---------------------------                                      
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL CODE                                       
*              BYTE  4    - MINIMUM LENGTH FOR COMPARE                          
*              BYTES 5-12 - EXTERNAL NAME                                       
*                                                                               
QUALTAB  DS    0H                                                               
         DC    AL1(QLEN),C'Q',AL1(MPQ#,01),CL8'#'                               
QLEN     EQU   *-QUALTAB                                                        
         DC    AL1(QLEN),C'Q',AL1(MPQPCTOT,01),CL8'%'                           
         DC    AL1(QLEN),C'Q',AL1(MPQPCTAR,02),CL8'%TARG'                       
         DC    AL1(QLEN),C'Q',AL1(MPQPCRCH,02),CL8'%RCH'                        
         DC    AL1(QLEN),C'Q',AL1(MPQPCIMP,02),CL8'%IMP'                        
         DC    AL1(QLEN),C'Q',AL1(MPQPCGRP,02),CL8'%GRP'                        
         DC    AL1(QLEN),C'Q',AL1(MPQRANK,01),CL8'RANK'                         
         DC    AL1(QLEN),C'Q',AL1(MPQCPM,03),CL8'CPM'                           
         DC    AL1(QLEN),C'Q',AL1(MPQCPP,03),CL8'CPP'                           
         DC    AL1(QLEN),C'Q',AL1(MPQIND,03),CL8'IND'                           
         DC    AL1(QLEN),C'Q',AL1(MPQCUME,02),CL8'CUME'                         
         DC    AL1(QLEN),C'Q',AL1(MPQCPC,05),CL8'CUME%'                         
         DC    AL1(QLEN),C'Q',AL1(MPQCPCTG,06),CL8'CUME%TAR'                    
         DC    AL1(QLEN),C'Q',AL1(MPQCPCIM,06),CL8'CUME%IMP'                    
         DC    AL1(QLEN),C'Q',AL1(MPQCPCGR,06),CL8'CUME%GRP'                    
         DC    AL1(QLEN),C'Q',AL1(MPQCPCR,06),CL8'CUME%RCH'                     
         EJECT                                                                  
*                                                                               
*              PAIRING AND CONTROL TABLE                                        
*              -------------------------                                        
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL BASE CODE                                  
*              BYTE  4    - INTERNAL QUAL CODE                                  
*              BYTES 5-6  - REPORT VALIDITY FLAGS                               
*              X'0001'    - SCHEDULE EVALUATION                                 
*              X'0002'    - FREQUENCY DISTRIBUTION                              
*              X'0004'    - REACH EXTRAPOLATION                                 
*              X'0008'    - VEHICLE/VEHICLE GRID                                
*              X'0010'    - VEHICLE COMPARISON                                  
*              ETC                                                              
*              BYTE  7    - WHERE INFO STORED IN DATA VECTOR                    
*                           0 - IN FIXED PORTION (I.E. VEHICLE DETAIL)          
*                           1 - IN VARIABLE PORTION (I.E. SURVEY DATA)          
*              BYTES 8 ON - CONTROL INFORMATION                                 
*                                                                               
*              TABLE ENTRIES MUST OBEY CERTAIN RULES.                           
*                                                                               
*              1) A TABLE ENTRY MUST NOT REFER TO ITSELF                        
*                 WITHIN A FORMULA - WOULD GENERATE A LOOP                      
*                                                                               
*              2) ALL FORMULAE MUST EVENTUALLY REDUCE TO                        
*                 A COLLECTION OF PRIMARY (RAW) COMPONENTS.                     
*                                                                               
*              3) IF A FORMULA CONTAINS AN OPERATOR '05','06','07'              
*                 (RANK ,CUME,REV CUME) THEN NO 2ND OR 3RD BASE/QUAL'S          
*                 ARE ALLOWED.                                                  
*                                                                               
*              4) IF CUME OR REVERSE CUME ARE SPECIFIED THEN THE                
*                 DATA LENGTH ,TYPE AND PRECISION MUST BE THE SAME AS           
*                 FOR FORMULA OPERAND 1.                                        
*                                                                               
*              5) RANK ENTRIES MUST BE BINARY DATA TYPE (1-4 BYTES)             
*                                                                               
*              6) BINARY FIELDS MAY NOT EXCEED 4 BYTES.                         
*                 PACKED FIELDS MAY NOT EXCEED 8 BYTES.                         
*                                                                               
*              7) IF A TABLE ENTRY IS DEFINED AS BEING IN THE FIXED             
*                 PORTION OF A DATA VECTOR THEN THAT BASE/QUALIFIER             
*                 PAIR MUST EXIST IN FXDTAB - (FIXED OFFSETS TABLE)             
*                 LIKEWISE GLOBAL VARIABLES MUST EXIST IN GBLTAB.               
*                                                                               
*              8) CUME , CUME% , RANK QUALIFIERS ARE NOT PERMITTED              
*                 AS OPERANDS WITHIN FORMULAE.                                  
*                                                                               
*                                                                               
*                                                                               
*              VALID OPERATORS ARE :-                                           
*              01 - DIVIDE              - OP1 / OP2                             
*              02 - CPT                 - OP1 * 1000 / OP2                      
*              03 - PCT OF TOTAL        - OP1 * 100 / TOT OP2                   
*              04 - INDEX               - OP1 / TOT (OR AVE) OP2                
*              05 - RANK                - RANK POSN OF OP1                      
*              06 - CUME                - CUME OF OP1                           
*              07 - REV CUME            - REVERSE CUME OF OP1                   
*                                         (TOTAL OP1 LESS CUME OP1)             
*              08 - PCT                 - OP1 * 100 / OP2                       
*                                                                               
         SPACE 2                                                                
CONTAB   DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFFIXABLE)                
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT001-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
PLEN     EQU   *-CONTAB                                                         
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        CONTROL FLAG (SUFFIX REQUIRED)               
         DC    X'01'               SUFFIX - IMPRESSIONS-'N'                     
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                FORMULA - PERFORM A SUBROUTINE               
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR05)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT001-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        CONTROL FLAG (SUFFIX REQUIRED)               
         DC    X'02'               SUFFIX - IMPRESSIONS-'0-N'                   
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR06)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT001-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        CONTROL FLAG (SUFFIX REQUIRED)               
         DC    X'03'               SUFFIX - IMPRESSIONS-'1-N'                   
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR07)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT001-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        CONTROL FLAG (SUFFIX REQUIRED)               
         DC    X'04'               SUFFIX - IMPRESSIONS-'N+'                    
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR08)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT001-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQ#)                                  
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPTIONAL)               
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS MEANINGLESS                      
         DS    0XL8                CALC FORMULA                                 
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR09)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT038-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBAUD,MPQ#)                                  
         DC    AL2(MPVXV+MPRANK)                                                
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL BYTE (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT023-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQPCTOT)                              
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCTT)       OPERATOR  1  - % OF TOTAL OPERAND 2          
         DC    AL1(MPBIMP)         OPERAND   2  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT002-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQPCTOT)                              
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCTT)       OPERATOR  1  - OP1  %  TOTAL OP2             
         DC    AL1(MPBIMP)         OPERAND   2  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT002-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQPCTAR)                              
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - % OF OPERAND 2                
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT027-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQPCTAR)                              
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX REQ)                  
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - % OF OPERAND 2                
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT039-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQPCTAR)                              
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  %  OP2                   
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT027-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQRANK)                               
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX REQ)                  
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT040-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQCPM)                                
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBIMP)         OPERAND   2  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT041-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQCUME)                               
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCUM)        OPERATOR  1  - CUME OF OPERAND 1             
         DC    XL5'0'                                                           
         DC    AL4(OPT003-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBIMP,MPQCPCTG)                              
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQCUME)        QUALIFIER 1  - CUME                          
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  %  OP2                   
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT004-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE (#)            EVAL           
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQ#)                                  
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT009-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE (#)            REACH          
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQ#)                                  
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT043-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE (% OF TARG)                   
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQPCTAR)                              
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX - (DEFAULT)                           
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBRCH)         OPERAND    1  - REACH                        
         DC    AL1(MPQ#)           QUALIFIER  1  - NUMBER                       
         DC    AL1(MPOPPCT)        OPERATOR   1  - OP1  %  OP2                  
         DC    AL1(MPBTARG)        OPERAND    2  - TARGET                       
         DC    AL1(MPQ#)           QUALIFIER  2  - NUMBER                       
         DC    XL3'00'                                                          
         DC    AL4(OPT044-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE (% OF TARG)                   
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQPCTAR)                              
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX - (DEFAULT)                           
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBRCH)         OPERAND    1  - REACH                        
         DC    AL1(MPQ#)           QUALIFIER  1  - NUMBER                       
         DC    AL1(MPOPPCT)        OPERATOR   1  - OP1  %  OP2                  
         DC    AL1(MPBTARG)        OPERAND    2  - TARGET                       
         DC    AL1(MPQ#)           QUALIFIER  2  - NUMBER                       
         DC    XL3'00'                                                          
         DC    AL4(OPT015-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE RANKED                        
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQRANK)                               
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX - (DEFAULT)                           
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBRCH)         OPERAND   1  - REACH                         
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT045-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE RANKED                        
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQRANK)                               
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX - (DEFAULT)                           
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBRCH)         OPERAND   1  - REACH                         
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT026-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE CPM                           
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQCPM)                                
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPT)                    
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBRCH)         OPERAND   2  - REACH                         
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT046-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  REACH/COVERAGE CPM                           
         DC    AL1(PLEN),C'P',AL1(MPBRCH,MPQCPM)                                
         DC    AL2(MPRANK)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPTIONAL)               
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBRCH)         OPERAND   2  - REACH                         
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT016-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  VRDR NUMBER                                  
         DC    AL1(PLEN),C'P',AL1(MPBVRDR,MPQ#)                                 
         DC    AL2(MPVXV+MPREACH+MPRANK)                                        
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT021-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  DUPE(RCH)      (#)            VXV            
         DC    AL1(PLEN),C'P',AL1(MPBDUPE,MPQ#)                                 
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT034-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  DUPE(RCH)      (% OF TARG)     VXV           
         DC    AL1(PLEN),C'P',AL1(MPBDUPE,MPQPCTAR)                             
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBDUPE)        OPERAND    1  - DUPE                         
         DC    AL1(MPQ#)           QUALIFIER  1  - NUMBER                       
         DC    AL1(MPOPPCT)        OPERATOR   1  - OP1  %  OP2                  
         DC    AL1(MPBTARG)        OPERAND    2  - TARGET                       
         DC    AL1(MPQ#)           QUALIFIER  2  - NUMBER                       
         DC    XL3'00'                                                          
         DC    AL4(OPT035-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  DUPE(RCH)CPM                   VXV           
         DC    AL1(PLEN),C'P',AL1(MPBDUPE,MPQCPM)                               
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBDUPE)        OPERAND   2  - DUPE                          
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT036-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  NET (RCH)      (#)            VXV            
         DC    AL1(PLEN),C'P',AL1(MPBNET,MPQ#)                                  
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT031-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  NET(RCH)       (% OF TARG)     VXV           
         DC    AL1(PLEN),C'P',AL1(MPBNET,MPQPCTAR)                              
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBNET)         OPERAND    1  - NET                          
         DC    AL1(MPQ#)           QUALIFIER  1  - NUMBER                       
         DC    AL1(MPOPPCT)        OPERATOR   1  - OP1  %  OP2                  
         DC    AL1(MPBTARG)        OPERAND    2  - TARGET                       
         DC    AL1(MPQ#)           QUALIFIER  2  - NUMBER                       
         DC    XL3'00'                                                          
         DC    AL4(OPT032-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  NET(RCH) CPM                   VXV           
         DC    AL1(PLEN),C'P',AL1(MPBNET,MPQCPM)                                
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBNET)         OPERAND   2  - NET                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT033-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  SUM (RCH)      (#)            VXV            
         DC    AL1(PLEN),C'P',AL1(MPBSUM,MPQ#)                                  
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT028-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  SUM(RCH)       (% OF TARG)     VXV           
         DC    AL1(PLEN),C'P',AL1(MPBSUM,MPQPCTAR)                              
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    X'06'               OPERAND    1  - SUM                          
         DC    AL1(MPQ#)           QUALIFIER  1  - NUMBER                       
         DC    AL1(MPOPPCT)        OPERATOR   1  - OP1  %  OP2                  
         DC    AL1(MPBTARG)        OPERAND    2  - TARGET                       
         DC    AL1(MPQ#)           QUALIFIER  2  - NUMBER                       
         DC    XL3'00'                                                          
         DC    AL4(OPT029-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  SUM(RCH) CPM                   VXV           
         DC    AL1(PLEN),C'P',AL1(MPBSUM,MPQCPM)                                
         DC    AL2(MPVXV)                                                       
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFF OPTIONAL)                 
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - *1000  /  OP2                 
         DC    AL1(MPBSUM)         OPERAND   2  - SUM                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT030-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  UNIQUE CONTRIBUTION           EVAL           
         DC    AL1(PLEN),C'P',AL1(MPBUNIQ,MPQ#)                                 
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NO SUFFIX ALLOWED)             
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FURMULA                                   
         DC    AL4(OPT049-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COST                                         
         DC    AL1(PLEN),C'P',AL1(MPBCOST,MPQ#)                                 
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
         DC    C'F'                FIXED PART OF DATA VECTOR                    
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR - UNITY                  
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT008-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COST RANKED                                  
         DC    AL1(PLEN),C'P',AL1(MPBCOST,MPQRANK)                              
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFFIXABLE)                
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'01'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRRNK)       OPERATOR  1  - REV RANK                      
         DC    XL5'0'                                                           
         DC    AL4(OPT013A-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                
         SPACE                                                                  
*                                  CIRCULATION                                  
         DC    AL1(PLEN),C'P',AL1(MPBCIRC,MPQ#)                                 
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
         DC    C'F'                FIXED PART OF DATA VECTOR                    
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT010-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  CIRCULATION RANKED                           
         DC    AL1(PLEN),C'P',AL1(MPBCIRC,MPQRANK)                              
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCIRC)        OPERAND   1  - CIRCULATION                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANKED                        
         DC    XL5'0'                                                           
         DC    AL4(OPT013C-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                
         SPACE                                                                  
*                                  CIRCULATION INDEX                            
         DC    AL1(PLEN),C'P',AL1(MPBCIRC,MPQIND)                               
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCIRC)        OPERAND   1  - CIRCULATION                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPNDX)        OPERATOR  1  - INDEX                         
         DC    AL1(MPBCIRC)        OPERAND   2  - CIRCULATION                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    AL1(MPOPMPLY)       OPERATOR  2  - MULTIPLY                      
         DC    AL1(MPBNDVS)        OPERAND   3  - VEHICLES (=NO DVECTS)         
         DC    AL1(MPQ#)           QUALIFIER 3  - NUMBER                        
         DC    AL4(OPT050-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  INSERTION COUNT                              
         DC    AL1(PLEN),C'P',AL1(MPBINS,MPQ#)                                  
         DC    AL2(MPEVAL)                                                      
         DC    C'F'                FIXED PART OF DATA VECTOR                    
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(OPT011-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQ#)                                  
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'P'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR  (99.999)                
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  % OP2                    
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT005-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQ#)                                  
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX                                       
         DC    C'P'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR  (99.999)                
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  % OP2                    
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT042-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQ#)                                  
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (OPTIONAL SUFFIX)             
         DC    X'01'               SUFFIX                                       
         DC    C'P'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR  (99.999)                
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  % OP2                    
         DC    AL1(MPBTARG)        OPERAND   2  - TARGET                        
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT005-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQPCTOT)                              
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBGRP)         OPERAND   1  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCTT)       OPERATOR  1  - % OF TOTAL OPERAND 2          
         DC    AL1(MPBGRP)         OPERAND   2  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT006-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  GRP AS % TOTAL GRP            FREQ           
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQPCTOT)                              
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPTIONAL)             
         DC    X'01'               SUFFIX          N TIMES ONLY                 
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR    (999.99)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBGRP)         OPERAND   1  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCTT)       OPERATOR  1  - % OF TOTAL OPERAND 2          
         DC    AL1(MPBGRP)         OPERAND   2  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT006-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  GRP RANKED                                   
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQRANK)                               
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBGRP)         OPERAND   1  - GROSS RATING POINTS           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT013-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  GRP RANKED                                   
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQRANK)                               
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG   (SUFFIX OPT)                  
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBGRP)         OPERAND   1  - GROSS RATING POINTS           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT013-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  GRP CUME                                     
         DC    AL1(PLEN),C'P',AL1(MPBGRP,MPQCUME)                               
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG   (NOT SUFFIXABLE)              
         DC    X'00'               SUFFIX                                       
         DC    C'P'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR   (999.999)              
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBGRP)         OPERAND   1  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCUM)        OPERATOR  1  - CUME OF OPERAND 1             
         DC    XL5'0'                                                           
         DC    AL4(OPT007-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  AF AS RAW NUMBER                             
         DC    AL1(PLEN),C'P',AL1(MPBAF,MPQ#)                                   
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPT)                    
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR (9.999)                  
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - /  OP2                        
         DC    AL1(MPBRCH)         OPERAND   2  - REACH                         
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT047-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  AF AS RAW NUMBER                             
         DC    AL1(PLEN),C'P',AL1(MPBAF,MPQ#)                                   
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX REQ)                    
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR (9.999)                  
         DC    AL1(MPTOTYES)      DISP TOTALS NOT MEANINGFUL                    
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBIMP)         OPERAND   1  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - /  OP2                        
         DC    AL1(MPBCOUNT)       OPERAND   2  - COUNT                         
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT052-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  AF RANKED                                    
         DC    AL1(PLEN),C'P',AL1(MPBAF,MPQRANK)                                
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPT)                    
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBAF)          OPERAND   1  - AF                            
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANK                          
         DC    XL5'0'                                                           
         DC    AL4(OPT048-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COMP NUMBER                                  
         DC    AL1(PLEN),C'P',AL1(MPBCOMP,MPQ#)                                 
         DC    AL2(MPVXV+MPRANK+MPREACH)                                        
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBAUD)         OPERAND   1  - AUDIENCE                      
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPPCT)        OPERATOR  1  - OP1  %  OP2                   
         DC    AL1(MPBVRDR)        OPERAND   2  - VRDR                          
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT017-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COMP RANKED                                  
         DC    AL1(PLEN),C'P',AL1(MPBCOMP,MPQRANK)                              
         DC    AL2(MPRANK)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFFIXABLE)                
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOMP)        OPERAND   1  - COMP                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANKED                        
         DC    XL5'0'                                                           
         DC    AL4(OPT018-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COMP INDEX                                   
         DC    AL1(PLEN),C'P',AL1(MPBCOMP,MPQIND)                               
         DC    AL2(MPRANK)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFFIXABLE)                
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR (999)                    
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOMP)        OPERAND   1  - COMP                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPNDX)        OPERATOR  1  - INDEX                         
         DC    AL1(MPBCOMP)        OPERAND   2  - COMP                          
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT019-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  CPT (** SEE BELOW FOR ANOTHER **)            
         DC    AL1(PLEN),C'P',AL1(MPBCPT,MPQ#)                                  
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFFIXABLE)                
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPCPT)        OPERATOR  1  - * 1000 / OP2                  
         DC    AL1(MPBIMP)         OPERAND   2  - IMPRESSIONS                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT012-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  CPT RANKED                                   
         DC    AL1(PLEN),C'P',AL1(MPBCPT,MPQRANK)                               
         DC    AL2(MPEVAL+MPRANK)                                               
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCPT)         OPERAND   1  - CPT                           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRRNK)       OPERATOR  1  - REV RANK                      
         DC    XL5'0'                                                           
         DC    AL4(OPT013B-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                
         SPACE                                                                  
*                                  CPP (COST PER RATING POINT)                  
         DC    AL1(PLEN),C'P',AL1(MPBCPP,MPQ#)                                  
         DC    AL2(MPEVAL)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - OP1 / OP2                     
         DC    AL1(MPBGRP)         OPERAND   2  - 4RP                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT014-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  CPP (COST PER RATING POINT)                  
         DC    AL1(PLEN),C'P',AL1(MPBCPP,MPQ#)                                  
         DC    AL2(MPREACH)                                                     
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        CONTROL FLAG (SUFFIX OPT)                    
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'83'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBCOST)        OPERAND   1  - COST                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - OP1 / OP2                     
         DC    AL1(MPBGRP)         OPERAND   2  - GRP                           
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'0'                                                           
         DC    AL4(OPT014-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  RPC NUMBER                                   
         DC    AL1(PLEN),C'P',AL1(MPBRPC,MPQ#)                                  
         DC    AL2(MPVXV+MPRANK+MPREACH)                                        
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBAUD)         OPERAND   1  - AUDIENCE                      
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - OP1  /  OP2                   
         DC    AL1(MPBCIRC)        OPERAND   1  - CIRCULATION                   
         DC    AL1(MPQ#)           OPERAND   1  - NUMBER                        
         DC    XL3'00'                                                          
         DC    AL4(OPT020-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  RPC RANKED                                   
         DC    AL1(PLEN),C'P',AL1(MPBRPC,MPQRANK)                               
         DC    AL2(MPRANK)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG                                 
         DC    X'00'               SUFFIXABLE                                   
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBRPC)         OPERAND   1  - RPC                           
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANKED                        
         DC    XL5'0'                                                           
         DC    AL4(OPT013D-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                
         SPACE                                                                  
*                                  VRPC NUMBER                                  
         DC    AL1(PLEN),C'P',AL1(MPBVRPC,MPQ#)                                 
         DC    AL2(MPREACH+MPVXV+MPRANK)                                        
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG (NOT SUFF)                      
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'82'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBVRDR)        OPERAND   1  - VRDR                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPDIV)        OPERATOR  1  - OP1  /  OP2                   
         DC    AL1(MPBCIRC)        OPERAND   2  - CIRCULATION                   
         DC    AL1(MPQ#)           QUALIFIER 2  - NUMBER                        
         DC    XL3'00'                                                          
         DC    AL4(OPT022-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  VRPC RANKED                                  
         DC    AL1(PLEN),C'P',AL1(MPBVRPC,MPQRANK)                              
         DC    AL2(MPRANK)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        CONTROL FLAG                                 
         DC    X'00'               SUFFIXABLE                                   
         DC    C'B'                INT DATA TYPE                                
         DC    X'02'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    AL1(MPBVRPC)        OPERAND   1  - VRPC                          
         DC    AL1(MPQ#)           QUALIFIER 1  - NUMBER                        
         DC    AL1(MPOPRNK)        OPERATOR  1  - RANKED                        
         DC    XL5'0'                                                           
         DC    AL4(OPT013E-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                
         SPACE                                                                  
*                                  COUNT 'N' (EXPOSURES)         FREQ           
         DC    AL1(PLEN),C'P',AL1(MPBCOUNT,MPQ#)                                
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        SUFFIX REQUIRED                              
         DC    X'01'               SUFFIX (COUNT 'N')                           
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                FORMULA - PERFORM A SUBROUTINE               
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR01)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT024-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COUNT '0-N' (EXPOSURES)       FREQ           
         DC    AL1(PLEN),C'P',AL1(MPBCOUNT,MPQ#)                                
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        SUFFIX REQUIRED                              
         DC    X'02'               SUFFIX (COUNT '0-N')                         
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                FORMULA - PERFORM A SUBROUTINE               
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR02)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT024-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COUNT '1-N' (EXPOSURES)       FREQ           
         DC    AL1(PLEN),C'P',AL1(MPBCOUNT,MPQ#)                                
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        SUFFIX REQUIRED                              
         DC    X'03'               SUFFIX (COUNT '1-N')                         
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS MEANINGFUL                       
         DS    0XL8                FORMULA - PERFORM A SUBROUTINE               
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR03)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT024-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COUNT 'N+' (EXPOSURES)        FREQ           
         DC    AL1(PLEN),C'P',AL1(MPBCOUNT,MPQ#)                                
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFR)        SUFFIX REQUIRED                              
         DC    X'04'               SUFFIX (COUNT 'N+')                          
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTYES)       DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                FORMULA - PERFORM A SUBROUTINE               
         DC    XL2'0'              FILL                                         
         DC    AL1(MPOPSUBR)       SUBROUTINE REQUIRED TO GET VALUE             
         DC    XL1'0'              FILL                                         
         DC    AL4(SUBR04)         A(SUBR) -GENERAL ELSE SUBR NUMBER            
         DC    AL4(OPT024-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  COUNT % TARGET                               
         DC    AL1(PLEN),C'P',AL1(MPBCOUNT,MPQPCTAR)                            
         DC    AL2(MPFREQ)                                                      
         DC    C'V'                VARIABLE PART OF DATA VECTOR                 
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFO)        SUFFIX OPTIONAL                              
         DC    X'01'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                FORMULA                                      
         DC    AL1(MPBCOUNT)       OPERAND   1 - COUNT                          
         DC    AL1(MPQ#)           QUALIFIER 1 - NUMBER                         
         DC    AL1(MPOPPCT)        OPERATOR  1 - PERCENT                        
         DC    AL1(MPBTARG)        OPERAND   2 - TARGET                         
         DC    AL1(MPQ#)           QUALIFER  2 - NUMBER                         
         DC    XL3'0'                                                           
         DC    AL4(OPT051-MPDITAB) ADDRESS OF OUTPUT BLOCK LIST                 
         SPACE                                                                  
*                                  TARGET AS RAW NUMBER - VALID FOR ALL         
*                                  BUT ONLY AS PART OF A FORMULA                
         DC    AL1(PLEN),C'P',AL1(MPBTARG,MPQ#)                                 
         DC    AL2(MPEVAL+MPFREQ+MPVXV+MPREACH+MPRANK)                          
         DC    C'G'                GLOBAL VECTOR                                
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        NOT SUFFIXABLE                               
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR    000'S                 
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(0)              NO OUTPUT BLOCK                              
         SPACE                                                                  
*                                  NUMBER OF DATA VECTORS. USED AS              
*                                  VEHICLE COUNT FOR AVERAGES                   
         DC    AL1(PLEN),C'P',AL1(MPBNDVS,MPQ#)                                 
         DC    AL2(MPEVAL+MPFREQ+MPVXV+MPREACH+MPRANK)                          
         DC    C'G'                GLOBAL VECTOR                                
         DC    X'00'               WEIGHT TYPE                                  
         DC    AL1(MPSUFFN)        NOT SUFFIXABLE                               
         DC    X'00'               SUFFIX                                       
         DC    C'B'                INT DATA TYPE                                
         DC    X'04'               INT DATA LENGTH                              
         DC    X'00'               DATA SCALING FACTOR                          
         DC    AL1(MPTOTNO)        DISP TOTALS NOT MEANINGFUL                   
         DS    0XL8                CALC FORMULA                                 
         DC    XL8'0'              NO FORMULA                                   
         DC    AL4(0)              NO OUTPUT BLOCK                              
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*        OUTPUT BLOCK DEFINITIONS                                               
*                                                                               
*        BYTE 1 -  LENGTH OF ENTRY                                              
*        BYTE 2 -  TABLE TYPE                                                   
*        BYTE 3 ON O/P BLOCK DEFINITION                                         
*                                                                               
OPT001   DC    AL1(13),C'O'        IMPRESSIONS AS A RAW NUMBER                  
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT001-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS AS A RAW NUMBER                  
         DC    AL1(3,1)            SCALING / DECIMALS                           
         DC    AL1(8)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT001-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS AS A RAW NUMBER                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(8)              OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(11,1)           TEXT WIDTH,LINES                             
         DC    AL4(TXT001A-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT002   DC    AL1(13),C'O'        IMPRESSIONS AS % TOTAL IMPRESSIONS           
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT002-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS AS % TOTAL IMPRESSIONS           
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT002-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS AS % TOTAL IMPRESSIONS           
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT002-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT003   DC    AL1(13),C'O'        IMPRESSIONS CUME                             
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT003-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS CUME                             
         DC    AL1(3,1)            SCALING / DECIMALS                           
         DC    AL1(8)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT003-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT004   DC    AL1(13),C'O'        IMPRESSIONS CUME%                            
         DC    AL1(0,1)            SCALING /DECIMALS                            
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT004-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS CUME%                            
         DC    AL1(0,2)            SCALING /DECIMALS                            
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT004-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT005   DC    AL1(13),C'O'        GROSS RATING POINTS                          
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT005-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS                          
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(7)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT005-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS                          
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT005-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT006   DC    AL1(13),C'O'        GROSS RATING POINTS AS % GRP                 
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)           TEXT WIDTH,LINES                              
         DC    AL4(TXT006-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS AS % GRP                 
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)           TEXT WIDTH,LINES                              
         DC    AL4(TXT006-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS AS % GRP                 
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,2)           TEXT WIDTH,LINES                              
         DC    AL4(TXT006-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT007   DC    AL1(13),C'O'        GROSS RATING POINTS CUME                     
         DC    AL1(0,1)            SCALING /DECIMALS                            
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(7,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT007-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS CUME                     
         DC    AL1(0,2)            SCALING /DECIMALS                            
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(7,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT007-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GROSS RATING POINTS CUME                     
         DC    AL1(0,0)            SCALING /DECIMALS                            
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(7,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT007-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT008   DC    AL1(13),C'O'        COST                                         
         DC    AL1(0,0)            SCALING /DECIMALS                            
         DC    AL1(12)             OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT008-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COST                                         
         DC    AL1(1,0)            SCALING / DECIMALS                           
         DC    AL1(12)             OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT009-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COST                                         
         DC    AL1(2,0)            SCALING DECIMALS                             
         DC    AL1(12)             OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT010-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COST                                         
         DC    AL1(3,0)            SCALING DECIMALS                             
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT011-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT009   DC    AL1(13),C'O'        REACH/COVERAGE AS A RAW NUMBER               
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT012-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        REACH/COVERAGE AS A RAW NUMBER               
         DC    AL1(3,1)            SCALING /DECIMALS                            
         DC    AL1(8)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT012-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT010   DC    AL1(13),C'O'        CIRCULATION                                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(12)             OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(11,1)           TEXT WIDTH,LINES                             
         DC    AL4(TXT013-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        CIRCULATION                                  
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT013A-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT011   DC    AL1(13),C'O'        INSERTION COUNT                              
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(4)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT014-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT012   DC    AL1(13),C'O'        COST PER THOUSAND                            
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT015-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013   DC    AL1(13),C'O'        GROSS RATINGS RANKED                         
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013A  DC    AL1(13),C'O'        COST RANKED                                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016A-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013B  DC    AL1(13),C'O'        CPT RANKED                                   
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016B-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013C  DC    AL1(13),C'O'        CIRC RANKED                                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016C-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013D  DC    AL1(13),C'O'        RPC RANKED                                   
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016D-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT013E  DC    AL1(13),C'O'        VRPC RANKED                                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT016E-MPDITAB) A(TEXT)                                     
         SPACE                                                                  
         DC    X'FF'                                                            
OPT014   DC    AL1(13),C'O'        CPP                                          
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT017-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT015   DC    AL1(13),C'O'        RCH % TARG                                   
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT018-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        RCH % TARG                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT018-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        RCH % TARG                                   
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT018-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT016   DC    AL1(13),C'O'        RCH CPM                                      
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT019-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT017   DC    AL1(13),C'O'        COMP AS RAW NUMBER                           
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT020-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COMP AS RAW NUMBER                           
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT020-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COMP AS RAW NUMBER                           
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT020-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT018   DC    AL1(13),C'O'        COMP RANKED                                  
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT021-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT019   DC    AL1(13),C'O'        COMP INDEX                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(5)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT022-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COMP INDEX                                   
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(5)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT022-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT020   DC    AL1(13),C'O'        RPC NUMBER                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT023-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT021   DC    AL1(13),C'O'        VRDR AS RAW NUMBER                           
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT024-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT022   DC    AL1(13),C'O'        VRPC AS RAW NUMBER                           
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT025-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT023   DC    AL1(13),C'O'        AUDIENCE AS NUMBER                           
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT026-MPDITAB) A(TEXT)                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT024   DC    AL1(13),C'O'        COUNT (EXPOSURES) AS RAW NUMBER              
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT027-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT026   DC    AL1(13),C'O'        REACH RANKED                                 
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT029-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT027   DC    AL1(13),C'O'        IMPRESSIONS % TARGET                         
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT030-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS % TARGET                         
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT030-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMPRESSIONS % TARGET                         
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT030-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT028   DC    AL1(13),C'O'        SUM AS RAW NUMBER                            
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT031-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT029   DC    AL1(13),C'O'        SUM AS % TARGET                              
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT032-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        SUM AS % TARGET                              
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT032-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        SUM AS % TARGET                              
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT032-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT030   DC    AL1(13),C'O'        SUM AS CPM                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(7,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT033-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT031   DC    AL1(13),C'O'        NET AS RAW NUMBER                            
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT034-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT032   DC    AL1(13),C'O'        NET AS % TARGET                              
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT035-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        NET AS % TARGET                              
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT035-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        NET AS % TARGET                              
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT035-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT033   DC    AL1(13),C'O'        NET AS CPM                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(7,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT036-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT034   DC    AL1(13),C'O'        DUPE AS RAW NUMBER                           
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT037-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT035   DC    AL1(13),C'O'        DUPE AS % TARGET                             
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(10,1)           TEXT WIDTH,LINES                             
         DC    AL4(TXT038-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        DUPE AS % TARGET                             
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(10,1)           TEXT WIDTH,LINES                             
         DC    AL4(TXT038-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        DUPE AS % TARGET                             
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(10,1)           TEXT WIDTH,LINES                             
         DC    AL4(TXT038-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
OPT036   DC    AL1(13),C'O'        DUPE AS CPM                                  
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(7)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT039-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT037   DS    0C                  NOT USED                                     
         SPACE                                                                  
OPT038   DC    AL1(13),C'O'        IMP '000                                     
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT041-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT039   DC    AL1(13),C'O'        IMP % TARGET                                 
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT042-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMP % TARGET                                 
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT042-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        IMP % TARGET                                 
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT042-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT040   DC    AL1(13),C'O'        IMP RANKED                                   
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT043-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT041   DC    AL1(13),C'O'        IMP CPM                                      
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(7)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(7,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT044-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT042   DC    AL1(13),C'O'        GRP                                          
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT045-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GRP                                          
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT045-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        GRP                                          
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(3,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT045-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT043   DC    AL1(13),C'O'        RCH '000                                     
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT046-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT044   DC    AL1(13),C'O'        RCH %TARG                                    
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT047-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        RCH %TARG                                    
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT047-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        RCH %TARG                                    
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'80'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(9,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT047-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT045   DC    AL1(13),C'O'        RCH RANK                                     
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(8,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT048-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT046   DC    AL1(13),C'O'        RCH CPM                                      
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(7)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(7,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT049-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT047   DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(5)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT050-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,3)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT050-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(4)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT050-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(4,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT050-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
OPT048   DC    AL1(13),C'O'        AF RANK                                      
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'S'                TEXT TYPE                                    
         DC    AL1(7,1)            TEXT WIDTH,LINES                             
         DC    AL4(TXT051-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT049   DC    AL1(13),C'O'        UNIQU CONTRIBUTION                           
         DC    AL1(3,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT052-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT050   DC    AL1(13),C'O'        CIRC INDEX                                   
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(5,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT053-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT051   DC    AL1(13),C'O'        COUNT % TARGET                               
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT054-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COUNT % TARGET                               
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT054-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        COUNT % TARGET                               
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(6,3)            TEXT WIDTH,LINES                             
         DC    AL4(TXT054-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
OPT052   DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,2)            SCALING / DECIMALS                           
         DC    AL1(5)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT055-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,3)            SCALING / DECIMALS                           
         DC    AL1(6)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT055-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,1)            SCALING / DECIMALS                           
         DC    AL1(4)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT055-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    AL1(13),C'O'        AF #                                         
         DC    AL1(0,0)            SCALING / DECIMALS                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    X'20'               EDIT CONTROLS                                
         DC    C'R'                TEXT TYPE                                    
         DC    AL1(4,2)            TEXT WIDTH,LINES                             
         DC    AL4(TXT055-MPDITAB) A(TEXT)                                      
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*              KEYWORD TRANSLATION TABLE                                        
*              -------------------------                                        
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL CODE                                       
*              BYTE  4    - MINIMUM LENGTH FOR COMPARE                          
*              BYTES 5-12 - EXTERNAL NAME                                       
*                                                                               
KEYTAB   DC    AL1(KLEN),C'K',AL1(21,05),CL8'RDREX'                             
KLEN     EQU   *-KEYTAB                                                         
         DC    AL1(KLEN),C'K',AL1(22,06),CL8'RDRWGT'                            
         DC    AL1(KLEN),C'K',AL1(23,05),CL8'WGTEX'                             
         DC    AL1(KLEN),C'K',AL1(24,02),CL8'SORT'                              
         DC    AL1(KLEN),C'K',AL1(25,02),CL8'SPACE'                             
         DC    AL1(KLEN),C'K',AL1(26,02),CL8'BLOCK'                             
         DC    AL1(KLEN),C'K',AL1(27,02),CL8'INSERTS'                           
         DC    AL1(KLEN),C'K',AL1(28,01),CL8'GAP'                               
         DC    AL1(KLEN),C'K',AL1(29,02),CL8'WIDTH'                             
         DC    AL1(KLEN),C'K',AL1(30,02),CL8'LASER'                             
         DC    AL1(KLEN),C'K',AL1(98,05),CL8'TRACE'                             
         DC    AL1(KLEN),C'K',AL1(99,06),CL8'VALTAB'                            
         EJECT                                                                  
*                                                                               
*              ANSWER TRANSLATION TABLE                                         
*              ------------------------                                         
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL CODE                                       
*              BYTE  4    - MINIMUM LENGTH FOR COMPARE                          
*              BYTES 5-12 - EXTERNAL NAME                                       
*                                                                               
ANSTAB   DC    AL1(ALEN),C'A',AL1(21,02),CL8'IH'                                
ALEN     EQU   *-ANSTAB                                                         
         DC    AL1(ALEN),C'A',AL1(22,02),CL8'OH'                                
         DC    AL1(ALEN),C'A',AL1(23,01),CL8'P'                                 
         DC    AL1(ALEN),C'A',AL1(24,01),CL8'S'                                 
         DC    AL1(ALEN),C'A',AL1(25,02),CL8'SURVEY'                            
         DC    AL1(ALEN),C'A',AL1(26,03),CL8'IMP'                               
         DC    AL1(ALEN),C'A',AL1(27,04),CL8'COMP'                              
         DC    AL1(ALEN),C'A',AL1(28,03),CL8'SEP'                               
         DC    AL1(ALEN),C'A',AL1(29,03),CL8'NARROW'                            
         DC    AL1(ALEN),C'A',AL1(30,02),CL8'WIDE'                              
         DC    AL1(ALEN),C'A',AL1(31,01),CL8'YES'                               
         DC    AL1(ALEN),C'A',AL1(32,01),CL8'NO'                                
         DC    AL1(ALEN),C'A',AL1(33,01),CL8'PARTIAL'                           
         DC    AL1(ALEN),C'A',AL1(99,03),CL8'DIE'                               
         EJECT                                                                  
*                                                                               
*              COMBINATION TABLE                                                
*              -----------------                                                
*                                                                               
*              BYTE  1    - ENTRY LENGTH                                        
*              BYTE  2    - TABLE TYPE                                          
*              BYTE  3    - INTERNAL KEYWORD CODE                               
*              BYTE  4    - INTERNAL ANSWER CODE                                
*              BYTES 5-6  - REPORT VALIDITY FLAGS                               
*              X'0001'    - SCHEDULE EVALUATION                                 
*                                                                               
         SPACE 2                                                                
COMBTAB  DS    0H                                                               
         DC    AL1(CLEN),C'C',AL1(21,21)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
CLEN     EQU   *-COMBTAB                                                        
         DC    AL1(CLEN),C'C',AL1(21,22)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(21,23)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(21,24)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(22,00)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(23,25)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(23,26)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(24,27)                                        
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(25,255)                                       
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(26,00)                                        
         DC    AL2(MPEVAL+MPFREQ+MPRANK)                                        
*                                                                               
         DC    AL1(CLEN),C'C',AL1(26,254)                                       
         DC    AL2(MPEVAL+MPFREQ+MPRANK)                                        
*                                                                               
         DC    AL1(CLEN),C'C',AL1(27,28)                                        
         DC    AL2(MPEVAL)                                                      
*                                                                               
         DC    AL1(CLEN),C'C',AL1(28,254)                                       
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(29,29)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(29,30)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(29,254)                                       
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(30,00)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(30,31)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(30,32)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(98,00)                                        
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(98,31)                                        
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(98,32)                                        
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(98,33)                                        
         DC    AL2(MPEVAL+MPREACH+MPVXV+MPRANK)                                 
*                                                                               
         DC    AL1(CLEN),C'C',AL1(99,00)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
*                                                                               
         DC    AL1(CLEN),C'C',AL1(99,99)                                        
         DC    AL2(MPEVAL+MPFREQ+MPREACH+MPVXV+MPRANK)                          
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
*                                                                               
*        FXDTAB - FIXED OFFSETS IN DATA VECTOR                                  
*                                                                               
*        BYTE  1      LENGTH OF ENTRY                                           
*        BYTE  2      TABLE TYPE                                                
*        BYTE  3      BASE CODE                                                 
*        BYTE  4      QUALIFIER CODE                                            
*        BYTES 5 - 8  OFFSET INTO DATA VECTOR                                   
*                                                                               
FXDTAB   DC    AL1(FLEN),C'F',AL1(MPBCOST,MPQ#)   COST                          
         DC    AL4(VTCOST-VTABD)                                                
FLEN     EQU   *-FXDTAB                                                         
         DC    AL1(FLEN),C'F',AL1(MPBCIRC,MPQ#)   CIRC                          
         DC    AL4(VTCIRC-VTABD)                                                
*                                                                               
         DC    AL1(FLEN),C'F',AL1(MPBINS,MPQ#)    INSERTION COUNT               
         DC    AL4(VTINS-VTABD)                                                 
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
*                                                                               
*        GBLTAB - FIXED OFFSETS IN READREX CONTROL BLOCK                        
*                                                                               
*        BYTE  1      LENGTH OF ENTRY                                           
*        BYTE  2      TABLE TYPE                                                
*        BYTE  3      BASE CODE                                                 
*        BYTE  4      QUALIFIER CODE                                            
*        BYTES 5 - 8  OFFSET INTO RXCTD                                         
*                                                                               
GBLTAB   DC    AL1(GLEN),C'G',AL1(50,01)          TARGET POPULATION             
         DC    AL4(RXTRGPOP-RXCTLD)                                             
GLEN     EQU   *-GBLTAB                                                         
         DC    AL1(GLEN),C'G',AL1(51,01)          VECTOR COUNT                  
         DC    AL4(RXNDVS-RXCTLD)                                               
*                                                                               
         DC    AL1(GLEN),C'G',AL1(00,00)   **TBD**BASE POPULATION               
         DC    AL4(RXBPOP-RXCTLD)                                               
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
SUBR01   EQU   01                                                               
SUBR02   EQU   02                                                               
SUBR03   EQU   03                                                               
SUBR04   EQU   04                                                               
SUBR05   EQU   05                                                               
SUBR06   EQU   06                                                               
SUBR07   EQU   07                                                               
SUBR08   EQU   08                                                               
SUBR09   EQU   09                                                               
         SPACE 5                                                                
TXT001   DC    C' IMP',C'''000'                                                 
TXT001A  DC    C'IMPRESSIONS'                                                   
TXT002   DC    C'IMP',C'  %'                                                    
TXT003   DC    C' IMP',C'CUME'                                                  
TXT004   DC    C'  IMP',C'CUME%',C' TARG'                                       
TXT005   DC    C' GROSS',C'RATING',C'POINTS'                                    
TXT006   DC    C'GRP',C' % '                                                    
TXT007   DC    C' GROSS ',C'RATINGS',C' CUME  '                                 
TXT008   DC    C'COST',C'   $'                                                  
TXT009   DC    C'COST',C'$/10'                                                  
TXT010   DC    C' COST',C'$/100'                                                
TXT011   DC    C'  COST',C'$/1000'                                              
TXT012   DC    C'RCHXX',C' ''000'                                               
TXT013   DC    C'CIRCULATION'                                                   
TXT013A  DC    C' CIRC',C'000''S'                                               
TXT014   DC    C'INS'                                                           
TXT015   DC    C'CPT'                                                           
TXT016   DC    C' GRP',C'RANK'                                                  
TXT016A  DC    C'COST',C'RANK'                                                  
TXT016B  DC    C' CPT',C'RANK'                                                  
TXT016C  DC    C'CIRC',C'RANK'                                                  
TXT016D  DC    C'  RPC',C'(TGT)',C' RANK'                                       
TXT016E  DC    C'  RPC',C'(TOT)',C' RANK'                                       
TXT017   DC    C'  COST',C'   PER',C'RATING'                                    
TXT018   DC    C' RCHXX',C'   %  ',C'TARGET'                                    
TXT019   DC    C'RCHXX',C'  CPM'                                                
TXT020   DC    C'COMP'                                                          
TXT021   DC    C'COMP',C'RANK'                                                  
TXT022   DC    C' COMP',C'INDEX'                                                
TXT023   DC    C'RPC'                                                           
TXT024   DC    C'VRDR'                                                          
TXT025   DC    C'VRPC'                                                          
TXT026   DC    C' AUD',C'''000'                                                 
TXT027   DC    C'COUNT'                                                         
TXT029   DC    C'RCHXX',C' RANK'                                                
TXT030   DC    C' IMP %',C'TARGET'                                              
TXT031   DC    C'SUM ''000'                                                     
TXT032   DC    C'SUM %TARG'                                                     
TXT033   DC    C'SUM CPM'                                                       
TXT034   DC    C'NET ''000'                                                     
TXT035   DC    C'NET %TARG'                                                     
TXT036   DC    C'NET CPM'                                                       
TXT037   DC    C'DUPE ''000'                                                    
TXT038   DC    C'DUPE %TARG'                                                    
TXT039   DC    C'DUPE CPM'                                                      
TXT040   DC    C'   IMP',C'    % ',C'TARGET'                                    
TXT041   DC    C'IMP ''000'                                                     
TXT042   DC    C'IMP %TARG'                                                     
TXT043   DC    C'IMP RANK'                                                      
TXT044   DC    C'IMP CPM'                                                       
TXT045   DC    C'GRP'                                                           
TXT046   DC    C'RCH ''000'                                                     
TXT047   DC    C'RCH %TARG'                                                     
TXT048   DC    C'RCH RANK'                                                      
TXT049   DC    C'RCH CPM'                                                       
TXT050   DC    C'AF #'                                                          
TXT051   DC    C'AF RANK'                                                       
TXT052   DC    C'UNIQ''000'                                                     
TXT053   DC    C' CIRC',C'INDEX'                                                
TXT054   DC    C' COUNT',C'   %  ',C'TARGET'                                    
TXT055   DC    C' AVE',C'FREQ'                                                  
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE MPDITABD                                                       
       ++INCLUDE MPVTABD                                                        
       ++INCLUDE MPRXCTLD                                                       
       ++INCLUDE MPEQUATES                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004MPDITAB   09/25/00'                                      
         END                                                                    
