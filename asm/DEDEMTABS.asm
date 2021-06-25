*          DATA SET DEDEMTABS  AT LEVEL 097 AS OF 03/05/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00AD1C                                                                  
         TITLE 'LOADABLE TABLES FOR INTERNAL USE BY DEMO MODULES'               
***********************************************************************         
*                                                                     *         
* THIS ROUTINE CONTAINS VARIOUS TABLES USED BY THE DEMOGRAPHICS       *         
* MODULES. ONLY RELATIVELY SMALL TABLES SHOULD BE KEPT HERE (THIS IS  *         
* A CORE-RESIDENT PHASE!) LARGE TABLES SHOULD BE MAINTAINED IN THE    *         
* DATASPACE.                                                          *         
*                                                                     *         
* ON INPUT:  P1 = EQUATED VALUE FOR DESIRED TABLE (SEE DEDEMTABD)     *         
* ON OUTPUT: P1 = RELOCATED A(TABLE), OR NULLS IF P1 IS INVALID       *         
*            P2 = L'TABLE ENTRY (FOR TABLES WITH FIXED-LENGTH ENTRIES,*         
*                 ELSE NULLS)                                         *         
*                                                                     *         
* NOTE: FOR TABLES WITH FIXED-LENGTH ENTRIES, IT IS ABSOLUTELY        *         
*       MANDATORY TO USE THE RETURNED TABLE ENTRY LENGTH IN P2, SO    *         
*       THAT ANY TABLE CAN BE MADE WIDER IF NECESSARY. DO NOT USE AN  *         
*       EQUATED SYMBOL TO REFER TO A TABLE ENTRY OF FIXED-LENGTH!!!   *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  SPOT/REP REMINDERS                                                 *         
*  ------------------                                                 *         
*  REMINDER - THE FOLLOWING TABLES HAS TO BE UPDATED EVERY YEAR -BEN  *         
*  1- TBL_SWEEPTBL                                                    *         
*                                                                     *         
*  REMINDER - TABLES THAT NEED TO BE UPDATED FOR NEW LPM MARKETS- BEN *         
*  1-TBL_FUSNENDP                                                     *         
*  2-TBL_LPMUPG                                                       *         
*  3-TBL_LPMSEP                                                       *         
*  4-TBL_WKLYLPM                                                      *         
*                                                                     *         
*  REMINDER - TABLE THAT NEEDS TO BE CHANGED FOR SPOT/REP CABLE       *         
*             CALL LETTER LINKS                                       *         
*  1- TBL_LOCCABLE                                                    *         
*                                                                     *         
*  NET REMINDERS                                                      *         
*  -------------                                                      *         
*  THE FOLLOWING TABLES NEED TO BE UPDATED FOR NEW OR CHANGES TO      *         
*  CABLE NETWORKS                                                     *         
*  1- TBL_NECABCLL         CABLE CALL LETTERS (INCLUDES DUPLICATES)   *         
*  2- TBL_NECABNAM         CABLE NETWORK NAMES (NO DUPLICATES)        *         
*  3- TBL_NADTPB           CABLE NETWORKS WITH ONLY TP DATA           *         
*                                                                     *         
*  THE FOLLOWING TABLES NEED TO BE UPDATED FOR NEW OR CHANGES TO      *         
*  HISPANIC CABLE NETWORKS                                            *         
*  1- TBL_NEHCBCLL         HISP CABLE CALL LETTERS (INCL DUPLICATES)  *         
*  2- TBL_NEHCBNAM         HISP CABLE NETWORK NAMES (NO DUPLICATES)   *         
*  3- TBL_NADTPB           HISP CABLE NETWORKS WITH ONLY TP DATA      *         
*                                                                     *         
*  OTHER TABLES OF NETWORKS                                           *         
*  TBL_NEBROD              BROADCAST NETWORKS                         *         
*  TBL_NESYND              SYNDICATION NETWORKS                       *         
*  TBL_NEHBRO              HISPANIC TV AUDIENCE BROADCAST NETWORKS    *         
*  TBL_NEHGES              HISPANIC GENERAL MARKET ENGLISH NETWORKS   *         
*  TBL_NEBAGG              BROADCAST AGGREGATES                       *         
*  TBL_NEHTAG              HISPANIC TV AUDIENCE AGGREGATES            *         
*  TBL_NEHGAG              HISPANIC GENERAL MARKET AGGREGATES         *         
*                                                                     *         
***********************************************************************         
* MACRO DEFINITIONS                                                             
         SPACE 3                                                                
         MACRO                                                                  
         TABLE_LEN &TABLE_START                                                 
         PUSH PRINT                                                             
         PRINT GEN                                                              
TABLE_LEN&SYSNDX EQU *-&TABLE_START                                             
         PRINT NOGEN                                                            
         POP PRINT                                                              
         MEND                                                                   
         SPACE 3                                                                
** MACRO DEFINITIONS                                                            
** ++INCLUDE DEDEMTABM                                                          
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABM                                                      
         PRINT ON                                                               
         EJECT                                                                  
DEMTABS  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*DEMTABS,RR=R4                                                 
         LR    R9,R1               A(PARAMETER LIST)                            
*                                                                               
         ICM   R3,15,0(R9)         TABLE NUMBER                                 
         JNP   *+12                MUST BE A POSITIVE NUMBER!                   
         CHI   R3,NUM_TABLES       VALID TABLE NUMBER?                          
         JNH   *+14                YES                                          
         XC    0(4,R9),0(R9)       CLEAR RETURN ADDRESS                         
         J     XIT                 CALLER PASSED BAD TABLEID                    
*                                                                               
         BCTR  R3,0                NUMBERS ARE ONE-BASED                        
         SLL   R3,2                DISPLACEMENT TO TABLE ENTRY (X4)             
         LA    R3,TBLADDRS(R3)     INDEX INTO TABLE                             
         L     R1,0(R3)            A(TABLE)                                     
         AR    R1,R4               RELOCATE ADCON                               
*                                                                               
         LH    R0,14(R1)           L'TABLE ENTRY                                
         ST    R0,4(R9)            RETURN IN P2                                 
         AHI   R1,16               L'EYECATCHER + L'SPARE + L'LENGTH            
         ST    R1,0(R9)            RETURN RELOCATED TABLE ADDRESS               
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* TABLE OF TABLES                                                               
*                                                                               
* **** THIS TABLE IS INDEXED BY THE TABLE'S EQUATED VALUE.                      
* **** ALL ENTRIES MUST BE PRESENT AND IN SEQUENCE, INCLUDING SPARE!!!          
*                                                                               
TBLADDRS DS    0A                                                               
*=======================================================                        
*              A(INTERNAL TABLE)   EQU FROM DEDEMTABD                           
*=======================================================                        
         DC    A(TBL_FUSNENDP)     FUSNENDP                                     
         DC    A(TBL_LPMUPG)       LPMUPG                                       
         DC    A(TBL_ONITEMKT)     ONITEMKT                                     
         DC    A(0)                ** AVAILABLE **                              
         DC    A(TBL_VWTYPTTB)     VWTYPTTB                                     
         DC    A(TBL_VWDESCTB)     VWDESCTB                                     
         DC    A(TBL_WKLYLPM)      WKLYLPM                                      
         DC    A(TBL_ROLLBKT)      ROLLPBKT                                     
         DC    A(TBL_SWEEPTBL)     SWEEPTBL                                     
         DC    A(TBL_SMETERT)      SETMETER                                     
         DC    A(TBL_SPBOOKTB)     SPBOOKTB                                     
         DC    A(TBL_LPMSEP)       LPMSTEND                                     
         DC    A(TBL_NSICBL)       NSICABLE                                     
         DC    A(TBL_LVBKYP)       LIVEBKTY                                     
         DC    A(TBL_LOCCABLE)     LOCALCAB                                     
         DC    A(TBL_HOMESTA)      HOMESTA                                      
         DC    A(TBL_PPMTAB)       PPMTAB                                       
         DC    A(TBL_MTHDATES)     MTHDATES                                     
         DC    A(TBL_ACTTABLE)     ACTTABLE                                     
         DC    A(TBL_ACTTABW)      ACTTABW                                      
         DC    A(TBL_ACTTABWC)     ACTTABWC                                     
         DC    A(TBL_NECABCLL)     NECABCLL                                     
         DC    A(TBL_NEHCBCLL)     NEHCBCLL                                     
         DC    A(TBL_NECABNAM)     NECABNAM                                     
         DC    A(TBL_NEHCBNAM)     NEHCBNAM                                     
         DC    A(TBL_NEBROD)       NEBROD                                       
         DC    A(TBL_NESYND)       NESYND                                       
         DC    A(TBL_NEHBRO)       NEHBRO                                       
         DC    A(TBL_NEBAGG)       NEBAGG                                       
         DC    A(TBL_NEHTAG)       NEHTAG                                       
         DC    A(TBL_NEHGAG)       NEHGAG                                       
         DC    A(TBL_SEPT_5WK)     SEPT_5WK                                     
         DC    A(TBL_NEHGES)       NEHGES                                       
         DC    A(TBL_CALL_LNK)     CALL_LNK                                     
         DC    A(TBL_NENACWKS)     NENACWKS                                     
         DC    A(TBL_NENTIQRT)     NENTIQRT                                     
         DC    A(TBL_NETVQCAL)     NETVQCAL                                     
         DC    A(TBL_MOVIEGOU)     MOVIEGOU                                     
         DC    A(TBL_NADTPB)       NADTPB                                       
         DC    A(TBL_UNIVYRS)      UNIVYRS                                      
         DC    A(TBL_ITNPNAME)     ITNPNAME                                     
         DC    A(TBL_RADNMKTS)     RAD_NMKT                                     
         DC    A(TBL_VTYPTRN)      VTYPTRN                                      
         DC    A(TBL_RECABCLL)     RECABCLL                                     
         DC    A(TBL_RECABNAM)     RECABNAM                                     
         DC    A(TBL_ARDNTRN)      ARDNTRN                                      
         DC    A(TBL_NSIMKTS)      NSI MARKETS                                  
         DC    A(TBL_FQABOOKS)     FQABOOKS                                     
         DC    A(TBL_CDMTAB)       CDMTAB                                       
NUM_TABLES EQU (*-TBLADDRS)/4                                                   
         SPACE 3                                                                
DEMTABS  RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* THE STRUCTURE OF EACH TABLE MUST BE:                                          
* 1:   DS 0D (FORCE DOUBLEWORD ALIGNMENT)                                       
* 2:   8-BYTE EYE-CATCHER (MUST DUPLICATE THIS IN TBLADDRS ABOVE!)              
* 3:   6-BYTE SPARE                                                             
* 4:   2-BYTE L'TABLE ENTRY (IF ENTRIES ARE FIXED LENGTH, ELSE NULLS)           
* 5:   TABLE ENTRIES                                                            
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
*-----------------BELOW COMMENTS NO LONGER APPLIES ----------------             
*-----------------DUE TO AIUE FUSION GETS 12 BOOKS ----------------             
*-----------------WE CAN ADD MKTS AHEAD OF TIME IN THIS TABLE------             
*                                                                               
* THIS TABLE IMPACTS SPOT FUSION POSTING                                        
* ANY TIME THE MKT IS IN THIS TABLE AND THE BUY EST END DATE IS NOT             
* LATER THAN THE LIVE BOOK IN THIS TABLE-IT IS TREATED AS LPM AND               
* BOOKS ARE POSTING OFF SWEEP DATES WHILE NON LPM POST OFF BROADCAST            
* DATES                                                                         
* ITS IMPORTANT THAT WE CAN'T ENTER NEW LPM MKTS TOO EARLY OR WE                
* WE MESS UP POSTING.                                                           
*                                                                               
***********************************************************************         
***********************************************************************         
                                                                                
* TABLE OF FUSION END DATES (LPM MARKETS)                                       
TBL_FUSNENDP DS 0D                                                              
* (FROM PAN BOOK DEFUSEND)                                                      
* DATE GIVEN IS THE FIRST MONTH FOR WHICH WE DO NOT HAVE FUSION DATA            
         DC    CL8'FUSNENDP'                                                    
         DS    XL6                                                              
         DC    AL2(FUSENDLQ)       L'TABLE ENTRY                                
*                                                                               
         DC    CL3'NY ',AL2(101,JUL_04),CL3'NYK'                                
         DC    CL3'LA ',AL2(403,JUL_04),CL3'LAX'                                
         DC    CL3'CHI',AL2(202,AUG_04),CL3'CHI'                                
         DC    CL3'SF ',AL2(407,OCT_04),CL3'SNF'                                
         DC    CL3'BOS',AL2(106,FEB_02),CL3'BOS'                                
         DC    CL3'PHL',AL2(104,JUL_05),CL3'PHI'                                
         DC    CL3'WAS',AL2(111,JUL_05),CL3'WAS'                                
         DC    CL3'DET',AL2(105,JAN_06),CL3'DET'                                
         DC    CL3'DF ',AL2(223,JAN_06),CL3'DAL'                                
         DC    CL3'ATL',AL2(168,JUL_06),CL3'ATL'                                
         DC    CL3'ATL',AL2(124,JUL_06),CL3'ATL'                                
         DC    CL3'HOU',AL2(218,OCT_07),CL3'HOU'                                
         DC    CL3'SEA',AL2(419,OCT_07),CL3'SEA'                                
         DC    CL3'TAM',AL2(139,OCT_07),CL3'TAM'                                
         DC    CL3'PHX',AL2(353,APR_08),CL3'PHO'                                
         DC    CL3'CLE',AL2(110,SEP_08),CL3'CLE'                                
         DC    CL3'MIN',AL2(213,SEP_08),CL3'MSP'                                
         DC    CL3'DEN',AL2(351,OCT_08),CL3'DEN'                                
         DC    CL3'MF ',AL2(128,OCT_08),CL3'MFH'                                
         DC    CL3'ORL',AL2(134,JAN_09),CL3'ORL'                                
         DC    CL3'SAC',AL2(462,JAN_09),CL3'SAC'                                
         DC    CL3'STL',AL2(209,JAN_09),CL3'STL'                                
         DC    CL3'PIT',AL2(108,JUL_09),CL3'PIT'                                
         DC    CL3'PTO',AL2(420,JUL_09),CL3'POO'                                
         DC    CL3'BAL',AL2(112,JUL_09),CL3'BAL'                                
         DC    CL3'CHL',AL2(117,JAN_10),CL3'CHL'                                
*&&DO                                                                           
         DC    CL3'IND',AL2(127,APR_10),CL3'IND'                                
         DC    CL3'SD ',AL2(425,APR_10),CL3'SND'                                
         DC    CL3'RAL',AL2(160,APR_10),CL3'RAL'                                
         DC    CL3'HAT',AL2(133,APR_10),CL3'HNH'                                
         DC    CL3'NAS',AL2(259,AUG_10),CL3'NAS'                                
         DC    CL3'KC ',AL2(216,AUG_10),CL3'KAN'                                
         DC    CL3'CLO',AL2(135,AUG_10),CL3'COO'                                
         DC    CL3'CIN',AL2(115,OCT_10),CL3'CIN'                                
         DC    CL3'MIL',AL2(217,OCT_10),CL3'MIL'                                
         DC    CL3'SLC',AL2(370,FEB_11),CL3'SLC'                                
         DC    CL3'GVS',AL2(167,FEB_11),CL3'GRS'                                
         DC    CL3'SAN',AL2(241,APR_11),CL3'SNT'                                
         DC    CL3'WPB',AL2(148,APR_11),CL3'WPB'                                
         DC    CL3'BIR',AL2(230,AUG_11),CL3'BIR'                                
         DC    CL3'NOR',AL2(144,AUG_11),CL3'NPN'                                
         DC    CL3'LAS',AL2(439,AUG_11),CL3'LAS'                                
         DC    CL3'ABQ',AL2(125,AUG_11),CL3'ALQ'                                
         DC    CL3'OKC',AL2(250,OCT_11),CL3'OKL'                                
         DC    CL3'GWH',AL2(118,OCT_11),CL3'GWH'                                
         DC    CL3'MEM',AL2(240,FEB_12),CL3'MEM'                                
         DC    CL3'LOU',AL2(129,FEB_12),CL3'LOU'                                
         DC    CL3'JKV',AL2(161,FEB_12),CL3'JAX'                                
         DC    CL3'BUF',AL2(114,FEB_12),CL3'BUF'                                
         DC    CL3'AUS',AL2(235,APR_12),CL3'AUS'                                
         DC    CL3'PRV',AL2(121,APR_12),CL3'PRO'                                
         DC    CL3'NOL',AL2(222,APR_12),CL3'NWO'                                
         DC    CL3'KNX',AL2(157,APR_12),CL3'KNO'                                
         DC    CL3'RCH',AL2(156,OCT_12),CL3'RIC'                                
         DC    CL3'TUL',AL2(271,OCT_12),CL3'TUL'                                
         DC    CL3'DAY',AL2(142,OCT_12),CL3'DAY'                                
         DC    CL3'FM ',AL2(171,OCT_12),CL3'FTM'                                
*&&                                                                             
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_FUSNENDP                                                 
         EJECT                                                                  
***********************************************************************         
* TABLE FOR -   LPM UPGRADE  AND PARALLEL BOOK TO USE FOR INDEX                 
* TABLE OF LPM MARKETS (FOR UPGRADES)                                           
* TABLE IS USED IN SPDEMUP/REDEMUP FOR LPM INDEX UPGRADE FORMULA                
TBL_LPMUPG DS 0D                                                                
* (WAS HARD-CODED IN SPDEMUP AND REDEMUP)                                       
         DC    CL8'*LPMUPG*'                                                    
         DS    XL6                                                              
         DC    AL2(LPMUDATQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL2(101,MAY_04)     NY                                           
         DC    AL2(106,MAY_04)     BOSTON                                       
         DC    AL2(202,JUL_04)     CHI                                          
         DC    AL2(403,MAY_04)     LA                                           
         DC    AL2(407,NOV_04)     SF                                           
         DC    AL2(104,MAY_05)     PHL                                          
         DC    AL2(111,MAY_05)     WAS                                          
         DC    AL2(105,NOV_05)     DET                                          
         DC    AL2(223,NOV_05)     DALLAS FT WORTH                              
         DC    AL2(168,MAY_06)     ATLANTA                                      
         DC    AL2(124,MAY_06)     ATLANTA EXTENDED                             
         DC    AL2(218,JUL_07)     HOUSTON                                      
         DC    AL2(419,JUL_07)     SEATTLE                                      
         DC    AL2(139,JUL_07)     TAMPA                                        
         DC    AL2(353,FEB_08)     PHOENIX                                      
         DC    AL2(110,JUL_08)     CLEVELAND                                    
         DC    AL2(213,JUL_08)     MIN                                          
         DC    AL2(351,JUL_08)     DEN                                          
*********DC    AL2(128,JUL_08)     MIAMI                                        
         DC    AL2(134,NOV_08)     ORL                                          
         DC    AL2(462,NOV_08)     SAC                                          
         DC    AL2(209,NOV_08)     STL                                          
         DC    AL2(108,MAY_09)     PIT                                          
         DC    AL2(420,MAY_09)     PTO                                          
         DC    AL2(112,MAY_09)     BAL                                          
         DC    AL2(117,NOV_09)     CHL                                          
*                                                                               
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_LPMUPG                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF LPM START AND END BOOKS                                              
* PARALLEL PERIOD FOR LPM MARKETS                                               
* (WAS HARD-CODED IN SPDEMUP AND REDEMUP,DEGETTP, REFETCH, RERMP18)             
*  ONLY HAVE TO ADD NEW LPM ENTRIES ONCE NIELSEN LIFTS PRELIM                   
*  RESTRICTIONS                                                                 
*                                                                               
* DEGETTP- CHANGES PRELIMINARY BOOKTYPE TO LIVE LPM BOOKTYPE FOR LOOKUP         
*          BOOK LATER THAN END PRELIMINARY BOOK                                 
*                                                                               
* SPDEMUP/                                                                      
* REDEMUP- FOR MKTS IN TABLE EITHER SET THE BOOKTYPE TO PRELIMINARY             
*          OR SET THE BOOKTYPE TO LIVE LPM BOOKTYPE DEPENDED IF BOOK            
*          IS BETWEEN PRELIMINARY PERIOD                                        
*                                                                               
* REFETCH- CHECKED AGAINST TABLE TO DETERMINE IF MKT IS LPM TO FORMAT           
*          UPGRADE FORMULA STRING                                               
***********************************************************************         
TBL_LPMSEP DS 0D                                                                
* (WAS HARD-CODED IN SPDEMUP AND REDEMUP,DEGETTP, REFETCH, RERMP18)             
         DC    CL8'*LPMSEP*'                                                    
         DS    XL6                                                              
         DC    AL2(LPMSETQ)        L'TABLE ENTRY                                
*                                                                               
* BOOKTYPE P                                                                    
         LPMSEP BOOKTYPE_P,101,APR_04,AUG_04,NY        NY                       
         LPMSEP BOOKTYPE_P,106,MAY_01,APR_02,BOS       BOSTON                   
         LPMSEP BOOKTYPE_P,202,JUL_04,SEP_04,CHI       CHI                      
         LPMSEP BOOKTYPE_P,403,MAY_04,JUL_04,LA        LA                       
         LPMSEP BOOKTYPE_P,407,OCT_04,DEC_04,SF        SF                       
         LPMSEP BOOKTYPE_P,104,MAY_05,JUN_05,PHL       PHL                      
         LPMSEP BOOKTYPE_P,111,MAY_05,JUN_05,WAS       WAS                      
         LPMSEP BOOKTYPE_P,105,NOV_05,DEC_05,DET       DET                      
         LPMSEP BOOKTYPE_P,223,NOV_05,DEC_05,DF        DALLAS FT WORTH          
         LPMSEP BOOKTYPE_P,168,APR_06,JUN_06,ATL       ATLANTA                  
         LPMSEP BOOKTYPE_P,124,APR_06,JUN_06,ATL       ATLANTA EXTENDED         
         LPMSEP BOOKTYPE_P,218,JUL_07,SEP_07,HOU       HOUSTON                  
         LPMSEP BOOKTYPE_P,419,JUL_07,SEP_07,SEA       SEATTLE                  
         LPMSEP BOOKTYPE_P,139,JUL_07,SEP_07,TAM       TAMPA                    
         LPMSEP BOOKTYPE_P,353,JAN_08,MAR_08,PHX       PHOENIX                  
         LPMSEP BOOKTYPE_P,110,JUN_08,AUG_08,CLE       CLEVELAND                
         LPMSEP BOOKTYPE_P,213,JUN_08,AUG_08,MIN       MINN                     
         LPMSEP BOOKTYPE_P,351,JUL_08,SEP_08,DEN       DENVER                   
         LPMSEP BOOKTYPE_P,134,NOV_08,DEC_08,ORL       ORLANDO                  
         LPMSEP BOOKTYPE_P,462,NOV_08,DEC_08,SAC       SACRAMENTO               
         LPMSEP BOOKTYPE_P,209,NOV_08,DEC_08,STL       ST LOUIS                 
         LPMSEP BOOKTYPE_P,108,APR_09,JUN_09,PIT       PITTSBURGH               
         LPMSEP BOOKTYPE_P,420,APR_09,JUN_09,PTO       PORTLAND                 
         LPMSEP BOOKTYPE_P,112,APR_09,JUN_09,BAL       BALTIMORE                
         LPMSEP BOOKTYPE_P,117,OCT_09,DEC_09,CHL       CHARLOTTE                
*********LPMSEP BOOKTYPE_P,128,JUL_08,SEP_08,MF        MIAMI                    
*                                                                               
*STANDARD BOOKTYPE                                                              
         LPMSEP BOOKTYPE_STANDARD,101,APR_04,AUG_04,NY     NY                   
         LPMSEP BOOKTYPE_STANDARD,106,MAY_01,APR_02,BOS    BOSTON               
         LPMSEP BOOKTYPE_STANDARD,202,JUL_04,SEP_04,CHI    CHI                  
         LPMSEP BOOKTYPE_STANDARD,403,MAY_04,JUL_04,LA     LA                   
         LPMSEP BOOKTYPE_STANDARD,407,OCT_04,DEC_04,SF     SF                   
         LPMSEP BOOKTYPE_STANDARD,104,MAY_05,JUN_05,PHL    PHL                  
         LPMSEP BOOKTYPE_STANDARD,111,MAY_05,JUN_05,WAS    WAS                  
         LPMSEP BOOKTYPE_STANDARD,105,NOV_05,DEC_05,DET    DET                  
         LPMSEP BOOKTYPE_STANDARD,223,NOV_05,DEC_05,DF     DAL/FT.WORTH         
         LPMSEP BOOKTYPE_STANDARD,168,APR_06,JUN_06,ATL    ATLANTA              
         LPMSEP BOOKTYPE_STANDARD,124,APR_06,JUN_06,ATL    ATL. EXTEND.         
         LPMSEP BOOKTYPE_STANDARD,218,JUL_07,SEP_07,HOU    HOUSTON              
         LPMSEP BOOKTYPE_STANDARD,419,JUL_07,SEP_07,SEA    SEATTLE              
         LPMSEP BOOKTYPE_STANDARD,139,JUL_07,SEP_07,TAM    TAMPA                
         LPMSEP BOOKTYPE_STANDARD,353,JAN_08,MAR_08,PHX    PHOENIX              
         LPMSEP BOOKTYPE_STANDARD,110,JUN_08,AUG_08,CLE    CLEVELAND            
         LPMSEP BOOKTYPE_STANDARD,213,JUN_08,AUG_08,MIN    MINN                 
         LPMSEP BOOKTYPE_STANDARD,351,JUL_08,SEP_08,DEN    DENVER               
         LPMSEP BOOKTYPE_STANDARD,134,NOV_08,DEC_08,ORL    ORLANDO              
         LPMSEP BOOKTYPE_STANDARD,462,NOV_08,DEC_08,SAC    SACRAMENTO           
         LPMSEP BOOKTYPE_STANDARD,209,NOV_08,DEC_08,STL    ST LOUIS             
         LPMSEP BOOKTYPE_STANDARD,108,APR_09,JUN_09,PIT    PITTSBURGH           
         LPMSEP BOOKTYPE_STANDARD,420,APR_09,JUN_09,PTO    PORTLAND             
         LPMSEP BOOKTYPE_STANDARD,112,APR_09,JUN_09,BAL    BALTIMORE            
         LPMSEP BOOKTYPE_STANDARD,117,OCT_09,DEC_09,CHL    CHARLOTTE            
*********LPMSEP BOOKTYPE_STANDARD,128,JUL_08,SEP_08,MF     MIAMI                
*                                                                               
* BOOKTYPE I                                                                    
         LPMSEP BOOKTYPE_I,101,APR_04,AUG_04,NY         NY                      
         LPMSEP BOOKTYPE_I,202,JUL_04,SEP_04,CHI        CHI                     
         LPMSEP BOOKTYPE_I,407,OCT_04,DEC_04,SF         SF                      
         LPMSEP BOOKTYPE_I,218,JUL_07,SEP_07,HOU        HOUSTON                 
         LPMSEP BOOKTYPE_I,139,JUL_07,SEP_07,TAM        TAMPA                   
         LPMSEP BOOKTYPE_I,353,JAN_08,MAR_08,PHX        PHOENIX                 
         LPMSEP BOOKTYPE_I,351,JUL_08,SEP_08,DEN        DENVER                  
         LPMSEP BOOKTYPE_I,134,NOV_08,DEC_08,ORL        ORLANDO                 
         LPMSEP BOOKTYPE_I,462,NOV_08,DEC_08,SAC        SACRAMENTO              
*********LPMSEP BOOKTYPE_I,128,JUL_08,SEP_08,MF         MIAMI                   
*                                                                               
* BOOKTYPE H                                                                    
         LPMSEP BOOKTYPE_H,101,APR_04,AUG_04,NY         NY                      
         LPMSEP BOOKTYPE_H,202,JUL_04,SEP_04,CHI        CHI                     
         LPMSEP BOOKTYPE_H,407,OCT_04,DEC_04,SF         SF                      
         LPMSEP BOOKTYPE_H,218,JUL_07,SEP_07,HOU        HOUSTON                 
         LPMSEP BOOKTYPE_H,139,JUL_07,SEP_07,TAM        TAMPA                   
         LPMSEP BOOKTYPE_H,353,JAN_08,MAR_08,PHX        PHOENIX                 
         LPMSEP BOOKTYPE_H,351,JUL_08,SEP_08,DEN        DENVER                  
         LPMSEP BOOKTYPE_H,134,NOV_08,DEC_08,ORL        ORLANDO                 
         LPMSEP BOOKTYPE_H,462,NOV_08,DEC_08,SAC        SACRAMENTO              
*********LPMSEP BOOKTYPE_H,128,JUL_08,SEP_08,MF         MIAMI                   
                                                                                
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_LPMSEP                                                   
         EJECT                                                                  
****************************************************************                
* TABLE OF PPM BOOK FOR ARBITRON                                                
* MARKET AND FIRST LIVE BOOK , PRELIMINARY START AND END BOOK                   
****************************************************************                
TBL_PPMTAB DS 0D                                                                
         DC    CL8'*PPMTAB*'                                                    
         DS    XL6                                                              
         DC    AL2(PPMTABQ)        L'TABLE ENTRY                                
*                                                                               
         DC    AL2(007,MAR_07,JAN_07,FEB_07),CL3'PHL'                           
         DC    AL2(904,MAR_07,JAN_07,FEB_07),CL3'PHL' B                         
         DC    AL2(033,JUN_07,APR_07,MAY_07),CL3'HOU'                           
         DC    AL2(913,JUN_07,APR_07,MAY_07),CL3'HOU' B                         
         DC    AL2(848,JUN_07,APR_07,MAY_07),CL3'HOU' H                         
         DC    AL2(001,SEP_08,OCT_07,AUG_08),CL3'NY '                           
         DC    AL2(819,SEP_08,OCT_07,AUG_08),CL3'NY ' H                         
         DC    AL2(901,SEP_08,OCT_07,AUG_08),CL3'NY ' B                         
         DC    AL2(321,SEP_08,OCT_07,AUG_08),CL3'NAU'                           
         DC    AL2(949,SEP_08,OCT_07,AUG_08),CL3'NAU' B                         
         DC    AL2(413,SEP_08,OCT_07,AUG_08),CL3'MSU'                           
         DC    AL2(003,SEP_08,JUL_08,AUG_08),CL3'LA '                           
         DC    AL2(902,SEP_08,JUL_08,AUG_08),CL3'LA ' B                         
         DC    AL2(842,SEP_08,JUL_08,AUG_08),CL3'LA ' H                         
         DC    AL2(379,SEP_08,JUL_08,AUG_08),CL3'RSB'                           
         DC    AL2(809,SEP_08,JUL_08,AUG_08),CL3'RSB' H                         
         DC    AL2(005,SEP_08,JUL_08,AUG_08),CL3'CHI'                           
         DC    AL2(903,SEP_08,JUL_08,AUG_08),CL3'CHI' B                         
         DC    AL2(852,SEP_08,JUL_08,AUG_08),CL3'CHI' H                         
         DC    AL2(009,SEP_08,JUL_08,AUG_08),CL3'SF '                           
         DC    AL2(905,SEP_08,JUL_08,AUG_08),CL3'SF ' B                         
         DC    AL2(860,SEP_08,JUL_08,AUG_08),CL3'SF ' H                         
         DC    AL2(215,SEP_08,JUL_08,AUG_08),CL3'SJ '                           
         DC    AL2(879,SEP_08,JUL_08,AUG_08),CL3'SJ ' H                         
         DC    AL2(024,DEC_08,OCT_08,NOV_08),CL3'DF '                           
         DC    AL2(912,DEC_08,OCT_08,NOV_08),CL3'DF ' B                         
         DC    AL2(846,DEC_08,OCT_08,NOV_08),CL3'DF ' H                         
         DC    AL2(015,DEC_08,OCT_08,NOV_08),CL3'WAS'                           
         DC    AL2(908,DEC_08,OCT_08,NOV_08),CL3'WAS' B                         
         DC    AL2(745,DEC_08,OCT_08,NOV_08),CL3'WAS' H                         
         DC    AL2(011,DEC_08,OCT_08,NOV_08),CL3'DET'                           
         DC    AL2(906,DEC_08,OCT_08,NOV_08),CL3'DET' B                         
         DC    AL2(047,DEC_08,OCT_08,NOV_08),CL3'ATL'                           
         DC    AL2(917,DEC_08,OCT_08,NOV_08),CL3'ATL' B                         
         DC    AL2(737,DEC_08,OCT_08,NOV_08),CL3'ATL' H                         
         DC    AL2(013,MAR_09,JAN_09,FEB_09),CL3'BOS'                           
         DC    AL2(934,MAR_09,JAN_09,FEB_09),CL3'BOS' B                         
         DC    AL2(738,MAR_09,JAN_09,FEB_09),CL3'BOS' H                         
         DC    AL2(429,JUN_09,APR_09,MAY_09),CL3'MF '                           
         DC    AL2(916,JUN_09,APR_09,MAY_09),CL3'MF ' B                         
         DC    AL2(827,JUN_09,APR_09,MAY_09),CL3'MF ' H                         
         DC    AL2(039,JUN_09,APR_09,MAY_09),CL3'SEA'                           
         DC    AL2(057,JUN_09,APR_09,MAY_09),CL3'PHX'                           
         DC    AL2(864,JUN_09,APR_09,MAY_09),CL3'PHX' H                         
         DC    AL2(027,JUN_09,APR_09,MAY_09),CL3'MIN'                           
         DC    AL2(063,JUN_09,APR_09,MAY_09),CL3'SD '                           
         DC    AL2(859,JUN_09,APR_09,MAY_09),CL3'SD ' H                         
         DC    AL2(087,SEP_09,JUL_09,AUG_09),CL3'TAM'                           
         DC    AL2(942,SEP_09,JUL_09,AUG_09),CL3'TAM' B                         
         DC    AL2(742,SEP_09,JUL_09,AUG_09),CL3'TAM' H                         
         DC    AL2(017,SEP_09,JUL_09,AUG_09),CL3'STL'                           
         DC    AL2(907,SEP_09,JUL_09,AUG_09),CL3'STL' B                         
         DC    AL2(021,SEP_09,JUL_09,AUG_09),CL3'BAL'                           
         DC    AL2(909,SEP_09,JUL_09,AUG_09),CL3'BAL' B                         
         DC    AL2(035,SEP_09,JUL_09,AUG_09),CL3'DEN'                           
         DC    AL2(784,SEP_09,JUL_09,AUG_09),CL3'DEN' H                         
         DC    AL2(023,SEP_09,JUL_09,AUG_09),CL3'PIT'                           
         DC    AL2(941,SEP_09,JUL_09,AUG_09),CL3'PIT' B                         
*                                                                               
         DC    AL2(051,DEC_09,OCT_09,NOV_09),CL3'PTO'                           
         DC    AL2(019,DEC_09,OCT_09,NOV_09),CL3'CLE'                           
         DC    AL2(911,DEC_09,OCT_09,NOV_09),CL3'CLE' B                         
         DC    AL2(065,DEC_09,OCT_09,NOV_09),CL3'SAC'                           
         DC    AL2(878,DEC_09,OCT_09,NOV_09),CL3'SAC' H                         
         DC    AL2(031,DEC_09,OCT_09,NOV_09),CL3'CIN'                           
         DC    AL2(910,DEC_09,OCT_09,NOV_09),CL3'CIN' B                         
         DC    AL2(101,DEC_09,OCT_09,NOV_09),CL3'SLC'                           
         DC    AL2(041,DEC_09,OCT_09,NOV_09),CL3'KC '                           
         DC    AL2(915,DEC_09,OCT_09,NOV_09),CL3'KC ' B                         
         DC    AL2(257,DEC_09,OCT_09,NOV_09),CL3'LAS'                           
         DC    AL2(831,DEC_09,OCT_09,NOV_09),CL3'LAS' H                         
         DC    AL2(059,DEC_09,OCT_09,NOV_09),CL3'SAN'                           
         DC    AL2(814,DEC_09,OCT_09,NOV_09),CL3'SAN' B                         
*                                                                               
         DC    AL2(043,SEP_10,JUL_10,AUG_10),CL3'MIL'                           
         DC    AL2(939,SEP_10,JUL_10,AUG_10),CL3'MIL' B                         
         DC    AL2(093,SEP_10,JUL_10,AUG_10),CL3'CHL'                           
         DC    AL2(924,SEP_10,JUL_10,AUG_10),CL3'CHL' B                         
         DC    AL2(077,SEP_10,JUL_10,AUG_10),CL3'PRV'                           
         DC    AL2(131,SEP_10,JUL_10,AUG_10),CL3'ORL'                           
         DC    AL2(940,SEP_10,JUL_10,AUG_10),CL3'ORL' B                         
         DC    AL2(845,SEP_10,JUL_10,AUG_10),CL3'ORL' H                         
         DC    AL2(045,SEP_10,JUL_10,AUG_10),CL3'CLO'                           
         DC    AL2(914,SEP_10,JUL_10,AUG_10),CL3'CLO' B                         
         DC    AL2(109,SEP_10,JUL_10,AUG_10),CL3'NOR'                           
         DC    AL2(929,SEP_10,JUL_10,AUG_10),CL3'NOR' B                         
         DC    AL2(049,SEP_10,JUL_10,AUG_10),CL3'IND'                           
         DC    AL2(919,SEP_10,JUL_10,AUG_10),CL3'IND' B                         
         DC    AL2(135,SEP_10,JUL_10,AUG_10),CL3'AUS'                           
         DC    AL2(835,SEP_10,JUL_10,AUG_10),CL3'AUS' H                         
         DC    AL2(115,SEP_10,JUL_10,AUG_10),CL3'RAL'                           
         DC    AL2(930,SEP_10,JUL_10,AUG_10),CL3'RAL' B                         
         DC    AL2(073,SEP_10,JUL_10,AUG_10),CL3'NAS'                           
         DC    AL2(921,SEP_10,JUL_10,AUG_10),CL3'NAS' B                         
*                                                                               
         DC    AL2(166,DEC_10,OCT_10,NOV_10),CL3'GWH'                           
         DC    AL2(932,DEC_10,OCT_10,NOV_10),CL3'GWH' B                         
         DC    AL2(299,DEC_10,OCT_10,NOV_10),CL3'WPB'                           
         DC    AL2(107,DEC_10,OCT_10,NOV_10),CL3'JKV'                           
         DC    AL2(927,DEC_10,OCT_10,NOV_10),CL3'JKV' B                         
         DC    AL2(075,DEC_10,OCT_10,NOV_10),CL3'MEM'                           
         DC    AL2(925,DEC_10,OCT_10,NOV_10),CL3'MEM' B                         
         DC    AL2(061,DEC_10,OCT_10,NOV_10),CL3'HAT'                           
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_PPMTAB                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE THAT EQUATES NSI CABLE CALL LETTERS                                     
* CALL LETTER CHANGES WITH THE SAME NETWORK ID'S ARE STORED WITH                
* EFFECTIVE DATES.  THIS WILL ENSURE RELOADS TO OLD BOOKS WILL PICK             
* UP THE CORRECT CALL LETTERS.                                                  
* EFFECTIVE DATE MUST BE ENTERED WITH WEEK NUMBER OF THE 1ST MONDAY             
* OF THE WEEK OF CHANGE - THE CONVERSION PROGRAM WILL ALWAYS                    
* COMPARE TO MAKE SURE THE WEEK OF CONVERSION IS A WEEK LATER                   
* THAN THE WEEK IN THE TABLE.                                                   
* (SEE TABLE TBL_MTHDATES IN THIS MODULE.)                                      
* THIS WILL PREVENT PARTIAL MAJOR KEYS OF THE OLD CALL LETTERS AND              
* NEW CALL LETTERS BEING AN ISSUE FOR REISSUES FOR STATIONS                     
* LOADED UNDER THE OLD CALL LETTERS.                                            
* KEEP CALL LETTERS WITH THE SAME NETWORK IDS IN CHRONOLOGICAL ORDER -          
* (LATEST TO EARLIEST)                                                          
*                                                                               
* FUSION NCC CALL LETTERS SHOULD BE ENTERED AFTER NSI                           
* SO IT DOESNT MESS UP OVERNIGHT/DAILIES CONVERSIONS                            
*                                                                               
* NOTE: OANA SAYS THIS TABLE HAD BEEN MAINTAINED BY SEAN, AND HASN'T            
*       BEEN UPDATED IN QUITE SOME TIME.                                        
***********************************************************************         
TBL_NSICBL DS  0D                                                               
         DC    CL8'*NSICBL*'                                                    
         DS    XL6                                                              
         DC    AL2(NSICABQ)        L'TABLE ENTRY                                
*                                                                               
         NSICBL AJE,1031                                                        
         NSICBL BAND,1036                                                       
         NSICBL FEAR,1037                                                       
         NSICBL NGM,1066                                                        
         NSICBL ANTV,1147                                                       
         NSICBL MCPS,1196,(YR_2014,WEEK_13)                                     
         NSICBL SWRV,1196                                                       
         NSICBL PRST,1233                                                       
         NSICBL NHLN,1269                                                       
         NSICBL RLTV,1377                                                       
         NSICBL LTN,1382                                                        
         NSICBL NTN,1383                                                        
         NSICBL TVVN,1384                                                       
         NSICBL MAV,1595                                                        
         NSICBL MFAM,1806                                                       
         NSICBL IONW,1876                                                       
         NSICBL TFRM,1996                                                       
         NSICBL PERU,1997                                                       
         NSICBL IMNF,2060                                                       
         NSICBL CHIL,2099                                                       
         NSICBL ILIF,2150                                                       
         NSICBL WORS,2151                                                       
         NSICBL QUBO,2152                                                       
         NSICBL NKJR,2181,(YR_2009,WEEK_40)                                     
         NSICBL NOGG,2181                                                       
         NSICBL MCOD,2192                                                       
         NSICBL ABCB,2249                                                       
         NSICBL KBSW,2250                                                       
         NSICBL FBN,2263                                                        
         NSICBL HGHD,2289                                                       
         NSICBL HLMC,2315,(YR_2009,WEEK_14)                                     
         NSICBL HMHD,2315                                                       
         NSICBL HMM,2315,(YR_2014,WEEK_27)                                      
         NSICBL HMC,2315                                                        
         NSICBL FDHD,2318                                                       
         NSICBL DFAM,2365                                                       
         NSICBL CCT9,2387                                                       
         NSICBL GOD,2340                                                        
         NSICBL HDNM,2366                                                       
         NSICBL MAS2,2368                                                       
         NSICBL CCT4,2388                                                       
         NSICBL DCOD,2447                                                       
         NSICBL DPR3,2513                                                       
         NSICBL N12,2514                                                        
         NSICBL NWT,2514                                                        
         NSICBL N12R,2514                                                       
         NSICBL ZSPS,2549,(YR_2017,WEEK_46)                                     
         NSICBL ZBHS,2549,(YR_2008,WEEK_49)                                     
         NSICBL ZC47,2549                                                       
         NSICBL MCNL,2655                                                       
         NSICBL RLZC,2680                                                       
         NSICBL NRBN,2688                                                       
         NSICBL BTN,2726                                                        
         NSICBL DSJR,2991                                                       
         NSICBL JADW,3831                                                       
         NSICBL SPRT,4001                                                       
         NSICBL REAL,4004                                                       
         NSICBL NGWD,9874                                                       
         NSICBL ETNW,4006                                                       
         NSICBL PAHD,4032,(YR_2008,WEEK_36)                                     
         NSICBL MHD,4032                                                        
         NSICBL IDA,4037                                                        
         NSICBL RTA,4038                                                        
         NSICBL D101,4114                                                       
         NSICBL HDT,4133,(YR_2010,WEEK_45)                                      
         NSICBL DHD,4133                                                        
         NSICBL INF,4139                                                        
         NSICBL CMEX,4144                                                       
         NSICBL PYE,4174                                                        
         NSICBL TBMC,4387                                                       
         NSICBL LOGO,4443                                                       
         NSICBL AZAN,4477                                                       
         NSICBL NWBK,4490                                                       
         NSICBL TLNV,4628                                                       
         NSICBL TMOD,4663                                                       
         NSICBL SHOD,4664                                                       
         NSICBL UP,4690,(YR_2013,WEEK_27)                                       
         NSICBL GMC,4690                                                        
         NSICBL FMTV,4709,(YR_2016,WEEK_3)                                      
         NSICBL SITV,4709                                                       
         NSICBL CSNC,4734                                                       
         NSICBL ADSM,4737                                                       
         NSICBL HDNT,4929                                                       
         NSICBL NOT1,4944                                                       
         NSICBL CSB,4952,(YR_2008,WEEK_22)                                      
         NSICBL FSB,4952                                                        
         NSICBL HTV,4963                                                        
         NSICBL ESPD,4964                                                       
         NSICBL XPLT,5008                                                       
         NSICBL SNY,6091                                                        
         NSICBL AMS,6104                                                        
         NSICBL BYU,6105                                                        
         NSICBL FXMR,6109,(YR_2016,WEEK_3)                                      
         NSICBL FMC,6109                                                        
         NSICBL NTT,6110                                                        
         NSICBL IFC,6111                                                        
         NSICBL NWNJ,6134                                                       
         NSICBL LIF,6196                                                        
         NSICBL MTV,6198                                                        
         NSICBL AMTV,12688                                                      
         NSICBL HLDR,12133                                                      
         NSICBL FETV,10388                                                      
         NSICBL NWSY,12724                                                      
         NSICBL HLN,6199                                                        
         NSICBL BET,6200                                                        
         NSICBL FRFM,6201,(YR_2016,WEEK_7)                                      
         NSICBL FAM,6201                                                        
         NSICBL CNN,6202                                                        
         NSICBL CPA,6203                                                        
         NSICBL ESPN,6204                                                       
         NSICBL MSG,6207                                                        
         NSICBL UNV,6209                                                        
         NSICBL NICK,6212                                                       
         NSICBL CRTV,6214                                                       
         NSICBL USA,6217                                                        
         NSICBL AEN,6218                                                        
         NSICBL SPK,6221                                                        
         NSICBL PAR,6221  SPK CHANGE TO PAR BUT NCC COMES AFTER NSI             
         NSICBL HBA,6223                                                        
         NSICBL PAA,6225                                                        
         NSICBL NWCN,6243                                                       
         NSICBL CXA,6266                                                        
         NSICBL NBCS,6275,(YR_2012,WEEK_1)                                      
         NSICBL VS,6275,(YR_2009,WEEK_27)                                       
         NSICBL OLN,6275                                                        
         NSICBL OUTD,6350,(YR_2007,WEEK_40)                                     
         NSICBL TOC,6350                                                        
         NSICBL ENT,6384                                                        
         NSICBL CASA,6389                                                       
         NSICBL TNT,6390                                                        
         NSICBL POP,6477,(YR_2015,WEEK_2)                                       
         NSICBL TVGN,6477,(YR_2007,WEEK_27)                                     
         NSICBL TVG,6477                                                        
         NSICBL CLOO,6479,(YR_2011,WEEK_37)                                     
         NSICBL SLTH,6479                                                       
         NSICBL TMA,6484                                                        
         NSICBL HALL,6485                                                       
         NSICBL STOD,6488                                                       
         NSICBL COX7,6493                                                       
         NSICBL HBO,6510                                                        
         NSICBL SHOW,6511                                                       
         NSICBL MAX,6513                                                        
         NSICBL TMC,6514                                                        
         NSICBL ENOD,6515                                                       
         NSICBL GALA,6517                                                       
         NSICBL HITN,6518                                                       
         NSICBL CNBC,6521              NSI                                      
         NSICBL CNB,6521               FUSION IS CNB                            
         NSICBL DSNY,6522                                                       
         NSICBL TWC,6523                                                        
         NSICBL PLBY,6525                                                       
         NSICBL BRVO,6526                                                       
         NSICBL FSW,6527                                                        
         NSICBL DISC,6530                                                       
         NSICBL DSC,6530                                                        
         NSICBL ALN,6532                                                        
         NSICBL HSN,6534                                                        
         NSICBL FSFL,6535                                                       
         NSICBL TMD,6536                                                        
         NSICBL MEX,6541                                                        
         NSICBL VH1,6546                                                        
         NSICBL VHI,6546                                                        
         NSICBL GOLF,6560                                                       
         NSICBL KALI,6565                                                       
         NSICBL NWBX,6571                                                       
         NSICBL CSNM,6572                                                       
         NSICBL NESN,6574                                                       
         NSICBL FSNW,6575                                                       
         NSICBL BHER,6577,(YR_2017,WEEK_46)                                     
         NSICBL CTRC,6577,(YR_2009,WEEK_45)                                     
         NSICBL BETJ,6577                                                       
         NSICBL FSS,6592                                                        
         NSICBL AMC,6593                                                        
         NSICBL JADE,6594                                                       
         NSICBL ONCE,6604                                                       
         NSICBL APL,6605                                                        
         NSICBL MSGP,6607,(YR_2008,WEEK_13)                                     
         NSICBL FSNY,6607                                                       
         NSICBL FSCH,6609                                                       
         NSICBL TBN,6614                                                        
         NSICBL FAMN,6622                                                       
         NSICBL SMTH,6626                                                       
         NSICBL INSP,6632                                                       
         NSICBL TLC,6635                                                        
         NSICBL CMT,6647                                                        
         NSICBL SUN,6651                                                        
         NSICBL ZAZN,6652                                                       
         NSICBL EWN,6655                                                        
         NSICBL CSNE,6668,(YR_2007,WEEK_40)                                     
         NSICBL FSNE,6668                                                       
         NSICBL TLFE,6675                                                       
         NSICBL TRAV,6678                                                       
         NSICBL NWSN,6788,(YR_2021,WEEK_11)                                     
         NSICBL WGNA,6788,(YR_2008,WEEK_26)                                     
         NSICBL XWGN,6788                                                       
         NSICBL QVC,6951                                                        
         NSICBL ESCL,7002                                                       
         NSICBL CSBA,7033,(YR_2008,WEEK_22)                                     
         NSICBL FSBA,7033                                                       
         NSICBL INHT,7047                                                       
         NSICBL MOJO,7048                                                       
         NSICBL FSMW,7052                                                       
         NSICBL TVES,7065                                                       
         NSICBL FSSO,7067                                                       
         NSICBL GSN,7099                                                        
         NSICBL RETX,7100                                                       
         NSICBL CMDY,7133,(YR_2012,WEEK_9)                                      
         NSICBL CMD,7133                                                        
         NSICBL INDX,7145                                                       
         NSICBL AZN,7165                                                        
         NSICBL ENCR,7167                                                       
         NSICBL DLIF,7180,(YR_2015,WEEK_2)                                      
         NSICBL FIT,7180                                                        
         NSICBL DFH,7180                                                        
         NSICBL ENA,7182                                                        
         NSICBL TRU,7183,(YR_2007,WEEK_1)                                       
         NSICBL CRT,7183                                                        
         NSICBL TVCH,7187                                                       
         NSICBL WAP,7196                                                        
         NSICBL LNCH,7197                                                       
         NSICBL WTBD,7197                                                       
         NSICBL WNWS,7197                                                       
         NSICBL NECN,7220                                                       
         NSICBL EGTV,7222                                                       
         NSICBL NYNB,7225                                                       
         NSICBL SYFY,7235,(YR_2010,WEEK_5)                                      
         NSICBL SYFI,7235,(YR_2009,WEEK_40)                                     
         NSICBL SFI,7235                                                        
         NSICBL TBSC,7237                                                       
         NSICBL ZNY1,7238                                                       
         NSICBL TOON,7241                                                       
         NSICBL LCLT,7255                                                       
         NSICBL CLTV,7255                                                       
         NSICBL MSP2,7271,(YR_2008,WEEK_13)                                     
         NSICBL FSN2,7271                                                       
         NSICBL ESP2,7287,(YR_2007,WEEK_1)                                      
         NSICBL ESX,7287                                                        
         NSICBL FHD9,7290                                                       
         NSICBL TR3S,7296                                                       
         NSICBL FSE,7300                                                        
         NSICBL FOXD,7300                                                       
         NSICBL SNBC,7301                                                       
         NSICBL FOOD,7304                                                       
         NSICBL STRZ,7306                                                       
         NSICBL UVSO,7310,(YR_2015,WEEK_5)                                      
         NSICBL MUN2,7310                                                       
         NSICBL SUR,7311                                                        
         NSICBL NBAH,7319                                                       
         NSICBL DSED,7320                                                       
         NSICBL PGOD,7321                                                       
         NSICBL SCID,7322                                                       
         NSICBL TCM,7325                                                        
         NSICBL FX,7328                                                         
         NSICBL IDOD,7332                                                       
         NSICBL MIOD,7334                                                       
         NSICBL TYSP,7338                                                       
         NSICBL UTSM,7339                                                       
         NSICBL EWRG,7340                                                       
         NSICBL ECUA,7356                                                       
         NSICBL NWHV,7357                                                       
         NSICBL JCTV,7359                                                       
         NSICBL CNNE,7371                                                       
         NSICBL NWCT,7387                                                       
         NSICBL FXNC,7401                                                       
         NSICBL TLOD,7442                                                       
         NSICBL FS1,7443,(YR_2013,WEEK_31)                                      
         NSICBL SPD,7443,(YR_2010,WEEK_25)                                      
         NSICBL SC,7443                                                         
         NSICBL ZBN9,7455                                                       
         NSICBL FSA,7470                                                        
         NSICBL FSTU,7470                                                       
         NSICBL ENN,7474                                                        
         NSICBL ZCSN,7485                                                       
         NSICBL FSW2,7498                                                       
         NSICBL BTV,7502                                                        
         NSICBL LNWT,7514                                                       
         NSICBL N12,7514                                                        
         NSICBL NFLH,7515                                                       
         NSICBL WETV,7516,(YR_2014,WEEK_14)                                     
         NSICBL WE,7516                                                         
         NSICBL MLBN,7564                                                       
         NSICBL CNNS,7606                                                       
         NSICBL DXD,7611,(YR_2009,WEEK_14)                                      
         NSICBL TDSY,7611                                                       
         NSICBL MST7,7612                                                       
         NSICBL PBES,7624                                                       
         NSICBL GAC,7651                                                        
         NSICBL FUSE,7652                                                       
         NSICBL OVA,7653                                                        
         NSICBL MASN,7655                                                       
         NSICBL DIY,7742                                                        
         NSICBL MNBC,7801                                                       
         NSICBL FSWI,7837                                                       
         NSICBL TVL,7838                                                        
         NSICBL AJAM,7981,(YR_2013,WEEK_35)                                     
         NSICBL CRNT,7981                                                       
         NSICBL CN8N,8005                                                       
         NSICBL KDVE,8117                                                       
         NSICBL ESQ,8151,(YR_2013,WEEK_35)                                      
         NSICBL STYL,8151                                                       
         NSICBL BBCA,8250                                                       
         NSICBL GRN,8251,(YR_2008,WEEK_26)                                      
         NSICBL DHOM,8251                                                       
         NSICBL ID,8252,(YR_2007,WEEK_1)                                        
         NSICBL DTMS,8252                                                       
         NSICBL DSCI,8253                                                       
         NSICBL SCI,8253                                                        
         NSICBL DFC,8254,(YR_2014,WEEK_40)                                      
         NSICBL DKID,8254                                                       
         NSICBL HUB,8254                                                        
         NSICBL OWN,8255,(YR_2011,WEEK_5)                                       
         NSICBL DHLT,8255                                                       
         NSICBL AHC,8256,(YR_2014,WEEK_9)                                       
         NSICBL MIL,8256                                                        
         NSICBL MRTV,8297                                                       
         NSICBL ZMTW,8299                                                       
         NSICBL CAMV,8320                                                       
         NSICBL TNNK,8331,(YR_2009,WEEK_40)                                     
         NSICBL THEN,8331,(YR_2007,WEEK_1)                                      
         NSICBL NOGT,8331                                                       
         NSICBL GAS,8332                                                        
         NSICBL G4,8333                                                         
         NSICBL LMN,8336                                                        
         NSICBL FYI,8338,(YR_2014,WEEK_27)                                      
         NSICBL BIO,8338                                                        
         NSICBL VICE,8339,(YR_2016,WEEK_19)                                     
         NSICBL H2,8339,(YR_2012,WEEK_10)                                       
         NSICBL HSTI,8339                                                       
         NSICBL CINL,8340                                                       
         NSICBL SHOP,8343                                                       
         NSICBL TLHT,8344                                                       
         NSICBL AOR,8380                                                        
         NSICBL DSE,8342                                                        
         NSICBL SOAP,8443                                                       
         NSICBL OXYG,8444                                                       
         NSICBL FSC,8445                                                        
         NSICBL MTV2,8466                                                       
         NSICBL MSDX,8472                                                       
         NSICBL MTVC,8512                                                       
         NSICBL CMTM,8513,(YR_2016,WEEK_7)                                      
         NSICBL CMTP,8513                                                       
         NSICBL WSCN,8515                                                       
         NSICBL BETJ,8516,(YR_2016,WEEK_3)                                      
         NSICBL MTVJ,8516                                                       
         NSICBL BETS,8517,(YR_2016,WEEK_7)                                      
         NSICBL VH1S,8517                                                       
         NSICBL TUDN,8518                                                       
         NSICBL VHU,8520                                                        
         NSICBL NIKT,8523                                                       
         NSICBL INLF,8525                                                       
         NSICBL TSOU,8533                                                       
         NSICBL TRIO,8535                                                       
         NSICBL CSTV,8607                                                       
         NSICBL AMXE,8754                                                       
         NSICBL DHMI,8851                                                       
         NSICBL TV1,8871                                                        
         NSICBL DPLA,8872                                                       
         NSICBL DPLC,8885                                                       
         NSICBL CCNN,8886,(YR_2009,WEEK_18)                                     
         NSICBL CGOS,8886                                                       
         NSICBL TVCL,8888                                                       
         NSICBL ESNU,8889                                                       
         NSICBL CNNI,8906                                                       
         NSICBL HIST,8908                                                       
         NSICBL STA,8911                                                        
         NSICBL HGTV,8920                                                       
         NSICBL HOD,8932                                                        
         NSICBL MOD,8933                                                        
         NSICBL FS2,9042,(YR_2013,WEEK_31)                                      
         NSICBL FUEL,9042                                                       
         NSICBL NAN,9043                                                        
         NSICBL HBOM,9100                                                       
         NSICBL MAXP,9118                                                       
         NSICBL ENCY,9130                                                       
         NSICBL ENCX,9133                                                       
         NSICBL STZP,9157                                                       
         NSICBL NC64,9212                                                       
         NSICBL NGC,9258                                                        
         NSICBL PBSK,9303                                                       
         NSICBL BOOM,9304                                                       
         NSICBL FILI,9330                                                       
         NSICBL TINT,9331                                                       
         NSICBL ZEE,9334                                                        
         NSICBL SMXE,9378                                                       
         NSICBL OMXE,9381                                                       
         NSICBL TFC,9385                                                        
         NSICBL WMXE,9394                                                       
         NSICBL WLNK,9407                                                       
         NSICBL CSS,9408                                                        
         NSICBL CGOA,9423,(YR_2009,WEEK_17)                                     
         NSICBL CGOL,9423                                                       
         NSICBL SHO1,9521                                                       
         NSICBL TMC1,9527                                                       
         NSICBL NFLN,9540                                                       
         NSICBL YES,9543,(YR_1970,WEEK_1)                                       
         NSICBL YESN,9543                                                       
         NSICBL MVA,9578                                                        
         NSICBL MBCN,9591                                                       
         NSICBL CTI,9604                                                        
         NSICBL MTVH,9606                                                       
         NSICBL CNBW,9721                                                       
         NSICBL NBAT,9772                                                       
         NSICBL RFD,9781                                                        
         NSICBL TVGN,9789                                                       
         NSICBL RTML,9798                                                       
         NSICBL VOD,9803                                                        
         NSICBL MC,9811                                                         
         NSICBL JWTV,9872                                                       
         NSICBL FINE,9876                                                       
         NSICBL CC,9876                                                         
         NSICBL LRWM,9877                                                       
         NSICBL FXM,9895                                                        
         NSICBL TENN,9897                                                       
         NSICBL GOL,9905                                                        
         NSICBL HESP,1772                                                       
         NSICBL RDZN,1301                                                       
         NSICBL TVDM,1343                                                       
         NSICBL MEGA,1428                                                       
         NSICBL LATV,1601                                                       
         NSICBL UNIS,1773                                                       
         NSICBL AUCN,2044                                                       
         NSICBL FOHD,2217                                                       
         NSICBL LFTV,4145                                                       
         NSICBL HFHD,6900                                                       
         NSICBL FOXV,7388                                                       
         NSICBL ASHE,7559                                                       
         NSICBL TOHE,7561                                                       
         NSICBL SUND,7602                                                       
         NSICBL THIS,8148                                                       
         NSICBL SPMN,8345,(YR_2012,WEEK_13)                                     
         NSICBL SPMC,8345                                                       
         NSICBL VER,8422                                                        
         NSICBL BBCW,8934                                                       
         NSICBL SIHD,9036                                                       
         NSICBL CI,9076                                                         
         NSICBL SNA,9847                                                        
         NSICBL PYA,9972                                                        
         NSICBL NSBI,9973                                                       
         NSICBL HSTE,1772                                                       
         NSICBL NKT,9963                                                        
         NSICBL FXX,10041                                                       
         NSICBL ZSPC,9791,(YR_2017,WEEK_46)                                     
         NSICBL ZCTW,9791                                                       
*                                                                               
         NSICBL BABY,10214,(YR_2017,WEEK_3)                                     
         NSICBL PASN,1124,(YR_2017,WEEK_3)                                      
         NSICBL FSSP,1187,(YR_2017,WEEK_3)                                      
         NSICBL HSNY,1363,(YR_2017,WEEK_3)                                      
         NSICBL P12L,1710,(YR_2017,WEEK_3)                                      
         NSICBL SPNB,1739,(YR_2017,WEEK_46)                                     
         NSICBL TWNB,1739,(YR_2017,WEEK_3)                                      
         NSICBL P12N,2401,(YR_2017,WEEK_3)                                      
         NSICBL ZSPG,2584,(YR_2017,WEEK_46)                                     
         NSICBL ZGTW,2584,(YR_2017,WEEK_3)                                      
         NSICBL P12B,3718,(YR_2017,WEEK_3)                                      
         NSICBL CSCA,4578,(YR_2017,WEEK_3)                                      
         NSICBL NSCA,4578,(YR_2017,WEEK_3)                                      
         NSICBL RTPT,6211,(YR_2017,WEEK_3)                                      
         NSICBL ATPT,6211,(YR_2017,WEEK_3)                                      
         NSICBL FSOH,6661,(YR_2017,WEEK_3)                                      
         NSICBL ATRM,7050,(YR_2017,WEEK_3)                                      
         NSICBL RTRM,7050,(YR_2017,WEEK_3)                                      
         NSICBL STO,7456,(YR_2017,WEEK_3)                                       
         NSICBL FSD,7479,(YR_2017,WEEK_3)                                       
         NSICBL FSSD,8802,(YR_2017,WEEK_3)                                      
         NSICBL SPSN,9774,(YR_2017,WEEK_3)                                      
         NSICBL CST,9790,(YR_2017,WEEK_3)                                       
         NSICBL ZSPR,9792,(YR_2017,WEEK_46)                                     
         NSICBL ZRTW,9792,(YR_2017,WEEK_3)                                      
         NSICBL SPDP,9850,(YR_2017,WEEK_3)                                      
         NSICBL SPNX,8381,(YR_2017,WEEK_46)                                     
         NSICBL SPKC,8870,(YR_2017,WEEK_46)                                     
         NSICBL CDTV,11933,(YR_2017,WEEK_46)                                    
         NSICBL ESNL,11973,(YR_2017,WEEK_46)                                    
         NSICBL SNLA,10171                                                      
         NSICBL ZCFN,7501                                                       
         NSICBL ATSW,9931                                                       
         NSICBL RTSW,9931                                                       
         NSICBL NSCX,1262                                                       
         NSICBL NSNN,2278                                                       
         NSICBL NSPP,8236                                                       
         NSICBL TCNP,8236                                                       
         NSICBL SPNA,9822                                                       
         NSICBL TWNA,9822                                                       
         NSICBL SPNR,5599                                                       
         NSICBL TWNR,5599                                                       
         NSICBL SPNS,8558                                                       
         NSICBL SPSA,12092                                                      
         NSICBL S1SA,12092                                                      
         NSICBL TWNX,8381                                                       
         NSICBL OLYM,11834         SPEC-52730                                   
*                                                                               
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_NSICBL                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF LIVE ONLY BOOKTYPE AND CORRESPONDING LIVE PLUS BOOKTYPE              
*                                                                               
* THIS TABLE IS USED FOR SPOT POSTING AND UPGRADES                              
* SPOT POSTING-IF BOOKTYPE (LIVE ONLY, LIVE+3, LIVE+ SAME DAY) IS               
* NOT FOUND, DO A BOOKTYPE SWITCH                                               
*                                                                               
* SPOT UPGRADE- BKTYPES BEFORE THE BOOK START/END AVAILABLE                     
* RANGE WILL BE SWITCHED TO THE LIVE +7 BOOKTYPE IN ORDER TO                    
* ALLOW GETTING SHARES FORM THE NEW BOOKTYPE AND PUTS FROM THE                  
* A BOOKTYPE BEFORE THE NEW BOOKTYPE WAS AVAILABLE                              
*                                                                               
* IT'S IMPORTANT TO MAKE SURE THE FIRST OCCURRENCE OF A BOOKTYPE                
* SWITCH IS ALWAYS A SWITCH TO THE STANDARD LIVE+7 BOOKTYPES                    
* BECAUSE SPGETDEME/SPGETDEMF WILL ALWAYS SWITCH TO THAT BOOKTYPE               
* IF THE BOOKTYPE IS DEFINED IN THIS TABLE IN THE SETMKTYP ROUTINE              
* IN ORDER TO FIGURE OUT THE MARKET THE STATION BELONGS TO                      
*                                                                               
* REMEMBER POSTING CODE KEEPS LOOKING IN THIS TABLE UNTIL A BOOKTYPE            
* IS NOT FOUND IN ONE THIS TABLE - SO BE CAREFUL OF INFINITE LOOP               
*                                                                               
***********************************************************************         
TBL_LVBKYP DS    0D                                                             
         DC    CL8'*LVBKTY*'                                                    
         DS    XL6                                                              
         DC    AL2(LVBKTYPQ)       L'TABLE ENTRY                                
*                                                                               
* ----------------------------------------------------------------              
* IF LIVE ONLY NOT AVAILABLE LOOK FOR LIVE + 7                                  
* THIS TABLE  HAS TO COME BEFORE THE ONE BELOW WHERE WE SWITCH TO               
* LIVE+SAME DAY - UPGRADES WILL ONLY SEARCH FOR THE 1ST OCCURRENCE              
* SO THIS TABLE NEEDS TO COME 1ST FOR UPGRADES                                  
* ----------------------------------------------------------------              
*                                  LIVE ONLY, LIVE PLUS STANDARD                
         LVBKYP BOOKTYPE_L,BOOKTYPE_STANDARD,NOV_06,DEC_09                      
*                                  LIVE ONLY, LIVE PLUS WIRED                   
         LVBKYP BOOKTYPE_Z,BOOKTYPE_W,NOV_06,DEC_09                             
*                                  LIVE ONLY, LIVE PLUS CABLE                   
         LVBKYP BOOKTYPE_U,BOOKTYPE_C,NOV_06,DEC_09                             
*                                   LIVE ONLY, LIVE PLUS HISPANIC               
         LVBKYP BOOKTYPE_J,BOOKTYPE_H,NOV_06,DEC_09                             
*&&DO                                                                           
*  LIVE +1 PUT SUPPORT FOR PROJECTIONS.                                         
         LVBKYP BOOKTYPE_L1,BOOKTYPE_STANDARD,JAN_17,DEC_18                     
         LVBKYP BOOKTYPE_C1,BOOKTYPE_C,JAN_17,DEC_18                            
         LVBKYP BOOKTYPE_W1,BOOKTYPE_W,JAN_17,DEC_18                            
         LVBKYP BOOKTYPE_H1,BOOKTYPE_H,JAN_17,DEC_18                            
*&&                                                                             
***** WE DONT NEED THE FOLLOWING LIVE TO LIVE+ SAME DAY                         
***** TRANSPARENCY CODE ANYMORE NOW THAT DATA COME TOGETHER                     
***** SINCE LOCAL MONTHLY  - LEAVE OLYMPICS FOR NOW                             
*&&DO                                                                           
*-----------------------------------------------------------------              
*  IF LIVE ONLY NOT AVAILABLE LOOK FOR LIVE + SAME DAY                          
         LVBKYP BOOKTYPE_L,BOOKTYPE_LS,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_Z,BOOKTYPE_WS,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_U,BOOKTYPE_WS,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_J,BOOKTYPE_HS,JAN_10,TO_END                            
*&&                                                                             
*                                  OLYMPIC LIVE ONLY ->LIVE+7                   
*******  LVBKYP BOOKTYPE_OL,BOOKTYPE_O,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_OL,BOOKTYPE_L,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_OS,BOOKTYPE_LS,JAN_10,TO_END                           
         LVBKYP BOOKTYPE_O3,BOOKTYPE_L3,JAN_10,TO_END                           
*                                                                               
*&&DO                                                                           
*THIS TABLE NEEDS TO REPLACE THE OTHER L3 BOOKTYPE TABLES ONCE                  
*THE JAN10(L3) BOOK GETS LOADED                                                 
*-----------------------------------------------------------------              
         LVBKYP BOOKTYPE_L3,BOOKTYPE_STANDARD,MAY_08,TO_END                     
         LVBKYP BOOKTYPE_W3,BOOKTYPE_W,MAY_08,TO_END                            
         LVBKYP BOOKTYPE_C3,BOOKTYPE_C,MAY_08,TO_END                            
*-----------------------------------------------------------------              
*&&                                                                             
*-----------------------------------------------------------------              
*                                                                               
* PRE JAN_10 LIVE+3 GOES TO STANDARD                                            
* THIS TABLE  HAS TO COME BEFORE THE ONE BELOW WHERE WE SWITCH TO               
* LIVE+SAME DAY - UPGRADES WILL ONLY SEARCH FOR THE 1ST OCCURRENCE              
* SO THIS TABLE NEEDS TO COME 1ST FOR UPGRADES                                  
*                                                                               
         LVBKYP BOOKTYPE_L3,BOOKTYPE_STANDARD,MAY_08,DEC_09                     
         LVBKYP BOOKTYPE_W3,BOOKTYPE_W,MAY_08,DEC_09                            
         LVBKYP BOOKTYPE_C3,BOOKTYPE_C,MAY_08,DEC_09                            
*                                                                               
*-----------------------------------------------------------------              
*                                                                               
* EFFECTIVE JAN2010                                                             
* IF LIVE +3 NOT AVAILABLE LOOK FOR LIVE +SAME DAY                              
* EXCEPT FOR C3 WHICH WE DECIDED SHOULD JUST GO TO C'C'                         
* BECAUSE CABLE IS LOADED TOGETHER WITH BOOKTYPE_LS WHICH                       
* WOULD RESULT IN BOOKTYPE X'00' FOR DIARY MKTS WHERE                           
* LIVE + SAME DAY DATA IS NOT AVAILBLE.                                         
* WE WOULD NEVER BE ABLE TO GET TO THE C BOOKTYPE.                              
*                                                                               
         LVBKYP BOOKTYPE_L3,BOOKTYPE_LS,JAN_10,TO_END                           
         LVBKYP BOOKTYPE_W3,BOOKTYPE_WS,JAN_10,JUN_18                           
         LVBKYP BOOKTYPE_C3,BOOKTYPE_C,JAN_10,TO_END                            
*                                                                               
         LVBKYP BOOKTYPE_W3,BOOKTYPE_C3,JUL_18,TO_END                           
*                                                                               
***** WE DONT NEED THE FOLLOWING LIVE TO LIVE+ SAME DAY                         
***** TRANSPARENCY CODE ANYMORE NOW THAT DATA COME TOGETHER                     
***** SINCE LOCAL MONTHLY  - LEAVE OLYMPICS FOR NOW                             
*&&DO                                                                           
* 1ST TABLE FOR L+SD USED FOR UPGRADES AND POSTING EFFECTIVE JAN_10             
* UPGRADES WILL SWITCH TO LIVE+7 IF BOOK IS PRIOR TO JAN_10                     
* IF LIVE +SAME DAY NOT AVAILABLE LOOK FOR LIVE +7                              
*                                                                               
         LVBKYP BOOKTYPE_LS,BOOKTYPE_STANDARD,JAN_10,TO_END                     
         LVBKYP BOOKTYPE_WS,BOOKTYPE_W,JAN_10,TO_END                            
         LVBKYP BOOKTYPE_HS,BOOKTYPE_H,JAN_10,TO_END                            
*&&                                                                             
         LVBKYP BOOKTYPE_OS,BOOKTYPE_O,JAN_10,TO_END                            
*                                                                               
*-----------------------------------------------------------------              
* THIS TABLE IS FOR SPOT POSTING TO DEAL WITH PRE JAN10 BOOK                    
* WHERE LIVE PLUS SAME DAY IS NOT AVAILBLE YET.                                 
* THESE ENTRIES ARE NECESSARY FOR SPOT POSTING FOR SETMETER AND DIARY           
* MARKET POSTING WHERE WE ALSO READ MONTHLY BOOKS FOR OVERNIGHT                 
* POSTING.  IF WE SPECIFY A MONTHLY BOOKFOR AN OVERNIGHT LIVE+SD                
* POST AND THE MONTHLY BOOK DOES NOT HAVE L+SD DATA WE NEED TO                  
* SWITCH TO LIVE+7 PRIOR TO JAN_10                                              
*                                                                               
         LVBKYP BOOKTYPE_LS,BOOKTYPE_STANDARD,JAN_09,DEC_09                     
         LVBKYP BOOKTYPE_WS,BOOKTYPE_W,JAN_09,DEC_09                            
         LVBKYP BOOKTYPE_HS,BOOKTYPE_H,JAN_09,DEC_09                            
*                                                                               
         DC    X'FFFF'                                                          
*-----------------------------------------------------------------              
*                                                                               
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_LVBKYP                                                   
         EJECT                                                                  
***********************************************************************         
                                                                                
* EQUATE NSI OVERNIGHT MARKETS TO NSI NUMERIC                                   
* (ORIGINALLY FROM PAN BOOK DEONMKNO)                                           
TBL_ONITEMKT DS 0D                                                              
         DC    CL8'*ONMKT**'                                                    
         DS    XL6                                                              
         DC    AL2(ONITMKLQ)       L'TABLE ENTRY                                
*                                                                               
* EQUATE NSI OVERNIGHT MARKETS TO NSI NUMERIC                                   
*                                                                               
         ONITEM AQ,390,BOOKTYPE_STANDARD  ALBUQUERQUE-SANTA FE                  
         ONITEM AT,168,BOOKTYPE_STANDARD  ATLANTA                               
         ONITEM ATP,168,BOOKTYPE_P        ATLANTA                               
         ONITEM AU,235,BOOKTYPE_STANDARD  AUSTIN                                
         ONITEM BL,112,BOOKTYPE_STANDARD  BALTIMORE                             
         ONITEM BLP,112,BOOKTYPE_P        BALTIMORE                             
         ONITEM BH,230,BOOKTYPE_STANDARD  BIRMINGHAM                            
         ONITEM BN,106,BOOKTYPE_STANDARD  BOSTON                                
         ONITEM BF,114,BOOKTYPE_STANDARD  BUFFALO                               
*********ONITEM BP,106,BOOKTYPE_STANDARD  BOSTON PEOPLE METER                   
         ONITEM CT,117,BOOKTYPE_STANDARD  CHARLOTTE                             
         ONITEM CTP,117,BOOKTYPE_P        CHARLOTTE PRELIM PEOPLE METER         
         ONITEM CH,202,BOOKTYPE_STANDARD  CHICAGO                               
         ONITEM CHH,202,BOOKTYPE_H        CHICAGO HISPANIC                      
         ONITEM CHP,202,BOOKTYPE_P        CHICAGO PRELIM PEOPLE METER           
         ONITEM CI,115,BOOKTYPE_STANDARD  CINCINNATI                            
         ONITEM CL,110,BOOKTYPE_STANDARD  CLEVELAND                             
         ONITEM CLP,110,BOOKTYPE_P        CLEVELAND PRELIM PEOPLE METER         
         ONITEM CO,135,BOOKTYPE_STANDARD  COLUMBUS - WHICH ONE????              
         ONITEM DL,223,BOOKTYPE_STANDARD  DALLAS                                
         ONITEM DLP,223,BOOKTYPE_P        DALLAS                                
         ONITEM DY,142,BOOKTYPE_STANDARD  DAYTON                                
         ONITEM DV,351,BOOKTYPE_STANDARD  DENVER                                
         ONITEM DVP,351,BOOKTYPE_P        DENVER PRELIM PEOPLE METER            
         ONITEM DE,105,BOOKTYPE_STANDARD  DETROIT                               
         ONITEM DEP,105,BOOKTYPE_P        DALLAS                                
         ONITEM FM,171,BOOKTYPE_STANDARD  FT. MYERS-NAPLES                      
         ONITEM GR,118,BOOKTYPE_STANDARD  GREENSBORO                            
         ONITEM HT,133,BOOKTYPE_STANDARD  HARTFORD=NWH                          
         ONITEM HN,218,BOOKTYPE_STANDARD  HOUSTON                               
         ONITEM HNP,218,BOOKTYPE_P        HOUSTON PRELIM PEOPLE METER           
         ONITEM IN,127,BOOKTYPE_STANDARD  INDIANAPOLIS                          
         ONITEM JX,161,BOOKTYPE_STANDARD  JACKSONVILLE                          
         ONITEM KC,216,BOOKTYPE_STANDARD  KANSAS CITY                           
         ONITEM LV,439,BOOKTYPE_STANDARD  LAS VEGAS                             
         ONITEM LA,403,BOOKTYPE_STANDARD  LOS ANGELES                           
         ONITEM LAH,403,BOOKTYPE_H        LOS ANGELES HISPANIC                  
         ONITEM LAP,403,BOOKTYPE_P        LOS ANG. PRELIM PEOPLE METER          
         ONITEM LK,129,BOOKTYPE_STANDARD  LOUISVILLE                            
         ONITEM MS,240,BOOKTYPE_STANDARD  MEMPHIS                               
         ONITEM MI,128,BOOKTYPE_STANDARD  MIAMI                                 
         ONITEM MIP,128,BOOKTYPE_P        MIAMI PRELIM PEOPLE METER             
         ONITEM MIH,128,BOOKTYPE_H        MIAMI HISPANIC                        
         ONITEM HMI,128,BOOKTYPE_I        MIAMI LPM HISPANIC PRELIM             
         ONITEM ML,217,BOOKTYPE_STANDARD  MILWAUKEE                             
         ONITEM MN,213,BOOKTYPE_STANDARD  MINNEAPOLIS                           
         ONITEM MNP,213,BOOKTYPE_P        MINNEAPOLIS PRELIM PEOPLE MTR         
         ONITEM NV,259,BOOKTYPE_STANDARD  NASHVILLE                             
         ONITEM HA,189,BOOKTYPE_T         HARTFORD METRO -T                     
         ONITEM HH,288,BOOKTYPE_M         NEW HAVEN-METRO -M                    
         ONITEM NO,222,BOOKTYPE_STANDARD  NEW ORLEANS                           
         ONITEM NY,101,BOOKTYPE_STANDARD  NEW YORK                              
         ONITEM NYH,101,BOOKTYPE_H        NEW YORK HISPANIC                     
         ONITEM NYP,101,BOOKTYPE_P        NEW YORK PRELIM PEOPLE METER          
         ONITEM NF,144,BOOKTYPE_STANDARD  NORFOLK                               
         ONITEM OK,250,BOOKTYPE_STANDARD  OKLAHOMA CITY                         
         ONITEM OR,134,BOOKTYPE_STANDARD  ORLANDO                               
         ONITEM ORP,134,BOOKTYPE_P        ORLANDO PRELIM PEOPLE METER           
         ONITEM PH,104,BOOKTYPE_STANDARD  PHILADELPHIA                          
         ONITEM PHP,104,BOOKTYPE_P        PHILA. PRELIM PEOPLE METER            
         ONITEM PX,353,BOOKTYPE_STANDARD  PHOENIX                               
         ONITEM PXP,353,BOOKTYPE_P        PHOENIX                               
         ONITEM PT,108,BOOKTYPE_STANDARD  PITTSBURGH                            
         ONITEM PTP,108,BOOKTYPE_P        PITTSBURGH                            
         ONITEM PD,420,BOOKTYPE_STANDARD  PORTLAND                              
         ONITEM PDP,420,BOOKTYPE_P        PORTLAND                              
         ONITEM PV,121,BOOKTYPE_STANDARD  PROVIDENCE                            
         ONITEM RD,160,BOOKTYPE_STANDARD  RALIGH-DURHAM                         
         ONITEM RH,156,BOOKTYPE_STANDARD  RICHMOND-PETERBURG                    
         ONITEM SM,462,BOOKTYPE_STANDARD  SACRAMENTO                            
         ONITEM SMP,462,BOOKTYPE_P        SACRAMENTO PRELIM PEOPLE MTR.         
         ONITEM ST,370,BOOKTYPE_STANDARD  SALK LAKE CITY                        
         ONITEM SA,241,BOOKTYPE_STANDARD  SAN ANTONIO                           
         ONITEM SAH,241,BOOKTYPE_H        SAN ANTONIO HISPANIC                  
         ONITEM SD,425,BOOKTYPE_STANDARD  SAN DIEGO                             
         ONITEM SF,407,BOOKTYPE_STANDARD  SAN FRANCISCO                         
         ONITEM SFP,407,BOOKTYPE_P        SAN FRAN. PRELIM PEOPLE METER         
         ONITEM SE,419,BOOKTYPE_STANDARD  SEATTLE                               
         ONITEM SEP,419,BOOKTYPE_P        SEATTLE PRELIM PEOPLE METER           
         ONITEM SL,209,BOOKTYPE_STANDARD  ST LOUIS                              
         ONITEM SLP,209,BOOKTYPE_P        ST LOUIS PRELIM PEOPLE METER          
         ONITEM TP,139,BOOKTYPE_STANDARD  TAMPA                                 
         ONITEM TPP,139,BOOKTYPE_P        TAMPA PRELIM PEOPLE METER             
         ONITEM DC,111,BOOKTYPE_STANDARD  WASHINGTON DC                         
         ONITEM DCP,111,BOOKTYPE_P        WASHINGTON DC PRELIM LPM              
         ONITEM WP,148,BOOKTYPE_STANDARD  WEST PALM BCH                         
         ONITEM GS,167,BOOKTYPE_STANDARD  GREENSVILLE-SPART-ASHEVILLE           
         ONITEM KX,157,BOOKTYPE_STANDARD  KNOXVILLE                             
         ONITEM TL,271,BOOKTYPE_STANDARD  TULSA                                 
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ONITEMKT                                                 
         EJECT                                                                  
*                                                                               
                                                                                
*-------USTV BOOKTYPE TABLE ------------------------------------------          
* EQUATE 2-CHARACTER ALPHA BOOKTYPES TO 1-BYTE INTERNAL NUMERIC                 
* BYTE+0(2)=2 BYTE USER BOOKTYPE REPRESENTATION                                 
* BYTE+2(1)=1 BYTE INTERNAL BOOKTYPE REPRESENTATION                             
* BYTE+3(1)=1 BYTE BOOKTYPE INDICATOR                                           
*                                                                               
* BKTYPE INDICATOR - THIS BYTE IMPACTS SPOT POSTING.                            
* IT INDICATES WHETHER THIS BOOKTYPE IS AVAILABLE FOR MONTHLY, WEEKLY,          
* OVERNIGHTS.  IF INDICATOR IS TURNED OFF FOR WEEKLY OR OVERNIGHTS              
* THE POSTING WITH THE BOOKTYPE FOR WEEKLY OR OVERNIGHTS WILL REDIRECT          
* TO MONTHLY.                                                                   
*                                                                               
* BYTE+4(25)= 25 CHARACTER BOOKTYPE DESCRIPTION                                 
*                                                                               
*---- CHANGE THE FOLLOWING BYTES IF OVERNIGHT BECOMES NEWLY AVAILABLE           
*                                                                               
*         FOLLOWING 2 ENTRIES ALSO AFFECT SPOT POSTING.                         
* EX..IF WE GET A NEW BOOKTYPE FOR OVERNIGHTS AND TURN ON THE BOOKTYPE          
*     INDICATOR IN BYTE+3 THEN SPOT POSTING WOULD NOT POST OFF MONTHLY          
* ANYMORE EVEN FOR PERIODS BEFORE THE NEW BOOKTYPE WAS NOT AVAILABLE            
* THE EFFECTIVE BOOK ENTRIES BELOW ENSURES THE BOOKS BEFORE EFFECTIVE           
* BOOK WILL CONTINUE TO POST OFF MONTHLY                                        
* THE EFFECTIVE BOOK SHOULD ALWAYS BE THE START OF A FULL WEEK                  
* IF NIELSEN MAKES THE BOOKTYPE AVAILABLE IN THE MIDDLE OF THE MEASURED         
* WEEK, WE WOULD NOT HONOR THE WEEK UNTIL IT IS COMPLETE BECAUSE IT             
* WOULD JUST CAUSE TOO MUCH PROBLEMS FOR SPOT POSTING !                         
*                                                                               
*  ALWAYS SET THIS TO AL2(0) FOR BRAND NEW BOOKTYPE ENTRIES WHERE IT IS         
*  NOT AVAILABLE FOR OVERNIGHTS                                                 
*                                                                               
* IMPORTANT- ALWAYS KEEP USTV TABLE ENTRIES FOR THE SAME BOOKTYPE 1ST           
* IF THERE IS A BOOKTYPE DEFINED FOR USTV AND RADIO - USTV HAS TO BE            
* 1ST                                                                           
*                                                                               
                                                                                
TBL_SPBOOKTB DS 0D                                                              
         DC    CL8'*SPBKTB*'                                                    
         DS    XL6                                                              
         DC    AL2(SPBKTYLQ)       L'TABLE ENTRY                                
*                                                                               
         DC    CL2'Z1',AL1(BOOKTYPE_TEST),AL1(0)                                
         DC    AL2(0)                                                           
         DC    CL25'Test Booktype'                                              
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'LS',AL1(BOOKTYPE_LS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'Live+SD'                                                    
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'WS',AL1(BOOKTYPE_WS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'Live+SD Hardwired'                                          
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'HS',AL1(BOOKTYPE_HS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'Live+SD Hispanic'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
                                                                                
         DC    CL2'QS',AL1(BOOKTYPE_QS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'Live+SD Parent Only'                                        
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
                                                                                
         DC    CL2'SS',AL1(BOOKTYPE_SS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'Live+SD Wired Parent Only'                                  
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'L3',AL1(BOOKTYPE_L3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)    OVERNIGHT EFFECTIVE BOOK                  
         DC    CL25'Live+3'                                                     
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'W3',AL1(BOOKTYPE_W3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+3 Hardwired'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'C3',AL1(BOOKTYPE_C3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+3 Cable'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'Q3',AL1(BOOKTYPE_Q3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+3 Parent Only'                                         
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'H3',AL1(BOOKTYPE_H3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_43)                                             
         DC    CL25'Live+3 Hispanic'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'S3',AL1(BOOKTYPE_S3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+3 Wired Parent Only'                                   
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'OL',AL1(BOOKTYPE_OL),AL1(SPBKMON)                            
         DC    AL2(0)                                                           
         DC    CL25'Olympic Excl. Live'                                         
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'OS',AL1(BOOKTYPE_OS),AL1(SPBKMON)                            
         DC    AL2(0)                                                           
         DC    CL25'Olympic Excl. Live+SD'                                      
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'O3',AL1(BOOKTYPE_O3),AL1(SPBKMON)                            
         DC    AL2(0)                                                           
         DC    CL25'Olympic Excl. Live+3'                                       
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'O1',AL1(BOOKTYPE_O1),AL1(SPBKMON)                            
         DC    AL2(0)                                                           
         DC    CL25'Olympic Excl. Live+1'                                       
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'HT',AL1(BOOKTYPE_HT),AL1(SPBKMON)                            
         DC    AL2(0)                                                           
         DC    CL25'Hispanic Test'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'A ',AL1(BOOKTYPE_A),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Parent Only'                                                
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'B ',AL1(BOOKTYPE_B),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Black'                                                      
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV+SP_NSI_WTP_X        
               TV+SP_ARB_RTP_RADIO+SP_TRI_RTP_RADIO)                            
*                                                                               
         DC    CL2'C ',AL1(BOOKTYPE_C),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Cable'                                                      
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'C ',AL1(BOOKTYPE_C),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'CSAR'                                                       
         DC    AL4(SP_ARB_RTP_RADIO)                                            
*                                                                               
         DC    CL2'D ',AL1(BOOKTYPE_D),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'DMA Prv/Spcl'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'D ',AL1(BOOKTYPE_D),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday PPM'                                                
         DC    AL4(SP_ARB_RTP_RADIO)                                            
*                                                                               
         DC    CL2'D ',AL1(BOOKTYPE_D),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday'                                                    
         DC    AL4(SP_TRI_RTP_RADIO+SP_IHT)                                     
*                                                                               
         DC    CL2'E ',AL1(BOOKTYPE_E),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Extended'                                                   
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'E ',AL1(BOOKTYPE_E),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Black PPM'                                                  
         DC    AL4(SP_ARB_RTP_RADIO)                                            
*                                                                               
         DC    CL2'F ',AL1(BOOKTYPE_F),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25''                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'G ',AL1(BOOKTYPE_G),AL1(SPBKMON+SPBKWKLY+SPBKOVN)            
         DC    AL2(0)                                                           
         DC    CL25'General Booktype'                                           
         DC    AL4(SP_DONT_SHOW_DESCRIPTION)                                    
*                                                                               
         DC    CL2'H ',AL1(BOOKTYPE_H),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Hispanic'                                                   
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV+SP_ARB_RTP_RADIO+SP_TRI_RTP_RADIO)                            
*                                                                               
         DC    CL2'I ',AL1(BOOKTYPE_I),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Hispanic LPM'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'I ',AL1(BOOKTYPE_I),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Hispanic PPM'                                               
         DC    AL4(SP_ARB_RTP_RADIO)                                            
*                                                                               
         DC    CL2'J ',AL1(BOOKTYPE_J),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Live Hispanic'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'K ',AL1(BOOKTYPE_K),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday Black PPM'                                          
         DC    AL4(SP_ARB_RTP_RADIO)                                            
*                                                                               
         DC    CL2'K ',AL1(BOOKTYPE_K),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday Black'                                              
         DC    AL4(SP_TRI_RTP_RADIO)                                            
*                                                                               
         DC    CL2'L ',AL1(BOOKTYPE_L),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Live'                                                       
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV+SP_VID+SP_COM)                                                
*                                                                               
         DC    CL2'M ',AL1(BOOKTYPE_M),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Metro'                                                      
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'N ',AL1(BOOKTYPE_N),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Special FoxNet'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'O ',AL1(BOOKTYPE_O),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Olympic Excl.'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'P ',AL1(BOOKTYPE_P),AL1(SPBKMON+SPBKWKLY+SPBKOVN)            
         DC    AL2(0)                                                           
         DC    CL25'People Meter'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV+SP_ARB_RTP_RADIO+SP_NSI_LPMWLY_TV)                            
*                                                                               
         DC    CL2'Q ',AL1(BOOKTYPE_Q),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'R ',AL1(BOOKTYPE_R),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'S ',AL1(BOOKTYPE_S),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday Hispanic PPM'                                       
         DC    AL4(SP_ARB_RTP_RADIO+SP_TRI_RTP_RADIO)                           
*                                                                               
         DC    CL2'S ',AL1(BOOKTYPE_S),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Holiday Hispanic'                                           
         DC    AL4(SP_TRI_RTP_RADIO)                                            
*                                                                               
         DC    CL2'T ',AL1(BOOKTYPE_T),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Trading Area'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'U ',AL1(BOOKTYPE_U),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Live Cable'                                                 
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'V ',AL1(BOOKTYPE_V),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'W ',AL1(BOOKTYPE_W),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Hardwired Cable'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'X ',AL1(BOOKTYPE_X),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Misc. Exclusions'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'Y ',AL1(BOOKTYPE_Y),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'Z ',AL1(BOOKTYPE_Z),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Live Hardwired'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'0 ',AL1(BOOKTYPE_0),AL1(SPBKMON+SPBKWKLY+SPBKOVN)            
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'1 ',AL1(BOOKTYPE_1),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Standard 0 Cell'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'2 ',AL1(BOOKTYPE_2),AL1(SPBKMON)                             
         DC    AL2(0)                                                           
         DC    CL25'Hispanic 0 Cell'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'3 ',AL1(BOOKTYPE_3),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Cable 0 Cell'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'4 ',AL1(BOOKTYPE_4),AL1(SPBKMON+SPBKOVN)                     
         DC    AL2(0)                                                           
         DC    CL25'Hardwired 0 Cell'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'5 ',AL1(BOOKTYPE_5),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'SP. Dominant'                                               
         DC    AL4(SP_NHTI_TP+SP_NHTI_PAV)                                      
*                                                                               
         DC    CL2'6 ',AL1(BOOKTYPE_6),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Eng. Dominant'                                              
         DC    AL4(SP_NHTI_TP+SP_NHTI_PAV)                                      
*                                                                               
         DC    CL2'7 ',AL1(BOOKTYPE_7),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25'Bilingual'                                                  
         DC    AL4(SP_NHTI_TP+SP_NHTI_PAV)                                      
*                                                                               
         DC    CL2'8 ',AL1(BOOKTYPE_8),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'9 ',AL1(BOOKTYPE_9),AL1(0)                                   
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
* NOTE- BECAUSE THE WAY NWS WAS CODED- SPACE CAN NOT BE ADDED AS AN             
* ENTRY FOR STANDARD                                                            
*                                                                               
         DC    X'0000',AL1(BOOKTYPE_STANDARD),AL1(SPBKMON+SPBKWKLY+SPBKX        
               OVN)                                                             
         DC    AL2(0)                                                           
         DC    CL25'Standard Survey'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV+SP_NSI_WTP_TV+SP_NSI_LPMWLY_TV+SP_ARB_RTP_RADIO+SP_NSX        
               I_TDP_TV+SP_ARB_CTY_RADIO+SP_TRI_RTP_RADIO+SP_IHT+SP_XT)         
                                                                                
         DC    X'0000',AL1(BOOKTYPE_STANDARD),AL1(0)                            
         DC    AL2(0)                                                           
         DC    CL25'Weekly'                                                     
         DC    AL4(SP_BBM_WTP_TV+SP_CSI_WTP_TV)                                 
*                                                                               
         DC    X'0000',AL1(BOOKTYPE_STANDARD),AL1(SPBKMON)                      
         DC    AL2(0)                                                           
         DC    CL25'Total DMA'                                                  
         DC    AL4(SP_NCM)                                                      
*                                                                               
* NOTE- SPECIAL BOOKTYPES TO HANDLE NE HUT/PUT DEMO VALUES, THESE ARE           
* TEMPORARY.                                                                    
*                                                                               
*                                                                               
         DC    CL2'ZL',AL1(BOOKTYPE_ZL),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT Live'                                                   
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'ZU',AL1(BOOKTYPE_ZU),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT DMA Live'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZZ',AL1(BOOKTYPE_ZZ),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT Wired Live'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZJ',AL1(BOOKTYPE_ZJ),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT Hispanic Live'                                          
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'ZS',AL1(BOOKTYPE_ZS),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'HUT Live+SD'                                                
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'ZA',AL1(BOOKTYPE_ZA),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'HUT Wired L+SD'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'ZE',AL1(BOOKTYPE_ZE),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_2)                                              
         DC    CL25'HUT Hisp L+SD'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'Z3',AL1(BOOKTYPE_Z3),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)    OVERNIGHT EFFECTIVE BOOK                  
         DC    CL25'HUT Live+3'                                                 
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'ZD',AL1(BOOKTYPE_ZD),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'HUT DMA L+3'                                                
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZB',AL1(BOOKTYPE_ZB),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'HUT Wired L+3'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZF',AL1(BOOKTYPE_ZF),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2010,WEEK_43)                                             
         DC    CL25'HUT Hispanic L+3'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
         DC    CL2'Z7',AL1(BOOKTYPE_Z7),AL1(SPBKMON+SPBKWKLY+SPBKOVN)           
         DC    AL2(0)                                                           
         DC    CL25'HUT Live+7'                                                 
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV+SP_NSI_WTP_TV+SP_NSI_LPMWLY_TV+SP_NSI_TDP_TV)                 
*                                                                               
         DC    CL2'ZC',AL1(BOOKTYPE_ZC),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT DMA L+7'                                                
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZW',AL1(BOOKTYPE_ZW),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT Wired L+7'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'ZH',AL1(BOOKTYPE_ZH),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'HUT Hispanic L+7'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV+SP_NSI_OTP_TV+SP_NSI_OPA_X        
               TV)                                                              
*                                                                               
* HYBRID (LOCAL TV SAMPLE IMPACT) BOOKTYPES                                     
*                                                                               
*                                                                               
         DC    CL2'YL',AL1(BOOKTYPE_YL),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Live'                                                
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YU',AL1(BOOKTYPE_YU),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact DMA Live'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YZ',AL1(BOOKTYPE_YZ),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Wired Live'                                          
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YJ',AL1(BOOKTYPE_YJ),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Hispanic Live'                                       
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YS',AL1(BOOKTYPE_YS),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Live+SD'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YA',AL1(BOOKTYPE_YA),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Wired L+SD'                                          
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YE',AL1(BOOKTYPE_YE),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Hisp L+SD'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'Y3',AL1(BOOKTYPE_Y3),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Live+3'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YD',AL1(BOOKTYPE_YD),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact DMA L+3'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YB',AL1(BOOKTYPE_YB),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Wired L+3'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YF',AL1(BOOKTYPE_YF),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Hispanic L+3'                                        
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'Y7',AL1(BOOKTYPE_Y7),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Live+7'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YC',AL1(BOOKTYPE_YC),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact DMA L+7'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YW',AL1(BOOKTYPE_YW),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Wired L+7'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YH',AL1(BOOKTYPE_YH),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Hispanic L+7'                                        
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
* Live+1 booktypes                                                              
         DC    CL2'L1',AL1(BOOKTYPE_L1),AL1(SPBKMON+SPBKOVN)                    
         DC    AL2(0)                                                           
         DC    CL25'Live+1'                                                     
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'C1',AL1(BOOKTYPE_C1),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+1 Cable'                                               
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'W1',AL1(BOOKTYPE_W1),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+1 Hardwired'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
*                                                                               
         DC    CL2'H1',AL1(BOOKTYPE_H1),AL1(SPBKMON+SPBKOVN)                    
         DC    AL1(YR_2009,WEEK_6)                                              
         DC    CL25'Live+1 Hispanic'                                            
         DC    AL4(SP_NSI_TP_TV+SP_NSI_OTP_TV+SP_NSI_OPA_TV)                    
* impact live+1 booktypes                                                       
         DC    CL2'Y1',AL1(BOOKTYPE_Y1),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Live+1'                                              
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YK',AL1(BOOKTYPE_YK),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact DMA L+1'                                             
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YM',AL1(BOOKTYPE_YM),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Wired L+1'                                           
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
         DC    CL2'YN',AL1(BOOKTYPE_YN),AL1(SPBKMON+SPBKTP+SPBKPAV)             
         DC    AL1(YR_2015,WEEK_5)                                              
         DC    CL25'Impact Hispanic L+1'                                        
         DC    AL4(SP_NSI_TP_TV+SP_NSI_PAV_TV)                                  
*                                                                               
* TAKE UP X'FF' SO WE CAN'T USE IT.  THIS WILL ALLOW CALLER TO PASS             
* AROUND A BAD BOOKTYPE INDICATOR                                               
         DC    X'FFFF',X'FF',AL1(0)                                             
         DC    AL2(0)                                                           
         DC    CL25' '                                                          
         DC    AL4(0)                                                           
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_SPBOOKTB                                                 
         EJECT                                                                  
*                                                                               
****************************************************************                
* TABLE OF NIELSEN MONTH/DATE DEFINITIONS.                     *                
****************************************************************                
TBL_MTHDATES DS 0D                                                              
         DC    CL8'MTHDATES'                                                    
         DS    XL6                                                              
         DC    AL2(MTHDATLQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL1(92,08,30),AL1(92,09,27),AL2(SEP_92)                          
         DC    AL1(92,09,28),AL1(92,10,25),AL2(OCT_92)                          
         DC    AL1(92,10,26),AL1(92,11,22),AL2(NOV_92)                          
         DC    AL1(92,11,23),AL1(92,12,27),AL2(DEC_92)                          
         DC    AL1(92,12,28),AL1(93,01,24),AL2(JAN_93)                          
         DC    AL1(93,01,25),AL1(93,02,21),AL2(FEB_93)                          
         DC    AL1(93,02,22),AL1(93,03,28),AL2(MAR_93)                          
         DC    AL1(93,03,29),AL1(93,04,25),AL2(APR_93)                          
         DC    AL1(93,04,26),AL1(93,05,30),AL2(MAY_93)                          
         DC    AL1(93,05,31),AL1(93,06,27),AL2(JUN_93)                          
         DC    AL1(93,06,28),AL1(93,07,25),AL2(JUL_93)                          
         DC    AL1(93,07,26),AL1(93,08,29),AL2(AUG_93)                          
         DC    AL1(93,08,30),AL1(93,09,26),AL2(SEP_93)                          
*                                                                               
         DC    AL1(93,09,27),AL1(93,10,24),AL2(OCT_93)                          
         DC    AL1(93,10,25),AL1(93,11,21),AL2(NOV_93)                          
         DC    AL1(93,11,22),AL1(93,12,26),AL2(DEC_93)                          
         DC    AL1(93,12,27),AL1(94,01,30),AL2(JAN_94)                          
         DC    AL1(94,01,31),AL1(94,02,27),AL2(FEB_94)                          
         DC    AL1(94,02,28),AL1(94,03,27),AL2(MAR_94)                          
         DC    AL1(94,03,28),AL1(94,05,01),AL2(APR_94)                          
         DC    AL1(94,05,02),AL1(94,05,29),AL2(MAY_94)                          
         DC    AL1(94,05,30),AL1(94,07,03),AL2(JUN_94)                          
         DC    AL1(94,07,04),AL1(94,07,31),AL2(JUL_94)                          
         DC    AL1(94,08,01),AL1(94,09,04),AL2(AUG_94)                          
         DC    AL1(94,09,05),AL1(94,10,02),AL2(SEP_94)                          
*                                                                               
         DC    AL1(94,10,03),AL1(94,10,30),AL2(OCT_94)                          
         DC    AL1(94,10,31),AL1(94,11,27),AL2(NOV_94)                          
         DC    AL1(94,11,28),AL1(95,01,01),AL2(DEC_94)                          
         DC    AL1(95,01,02),AL1(95,01,29),AL2(JAN_95)                          
         DC    AL1(95,01,30),AL1(95,02,26),AL2(FEB_95)                          
         DC    AL1(95,02,27),AL1(95,03,26),AL2(MAR_95)                          
         DC    AL1(95,03,27),AL1(95,04,30),AL2(APR_95)                          
         DC    AL1(95,05,01),AL1(95,05,28),AL2(MAY_95)                          
         DC    AL1(95,05,29),AL1(95,06,25),AL2(JUN_95)                          
         DC    AL1(95,06,26),AL1(95,07,30),AL2(JUL_95)                          
         DC    AL1(95,07,31),AL1(95,09,03),AL2(AUG_95)                          
         DC    AL1(95,09,04),AL1(95,10,01),AL2(SEP_95)                          
*                                                                               
         DC    AL1(95,10,02),AL1(95,10,29),AL2(OCT_95)                          
         DC    AL1(95,10,30),AL1(95,11,26),AL2(NOV_95)                          
         DC    AL1(95,11,27),AL1(95,12,31),AL2(DEC_95)                          
         DC    AL1(96,01,01),AL1(96,01,28),AL2(JAN_96)                          
         DC    AL1(96,01,29),AL1(96,02,25),AL2(FEB_96)                          
         DC    AL1(96,02,26),AL1(96,03,31),AL2(MAR_96)                          
         DC    AL1(96,04,01),AL1(96,04,28),AL2(APR_96)                          
         DC    AL1(96,04,29),AL1(96,05,26),AL2(MAY_96)                          
         DC    AL1(96,05,27),AL1(96,06,30),AL2(JUN_96)                          
         DC    AL1(96,07,01),AL1(96,07,28),AL2(JUL_96)                          
         DC    AL1(96,07,29),AL1(96,09,01),AL2(AUG_96)                          
         DC    AL1(96,09,02),AL1(96,09,29),AL2(SEP_96)                          
*                                                                               
         DC    AL1(96,09,30),AL1(96,10,27),AL2(OCT_96)                          
         DC    AL1(96,10,28),AL1(96,11,24),AL2(NOV_96)                          
         DC    AL1(96,11,25),AL1(96,12,29),AL2(DEC_96)                          
         DC    AL1(96,12,30),AL1(97,01,26),AL2(JAN_97)                          
         DC    AL1(97,01,27),AL1(97,02,23),AL2(FEB_97)                          
         DC    AL1(97,02,24),AL1(97,03,30),AL2(MAR_97)                          
         DC    AL1(97,03,31),AL1(97,04,27),AL2(APR_97)                          
         DC    AL1(97,04,28),AL1(97,05,25),AL2(MAY_97)                          
         DC    AL1(97,05,26),AL1(97,06,29),AL2(JUN_97)                          
         DC    AL1(97,06,30),AL1(97,07,27),AL2(JUL_97)                          
         DC    AL1(97,07,28),AL1(97,08,31),AL2(AUG_97)                          
         DC    AL1(97,09,01),AL1(97,09,28),AL2(SEP_97)                          
*                                                                               
         DC    AL1(97,09,29),AL1(97,10,26),AL2(OCT_97)                          
         DC    AL1(97,10,27),AL1(97,11,23),AL2(NOV_97)                          
         DC    AL1(97,11,24),AL1(97,12,28),AL2(DEC_97)                          
         DC    AL1(97,12,29),AL1(98,01,25),AL2(JAN_98)                          
         DC    AL1(98,01,26),AL1(98,02,22),AL2(FEB_98)                          
         DC    AL1(98,02,23),AL1(98,03,29),AL2(MAR_98)                          
         DC    AL1(98,03,30),AL1(98,04,26),AL2(APR_98)                          
         DC    AL1(98,04,25),AL1(98,05,24),AL2(MAY_98)                          
         DC    AL1(98,05,25),AL1(98,06,28),AL2(JUN_98)                          
         DC    AL1(98,06,29),AL1(98,07,26),AL2(JUL_98)                          
         DC    AL1(98,07,27),AL1(98,08,30),AL2(AUG_98)                          
         DC    AL1(98,08,31),AL1(98,09,27),AL2(SEP_98)                          
*                                                                               
         DC    AL1(98,09,28),AL1(98,10,25),AL2(OCT_98)                          
         DC    AL1(98,10,26),AL1(98,11,29),AL2(NOV_98)                          
         DC    AL1(98,11,30),AL1(98,12,27),AL2(DEC_98)                          
         DC    AL1(98,12,28),AL1(99,01,31),AL2(JAN_99)                          
         DC    AL1(99,02,01),AL1(99,02,28),AL2(FEB_99)                          
         DC    AL1(99,03,01),AL1(99,03,28),AL2(MAR_99)                          
         DC    AL1(99,03,29),AL1(99,04,25),AL2(APR_99)                          
         DC    AL1(99,04,26),AL1(99,05,30),AL2(MAY_99)                          
         DC    AL1(99,05,31),AL1(99,06,27),AL2(JUN_99)                          
         DC    AL1(99,06,28),AL1(99,07,25),AL2(JUL_99)                          
         DC    AL1(99,07,26),AL1(99,08,29),AL2(AUG_99)                          
         DC    AL1(99,08,30),AL1(99,09,26),AL2(SEP_99)                          
*                                                                               
         DC    AL1(99,09,27),AL1(99,10,31),AL2(OCT_99)                          
         DC    AL1(99,11,01),AL1(99,11,28),AL2(NOV_99)                          
         DC    AL1(99,11,29),AL1(99,12,26),AL2(DEC_99)                          
         DC    AL1(99,12,27),AL1(100,01,30),AL2(JAN_00)                         
         DC    AL1(100,01,31),AL1(100,02,27),AL2(FEB_00)                        
         DC    AL1(100,02,28),AL1(100,03,26),AL2(MAR_00)                        
         DC    AL1(100,03,27),AL1(100,04,30),AL2(APR_00)                        
         DC    AL1(100,05,01),AL1(100,05,28),AL2(MAY_00)                        
         DC    AL1(100,05,29),AL1(100,06,25),AL2(JUN_00)                        
         DC    AL1(100,06,26),AL1(100,07,30),AL2(JUL_00)                        
         DC    AL1(100,07,31),AL1(100,09,03),AL2(AUG_00)                        
         DC    AL1(100,09,04),AL1(100,10,01),AL2(SEP_00)                        
*                                                                               
         DC    AL1(100,10,02),AL1(100,10,29),AL2(OCT_00)                        
         DC    AL1(100,10,30),AL1(100,11,26),AL2(NOV_00)                        
         DC    AL1(100,11,27),AL1(100,12,31),AL2(DEC_00)                        
         DC    AL1(101,01,01),AL1(101,01,28),AL2(JAN_01)                        
         DC    AL1(101,01,29),AL1(101,02,25),AL2(FEB_01)                        
         DC    AL1(101,02,26),AL1(101,04,01),AL2(MAR_01)                        
         DC    AL1(101,04,02),AL1(101,04,29),AL2(APR_01)                        
         DC    AL1(101,04,30),AL1(101,05,27),AL2(MAY_01)                        
         DC    AL1(101,05,28),AL1(101,07,01),AL2(JUN_01)                        
         DC    AL1(101,07,02),AL1(101,07,29),AL2(JUL_01)                        
         DC    AL1(101,07,30),AL1(101,08,26),AL2(AUG_01)                        
         DC    AL1(101,08,27),AL1(101,09,30),AL2(SEP_01)                        
*                                                                               
         DC    AL1(101,10,01),AL1(101,10,28),AL2(OCT_01)                        
         DC    AL1(101,10,29),AL1(101,11,25),AL2(NOV_01)                        
         DC    AL1(101,11,26),AL1(101,12,30),AL2(DEC_01)                        
         DC    AL1(101,12,31),AL1(102,01,28),AL2(JAN_02)                        
         DC    AL1(102,01,29),AL1(102,02,24),AL2(FEB_02)                        
         DC    AL1(102,02,25),AL1(102,03,31),AL2(MAR_02)                        
         DC    AL1(102,04,01),AL1(102,04,28),AL2(APR_02)                        
         DC    AL1(102,04,29),AL1(102,05,26),AL2(MAY_02)                        
         DC    AL1(102,05,27),AL1(102,06,30),AL2(JUN_02)                        
         DC    AL1(102,07,01),AL1(102,07,28),AL2(JUL_02)                        
         DC    AL1(102,07,29),AL1(102,08,25),AL2(AUG_02)                        
         DC    AL1(102,08,26),AL1(102,09,29),AL2(SEP_02)                        
*                                                                               
         DC    AL1(102,09,30),AL1(102,10,27),AL2(OCT_02)                        
         DC    AL1(102,10,28),AL1(102,11,24),AL2(NOV_02)                        
         DC    AL1(102,11,25),AL1(102,12,29),AL2(DEC_02)                        
         DC    AL1(102,12,30),AL1(103,01,26),AL2(JAN_03)                        
         DC    AL1(103,01,27),AL1(103,02,23),AL2(FEB_03)                        
         DC    AL1(103,02,24),AL1(103,03,30),AL2(MAR_03)                        
         DC    AL1(103,03,31),AL1(103,04,27),AL2(APR_03)                        
         DC    AL1(103,04,28),AL1(103,05,25),AL2(MAY_03)                        
         DC    AL1(103,05,26),AL1(103,06,29),AL2(JUN_03)                        
         DC    AL1(103,06,30),AL1(103,07,27),AL2(JUL_03)                        
         DC    AL1(103,07,28),AL1(103,08,31),AL2(AUG_03)                        
         DC    AL1(103,09,01),AL1(103,09,28),AL2(SEP_03)                        
*                                                                               
         DC    AL1(103,09,29),AL1(103,10,26),AL2(OCT_03)                        
         DC    AL1(103,10,27),AL1(103,11,30),AL2(NOV_03)                        
         DC    AL1(103,12,01),AL1(103,12,28),AL2(DEC_03)                        
         DC    AL1(103,12,29),AL1(104,01,25),AL2(JAN_04)                        
         DC    AL1(104,01,26),AL1(104,02,29),AL2(FEB_04)                        
         DC    AL1(104,03,01),AL1(104,03,28),AL2(MAR_04)                        
         DC    AL1(104,03,29),AL1(104,04,25),AL2(APR_04)                        
         DC    AL1(104,04,26),AL1(104,05,30),AL2(MAY_04)                        
         DC    AL1(104,05,31),AL1(104,06,27),AL2(JUN_04)                        
         DC    AL1(104,06,28),AL1(104,07,25),AL2(JUL_04)                        
         DC    AL1(104,07,26),AL1(104,08,29),AL2(AUG_04)                        
         DC    AL1(104,08,30),AL1(104,09,26),AL2(SEP_04)                        
*                                                                               
         DC    AL1(104,09,27),AL1(104,10,31),AL2(OCT_04)                        
         DC    AL1(104,11,01),AL1(104,11,28),AL2(NOV_04)                        
         DC    AL1(104,11,29),AL1(104,12,26),AL2(DEC_04)                        
         DC    AL1(104,12,27),AL1(105,01,30),AL2(JAN_05)                        
         DC    AL1(105,01,31),AL1(105,02,27),AL2(FEB_05)                        
         DC    AL1(105,02,28),AL1(105,03,27),AL2(MAR_05)                        
         DC    AL1(105,03,28),AL1(105,04,24),AL2(APR_05)                        
         DC    AL1(105,04,25),AL1(105,05,29),AL2(MAY_05)                        
         DC    AL1(105,05,30),AL1(105,06,26),AL2(JUN_05)                        
         DC    AL1(105,06,27),AL1(105,07,31),AL2(JUL_05)                        
         DC    AL1(105,08,01),AL1(105,08,28),AL2(AUG_05)                        
         DC    AL1(105,08,29),AL1(105,09,25),AL2(SEP_05)                        
*                                                                               
         DC    AL1(105,09,26),AL1(105,10,30),AL2(OCT_05)                        
         DC    AL1(105,10,31),AL1(105,11,27),AL2(NOV_05)                        
         DC    AL1(105,11,28),AL1(105,12,25),AL2(DEC_05)                        
         DC    AL1(105,12,26),AL1(106,01,29),AL2(JAN_06)                        
         DC    AL1(106,01,30),AL1(106,02,26),AL2(FEB_06)                        
         DC    AL1(106,02,27),AL1(106,03,26),AL2(MAR_06)                        
         DC    AL1(106,03,27),AL1(106,04,30),AL2(APR_06)                        
         DC    AL1(106,05,01),AL1(106,05,28),AL2(MAY_06)                        
         DC    AL1(106,05,29),AL1(106,06,25),AL2(JUN_06)                        
         DC    AL1(106,06,26),AL1(106,07,30),AL2(JUL_06)                        
         DC    AL1(106,07,31),AL1(106,08,27),AL2(AUG_06)                        
         DC    AL1(106,08,28),AL1(106,09,24),AL2(SEP_06)                        
*                                                                               
         DC    AL1(106,09,25),AL1(106,10,29),AL2(OCT_06)                        
         DC    AL1(106,10,30),AL1(106,11,26),AL2(NOV_06)                        
         DC    AL1(106,11,27),AL1(106,12,31),AL2(DEC_06)                        
         DC    AL1(107,01,01),AL1(107,01,28),AL2(JAN_07)                        
         DC    AL1(107,01,29),AL1(107,02,25),AL2(FEB_07)                        
         DC    AL1(107,02,26),AL1(107,04,01),AL2(MAR_07)                        
         DC    AL1(107,04,02),AL1(107,04,29),AL2(APR_07)                        
         DC    AL1(107,04,30),AL1(107,05,27),AL2(MAY_07)                        
         DC    AL1(107,05,28),AL1(107,07,01),AL2(JUN_07)                        
         DC    AL1(107,07,02),AL1(107,07,29),AL2(JUL_07)                        
         DC    AL1(107,07,30),AL1(107,08,26),AL2(AUG_07)                        
         DC    AL1(107,08,27),AL1(107,09,30),AL2(SEP_07)                        
*                                                                               
         DC    AL1(107,10,01),AL1(107,10,28),AL2(OCT_07)                        
         DC    AL1(107,10,29),AL1(107,11,25),AL2(NOV_07)                        
         DC    AL1(107,11,26),AL1(107,12,30),AL2(DEC_07)                        
         DC    AL1(107,12,31),AL1(108,01,27),AL2(JAN_08)                        
         DC    AL1(108,01,28),AL1(108,02,24),AL2(FEB_08)                        
         DC    AL1(108,02,25),AL1(108,03,30),AL2(MAR_08)                        
         DC    AL1(108,03,31),AL1(108,04,27),AL2(APR_08)                        
         DC    AL1(108,04,28),AL1(108,05,25),AL2(MAY_08)                        
         DC    AL1(108,05,26),AL1(108,06,29),AL2(JUN_08)                        
         DC    AL1(108,06,30),AL1(108,07,27),AL2(JUL_08)                        
         DC    AL1(108,07,28),AL1(108,08,31),AL2(AUG_08)                        
         DC    AL1(108,09,01),AL1(108,09,28),AL2(SEP_08)                        
*                                                                               
         DC    AL1(108,09,29),AL1(108,10,26),AL2(OCT_08)                        
         DC    AL1(108,10,27),AL1(108,11,30),AL2(NOV_08)                        
         DC    AL1(108,12,01),AL1(108,12,28),AL2(DEC_08)                        
         DC    AL1(108,12,29),AL1(109,01,25),AL2(JAN_09)                        
         DC    AL1(109,01,26),AL1(109,02,22),AL2(FEB_09)                        
         DC    AL1(109,02,23),AL1(109,03,29),AL2(MAR_09)                        
         DC    AL1(109,03,30),AL1(109,04,26),AL2(APR_09)                        
         DC    AL1(109,04,27),AL1(109,05,31),AL2(MAY_09)                        
         DC    AL1(109,06,01),AL1(109,06,28),AL2(JUN_09)                        
         DC    AL1(109,06,29),AL1(109,07,26),AL2(JUL_09)                        
         DC    AL1(109,07,27),AL1(109,08,30),AL2(AUG_09)                        
         DC    AL1(109,08,31),AL1(109,09,27),AL2(SEP_09)                        
*                                                                               
         DC    AL1(109,09,28),AL1(109,10,25),AL2(OCT_09)                        
         DC    AL1(109,10,26),AL1(109,11,29),AL2(NOV_09)                        
         DC    AL1(109,11,30),AL1(109,12,27),AL2(DEC_09)                        
         DC    AL1(109,12,28),AL1(110,01,31),AL2(JAN_10)                        
         DC    AL1(110,02,01),AL1(110,02,28),AL2(FEB_10)                        
         DC    AL1(110,03,01),AL1(110,03,28),AL2(MAR_10)                        
         DC    AL1(110,03,29),AL1(110,04,25),AL2(APR_10)                        
         DC    AL1(110,04,26),AL1(110,05,30),AL2(MAY_10)                        
         DC    AL1(110,05,31),AL1(110,06,27),AL2(JUN_10)                        
         DC    AL1(110,06,28),AL1(110,07,25),AL2(JUL_10)                        
         DC    AL1(110,07,26),AL1(110,08,29),AL2(AUG_10)                        
         DC    AL1(110,08,30),AL1(110,09,26),AL2(SEP_10)                        
*                                                                               
         DC    AL1(110,09,27),AL1(110,10,31),AL2(OCT_10)                        
         DC    AL1(110,11,01),AL1(110,11,28),AL2(NOV_10)                        
         DC    AL1(110,11,29),AL1(110,12,26),AL2(DEC_10)                        
         DC    AL1(110,12,27),AL1(111,01,30),AL2(JAN_11)                        
         DC    AL1(111,01,31),AL1(111,02,27),AL2(FEB_11)                        
         DC    AL1(111,02,28),AL1(111,03,27),AL2(MAR_11)                        
         DC    AL1(111,03,28),AL1(111,04,24),AL2(APR_11)                        
         DC    AL1(111,04,25),AL1(111,05,29),AL2(MAY_11)                        
         DC    AL1(111,05,30),AL1(111,06,26),AL2(JUN_11)                        
         DC    AL1(111,06,27),AL1(111,07,31),AL2(JUL_11)                        
         DC    AL1(111,08,01),AL1(111,08,28),AL2(AUG_11)                        
         DC    AL1(111,08,29),AL1(111,09,25),AL2(SEP_11)                        
*                                                                               
         DC    AL1(111,09,26),AL1(111,10,30),AL2(OCT_11)                        
         DC    AL1(111,10,31),AL1(111,11,27),AL2(NOV_11)                        
         DC    AL1(111,11,28),AL1(111,12,25),AL2(DEC_11)                        
         DC    AL1(111,12,26),AL1(112,01,29),AL2(JAN_12)                        
         DC    AL1(112,01,30),AL1(112,02,26),AL2(FEB_12)                        
         DC    AL1(112,02,27),AL1(112,03,25),AL2(MAR_12)                        
         DC    AL1(112,03,26),AL1(112,04,29),AL2(APR_12)                        
         DC    AL1(112,04,30),AL1(112,05,27),AL2(MAY_12)                        
         DC    AL1(112,05,28),AL1(112,06,24),AL2(JUN_12)                        
         DC    AL1(112,06,25),AL1(112,07,29),AL2(JUL_12)                        
         DC    AL1(112,07,30),AL1(112,08,26),AL2(AUG_12)                        
         DC    AL1(112,08,27),AL1(112,09,30),AL2(SEP_12)                        
*                                                                               
         DC    AL1(112,10,01),AL1(112,10,28),AL2(OCT_12)                        
         DC    AL1(112,10,29),AL1(112,11,25),AL2(NOV_12)                        
         DC    AL1(112,11,26),AL1(112,12,30),AL2(DEC_12)                        
         DC    AL1(112,12,31),AL1(113,01,27),AL2(JAN_13)                        
         DC    AL1(113,01,28),AL1(113,02,24),AL2(FEB_13)                        
         DC    AL1(113,02,25),AL1(113,03,31),AL2(MAR_13)                        
         DC    AL1(113,04,01),AL1(113,04,28),AL2(APR_13)                        
         DC    AL1(113,04,29),AL1(113,05,26),AL2(MAY_13)                        
         DC    AL1(113,05,27),AL1(113,06,30),AL2(JUN_13)                        
         DC    AL1(113,07,01),AL1(113,07,28),AL2(JUL_13)                        
         DC    AL1(113,07,29),AL1(113,08,25),AL2(AUG_13)                        
         DC    AL1(113,08,26),AL1(113,09,29),AL2(SEP_13)                        
*                                                                               
         DC    AL1(113,09,30),AL1(113,10,27),AL2(OCT_13)                        
         DC    AL1(113,10,28),AL1(113,11,24),AL2(NOV_13)                        
         DC    AL1(113,11,25),AL1(113,12,29),AL2(DEC_13)                        
         DC    AL1(113,12,30),AL1(114,01,26),AL2(JAN_14)                        
         DC    AL1(114,01,27),AL1(114,02,23),AL2(FEB_14)                        
         DC    AL1(114,02,24),AL1(114,03,30),AL2(MAR_14)                        
         DC    AL1(114,03,31),AL1(114,04,27),AL2(APR_14)                        
         DC    AL1(114,04,28),AL1(114,05,25),AL2(MAY_14)                        
         DC    AL1(114,05,26),AL1(114,06,29),AL2(JUN_14)                        
         DC    AL1(114,06,30),AL1(114,07,27),AL2(JUL_14)                        
         DC    AL1(114,07,28),AL1(114,08,31),AL2(AUG_14)                        
         DC    AL1(114,09,01),AL1(114,09,28),AL2(SEP_14)                        
*                                                                               
         DC AL1(YR_2014,MON_SEP,29),AL1(YR_2014,MON_OCT,26),AL2(OCT_14)         
         DC AL1(YR_2014,MON_OCT,27),AL1(YR_2014,MON_NOV,30),AL2(NOV_14)         
         DC AL1(YR_2014,MON_DEC,01),AL1(YR_2014,MON_DEC,28),AL2(DEC_14)         
         DC AL1(YR_2014,MON_DEC,29),AL1(YR_2015,MON_JAN,25),AL2(JAN_15)         
         DC AL1(YR_2015,MON_JAN,26),AL1(YR_2015,MON_FEB,22),AL2(FEB_15)         
         DC AL1(YR_2015,MON_FEB,23),AL1(YR_2015,MON_MAR,29),AL2(MAR_15)         
         DC AL1(YR_2015,MON_MAR,30),AL1(YR_2015,MON_APR,26),AL2(APR_15)         
         DC AL1(YR_2015,MON_APR,27),AL1(YR_2015,MON_MAY,31),AL2(MAY_15)         
         DC AL1(YR_2015,MON_JUN,01),AL1(YR_2015,MON_JUN,28),AL2(JUN_15)         
         DC AL1(YR_2015,MON_JUN,29),AL1(YR_2015,MON_JUL,26),AL2(JUL_15)         
         DC AL1(YR_2015,MON_JUL,27),AL1(YR_2015,MON_AUG,30),AL2(AUG_15)         
         DC AL1(YR_2015,MON_AUG,31),AL1(YR_2015,MON_SEP,27),AL2(SEP_15)         
*                                                                               
         DC AL1(YR_2015,MON_SEP,28),AL1(YR_2015,MON_OCT,25),AL2(OCT_15)         
         DC AL1(YR_2015,MON_OCT,26),AL1(YR_2015,MON_NOV,29),AL2(NOV_15)         
         DC AL1(YR_2015,MON_NOV,30),AL1(YR_2015,MON_DEC,27),AL2(DEC_15)         
         DC AL1(YR_2015,MON_DEC,28),AL1(YR_2016,MON_JAN,31),AL2(JAN_16)         
         DC AL1(YR_2016,MON_FEB,01),AL1(YR_2016,MON_FEB,28),AL2(FEB_16)         
         DC AL1(YR_2016,MON_FEB,29),AL1(YR_2016,MON_MAR,27),AL2(MAR_16)         
         DC AL1(YR_2016,MON_MAR,28),AL1(YR_2016,MON_APR,24),AL2(APR_16)         
         DC AL1(YR_2016,MON_APR,25),AL1(YR_2016,MON_MAY,29),AL2(MAY_16)         
         DC AL1(YR_2016,MON_MAY,30),AL1(YR_2016,MON_JUN,26),AL2(JUN_16)         
         DC AL1(YR_2016,MON_JUN,27),AL1(YR_2016,MON_JUL,31),AL2(JUL_16)         
         DC AL1(YR_2016,MON_AUG,01),AL1(YR_2016,MON_AUG,28),AL2(AUG_16)         
         DC AL1(YR_2016,MON_AUG,29),AL1(YR_2016,MON_SEP,25),AL2(SEP_16)         
*                                                                               
         DC AL1(YR_2016,MON_SEP,26),AL1(YR_2016,MON_OCT,30),AL2(OCT_16)         
         DC AL1(YR_2016,MON_OCT,31),AL1(YR_2016,MON_NOV,27),AL2(NOV_16)         
         DC AL1(YR_2016,MON_NOV,28),AL1(YR_2016,MON_DEC,25),AL2(DEC_16)         
         DC AL1(YR_2016,MON_DEC,26),AL1(YR_2017,MON_JAN,29),AL2(JAN_17)         
         DC AL1(YR_2017,MON_JAN,30),AL1(YR_2017,MON_FEB,26),AL2(FEB_17)         
         DC AL1(YR_2017,MON_FEB,27),AL1(YR_2017,MON_MAR,26),AL2(MAR_17)         
         DC AL1(YR_2017,MON_MAR,27),AL1(YR_2017,MON_APR,30),AL2(APR_17)         
         DC AL1(YR_2017,MON_MAY,01),AL1(YR_2017,MON_MAY,28),AL2(MAY_17)         
         DC AL1(YR_2017,MON_MAY,29),AL1(YR_2017,MON_JUN,25),AL2(JUN_17)         
         DC AL1(YR_2017,MON_JUN,26),AL1(YR_2017,MON_JUL,30),AL2(JUL_17)         
         DC AL1(YR_2017,MON_JUL,31),AL1(YR_2017,MON_AUG,27),AL2(AUG_17)         
         DC AL1(YR_2017,MON_AUG,28),AL1(YR_2017,MON_SEP,24),AL2(SEP_17)         
*                                                                               
         DC AL1(YR_2017,MON_SEP,25),AL1(YR_2017,MON_OCT,29),AL2(OCT_17)         
         DC AL1(YR_2017,MON_OCT,30),AL1(YR_2017,MON_NOV,26),AL2(NOV_17)         
         DC AL1(YR_2017,MON_NOV,27),AL1(YR_2017,MON_DEC,31),AL2(DEC_17)         
         DC AL1(YR_2018,MON_JAN,01),AL1(YR_2018,MON_JAN,28),AL2(JAN_18)         
         DC AL1(YR_2018,MON_JAN,29),AL1(YR_2018,MON_FEB,25),AL2(FEB_18)         
         DC AL1(YR_2018,MON_FEB,26),AL1(YR_2018,MON_APR,01),AL2(MAR_18)         
         DC AL1(YR_2018,MON_APR,02),AL1(YR_2018,MON_APR,29),AL2(APR_18)         
         DC AL1(YR_2018,MON_APR,30),AL1(YR_2018,MON_MAY,27),AL2(MAY_18)         
         DC AL1(YR_2018,MON_MAY,28),AL1(YR_2018,MON_JUL,01),AL2(JUN_18)         
         DC AL1(YR_2018,MON_JUL,02),AL1(YR_2018,MON_JUL,29),AL2(JUL_18)         
         DC AL1(YR_2018,MON_JUL,30),AL1(YR_2018,MON_AUG,26),AL2(AUG_18)         
         DC AL1(YR_2018,MON_AUG,27),AL1(YR_2018,MON_SEP,30),AL2(SEP_18)         
*                                                                               
         DC AL1(YR_2018,MON_OCT,01),AL1(YR_2018,MON_OCT,28),AL2(OCT_18)         
         DC AL1(YR_2018,MON_OCT,29),AL1(YR_2018,MON_NOV,25),AL2(NOV_18)         
         DC AL1(YR_2018,MON_NOV,26),AL1(YR_2018,MON_DEC,30),AL2(DEC_18)         
         DC AL1(YR_2018,MON_DEC,31),AL1(YR_2019,MON_JAN,27),AL2(JAN_19)         
         DC AL1(YR_2019,MON_JAN,28),AL1(YR_2019,MON_FEB,24),AL2(FEB_19)         
         DC AL1(YR_2019,MON_FEB,25),AL1(YR_2019,MON_MAR,31),AL2(MAR_19)         
         DC AL1(YR_2019,MON_APR,01),AL1(YR_2019,MON_APR,28),AL2(APR_19)         
         DC AL1(YR_2019,MON_APR,29),AL1(YR_2019,MON_MAY,26),AL2(MAY_19)         
         DC AL1(YR_2019,MON_MAY,27),AL1(YR_2019,MON_JUN,30),AL2(JUN_19)         
         DC AL1(YR_2019,MON_JUL,01),AL1(YR_2019,MON_JUL,28),AL2(JUL_19)         
         DC AL1(YR_2019,MON_JUL,29),AL1(YR_2019,MON_AUG,25),AL2(AUG_19)         
         DC AL1(YR_2019,MON_AUG,26),AL1(YR_2019,MON_SEP,29),AL2(SEP_19)         
*                                                                               
         DC AL1(YR_2019,MON_SEP,30),AL1(YR_2019,MON_OCT,27),AL2(OCT_19)         
         DC AL1(YR_2019,MON_OCT,28),AL1(YR_2019,MON_NOV,24),AL2(NOV_19)         
         DC AL1(YR_2019,MON_NOV,25),AL1(YR_2019,MON_DEC,29),AL2(DEC_19)         
         DC AL1(YR_2019,MON_DEC,30),AL1(YR_2020,MON_JAN,26),AL2(JAN_20)         
         DC AL1(YR_2020,MON_JAN,27),AL1(YR_2020,MON_FEB,23),AL2(FEB_20)         
         DC AL1(YR_2020,MON_FEB,24),AL1(YR_2020,MON_MAR,29),AL2(MAR_20)         
         DC AL1(YR_2020,MON_MAR,30),AL1(YR_2020,MON_APR,26),AL2(APR_20)         
         DC AL1(YR_2020,MON_APR,27),AL1(YR_2020,MON_MAY,31),AL2(MAY_20)         
         DC AL1(YR_2020,MON_JUN,01),AL1(YR_2020,MON_JUN,28),AL2(JUN_20)         
         DC AL1(YR_2020,MON_JUN,29),AL1(YR_2020,MON_JUL,26),AL2(JUL_20)         
         DC AL1(YR_2020,MON_JUL,27),AL1(YR_2020,MON_AUG,30),AL2(AUG_20)         
         DC AL1(YR_2020,MON_AUG,31),AL1(YR_2020,MON_SEP,27),AL2(SEP_20)         
*                                                                               
         DC AL1(YR_2020,MON_SEP,28),AL1(YR_2020,MON_OCT,25),AL2(OCT_20)         
         DC AL1(YR_2020,MON_OCT,26),AL1(YR_2020,MON_NOV,29),AL2(NOV_20)         
         DC AL1(YR_2020,MON_NOV,30),AL1(YR_2020,MON_DEC,27),AL2(DEC_20)         
         DC AL1(YR_2020,MON_DEC,28),AL1(YR_2021,MON_JAN,31),AL2(JAN_21)         
         DC AL1(YR_2021,MON_FEB,01),AL1(YR_2021,MON_FEB,28),AL2(FEB_21)         
         DC AL1(YR_2021,MON_MAR,01),AL1(YR_2021,MON_MAR,28),AL2(MAR_21)         
         DC AL1(YR_2021,MON_MAR,29),AL1(YR_2021,MON_APR,25),AL2(APR_21)         
         DC AL1(YR_2021,MON_APR,26),AL1(YR_2021,MON_MAY,30),AL2(MAY_21)         
         DC AL1(YR_2021,MON_MAY,31),AL1(YR_2021,MON_JUN,27),AL2(JUN_21)         
         DC AL1(YR_2021,MON_JUN,28),AL1(YR_2021,MON_JUL,25),AL2(JUL_21)         
         DC AL1(YR_2021,MON_JUL,26),AL1(YR_2021,MON_AUG,29),AL2(AUG_21)         
         DC AL1(YR_2021,MON_AUG,30),AL1(YR_2021,MON_SEP,26),AL2(SEP_21)         
*                                                                               
*                                                        EOF- BAD BOOK          
         DC AL1(YR_2021,MON_SEP,27),AL1(YR_2021,MON_DEC,31)                     
         DC AL1(YR_2021,13)        NON-EXISTENT MONTH                           
*                                                                               
         SPACE 2                                                                
         TABLE_LEN TBL_MTHDATES                                                 
         EJECT                                                                  
***********************************************************************         
*                                                                               
* TABLE OF VIEWING TYPES (FOR NET TIME SHIFTED DATA), INCLUDING VCR             
                                                                                
TBL_VWTYPTTB DS 0D                                                              
         DC    CL8'*VTYPS**'                                                    
         DS    XL6                                                              
         DC    AL2(VWTYPTL)                                                     
*                                                                               
         VWTYPN ' ',SRCLIVE,VCR=YES    LIVE WITH VCR                            
         VWTYPN '1',SRCLIVE,VCR=NO     LIVE W/O VCR                             
         VWTYPN '2',SRCLIVSD,VCR=YES   LIVE+SD WITH VCR                         
         VWTYPN '3',SRCLIVSD,VCR=NO    LIVE+SD W/O VCR                          
         VWTYPN '4',SRCLIVE7,VCR=YES   LIVE+7 WITH VCR                          
         VWTYPN '5',SRCLIVE7,VCR=NO    LIVE+7 W/O VCR                           
         VWTYPN '6',SRCLIVE1,VCR=YES   LIVE+1 WITH VCR                          
         VWTYPN '7',SRCLIVE1,VCR=NO    LIVE+1 W/O VCR                           
         VWTYPN '8',SRCLIVE2,VCR=YES   LIVE+2 WITH VCR                          
         VWTYPN '9',SRCLIVE2,VCR=NO    LIVE+2 W/O VCR                           
         VWTYPN 'A',SRCLIVE3,VCR=YES   LIVE+3 WITH VCR                          
         VWTYPN 'B',SRCLIVE3,VCR=NO    LIVE+3 W/O VCR                           
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_VWTYPTTB                                                 
         EJECT                                                                  
***********************************************************************         
*                                                                               
* TABLE OF VIEWING TYPE DESCRIPTIONS                                            
                                                                                
TBL_VWDESCTB DS 0D                                                              
         DC    CL8'*VWDESC*'                                                    
         DS    XL6                                                              
         DC    AL2(VWDESCL)                                                     
*                                                                               
         VWDESC SRCLIVE,'LIVE'                                                  
         VWDESC SRCLIVSD,'LIVE+SD'                                              
         VWDESC SRCLIVE7,'LIVE+7'                                               
         VWDESC SRCLIVE1,'LIVE+1'                                               
         VWDESC SRCLIVE2,'LIVE+2'                                               
         VWDESC SRCLIVE3,'LIVE+3'                                               
         VWDESC SRCALV,'A/LIVE'                                                 
         VWDESC SRCALS,'A/LIVE+SD'                                              
         VWDESC SRCAL7,'A/LIVE+7'                                               
         VWDESC SRCNTIL3,'N/LIVE+3'                                             
         VWDESC SRCLCLQ,'CSLIVE'                                                
         VWDESC SRCLCLCQ,'CSLIVECOM'                                            
         VWDESC SRCLCL3Q,'CSL+3COM'                                             
         VWDESC SRCLCL7Q,'CSL+7COM'                                             
         VWDESC SRCLOLQ,'OOH LIVE'                                              
         VWDESC SRCLOSQ,'OOH L+SD'                                              
         VWDESC SRCLO3Q,'OOH L+3'                                               
         VWDESC SRCLO7Q,'OOH L+7'                                               
         VWDESC SRCLOCLQ,'OOHC LIVE'                                            
         VWDESC SRCLOCSQ,'OOHC L+SD'                                            
         VWDESC SRCLOC3Q,'OOHC L+3'                                             
         VWDESC SRCLOC7Q,'OOHC L+7'                                             
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_VWDESCTB                                                 
         EJECT                                                                  
***********************************************************************         
*                                                                               
* TABLE OF VIEWING TYPE TRANSLATIONS.                                           
* TRANSLATES APPLICATION INPUT VIEWING TYPES (PASSED IN THE 'DBXLIVD'           
* EXTENT) TO THE SOURCE ON THE DEMO RECORD.                                     
* THE FIRST COLUMN MATCHES FIELD 'DBXLIVE' IN EXTENT 'DBXLIVD'.                 
                                                                                
TBL_VTYPTRN DS 0D                                                               
         DC    CL8'*VTYTRN*'                                                    
         DS    XL6                                                              
         DC    AL2(VTYPTRNL)                                                    
*                                                                               
         VWTYPT X'00',SRCLIVE      LIVE                                         
         VWTYPT X'01',SRCLIVSD     LIVE+SD                                      
         VWTYPT X'07',SRCLIVE7     LIVE+7                                       
         VWTYPT X'F1',SRCLIVE1     LIVE+1                                       
         VWTYPT X'02',SRCLIVE2     LIVE+2                                       
         VWTYPT X'03',SRCLIVE3     LIVE+3                                       
         VWTYPT X'FA',SRCALV       LIVE    FROM ACM TAPES                       
         VWTYPT X'FB',SRCALS       LIVE+SD FROM ACM TAPES                       
         VWTYPT X'FC',SRCAL7       LIVE+7  FROM ACM TAPES                       
         VWTYPT X'FD',SRCNTIL3     LIVE+3  FROM NTI TAPES                       
         VWTYPT X'C0',SRCLOLQ      OOH PAV LIVE                                 
         VWTYPT X'C1',SRCLOSQ      OOH PAV LIVE+SD                              
         VWTYPT X'C2',SRCLO3Q      OOH PAV LIVE+3                               
         VWTYPT X'C3',SRCLO7Q      OOH PAV LIVE+7                               
         VWTYPT X'C4',SRCLOCLQ     OOH COMMERCIAL AVG LIVE                      
         VWTYPT X'C5',SRCLOCSQ     OOH COMMERCIAL AVG LIVE+SD                   
         VWTYPT X'C6',SRCLOC3Q     OOH COMMERCIAL AVG LIVE+3                    
         VWTYPT X'C7',SRCLOC7Q     OOH COMMERCIAL AVG LIVE+7                    
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_VTYPTRN                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* TABLE WEEKLY METERED LPM MARKETS AND LAST PRELIMINARY BOOK                    
* P BOOK MARKETS                                                                
* THIS TABLE IS USED FOR 2 PURPOSE                                              
* 1- DEFINE THE LAST PRELIMINARY BOOK- SO WE CAN STRIP THE BOOKTYPE             
*    AFTER MKT IS LIVE LPM                                                      
* 2- DEFINE THE WEEK WHICH THE MKT IS NOT SWEPT- USUALLY WK BEFORE LPM          
*    WEEK. THIS IS USED IN SPOT POSTING. -DARK WEEK                             
*                                                                               
* NOTE WE DONT GET PRELIMINARY AT ALL SET THE PRELIM DATE TO EARLIER YR         
* WEEK 1 OF 2004 -IT REALLY SHOULDN'T MATTER                                    
* BUT YOU STILL HAVE TO ENTER THE DARK WEEK ENTRY ACCURATELY                    
* NIELSEN SENDS OUT EMAIL NOTICE WHICH EXACT DATE THE LPM ROLL IS               
* AND THAT IS THE DARK WEEK                                                     
*                                                                               
TBL_WKLYLPM DS 0D                                                               
         DC    CL8'*WLYLPM*'                                                    
         DS    XL6                                                              
         DC    AL2(WKLYLPMQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL2(101),AL1(YR_2004,WEEK_36)           NY LAST P WEEK           
         DC     CL3'NY ',AL1(YR_2004,WEEK_36)           NY DARK WEEK            
         DC    AL2(202),AL1(YR_2004,WEEK_39)           CHI LAST P WEEK          
         DC     CL3'CHI',AL1(0,0)                       CHI-NO DARK WK          
         DC    AL2(403),AL1(YR_2004,WEEK_31)           LA LAST P WEEK           
         DC     CL3'LA ',AL1(0,0)                       LA -NO DARK WK          
         DC    AL2(407),AL1(YR_2005,WEEK_3)            SF LAST P WEEK           
         DC     CL3'SF ',AL1(0,0)                       SF -NO DARK WK          
         DC    AL2(104),AL1(YR_2004,WEEK_1)            PHL                      
         DC     CL3'PHL',AL1(YR_2005,WEEK_26)           PHL DARK WEEK           
         DC    AL2(111),AL1(YR_2004,WEEK_1)            WAS                      
         DC     CL3'WAS',AL1(YR_2005,WEEK_26)           WAS DARK WEEK           
         DC    AL2(223),AL1(YR_2004,WEEK_1)            DALLAS                   
         DC     CL3'DF ',AL1(YR_2006,WEEK_1)            DF DARK WEEK            
         DC    AL2(105),AL1(YR_2004,WEEK_1)            DET                      
         DC     CL3'DET',AL1(YR_2006,WEEK_1)            DET DARK WEEK           
         DC    AL2(168),AL1(YR_2004,WEEK_1)            ATL                      
         DC     CL3'ATL',AL1(YR_2006,WEEK_26)           ATL DARK WEEK           
*                                                                               
         DC    AL2(218),AL1(YR_2007,WEEK_39)           HOU                      
         DC     CL3'HOU',AL1(YR_2007,WEEK_40)           HOU DARK WEEK           
         DC    AL2(419),AL1(YR_2007,WEEK_39)           SEA                      
         DC     CL3'SEA',AL1(YR_2007,WEEK_40)           SEA DARK WEEK           
         DC    AL2(139),AL1(YR_2007,WEEK_39)           TAM                      
         DC     CL3'TAM',AL1(YR_2007,WEEK_40)           TAM DARK WEEK           
         DC    AL2(353),AL1(YR_2008,WEEK_13)           PHX                      
         DC     CL3'PHX',AL1(YR_2008,WEEK_14)           PHX DARK WEEK           
         DC    AL2(110),AL1(YR_2008,WEEK_34)           CLE                      
         DC     CL3'CLE',AL1(YR_2008,WEEK_35)           CLE DARK WEEK           
         DC    AL2(213),AL1(YR_2008,WEEK_34)           MIN                      
         DC     CL3'MIN',AL1(YR_2008,WEEK_35)           MIN DARK WEEK           
*                                                                               
         DC    AL2(351),AL1(YR_2004,WEEK_1)            DEN   - NO P BK          
         DC     CL3'DEN',AL1(YR_2008,WEEK_40)           DEN DARK WEEK           
         DC    AL2(128),AL1(YR_2004,WEEK_1)            MF    -NO P BK           
         DC     CL3'MF ',AL1(YR_2008,WEEK_40)           MF  DARK WEEK           
         DC    AL2(134),AL1(YR_2004,WEEK_1)            ORL   - NO P BK          
         DC     CL3'ORL',AL1(YR_2009,WEEK_2)            ORL DARK WEEK           
         DC    AL2(209),AL1(YR_2004,WEEK_1)            STL   -NO P BK           
         DC     CL3'STL',AL1(YR_2009,WEEK_2)            STL DARK WEEK           
         DC    AL2(462),AL1(YR_2004,WEEK_1)            SAC   -NO P BK           
         DC     CL3'SAC',AL1(YR_2009,WEEK_2)            SAC DARK WEEK           
*                                                                               
         DC    AL2(112),AL1(YR_2004,WEEK_1)            BAL   -NO P BK           
         DC     CL3'BAL',AL1(YR_2009,WEEK_27)           BAL  DARK WEEK          
         DC    AL2(420),AL1(YR_2004,WEEK_1)            PTO   -NO P BK           
         DC     CL3'PTO',AL1(YR_2009,WEEK_27)           PTO  DARK WEEK          
         DC    AL2(108),AL1(YR_2004,WEEK_1)            PIT   -NO P BK           
         DC     CL3'PIT',AL1(YR_2009,WEEK_27)           PIT  DARK WEEK          
         DC    AL2(117),AL1(YR_2004,WEEK_1)            CHL   -NO P BK           
         DC     CL3'CHL',AL1(YR_2010,WEEK_3)            CHL  DARK WEEK          
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_WKLYLPM                                                  
         EJECT                                                                  
*                                                                               
* TABLE FOR OVERNIGHTS ROLLING AVERAGE PUT BOOKTYPE                             
* (STARTBOOK),(OVERNIGHTS BOOKTYPE),(MONTHLY BOOKTYPE TO USE)                   
*                                                                               
TBL_ROLLBKT DS 0D                                                               
         DC    CL8'*ROLLPB*'                                                    
         DS    XL6                                                              
         DC    AL2(ROLLBKTQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL2(JAN_70),AL1(BOOKTYPE_L,BOOKTYPE_STANDARD)                    
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ROLLBKT                                                  
         EJECT                                                                  
****************************************************************                
*LOCAL CABLE STATION LINKS                                                      
*  LOCAL CALL LETTERS, NSI CABLE CALL LETTERS ,SPILL MKT,BOOKTYPE               
*                                                                               
*  IF STATION IS NOT REPORTED IN FUSION WE MUST ADD THE NUMERIC                 
*  CABLE NUMBER SO WE CAN LOOKUP IT UP FOR NSI                                  
*                                                                               
* SOME OF THESE ENTRIES HAVE THE SAME LOCAL CABLE CALL LETTERS                  
* AND NSI CABLE CALL LETTERS, BUT WE STILL ADD THEM IN THE TABLE                
* TO FORCE A SPILL MKT OR BOOKTYPE IN A LOOKUP.                                 
*                                                                               
* NOTE - IF A STATION CHANGES CALL LETTERS, THE NEW CALL LETTER                 
*        MUST ALSO BE ADDED TO THIS TABLE                                       
* IF A STATION GOES FROM SPILL TO BROADCAST OR BROADCAST TO SPILL               
* ADD MULTIPLE ENTRIES FOR CALL LETTERS.  THE LAST ENTRIES FOR CALL             
* LETTER MUST HAVE "LOC_LAST_LEVEL" DEFINED BY LOCLEVEL DSECT FIELD             
* WHEN MULTIPLE ENTRIES ARE ENTERED FOR A CALL LETTER - ALWAYS HAVE THE         
* LATEST DEFINED 1ST TO ENSURE THE LATEST BOOK LOOKUP WILL GET THE              
* CORRECT LATEST BOOKS                                                          
*                                                                               
* AS PER BEN, THE FOLLOWING COMMENT IS OBSOLETE:                                
* THE X'99' BOOKTYPE ENTRIES ARE DEFINED TO BE NON-VALID                        
* THE LOOKUPS FOR THESE CALL LETTER SWTICHES WILL NOT WORK                      
* UNTIL THE APPROPRIATE SOFTWARE IS LOADED TO LOOK AT THE                       
* APPROPRIATE BOOKTYPE TABLES.  THIS ALLOWS EARLY INSTALL OF THIS               
* TABLE WITHOUT HAVING TO LOAD THE SOFTWARE FIRST.                              
*                                                                               
* WHEN A NEW CALL LETTER LINK IS ADDED ALSO ADD ALL THE OLDER LINKS             
* FOR THE OLD CALL LETTER TO THE NEW CALL LETTER SO THAT USRS                   
* CAN USE THE NEW CALL LETTERS AND GET OLD DATA FOR THE OLD LINKS               
*                                                                               
* WHEN ADDING NEW CALL LETTER CHANGES ALL THE LEVELS OF THE OLD                 
* CALL LETTER LINKS SHOULD BE DUPLICATED FOR THE NEW CALL LETTER                
*                                                                               
* NEW CALL LETTER HAS TO HAVE ALL THE OLD CALL LETTERS LINKS                    
* WE MUST ALSO ADD THE OLD CALL LETTER POINT TO NEW CALL LETTER LINKS           
*                                                                               
*  ALSO ONLY NEED SPILL OR NON SPILL LEVEL DEPENDING ON HOW                     
*  WE LOAD THE DATA FOR RTN AND RON - CHECK IN PFM                              
* WE WE LOAD AS SPILL IN ONE AND NON SPILL IN THE OTHER THEN WE NEED            
* BOTH LEVELS OF LINKS IN THIS TABLE FOR THAT CALL LETTER                       
*                                                                               
* TO TEST USE LDA/DEM APPLICATION - MAKE SPILL AND ON SPILL REQUESTS            
* LOOK FOR OVERNIGHTS AND MONTHLY REQUESTS                                      
*                                                                               
* AS OF MAY2018 WE CHANGED THE CODE IN DEGETTP TO ALSO LOOK                     
* IN THIS TABLE IF SPILL MARKET IS REQUESTED.  WE USED TO ONLY LOOK             
* IN THIS TABLE FOR NON SPILL CALL LETTER REQUESTS                              
* THIS IS TO SOLVE THE LORIG SITUATION WHERE WE LOAD AS SPILL ON DAILY          
* BUT WE LOAD AS SPILL IN MONTHLY OR VS VERSA.                                  
* THE RESULT OF THIS IS THAT IF A SPILL REQUESTS IS MADE AND THERE              
* A LINK TO ANOTHER CALL LETTER FOUND IN THE TABLE                              
* IT WILL NOT FIND THE DATA UNDER CERTAIN BOOK CUTOFFS BECAUSE                  
* TJHE CALL LETTER CHANGED                                                      
* WE ALSO NEED TO ADD LINKS FOR THE CALL LETTER BACK TO ITSELF                  
* EX,  TWNA CHANGES TO SPNA                                                     
* IF WE LOOK UP TWNA/ALB FOR MAY17, WE WOULD TRY TO LOOK UP SPNA/ALB            
* THE DATA IS REALLY STILL LOADED UNDER MAY17.  WE NEED TO HAVE A               
* LINK FOR TWNA BACK IT ITSELF.                                                 
*                                                                               
****************************************************************                
                                                                                
*                                                                               
TBL_LOCCABLE DS 0D                                                              
         DC    CL8'*LOCCAB*'                                                    
         DS    XL6                                                              
         DC    AL2(LOCCABQ)        L'TABLE ENTRY                                
*                                                                               
* EACH TABLE ENTRY HAS THESE POSITIONAL PARAMETERS:                             
*  1. FAKE FSN CALL LETTERS                                                     
*  2. DEMO SYSTEM CALL LETTERS                                                  
*  3. SPILL MARKET NUMBER (OR "NOSPILL")                                        
*  4. SPILL MARKET ALPHA CODE (OR ABSENT)                                       
*  5. LOCAL BOOKTYPE TABLE LABEL                                                
*      SEE EXPLANATION FOR LOCBKTBD FURTHER DOWN.                               
*                                                                               
* THE ENTRIES IN THE TABLE MUST BE IN SORTED SEQUENCE BY THE *FIRST*            
* FIELD (THE FAKE CALL LETTERS). WHEN ADDING A NEW ENTRY, IF THERE IS           
* ALREADY AN ENTRY FOR THE FAKE CALL LETTERS, ADD THE NEW ENTRY                 
* ***BEFORE*** ANY EXISTING ENTRIES. THIS WILL FORCE THE MOST RECENT            
* ENTRIES TO BE LOOKED UP FIRST.                                                
* (NOTE: BECAUSE THERE ARE SOME DUPLICATE KEYS, WE CANNOT USE BINSRCH           
*  TO TRAVERSE THIS TABLE.)                                                     
         LOCCAB ALT,'ALT T',NOSPILL,,BTYPTAB2,351,DEN                           
         LOCCAB ALT,'ALT T',351,DEN,BTYPTAB1,351,DEN                            
         LOCCAB ATNW,'ATNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB ATPT,'ATPTT',NOSPILL,,BTYPTAB2,108,PIT                          
         LOCCAB ATPT,'ATPTT',108,PIT,BTYPTAB1,108,PIT                           
         LOCCAB ATPT,'RTPTT',108,PIT,BTYPTAB1,108,PIT                           
         LOCCAB ATPT,'RTPTT',NOSPILL,,BTYPTAB2,108,PIT                          
         LOCCAB ATRM,'ATRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB ATRM,'RTRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB ATRM,'FSRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB ATSW,'ATSWT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB ATSW,'RTSWT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB ATSW,'CSNHT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB BDSU,'WDSUT',346,BLX,BTYPTAB2,346,BLX                           
         LOCCAB BSNM,'NSWAT',112,BAL,BTYPTAB1,112,BAL                           
         LOCCAB BSNM,'CSNMT',112,BAL,BTYPTAB1,112,BAL                           
         LOCCAB BTN,'BTN T',202,CHI,BTYPTAB1,202,CHI                            
         LOCCAB CCSF,'CCSFT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB CCSF,'CSCAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB CMCH,'CSNCT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB CMCH,'CSNCT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB COM,'ZCSNT',104,PHL,BTYPTAB1,104,PHL                            
         LOCCAB COM,'ZCSNT',NOSPILL,,BTYPTAB2,104,PHL                           
         LOCCAB CSBA,'NSBAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB CSBA,'CSBAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB CSCA,'CSCAT',NOSPILL,,BTYPTAB2,462,SAC                          
         LOCCAB CSCX,'CSCXT',462,SAC,BTYPTAB1,462,SAC                           
         LOCCAB CSCX,'NSCXT',462,SAC,BTYPTAB1,462,SAC                           
         LOCCAB CSHA,'CSNET',133,HAT,BTYPTAB1,133,HAT                           
         LOCCAB CSHA,'CSNET',133,HAT,BTYPTAB2,133,HAT                           
         LOCCAB CSN,'CSNMT',111,WAS,BTYPTAB1,111,WAS                            
         LOCCAB CSNB,'CSNET',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB CSNB,'CSNET',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB CSNC,'CSNCT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB CSNC,'CSNCT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB CSNC,'NSCHT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB CSNC,'NSCHT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB CSNE,'CSNET',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB CSNE,'CSNET',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB CSNE,'NSBOT',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB CSNE,'NSBOT',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB CSNH,'RTSWT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB CSNH,'CSNHT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB CSNH,'CSNET',133,HAT,BTYPTAB1,133,HAT                           
         LOCCAB CSNM,'CSNMT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB CSNN,'CSNNT',NOSPILL,,BTYPTAB2,420,PTO                          
         LOCCAB CSNN,'CSNNT',420,PTO,BTYPTAB1,420,PTO                           
         LOCCAB CSNN,'NSNNT',NOSPILL,,BTYPTAB2,420,PTO                          
         LOCCAB CSNN,'NSNNT',420,PTO,BTYPTAB1,420,PTO                           
         LOCCAB CSNP,'CSNPT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB CSSF,'CSCAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB FBOB,'FSSOT',117,CHL,BTYPTAB1,117,CHL                           
         LOCCAB FSA,'FSA T',353,PHX,BTYPTAB1,353,PHX                            
         LOCCAB FSA,'FSA T',NOSPILL,,BTYPTAB2,353,PHX                           
         LOCCAB FSCI,'FSOHT',115,CIN,BTYPTAB1,115,CIN                           
         LOCCAB FSCO,'FSOHT',135,CLO,BTYPTAB1,135,CLO                           
         LOCCAB FSDA,'FSOHT',142,DAY,BTYPTAB1,142,DAY                           
         LOCCAB FSDT,'FSD T',105,DET,BTYPTAB1,105,DET                           
         LOCCAB FSFL,'FSFLT',128,MF,BTYPTAB1,128,MF                             
         LOCCAB FSFO,'FSFLT',134,ORL,BTYPTAB1,134,ORL                           
         LOCCAB FSFT,'FSFLT',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB FSHO,'FSH T',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB FSIN,'FSMWT',127,IND,BTYPTAB1,127,IND                           
         LOCCAB FSKC,'FSMWT',216,KC,BTYPTAB1,216,KC                             
         LOCCAB FSMW,'FSMWT',209,STL,BTYPTAB1,209,STL                           
         LOCCAB FSNO,'FSNOT',213,MIN,BTYPTAB1,213,MIN                           
         LOCCAB FSNW,'RTNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB FSNW,'FSNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB FSOH,'FSOHT',110,CLE,BTYPTAB1,110,CLE                           
         LOCCAB FSOK,'FSS T',250,OKC,BTYPTAB1,250,OKC                           
         LOCCAB FSP,'RTPTT',108,PIT,BTYPTAB1,108,PIT                            
         LOCCAB FSP,'FSP T',108,PIT,BTYPTAB1,108,PIT                            
         LOCCAB FSPT,'FSPTT',403,LA,BTYPTAB1,403,LA                             
         LOCCAB FSRM,'FSRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB FSRM,'RTRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB FSSD,'FSSDT',425,SD,BTYPTAB1,425,SD                             
         LOCCAB FSSD,'FSW T',425,SD,BTYPTAB1,425,SD                             
         LOCCAB FSSE,'SPSOT',168,ATL,BTYPTAB1,168,ATL                           
         LOCCAB FSSE,'SPSOT',124,ATL,BTYPTAB1,124,ATL                           
         LOCCAB FSSE,'FSSET',168,ATL,BTYPTAB1,168,ATL                           
         LOCCAB FSSE,'FSSET',124,ATL,BTYPTAB1,124,ATL                           
         LOCCAB FSSO,'FSSOT',168,ATL,BTYPTAB1,168,ATL                           
         LOCCAB FSSO,'FSSOT',124,ATL,BTYPTAB1,124,ATL                           
         LOCCAB FSSP,'FSSPT',223,DF,BTYPTAB1,223,DF                             
         LOCCAB FSSW,'FSS T',223,DF,BTYPTAB1,223,DF                             
         LOCCAB FSTN,'FSSOT',259,NAS,BTYPTAB1,259,NAS                           
         LOCCAB FSW,'FSW T',403,LA,BTYPTAB1,403,LA                              
         LOCCAB FSWI,'FSWIT',NOSPILL,,BTYPTAB2,217,MIL                          
         LOCCAB FSWI,'FSWIT',217,MIL,BTYPTAB1,217,MIL                           
         LOCCAB LNCH,'WTBDT',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB LNCH,'LNCHT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB LNCH,'WNWST',NOSPILL,,BTYPTAB1,111,WAS                          
         LOCCAB MARQ,'MARQT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB MARQ,'MARQT',NOSPILL,,BTYPTAB1,202,CHI                          
         LOCCAB MASA,'MAS2T',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB MASN,'MASNT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB MASP,'MASNT',112,BAL,BTYPTAB1,112,BAL                           
         LOCCAB MAST,'MAS2T',112,BAL,BTYPTAB1,112,BAL                           
         LOCCAB MSG,'MSG T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB MSGP,'MSGPT',101,NY,BTYPTAB1,101,NY                             
         LOCCAB MTN,'MTN T',370,SLC,BTYPTAB1,370,SLC                            
         LOCCAB NEHA,'NESNT',133,HAT,BTYPTAB1,133,HAT                           
         LOCCAB NEPO,'NESNT',100,PTM,BTYPTAB1,100,PTM                           
         LOCCAB NEPR,'NESNT',121,PRV,BTYPTAB1,121,PRV                           
         LOCCAB NESN,'NESNT',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB NESN,'NESNT',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB NOT1,'S1NTT',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB NOT1,'NOT1T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB NSBA,'NSBAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB NSBO,'NSBOT',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB NSBO,'NSBOT',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB NSBO,'CSNET',106,BOS,BTYPTAB1,106,BOS                           
         LOCCAB NSBO,'CSNET',NOSPILL,,BTYPTAB2,106,BOS                          
         LOCCAB NSCA,'NSCAT',462,SAC,BTYPTAB1,462,SAC                           
         LOCCAB NSCA,'CSCAT',NOSPILL,,BTYPTAB2,462,SAC                          
         LOCCAB NSCH,'NSCHT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB NSCH,'NSCHT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB NSCH,'CSNCT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB NSCH,'CSNCT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB NSCP,'NSCPT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB NSCP,'NSCPT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB NSCP,'CSNPT',202,CHI,BTYPTAB1,202,CHI                           
         LOCCAB NSCP,'CSNPT',NOSPILL,,BTYPTAB2,202,CHI                          
         LOCCAB NSCS,'NSCAT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB NSCX,'CSCXT',462,SAC,BTYPTAB1,462,SAC                           
         LOCCAB NSCX,'NSCXT',462,SAC,BTYPTAB1,462,SAC                           
         LOCCAB NSNN,'CSNNT',NOSPILL,,BTYPTAB2,420,PTO                          
         LOCCAB NSNN,'CSNNT',420,PTO,BTYPTAB1,420,PTO                           
         LOCCAB NSNN,'NSNNT',NOSPILL,,BTYPTAB2,420,PTO                          
         LOCCAB NSNN,'NSNNT',420,PTO,BTYPTAB1,420,PTO                           
*                                                                               
         LOCCAB NSPH,'NSPHT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB NSPH,'NSPHT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB NSPH,'ZCSNT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB NSPH,'ZCSNT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB NSPP,'NSPPT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB NSPP,'NSPPT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB NSPP,'TCNPT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB NSWA,'NSWAT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB NSWA,'CSNMT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB NSWP,'NSWPT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB NSWP,'NSWPT',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB NSWP,'TCNWT',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB NSWX,'NSWXT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB NWT,'N12 T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB NWT,'NWT T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB NWT,'NWT T',NOSPILL,,BTYPTAB2,101,NY                            
         LOCCAB NWT+,'N12 T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB NWT+,'NWT T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB NWT+,'NWT T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB NYON,'S1NYT',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB NYON,'ZNY1T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB N12,'NWT T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB N12,'NWT T',NOSPILL,,BTYPTAB2,101,NY                            
         LOCCAB N12,'N12 T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB N12,'N12 T',NOSPILL,,BTYPTAB2,101,NY                            
         LOCCAB N12+,'NWT T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB N12+,'NWT T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB N12+,'N12 T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB N12+,'N12 T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB PACA,'P12AT',353,PHX,BTYPTAB1,353,PHX                           
         LOCCAB PACB,'P12BT',407,SF,BTYPTAB1,407,SF                             
         LOCCAB PACL,'P12LT',403,LA,BTYPTAB1,403,LA                             
         LOCCAB PACM,'P12MT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB PACO,'P12OT',420,PTO,BTYPTAB1,420,PTO                           
         LOCCAB PACW,'P12WT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB RTNW,'ATNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB RTNW,'RTNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB RTNW,'FSNWT',419,SEA,BTYPTAB1,419,SEA                           
         LOCCAB RTPO,'RTNWT',420,PTO,BTYPTAB1,420,PTO                           
*****    LOCCAB RTPT,'ATPTT',108,PIT,BTYPTAB1,108,PIT                           
         LOCCAB RTPT,'RTPTT',108,PIT,BTYPTAB1,108,PIT                           
         LOCCAB RTPT,'RTPTT',NOSPILL,,BTYPTAB2,108,PIT                          
         LOCCAB RTPT,'FSP T',108,PIT,BTYPTAB1,108,PIT                           
         LOCCAB RTRM,'ATRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB RTRM,'RTRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB RTRM,'FSRMT',351,DEN,BTYPTAB1,351,DEN                           
         LOCCAB RTSW,'ATSWT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB RTSW,'RTSWT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB RTSW,'CSNHT',218,HOU,BTYPTAB1,218,HOU                           
         LOCCAB SBOB,'SPSOT',117,CHL,BTYPTAB1,117,CHL                           
         LOCCAB SNY,'SNY T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB SPNA,'TWNAT',132,ALB,BTYPTAB1,132,ALB                           
         LOCCAB SPNA,'TWNAT',NOSPILL,,BTYPTAB2,132,ALB                          
         LOCCAB SPNA,'SPNAT',132,ALB,BTYPTAB1,132,ALB                           
         LOCCAB SPNA,'SPNAT',NOSPILL,,BTYPTAB2,132,ALB                          
         LOCCAB SPNB,'SPNBT',NOSPILL,,BTYPTAB2,113,BUF                          
         LOCCAB SPNB,'S1BFT',NOSPILL,,BTYPTAB2,113,BUF                          
         LOCCAB SPNR,'TWNRT',138,ROH,BTYPTAB1,138,ROH                           
         LOCCAB SPNR,'TWNRT',NOSPILL,,BTYPTAB2,138,ROH                          
         LOCCAB SPNS,'TWNST',155,SYR,BTYPTAB1,155,SYR                           
         LOCCAB SPNS,'TWNST',NOSPILL,,BTYPTAB2,155,SYR                          
         LOCCAB SPNS,'SPNST',NOSPILL,,BTYPTAB2,155,SYR                          
         LOCCAB SPNX,'SPNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB SPNX,'SPNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB SPNX,'TWNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB SPNX,'TWNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB SPNX,'S1AUT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB SPNX,'S1AUT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB SPSA,'SPSAT',NOSPILL,,BTYPTAB2,241,SAN                          
         LOCCAB SPSA,'S1SAT',NOSPILL,,BTYPTAB2,241,SAN                          
         LOCCAB SPSO,'SPSOT',168,ATL,BTYPTAB1,168,ATL                           
         LOCCAB SPSO,'SPSOT',124,ATL,BTYPTAB1,124,ATL                           
         LOCCAB SPTN,'SPSOT',259,NAS,BTYPTAB1,259,NAS                           
         LOCCAB SPUR,'FSS T',241,SAN,BTYPTAB1,241,SAN                           
         LOCCAB STO,'STO T',110,CLE,BTYPTAB1,110,CLE                            
         LOCCAB SUN,'SUN T',134,ORL,BTYPTAB1,134,ORL                            
         LOCCAB SUNM,'SUN T',128,MF,BTYPTAB1,128,MF                             
         LOCCAB SUNT,'SUN T',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB S1AU,'SPNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB S1AU,'SPNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB S1AU,'TWNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB S1AU,'TWNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB S1AU,'S1AUT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB S1AU,'S1AUT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB S1BF,'SPNBT',NOSPILL,,BTYPTAB2,113,BUF                          
         LOCCAB S1BF,'S1BFT',NOSPILL,,BTYPTAB2,113,BUF                          
         LOCCAB S1CT,'S1CTT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB S1CT,'ZCTWT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB S1CT,'ZSPCT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB S1GO,'S1GOT',NOSPILL,,BTYPTAB2,118,GWH                          
         LOCCAB S1GO,'ZSPGT',NOSPILL,,BTYPTAB2,118,GWH                          
         LOCCAB S1NT,'S1NTT',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB S1NT,'NOT1T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB S1NY,'ZNY1T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB S1NY,'S1NYT',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB S1OR,'ZCFNT',134,ORL,BTYPTAB1,134,ORL                           
         LOCCAB S1OR,'ZCFNT',NOSPILL,,BTYPTAB2,134,ORL                          
         LOCCAB S1OR,'S1ORT',134,ORL,BTYPTAB1,134,ORL                           
         LOCCAB S1OR,'S1ORT',NOSPILL,,BTYPTAB2,134,ORL                          
         LOCCAB S1RL,'S1RLT',NOSPILL,,BTYPTAB2,160,RAL                          
         LOCCAB S1RL,'ZSPRT',NOSPILL,,BTYPTAB2,160,RAL                          
         LOCCAB S1SA,'SPSAT',NOSPILL,,BTYPTAB2,241,SAN                          
         LOCCAB S1SA,'S1SAT',NOSPILL,,BTYPTAB2,241,SAN                          
         LOCCAB S1TP,'S1TPT',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB S1TP,'S1TPT',NOSPILL,,BTYPTAB2,139,TAM                          
         LOCCAB S1TP,'ZBN9T',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB S1TP,'ZBN9T',NOSPILL,,BTYPTAB2,139,TAM                          
         LOCCAB TBLA,'CSNNT',NOSPILL,,BTYPTAB2,420,PTO                          
         LOCCAB TCNP,'NSPPT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB TCNP,'TCNPT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB TDUC,'FSW T',403,LA,BTYPTAB1,403,LA                             
         LOCCAB TGRZ,'FSSOT',240,MEM,BTYPTAB1,240,MEM                           
         LOCCAB THOR,'FSS T',222,NOL,BTYPTAB1,222,NOL                           
         LOCCAB THUR,'FSSOT',160,RAL,BTYPTAB1,160,RAL                           
         LOCCAB TJAZ,'RTRMT',370,SLC,BTYPTAB1,370,SLC                           
         LOCCAB TJAZ,'FSRMT',370,SLC,BTYPTAB1,370,SLC                           
         LOCCAB TOKT,'FSS T',250,OKC,BTYPTAB1,250,OKC                           
         LOCCAB TPAD,'SD4 T',NOSPILL,,BTYPTAB2,425,SD                           
         LOCCAB TPAN,'FSFLT',128,MF,BTYPTAB1,128,MF                             
         LOCCAB TPHI,'ZCSNT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB TPHI,'ZCSNT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB TPHL,'WPHLT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB TPSN,'FSA T',353,PHX,BTYPTAB1,353,PHX                           
         LOCCAB TSAB,'MSG T',114,BUF,BTYPTAB1,114,BUF                           
         LOCCAB TSAB,'SNY T',114,BUF,BTYPTAB1,114,BUF                           
         LOCCAB TWLD,'FSNOT',213,MIN,BTYPTAB1,213,MIN                           
         LOCCAB TWNA,'SPNAT',132,ALB,BTYPTAB1,132,ALB                           
         LOCCAB TWNA,'SPNAT',NOSPILL,,BTYPTAB2,132,ALB                          
         LOCCAB TWNA,'TWNAT',132,ALB,BTYPTAB1,132,ALB                           
         LOCCAB TWNA,'TWNAT',NOSPILL,,BTYPTAB2,132,ALB                          
         LOCCAB TWNR,'SPNRT',138,ROH,BTYPTAB1,138,ROH                           
         LOCCAB TWNR,'SPNRT',NOSPILL,,BTYPTAB2,138,ROH                          
         LOCCAB TWNX,'SPNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB TWNX,'SPNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB TWNX,'TWNXT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB TWNX,'TWNXT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB TWNX,'S1AUT',NOSPILL,,BTYPTAB2,235,AUS                          
         LOCCAB TWNX,'S1AUT',235,AUS,BTYPTAB1,235,AUS                           
         LOCCAB UNIV,'UNV T',403,LA,BTYPTAB1,403,LA                             
         LOCCAB UNIV,'UNIVT',403,LA,BTYPTAB1,403,LA                             
         LOCCAB UNV,'UNV T',403,LA,BTYPTAB1,403,LA                              
         LOCCAB UNV,'UNIVT',403,LA,BTYPTAB1,403,LA                              
         LOCCAB WNWS,'WTBDT',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB WNWS,'WNWST',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB WNWS,'LNCHT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB WTBD,'WTBDT',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB WTBD,'WNWST',NOSPILL,,BTYPTAB2,111,WAS                          
         LOCCAB WTBD,'LNCHT',111,WAS,BTYPTAB1,111,WAS                           
         LOCCAB YES,'YES T',101,NY,BTYPTAB1,101,NY                              
         LOCCAB YESD,'YES T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB YESN,'YES T',101,NY,BTYPTAB1,101,NY                             
         LOCCAB YESN,'YES T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB ZBN9,'ZBN9T',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB ZBN9,'ZBN9T',NOSPILL,,BTYPTAB2,139,TAM                          
         LOCCAB ZBN9,'S1TPT',139,TAM,BTYPTAB1,139,TAM                           
         LOCCAB ZBN9,'S1TPT',NOSPILL,,BTYPTAB2,139,TAM                          
         LOCCAB ZCFN,'ZCFNT',134,ORL,BTYPTAB1,134,ORL                           
         LOCCAB ZCFN,'ZCFNT',NOSPILL,,BTYPTAB2,134,ORL                          
         LOCCAB ZCFN,'S1ORT',134,ORL,BTYPTAB1,134,ORL                           
         LOCCAB ZCFN,'S1ORT',NOSPILL,,BTYPTAB2,134,ORL                          
*        LOCCAB ZCSN,'ZCSNT',104,PHL,BTYPTAB1,104,PHL                           
*        LOCCAB ZCSN,'ZCSNT',NOSPILL,,BTYPTAB2,104,PHL                          
**                                                                              
         LOCCAB ZCSN,'NSPHT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB ZCSN,'NSPHT',NOSPILL,,BTYPTAB2,104,PHL                          
         LOCCAB ZCSN,'ZCSNT',104,PHL,BTYPTAB1,104,PHL                           
         LOCCAB ZCSN,'ZCSNT',NOSPILL,,BTYPTAB2,104,PHL                          
**                                                                              
         LOCCAB ZCTW,'ZCTWT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB ZCTW,'S1CTT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB ZNY1,'ZNY1T',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB ZNY1,'S1NYT',NOSPILL,,BTYPTAB2,101,NY                           
         LOCCAB ZSPC,'ZCTWT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB ZSPC,'ZSPCT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB ZSPC,'S1CTT',NOSPILL,,BTYPTAB2,117,CHL                          
         LOCCAB ZSPG,'S1GOT',NOSPILL,,BTYPTAB2,118,GWH                          
         LOCCAB ZSPG,'ZSPGT',NOSPILL,,BTYPTAB2,118,GWH                          
         LOCCAB ZSPR,'S1RLT',NOSPILL,,BTYPTAB2,160,RAL                          
         LOCCAB ZSPR,'ZSPRT',NOSPILL,,BTYPTAB2,160,RAL                          
         LOCCAB 9543,'YES T',101,NY,BTYPTAB1,101,NY                             
**** THE END_OF_TABLE ENTRY MUST BE THE LAST ONE !!!                            
         LOCCAB END_OF_TABLE       *** MUST BE LAST ENTRY ***                   
                                                                                
         SPACE 3                                                                
* SEE DSECT LOCBKTBD IN DEDEMTABD. THIS CONVERTS A USER-INPUT BOOKTYPE          
* INTO THE DEMOS SYSTEM LOOKUP BOOKTYPE. NORMALLY, BTYPTAB1 IS USED FOR         
* SPILL MARKETS, AND BTYPTAB2 IS FOR NON-SPILL MARKETS. ON RARE                 
* OCCASION (ACCORDING TO MARIA), A DIFFERENT RULE MAY APPLY.                    
*                                                                               
BTYPTAB1 DS    0D                  NORMALLY FOR SPILL MARKETS                   
         DC    AL1(BOOKTYPE_P),AL1(BOOKTYPE_C)                                  
         DC    AL1(BOOKTYPE_C),AL1(BOOKTYPE_C)                                  
         DC    AL1(BOOKTYPE_STANDARD),AL1(BOOKTYPE_C)                           
         DC    AL1(BOOKTYPE_C3),AL1(BOOKTYPE_C3)                                
         DC    AL1(BOOKTYPE_C1),AL1(BOOKTYPE_C1)                                
         DC    AL1(BOOKTYPE_L3),AL1(BOOKTYPE_C3)                                
         DC    AL1(BOOKTYPE_U),AL1(BOOKTYPE_U)                                  
         DC    AL1(BOOKTYPE_L),AL1(BOOKTYPE_U)                                  
         DC    AL1(BOOKTYPE_L1),AL1(BOOKTYPE_C1)                                
         DC    X'FF'                                                            
BTYPTAB2 DS    0D                  NORMALLY USED WHEN THERE IS NO SPILL         
         DC    AL1(BOOKTYPE_C),AL1(BOOKTYPE_STANDARD)                           
         DC    AL1(BOOKTYPE_STANDARD),AL1(BOOKTYPE_C)                           
         DC    AL1(BOOKTYPE_C3),AL1(BOOKTYPE_L3)                                
         DC    AL1(BOOKTYPE_C1),AL1(BOOKTYPE_L1)                                
         DC    AL1(BOOKTYPE_L3),AL1(BOOKTYPE_C3)                                
         DC    AL1(BOOKTYPE_U),AL1(BOOKTYPE_L)                                  
         DC    AL1(BOOKTYPE_L),AL1(BOOKTYPE_L)                                  
         DC    AL1(BOOKTYPE_L1),AL1(BOOKTYPE_C1)                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_LOCCABLE                                                 
         EJECT                                                                  
********************************************************                        
* MAINTAIN A LIST OF SET METERED MKTS                  *                        
* REMEMBER TO TAKE MKT OFF THIS LIST WHEN IT GOES LPM  *                        
*                                                      *                        
* NOTE: IN SEP/12, DEIS DISCOVERED THAT THERE ARE MANY *                        
* LPM MARKETS WHICH WERE NEVER REMOVED FROM THIS LIST. *                        
* BEN AND MARIA CONFIRM THAT THEY SHOULD ACTUALLY      *                        
* REMAIN THERE.                                        *                        
*                                                      *                        
* THIS TABLE SHOULD BE REALLY BE THOUGHT OF AS A       *                        
* "NON-DIARY" MARKET LIST, RATHER THAN A "SET-METER"   *                        
* LIST.                                                *                        
*                                                      *                        
* THE ASSUMPTION IS THAT THIS TABLE IS BASICALLY USED  *                        
* ONLY IN CASES WHERE THE CODE BELIEVES THAT IT'S      *                        
* WORKING WITH A NON-LPM MARKET. IN THAT CASE, THE     *                        
* PRESENCE OF A MARKET MEANS IT'S SETMETER, OTHERWISE  *                        
* IT'S DIARY. IF THE LPM START DATE IS MISSING FROM    *                        
* THE SPOT/SFM MARKET RECORD, THEN WE WANT THE MARKET  *                        
* TREATED AS SETMETER, SO IT'S BEST TO KEEP THE LPM    *                        
* MARKETS IN THIS LIST.                                *                        
*                                                      *                        
********************************************************                        
TBL_SMETERT DS 0D                                                               
         DC    CL8'*SETMET*'                                                    
         DS    XL6                                                              
         DC    AL2(SETMETRQ)       L'TABLE ENTRY                                
*                                                                               
         DC    CL3'ABQ',AL2(390)                                                
         DC    CL3'AUS',AL2(235)                                                
         DC    CL3'BAL',AL2(112)   LPM MARKET                                   
         DC    CL3'BIR',AL2(230)                                                
         DC    CL3'BUF',AL2(114)                                                
         DC    CL3'CHL',AL2(117)   LPM MARKET                                   
         DC    CL3'CIN',AL2(115)                                                
         DC    CL3'CLE',AL2(110)   LPM MARKET                                   
         DC    CL3'CLO',AL2(135)                                                
         DC    CL3'DAY',AL2(142)                                                
         DC    CL3'DEN',AL2(351)   LPM MARKET                                   
         DC    CL3'FM ',AL2(171)                                                
         DC    CL3'GWH',AL2(118)                                                
         DC    CL3'GVS',AL2(167)                                                
         DC    CL3'HAT',AL2(133)                                                
*********DC    CL3'HOU',AL2(218)                                                
         DC    CL3'IND',AL2(127)                                                
         DC    CL3'JKV',AL2(161)                                                
         DC    CL3'KC ',AL2(216)                                                
         DC    CL3'KNX',AL2(157)                                                
         DC    CL3'LAS',AL2(439)                                                
         DC    CL3'LOU',AL2(129)                                                
         DC    CL3'MEM',AL2(240)                                                
         DC    CL3'MF ',AL2(128)   LPM MARKET                                   
         DC    CL3'MIL',AL2(217)                                                
         DC    CL3'MIN',AL2(213)   LPM MARKET                                   
         DC    CL3'NAS',AL2(259)                                                
         DC    CL3'NOL',AL2(222)                                                
         DC    CL3'NOR',AL2(144)                                                
         DC    CL3'OKC',AL2(250)                                                
         DC    CL3'ORL',AL2(134)   LPM MARKET                                   
*********DC    CL3'PHX',AL2(353)                                                
         DC    CL3'PIT',AL2(108)   LPM MARKET                                   
         DC    CL3'PTO',AL2(420)   LPM MARKET                                   
         DC    CL3'PRV',AL2(121)                                                
         DC    CL3'RAL',AL2(160)                                                
         DC    CL3'RCH',AL2(156)                                                
         DC    CL3'SAC',AL2(462)   LPM MARKET                                   
         DC    CL3'SLC',AL2(370)                                                
         DC    CL3'SAN',AL2(241)                                                
         DC    CL3'SD ',AL2(425)                                                
*********DC    CL3'SEA',AL2(419)                                                
         DC    CL3'STL',AL2(209)   LPM MARKET                                   
*********DC    CL3'TAM',AL2(139)                                                
         DC    CL3'TUL',AL2(271)                                                
         DC    CL3'WPB',AL2(148)                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_SMETERT                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* TABLE OF NSI MARKETS                                                          
**********************************************************************          
TBL_NSIMKTS DS     0D                                                           
         DC    CL8'*NSIMKT*'                                                    
         DS    XL6                                                              
         DC    AL2(NMKTRQ)         L'TABLE ENTRY                                
*                                                                               
         DC    C'ABI',AL2(262),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ALB',AL2(132),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'ABG',AL2(125),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ABQ',AL2(390),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'ALX',AL2(244),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ALP',AL2(183),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'AMA',AL2(234),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ANC',AL2(343),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ATL',AL2(168),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'AUG',AL2(120),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'AUS',AL2(235),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'BAK',AL2(400),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BAL',AL2(112),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'BME',AL2(137),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BAT',AL2(316),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BEA',AL2(292),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BEN',AL2(421),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BLL',AL2(356),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BLX',AL2(346),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BNG',AL2(102),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BIR',AL2(230),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'BEC',AL2(159),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BSC',AL2(357),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BOS',AL2(106),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'BOW',AL2(336),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BUF',AL2(114),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'BUR',AL2(123),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'BUT',AL2(354),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CAS',AL2(367),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CED',AL2(237),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CHM',AL2(248),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CWV',AL2(164),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CRL',AL2(119),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'CHL',AL2(117),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'CVA',AL2(184),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CKA',AL2(175),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CHY',AL2(359),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CHI',AL2(202),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'CHC',AL2(468),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CIN',AL2(115),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'CLA',AL2(198),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CLE',AL2(110),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'CSP',AL2(352),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CMO',AL2(204),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CSC',AL2(146),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'COT',AL2(273),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CMB',AL2(122),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'CLO',AL2(135),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'CC ',AL2(200),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'DF ',AL2(223),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'DAV',AL2(282),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'DAY',AL2(142),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'DEN',AL2(351),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'DMO',AL2(279),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'DET',AL2(105),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'DOT',AL2(206),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'DUL',AL2(276),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ELP',AL2(365),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ELM',AL2(165),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ERE',AL2(116),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'EUG',AL2(401),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'EUR',AL2(402),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'EVN',AL2(249),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'FAI',AL2(345),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'FAR',AL2(324),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'FLT',AL2(113),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'FRS',AL2(466),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'FM ',AL2(171),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'FSM',AL2(270),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'FTW',AL2(109),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GAN',AL2(192),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GLE',AL2(398),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GJC',AL2(373),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GRP',AL2(163),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'GFL',AL2(355),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GRB',AL2(258),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GWH',AL2(118),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'GNC',AL2(145),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'GVS',AL2(167),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'GRW',AL2(247),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MB ',AL2(236),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'HRS',AL2(166),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'ABD',AL2(169),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'HAT',AL2(133),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'LAU',AL2(310),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'HEL',AL2(366),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'HON',AL2(344),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'HOU',AL2(218),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'HNT',AL2(291),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'IDF',AL2(358),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'IND',AL2(127),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'JMS',AL2(318),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'JAC',AL2(239),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'JKV',AL2(161),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'JST',AL2(174),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'JON',AL2(334),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'JOP',AL2(203),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'JUN',AL2(347),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'KC ',AL2(216),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'KNX',AL2(157),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'LWI',AL2(302),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LFI',AL2(182),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LAF',AL2(242),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LCL',AL2(243),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LNS',AL2(151),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LAR',AL2(349),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LAS',AL2(439),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'LEX',AL2(141),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LMA',AL2(158),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LIN',AL2(322),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LRK',AL2(293),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'LA ',AL2(403),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'LOU',AL2(129),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'LUB',AL2(251),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MAC',AL2(103),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MAD',AL2(269),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'MAN',AL2(337),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MAR',AL2(153),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MED',AL2(413),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MEM',AL2(240),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'MER',AL2(311),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MF ',AL2(128),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'MIL',AL2(217),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'MIN',AL2(213),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'MNT',AL2(287),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MIS',AL2(362),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MOB',AL2(286),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'MLA',AL2(228),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SAL',AL2(428),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MON',AL2(298),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'FLO',AL2(170),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'NAS',AL2(259),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'NOL',AL2(222),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'NY ',AL2(101),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'NOR',AL2(144),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'NPL',AL2(340),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ODM',AL2(233),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'OKC',AL2(250),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'OMH',AL2(252),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ORL',AL2(134),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'OTT',AL2(231),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PAD',AL2(232),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'PS ',AL2(404),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PAN',AL2(256),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PKS',AL2(197),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PEO',AL2(275),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PHL',AL2(104),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'PHX',AL2(353),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'PIT',AL2(108),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'PTM',AL2(100),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PTO',AL2(420),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'PRE',AL2(152),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'PRV',AL2(121),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'QUI',AL2(317),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'RAL',AL2(160),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'RAP',AL2(364),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'REN',AL2(411),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'RCH',AL2(156),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'RNK',AL2(173),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ROH',AL2(138),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'MAS',AL2(211),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ROK',AL2(210),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SAC',AL2(462),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'SOC',AL2(176),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SLC',AL2(370),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'SAT',AL2(261),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SAN',AL2(241),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'SD ',AL2(425),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'SF ',AL2(407),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'SB ',AL2(455),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'SAV',AL2(107),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SEA',AL2(419),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'ARD',AL2(257),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SVP',AL2(212),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SXC',AL2(224),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SXF',AL2(325),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SBD',AL2(188),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SPK',AL2(481),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SCH',AL2(143),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SMO',AL2(219),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'STJ',AL2(238),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'STL',AL2(209),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'SYR',AL2(155),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TAL',AL2(130),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TAM',AL2(139),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'THI',AL2(181),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TOL',AL2(147),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TOK',AL2(205),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TRA',AL2(140),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'RKP',AL2(131),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TUC',AL2(389),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'TUL',AL2(271),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'TWI',AL2(360),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'TYL',AL2(309),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'UTR',AL2(126),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'VIC',AL2(226),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WCO',AL2(225),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WAS',AL2(111),AL1(NMKTLQ),CL15'LPM'                            
         DC    C'WTN',AL2(149),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WAU',AL2(305),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WPB',AL2(148),AL1(NMKTSQ),CL15'SET METER'                      
         DC    C'WHL',AL2(154),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WFL',AL2(227),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WIT',AL2(278),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'SCR',AL2(177),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'WLM',AL2(150),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'YAK',AL2(410),AL1(NMKTCQ),CL15'CODE READER'                    
         DC    C'YGS',AL2(136),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'YUM',AL2(371),AL1(NMKTDQ),CL15'DIARY'                          
         DC    C'ZAN',AL2(196),AL1(NMKTDQ),CL15'DIARY'                          
*                                                                               
         DC    X'FF'                                                            
         TABLE_LEN TBL_NSIMKTS                                                  
*                                                                               
* TABLE OF STARTING DATES FOR NIELSEN SWEEPS                                    
*                                                                               
* NOTE: KEEP THIS TABLE SORTED CHRONOLOGICALLY WITHIN A GIVEN                   
*       FILE/MEDIA/SOURCE !!! (NENAV01 EXPECTS THE TTN TABLE TO BE              
*       SORTED IN THAT MANNER.)                                                 
*                                                                               
TBL_SWEEPTBL DS    0D                                                           
         DC    CL8'*SWPTAB*'                                                    
         DS    XL6                                                              
         DC    AL2(SWPTBLQ)        L'TABLE ENTRY                                
*                                                                               
*        DC    C'TTN',AL1(83,10),AL1(4),C'830929'                               
*        DC    C'TTN',AL1(83,11),AL1(4),C'831103'                               
** 1984 NSI                                                                     
*        DC    C'TTN',AL1(84,01),AL1(4),C'840105'                               
*        DC    C'TTN',AL1(84,02),AL1(4),C'840202'                               
*        DC    C'TTN',AL1(84,03),AL1(4),C'840301'                               
*        DC    C'TTN',AL1(84,05),AL1(4),C'840503'                               
*        DC    C'TTN',AL1(84,07),AL1(4),C'840712'                               
*        DC    C'TTN',AL1(84,10),AL1(4),C'840927'                               
*        DC    C'TTN',AL1(84,11),AL1(4),C'841101'                               
** 1985 NSI                                                                     
*        DC    C'TTN',AL1(85,01),AL1(4),C'850103'                               
*        DC    C'TTN',AL1(85,02),AL1(4),C'850131'                               
*        DC    C'TTN',AL1(85,03),AL1(4),C'850228'                               
*        DC    C'TTN',AL1(85,05),AL1(4),C'850502'                               
*        DC    C'TTN',AL1(85,07),AL1(4),C'850711'                               
*        DC    C'TTN',AL1(85,10),AL1(4),C'850926'                               
*        DC    C'TTN',AL1(85,11),AL1(4),C'851031'                               
** 1986 NSI                                                                     
*        DC    C'TTN',AL1(86,01),AL1(4),C'860102'                               
*        DC    C'TTN',AL1(86,02),AL1(4),C'860130'                               
*        DC    C'TTN',AL1(86,03),AL1(4),C'860227'                               
*        DC    C'TTN',AL1(86,05),AL1(4),C'860501'                               
*        DC    C'TTN',AL1(86,07),AL1(4),C'860710'                               
*        DC    C'TTN',AL1(86,10),AL1(4),C'860925'                               
*        DC    C'TTN',AL1(86,11),AL1(4),C'861030'                               
** 1987 NSI                                                                     
*        DC    C'TTN',AL1(87,01),AL1(4),C'870108'                               
*        DC    C'TTN',AL1(87,02),AL1(4),C'870205'                               
*        DC    C'TTN',AL1(87,03),AL1(4),C'870305'                               
*        DC    C'TTN',AL1(87,05),AL1(4),C'870430'                               
*        DC    C'TTN',AL1(87,07),AL1(4),C'870709'                               
*        DC    C'TTN',AL1(87,10),AL1(4),C'870924'                               
*        DC    C'TTN',AL1(87,11),AL1(4),C'871031'                               
** 1988 NSI                                                                     
*        DC    C'TTN',AL1(88,01),AL1(4),C'880107'                               
*        DC    C'TTN',AL1(88,02),AL1(4),C'880204'                               
*        DC    C'TTN',AL1(88,03),AL1(4),C'880303'                               
*        DC    C'TTN',AL1(88,05),AL1(4),C'880428'                               
*        DC    C'TTN',AL1(88,07),AL1(4),C'880707'                               
*        DC    C'TTN',AL1(88,10),AL1(4),C'880929'                               
*        DC    C'TTN',AL1(88,11),AL1(4),C'881103'                               
** 1989 NSI                                                                     
*        DC    C'TTN',AL1(89,01),AL1(4),C'890105'                               
*        DC    C'TTN',AL1(89,02),AL1(4),C'890202'                               
*        DC    C'TTN',AL1(89,03),AL1(4),C'890302'                               
*        DC    C'TTN',AL1(89,05),AL1(4),C'890427'                               
*        DC    C'TTN',AL1(89,07),AL1(4),C'890706'                               
*        DC    C'TTN',AL1(89,10),AL1(4),C'890928'                               
*        DC    C'TTN',AL1(89,11),AL1(4),C'891102'                               
** 1990 NSI                                                                     
*        DC    C'TTN',AL1(90,01),AL1(4),C'900104'                               
*        DC    C'TTN',AL1(90,02),AL1(4),C'900201'                               
*        DC    C'TTN',AL1(90,03),AL1(4),C'900301'                               
*        DC    C'TTN',AL1(90,05),AL1(4),C'900426'                               
*        DC    C'TTN',AL1(90,07),AL1(4),C'900712'                               
*        DC    C'TTN',AL1(90,10),AL1(4),C'900927'                               
*        DC    C'TTN',AL1(90,11),AL1(4),C'901101'                               
** 1991 NSI                                                                     
         SPACE 3                                                                
         WK_STRT TTN,JAN_91,4_WEEK_SWEEP,1991-01-03                             
         WK_STRT TTN,FEB_91,4_WEEK_SWEEP,1991-01-31,MAJOR=Y                     
         WK_STRT TTN,MAR_91,4_WEEK_SWEEP,1991-02-28                             
         WK_STRT TTN,MAY_91,4_WEEK_SWEEP,1991-04-25,MAJOR=Y                     
         WK_STRT TTN,JUL_91,4_WEEK_SWEEP,1991-07-11,MAJOR=Y                     
         WK_STRT TTN,OCT_91,4_WEEK_SWEEP,1991-09-26                             
         WK_STRT TTN,NOV_91,4_WEEK_SWEEP,1991-10-31,MAJOR=Y                     
** 1992 NSI                                                                     
         WK_STRT TTN,JAN_92,4_WEEK_SWEEP,1992-01-09                             
         WK_STRT TTN,FEB_92,4_WEEK_SWEEP,1992-02-06,MAJOR=Y                     
         WK_STRT TTN,MAR_92,4_WEEK_SWEEP,1992-03-05                             
         WK_STRT TTN,MAY_92,4_WEEK_SWEEP,1992-04-23,MAJOR=Y                     
         WK_STRT TTN,JUL_92,4_WEEK_SWEEP,1992-07-09,MAJOR=Y                     
         WK_STRT TTN,OCT_92,4_WEEK_SWEEP,1992-09-24                             
         WK_STRT TTN,NOV_92,4_WEEK_SWEEP,1992-10-29,MAJOR=Y                     
** 1993 NSI                                                                     
         WK_STRT TTN,JAN_93,4_WEEK_SWEEP,1993-01-07                             
         WK_STRT TTN,FEB_93,4_WEEK_SWEEP,1993-02-04,MAJOR=Y                     
         WK_STRT TTN,MAR_93,4_WEEK_SWEEP,1993-03-04                             
         WK_STRT TTN,MAY_93,4_WEEK_SWEEP,1993-04-29,MAJOR=Y                     
         WK_STRT TTN,JUL_93,4_WEEK_SWEEP,1993-07-08,MAJOR=Y                     
         WK_STRT TTN,OCT_93,4_WEEK_SWEEP,1993-09-30                             
         WK_STRT TTN,NOV_93,4_WEEK_SWEEP,1993-11-04,MAJOR=Y                     
** 1994 NSI                                                                     
         WK_STRT TTN,JAN_94,4_WEEK_SWEEP,1994-01-06                             
         WK_STRT TTN,FEB_94,4_WEEK_SWEEP,1994-02-03,MAJOR=Y                     
         WK_STRT TTN,MAR_94,4_WEEK_SWEEP,1994-03-03                             
         WK_STRT TTN,MAY_94,4_WEEK_SWEEP,1994-04-28,MAJOR=Y                     
         WK_STRT TTN,JUL_94,4_WEEK_SWEEP,1994-07-07,MAJOR=Y                     
         WK_STRT TTN,OCT_94,4_WEEK_SWEEP,1994-09-29                             
         WK_STRT TTN,NOV_94,4_WEEK_SWEEP,1994-11-03,MAJOR=Y                     
** 1995 NSI                                                                     
         WK_STRT TTN,JAN_95,4_WEEK_SWEEP,1995-01-05                             
         WK_STRT TTN,FEB_95,4_WEEK_SWEEP,1995-02-02,MAJOR=Y                     
         WK_STRT TTN,MAR_95,4_WEEK_SWEEP,1995-03-02                             
         WK_STRT TTN,MAY_95,4_WEEK_SWEEP,1995-04-27,MAJOR=Y                     
         WK_STRT TTN,JUL_95,4_WEEK_SWEEP,1995-07-06,MAJOR=Y                     
         WK_STRT TTN,OCT_95,4_WEEK_SWEEP,1995-09-28                             
         WK_STRT TTN,NOV_95,4_WEEK_SWEEP,1995-11-02,MAJOR=Y                     
** 1996 NSI                                                                     
         WK_STRT TTN,JAN_96,4_WEEK_SWEEP,1996-01-04                             
         WK_STRT TTN,FEB_96,4_WEEK_SWEEP,1996-02-01,MAJOR=Y                     
         WK_STRT TTN,MAR_96,4_WEEK_SWEEP,1996-02-29                             
         WK_STRT TTN,MAY_96,4_WEEK_SWEEP,1996-04-25,MAJOR=Y                     
         WK_STRT TTN,JUL_96,4_WEEK_SWEEP,1996-07-11,MAJOR=Y                     
         WK_STRT TTN,OCT_96,4_WEEK_SWEEP,1996-09-26                             
         WK_STRT TTN,NOV_96,4_WEEK_SWEEP,1996-10-31,MAJOR=Y                     
** 1997 NSI                                                                     
         WK_STRT TTN,JAN_97,4_WEEK_SWEEP,1997-01-02                             
         WK_STRT TTN,FEB_97,4_WEEK_SWEEP,1997-01-30,MAJOR=Y                     
         WK_STRT TTN,MAR_97,4_WEEK_SWEEP,1997-02-27                             
         WK_STRT TTN,MAY_97,4_WEEK_SWEEP,1997-04-24,MAJOR=Y                     
         WK_STRT TTN,JUL_97,4_WEEK_SWEEP,1997-07-10,MAJOR=Y                     
         WK_STRT TTN,OCT_97,4_WEEK_SWEEP,1997-09-25                             
         WK_STRT TTN,NOV_97,4_WEEK_SWEEP,1997-10-30,MAJOR=Y                     
** 1998 NSI                                                                     
         WK_STRT TTN,JAN_98,4_WEEK_SWEEP,1998-01-08                             
         WK_STRT TTN,FEB_98,4_WEEK_SWEEP,1998-02-05,MAJOR=Y                     
         WK_STRT TTN,MAR_98,4_WEEK_SWEEP,1998-03-05                             
         WK_STRT TTN,MAY_98,4_WEEK_SWEEP,1998-04-23,MAJOR=Y                     
         WK_STRT TTN,JUL_98,4_WEEK_SWEEP,1998-07-09,MAJOR=Y                     
         WK_STRT TTN,OCT_98,4_WEEK_SWEEP,1998-09-24                             
         WK_STRT TTN,NOV_98,4_WEEK_SWEEP,1998-10-29,MAJOR=Y                     
** 1999 NSI                                                                     
         WK_STRT TTN,JAN_99,4_WEEK_SWEEP,1999-01-07                             
         WK_STRT TTN,FEB_99,4_WEEK_SWEEP,1999-02-04,MAJOR=Y                     
         WK_STRT TTN,MAR_99,4_WEEK_SWEEP,1999-03-04                             
         WK_STRT TTN,MAY_99,4_WEEK_SWEEP,1999-04-29,MAJOR=Y                     
         WK_STRT TTN,JUL_99,4_WEEK_SWEEP,1999-07-08,MAJOR=Y                     
         WK_STRT TTN,OCT_99,4_WEEK_SWEEP,1999-09-30                             
         WK_STRT TTN,NOV_99,4_WEEK_SWEEP,1999-11-04,MAJOR=Y                     
** 2000 NSI                                                                     
         WK_STRT TTN,JAN_00,4_WEEK_SWEEP,2000-01-06                             
         WK_STRT TTN,FEB_00,4_WEEK_SWEEP,2000-02-03,MAJOR=Y                     
         WK_STRT TTN,MAR_00,4_WEEK_SWEEP,2000-03-02                             
         WK_STRT TTN,MAY_00,4_WEEK_SWEEP,2000-04-27,MAJOR=Y                     
         WK_STRT TTN,JUL_00,4_WEEK_SWEEP,2000-07-06,MAJOR=Y                     
         WK_STRT TTN,OCT_00,4_WEEK_SWEEP,2000-09-28                             
         WK_STRT TTN,NOV_00,4_WEEK_SWEEP,2000-11-02,MAJOR=Y                     
** 2001 NSI                                                                     
         WK_STRT TTN,JAN_01,4_WEEK_SWEEP,2001-01-04                             
         WK_STRT TTN,FEB_01,4_WEEK_SWEEP,2001-02-01,MAJOR=Y                     
         WK_STRT TTN,MAR_01,4_WEEK_SWEEP,2001-03-01                             
         WK_STRT TTN,MAY_01,4_WEEK_SWEEP,2001-04-26,MAJOR=Y                     
         WK_STRT TTN,JUL_01,4_WEEK_SWEEP,2001-07-05,MAJOR=Y                     
         WK_STRT TTN,OCT_01,4_WEEK_SWEEP,2001-09-27                             
         WK_STRT TTN,NOV_01,4_WEEK_SWEEP,2001-11-01,MAJOR=Y                     
** 2002 NSI                                                                     
         WK_STRT TTN,JAN_02,4_WEEK_SWEEP,2002-01-03                             
         WK_STRT TTN,FEB_02,4_WEEK_SWEEP,2002-01-31,MAJOR=Y                     
         WK_STRT TTN,MAR_02,4_WEEK_SWEEP,2002-02-28                             
         WK_STRT TTN,MAY_02,4_WEEK_SWEEP,2002-04-25,MAJOR=Y                     
         WK_STRT TTN,JUL_02,4_WEEK_SWEEP,2002-07-11,MAJOR=Y                     
         WK_STRT TTN,OCT_02,4_WEEK_SWEEP,2002-09-26                             
         WK_STRT TTN,NOV_02,4_WEEK_SWEEP,2002-10-31,MAJOR=Y                     
** 2003 NSI                                                                     
         WK_STRT TTN,JAN_03,4_WEEK_SWEEP,2003-01-02                             
         WK_STRT TTN,FEB_03,4_WEEK_SWEEP,2003-01-30,MAJOR=Y                     
         WK_STRT TTN,MAR_03,4_WEEK_SWEEP,2003-02-27                             
         WK_STRT TTN,MAY_03,4_WEEK_SWEEP,2003-04-24,MAJOR=Y                     
         WK_STRT TTN,JUL_03,4_WEEK_SWEEP,2003-07-10,MAJOR=Y                     
         WK_STRT TTN,SEP_03,3_WEEK_SWEEP,2003-08-28                             
         WK_STRT TTN,OCT_03,4_WEEK_SWEEP,2003-09-25                             
         WK_STRT TTN,NOV_03,4_WEEK_SWEEP,2003-10-30,MAJOR=Y                     
         WK_STRT TTN,DEC_03,4_WEEK_SWEEP,2003-11-27                             
** 2004 NSI                                                                     
         WK_STRT TTN,JAN_04,4_WEEK_SWEEP,2004-01-08                             
         WK_STRT TTN,FEB_04,4_WEEK_SWEEP,2004-02-05,MAJOR=Y                     
         WK_STRT TTN,MAR_04,4_WEEK_SWEEP,2004-03-04                             
         WK_STRT TTN,APR_04,3_WEEK_SWEEP,2004-04-08                             
         WK_STRT TTN,MAY_04,4_WEEK_SWEEP,2004-04-29,MAJOR=Y                     
         WK_STRT TTN,JUN_04,4_WEEK_SWEEP,2004-06-03                             
         WK_STRT TTN,JUL_04,4_WEEK_SWEEP,2004-07-08,MAJOR=Y                     
         WK_STRT TTN,AUG_04,3_WEEK_SWEEP,2004-08-05                             
         WK_STRT TTN,SEP_04,4_WEEK_SWEEP,2004-08-26                             
         WK_STRT TTN,OCT_04,4_WEEK_SWEEP,2004-09-30                             
         WK_STRT TTN,NOV_04,4_WEEK_SWEEP,2004-11-04,MAJOR=Y                     
         WK_STRT TTN,DEC_04,4_WEEK_SWEEP,2004-12-02                             
** 2005 NSI                                                                     
         WK_STRT TTN,JAN_05,4_WEEK_SWEEP,2005-01-06                             
         WK_STRT TTN,FEB_05,4_WEEK_SWEEP,2005-02-03,MAJOR=Y                     
         WK_STRT TTN,MAR_05,4_WEEK_SWEEP,2005-03-03                             
         WK_STRT TTN,APR_05,3_WEEK_SWEEP,2005-04-07                             
         WK_STRT TTN,MAY_05,4_WEEK_SWEEP,2005-04-28,MAJOR=Y                     
         WK_STRT TTN,JUN_05,4_WEEK_SWEEP,2005-06-02                             
         WK_STRT TTN,JUL_05,4_WEEK_SWEEP,2005-06-30,MAJOR=Y                     
         WK_STRT TTN,AUG_05,4_WEEK_SWEEP,2005-07-28                             
         WK_STRT TTN,SEP_05,4_WEEK_SWEEP,2005-08-25                             
         WK_STRT TTN,OCT_05,4_WEEK_SWEEP,2005-09-29                             
         WK_STRT TTN,NOV_05,4_WEEK_SWEEP,2005-11-03,MAJOR=Y                     
         WK_STRT TTN,DEC_05,4_WEEK_SWEEP,2005-12-01                             
** 2006 NSI                                                                     
         WK_STRT TTN,JAN_06,4_WEEK_SWEEP,2006-01-05                             
         WK_STRT TTN,FEB_06,4_WEEK_SWEEP,2006-02-02,MAJOR=Y                     
         WK_STRT TTN,MAR_06,4_WEEK_SWEEP,2006-03-02                             
         WK_STRT TTN,APR_06,3_WEEK_SWEEP,2006-04-06                             
         WK_STRT TTN,MAY_06,4_WEEK_SWEEP,2006-04-27,MAJOR=Y                     
         WK_STRT TTN,JUN_06,4_WEEK_SWEEP,2006-06-01                             
         WK_STRT TTN,JUL_06,4_WEEK_SWEEP,2006-06-29,MAJOR=Y                     
         WK_STRT TTN,AUG_06,4_WEEK_SWEEP,2006-07-27                             
         WK_STRT TTN,SEP_06,4_WEEK_SWEEP,2006-08-24                             
         WK_STRT TTN,OCT_06,4_WEEK_SWEEP,2006-09-28                             
         WK_STRT TTN,NOV_06,4_WEEK_SWEEP,2006-11-02,MAJOR=Y                     
         WK_STRT TTN,DEC_06,4_WEEK_SWEEP,2006-11-30                             
** 2007 NSI                                                                     
         WK_STRT TTN,JAN_07,4_WEEK_SWEEP,2007-01-04                             
         WK_STRT TTN,FEB_07,4_WEEK_SWEEP,2007-02-01,MAJOR=Y                     
         WK_STRT TTN,MAR_07,4_WEEK_SWEEP,2007-03-01                             
         WK_STRT TTN,APR_07,4_WEEK_SWEEP,2007-03-29                             
         WK_STRT TTN,MAY_07,4_WEEK_SWEEP,2007-04-26,MAJOR=Y                     
         WK_STRT TTN,JUN_07,4_WEEK_SWEEP,2007-05-31                             
         WK_STRT TTN,JUL_07,4_WEEK_SWEEP,2007-07-05,MAJOR=Y                     
         WK_STRT TTN,AUG_07,3_WEEK_SWEEP,2007-08-02                             
         WK_STRT TTN,SEP_07,4_WEEK_SWEEP,2007-08-23                             
         WK_STRT TTN,OCT_07,4_WEEK_SWEEP,2007-10-04                             
         WK_STRT TTN,NOV_07,4_WEEK_SWEEP,2007-11-01,MAJOR=Y                     
         WK_STRT TTN,DEC_07,4_WEEK_SWEEP,2007-11-29                             
** 2008 NSI                                                                     
         WK_STRT TTN,JAN_08,4_WEEK_SWEEP,2008-01-03                             
         WK_STRT TTN,FEB_08,4_WEEK_SWEEP,2008-01-31,MAJOR=Y                     
         WK_STRT TTN,MAR_08,4_WEEK_SWEEP,2008-02-28                             
         WK_STRT TTN,APR_08,4_WEEK_SWEEP,2008-03-27                             
         WK_STRT TTN,MAY_08,4_WEEK_SWEEP,2008-04-24,MAJOR=Y                     
         WK_STRT TTN,JUN_08,4_WEEK_SWEEP,2008-05-29                             
         WK_STRT TTN,JUL_08,4_WEEK_SWEEP,2008-07-03,MAJOR=Y                     
         WK_STRT TTN,AUG_08,4_WEEK_SWEEP,2008-07-31                             
         WK_STRT TTN,SEP_08,4_WEEK_SWEEP,2008-08-28                             
         WK_STRT TTN,OCT_08,4_WEEK_SWEEP,2008-10-02                             
         WK_STRT TTN,NOV_08,4_WEEK_SWEEP,2008-10-30,MAJOR=Y                     
         WK_STRT TTN,DEC_08,4_WEEK_SWEEP,2008-12-04                             
*                                                                               
** 2009 NSI                                                                     
*** NOTE: THIS IS A FUNKY YEAR BECAUSE NIELSEN ISN'T CONDUCTING SURVEYS         
*** IN FEB/09 (DUE TO THE CONVERSION TO DIGITAL TV SIGNALS).                    
*** ALSO, THE APRIL SWEEP WAS SHORTENED TO 3 WEEKS, AND THE MAY SWEEP           
*** WAS MOVED BACK A WEEK, SO THE MEMORIAL DAY WEEKEND WOULDN'T BE              
*** INCLUDED IN THE MAY SWEEP.                                                  
         WK_STRT TTN,JAN_09,4_WEEK_SWEEP,2009-01-08                             
         WK_STRT TTN,FEB_09,4_WEEK_SWEEP,2009-02-05                             
         WK_STRT TTN,MAR_09,4_WEEK_SWEEP,2009-03-05,MAJOR=Y                     
         WK_STRT TTN,APR_09,3_WEEK_SWEEP,2009-04-02                             
         WK_STRT TTN,MAY_09,4_WEEK_SWEEP,2009-04-23,MAJOR=Y                     
         WK_STRT TTN,JUN_09,4_WEEK_SWEEP,2009-05-28                             
         WK_STRT TTN,JUL_09,4_WEEK_SWEEP,2009-07-02,MAJOR=Y                     
         WK_STRT TTN,AUG_09,4_WEEK_SWEEP,2009-07-30                             
         WK_STRT TTN,SEP_09,4_WEEK_SWEEP,2009-08-27                             
         WK_STRT TTN,OCT_09,4_WEEK_SWEEP,2009-10-01                             
         WK_STRT TTN,NOV_09,4_WEEK_SWEEP,2009-10-29,MAJOR=Y                     
         WK_STRT TTN,DEC_09,4_WEEK_SWEEP,2009-12-03                             
*                                                                               
** EFFECTIVE 2010, DEIS SAYS:                                                   
***  IF THE SWEEP PERIOD FOR A MONTH CONTAINS *ANY* WEEKS DURING WHICH          
***  THE OLYMPIC GAMES OCCUR, THEN WE MUST SPECIFY THE "OLYMPICS=Y"             
***  PARAMETER. THIS IS HOW WE KNOW TO RETAIN A GIVEN SURVEY ON DISK            
***  LONGER THAN THE STANDARD "HISTORICAL BOOK" PURGE PERIOD.                   
*                                                                               
** 2010 NSI                                                                     
         WK_STRT TTN,JAN_10,4_WEEK_SWEEP,2010-01-07                             
         WK_STRT TTN,FEB_10,4_WEEK_SWEEP,2010-02-04,MAJOR=Y,OLYMPICS=Y          
         WK_STRT TTN,MAR_10,4_WEEK_SWEEP,2010-03-04                             
         WK_STRT TTN,APR_10,4_WEEK_SWEEP,2010-04-01                             
         WK_STRT TTN,MAY_10,4_WEEK_SWEEP,2010-04-29,MAJOR=Y                     
         WK_STRT TTN,JUN_10,4_WEEK_SWEEP,2010-06-03                             
         WK_STRT TTN,JUL_10,4_WEEK_SWEEP,2010-07-01,MAJOR=Y                     
         WK_STRT TTN,AUG_10,4_WEEK_SWEEP,2010-07-29                             
         WK_STRT TTN,SEP_10,4_WEEK_SWEEP,2010-08-26                             
         WK_STRT TTN,OCT_10,4_WEEK_SWEEP,2010-09-30                             
         WK_STRT TTN,NOV_10,4_WEEK_SWEEP,2010-10-28,MAJOR=Y                     
         WK_STRT TTN,DEC_10,4_WEEK_SWEEP,2010-12-02                             
*                                                                               
** 2011 NSI                                                                     
         WK_STRT TTN,JAN_11,4_WEEK_SWEEP,2011-01-06                             
         WK_STRT TTN,FEB_11,4_WEEK_SWEEP,2011-02-03,MAJOR=Y                     
         WK_STRT TTN,MAR_11,4_WEEK_SWEEP,2011-03-03                             
         WK_STRT TTN,APR_11,4_WEEK_SWEEP,2011-03-31                             
         WK_STRT TTN,MAY_11,4_WEEK_SWEEP,2011-04-28,MAJOR=Y                     
         WK_STRT TTN,JUN_11,4_WEEK_SWEEP,2011-06-02                             
         WK_STRT TTN,JUL_11,4_WEEK_SWEEP,2011-06-30,MAJOR=Y                     
         WK_STRT TTN,AUG_11,4_WEEK_SWEEP,2011-07-28                             
         WK_STRT TTN,SEP_11,4_WEEK_SWEEP,2011-08-25                             
         WK_STRT TTN,OCT_11,4_WEEK_SWEEP,2011-09-29                             
         WK_STRT TTN,NOV_11,4_WEEK_SWEEP,2011-10-27,MAJOR=Y                     
         WK_STRT TTN,DEC_11,4_WEEK_SWEEP,2011-12-01                             
*                                                                               
** 2012 NSI                                                                     
         WK_STRT TTN,JAN_12,4_WEEK_SWEEP,2012-01-05                             
         WK_STRT TTN,FEB_12,4_WEEK_SWEEP,2012-02-02,MAJOR=Y                     
         WK_STRT TTN,MAR_12,4_WEEK_SWEEP,2012-03-01                             
         WK_STRT TTN,APR_12,4_WEEK_SWEEP,2012-03-29                             
         WK_STRT TTN,MAY_12,4_WEEK_SWEEP,2012-04-26,MAJOR=Y                     
         WK_STRT TTN,JUN_12,4_WEEK_SWEEP,2012-05-31                             
         WK_STRT TTN,JUL_12,4_WEEK_SWEEP,2012-06-28,MAJOR=Y                     
         WK_STRT TTN,AUG_12,4_WEEK_SWEEP,2012-07-26,OLYMPICS=Y                  
         WK_STRT TTN,SEP_12,4_WEEK_SWEEP,2012-08-23                             
         WK_STRT TTN,OCT_12,4_WEEK_SWEEP,2012-09-27                             
         WK_STRT TTN,NOV_12,4_WEEK_SWEEP,2012-10-25,MAJOR=Y                     
         WK_STRT TTN,DEC_12,4_WEEK_SWEEP,2012-11-29                             
*                                                                               
** 2013 NSI                                                                     
         WK_STRT TTN,JAN_13,4_WEEK_SWEEP,2013-01-03                             
         WK_STRT TTN,FEB_13,4_WEEK_SWEEP,2013-01-31,MAJOR=Y                     
         WK_STRT TTN,MAR_13,4_WEEK_SWEEP,2013-02-28                             
         WK_STRT TTN,APR_13,4_WEEK_SWEEP,2013-03-28                             
         WK_STRT TTN,MAY_13,4_WEEK_SWEEP,2013-04-25,MAJOR=Y                     
         WK_STRT TTN,JUN_13,4_WEEK_SWEEP,2013-05-30                             
         WK_STRT TTN,JUL_13,4_WEEK_SWEEP,2013-06-27,MAJOR=Y                     
         WK_STRT TTN,AUG_13,4_WEEK_SWEEP,2013-08-01                             
         WK_STRT TTN,SEP_13,4_WEEK_SWEEP,2013-08-29                             
         WK_STRT TTN,OCT_13,4_WEEK_SWEEP,2013-10-03                             
         WK_STRT TTN,NOV_13,4_WEEK_SWEEP,2013-10-31,MAJOR=Y                     
         WK_STRT TTN,DEC_13,4_WEEK_SWEEP,2013-12-05                             
*                                                                               
** 2014 NSI                                                                     
         WK_STRT TTN,JAN_14,4_WEEK_SWEEP,2014-01-02                             
         WK_STRT TTN,FEB_14,4_WEEK_SWEEP,2014-01-30,MAJOR=Y,OLYMPICS=Y          
         WK_STRT TTN,MAR_14,4_WEEK_SWEEP,2014-02-27                             
         WK_STRT TTN,APR_14,4_WEEK_SWEEP,2014-03-27                             
         WK_STRT TTN,MAY_14,4_WEEK_SWEEP,2014-04-24,MAJOR=Y                     
         WK_STRT TTN,JUN_14,4_WEEK_SWEEP,2014-05-29                             
         WK_STRT TTN,JUL_14,4_WEEK_SWEEP,2014-07-03,MAJOR=Y                     
         WK_STRT TTN,AUG_14,4_WEEK_SWEEP,2014-07-31                             
         WK_STRT TTN,SEP_14,4_WEEK_SWEEP,2014-08-28                             
         WK_STRT TTN,OCT_14,4_WEEK_SWEEP,2014-10-02                             
         WK_STRT TTN,NOV_14,4_WEEK_SWEEP,2014-10-30,MAJOR=Y                     
         WK_STRT TTN,DEC_14,4_WEEK_SWEEP,2014-12-04                             
*                                                                               
** 2015 NSI                                                                     
         WK_STRT TTN,JAN_15,4_WEEK_SWEEP,2015-01-01                             
         WK_STRT TTN,FEB_15,4_WEEK_SWEEP,2015-01-29,MAJOR=Y                     
         WK_STRT TTN,MAR_15,4_WEEK_SWEEP,2015-02-26                             
         WK_STRT TTN,APR_15,4_WEEK_SWEEP,2015-03-26                             
         WK_STRT TTN,MAY_15,4_WEEK_SWEEP,2015-04-23,MAJOR=Y                     
         WK_STRT TTN,JUN_15,4_WEEK_SWEEP,2015-05-28                             
         WK_STRT TTN,JUL_15,4_WEEK_SWEEP,2015-07-02,MAJOR=Y                     
         WK_STRT TTN,AUG_15,4_WEEK_SWEEP,2015-07-30                             
         WK_STRT TTN,SEP_15,4_WEEK_SWEEP,2015-08-27                             
         WK_STRT TTN,OCT_15,4_WEEK_SWEEP,2015-10-01                             
         WK_STRT TTN,NOV_15,4_WEEK_SWEEP,2015-10-29,MAJOR=Y                     
         WK_STRT TTN,DEC_15,4_WEEK_SWEEP,2015-12-03                             
*                                                                               
** 2016 NSI                                                                     
         WK_STRT TTN,JAN_16,4_WEEK_SWEEP,2015-12-31                             
         WK_STRT TTN,FEB_16,4_WEEK_SWEEP,2016-02-04,MAJOR=Y                     
         WK_STRT TTN,MAR_16,4_WEEK_SWEEP,2016-03-03                             
         WK_STRT TTN,APR_16,4_WEEK_SWEEP,2016-03-31                             
         WK_STRT TTN,MAY_16,4_WEEK_SWEEP,2016-04-28,MAJOR=Y                     
         WK_STRT TTN,JUN_16,4_WEEK_SWEEP,2016-06-02                             
         WK_STRT TTN,JUL_16,4_WEEK_SWEEP,2016-06-30,MAJOR=Y                     
         WK_STRT TTN,AUG_16,4_WEEK_SWEEP,2016-07-28,OLYMPICS=Y                  
         WK_STRT TTN,SEP_16,4_WEEK_SWEEP,2016-08-25                             
         WK_STRT TTN,OCT_16,4_WEEK_SWEEP,2016-09-29                             
         WK_STRT TTN,NOV_16,4_WEEK_SWEEP,2016-10-27,MAJOR=Y                     
         WK_STRT TTN,DEC_16,4_WEEK_SWEEP,2016-12-01                             
*                                                                               
** 2017 NSI                                                                     
         WK_STRT TTN,JAN_17,4_WEEK_SWEEP,2017-01-05                             
         WK_STRT TTN,FEB_17,4_WEEK_SWEEP,2017-02-02,MAJOR=Y                     
         WK_STRT TTN,MAR_17,4_WEEK_SWEEP,2017-03-02                             
         WK_STRT TTN,APR_17,4_WEEK_SWEEP,2017-03-30                             
         WK_STRT TTN,MAY_17,4_WEEK_SWEEP,2017-04-27,MAJOR=Y                     
         WK_STRT TTN,JUN_17,4_WEEK_SWEEP,2017-06-01                             
         WK_STRT TTN,JUL_17,4_WEEK_SWEEP,2017-06-29,MAJOR=Y                     
         WK_STRT TTN,AUG_17,4_WEEK_SWEEP,2017-07-27                             
         WK_STRT TTN,SEP_17,4_WEEK_SWEEP,2017-08-24                             
         WK_STRT TTN,OCT_17,4_WEEK_SWEEP,2017-09-28                             
         WK_STRT TTN,NOV_17,4_WEEK_SWEEP,2017-10-26,MAJOR=Y                     
         WK_STRT TTN,DEC_17,4_WEEK_SWEEP,2017-11-30                             
*                                                                               
** 2018 NSI                                                                     
         WK_STRT TTN,JAN_18,4_WEEK_SWEEP,2018-01-04                             
         WK_STRT TTN,FEB_18,4_WEEK_SWEEP,2018-02-01,MAJOR=Y,OLYMPICS=Y          
         WK_STRT TTN,MAR_18,4_WEEK_SWEEP,2018-03-01                             
         WK_STRT TTN,APR_18,4_WEEK_SWEEP,2018-03-29                             
         WK_STRT TTN,MAY_18,4_WEEK_SWEEP,2018-04-26,MAJOR=Y                     
         WK_STRT TTN,JUN_18,4_WEEK_SWEEP,2018-05-31                             
         WK_STRT TTN,JUL_18,4_WEEK_SWEEP,2018-06-28,MAJOR=Y                     
         WK_STRT TTN,AUG_18,4_WEEK_SWEEP,2018-07-26                             
         WK_STRT TTN,SEP_18,4_WEEK_SWEEP,2018-08-23                             
         WK_STRT TTN,OCT_18,4_WEEK_SWEEP,2018-09-27                             
         WK_STRT TTN,NOV_18,4_WEEK_SWEEP,2018-10-25,MAJOR=Y                     
         WK_STRT TTN,DEC_18,4_WEEK_SWEEP,2018-11-29                             
*                                                                               
** 2019 NSI                                                                     
         WK_STRT TTN,JAN_19,4_WEEK_SWEEP,2019-01-03                             
         WK_STRT TTN,FEB_19,4_WEEK_SWEEP,2019-01-31,MAJOR=Y                     
         WK_STRT TTN,MAR_19,4_WEEK_SWEEP,2019-02-28                             
         WK_STRT TTN,APR_19,4_WEEK_SWEEP,2019-03-28                             
         WK_STRT TTN,MAY_19,4_WEEK_SWEEP,2019-04-25,MAJOR=Y                     
         WK_STRT TTN,JUN_19,4_WEEK_SWEEP,2019-05-30                             
         WK_STRT TTN,JUL_19,4_WEEK_SWEEP,2019-07-04,MAJOR=Y                     
         WK_STRT TTN,AUG_19,4_WEEK_SWEEP,2019-08-01                             
         WK_STRT TTN,SEP_19,4_WEEK_SWEEP,2019-08-29                             
         WK_STRT TTN,OCT_19,4_WEEK_SWEEP,2019-10-03                             
         WK_STRT TTN,NOV_19,4_WEEK_SWEEP,2019-10-31,MAJOR=Y                     
         WK_STRT TTN,DEC_19,4_WEEK_SWEEP,2019-12-05                             
*                                                                               
** 2020 NSI                                                                     
         WK_STRT TTN,JAN_20,4_WEEK_SWEEP,2020-01-02                             
         WK_STRT TTN,FEB_20,4_WEEK_SWEEP,2020-01-30,MAJOR=Y                     
         WK_STRT TTN,MAR_20,4_WEEK_SWEEP,2020-02-27                             
         WK_STRT TTN,APR_20,4_WEEK_SWEEP,2020-03-26                             
         WK_STRT TTN,MAY_20,4_WEEK_SWEEP,2020-04-23,MAJOR=Y                     
         WK_STRT TTN,JUN_20,4_WEEK_SWEEP,2020-05-28                             
         WK_STRT TTN,JUL_20,4_WEEK_SWEEP,2020-07-02,MAJOR=Y                     
         WK_STRT TTN,AUG_20,4_WEEK_SWEEP,2020-07-30                             
         WK_STRT TTN,SEP_20,4_WEEK_SWEEP,2020-08-27                             
         WK_STRT TTN,OCT_20,4_WEEK_SWEEP,2020-10-01                             
         WK_STRT TTN,NOV_20,4_WEEK_SWEEP,2020-10-29,MAJOR=Y                     
         WK_STRT TTN,DEC_20,4_WEEK_SWEEP,2020-12-03                             
*                                                                               
** 2021 NSI                                                                     
         WK_STRT TTN,JAN_21,4_WEEK_SWEEP,2021-01-07                             
         WK_STRT TTN,FEB_21,4_WEEK_SWEEP,2021-02-04,MAJOR=Y                     
         WK_STRT TTN,MAR_21,4_WEEK_SWEEP,2021-03-04                             
         WK_STRT TTN,APR_21,4_WEEK_SWEEP,2021-04-01                             
         WK_STRT TTN,MAY_21,4_WEEK_SWEEP,2021-04-29,MAJOR=Y                     
         WK_STRT TTN,JUN_21,4_WEEK_SWEEP,2021-06-03                             
         WK_STRT TTN,JUL_21,4_WEEK_SWEEP,2021-07-01,MAJOR=Y                     
         WK_STRT TTN,AUG_21,4_WEEK_SWEEP,2021-07-29                             
         WK_STRT TTN,SEP_21,4_WEEK_SWEEP,2021-08-26                             
         WK_STRT TTN,OCT_21,4_WEEK_SWEEP,2021-09-30                             
*                                                                               
** 1983 ARB                                                                     
*        DC    C'TTA',AL1(83,10),AL1(4),C'830928'                               
*        DC    C'TTA',AL1(83,11),AL1(4),C'831102'                               
** 1984 ARB                                                                     
*        DC    C'TTA',AL1(84,01),AL1(4),C'840104'                               
*        DC    C'TTA',AL1(84,02),AL1(4),C'840201'                               
*        DC    C'TTA',AL1(84,03),AL1(4),C'840229'                               
*        DC    C'TTA',AL1(84,05),AL1(4),C'840502'                               
*        DC    C'TTA',AL1(84,07),AL1(4),C'840711'                               
*        DC    C'TTA',AL1(84,10),AL1(4),C'840926'                               
*        DC    C'TTA',AL1(84,11),AL1(4),C'841031'                               
** 1985 ARB                                                                     
*        DC    C'TTA',AL1(85,01),AL1(4),C'850102'                               
*        DC    C'TTA',AL1(85,02),AL1(4),C'850130'                               
*        DC    C'TTA',AL1(85,03),AL1(4),C'850227'                               
*        DC    C'TTA',AL1(85,05),AL1(4),C'850501'                               
*        DC    C'TTA',AL1(85,07),AL1(4),C'850710'                               
*        DC    C'TTA',AL1(85,10),AL1(4),C'850925'                               
*        DC    C'TTA',AL1(85,11),AL1(4),C'851030'                               
** 1986 ARB                                                                     
*        DC    C'TTA',AL1(86,01),AL1(4),C'860101'                               
*        DC    C'TTA',AL1(86,02),AL1(4),C'860129'                               
*        DC    C'TTA',AL1(86,03),AL1(4),C'860226'                               
*        DC    C'TTA',AL1(86,05),AL1(4),C'860430'                               
*        DC    C'TTA',AL1(86,07),AL1(4),C'860709'                               
*        DC    C'TTA',AL1(86,10),AL1(4),C'860924'                               
*        DC    C'TTA',AL1(86,11),AL1(4),C'861029'                               
** 1987 ARB                                                                     
*        DC    C'TTA',AL1(87,01),AL1(4),C'870107'                               
*        DC    C'TTA',AL1(87,02),AL1(4),C'870204'                               
*        DC    C'TTA',AL1(87,03),AL1(4),C'870304'                               
*        DC    C'TTA',AL1(87,05),AL1(4),C'870429'                               
*        DC    C'TTA',AL1(87,07),AL1(4),C'870708'                               
*        DC    C'TTA',AL1(87,10),AL1(4),C'870923'                               
*        DC    C'TTA',AL1(87,11),AL1(4),C'871030'                               
** 1988 ARB                                                                     
*        DC    C'TTA',AL1(88,01),AL1(4),C'880106'                               
*        DC    C'TTA',AL1(88,02),AL1(4),C'880203'                               
*        DC    C'TTA',AL1(88,03),AL1(4),C'880302'                               
*        DC    C'TTA',AL1(88,05),AL1(4),C'880427'                               
*        DC    C'TTA',AL1(88,07),AL1(4),C'880706'                               
*        DC    C'TTA',AL1(88,10),AL1(4),C'880928'                               
*        DC    C'TTA',AL1(88,11),AL1(4),C'881102'                               
** 1989 ARB                                                                     
*        DC    C'TTA',AL1(89,01),AL1(4),C'890104'                               
*        DC    C'TTA',AL1(89,02),AL1(4),C'890201'                               
*        DC    C'TTA',AL1(89,03),AL1(4),C'890301'                               
*        DC    C'TTA',AL1(89,05),AL1(4),C'890426'                               
*        DC    C'TTA',AL1(89,07),AL1(4),C'890705'                               
*        DC    C'TTA',AL1(89,10),AL1(4),C'890927'                               
*        DC    C'TTA',AL1(89,11),AL1(4),C'891101'                               
** 1990 ARB                                                                     
*        DC    C'TTA',AL1(90,01),AL1(4),C'900103'                               
*        DC    C'TTA',AL1(90,02),AL1(4),C'900131'                               
*        DC    C'TTA',AL1(90,03),AL1(4),C'900228'                               
*        DC    C'TTA',AL1(90,05),AL1(4),C'900425'                               
*        DC    C'TTA',AL1(90,07),AL1(4),C'900711'                               
*        DC    C'TTA',AL1(90,10),AL1(4),C'900926'                               
*        DC    C'TTA',AL1(90,11),AL1(4),C'901031'                               
** 1991 ARB                                                                     
*        DC    C'TTA',AL1(91,01),AL1(4),C'910102'                               
*        DC    C'TTA',AL1(91,02),AL1(4),C'910130'                               
*        DC    C'TTA',AL1(91,03),AL1(4),C'910227'                               
*        DC    C'TTA',AL1(91,05),AL1(4),C'910424'                               
*        DC    C'TTA',AL1(91,07),AL1(4),C'910710'                               
*        DC    C'TTA',AL1(91,10),AL1(4),C'910925'                               
*        DC    C'TTA',AL1(91,11),AL1(4),C'911030'                               
** 1992 ARB                                                                     
*        DC    C'TTA',AL1(92,01),AL1(4),C'920108'                               
*        DC    C'TTA',AL1(92,02),AL1(4),C'920205'                               
*        DC    C'TTA',AL1(92,03),AL1(4),C'920304'                               
*        DC    C'TTA',AL1(92,05),AL1(4),C'920422'                               
*        DC    C'TTA',AL1(92,07),AL1(4),C'920708'                               
*        DC    C'TTA',AL1(92,10),AL1(4),C'920923'                               
*        DC    C'TTA',AL1(92,11),AL1(4),C'921028'                               
** 1993 ARB                                                                     
*        DC    C'TTA',AL1(93,01),AL1(4),C'930106'                               
*        DC    C'TTA',AL1(93,02),AL1(4),C'930203'                               
*        DC    C'TTA',AL1(93,03),AL1(4),C'930303'                               
*        DC    C'TTA',AL1(93,05),AL1(4),C'930428'                               
*        DC    C'TTA',AL1(93,07),AL1(4),C'930707'                               
*        DC    C'TTA',AL1(93,10),AL1(4),C'930929'                               
*        DC    C'TTA',AL1(93,11),AL1(4),C'931103'                               
** 1985 BBM                                                                     
         WK_STRT TCA,JAN_85,2_WEEK_SWEEP,1985-01-17                             
         WK_STRT TCA,MAR_85,3_WEEK_SWEEP,1985-02-21                             
         WK_STRT TCA,JUL_85,1_WEEK_SWEEP,1985-06-13                             
         WK_STRT TCA,JUL_85,1_WEEK_SWEEP,1985-07-11                             
         WK_STRT TCA,JUL_85,1_WEEK_SWEEP,1985-08-08                             
         WK_STRT TCA,NOV_85,3_WEEK_SWEEP,1985-10-17                             
** 1986 BBM                                                                     
         WK_STRT TCA,JAN_86,2_WEEK_SWEEP,1986-01-30                             
         WK_STRT TCA,MAR_86,3_WEEK_SWEEP,1986-02-20                             
         WK_STRT TCA,JUL_86,1_WEEK_SWEEP,1986-06-05                             
         WK_STRT TCA,JUL_86,1_WEEK_SWEEP,1986-07-10                             
         WK_STRT TCA,JUL_86,1_WEEK_SWEEP,1986-07-31                             
         WK_STRT TCA,NOV_86,3_WEEK_SWEEP,1986-10-30                             
** 1987 BBM                                                                     
         WK_STRT TCA,JAN_87,2_WEEK_SWEEP,1987-01-22                             
         WK_STRT TCA,MAR_87,3_WEEK_SWEEP,1987-02-12                             
         WK_STRT TCA,JUL_87,1_WEEK_SWEEP,1987-06-04                             
         WK_STRT TCA,JUL_87,1_WEEK_SWEEP,1987-07-09                             
         WK_STRT TCA,JUL_87,1_WEEK_SWEEP,1987-07-30                             
         WK_STRT TCA,NOV_87,3_WEEK_SWEEP,1987-10-29                             
** 1988 BBM                                                                     
         WK_STRT TCA,JAN_88,2_WEEK_SWEEP,1988-01-14                             
         WK_STRT TCA,FEB_88,2_WEEK_SWEEP,1988-02-11                             
         WK_STRT TCA,MAR_88,3_WEEK_SWEEP,1988-02-25                             
** 1992 BBM                                                                     
         WK_STRT TCA,OCT_92,2_WEEK_SWEEP,1992-09-24                             
         WK_STRT TCA,NOV_92,4_WEEK_SWEEP,1992-10-29                             
** 1993 BBM                                                                     
         WK_STRT TCA,MAR_93,3_WEEK_SWEEP,1993-02-25                             
         WK_STRT TCA,JUL_93,3_WEEK_SWEEP,1993-07-14                             
** 1985 CSI                                                                     
         WK_STRT TCN,SEP_85,3_WEEK_SWEEP,1985-09-09                             
         WK_STRT TCN,OCT_85,3_WEEK_SWEEP,1985-09-30                             
         WK_STRT TCN,NOV_85,3_WEEK_SWEEP,1985-10-21                             
** 1986 CSI                                                                     
         WK_STRT TCN,JAN_86,3_WEEK_SWEEP,1986-01-06                             
         WK_STRT TCN,FEB_86,3_WEEK_SWEEP,1986-01-27                             
         WK_STRT TCN,MAR_86,3_WEEK_SWEEP,1986-02-17                             
         WK_STRT TCN,AUG_86,3_WEEK_SWEEP,1986-07-28                             
         WK_STRT TCN,SEP_86,3_WEEK_SWEEP,1986-09-08                             
         WK_STRT TCN,OCT_86,3_WEEK_SWEEP,1986-09-29                             
         WK_STRT TCN,NOV_86,3_WEEK_SWEEP,1986-10-20                             
** 1987 CSI                                                                     
         WK_STRT TCN,JAN_87,3_WEEK_SWEEP,1987-01-05                             
         WK_STRT TCN,FEB_87,3_WEEK_SWEEP,1987-01-26                             
         WK_STRT TCN,MAR_87,3_WEEK_SWEEP,1987-02-16                             
         WK_STRT TCN,AUG_87,3_WEEK_SWEEP,1987-07-27                             
         WK_STRT TCN,SEP_87,3_WEEK_SWEEP,1987-09-14                             
         WK_STRT TCN,OCT_87,3_WEEK_SWEEP,1987-10-05                             
         WK_STRT TCN,NOV_87,3_WEEK_SWEEP,1987-10-26                             
** 1988 CSI                                                                     
         WK_STRT TCN,JAN_88,3_WEEK_SWEEP,1988-01-04                             
         WK_STRT TCN,FEB_88,3_WEEK_SWEEP,1988-01-25                             
         WK_STRT TCN,MAR_88,3_WEEK_SWEEP,1988-02-15                             
         WK_STRT TCN,AUG_88,3_WEEK_SWEEP,1988-07-25                             
** 1989 CSI                                                                     
         WK_STRT TCN,SEP_89,2_WEEK_SWEEP,1989-09-14                             
         WK_STRT TCN,OCT_89,1_WEEK_SWEEP,1989-10-09                             
         WK_STRT TCN,OCT_89,1_WEEK_SWEEP,1989-10-23                             
         WK_STRT TCN,NOV_89,3_WEEK_SWEEP,1989-10-30                             
** 1990 CSI                                                                     
         WK_STRT TCN,JAN_90,2_WEEK_SWEEP,1990-01-08                             
         WK_STRT TCN,FEB_90,2_WEEK_SWEEP,1990-02-05                             
         WK_STRT TCN,MAR_90,2_WEEK_SWEEP,1990-02-26                             
         WK_STRT TCN,JUL_90,2_WEEK_SWEEP,1990-07-16                             
** 1992 CSI                                                                     
         WK_STRT TCN,JAN_92,2_WEEK_SWEEP,1992-01-12                             
         WK_STRT TCN,MAR_92,2_WEEK_SWEEP,1992-02-23                             
         WK_STRT TCN,JUL_92,2_WEEK_SWEEP,1992-06-28                             
         WK_STRT TCN,OCT_92,3_WEEK_SWEEP,1992-09-21                             
         WK_STRT TCN,NOV_92,3_WEEK_SWEEP,1992-10-26                             
** 1993 CSI                                                                     
         WK_STRT TCN,JAN_93,2_WEEK_SWEEP,1993-01-11                             
         WK_STRT TCN,MAR_93,3_WEEK_SWEEP,1993-02-22                             
         WK_STRT TCN,JUL_93,2_WEEK_SWEEP,1993-06-28                             
** 1993 ARB RADIO                                                               
         WK_STRT TRA,FEB_93,4_WEEK_SWEEP,1993-02-01                             
         WK_STRT TRA,MAY_93,4_WEEK_SWEEP,1993-05-01                             
         WK_STRT TRA,JUL_93,4_WEEK_SWEEP,1993-07-01                             
         WK_STRT TRA,NOV_93,4_WEEK_SWEEP,1993-11-01                             
** 1994 ARB RADIO                                                               
         WK_STRT TRA,FEB_94,4_WEEK_SWEEP,1994-02-01                             
         WK_STRT TRA,MAY_94,4_WEEK_SWEEP,1994-05-01                             
         WK_STRT TRA,JUL_94,4_WEEK_SWEEP,1994-07-01                             
         WK_STRT TRA,NOV_94,4_WEEK_SWEEP,1994-11-01                             
** 1995 ARB RADIO                                                               
         WK_STRT TRA,FEB_95,4_WEEK_SWEEP,1995-02-01                             
         WK_STRT TRA,MAY_95,4_WEEK_SWEEP,1995-05-01                             
         WK_STRT TRA,JUL_95,4_WEEK_SWEEP,1995-07-01                             
         WK_STRT TRA,NOV_95,4_WEEK_SWEEP,1995-11-01                             
*                                                                               
         DC    X'00'               END OF SWEEPTBL                              
         SPACE 2                                                                
         TABLE_LEN TBL_SWEEPTBL                                                 
*                                                                               
         EJECT                                                                  
********************************************************                        
* MAINTAIN A LIST OF CANADIAN HOME STATIONS            *                        
********************************************************                        
TBL_HOMESTA DS 0D                                                               
         DC    CL8'*HOMSTA*'                                                    
         DS    XL6                                                              
         DC    AL2(HOMESTAQ)       L'TABLE ENTRY                                
*                                                                               
         CANHOM 0009,CBNT          ST JOHNS CMA                                 
         CANHOM 0009,CJON                                                       
         CANHOM 0009,CJON+                                                      
         CANHOM 0041,CBYT          CORNRBR C.AREA                               
         CANHOM 0041,CJWN                                                       
         CANHOM 0060,CJCN          CD6                                          
         CANHOM 1021,CBCT          CHARTWN C.AREA                               
         CANHOM 1031,ASN           ATLANTIC                                     
         CANHOM 1031,ATV           (ATV+ ON INPUT)                              
         CANHOM 2009,CBIT          SYD-GLAC,SYD-M                               
         CANHOM 2009,CJCB                                                       
         CANHOM 2010,CBIT          CAPE BRETON                                  
         CANHOM 2010,CJCB                                                       
         CANHOM 2079,CBHT          HALIFAX,CMA                                  
         CANHOM 2079,CJCH                                                       
         CANHOM 2080,CBHT          HALIFAX CO                                   
         CANHOM 2080,CIHF                                                       
         CANHOM 2080,CJCH                                                       
         CANHOM 3011,CBAF          STJN-MON C.AREA                              
         CANHOM 3011,CBAFT                                                      
         CANHOM 3011,CBAT                                                       
         CANHOM 3011,CHSJ                                                       
         CANHOM 3011,CKCW                                                       
         CANHOM 3011,WLBZ                                                       
         CANHOM 3011,WVII                                                       
         CANHOM 3111,CHAU          CARLTON C.AREA                               
         CANHOM 3111,CIVK                                                       
         CANHOM 4041,CBGAT         MATANE C.AREA                                
         CANHOM 4041,CIVF                                                       
         CANHOM 4061,CFER                                                       
         CANHOM 4061,CJBR                                                       
         CANHOM 4061,CJBRT                                                      
         CANHOM 4061,RCAN                                                       
         CANHOM 4071,CIVB          RIMOUSK C.AREA                               
         CANHOM 4091,CBST                                                       
         CANHOM 4091,CIVG                                                       
         CANHOM 4101,CFTF          RIVLOUP C.AREA                               
         CANHOM 4101,CIMT                                                       
         CANHOM 4101,CKRT                                                       
         CANHOM 4119,CJPM                                                       
         CANHOM 4119,CKRS                                                       
         CANHOM 4120,CFRS          CHICOUTI-JONQU                               
         CANHOM 4120,CIVV                                                       
         CANHOM 4120,CJPM                                                       
         CANHOM 4120,CKRS                                                       
         CANHOM 4120,CKTV                                                       
         CANHOM 4199,CBVT          QUEBEC                                       
         CANHOM 4199,CFAP                                                       
         CANHOM 4199,CFCM                                                       
         CANHOM 4199,CIVQ                                                       
         CANHOM 4199,CKMI                                                       
         CANHOM 4350,CFKS                                                       
         CANHOM 4350,CHLT                                                       
         CANHOM 4350,CIVS                                                       
         CANHOM 4350,CKSH                                                       
         CANHOM 4351,CFKS          SHERBRK C.AREA                               
         CANHOM 4351,CHLT                                                       
         CANHOM 4351,CIVS                                                       
         CANHOM 4351,CKSH                                                       
         CANHOM 4479,CBFT          MONTREAL                                     
         CANHOM 4479,CBFT+                                                      
         CANHOM 4479,CFAP                                                       
         CANHOM 4479,CFJP                                                       
         CANHOM 4479,CFJP+                                                      
         CANHOM 4479,CFTM                                                       
         CANHOM 4479,CIVM                                                       
         CANHOM 4479,CJPC                                                       
         CANHOM 4479,CKMI                                                       
         CANHOM 4479,WFFF                                                       
         CANHOM 4480,WFFF,MAR_10   MONTREAL ANGLO                               
         CANHOM 4480,WPTZ,MAR_10                                                
         CANHOM 4480,WVNY,MAR_10                                                
         CANHOM 4480,CBMT                                                       
         CANHOM 4480,CBMT+                                                      
         CANHOM 4480,CFCF                                                       
         CANHOM 4480,WCFE                                                       
         CANHOM 4480,WETK                                                       
         CANHOM 4661,CFKM                                                       
         CANHOM 4661,CHEM          TR RIV. C.AREA                               
         CANHOM 4661,CIVC                                                       
         CANHOM 4661,CKTM                                                       
         CANHOM 4667,CHLT+         SHER-TR RIV EM                               
         CANHOM 4667,CKTM+                                                      
         CANHOM 4667,R-QUE                                                      
         CANHOM 4723,CFEM          ROUYN EM                                     
         CANHOM 4723,CFVS                                                       
         CANHOM 4723,CIVN+                                                      
         CANHOM 4723,CKRN                                                       
         CANHOM 5071,CBOT          OTTAWA CMA                                   
         CANHOM 5071,CHRO                                                       
         CANHOM 5071,CJOH                                                       
         CANHOM 5072,CBOFT                                                      
         CANHOM 5072,CFGS                                                       
         CANHOM 5072,CHOT                                                       
         CANHOM 5072,CIVO                                                       
         CANHOM 5109,CKWS          KINGSTON CA                                  
         CANHOM 5109,WWTI                                                       
         CANHOM 5145,CECO          EAST CENTRAL ONTARIO                         
         CANHOM 5159,CHEX          PETERBOROUGH CA                              
************************************************************                    
* MATCHING TORONTO TO NIELSEN CONVERSION (MARCH 2008)                           
*                                                                               
* METERED!!!                                                                    
************************************************************                    
         CANHOM 5199,CBLFT         TORONTO                                      
         CANHOM 5199,CBLT                                                       
         CANHOM 5199,CFMT                                                       
         CANHOM 5199,CFTO                                                       
         CANHOM 5199,CHCH                                                       
         CANHOM 5199,CICO                                                       
         CANHOM 5199,CITY                                                       
         CANHOM 5199,WGRZ                                                       
         CANHOM 5199,WIVB                                                       
         CANHOM 5199,WKBW                                                       
         CANHOM 5199,WNED                                                       
         CANHOM 5199,WUTV                                                       
************************************************************                    
         CANHOM 5243,CKVR          BARRIE C.AREA                                
         CANHOM 5269,,             HAMILTON CMA                                 
         CANHOM 5339,CKCO          KITCHENER CMA                                
         CANHOM 5369,CBLN          LONDON CMA                                   
         CANHOM 5369,CFPL                                                       
         CANHOM 5369,CFPL+                                                      
         CANHOM 5369,CKNX                                                       
         CANHOM 5369,WICU                                                       
         CANHOM 5369,WJET                                                       
         CANHOM 5369,WSEE                                                       
         CANHOM 5369,WUAB                                                       
         CANHOM 5409,CBEFT         WINDSOR CMA                                  
         CANHOM 5409,CBET                                                       
         CANHOM 5409,WDIV                                                       
         CANHOM 5409,WJBK                                                       
         CANHOM 5409,WKBD                                                       
         CANHOM 5409,WXON                                                       
         CANHOM 5409,WXYZ                                                       
         CANHOM 5409,CHWI,MAR_10   WINDSOR (NEW 2010)                           
         CANHOM 5441,,             WINGHAM C.AREA                               
         CANHOM 5469,,             NORTH BAY CA                                 
         CANHOM 5479,CBFST         SUDBURY 8011                                 
         CANHOM 5479,CICI+                                                      
         CANHOM 5479,CKNC+                                                      
         CANHOM 5499,,             TIMMINS C                                    
         CANHOM 5531,CHBX          ALGOMA WEST                                  
         CANHOM 5531,CJIC                                                       
         CANHOM 5531,CKCY                                                       
         CANHOM 5531,WJRT                                                       
         CANHOM 5531,WKBD                                                       
         CANHOM 5531,WNEM                                                       
         CANHOM 5531,WWUP                                                       
         CANHOM 5539,CHFD          THUNDER BAY                                  
         CANHOM 5539,CKPR                                                       
         CANHOM 5539,KBJR                                                       
         CANHOM 5539,KDLH                                                       
         CANHOM 5539,WDIO                                                       
         CANHOM 5565,CJBN          KENORA                                       
         CANHOM 6061,CKX           BRANDON C AREA                               
         CANHOM 6119,CBWFT         WINNIPEG CMA                                 
         CANHOM 6119,CBWT                                                       
         CANHOM 6119,CHMI                                                       
         CANHOM 6119,CKND                                                       
         CANHOM 6119,CKY                                                        
         CANHOM 6119,KTHI                                                       
         CANHOM 6119,KTHT                                                       
         CANHOM 6119,KXJB                                                       
         CANHOM 6119,WDAZ                                                       
         CANHOM 6119,CIIT,MAR_10   WINNIPEGE (NEW 2010)                         
         CANHOM 7011,CICC          YORKTON C.AREA                               
         CANHOM 7011,CKOS                                                       
         CANHOM 7045,CJFB          CD4,CD8 (SWIFT CURRENT)                      
         CANHOM 7071,CBKFT         REG-MOOS CAREA                               
         CANHOM 7071,CBKT                                                       
         CANHOM 7071,CFRE                                                       
         CANHOM 7071,CKCK                                                       
         CANHOM 7071,KUMV                                                       
         CANHOM 7109,CBKST         SASKATOON CMA                                
         CANHOM 7109,CFQC                                                       
         CANHOM 7109,CFSK                                                       
         CANHOM 7109,KXMD                                                       
         CANHOM 7153,CIPA          PR ALBER C.AREA                              
         CANHOM 7153,CKBI                                                       
         CANHOM 8010,CHAT          MEDICINE HAT                                 
         CANHOM 8010,KFBB                                                       
         CANHOM 8020,CJOC          CD2                                          
************************************************************                    
* MATCHING CALGARY TO NIELSEN CONVERSION (MARCH 2008)                           
*                                                                               
* METERED!!!                                                                    
************************************************************                    
         CANHOM 8069,CBRT          CALGARY CMA                                  
         CANHOM 8069,CFCN                                                       
         CANHOM 8069,CTVC                                                       
         CANHOM 8069,CKAL          ...NEW CKKX                                  
         CANHOM 8069,KAYU                                                       
         CANHOM 8069,KHQ                                                        
         CANHOM 8069,KIRO                                                       
         CANHOM 8069,KREM          8119 EDMONTON HAS KREM                       
         CANHOM 8069,KSPS                                                       
************************************************************                    
         CANHOM 8078,CHCA          RED DEER                                     
         CANHOM 8080,CHCA                                                       
         CANHOM 8091,CITL          LLOYDMIN C AREA                              
         CANHOM 8091,CKSA                                                       
         CANHOM 8119,ACCES         EDMONTON CMA                                 
         CANHOM 8119,CBXFT         EDMONTON CMA                                 
         CANHOM 8119,CBXT                                                       
         CANHOM 8119,CFRN                                                       
         CANHOM 8119,CITV                                                       
         CANHOM 8119,CKEM                                                       
         CANHOM 8119,CJEO,MAR_10   EDMONTON (NEW: 2010)                         
         CANHOM 8119,CKES,MAY_11   EDMONTON (NEW: 2011)                         
         CANHOM 9071,CFJC+                                                      
         CANHOM 9071,CHBC+         OKAN-KAM CAREA                               
************************************************************                    
* MATCHING VANCOUVER TO NIELSEN CONVERSION (MARCH 2008)                         
*                                                                               
* METERED!!!                                                                    
************************************************************                    
         CANHOM 9109,CBUT          VANCOUVER                                    
         CANHOM 9109,CHEK                                                       
         CANHOM 9109,CIVI                                                       
         CANHOM 9109,CIVT                                                       
         CANHOM 9109,CTVV                                                       
         CANHOM 9109,CKNO                                                       
         CANHOM 9109,CKVU                                                       
         CANHOM 9109,KCPQ                                                       
         CANHOM 9109,KCTS                                                       
         CANHOM 9109,KING                                                       
         CANHOM 9109,KOMO                                                       
         CANHOM 9109,KSTW                                                       
         CANHOM 9109,KVOS                                                       
         CANHOM 9109,MUL                                                        
         CANHOM 9109,SPNP                                                       
         CANHOM 9109,CHAN                                                       
************************************************************                    
         CANHOM 9119,,                                                          
         CANHOM 9119,KSTW          VICTORIA CMA                                 
         CANHOM 9301,CFTK                                                       
         CANHOM 9331,INTV,MAR_10   PRINCE GEORGE KAMLOOP                        
         CANHOM 9341,CKPZ          TER-KIT C.AREA                               
         CANHOM 9350,CKPG                                                       
         CANHOM 9351,CKPG                                                       
         CANHOM 9363,CJDC          DAWSON CREEK                                 
         CANHOM 9710,BBS                                                        
         CANHOM 9710,CKBI                                                       
         CANHOM 9760,WCAX                                                       
         CANHOM 9760,WPTZ                                                       
         CANHOM 9760,WVNY                                                       
         CANHOM 9762,WUTV                                                       
************************************************************                    
* CALGARY'S METERED DATA, WE ARE ASSIGNING THESE STATIONS                       
* AS HOME TO THE WEEKLY FILES.                                                  
************************************************************                    
         CANHOM 9764,WWNY                                                       
         CANHOM 9765,WHEC                                                       
         CANHOM 9802,CBC           MARITIME                                     
*                                                                               
         DC    X'FFFF'             END OF HOME STATION TABLE                    
         SPACE 2                                                                
         TABLE_LEN TBL_HOMESTA                                                  
         EJECT                                                                  
*                                                                               
****************************************************************                
*                  ACT TABLES                                                   
*                                                                               
* ACTUAL BOOK TABLES                                                            
*                                                                               
*        PROFILE CODE   0                                                       
*        SWEEP CLASS    1          FF=DEFAULT                                   
*        TYPE           2          1=DEFAULT 2=FORCE                            
*        DEFAULT BOOK              FF=MTH BROKEN BY WK                          
*                                  GT. 128=FORCE TO BOOK                        
*        SPECIAL MONTHS 15-38      4 6BYTE FIELDS                               
*                                     0=MONTH                                   
*                                     1=WEEK1                                   
*                                     2=WEEK2                                   
*                                     3=WEEK3                                   
*                                     4=WEEK4                                   
*                                     5=WEEK5                                   
*                                                                               
****************************************************************                
TBL_ACTTABLE DS 0D                                                              
         DC    CL8'*ACTTAB*'                                                    
         DS    XL6                                                              
         DC    AL2(ACTTBLN)        L'TABLE ENTRY                                
*                                                                               
         SPACE 2                                                                
* TABLE 1                                                                       
         ACT_TABLE 1,DEFAULT                                                    
*                                                                               
         CLASS 8,(NOV,NOV,NOV,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 2                                                                       
         ACT_TABLE 2,DEFAULT                                                    
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 3                                                                       
         ACT_TABLE 3,DEFAULT                                                    
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 4                                                                       
         ACT_TABLE 4,FORCE                                                      
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 5                                                                       
         ACT_TABLE 5,FORCE                                                      
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 6                                                                       
         ACT_TABLE 6,FORCE                                                      
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 7                                                                       
         ACT_TABLE 7,FORCE                                                      
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 8                                                                       
         ACT_TABLE 8,DEFAULT                                                    
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY!,MAY!,NOV,NOV,NOV,NOV)            
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 9                                                                       
         ACT_TABLE 9,DEFAULT                                                    
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 10                                                                      
         ACT_TABLE 10,DEFAULT                                                   
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY!,MAY!,MAY!,NOV,NOV,NOV)           
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 11                                                                      
         ACT_TABLE 11,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 12                                                                      
         ACT_TABLE 12,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY!,MAY!,W,NOV,NOV,NOV)              
         WEEKS SEP,(MAY!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 13                                                                      
         ACT_TABLE 13,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 14                                                                      
         ACT_TABLE 14,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 15                                                                      
         ACT_TABLE 15,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,W,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                    
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                    
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                    
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,W,JUL,JUL,W,NOV,NOV,NOV)                 
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 16                                                                      
         ACT_TABLE 16,DEFAULT                                                   
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 17                                                                      
         ACT_TABLE 17,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 18                                                                      
         ACT_TABLE 18,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY!,MAY!,W,NOV,NOV,NOV)              
         WEEKS SEP,(NOV!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(NOV!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 19                                                                      
         ACT_TABLE 19,DEFAULT                                                   
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,MAR,MAR,W,JUL,JUL,W,W,NOV,NOV)                 
         WEEKS JUN,(MAR!,MAR!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         WEEKS OCT,(NOV!,NOV,NOV,NOV!,NOV!)                                     
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 20 BATES BBM                                                            
         ACT_TABLE 20,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAR,NOV-,JUL,JUL,SEP,OCT,NOV,NOV)             
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,JAN,MAR,MAR,MAR,NOV-,JUL,JUL,NOV,NOV,NOV,NOV)             
         CLASS END                                                              
*                                                                               
         CLASS 3,(NOV-,NOV-,MAR,MAR,MAR,NOV-,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(NOV-,NOV-,MAR,MAR,MAR,NOV-,NOV-,NOV-,NOV,NOV,NOV, +        
               NOV)                                                             
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 21                                                                      
         ACT_TABLE 21,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
*              35=NOV OF PREV YEAR 35-24=11                                     
         CLASS 8,(NOV-,NOV-,NOV-,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)           
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 22 (SAME AS 13&14  EXCEPT SEPT) PEPSI 1984                              
         ACT_TABLE 22,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 23                                                                      
         ACT_TABLE 23,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 24                                                                      
         ACT_TABLE 24,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 25                                                                      
         ACT_TABLE 25,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,W)                  
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,W)                  
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,W)                  
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,W)               
         WEEKS SEP,(MAY!,NOV!,NOV!,NOV!,NOV!)                                   
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 26 (CHANGES TO 11/SEPT)                                                 
         ACT_TABLE 26,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 27 ( BBDO SEPT/85 )                                                     
         ACT_TABLE 27,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,W)                
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,W)                
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,W)                
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,W)             
         WEEKS DEC,(NOV!,NOV!,NOV!,NOV!,FEB+!)                                  
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 28 (CANADIAN CPPRS TABLE - CSI)                                         
         ACT_TABLE 28,DEFAULT                                                   
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,MAR,MAR,AUG,AUG,AUG,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 29        (MODIFIED FROM TABLE 15)                                      
         ACT_TABLE 29,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,W,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 30 (FROM TABLE/16 -- FOR BDNY 01/20/86)                                 
         ACT_TABLE 30,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         SPACE 2                                                                
* TABLE 31 (SAME AS 22 EXCEPT APR) JWT 01/24/86                                 
         ACT_TABLE 31,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(MAR!,MAR!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 32 (SAME AS 25 EXCEPT FOR DEC. - BBDO 3/04/86)                          
         ACT_TABLE 32,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 33 (CHANGES TO 11/SEPT)                                                 
         ACT_TABLE 33,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 34 (SAME AS 21 EXCEPT SEPT - MCE 09/02/86)                              
         ACT_TABLE 34,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 35  (FROM 14-SEPT FROM 15)                                              
         ACT_TABLE 35,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 36 (SAME AS 31 EXCEPT SEP) JWT 12/18/86                                 
         ACT_TABLE 36,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(MAR!,MAR!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 37  (FROM 35-APR USES MAY)                                              
         ACT_TABLE 37,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 38  ( USED FOR CPPRS EXTRACTS 1987)                                     
         ACT_TABLE 38,DEFAULT                                                   
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB!,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)            
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 39  ( USED FOR CPPRS EXTRACTS 1988)                                     
         ACT_TABLE 39,DEFAULT                                                   
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 40 (CANADIAN CPPRS TABLE - BBM)                                         
         ACT_TABLE 40,SPECIAL                                                   
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,MAR,MAR,MAR,MAR,MAR,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 41     (MC DONALDS DWLA)                                                
         ACT_TABLE 41,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 42 (FROM TABLE 14 08/16/88) LCNY REQUEST                                
* REVISED 11/11/88 AS PER LCNY                                                  
         ACT_TABLE 42,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,JUL,JUL,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 43  (LOWE/MARSCHALK 1/30/90)                                            
         ACT_TABLE 43,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 8,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV),    +        
               OVERRIDE_TABLE_TYPE=DEFAULT                                      
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 44  CME CHICAGO (6/01/90)                                               
         ACT_TABLE 44,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,W,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,JUL!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 45 (TYCO TOYS - BJDA 11/16/90)                                          
         ACT_TABLE 45,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,MAY,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 46    (SAME AS 24 EXCEPT SEPT. --- PEPSI)                               
         ACT_TABLE 46,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 47 (WESTERN 5/10/91)                                                    
         ACT_TABLE 47,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(OCT!,OCT!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(NOV!,NOV!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 48 (TLDA KFC 7/23/91)                                                   
*    REVISED 9/3/91 PER JUDY MILLER (USE JUL. FOR SEPT.)                        
         ACT_TABLE 48,FORCE                                                     
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 49 (SAME AS 22 EXCEPT SEPT) DDB 9/23/91                                 
         ACT_TABLE 49,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 50 (CANADIAN TABLE - BBM) 2/12/92                                       
         ACT_TABLE 50,FORCE                                                     
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,MAR,MAR,JUL,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 51 (SAME AS TABLE 23 EXCEPT SEPT)                                       
         ACT_TABLE 51,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 52 (FROM 36) JWT 12/10/92                                               
         ACT_TABLE 52,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,W,MAY,MAY,W,W,W,OCT,NOV,NOV)                      
         WEEKS APR,(MAR!,MAR!,MAY!,MAY!,MAY!)                                   
         WEEKS JUL,(MAY!,JUL!,JUL!,JUL!,JUL!)                                   
         WEEKS AUG,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,MAY,W,W,W,OCT,NOV,NOV)                      
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUL,(MAY!,JUL!,JUL!,JUL!,JUL!)                                   
         WEEKS AUG,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,MAY,W,W,W,OCT,NOV,NOV)                      
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUL,(MAY!,JUL!,JUL!,JUL!,JUL!)                                   
         WEEKS AUG,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,MAY,W,W,W,NOV,NOV,NOV)                   
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS JUL,(MAY!,JUL!,JUL!,JUL!,JUL!)                                   
         WEEKS AUG,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 53  SAME AS 24 EXCEPT SEPT ON ALL X'FF' LEVEL - KETCHUM 2/5/93          
         ACT_TABLE 53,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,MAY!,MAY!,MAY!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 54  (FROM TABLE 24) COKE 9/16/93                                        
         ACT_TABLE 54,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 55 WESTERN CANADIAN(FROM TABLE 20)                                      
         ACT_TABLE 55,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAR,NOV-,JUL,JUL,SEP,OCT,NOV,NOV)             
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,JAN,MAR,MAR,MAR,MAR,JUL,JUL,OCT,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(JAN,JAN,MAR,MAR,MAR,MAR,JUL,JUL,NOV,NOV,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,MAR,MAR,MAR,MAR,MAR,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 56     (FROM TABLE 24  FF GROUP CHANGE JWT 11/23/94)                    
         ACT_TABLE 56,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,JUL!,JUL!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 57 (FROM TABLE 36 EXCEPT SC 7) RUBIN/POSTAR 99/02/03                    
         ACT_TABLE 57,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(MAR!,MAR!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,W,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                  
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS APR,(FEB!,FEB!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 58  (BBDO 09/01/00 FROM TABLE 43 AND SC7 FROM TABLE 6)                  
         ACT_TABLE 58,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 8,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV),    +        
               OVERRIDE_TABLE_TYPE=DEFAULT                                      
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 59    (SAME AS 46 EXCEPT DEFAULT )                                      
         ACT_TABLE 59,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 60 SAME AS 23 EXCEPT SEPT                                               
         ACT_TABLE 60,FORCE                                                     
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,JUL!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 61  (FROM TABLE 54) LCI 1/14/03                                         
         ACT_TABLE 61,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 62 - SAME AS 1 EXCEPT FORCE 1 6/9/03                                    
         ACT_TABLE 62,FORCE                                                     
*                                                                               
         CLASS 8,(NOV,NOV,NOV,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,NOV,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 63  (FROM TABLE 54) BUT THIS IS A DEFAULT TABLE                         
         ACT_TABLE 63,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 64  (FROM TABLE 61) LCI 5/25/04                                         
         ACT_TABLE 64,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,W,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,W,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 65  (FROM TABLE 64) LCI 7/23/04                                         
         ACT_TABLE 65,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,W,MAR,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)                
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,W,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)                
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,OCT,NOV,NOV)              
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,JUL,NOV,NOV,NOV)           
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 66  (FROM TABLE 64)                                                     
         ACT_TABLE 66,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,W,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,W,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
* TABLE 67  (FROM TABLE 59) BUT THIS IS 4 WEEK SEPT VERSION                     
         ACT_TABLE 67,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
*                                                                               
* TABLE 68 (SAME AS 49) BUT THIS IS 4 WEEK VERSION                              
         ACT_TABLE 68,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAR,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,FEB,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 69  (FROM TABLE 61) BUT THIS IS DEFAULT VERSION                         
         ACT_TABLE 69,DEFAULT                                                   
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
* TABLE 70  (FROM TABLE 24) SWEEP CLASS 3 IS DIFFERENT                          
         ACT_TABLE 70,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
*                                                                               
* TABLE 71  (FROM TABLE 66) DOES NOT AUTO SWITCH FOR SEPT                       
         ACT_TABLE 71,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,W,MAR,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,W,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
* TABLE 72                                                                      
         ACT_TABLE 72,FORCE                                                     
*                                                                               
         CLASS 2,(JAN,JAN,MAR,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(MAR!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(MAR,MAR,MAR,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(MAR!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(MAR,MAR,MAR,W,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                  
         WEEKS APR,(MAR!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 9,(JAN,JAN,MAR,W,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)                
         WEEKS APR,(MAR!,APR!,APR!,APR!,APR!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,W,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS APR,(MAR!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
* TABLE 73     -COPIED FROM 21                                                  
         ACT_TABLE 73,DEFAULT                                                   
*                                                                               
         CLASS 1,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)                
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,MAY,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                
         WEEKS SEP,(MAY!,MAY!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
*              35=NOV OF PREV YEAR 35-24=11                                     
         CLASS 8,(NOV-,NOV-,NOV-,MAY,MAY,MAY,MAY,MAY,MAY,NOV,NOV,NOV)           
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)             
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
*                                                                               
* TABLE 74                                                                      
         ACT_TABLE 74,FORCE                                                     
*                                                                               
         CLASS 2,(JAN,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS APR,(FEB!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 7,(FEB,FEB,FEB,W,MAY,MAY,MAY,MAY,W,NOV,NOV,NOV)                  
         WEEKS APR,(FEB!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(MAY!,MAY!,MAY!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 9,(JAN,FEB,MAR,W,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)                
         WEEKS APR,(MAR!,APR!,APR!,APR!,APR!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,W,MAY,MAY,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS APR,(FEB!,MAY!,MAY!,MAY!,MAY!)                                   
         WEEKS SEP,(JUL!,JUL!,JUL!,NOV!,NOV!)                                   
         CLASS END                                                              
*                                                                               
* TABLE 75        (copied FROM TABLE 29)                                        
         ACT_TABLE 75,FORCE                                                     
*                                                                               
         CLASS 1,(JAN,FEB,MAR,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 2,(JAN,FEB,FEB,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS 3,(FEB,FEB,FEB,MAY,MAY,W,JUL,JUL,W,OCT,NOV,NOV)                  
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,OCT!,OCT!,OCT!)                                   
         CLASS END                                                              
*                                                                               
         CLASS DFLT,(FEB,FEB,FEB,MAY,MAY,W,JUL,JUL,W,NOV,NOV,NOV)               
         WEEKS JUN,(MAY!,MAY!,JUL!,JUL!,JUL!)                                   
         WEEKS SEP,(JUL!,JUL!,NOV!,NOV!,NOV!)                                   
         CLASS END                                                              
         SPACE 2                                                                
*                                                                               
* TABLE 76                                                                      
         ACT_TABLE 76,FORCE                                                     
*                                                                               
         CLASS DFLT,(MAR,MAR,MAR,JUN,JUN,JUN,SEP,SEP,SEP,DEC,DEC,DEC)           
         CLASS END                                                              
         SPACE 2                                                                
         CLASS END                                                              
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ACTTABLE                                                 
         EJECT                                                                  
*************************************************************                   
* ACTTABW     ANOTHER ACTTAB                                *                   
*************************************************************                   
TBL_ACTTABW DS 0D                                                               
         DC    CL8'*ACTTBW*'                                                    
         DS    XL6                                                              
         DC    AL2(ACTTBLN)        L'TABLE ENTRY                                
                                                                                
         ACT_TABLE 1,DEFAULT                                                    
                                                                                
* WEEKLY TABLE 1 - DEFAULT EXCEPT SEPTEMBER                                     
         CLASS 9,(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,W,OCT,NOV,DEC)                
         WEEKS SEP,(SEP!,SEP!,SEP!,SEP!,OCT!)                                   
         CLASS END                                                              
*                                                                               
* WEEKLY TABLE 1 - DEFAULT EXCEPT SEPTEMBER                                     
         CLASS DFLT,(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,W,OCT,NOV,DEC)             
         WEEKS SEP,(SEP!,SEP!,SEP!,SEP!,OCT!)                                   
         CLASS END                                                              
         SPACE 2                                                                
         ACT_TABLE 2,DEFAULT                                                    
                                                                                
* WEEKLY TABLE 2 - DEFAULT EXCEPT FEB, AUG                                      
         CLASS 9,(JAN,W,MAR,APR,MAY,JUN,JUL,W,SEP,OCT,NOV,DEC)                  
         WEEKS FEB,(JAN!,FEB!,FEB!,FEB!,FEB!)                                   
         WEEKS AUG,(JUL!,AUG!,AUG!,AUG!,AUG!)                                   
         CLASS END                                                              
*                                                                               
* WEEKLY TABLE 2 - DEFAULT EXCEPT AUG, SEP                                      
         CLASS DFLT,(JAN,FEB,MAR,APR,MAY,JUN,JUL,W,W,OCT,NOV,DEC)               
         WEEKS AUG,(JUL!,AUG!,AUG!,AUG!,AUG!)                                   
         WEEKS SEP,(SEP!,SEP!,SEP!,SEP!,OCT!)                                   
         CLASS END                                                              
         SPACE 2                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ACTTABW                                                  
         EJECT                                                                  
*************************************************************                   
* ACTTABC     ANOTHER ACTTAB                                *                   
*************************************************************                   
TBL_ACTTABWC DS 0D                                                              
         DC    CL8'ACTTABWC'                                                    
         DS    XL6                                                              
         DC    AL2(ACTTBLN)        L'TABLE ENTRY                                
*                                                                               
* WEEKLY TABLE 1 - FORCE                                                        
         ACT_TABLE 1,FORCE                                                      
         CLASS DFLT,(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)           
         CLASS END                                                              
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ACTTABWC                                                 
         EJECT                                                                  
*************************************************************                   
* 5WK SEPT ACT TABLE SWITCH TO 4 WK SEPT ACT TABLE          *                   
* THIS TABLE LINKS THE 5 SEPTEMBER BROADCAST WEEKS TABLES   *                   
* W THE CORRESPONDING 4 SEPTEMBER BROADCAST WEEKS TABLES    *                   
*************************************************************                   
TBL_SEPT_5WK DS 0D                                                              
         DC    CL8'SEPT_5WK'                                                    
         DS    XL6                                                              
         DC    AL2(SEP_5WKL)       L'TABLE ENTRY                                
*                                                                               
* 5 WK ACTTAB SWITCH TO 4 WK ACTTAB                                             
*                                                                               
         DC    AL1(22,14)          ACTTAB 22 SWITCH TO ACTTAB 14                
         DC    AL1(23,51)          ACTTAB 23 SWITCH TO ACTTAB 51                
         DC    AL1(31,36)          ACTTAB 31 SWITCH TO ACTTAB 36                
         DC    AL1(46,24)          ACTTAB 46 SWITCH TO ACTTAB 24                
         DC    AL1(49,68)          ACTTAB 22 SWITCH TO ACTTAB 68                
         DC    AL1(59,67)          ACTTAB 59 SWITCH TO ACTTAB 67                
         DC    AL1(61,54)          ACTTAB 61 SWITCH TO ACTTAB 54                
         DC    AL1(66,64)          ACTTAB 66 SWITCH TO ACTTAB 64                
         DC    AL1(69,63)          ACTTAB 69 SWITCH TO ACTTAB 63                
*********DC    AL1(71,70)          ACTTAB 71 SWITCH TO ACTTAB 70                
*                                                                               
         DC    AL1(00)             CHECK FOR 00 FOR EOT                         
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_SEPT_5WK                                                 
         EJECT                                                                  
**********************************************************************          
* STATION CALL LETTER LINK TABLE.  WHEN STATION CALL LETTERS                    
* CHANGE, WE CAN ENTER ONE LEVEL OF CHANGE INTO                                 
* DSTATION RECORDS, BUT WHEN WE HAVE MULTIPLE LEVELS OF CHANGES                 
* WE USED TO HARD CODE THE CHANGES INTO THE SOURCE CODE.                        
* - THIS TABLE WILL OFFER A STRUCTURE WAY OF SUPPORTING                         
*   BACKWARD AND FORWARD CALL LETTER CHANGES                                    
*                                                                               
*            =======TABLE ENTRY LAYOUT ===============                          
*  STATION,START BOOK(FROM_BEGINNING=EARLIEST),END BOOK (TO_END=LATEST)         
*                                                                               
*                                                                               
*   EACH GROUP IN TABLE SHOULD BE BLOCKED WITHIN "============="                
*   FOR READBILITY                                                              
*   FOR EVERY GROUP , THERE MUST BE AN EARLIEST STARTBK=FROM_BEGINNING          
*   AND A LATEST BOOK=TO_END ENTRY                                              
*                                                                               
*   THE 1ST LINE OF A STATION GROUP IN THIS TABLE IS FOR MONTHLY                
*   THE 2ND LINE OF A STATION GROUP IN THIS TABLE IS FOR WEEKLY                 
*   THE 3RD LINE OF A STATION GROUP IN THIS TABLE IS FOR OVERNIGHTS             
*                                                                               
*   SOMETIMES OVERNIGHTS CALL LETTER CHANGES CAN HAPPEN MID WEEK                
*   HENCE- THE END BOOK OF ONE LINK CAN HAVE THE SAME WEEK# AS THE              
*   START WEEK# OF THE NEXT LINK.                                               
*                                                                               
**********************************************************************          
TBL_CALL_LNK DS 0D                                                              
         DC    CL8'CALL_LNK'                                                    
         DS    XL6                                                              
         DC    AL2(CAL_LNKL)       L'TABLE ENTRY                                
*                                                                               
*==================================================                             
*&&DO                                                                           
         DC    CL4'KHTV',AL2(FROM_BEGINNING),AL2(SEP_99)                        
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*&&                                                                             
*                                                                               
         DC    CL4'KHWB',AL2(FROM_BEGINNING),AL2(APR_06)                        
         DC    AL1(YR_1999,WEEK_39),AL1(YR_2006,WEEK_16)                        
         DC    AL1(YR_1999,WEEK_39),AL1(YR_2006,WEEK_18)                        
*                                                                               
         DC    CL4'KHCW',AL2(MAY_06),AL2(JUN_08)                                
         DC    AL1(YR_2006,WEEK_17),AL1(YR_2008,WEEK_28)                        
         DC    AL1(YR_2006,WEEK_18),AL1(YR_2008,WEEK_29)                        
*                                                                               
         DC    CL4'KIAH',AL2(JUL_08),AL2(TO_END)                                
         DC    AL1(YR_2008,WEEK_29),AL2(TO_END)                                 
         DC    AL1(YR_2008,WEEK_29),AL2(TO_END)                                 
*                                                                               
*==================================================                             
         DC    CL4'GGEM',AL2(FROM_BEGINNING),AL2(FEB_80)                        
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'CGEM',AL2(MAR_80),AL2(OCT_09)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'HGEM',AL2(NOV_09),AL2(JAN_10)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'GGEM',AL2(FEB_10),AL2(TO_END)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
*==================================================                             
         DC    CL4'KSFE',AL2(FROM_BEGINNING),AL2(NOV_11)                        
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'KFTN',AL2(DEC_11),AL2(APR_12)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'KCWT',AL2(MAY_12),AL2(TO_END)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*==================================================                             
         DC    CL4'TSOU',AL2(FROM_BEGINNING),AL2(SEP_06) TILL SEP/06            
         DC    AL1(YR_1999,WEEK_01),AL1(YR_2012,WEEK_02)                        
         DC    AL1(YR_1999,WEEK_01),AL1(YR_2012,WEEK_02)                        
*                                                                               
         DC    CL4'SPSO',AL2(OCT_06),AL2(SEP_15)         OCT06-SEP15            
         DC    AL1(YR_2012,WEEK_03),AL1(YR_2015,WEEK_39)                        
         DC    AL1(YR_2012,WEEK_03),AL1(YR_2015,WEEK_39)                        
*                                                                               
         DC    CL4'FSSE',AL2(OCT_15),AL2(TO_END)         OCT15-END              
         DC    AL1(YR_2015,WEEK_40),AL2(TO_END)                                 
         DC    AL1(YR_2015,WEEK_40),AL2(TO_END)                                 
*                                                                               
*==================================================                             
         DC    CL4'KYMA',AL2(FROM_BEGINNING),AL2(DEC_00) TILL SEP/06            
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
         DC    CL4'KSWT',AL2(JAN_01),AL2(DEC_19)         OCT06-SEP15            
         DC    AL1(YR_2012,WEEK_03),AL1(YR_2015,WEEK_39)                        
         DC    AL1(YR_2012,WEEK_03),AL1(YR_2015,WEEK_39)                        
*                                                                               
         DC    CL4'KYMA',AL2(JAN_20),AL2(TO_END)                                
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
         DC    AL2(NO_ENTRY),AL2(NO_ENTRY)                                      
*                                                                               
*==================================================                             
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_CALL_LNK                                                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE OF NET CABLE CALL LETTERS.                                    *         
* THIS TABLE INCLUDES DUPLICATES BY NUMERIC CODE:IF A NETWORK CHANGED *         
* CALL LETTERS AT SOME POINT IN TIME, ALL CALL LETTERS FOR A GIVEN    *         
* NUMERIC CODE SHOULD BE KEPT IN THIS TABLE. THE MOST RECENT CALL     *         
* LETTERS SHOULD COME FIRST.                                          *         
***********************************************************************         
TBL_NECABCLL DS 0D                                                              
         DC    CL8'NECABCLL'                                                    
         DS    XL6                                                              
         DC    AL2(NECBCLQ)        L'TABLE ENTRY                                
*                                                                               
** OLD CABLE CALL LETTERS                                                       
         CABCLL WTBS,5830                                                       
         CABCLL TBS,5830                                                        
         CABCLL USA,6217                                                        
         CABCLL CMD,7133                                                        
         CABCLL CMT,6647                                                        
         CABCLL DSC,6530                                                        
         CABCLL CART,7241                                                       
** NON-PAY                                                                      
         CABCLL AEN,6218                                                        
         CABCLL ATVP,6558                                                       
         CABCLL AMC,6593                                                        
         CABCLL APL,6605                                                        
         CABCLL ASN,6493                                                        
         CABCLL ADSM,4737                                                       
         CABCLL BABY,10214                                                      
         CABCLL BAND,1036                                                       
         CABCLL BEIE,10165                                                      
         CABCLL BEIN,10164                                                      
         CABCLL BET,6200                                                        
         CABCLL BRVO,6526                                                       
         CABCLL BTN,2726                                                        
         CABCLL BBCA,8250                                                       
         CABCLL BOOM,9304                                                       
         CABCLL FYI,8338                                                        
         CABCLL BIO,8338                                                        
         CABCLL CNNE,7371                                                       
         CABCLL CINL,8340                                                       
         CABCLL CSPN,6203                                                       
         CABCLL SPAN,6220                                                       
         CABCLL CNN,6202                                                        
         CABCLL CTN,6529                                                        
         CABCLL CALS,7247                                                       
         CABCLL CBC,6533                                                        
         CABCLL CANA,6524                                                       
         CABCLL CHIL,2099                                                       
         CABCLL CHLA,7224                                                       
         CABCLL CLTV,7255                                                       
         CABCLL CNBC,6521                                                       
         CABCLL CMDY,7133                                                       
         CABCLL CMAC,6645                                                       
         CABCLL AJAM,7981                                                       
         CABCLL CRNT,7981                                                       
         CABCLL CMTV,6647                                                       
         CABCLL TRU,7183                                                        
         CABCLL CRT,7183                                                        
         CABCLL BHER,6577                                                       
         CABCLL CTRC,6577                                                       
         CABCLL CV,7130                                                         
         CABCLL MT,4133                                                         
         CABCLL VEL,4133                                                        
         CABCLL HDT,4133                                                        
         CABCLL DFAM,2365                                                       
         CABCLL DHD,4133                                                        
         CABCLL OWN,8255                                                        
         CABCLL DHLT,8255                                                       
         CABCLL DISC,6530                                                       
         CABCLL ID,8252                                                         
         CABCLL DTMS,8252                                                       
         CABCLL IFC,6111                                                        
         CABCLL SCI,8253                                                        
         CABCLL DSCI,8253                                                       
         CABCLL DFC,8254                                                        
         CABCLL HUB,8254                                                        
         CABCLL DKID,8254                                                       
         CABCLL DIY,7742                                                        
         CABCLL DSCE,8342                                                       
         CABCLL DSJR,2991                                                       
         CABCLL ENT,6384                                                        
         CABCLL EDAC,6659                                                       
         CABCLL EPJR,6538                                                       
         CABCLL EPG,6660                                                        
         CABCLL EMP,6900                                                        
         CABCLL ENC,7167                                                        
         CABCLL ESPN,6204                                                       
         CABCLL ESPB,6537                                                       
         CABCLL ESPD,4964                                                       
         CABCLL ESPU,8889                                                       
         CABCLL ESP2,7287                                                       
         CABCLL ESX,7287                                                        
         CABCLL ESPC,7002                                                       
         CABCLL ESCL,7002                                                       
         CABCLL ENN,7474                                                        
         CABCLL EWN,6655                                                        
         CABCLL ENCY,9130                                                       
         CABCLL TFN,6622                                                        
         CABCLL FBN,2263                                                        
         CABCLL CC,9876                                                         
         CABCLL FINE,9876                                                       
         CABCLL FLIX,7248                                                       
         CABCLL FOOD,7304                                                       
         CABCLL FOXD,7300                                                       
         CABCLL FS2,9042                                                        
         CABCLL FUEL,9042                                                       
         CABCLL FX,7328                                                         
         CABCLL FXM,9895                                                        
         CABCLL FXNC,7401                                                       
         CABCLL FXX,10041                                                       
         CABCLL FSOC,8445                                                       
         CABCLL FSC,8445                                                        
         CABCLL GAC,7651                                                        
         CABCLL GALA,6517                                                       
         CABCLL GSN,7099                                                        
         CABCLL GAME,7099                                                       
         CABCLL GENS,6566                                                       
         CABCLL UP,4690                                                         
         CABCLL GMC,4690                                                        
         CABCLL GVAC,6665                                                       
         CABCLL GARD,6664                                                       
         CABCLL GOLF,6560                                                       
         CABCLL DAM,8251                                                        
         CABCLL GRN,8251                                                        
         CABCLL HGTV,8920                                                       
         CABCLL H2,8339                                                         
*        CABCLL HI,8339                                                         
         CABCLL HITS,6667                                                       
         CABCLL HITV,6494                                                       
         CABCLL HMM,2315                                                        
         CABCLL HMC,2315                                                        
         CABCLL HLMC,2315                                                       
         CABCLL HLN,6199                                                        
         CABCLL HLTH,6666                                                       
         CABCLL HMTM,6572                                                       
         CABCLL HSE,6592                                                        
         CABCLL HSN1,6534                                                       
         CABCLL HSN2,6695                                                       
         CABCLL ICN,6549                                                        
         CABCLL INTL,7165                                                       
         CABCLL ITN,6197                                                        
         CABCLL KBLN,6211                                                       
         CABCLL KBL,7163                                                        
         CABCLL KCBL,7098                                                       
         CABCLL KFB,7271                                                        
         CABCLL LSAC,6672                                                       
         CABCLL LIF,6196                                                        
         CABCLL LMN,8336                                                        
         CABCLL LMT,6381                                                        
         CABCLL LOC,6589                                                        
         CABCLL LOGO,4443                                                       
         CABCLL LOV,6590                                                        
         CABCLL SWSH,6612                                                       
         CABCLL MSG,6207                                                        
         CABCLL MNPR,6618                                                       
         CABCLL RCNG,6634                                                       
         CABCLL MTRO,6674                                                       
         CABCLL MSC,6213                                                        
         CABCLL MLAC,6617                                                       
         CABCLL CLOO,6479                                                       
         CABCLL SLTH,6479                                                       
         CABCLL MEU,6479                                                        
         CABCLL AHC,8256                                                        
         CABCLL MIL,8256                                                        
         CABCLL MLBN,7564                                                       
         CABCLL MSG2,7187                                                       
         CABCLL MSNB,7801                                                       
         CABCLL MNBC,7801                                                       
         CABCLL MUCH,7652                                                       
         CABCLL FUSE,7652                                                       
         CABCLL FM,6662                                                         
         CABCLL MTV,6198                                                        
         CABCLL AMTV,12688                                                      
         CABCLL HLDR,12133                                                      
         CABCLL AXS,4929                                                        
         CABCLL FETV,10388                                                      
         CABCLL NWSY,12724                                                      
         CABCLL MTV2,8466                                                       
         CABCLL MUN2,7310                                                       
         CABCLL NJT,6623                                                        
         CABCLL NETW,6569                                                       
         CABCLL NFLN,9540                                                       
         CABCLL NGC,9258                                                        
         CABCLL NGWD,9874                                                       
         CABCLL NECN,7220                                                       
         CABCLL NESN,6574                                                       
         CABCLL INSP,6632                                                       
         CABCLL NWS,6625                                                        
         CABCLL RAI,6480                                                        
         CABCLL NLI,6377                                                        
         CABCLL NCH,7197                                                        
         CABCLL NICK,6212                                                       
         CABCLL NAN,9043                                                        
         CABCLL NKTN,9963                                                       
         CABCLL NTN,6532                                                        
         CABCLL ENA,7182                                                        
         CABCLL TNNK,8331                                                       
         CABCLL THEN,8331                                                       
         CABCLL NOGG,8331                                                       
         CABCLL NKJR,2181                                                       
         CABCLL NOGN,2181                                                       
         CABCLL NBAT,9772                                                       
         CABCLL FMTV,4709                                                       
         CABCLL NUVO,4709                                                       
         CABCLL NY1,7238                                                        
         CABCLL ZNY1,7238                                                       
         CABCLL HALL,6485                                                       
         CABCLL ODSY,6485                                                       
         CABCLL NBCS,6275                                                       
         CABCLL VS,6275                                                         
         CABCLL OLN,6275                                                        
         CABCLL OTB,6628                                                        
         CABCLL OVTN,7653                                                       
         CABCLL OXYG,8444                                                       
         CABCLL PENN,6573                                                       
         CABCLL PPR,6547                                                        
         CABCLL TV1,8871                                                        
         CABCLL POP,6477                                                        
         CABCLL TVGN,6477                                                       
         CABCLL TVGC,6477                                                       
         CABCLL PREV,6477                                                       
         CABCLL PRST,1233                                                       
         CABCLL PSNW,6575                                                       
         CABCLL PSU,6901                                                        
         CABCLL PSMW,7052                                                       
         CABCLL PSRM,7050                                                       
         CABCLL PSUM,7051                                                       
         CABCLL PMTK,6527                                                       
         CABCLL PRSM,6515                                                       
         CABCLL PASS,6675                                                       
         CABCLL PGGD,6630                                                       
         CABCLL PMCH,6657                                                       
         CABCLL PBAC,6629                                                       
         CABCLL QVC,6951                                                        
         CABCLL QVCF,7196                                                       
         CABCLL RLES,6602                                                       
         CABCLL REAL,4004                                                       
         CABCLL RELG,6599                                                       
         CABCLL REY,10123                                                       
         CABCLL RLZC,2680                                                       
         CABCLL RFD,9781                                                        
         CABCLL RI2,6690                                                        
         CABCLL RI1,6689                                                        
         CABCLL SLA,6597                                                        
         CABCLL SOAP,8443                                                       
         CABCLL SPC,6698                                                        
         CABCLL SYFY,7235                                                       
         CABCLL SCIF,7235                                                       
         CABCLL SRCN,6610                                                       
         CABCLL SCGO,6637                                                       
         CABCLL SKPV,7225                                                       
         CABCLL SCO,6661                                                        
         CABCLL SCP,6389                                                        
         CABCLL SCB,7033                                                        
         CABCLL SCC,6609                                                        
         CABCLL SCA,6488                                                        
         CABCLL SMTH,6626                                                       
         CABCLL SPH,7034                                                        
         CABCLL SPMN,8345                                                       
         CABCLL UKID,4001                                                       
         CABCLL SPRT,4001                                                       
         CABCLL SPTR,6587                                                       
         CABCLL SPSN,7067                                                       
         CABCLL SPTS,6608                                                       
         CABCLL SCPC,7222                                                       
         CABCLL SCCI,7036                                                       
         CABCLL SCNE,6668                                                       
         CABCLL SCF,6535                                                        
         CABCLL SCNY,6607                                                       
         CABCLL SUN,6651                                                        
         CABCLL FS1,7443                                                        
         CABCLL SPD,7443                                                        
         CABCLL SV,7443                                                         
         CABCLL SYS,6564                                                        
         CABCLL STZ,7306                                                        
         CABCLL STPZ,9157                                                       
         CABCLL ESQ,8151                                                        
         CABCLL STYL,8151                                                       
         CABCLL SUND,7602                                                       
         CABCLL TPCH,6613                                                       
         CABCLL TBSN,7237                                                       
         CABCLL TBSC,7237                                                       
         CABCLL DXD,7611                                                        
         CABCLL TDSN,7611                                                       
         CABCLL TELB,6615                                                       
         CABCLL TMND,6536                                                       
         CABCLL TMD,6536                                                        
         CABCLL TEST,6616                                                       
         CABCLL TOON,7241                                                       
         CABCLL TECH,8333                                                       
         CABCLL G4,8333                                                         
         CABCLL TSOU,8533                                                       
         CABCLL FRFM,6201                                                       
         CABCLL FAM,6201                                                        
         CABCLL TLC,6635                                                        
         CABCLL PAR,6221                                                        
         CABCLL SPIK,6221                                                       
         CABCLL TNN,6221                                                        
         CABCLL SPK,6221                                                        
         CABCLL SLNT,6565                                                       
         CABCLL TRAV,6678                                                       
         CABCLL TRVL,6678                                                       
         CABCLL TR3S,7296                                                       
         CABCLL TWC,6523                                                        
         CABCLL TM,6649                                                         
         CABCLL TMSP,6641                                                       
         CABCLL TVV,7068                                                        
         CABCLL TBN,6614                                                        
         CABCLL TNT,6390                                                        
         CABCLL TVL,7838                                                        
         CABCLL TOC,6350                                                        
         CABCLL DLIF,7180                                                       
         CABCLL DFH,7180                                                        
         CABCLL THN,7180                                                        
         CABCLL TUDN,8518                                                       
         CABCLL UNIV,6209                                                       
         CABCLL UNV,6209                                                        
         CABCLL USAB,6650                                                       
         CABCLL USAN,6217                                                       
         CABCLL VH1,6546                                                        
         CABCLL MTVC,8512                                                       
         CABCLL JUKE,6091                                                       
         CABCLL WAPA,7196                                                       
         CABCLL WCWX,6214                                                       
         CABCLL WETV,7516                                                       
         CABCLL WE,7516                                                         
         CABCLL WX,6654                                                         
         CABCLL WGRC,5599                                                       
         CABCLL XPRE,6267                                                       
         CABCLL XHIS,8908                                                       
         CABCLL NWSN,6788                                                       
         CABCLL WGNA,6788                                                       
         CABCLL WGNC,6788                                                       
         CABCLL XWGN,6788                                                       
** PAY                                                                          
         CABCLL CMAX,6513                                                       
         CABCLL MAXP,9118                                                       
         CABCLL HBO,6510                                                        
         CABCLL HBOM,9100                                                       
         CABCLL ITV,6387                                                        
         CABCLL JADE,6594                                                       
         CABCLL NGN,6582                                                        
         CABCLL PLBY,6525                                                       
         CABCLL RPT,6047                                                        
         CABCLL SHOW,6511                                                       
         CABCLL SHO1,9521                                                       
         CABCLL SPCE,7058                                                       
         CABCLL DSNY,6522                                                       
         CABCLL TMC,6514                                                        
         CABCLL TMC1,9527                                                       
** PAY PER VIEW                                                                 
         CABCLL CVS,7065                                                        
         CABCLL VCH3,7256                                                       
         CABCLL VCH2,6570                                                       
         CABCLL PPV,6598                                                        
         CABCLL REQ1,6604                                                       
         CABCLL REQ2,6518                                                       
         CABCLL VCH1,6595                                                       
*                                                                               
         CABCLL TVLC,11359                                                      
         CABCLL TENN,9897                                                       
         CABCLL ZLIV,11233                                                      
         CABCLL CDTV,11933                                                      
         CABCLL ESNL,11973                                                      
         CABCLL JCTV,11928                                                      
*                                                                               
         CABCLL NMX,10680          SPEC-47677                                   
         CABCLL HHG,13960          SPEC-49550                                   
         CABCLL COWB,12162         SPEC-52562                                   
         CABCLL OLYM,11834         SPEC-52730                                   
*                                                                               
         DC    X'FF'               EOT                                          
         SPACE 2                                                                
         TABLE_LEN TBL_NECABCLL                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF NET CABLE NETWORK NAMES.                                   *         
* THIS TABLE DOESN'T HAVE DUPLICATES. IF A NETWORK CHANGED CALL       *         
* LETTERS OR ITS NAME, ONLY THE LATEST CALL LETTERS/NAME SHOULD BE    *         
* INCLUDED FOR A GIVEN NUMERIC CODE.                                  *         
* TABLE IS SORTED IN ALPHABETICAL ORDER BY NETWORK NAME WITH SOME     *         
* LOGICAL EXCEPTIONS.                                                 *         
* THEREFORE, WE CANNOT SORT THIS TABLE, EVEN THOUGH THE KEYS ARE      *         
* UNIQUE.                                                             *         
***********************************************************************         
TBL_NECABNAM DS 0D                                                              
         DC    CL8'NECABNAM'                                                    
         DS    XL6                                                              
         DC    AL2(NECBNMLQ)       L'TABLE ENTRY                                
*                                                                               
         CABNAM AEN,6218,'A&&E Network'                                         
         CABNAM FRFM,6201,'Freeform'                                            
         CABNAM ADSM,4737,'Adult Swim'                                          
         CABNAM AHC,8256,'American Heroes Ch'                                   
         CABNAM AMC,6593,'American Movie Classics'                              
         CABNAM APL,6605,'Animal Planet'                                        
         CABNAM BABY,10214,'BabyfirstTV'                                        
         CABNAM BAND,1036,'Bandamax'                                            
         CABNAM BEIE,10165,'Beie Sport Espanol'                                 
         CABNAM BEIN,10164,'beIN SPORT'                                         
         CABNAM BBCA,8250,'Bbc-America'                                         
         CABNAM BOOM,9304,'Boomerang'                                           
         CABNAM FYI,8338,'Fyi'                                                  
         CABNAM BET,6200,'Black Entertainment Tv'                               
         CABNAM BRVO,6526,'Bravo'                                               
         CABNAM BTN,2726,'Big Ten Network'                                      
         CABNAM CDTV,11933,'Comedy. Tv'                                         
         CABNAM CINL,8340,'Cine Latino'                                         
         CABNAM CNN,6202,'CNN'                                                  
         CABNAM CNNE,7371,'CNN En Espanol'                                      
         CABNAM BHER,6577,'BET Her'                                             
         CABNAM CHIL,2099,'Chiller'                                             
         CABNAM CMAX,6513,'Cinemax'                                             
         CABNAM MAXP,9118,'Max Prime'                                           
         CABNAM CMT,6647,'Cmt'                                                  
         CABNAM CNBC,6521,'Cnbc'                                                
         CABNAM CMD,7133,'Comedy Central'                                       
         CABNAM AJAM,7981,'Al Jazeera America'                                  
         CABNAM DFAM,2365,'Discovery Familia'                                   
         CABNAM DLIF,7180,'Discovery Life Channel'                              
         CABNAM DSCE,8342,'Discovery En Espanol'                                
         CABNAM DSNY,6522,'Disney Channel'                                      
         CABNAM DSJR,2991,'Disney Jr'                                           
         CABNAM DXD,7611,'Disney XD'                                            
         CABNAM DIY,7742,'DIY Network'                                          
         CABNAM ENT,6384,'E!'                                                   
         CABNAM ENC,7167,'Encore'                                               
         CABNAM ENCY,9130,'Starz Encore Primary'                                
         CABNAM ENN,7474,'Esp News'                                             
         CABNAM ESNL,11973,'ESN Lifestyles'                                     
         CABNAM ESPD,4964,'Espn Deportes'                                       
         CABNAM ESPN,6204,'Espn'                                                
         CABNAM ESPC,7002,'Espn Classic'                                        
         CABNAM ESPU,8889,'Espnu'                                               
         CABNAM ESP2,7287,'Espn2'                                               
         CABNAM CC,9876,'Cooking Channel'                                       
         CABNAM FLIX,7248,'Flix'                                                
         CABNAM FMTV,4709,'FM'                                                  
         CABNAM FOOD,7304,'Food Network'                                        
         CABNAM FBN,2263,'Fox Business Network'                                 
         CABNAM FOXD,7300,'Fox Deportes'                                        
         CABNAM FXNC,7401,'Fox News Channel'                                    
         CABNAM FXX,10041,'FXX'                                                 
         CABNAM REAL,4004,'Fox Reality'                                         
         CABNAM FSOC,8445,'Fox Soccer'                                          
         CABNAM FS1,7443,'Fox Sports 1'                                         
         CABNAM FS2,9042,'Fox Sports 2'                                         
         CABNAM FUSE,7652,'Fuse'                                                
         CABNAM FX,7328,'Fx'                                                    
         CABNAM FXM,9895,'Fx Movie Channel'                                     
         CABNAM G4,8333,'G4'                                                    
         CABNAM GOLF,6560,'Golf Channel'                                        
         CABNAM UP,4690,'UP'                                                    
         CABNAM GAC,7651,'Great American Country'                               
         CABNAM GALA,6517,'Galavision'                                          
         CABNAM GSN,7099,'Gsn'                                                  
         CABNAM HALL,6485,'Hallmark'                                            
         CABNAM HMM,2315,'Hallmark Movie - Mysteries'                           
         CABNAM MT,4133,'Motor Trend'                                           
         CABNAM VEL,4133,'Velocity'                                             
         CABNAM HLN,6199,'Hln'                                                  
         CABNAM XHIS,8908,'History'                                             
         CABNAM HGTV,8920,'Home And Garden Tv'                                  
         CABNAM HBO,6510,'Home Box Office'                                      
         CABNAM HBOM,9100,'Hbo Prime'                                           
         CABNAM H2,8339,'H2'                                                    
         CABNAM IFC,6111,'IFC TV'                                               
         CABNAM INSP,6632,'Insp'                                                
         CABNAM ID,8252,'Investigation Discovery'                               
         CABNAM JCTV,11928,'Justice Central'                                    
         CABNAM LMN,8336,'Lifetime Movie Network'                               
         CABNAM LIF,6196,'Lifetime Television'                                  
         CABNAM LOGO,4443,'Logo'                                                
         CABNAM MLBN,7564,'Mlb Network'                                         
         CABNAM MSNB,7801,'Msnbc'                                               
         CABNAM MTV,6198,'Mtv'                                                  
         CABNAM AMTV,12688,'AMtv'                                               
         CABNAM HLDR,12133,'Hallmark Drama'                                     
         CABNAM AXS,4929,'AXS TV'                                               
         CABNAM FETV,10388,'FETV'                                               
         CABNAM NWSY,12724,'Newsy'                                              
         CABNAM MTV2,8466,'Mtv2'                                                
         CABNAM MUN2,7310,'Mun2'                                                
         CABNAM NGC,9258,'National Geographic'                                  
         CABNAM NGWD,9874,'Nat Geo Wild'                                        
         CABNAM NBAT,9772,'Nba Tv'                                              
         CABNAM NFLN,9540,'Nfl Network'                                         
         CABNAM NKJR,2181,'Nick Jr'                                             
         CABNAM NAN,9043,'Nick-At-Nite'                                         
         CABNAM NICK,6212,'Nickelodeon'                                         
         CABNAM NKTN,9963,'Nicktoons'                                           
         CABNAM OWN,8255,'Oprah Winfrey Network'                                
         CABNAM OVTN,7653,'Ovation Network'                                     
         CABNAM OXYG,8444,'Oxygen Media'                                        
         CABNAM DAM,8251,'Destination America'                                  
         CABNAM PLBY,6525,'Playboy'                                             
         CABNAM QVC,6951,'Qvc'                                                  
         CABNAM REY,10123,'El Rey'                                              
         CABNAM RLZC,2680,'ReelzChannel'                                        
         CABNAM RFD,9781,'Rfd-Tv'                                               
         CABNAM SHOW,6511,'Showtime'                                            
         CABNAM SHO1,9521,'Showtime Prime'                                      
         CABNAM CLOO,6479,'Cloo'                                                
         CABNAM SOAP,8443,'Soapnet'                                             
         CABNAM PAR,6221,'Paramount'                                            
         CABNAM PRST,1233,'Pursuit Channel'                                     
         CABNAM SPMN,8345,'Sportsman Channel'                                   
         CABNAM UKID,4001,'Universal Kids'                                      
         CABNAM STZ,7306,'Starz       '                                         
         CABNAM STPZ,9157,'Starz Primary'                                       
         CABNAM SUND,7602,'Sundance Tv'                                         
         CABNAM SMTH,6626,'Smithsonian'                                         
         CABNAM ESQ,8151,'Esquire Network'                                      
         CABNAM SYFY,7235,'Syfy'                                                
         CABNAM TBS,5830,'Tbs-Superstation'                                     
         CABNAM TBSN,7237,'Tbs Network'                                         
         CABNAM TNNK,8331,'TeenNick'                                            
         CABNAM TOON,7241,'The Cartoon Network'                                 
         CABNAM TRU,7183,'TruTv'                                                
         CABNAM DISC,6530,'The Discovery Channel'                               
         CABNAM DFC,8254,'Discovery Family Channel'                             
         CABNAM TLC,6635,'The Learning Channel'                                 
         CABNAM TMC,6514,'The Movie Channel'                                    
         CABNAM TMC1,9527,'The Movie Channel'                                   
         CABNAM TOC,6350,'The Outdoor Channel'                                  
         CABNAM SCI,8253,'Science'                                              
         CABNAM TENN,9897,'The Tennis Channel'                                  
         CABNAM TRAV,6678,'The Travel Channel'                                  
         CABNAM TWC,6523,'The Weather Channel'                                  
         CABNAM TR3S,7296,'Tr3s'                                                
         CABNAM TNT,6390,'Turner Network Television'                            
         CABNAM TSOU,8533,'Turner South'                                        
         CABNAM POP,6477,'Pop'                                                  
         CABNAM TVL,7838,'Tv Land'                                              
         CABNAM TV1,8871,'Tv One'                                               
         CABNAM TUDN,8518,'TUDN'                                                
         CABNAM USA,6217,'Usa Network'                                          
         CABNAM NBCS,6275,'Nbc Sports Network'                                  
         CABNAM VH1,6546,'Vh1'                                                  
         CABNAM MTVC,8512,'MTV Classic'                                         
         CABNAM WAPA,7196,'Wapa America'                                        
         CABNAM NWSN,6788,'NewsNation'                                          
         CABNAM WETV,7516,'WE tv'                                               
         CABNAM TVLC,11359,'TV Land Classic'                                    
         CABNAM ZLIV,11233,'Z Living'                                           
         CABNAM NMX,10680,'Newsmax TV'    SPEC-47677                            
         CABNAM HHG,13960,'Hogar de HGTV' SPEC-49550                            
         CABNAM COWB,12162,'The Cowboy Channel'  SPEC-52562                     
         CABNAM OLYM,11834,'Olympic Channel'     SPEC-52730                     
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NECABNAM                                                 
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TABLE OF NET HISPANIC CABLE CALL LETTERS.                           *         
* THIS TABLE INCLUDES DUPLICATES BY NUMERIC CODE:IF A NETWORK CHANGED *         
* CALL LETTERS AT SOME POINT IN TIME, ALL CALL LETTERS FOR A GIVEN    *         
* NUMERIC CODE SHOULD BE KEPT IN THIS TABLE. THE MOST RECENT CALL     *         
* LETTERS SHOULD COME FIRST.                                          *         
***********************************************************************         
TBL_NEHCBCLL DS 0D                                                              
         DC    CL8'NEHCBCLL'                                                    
         DS    XL6                                                              
         DC    AL2(NEHCBCLQ)       L'TABLE ENTRY                                
*                                                                               
         HCBCLL HGAL,6517,PGMDATA=YES                                           
         HCBCLL HFXD,7300,PGMDATA=YES                                           
         HCBCLL HFXS,7300,PGMDATA=YES                                           
         HCBCLL HMU2,7310,PGMDATA=YES                                           
         HCBCLL HGOL,9905,PGMDATA=YES                                           
         HCBCLL HDFA,2365,PGMDATA=YES                                           
         HCBCLL HDSE,8342,PGMDATA=YES                                           
         HCBCLL HESD,4964,PGMDATA=YES                                           
         HCBCLL HTR3,7296,PGMDATA=YES                                           
         HCBCLL HFXL,7339,PGMDATA=YES                                           
         HCBCLL HUTL,7339,PGMDATA=YES                                           
         HCBCLL HNGM,1066,PGMDATA=YES                                           
         HCBCLL HBSE,10165,PGMDATA=YES                                          
         HCBCLL HCNL,8340,PGMDATA=YES                                           
         HCBCLL HWPA,7196,PGMDATA=YES                                           
         HCBCLL HTBS,5830                                                       
         HCBCLL HLIF,6196                                                       
         HCBCLL HMTV,6198                                                       
         HCBCLL HHLN,6199                                                       
         HCBCLL HBET,6200                                                       
         HCBCLL HFRF,6201                                                       
         HCBCLL HFAM,6201                                                       
         HCBCLL HCNN,6202                                                       
         HCBCLL HESP,6204                                                       
         HCBCLL HNIC,6212                                                       
         HCBCLL HUSA,6217                                                       
         HCBCLL HAEN,6218                                                       
         HCBCLL HENT,6384                                                       
         HCBCLL HTNT,6390                                                       
         HCBCLL HPRE,6477                                                       
         HCBCLL HDSN,6522                                                       
         HCBCLL HVH1,6546                                                       
         HCBCLL HTLC,6635                                                       
         HCBCLL HWGN,6788                                                       
         HCBCLL HTNN,6221                                                       
         HCBCLL HDIS,6530                                                       
         HCBCLL HCMD,7133                                                       
         HCBCLL HSCI,7235                                                       
         HCBCLL HTOO,7241                                                       
         HCBCLL HES2,7287                                                       
         HCBCLL HFX,7328                                                        
         HCBCLL HXHI,8908                                                       
         HCBCLL COWB,12162                                                      
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHCBCLL                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF HISPANIC CABLE NETWORK NAMES.                              *         
* THIS TABLE DOESN'T HAVE DUPLICATES. IF A NETWORK CHANGED CALL       *         
* LETTERS OR ITS NAME, ONLY THE LATEST CALL LETTERS/NAME SHOULD BE    *         
* INCLUDED FOR A GIVEN NUMERIC CODE.                                  *         
* TABLE IS SORTED IN ALPHABETICAL ORDER BY NETWORK NAME WITH SOME     *         
* LOGICAL EXCEPTIONS.                                                 *         
***********************************************************************         
TBL_NEHCBNAM DS 0D                                                              
         DC    CL8'NEHCBNAM'                                                    
         DS    XL6                                                              
         DC    AL2(NEHCBNLQ)       L'TABLE ENTRY                                
*                                                                               
         HCBNAM HAEN,6218,'Arts And Entertainment'                              
         HCBNAM HBET,6200,'Black Entertainment Network'                         
         HCBNAM HCMD,7133,'Comedy Central'                                      
         HCBNAM HMU2,7310,'Mun2'                                                
         HCBNAM HCNN,6202,'CNN'                                                 
         HCBNAM HDIS,6530,'Discovery Channel'                                   
         HCBNAM HDFA,2365,'Discovery Familia'                                   
         HCBNAM HDSE,8342,'Discovery en Espanol'                                
         HCBNAM HDSN,6522,'Disney '                                             
         HCBNAM HENT,6384,'E! Entertainment Television'                         
         HCBNAM HESP,6204,'Espn'                                                
         HCBNAM HESD,4964,'Espn Deportes'                                       
         HCBNAM HES2,7287,'Espn2'                                               
         HCBNAM HFRF,6201,'Freeform'                                            
         HCBNAM HFX,7328,'Fx Networks'                                          
         HCBNAM HFXD,7300,'Fox Deportes'                                        
         HCBNAM HFXL,7339,'FOX Life'                                            
         HCBNAM HGAL,6517,'Galavision'                                          
         HCBNAM HGOL,9905,'Gol Tv'                                              
         HCBNAM HHLN,6199,'Headline News'                                       
         HCBNAM HLIF,6196,'Lifetime'                                            
         HCBNAM HMTV,6198,'Mtv'                                                 
         HCBNAM HNGM,1066,'Nat Geo Mundo'                                       
         HCBNAM HBSE,10165,'Bein Sport Espanol'                                 
         HCBNAM HNIC,6212,'Nickelodeon'                                         
         HCBNAM HPRE,6477,'Prevue Channel'                                      
         HCBNAM HSCI,7235,'Sci-Fi'                                              
         HCBNAM HTBS,5830,'Wtbs'                                                
         HCBNAM HTLC,6635,'The Learning Channel'                                
         HCBNAM HTNN,6221,'The Nashville Network'                               
         HCBNAM HTNT,6390,'Tnt'                                                 
         HCBNAM HTOO,7241,'The Cartoon Network'                                 
         HCBNAM HTR3,7296,'Tr3s'                                                
         HCBNAM HUSA,6217,'Usa'                                                 
         HCBNAM HVH1,6546,'Vh-1'                                                
         HCBNAM HWGN,6788,'Wgn'                                                 
         HCBNAM HXHI,8908,'The History Channel'                                 
         HCBNAM HCNL,8340,'Cine Latino'                                         
         HCBNAM HWPA,7196,'WAPA America'                                        
         HCBNAM COWB,12162,'The Cowboy Channel'   SPEC-52562                    
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHCBNAM                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF BROADCAST NETWORK NAMES.                                   *         
* TABLE IS SORTED IN THE ORDER DEFINED FOR THE AUDIENCE ESTIMATOR     *         
* INITIAL DOWNLOAD.                                                   *         
***********************************************************************         
TBL_NEBROD DS   0D                                                              
         DC    CL8'NEBROD'                                                      
         DS    XL6                                                              
         DC    AL2(NEBRDNLQ)       L'TABLE ENTRY                                
*                                                                               
         NETNMB ABC,'American Broadcasting Company'                             
         NETNMB CBS,'Columbia Broadcasting System'                              
         NETNMB NBC,'National Broadcasting Company'                             
         NETNMB FOX,'Fox Broadcasting Company'                                  
         NETNMB CW,'CW'                                                         
         NETNMB MNT,'MyNetworkTV'                                               
         NETNMB WB,'Warner Brothers Television'                                 
         NETNMB UPN,'United Paramount Network',FILENET=PAR                      
         NETNMB PBS,'Pbs Network'                                               
         NETNMB ION,'ION Media Networks Inc',FILENET=PAX                        
         NETNMB BOU,'Bounce Tv'                                                 
         NETNMB MET,'MeTv'                                                      
         NETNMB COZ,'Cozi TV'                                                   
         NETNMB UNI,'Univision'                                                 
         NETNMB TEL,'Telemundo'                                                 
         NETNMB TF,'Telefutura',UNIQUE=NO                                       
         NETNMB UMA,'UniMas',FILENET=TF                                         
         NETNMB AZA,'Azteca America'                                            
         NETNMB ETV,'Estrella TV'                                               
         NETNMB MFX,'MundoFox',UNIQUE=NO                                        
         NETNMB MMX,'MundoMax',FILENET=MFX                                      
         NETNMB GRT,'GRIT'                                                      
         NETNMB ESC,'Escape'                                                    
         NETNMB MYS,'Court TV Mystery'                                          
         NETNMB LAF,'LAFF'                                                      
         NETNMB HI,'Heroes and Icons'                                           
         NETNMB COM,'COMET'                                                     
         NETNMB STV,'Start TV'                                                  
         NETNMB TLX,'Telexitos'    SPEC-47677                                   
         NETNMB CHG,'Charge'       SPEC-49585                                   
         NETNMB TBD,'TBD TV'       SPEC-43280                                   
         NETNMB NLX,'NBCLX'        SPEC-50377                                   
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEBROD                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF SYNDICATION NETWORK NAMES.                                 *         
* TABLE IS SORTED IN ALPHABETICAL ORDER BY CALL LETTERS.              *         
* THE SYNDICATORS THAT ARE COMMENTED OUT ARE NO LONGER ON THE NIELSEN *         
*  LIST OF ACTIVE STATIONS, SO THEY WERE REMOVED. WEL ALSO REMOVED ALL*         
*  THE UNWIRED STATIONS (PER MARIA).  FEEL FREE TO ADD THESE STATIONS *         
*  BACK IF NEEDED.                                                              
***********************************************************************         
TBL_NESYND DS   0D                                                              
         DC    CL8'NESYND'                                                      
         DS    XL6                                                              
         DC    AL2(NESYNNLQ)       L'TABLE ENTRY                                
*                                                                               
         NETNAM 2/T,'20Th Television'                                           
*********NETNAM ALL,'All American Tv'                                           
         NETNAM AMC,'African Heritage Net'                                      
         NETNAM ATI,'Assoc. Tv International'                                   
         NETNAM ATV,'Aim Tell-A-Vision'                                         
         NETNAM B/A,'Buena Vista Tv/Abc'                                        
         NETNAM B/L,'Business Week/Litton'                                      
         NETNAM B/M,'Buena Vista/Mgm'                                           
         NETNAM BCI,'Big Content, Inc.'                                         
         NETNAM BDM,'Badami Productions'                                        
*********NETNAM BKN,'Bkn Kids Network'                                          
*********NETNAM BLM,'Bloomberg Television Unwired'                              
         NETNAM BPR,'B4 Productions, Inc.'                                      
         NETNAM BUS,'Business Week'                                             
         NETNAM BV,'Buena Vista Television'                                     
         NETNAM BVC,'Bvtas/Carsey-Werner'                                       
         NETNAM BVT,'Bvtas'                                                     
         NETNAM C/H,'Columbia/Heritage'                                         
         NETNAM C/O,'Columbia'                                                  
         NETNAM C/P,'Cbs Paramount'                                             
*********NETNAM C/W,'Cf Ent/Worldvision'                                        
         NETNAM CAP,'Cash Plus Media Srvs'                                      
         NETNAM CAR,'Carsey-Warner Dist'                                        
         NETNAM CCP,'Central City Production'                                   
         NETNAM CEC,'Connection 3 Ent Co'                                       
*********NETNAM CEG,'Capital Entertainment'                                     
         NETNAM CES,'Creative Ent. Serv'                                        
         NETNAM CFE,'Cf Entertainment'                                          
         NETNAM CMG,'Camstar Media Group'                                       
         NETNAM CMV,'Cottage Media'                                             
         NETNAM CNB,'Nbc Enterprises'                                           
         NETNAM CNC,'Cnbc'                                                      
*********NETNAM COC,'Chamber Of Commerce'                                       
         NETNAM CON,'Tvi Media'                                                 
         NETNAM CTD,'Cbs Tv Distribution'                                       
         NETNAM CTM,'Ctm'                                                       
         NETNAM CTS,'Columbia Tristar Tv'                                       
         NETNAM CTW,'Ctw'                                                       
         NETNAM CWB,'CW/Warner Bros'                                            
*********NETNAM CWI,'Camelot Western International'                             
         NETNAM DAD,'Disney Abc Domest Tv'                                      
         NETNAM DAL,'Dadt/Litton'                                               
*********NETNAM DAR,'D''Arcy Masius'                                            
         NETNAM DBZ,'Daily Buzz'                                                
         NETNAM DCM,'Dom Camera Media'                                          
         NETNAM DIC,'Discover Comm'                                             
         NETNAM DMB,'Dmb && B'                                                  
*********NETNAM DOW,'Dow Jones && Co'                                           
         NETNAM DSC,'Discovery Communications'                                  
         NETNAM ECL,'Eclipse Television'                                        
*********NETNAM ESM,'Eyemark/Summit Media'                                      
         NETNAM ESP,'Espn Sports Network'                                       
*********NETNAM EYE,'Eyemark Entertainment'                                     
         NETNAM EZS,'E&&Z Sales And Mktg'                                       
         NETNAM FF,'Fox Family'                                                 
         NETNAM FJR,'Farm Journal Em'                                           
         NETNAM FRE,'Fremantle Media Na'                                        
         NETNAM FWN,'First World'                                               
         NETNAM FXS,'Fox Sports Net'                                            
         NETNAM GEN,'Genesis Int TV Sales'                                      
         NETNAM GTS,'GTS Records'                                               
*********NETNAM GUI,'Guy Tv Inc'                                                
*********NETNAM H/E,'Hearst Ent/Eyemark'                                        
         NETNAM H/K,'Hearst/Kingworld'                                          
         NETNAM H/L,'Hometeam/Litton'                                           
         NETNAM H/T,'Hearst/Tribune'                                            
         NETNAM HEA,'Hearst Entertainment'                                      
         NETNAM HOL,'Mh2 Technologies'                                          
         NETNAM HSA,'Hearst Argyle'                                             
*********NETNAM I/E,'Itn Entertainment'                                         
         NETNAM IDS,'Innovative Data Svcs'                                      
         NETNAM IEG,'Innervisions Ent Grp'                                      
*********NETNAM IFE,'Ife'                                                       
         NETNAM IMG,'Img Media'                                                 
         NETNAM ITN,'Itn, Inc.'                                                 
*********NETNAM ITV,'Intersport Tv'                                             
         NETNAM K/P,'Kingworld/Pearson'                                         
         NETNAM K/T,'Kingworld/Tribune'                                         
         NETNAM K/W,'Kingworld/Westrn Intl Synd'                                
         NETNAM KET,'Creative Post Inc'                                         
         NETNAM KIN,'Kingworld Media Sales'                                     
*********NETNAM KNG,'Kingworld Media Sales Unwired'                             
         NETNAM LAT,'Latination LLC'                                            
         NETNAM LEO,'Leo Burnett'                                               
         NETNAM LJH,'Louis J Horvitz Prod'                                      
         NETNAM LMS,'Litton Media Sales'                                        
         NETNAM M/N,'Mgm-Nbc Media Sales'                                       
*********NETNAM M/S,'Mgm-Nbc Media Sales Unwired'                               
         NETNAM M/T,'Mighty Oak/Trifecta'                                       
*********NETNAM MAR,'Marathon Management'                                       
         NETNAM MAR,'Marathon Ventures'                                         
         NETNAM MDT,'MagicDust Television'                                      
         NETNAM MDV,'Mediavest'                                                 
         NETNAM MED,'Mediacast'                                                 
         NETNAM MER,'Meredith'                                                  
         NETNAM MET,'Metro Traffic Cont''l'                                     
         NETNAM MEV,'Mediavest'                                                 
         NETNAM MGM,'Mgm'                                                       
         NETNAM MGY,'Mgm Domestic Tv Dist'                                      
         NETNAM MMN,'Major Market Network'                                      
*********NETNAM MMU,'Major Market Network Unwired'                              
         NETNAM MOE,'Mighty Oak Ent'                                            
         NETNAM MOS,'Mighty Oak/Sony'                                           
         NETNAM MSE,'Mainstreet Enter''mnt'                                     
*********NETNAM MTC,'Metro Traffic Cont''l Unwired'                             
         NETNAM N/M,'Nbc Universal/Mslo'                                        
         NETNAM NAN,'National Advertising Network'                              
         NETNAM NBL,'Nbcu/Litton'                                               
         NETNAM NBU,'Nbc Universal'                                             
         NETNAM NCA,'Ncaa March Madness'                                        
         NETNAM NFL,'Nfl Network'                                               
         NETNAM NLT,'New Line Television'                                       
         NETNAM NMI,'New Millennium Media'                                      
*********NETNAM NSF,'Nib/Sachs Finley'                                          
*********NETNAM NUN,'Nbc-Universal Unwired'                                     
*********NETNAM OMG,'Onyx Media Group'                                          
*********NETNAM P/C,'Paramount/Cnbc'                                            
         NETNAM P/H,'Paramount/Heritage'                                        
         NETNAM PAR,'Paramount Pictures Tv'                                     
         NETNAM PAS,'Passport Ent'                                              
         NETNAM PBN,'PBS'                                                       
*********NETNAM PBS,'Pbs'                                                       
         NETNAM PBV,'Paramount/Buena Vista'                                     
         NETNAM PCC,'Phoenix Comm. Group'                                       
*********NETNAM PCU,'Phoenix Comm. Group Unwired'                               
         NETNAM PEA,'Pearson Television'                                        
*********NETNAM PET,'Petry Television Unwired'                                  
*********NETNAM POL,'Polygram Tv'                                               
         NETNAM PPI,'PPI Releasing Inc.'                                        
         NETNAM PRM,'Paramount'                                                 
*********NETNAM PRO,'Promark'                                                   
         NETNAM PTV,'Ptv'                                                       
         NETNAM R/T,'Radar/Trifecta'                                            
         NETNAM RAY,'Raycom'                                                    
*********NETNAM RE,'Rysher Entertainment'                                       
         NETNAM RMT,'Reach Media Tv Prods'                                      
         NETNAM S/A,'Studiosusa/Alto-Marc'                                      
         NETNAM S/H,'Sony/Heritage'                                             
         NETNAM SIN,'Sinclair Broadcast Group'                                  
*********NETNAM SAB,'Saban Entertainment'                                       
         NETNAM SPT,'Sony Pictures Tv'                                          
         NETNAM SSA,'Saatchi && Saatchi Advertising'                            
         NETNAM STD,'Studios Usa'                                               
         NETNAM T/B,'Tribune/Nbc'                                               
         NETNAM T/C,'Tribune/Columbia'                                          
         NETNAM T/L,'Tribune/New Line'                                          
         NETNAM T/P,'Tribune/Universal Tv'                                      
*********NETNAM T/S,'Titan Sports'                                              
         NETNAM T/U,'Tribune/Nbcu'                                              
         NETNAM T/W,'Tribune/Western Int'                                       
         NETNAM TAV,'Targetvision'                                              
         NETNAM TEC,'Tribune Entert. Co.'                                       
         NETNAM TEE,'Trifecta Ent/Esc'                                          
         NETNAM THN,'Heritage Media Group'                                      
*********NETNAM THT,'This Tv Network Unwired'                                   
         NETNAM TRE,'Tribune Entertainment'                                     
         NETNAM TRI,'Trifecta Media'                                            
         NETNAM TRM,'Tribune/Mgm'                                               
         NETNAM TUR,'Turner Entertainment'                                      
         NETNAM TW,'Twi'                                                        
         NETNAM TWB,'The W B'                                                   
         NETNAM UNE,'Uniworld Entertainment'                                    
         NETNAM UNL,'United Television Llc'                                     
         NETNAM UNV,'Universal Tv'                                              
         NETNAM UWT,'Universal Worldwide Television'                            
         NETNAM V/T,'Vt Ent/Trifecta'                                           
         NETNAM VKC,'Victor King/Cbn'                                           
*********NETNAM W/C,'Carsey-Warner Dist'                                        
         NETNAM W/H,'Warner/Heritage'                                           
         NETNAM W/N,'Warner Bros./New Line'                                     
         NETNAM W/T,'Warner Bros./Turner'                                       
         NETNAM WB,'Warner Bros. Tv'                                            
         NETNAM WIS,'Western Internat''l Syndication'                           
*********NETNAM WLM,'World Link Media Unwired'                                  
*********NETNAM WTU,'Warner Bros./Turner Unwired'                               
*********NETNAM WV,'Worldvision'                                                
         NETNAM WWE,'Wwe Entertainment'                                         
*********NETNAM WWF,'Wwf Entertainment'                                         
*********NETNAM ZME,'Zenith Media Unwired'                                      
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NESYND                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF HISPANIC TV AUDIENCE BROADCAST NETWORK NAMES.              *         
* TABLE IS SORTED BY IMPORTANCE.                                      *         
***********************************************************************         
TBL_NEHBRO DS   0D                                                              
         DC    CL8'NEHBRO'                                                      
         DS    XL6                                                              
         DC    AL2(NEHBRNLQ)       L'TABLE ENTRY                                
*                                                                               
         NETNMH UNI,'Univision'                                                 
         NETNMH TEL,'Telemundo'                                                 
         NETNMH TF,'Telefutura',UNIQUE=NO                                       
         NETNMH UMA,'UniMas'                                                    
         NETNMH AZA,'Azteca America'                                            
         NETNMH MT3,'Mtv Tr3s'                                                  
         NETNMH ETV,'Estrella TV'                                               
         NETNMH MFX,'MundoFox',UNIQUE=NO                                        
         NETNMH MMX,'MundoMax'                                                  
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHBRO                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF HISPANIC GENERAL MARKET ENGLISH NETWORK NAMES.             *         
* TABLE IS SORTED BY IMPORTANCE.                                      *         
***********************************************************************         
TBL_NEHGES DS   0D                                                              
         DC    CL8'NEHGES'                                                      
         DS    XL6                                                              
         DC    AL2(NEHGENLQ)       L'TABLE ENTRY                                
*                                                                               
         NETNAM ABC,'American Broadcasting Company'                             
         NETNAM CBS,'Columbia Broadcasting System'                              
         NETNAM NBC,'National Broadcasting Company'                             
         NETNAM FOX,'Fox Broadcasting Company'                                  
         NETNAM CW,'CW'                                                         
         NETNAM MNT,'MyNetworkTV'                                               
         NETNAM WB,'Warner Brothers Television'                                 
         NETNAM UPN,'United Paramount Network'                                  
         NETNAM ION,'ION Media Networks Inc'                                    
         NETNAM BOU,'Bounce Tv'                                                 
         NETNAM MET,'MeTv'                                                      
         NETNAM COZ,'Cozi Tv'                                                   
         NETNAM GRT,'GRIT'                                                      
         NETNAM ESC,'Escape'                                                    
         NETNAM MYS,'Court TV Mystery'                                          
         NETNAM LAF,'LAFF'                                                      
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHGES                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF BROADCAST AGGREGATE NETWORK NAMES.                         *         
* TABLE IS SORTED BY IMPORTANCE.                                      *         
***********************************************************************         
TBL_NEBAGG DS   0D                                                              
         DC    CL8'NEBAGG'                                                      
         DS    XL6                                                              
         DC    AL2(NEBAGNLQ)       L'TABLE ENTRY                                
*                                                                               
         NETAGG PBA,03,'All Pbs Stations'                                       
         NETAGG PBP,42,'Pbs Primary Network Affiliates'                         
         NETAGG PPY,04,'Premium Pay'                                            
         NETAGG AGD,13,'All Net Affiliates'                                     
         NETAGG AGA,10,'Abc Affiliates'                                         
         NETAGG AGB,11,'Cbs Affiliates'                                         
         NETAGG AGC,12,'Nbc Affiliates'                                         
         NETAGG FAF,21,'Fox Affiliates'                                         
         NETAGG AGW,38,'CW Affiliates'                                          
         NETAGG AGM,39,'MyNetworkTV Affiliates'                                 
         NETAGG WAF,22,'Wb Affiliates'                                          
         NETAGG UAF,25,'Upn Affiliates'                                         
         NETAGG PAF,23,'Ion Affiliates'                                         
         NETAGG ABO,47,'Bounce Tv Affiliates'                                   
         NETAGG AME,48,'MeTv Affiliates'                                        
         NETAGG ACO,49,'Cozi Tv Affiliates'                                     
         NETAGG AGU,16,'Univision Affiliates'                                   
         NETAGG AGT,15,'Telemundo Affiliates'                                   
         NETAGG AGF,35,'UniMas Affiliates'                                      
         NETAGG AGZ,19,'Azteca America Affiliates'                              
         NETAGG AGE,40,'Estrella Affiliates'                                    
         NETAGG AGX,46,'MundoMax Affiliates'                                    
         NETAGG TNA,26,'Total English Lang Net Afflts'                          
         NETAGG ONA,24,'Other Network Affiliates'                               
         NETAGG IND,01,'Independents And Superstations'                         
         NETAGG XIN,07,'Independents'                                           
         NETAGG IBR,27,'Independent Broadcast'                                  
         NETAGG CAB,05,'Cable Orig W/O Tbs,Foxnet'                              
         NETAGG CAT,09,'Cable Orig W/ Tbs,Foxnet'                               
         NETAGG NAC,29,'All Other Cable Origination'                            
         NETAGG XSU,08,'Superstations'                                          
         NETAGG SUP,02,'Superstations W/Tbs'                                    
         NETAGG ADC,28,'Ad-Supported Cable Origination'                         
         NETAGG AOT,36,'All Other Tuning'                                       
         NETAGG TNF,37,'Total Network Affiliates'                               
         NETAGG SBR,20,'Spanish Broadcast'                                      
         NETAGG DVD,43,'DVD Playback'                                           
         NETAGG DVR,44,'DVR Playback'                                           
         NETAGG VDG,45,'Video Game Play'                                        
         NETAGG AGR,51,'GRIT Affiliates'                                        
         NETAGG AES,50,'Escape Affiliates'                                      
         NETAGG AGL,53,'LAFF Affiliates'                                        
         NETAGG PBC,54,'PBS Subchannels'                                        
         NETAGG AHI,56,'Heroes and Icons Affiliates'                            
         NETAGG COM,55,'COMET'                                                  
         NETAGG AST,57,'Start TV Affiliates'                                    
         NETAGG ATL,58,'Telexitos Affiliates'                                   
         NETAGG ATG,59,'Charge Affiliates'                                      
         NETAGG ATB,60,'TBD TV Affiliates'                                      
         NETAGG ANX,61,'NBCLX Affiliates'                                       
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEBAGG                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF HISPANIC TV AUDIENCE AGGREGATES.                           *         
* TABLE IS SORTED BY IMPORTANCE.                                      *         
***********************************************************************         
TBL_NEHTAG DS   0D                                                              
         DC    CL8'NEHTAG'                                                      
         DS    XL6                                                              
         DC    AL2(NEHTAGLQ)       L'TABLE ENTRY                                
*                                                                               
         NETAGG PBA,03,'All Pbs Stations'                                       
         NETAGG PBP,42,'Pbs Primary Network Affiliates'                         
         NETAGG PPY,04,'Premium Pay Services'                                   
         NETAGG AGT,15,'Telemundo Affiliates'                                   
         NETAGG AGU,16,'Univision Affiliates'                                   
         NETAGG AGA,19,'Azteca America Affiliates'                              
         NETAGG AGE,40,'Estrella Affiliates'                                    
         NETAGG AGX,46,'MundoMax Affiliates'                                    
         NETAGG AGH,17,'All Spanish Lang Affiliates'                            
         NETAGG TFA,35,'UniMas Affiliates'                                      
         NETAGG TNA,26,'Total English Lang Net Afflts'                          
         NETAGG EIN,30,'English Lang Broadcast Indep'                           
         NETAGG ACX,31,'Ad-Supported Cable'                                     
         NETAGG OCX,32,'All Other Cable'                                        
         NETAGG SCN,33,'Spanish Language Cable'                                 
         NETAGG SIN,34,'Spanish Language Independents'                          
         NETAGG ONA,24,'Other English Lang Net Afflts'                          
         NETAGG AGD,13,'All English Lang Net Afflts'                            
         NETAGG AOT,36,'All Other Tuning'                                       
         NETAGG SBR,20,'Spanish Broadcast'                                      
         NETAGG DVD,43,'DVD Playback'                                           
         NETAGG DVR,44,'DVR Playback'                                           
         NETAGG VDG,45,'Video Game Play'                                        
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHTAG                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF HISPANIC GENERAL MARKET AGGREGATES.                        *         
* TABLE IS SORTED BY IMPORTANCE.                                      *         
***********************************************************************         
TBL_NEHGAG DS   0D                                                              
         DC    CL8'NEHGAG'                                                      
         DS    XL6                                                              
         DC    AL2(NEHGAGLQ)       L'TABLE ENTRY                                
*                                                                               
         NETAGG AFA,10,'Abc Affiliates'                                         
         NETAGG AFC,11,'Cbs Affiliates'                                         
         NETAGG AFN,12,'Nbc Affiliates'                                         
         NETAGG AFF,21,'Fox Affiliates'                                         
         NETAGG AGW,38,'CW Affiliates'                                          
         NETAGG AGM,39,'MyNetworkTV Affiliates'                                 
         NETAGG WAF,22,'Wb Affiliates'                                          
         NETAGG PAF,23,'Ion Affiliates'                                         
         NETAGG UAF,25,'Upn Affiliates'                                         
         NETAGG ABO,47,'Bounce Tv Affiliates'                                   
         NETAGG AME,48,'MeTv Affiliates'                                        
         NETAGG ACO,49,'Cozi Tv Affiliates'                                     
         NETAGG IND,07,'Independents excl/Fox,Tbs,Sup'                          
         NETAGG SUP,08,'Superstations excl/Tbs'                                 
         NETAGG TNF,37,'Total Network Affiliates'                               
         NETAGG AGR,51,'GRIT Affiliates'                                        
         NETAGG AES,50,'Escape Affiliates'                                      
         NETAGG AGL,53,'LAFF Affiliates'                                        
         NETAGG PBC,54,'PBS Subchannels'                                        
         NETAGG AHI,56,'Heroes and Icons Affiliates'                            
         NETAGG COM,55,'COMET'                                                  
         NETAGG AST,57,'Start TV Affiliates'                                    
         NETAGG ATL,58,'Telexitos Affiliates'                                   
         NETAGG ATG,59,'Charge Affiliates'                                      
         NETAGG ATB,60,'TBD TV Affiliates'                                      
         NETAGG ANX,61,'NBCLX Affiliates'                                       
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NEHGAG                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF NIELSEN NATIONAL CALENDAR WEEKS.                           *         
* USED FOR NAD ONLY.                                                  *         
***********************************************************************         
TBL_NENACWKS DS   0D                                                            
         DC    CL8'NENACWKS'                                                    
         DS    XL6                                                              
         DC    AL2(NEWKSQ)         L'TABLE ENTRY                                
*                                                                               
         DC    AL2(OCT_92),AL1(WEEK_37,WEEK_40)                                 
         DC    AL2(NOV_92),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(DEC_92),AL1(WEEK_45,WEEK_52)                                 
         DC    AL2(JAN_93),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_93),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_93),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_93),AL1(WEEK_14,WEEK_16)                                 
         DC    AL2(MAY_93),AL1(WEEK_17,WEEK_21)                                 
         DC    AL2(JUN_93),AL1(WEEK_22,WEEK_24)                                 
         DC    AL2(JUL_93),AL1(WEEK_25,WEEK_28)                                 
         DC    AL2(AUG_93),AL1(WEEK_29,WEEK_32)                                 
         DC    AL2(SEP_93),AL1(WEEK_33,WEEK_36)                                 
*                                                                               
         DC    AL2(OCT_93),AL1(WEEK_37,WEEK_40)                                 
         DC    AL2(NOV_93),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(DEC_93),AL1(WEEK_45,WEEK_52)                                 
         DC    AL2(JAN_94),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_94),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_94),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_94),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_94),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_94),AL1(WEEK_23,WEEK_27)                                 
         DC    AL2(JUL_94),AL1(WEEK_28,WEEK_31)                                 
         DC    AL2(AUG_94),AL1(WEEK_32,WEEK_36)                                 
         DC    AL2(SEP_94),AL1(WEEK_37,WEEK_40)                                 
*                                                                               
         DC    AL2(OCT_94),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(NOV_94),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_94),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_95),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_95),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_95),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_95),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_95),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_95),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_95),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_95),AL1(WEEK_32,WEEK_36)                                 
         DC    AL2(SEP_95),AL1(WEEK_37,WEEK_40)                                 
*                                                                               
         DC    AL2(OCT_95),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(NOV_95),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_95),AL1(WEEK_49,WEEK_53)                                 
         DC    AL2(JAN_96),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_96),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_96),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_96),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_96),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_96),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_96),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_96),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_96),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_96),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_96),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_96),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_97),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_97),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_97),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_97),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_97),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_97),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_97),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_97),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_97),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_97),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_97),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_97),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_98),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_98),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_98),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_98),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_98),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_98),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_98),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_98),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_98),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_98),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_98),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_98),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_99),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_99),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_99),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_99),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_99),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_99),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_99),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_99),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_99),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_99),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_99),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_99),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_00),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_00),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_00),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_00),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_00),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_00),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_00),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_00),AL1(WEEK_32,WEEK_36)                                 
         DC    AL2(SEP_00),AL1(WEEK_37,WEEK_40)                                 
*                                                                               
         DC    AL2(OCT_00),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(NOV_00),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_00),AL1(WEEK_49,WEEK_53)                                 
         DC    AL2(JAN_01),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_01),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_01),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_01),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_01),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_01),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_01),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_01),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_01),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_01),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_01),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_01),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_02),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_02),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_02),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_02),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_02),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_02),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_02),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_02),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_02),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_02),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_02),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_02),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_03),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_03),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_03),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_03),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_03),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_03),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_03),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_03),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_03),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_03),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_03),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_03),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_04),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_04),AL1(WEEK_05,WEEK_09)                                 
         DC    AL2(MAR_04),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_04),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_04),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_04),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_04),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_04),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_04),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_04),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_04),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_04),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_05),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_05),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_05),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_05),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_05),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_05),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_05),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_05),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_05),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_05),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_05),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_05),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_06),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_06),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_06),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_06),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_06),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_06),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_06),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_06),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_06),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_06),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_06),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_06),AL1(WEEK_49,WEEK_53)                                 
         DC    AL2(JAN_07),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_07),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_07),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_07),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_07),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_07),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_07),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_07),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_07),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_07),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_07),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_07),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_08),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_08),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_08),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_08),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_08),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_08),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_08),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_08),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_08),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_08),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_08),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_08),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_09),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_09),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_09),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_09),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_09),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_09),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_09),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_09),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_09),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_09),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_09),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_09),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_10),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_10),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_10),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_10),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_10),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_10),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_10),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_10),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_10),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_10),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_10),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_10),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_11),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_11),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_11),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_11),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_11),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_11),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_11),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_11),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_11),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_11),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_11),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_11),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_12),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_12),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_12),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_12),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_12),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_12),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_12),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_12),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_12),AL1(WEEK_36,WEEK_40)                                 
*                                                                               
         DC    AL2(OCT_12),AL1(WEEK_41,WEEK_44)                                 
         DC    AL2(NOV_12),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_12),AL1(WEEK_49,WEEK_53)                                 
         DC    AL2(JAN_13),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_13),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_13),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_13),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_13),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_13),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_13),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_13),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_13),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_13),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_13),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_13),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_14),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_14),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_14),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_14),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_14),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_14),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_14),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_14),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_14),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_14),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_14),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_14),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_15),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_15),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_15),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_15),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_15),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_15),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_15),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_15),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_15),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_15),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_15),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_15),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_16),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_16),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_16),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_16),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_16),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_16),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_16),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_16),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_16),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_16),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_16),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_16),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_17),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_17),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_17),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_17),AL1(WEEK_14,WEEK_18)                                 
         DC    AL2(MAY_17),AL1(WEEK_19,WEEK_22)                                 
         DC    AL2(JUN_17),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_17),AL1(WEEK_27,WEEK_31)                                 
         DC    AL2(AUG_17),AL1(WEEK_32,WEEK_35)                                 
         DC    AL2(SEP_17),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_17),AL1(WEEK_40,WEEK_44)                                 
         DC    AL2(NOV_17),AL1(WEEK_45,WEEK_48)                                 
         DC    AL2(DEC_17),AL1(WEEK_49,WEEK_53)                                 
         DC    AL2(JAN_18),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_18),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_18),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_18),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_18),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_18),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_18),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_18),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_18),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_18),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_18),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_18),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_19),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_19),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_19),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_19),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_19),AL1(WEEK_18,WEEK_21)                                 
         DC    AL2(JUN_19),AL1(WEEK_22,WEEK_26)                                 
         DC    AL2(JUL_19),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_19),AL1(WEEK_31,WEEK_34)                                 
         DC    AL2(SEP_19),AL1(WEEK_35,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_19),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_19),AL1(WEEK_44,WEEK_47)                                 
         DC    AL2(DEC_19),AL1(WEEK_48,WEEK_52)                                 
         DC    AL2(JAN_20),AL1(WEEK_01,WEEK_04)                                 
         DC    AL2(FEB_20),AL1(WEEK_05,WEEK_08)                                 
         DC    AL2(MAR_20),AL1(WEEK_09,WEEK_13)                                 
         DC    AL2(APR_20),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_20),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_20),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_20),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_20),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_20),AL1(WEEK_36,WEEK_39)                                 
*                                                                               
         DC    AL2(OCT_20),AL1(WEEK_40,WEEK_43)                                 
         DC    AL2(NOV_20),AL1(WEEK_44,WEEK_48)                                 
         DC    AL2(DEC_20),AL1(WEEK_49,WEEK_52)                                 
         DC    AL2(JAN_21),AL1(WEEK_01,WEEK_05)                                 
         DC    AL2(FEB_21),AL1(WEEK_06,WEEK_09)                                 
         DC    AL2(MAR_21),AL1(WEEK_10,WEEK_13)                                 
         DC    AL2(APR_21),AL1(WEEK_14,WEEK_17)                                 
         DC    AL2(MAY_21),AL1(WEEK_18,WEEK_22)                                 
         DC    AL2(JUN_21),AL1(WEEK_23,WEEK_26)                                 
         DC    AL2(JUL_21),AL1(WEEK_27,WEEK_30)                                 
         DC    AL2(AUG_21),AL1(WEEK_31,WEEK_35)                                 
         DC    AL2(SEP_21),AL1(WEEK_36,WEEK_39)                                 
         DC    X'FF'                                                            
*                                                                               
         SPACE 2                                                                
         TABLE_LEN TBL_NENACWKS                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF NIELSEN NATIONAL CALENDAR QUARTERS                         *         
* DATES FOR QTR ARE FIRST SET TO CABLE DATES WHEN BROADCAST DATES ARE *         
* NOT AVAILABLE.  DATES NEED TO CHANGE TO BROADCAST DATES WHEN        *         
* BROADCAST DATES ARE AVAILABLE.                                      *         
***********************************************************************         
TBL_NENTIQRT DS   0D                                                            
         DC    CL8'NENTIQRT'                                                    
         DS    XL6                                                              
         DC    AL2(NEQTRQ)         L'TABLE ENTRY                                
*                                                                               
         NTIQRT 1993,Q1,1992-12-28,1993-03-28                                   
         NTIQRT 1993,Q2,1993-03-29,1993-06-27                                   
         NTIQRT 1993,Q3,1993-06-28,1993-09-12                                   
         NTIQRT 1993,Q4,1993-09-13,1993-12-26                                   
         NTIQRT 1994,Q1,1993-12-27,1994-03-27                                   
         NTIQRT 1994,Q2,1994-03-28,1994-06-26                                   
         NTIQRT 1994,Q3,1994-06-27,1994-09-18                                   
         NTIQRT 1994,Q4,1994-09-19,1995-01-01                                   
         NTIQRT 1995,Q1,1995-01-02,1995-03-26                                   
         NTIQRT 1995,Q2,1995-03-27,1995-06-25                                   
         NTIQRT 1995,Q3,1995-06-26,1995-09-17                                   
         NTIQRT 1995,Q4,1995-09-18,1995-12-31                                   
         NTIQRT 1996,Q1,1996-01-01,1996-03-31                                   
         NTIQRT 1996,Q2,1996-04-01,1996-06-25                                   
         NTIQRT 1996,Q3,1996-06-26,1996-09-16                                   
         NTIQRT 1996,Q4,1996-09-17,1996-12-29                                   
         NTIQRT 1997,Q1,1996-12-30,1997-03-30                                   
         NTIQRT 1997,Q2,1997-03-31,1997-06-29                                   
         NTIQRT 1997,Q3,1997-06-30,1997-09-21                                   
         NTIQRT 1997,Q4,1997-09-22,1997-12-28                                   
         NTIQRT 1998,Q1,1997-12-29,1998-03-29                                   
         NTIQRT 1998,Q2,1998-03-30,1998-06-28                                   
         NTIQRT 1998,Q3,1998-06-29,1998-09-20                                   
         NTIQRT 1998,Q4,1998-09-21,1998-12-27                                   
         NTIQRT 1999,Q1,1998-12-28,1999-03-28                                   
         NTIQRT 1999,Q2,1999-03-29,1999-06-27                                   
         NTIQRT 1999,Q3,1999-06-28,1999-09-19                                   
         NTIQRT 1999,Q4,1999-09-20,1999-12-26                                   
         NTIQRT 2000,Q1,1999-12-27,2000-03-26                                   
         NTIQRT 2000,Q2,2000-03-27,2000-06-25                                   
         NTIQRT 2000,Q3,2000-06-26,2000-10-01                                   
         NTIQRT 2000,Q4,2000-10-02,2000-12-31                                   
         NTIQRT 2001,Q1,2001-01-01,2001-04-01                                   
         NTIQRT 2001,Q2,2001-04-02,2001-07-01                                   
         NTIQRT 2001,Q3,2001-07-02,2001-09-16                                   
         NTIQRT 2001,Q4,2001-09-17,2001-12-30                                   
         NTIQRT 2002,Q1,2001-12-31,2002-03-31                                   
         NTIQRT 2002,Q2,2002-04-01,2002-06-30                                   
         NTIQRT 2002,Q3,2002-07-01,2002-09-29                                   
         NTIQRT 2002,Q4,2002-09-30,2002-12-29                                   
         NTIQRT 2003,Q1,2002-12-30,2003-03-30                                   
         NTIQRT 2003,Q2,2003-03-31,2003-06-29                                   
         NTIQRT 2003,Q3,2003-06-30,2003-09-21                                   
         NTIQRT 2003,Q4,2003-09-22,2003-12-28                                   
         NTIQRT 2004,Q1,2003-12-29,2004-03-28                                   
         NTIQRT 2004,Q2,2004-03-29,2004-06-27                                   
         NTIQRT 2004,Q3,2004-06-28,2004-09-19                                   
         NTIQRT 2004,Q4,2004-09-20,2004-12-26                                   
         NTIQRT 2005,Q1,2004-12-27,2005-03-27                                   
         NTIQRT 2005,Q2,2005-03-28,2005-06-26                                   
         NTIQRT 2005,Q3,2005-06-27,2005-09-18                                   
         NTIQRT 2005,Q4,2005-09-19,2005-12-25                                   
         NTIQRT 2006,Q1,2005-12-26,2006-03-26                                   
         NTIQRT 2006,Q2,2006-03-27,2006-06-25                                   
         NTIQRT 2006,Q3,2006-06-26,2006-09-17                                   
         NTIQRT 2006,Q4,2006-09-18,2006-12-31                                   
         NTIQRT 2007,Q1,2007-01-01,2007-04-01                                   
         NTIQRT 2007,Q2,2007-04-02,2007-07-01                                   
         NTIQRT 2007,Q3,2007-07-02,2007-09-23                                   
         NTIQRT 2007,Q4,2007-09-24,2007-12-30                                   
         NTIQRT 2008,Q1,2007-12-31,2008-03-30                                   
         NTIQRT 2008,Q2,2008-03-31,2008-06-29                                   
         NTIQRT 2008,Q3,2008-06-30,2008-09-21                                   
         NTIQRT 2008,Q4,2008-09-22,2008-12-28                                   
         NTIQRT 2009,Q1,2008-12-29,2009-03-29                                   
         NTIQRT 2009,Q2,2009-03-30,2009-06-28                                   
         NTIQRT 2009,Q3,2009-06-29,2009-09-20                                   
         NTIQRT 2009,Q4,2009-09-21,2009-12-27                                   
         NTIQRT 2010,Q1,2009-12-28,2010-03-28                                   
         NTIQRT 2010,Q2,2010-03-29,2010-06-27                                   
         NTIQRT 2010,Q3,2010-06-28,2010-09-19                                   
         NTIQRT 2010,Q4,2010-09-20,2010-09-26                                   
         NTIQRT 2011,Q1,2010-12-27,2011-03-27                                   
         NTIQRT 2011,Q2,2011-03-28,2011-06-26                                   
         NTIQRT 2011,Q3,2011-06-27,2011-09-18                                   
         NTIQRT 2011,Q4,2011-09-19,2011-12-25                                   
         NTIQRT 2012,Q1,2011-12-26,2012-03-25                                   
         NTIQRT 2012,Q2,2012-03-26,2012-06-24                                   
         NTIQRT 2012,Q3,2012-06-25,2012-09-23                                   
         NTIQRT 2012,Q4,2012-09-24,2012-12-30                                   
         NTIQRT 2013,Q1,2012-12-31,2013-03-31                                   
         NTIQRT 2013,Q2,2013-04-01,2013-06-30                                   
         NTIQRT 2013,Q3,2013-07-01,2013-09-22                                   
         NTIQRT 2013,Q4,2013-09-23,2013-12-29                                   
         NTIQRT 2014,Q1,2013-12-30,2014-03-30                                   
         NTIQRT 2014,Q2,2014-03-31,2014-06-29                                   
         NTIQRT 2014,Q3,2014-06-30,2014-09-21                                   
         NTIQRT 2014,Q4,2014-09-22,2014-12-28                                   
         NTIQRT 2015,Q1,2014-12-29,2015-03-29                                   
         NTIQRT 2015,Q2,2015-03-30,2015-06-28                                   
         NTIQRT 2015,Q3,2015-06-29,2015-09-27                                   
         NTIQRT 2015,Q4,2015-09-28,2015-12-27                                   
         NTIQRT 2016,Q1,2015-12-28,2016-03-27                                   
         NTIQRT 2016,Q2,2016-03-28,2016-06-26                                   
         NTIQRT 2016,Q3,2016-06-27,2016-09-25                                   
         NTIQRT 2016,Q4,2016-09-26,2016-12-25                                   
         NTIQRT 2017,Q1,2016-12-26,2017-03-26                                   
         NTIQRT 2017,Q2,2017-03-27,2017-06-25                                   
         NTIQRT 2017,Q3,2017-06-26,2017-09-24                                   
         NTIQRT 2017,Q4,2017-09-25,2017-12-31                                   
         NTIQRT 2018,Q1,2018-01-01,2018-04-01                                   
         NTIQRT 2018,Q2,2018-04-02,2018-07-01                                   
         NTIQRT 2018,Q3,2018-07-02,2018-09-23                                   
*                                                                               
         NTIQRT 2018,Q4,2018-09-24,2018-12-30                                   
         NTIQRT 2019,Q1,2018-12-31,2019-03-31                                   
         NTIQRT 2019,Q2,2019-04-01,2019-06-30                                   
         NTIQRT 2019,Q3,2019-07-01,2019-09-29                                   
*                                                                               
         NTIQRT 2019,Q4,2019-09-23,2019-12-29                                   
         NTIQRT 2020,Q1,2019-12-30,2020-03-29                                   
         NTIQRT 2020,Q2,2020-03-30,2020-06-28                                   
         NTIQRT 2020,Q3,2020-06-29,2020-09-27                                   
*                                                                               
         NTIQRT 2020,Q4,2020-09-28,2020-12-27                                   
         NTIQRT 2020,Q1,2020-12-28,2021-03-28                                   
         NTIQRT 2021,Q2,2021-03-29,2021-06-27                                   
         NTIQRT 2021,Q3,2021-06-28,2021-09-19                                   
                                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_NENTIQRT                                                 
         EJECT                                                                  
***********************************************************************         
* TABLE FOR TVQ/FORETEL CALENDAR.                                     *         
* UPDATE THIS EVERY YEAR USING THE SCHEDULE SENT BY THE CLIENT.       *         
***********************************************************************         
TBL_NETVQCAL DS   0D                                                            
         DC    CL8'NETVQCAL'                                                    
         DS    XL6                                                              
         DC    AL2(NETVQQ)         L'TABLE ENTRY                                
*                                                                               
         DC    AL1(97,33),AL1(97,39),AL2(SEP_97)    08/11/97-09/28/97           
         DC    AL1(97,40),AL1(97,42),AL2(OCT_97)    09/29/97-10/19/97           
         DC    AL1(97,43),AL1(97,47),AL2(NOV_97)    10/20/97-11/23/97           
         DC    AL1(97,48),AL1(98,02),AL2(JAN_98)    11/24/97-01/11/98           
         DC    AL1(98,03),AL1(98,08),AL2(FEB_98)    01/12/98-02/22/98           
         DC    AL1(98,09),AL1(98,14),AL2(MAR_98)    02/23/98-04/05/98           
         DC    AL1(98,15),AL1(98,24),AL2(MAY_98)    04/06/98-06/14/98           
         DC    AL1(98,25),AL1(98,32),AL2(JUL_98)    06/15/98-08/09/98           
                                                                                
         DC    AL1(98,33),AL1(98,39),AL2(SEP_98)    08/10/98-09/27/98           
         DC    AL1(98,40),AL1(98,42),AL2(OCT_98)    09/28/98-10/18/98           
         DC    AL1(98,43),AL1(98,47),AL2(NOV_98)    10/19/98-11/22/98           
         DC    AL1(98,48),AL1(99,02),AL2(JAN_99)    11/23/98-01/10/99           
         DC    AL1(99,03),AL1(99,08),AL2(FEB_99)    01/11/99-02/21/99           
         DC    AL1(99,09),AL1(99,14),AL2(MAR_99)    02/22/99-04/04/99           
         DC    AL1(99,15),AL1(99,24),AL2(MAY_99)    04/05/99-06/13/99           
         DC    AL1(99,25),AL1(99,32),AL2(JUL_99)    06/14/99-08/08/99           
                                                                                
         DC    AL1(99,33),AL1(99,39),AL2(SEP_99)    08/09/99-09/26/99           
         DC    AL1(99,40),AL1(99,42),AL2(OCT_99)    09/27/99-10/17/99           
         DC    AL1(99,43),AL1(99,47),AL2(NOV_99)    10/18/99-11/21/99           
         DC    AL1(99,48),AL1(100,02),AL2(JAN_00)   11/22/99-01/09/00           
         DC    AL1(100,03),AL1(100,08),AL2(FEB_00)  01/10/00-02/20/00           
         DC    AL1(100,09),AL1(100,14),AL2(MAR_00)  02/21/00-04/02/00           
         DC    AL1(100,15),AL1(100,24),AL2(MAY_00)  04/03/00-06/11/00           
         DC    AL1(100,25),AL1(100,32),AL2(JUL_00)  06/12/00-09/11/00           
                                                                                
         DC    AL1(100,33),AL1(100,40),AL2(SEP_00)  08/07/00-10/01/00           
         DC    AL1(100,41),AL1(100,43),AL2(OCT_00)  10/02/00-10/22/00           
         DC    AL1(100,44),AL1(100,47),AL2(NOV_00)  10/23/00-11/19/00           
         DC    AL1(100,48),AL1(101,01),AL2(JAN_01)  11/20/00-01/07/01           
         DC    AL1(101,02),AL1(101,07),AL2(FEB_01)  01/08/01-02/18/01           
         DC    AL1(101,08),AL1(101,13),AL2(MAR_01)  02/19/01-04/01/01           
         DC    AL1(101,14),AL1(101,23),AL2(MAY_01)  04/02/01-06/10/01           
         DC    AL1(101,24),AL1(101,32),AL2(JUL_01)  06/11/01-08/12/01           
                                                                                
         DC    AL1(101,33),AL1(101,38),AL2(SEP_01)  08/13/01-09/23/01           
         DC    AL1(101,39),AL1(101,41),AL2(OCT_01)  09/24/01-10/14/01           
         DC    AL1(101,42),AL1(101,45),AL2(NOV_01)  10/15/01-11/11/01           
         DC    AL1(101,46),AL1(101,52),AL2(JAN_02)  11/12/01-12/30/01           
         DC    AL1(102,01),AL1(102,06),AL2(FEB_02)  12/31/01-02/10/02           
         DC    AL1(102,07),AL1(102,12),AL2(MAR_02)  02/11/02-03/24/02           
         DC    AL1(102,13),AL1(102,22),AL2(MAY_02)  03/25/02-06/02/02           
         DC    AL1(102,23),AL1(102,32),AL2(JUL_02)  06/03/02-08/11/02           
                                                                                
         DC    AL1(102,33),AL1(102,38),AL2(SEP_02)  08/12/02-09/22/02           
         DC    AL1(102,39),AL1(102,41),AL2(OCT_02)  09/23/02-10/13/02           
         DC    AL1(102,42),AL1(102,45),AL2(NOV_02)  10/14/02-11/10/02           
         DC    AL1(102,46),AL1(102,52),AL2(JAN_03)  11/11/02-12/29/02           
         DC    AL1(103,01),AL1(103,06),AL2(FEB_03)  12/30/02-02/09/03           
         DC    AL1(103,07),AL1(103,12),AL2(MAR_03)  02/10/03-03/23/03           
         DC    AL1(103,13),AL1(103,22),AL2(MAY_03)  03/24/03-06/01/03           
         DC    AL1(103,23),AL1(103,32),AL2(JUL_03)  06/02/03-08/10/03           
                                                                                
         DC    AL1(103,33),AL1(103,38),AL2(SEP_03)  08/11/03-09/21/03           
         DC    AL1(103,39),AL1(103,41),AL2(OCT_03)  09/22/03-10/12/03           
         DC    AL1(103,42),AL1(103,45),AL2(NOV_03)  10/13/03-11/09/03           
         DC    AL1(103,46),AL1(103,52),AL2(JAN_04)  11/10/03-12/28/03           
         DC    AL1(104,01),AL1(104,06),AL2(FEB_04)  12/29/03-02/08/04           
         DC    AL1(104,07),AL1(104,12),AL2(MAR_04)  02/09/04-03/21/04           
         DC    AL1(104,13),AL1(104,22),AL2(MAY_04)  03/22/04-05/30/04           
         DC    AL1(104,23),AL1(104,32),AL2(JUL_04)  05/31/04-08/08/04           
                                                                                
         DC    AL1(104,33),AL1(104,38),AL2(SEP_04)  08/09/04-09/19/04           
         DC    AL1(104,39),AL1(104,40),AL2(OCT_04)  09/20/04-10/03/04           
         DC    AL1(104,41),AL1(104,44),AL2(NOV_04)  10/04/04-10/31/04           
         DC    AL1(104,45),AL1(104,48),AL2(JAN_05)  11/01/04-11/28/04           
         DC    AL1(105,02),AL1(105,05),AL2(FEB_05)  01/03/05-01/30/05           
         DC    AL1(105,06),AL1(105,12),AL2(MAR_05)  01/31/05-03/20/05           
         DC    AL1(105,13),AL1(105,22),AL2(MAY_05)  03/21/05-05/29/05           
         DC    AL1(105,23),AL1(105,32),AL2(JUL_05)  05/30/05-08/07/05           
                                                                                
         DC    AL1(105,33),AL1(105,38),AL2(AUG_05)  08/08/05-09/18/05           
         DC    AL1(105,39),AL1(105,40),AL2(SEP_05)  09/19/05-10/02/05           
         DC    AL1(105,41),AL1(105,44),AL2(OCT_05)  10/03/05-10/30/05           
         DC    AL1(105,45),AL1(105,48),AL2(NOV_05)  10/31/05-11/27/05           
         DC    AL1(106,02),AL1(106,05),AL2(JAN_06)  01/02/06-01/29/06           
         DC    AL1(106,06),AL1(106,12),AL2(MAR_06)  01/30/06-03/19/06           
         DC    AL1(106,13),AL1(106,22),AL2(MAY_06)  03/20/06-05/28/06           
         DC    AL1(106,23),AL1(106,32),AL2(JUL_06)  05/29/06-08/06/06           
                                                                                
         DC    AL1(106,33),AL1(106,38),AL2(AUG_06)  08/07/06-09/17/06           
         DC    AL1(106,39),AL1(106,40),AL2(SEP_06)  09/18/06-10/01/06           
         DC    AL1(106,41),AL1(106,44),AL2(OCT_06)  10/02/06-10/29/06           
         DC    AL1(106,45),AL1(106,48),AL2(NOV_06)  10/30/06-11/26/06           
         DC    AL1(107,01),AL1(107,04),AL2(JAN_07)  01/01/07-01/28/07           
         DC    AL1(107,05),AL1(107,11),AL2(MAR_07)  01/29/07-03/18/07           
         DC    AL1(107,12),AL1(107,21),AL2(MAY_07)  03/19/07-05/27/07           
         DC    AL1(107,22),AL1(107,31),AL2(JUL_07)  05/28/07-08/05/07           
                                                                                
         DC    AL1(107,36),AL1(107,39),AL2(SEP_07)  09/03/07-09/30/07           
         DC    AL1(107,40),AL1(107,42),AL2(OCT_07)  10/01/07-10/21/07           
         DC    AL1(107,43),AL1(107,45),AL2(NOV_07)  10/22/07-11/11/07           
         DC    AL1(107,46),AL1(107,48),AL2(DEC_07)  11/12/07-12/02/07           
*********DC    AL1(107,49),AL1(108,02),AL2(JAN_08)  12/03/07-01/13/08           
         DC    AL1(108,03),AL1(108,06),AL2(FEB_08)  01/14/08-02/10/08           
         DC    AL1(108,07),AL1(108,10),AL2(MAR_08)  02/11/08-03/09/08           
         DC    AL1(108,11),AL1(108,19),AL2(APR_08)  03/10/08-05/11/08           
         DC    AL1(108,20),AL1(108,24),AL2(MAY_08)  05/12/08-06/15/08           
*********DC    AL1(108,25),AL1(108,28),AL2(JUN_08)  06/16/08-07/13/08           
         DC    AL1(108,31),AL1(108,34),AL2(JUL_08)  07/28/08-08/24/08           
                                                                                
         DC    AL1(108,37),AL1(108,40),AL2(SEP_08)  09/08/08-10/05/08           
         DC    AL1(108,41),AL1(108,43),AL2(OCT_08)  10/06/08-10/26/08           
         DC    AL1(108,44),AL1(108,46),AL2(NOV_08)  10/27/08-11/16/08           
         DC    AL1(108,47),AL1(108,49),AL2(DEC_08)  11/17/08-12/07/08           
         DC    AL1(109,03),AL1(109,06),AL2(FEB_09)  01/12/09-02/08/09           
         DC    AL1(109,10),AL1(109,14),AL2(MAR_09)  03/05/09-04/05/09           
         DC    AL1(109,18),AL1(109,22),AL2(MAY_09)  04/30/09-05/31/09           
         DC    AL1(109,24),AL1(109,27),AL2(JUN_09)  06/08/09-07/05/09           
         DC    AL1(109,29),AL1(109,32),AL2(JUL_09)  07/13/09-08/09/09           
                                                                                
         DC    AL1(109,37),AL1(109,40),AL2(SEP_09)  09/07/09-10/04/09           
         DC    AL1(109,41),AL1(109,42),AL2(OCT_09)  10/05/09-10/18/09           
         DC    AL1(109,43),AL1(109,46),AL2(NOV_09)  10/19/09-11/15/09           
         DC    AL1(109,47),AL1(109,49),AL2(DEC_09)  11/16/09-12/06/09           
         DC    AL1(110,02),AL1(110,05),AL2(JAN_10)  01/04/10-01/31/10           
         DC    AL1(110,06),AL1(110,10),AL2(FEB_10)  02/01/10-03/07/10           
         DC    AL1(110,18),AL1(110,22),AL2(MAY_10)  04/26/10-05/30/10           
         DC    AL1(110,28),AL1(110,31),AL2(JUL_10)  07/05/10-08/01/10           
         DC    AL1(110,32),AL1(110,35),AL2(AUG_10)  08/02/10-08/29/10           
                                                                                
         DC    AL1(110,37),AL1(110,40),AL2(SEP_10)  09/06/10-10/03/10           
         DC    AL1(110,41),AL1(110,42),AL2(OCT_10)  10/04/10-10/17/10           
         DC    AL1(110,43),AL1(110,46),AL2(NOV_10)  10/18/10-11/14/10           
         DC    AL1(110,47),AL1(110,49),AL2(DEC_10)  11/15/10-12/05/10           
         DC    AL1(111,02),AL1(111,05),AL2(JAN_11)  01/03/11-01/30/11           
         DC    AL1(111,06),AL1(111,10),AL2(FEB_11)  02/03/11-03/06/11           
         DC    AL1(111,18),AL1(111,22),AL2(MAY_11)  04/25/11-05/29/11           
         DC    AL1(111,28),AL1(111,31),AL2(JUL_11)  07/04/11-07/31/11           
         DC    AL1(111,32),AL1(111,35),AL2(AUG_11)  08/01/11-08/28/11           
                                                                                
         DC    AL1(111,37),AL1(111,40),AL2(SEP_11)  09/05/11-10/02/11           
         DC    AL1(111,41),AL1(111,42),AL2(OCT_11)  10/03/11-10/16/11           
         DC    AL1(111,43),AL1(111,46),AL2(NOV_11)  10/17/11-11/13/11           
         DC    AL1(111,47),AL1(111,49),AL2(DEC_11)  11/14/11-12/04/11           
         DC    AL1(112,02),AL1(112,05),AL2(JAN_12)  01/02/12-01/29/12           
         DC    AL1(112,06),AL1(112,10),AL2(FEB_12)  01/30/12-03/04/12           
         DC    AL1(112,14),AL1(112,17),AL2(APR_12)  03/26/12-04/22/12           
         DC    AL1(112,18),AL1(112,22),AL2(MAY_12)  04/23/12-05/27/12           
         DC    AL1(112,29),AL1(112,32),AL2(JUL_12)  07/09/12-08/05/12           
                                                                                
         DC    X'FFFFFFFFFFFF'                                                  
         SPACE 2                                                                
         TABLE_LEN TBL_NETVQCAL                                                 
         EJECT                                                                  
***********************************************************************         
* TABLE FOR CABLE MOVIE GOER UNIVERSES.                                         
* UPDATE THIS EVERY SEPTEMBER USING THE REGULAR MOVIE GOER UNIVERSES.           
*                                                                               
* HERE'S WHAT NEEDS TO BE DONE ANNUALLY:                                        
*  1. ON THE DAY THAT THE UNIVERSES WILL CHANGE, WE NEED TO RUN THE             
*      REGULAR MOVIE GOER PROCESS, BUT NOT THE CABLE MOVIE GOER.                
*      THE DELIVERY DATE CAN BE DERIVED FROM THE START OF THE NIELSEN           
*      NATIONAL CALENDAR YEAR (SEE TABLE TBL_NENACWKS)                          
*      NOTE: THE CONVERT/LOAD PROCESS IS AUTOMATED. AS A RESULT, THE            
*      CABLE MOVIEGOER CONVERSION STARTS AUTOMATICALLY. IT WILL ISSUE           
*      A WARNING E-MAIL IF THIS TABLE NEEDS UPDATING, AND THEN IT WILL          
*      DELIBERATELY FAIL.                                                       
*  2. AFTER THE REGULAR DATA IS PROCESSED, LOOK AT THE LOG DATASET              
*      'EXTDATA.DEM.SYSPRINT.NTIFILP'                                           
*      AND FIND THE **LAST** OCCURRENCE OF THE                                  
*      STRING 'NTI/NSS WEEKLY MOVIEGOER CONVERSION'. YOU'LL SEE A               
*      HEX DUMP OF THE UNIVERSE ELEMENTS.                                       
*  3. THE TABLE ENTRY BEGINS WITH AN MVGOER MACRO CALL. TWO                     
*      PARAMETERS ARE REQUIRED: THE START YEAR AND WEEK NUMBER.                 
*      THESE SHOULD MATCH THE YEAR/WEEK IN THE X'5E' ELEMENT.                   
*      ADD THE NEW ENTRIES AT THE *BEGINNING* OF THE TABLE, SO THAT             
*      THE DATES ARE IN REVERSE CHRONOLOGICAL SEQUENCE.                         
*  4. COPY/PASTE THE ELEMENTS (X'23' AND X'4B'). BE SURE TO ALSO COPY           
*      THE FINAL X'5E' ELEMENT.                                                 
*  5. THE TABLE ENTRY ENDS WITH ANOTHER MVGOER MACRO. WITH THE SINGLE           
*      PARAMETER 'END'.                                                         
*  6. RELINK THIS MODULE AS AN "A" VERSION, AND PROMOTE IT TO STGE.             
*      ASK OPS SUPPORT TO CONVERT                                               
*      AND LOAD THE CABLE MOVIEGOER DATA, WITH A "DEMTABS=T00AD1A"              
*      CONTROL CARD IN THE CONVERSION STEP.                                     
*  7. TEST THE NEW UNIVERSE TABLE DATA BY SUBMITTING                            
*      'DEIS.DDS.JCL(TESTMVGO)' WITH A "TEST=T00AD1A" CARD. MODIFY THE          
*      REQUEST DATES AS NEEDED TO GENERATE A REPORT FROM THE BEGINNING          
*      OF AUGUST TO THE END OF SEPTEMBER. YOU SHOULD SEE THE NEW                
*      UNIVERSE VALUES PRINTED FOR THE CURRENT WEEK. THIS MEANS THAT            
*      THE TABLE WAS UPDATED CORRECTLY.                                         
*  8. INSTALL THE NEW DEMTABS INTO PRODUCTION.                                  
***********************************************************************         
TBL_MOVIEGOU DS   0D                                                            
         DC    CL8'MOVIEGOU'                                                    
         DS    XL6                                                              
         DC    AL2(MGULENQ)        L'TABLE ENTRY                                
*                                                                               
 MVGOER YR_2020,WEEK_36           8/31/20-8/29/21                               
 DC X'2303A7'                                                                   
 DC X'4B2D4301D8A801D8A80005A8000D920013FC001C47001F5D00410B0005FD000B'         
 DC X'E60012E0001B2F001DAE0027EB'                                               
 DC X'5E07D4D5D57824'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301D8A801D8A800017D00056A0008C20008B00009630011CE0001B90004'         
 DC X'FA00081900093000096F000BFB'                                               
 DC X'5E07D4D5D57824'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301D8A801D8A80000890002110004140003BE00043A00080D00008D0002'         
 DC X'3B0003F200041800043C0005B9'                                               
 DC X'5E07D4D5D57824'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2019,WEEK_35            8/26/19-08/30/20                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301D71801D718000631000DCE001475001C030020D80043E60006CE000D'         
 DC X'80001362001C60001E5E002840'                                               
 DC X'5E07D4D5D57723'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301D71801D71800022E00061B000A1200094F000A7800132D0002560006'         
 DC X'2C0009A9000A1D000AC3000C7A'                                               
 DC X'5E07D4D5D57723'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301D71801D7180000EB0002880004EB00044C0004A60008D20000EC0002'         
 DC X'F40004F000049A0004F100064A'                                               
 DC X'5E07D4D5D57723'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2018,WEEK_35            8/27/18-08/25/19                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301D45C01D45C0007B4000FBC001573001D38001E860042D40006EF000D'         
 DC X'92001400001D6E001F8500296D'                                               
 DC X'5E07D4D5D57623'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301D45C01D45C0002C900080D000A990009F40009F200125C0002750006'         
 DC X'F2000A8F000BDA000B81000D75'                                               
 DC X'5E07D4D5D57623'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301D45C01D45C0001020003C000054900049F00047C00093800010E0002'         
 DC X'FD00055400054A00058F0006E9'                                               
 DC X'5E07D4D5D57623'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2017,WEEK_36            8/28/17-08/26/18                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301D33001D3300008030010730016C3001D2B001F540042E7000844000E'         
 DC X'91001555001C95001E4B00274C'                                               
 DC X'5E07D4D5D57524'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301D33001D3300002DA0008B5000B97000AB9000A4A0013AC0002D70007'         
 DC X'2C000ADB000A400009FE000C6E'                                               
 DC X'5E07D4D5D57524'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301D33001D3300000E10003C90005C200048000047D0009BE0001000003'         
 DC X'A20005E4000473000484000633'                                               
 DC X'5E07D4D5D57524'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2016,WEEK_36            8/29/16-08/27/17                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301CE8001CE800006D1000FBE00164B001D21001E16003FD0000738000F'         
 DC X'6100167A001DA7001F74002919'                                               
 DC X'2303A8'                                                                   
 DC X'4B2D4301CE8001CE8000023E0007C8000BAA0009F80009940011C600026B0007'         
 DC X'B0000B56000B15000A5D000C06'                                               
 DC X'2303A9'                                                                   
 DC X'4B2D4301CE8001CE800000C50003A90005910004020003FC0008C00001150003'         
 DC X'C70005EB0004AB0004A00005E5'                                               
 DC X'5E07D4D5D57424'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2015,WEEK_36            8/31/15-08/28/16                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301C6B001C6B000071B0011AF001778001CA7001D82003EF600073C000E'         
 DC X'0C0015E3001AE6001C100026A8'                                               
 DC X'2303A8'                                                                   
 DC X'4B2D4301C6B001C6B0000253000956000D1E000A09000A1400129B0002AB0006'         
 DC X'DC000C180009CD000A58000CA9'                                               
 DC X'2303A9'                                                                   
 DC X'4B2D4301C6B001C6B00000E30004500007240003C300048D0009630000FF0003'         
 DC X'8600064700043B0004A700060C'                                               
 DC X'5E07D4D5D57324'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2014,WEEK_36            9/1/14-08/30/15                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301C6B001C6B00007DA00122100186D001C6E001E77003F5A00086C000E'         
 DC X'470015C1001A4A001C330024B8'                                               
 DC X'2303A8'                                                                   
 DC X'4B2D4301C6B001C6B00002B60009D3000E29000948000AC40014160003380007'         
 DC X'BE000C75000B0D000B45000CB1'                                               
 DC X'2303A9'                                                                   
 DC X'4B2D4301C6B001C6B000010B00054900075B0003EA0005710009800001210003'         
 DC X'AC0006680005070005E000065C'                                               
 DC X'5E07D4D5D57224'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2013,WEEK_35            8/26/13-08/31/14                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301C45801C4580007440013A90018E2001EDD001FE6003F51000674000F'         
 DC X'260015D2001A77001A7E00223E'                                               
 DC X'2303A8'                                                                   
 DC X'4B2D4301C45801C458000285000AD8000E540009C7000B2900126F0002720007'         
 DC X'B8000CAC000B6C000A6E000B7B'                                               
 DC X'2303A9'                                                                   
 DC X'4B2D4301C45801C4580000BB0006050007BE00046D0005AC0008A60000E40003'         
 DC X'C80006D8000568000530000583'                                               
 DC X'5E07D4D5D57123'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2012,WEEK_36            8/27/12-8/25/13                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301BE1801BE180007EB001549001A7C001F72001DE5003E36000904000F'         
 DC X'DA001739001840001866001DDD'                                               
 DC X'2303A8'                                                                   
 DC X'4B2D4301BE1801BE180002ED000D2D000FA0000B30000A980011D500034B0008'         
 DC X'60000EC2000A960009150009AB'                                               
 DC X'2303A9'                                                                   
 DC X'4B2D4301BE1801BE180000FC0006C50008DE00054F00057400084A0001480004'         
 DC X'7D0008650005960004D5000518'                                               
 DC X'5E07D4D5D57024'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2011,WEEK_36             8/29/11-8/26/12                             
 DC X'2303A7'                                                                   
 DC X'4B2D4301C00C01C00C0009A30014EC0019D5001DB1001D04003A'                     
 DC X'980009B9001061001790001928001904001DD7'                                   
 DC X'2303A8'                                                                   
 DC X'4B2D4301C00C01C00C00041C000CD6000F9A000B40000A470010'                     
 DC X'960003D300090E000ED9000A4900099B00097A'                                   
 DC X'2303A9'                                                                   
 DC X'4B2D4301C00C01C00C0001880006B400096000057D0004AC0008'                     
 DC X'320001B70004DE00087A000532000453000551'                                   
 DC X'5E07D4D5D56F24'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2010,WEEK_36            8/30/10-8/28/11                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301C4BC01C4BC0008B20015ED0019C3001F1C001F8C003E'                     
 DC X'31000A7300113800181F001B8E001B09001D07'                                   
 DC X'2303A8'                                                                   
 DC X'4B2D4301C4BC01C4BC0003B6000E92001034000C80000B430012'                     
 DC X'06000484000ABE000FFD000C3E000A94000951'                                   
 DC X'2303A9'                                                                   
 DC X'4B2D4301C4BC01C4BC0001AD00084C0009570005830005B40009'                     
 DC X'300001BC0005D40009BB00067700051F000465'                                   
 DC X'5E07D4D5D56E24'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2009,WEEK_36            8/31/09-8/29/10                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301C0D401C0D4000A68001685001B3B001DB7001CC6003C'                     
 DC X'BD00098500124E00174F001B79001AEA001D6E'                                   
 DC X'2303A8'                                                                   
 DC X'4B2D4301C0D401C0D4000456000F2D000FED000BAE000B0A0013'                     
 DC X'0A00039E000C27000F80000BFC000A19000A36'                                   
 DC X'2303A9'                                                                   
 DC X'4B2D4301C0D401C0D40001F800092B0009C400059A00055F0008'                     
 DC X'EA00012E0006CE0009C900062B0004B5000501'                                   
 DC X'5E07D4D5D56D24'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2008,WEEK_36            9/01/08-8/30/09                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301BF4401BF44000B0C00160C001ADB001D81001F4E0039'                     
 DC X'A5000ABE0013B20016FD001B5A001A42001E78'                                   
 DC X'2303A8'                                                                   
 DC X'4B2D4301BF4401BF440004AC000E2A001030000B40000AE10011'                     
 DC X'09000488000C37000F3D000C5A000AA30009F1'                                   
 DC X'2303A9'                                                                   
 DC X'4B2D4301BF4401BF4400022E0008AB0009E600059C00057D0009'                     
 DC X'530002610006D200099B00068200057A000539'                                   
 DC X'5E07D4D5D56C24'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2007,WEEK_35            8/27/07-8/31/08                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301B8A001B8A0000A0E001511001A46001E260023870043'                     
 DC X'37000A990013B6001884001CD5001CD000216F'                                   
 DC X'2303A8'                                                                   
 DC X'4B2D4301B8A001B8A0000451000E1A0010CB000B8B000C790012'                     
 DC X'FE000496000C8C0010E0000DCE000B000009B1'                                   
 DC X'2303A9'                                                                   
 DC X'4B2D4301B8A001B8A00001C20008A4000A9700057B0005C90008'                     
 DC X'A20002250007BC000A8400070D000562000549'                                   
 DC X'5E07D4D5D56B23'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2006,WEEK_36            8/28/06-8/26/07                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301B32801B328000A6D001673001B530020450022820045'                     
 DC X'C60009280014350016EA001B80001D7A002115'                                   
 DC X'5E07D4D5D56A24'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301B32801B3280004D8000F80001068000E06000C3B0014'                     
 DC X'130003D2000D89000FF2000CC7000A6B0009A4'                                   
 DC X'5E07D4D5D56A24'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301B32801B3280002670009D7000A0E0006740006130009'                     
 DC X'A3000214000815000A6B0006D800058300055F'                                   
 DC X'5E07D4D5D56A24'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2005,WEEK_36            8/29/05-8/27/06                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301AE7801AE78000ABD0015E4001B6600210700241F0048'                     
 DC X'E600087A00140D001703001C8E001C7C0021E1'                                   
 DC X'5E07D4D5D56924'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301AE7801AE78000544000F440011A6000DF9000CBF0013'                     
 DC X'C80003B4000C7F000F28000EB1000A62000977'                                   
 DC X'5E07D4D5D56924'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301AE7801AE780001DF0009D0000B820006740006500009'                     
 DC X'5B0001BB0007D10009E60007EE0005980004BF'                                   
 DC X'5E07D4D5D56924'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2004,WEEK_36            8/30/04-8/28/05                              
 DC X'2303A7'                                                                   
 DC X'4B2D4301AC2001AC2000091A00165C001B020022510026E60047'                     
 DC X'160008EC0012E4001767001FA5001D760022F3'                                   
 DC X'5E07D4D5D56824'                                                           
 DC X'2303A8'                                                                   
 DC X'4B2D4301AC2001AC2000048800108E0011AC000E45000F650012'                     
 DC X'CA0003C0000CD40010370010C30009F50009A5'                                   
 DC X'5E07D4D5D56824'                                                           
 DC X'2303A9'                                                                   
 DC X'4B2D4301AC2001AC200001CB000A95000B3400079800088B000A'                     
 DC X'35000190000734000B330008A50004110004D6'                                   
 DC X'5E07D4D5D56824'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2003,WEEK_36            9/01/03-8/29/04                              
 DC X'2303A7'                                                                   
 DC X'4B1F022A582A5800F402200286035B041A073100F301E1024A02'                     
 DC X'C702EF0344'                                                               
 DC X'5E07D4D5D56724'                                                           
 DC X'2303A8'                                                                   
 DC X'4B1F022A582A580069018E01920194019A01C90060014001A501'                     
 DC X'65013300FE'                                                               
 DC X'5E07D4D5D56724'                                                           
 DC X'2303A9'                                                                   
 DC X'4B1F022A582A58002D00F800F600C000EB00E2002A00CD011C00'                     
 DC X'C8008F0094'                                                               
 DC X'5E07D4D5D56724'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2002,WEEK_35            8/26/02-08/31/03                             
 DC X'2303A7'                                                                   
 DC X'4B1F0229AE29AE00E0021A02E1039B0444079700EE01E4024302'                     
 DC X'F9033E03B3'                                                               
 DC X'5E07D4D5D56623'                                                           
 DC X'2303A8'                                                                   
 DC X'4B1F0229AE29AE005C019101F801BF019B01D7006C013B01A701'                     
 DC X'B1013700EC'                                                               
 DC X'5E07D4D5D56623'                                                           
 DC X'2303A9'                                                                   
 DC X'4B1F0229AE29AE003600E8014300E300C700E8003200B6010E00'                     
 DC X'E7009C0088'                                                               
 DC X'5E07D4D5D56623'                                                           
 MVGOER END                                                                     
*                                                                               
 MVGOER YR_2001,WEEK_35            8/27/01-8/25/02                              
 DC X'2303A7'                                                                   
 DC X'4B1F0229362936007B01D0028903CA04F6089D00B001C5021403'                     
 DC X'2F03B803C7'                                                               
 DC X'5E07D4D5D56622'                                                           
 DC X'2303A8'                                                                   
 DC X'4B1F0229362936003F015D01AA01CB020101E50052013B019001'                     
 DC X'B2016C00EA'                                                               
 DC X'5E07D4D5D56622'                                                           
 DC X'2303A9'                                                                   
 DC X'4B1F0229362936001A00CE010F00C700F700DC001E00C600F401'                     
 DC X'0300AB008A'                                                               
 DC X'5E07D4D5D56622'                                                           
 MVGOER END                                                                     
*                                                                               
         DC    X'FFFF'             EOT                                          
         SPACE 2                                                                
         TABLE_LEN TBL_MOVIEGOU                                                 
         EJECT                                                                  
***********************************************************************         
* TABLE OF CABLE NAD STATIONS THAT DON'T HAVE PROGRAM DATA            *         
* TABLE IS KEYED BY CALL LETTERS, THEREFORE ALL HISTORICAL CALL       *         
*  LETTERS MUST BE REPRESENTED IN THIS TABLE.                         *         
* MAY ALSO CONTAIN EFFECTIVE DATE FOR PROGRAM DATA.                   *         
***********************************************************************         
TBL_NADTPB DS   0D                                                              
         DC    CL8'NADTPB'                                                      
         DS    XL6                                                              
         DC    AL2(CNADTLNQ)       L'TABLE ENTRY                                
*                                                                               
         NADTPB BBCA,(YR_2003,WEEK_27)                                          
         NADTPB CC,(YR_2012,WEEK_41)                                            
         NADTPB CINL,(YR_2015,WEEK_23)                                          
         NADTPB AJAM,(YR_2012,WEEK_01)         AJAM, NEW NAME FOR CRNT          
         NADTPB DHLT,(YR_2002,WEEK_40)                                          
         NADTPB OWN,(YR_2002,WEEK_40)          OWN, NEW NAME FOR DHLT           
         NADTPB DIY,(YR_2009,WEEK_01)                                           
         NADTPB DTMS,(YR_2006,WEEK_27)                                          
         NADTPB ID,(YR_2006,WEEK_27)           ID, NEW NAME FOR DTMS            
         NADTPB DSCI,(YR_2005,WEEK_27)                                          
         NADTPB SCI,(YR_2005,WEEK_27)                                           
         NADTPB DKID,(YR_2010,WEEK_40)                                          
         NADTPB HUB,(YR_2010,WEEK_40)                                           
         NADTPB ENC,NO_PGM_DATA                                                 
         NADTPB ENN,(YR_2005,WEEK_01)                                           
         NADTPB ESPC,(YR_2005,WEEK_01)                                          
         NADTPB ESPU,(YR_2012,WEEK_45)                                          
         NADTPB FINE,(YR_2012,WEEK_41)                                          
         NADTPB GAC,(YR_2007,WEEK_01)                                           
         NADTPB UP,(YR_2010,WEEK_40)           UP, NEW NAME FOR GMC             
         NADTPB WAPA,(YR_2015,WEEK_14)                                          
         NADTPB GOLF,(YR_2006,WEEK_01)                                          
         NADTPB INSP,(YR_2011,WEEK_23)                                          
         NADTPB MIL,(YR_2005,WEEK_40)                                           
         NADTPB NUVO,NO_PGM_DATA                                                
         NADTPB OXYG,(YR_2003,WEEK_14)                                          
         NADTPB OVTN,NO_PGM_DATA                                                
         NADTPB RLZC,(YR_2011,WEEK_14)                                          
         NADTPB SPMN,NO_PGM_DATA                                                
         NADTPB STZ,NO_PGM_DATA                                                 
         NADTPB ESQ,(YR_2004,WEEK_40)          ESQ, NEW NAME FOR STYL           
         NADTPB SV,(YR_2002,WEEK_05)                                            
         NADTPB TECH,(YR_2005,WEEK_40)                                          
         NADTPB G4,(YR_2005,WEEK_40)           G4, NEW NAME FOR TECH            
         NADTPB THN,NO_PGM_DATA                                                 
         NADTPB TOC,NO_PGM_DATA                                                 
         NADTPB TV1,(YR_2006,WEEK_40)                                           
         NADTPB RFD,(YR_2009,WEEK_04)                                           
         NADTPB FBN,(YR_2011,WEEK_14)                                           
         NADTPB VS,(YR_2003,WEEK_01)                                            
         NADTPB OLN,(YR_2003,WEEK_01)                                           
         NADTPB NBCS,(YR_2003,WEEK_01)                                          
         NADTPB BAND,NO_PGM_DATA                                                
         NADTPB TVLC,(YR_2016,WEEK_40)                                          
         NADTPB ZLIV,NO_PGM_DATA                                                
         NADTPB ESNL,NO_PGM_DATA                                                
*                                                                               
         DC    X'FF'               EOT                                          
         SPACE 2                                                                
         TABLE_LEN TBL_NADTPB                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE OF UNIVERSE YEARS FOR NETWORK                                           
***********************************************************************         
TBL_UNIVYRS DS   0D                                                             
         DC    CL8'UNIVYRS'                                                     
         DS    XL6                                                              
         DC    AL2(UNVYRLNQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL2(2021),CL8'08/31/20',AL2(SEP_20)                              
         DC    AL2(2020),CL8'08/29/19',AL2(SEP_19)                              
         DC    AL2(2019),CL8'08/27/18',AL2(SEP_18)                              
         DC    AL2(2018),CL8'08/28/17',AL2(SEP_17)                              
         DC    AL2(2017),CL8'08/29/16',AL2(SEP_16)                              
         DC    AL2(2016),CL8'08/31/15',AL2(SEP_15)                              
         DC    AL2(2015),CL8'09/01/14',AL2(SEP_14)                              
         DC    AL2(2014),CL8'08/26/13',AL2(SEP_13)                              
         DC    AL2(2013),CL8'08/27/12',AL2(SEP_12)                              
         DC    AL2(2012),CL8'08/29/11',AL2(SEP_11)                              
         DC    AL2(2011),CL8'08/30/10',AL2(SEP_10)                              
         DC    AL2(2010),CL8'08/31/09',AL2(SEP_09)                              
         DC    AL2(2009),CL8'09/01/08',AL2(SEP_08)                              
         DC    AL2(2008),CL8'08/27/07',AL2(SEP_07)                              
         DC    AL2(2007),CL8'08/28/06',AL2(SEP_06)                              
         DC    AL2(2006),CL8'08/29/05',AL2(SEP_05)                              
         DC    AL2(2005),CL8'08/30/04',AL2(SEP_04)                              
         DC    AL2(2004),CL8'09/01/03',AL2(SEP_03)                              
         DC    AL2(2003),CL8'08/26/02',AL2(SEP_02)                              
         DC    AL2(2002),CL8'08/27/01',AL2(SEP_01)                              
         DC    AL2(2001),CL8'09/04/00',AL2(SEP_00)                              
         DC    AL2(2000),CL8'08/30/99',AL2(SEP_99)                              
         DC    AL2(1999),CL8'08/31/98',AL2(SEP_98)                              
         DC    AL2(1998),CL8'09/01/97',AL2(SEP_97)                              
         DC    AL2(1997),CL8'09/02/96',AL2(SEP_96)                              
         DC    AL2(1996),CL8'09/04/95',AL2(SEP_95)                              
         DC    AL2(1995),CL8'08/29/94',AL2(SEP_94)                              
         DC    AL2(1994),CL8'08/30/93',AL2(SEP_93)                              
         DC    AL2(1993),CL8'08/31/92',AL2(SEP_92)                              
         DC    AL2(1992),CL8'09/02/91',AL2(SEP_91)                              
         DC    AL2(1991),CL8'09/03/90',AL2(SEP_90)                              
         DC    AL2(1990),CL8'09/04/89',AL2(SEP_89)                              
         DC    AL2(1989),CL8'08/29/88',AL2(SEP_88)                              
         DC    AL2(1988),CL8'08/31/87',AL2(SEP_87)                              
         DC    AL2(1987),CL8'09/01/86',AL2(SEP_86)                              
         DC    AL2(1986),CL8'09/02/85',AL2(SEP_85)                              
         DC    AL2(1985),CL8'09/03/84',AL2(SEP_84)                              
         DC    AL2(1984),CL8'08/29/83',AL2(SEP_83)                              
         DC    AL2(1983),CL8'08/30/82',AL2(SEP_82)                              
         DC    AL2(1982),CL8'08/31/81',AL2(SEP_81)                              
         DC    AL2(1981),CL8'09/01/80',AL2(SEP_80)                              
         DC    AL2(1980),CL8'09/03/79',AL2(SEP_79)                              
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_UNIVYRS                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF OVERRIDE PROGRAM NAMES FOR ITN                                       
***********************************************************************         
TBL_ITNPNAME DS   0D                                                            
         DC    CL8'ITNPNAME'                                                    
         DS    XL6                                                              
         DC    AL2(ITNPNMLQ)       L'TABLE ENTRY                                
*                                                                               
         DC    AL2(3739),CL25'ITN EVENING NEWS A'                               
         DC    AL2(3740),CL25'ITN EVENING NEWS B'                               
         DC    AL2(3741),CL25'ITN EVENING NEWS C'                               
         DC    AL2(3742),CL25'ITN EVENING NEWS D'                               
         DC    AL2(3743),CL25'ITN EVENING NEWS E'                               
         DC    AL2(3761),CL25'ITN EVENING NEWS F'                               
         DC    AL2(3762),CL25'ITN EVENING NEWS G'                               
         DC    AL2(3763),CL25'ITN EVENING NEWS H'                               
         DC    AL2(3764),CL25'ITN EVENING NEWS I'                               
         DC    AL2(3765),CL25'ITN EVENING NEWS J'                               
* ADDED ON 6/13/2018                                                            
         DC    AL2(6222),CL25'ITN EVENING NEWS K'                               
         DC    AL2(6223),CL25'ITN EVENING NEWS L'                               
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ITNPNAME                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF RADIO ETHNIC MARKET NUMBERS LINK TO STANDARD BOOKTYPE                
* MARKET NUMBERS                                                                
* 2 BYTE STANDARD BOOKTYPE MARKET NUMBER, 2 BYTE ETHNIC MKT NUM,                
* 1 BYTE ETHNIC BOOKTYPE                                                        
***********************************************************************         
TBL_RADNMKTS DS   0D                                                            
         DC    CL8'RAD_NMKT'                                                    
         DS    XL6                                                              
         DC    AL2(RADNMKTQ)       L'TABLE ENTRY                                
*                                                                               
         RADETH 580,963,BOOKTYPE_B ABG                                          
         RADETH 81,974,BOOKTYPE_B  Akron Black                                  
         RADETH 47,917,BOOKTYPE_B  ATL                                          
         RADETH 305,952,BOOKTYPE_B AUG                                          
         RADETH 21,909,BOOKTYPE_B  BAL                                          
         RADETH 533,976,BOOKTYPE_B BLX                                          
         RADETH 13,934,BOOKTYPE_B  BOS                                          
         RADETH 37,935,BOOKTYPE_B  BUF                                          
         RADETH 223,931,BOOKTYPE_B BAT                                          
         RADETH 95,923,BOOKTYPE_B  BIR                                          
         RADETH 231,933,BOOKTYPE_B CRL                                          
         RADETH 5,903,BOOKTYPE_B   CHI                                          
         RADETH 93,924,BOOKTYPE_B  CHL                                          
         RADETH 31,910,BOOKTYPE_B  CIN                                          
         RADETH 181,936,BOOKTYPE_B CKA                                          
         RADETH 19,911,BOOKTYPE_B  CLE                                          
         RADETH 183,944,BOOKTYPE_B CSC                                          
         RADETH 45,914,BOOKTYPE_B  CLO                                          
         RADETH 235,961,BOOKTYPE_B CMB                                          
         RADETH 24,912,BOOKTYPE_B  DF                                           
         RADETH 67,918,BOOKTYPE_B  DAY                                          
         RADETH 11,906,BOOKTYPE_B  DET                                          
         RADETH 359,954,BOOKTYPE_B FA                                           
         RADETH 163,962,BOOKTYPE_B FLT                                          
         RADETH 166,932,BOOKTYPE_B GWH                                          
         RADETH 361,945,BOOKTYPE_B GNC                                          
         RADETH 191,937,BOOKTYPE_B GVS                                          
         RADETH 327,946,BOOKTYPE_B HNT                                          
         RADETH 33,913,BOOKTYPE_B  HOU                                          
         RADETH 393,977,BOOKTYPE_B HV                                           
         RADETH 49,919,BOOKTYPE_B  IND                                          
         RADETH 169,938,BOOKTYPE_B JMS                                          
         RADETH 107,927,BOOKTYPE_B JKV                                          
         RADETH 41,915,BOOKTYPE_B  KC                                           
         RADETH 253,955,BOOKTYPE_B LAF                                          
         RADETH 3,902,BOOKTYPE_B   LA                                           
         RADETH 55,920,BOOKTYPE_B  LOU                                          
         RADETH 123,947,BOOKTYPE_B LRK                                          
         RADETH 265,957,BOOKTYPE_B MAC                                          
         RADETH 43,939,BOOKTYPE_B  MIL                                          
         RADETH 75,925,BOOKTYPE_B  MEM                                          
         RADETH 429,916,BOOKTYPE_B MF                                           
         RADETH 133,948,BOOKTYPE_B MOB                                          
         RADETH 173,953,BOOKTYPE_B MON                                          
         RADETH 73,921,BOOKTYPE_B  NAS                                          
         RADETH 53,922,BOOKTYPE_B  NOL                                          
         RADETH 1,901,BOOKTYPE_B   NY                                           
         RADETH 109,929,BOOKTYPE_B NOR                                          
         RADETH 83,956,BOOKTYPE_B  OKC                                          
         RADETH 131,940,BOOKTYPE_B ORL                                          
         RADETH 23,941,BOOKTYPE_B  PIT                                          
         RADETH 7,904,BOOKTYPE_B   PHL                                          
         RADETH 115,930,BOOKTYPE_B RAL                                          
         RADETH 105,926,BOOKTYPE_B RCH                                          
         RADETH 79,958,BOOKTYPE_B  ROH                                          
         RADETH 9,905,BOOKTYPE_B   SF                                           
         RADETH 285,959,BOOKTYPE_B SAV                                          
         RADETH 17,907,BOOKTYPE_B  STL                                          
         RADETH 111,951,BOOKTYPE_B SVP                                          
         RADETH 87,942,BOOKTYPE_B  TAM                                          
         RADETH 97,943,BOOKTYPE_B  TOL                                          
         RADETH 15,908,BOOKTYPE_B  WAS                                          
         RADETH 139,979,BOOKTYPE_B WLM                                          
         RADETH 299,950,BOOKTYPE_B WPB                                          
*                                                                               
         RADETH 141,781,BOOKTYPE_H ABQ                                          
         RADETH 47,737,BOOKTYPE_H  ATL                                          
         RADETH 135,835,BOOKTYPE_H AUS                                          
         RADETH 143,783,BOOKTYPE_H BAK                                          
         RADETH 13,738,BOOKTYPE_H  BOS                                          
         RADETH 155,747,BOOKTYPE_H CC                                           
         RADETH 5,852,BOOKTYPE_H   CHI                                          
         RADETH 35,784,BOOKTYPE_H  DEN                                          
         RADETH 24,846,BOOKTYPE_H  DF                                           
         RADETH 161,850,BOOKTYPE_H ELP                                          
         RADETH 89,861,BOOKTYPE_H  FRS                                          
         RADETH 33,848,BOOKTYPE_H  HOU                                          
         RADETH 393,978,BOOKTYPE_H HV                                           
         RADETH 3,842,BOOKTYPE_H   LA                                           
         RADETH 506,749,BOOKTYPE_H LAR                                          
         RADETH 441,970,BOOKTYPE_H LNM                                          
         RADETH 257,831,BOOKTYPE_H LAS                                          
         RADETH 263,980,BOOKTYPE_H LUB                                          
         RADETH 269,849,BOOKTYPE_H MB                                           
         RADETH 536,969,BOOKTYPE_H MRC                                          
         RADETH 429,827,BOOKTYPE_H MF                                           
         RADETH 343,966,BOOKTYPE_H MOD                                          
         RADETH 413,928,BOOKTYPE_H MSU                                          
         RADETH 321,750,BOOKTYPE_H NAU                                          
         RADETH 1,819,BOOKTYPE_H   NY                                           
         RADETH 283,862,BOOKTYPE_H SAL                                          
         RADETH 561,971,BOOKTYPE_H ODM                                          
         RADETH 83,968,BOOKTYPE_H  OKC                                          
         RADETH 131,845,BOOKTYPE_H ORL                                          
         RADETH 594,775,BOOKTYPE_H OXV                                          
         RADETH 592,965,BOOKTYPE_H PS                                           
         RADETH 57,864,BOOKTYPE_H  PHX                                          
         RADETH 353,973,BOOKTYPE_H PBL                                          
         RADETH 379,809,BOOKTYPE_H RSB                                          
         RADETH 65,878,BOOKTYPE_H  SAC                                          
         RADETH 59,814,BOOKTYPE_H  SAN                                          
         RADETH 63,859,BOOKTYPE_H  SD                                           
         RADETH 9,860,BOOKTYPE_H   SF                                           
         RADETH 215,879,BOOKTYPE_H SJ                                           
         RADETH 567,972,BOOKTYPE_H SML                                          
         RADETH 291,964,BOOKTYPE_H STK                                          
         RADETH 87,742,BOOKTYPE_H  TAM                                          
         RADETH 207,865,BOOKTYPE_H TUC                                          
         RADETH 419,967,BOOKTYPE_H VTV                                          
         RADETH 293,773,BOOKTYPE_H VIS                                          
         RADETH 15,745,BOOKTYPE_H  WAS                                          
         RADETH 299,960,BOOKTYPE_H WPB                                          
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_RADNMKTS                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF RENTRAK CABLE CALL LETTERS.                                *         
* THIS TABLE INCLUDES DUPLICATES BY NUMERIC CODE:IF A NETWORK CHANGED *         
* CALL LETTERS AT SOME POINT IN TIME, ALL CALL LETTERS FOR A GIVEN    *         
* NUMERIC CODE SHOULD BE KEPT IN THIS TABLE. THE MOST RECENT CALL     *         
* LETTERS SHOULD COME FIRST.                                          *         
***********************************************************************         
TBL_RECABCLL DS 0D                                                              
         DC    CL8'RECABCLL'                                                    
         DS    XL6                                                              
         DC    AL2(RECBCLQ)        L'TABLE ENTRY                                
*                                                                               
         RENCBL COLR,0001                                                       
         RENCBL CAS,0002                                                        
         RENCBL PSNE,0003                                                       
         RENCBL BDMX,0004                                                       
         RENCBL APL,0005                                                        
         RENCBL MTV,0006                                                        
         RENCBL NICK,0011                                                       
         RENCBL TNT,0012                                                        
         RENCBL TBSN,0013          TBS NETWORK                                  
         RENCBL TBSC,0013                                                       
         RENCBL HBO,0014                                                        
         RENCBL SHO,0016                                                        
         RENCBL CMAX,0017                                                       
         RENCBL DSC,0025                                                        
         RENCBL DISC,0025                                                       
         RENCBL CNN,0026                                                        
         RENCBL XHIS,0027                                                       
         RENCBL BET,0028                                                        
         RENCBL DSNY,0029                                                       
         RENCBL ESPN,0030                                                       
         RENCBL STRZ,0031                                                       
         RENCBL VH1,0032                                                        
         RENCBL CSPN,0033                                                       
         RENCBL ENC,0034                                                        
         RENCBL TMC,0035                                                        
         RENCBL PLBY,0036                                                       
         RENCBL FX,0037                                                         
         RENCBL RFD,0038           RFD-TV                                       
         RENCBL DOCM,0039                                                       
         RENCBL CMT,0040                                                        
         RENCBL CMTV,0040                                                       
         RENCBL THIT,0041                                                       
         RENCBL GALA,0043                                                       
         RENCBL FLIX,0044                                                       
         RENCBL SUND,0045                                                       
         RENCBL TCM,0046                                                        
         RENCBL TVG,0047                                                        
         RENCBL SV,0048            SPEED CHANNEL                                
         RENCBL GOLF,0049                                                       
         RENCBL VS,0050            VERSUS - NAME CHANGE FROM OLN                
         RENCBL OLN,0050           OUTDOOR LIFE NETWORK                         
         RENCBL NFLN,0051          NFL NETWORK                                  
         RENCBL INSP,0052                                                       
         RENCBL EWTN,0053                                                       
         RENCBL TBNC,0054                                                       
         RENCBL FUEL,0055          FUEL                                         
         RENCBL GAC,0056                                                        
         RENCBL DIY,0057           DO IT YOURSELF                               
         RENCBL FOOD,0058                                                       
         RENCBL HGTV,0059                                                       
         RENCBL SNBC,0060                                                       
         RENCBL OXYG,0061          OXYGEN MEDIA                                 
         RENCBL SOAP,0062                                                       
         RENCBL BOOM,0063                                                       
         RENCBL CART,0064                                                       
         RENCBL TOON,0064                                                       
         RENCBL NKJR,0065          NICK JR                                      
         RENCBL NOGN,0065          NOGGIN                                       
         RENCBL BIO,0066           BIOGRAPHY CHANNEL                            
         RENCBL MIL,0067           MILITARY CHANNEL                             
         RENCBL TRAV,0068                                                       
         RENCBL TRVL,0068          ORIGINAL TRAV                                
         RENCBL TLC,0069                                                        
         RENCBL BLM,0070                                                        
         RENCBL CNBW,0071                                                       
         RENCBL QVC,0073                                                        
         RENCBL MUN2,0074          MUN2                                         
         RENCBL LOGO,0075                                                       
         RENCBL BRVO,0076                                                       
         RENCBL WGNA,0077          WGNA                                         
         RENCBL WGNC,0077          WGNC                                         
         RENCBL XWGN,0077          WGNC (NSI)                                   
         RENCBL GSN,0078           GSN (NEA NAME FOR GAME)                      
         RENCBL GAME,0078                                                       
         RENCBL CLOO,0080          CLOO                                         
         RENCBL SLTH,0080          SLEUTH                                       
         RENCBL MEU,0080                                                        
         RENCBL SYFY,0081                                                       
         RENCBL SCIF,0081                                                       
         RENCBL SPIK,0082          SPIKE,NEW NAME FOR TNN                       
         RENCBL TNN,0082                                                        
         RENCBL SPK,0082           SPIKE (NSI)                                  
         RENCBL CMD,0083                                                        
         RENCBL CMDY,0083                                                       
         RENCBL ENT,0084                                                        
         RENCBL USA,0085                                                        
         RENCBL USAN,0085                                                       
         RENCBL HSN,0086                                                        
         RENCBL HDNT,0087                                                       
         RENCBL HDNM,0088                                                       
         RENCBL UNHD,0089                                                       
         RENCBL LIF,0090                                                        
         RENCBL LMN,0091           LIFETIME MOVIE NETWORK                       
         RENCBL HALL,0092                                                       
         RENCBL ODSY,0092          ORIGINAL HALL                                
         RENCBL NGC,0093           NATIONAL GEOGRAPHIC                          
         RENCBL AMC,0094                                                        
         RENCBL TWC,0095                                                        
         RENCBL HLN,0096                                                        
         RENCBL TOC,0097           THE OUTDOOR CHANNEL                          
         RENCBL TECH,0098          TECH TV                                      
         RENCBL G4,0098            G4, NEW NAME FOR TECH                        
         RENCBL WE,0099                                                         
         RENCBL TENN,0100                                                       
         RENCBL IND,0102                                                        
         RENCBL TVGN,0103          TV GUIDE NETWORK (NEW NAME)                  
         RENCBL TVGC,0103          TV GUIDE CHANNEL                             
         RENCBL PREV,0103          ORIGINAL TVGC                                
         RENCBL AMAX,0104                                                       
         RENCBL TR3S,0105                                                       
         RENCBL 5STR,0106                                                       
         RENCBL HRTV,0107                                                       
         RENCBL DSTN,0108                                                       
         RENCBL JWLT,0109                                                       
         RENCBL MTVC,0110                                                       
         RENCBL DEPE,0111                                                       
         RENCBL BYU,0112                                                        
         RENCBL NBAT,0113          NBA TV                                       
         RENCBL DEPC,0114                                                       
         RENCBL INDI,0115                                                       
         RENCBL TVL,0116           TV LAND                                      
         RENCBL HBOC,0117                                                       
         RENCBL HBOZ,0118                                                       
         RENCBL FAM,0119                                                        
         RENCBL TRU,0120           TRU TV                                       
         RENCBL CRT,0120                                                        
         RENCBL HBOS,0121                                                       
         RENCBL SET,0122                                                        
         RENCBL BBCA,0123          BBC WORLD/AMERICA                            
         RENCBL HITN,0124                                                       
         RENCBL SPMN,0125                                                       
         RENCBL HBOF,0126                                                       
         RENCBL MTV2,0127          MTV2                                         
         RENCBL STYL,0128          STYLE                                        
         RENCBL FXNC,0129                                                       
         RENCBL FMC,0130                                                        
         RENCBL FOXD,0131                                                       
         RENCBL HBO2,0132                                                       
         RENCBL SHO2,0133                                                       
         RENCBL SHOS,0134                                                       
         RENCBL SHOE,0135                                                       
         RENCBL SHOB,0136                                                       
         RENCBL HBOL,0137                                                       
         RENCBL MMAX,0138                                                       
         RENCBL IFC,0139           INDEPENDENT FILM CHANNEL                     
         RENCBL CNBC,0140                                                       
         RENCBL TMCX,0141                                                       
         RENCBL HUB,0142           NEW NAME THE HUB                             
         RENCBL DKID,0142          DISCOVERY KIDS                               
         RENCBL CSP2,0143                                                       
         RENCBL ID,0144            INVESTIGATION DISCOVERY                      
         RENCBL DTMS,0144          DISCOVERY TIMES CHANNEL                      
         RENCBL DSCE,0145          DISCOVERY EN ESPANOL                         
         RENCBL VEL,0146           VELOCITY                                     
         RENCBL HDT,0146           HD THEATER                                   
         RENCBL DHD,0146           DISCOVERY HD THEATER                         
         RENCBL GRN,0147           PLANET GREEN                                 
         RENCBL DSCF,0148                                                       
         RENCBL HCE,0149                                                        
         RENCBL CTRC,0150          CENTRIC                                      
         RENCBL ESP2,0151          ESPN 2                                       
         RENCBL ESX,0151           ESPN 2 (NSI)                                 
         RENCBL ESPC,0152          ESPN CLASSIC                                 
         RENCBL ESCL,0152          ESPN CLASSIC (NSI)                           
         RENCBL ESPD,0153                                                       
         RENCBL ENN,0154           ESPNEWS                                      
         RENCBL ESPU,0155          ESPNU                                        
         RENCBL STZE,0156                                                       
         RENCBL STKF,0157                                                       
         RENCBL STRC,0158                                                       
         RENCBL STZB,0159                                                       
         RENCBL STCM,0160                                                       
         RENCBL ENCA,0161                                                       
         RENCBL ENCL,0162                                                       
         RENCBL ENCD,0163                                                       
         RENCBL ENWS,0164                                                       
         RENCBL CNNE,0165                                                       
         RENCBL RTRO,0166                                                       
         RENCBL H2,0167            H2                                           
*        RENCBL HI,0167            HISTORY INTERNATIONAL                        
         RENCBL CIN,0168                                                        
         RENCBL CINE,0169                                                       
         RENCBL HMC,0170           NEW CALL LETTERS FOR HALLMARK MOVIE          
         RENCBL HLMC,0170                                                       
         RENCBL NKTN,0171          NICK TOONS                                   
         RENCBL FBN,0172           FOX BUSINESS NETWORK                         
         RENCBL CHIL,0173          CHILLER                                      
         RENCBL CBSN,0174                                                       
         RENCBL CRNT,0175          CURRENT TV                                   
         RENCBL PNYO,0176                                                       
         RENCBL TNNK,0177          TEENNICK                                     
         RENCBL THEN,0177          THE N                                        
         RENCBL NOGG,0177                                                       
         RENCBL PDIA,0178                                                       
         RENCBL NTVA,0179                                                       
         RENCBL NTVA,0179                                                       
         RENCBL OVTN,0180          OVATION NETWORK                              
         RENCBL TVE,0181                                                        
         RENCBL PHNA,0182                                                       
         RENCBL PHNW,0183                                                       
         RENCBL MGM,0184                                                        
         RENCBL IONC,0185                                                       
         RENCBL TELC,0186                                                       
         RENCBL UNIC,0187                                                       
         RENCBL AZAC,0188                                                       
         RENCBL TFC,0189                                                        
         RENCBL PBSX,0190                                                       
         RENCBL NHL,0191                                                        
         RENCBL DXD,0192           DISNEY XD (MAR/09)                           
         RENCBL TDSN,0192          TOONY DISNEY - NOW DISNEY XD                 
         RENCBL DOMI,0193                                                       
         RENCBL RLZC,0194                                                       
         RENCBL UTIL,0195                                                       
         RENCBL SHRT,0196                                                       
         RENCBL NFLR,0197                                                       
         RENCBL WFN,0198                                                        
         RENCBL MAV,0199                                                        
         RENCBL CNTC,0200                                                       
         RENCBL EPIX,0201                                                       
         RENCBL CNTR,0202                                                       
         RENCBL NGWD,0203                                                       
         RENCBL CC,0204            COOKING CHANNEL                              
         RENCBL FINE,0204          FINE LIVING                                  
         RENCBL OWN,0205           OPRAH WINFREY NETWORK                        
         RENCBL DHLT,0205          DISCOVERY HEALTH                             
         RENCBL BTN,0206                                                        
         RENCBL ENCS,0207                                                       
         RENCBL ENCM,0207                                                       
         RENCBL ENCF,0208                                                       
         RENCBL ENCW,0208                                                       
         RENCBL SCI,0209           NEW CALL LETTERS FOR SCIENCE CHN             
         RENCBL DSCI,0209          THE SCIENCE CHANNEL                          
         RENCBL NUVO,0210                                                       
         RENCBL SITV,0210                                                       
         RENCBL TCCD,0211                                                       
         RENCBL NBCS,0212                                                       
         RENCBL DAM,0214                                                        
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_RECABCLL                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF RENTRAK CABLE NETWORK NAMES.                               *         
* THIS TABLE DOESN'T HAVE DUPLICATES. IF A NETWORK CHANGED CALL       *         
* LETTERS OR ITS NAME, ONLY THE LATEST CALL LETTERS/NAME SHOULD BE    *         
* INCLUDED FOR A GIVEN NUMERIC CODE.                                  *         
* TABLE IS SORTED BY NUMERIC CODE.                                    *         
***********************************************************************         
TBL_RECABNAM DS 0D                                                              
         DC    CL8'RECABNAM'                                                    
         DS    XL6                                                              
         DC    AL2(RECBNMLQ)       L'TABLE ENTRY                                
*                                                                               
         RENNAM COLR,0001,'CoLours TV'                                          
         RENNAM CAS,0002,'Classic Arts Showcase'                                
         RENNAM PSNE,0003,'Pasiones TV'                                         
         RENNAM BDMX,0004,'Bandamax'                                            
         RENNAM APL,0005,'Animal Planet'                                        
         RENNAM MTV,0006,'Mtv'                                                  
         RENNAM NICK,0011,'Nickelodeon'                                         
         RENNAM TNT,0012,'Turner Network Television'                            
         RENNAM TBSN,0013,'Tbs Network'                                         
         RENNAM HBO,0014,'Home Box Office'                                      
         RENNAM SHO,0016,'Showtime'                                             
         RENNAM CMAX,0017,'Cinemax'                                             
         RENNAM DISC,0025,'The Discovery Channel'                               
         RENNAM CNN,0026,'Cable News Network'                                   
         RENNAM XHIS,0027,'History'                                             
         RENNAM BET,0028,'Black Entertainment Tv'                               
         RENNAM DSNY,0029,'Disney Channel'                                      
         RENNAM ESPN,0030,'Espn'                                                
         RENNAM STRZ,0031,'Starz'                                               
         RENNAM VH1,0032,'Vh1'                                                  
         RENNAM CSPN,0033,'CSPAN'                                               
         RENNAM ENC,0034,'Encore'                                               
         RENNAM TMC,0035,'The Movie Channel'                                    
         RENNAM PLBY,0036,'Playboy'                                             
         RENNAM FX,0037,'Fx'                                                    
         RENNAM RFD,0038,'Rfd-Tv'                                               
         RENNAM DOCM,0039,'The Documentary Channel'                             
         RENNAM CMT,0040,'Cmt'                                                  
         RENNAM THIT,0041,'Telehit'                                             
         RENNAM SUR,0042,'SUR - TV / Canal'                                     
         RENNAM GALA,0043,'Galavision'                                          
         RENNAM FLIX,0044,'Flix'                                                
         RENNAM SUND,0045,'Sundance Channel'                                    
         RENNAM TCM,0046,'Turner Classic Movies'                                
         RENNAM TVG,0047,'TVG Network / Horse Racing'                           
         RENNAM SV,0048,'Speed'                                                 
         RENNAM GOLF,0049,'Golf Channel'                                        
         RENNAM VS,0050,'Versus'                                                
         RENNAM NFLN,0051,'Nfl Network'                                         
         RENNAM INSP,0052,'Insp'                                                
         RENNAM EWTN,0053,'Eternal Word TV Net'                                 
         RENNAM TBNC,0054,'Trinity Bcasting Net (Cable)'                        
         RENNAM FUEL,0055,'Fuel'                                                
         RENNAM GAC,0056,'Great American Country'                               
         RENNAM DIY,0057,'DIY Network'                                          
         RENNAM FOOD,0058,'Food Network'                                        
         RENNAM HGTV,0059,'Home And Garden Tv'                                  
         RENNAM SNBC,0060,'Shop NBC (Cable)'                                    
         RENNAM OXYG,0061,'Oxygen Media'                                        
         RENNAM SOAP,0062,'Soapnet'                                             
         RENNAM BOOM,0063,'Boomerang'                                           
         RENNAM TOON,0064,'The Cartoon Network'                                 
         RENNAM NKJR,0065,'Nick Jr'                                             
         RENNAM BIO,0066,'Biography Channel'                                    
         RENNAM MIL,0067,'Military Channel'                                     
         RENNAM TRAV,0068,'The Travel Channel'                                  
         RENNAM TLC,0069,'The Learning Channel'                                 
         RENNAM BLM,0070,'Bloomberg Television'                                 
         RENNAM CNBW,0071,'CNBC World'                                          
         RENNAM MSNB,0072,'Msnbc'                                               
         RENNAM QVC,0073,'Qvc'                                                  
         RENNAM MUN2,0074,'Mun2'                                                
         RENNAM LOGO,0075,'Logo'                                                
         RENNAM BRVO,0076,'Bravo'                                               
         RENNAM WGNA,0077,'Wgn America'                                         
         RENNAM GSN,0078,'Gsn'                                                  
         RENNAM AEN,0079,'A&&E Network'                                         
         RENNAM CLOO,0080,'Cloo'                                                
         RENNAM SYFY,0081,'Syfy'                                                
         RENNAM SPIK,0082,'Spike Tv'                                            
         RENNAM CMD,0083,'Comedy Central'                                       
         RENNAM ENT,0084,'E! Entertainment Tv'                                  
         RENNAM USA,0085,'Usa Network'                                          
         RENNAM HSN,0086,'HSN (Cable)'                                          
         RENNAM HDNT,0087,'HDNet'                                               
         RENNAM HDNM,0088,'HDNet Movies'                                        
         RENNAM UNHD,0089,'Universal HD'                                        
         RENNAM LIF,0090,'Lifetime Television'                                  
         RENNAM LMN,0091,'Lifetime Movie Network'                               
         RENNAM HALL,0092,'Hallmark'                                            
         RENNAM NGC,0093,'National Geographic Channel'                          
         RENNAM AMC,0094,'American Movie Classics'                              
         RENNAM TWC,0095,'The Weather Channel'                                  
         RENNAM HLN,0096,'HLN'                                                  
         RENNAM TOC,0097,'The Outdoor Channel'                                  
         RENNAM G4,0098,'G4'                                                    
         RENNAM WE,0099,'Womens Entertainment'                                  
         RENNAM TENN,0100,'Tennis Channel'                                      
         RENNAM IND,0102,'Independent'                                          
         RENNAM TVGN,0103,'Tv Guide Network'                                    
         RENNAM AMAX,0104,'Action Max'                                          
         RENNAM TR3S,0105,'Tr3s'                                                
         RENNAM 5STR,0106,'5StarMAX'                                            
         RENNAM HRTV,0107,'Horse Racing Television'                             
         RENNAM DSTN,0108,'DayStar TV (Cable)'                                  
         RENNAM JWLT,0109,'Jewelry Television (Cable)'                          
         RENNAM MTVC,0110,'MTV Classic'                                         
         RENNAM DEPE,0111,'De Pelicula'                                         
         RENNAM BYU,0112,'BYU Television'                                       
         RENNAM NBAT,0113,'Nba Tv'                                              
         RENNAM DEPC,0114,'De Pelicula Clasico'                                 
         RENNAM INDI,0115,'IndiePlex'                                           
         RENNAM TVL,0116,'Tv Land'                                              
         RENNAM HBOC,0117,'HBO Comedy'                                          
         RENNAM HBOZ,0118,'HBO Zone'                                            
         RENNAM FAM,0119,'Abc Family'                                           
         RENNAM TRU,0120,'TruTv'                                                
         RENNAM HBOS,0121,'HBO Signature'                                       
         RENNAM SET,0122,'Sony Ent TV Asia'                                     
         RENNAM BBCA,0123,'Bbc World/America'                                   
         RENNAM HITN,0124,'HITN TV'                                             
         RENNAM SPMN,0125,'Sportsman Channel'                                   
         RENNAM HBOF,0126,'HBO Family'                                          
         RENNAM MTV2,0127,'Mtv2'                                                
         RENNAM STYL,0128,'Style'                                               
         RENNAM FXNC,0129,'Fox News Channel'                                    
         RENNAM FMC,0130,'Fox Movie Channel'                                    
         RENNAM FOXD,0131,'FOX Deportes'                                        
         RENNAM HBO2,0132,'HBO2'                                                
         RENNAM SHO2,0133,'Showtime Too'                                        
         RENNAM SHOS,0134,'Showtime Showcase'                                   
         RENNAM SHOE,0135,'Showtime Extreme'                                    
         RENNAM SHOB,0136,'Showtime Beyond'                                     
         RENNAM HBOL,0137,'HBO Latino'                                          
         RENNAM MMAX,0138,'MoreMax'                                             
         RENNAM IFC,0139,'Independent Film Channel'                             
         RENNAM CNBC,0140,'Cnbc'                                                
         RENNAM TMCX,0141,'The Movie Channel Xtra'                              
         RENNAM HUB,0142,'The Hub'                                              
         RENNAM CSP2,0143,'CSPAN 2'                                             
         RENNAM ID,0144,'Investigation Discovery'                               
         RENNAM DSCE,0145,'Discovery En Espanol'                                
         RENNAM VEL,0146,'Velocity'                                             
         RENNAM GRN,0147,'Planet Green'                                         
         RENNAM DSCF,0148,'Discovery Familia'                                   
         RENNAM HCE,0149,'History Channel en Espanol'                           
         RENNAM CTRC,0150,'Centric'                                             
         RENNAM ESP2,0151,'Espn2'                                               
         RENNAM ESPC,0152,'Espn Classic'                                        
         RENNAM ESPD,0153,'ESPN Deportes'                                       
         RENNAM ENN,0154,'Esp News'                                             
         RENNAM ESPU,0155,'Espnu'                                               
         RENNAM STZE,0156,'Starz Edge'                                          
         RENNAM STKF,0157,'Starz Kids && Family'                                
         RENNAM STZC,0158,'Starz Cinema'                                        
         RENNAM STZB,0159,'Starz InBlack'                                       
         RENNAM STCM,0160,'Starz Comedy'                                        
         RENNAM ENCA,0161,'Encore Action'                                       
         RENNAM ENCL,0162,'Encore Love'                                         
         RENNAM ENCD,0163,'Encore Drama'                                        
         RENNAM ENWS,0164,'Encore Westerns'                                     
         RENNAM CNNE,0165,'CNN ESPANOL'                                         
         RENNAM RTRO,0166,'RetroPlex'                                           
         RENNAM H2,0167,'H2'                                                    
         RENNAM CIN,0168,'Crime && Investigation Network'                       
         RENNAM CINE,0169,'Cinelatino (Espanol)'                                
         RENNAM HMC,0170,'Hallmark Movie Channel'                               
         RENNAM NKTN,0171,'Nicktoons'                                           
         RENNAM FBN,0172,'Fox Business Network'                                 
         RENNAM CHIL,0173,'Chiller'                                             
         RENNAM CBSN,0174,'CBS Sports Network'                                  
         RENNAM CRNT,0175,'Current Tv'                                          
         RENNAM PNYO,0176,'GMA Pinoy'                                           
         RENNAM TNNK,0177,'TeenNick'                                            
         RENNAM PDIA,0178,'Palladia'                                            
         RENNAM NTVA,0179,'NTV America'                                         
         RENNAM OVTN,0180,'Ovation Network'                                     
         RENNAM TVE,0181,'TVE Internacional'                                    
         RENNAM PHNA,0182,'Phoenix NA Chinese Channel'                          
         RENNAM PHNW,0183,'Phoenix InfoNews'                                    
         RENNAM MGM,0184,'MGM'                                                  
         RENNAM IONC,0185,'ION (Cable)'                                         
         RENNAM TELC,0186,'Telemundo (Cable)'                                   
         RENNAM UNIC,0187,'Univision (Cable)'                                   
         RENNAM AZAC,0188,'Azteca America(Cable)'                               
         RENNAM TFC,0189,'Telefutura (Cable)'                                   
         RENNAM PBSX,0190,'PBS (National)'                                      
         RENNAM NHL,0191,'NHL Network'                                          
         RENNAM DXD,0192,'Disney XD'                                            
         RENNAM DOMI,0193,'Television Dominicana'                               
         RENNAM RLZC,0194,'ReelzChannel'                                        
         RENNAM UTIL,0195,'Utilisima - TV / Canal'                              
         RENNAM SHRT,0196,'Shorts'                                              
         RENNAM NFLR,0197,'NFL RedZone'                                         
         RENNAM WFN,0198,'World Fishing Network'                                
         RENNAM MAV,0199,'MAV TV'                                               
         RENNAM CTNC,0200,'Christian TV Net (Cable)'                            
         RENNAM EPIX,0201,'EPIX'                                                
         RENNAM CNTR,0202,'Centroamerica TV'                                    
         RENNAM NGWD,0203,'Nat Geo Wild'                                        
         RENNAM CC,0204,'Cooking Channel'                                       
         RENNAM OWN,0205,'Oprah Winfrey Network'                                
         RENNAM BTN,0206,'Big Ten Network'                                      
         RENNAM ENCS,0207,'Encore Suspense'                                     
         RENNAM ENCF,0208,'Encore Family'                                       
         RENNAM SCI,0209,'Science'                                              
         RENNAM NUVO,0210,'NuvoTv'                                              
         RENNAM TCCD,0211,'The Church Channel'                                  
         RENNAM NBCS,0212,'Nbc Sports Network'                                  
         RENNAM DAM,0214,'Destination America'                                  
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_RECABNAM                                                 
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ARBITRON DIGITAL CALL LETTERS TRANSLATIONS                                    
* WE ARE CHANGING THE FIRST CALL LETTER OF EVERY DIGITAL STATION                
* TO A PRE-DEFINED LETTER, A DIFFERENT ONE FOR EAST COAST AND WEST.             
*******************************************************************             
TBL_ARDNTRN DS 0D                                                               
         DC    CL8'ARDNTRN'                                                     
         DS    XL6                                                              
         DC    AL2(ARDNTRNQ)       L'TABLE ENTRY                                
*----------------|BAND|--|EAST|-|WEST|---|DESCRIPTION|                          
         DC    CL2'FM',CL1'W',CL1'K',CL2'FM',CL30'FM'                           
         DC    CL2'AM',CL1'W',CL1'K',CL2'AM',CL30'AM'                           
         DC    CL2'IF',CL1'I',CL1'R',CL2'FM',CL30'Internet Stream'              
         DC    CL2'IA',CL1'I',CL1'R',CL2'AM',CL30'Internet Stream'              
         DC    CL2'G2',CL1'J',CL1'U',CL2'FM',CL30'Internet Multicast'           
         DC    CL2'B2',CL1'J',CL1'U',CL2'AM',CL30'Internet Multicast'           
         DC    CL2'HF',CL1'A',CL1'L',CL2'FM',CL30'HD Primary'                   
         DC    CL2'HA',CL1'A',CL1'L',CL2'AM',CL30'HD Primary'                   
         DC    CL2'F2',CL1'B',CL1'N',CL2'FM',CL30'HD Secondary'                 
         DC    CL2'A2',CL1'B',CL1'N',CL2'AM',CL30'HD Secondary'                 
         DC    CL2'F3',CL1'D',CL1'O',CL2'FM',CL30'HD Tertiary'                  
         DC    CL2'A3',CL1'D',CL1'O',CL2'AM',CL30'HD Tertiary'                  
         DC    CL2'F4',CL1'E',CL1'Q',CL2'FM',CL30'HD Tertiary'                  
         DC    CL2'A4',CL1'E',CL1'Q',CL2'AM',CL30'HD Tertiary'                  
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         TABLE_LEN TBL_ARDNTRN                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TABLE OF BOOK/MARKET COMBINATIONS USED IN A CUSTOM DEMOFILE FOR FQA.          
* THESE SURVEYS HAVE BEEN IDENTIFIED AS BEING USED IN FQA REGRESSION            
* TESTING, SO WE MUST KEEP THEM AROUND TO PREVENT FQA SCRIPTS FROM              
* FAILING (OR PRODUCING DIFFERENT BASELINE RESULTS).                            
*                                                                               
* THESE TABLE ENTRIES MUST BE SORTED BY BOOK,MARKET. IT IS SEARCHABLE           
* VIA BINSRCH. NOTE THE STRUCTURE OF THE 16-BYTE TABLE HEADER:                  
*                                                                               
*   CL8 EYE-CATCHER                                                             
*   XL4 SPARE                                                                   
*   AL2 # OF TABLE ENTRIES (FOR BINSRCH)                                        
*   AL2 L'TABLE ENTRY (FOR BINSRCH)                                             
***********************************************************************         
TBL_FQABOOKS DS 0D                                                              
         DC    CL8'FQABOOKS'                                                    
         DC    XL4'00'                                                          
         DC    AL2(TBL_FQABOOKS_#ENTRIES)   FOR BINSRCH: # OF ENTRIES           
         DC    AL2(FQABOOKQ)                L'TABLE ENTRY                       
*                                                                               
** NOTE: BOOKS PRIOR TO THE YEAR 2000 ARE REJECTED BY BOOKVAL. IF SUCH          
**       SURVEYS ARE REFERENCED IN ANY FQA SCRIPTS, THEN THE SCRIPTS            
**       MUST BE CHANGED.                                                       
*********DC    AL2(YM_1995_07,111)   WASHINGTON, DC                             
*********DC    AL2(YM_1998_07,101)   NEW YORK                                   
*********DC    AL2(YM_1999_02,105)   DETROIT                                    
*********DC    AL2(YM_1999_02,403)   LOS ANGELES                                
         DC    AL2(YM_2000_07,176)   SALISBURY                                  
         DC    AL2(YM_2001_10,168)   ATLANTA                                    
         DC    AL2(YM_2001_10,419)   SEATTLE-TACOMA                             
         DC    AL2(YM_2001_11,347)   JUNEAU                                     
         DC    AL2(YM_2003_11,101)   NEW YORK                                   
         DC    AL2(YM_2003_11,202)   CHICAGO                                    
         DC    AL2(YM_2004_01,101)   NEW YORK                                   
         DC    AL2(YM_2004_02,101)   NEW YORK                                   
         DC    AL2(YM_2005_05,168)   ATLANTA                                    
         DC    AL2(YM_2005_06,101)   NEW YORK                                   
         DC    AL2(YM_2005_07,101)   NEW YORK                                   
         DC    AL2(YM_2005_08,101)   NEW YORK                                   
         DC    AL2(YM_2005_09,101)   NEW YORK                                   
         DC    AL2(YM_2005_10,101)   NEW YORK                                   
         DC    AL2(YM_2005_11,101)   NEW YORK                                   
         DC    AL2(YM_2005_11,209)   ST. LOUIS                                  
         DC    AL2(YM_2005_12,101)   NEW YORK                                   
         DC    AL2(YM_2006_01,101)   NEW YORK                                   
         DC    AL2(YM_2006_02,101)   NEW YORK                                   
         DC    AL2(YM_2006_02,353)   PHOENIX                                    
         DC    AL2(YM_2006_03,101)   NEW YORK                                   
         DC    AL2(YM_2006_04,101)   NEW YORK                                   
         DC    AL2(YM_2006_07,137)   BANGOR                                     
         DC    AL2(YM_2006_09,101)   NEW YORK                                   
         DC    AL2(YM_2006_10,101)   NEW YORK                                   
         DC    AL2(YM_2006_10,168)   ATLANTA                                    
         DC    AL2(YM_2006_10,209)   ST. LOUIS                                  
         DC    AL2(YM_2006_11,101)   NEW YORK                                   
         DC    AL2(YM_2006_11,106)   BOSTON                                     
         DC    AL2(YM_2006_11,168)   ATLANTA                                    
         DC    AL2(YM_2006_11,176)   SALISBURY                                  
         DC    AL2(YM_2006_11,202)   CHICAGO                                    
         DC    AL2(YM_2006_11,209)   ST. LOUIS                                  
         DC    AL2(YM_2006_11,353)   PHOENIX                                    
         DC    AL2(YM_2006_11,400)   BAKERSFIELD                                
         DC    AL2(YM_2006_11,403)   LOS ANGELES                                
         DC    AL2(YM_2006_12,101)   NEW YORK                                   
         DC    AL2(YM_2006_12,168)   ATLANTA                                    
         DC    AL2(YM_2007_01,101)   NEW YORK                                   
         DC    AL2(YM_2007_01,209)   ST. LOUIS                                  
         DC    AL2(YM_2007_02,101)   NEW YORK                                   
         DC    AL2(YM_2007_02,106)   BOSTON                                     
         DC    AL2(YM_2007_02,202)   CHICAGO                                    
         DC    AL2(YM_2007_02,353)   PHOENIX                                    
         DC    AL2(YM_2007_02,403)   LOS ANGELES                                
         DC    AL2(YM_2007_03,101)   NEW YORK                                   
         DC    AL2(YM_2007_04,101)   NEW YORK                                   
         DC    AL2(YM_2007_05,101)   NEW YORK                                   
         DC    AL2(YM_2007_05,106)   BOSTON                                     
         DC    AL2(YM_2007_05,168)   ATLANTA                                    
         DC    AL2(YM_2007_05,202)   CHICAGO                                    
         DC    AL2(YM_2007_05,209)   ST. LOUIS                                  
         DC    AL2(YM_2007_05,400)   BAKERSFIELD                                
         DC    AL2(YM_2007_05,403)   LOS ANGELES                                
         DC    AL2(YM_2007_06,101)   NEW YORK                                   
         DC    AL2(YM_2007_06,168)   ATLANTA                                    
         DC    AL2(YM_2007_07,101)   NEW YORK                                   
         DC    AL2(YM_2007_07,168)   ATLANTA                                    
         DC    AL2(YM_2007_07,202)   CHICAGO                                    
         DC    AL2(YM_2007_08,101)   NEW YORK                                   
         DC    AL2(YM_2007_09,101)   NEW YORK                                   
         DC    AL2(YM_2007_09,106)   BOSTON                                     
         DC    AL2(YM_2007_10,101)   NEW YORK                                   
         DC    AL2(YM_2007_10,106)   BOSTON                                     
         DC    AL2(YM_2007_10,202)   CHICAGO                                    
         DC    AL2(YM_2007_11,101)   NEW YORK                                   
         DC    AL2(YM_2007_11,202)   CHICAGO                                    
         DC    AL2(YM_2007_11,209)   ST. LOUIS                                  
         DC    AL2(YM_2007_11,353)   PHOENIX                                    
         DC    AL2(YM_2007_11,400)   BAKERSFIELD                                
         DC    AL2(YM_2007_11,403)   LOS ANGELES                                
         DC    AL2(YM_2007_12,101)   NEW YORK                                   
         DC    AL2(YM_2007_12,202)   CHICAGO                                    
         DC    AL2(YM_2008_01,101)   NEW YORK                                   
         DC    AL2(YM_2008_02,101)   NEW YORK                                   
         DC    AL2(YM_2008_02,168)   ATLANTA                                    
         DC    AL2(YM_2008_02,217)   MILWAUKEE                                  
         DC    AL2(YM_2008_02,403)   LOS ANGELES                                
         DC    AL2(YM_2008_03,101)   NEW YORK                                   
         DC    AL2(YM_2008_04,101)   NEW YORK                                   
         DC    AL2(YM_2008_04,223)   DALLAS-FT. WORTH                           
         DC    AL2(YM_2008_05,101)   NEW YORK                                   
         DC    AL2(YM_2008_05,133)   HARTFORD & NEW HAVEN                       
         DC    AL2(YM_2008_05,217)   MILWAUKEE                                  
         DC    AL2(YM_2008_05,403)   LOS ANGELES                                
         DC    AL2(YM_2008_06,101)   NEW YORK                                   
         DC    AL2(YM_2008_06,106)   BOSTON                                     
         DC    AL2(YM_2008_06,168)   ATLANTA                                    
         DC    AL2(YM_2008_06,403)   LOS ANGELES                                
         DC    AL2(YM_2008_07,101)   NEW YORK                                   
         DC    AL2(YM_2008_07,403)   LOS ANGELES                                
         DC    AL2(YM_2008_08,101)   NEW YORK                                   
         DC    AL2(YM_2008_09,101)   NEW YORK                                   
         DC    AL2(YM_2008_09,106)   BOSTON                                     
         DC    AL2(YM_2008_10,101)   NEW YORK                                   
         DC    AL2(YM_2008_10,106)   BOSTON                                     
         DC    AL2(YM_2008_11,106)   BOSTON                                     
         DC    AL2(YM_2008_11,168)   ATLANTA                                    
         DC    AL2(YM_2009_01,101)   NEW YORK                                   
         DC    AL2(YM_2009_02,101)   NEW YORK                                   
         DC    AL2(YM_2009_02,104)   PHILADELPHIA                               
         DC    AL2(YM_2009_02,403)   LOS ANGELES                                
         DC    AL2(YM_2009_03,101)   NEW YORK                                   
         DC    AL2(YM_2009_04,101)   NEW YORK                                   
         DC    AL2(YM_2010_11,101)   NEW YORK                                   
         DC    AL2(YM_2010_12,101)   NEW YORK                                   
         DC    AL2(YM_2011_01,101)   NEW YORK                                   
         DC    AL2(YM_2011_02,101)   NEW YORK                                   
         DC    AL2(YM_2011_09,101)   NEW YORK                                   
         DC    AL2(YM_2012_03,101)   NEW YORK                                   
         DC    AL2(YM_2012_12,101)   NEW YORK                                   
         DC    AL2(YM_2014_01,101)   NEW YORK                                   
         DC    AL2(YM_2014_02,101)   NEW YORK (+ WINTER/2014 OLYMPICS)          
         DC    AL2(YM_2014_11,101)   NEW YORK                                   
*&&DO                                                                           
** THE ENTRIES BELOW ARE REALLY ONLY FOR DOCUMENTATION PURPOSES.                
** WE DON'T NEED TABLE ENTRIES FOR THE BOOKS THAT FQA IS READING IF             
** THEY ARE RECENT ENOUGH THAT THEY AREN'T AT RISK OF BEING PURGED.             
         DC    AL2(YM_2015_03,101)                                              
         DC    AL2(YM_2015_07,101)                                              
         DC    AL2(YM_2015_09,101)                                              
         DC    AL2(YM_2015_11,101)                                              
         DC    AL2(YM_2015_11,202)                                              
         DC    AL2(YM_2016_02,101)                                              
         DC    AL2(YM_2016_02,202)                                              
         DC    AL2(YM_2016_03,101)                                              
         DC    AL2(YM_2016_04,101)                                              
         DC    AL2(YM_2016_04,168)                                              
         DC    AL2(YM_2016_04,403)                                              
         DC    AL2(YM_2016_05,101)                                              
         DC    AL2(YM_2016_05,168)                                              
         DC    AL2(YM_2016_05,183)                                              
         DC    AL2(YM_2016_05,233)                                              
         DC    AL2(YM_2016_06,101)                                              
         DC    AL2(YM_2016_06,202)                                              
         DC    AL2(YM_2016_07,101)                                              
         DC    AL2(YM_2016_07,168)                                              
         DC    AL2(YM_2016_07,183)                                              
         DC    AL2(YM_2016_08,101) NOTE: ALSO SUMMER/2016 OLYMPICS              
         DC    AL2(YM_2016_09,101)                                              
         DC    AL2(YM_2016_09,168)                                              
         DC    AL2(YM_2016_09,202)                                              
         DC    AL2(YM_2016_10,101)                                              
         DC    AL2(YM_2016_10,168)                                              
         DC    AL2(YM_2016_11,101)                                              
         DC    AL2(YM_2016_11,168)                                              
         DC    AL2(YM_2016_11,282)                                              
         DC    AL2(YM_2016_12,101)                                              
         DC    AL2(YM_2017_01,101)                                              
         DC    AL2(YM_2017_01,168)                                              
         DC    AL2(YM_2017_01,403)                                              
         DC    AL2(YM_2017_02,101)                                              
         DC    AL2(YM_2017_02,168)                                              
         DC    AL2(YM_2017_03,101)                                              
         DC    AL2(YM_2017_03,168)                                              
         DC    AL2(YM_2017_04,168)                                              
         DC    AL2(YM_2017_05,101)                                              
         DC    AL2(YM_2017_05,133)                                              
         DC    AL2(YM_2017_05,168)                                              
         DC    AL2(YM_2017_05,403)                                              
         DC    AL2(YM_2017_06,168)                                              
         DC    AL2(YM_2017_07,101)                                              
         DC    AL2(YM_2017_07,202)                                              
         DC    AL2(YM_2017_09,403)                                              
         DC    AL2(YM_2017_10,101)                                              
         DC    AL2(YM_2017_10,168)                                              
         DC    AL2(YM_2017_10,202)                                              
         DC    AL2(YM_2017_11,101)                                              
         DC    AL2(YM_2017_11,168)                                              
         DC    AL2(YM_2017_11,176)                                              
         DC    AL2(YM_2017_11,282)                                              
         DC    AL2(YM_2017_12,101)                                              
         DC    AL2(YM_2017_12,168)                                              
         DC    AL2(YM_2017_12,213)                                              
         DC    AL2(YM_2017_12,419)                                              
         DC    AL2(YM_2018_01,101)                                              
         DC    AL2(YM_2018_01,168)                                              
         DC    AL2(YM_2018_01,213)                                              
         DC    AL2(YM_2018_01,419)                                              
         DC    AL2(YM_2018_02,101) NOTE: ALSO WINTER/2018 OLYMPICS              
         DC    AL2(YM_2018_02,213) NOTE: ALSO WINTER/2018 OLYMPICS              
         DC    AL2(YM_2018_02,419) NOTE: ALSO WINTER/2018 OLYMPICS              
         DC    AL2(YM_2018_03,101)                                              
         DC    AL2(YM_2018_03,213)                                              
         DC    AL2(YM_2018_03,419)                                              
         DC    AL2(YM_2018_04,101)                                              
         DC    AL2(YM_2018_04,168)                                              
         DC    AL2(YM_2018_04,213)                                              
         DC    AL2(YM_2018_04,419)                                              
         DC    AL2(YM_2018_05,101)                                              
         DC    AL2(YM_2018_06,101)                                              
         DC    AL2(YM_2018_06,168)                                              
         DC    AL2(YM_2018_06,403)                                              
         DC    AL2(YM_2018_07,101)                                              
         DC    AL2(YM_2018_07,168)                                              
         DC    AL2(YM_2018_08,101)                                              
         DC    AL2(YM_2018_08,168)                                              
         DC    AL2(YM_2018_08,282)                                              
         DC    AL2(YM_2018_08,309)                                              
         DC    AL2(YM_2018_08,403)                                              
         DC    AL2(YM_2018_09,101)                                              
         DC    AL2(YM_2018_09,168)                                              
         DC    AL2(YM_2018_09,403)                                              
         DC    AL2(YM_2018_10,101)                                              
         DC    AL2(YM_2018_10,168)                                              
         DC    AL2(YM_2018_10,403)                                              
*&&                                                                             
TBL_FQABOOKS_#ENTRIES EQU (*-(TBL_FQABOOKS+16))/FQABOOKQ                        
*                                                                               
* THIS EOT MARKER REALLY SHOULDN'T BE NEEDED, BECAUSE THIS TABLE IS             
* SEARCHABLE VIA BINSRCH. BUT AS A PRECAUTION, THE EOT MARKER IS                
* PROVIDED (IN CASE WE NEED TO DO A LINEAR SEARCH FOR SOME REASON).             
*                                                                               
         DC    X'FFFFFFFF'         EOT                                          
         SPACE 2                                                                
         TABLE_LEN TBL_FQABOOKS                                                 
         EJECT                                                                  
****************************************************************                
* TABLE OF PPM BOOK FOR ARBITRON                                                
* MARKET AND FIRST LIVE BOOK , PRELIMINARY START AND END BOOK                   
****************************************************************                
TBL_CDMTAB DS 0D                                                                
         DC    CL8'*CDMTAB*'                                                    
         DS    XL6                                                              
         DC    AL2(CDMTABQ)        L'TABLE ENTRY                                
* CONTINUOUS AUDIO MARKETS - WE TREAT LIKE PPM MARKETS AS WE                    
* GET SURVEYS EVERY MONTH                                                       
         DC    AL2(081,JUL_19),CL3'AKR',AL1(3),AL2(0)                           
         DC    AL2(069,JUL_19),CL3'ALB',AL1(3),AL2(0)                           
         DC    AL2(141,JUL_19),CL3'ABQ',AL1(3),AL2(0)                           
         DC    AL2(145,JUL_19),CL3'ALL',AL1(3),AL2(0)                           
         DC    AL2(143,JUL_19),CL3'BAK',AL1(3),AL2(0)                           
         DC    AL2(223,JUL_19),CL3'BAT',AL1(3),AL2(0)                           
         DC    AL2(095,JUL_19),CL3'BIR',AL1(3),AL2(0)                           
         DC    AL2(037,JUL_19),CL3'BUF',AL1(3),AL2(0)                           
         DC    AL2(231,JUL_19),CL3'CRL',AL1(3),AL2(0)                           
         DC    AL2(181,JUL_19),CL3'CKA',AL1(3),AL2(0)                           
         DC    AL2(233,JUL_19),CL3'CSP',AL1(3),AL2(0)                           
         DC    AL2(183,JUL_19),CL3'CSC',AL1(3),AL2(0)                           
         DC    AL2(067,JUL_19),CL3'DAY',AL1(3),AL2(0)                           
         DC    AL2(071,JUL_19),CL3'DMO',AL1(3),AL2(0)                           
         DC    AL2(161,JUL_19),CL3'ELP',AL1(3),AL2(0)                           
         DC    AL2(089,JUL_19),CL3'FRS',AL1(3),AL2(0)                           
         DC    AL2(515,JUL_19),CL3'FM ',AL1(3),AL2(0)                           
         DC    AL2(127,JUL_19),CL3'GRP',AL1(3),AL2(0)                           
         DC    AL2(361,JUL_19),CL3'GNC',AL1(3),AL2(0)                           
         DC    AL2(191,JUL_19),CL3'GVS',AL1(3),AL2(0)                           
         DC    AL2(119,JUL_19),CL3'HRS',AL1(3),AL2(0)                           
         DC    AL2(099,JUL_19),CL3'HON',AL1(3),AL2(0)                           
         DC    AL2(327,JUL_19),CL3'HNT',AL1(3),AL2(0)                           
         DC    AL2(169,JUL_19),CL3'JMS',AL1(3),AL2(0)                           
****     DC    AL2(345,JUL_19),CL3'JOH',AL1(3),AL2(0)                           
         DC    AL2(121,JUL_19),CL3'KNX',AL1(3),AL2(0)                           
         DC    AL2(123,JUL_19),CL3'LRK',AL1(3),AL2(0)                           
         DC    AL2(055,JUL_19),CL3'LOU',AL1(3),AL2(0)                           
         DC    AL2(171,JUL_19),CL3'MAD',AL1(3),AL2(0)                           
         DC    AL2(133,JUL_19),CL3'MOB',AL1(3),AL2(0)                           
         DC    AL2(283,JUL_19),CL3'SAL',AL1(3),AL2(0)                           
         DC    AL2(053,JUL_19),CL3'NOL',AL1(3),AL2(0)                           
         DC    AL2(083,JUL_19),CL3'OKC',AL1(3),AL2(0)                           
         DC    AL2(085,JUL_19),CL3'OMH',AL1(3),AL2(0)                           
         DC    AL2(540,JUL_19),CL3'MNT',AL1(3),AL2(0)                           
         DC    AL2(105,JUL_19),CL3'RCH',AL1(3),AL2(0)                           
         DC    AL2(079,JUL_19),CL3'ROH',AL1(3),AL2(0)                           
         DC    AL2(111,JUL_19),CL3'SVP',AL1(3),AL2(DEC_19)                      
         DC    AL2(177,JUL_19),CL3'SPK',AL1(3),AL2(0)                           
         DC    AL2(117,JUL_19),CL3'SCH',AL1(3),AL2(0)                           
         DC    AL2(091,JUL_19),CL3'SYR',AL1(3),AL2(0)                           
         DC    AL2(097,JUL_19),CL3'TOL',AL1(3),AL2(0)                           
         DC    AL2(207,JUL_19),CL3'TUC',AL1(3),AL2(0)                           
         DC    AL2(103,JUL_19),CL3'TUL',AL1(3),AL2(0)                           
         DC    AL2(125,JUL_19),CL3'WIT',AL1(3),AL2(0)                           
         DC    AL2(175,JUL_19),CL3'SCR',AL1(3),AL2(0)                           
         DC    AL2(301,JUL_19),CL3'YRK',AL1(3),AL2(DEC_19)                      
         DC    AL2(189,JUN_20),CL3'MFC',AL1(3),AL2(0)                           
         DC    X'FFFF'                                                          
         SPACE 2                                                                
         TABLE_LEN TBL_CDMTAB                                                   
*                                                                               
***********************************************************************         
                                                                                
         ORG   DEMTABS+(((*-DEMTABS)/1024)+1)*1024                              
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         SPACE 3                                                                
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097DEDEMTABS 03/05/21'                                      
         END                                                                    
