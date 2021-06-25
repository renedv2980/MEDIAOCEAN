*          DATA SET SRWKZ02    AT LEVEL 005 AS OF 03/04/16                      
*PHASE T13B02A                                                                  
         TITLE '$WKZ - FILE FUNCTIONS'                                          
         PRINT NOGEN                                                            
WKRPRT   CSECT                                                                  
         NMOD1 000,**$NW2**,RA,R9,R8,RR=R4                                      
         LR    RC,R1                                                            
         USING WKZWKD,RC           RC=A(ROOTS WORKING STORAGE)                  
         ST    RD,PHASERD          SAVE THIS RD                                 
         ST    R4,RELO                                                          
         L     R1,APARM                                                         
         L     R2,12(R1)                                                        
         USING COMFACSD,R2         R2=A(COM FAC LIST)                           
         L     R3,20(R1)                                                        
         USING SRWKZFFD,R3         R3=A(TWA)                                    
         L     R2,ASAVESTR                                                      
         USING WKSAVED,R2                                                       
*                                                                               
         TM    INTFLAG,INTRUN      INTERNAL                                     
         BO    CON000              YES - IGNORE P3/P4 VALIDATION                
                                                                                
*************************************************************                   
*        VALIDATE FILE STAT AND ATTRIBUTES                  *                   
*************************************************************                   
P3VAL    DS    0H                  P3=FILE STAT AND ATTRIBUTES                  
         MVI   FILSTAT,0                                                        
         MVI   FILATTB,0                                                        
         LA    R4,SRVP3H                                                        
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    P3VX                                                             
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(4,(R6))                                      
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0               R0=NUMBER OF INPUT FIELDS                    
         BZ    ERR2                                                             
*                                                                               
P3V1     CLI   1(R6),0             EACH FLD HAS MIN OF 3 CHRS                   
         BNE   ERR2                                                             
         CLI   0(R6),3                                                          
         BL    ERR2                                                             
         CLI   0(R6),8                                                          
         BH    ERR2                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         LA    RE,STATTBL                                                       
*                                                                               
P3V2     CLI   0(RE),0             SEARCH FILE STAT TABLE                       
         BE    P3V4                                                             
         EX    0,0(RE)             RF =A(KEYWORD)                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P3V3                                                             
         LA    RE,L'STATTBL(RE)                                                 
         B     P3V2                                                             
*                                                                               
P3V3     IC    R1,4(RE)            TEST FOR DUPLICATE BIT VALUE                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    FILSTAT,0                                                        
         BNZ   ERR3                                                             
         OC    FILSTAT,4(RE)                                                    
         B     P3V8                                                             
*                                                                               
P3V4     LA    RE,ATTBTBL          SEARCH FILE ATTB TABLE                       
P3V5     CLI   0(RE),0                                                          
         BE    ERR2                                                             
         EX    0,0(RE)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P3V6                                                             
         LA    RE,L'ATTBTBL(RE)                                                 
         B     P3V5                                                             
*                                                                               
P3V6     ICM   R1,1,4(RE)          TEST FOR DUPLICATE BIT VALUE                 
         BZ    P3V7                                                             
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    FILATTB,0                                                        
         BNZ   ERR3                                                             
         OC    FILATTB,4(RE)                                                    
         B     P3V8                                                             
P3V7     OC    FILSTAT2,5(RE)                                                   
*                                                                               
P3V8     LA    R6,32(R6)           BUMP TO NEXT FIELD                           
         BCT   R0,P3V1                                                          
*                                                                               
P3VX     DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE FILTERS                                                              
***********************************************************************         
P4VAL    DS    0H                  P4=FILTERS NAMED BY KEYWORDS                 
*                                                                               
         LA    R4,SRVP4H                                                        
         XC    IFFILTS(IFFILTL),IFFILTS                                         
*                                                                               
         MVI   IFSORV,X'02'        SET DEFAULT SORT TO CREATE DATE              
         MVI   IFFMTV,1            SET DEFAULT FORMAT                           
         CLI   FLDILEN,0                                                        
         BE    P4VXX                                                            
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(8,(R6))                                      
         MVC   FLAG,4(R1)                                                       
         CLI   FLAG,0                                                           
         BE    ERR0                NUMBER/SYNTAX OF INPUT FIELDS                
*                                                                               
P4V0     CLI   1(R6),0             FORMAT MUST BE KEYWORD=VALUE                 
         BE    ERR0                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERR0                                                             
         MVI   FLAG1,X'80'         SET DEFAULT EQ SIGN                          
         LA    RE,11(R6,R1)                                                     
         CLI   0(RE),X'4C'         CHECK AND SET LT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'D0'                                                      
         CLI   0(RE),X'6E'         CHECK AND SET GT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'B0'                                                      
         CLI   0(RE),X'61'         CHECK AND SET NE SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'70'                                                      
         CLI   FLAG1,X'80'         ADJUST LEN IF SIGN VALUE LAST CHR            
         BE    *+14                                                             
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         STC   R1,0(R6)                                                         
         CLI   0(R6),2             KEYWORD MUST BE 2 THRU 8 CHRS LONG           
         BL    ERRF                                                             
         CLI   0(R6),8                                                          
         BH    ERRF                                                             
         BCTR  R1,0                                                             
         LA    R7,FILTTBL                                                       
*                                                                               
P4V1     CLI   0(R7),0             SEARCH FILTER NAME TABLE                     
         BE    ERRF                                                             
         EX    0,0(R7)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P4V2                                                             
         LA    R7,L'FILTTBL(R7)                                                 
         B     P4V1                                                             
*                                                                               
P4V2     TM    6(R7),X'01'         TEST IF DDS ONLY KEYWORD                     
         BZ    *+12                                                             
         TM    DDS,DDSTRM                                                       
         BZ    ERRF                                                             
         CLI   FLAG1,X'80'         TEST IF SIGN VALUE ALLOWED                   
         BE    P4V3                                                             
         CLI   FLAG1,X'70'         TEST NE ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'80'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'D0'         TEST LT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'40'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'B0'         TEST GT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'20'                                                      
         BZ    ERRFA                                                            
*                                                                               
P4V3     CLI   4(R7),X'F5'         TEST FOR XSORT                               
         BNE   *+12                                                             
         MVI   4(R7),X'05'         SET TO SORT                                  
         OI    DDS,DDSXSRT                                                      
         SR    RF,RF                                                            
         IC    RF,4(R7)            GOTO ROUTINE FOR VALUE                       
         SLL   RF,2                                                             
         B     P4ROUTS(RF)         R6=A(SCANNER TABLE ENTRY)                    
*                                                                               
P4VX     LA    R6,32(R6)           BACK FOR NEXT KEYWORD                        
         SR    R1,R1                                                            
         IC    R1,FLAG                                                          
         SH    R1,=H'1'            DECR NUM OF KEYWORDS INPUT                   
         BZ    P4VXX                                                            
         STC   R1,FLAG                                                          
         B     P4V0                                                             
P4VXX    B     STAVAL                                                           
*                                                                               
P4ROUTS  B     ERRF                VALIDATION ROUTINES                          
         B     CLAVALR             01                                           
         B     DATVALR             02                                           
         B     DDSVALR             03                                           
         B     SIZVALR             04                                           
         B     SORVALR             05                                           
         B     TIMVALR             06                                           
         B     FMTVALR             07                                           
         B     CDAVALR             08                                           
         B     PDAVALR             09                                           
         B     RDAVALR             10                                           
         B     TYPVALR             11                                           
         B     ERRF                12 N/D                                       
*                                                                               
CLAVALR  MVC   IFCLAF,FLAG1        SET CLASS FILTER INPUT FLAG                  
         LA    R7,IFCLAV                                                        
         CLI   1(R6),8             MAX OF 8 CLASSES                             
         BH    ERRFB                                                            
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     CLAVX                                                            
         MVC   0(0,R7),22(R6)      SET LIST OF CLASSES                          
CLAVX    B     P4VX                                                             
*                                                                               
TYPVALR  MVC   IFTYPF,FLAG1        SET TYPE FILTER INPUT FLAG                   
         LA    R7,IFTYPV                                                        
         CLI   1(R6),8             MAX OF 8 TYPES                               
         BH    ERRFB                                                            
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     TYPVX                                                            
         MVC   0(0,R7),22(R6)      SET LIST OF CLASSES                          
TYPVX    B     P4VX                                                             
*                                                                               
DATVALR  MVC   IFCDAF,FLAG1        SET DATE FILTER INPUT FLAG                   
         LA    R7,IFCDAV                                                        
         B     DATV0                                                            
CDAVALR  MVC   IFCDAF,FLAG1        SET CREATED DATE FILTER INPUT FLAG           
         LA    R7,IFCDAV                                                        
         B     DATV0                                                            
PDAVALR  MVC   IFPDAF,FLAG1        SET PRINTED DATE FILTER INPUT FLAG           
         LA    R7,IFPDAV                                                        
         B     DATV0                                                            
RDAVALR  MVC   IFRDAF,FLAG1        SET RETAINED DATE FILTER INPUT FLAG          
         LA    R7,IFRDAV                                                        
         B     DATV0                                                            
DATV0    CLC   22(5,R6),SR@TODAY   CHECK FOR TODAYS DATE                        
         BNE   DATV1                                                            
         MVC   0(2,R7),DATE2                                                    
         B     DATVX                                                            
DATV1    GOTO1 ADATVAL,DMCB,(0,22(R6)),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   DATV3               VALID YYMMDD INPUT                           
DATV2    GOTO1 (RF),(R1),(1,22(R6))                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRFB                                                            
         MVC   DUB(2),DATE         VALID MMDD INPUT                             
DATV3    GOTO1 ADATCON,DMCB,(0,DUB),(2,(R7))                                    
DATVX    B     P4VX                                                             
*                                                                               
TIMVALR  MVC   IFTIMF,FLAG1        SET TIME FILTER INPUT FLAG                   
         LA    R7,IFTIMV                                                        
         TM    3(R6),X'80'         MUST BE HH OR HHMM FORMAT                    
         BZ    ERRFB                                                            
         CLI   1(R6),2                                                          
         BL    ERRFB                                                            
         CLC   22(2,R6),=C'00'     TEST HOURS 00-23                             
         BL    ERRFB                                                            
         CLC   22(2,R6),=C'23'                                                  
         BH    ERRFB                                                            
         PACK  DUB,22(2,R6)        CONVERT HOURS                                
         CVB   R0,DUB                                                           
         STC   R0,0(R7)                                                         
         MVI   1(R7),0                                                          
         CLI   1(R6),2                                                          
         BE    TIMVX                                                            
         CLI   1(R6),4                                                          
         BNE   ERRFB                                                            
         CLC   24(2,R6),=C'00'     TEST MINUTES 00-59                           
         BL    ERRFB                                                            
         CLC   24(2,R6),=C'59'                                                  
         BH    ERRFB                                                            
         PACK  DUB,24(2,R6)        CONVERT MINS                                 
         CVB   R0,DUB                                                           
         STC   R0,1(R7)                                                         
TIMVX    B     P4VX                                                             
         SPACE 2                                                                
DDSVALR  MVC   IFDDSF,FLAG1        SET SPECIAL DDS INPUT FLAG                   
         LA    R7,IFDDSV                                                        
         CLI   1(R6),1                                                          
         BNE   ERRFB                                                            
         CLI   22(R6),C'A'         ONE CHR A THRU Z                             
         BL    ERRFB                                                            
         CLI   22(R6),C'Z'                                                      
         BH    ERRFB                                                            
         MVC   0(1,R7),22(R6)                                                   
DDSVX    B     P4VX                                                             
*                                                                               
FMTVALR  MVC   IFFMTF,FLAG1        SET FORMAT FILTER INPUT FLAG                 
         LA    R7,IFFMTV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 1 THRU 2                     
         BZ    ERRFB                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BZ    ERRFB                                                            
         CH    RE,=H'2'                                                         
         BH    ERRFB                                                            
         STC   RE,0(R7)                                                         
FMTVX    B     P4VX                                                             
*                                                                               
SIZVALR  MVC   IFSIZF,FLAG1        SET SIZE FILTER INPUT FLAG                   
         LA    R7,IFSIZV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 0 THRU 255                   
         BZ    ERRFB                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BM    ERRFB                                                            
         CH    RE,=H'255'                                                       
         BH    ERRFB                                                            
         STC   RE,0(R7)                                                         
SIZVX    B     P4VX                                                             
*                                                                               
SORVALR  MVC   IFSORF,FLAG1        SET SORT FILTER INPUT FLAG                   
         SR    R1,R1                                                            
         ICM   R1,1,1(R6)                                                       
         BZ    ERRFB               MUST BE AT LEAST 1 CHR                       
         BCTR  R1,0                                                             
         LA    R7,SORTTBL                                                       
         LA    RE,COMP1                                                         
         CLI   22(R6),C'A'         IS 1ST CHR SIGN                              
         BNL   SORV1                                                            
         BCTR  R1,0                                                             
         LA    RE,COMP2                                                         
SORV1    CLI   0(R7),0                                                          
         BE    ERRFB                                                            
         CLM   R1,1,6(R7)                                                       
         BNL   SORV2                                                            
         EX    0,0(R7)             RF=A(KEYWORD)                                
         EX    R1,0(RE)                                                         
         BE    SORV3                                                            
SORV2    LA    R7,L'SORTTBL(R7)                                                 
         B     SORV1                                                            
SORV3    CLI   22(R6),C'-'                                                      
         BNE   SORV4                                                            
         MVC   IFSORV(1),5(R7)     -VE SORT                                     
         B     P4VX                                                             
SORV4    MVC   IFSORV(1),4(R7)     +VE SORT                                     
         B     P4VX                                                             
*                                                                               
COMP1    CLC   22(0,R6),0(RF)      CIMPARE WITHOUT SIGN                         
COMP2    CLC   23(0,R6),0(RF)      COMPARE WITH SIGN                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK ACTION/STAT COMPATIBILITY                                               
***********************************************************************         
STAVAL   DS    0H                                                               
*                                                                               
         OC    WKSFILEN,WKSFILEN   INDIVIDUAL FILE SPECIFIED                    
         BZ    STAV1               NO                                           
         MVI   FILSTAT,X'FF'       IGNORE STAT                                  
         B     STAVX                                                            
*                                                                               
STAV1    LA    R4,SRVP3H           GROUP OF FILES SPECIFIED                     
         LA    R1,COMPTBL                                                       
STAV2    CLI   0(R1),X'FF'         FIND FIRST ENTRY FOR ACTION                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),WKACT                                                    
         BE    STAV3                                                            
         LA    R1,L'COMPTBL(R1)                                                 
         B     STAV2                                                            
*                                                                               
STAV3    CLI   1(R1),0             TEST IF OPTIONAL FILE STAT                   
         BNE   STAV4                                                            
         CLI   FILSTAT,0                                                        
         BNE   STAV4                                                            
         LA    R1,L'COMPTBL(R1)    EXTRACT DEFAULT                              
         MVC   FILSTAT,1(R1)                                                    
         B     STAVX                                                            
*                                                                               
STAV4    CLI   FILSTAT,0           REQUIRED FILE STAT                           
         BE    ERR1                                                             
STAV5    LA    R1,L'COMPTBL(R1)                                                 
         CLC   WKACT,0(R1)                                                      
         BH    ERR2                NOT IN TABLE                                 
         MVC   BYTE,1(R1)                                                       
         NC    BYTE,FILSTAT                                                     
         BZ    STAV5                                                            
*                                                                               
STAVX    EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONTROL  (WHAT NEXT?)                                                         
***********************************************************************         
CON000   LA    R4,SRVP3H           SET R4 TO P3 FOR CURSOR                      
         CLI   WKACT,X'28'                                                      
         BE    BUILD001            SIZE (GO BUILD A TABLE)                      
         CLI   WKACT,X'27'                                                      
         BE    CLR                 CLEAR (GO CLEAR WRKZ)                        
*                                                                               
         OC    WKFILEN,WKFILEN     DO WE HAVE AN INDIVIDUAL FILE                
         BNZ   INDI000                                                          
         OC    CIPASS,CIPASS       DO WE HAVE A CIADDR                          
         BNZ   INDI000                                                          
*                                                                               
CON010   TM    INTFLAG,INTLAST     TEST FOR RE-DISPLAY LAST PAGE                
         BO    CON020                                                           
         SR    R1,R1               GROUP DISP (GET PAGE NUM INTO R1)            
         IC    R1,WKSPAGE                                                       
*                                                                               
         CLI   PFKEY,7             PF7 = UP                                     
         BNE   *+6                                                              
         BCTR  R1,0                PAGE=PAGE-1                                  
*                                                                               
         CLI   PFKEY,0             ENT = DOWN                                   
         BE    *+12                                                             
         CLI   PFKEY,8             PF8 = DOWN                                   
         BNE   *+8                                                              
         LA    R1,1(R1)            PAGE=PAGE+1                                  
*                                                                               
         LTR   R1,R1               DONT LET PAGE GO -VE                         
         BNM   *+6                                                              
         SR    R1,R1                                                            
*                                                                               
         STC   R1,WKSPAGE          STORE PAGE BACK                              
         CLC   WKSPAGE,WKSPAGES    TEST => HIGHEST                              
         BL    *+8                                                              
         MVI   WKSPAGE,0           ZAP BACK TO START                            
*                                                                               
CON020   B     BUILDTAB                                                         
         EJECT                                                                  
*************************************************************                   
*        INDIVIDUAL FILE FUNCTIONS                          *                   
*************************************************************                   
         SPACE 1                                                                
INDI000  DS    0H                                                               
         TM    DDS,DDSTRM          DDS TERMINAL?                                
         BNZ   *+12                                                             
         LA    RE,SREACT           NO - INVALID ACTION                          
         B     ERRX                                                             
*                                                                               
         OC    CIPASS,CIPASS       DO WE HAVE A DA ALREADY                      
         BNZ   INDI010                                                          
         MVC   CIRSN,WKFILEN       CALCULATE FROM WKFILEN                       
         BAS   RE,RSNXPE                                                        
         BAS   RE,GETCAD                                                        
         MVC   CIPASS,CIADDR                                                    
*                                                                               
INDI010  MVC   SBYTE,WKACT2        SAVE ANY NUMERICS IN SBYTE                   
         CLI   WKACT,X'21'         DISP                                         
         BE    INDI090                                                          
         CLI   WKACT,X'2D'         SELECT                                       
         BE    INDI090                                                          
         BAS   RE,STATUS           CHANGE STATUS                                
*                                                                               
INDI090  TM    INTFLAG,INTCONT     EXIT IF INTERNAL ONLY                        
         BO    EXIT                                                             
*                                                                               
INDI091  MVI   SCREENR,X'FD'       LOAD FD SCREEN                               
         GOTO1 ALOADSCR                                                         
         CLI   WKACT,X'2D'         SELECT                                       
         BNE   INDI092                                                          
         MVCDD SRVP3A,SR#NSTAT     SET NEW STATUS MESSAGE                       
         MVC   SRVP4A,SPACES                                                    
         OI    SRVP3AH+6,X'80'                                                  
         OI    SRVP4AH+6,X'80'                                                  
         XC    SRVP3,SRVP3         CLEAR THESE FIELDS                           
         XC    SRVP4,SRVP4                                                      
INDI092  BAS   RE,DISFILE          DISPLAY FULL FILE DETAIL                     
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD FILE TAB IN CIREC (AFILTAB)                  *                   
*************************************************************                   
         SPACE 1                                                                
BUILDTAB TM    DDS,DDSNEW          TEST FOR READ WK FLAG                        
         BNO   CIUNCO              GO UNCOMPRESS SAVED STORAGE                  
*                                                                               
BUILD001 LA    R6,WORK                                                          
         USING WSBLOCKD,R6         BUILD SCAN KEY                               
         XC    WSBLOCK,WSBLOCK                                                  
*                                                                               
         MVC   WSSRCID,USERID                                                   
         OC    WSSRCID,WSSRCID     IF NO USERID                                 
         BNZ   *+8                                                              
         MVI   WSFILE,X'FF'        SCAN ALL FILES                               
         CLI   WKQUEUE,0                                                        
         BE    *+10                                                             
         MVC   WSFILE,WKQUEUE      SCAN THIS FILE                               
*                                                                               
         MVC   WSSYSPRG,WKSYSPG                                                 
         MVC   WSSUBPRG,WKSUBPG                                                 
         MVC   WSDAY,WKDAY                                                      
         MVC   WSCLAS,WKCLASS                                                   
*                                                                               
         MVC   WSSTAT,FILSTAT      STAT AND ATTB FILTERS                        
         MVC   WSATTB,FILATTB                                                   
*                                                                               
         MVC   WSAGESF,IFSIZF      AGE SIZE                                     
         MVC   WSAGESV,IFSIZV                                                   
         MVC   WSAGELF,IFCDAF      CDATE                                        
         MVC   WSAGELV,IFCDAV                                                   
         MVC   WSAGEDF,IFPDAF      DDATE                                        
         MVC   WSAGEDV,IFPDAV                                                   
         MVC   WSAGERF,IFRDAF      RDATE                                        
         MVC   WSAGERV,IFRDAV                                                   
*                                                                               
         CLI   IFTYPF,X'80'        TYPE FILTERS                                 
         BE    *+14                                                             
         MVC   WSTYPEN,IFTYPV                                                   
         B     *+10                                                             
         MVC   WSTYPE,IFTYPV                                                    
*                                                                               
         CLI   IFCLAF,X'80'        CLASS FILTERS                                
         BE    *+14                                                             
         MVC   WSCLASN,IFCLAV                                                   
         B     *+10                                                             
         MVC   WSCLAS,IFCLAV                                                    
*                                                                               
         MVC   WSSORT,IFSORV       SET SORT VALUE                               
*                                                                               
         TM    DDS,DDSXSRT         TEST FOR XSORT                               
         BZ    *+8                                                              
         OI    WSFLAGS,WSFXUSER    SET XSORT FLAG                               
         TM    DDS,DDSTOTL         TEST FOR T=                                  
         BZ    *+8                                                              
         OI    WSFLAGS,WSFTOTAL    SET TOTAL FLAG                               
*                                                                               
         CLI   WKACT,X'28'         TEST SIZE ACTION                             
         BNE   BUILD090                                                         
         OI    WSFLAGS,WSFPART2    SEARCH PART2 FLAG                            
         MVI   WSFILE,X'FF'        SCAN ALL FILES                               
*                                                                               
BUILD090 DS    0H                                                               
         LR    R0,R6                                                            
         TM    DDS,DDSTRM          DDS TERMINAL?                                
         BNZ   *+8                                                              
         ICM   R0,8,=C'D'          NO - REQUEST DEMO ACCESS CHECK               
*                                                                               
        GOTO1 AWKZSCAN,DMCB,(R0),ACTREC,AFILTAB,ACXREC,ACOMFACS,LOGONAG         
        CLI   8(R1),0                                                           
        JNE   *+2                  ERROR ON WRKZ SCAN                           
*                                                                               
         CLI   WKACT,X'28'         TEST SIZE ACTION                             
         BE    SIZE000             GOTO SIZE SCREEN                             
*                                                                               
         L     R4,ACTREC                                                        
         USING WSCOUNTD,R4                                                      
         MVC   QNDATA,WSCOUNT      SAVE COUNTS                                  
         SR    R1,R1                                                            
         IC    R1,WSFILES                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    BUILD095            1 FILE ONLY                                  
*                                                                               
BUILD091 LA    R4,L'WSCOUNT(R4)    NEXT COUNT BLOCK                             
         LA    R0,(L'QNDATA/4)                                                  
         LA    RE,QNDATA                                                        
         LR    RF,R4                                                            
         CLM   R1,1,=X'1'                                                       
         BNE   *+10                                                             
         ZAP   QT,=P'0'            LAST QT IS TRUE TOTAL                        
*                                                                               
         AP    0(4,RE),0(4,RF)     ACCUMULATE COUNTS                            
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-14                                                          
         BCT   R1,BUILD091                                                      
*                                                                               
BUILD095 ZAP   DUB,WST                                                          
         CVB   R1,DUB              R1=NUMBER IN TABLE                           
         ST    R1,DUB              SAVE R1 AT DUB                               
         SRL   R1,4                R1=R1/16 TO GET NUM OF PAGES                 
         TM    DUB+3,X'0F'         TEST FOR REMAINDER                           
         BZ    *+8                                                              
         LA    R1,1(R1)            R1=NUMBER OF PAGES                           
         STC   R1,WKSPAGES                                                      
*                                                                               
         DROP  R6,R4                                                            
         MVI   WKSPAGE,0           MUST RESET PAGE                              
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* COMPRESS FILTAB INTO CISAVEDS AT WKSCIADS                                     
*----------------------------------------------------------------------         
CICOMP   L     R6,AFILTAB          COMPRESS FROM R6 TABLE                       
         USING WSEDATAD,R6                                                      
         LA    R7,WKSCIADS         TO R7 TABLE                                  
         USING CISAVED,R7                                                       
         CLC   WSEDATA,FFS         TEST FOR NO FILES FOUND                      
         BE    ERR11                                                            
*                                                                               
COMP010  XC    CISAVE,CISAVE       CLEAR ENTRY                                  
         MVC   CISWRKZ,WSEWRKZ                                                  
         MVC   CISADR,WSECIAD      SAVE CIADR                                   
         TM    DDS,DDSTOTL                                                      
         BNO   *+10                                                             
         MVC   CISACTN(2),WSESORT+2                                             
         CLC   WSEDATA,FFS                                                      
         BE    COMP020                                                          
         LA    R6,L'WSEDATA(R6)    BUMP TO NEXT ENTRY                           
         LA    R7,L'CISAVE(R7)                                                  
         B     COMP010                                                          
*                                                                               
COMP020  MVC   CISAVE,FFS          MARK EOT                                     
         B     GRP010                                                           
         DROP  R6,R7                                                            
                                                                                
*----------------------------------------------------------------------         
* UNCOMPRESS CISAVEDS INTO WKSCIADS                                             
*----------------------------------------------------------------------         
CIUNCO   L     R6,AFILTAB          UNCOMPRESS TO R6 TABLE                       
         USING WSEDATAD,R6                                                      
         LA    R7,WKSCIADS         FROM R7 TABLE                                
         USING CISAVED,R7                                                       
         CP    QT,=P'0'            TEST NO FILES                                
         BE    ERR11                                                            
*                                                                               
UNCO010  XC    WSEDATA,WSEDATA                                                  
         MVC   WSEWRKZ,CISWRKZ                                                  
         MVC   WSECIAD(L'CISADR),CISADR RESTORE CIADR                           
         TM    DDS,DDSTOTL                                                      
         BO    UNCO016                                                          
         MVC   WSESORT+0(1),CISFLAG     SAVE FLAG IN SORT                       
         MVC   WSESORT+1(1),CISACTN     SAVE ACTN IN SORT+1                     
         B     UNCO018                                                          
*                                                                               
UNCO016  XC    WSESORT,WSESORT                                                  
         MVC   WSESORT+2(2),CISACTN                                             
*                                                                               
UNCO018  CLC   CISAVE,FFS                                                       
         BE    UNCO020                                                          
         LA    R6,L'WSEDATA(R6)    BUMP TO NEXT ENTRY                           
         LA    R7,L'CISAVE(R7)                                                  
         B     UNCO010                                                          
*                                                                               
UNCO020  MVC   WSEDATA,FFS         MARK EOT                                     
         B     GRP010                                                           
         DROP  R6,R7                                                            
                                                                                
*----------------------------------------------------------------------         
* CHECK FOR GROUP STATUS CHANGE                                                 
*----------------------------------------------------------------------         
GRP010   CLI   WKACT,X'21'         TEST DISP ONLY                               
         BE    GRP090                                                           
*                                                                               
         LA    R7,WKSCIADS         GO THROUGH SAVE LIST                         
         USING CISAVED,R7                                                       
         MVC   SBYTE,WKACT2        SAVE ANY NUMERICS IN SBYTE                   
*                                                                               
GRP020   MVC   WKPASS,CISWRKZ      CHANGE STATUS OF ALL                         
*                                                                               
         MVC   CIPASS(L'CISADR),CISADR                                          
         MVI   CIPASS+3,1                                                       
*                                                                               
         BAS   RE,STATUS                                                        
         LA    R7,L'CISAVE(R7)                                                  
         CLC   CISAVE,FFS                                                       
         BNE   GRP020                                                           
*                                                                               
GRP090   BAS   RE,FILELIST         DISPLAY LIST                                 
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
***********************************************************************         
* DISPLAY SIZE DETAILS                                                          
***********************************************************************         
SIZE000  MVC   SRVEX2,SIZEH0                                                    
         MVC   SRVHD1,SIZEH1                                                    
*                                                                               
         LA    R4,SRVSA1H                                                       
         USING SIZED,R4                                                         
         L     R6,ACTREC                                                        
         USING WSCOUNTD,R6                                                      
*                                                                               
         LA    RE,WRKZLST+8                                                     
         ST    RE,AWRKZLST                                                      
         CLI   WKACT1,2                                                         
         BNE   NSIZE1                                                           
         CLI   WKQUEUE,0                                                        
         BE    NSIZE1                                                           
         MVC   WRKZCHR,WKQUEUE                                                  
         B     *+10                                                             
NSIZE1   MVC   WRKZCHR,1(RE)                                                    
         MVC   SZQUE,WRKZID                                                     
         GOTO1 ADATAMGR,WKDMCB,(0,BUFFER),WRKZID,,,ACXREC                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACXREC                                                        
         MVC   BUFFDATA,0(R1)                                                   
         MVC   CIDATA(L'CIDATA),12(R1)                                          
         CLI   WKACT1,1                                                         
         BE    NSIZEPC                                                          
         CLI   WKACT1,2                                                         
         BE    NSIZEFIL                                                         
                                                                                
***********************************************************************         
*        WRKZ SIZE CONTROL INTERVAL COUNTS                                      
***********************************************************************         
         ICM   R1,15,CICITOT       TOTAL PART1 (FULL)                           
         BAS   RE,EDIT1                                                         
         MVC   SZTOT1,SWORK                                                     
*                                                                               
         ICM   R1,15,CICITOT      TOTAL-INDEX-INUSE=AVAILABLE                   
         SH    R1,CICINDX                                                       
         ZAP   DUB,WSC1                                                         
         CVB   RF,DUB              RF=INUSE PART1                               
         SR    R1,RF               R1=AVAILABLE PART1                           
         BAS   RE,EDIT1                                                         
         MVC   SZAVA1,SWORK                                                     
*                                                                               
         LR    R1,RF               R1=INUSE PART1                               
         ZAP   DUB,WSC3            ACTIVE PART1                                 
         CVB   RF,DUB              RF=ACTIVE PART1                              
         SR    R1,RF               SENT=INUSE-ACTIVE                            
         BAS   RE,EDIT1                                                         
         MVC   SZPRT1,SWORK                                                     
*                                                                               
         LR    R1,RF               R1=ACTIVE PART1                              
         BAS   RE,EDIT1                                                         
         MVC   SZACT1,SWORK                                                     
*                                                                               
         LH    R1,CICINDX          R1=INDEX PART1                               
         BAS   RE,EDIT1                                                         
         MVC   SZIND1,SWORK                                                     
*                                                                               
         ICM   R1,15,CJCITOT        R1=TOTAL PART2                              
         BAS   RE,EDIT1                                                         
         MVC   SZTOT2,SWORK                                                     
*                                                                               
         ICM   R1,15,CJCITOT        R1=TOTAL PART2                              
         ZAP   DUB,WSC2                                                         
         CVB   RF,DUB              RF=INUSE PART2                               
         SR    R1,RF               R1=AVAILABLE PART2                           
         BAS   RE,EDIT1                                                         
         MVC   SZAVA2,SWORK                                                     
*                                                                               
         LR    R1,RF               R1=INUSE PART2                               
         ZAP   DUB,WSC4            ACTIVE PART2                                 
         CVB   RF,DUB              RF=ACTIVE PART2                              
         SR    R1,RF               SENT=INUSE-ACTIVE                            
         BAS   RE,EDIT1                                                         
         MVC   SZPRT2,SWORK                                                     
*                                                                               
         LR    R1,RF               R1=ACTIVE PART2                              
         BAS   RE,EDIT1                                                         
         MVC   SZACT2,SWORK                                                     
*                                                                               
         B     NSIZENXT                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
*        WRKZ SIZE CONTROL INTERVAL PERCENTAGES                                 
***********************************************************************         
NSIZEPC  ICM   R1,15,CICITOT      TOTAL PART1                                   
         BAS   RE,EDIT1                                                         
         MVC   SZTOT1,SWORK                                                     
*                                                                               
         ICM   RF,15,CICITOT                                                    
         SH    RF,CICINDX                                                       
         ZAP   DUB,WSC1            PART1 INUSE                                  
         CVB   R1,DUB                                                           
         ST    R1,FULL             SAVE INUSE                                   
         SR    RF,R1                                                            
         BAS   RE,CALCPCS          CALCULATE PERCENTAGE                         
         LA    R1,SZAVA1                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         L     RF,FULL             RESTORE INUSE                                
         ZAP   DUB,WSC3            ACTIVE PART1                                 
         CVB   R1,DUB                                                           
         SR    RF,R1               SUBTRACT ACTIVES FROM INUSE                  
         BAS   RE,CALCPCS          CALCULATE PERCENTAGE                         
         LA    R1,SZPRT1                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         ZAP   DUB,WSC3                                                         
         CVB   RF,DUB                                                           
         BAS   RE,CALCPCS          CALCULATE PERCENTAGE                         
         LA    R1,SZACT1                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         LH    RF,CICINDX                                                       
         BAS   RE,CALCPCS          CALCULATE PERCENTAGE                         
         LA    R1,SZIND1                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         EDIT  (B4,CJCITOT),(6,SZTOT2)    TOTAL PART1                           
*                                                                               
         ICM   RF,15,CJCITOT                                                    
         ZAP   DUB,WSC2            PART2 INUSE                                  
         CVB   R1,DUB                                                           
         SR    RF,R1                                                            
         ST    R1,FULL             SAVE INUSE                                   
         BAS   RE,CALCPC2          CALCULATE PERCENTAGE                         
         LA    R1,SZAVA2                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         L     RF,FULL             RESTORE INUSE                                
         ZAP   DUB,WSC4            ACTIVE PART2                                 
         CVB   R1,DUB                                                           
         SR    RF,R1               SUBTRACT ACTIVES FROM INUSE                  
         BAS   RE,CALCPC2          CALCULATE PERCENTAGE                         
         LA    R1,SZPRT2                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
*                                                                               
         ZAP   DUB,WSC4                                                         
         CVB   RF,DUB                                                           
         BAS   RE,CALCPC2          CALCULATE PERCENTAGE                         
         LA    R1,SZACT2                                                        
         BAS   RE,EDITPCS          DISPLAY PERCENTAGE                           
         B     NSIZENXT                                                         
         EJECT                                                                  
********************************************************                        
*        DISPLAY FILE SIZE INFO                        *                        
********************************************************                        
         SPACE 1                                                                
NSIZEFIL LA    R4,SRVSA1H          DISPLAY FILE ATTRIBUTES                      
         USING WKRLD,R4                                                         
         MVC   WKLDATA(13),=C'TRKS/PART1 CI'                                    
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CITRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(13),=C'TRKS/PART2 CI'                                    
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CJTRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'TRKS FOR INDEX'                                   
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'TRKS FOR PART1'                                   
         OI    WKLHDR+6,X'80'                                                   
         SR    R0,R0                                                            
         ICM   R0,15,CICITOT                                                    
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'TRKS FOR PART2'                                   
         OI    WKLHDR+6,X'80'                                                   
         ICM   R0,15,CJCITOT                                                    
         MH    R0,CJTRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(13),=C'RECORD LENGTH'                                    
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CIBLKLN                                                       
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(13),=C'RECORDS/TRACK'                                    
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CIHIREC                                                       
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'NDX ENTRYS/REC'                                   
         OI    WKLHDR+6,X'80'                                                   
         SR    R0,R0                                                            
         ICM   R0,3,CIENTRYS                                                    
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'NDX TOTAL RECS'                                   
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CIPAGES                                                       
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'NDX PART1 RECS'                                   
         OI    WKLHDR+6,X'80'                                                   
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'NDX PART2 RECS'                                   
         OI    WKLHDR+6,X'80'                                                   
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
SIZE5    MVC   SRVMSG(30),=C'FILE SIZE ATTRIBUTES DISPLAYED'                    
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
SIZEOUT  EDIT  (R0),(7,0(RF))             SUBROUTINE TO EDIT NUMBERS            
         BR    RE                                                               
*                                                                               
NSIZENXT LA    R4,93(R4)                  NEXT LINE                             
         LA    R6,64(R6)                  NEXT COUNTS                           
         ICM   RE,15,AWRKZLST             NEXT QUEUE                            
         LA    RE,8(RE)                                                         
         ST    RE,AWRKZLST                                                      
         CLI   0(RE),0                                                          
         BNE   NSIZE1                                                           
         MVC   SRVMSG(30),=C'FILE SIZE ATTRIBUTES DISPLAYED'                    
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        THESE ROUTINES SAVE ON PROGRAM SPACE               *                   
*************************************************************                   
         SPACE 1                                                                
EDIT     MVC   SWORK,SPACES        EDIT FROM R1 TO SWORK                        
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK,ALIGN=LEFT                           
         BR    RE                  R0=LEN ON EXIT                               
EDIT1    MVC   SWORK,SPACES        EDIT FROM R1 TO SWORK                        
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK                                      
         BR    RE                                                               
*                                                                               
CALCPCS  ICM   R1,15,CICITOT       CALCULATE RF PERCENTAGE OF CICITOT           
         B     *+8                                                              
CALCPC2  ICM   R1,15,CJCITOT       CALCULATE RF PERCENTAGE OF CJCITOT           
         ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         DR    RE,R1                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDITPCS  LTR   RF,RF               SHOW 0% AS 0%                                
         BNE   *+12                                                             
         MVC   4(2,R1),=C'0%'                                                   
         BR    RE                                                               
         CH    RF,=H'1000'         SHOW 100% AS 100%                            
         BNE   *+12                                                             
         MVC   2(4,R1),=C'100%'                                                 
         BR    RE                                                               
         EDIT  (RF),(6,0(R1)),1,TRAIL=C'%'                                      
         CH    RF,=H'10'                                                        
         BNLR  RE                                                               
         MVI   2(R1),C'0'          FILL IN MISSING ZERO IF .X%                  
         BR    RE                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
                                                                                
*************************************************************                   
*        PRODUCE LIST OF FILES FROM FILTAB                  *                   
*************************************************************                   
FILELIST NTR1                                                                   
         L     R6,AFILTAB                                                       
         USING WSEDATAD,R6         R6=TABLE ENTRY                               
         LA    R4,SRVSA1H                                                       
         USING WKRLD,R4            R4=SCREEN LINE                               
         MVI   RTNERR,0                                                         
*                                                                               
         LLC   R1,WKSPAGE          CALCULATE INDEX INTO DATA                    
         MHI   R1,16*L'WSEDATA                                                  
         LA    R6,0(R6,R1)         INDEX INTO DATA                              
*                                                                               
FLST010  TM    INTFLAG,INTLAST     RE-DISPLAY PREV                              
         BO    FLST011                                                          
         XC    WKLSEL,WKLSEL       NO SO CLEAR ALL SEL FIELDS                   
         MVI   WKLSELH+5,0                                                      
         B     FLST020                                                          
*                                                                               
FLST011  TM    WSESORT,ACTDONE     DO WE HAVE A COMPLETED SUBACT                
         BZ    FLST020                                                          
         TM    WSESORT,ACTERR      NOT ERRORS                                   
         BO    FLST015                                                          
         CLI   WKLSEL,C'*'         IGNORE IF * IN POS 1                         
         BE    FLST020                                                          
         L     R1,ASELTABL                                                      
         CLC   4(1,R1),WSESORT+1   MATCH ACTION                                 
         BE    *+16                                                             
         LA    R1,8(R1)            TRY NEXT ENTRY                               
         CLI   0(R1),0                                                          
         BNE   *-18                                                             
         EX    0,0(R1)                                                          
         XC    WKLSEL,WKLSEL                                                    
         MVI   WKLSEL,C'*'         SET *A TO SHOW COMPLETED ACT                 
         MVC   WKLSEL+1(1),0(RF)                                                
         B     FLST020                                                          
*                                                                               
FLST015  ST    R4,CURSOR           SET CURSOR FOR ERRORS                        
*                                                                               
FLST020  MVC   WKPASS,WSEWRKZ                                                   
*                                                                               
         MVC   CIPASS,WSECIAD      GET CIADDR FROM TABLE                        
         MVI   CIPASS+3,1                                                       
*                                                                               
         MVI   RTNERR,YES                                                       
         BAS   RE,READREC          READ 1ST REC                                 
*                                                                               
         USING W_RECD,R5           R5=WORKER RECORD                             
         LR    R5,R1                                                            
         MVC   WKLDATA,SPACES      CLEAR DATA LINE                              
         OC    W_FILENO,W_FILENO                                                
         BNZ   FLST030                                                          
         MVC   WKLSDATE(4),=C'ERR='                                             
         GOTO1 AHEXOUT,DMCB,RTNERR,WKLSDATE+4,1,0                               
         MVC   WKLDATA(L'FRMATMSG),FRMATMSG                                     
         CLI   RTNERR,X'41'        Format error                                 
         BE    FLST022                                                          
         MVC   WKLDATA(L'RPTNFMSG),RPTNFMSG                                     
         CLI   RTNERR,X'10'        Test not found                               
         BE    FLST059                                                          
         MVC   WKLDATA,SPACES      CLEAR DATA LINE                              
         MVC   WKLDATA(L'PURGEMSG),PURGEMSG                                     
         B     FLST059                                                          
*                                                                               
         USING CISAVED,R7                                                       
FLST022  LA    R7,WKSCIADS                                                      
FLST023  CLC   CISAVE,FFS                                                       
         BNE   *+6                                                              
         DC    H'00'               Check coding problem                         
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R0,B'1110',CISADR   TTTTT000                                     
         ICM   R1,B'1110',CIPASS   TTTTT000                                     
*                                                                               
         CR    R0,R1               COMPARE TRACK                                
         BE    FLST028                                                          
         AHI   R7,L'CISAVE                                                      
         B     FLST023                                                          
*                                                                               
FLST028  XC    CISADR,CISADR       Clear for error indication                   
         BRAS  RE,GETXPE           Get page/entry based on A(Part 1 CI)         
         BRAS  RE,GETXAD           Get A(Index) based on page/entry             
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),WRKZID,CXADDR,ACXREC                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'               Stop and look                                
         LH    R5,CXENTRY          ZAP INDEX INDEX AND WRITE BACK               
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
*                                                                               
FLST030  MVC   DUB,W_KEY                                                        
         BAS   RE,GETKEY                                                        
         MVC   WKLFID,WORK                                                      
*                                                                               
         MVC   WKLWKR,WKPASS                                                    
*                                                                               
         ICM   R1,15,W_FILENO                                                   
         EDIT  (R1),(6,WKLREF),FILL=0                                           
*                                                                               
         MVC   CISTAT,W_STAT       DISPLAY STATUS                               
         BAS   RE,STATOUT                                                       
         MVC   WKLSTAT,SFULL                                                    
*                                                                               
         GOTO1 ADATCON,DMCB,(X'42',W_DATEL),(7,WKLCDATE)                        
         MVC   DUB(2),W_TIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   WKLCDATE+6(5),DUB+2                                              
*                                                                               
FLST040  MVC   HALF,W_AGERD        GET RETAIN DATE                              
         MVC   BYTE,W_AGERT        GET RETAIN TIME                              
         BAS   RE,GETHOUR                                                       
FLST04X  MVC   WKLHOURS,FULL                                                    
*                                                                               
         MVC   WKLSDATE(5),DOTS                                                 
         MVC   WKLSDATE+6(5),DOTS                                               
         OC    W_DATED,W_DATED                                                  
         BZ    FLST050                                                          
*                                                                               
         GOTO1 ADATCON,DMCB,(X'42',W_DATED),(7,WKLSDATE)                        
         MVC   DUB(2),W_TIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   WKLSDATE+6(5),DUB+2                                              
*                                                                               
FLST050  CLI   RTNERR,X'41'        Format error                                 
         BE    FLST059                                                          
         TM    DDS,DDSTOTL+DDSUSR+DDSGEN                                        
         BZ    FLST051                                                          
         CLC   GIPREV,W_USRID                                                   
         BE    FLST051                                                          
         MVC   GIUSER,W_USRID                                                   
         GOTO1 AGETUSER                                                         
         MVC   WKLDESC+0(2),=C'U='                                              
         MVC   WKLDESC+2(8),GIUSERID                                            
         B     FLST055                                                          
*                                                                               
FLST051  MVC   WKLDESC,W_DESC                                                   
         LA    R1,WKLDESC                                                       
         LA    RF,L'WKLDESC                                                     
FLST052  CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C' '          REMOVE UNPRINTABLE CHARS FOR DISPLAY         
         LA    R1,1(,R1)                                                        
         BCT   RF,FLST052                                                       
*                                                                               
FLST055  EDIT  (B4,W_RECS),(6,WKLRECS)                                          
*                                                                               
         TM    DDS,DDSTOTL         TEST FOR TOTALS                              
         BZ    FLST059                                                          
         EDIT  (B4,WSESORT),(6,WKLRECS)                                         
*                                                                               
FLST059  LA    R6,L'WSEDATA(R6)    BUMP TO NEXT DATA FIELD                      
*                                                                               
FLST060  LA    R4,WKLLEN(R4)       BUMP SCREEN LINES                            
         LA    R1,SRVPFKH                                                       
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BNL   FLSTXXX                                                          
*                                                                               
         CLC   WSEDATA(2),FFS      TEST END OF DATA                             
         BNE   FLST010                                                          
         XC    WKLSEL,WKLSEL       ALWAYS CLEAR REMAINING SEL FIELDS            
         XC    WKLDATA,WKLDATA     AND DATA FILEDS                              
         MVI   WKLSELH+5,0                                                      
         B     FLST060                                                          
*                                                                               
FLSTXXX  TM    INTFLAG,INTERR+INTLAST                                           
         BO    XIT1                EXIT NOW IF ERROR SET                        
*                                                                               
         LA    R4,SRVSA1H          SET CURS TO 1ST SEL                          
         OC    WKSCURS,WKSCURS     TEST FOR SAVED CURSOR                        
         BZ    *+10                                                             
         LH    R4,WKSCURS                                                       
         AR    R4,R3               ADD TWA ADDR                                 
         XC    WKSCURS,WKSCURS                                                  
         ST    R4,CURSOR                                                        
*                                                                               
         MVC   MSGREF,I168         N FILES PAGE N OF N DISPLAYED                
         LA    RF,MSGXTRA                                                       
*                                                                               
         ZAP   DUB,QT              TOTAL NUM OF FILES                           
         CVB   R1,DUB                                                           
         BAS   RE,EDIT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,0(RF)                                                         
         MVC   1(8,RF),SWORK                                                    
         AR    RF,R0                                                            
         MVI   0(RF),8             KEYWORD FILES                                
         MVCDD 1(7,RF),SR#FILES                                                 
         LA    RF,8(RF)                                                         
         SR    R1,R1               CURRENT PAGE                                 
         IC    R1,WKSPAGE                                                       
         LA    R1,1(R1)                                                         
         BAS   RE,EDIT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,0(RF)                                                         
         MVC   1(8,RF),SWORK                                                    
         AR    RF,R0                                                            
         IC    R1,WKSPAGES         TOTAL PAGES                                  
         BAS   RE,EDIT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,0(RF)                                                         
         MVC   1(8,RF),SWORK                                                    
         AR    RF,R0                                                            
         MVI   0(RF),1             ASSUME NO STATUS MESSAGE                     
         MVI   1(RF),0             END MARKER                                   
*                                                                               
         TM    DDS,DDSTOTL         IF TOTALS FIT ON ONE PAGE                    
         BNO   FLSTXX1                                                          
         CLI   WKSPAGES,1                                                       
         BNE   FLSTXX1                                                          
         LA    R1,SRVP1H           RETURN THE CURSOR TO P1                      
         ST    R1,CURSOR                                                        
*                                                                               
FLSTXX1  SR    R1,R1                                                            
         ICM   R1,3,STATCH         ANY STATUS CHANGES                           
         BZ    XIT1                                                             
*                                                                               
         BAS   RE,EDIT             EDIT NUMBER OF CHANGES                       
         MVC   1(8,RF),SWORK                                                    
         LA    R1,2(RF)                                                         
         AR    R1,R0                                                            
         MVCDD 0(16,R1),SR#STACH   1 STATUS CHANGED                             
         CLC   STATCH,=H'1'                                                     
         BE    *+10                                                             
         MVCDD 0(16,R1),SR#STCHS   N STATUS CHANGES                             
*                                                                               
         AH    R0,=H'20'           ROOM FOR STATUS CHANGE MESSAGE               
         STC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVI   0(RF),0             END MARKER                                   
         B     XIT1                                                             
*                                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*************************************************************                   
*        PRINT FULL SCREEN OF FILE DETAIL CIPASS=TTTTBB00/TTTTT0RR              
*************************************************************                   
         SPACE 1                                                                
DISFILE  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
DISF010  MVC   WKUSER,W_USRID      COPY USERID                                  
         MVC   WKSYSPG(6),W_SYSPRG COPY SPPSDC                                  
         MVC   WKFILEN,W_FILENO    COPY REFERENCE NUMBER                        
         XC    WKTIMES,WKTIMES     CLEAR TIMES                                  
         GOTO1 ARIDXPND,SRVP2H                                                  
*                                                                               
DISF012  MVC   DUB,W_KEY                                                        
         BAS   RE,GETKEY                                                        
         MVC   SRVFIL,WORK                                                      
*                                                                               
         ICM   R1,15,W_FILENO                                                   
         BAS   RE,EDIT                                                          
         MVC   SRVREF,SWORK                                                     
*                                                                               
         MVC   CISTAT,W_STAT       DISPLAY STATUS                               
         BAS   RE,STATOUT                                                       
         MVC   SRVSTAT,SWORK                                                    
*                                                                               
         GOTO1 ADATCON,DMCB,(X'42',W_AGELD),(8,SRVDATC)                         
         GOTO1 (RF),(R1),(X'42',W_AGERD),(8,SRVDATD)                            
*                                                                               
DISF020  SR    R0,R0               RETAIN TIME                                  
         SR    R1,R1                                                            
         IC    R1,W_AGERT                                                       
         MH    R1,=H'10'           CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   SRVTIMR,DUB+2       RETAIN TIME                                  
*                                                                               
         MVC   HALF,W_AGERD        GET RETAIN DATE                              
         MVC   BYTE,W_AGERT        GET RETAIN TIME                              
         BAS   RE,GETHOUR                                                       
         MVC   SRVRMNG(3),FULL     HOURS TO GO                                  
         MVCDD SRVRMNG+3(3),SR#HOURS                                            
*                                                                               
DISF030  MVC   SRVTYPE(1),W_TYPE   DISPLAY TYPE                                 
*                                                                               
         LA    R1,ATTINDS          DISPLAY ATTRIBUTES                           
         MVC   BYTE,W_ATTB                                                      
         BAS   RE,GETBITS                                                       
         MVC   SRVATTB,WORK                                                     
*                                                                               
DISF040  ICM   R1,15,W_RECS                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVRECL,SWORK                                                    
         ICM   R1,15,W_RECH                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVRECH,SWORK                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,W_SEQ                                                       
         BNZ   *+8                                                              
         LA    R1,1                IF ZERO MAKE 1                               
         BAS   RE,EDIT                                                          
         MVC   SRVCISQ,SWORK                                                    
*                                                                               
         SR    R1,R1                                                            
         IC    R1,W_AGES                                                        
         BAS   RE,EDIT                                                          
         MVC   SRVSIZE,SWORK                                                    
*                                                                               
         MVC   SRVWKDA,SPACES                                                   
         GOTO1 AHEXOUT,DMCB,CIADDR,SRVWKDA,4,TOG                                
         MVC   SRVWKDA+09(5),=C'WRKZ#'                                          
         MVC   SRVWKDA+14(1),WRKZCHR                                            
         BAS   RE,GETXPE                                                        
         BAS   RE,GETXAD                                                        
         GOTO1 AHEXOUT,DMCB,CXADDR,SRVWKDA+16,4,TOG                             
         MVI   SRVWKDA+24,C'+'                                                  
         LH    RF,CXENTRY                                                       
         MH    RF,CINDXLN                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVWKDA+25(5),DUB                                                
*                                                                               
         GOTO1 AHEXOUT,DMCB,W_CINEXT,SRVNEXT,4                                  
*                                                                               
         CLI   W_SEQ,2             IS THIS PART 1                               
         BNL   DISF990                                                          
*                                                                               
         CLI   CIADDR+3,1          IS IT RECORD 1                               
         BNE   DISF990                                                          
         MVC   SRVRECS,=CL12'Total Rec#'                                        
*                                                                               
         XC    SRVCISQ,SRVCISQ     REDISPLAY WITH N OF N                        
         SR    R1,R1                                                            
         ICM   R1,1,W_SEQ                                                       
         BNZ   *+8                                                              
         LA    R1,1                IF ZERO MAKE 1                               
         BAS   RE,EDIT                                                          
         MVC   SRVCISQ,SWORK                                                    
         LA    RF,SRVCISQ                                                       
         AR    RF,R0                                                            
         MVC   0(4,RF),=C' of '                                                 
         LA    RF,4(RF)                                                         
         SR    R1,R1                                                            
         IC    R1,W_NCI                                                         
         BAS   RE,EDIT                                                          
         MVC   0(4,RF),SWORK                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,W_AVGRL                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVAVGL,SWORK                                                    
         SR    R1,R1                                                            
         ICM   R1,3,W_MAXRL                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVMAXL,SWORK                                                    
*                                                                               
         OC    W_DATED,W_DATED     TEST & DISPLAY LOCN SENT                     
         BZ    DISF050                                                          
         CLC   W_PRLOC,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVC   SRVLOCS(3),=C'DDS'  FFS IS DDS                                   
         B     DISF050                                                          
         MVC   GIUSER,W_PRLOC                                                   
         GOTO1 AGETUSER                                                         
         LA    RF,SRVLOCS                                                       
         MVC   0(8,RF),GIUSERID    USERID SENT TO                               
*                                                                               
DISF050  MVC   SRVDEVS,W_PRSYM     LUID SENT TO                                 
         SR    R1,R1                                                            
         ICM   R1,1,W_PRCNT        EDIT COUNT UNLESS ZERO                       
         BZ    DISF051                                                          
         BAS   RE,EDIT                                                          
         MVC   SRVCNTS,SWORK                                                    
*                                                                               
DISF051  MVC   SRVNAME,W_DESC      DESCRIPTION                                  
         LA    R1,SRVNAME                                                       
         LA    RF,L'SRVNAME                                                     
DISF052  CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C' '          REMOVE UNPRINTABLE CHARS FOR DISPLAY         
         LA    R1,1(,R1)                                                        
         BCT   RF,DISF052                                                       
*                                                                               
         GOTO1 AHEXOUT,DMCB,W_TTBR,SRVTTBL,4,TOG                                
*                                                                               
         CLC   W_RETNL,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVCDD SRVRETC(9),SR#PERM                                               
         B     DISF060                                                          
         SR    R1,R1                                                            
         ICM   R1,3,W_RETNL                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVRETC,SWORK                                                    
         LA    R1,SRVRETC                                                       
         AR    R1,R0                                                            
         MVCDD 1(3,R1),SR#HOURS                                                 
*                                                                               
DISF060  GOTO1 AHEXOUT,DMCB,W_UDATA,SRVUSER,2,TOG                               
*                                                                               
DISF065  CLC   W_RETNL,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVCDD SRVRETS(9),SR#PERM                                               
         B     DISF070                                                          
         SR    R1,R1                                                            
         ICM   R1,3,W_RETND                                                     
         BAS   RE,EDIT                                                          
         MVC   SRVRETS,SWORK                                                    
         LA    R1,SRVRETS                                                       
         AR    R1,R0                                                            
         MVCDD 1(3,R1),SR#HOURS                                                 
*                                                                               
DISF070  MVC   DUB(2),W_TIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   SRVTIMC,DUB+2       CREATE TIME                                  
*                                                                               
         MVC   DUB(2),W_TIMED                                                   
         OC    DUB(2),DUB                                                       
         BZ    DISF100                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   SRVTIMS,DUB+2       SENT TIME                                    
*                                                                               
DISF100  MVC   SRVPSWD,W_PSWD                                                   
*                                                                               
DISF990  MVC   MSGREF,I189         FILE ATTRIBS DISPLAYED                       
         TM    INTFLAG,INTRUN                                                   
         BNO   *+10                                                             
         MVC   MSGREF,I188         FILE SELECTED (NEW STSTUS)                   
         LA    R4,SRVP3H           SET CURS TO P3                               
         ST    R4,CURSOR                                                        
         B     XIT1                                                             
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        CHANGE STATUS OF REPORT AT CIADDR                  *                   
*************************************************************                   
         SPACE 1                                                                
STATUS   NTR1                                                                   
         LA    R1,STATTAB          STATUS CHANGE                                
STAT020  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID ACTION                               
*                                                                               
         CLC   WKACT,0(R1)         IS THIS OUR ACTION                           
         BE    STAT021                                                          
         LA    R1,2(R1)            BUMP TO NEXT                                 
         B     STAT020                                                          
*                                                                               
STAT021  SR    RF,RF               PICK UP ROUTINE NUMBER                       
         IC    RF,1(R1)                                                         
         SLL   RF,2                                                             
         EX    0,STAT030(RF)       GET A(ROUTINE) IN RF                         
         BASR  RE,RF                                                            
         B     XIT1                EXIT WHEN FINISHED                           
*                                                                               
STAT030  LA    RF,HOLDFIL          HOLD ROUTINE                                 
         LA    RF,ACTVFIL          ACTV ROUTINE                                 
         LA    RF,SENTFIL          SENT ROUTINE                                 
         LA    RF,KEEPFIL          KEEP ROUTINE                                 
         LA    RF,UNKEFIL          UNKEEP ROUTINE                               
         LA    RF,PURGFIL          PURGE ROUTINE                                
         LA    RF,RETAFIL          RETAIN ROUTINE                               
         LA    RF,DELEFIL          DELETE ROUTINE                               
*                                                                               
STATTAB  DC    X'2200'             HOLD                                         
         DC    X'2401'             ACTV                                         
         DC    X'2A02'             SENT                                         
         DC    X'2503'             KEEP                                         
         DC    X'2604'             UNKEEP                                       
         DC    X'2305'             PURGE                                        
         DC    X'2906'             RETAIN                                       
         DC    X'2B07'             DELETE                                       
         DC    X'FFFF'             EOT                                          
         EJECT                                                                  
*************************************************************                   
*        INITIALISE & READ RECORD CIPASS=TTTTBB00/TTTTT0RR 16/20BIT             
*************************************************************                   
READREC  NTR1                                                                   
         L     R5,ACXREC           READ RECORD INTO INDEX AREA                  
         MVC   WRKZID,=CL8'WRKZ'                                                
         MVC   WRKZCHR,WKPASS                                                   
         BAS   RE,SETBUFF                                                       
         MVC   CIADDR,CIPASS       SET DISK ADDR FROM CIPASS                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,B'1110',CIADDR                                                
*                                                                               
         LTR   R0,R0               Any track set ?                              
         BZ    ERR15                                                            
*                                                                               
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFWKINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUWFFILE                                   
         GOTO1 ADATAMGR,WKDMCB,(X'03',READ)                                     
         CLI   8(R1),0                                                          
         BE    READRX                                                           
         CLI   RTNERR,YES                                                       
         BNE   READR10                                                          
         MVC   RTNERR,8(R1)                                                     
         B     READRX              Exit with error in RTNERR                    
*                                                                               
READR10  CLI   8(R1),X'10'         TEST NOT FOUND                               
         BE    ERR6                                                             
         CLI   8(R1),X'41'         TEST FORMAT ERROR                            
         BE    ERR15                                                            
         CLI   8(R1),X'90'         TEST EOF                                     
         BE    READRX                                                           
         DC    H'0'                                                             
*                                                                               
READRX   LR    R1,R5                                                            
XIT1     XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO HOLD                         *                   
*************************************************************                   
         SPACE 1                                                                
HOLDFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    W_STAT,W_STAC       TEST FILE IS ACTIVE                          
         BNO   ERR7                                                             
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTHOL       SET TO HOLD                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
HOLDFILX B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        SET FILE AT CIPASS TO ACTV                         *                   
*************************************************************                   
         SPACE 1                                                                
ACTVFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    W_STAT,W_STDL       TEST FOR DELETED                             
         BO    ACTV010                                                          
         TM    W_STAT,W_STPR       TEST FOR PROCESSED                           
         BO    ACTV010                                                          
         TM    W_STAT,W_STAC       TEST FILE IS NOT ACTIVE                      
         BO    ERR8                                                             
ACTV010  GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTACT       SET TO ACTV                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
ACTVFILX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO KEEP                         *                   
*************************************************************                   
         SPACE 1                                                                
KEEPFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    W_STAT,W_STKE       TEST FILE IS NOT KEEP                        
         BO    ERR9                                                             
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTKEE       SET TO KEEP                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
KEEPFILX B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        SET FILE AT CIPASS TO UNKEEP                       *                   
*************************************************************                   
         SPACE 1                                                                
UNKEFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    W_STAT,W_STKE       TEST FILE IS NOT KEEP                        
         BNO   ERR10                                                            
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTUNK       SET TO UNKEEP                                
         GOTO1 AWKUPDT                                                          
*                                                                               
UNKEFILX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO SENT                         *                   
*************************************************************                   
         SPACE 1                                                                
SENTFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    W_STAT,W_STAC       TEST FILE IS ACTIVE                          
         BNO   ERR7                                                             
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTSEN       SET TO SENT                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
SENTFILX B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        SET FILE AT CIPASS TO DELETED                      *                   
*************************************************************                   
         SPACE 1                                                                
DELEFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTDEL       SET TO DELETED                               
         GOTO1 AWKUPDT                                                          
*                                                                               
DELEFILX B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        PURGE FILE AT CIPASS                               *                   
*************************************************************                   
         SPACE 1                                                                
PURGFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTPUR       SET TO PURGE                                 
         GOTO1 AWKUPDT                                                          
*                                                                               
PURGFILX B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET RETAIN TIME OF FILE AT CIPASS SBYTE=HRS                                   
***********************************************************************         
RETAFIL  NTR1                                                                   
*                                                                               
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING W_RECD,R5           R5=WORKER RECORD                             
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
*                                                                               
         XC    NXINFO,NXINFO       PUT HRS INTO INFO                            
         MVC   NXINFO+1(1),SBYTE                                                
         CLI   SBYTE,255           255=PERMANENT                                
         BNE   *+10                                                             
         MVC   NXINFO(2),=X'FFFF'                                               
         OI    NXFLAG,UKFLHRS      SET HRS INDICATOR                            
*                                                                               
         MVC   STAACT,ACTRET       SET TO RETAIN                                
         GOTO1 AWKUPDT                                                          
*                                                                               
RETAFILX B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLEAR ALL FILES FOR A USER (OR ALL USERS)                                     
***********************************************************************         
CLR      DS    0H                                                               
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKZ IF NOT ALREADY                     
         MVI   SFULL,0             SET NON UPDATIVE FIRST PASS                  
         CLI   IFDDSV,C'F'         DDS=F IS THE SECRET CODE                     
         BNE   CLR1                TO FORCE UPDATE                              
*                                                                               
CLR0     MVI   SFULL,1             DO UPDATING NOW                              
*                                                                               
CLR1     BAS   RE,CXLOOPI          INIT INDEX LOOP                              
         USING W_RECD,R5           R5=A(WRKZ INDEX ENTRY)                       
*                                                                               
CLR2     BAS   RE,GETXAD                                                        
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),WRKZID,CXADDR,ACXREC                
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
*                                                                               
CLR4     OC    USERID,USERID       CAN BE ALL FOR DDS                           
         BZ    *+14                                                             
         CLC   W_USRID,USERID      ELSE MATCH USERID                            
         BNE   CLR8                                                             
         CLI   SFULL,0             PASS 1 CHECK SENT/PURGED                     
         BNE   CLR6                                                             
         TM    W_STAT,W_STLIVE+W_STRUN+W_STKE                                   
         BZ    CLR8                                                             
         B     ERR12               ERROR NOT PURGED/PRINTED                     
*                                                                               
CLR6     BAS   RE,GETCAD           PASS 2 ZAP CIREC INDEX                       
         L     R6,ACIREC                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),WRKZID,CIADDR,(R6)                  
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
         XC    0(L'W_INDEX,R6),0(R6)                                            
         GOTO1 (RF),(R1),(X'00',DMWRT)                                          
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
*                                                                               
         LH    RE,CXENTRY          ZAP INDEX INDEX AND WRITE BACK               
         MH    RE,CINDXLN                                                       
         A     RE,ACXREC                                                        
         XC    0(L'W_INDEX,RE),0(RE)                                            
         GOTO1 (RF),(R1),,,CXADDR,ACXREC                                        
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
*                                                                               
CLR8     BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CLR4                                                             
         B     CLR2                END OF INDEX PAGE                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    CLR8A               NO PART2 INDEX                               
         CLI   SFULL,0                                                          
         BE    CLR0                ONLY NEED PART1 FOR STATIC TEST              
         CLI   SFULL+1,0                                                        
         BNE   CLRA                EXIT IF END OF PART2 INDEX                   
         MVI   SFULL+1,1                                                        
         BAS   RE,CXLOOPJ          SET TO UPDATE PART2 INDEX                    
         B     CLR2                                                             
*                                                                               
CLR8A    CLI   SFULL,0                                                          
         BE    CLR0                PASS 1 FINISHED                              
*                                                                               
CLRA     MVC   SRVMSG(17),=C'ALL FILES CLEARED'                                 
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
CLRERR   BAS   RE,W_UNLK                                                        
         MVI   LOCKT,0                                                          
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* INIT BUFFER FOR WRKZID R5 MUST POINT TO A BUFFER                              
***********************************************************************         
SETBUFF  ST    RE,SAVERE                                                        
*                                                                               
         GOTO1 ADATAMGR,WKDMCB,(X'00',BUFFER),WRKZID,,,(R5)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R5)         SAVE V(W_TAB)/V(W_FILE)                   
         MVC   CIDATA(L'CIDATA),12(R5) SAVE FILE DATA FOR THIS WRKZ             
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD SPPSDDC IN WORK FROM W_KEY IN DUB                                       
***********************************************************************         
GETKEY   ST    RE,SAVERE                                                        
*                                                                               
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(4),DUB+2       DISPLAY SYSPRG & SUBPRG                      
         LA    RF,WORK+4                                                        
*                                                                               
         CLI   DUB+6,X'5C'         TEST FOR "*"                                 
         BE    GETK020                                                          
         CLI   DUB+6,X'C1'         TEST FOR A-9                                 
         BNL   GETK020                                                          
         IC    R1,DUB+6            MUST BE PACKED DAY                           
         MVC   1(1,RF),DUB+6                                                    
         OC    1(1,RF),=X'F0'      UNPACK AND DISPLAY                           
         SRL   R1,4                                                             
         STC   R1,0(RF)                                                         
         OC    0(1,RF),=X'F0'                                                   
         LA    RF,2(RF)            BUMP 2 CHRS                                  
         B     GETK026                                                          
*                                                                               
GETK020  MVC   0(1,RF),DUB+6       SINGLE CHR DAY                               
GETK025  LA    RF,1(RF)                                                         
*                                                                               
GETK026  MVC   0(1,RF),DUB+7       DISPLAY CLASS                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* BUILD ATTB LINE FROM BITS IN BYTE R1=A(TABLE)                                 
***********************************************************************         
GETBITS  ST    RE,SAVERE                                                        
*                                                                               
         MVC   WORK,SPACES         USE WORK AS OUTPUT AREA                      
         LA    RE,WORK                                                          
         LA    RF,X'80'            START WITH X'80'                             
GETB010  EX    RF,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              EXECUTED TEST                                
         BZ    *+14                                                             
         MVC   0(4,RE),0(R1)       COPY XXX/ FROM TABLE                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)            BUMP TO NEXT ENTRY                           
         SRL   RF,1                BUMP TO NEXT BIT                             
         LTR   RF,RF                                                            
         BNZ   GETB010             CONTINUE UNTIL ZERO                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET RET HOURS  HALF=RDATE BYTE=RTIME RET FULL=HHH                             
***********************************************************************         
GETHOUR  NTR1                                                                   
*                                                                               
         XC    FULL,FULL                                                        
         CLC   HALF,=X'FF9F'       TEST FOR PERMANENT                           
         BE    HOUR045                                                          
         MVC   DUB(2),HALF         GET RETAIN DATE                              
         CLC   DUB(2),DATE2        CHECK WITH TODAY                             
         BL    HOUR042                                                          
         GOTO1 ADATCON,DMCB,(2,DUB),(0,DUB1)                                    
         GOTO1 APERVERT,DMCB,DATE,DUB1                                          
         LH    R1,DMCB+8           NUMBER OF DAYS                               
         BCTR  R1,0                                                             
         MH    R1,=H'1440'         24 HRS = 1440 MINS                           
         STH   R1,HALF                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,BYTE             GET RETAIN TIME                              
         MH    R1,=H'10'                                                        
         ST    R1,DUB1                                                          
         BAS   RE,GETTIME          GET TIME NOW                                 
*                                                                               
         L     RF,DUB1             RETAIN TIME                                  
         L     RE,FULL1            TIME NOW                                     
         SR    RF,RE                                                            
         AH    RF,HALF             ADD 24 HRS PER DAY                           
         BM    HOUR042                                                          
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         LTR   RF,RF                                                            
         BZ    HOUR043                                                          
         CH    RF,=H'99'                                                        
         BH    HOUR044                                                          
         LR    R1,RF                                                            
         BAS   RE,EDIT                                                          
         MVC   FULL,SWORK                                                       
         B     XIT1                                                             
HOUR042  MVI   FULL,C'0'           SHOW EXPIRED                                 
         SR    RF,RF               SET TO ZERO HRS FOR COLOUR                   
         B     XIT1                                                             
HOUR043  MVC   FULL,=C'<1 '        SOON TO EXPIRE                               
         B     XIT1                                                             
HOUR044  MVC   FULL,=C'>> '        GREATER THAN 99 HRS                          
         B     XIT1                                                             
HOUR045  MVC   FULL,=C'PER'        PERMANENT                                    
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERRORS AND EXITS                                                              
***********************************************************************         
ERR0     LA    RE,SREIIS           INVALID INPUT FIELD SYNTAX                   
         B     ERRX                                                             
ERR1     LA    RE,286              MISSING FILE TYPE                            
         B     ERRX                                                             
ERR2     LA    RE,287              INVALID FILE TYPE                            
         B     ERRX                                                             
ERR3     LA    RE,288              DUPLICATE FILE TYPE                          
         B     ERRX                                                             
ERR4     LA    RE,289              INCOMPATIBLE FILE TYPE                       
         B     ERRX                                                             
ERR6     LA    RE,290              FILE NOT FOUND                               
         B     ERRX                                                             
ERR7     LA    RE,291              FILE NOT ACTIVE                              
         B     ERRX                                                             
ERR8     LA    RE,292              FILE NOT HOLD OR SENT                        
         B     ERRX                                                             
ERR9     LA    RE,293              FILE IS ALREADY KEEP                         
         B     ERRX                                                             
ERR10    LA    RE,294              FILE NOT KEEP                                
         B     ERRX                                                             
ERR11    LA    RE,295              NO FILES FOUND                               
         B     ERRX                                                             
ERR12    LA    RE,296              ALL FILES MUST BE SENT OR PRGD               
         B     ERRX                                                             
ERR13    LA    RE,282              INVALID FILE ID                              
         B     ERRX                                                             
ERR14    LA    RE,297              SENDING FILE CANT CHNG STATUS                
         B     ERRX                                                             
ERR15    LA    RE,298              FORMAT ERROR                                 
         B     ERRX                                                             
*                                                                               
ERRF     LA    RE,SREIFK1          INVALID FILTER KEYWORD=                      
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
*                                                                               
ERRFA    LA    RE,SREIFSK1         INVALID FILTER SIGN KEYWORD=                 
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
*                                                                               
ERRFB    LA    RE,SREIVFK1         INVALID VALUE FOR FILTER KEYWORD=            
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
*                                                                               
ERRX     MVI   MSGTYP,C'E'         SET ERROR TYPE                               
         STCM  RE,3,MSGNUM         SET NUMBER                                   
         L     RD,PHASERD          RESTORE RD                                   
         ST    R4,CURSOR                                                        
*                                                                               
         TM    INTFLAG,INTRUN      EXIT IF NOT INTERNAL                         
         BNO   EXIT                                                             
         OI    INTFLAG,INTERR      FLAG ERROR FOR ROOT                          
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY FILE COUNTERS                                                         
***********************************************************************         
CNTDSP   NTR1                                                                   
*                                                                               
         LA    R5,WORK                                                          
         MVI   0(R5),C' '                                                       
         MVC   1(99,R5),0(R5)                                                   
*                                                                               
CNTD2    CP    QR,=P'0'            TOTAL                                        
         BE    CNTD4                                                            
         MVCDD 0(4,R5),SR#TOTAL                                                 
         MVI   9(R5),C','                                                       
         EDIT  (P3,QR),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD4    CP    QA,=P'0'            ACTIVE                                       
         BE    CNTD6                                                            
         MVCDD 0(4,R5),SR#ACTV                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QA),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD6    CP    QH,=P'0'            HOLD                                         
         BE    CNTD8                                                            
         MVCDD 0(4,R5),SR#HOLD                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QH),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD8    CP    QK,=P'0'            KEEP                                         
         BE    CNTDA                                                            
         MVCDD 0(4,R5),SR#KEEP                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QK),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDA    CP    QD,=P'0'            PRINTED                                      
         BE    CNTDC                                                            
         MVCDD 0(4,R5),SR#PRTD                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QD),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDC    CP    QE,=P'0'            SENT                                         
         BE    CNTDE                                                            
         MVCDD 0(4,R5),SR#SENT                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QE),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDE    CP    QG,=P'0'            SENDING                                      
         BE    CNTDG                                                            
         MVCDD 0(4,R5),SR#SNDG                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QG),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDG    BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         LA    R5,WORK             SQUASH AND PUT IN EQUAL SIGNS                
         LA    R6,99                                                            
         GOTO1 ASQUASH,DMCB,(R5),(C'=',(R6))                                    
*                                                                               
         MVC   SRVINF+14(L'SRVINF-14),0(R5)     MOVE TO TWA FIELD               
         TM    DDS,DDSTRM                                                       
         BZ    CNTDH                                                            
         CLI   WRKZCHR,C'I'         TEST DDS TERM AND MULTIPLE WRKZS            
         BE    CNTDSPX                                                          
         MVC   SRVINF(6),=C'WRKZ#N'                                             
         MVC   SRVINF+5(1),WRKZCHR                                              
         B     CNTDSPX                                                          
*                                                                               
CNTDH    MVCDD SRVINF(13),SR#RPTCT FILE COUNT FOR NON DDS TRMS                  
*                                                                               
CNTDSPX  B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* TIME ROUTINES                                                                 
***********************************************************************         
TIMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
         CLI   DUB,23                                                           
         BH    TIMEOUTX                                                         
         CLI   DUB+1,59                                                         
         BH    TIMEOUTX                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+2(2),DUB1+6(2)                                               
*                                                                               
         MVI   DUB+4,C':'                                                       
         SR    R0,R0                                                            
         IC    R0,DUB+1                                                         
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+5(2),DUB1+6(2)                                               
*                                                                               
TIMEOUTX BR    RE                                                               
*                                                                               
GETTIME  ST    RE,SAVERE                                                        
         TBIN  SECS                                                             
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY MINS                               
         ST    R1,FULL1                                                         
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY HOURS                              
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT FILE STATUS                                                            
***********************************************************************         
STATOUT  NTR1                                                                   
*                                                                               
         CLI   CISTAT,W_STPU       PURGED STATUS                                
         BNE   STOU1                                                            
         MVCDD SWORK(8),SR#PRGED                                                
         MVC   SFULL,=C'****'                                                   
         B     STATOUTX                                                         
*                                                                               
STOU1    MVC   SFULL(4),SR@DELD    TEST DELETED FIRST                           
         MVC   SWORK(8),SR@DELD                                                 
         TM    CISTAT,W_STDL                                                    
         BO    STOUT90                                                          
         MVC   SFULL(4),SR@HOLD    TEST HOLD                                    
         MVC   SWORK(8),SR@HOLD                                                 
         TM    CISTAT,W_STHO                                                    
         BO    STOUT90                                                          
         MVC   SFULL(4),SR@RNING   TEST RUNNING                                 
         MVC   SWORK(4),SR@RNING                                                
         CLI   CISTAT,W_STAC+W_STRUN                                            
         BE    STOUT90                                                          
         MVC   SFULL(4),SR@SNDG    TEST SENDING                                 
         MVC   SWORK(4),SR@SNDG                                                 
         CLI   CISTAT,W_STAC+W_STSEN                                            
         BE    STOUT90                                                          
         MVC   SFULL(4),SR@CRTG    TEST CREATING                                
         MVC   SWORK(4),SR@CRTG                                                 
         CLI   CISTAT,W_STAC+W_STCRE                                            
         BE    STOUT90                                                          
         MVC   SFULL(4),SR@PROC    TEST PROCESSED                               
         MVC   SWORK(4),SR@PROC                                                 
         CLI   CISTAT,W_STAC+W_STPR                                             
         BE    STOUT90                                                          
         MVC   SFULL(4),SR@SENT    TEST SENT                                    
         MVC   SWORK(4),SR@SENT                                                 
         TM    CISTAT,W_STSE                                                    
         BO    STOUT90                                                          
         MVC   SFULL(4),SR4ACTV    TEST ACTV                                    
         MVC   SWORK(8),SR@ACTV                                                 
         TM    CISTAT,W_STAC                                                    
         BO    STOUT90                                                          
*                                                                               
         MVC   SFULL(4),=C'Unkn'   SET TO UNKNOWN                               
         MVC   SWORK(8),=C'Unknown '                                            
*                                                                               
STOUT90  TM    CISTAT,W_STKE       SUB-STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         MVI   SWORK+4,C','                                                     
         MVCDD SWORK+5(8),SR#KEEP                                               
         MVI   SFULL+2,C','                                                     
         MVC   SFULL+3(1),SR@KEEP                                               
*                                                                               
STATOUTX B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* WRKZ ROUTINES                                                                 
***********************************************************************         
       ++INCLUDE DMWRKZR                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONSTANTS & TABLES                                                            
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
RTNERR   DC    C'0'                                                             
MAXSEQ   DC    F'65000'                                                         
DSPMAX   DC    PL2'16'                                                          
QTMAX    DC    PL2'600'                                                         
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
READ     DC    CL8'READ  '                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DC@XSORT DC    CL5'XSORT'                                                       
DC@SHIT  DC    CL4'SHIT'                                                        
DC@OBJ   DC    CL4'OBJ '                                                        
FFS      DC    18X'FF'                                                          
DOTS     DC    16C'.'                                                           
SPACES   DC    80C' '                                                           
PURGEMSG DC    CL32'File has been purged '                                      
FRMATMSG DC    CL16'Format error'                                               
RPTNFMSG DC    CL16'File not found'                                             
SIZEL    DC    CL78'PART1 CI TOTAL=1234,AVAIL=1234,INUSE=1234,INDEX=123X        
               4        '                                                       
*                                                                               
ACTACT   DC    CL3'ACT'                                                         
ACTHOL   DC    CL3'HOL'                                                         
ACTKEE   DC    CL3'KEE'                                                         
ACTUNK   DC    CL3'UNK'                                                         
ACTPUR   DC    CL3'PUR'                                                         
ACTRET   DC    CL3'RET'                                                         
ACTSEN   DC    CL3'SEN'                                                         
ACTDEL   DC    CL3'DEL'                                                         
TOG      DC    CL3'TOG'                                                         
                                                                                
*----------------------------------------------------------------------         
* TABLE OF VALID SORT KEYWORDS                                                  
*        LA RF,KEYWORD,(+VE ACTN),(-VE ACTN),LEN,N/D                            
*----------------------------------------------------------------------         
         DS    0H                                                               
SORTTBL  DS    0CL8                                                             
         DC    X'41F0',S(SR@ALPHA),X'01',X'01',X'05',X'00'                      
         DC    X'41F0',S(SR@CRTD),X'02',X'03',X'07',X'00'                       
         DC    X'41F0',S(SR@SENT),X'04',X'05',X'07',X'00'                       
         DC    X'41F0',S(SR@RTAIN),X'06',X'07',X'06',X'00'                      
         DC    X'41F0',S(SR@SIZE),X'08',X'09',X'04',X'00'                       
SORTBLX  DC    X'00'                                                            
                                                                                
*----------------------------------------------------------------------         
* TABLE OF VALID FILE STATS AND BIT VALUES                                      
*----------------------------------------------------------------------         
         DS    0H                                                               
STATTBL  DS    0CL6                                                             
         DC    X'41F0',S(SR8ALL),X'FF',X'0'                                     
         DC    X'41F0',S(SR@LIVE),X'E0',X'0' LIVE=ACTV/HOLD/PROC                
         DC    X'41F0',S(SR@ACTV),X'80',X'0'                                    
         DC    X'41F0',S(SR4ACTV),X'80',X'0'                                    
         DC    X'41F0',S(SR@HOLD),X'40',X'0'                                    
         DC    X'41F0',S(SR@PROC),X'20',X'0'                                    
         DC    X'41F0',S(SR@DEAD),X'14',X'0' DEAD=SENT/DELD                     
         DC    X'41F0',S(SR@DELD),X'04',X'0'                                    
         DC    X'41F0',S(SR@SENT),X'10',X'0'                                    
         DC    X'41F0',S(SR@KEEP),X'08',X'0'                                    
         DC    X'41F0',S(SR@RNING),X'03',X'0'                                   
         DC    X'41F0',S(SR@SNDG),X'02',X'0'                                    
         DC    X'41F0',S(SR@CRTG),X'01',X'0'                                    
STATTBLX DC    X'00'                                                            
                                                                                
*----------------------------------------------------------------------         
* TABLE OF VALID FILE ATTRIBUTES AND BIT VALUES                                 
*----------------------------------------------------------------------         
         DS    0H                                                               
ATTBTBL  DS    0CL6                                                             
         DC    X'41F0',S(DC@OBJ),X'80',X'0'                                     
         DC    X'41F0',S(SR@ERROR),X'20',X'0'                                   
         DC    X'41F0',S(SR@PSWD),X'10',X'0'                                    
         DC    X'41F0',S(DC@SHIT),X'01',X'0'                                    
ATTBTBLX DC    X'00'                                                            
*                                                                               
ATTINDS  DC    C'Obj/AT2/Err/Pwd/AT5/Xtn/AT7/Sht'                               
                                                                                
*----------------------------------------------------------------------         
* TABLE OF VALID KEYWORDS FOR FILTERS                                           
* CL8    KEYWORD NAME                                                           
* XL1    KEYWORD NUMBER                                                         
* XL1    KEYWORD SIGN BITS X'80'=NE,X'40'=LT,X'20'=GT                           
* XL1    KEYWORD VALD BITS X'01'=DDSONLY                                        
*----------------------------------------------------------------------         
         DS    0H                                                               
FILTTBL  DS    0CL8                                                             
         DC    X'41F0',S(SR@CLASS),X'01',X'80',X'00',X'0'  CLASS                
         DC    X'41F0',S(SR@DATE),X'02',X'E0',X'00',X'0'   DATE                 
         DC    X'41F0',S(SR@DDS),X'03',X'00',X'01',X'0'    DDS                  
         DC    X'41F0',S(SR@SIZE),X'04',X'E0',X'01',X'0'   SIZE FACTOR          
         DC    X'41F0',S(SR@SORT),X'05',X'00',X'00',X'0'   SORT ORDER           
         DC    X'41F0',S(DC@XSORT),X'F5',X'00',X'01',X'0'  SORT U=XXX           
         DC    X'41F0',S(SR@TIME),X'06',X'60',X'00',X'0'   TIME                 
         DC    X'41F0',S(SR@FMT),X'07',X'00',X'01',X'0'    DISP FRMT            
         DC    X'41F0',S(SR@CDATE),X'08',X'E0',X'00',X'0'  DT CREATED           
         DC    X'41F0',S(SR@SDATE),X'09',X'E0',X'00',X'0'  DT SENT              
         DC    X'41F0',S(SR@RDATE),X'0A',X'E0',X'00',X'0'  DT RETAINED          
         DC    X'41F0',S(SR@TYPE),X'0B',X'80',X'00',X'0'   TYPE                 
         DC    X'00'                                                            
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* I TABLE                                                                       
*----------------------------------------------------------------------         
I168     DC    C'I',AL2(168)       N FILES PAGE N OF N DISPLAYED                
I188     DC    C'I',AL2(188)       FILE SELECTED (NEW STATUS)                   
I189     DC    C'I',AL2(189)       FILET ATTRIBS DISPLAYED                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* TABLE OF COMPATIBLE ACTNS & STATS                                             
*----------------------------------------------------------------------         
COMPTBL  DS    0CL2                                                             
*                                                                               
         DC    X'2100'             DISPLAY...OPTIONAL STAT                      
         DC    X'21FF'             DEFAULT ALL                                  
         DC    X'21FF'             VALID ANY                                    
*                                                                               
         DC    X'2200'             HOLD......OPTIONAL STAT                      
         DC    X'2280'             ACTV                                         
*                                                                               
         DC    X'2301'             PURGE.....REQUIRED STAT                      
         DC    X'23FF'             ALL                                          
         DC    X'23FF'             VALID ANY                                    
*                                                                               
         DC    X'2401'             ACTIVATE..REQUIRED STAT                      
         DC    X'2440'             DEFAULT HOLD                                 
         DC    X'24FF'             VALID ANY                                    
*                                                                               
         DC    X'2501'             KEEP......REQUIRED STAT                      
         DC    X'25FF'             ALL                                          
         DC    X'25FF'             ANY                                          
*                                                                               
         DC    X'2600'             UNKEEP....OPTIONAL STAT                      
         DC    X'2608'             KEEP                                         
*                                                                               
         DC    X'2701'             CLEAR.....REQUIRED STAT                      
         DC    X'27FF'             ALL                                          
*                                                                               
         DC    X'2800'             SIZE......OPTIONAL STAT                      
         DC    X'28FF'             ALL                                          
         DC    X'28FF'             ANY                                          
*                                                                               
         DC    X'2901'             RETAIN....REQUIRED STAT                      
         DC    X'29FF'             ALL                                          
         DC    X'29FF'             ANY                                          
*                                                                               
         DC    X'2B00'             DELETE .OPTIONAL STAT                        
         DC    X'2B80'             ACTV                                         
         DC    X'2BFF'             ANY                                          
*                                                                               
COMPTBLX DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
SIZEH0   DC    CL40'             ----------- Part 1 CIs ----'                   
         DC    CL39'-------  ------- Part 2 CIs --------   '                    
SIZEH1   DC    CL40'Actn File     Total  Avail   Sent Active'                   
         DC    CL39'  Index   Total  Avail   Sent Active   '                    
         EJECT                                                                  
                                                                                
***********************************************************************         
* DATA DICTIONARY                                                               
***********************************************************************         
       ++INCLUDE SRWKZDD                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* LINE DSECTS                                                                   
***********************************************************************         
WKRLD    DSECT                                                                  
WKLSELH  DS    CL8                                                              
WKLSEL   DS    CL4                                                              
WKLHDR   DS    CL8                                                              
WKLDATA  DS    0CL73                                                            
*                                                                               
WKLFID   DS    CL7                                                              
         DS    CL1                                                              
WKLWKR   DS    CL1                                                              
         DS    CL1                                                              
WKLREF   DS    CL6                                                              
         DS    CL1                                                              
WKLSTAT  DS    CL4                                                              
         DS    CL1                                                              
WKLCDATE DS    CL11                                                             
         DS    CL1                                                              
WKLHOURS DS    CL3                                                              
         DS    CL1                                                              
WKLSDATE DS    CL11                                                             
         DS    CL1                                                              
WKLDESC  DS    CL16                                                             
         DS    CL1                                                              
WKLRECS  DS    CL6                                                              
WKLLEN   EQU   *-WKRLD                                                          
         EJECT                                                                  
                                                                                
SIZED    DSECT                                                                  
         DS    CL8                                                              
SZACT    DS    CL4                                                              
SZHDR    DS    CL8                                                              
SZDATA   DS    0CL73                                                            
SZQUE    DS    CL7                                                              
         DS    CL1                                                              
SZTOT1   DS    CL6                                                              
         DS    CL1                                                              
SZAVA1   DS    CL6                                                              
         DS    CL1                                                              
SZPRT1   DS    CL6                                                              
         DS    CL1                                                              
SZACT1   DS    CL6                                                              
         DS    CL1                                                              
SZIND1   DS    CL6                                                              
         DS    CL2                                                              
SZTOT2   DS    CL6                                                              
         DS    CL1                                                              
SZAVA2   DS    CL6                                                              
         DS    CL1                                                              
SZPRT2   DS    CL6                                                              
         DS    CL1                                                              
SZACT2   DS    CL6                                                              
         DS    CL2                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
       ++INCLUDE SRWKZWK                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* SCREEN DSECTS                                                                 
***********************************************************************         
SRWKZFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRWKZFFD                                                       
         ORG   SRVEX2H                                                          
       ++INCLUDE SRWKZFDD                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
*DDWKZSCAND                                                                     
*FASYSFAC                                                                       
*FASRS                                                                          
*DDCOMFACS                                                                      
*DDFLDHDR                                                                       
*SRERREQUS                                                                      
*FATCB                                                                          
*DMWRKZK                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDWKZSCAND                                                     
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE FATCB                                                          
       ++INCLUDE DMWRKZK                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRWKZ02   03/04/16'                                      
         END                                                                    
