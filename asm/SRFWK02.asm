*          DATA SET SRFWK02    AT LEVEL 117 AS OF 10/31/13                      
*PHASE T13D02A                                                                  
         TITLE '=FWK - FILE FUNCTIONS'                                          
         PRINT NOGEN                                                            
WKRPRT   CSECT                                                                  
         NMOD1 LWORKX-LWORKD,**=NW2**,RA,R9,RR=R4                               
         LR    R8,RC               LOCAL WORKING STORAGE                        
         LR    RC,R1                                                            
         USING LWORKD,R8                                                        
         USING FWKWKD,RC           RC=A(ROOTS WORKING STORAGE)                  
         ST    RD,PHASERD          SAVE THIS RD                                 
         ST    R4,RELO                                                          
         L     R1,APARM                                                         
         L     R2,12(R1)                                                        
         USING COMFACSD,R2         R2=A(COM FAC LIST)                           
         L     R3,20(R1)                                                        
         USING SRFWKFFD,R3         R3=A(TWA)                                    
         L     R2,ASAVESTR                                                      
         USING WKSAVED,R2                                                       
*                                                                               
         TM    INTFLAG,INTRUN      INTERNAL                                     
         BO    CON000              YES - IGNORE P3/P4 VALIDATION                
         EJECT                                                                  
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
         EJECT                                                                  
*************************************************************                   
*        VALIDATE FILTERS                                   *                   
*************************************************************                   
P4VAL    DS    0H                  P4=FILTERS NAMED BY KEYWORDS                 
         LA    R4,SRVP4H                                                        
         XC    IFFILTS(IFFILTL),IFFILTS                                         
*                                                                               
         MVI   IFSORV,X'03'        SET DEFAULT SORT TO CREATE DATE              
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
         SHI   R1,1                DECR NUM OF KEYWORDS INPUT                   
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
         EJECT                                                                  
**********************************************************************          
* CLASS VALUE                                                                   
**********************************************************************          
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
                                                                                
**********************************************************************          
* TYPE  VALUE                                                                   
**********************************************************************          
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
**********************************************************************          
* DATE  VALUE                                                                   
**********************************************************************          
DATVALR  MVC   IFCDAF,FLAG1        SET DATE FILTER INPUT FLAG                   
         LA    R7,IFCDAV                                                        
         B     DATV0                                                            
**********************************************************************          
* CREATE DATE VALUE                                                             
**********************************************************************          
CDAVALR  MVC   IFCDAF,FLAG1        SET CREATED DATE FILTER INPUT FLAG           
         LA    R7,IFCDAV                                                        
         B     DATV0                                                            
**********************************************************************          
* PRINTED DATE VALUE                                                            
**********************************************************************          
PDAVALR  MVC   IFPDAF,FLAG1        SET PRINTED DATE FILTER INPUT FLAG           
         LA    R7,IFPDAV                                                        
         B     DATV0                                                            
**********************************************************************          
* RETAIN DATE VALUE                                                             
**********************************************************************          
RDAVALR  MVC   IFRDAF,FLAG1        SET RETAINED DATE FILTER INPUT FLAG          
         LA    R7,IFRDAV                                                        
         B     DATV0                                                            
**********************************************************************          
* CHECK FOR TODAY                                                               
**********************************************************************          
DATV0    CLC   22(5,R6),SR@TODAY   CHECK FOR TODAYS DATE                        
         BNE   DATV1                                                            
         MVC   0(3,R7),DATE1                                                    
         B     DATVX                                                            
**********************************************************************          
* VALIDTE DATE                                                                  
**********************************************************************          
DATV1    GOTO1 ADATVAL,DMCB,(0,22(R6)),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   DATV3               VALID YYMMDD INPUT                           
DATV2    GOTO1 (RF),(R1),(0,22(R6))                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRFB                                                            
         MVC   DUB(2),DATE         VALID MMDD INPUT                             
DATV3    GOTO1 ADATCON,DMCB,(0,DUB),(1,(R7))                                    
DATVX    B     P4VX                                                             
         EJECT                                                                  
**********************************************************************          
* TIME VALIDATION                                                               
**********************************************************************          
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
**********************************************************************          
* DDS SPECIAL INPUT                                                             
**********************************************************************          
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
**********************************************************************          
* FORMAT FILTER                                                                 
**********************************************************************          
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
         EJECT                                                                  
**********************************************************************          
* SIZE FILTER                                                                   
**********************************************************************          
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
**********************************************************************          
* SORT FITLER                                                                   
**********************************************************************          
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
*************************************************************                   
*        CHECK ACTION/STAT COMPATIBILITY                    *                   
*************************************************************                   
STAVAL   DS    0H                                                               
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
*************************************************************                   
*        CONTROL  (WHAT NEXT?)                              *                   
*************************************************************                   
CON000   LA    R4,SRVP3H           SET R4 TO P3 FOR CURSOR                      
         CLI   WKACT,X'28'                                                      
         BE    BUILD001            SIZE (GO BUILD A TABLE)                      
         CLI   WKACT,X'27'                                                      
         BE    CLR                 CLEAR (GO CLEAR WRKF)                        
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
INDI000  DS    0H                                                               
         TM    DDS,DDSTRM          DDS TERMINAL?                                
         BNZ   *+12                                                             
         LA    RE,SREACT           NO - INVALID ACTION                          
         B     ERRX                                                             
*                                                                               
         OC    CIPASS,CIPASS       DO WE HAVE A DA ALREADY                      
         BNZ   INDI010                                                          
*                                                                               
         XC    NDX,NDX                                                          
         MVC   NXSRCID,USERID                                                   
         MVC   NXSYSPG(7),WKSYSPG                                               
         MVC   NXFILENO,WKFILEN                                                 
         GOTO1 ADATAMGR,WKDMCB,(X'08',=C'INDEX')                                
         CLI   8(R1),0                                                          
         BNE   ERR1                FILE NOT FOUND                               
*                                                                               
         MVC   CIPASS,NXCIADDR                                                  
         MVC   CIPASS+2(2),=X'0100'                                             
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
BUILDTAB TM    DDS,DDSNEW          TEST FOR READ WK FLAG                        
         BNO   CIUNCO              GO UNCOMPRESS SAVED STORAGE                  
*                                                                               
BUILD001 LA    R6,WORK                                                          
         USING WSBLOCKD,R6         BUILD SCAN KEY                               
         XC    WSBLOCK,WSBLOCK                                                  
*                                                                               
         MVC   WSSRCID,USERID                                                   
         MVC   WSFILE,WRKFID       SCAN THIS FILE                               
*                                                                               
         MVC   WSSYSPRG,WKSYSPG                                                 
         MVC   WSSUBPRG,WKSUBPG                                                 
*                                                                               
         MVC   WSDAY,WRKDAY                                                     
         CLI   WRKDAY0,C'Y'        WAS 00 ENTRED                                
         BE    BUILD008                                                         
         OC    WSDAY,WSDAY         IS WSDAY=0                                   
         BNZ   *+8                                                              
         MVI   WSDAY,C'*'          SET TO * IF NOT 00 INPUT                     
BUILD008 MVC   WSCLAS(1),WRKCLASS                                               
*                                                                               
         MVC   WSEXTRA,WRKEXTRA                                                 
         MVC   WSSTAT,FILSTAT      STAT AND ATTB FILTERS                        
*                                                                               
         MVC   WSAGESF,IFSIZF      AGE SIZE                                     
         MVC   WSAGESV,IFSIZV                                                   
         MVC   WSAGELF,IFCDAF      CDATE                                        
         MVC   WSAGELV,IFCDAV                                                   
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
*                                                                               
BUILD090 DS    0H                                                               
*                                                                               
         GOTO1 AWRSCAN,DMCB,(R6),ACTREC,AFILTAB,ACXREC,ACOMFACS                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERROR ON WRKF SCAN                           
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
         LA    R0,(L'QNDATA/3)                                                  
         LA    RE,QNDATA                                                        
         LR    RF,R4                                                            
         CLM   R1,1,=X'1'                                                       
         BNE   *+10                                                             
         ZAP   QT,=P'0'            LAST QT IS TRUE TOTAL                        
*                                                                               
         AP    0(3,RE),0(3,RF)     ACCUMULATE COUNTS                            
         LA    RF,3(RF)                                                         
         LA    RE,3(RE)                                                         
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
*************************************************************                   
*        COMPRESS FILTAB INTO CISAVEDS AT WKSCIADS          *                   
*************************************************************                   
CICOMP   L     R6,AFILTAB          COMPRESS FROM R6 TABLE                       
         USING WSEDATAD,R6                                                      
         LA    R7,WKSCIADS         TO R7 TABLE                                  
         USING CISAVED,R7                                                       
         CLC   WSEDATA,FFS         TEST FOR NO FILES FOUND                      
         BE    ERR11                                                            
*                                                                               
COMP010  XC    CISAVE,CISAVE       CLEAR ENTRY                                  
         MVC   CISWRKF,WSEWRKF                                                  
         MVC   CISADR,WSECIAD      SAVE CIADR                                   
         CLC   WSEDATA,FFS                                                      
         BE    COMP020                                                          
         LA    R6,L'WSEDATA(R6)    BUMP TO NEXT ENTRY                           
         LA    R7,L'CISAVE(R7)                                                  
         B     COMP010                                                          
*                                                                               
COMP020  MVC   CISAVE,FFS          MARK EOT                                     
         B     GRP010                                                           
         DROP  R6,R7                                                            
*************************************************************                   
*        UNCOMPRESS CISAVEDS INTO WKSCIADS                  *                   
*************************************************************                   
CIUNCO   EQU   *                                                                
         L     R6,AFILTAB          UNCOMPRESS TO R6 TABLE                       
         USING WSEDATAD,R6                                                      
         LA    R7,WKSCIADS         FROM R7 TABLE                                
         USING CISAVED,R7                                                       
         CP    QT,=P'0'            TEST NO FILES                                
         BE    ERR11                                                            
*                                                                               
UNCO010  XC    WSEDATA,WSEDATA                                                  
         MVC   WSEWRKF,CISWRKF                                                  
         MVC   WSECIAD,CISADR           RESTORE CIADR                           
         MVC   WSESORT+0(1),CISFLAG     SAVE FLAG IN SORT                       
         MVC   WSESORT+1(1),CISACTN     SAVE ACTN IN SORT+1                     
         CLC   CISAVE,FFS                                                       
         BE    UNCO020                                                          
         LA    R6,L'WSEDATA(R6)    BUMP TO NEXT ENTRY                           
         LA    R7,L'CISAVE(R7)                                                  
         B     UNCO010                                                          
*                                                                               
UNCO020  MVC   WSEDATA,FFS         MARK EOT                                     
         B     GRP010                                                           
         DROP  R6,R7                                                            
         EJECT                                                                  
*************************************************************                   
*        CHECK FOR GROUP STATUS CHANGE                      *                   
*************************************************************                   
GRP010   CLI   WKACT,X'21'         TEST DISP ONLY                               
         BE    GRP090                                                           
*                                                                               
         LA    R7,WKSCIADS         GO THROUGH SAVE LIST                         
         USING CISAVED,R7                                                       
         MVC   SBYTE,WKACT2        SAVE ANY NUMERICS IN SBYTE                   
*                                                                               
GRP020   MVC   WKPASS,CISWRKF      CHANGE STATUS OF ALL                         
         MVC   CIPASS,CISADR                                                    
         MVC   CIPASS+2(2),=X'0100'                                             
         BAS   RE,STATUS                                                        
         LA    R7,L'CISAVE(R7)                                                  
         CLC   CISAVE,FFS                                                       
         BNE   GRP020                                                           
*                                                                               
GRP090   BAS   RE,FILELIST         DISPLAY LIST                                 
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY SIZE DETAILS                               *                   
*************************************************************                   
SIZE000  MVC   SRVEX2,SIZEH0                                                    
         MVC   SRVHD1,SIZEH1                                                    
*                                                                               
         LA    R4,SRVSA1H                                                       
         USING SIZED,R4                                                         
         L     R6,ACTREC                                                        
         USING WSCOUNTD,R6                                                      
*                                                                               
         MVC   SZQUE,WRKFID                                                     
         GOTO1 ADATAMGR,WKDMCB,(0,BUFFER),WRKFID,,,ACXREC                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACXREC                                                        
         MVC   BUFFDATA,0(R1)                                                   
         MVC   CIDATA,12(R1)                                                    
         CLI   WKACT1,1                                                         
         BE    NSIZEPC                                                          
         CLI   WKACT1,2                                                         
         BE    NSIZEFIL                                                         
         EJECT                                                                  
********************************************************                        
*        WRKF SIZE CONTROL INTERVAL COUNTS             *                        
********************************************************                        
         SR    R1,R1                                                            
         ICM   R1,3,CICITOT        TOTAL PART1                                  
         BAS   RE,EDIT1                                                         
         MVC   SZTOT1,SWORK                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,CICITOT        TOTAL-INDEX-INUSE=AVAILABLE                  
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
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT        R1=TOTAL PART2                               
         BAS   RE,EDIT1                                                         
         MVC   SZTOT2,SWORK                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,CJCITOT        R1=TOTAL PART2                               
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
********************************************************                        
*        WRKF SIZE CONTROL INTERVAL PERCENTAGES        *                        
********************************************************                        
NSIZEPC  SR    R1,R1                                                            
         ICM   R1,3,CICITOT        TOTAL PART1                                  
         BAS   RE,EDIT1                                                         
         MVC   SZTOT1,SWORK                                                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
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
         EDIT  (B2,CJCITOT),(6,SZTOT2)    TOTAL PART1                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CJCITOT                                                     
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
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,WKLDATA+15                                                    
         BAS   RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   WKLDATA(14),=C'TRKS FOR PART2'                                   
         OI    WKLHDR+6,X'80'                                                   
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
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
         EJECT                                                                  
NSIZENXT LA    R4,93(R4)                  NEXT LINE                             
         LA    R6,48(R6)                  NEXT COUNTS                           
         MVC   SRVMSG(30),=C'FILE SIZE ATTRIBUTES DISPLAYED'                    
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        THESE ROUTINES SAVE ON PROGRAM SPACE               *                   
*************************************************************                   
EDIT     MVC   SWORK,SPACES        EDIT FROM R1 TO SWORK                        
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK,ALIGN=LEFT                           
         BR    RE                  R0=LEN ON EXIT                               
EDIT1    MVC   SWORK,SPACES        EDIT FROM R1 TO SWORK                        
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK                                      
         BR    RE                                                               
*                                                                               
CALCPCS  SR    R1,R1               CALCULATE RF PERCENTAGE OF CICITOT           
         ICM   R1,3,CICITOT                                                     
         B     *+10                                                             
CALCPC2  SR    R1,R1               CALCULATE RF PERCENTAGE OF CJCITOT           
         ICM   R1,3,CJCITOT                                                     
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
*                                                                               
         LLC   R1,WKSPAGE          CALCULATE INDEX INTO DATA                    
         MHI   R1,(16*L'WSEDATA)                                                
         AR    R6,R1               INDEX INTO DATA                              
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
         BE    FLST012                                                          
         LA    R1,8(R1)            TRY NEXT ENTRY                               
         CLI   0(R1),0                                                          
         BNE   *-18                                                             
         B     FLST020                                                          
*                                                                               
FLST012  EX    0,0(R1)                                                          
         XC    WKLSEL,WKLSEL                                                    
         MVI   WKLSEL,C'*'         SET *A TO SHOW COMPLETED ACT                 
         MVC   WKLSEL+1(1),0(RF)                                                
         B     FLST020                                                          
*                                                                               
FLST015  ST    R4,CURSOR           SET CURSOR FOR ERRORS                        
*                                                                               
FLST020  MVC   WKPASS,WSEWRKF                                                   
         MVC   CIPASS(2),WSECIAD   GET CIADDR FROM TABLE                        
         MVC   CIPASS+2(2),=X'0100' RECORD 1                                    
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         OC    WKFILNO,WKFILNO     CHECK WE HAVE FILE NUMBER                    
         BNZ   FLST030                                                          
*                                                                               
         MVC   WKLDATA,PURGEMSG    SHOW PURGED MESSAGE                          
         B     FLST059                                                          
*                                                                               
FLST030  MVC   WKLDATA,SPACES      CLEAR DATA LINE                              
         MVC   DUB,WKBKEY+2                                                     
         BAS   RE,GETKEY                                                        
         MVC   WKLFID,WORK                                                      
*                                                                               
         SR    R1,R1               EDIT FILE NUMBER                             
         ICM   R1,3,WKFILNO                                                     
         EDIT  (R1),(5,WKLREF),FILL=0                                           
*                                                                               
         MVC   CISTAT,WKSTAT       DISPLAY STATUS                               
         BAS   RE,STATOUT                                                       
         MVC   WKLSTAT,SFULL                                                    
*                                                                               
         CLI   WKDATEC+1,X'01'     CREATED DATE AND TIME                        
         BL    FLST032                                                          
         CLI   WKDATEC+1,X'12'                                                  
         BH    FLST032                                                          
         CLI   WKDATEC+2,X'01'                                                  
         BL    FLST032                                                          
         CLI   WKDATEC+2,X'31'                                                  
         BH    FLST032                                                          
*                                                                               
         GOTO1 ADATCON,DMCB,(X'41',WKDATEC),(7,WKLCDATE)                        
         B     FLST033                                                          
*                                                                               
FLST032  GOTO1 AHEXOUT,DMCB,WKDATEC,WKLCDATE,3,TOG                              
*                                                                               
FLST033  MVC   DUB(2),WKTIMEC                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   WKLCDATE+6(5),DUB+2                                              
*                                                                               
         MVC   WKLRDAYS,DOTS                                                    
*                                                                               
         CLI   WKCOMNT,C'S'        SPECIAL STATUS CHANGE COMMENT                
         BNE   FLST040                                                          
         CLI   WKCOMNT+10,X'12'    AND VALID MONTH IN +10                       
         BH    FLST040                                                          
         CLI   WKCOMNT+10,X'00'                                                 
         BE    FLST040                                                          
*                                                                               
         GOTO1 ADATCON,DMCB,(X'41',WKCOMNT+9),(7,WKLDESC+8)                     
         ICM   R1,15,WKCOMNT+12                                                 
         SLL   R1,4                                                             
         ST    R1,DUB                                                           
         BAS   RE,TIMEOUT          SHOW "JOBNAME DATE/TIME"                     
         MVI   WKLDESC+13,C'/'                                                  
         MVC   WKLDESC+14(5),DUB+2                                              
         MVC   WKLDESC(8),WKCOMNT+2                                             
         B     FLST050                                                          
*                                                                               
FLST040  MVC   WKLDESC(16),WKCOMNT NORMAL COMMENT OUT                           
*                                                                               
FLST050  TM    DDS,DDSTOTL+DDSUSR+DDSGEN                                        
         BZ    FLST051                                                          
*                                                                               
         CLC   GIPREV,WKUSRID      SHOW COUNTS IF TOTALS DISPLAY                
         BE    FLST051                                                          
         MVC   GIUSER,WKUSRID                                                   
         MVC   WKLDESC,SPACES                                                   
         GOTO1 AGETUSER                                                         
         MVC   WKLDESC+0(2),=C'U='                                              
         MVC   WKLDESC+2(8),GIUSERID                                            
*                                                                               
FLST051  EDIT  (B4,WKRECS),(6,WKLRECS)                                          
         EDIT  (B2,WKRECL),(4,WKLRECL)                                          
*                                                                               
         ICM   R1,15,WKRECS        CALCULATE FILE SIZE                          
         SR    RF,RF                                                            
         ICM   RF,3,WKRECL         NUM OF RECS * AVG LEN                        
         MR    RE,R1                                                            
*                                                                               
         LTR   RE,RE               TEST FOR OVERFLOW INTO RE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CHI   RF,1024             LESS THAN 1024                               
         BH    FLST052                                                          
         EDIT  (RF),(4,WKLSIZE),TRAIL=C'b'                                      
         B     FLST058                                                          
*                                                                               
FLST052  SRL   RF,10               RF = RF / 1024                               
*                                                                               
         CHI   RF,1024                                                          
         BH    FLST053                                                          
         EDIT  (RF),(4,WKLSIZE),TRAIL=C'k'                                      
         B     FLST058                                                          
*                                                                               
FLST053  SRL   RF,10                                                            
         EDIT  (RF),(4,WKLSIZE),TRAIL=C'M'                                      
         B     FLST058                                                          
*                                                                               
FLST058  TM    DDS,DDSTOTL         TEST FOR TOTALS                              
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
         AHI   R0,1                                                             
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
         AHI   R0,1                                                             
         STC   R0,0(RF)                                                         
         MVC   1(8,RF),SWORK                                                    
         AR    RF,R0                                                            
         IC    R1,WKSPAGES         TOTAL PAGES                                  
         BAS   RE,EDIT                                                          
         AHI   R0,1                                                             
         STC   R0,0(RF)                                                         
         MVC   1(8,RF),SWORK                                                    
         AR    RF,R0                                                            
         MVI   0(RF),1             ASSUME NO STATUS MESSAGE                     
         MVI   1(RF),0             END MARKER                                   
*                                                                               
         SR    R1,R1                                                            
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
         AHI   R0,20               ROOM FOR STATUS CHANGE MESSAGE               
         STC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVI   0(RF),0             END MARKER                                   
         B     XIT1                                                             
*                                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*************************************************************                   
*        PRINT FULL SCREEN OF FILE DETAIL CIPASS=CIADDR+R+0 *                   
*************************************************************                   
DISFILE  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1               CXREC                                        
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
DISF010  MVC   WKUSER,WKUSRID      COPY USERID                                  
         MVC   WKSYSPG(8),WKSYSPRG COPY SPPSDCNN                                
         MVC   WKFILEN,WKFILNO                                                  
         XC    WKTIMES,WKTIMES     CLEAR TIMES                                  
         GOTO1 ARIDXPND,SRVP2H                                                  
*                                                                               
DISF012  MVC   DUB,WKBKEY+2                                                     
         BAS   RE,GETKEY                                                        
         MVC   SRVFIL,WORK                                                      
*                                                                               
         SR    R1,R1               EDIT FILE NUMBER                             
         ICM   R1,3,WKFILNO                                                     
         EDIT  (R1),(5,SRVREF),FILL=0                                           
*                                                                               
         MVC   CISTAT,WKSTAT       DISPLAY STATUS                               
         BAS   RE,STATOUT                                                       
         MVC   SRVSTAT(8),SWORK                                                 
*                                                                               
         GOTO1 ADATCON,DMCB,(X'81',WKAGED),(8,SRVDATC)                          
*                                                                               
DISF030  EQU   *                                                                
*                                                                               
DISF040  ICM   R1,15,WKRECS        NUMBER OF RECORDS                            
         BAS   RE,EDIT                                                          
         MVC   SRVRECL,SWORK                                                    
*                                                                               
         ICM   R1,15,WKRECH        HIGH RECORD IN BLOCK                         
         BAS   RE,EDIT                                                          
         MVC   SRVRECH,SWORK                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,WKSEQ                                                       
         BNZ   *+8                                                              
         LA    R1,1                IF ZERO MAKE 1                               
         BAS   RE,EDIT                                                          
         MVC   SRVCISQ,SWORK                                                    
*                                                                               
         SR    R1,R1                                                            
         IC    R1,WKAGES                                                        
         BAS   RE,EDIT                                                          
         MVC   SRVSIZE,SWORK                                                    
*                                                                               
         GOTO1 AHEXOUT,DMCB,CIADDR,SRVWKDA,4,TOG                                
         GOTO1 (RF),(R1),WKCINEXT,SRVNEXT,2                                     
*                                                                               
         CLI   WKSEQ,2             IS THIS PART 1                               
         BNL   DISF990                                                          
         CLI   CIADDR+2,X'01'      IS IT BLOCK 1                                
         BNE   DISF990                                                          
         MVC   SRVRECS,=CL12'Total Rec#'                                        
*                                                                               
         XC    SRVCISQ,SRVCISQ     REDISPLAY WITH N OF N                        
         SR    R1,R1                                                            
         ICM   R1,1,WKSEQ                                                       
         BNZ   *+8                                                              
         LA    R1,1                IF ZERO MAKE 1                               
         BAS   RE,EDIT                                                          
         MVC   SRVCISQ,SWORK                                                    
         LA    RF,SRVCISQ                                                       
         AR    RF,R0                                                            
         MVC   0(4,RF),=C' of '                                                 
         LA    RF,4(RF)                                                         
         MVC   0(3,RF),SRVSIZE                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WKRECL                                                      
         BAS   RE,EDIT                                                          
         MVC   SRVAVGL,SWORK                                                    
*                                                                               
DISF050  MVC   SRVNAME,WKCOMNT     DESCRIPTION                                  
         GOTO1 AHEXOUT,DMCB,WKTTBL,SRVTTBL,4,TOG                                
*                                                                               
DISF070  MVC   DUB(2),WKTIMEC                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   SRVTIMC,DUB+2       CREATE TIME                                  
*                                                                               
DISF100  MVC   SRVPSWD,WKPSWD                                                   
*                                                                               
*&&US                                                                           
         L     RE,ASSB             LOOK UP FACPAK ID IN FACIDTAB                
         L     RE,SSBAFID-SSBD(RE)                                              
         LR    R0,RE               SAVE A(FACIDTAB)                             
         USING FACITABD,RE                                                      
DISF105  CLC   FACISN1,WKSUBPRG                                                 
         BE    DISF110                                                          
         LA    RE,L'FACITAB(RE)                                                 
         CLI   0(RE),X'FF'                                                      
         BNE   DISF105                                                          
         B     DISF990             INVALID FACPAK ID IN FACWRK KEY              
*                                                                               
DISF110  MVC   SRVREPT,=C'*** OTHER PQ ***'  ASSUME IT WILL BE WRONG            
         ZIC   R1,SYSID            FACPAK SYSID WE'RE CURRENTLY ON              
         MHI   R1,L'FACITAB        R1 = DISPLACEMENT TO FACIDTAB ENTRY          
         AR    R1,R0               R1 = A(FACIDTAB ENTRY)                       
         TM    FACIFL-FACITAB(R1),FACIREP  ARE WE ON A REP SYSTEM?              
         BZ    *+16                WE'RE NOT ON A REP SYSTEM                    
         TM    FACIFL,FACIREP      YES: IS THIS FACWRK FILE FROM A REP?         
         BZ    DISF990             NO: IT'S FROM AN ADV                         
         B     DISF115                                                          
*                                                                               
         TM    FACIFL,FACIREP      WE'RE ON ADV: IS THIS FILE FROM ADV?         
         BO    DISF990             NO: IT'S FROM A REP                          
         DROP  RE                                                               
*                                                                               
DISF115  DS    0H                                                               
*&&                                                                             
         USING PQUKRECD,R6                                                      
         LA    R6,PQNDX                                                         
         MVC   SRVREPT,SPACES                                                   
         XC    PQNDX,PQNDX         GET PRTQ FILE ID FROM WKPQ                   
         OC    WKPQUSER,WKPQUSER                                                
         BZ    DISF990                                                          
         MVC   PQUKSRCID,WKPQUSER                                               
         TM    WKIND1,WKIREP#      CI or REPORT number?                         
         BO    DISF116                                                          
         GOTO1 ADATAMGR,DMCB,GFILE,PRTQUE,PQNDX,WORK,CXREC                      
         CLI   8(R1),0                                                          
         BNE   DISF990                                                          
         MVC   PRTQID(8),PQUKUSRINF                                             
         MVC   CIADDR+0(2),WKPQCI                                               
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 ADATAMGR,DMCB,DMREAD,PRTQID,CIADDR,CXREC                         
         CLI   8(R1),0             TEST FOR ERRORS                              
         BNE   DISF990                                                          
         B     DISF118                                                          
*                                                                               
                                                                                
DISF116  MVC   PQUKREPNO,WKPQREP#                                               
         MVC   PRTQID(8),PRTQUE     PRIME FOR DATAMGR CALL                      
         OI    PQUKFLAG,PQUKFLNUM   DIRECT LOCATE BY REF. NUMBER                
         GOTO1 ADATAMGR,DMCB,(0,INDEX),PRTQID,PQNDX,SAVE,CXREC,0                
         CLI   8(R1),0                                                          
         BNE   DISF990                                                          
         DROP  R6                                                               
*                                                                               
DISF118  LA    RE,PQNDX                                                         
         USING PQINDEX,RE                                                       
         TM    PQTYPE,PQTYUPDT     UPDATIVE SOON?                               
         BO    *+14                                                             
         MVC   SRVREPT,=C'*** EXPIRED *** '                                     
         B     DISF990             IF NOT, THIS CAN'T BE RIGHT REPORT           
         MVC   GIUSER,PQSRCID                                                   
         DROP  RE                                                               
         GOTO1 AGETUSER                                                         
         ZIC   R1,GIULEN                                                        
         CHI   R1,6                                                             
         BNH   DISF120                                                          
         EDIT  (B2,GIUSER),(5,SRVREPT),ALIGN=LEFT                               
         LR    R1,R0                                                            
         B     DISF130                                                          
*                                                                               
DISF120  MVC   SRVREPT(6),GIUSERID                                              
*                                                                               
DISF130  LA    R1,SRVREPT(R1)                                                   
         MVI   0(R1),C','                                                       
         LA    RE,PQNDX                                                         
         USING PQINDEX,RE                                                       
         MVC   1(3,R1),PQSUBID     3 CHR ID                                     
         MVI   4(R1),C','                                                       
         EDIT  PQREPNO,(5,5(R1)),ALIGN=LEFT                                     
         DROP  RE                                                               
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
         LA    RF,DELEFIL          DELETE ROUTINE                               
*                                                                               
STATTAB  DC    X'2200'             HOLD                                         
         DC    X'2401'             ACTV                                         
         DC    X'2A02'             SENT                                         
         DC    X'2503'             KEEP                                         
         DC    X'2604'             UNKEEP                                       
         DC    X'2305'             PURGE                                        
         DC    X'2B06'             DELETE                                       
         DC    X'FFFF'             EOT                                          
         EJECT                                                                  
*************************************************************                   
*        INITIALISE & READ RECORD CIPASS=CIADDR+R+0         *                   
*************************************************************                   
READREC  NTR1                                                                   
         L     R5,ACXREC           READ RECORD INTO INDEX AREA                  
*        MVC   WRKFID,=CL8'WKFILE'                                              
         MVC   CIADDR,CIPASS       SET DISK ADDR FROM CIPASS                    
         BAS   RE,SETBUFF                                                       
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         XC    SKFCTRL-SKBUFFD(28,RE),SKFCTRL-SKBUFFD(RE)                       
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKEXTNO-SKBUFFD(1,RE),BUWFFILE                                   
         GOTO1 ADATAMGR,WKDMCB,(X'03',READ)                                     
         CLI   8(R1),0                                                          
         BE    READRX                                                           
         CLI   8(R1),X'10'         TEST NOT FOUND                               
         BE    ERR6                                                             
         CLI   8(R1),X'41'         TEST FORMAT ERROR                            
         BE    READRX              WAS ERR15                                    
         CLI   8(R1),X'90'         TEST EOF                                     
         BE    READRX                                                           
         DC    H'0'                                                             
READRX   LR    R1,R5                                                            
XIT1     XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO HOLD                         *                   
*************************************************************                   
HOLDFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    WKSTAT,WKSTAC       TEST FILE IS ACTIVE                          
         BNO   ERR7                                                             
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTHOL       SET TO HOLD                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
HOLDFILX B     XIT1                                                             
*************************************************************                   
*        SET FILE AT CIPASS TO ACTV                         *                   
*************************************************************                   
ACTVFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    WKSTAT,WKSTPU       TEST FOR DELETED                             
         BO    ACTV010                                                          
         TM    WKSTAT,WKSTAC       TEST FILE IS NOT ACTIVE                      
         BO    ERR8                                                             
ACTV010  GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTACT       SET TO ACTV                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
ACTVFILX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO KEEP                         *                   
*************************************************************                   
KEEPFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    WKSTAT,WKSTKE       TEST FILE IS NOT KEEP                        
         BO    ERR9                                                             
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTKEE       SET TO KEEP                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
KEEPFILX B     XIT1                                                             
*************************************************************                   
*        SET FILE AT CIPASS TO UNKEEP                       *                   
*************************************************************                   
UNKEFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    WKSTAT,WKSTKE       TEST FILE IS NOT KEEP                        
         BNO   ERR10                                                            
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTUNK       SET TO UNKEEP                                
         GOTO1 AWKUPDT                                                          
*                                                                               
UNKEFILX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SET FILE AT CIPASS TO SENT                         *                   
*************************************************************                   
SENTFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         TM    WKSTAT,WKSTAC       TEST FILE IS ACTIVE                          
         BNO   ERR7                                                             
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTSEN       SET TO SENT                                  
         GOTO1 AWKUPDT                                                          
*                                                                               
SENTFILX B     XIT1                                                             
*************************************************************                   
*        SET FILE AT CIPASS TO DELETED                      *                   
*************************************************************                   
DELEFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTDEL       SET TO DELETED                               
         GOTO1 AWKUPDT                                                          
*                                                                               
DELEFILX B     XIT1                                                             
*************************************************************                   
*        PURGE FILE AT CIPASS                               *                   
*************************************************************                   
PURGFIL  NTR1                                                                   
         BAS   RE,READREC          READ 1ST REC                                 
         LR    R5,R1                                                            
         USING WKRECD,R5           R5=WORKER RECORD                             
*                                                                               
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
*                                                                               
         MVC   STAACT,ACTPUR       SET TO PURGE                                 
         GOTO1 AWKUPDT                                                          
*                                                                               
PURGFILX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR ALL FILES FOR A USER (OR ALL USERS)          *                   
*************************************************************                   
CLR      EQU   *                                                                
         GOTO1 AWKLOCK             LOCK WRKF IF NOT ALREADY                     
         MVI   SFULL,0             SET NON UPDATIVE FIRST PASS                  
         CLI   IFDDSV,C'F'         DDS=F IS THE SECRET CODE                     
         BNE   CLR1                TO FORCE UPDATE                              
*                                                                               
CLR0     MVI   SFULL,1             DO UPDATING NOW                              
*                                                                               
CLR1     BAS   RE,CXLOOPI          INIT INDEX LOOP                              
         USING WKRECD,R5           R5=A(WRKF INDEX ENTRY)                       
*                                                                               
CLR2     BAS   RE,GETXAD                                                        
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),WRKFID,CXADDR,ACXREC                
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
*                                                                               
CLR4     OC    USERID,USERID       CAN BE ALL FOR DDS                           
         BZ    *+14                                                             
         CLC   WKUSRID,USERID      ELSE MATCH USERID                            
         BNE   CLR8                                                             
         CLI   SFULL,0             PASS 1 CHECK SENT/PURGED                     
         BNE   CLR6                                                             
         TM    WKSTAT,WKSTAC+WKSTUS                                             
         BZ    CLR8                                                             
         B     ERR12               ERROR NOT PURGED/PRINTED                     
*                                                                               
CLR6     BAS   RE,GETCAD           PASS 2 ZAP CIREC INDEX                       
         L     R6,ACIREC                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),WRKFID,CIADDR,(R6)                  
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
         XC    0(L'WKINDEX,R6),0(R6)                                            
         GOTO1 (RF),(R1),(X'00',DMWRT)                                          
         CLI   8(R1),0                                                          
         BNE   CLRERR                                                           
*                                                                               
         LH    RE,CXENTRY          ZAP INDEX INDEX AND WRITE BACK               
         MH    RE,CINDXLN                                                       
         A     RE,ACXREC                                                        
         XC    0(L'WKINDEX,RE),0(RE)                                            
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
CLRERR   BAS   RE,WKUNLK                                                        
         MVI   LOCKT,0                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        INIT BUFFER FOR WRKFID R5 MUST POINT TO A BUFFER   *                   
*************************************************************                   
SETBUFF  ST    RE,SAVERE                                                        
         GOTO1 ADATAMGR,WKDMCB,(X'00',BUFFER),WRKFID,,,(R5)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R5)      SAVE V(WKTAB)/V(WKFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS WRKF                 
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
         EJECT                                                                  
*************************************************************                   
*        BUILD SPPSDDCX IN WORK FROM WKBKEY IN DUB          *                   
*************************************************************                   
GETKEY   ST    RE,SAVERE                                                        
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(4),DUB         DISPLAY SYSPRG & SUBPRG                      
         LA    RF,WORK+4                                                        
*                                                                               
         CLI   DUB+4,X'5C'         TEST FOR "*"                                 
         BE    GETK020                                                          
         CLI   DUB+4,X'C1'         TEST FOR A-9                                 
         BNL   GETK020                                                          
         IC    R1,DUB+4            MUST BE PACKED DAY                           
         MVC   1(1,RF),DUB+4                                                    
         OC    1(1,RF),=X'F0'      UNPACK AND DISPLAY                           
         SRL   R1,4                                                             
         STC   R1,0(RF)                                                         
         OC    0(1,RF),=X'F0'                                                   
         LA    RF,2(RF)            BUMP 2 CHRS                                  
         B     GETK026                                                          
*                                                                               
GETK020  MVC   0(1,RF),DUB+4       SINGLE CHR DAY or "*"                        
GETK025  LA    RF,1(RF)                                                         
*                                                                               
GETK026  MVC   0(1,RF),DUB+5       DISPLAY CLASS                                
         LA    RF,1(RF)                                                         
         MVC   0(1,RF),DUB+6       DISPLAY EXTRA                                
         LA    RF,1(RF)                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*************************************************************                   
*        BUILD ATTB LINE FROM BITS IN BYTE R1=A(TABLE)      *                   
*************************************************************                   
GETBITS  ST    RE,SAVERE                                                        
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
*************************************************************                   
*        GET RET HOURS  HALF=RDATE BYTE=RTIME RET FULL=HHH  *                   
*************************************************************                   
GETHOUR  NTR1                                                                   
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
ERRFA    LA    RE,SREIFSK1         INVALID FILTER SIGN KEYWORD=                 
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
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
**********************************************************************          
* DISPLAY COUNTERS                                                              
**********************************************************************          
CNTDSP   NTR1  ,                   DISPLAY FILE COUNTERS                        
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
         BE    CNTDG                                                            
         MVCDD 0(4,R5),SR#SENT                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QE),(5,4(R5))                                                
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
         B     CNTDSPX                                                          
*                                                                               
CNTDH    MVCDD SRVINF(13),SR#RPTCT FILE COUNT FOR NON DDS TRMS                  
*                                                                               
CNTDSPX  B     XIT1                                                             
         EJECT                                                                  
TIMEOUT  UNPK  DUB+3(5),DUB(3)                                                  
         MVC   DUB+2(2),DUB+3                                                   
         MVI   DUB+4,C':'                                                       
         BR    RE                                                               
         EJECT                                                                  
*IMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
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
         SPACE 2                                                                
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
STATOUT  NTR1                      OUTPUT FILE STATUS                           
         XC    SHALF,SHALF                                                      
         CLI   CISTAT,WKSTPU       PURGED STATUS                                
         BNE   STOU1                                                            
         MVCDD SWORK(8),SR#PRGED                                                
         MVC   SFULL,=C'****'                                                   
         B     STATOUTX                                                         
*                                                                               
STOU1    MVC   SFULL(4),SR@DELD    TEST DELETED FIRST                           
         MVC   SWORK(8),SR@DELD                                                 
         TM    CISTAT,WKSTUS                                                    
         BO    STOUT90                                                          
         MVC   SFULL(4),SR@HOLD    TEST HOLD                                    
         MVC   SWORK(8),SR@HOLD                                                 
         TM    CISTAT,WKSTHO                                                    
         BO    STOUT90                                                          
         MVC   SFULL(4),SR@CRTG    TEST CREATING                                
         MVC   SWORK(4),SR@CRTG                                                 
         CLI   CISTAT,WKSTAC+WKSTTE                                             
         BE    STOUT90                                                          
         MVC   SFULL(4),SR4ACTV    TEST ACTV                                    
         MVC   SWORK(8),SR@ACTV                                                 
         TM    CISTAT,WKSTAC                                                    
         BO    STOUT90                                                          
*                                                                               
         MVC   SFULL(4),=C'Unkn'   SET TO UNKNOWN                               
         MVC   SWORK(8),=C'Unknown '                                            
*                                                                               
STOUT90  TM    CISTAT,WKSTKE       SUB-STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         MVI   SWORK+4,C','                                                     
         MVCDD SWORK+5(8),SR#KEEP                                               
         MVI   SHALF,C','                                                       
         MVC   SHALF+1(1),SR@KEEP                                               
*                                                                               
STATOUTX B     XIT1                                                             
         EJECT                                                                  
*DMWRKRR                                                                        
       ++INCLUDE DMWRKRR                                                        
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & TABLES                                 *                   
*************************************************************                   
MAXSEQ   DC    F'65000'                                                         
DSPMAX   DC    PL2'16'                                                          
QTMAX    DC    PL2'600'                                                         
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
INDEX    DC    CL8'INDEX'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
READ     DC    CL8'READ  '                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DC@XSORT DC    CL5'XSORT'                                                       
DC@SHIT  DC    CL4'SHIT'                                                        
DC@OBJ   DC    CL4'OBJ '                                                        
FFS      DC    16X'FF'                                                          
DOTS     DC    16C'.'                                                           
SPACES   DC    80C' '                                                           
PURGEMSG DC    CL80'File has been purged '                                      
SIZEL    DC    CL78'PART1 CI TOTAL=1234,AVAIL=1234,INUSE=1234,INDEX=123X        
               4        '                                                       
         SPACE 2                                                                
ACTACT   DC    CL3'RES'                                                         
ACTHOL   DC    CL3'HOL'                                                         
ACTKEE   DC    CL3'KEE'                                                         
ACTUNK   DC    CL3'UNK'                                                         
ACTPUR   DC    CL3'PUR'                                                         
ACTRET   DC    CL3'RET'                                                         
ACTSEN   DC    CL3'SEN'                                                         
ACTDEL   DC    CL3'DEL'                                                         
TOG      DC    CL3'TOG'                                                         
         SPACE 2                                                                
* TABLE OF VALID SORT KEYWORDS                                                  
*               LA RF,KEYWORD,(+VE ACTN),(-VE ACTN),LEN,N/D                     
         DS    0H                                                               
SORTTBL  DS    0CL8                                                             
         DC    X'41F0',S(SR@ALPHA),X'01',X'01',X'05',X'00'                      
         DC    X'41F0',S(SR@CRTD),X'02',X'03',X'07',X'00'                       
         DC    X'41F0',S(SR@RTAIN),X'06',X'07',X'06',X'00'                      
         DC    X'41F0',S(SR@SIZE),X'08',X'09',X'04',X'00'                       
SORTBLX  DC    X'00'                                                            
*                                                                               
* TABLE OF VALID FILE STATS AND BIT VALUES                                      
*                                                                               
         DS    0H                                                               
STATTBL  DS    0CL6                                                             
         DC    X'41F0',S(SR8ALL),X'FF',X'0'                                     
         DC    X'41F0',S(SR@LIVE),X'E0',X'0' LIVE=ACTV/HOLD/PROC                
         DC    X'41F0',S(SR@ACTV),X'80',X'0'                                    
         DC    X'41F0',S(SR4ACTV),X'80',X'0'                                    
         DC    X'41F0',S(SR@HOLD),X'40',X'0'                                    
         DC    X'41F0',S(SR@PROC),X'20',X'0'                                    
         DC    X'41F0',S(SR@DEAD),X'30',X'0' DEAD=SENT/DELD                     
         DC    X'41F0',S(SR@DELD),X'20',X'0'                                    
         DC    X'41F0',S(SR@SENT),X'10',X'0'                                    
         DC    X'41F0',S(SR@KEEP),X'08',X'0'                                    
         DC    X'41F0',S(SR@RNING),X'03',X'0'                                   
         DC    X'41F0',S(SR@SNDG),X'02',X'0'                                    
         DC    X'41F0',S(SR@CRTG),X'01',X'0'                                    
STATTBLX DC    X'00'                                                            
         SPACE 2                                                                
* TABLE OF VALID FILE ATTRIBUTES AND BIT VALUES                                 
*                                                                               
         DS    0H                                                               
ATTBTBL  DS    0CL6                                                             
         DC    X'41F0',S(DC@OBJ),X'80',X'0'                                     
         DC    X'41F0',S(SR@ERROR),X'20',X'0'                                   
         DC    X'41F0',S(SR@PSWD),X'10',X'0'                                    
         DC    X'41F0',S(DC@SHIT),X'01',X'0'                                    
ATTBTBLX DC    X'00'                                                            
         SPACE 2                                                                
ATTINDS  DC    C'Obj/AT2/Err/Pwd/AT5/Xtn/AT7/Sht'                               
         SPACE 2                                                                
* TABLE OF VALID KEYWORDS FOR FILTERS                                           
* CL8    KEYWORD NAME                                                           
* XL1    KEYWORD NUMBER                                                         
* XL1    KEYWORD SIGN BITS X'80'=NE,X'40'=LT,X'20'=GT                           
* XL1    KEYWORD VALD BITS X'01'=DDSONLY                                        
*                                                                               
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
*                                                                               
         EJECT                                                                  
I168     DC    C'I',AL2(168)       N FILES PAGE N OF N DISPLAYED                
I188     DC    C'I',AL2(188)       FILE SELECTED (NEW STATUS)                   
I189     DC    C'I',AL2(189)       FILET ATTRIBS DISPLAYED                      
         LTORG                                                                  
         EJECT                                                                  
COMPTBL  DS    0CL2                TABLE OF COMPATIBLE ACTNS & STATS            
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
*              CL40'.........+.........+.........+.........+'                   
SIZEH0   DC    CL40'             ----------- Part 1 CIs ----'                   
         DC    CL39'-------  ------- Part 2 CIs --------   '                    
SIZEH1   DC    CL40'Actn File     Total  Avail   Sent Active'                   
         DC    CL39'  Index   Total  Avail   Sent Active   '                    
         EJECT                                                                  
*SRFWKDD                                                                        
       ++INCLUDE SRFWKDD                                                        
         EJECT                                                                  
LWORKD   DSECT                                                                  
PQNDX    DS    XL40                                                             
SAVE     DS    50F                                                              
LWORKX   DS    0XL4                                                             
*                                                                               
WKRLD    DSECT                                                                  
WKLSELH  DS    CL8                                                              
WKLSEL   DS    CL4                                                              
WKLHDR   DS    CL8                                                              
WKLDATA  DS    0CL73                                                            
*                                                                               
WKLFID   DS    CL8                                                              
         DS    CL1                                                              
WKLREF   DS    CL5                                                              
         DS    CL1                                                              
WKLSTAT  DS    CL6                                                              
         DS    CL1                                                              
WKLCDATE DS    CL11                                                             
         DS    CL1                                                              
WKLRDAYS DS    CL2                                                              
         DS    CL1                                                              
WKLRECS  DS    CL6                                                              
         DS    CL1                                                              
WKLRECL  DS    CL4                                                              
         DS    CL1                                                              
WKLSIZE  DS    CL4                                                              
         DS    CL1                                                              
WKLDESC  DS    CL19                                                             
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
*SRFWKWK                                                                        
       ++INCLUDE SRFWKWK                                                        
         EJECT                                                                  
SRFWKFFD DSECT                                                                  
         DS    CL64                                                             
*SRFWKFFD                                                                       
       ++INCLUDE SRFWKFFD                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRFWKFDD                                                                       
       ++INCLUDE SRFWKFDD                                                       
*                                                                               
         EJECT                                                                  
*DDWRSCAND                                                                      
*FASYSFAC                                                                       
*FASRS                                                                          
*DDCOMFACS                                                                      
*DDFLDHDR                                                                       
*SRERREQUS                                                                      
*FATCB                                                                          
*DMWRKFK                                                                        
*DMPRTQK                                                                        
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDWRSCAND                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
**AH3  ++INCLUDE DMWRKFK                                                        
       ++INCLUDE FACIDTABD                                                      
       ++INCLUDE DMPRTQD                                                        
*PREFIX=PQ                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117SRFWK02   10/31/13'                                      
         END                                                                    
