*          DATA SET PXTRACT    AT LEVEL 046 AS OF 11/15/18                      
*PHASE PXTRACTA                                                                 
*INCLUDE PRESTO                                                                 
*INCLUDE PRESENT                                                                
*INCLUDE DMUTLAC                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*                                                                               
         TITLE 'PXTRACT - EXTRACT PRESTO MASTER FILE DATA'                      
**********************************************************                      
*                                                        *                      
* PRESTO MASTER FILE EXTRACT CONTROL MODULE              *                      
*                                                        *                      
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:        *                      
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)  *                      
*                                                        *                      
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:  *                      
*   DXOPENQ  - OPEN SYSTEM FILES                         *                      
*   DXCLOSEQ - CLOSE SYSTEM FILES                        *                      
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE           *                      
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE         *                      
*                                                        *                      
* FOR DXLOADQ AND DXUPDTQ MODES,                         *                      
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                    *                      
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE -   *                      
*     SEE DSECT SYSTABD)                                 *                      
*                                                        *                      
*                                                        *                      
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK  *                      
*                                                        *                      
**********************************************************                      
         SPACE 1                                                                
PXTRACT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**PXTRA*,RA,R9,RR=RE                                 
*                                                                               
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R8,0(R1)            R8=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R8                                                      
         L     R7,DXSTPTR          R7=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R7                                                       
*                                                                               
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MERR                                                             
*        BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
*        BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
         MVC   MAXIOS,=F'1000000'                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN SYSTEM FILES AND INITIALISE TABLES          *         
***********************************************************************         
PROCOPEN NTR1  ,                                                                
         L     RE,=V(UTL)          SET UTL SENUM                                
         MVC   4(1,RE),DXSENUM                                                  
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCOUNT,ACCFILS,IOL                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE SYSTEM FILES                             *          
***********************************************************************         
PROCCLOS NTR1  ,                                                                
         L     RE,=V(UTL)          SET UTL SENUM                                
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTROL FILE SECURITY FILE DATA IN LOAD MODE                *         
***********************************************************************         
PROCLOAD NTR1  ,                                                                
         MVC   CUL(1),SXDTAGB      SET COMPANY/UNIT/LEDGER                      
         BAS   RE,GETCPY           GET COMPANY RECORD, EXTRACT DATA             
*                                                                               
         BAS   RE,SETHEIR          GET HEIRARCHY LENGTHS                        
         MVC   HALF,CUL+1                                                       
         MVC   CUL+1(2),=C'1R'     REPLACE LEDGER WITH 1R                       
         BAS   RE,SETHEIR          GET 1R LEDGER LENGTHS                        
         MVC   CUL+1(2),HALF       RESTORE LEDGER AGAIN                         
*                                                                               
         MVC   TYPECODE,SXDTTYP    CONVERT RECORD TYPE TO NUMERIC               
         CLC   TYPECODE,=C'V14'    TEST FOR SPECIAL V2.1 -> V2.4 LOAD           
         BE    PL10                YES                                          
         CLC   TYPECODE,=C'V28'    TEST FOR UPGRADE TO V2.8                     
         BE    PL20                                                             
*                                                                               
         BAS   RE,GETTYP                                                        
*                                                                               
         BAS   RE,PUTDT            PUT DATE/TIME OBJECT                         
*                                                                               
         BAS   RE,CALLPSNT         CALL ACPRESENT INTERFACE                     
         B     PLX                                                              
*                                                                               
PL10     BAS   RE,PUTDT                                                         
*                                                                               
         MVC   TYPECODE,=C'AGX'    AGENCY                                       
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'PLX'    PRICE LIST                                   
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'CLX'    START WITH CLIENTS TO PICK                   
         BAS   RE,GETTYP           UP LOCKED STATUS                             
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'PRX'    THEN DO PRODUCTS TO GET LOCKED               
         BAS   RE,GETTYP           STATUS                                       
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'WGX'    FIRST LOAD WORK-GROUPS                       
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'WCX'    THEN REFRESH WORKCODES                       
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
*&&US                                                                           
         MVC   TYPECODE,=C'SCX'    THEN REFRESH SCHEMES                         
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'CWX'    THEN REFRESH CATEGORIES                      
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
*&&                                                                             
         MVC   TYPECODE,=C'OPU'    FINALLY DO OPTIONS                           
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'USX'    USER FIELD KEYS/VALUES                       
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
         B     PLX                                                              
*                                                                               
PL20     BAS   RE,PUTDT            V28 LOAD                                     
*                                                                               
         MVC   TYPECODE,=C'AGX'    AGENCY (FOR PXTRDATA SETTING)                
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                  CLT, PRD, JOB FOR 5TH ADDRESS LINE           
         MVC   TYPECODE,=C'CLX'    CLIENTS                                      
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'PRX'    PRODUCT                                      
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'JBX'    JOB                                          
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                  TEMPO DATA:                                  
         MVC   TYPECODE,=C'LES'    LEDGER STRUCTURES                            
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'LEN'    LEDGER NAMES                                 
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'PER'    PERSON AND PERSON ASSIGNMENT                 
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
         MVC   TYPECODE,=C'RAT'    RATE                                         
         BAS   RE,GETTYP                                                        
         BAS   RE,CALLPSNT                                                      
*                                                                               
PLX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READS COMPANY RECORD AND EXTRACTS DATA FROM COMPANY ELEMENT                   
*                                                                               
* ON ENTRY:    CUL                 COMPANY                                      
*                                                                               
* RETURNS:     PRODUCTION UNIT/LEDGER, COMPANY CURRENCIES, STATUS FLAG          
***********************************************************************         
GETCPY   NTR1  ,                                                                
         LA    R4,BIGKEY                                                        
         USING CPYKEY,R4                                                        
         MVC   CPYKEY,SPACES       GET COMPANY RECORD                           
         MVC   CPYKCPY,CUL                                                      
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),ACCDIR,CPYKEY,CPYKEY,0,0            
         CLC   CPYKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),ACCMST,CPYKDA,IO,DMWORK,0           
*                                                                               
*                                  GET COMPANY ELEMENT                          
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('CPYELQ',IO),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING CPYELD,R6                                                        
         MVC   CUL+1(2),CPYPROD                                                 
         XC    COMPVALS(COMPVALN),COMPVALS                                      
         CLI   CPYLN,CPYLN3Q                                                    
         BL    GETCPYX                                                          
*                                                                               
*&&UK                                                                           
         MVC   COMPCUR,CPYCURR     PRIMARY CURRENCY                             
         MVC   COMPCURS,CPYCURRS   SECONDARY CURRENCY                           
         MVC   COMPSTA7,CPYSTAT7   STATUS BYTE 7                                
         MVC   COMPSTA9,CPYSTAT9   STATUS BYTE 9                                
*&&                                                                             
*                                                                               
GETCPYX  B     XIT                                                              
         DROP  R4,R6                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACTS ACCOUNT LEDGER HEIRARCHY INFO FROM LEDGER RECORD                     
*                                                                               
* ON ENTRY:    CUL                 COMPANY/UNIT/LEDGER                          
*                                                                               
* RETURNS:     LCLI/LPRO/LJOB      HEIRARCHY LENGTHS                            
*              OR IF 1R LEDGER LOFF/LDEP/LSUB   HEIRARCHY LENGTHS               
***********************************************************************         
SETHEIR  NTR1  ,                                                                
         LA    R4,BIGKEY                                                        
         USING LDGKEY,R4                                                        
         MVC   LDGKEY,SPACES       GET LEDGER RECORD                            
         MVC   LDGKCPY(LDGKEND),CUL                                             
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),ACCDIR,LDGKEY,LDGKEY,0,0            
         CLC   LDGKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),ACCMST,LDGKDA,IO,DMWORK,0           
*                                                                               
*                                  GET HEIRARCHY ELEMENT                        
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('ACLELQ',IO),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING ACLELD,R6                                                        
*                                                                               
         CLC   CUL+1(2),=C'1R'     CHECK IF GETTING 1R LEDGER LENGTHS           
         BE    SHEIR10                                                          
         MVC   LCLI,ACLVLEN        EXTRACT AND CALCULATE LENGTHS                
         ZIC   RF,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         ZIC   RE,LCLI                                                          
         SR    RF,RE                                                            
         STC   RF,LPRO                                                          
         ZIC   RF,ACLVLEN+(2*L'ACLVALS)  LEVEL C LENGTH                         
         ZIC   RE,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         SR    RF,RE                                                            
         STC   RF,LJOB                                                          
         B     XIT                                                              
*                                                                               
SHEIR10  DS    0H                                                               
         MVC   LOFF,ACLVLEN        EXTRACT AND CALCULATE LENGTHS                
         ZIC   RF,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         ZIC   RE,LOFF                                                          
         SR    RF,RE                                                            
         STC   RF,LDEP                                                          
         ZIC   RF,ACLVLEN+(2*L'ACLVALS)  LEVEL C LENGTH                         
         ZIC   RE,ACLVLEN+L'ACLVALS      LEVEL B LENGTH                         
         SR    RF,RE                                                            
         STC   RF,LSUB                                                          
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET RECORD TYPE TABLE VALUES FROM 3 CHARACTER CODE                  *         
***********************************************************************         
GETTYP   NTR1  ,                                                                
         LA    RE,TYPTAB                                                        
*                                                                               
GT10     CLI   0(RE),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TYPECODE,0(RE)      COMPARE NAME                                 
         BE    GT20                                                             
         LA    RE,4(RE)            GET NEXT ENTRY                               
         B     GT10                                                             
*                                                                               
GT20     MVC   TYPEREC,3(RE)       MATCH FOUND                                  
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ACPRESENT INTERFACE - PASSES RECORD CODE AND A(HOOK)                *         
***********************************************************************         
CALLPSNT NTR1  ,                                                                
         LA    R1,PSNTBLK          BUILD ACPRESENT INTERFACE BLOCK              
         USING PSNTBLKD,R1                                                      
         XC    PSNTBLK,PSNTBLK                                                  
         MVC   PSNTRTYP,TYPEREC                                                 
         MVC   PSNTFTYP,=AL1(PRRQFTAL)                                          
         MVC   PSNTCUL,CUL                                                      
         MVC   PSNTLCLI,LCLI                                                    
         MVC   PSNTLPRO,LPRO                                                    
         MVC   PSNTLJOB,LJOB                                                    
         MVC   PSNTLOFF,LOFF       1R OFFICE CODE LENGTH                        
         MVC   PSNTLDEP,LDEP       1R DEP CODE LENGTH                           
         MVC   PSNTLSUB,LSUB       1R SUB-DEP CODE LENGTH                       
         MVC   PSNTSTA7,COMPSTA7                                                
         MVC   PSNTCTRY,SXDTCTRY   COUNTRY                                      
         MVC   PSNTCLS,DXUSER                                                   
         LA    RF,COMFACS                                                       
         ST    RF,PSNTCOMF                                                      
         LA    RF,IO                                                            
         ST    RF,PSNTAIO                                                       
         LA    RF,MASTHOOK         SET A(RECORD HOOK)                           
         ST    RF,PSNTHOOK                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,VPRESENT         CALL PRESENT                                 
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
CPX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS IS THE HOOK FOR MASTER FILE RECORDS PASSED BY ACPRESENT.  IT             
* CALLS ACPRESTO TO BUILD A BLOCK OF MAD OBJECTS AND CALLS DXPUT                
* TO WRITE THEM TO THE OUTPUT FILE.                                             
***********************************************************************         
MASTHOOK NTR1  ,                                                                
         LA    R4,OBJECTS          SET AREA TO BUILD OBJECTS                    
*                                                                               
         LA    R1,ACPRBLK          BUILD PARM BLOCK AND CALL ACPRESTO           
         USING ACPRESTD,R1            TO BUILD THE OBJECT BLOCK                 
         XC    ACPRBLK,ACPRBLK                                                  
         LA    R2,PSNTBLK                                                       
         USING PSNTBLKD,R2                                                      
         MVC   ACPRREC,PSNTAIO                                                  
         ST    R4,ACPROBJ                                                       
         MVC   ACPRTYP,PSNTBTYP                                                 
         MVC   ACPRCOMF,PSNTCOMF                                                
         MVC   ACPRLCLI,LCLI                                                    
         MVC   ACPRLPRO,LPRO                                                    
         MVC   ACPRLJOB,LJOB                                                    
         MVC   ACPRLOFF,LOFF       1R OFFICE CODE LENGTH                        
         MVC   ACPRLDEP,LDEP       1R DEP CODE LENGTH                           
         MVC   ACPRLSUB,LSUB       1R SUB-DEP CODE LENGTH                       
         MVC   ACPRSTA7,COMPSTA7                                                
         MVC   ACPRCUR,COMPCUR                                                  
         MVC   ACPRCURS,COMPCURS                                                
         MVC   ACPRAGY,SXDTAGY     AGENCY ALPHA ID                              
         CLC   TYPECODE,=C'ALL'    TEST LOADING ALL (INITIAL LOAD)              
         BE    *+14                                                             
         CLC   TYPECODE,=C'OPX'    TEST INITIAL OPTIONS LOAD                    
         BNE   *+8                                                              
         MVI   ACPRLOAD,C'Y'       YES                                          
*                                                                               
         L     RF,VPRESTO                                                       
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         DROP  R1,R2                                                            
*                                                                               
         BNE   MH90                SKIP THIS RECORD IF ERROR                    
*                                                                               
MH50     OC    0(2,R4),0(R4)       IF END OF OBJECTS THEN DONE                  
         BZ    MH90                                                             
*                                                                               
* CONVERT ACPRESTO OBJECT FORMAT (LLTTTTDDDDDDDD...) TO PXTRACT                 
* OUTPUT FORMAT (TTTT LLLL DDDDDDDDDDDDDDD....).  PXTRACT NEEDS                 
* TO CONVERT THE OBJECT TYPE AND LENGTH TO CHARACTER SO THAT                    
* THE PC SIDE CAN EXTRACT THEM ($MAD SUPPLIES THEM AS PART OF THE               
* OBJECT HEADER).                                                               
*                                                                               
         SR    R5,R5               R5 = LENGTH OF OBJECT DATA                   
         ICM   R5,3,0(R4)                                                       
         SH    R5,=H'4'                                                         
*                                                                               
         XC    OUTIOL,OUTIOL       SET OUTPUT IOAREA LENGTH                     
         LA    RF,14(R5)              HEADER(4) + TYPE(4) + SPACE(1) +          
         STH   RF,OUTIOL              LENGTH(4) +  SPACE(1) + DATA(LEN)         
*                                                                               
*                                  OUPUT TYPE AND LENGTH                        
         GOTO1 VHEXOUT,DMCB,4(R4),OUTIO,2,=C'MIX'                               
         MVI   OUTIO+4,C' '                                                     
         ST    R5,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL+2,OUTIO+5,2,=C'MIX'                            
         MVI   OUTIO+9,C' '                                                     
*                                                                               
         LA    RE,OUTIO+10         OUTPUT OBJECT DATA                           
         LR    RF,R5                                                            
         LA    R0,6(R4)                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
* BUILD A TRANSLATE TABLE TO REMOVE ALL BINARY ZEROES IN OBJECT                 
*                                                                               
         MVI   TRTAB,X'40'                                                      
         MVC   TRTAB+1(63),TRTAB+0 ALL CHARS UP TO BLANK                        
         LA    R0,256-64           ARE TRANSLATED TO BLANK                      
         LA    R1,64               OTHERS ARE THEMSELVES                        
         LA    RE,TRTAB+64         RE=TRANSLATE TABLE POINTER                   
*                                                                               
         STC   R1,0(RE)                                                         
         LA    R1,1(R1)            INCREMENT TRANSLATE TABLE VALUE              
         LA    RE,1(RE)            BUMP TABLE POINTER                           
         BCT   R0,*-12                                                          
*                                                                               
         LA    RE,OUTIO+10         START AT OBJECT DATA AND SET NULLS           
         LR    RF,R5                   TO SPACES FOR OBJECT LENGTH              
MH60     C     RF,=F'256'                                                       
         BNH   MH70                                                             
         TR    0(256,RE),TRTAB                                                  
         LA    RE,256(RE)                                                       
         S     RF,=F'256'                                                       
         BP    MH60                                                             
         B     MH75                                                             
*                                                                               
MH70     BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    0(0,RE),TRTAB                                                    
*                                  PUT OUTPUT                                   
MH75     GOTO1 DXPUT,DMCB,OUTIOL,(R8)                                           
*                                                                               
         BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF               BUMP TO NEXT OBJECT                          
         ICM   RF,3,0(R4)                                                       
         LA    R4,2(R4,RF)                                                      
         B     MH50                AND LOOP BACK                                
*                                                                               
MH90     DS    0H                                                               
*                                                                               
MHX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS A DDATE/TIME OBJECT TO THE XTRACT FILE.                     
***********************************************************************         
PUTDT    NTR1  ,                                                                
         XC    OUTIOL,OUTIOL       SET OUTPUT IOAREA LENGTH                     
         LA    RF,26                  HEADER(4) + TYPE(4) + SPACE(1) +          
         STH   RF,OUTIOL              LENGTH(4) +  SPACE(1) + DATA(12)          
*                                                                               
*                                  OUPUT TYPE AND LENGTH                        
         LA    RF,OUTIO                                                         
         MVC   0(10,RF),=C'139D 000C '                                          
*                                                                               
*                                  CURRENT DATE 'YYMMDD'                        
         GOTO1 VDATCON,DMCB,(5,0),(X'20',OUTIO+10)                              
*                                                                               
         TIME  DEC                 BINARY CURRENT TIME IN FULL                  
         ST    R0,FULL                                                          
*                                                                               
         XC    DUB,DUB             R2 = HOUR + 8                                
         ZIC   RF,FULL                                                          
         SLL   RF,4                                                             
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R2,DUB                                                           
*&&US*&& LA    R2,6(R2)                                                         
*&&UK*&& LA    R2,0(R2)                                                         
*                                                                               
         C     R2,=F'24'           IF HOUR IS PAST 24 THEN SUBTRACT             
         BL    PDT50                   24 AND ADD 1 DAY                         
         S     R2,=F'24'                                                        
         GOTO1 VADDAY,DMCB,OUTIO+10,(X'20',OUTIO+10),1                          
*                                                                               
PDT50    CVD   R2,DUB              STORE R2 BACK TO BINARY HOUR                 
         L     RF,DUB+4                                                         
         SRL   RF,4                                                             
         STC   RF,FULL                                                          
*                                  CURRENT TIME 'HHMMSS'                        
         GOTO1 VHEXOUT,DMCB,FULL,OUTIO+16,3,=C'MIX'                             
*                                                                               
*                                  PUT OUTPUT                                   
         GOTO1 DXPUT,DMCB,OUTIOL,(R8)                                           
*                                                                               
         BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PDTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
DECIOC   NTR1  ,                                                                
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         BZ    NO                                                               
         B     YES                                                              
         EJECT                                                                  
* TYPTAB DEFINES PROCESS RECORD TYPES                                           
* CL3    TYPE NAME                                                              
* AL1    TYPE NUMBER                                                            
*                                                                               
         DS    0D                                                               
TYPTAB   DC    CL3'ALL',AL1(PRRQRTC1)                                           
         DC    CL3'MGX',AL1(PRRQRTMG)                                           
         DC    CL3'MDX',AL1(PRRQRTMD)                                           
         DC    CL3'OGX',AL1(PRRQRTOG)                                           
         DC    CL3'OFX',AL1(PRRQRTOF)                                           
         DC    CL3'WGX',AL1(PRRQRTWG)                                           
         DC    CL3'WCX',AL1(PRRQRTWC)                                           
         DC    CL3'SCX',AL1(PRRQRTSC)                                           
         DC    CL3'CWX',AL1(PRRQRTCW)                                           
         DC    CL3'CLX',AL1(PRRQRTCL)                                           
         DC    CL3'PRX',AL1(PRRQRTPR)                                           
         DC    CL3'JBX',AL1(PRRQRTJB)                                           
         DC    CL3'SPX',AL1(PRRQRTSP)                                           
         DC    CL3'AGX',AL1(PRRQRTAG)                                           
         DC    CL3'OLX',AL1(PRRQRTOL)                                           
         DC    CL3'PLX',AL1(PRRQRTPL)                                           
         DC    CL3'SWX',AL1(PRRQRTSW)                                           
         DC    CL3'TAX',AL1(PRRQRTTX)                                           
         DC    CL3'OPX',AL1(PRRQRTOP)  INITIAL OPTIONS LOAD                     
         DC    CL3'OPU',AL1(PRRQRTOP)  UPDATE EXISTING OPTIONS                  
         DC    CL3'USX',AL1(PRRQRTUS)                                           
         DC    CL3'OXX',AL1(PRRQRTOX)  ALL PRESTO ORDERS - SPECIAL              
         DC    CL3'LES',AL1(PRRQRTLS)  LEDGER STRUCTURES                        
         DC    CL3'LEN',AL1(PRRQRTLN)  LEDGER NAMES                             
         DC    CL3'PER',AL1(PRRQRTPE)  PERSON                                   
         DC    CL3'RAT',AL1(PRRQRTRA)  RATE                                     
         DC    CL3'EAP',AL1(PRRQREAP)  ESTIMATE APPROVAL                        
         DC    AL1(00)                                                          
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB**SSB**SSB**'                                           
SSB      DC    (SSOOFFX-SSOOFF)X'00'                                            
         ORG   SSB+(SSOXTND-SSOOFF)                                             
         DC    X'FF'               EXTENDED OFFLINE SSB                         
         ORG   SSB+(SSOSTAT2-SSOOFF)                                            
         DC    AL1(SSOSNRCV)       NO RECOVERY                                  
         ORG                                                                    
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
         EJECT                                                                  
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VADDAY   DC    V(ADDAY)                                                         
VHELLO   DC    V(HELLO)                                                         
VPRESTO  DC    V(PRESTO)                                                        
VPRESENT DC    V(PRESENT)                                                       
ACOMFACS DC    A(COMFACS)                                                       
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
GETREC   DC    C'GETREC '                                                       
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
ACCFILS  DC    C'NACCDIR NACCMST NACCARC X'                                     
*                                                                               
COMFACS  DS    0V                                                               
         DC    V(DATAMGR)                                                       
         DC    4A(0)                                                            
         DC    V(HELLO)                                                         
         DC    3A(0)                                                            
         DC    V(HEXOUT)                                                        
         DC    2A(0)                                                            
         DC    V(DATCON)                                                        
         DC    50A(0)                                                           
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         EJECT                                                                  
* ACPRESENTD                                                                    
       ++INCLUDE ACPRESENTD                                                     
         EJECT                                                                  
* ACPRESENTQ                                                                    
       ++INCLUDE ACPRESENTQ                                                     
         EJECT                                                                  
* ACPRESTOD                                                                     
       ++INCLUDE ACPRESTOD                                                      
         EJECT                                                                  
* ACPRESTOQ                                                                     
       ++INCLUDE ACPRESTOQ                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
*                                                                               
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
HALF     DS    H                                                                
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
*                                                                               
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
*                                  RECORD TYPE TABLE VALUES                     
TYPECODE DS    CL3                 TYPE CODE                                    
TYPEREC  DS    XL1                 TYPE NUMBER                                  
*                                                                               
CUL      DS    XL3                 COMPANY/UNIT/LEDGER                          
COMPVALS DS    0X                                                               
COMPCUR  DS    CL3                 PRIMARY CURRENCY                             
COMPCURS DS    CL3                 SECONDARY CURRENCY                           
COMPSTA7 DS    X                   STATUS BYTE 7                                
COMPSTA9 DS    X                   STATUS BYTE 9                                
COMPVALN EQU   *-COMPVALS                                                       
LCLI     DS    X                   LENGTH OF CLIENT                             
LPRO     DS    X                   LENGTH OF PRODUCT                            
LJOB     DS    X                   LENGTH OF JOB                                
LOFF     DS    X                   LENGTH OF OFFICE CODE                        
LDEP     DS    X                   LENGTH OF DEPARTMENT CODE                    
LSUB     DS    X                   LENGTH OF SUB-DEPARTMENT CODE                
*                                                                               
PSNTBLK  DS    XL(PSNTBLKL)        ACPRESENT INTERFACE BLOCK                    
ACPRBLK  DS    XL(ACPRL)           ACPRESTO INTERFACE BLOCK                     
*                                                                               
TRTAB    DS    XL256               TRANSLATE TABLE FOR OBJECTS                  
*                                                                               
BIGKEY   DS    XL(ACCKLEN)         KEY FOR ACC DIR READS                        
KEYSAVE  DS    XL(ACCKLEN)                                                      
         DS    0D                                                               
DMWORK   DS    80X                                                              
IOL      DS    F                   IO AREA FOR READING RECORDS                  
IO       DS    2048X                                                            
OUTIOL   DS    F                   IO AREA FOR OUTPUTTING MAD OBJECTS           
OUTIO    DS    1000X                                                            
OBJECTS  DS    CL8000                                                           
WORKX    DS    0D                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046PXTRACT   11/15/18'                                      
         END                                                                    
