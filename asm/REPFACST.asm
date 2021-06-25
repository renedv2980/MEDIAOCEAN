*          DATA SET REPFACST   AT LEVEL 111 AS OF 01/15/02                      
*PHASE T00AACA                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGENPTR                                                               
*INCLUDE REBROWSE                                                               
*INCLUDE REGENDEL                                                               
*INCLUDE REGENBUC                                                               
*INCLUDE REGENVER                                                               
*INCLUDE READDBUC                                                               
*INCLUDE RETIMVAL                                                               
*INCLUDE REDAYVAL                                                               
*INCLUDE REFLSCAN                                                               
*INCLUDE RECKSEC                                                                
*INCLUDE RECKASEC                                                               
*INCLUDE REAUDIT                                                                
*INCLUDE REBLAME                                                                
*INCLUDE GETBROAD                                                               
         TITLE 'REPFACS  - REPPAK COMMON SUBROUTINE FACILITY'                   
** THIS MODULE IS CORE RESIDENT **                                              
***********************************************************************         
* HISTORY                                                             *         
***********************************************************************         
*                                                                     *         
* 16SEP97 RHV  GOT TIRED OF WRITING THE SAME CODE 1500 TIMES          *         
*                                                                     *         
* 22OCT97 RHV  SUPPORT TRANSPARENT RM CALLS & USE GOTOX               *         
*                                                                     *         
* 07NOV97 JRD  ADDED BUCKUP ROUNTINE                                  *         
*              ADDED ALTERNATE FLAG SETTING ROUTINE                   *         
*              TEMPORARY INCLUDE OF REGENBUJ (ALT BUCKET SUPPORT)     *         
*                                                                     *         
* 24NOV97 RHV  ADD REDAYVAL & RETIMVAL                                *         
*                                                                     *         
* 15DEC97 JRD  GETMONTH ENTRY FROM REGENBUCJ                          *         
*                                                                     *         
* 09FEB98 JRD  ALTERNATE CALENDAR VS CONTRACT FLIGHT VALIDATION       *         
*                                                                     *         
* 18MAR98 RHV  REGENPTR                                               *         
*                                                                     *         
* 29JUN98 RHV  GETREC ROUTINE, REVISE RESOLUTION OF RFBLOCK ADDRESSES *         
*                                                                     *         
* 01SEP98 RHV  TRANSPARENT CALLS DON'T CLAIM STORAGE                  *         
*                                                                     *         
* 02SEP98 RHV  P5 PASS THRU OF REPFACS ADDRESS                        *         
*                                                                     *         
***********************************************************************         
*  PARAMETERS FOR CALLING REPFACS:                                              
*                                                                               
*              (RF) = BYTE 0: SUBROUTINE CODE (REPFACSQ)                        
*                                                                               
*              ALL DMCB PARAMETERS DEFINED BY SUBROUTINE                        
*                                                                               
*      NOTE: SOME ROUTINES REQUIRE P4=RFBLOCK WHERE:                            
*            RFBLOCK= CL4 A(COMFACS)                                            
*                     CL2 CURRENT REP CODE                                      
*                                                                               
***********************************************************************         
* REPFACS MAIN PROGRAM                                                          
***********************************************************************         
REPFACS  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*REPFA*,RR=R7                                                  
         LR    R9,RB                                                            
         A     R9,=A(COMMON-REPFACS)                                            
         USING COMMON,R9                                                        
*                                                                               
RFMAIN05 DS    0H                                                               
         LA    RE,RFTAB            FIND SUBROUTINE CODE IN RFTAB                
         B     *+8                                                              
RFMAIN10 LA    RE,6(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID SUBROUTINE CODE!!                    
         CLM   RF,8,0(RE)                                                       
         BNE   RFMAIN10                                                         
*                                                                               
         TM    5(RE),X'80'         EXTERNAL ADDRESS?                            
         BZ    RFMAIN20            NO - INTERNAL                                
         TM    5(RE),X'10'         PASS V(REPFACS) IN P5?                       
         BZ    *+8                 NO                                           
         ST    RB,16(R1)                                                        
         GOTO1 1(RE),(R1),,,,,,,RR=Y  YES -DO IT                                
         B     EXIT                                                             
*                                                                               
RFMAIN20 DS    0H                  INTERNAL ADDRESS                             
         BAS   R8,GETSTOR          GET SOME WORKING STORAGE                     
         USING WORKD,RC                                                         
         XC    ANEWIO,ANEWIO                                                    
         TM    5(RE),X'20'         CREATE IO AREA IF P2 NULLS?                  
         BZ    RFMAIN25            NO                                           
         OC    4(4,R1),4(R1)       NULLS?                                       
         BNZ   RFMAIN25            NOT NULLS                                    
         BAS   R8,GETNEWIO         MAKE NEW IO AREA IN CALLERS STORAGE          
         OI    FLAGS,FNEWIO        REMEMBER WE DID THIS                         
         ST    R6,ANEWIO           A(NEW IOAREA) IN CALLER                      
*                                                                               
RFMAIN25 DS    0H                  NOW SAFE TO INIT WORKING STORAGE             
         ST    R7,RELO                                                          
         ST    R1,PSAVE            SAVE A(PARAMETERS)                           
*                                                                               
         TM    5(RE),X'40'         RFBLOCK PASSED IN P4?                        
         BZ    RFMAIN30            NO                                           
*                                                                               
         ICM   RF,15,12(R1)        SAVE OFF RFBLOCK PARAMS                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACOMFACS,0(RF)                                                   
         MVC   REPALPHA,4(RF)                                                   
         L     RF,ACOMFACS         RESOLVE COMFACS ADDRESSES                    
         USING COMFACSD,RF                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         DROP  RF                                                               
*                                                                               
RFMAIN30 DS    0H                                                               
         ICM   RF,15,1(RE)         TAKE ROUTINE ADDRESS                         
         A     RF,RELO             RELOCATE                                     
         BASR  RE,RF               AND GO!!                                     
*                                                                               
         BNL   EXIT                NOT LOW - GET OUT OF HERE                    
         TM    FLAGS,FNEWIO        DID WE MAKE IOAREA IN CALLER?                
         BZ    EXITL               NO                                           
         BAS   R8,REMNEWIO         WE DIDN'T USE IT, REMOVE IT                  
         B     EXITL                                                            
***********************************************************************         
* SUBROUTINE ADDRESS TABLE                                                      
***********************************************************************         
* RFTAB DEFINITON:                                                              
*        AL1   SUBROUTINE CODE EQUATE (FROM REPFACSQ)                           
*        AL4   SUBROUTINE ADDRESS / VL4 IF EXTERNAL                             
*        XL1   FLAGS: X'80' - EXTERNAL ROUTINE ADDRESS                          
*                     X'40' - REQUIRES RFBLOCK IN P4 (INTERNAL ONLY)            
*                     X'20' - IF P2 NULLS, CREATE IO AREA IN CALLER             
*                     X'10' - PASS V(REPFACS) IN P5 (EXT ROUTS ONLY)            
*                                                                               
RFTAB    DS    0H                                                               
         DC    AL1(RFSTAOUT),AL4(STAO),X'00'                                    
         DC    AL1(RFCMBNUM),AL4(CMBN),X'00'                                    
         DC    AL1(RFCONNUM),AL4(CONNUM),X'00'                                  
         DC    AL1(RFAGYOUT),AL4(AGYO),X'40'                                    
         DC    AL1(RFCONLOW),AL4(CONLOW),X'00'                                  
         DC    AL1(RFGENBUC),VL4(REGENBUC),X'80'                                
         DC    AL1(RFGENVER),VL4(REGENVER),X'80'                                
         DC    AL1(RFADDBUC),VL4(ADDBUCK),X'80'                                 
         DC    AL1(RFBUCKUP),AL4(BUCKUP),X'00'                                  
         DC    AL1(RFCHKALT),AL4(CHKALT),X'00'                                  
         DC    AL1(RFDAYVAL),VL4(REDAYVAL),X'80'                                
         DC    AL1(RFTIMVAL),VL4(RETIMVAL),X'80'                                
         DC    AL1(RFFLSCAN),VL4(REFLSCAN),X'80'                                
         DC    AL1(RFGMONTH),VL4(GETMONTH),X'80'                                
         DC    AL1(RFVALTCL),AL4(VALTCAL),X'40'                                 
         DC    AL1(RFGENPTR),VL4(REGENPTR),X'80'                                
         DC    AL1(RFGENDEL),VL4(REGENDEL),X'80'                                
         DC    AL1(RFBROWSE),VL4(REBROWSE),X'80'                                
         DC    AL1(RFGETREC),AL4(GETR),X'60'                                    
         DC    AL1(RFCKSEC),VL4(RECKSEC),X'90'                                  
         DC    AL1(RFCONKY),AL4(CONKEYS),X'40'                                  
         DC    AL1(RFCKASEC),VL4(RECKASEC),X'90'                                
         DC    AL1(RFGETID),AL4(GETID),X'40'                                    
         DC    AL1(RFVALAGY),AL4(VAGY),X'40'                                    
         DC    AL1(RFADVOUT),AL4(ADVO),X'40'                                    
         DC    AL1(RFSALOUT),AL4(SALO),X'40'                                    
         DC    AL1(RFKFLT),AL4(KFLT),X'40'                                      
         DC    AL1(RFGETBRD),VL4(GETBROAD),X'80'                                
         DC    AL1(RFAUDIT),VL4(REAUDIT),X'80'                                  
         DC    AL1(RFBLAME),VL4(REBLAME),X'80'                                  
         DC    AL1(RFGPROF),AL4(GETPROF),X'40'                                  
         DC    AL1(RFGETTAB),AL4(GETTAB),X'00'                                  
         DC    X'FF'                                                            
*                                                                               
         DROP  RB                                                               
***********************************************************************         
* >>>>>>>>>>>>>>  START OF SUBROUTINE LIBRARY  <<<<<<<<<<<<<<<<<<<<<< *         
***********************************************************************         
***********************************************************************         
* KFLT - FORMATS CONTRACT FLIGHT DATES FOR OUTPUT                               
*                                                                               
*          P1: A(CONTRACT RECORD)                                               
*              BYTE 0 = 0   : OUTPUT YYMMDDYYMMDD                               
*                     = 80  : OUTPUT MMMDD/YY-MMMDD/YY                          
*                                                                               
*          P2: A(OUTPUT AREA)  YYMMDDYYMMDD   OR                                
*                              MMMDD/YY-MMMDD/YY                                
*                                                                               
* ON RETURN: CC = EQ  : RCONDATE USED                                           
*            CC = ~EQ : RCONRFLT USED                                           
***********************************************************************         
KFLT     NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            RCONREC                                      
         L     R4,4(R1)            A(OUTPUT AREA)                               
         USING RCONREC,R3                                                       
         LA    R2,RCONDATE                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   KFLT10                                                           
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    KFLT10                                                           
         LA    R2,RCONRFLT                                                      
         DROP  R6                                                               
KFLT10   DS    0H                                                               
         TM    0(R1),X'80'                                                      
         BO    KFLT20                                                           
         GOTO1 DATCON,DMCB,(3,0(R2)),(0,0(R4))                                  
         GOTO1 DATCON,DMCB,(3,3(R2)),(0,6(R4))                                  
         B     KFLT30                                                           
KFLT20   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(5,0(R4))                                  
         GOTO1 DATCON,DMCB,(3,3(R2)),(5,9(R4))                                  
         MVI   8(R4),C'-'                                                       
KFLT30   DS    0H                                                               
         CLC   RCONDATE,0(R3)      USED REVISED DATE? (SET CC)                  
         B     EXIT                                                             
         DROP  R3                                                               
***********************************************************************         
* CONLOW - RETURNS THE LOWEST # K IN COMBO OR K NUM IF NOT COMBO                
*                                                                               
*          P1: A(CONTRACT RECORD)                                               
*                                                                               
* ON RETURN: P1: CONTRACT # (0C KEY STYLE PWOS)                                 
*                                                                               
***********************************************************************         
CONLOW   NTR1  BASE=*,LABEL=*                                                   
         L     R6,0(R1)                                                         
         MVC   0(4,R1),23(R6)      DEFAULT RETURN K NUM                         
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'2'                                                         
         SR    R4,R4                                                            
         D     R4,=F'9'                                                         
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,7(R6)                                                         
CONLOW20 CLC   0(4,R6),0(R1)                                                    
         BNL   *+10                                                             
         MVC   0(4,R1),0(R6)                                                    
         BCT   R5,CONLOW20                                                      
         B     EXIT                                                             
***********************************************************************         
* CMBN - CHECKS IF CONTRACT IS A COMBO & RETURNS # OF RELATED CONTRACTS         
*                                                                               
*          P1: A(CONTRACT RECORD)                                               
*                                                                               
* ON RETURN: P1: BYTE 0 - # OF CONTRACTS IN COMBO OR 1 IF NOT COMBO             
*                BYTES 1-3 - A(X'17' COMBO ELEMENT)                             
*                                                                               
***********************************************************************         
CMBN     NTR1  BASE=*,LABEL=*                                                   
         ZICM  R6,1(R1),3                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    CMBN10                                                           
         MVI   0(R1),1                                                          
         B     EXIT                                                             
CMBN10   DS    0H                                                               
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'2'                                                         
         SR    R4,R4                                                            
         D     R4,=F'9'                                                         
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R6,0(R1)                                                         
         STC   R5,0(R1)                                                         
         B     EXIT                                                             
***********************************************************************         
* STAO - FORMATS STATION CALL LETTERS FOR OUTPUT                                
*                                                                               
*          P1: A(5 CHAR STATION CALL LETTERS) AS IN RECORDS                     
*                                                                               
*          P2: BYTE 0 - X'01' 1 LETTER BAND EXPANSION (IE '-A')                 
*                             DEFAULT IS 2 LETTER EXPANSION (IE '-AM')          
*                       ON RETURN SET TO # OF OUTPUT CHARACTERS                 
*              BYTES 1-3 - A(OUTPUT AREA)                                       
*                                                                               
***********************************************************************         
STAO     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         ZICM  R3,1(R1),3                                                       
         MVC   WORK(5),0(R3)                                                    
         OC    WORK(5),SPACES                                                   
         CLC   WORK(4),SPACES                                                   
         BE    EXIT                                                             
         ZICM  R5,5(R1),3                                                       
         LA    R3,WORK+4                                                        
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BE    *-6                                                              
         LA    R4,WORK                                                          
         SR    R3,R4                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         LA    R3,1(R3)                                                         
         AR    R5,R3                                                            
         TM    4(R1),X'01'                                                      
         BZ    STAO50                                                           
         CLI   WORK+4,C' '                                                      
         BE    STAO20                                                           
         MVI   0(R5),C'-'                                                       
         MVC   1(1,R5),WORK+4                                                   
         LA    R3,2(R3)                                                         
STAO20   STC   R3,4(R1)                                                         
         B     EXIT                                                             
STAO50   DS    0H                                                               
         LA    R3,2(R3)                                                         
         ST    R3,4(R1)                                                         
         MVI   0(R5),C'-'                                                       
         MVI   1(R5),C'L'                                                       
         CLI   WORK+4,C'L'                                                      
         BE    EXIT                                                             
         MVI   1(R5),C'C'                                                       
         CLI   WORK+4,C'C'                                                      
         BE    EXIT                                                             
         LA    R3,1(R3)                                                         
         STC   R3,4(R1)                                                         
         MVC   1(2,R5),=C'TV'                                                   
         CLI   WORK+4,C' '                                                      
         BE    EXIT                                                             
         CLI   WORK+4,C'T'                                                      
         BE    EXIT                                                             
         MVC   1(2,R5),=C'AM'                                                   
         CLI   WORK+4,C'A'                                                      
         BE    EXIT                                                             
         MVC   1(2,R5),=C'FM'                                                   
         CLI   WORK+4,C'F'                                                      
         BE    EXIT                                                             
         DC    H'0'                                                             
***********************************************************************         
* SALO - EXPANDS SALESMAN NAME                                                  
*                                                                               
*          P1: A(3 BYTE SAL CODE) AS IN RECORDS                                 
*                                                                               
*          P2: A(OUTPUT AREA) FOR EXPANSION                                     
*                                                                               
*          P3: NOT USED                                                         
*                                                                               
*          P4: RFBLOCK                                                          
*                                                                               
*          CC: RETURNS EQUAL     - ACTION SUCCESSFUL                            
*                      NOT EQUAL - NOT SUCCESSFUL                               
*                                                                               
***********************************************************************         
SALO     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         L     R4,4(R1)            A(OUTPUT AREA)                               
         ZICM  R3,1(R1),3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(4),0(R3)                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BNE   EXITL                                                            
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA,DMWORK                     
         MVC   0(L'RSALNAME,R4),IOAREA+(RSALNAME-RSALREC) SAL NAME              
         B     EXITOK                                                           
***********************************************************************         
* ADVO - EXPANDS ADVERTISER NAME                                                
*                                                                               
*          P1: A(4 BYTE ADV CODE) AS IN RECORDS                                 
*                                                                               
*          P2: A(OUTPUT AREA) FOR EXPANSION                                     
*                                                                               
*          P3: NOT USED                                                         
*                                                                               
*          P4: RFBLOCK                                                          
*                                                                               
*          CC: RETURNS EQUAL     - ACTION SUCCESSFUL                            
*                      NOT EQUAL - NOT SUCCESSFUL                               
*                                                                               
***********************************************************************         
ADVO     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         L     R4,4(R1)            A(OUTPUT AREA)                               
         ZICM  R3,1(R1),3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),0(R3)                                                  
         MVC   KEY+25(2),REPALPHA                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BNE   EXITL                                                            
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA,DMWORK                     
         MVC   0(L'RADVNAME,R4),IOAREA+(RADVNAME-RADVREC) ADV NAME              
         B     EXITOK                                                           
***********************************************************************         
* AGYO - FORMATS AGENCY CODES FOR OUTPUT                                        
*                                                                               
*          P1: A(6 BYTE AGY & AGY OFC CODE) AS IN RECORDS                       
*                                                                               
*          P2: BYTE 0 - ON RETURN = # OF OUTPUT CHARACTERS                      
*              BYTES 1-3 - A(OUTPUT AREA)                                       
*                                                                               
*          P3: A(AGY EXPANSION OUTPUT AREA) - OPTIONAL                          
*              OR NULLS!!!!!                                                    
*                                                                               
*          P4: RFBLOCK                                                          
*                                                                               
***********************************************************************         
AGYO     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         ZICM  R3,1(R1),3                                                       
         MVC   WORK(6),0(R3)                                                    
         OC    WORK(6),SPACES                                                   
         CLC   WORK(4),SPACES                                                   
         BE    EXIT                                                             
         ZICM  R5,5(R1),3                                                       
         LA    R3,WORK+4                                                        
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BE    *-6                                                              
         LA    R4,WORK                                                          
         SR    R3,R4                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         LA    R3,1(R3)                                                         
         AR    R5,R3                                                            
         CLC   WORK+4(2),SPACES                                                 
         BE    AGYO50                                                           
         MVI   0(R5),C'-'                                                       
         MVC   1(2,R5),WORK+4                                                   
         LA    R3,4(R3)                                                         
         STC   R3,4(R1)                                                         
AGYO50   DS    0H                                                               
         OC    8(4,R1),8(R1)                                                    
         BZ    EXIT                                                             
         L     R4,8(R1)                                                         
         ZICM  R3,1(R1),3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),0(R3)                                                  
         MVC   KEY+25(2),REPALPHA                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MISSING AGYREC                               
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA,DMWORK                     
         MVC   0(20,R4),IOAREA+36  AGY NAME                                     
         B     EXIT                                                             
***********************************************************************         
* VAGY - VALIDATES AGENCY CODE FIELD                                            
*                                                                               
*          P1: A(FIELD HEADER TO VALIDATE AGY CODE FROM)                        
*                                                                               
*          P2: A(IOAREA TO RETURN AGY RECORD)                                   
*                                                                               
*          P3: NOT USED                                                         
*                                                                               
*          P4: RFBLOCK                                                          
*                                                                               
*  ON RETURN: CC - EQUAL - AGY VALID, RECORDS RETRIEVED                         
*                - NOT EQUAL - INPUT NOT VALID AGENCY                           
*                                                                               
***********************************************************************         
VAGY     NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)          A(FIELD HEADER)                              
         CLI   5(R2),0                                                          
         BE    EXITL                                                            
         CLI   5(R2),7                                                          
         BH    EXITL                                                            
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         OC    WORK(20),SPACES                                                  
         LA    R2,WORK                                                          
         LA    R3,WORK+10                                                       
         LA    R4,WORK+14                                                       
VAGY10   DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BE    VAGY15                                                           
         CLI   0(R2),C'-'                                                       
         BE    VAGY15                                                           
         CR    R3,R4                                                            
         BNL   VAGY16                                                           
         MVC   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         B     VAGY10                                                           
*                                                                               
VAGY15   DS    0H                                                               
         LA    R2,1(R2)                                                         
VAGY16   DS    0H                                                               
         MVC   0(2,R4),0(R2)                                                    
*                                                                               
VAGY50   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),WORK+10                                                
         MVC   KEY+25(2),REPALPHA                                               
         L     R6,4(R1)            AIOAREA FROM P2                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    8(R1),X'10'                                                      
         BO    EXITL                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,(R6),DMWORK                       
         B     EXITOK                                                           
***********************************************************************         
* CONNUM - CONVERTS CONTRACT NUMBER FORMATS                                     
*                                                                               
*          P1: BYTE 0 - INPUT TYPE (SEE BELOW)                                  
*              BYTES 1-3 - A(INPUT CONTRACT NUMBER OR FLD HEADER)               
*                                                                               
*          P2: BYTE 0 - OUTPUT TYPE (SEE BELOW)                                 
*              BYTES 1-3 - A(OUTPUT CONTRACT NUMBER OR FLD HEADER)              
*                                                                               
* CONTRACT NUMBER INPUT/OUTPUT TYPES:                                           
* 1 = PWOS 4-BYTE (0C KEY NORMAL STYLE)                                         
* 2 = PWOS 9'S COMPLIMENT 4-BYTE (8C KEY STYLE)                                 
* 3 = PWOS 9'S COMPLIMENT REVERSED (RBUYREC 0B KEY STYLE)                       
* 4 = FIELD HEADER                                                              
* 5 = EBCDIC 8 BYTE RIGHT-ALIGNED (OUTPUT ONLY)                                 
* 6 = EBCDIC LEFT-ALIGNED (OUTPUT ONLY) LEN RETURNED IN P1 HIGH BYTE            
* 7 = BINARY 4-BYTE                                                             
***********************************************************************         
CONNUM   NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL   **CONVERT INPUT TO PWOS IN FULL**                    
         ZICM  R3,1(R1),3                                                       
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                MISSING A(INPUT)                             
*                                                                               
         CLI   0(R1),1             INPUT TYPE 1 (PWOS)                          
         BNE   CNUM010                                                          
         MVC   FULL,0(R3)                                                       
         B     CNUM100                                                          
CNUM010  DS    0H                  INPUT TYPE 7 (BINARY 4 BYTE)                 
         CLI   0(R1),7                                                          
         BNE   CNUM020                                                          
         XC    DUB,DUB                                                          
         ICM   R4,15,0(R3)                                                      
         CVD   R4,DUB                                                           
         LM    R4,R5,DUB                                                        
         SRDL  R4,4                                                             
         ST    R5,FULL                                                          
         B     CNUM100                                                          
CNUM020  DS    0H                  INPUT TYPE 4 - FIELD HEADER                  
         CLI   0(R1),4                                                          
         BNE   CNUM030                                                          
         ZIC   R4,0(R3)            FLD LEN                                      
         AHI   R4,-8               MINUS HDR LEN                                
         TM    1(R3),X'2'                                                       
         BZ    *+8                                                              
         AHI   R4,-8             MINUS EXT HDR LEN                              
         BCTR  R4,0                MAX DATA LEN -1                              
         TM    4(R3),X'08'         VALID NUMERIC?                               
         BO    *+6                                                              
         DC    H'0'                HAD BETTER BE                                
         LA    R3,8(R3)            A(DATA)                                      
         EX    R4,*+4                                                           
         OC    0(0,R3),SPACES                                                   
         LA    R5,0(R4,R3)         LAST CHAR OF DATA                            
         CLI   0(R3),X'40'         STRIP LEADING SPACES                         
         BNE   *+14                                                             
         LA    R3,1(R3)                                                         
         BCTR  R4,0                                                             
         B     *-14                                                             
         CLI   0(R5),X'40'         STRIP TRAILING SPACES                        
         BNE   *+12                                                             
         BCTR  R5,0                                                             
         BCTR  R4,0                                                             
         B     *-12                                                             
         CHI   R4,7                                                             
         BNH   *+6                                                              
         DC    H'0'                MAX LEN OF K NUM IS 8                        
         ZAP   DUB,=P'0'                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         LM    R4,R5,DUB                                                        
         SRDL  R4,4                                                             
         ST    R5,FULL                                                          
         B     CNUM100                                                          
CNUM030  DS    0H                                                               
         CLI   0(R1),2                                                          
         BE    CNUM035                                                          
         CLI   0(R1),3                                                          
         BNE   CNUM040                                                          
         PACK  FULL+0(1),3(1,R3)                                                
         PACK  FULL+1(1),2(1,R3)                                                
         PACK  FULL+2(1),1(1,R3)                                                
         PACK  FULL+3(1),0(1,R3)                                                
         LA    R3,FULL                                                          
*                                                                               
CNUM035  L     R0,=X'99999999'                                                  
         S     R0,0(R3)                                                         
         STCM  R0,15,FULL                                                       
         B     CNUM100                                                          
*                                                                               
CNUM040  DS    0H                                                               
         DC    H'0'                INVALID INPUT TYPE                           
*                                                                               
CNUM100  DS    0H      **NOW CONVERT PWOS IN FULL TO DESIRED OUTPUT**           
         OC    FULL,FULL                                                        
         BZ    CNUM200                                                          
         ZICM  R3,5(R1),3                                                       
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                MISSING A(OUTPUT)                            
*                                                                               
         CLI   4(R1),1             TYPE 1 (PWOS 4 BYTE)                         
         BNE   CNUM110                                                          
         MVC   0(4,R3),FULL                                                     
         B     CNUM200                                                          
CNUM110  DS    0H                  TYPE 5 (EBCDIC R-ALIGNED 8 BYTE)             
         CLI   4(R1),5                                                          
         BNE   CNUM120                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,0(R3),4,RR=Y                                
         LR    R4,R3                                                            
CNUM115  DS    0H                                                               
         CLI   0(R4),C'0'                                                       
         BNE   CNUM200                                                          
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         B     CNUM115                                                          
CNUM120  DS    0H                  TYPE 4 (FLD HEADER)                          
         CLI   4(R1),4                                                          
         BNE   CNUM160                                                          
         B     CNUM200                                                          
CNUM160  DS    0H                  TYPE 6 (LEFT ALIGN EBCDIC)                   
         CLI   4(R1),6                                                          
         BNE   CNUM170                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,WORK,4,RR=Y                                 
         LA    R4,WORK                                                          
         LA    R5,8                                                             
CNUM165  DS    0H                                                               
         CLI   0(R4),C'0'                                                       
         BNE   CNUM168                                                          
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNZ   CNUM165                                                          
CNUM168  DS    0H                                                               
         L     R1,PSAVE                                                         
         STC   R5,0(R1)                                                         
         LTR   R5,R5                                                            
         BZ    CNUM200                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         B     CNUM200                                                          
CNUM170  DS    0H                  TYPE 2 (PWOS 9'S COMP 4 BYTE)                
         L     R0,=X'99999999'                                                  
         S     R0,FULL                                                          
         CLI   4(R1),2                                                          
         BNE   CNUM180                                                          
         STCM  R0,15,0(R3)                                                      
         B     CNUM200                                                          
CNUM180  DS    0H                  TYPE 3 (PWOS 9'S COMP REVERSED 4BYT)         
         CLI   4(R1),3                                                          
         BE    *+6                                                              
         DC    H'0'                INVALID TYPE                                 
         ST    R0,FULL                                                          
         PACK  0(1,R3),FULL+3(1)                                                
         PACK  1(1,R3),FULL+2(1)                                                
         PACK  2(1,R3),FULL+1(1)                                                
         PACK  3(1,R3),FULL+0(1)                                                
CNUM200  DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* BUCKUP:                                                                       
*    INPUT:   PARM1 - BYTE 0        FF - SUBTRACT BUCKETS                       
*                     BYTE 1-3      A(BUY OR PLAN RECORD)                       
*                                                                               
*             PARM1 2 BYTE 0        ALT. CALENDAR FLAGS                         
*                                   80 - USE ALT CALENDAR                       
*                                   40 - FORCE REP LEVEL                        
*                                   20 - FORCE STATION LEVEL                    
*                                   10 - DON'T IGNORE CANCELLED BUYS            
*                                                                               
*                     BYTE 1-3      A(CONTRACT)                                 
*                                                                               
*             PARM3 - BYTE 1-3      A(COMFACS)                                  
*             PARM4 - BYTE 1-3      A(GETBROAD)                                 
*             PARM5 - BYTE 1-3      A(RECUP)                                    
*                                                                               
*    OUTPUT:  CC EQUAL & UPDATED CONTRACT & BUY OR PLAN RECORDS                 
*                            OR                                                 
*             CC NOT EQUAL & ERROR MESSAGE # IN PARM1                           
*                                                                               
***********************************************************************         
BUCKUP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(BUYREC)                                    
         MVC   BYTE,4(R1)          BUCKET FLAGS                                 
         L     R7,4(R1)            A(RCONREC)                                   
         USING RCONREC,R7                                                       
         L     RE,8(R1)            A(COMFACS)                                   
         ST    RE,ACOMFACS                                                      
         MVC   WORK(4),12(R1)                            A(GETBROAD)            
         MVC   WORK+4(4),CGETDAY-COMFACSD(RE)            A(GETDAY)              
         MVC   WORK+8(4),CADDAY-COMFACSD(RE)             A(ADDAY)               
         MVC   WORK+12(4),CDATCON-COMFACSD(RE)           A(DATCON)              
         MVC   WORK+16(4),CDATAMGR-COMFACSD(RE)          A(DATAMGR)             
         MVC   WORK+20(4),16(R1)                         A(RECUP)               
*                                                                               
         USING RBUYREC,R2                                                       
         CLI   RBUYCHGI,C'C'                                                    
         BNE   BUCU005                                                          
         TM    BYTE,X'10'          PROCESS CANCELLED BUYS?                      
         BZ    BUCU022                                                          
         DROP  R2                                                               
*                                                                               
* BUILD BUCKETS EST BUCKETS                                                     
*                                                                               
BUCU005  DS    0H                                                               
         GOTO1 =V(REGENBUC),DMCB,(R2),IOAREA,WORK,RR=Y                          
*                                                                               
* ADD BUCKETS TO CONREC                                                         
*                                                                               
         CLC   IOAREA(2),=H'2'     NONE?                                        
         BE    BUCU012                                                          
*                                                                               
         MVC   HALF,IOAREA                                                      
         LH    R5,HALF             LEN OF BUCKETS                               
         LA    R5,IOAREA-1(R5)                                                  
         LA    R3,IOAREA+2         1ST BUCKET                                   
*                                                                               
* ADD BUCKET TO CONREC (OR SUBTRACT)                                            
*                                                                               
         ICM   R0,15,WORK+20                                                    
BUCU010  MVI   1(R3),10            K BUCKET LENGTH                              
         GOTO1 =V(ADDBUCK),DMCB,(R7),(R3),(R0),RR=Y                             
         MVI   1(R3),14            RESTORE LENGTH                               
         ZIC   R4,1(R3)                                                         
         BXLE  R3,R4,BUCU010       NEXT BUCKET                                  
*                                                                               
BUCU012  DS    0H                                                               
*                                                                               
* BUILD BUCKETS ALTERNATE EST BUCKETS                                           
*                                                                               
         TM    BYTE,X'80'          USING ALTERNATE CALENDAR?                    
         BZ    BUCU022             NO - DON'T BUILD THEM                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,RCONKREP                                                    
         LA    R5,X'FF'            SET DEFAULT ALT. CALENDAR REQ.               
         TM    BYTE,X'40'          REP LEVEL FORCED?                            
         BZ    *+8                 NO                                           
         LA    R5,X'F1'                                                         
         TM    BYTE,X'20'          STATION LEVEL FORCED?                        
         BZ    *+8                 NO                                           
         LA    R5,X'F0'                                                         
         GOTO1 =V(REGENBUC),DMCB,(R2),((R5),IOAREA),WORK,(R0),         +        
               RCONKSTA,RR=Y                                                    
         BE    *+18                                                             
         L     R1,PSAVE                                                         
         MVC   0(4,R1),=A(735)                                                  
         B     EXITL                                                            
*                                                                               
* ADD ALTERNATE CALENDAR ELEMENT TO CONREC                                      
*                                                                               
         XC    WORK+100(RCONAXLQ),WORK+100                                      
E        USING RCONAXEL,WORK+100                                                
         MVI   E.RCONAXCO,X'A4'                                                 
         MVI   E.RCONAXLN,RCONAXLQ                                              
         TM    IOAREA,X'80'        STATION RECORD USED?                         
         BNO   *+12                NO                                           
         OI    E.RCONAXFL,X'40'                                                 
         B     *+8                                                              
         OI    E.RCONAXFL,X'80'                                                 
         DROP  E                                                                
*                                                                               
         LA    R5,RCONELEM                                                      
BUCU014  DS    0H                                                               
         CLI   0(R5),0             END OF RECORD?                               
         BE    BUCU016             YES - ADD ELEMENT                            
         CLI   0(R5),X'A4'         ALT CALENDAR ELEM?                           
         BE    BUCU018             YES - NOTHING TO ADD                         
         BH    BUCU016             ADD ELEMENT HERE                             
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     BUCU014                                                          
*                                                                               
BUCU016  DS    0H                                                               
         ICM   RF,15,WORK+20                                                    
         GOTO1 (RF),DMCB,(C'R',(R7)),WORK+100,(R5)                              
*                                                                               
* ADD BUCKETS TO CONREC                                                         
*                                                                               
BUCU018  DS    0H                                                               
         NI    IOAREA,X'FF'-X'80'  TURN OFF STATION FLAG                        
         CLC   IOAREA(2),=H'2'     NONE?                                        
         BE    BUCU022                                                          
*                                                                               
         MVC   HALF,IOAREA                                                      
         LH    R5,HALF             LEN OF BUCKETS                               
         LA    R5,IOAREA-1(R5)                                                  
         LA    R3,IOAREA+2         1ST BUCKET                                   
*                                                                               
* ADD BUCKET TO CONREC (OR SUBTRACT)                                            
*                                                                               
         ICM   R0,15,WORK+20                                                    
BUCU020  MVI   1(R3),10            K BUCKET LENGTH                              
         GOTO1 =V(ADDBUCK),DMCB,(R7),(R3),(R0),RR=Y                             
         MVI   1(R3),14            RESTORE LENGTH                               
         ZIC   R4,1(R3)                                                         
         BXLE  R3,R4,BUCU020       NEXT BUCKET                                  
*                                                                               
BUCU022  DS    0H                                                               
         B     EXITOK                                                           
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHKALT:                                                                       
*    INPUT:   PARM1 - BYTE 0        CURRENT BUCKET FLAG                         
*                     BYTE 1-3      A(CONTRACT)                                 
*             PARM2 - BYTE 1-3      A(COMFACS)                                  
*                                                                               
*    OUTPUT:  PARM1 - BYTE 0        NEW BUCKET FLAGS                            
*                                                                               
***********************************************************************         
CHKALT   NTR1  BASE=*,LABEL=*                                                   
         ST    R1,FOO              ALLOW FOR INTERNAL CALLS                     
         MVC   BYTE,0(R1)          BUCKET FLAGS                                 
         NI    BYTE,X'FF'-(X'80'+X'40'+X'20')                                   
         L     R7,0(R1)            A(CONTRACT)                                  
         USING RCONREC,R7                                                       
         L     RE,4(R1)            A(COMFACS)                                   
         ST    RE,ACOMFACS                                                      
         L     RE,CDATAMGR-COMFACSD(RE)                                         
         ST    RE,DATAMGR                                                       
*                                                                               
         XC    KEY,KEY             READ STATION RECORD AND SET ACL FLAG         
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,RCONKREP                                              
         MVC   K.RSTAKSTA,RCONKSTA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MISSING STATION RECORD                       
*                                                                               
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA2,DMWORK                    
*                                                                               
         LA    R6,IOAREA2                                                       
         LA    R6,RSTAELEM-RSTAREC(R6)                                          
         MVI   ELCODE,X'08'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,NEXTEL                                                        
         BNE   CHKALTX             NO                                           
*                                                                               
         TM    RSTAOPTA-RSTAXXEL(R6),X'20'       USES ALT CALENDAR?             
         BZ    CHKALTX                           NO                             
*                                                                               
* CHECH ALTERNATE CALENDAR INDICATORS IN CONTRACT                               
*                                                                               
         LA    R6,RCONELEM                                                      
         MVI   ELCODE,X'03'        BUCKETS?                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHKALT10            NO - SET ALT CALENDAR FLAG                   
*                                                                               
         LA    R6,RCONELEM                                                      
         MVI   ELCODE,X'A4'        ALT CALENDAR ELEMENT?                        
         BAS   RE,NEXTEL                                                        
         BNE   CHKALTX             NO - OLD CONTRACT NO ALT BUCKETS             
*                                                                               
         TM    RCONAXFL-RCONAXEL(R6),X'80'       FORCE REP LEVEL?               
         BZ    *+8                                                              
         OI    BYTE,X'40'                        YES                            
*                                                                               
         TM    RCONAXFL-RCONAXEL(R6),X'40'       FORCE STATION LEVEL?           
         BZ    *+8                                                              
         OI    BYTE,X'20'                        YES                            
*                                                                               
         TM    BYTE,X'40'+X'20'                  SOMETHING SET?                 
         BNZ   *+6                               YES - OK TO CONTINUE           
         DC    H'0'                                                             
*                                                                               
CHKALT10 DS    0H                                                               
         OI    BYTE,X'80'          SET USING ALT CALENDAR                       
*                                                                               
CHKALTX  DS    0H                                                               
         L     R1,FOO              RETURN NEW FLAGS                             
         MVC   0(1,R1),BYTE                                                     
         B     EXITOK                                                           
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                  <<-- SAVE COMMON BLOCK ADDRESSING            
         SPACE 3                                                                
***********************************************************************         
* VALIDATE CONTRACT FLIGHT AGAINST ALTERNATE CALENDARS IF REQ'D                 
*                                                                               
* INPUT:                                                                        
*             P1 - BYTE  0   X'FF' - DON'T READ CONTRACTS ON ADD                
*                  BYTES 1-3 A(CONTRACT)                                        
*             P2 - A(GETBROAD)                                                  
*             P3 -                                                              
*             P4 - A(RFBLOCK)                                                   
*                                                                               
* OUTPUT:                                                                       
*             CC EQ - DATES OK                                                  
*             CC NE - ERROR ENCOUNTERED, MESSAGE CODE IN P1                     
*                                                                               
***********************************************************************         
         DS    0H                                                               
VALTCAL  NTR1  BASE=*,LABEL=*                                                   
         ZICM  RE,1(R1),3                                                       
         LA    R0,IOAREA                                                        
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN-RCONREC(RE)                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY CONTRACT TO IOAREA                      
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         MVC   RCONKREP,REPALPHA   IN CASE ITS AN ADD                           
*                                                                               
         L     RE,ACOMFACS                                                      
         MVC   WORK(4),04(R1)                            A(GETBROAD)            
         MVC   WORK+4(4),CGETDAY-COMFACSD(RE)            A(GETDAY)              
         MVC   WORK+8(4),CADDAY-COMFACSD(RE)             A(ADDAY)               
         MVC   WORK+12(4),CDATCON-COMFACSD(RE)           A(DATCON)              
         MVC   WORK+16(4),CDATAMGR-COMFACSD(RE)          A(DATAMGR)             
         MVC   WORK+20(4),DATAMGR                        A(RECUP)               
*                                                                               
         LA    R2,1                PRESET TO 1 CONTRACT                         
         L     RE,PSAVE                                                         
         CLI   0(RE),X'FF'         CALL ON ADD?                                 
         BE    VACL007                                                          
*                                                                               
         ZICM  R2,1(RE),3          ORIGINAL CONTRACT                            
         GOTO1 =A(CMBN),DMCB,(R2),RR=Y                                          
         ZIC   R2,0(R1)            GET NUMBER OF CONTRACTS                      
         ZICM  R3,1(R1),3          POINT TO '17' ELEM(FOR COMBOS)               
         LA    R3,2(R3)            BUMP PAST CODE/LEN                           
*                                                                               
         CH    R2,=H'1'            SINGLE CONTRACT?                             
         BE    VACL007             YES                                          
*                                                                               
VACL005  DS    0H                                                               
         XC    KEY,KEY                                                          
K        USING RCONKEY,KEY                                                      
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,REPALPHA                                              
         GOTO1 =A(CONNUM),DMCB,(1,5(R3)),(2,K.RCONPCON),RR=Y                    
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MISSING CONTRACT IN COMBO                    
*                                                                               
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA,DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MISSING CONTRACT IN COMBO                    
*                                                                               
VACL007  DS    0H                                                               
         GOTO1 =A(CHKALT),DMCB,(0,(R6)),ACOMFACS,RR=Y                           
*                                                                               
         TM    0(R1),X'80'         ALTERNATE CALENDARS USED?                    
         BZ    VACL020             NO - NEXT CONTRACT                           
*                                                                               
         TM    0(R1),X'40'         REP LEVEL FORCED?                            
         BZ    *+12                NO                                           
         MVI   WORK+20,X'F1'                                                    
         B     VACL010                                                          
*                                                                               
         TM    0(R1),X'20'         STATION LEVEL FORCED?                        
         BZ    *+12                NO                                           
         MVI   WORK+20,X'F0'                                                    
         B     VACL010                                                          
*                                                                               
         MVI   WORK+20,X'F1'       ASSUME REP LEVEL                             
*                                                                               
         XC    KEY,KEY             READ TO DETERMINE WHAT ACL TO USE            
K        USING RACLKEY,KEY                                                      
         MVI   K.RACLKTYP,X'20'                                                 
         MVC   K.RACLKREP,REPALPHA                                              
         MVC   K.RACLKNAM,RCONKSTA                                              
         CLI   K.RACLKNAM+4,C'T'                                                
         BNE   *+8                                                              
         MVI   K.RACLKNAM+4,C' '                                                
         XC    K.RACLKYR,K.RACLKYR                                              
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         CLC   KEY(RACLKYR-RACLKEY),KEYSAVE                                     
         BNE   *+8                 NO STATION ACL REC                           
         MVI   WORK+20,X'F0'                                                    
*                                                                               
VACL010  DS    0H                                                               
         ICM   RF,15,WORK+12        A(DATCON)                                   
         GOTO1 (RF),DMCB,(3,RCONDATE),(0,WORK+21)                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,REPALPHA                                                    
         GOTO1 =V(GETMONTH),DMCB,(WORK+20,WORK+21),WORK+32,            +        
               WORK,(R0),RCONKSTA,RR=Y                                          
         CLI   DMCB,X'FF'                                                       
         BE    VACLERRX                                                         
*                                                                               
         ICM   RF,15,WORK+12        A(DATCON)                                   
         GOTO1 (RF),DMCB,(3,RCONDATE+3),(0,WORK+21)                             
*                                                                               
         GOTO1 =V(GETMONTH),DMCB,(WORK+20,WORK+21),WORK+32,            +        
               WORK,(R0),RCONKSTA,RR=Y                                          
         CLI   DMCB,X'FF'                                                       
         BE    VACLERRX                                                         
*                                                                               
VACL020  DS    0H                                                               
         LA    R3,9(R3)                                                         
         BCT   R2,VACL005          DECREASE COMBO COUNT                         
         B     VACLX                                                            
*                                                                               
VACLERRX DS    0H                                                               
         L     R1,PSAVE            RETURN ERROR CODE                            
         MVC   0(4,R1),=A(735)                                                  
         LTR   RB,RB                                                            
         B     *+6                                                              
VACLX    DS    0H                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CONTRACT KEYS                                                          
*                                                                               
* INPUT:                                                                        
*             P1 - A(OLD CONTRACT)                                              
*             P2 - A(NEW CONTRACT)                                              
*             P3 - DISK ADRESS                                                  
*             P4 - A(RFBLOCK)                                                   
*                                                                               
* OUTPUT:                                                                       
*             CC EQ - KEYS OK                                                   
*                                                                               
* USES IOAREA2 WHICH MUST BE AT LEAST 4096 BYTES                                
*                                                                               
***********************************************************************         
         DS    0H                                                               
CONKEYS  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         L     R6,8(R1)                                                         
*                                                                               
         LA    R0,IOAREA2          CLEAR IOAREA2                                
         LHI   R1,L'IOAREA2                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ACOMFACS                                                      
         LA    R2,CDATCON-COMFACSD(R2)                                          
         LA    R5,IOAREA2          BUILD OLD KEYS                               
         GOTO1 =V(REGENPTR),DMCB,(R3),(R5),(R2),RR=Y                            
         CLC   =Y(2048),0(R5)                                                   
         BH    *+6                                                              
         DC    H'0'                WAY TO MANY KEYS                             
*                                                                               
         LA    R5,IOAREA2                                                       
         AHI   R5,2048             BUILD NEW KEYS                               
         GOTO1 =V(REGENPTR),DMCB,(R4),(R5),(R2),RR=Y                            
         CLC   =Y(2048),0(R5)                                                   
         BH    *+6                                                              
         DC    H'0'                WAY TO MANY KEYS                             
*                                                                               
* COMPARE OLD KEYS TO NEW KEYS AND REMOVE ANY MATCHES FROM                      
* BOTH LISTS                                                                    
*                                                                               
         LA    R2,IOAREA2                                                       
         ZIC   R3,2(R2)            GET OLD KEY COUNT                            
         LA    R2,3(R2)                                                         
*                                                                               
CKEY010  DS    0H                                                               
         LA    R4,IOAREA2                                                       
         AHI   R4,2048                                                          
         ZIC   R5,2(R4)            GET NEW KEY COUNT                            
         LA    R4,3(R4)                                                         
*                                                                               
CKEY020  DS    0H                                                               
         CLC   0(L'RCONKEY,R2),0(R4)                                            
         BNE   CKEY030             NO MATCH, LEAVE KEY                          
*                                                                               
         LR    R0,R2               REMOVE OLD KEY                               
         LA    R1,IOAREA2                                                       
         AHI   R1,2048                                                          
         SR    R1,R2               GET LENGTH LEFT IN KEY BLOCK                 
         LA    RE,L'RCONKEY(R2)                                                 
         LR    RF,R1                                                            
         AHI   RF,-(L'RCONKEY)                                                  
         MVCL  R0,RE                                                            
*                                                                               
         LR    R0,R4               REMOVE NEW KEY                               
         LA    R1,IOAREA2                                                       
         AHI   R1,4096                                                          
         SR    R1,R4               GET LENGTH LEFT IN KEY BLOCK                 
         LA    RE,L'RCONKEY(R4)                                                 
         LR    RF,R1                                                            
         AHI   RF,-(L'RCONKEY)                                                  
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,IOAREA2          UPDATE OLD KEY COUNT                         
         ZIC   RF,2(R1)                                                         
         BCTR  RF,0                                                             
         STC   RF,2(R1)                                                         
*                                                                               
         LA    R1,IOAREA2          UPDATE NEW KEY COUNT                         
         AHI   R1,2048                                                          
         ZIC   RE,2(R1)                                                         
         BCTR  RE,0                                                             
         STC   RE,2(R1)                                                         
*                                                                               
         LTR   RE,RE               IF EITHER LIST IS EMPTY                      
         BZ    CKEY050               STOP COMPARING                             
         LTR   RF,RF                                                            
         BZ    CKEY050                                                          
*                                                                               
         B     CKEY040             GET NEXT OLD KEY(IN THIS SLOT)               
*                                                                               
CKEY030  DS    0H                                                               
         LA    R4,L'RCONKEY(R4)    NEXT NEW KEY                                 
         BCT   R5,CKEY020                                                       
*                                                                               
         LA    R2,L'RCONKEY(R2)    NEXT OLD KEY                                 
CKEY040  DS    0H                                                               
         BCT   R3,CKEY010                                                       
*                                                                               
CKEY050  DS    0H                                                               
*                                                                               
* DELETE ANY KEYS LEFT IN OLD POINTER LIST                                      
*                                                                               
         LA    R2,IOAREA2                                                       
         SR    R3,R3                                                            
         ICM   R3,1,2(R2)            GET OLD KEY COUNT                          
         BZ    CKEY150                                                          
         LA    R2,3(R2)                                                         
*                                                                               
CKEY100  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),0(R2)                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),REPDIR,KEY,KEY,0                     
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'        SET DELETED BIT                              
         GOTO1 DATAMGR,DMCB,DMWRT,REPDIR,KEY,KEY,0                              
*                                                                               
         LA    R2,L'RCONKEY(R2)                                                 
         BCT   R3,CKEY100                                                       
*                                                                               
CKEY150  DS    0H                                                               
*                                                                               
* ADD ANY KEYS LEFT IN NEW POINTER LIST                                         
*                                                                               
         LA    R2,IOAREA2                                                       
         AHI   R2,2048                                                          
         SR    R3,R3                                                            
         ICM   R3,1,2(R2)            GET OLD KEY COUNT                          
         BZ    CKEY250                                                          
         LA    R2,3(R2)                                                         
*                                                                               
CKEY200  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),0(R2)                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),REPDIR,KEY,KEY,0                     
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CKEY210                                                          
*                                                                               
         NI    KEY+27,X'FF'-X'80'   SET DELETED BIT                             
         STCM  R6,15,KEY+28        SET DISK ADRESS                              
         GOTO1 DATAMGR,DMCB,DMWRT,REPDIR,KEY,KEY,0                              
*                                                                               
         B     CKEY220                                                          
*                                                                               
CKEY210  DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         STCM  R6,15,KEY+28        SET DISK ADRESS                              
         GOTO1 DATAMGR,DMCB,DMADD,REPDIR,KEY,KEY,0                              
*                                                                               
CKEY220  DS    0H                                                               
         LA    R2,L'RCONKEY(R2)                                                 
         BCT   R3,CKEY200                                                       
*                                                                               
CKEY250  DS    0H                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETR - GET/VALIDATE ANY REPREC FROM KEY                                       
*                                                                               
* INPUT:                                                                        
*             P1 - BYTE  0   DMINBTS FOR READ/GETREC                            
*                  BYTES 1-3 A(27-BYTE KEY)                                     
*                                                                               
*             P2 - A(IO AREA TO RETURN RECORD) OR NULLS                         
*                  IF NULLS, REPFACS WILL CREATE AN IOAREA IN YOUR              
*                  WORKING STORAGE AND RETURN THE ADDRESS TO YOU                
*                  REPFACS CAN ONLY CREATE 1000-BYTE IO AREA MAX                
*                                                                               
*             P3 - NOT USED                                                     
*                                                                               
*             P4 - A(RFBLOCK)                                                   
*                                                                               
* OUTPUT:                                                                       
*                                                                               
*             P1 - BYTE  0   X'FF' = NOT VALID INPUT (EXCEPT NOT FOUND)         
*                                    ELSE DMOUTBTS                              
*                  BYTES 1-3 NOT USED                                           
*                                                                               
*             P2 - NULLS = NOT VALID/NOT FOUND                                  
*                  ELSE  = A(RECORD) WHEREVER SPECIFIED                         
*                                                                               
*             P3 - NULLS = NOT VALID/NOT FOUND                                  
*                  ELSE  = DISK ADDRESS OF RECORD                               
*                                                                               
*             CC - ZERO: RECORD RETURNED                                        
*                  LOW:  ERROR/NOT FOUND - NO RECORD RETURNED                   
*                                                                               
***********************************************************************         
         DS    0H                                                               
GETR     NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   0(R2),0             REC TYPE?                                    
         BE    GETR05              NO - ERROR                                   
*                                                                               
         OC    1(26,R2),1(R2)      REST OF KEY?                                 
         BNZ   GETR10              GOT SOMETHING                                
*                                                                               
GETR05   DS    0H                                                               
         MVI   0(R1),X'FF'         INV INPUT                                    
         XC    4(8,R1),4(R1)       P2/P3 NULLS                                  
         B     EXITL               RETURN                                       
*                                                                               
GETR10   DS    0H                  LOOKUP REC                                   
         XC    DMCB,DMCB                                                        
         LA    RF,DMREAD                                                        
         ST    RF,DMCB                                                          
         MVC   DMCB(1),0(R1)       DMINBTS                                      
         GOTO1 DATAMGR,DMCB,,REPDIR,0(R2),KEY,0                                 
         L     R1,PSAVE            OLD PARAMS                                   
         TM    DMCB+8,X'FF'-X'02'                                               
         BZ    GETR20                                                           
         MVC   0(1,R1),DMCB+8      DMOUTBTS                                     
         XC    4(8,R1),4(R1)       P2/P3 NULLS                                  
         B     EXITL               RETURN                                       
GETR20   DS    0H                                                               
         XC    DMCB,DMCB                                                        
         LA    RF,GETREC                                                        
         ST    RF,DMCB                                                          
         MVC   DMCB(1),0(R1)       DMINBTS                                      
         OC    4(4,R1),4(R1)       IOAREA PROVIDED?                             
         BNZ   GETR25              YES                                          
         L     R3,ANEWIO           NO - USE NEW AREA                            
         ST    R3,4(R1)            AND PASS IT BACK                             
         B     *+8                                                              
GETR25 DS      0H                                                               
         L     R3,4(R1)            IOAREA                                       
         GOTO1 DATAMGR,DMCB,,REPFIL,KEY+28,(R3),DMWORK                          
         L     R1,PSAVE            OLD PARAMS                                   
         TM    DMCB+8,X'FF'-X'02'                                               
         BZ    GETR30                                                           
         MVC   0(1,R1),DMCB+8      DMOUTBTS                                     
         XC    4(8,R1),4(R1)       P2/P3 NULLS                                  
         B     EXITL               RETURN                                       
GETR30 DS      0H                                                               
         MVC   0(1,R1),DMCB+8      DMOUTBTS                                     
         MVC   8(4,R1),KEY+28      DISK ADDR                                    
         B     EXITOK              RETURN                                       
***********************************************************************         
* GETPROF - PROGRAM PROFILES                                                    
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUPUT:   PROFILES IN PROFILE AREA                                           
*                                                                               
***********************************************************************         
GETPROF  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R3,0(R1)                                                         
         L     R2,0(R1)                                                         
         XC    0(10,R2),0(R2)                                                   
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 (RF),DMCB,GETREC,REPFIL,KEY+28,IOAREA,DMWORK                     
*                                                                               
         LA    R6,IOAREA                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
GPROF02  CLI   0(R6),0                                                          
         BE    GETPROFX            NO PROFILE ELEMENT                           
         CLI   0(R6),X'04'                                                      
         BE    GPROF04                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GPROF02                                                          
*                                                                               
GPROF04  DS    0H                                                               
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                             
         BE    GPROF20             YES                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            NOT FOUND. USE DEFAULTS.                     
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETID - READ CONTROL FILE FOR SIGNON ID                                       
*                                                                               
* INPUT:                                                                        
*             P1 - A(TWA)                                                       
*                                                                               
*             P2 - A(OUTPUT AREA FOR ID) - 10 BYTE, LEFT ALIGNED                
*                                                                               
*             P3 - NOT USED                                                     
*                                                                               
*             P4 - A(RFBLOCK)                                                   
*                                                                               
***********************************************************************         
         DS    0H                                                               
GETID    NTR1  BASE=*,LABEL=*                                                   
         L     RA,0(R1)            A(TWA)                                       
         USING TWAD,RA                                                          
*                                                                               
         LA    R6,IOAREA                                                        
         USING SAIREC,R6                                                        
         XC    SAIKEY,SAIKEY                                                    
         MVI   SAIKTYP,SAIKTYPQ                                                 
         MVC   SAIKNUM,TWAUSRID                                                 
         MVC   KEYSAVE(L'SAIKEY),SAIKEY                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,SAIKEY,SAIREC                         
         CLC   SAIKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,PSAVE                                                         
         L     R2,4(R1)                                                         
         MVC   0(10,R2),2(R6)      SIGN-ON ID NAME                              
         B     EXITOK                                                           
         DROP  RA                                                               
*                                                                               
***********************************************************************         
* GETTAB - GET ADDRESS OF REPTABS TABLE                                         
*                                                                               
* INPUT:                                                                        
*             P1 - BYTE 1: TABLE EQUATE (FROM REPTABSQ)                         
*                                                                               
* OUTPUT:                                                                       
*             P1 - A(TABLE)                                                     
*             CC EQ:  OK                                                        
*             CC NEQ: TABLE NOT FOUND                                           
*                                                                               
***********************************************************************         
GETTAB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,TABDIR                                                        
GTAB10   DS    0H                                                               
         CLI   0(R6),X'FF'                                                      
         BE    EXITL                                                            
         CLC   0(1,R6),0(R1)       MATCH TABLE EQU?                             
         BE    GTAB20                                                           
         LA    R6,5(R6)                                                         
         B     GTAB10                                                           
GTAB20   DS    0H                                                               
         ICM   R6,15,1(R6)                                                      
         A     R6,RELO                                                          
         ST    R6,0(R1)                                                         
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* >>>>>>>>>>>>>>>>  END OF SUBROUTINE LIBRARY  <<<<<<<<<<<<<<<<<<<<<< *         
***********************************************************************         
***********************************************************************         
* COMMON ROUTINES & CONSTANTS                                                   
***********************************************************************         
COMMON   DS    0H                                                               
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
*                                                                               
         GETEL  R6,34,ELCODE       REPFILE                                      
         GETEL2 R6,28,ELCODE       CTFILE                                       
*----------------------------------------------------------------------         
* GETS WORKING STORAGE FOR CURRENT MODULE                             *         
* USES R2,R3,R4,RD THIS ROUTINE CLEARS WORKING STORAGE!!!!!        *            
*----------------------------------------------------------------------         
GETSTOR  DS    0H                                                               
         ICM   R4,15,=AL4((((WORKDX-WORKD)/8)+1)*8)                             
         L     RD,4(RD)                                                         
         L     R2,8(RD)            OLD FWD PTR                                  
         AR    R2,R4               PLUS STORAGE LEN                             
         STCM  R2,15,8(RD)         NEW FWD PTR                                  
         STCM  RD,15,4(R2)         NEW BACK PTR                                 
         LR    RD,R2               NEW RD                                       
*                                                                               
         LR    R2,RC               CLEAR NEW WORKING STORAGE                    
         L     R3,=AL4(WORKDX-WORKD)                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
* GRAB IO SPACE IN CALLERS WORKING STORAGE                            *         
* USES R2,R3,R4,RD,RC THIS ROUTINE CLEARS WORKING STORAGE!!!!!        *         
*----------------------------------------------------------------------         
GETNEWIO DS    0H                                                               
         ICM   R4,15,=AL4(NEWIOLEN) STORAGE LENGTH TO ACQUIRE                   
         L     RD,4(RD)                                                         
         LR    R2,RD                                                            
         AR    R2,R4                                                            
         MVC   0(72,R2),0(RD)      POINTERS & REGS IN NEW LOCATION              
         L     RD,4(R2)                                                         
         L     R6,8(RD)                                                         
         ST    R2,8(RD)                                                         
         L     RD,8(R2)                                                         
         AR    RD,R4                                                            
         ST    R2,4(RD)                                                         
         ST    RD,8(R2)                                                         
         AR    RC,R4               NEW WORKING STORAGE AREA                     
*                                                                               
         LR    R2,R6               CLEAR NEW IOAREA                             
         LR    R3,R4                                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         LR    R2,RC               CLEAR NEW WORKING STORAGE                    
         L     R3,=AL4(WORKDX-WORKD)                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
* REMOVE NEW IO AREA FROM CALLERS WORKING STORAGE                     *         
* USES R2,R3,R4,RD EXTRA SPACE IS PUT AT FRONT OF REPFACS STORAGE     *         
*----------------------------------------------------------------------         
REMNEWIO DS    0H                                                               
         ICM   R4,15,=AL4(NEWIOLEN) STORAGE LENGTH TO REMOVE                    
         L     RD,4(RD)                                                         
         LR    R2,RD                                                            
         SR    R2,R4                                                            
         MVC   0(72,R2),0(RD)      POINTERS & REGS IN NEW LOCATION              
         L     RD,4(R2)                                                         
         ST    R2,8(RD)                                                         
         L     RD,8(R2)                                                         
         SR    RD,R4                                                            
         ST    R2,4(RD)                                                         
         ST    RD,8(R2)                                                         
         SR    RC,R4               NEW WORKING STORAGE AREA                     
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
NEWIOLEN EQU   1000                                                             
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
DMREAD   DC    C'DMREAD'          COMMANDS                                      
DMRDHI   DC    C'DMRDHI'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
DMADD    DC    C'DMADD'                                                         
DMWRT    DC    C'DMWRT'                                                         
GETREC   DC    C'GETREC'                                                        
PUTREC   DC    C'PUTREC'                                                        
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
REPFIL   DC    C'REPFIL'         FILES                                          
REPDIR   DC    C'REPDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
         LTORG                                                                  
*                                                                               
       ++INCLUDE REPTABS                                                        
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
PSAVE    DS    F                   SAVE A(INCOMING PARAM LIST)                  
FOO      DS    F                   INTERNAL PSAVE FOR CHKALT                    
RELO     DS    F                                                                
ANEWIO   DS    A                   IOAREA CREATED IN CALLERS STORAGE            
ACOMFACS DS    A                                                                
DATAMGR  DS    A                                                                
DATCON   DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
ELCODE   DS    C                                                                
BYTE     DS    X                                                                
*                                                                               
FLAGS    DS    X                   PROGRAM STATUS FLAGS                         
FNEWIO   EQU   X'80'               CREATED NEW IO AREA IN CALLER                
*                                                                               
REPALPHA DS    CL2                                                              
WORK     DS    CL200                                                            
KEY      DS    CL32                KEY                                          
KEYSAVE  DS    CL32                KEY SAVED BEFORE READ HIGH                   
IOAREA2  DS    XL1000                                                           
IOAREA   DS    CL4096              4096 USED BY CONKEYS                         
*                                                                               
WORKDX   EQU   *                                                                
*                                                                               
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENACL                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENOWN                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111REPFACST  01/15/02'                                      
         END                                                                    
