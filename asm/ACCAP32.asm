*          DATA SET ACCAP32    AT LEVEL 023 AS OF 12/11/09                      
*PHASE T61D32A                                                                  
* EMOU 019 18AUG00 PASS LIMIT ACCESS OFFICE TO BMONVAL                          
* NSHE 024 09JUL04 MERGE US AND UK VERSIONS                                     
* NSHE 025 17AUG04 DAILY TIME CHANGES                                           
* YNGX 026 09AUG07 <BR11129D> CHECK IF IT'S A DRAFT JOB                         
         TITLE 'TIMESHEET ROUTINES - #2'                                        
T61D32   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
ROUT     NMOD1 250,**ROU2**,R7,CLEAR=YES                                        
         USING TIMEGWSD,R9         R9=A(GLOBAL WORKING STORAGE)                 
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(GEND)                                   
         L     R8,ASYSD            R8=A(SYSD)                                   
         USING SYSD,R8                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     VALHRS               1 - VALIDATE HOURS                          
         B     VALPRSN              2 - VALIDATE PERSON                         
         B     VALCPJ               3 - VALIDATE CLIENT/PRODUCT/JOB             
         B     XAIO                 4 - CLEAR IO AREA                           
         B     XFIELD               5 - CLEAR TWA FIELD                         
         B     FVAL                 6 - PROCESS A FIELD                         
         B     MTHLOCK              7 - CHECK FOR MONTH LOCK                    
         B     LOCLIST              8 - BUILD LOCATION LIST                     
         B     VALOPTS              9 - VALIDATE OPTIONS LINE                   
         B     ADJDATE             10 - ADJUST PERIOD DATE                      
         B     VALLOC              11 - VALIDATE LOCALITY                       
         B     VALTXWC             12 - VALIDATE TAX WORKCODE                   
         B     XMIT                13 - TRANSMIT SCREEN FIELDS                  
         B     FORMAT              14 - FORMAT OUTPUT LINE                      
*&&UK*&& B     GETDTES             15 - GET DATES OF DAILY TIME                 
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1                      EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS                                                      *         
*                                                                     *         
* NTRY - R1=A(HOURS FIELD HEADER)                                     *         
*                                                                     *         
* EXIT - CC=EQU - BCHOURS CONTAINS # OF HOURS 2DP                     *         
*      - CC=NEQ - INVALID INPUT & GERROR IS SET CORRECTLY             *         
***********************************************************************         
         SPACE 1                                                                
VALHRS   DS    0H                                                               
         LR    R2,R1                                                            
         MVI   BCIFMIN,1           REQUIRED FIELD                               
         MVI   BCIFMAX,7           MAX OF 999.99 HOURS                          
         GOTO1 AFVAL,(R2)                                                       
         BH    ROUTH                                                            
*                                                                               
         ZAP   BCHOURS,=P'0'                                                    
         LA    R0,L'BCIFLD                                                      
         LA    RF,BCIFLD+L'BCIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   VALH05                                                           
         GOTO1 CASHVAL,DMCB,(X'82',BCIFLD),(X'01',(R0))                         
         B     VALH10                                                           
*                                                                               
VALH05   GOTO1 CASHVAL,DMCB,(X'82',BCIFLD),(R0)                                 
VALH10   CLI   DMCB,X'FF'                                                       
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ROUTH                                                            
*                                                                               
         ZAP   BCHOURS,DMCB+8(4)                                                
         ZAP   BCDUB,BCHOURS       CAN ONLY HAVE QUARTER HOURS                  
         DP    BCDUB,=P'25'                                                     
         CP    BCDUB+6(2),=P'0'                                                 
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEQTRHR)                                            
         B     ROUTH                                                            
*                                                                               
         CP    BCHOURS,=P'30000'   MAX # HOURS = 300/LINE                       
         BH    *+14                                                             
         CP    BCHOURS,=P'-30000'  MIN # HOURS = -300/LINE                      
         BNL   *+14                                                             
         MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ROUTH                                                            
*                                                                               
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PERSON                                                   *         
*                                                                     *         
* NTRY - R1=A(PERSON FIELD HEADER)                                    *         
*                                                                     *         
* EXIT - CC=EQU - BCWORK   = 'LASTNAME, FIRSTNAME'                    *         
*               - BCPRNFST = 'FIRSTNAME'                              *         
*               - BCPRNLST = 'LASTNAME'                               *         
*                                                                     *         
*      - CC=NEQ - INVALID PERSON CODE                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPRSN  DS    0H                                                               
         XC    BCPIDNO,BCPIDNO     CLEAR PERSON ID #                            
*                                                                               
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING PERRECD,R6                                                       
         MVC   PERKEY,BCSPACES                                                  
         MVI   PERKTYP,PERKTYPQ    X'0F' PERSON RECORD                          
         MVC   PERKCPY,CMPY                                                     
         SR    RF,RF                                                            
         IC    RF,5(R1)                                                         
         SH    RF,=H'1'                                                         
         BM    ROUTH               INVALID HEADER LENGTH                        
         MVC   PERKCODE(0),8(R1)                                                
         EX    RF,*-6                                                           
         GOTO1 AGETACT,0                                                        
         BNE   ROUTH                                                            
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(L'BCPRNLST),BCPRNLST     LAST NAME                        
         SR    R1,R1                                                            
         IC    R1,BCPRNLLN                                                      
         LA    R1,BCWORK(R1)                                                    
         MVI   0(R1),C','                                                       
         MVC   2(L'BCPRNFST,R1),BCPRNFST       FIRST NAME                       
*                                                                               
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
         SR    R1,R1                                                            
VALPRS10 IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    VALPRSNX                                                         
         USING PIDELD,R6                                                        
         CLI   0(R6),PIDELQ        X'D8' PERSON ID ELEMENT                      
         BNE   VALPRS10                                                         
         MVC   BCPIDNO,PIDNO       PERSON ID NUMBER                             
*                                                                               
VALPRSNX B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PRODUCTION ACCOUNT                                       *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER) OF CLIENT OR PROD OR JOB FIELD            *         
*      - BCFLAG1 INDICATES WHAT TYPE OF RECORD TO VALIDATE            *         
*        - BCFL1CLI = VALIDATE CLIENT FIELD                           *         
*        - BCFL1PRD = VALIDATE PRODUCT FIELD                          *         
*        - BCFL1JOB = VALIDATE JOB FIELD                              *         
*                                                                     *         
* EXIT - CC=EQU - ALL IS OK                                           *         
*      - CC=NEQ - INVALID WITH GERROR SET                             *         
***********************************************************************         
         SPACE 1                                                                
VALCPJ   DS    0H                                                               
         LR    R2,R1                                                            
         GOTO1 AFVAL,(R2)                                                       
*                                                                               
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6          BUILD KEY                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'SJ'                                                
         LA    R1,BCIFLD                                                        
*                                                                               
         TM    BCFLAG1,BCFL1CLI    TEST VALIDATE CLIENT                         
         BZ    VALCPJ02                                                         
         MVC   BCWORK,BCSPACES                                                  
         SR    RF,RF                                                            
         IC    RF,BCSJLEV1                                                      
         SH    RF,=H'1'                                                         
         MVC   BCWORK(0),0(R1)                                                  
         EX    RF,*-6                                                           
         EXCLC RF,BCWORK,BCSPACES                                               
         BE    VALCPJEC            ERROR - CLIENT NOT GIVEN                     
         MVC   ACTKACT,BCWORK                                                   
         GOTO1 AGETACT,0                                                        
         BNE   VALCPJEC                                                         
         B     VALCPJX                                                          
*                                                                               
VALCPJ02 TM    BCFLAG1,BCFL1PRD    TEST VALIDATE PRODUCT                        
         BZ    VALCPJ04                                                         
         LA    RE,BCWORK                                                        
         SR    R0,R0                                                            
         IC    R0,BCSJLNQ1                                                      
         AR    RE,R0                                                            
         SR    RF,RF                                                            
         IC    RF,BCSJLEV2                                                      
         SH    RF,=H'1'                                                         
         MVC   0(0,RE),0(R1)                                                    
         EX    RF,*-6                                                           
         EXCLC RF,0(RE),BCSPACES                                                
         BE    VALCPJEP            ERROR - PRODUCT NOT GIVEN                    
         MVC   ACTKACT,BCWORK                                                   
         GOTO1 AGETACT,0                                                        
         BNE   VALCPJEP                                                         
         B     VALCPJX                                                          
*                                                                               
VALCPJ04 TM    BCFLAG1,BCFL1JOB    TEST VALIDATE JOB                            
         BZ    VALCPJX                                                          
         LA    RE,BCWORK                                                        
         SR    R0,R0                                                            
         IC    R0,BCSJLNQ2                                                      
         AR    RE,R0                                                            
         SR    RF,RF                                                            
         IC    RF,BCSJLEV3                                                      
         SH    RF,=H'1'                                                         
         MVC   0(0,RE),0(R1)                                                    
         EX    RF,*-6                                                           
         EXCLC RF,0(RE),BCSPACES                                                
         BE    VALCPJEJ            ERROR - JOB NOT GIVEN                        
         MVC   ACTKACT,BCWORK                                                   
         GOTO1 AGETACT,0                                                        
         BNE   VALCPJEJ                                                         
         TM    ACTKSTAT,ACTSDRFT   DRAFT ACCOUNT?                               
*&&UK*&& BNZ   VALCPJEJ                                                         
*&&US*&& BNZ   VALCPJEU                                                         
         B     VALCPJX                                                          
*                                                                               
VALCPJX  TM    BCFLAG1,BCFL1CLI+BCFL1PRD+BCFL1JOB                               
         BNZ   *+6                                                              
         DC    H'0'                FLAG WAS NOT SET ON CALL                     
         MVI   BCFLAG1,0                                                        
         B     ROUTE                                                            
*                                                                               
VALCPJEC MVC   GERROR,=AL2(ACECLI)         INVALID CLIENT                       
         B     ROUTH                                                            
VALCPJEP MVC   GERROR,=AL2(ACEPROD)        INVALID PRODUCT                      
         B     ROUTH                                                            
VALCPJEJ MVC   GERROR,=AL2(ACEJOB)         INVALID JOB                          
         B     ROUTH                                                            
VALCPJEU MVC   GERROR,=AL2(ACEJUAPP)       JOB IS UNAPPROVED                    
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR I/O AREA                                                      *         
*                                                                     *         
* NTRY - R1 = A(IO AREA TO CLEAR) OR IF R1=0 DEFAULT TO 'AIO'         *         
***********************************************************************         
         SPACE 1                                                                
XAIO     DS    0H                                                               
         L     RE,0(R1)            A(AIO AREA)                                  
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         L     RE,AIO              CLEAR AREA POINTED TO BY AIO                 
         LA    RF,2000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN FIELD - CLEARS ALL FIELDS BETWEEN PARM1 & PARM2        *         
*                                                                     *         
* NTRY - PARM1 = A(START FIELD)                                       *         
*        PARM2 = A(END FIELD)                                         *         
*                                                                     *         
* EXIT - CC=EQU - OK                                                  *         
*        CC=NEQ - IF (END FIELD < START FIELD)                        *         
***********************************************************************         
         SPACE 1                                                                
XFIELD   DS    0H                                                               
         L     R2,0(R1)            R2=A(START FIELD)                            
         L     R4,4(R1)            R4=A(END FIELD)                              
         CR    R4,R2                                                            
         BL    ROUTH               ERROR IF END < START                         
*                                                                               
XFIELD10 TWAXC 0(R2),0(R2),PROT=Y                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         CLI   1(R2),X'FF'         SKIP NOP FIELDS                              
         BE    *+12                                                             
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         NI    4(R2),X'FF'-X'20'   NOT VALIDATED                                
         SR    R1,R1                                                            
         IC    R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CR    R2,R4                                                            
         BNH   XFIELD10                                                         
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
FVAL     DS    0H                                                               
         XC    BCIFLDH,BCIFLDH                                                  
         MVC   BCIFLD,BCSPACES                                                  
         CLI   BCIFMIN,0                                                        
         BE    FVAL100                                                          
         CLI   5(R1),0                                                          
         BH    *+14                ERROR IF REQUIRED BUT NOT INPUT              
         MVC   GERROR,=AL2(ACEMISS)                                             
         B     ROUTH                                                            
*                                                                               
FVAL100  DS    0H                                                               
         CLC   BCIFMIN,5(R1)                                                    
         BNH   *+14                                                             
         MVC   GERROR,=AL2(ACELSHO)                                             
         B     ROUTH                                                            
         CLC   BCIFMAX,5(R1)                                                    
         BNL   *+14                                                             
         MVC   GERROR,=AL2(ACELLONG)                                            
         B     ROUTH                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R1)          L'INPUT FIELD                                
         BZ    ROUTL               NOTHING ENTERED NOTHING REQUIRED             
         LA    RF,L'BCIFLDH(RF)   +L'HEADER (8 BYTES)                           
         SH    RF,=H'1'           -1 FOR EXMVC                                  
         BM    ROUTL               NOTHING ENTERED NOTHING REQUIRED             
         MVC   BCIFLDH(0),0(R1)                                                 
         EX    RF,*-6                                                           
*        OC    BCIFLD,BCSPACES     FACPAK CONVERTS TO UPPER CASE                
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR MONTH LOCK                                                *         
***********************************************************************         
         SPACE 1                                                                
MTHLOCK  DS    0H                                                               
         L     R3,0(R1)                                                         
         MVC   BCYYMMDD(2),0(R3)   PERIOD MONTH                                 
         L     R3,8(R1)                                                         
         MVC   BCOFFC,0(R3)        LIMIT ACCESS OFFICE                          
         L     R2,4(R1)            FUTURE PARM.                                 
MTHL10   MVI   BCYYMMDD+2,X'01'                                                 
         GOTO1 DATCON,DMCB,(X'81',BCYYMMDD),(6,BCDUB)                           
         L     R4,4(R1)            ACTUAL LENGTH OF OUTPUT FIELD                
         STCM  R4,8,BYTE           STORE FOR BMONVAL CALL                       
         GOTO1 VBMONVAL,DMCB,(BYTE,BCDUB),(49,ACOMFACS),(LANGCODE,BCWORX        
               K),(CMPY,BCOFFC)                                                 
         USING BMONVALD,R3                                                      
         LA    R3,BCWORK                                                        
         CLI   BMOERR,BMOEOKQ                                                   
         BE    ROUTE                                                            
*                                                                               
         MVC   GERROR,BMOMSG       DISPLAY ERROR                                
         MVI   BCYYMMDD+2,X'15'                                                 
         GOTO1 DATCON,DMCB,(1,BCYYMMDD),(0,BCWORK)                              
         GOTO1 ADDAY,DMCB,BCWORK,BCWORK+6,31                                    
         GOTO1 DATCON,DMCB,(0,BCWORK+6),(1,BCYYMMDD)                            
         CLC   BCYYMMDD(2),0(R2)   COMPARE IF FUTURE PARM EXCEEDED              
         BL    MTHL10                                                           
         MVC   BCOFFC,BCSPACES                                                  
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATE LOCATIONS IN ONE PERIOD                         *         
*                                                                     *         
* NTRY - PARM1 = A(PERSON RECORD)                                     *         
*        PARM2 = PERIOD START DATE                                    *         
*        PARM3 = PERIOD END DATE                                      *         
*                                                                     *         
* EXIT - CC=EQU - MULTIPLE LOCATIONS FOUND & BCWORK HAS LIST          *         
*      - CC=HI  - NO LOCATION LIST BUILT                              *         
*      - CC=LO  - MULTIPLE LOCATIONS BUT INCORRECT LOCATION END DATE  *         
***********************************************************************         
         SPACE 1                                                                
LOCLIST  DS    0H                                                               
         ICM   R6,15,0(R1)         R6=A(PERSON RECORD)                          
         BZ    ROUTH                                                            
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
         ICM   RF,15,4(R1)         PERIOD START DATE                            
         BZ    ROUTH                                                            
         MVC   BCSTDTE,0(RF)                                                    
         ICM   RF,15,8(R1)         PERIOD END DATE                              
         BZ    ROUTH                                                            
         MVC   BCENDTE,0(RF)                                                    
         ICM   RF,15,12(R1)        FIELD INPUT DATE                             
         BZ    ROUTH                                                            
         MVC   BCINPDTE,0(RF)                                                   
         MVI   BCFLAG3,0           TEMPORARY STATUS FLAG                        
         GOTO1 AXAIO,AIOBC         CLEAR OUTPUT BLOCK                           
         L     R5,AIOBC            R5=A(OUTPUT LINE)                            
         SR    R3,R3               R3=COUNTER                                   
         MVC   SV1RODS,BCSPACES                                                 
*                                                                               
         USING LOCELD,R6                                                        
LOCLIS10 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    LOCLISX                                                          
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   LOCLIS10                                                         
         CLC   SV1ROFF,LOCOFF      SAVE OFFICE AS BEFORE?                       
         BNE   LOCLIS20                                                         
         CLC   SV1RDPT,LOCDEPT     SAME DEPARTMENT AS BEFORE?                   
         BNE   LOCLIS20                                                         
         CLC   SV1RSUB,LOCSUB      SAME SUB-DEPT AS BEFORE?                     
         BE    LOCLIS10            SAME O/D/S AS BEFORE SKIP                    
*                                                                               
LOCLIS20 CLC   LOCSTART,BCENDTE    IF LOCATION START > PERIOD END               
         BH    LOCLIS10            THEN NOT IN LOCATION @ THIS TIME             
         OC    LOCEND,LOCEND       STILL IN THIS LOCATION IF ZERO               
         BZ    *+14                                                             
         CLC   LOCEND,BCSTDTE      IF LOCATION END < PERIOD START               
         BL    LOCLIS10            THEN NOT IN LOCATION @ THIS TIME             
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'('                                                      
         MVC   BCWORK+1(L'LOCOFF),LOCOFF       OFFICE CODE                      
         MVC   BCWORK+4(L'LOCDEPT),LOCDEPT     DEPARTMENT CODE                  
         MVC   BCWORK+12(L'LOCSUB),LOCSUB      SUBDEPARTMENT CODE               
         MVI   BCWORK+19,C'='                                                   
         MVC   BCYYMMDD,LOCEND                                                  
         OC    LOCEND,LOCEND       STILL IN THIS LOCATION IF ZERO               
         BZ    *+14                                                             
         CLC   LOCEND,BCENDTE                                                   
         BNH   *+10                                                             
         MVC   BCYYMMDD,BCENDTE                                                 
*                                                                               
         DS    0H                                                               
         CLC   BCINPDTE,BCYYMMDD                                                
         BNE   *+8                                                              
         OI    BCFLAG3,BCFL3OK                                                  
         GOTO1 DATCON,BCDMCB,(1,BCYYMMDD),(17,BCWORK+21)                        
         MVI   BCWORK+29,C')'                                                   
*                                                                               
         GOTO1 SQUASHER,BCDMCB,BCWORK,L'BCWORK                                  
         SR    RF,RF                                                            
         ICM   RF,15,BCDMCB+4                                                   
         SH    RF,=H'1'                                                         
         BM    LOCLIS10                                                         
         MVC   0(0,R5),BCWORK                                                   
         EX    RF,*-6                                                           
         LA    R5,1(RF,R5)                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   SV1ROFF,LOCOFF      SAVE OFF LOCATION OFFICE                     
         MVC   SV1RDPT,LOCDEPT     SAVE OFF LOCATION DEPARTMENT                 
         MVC   SV1RSUB,LOCSUB      SAVE OFF LOCATION SUB-DEPART                 
         B     LOCLIS10                                                         
*                                                                               
LOCLISX  DS    0H                                                               
         L     R5,AIOBC            R5=A(OUTPUT LINE)                            
         MVC   BCWORK,BCSPACES                                                  
         CH    R3,=H'1'                                                         
         BNH   ROUTH                                                            
         MVC   BCWORK,0(R5)                                                     
         OC    BCWORK,BCSPACES                                                  
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS LINE                                               *         
*                                                                     *         
* NTRY - R1=A(OPTIONS FIELD HEADER)                                   *         
*                                                                     *         
* EXIT - CC=EQU - BCOPTS SET WITH OPTIONS                             *         
*      - CC=NEQ - INVALID INPUT & GERROR IS SET CORRECTLY             *         
***********************************************************************         
         SPACE 1                                                                
VALOPTS  DS    0H                                                               
         LR    R2,R1                                                            
         MVI   BCIFMAX,L'BCIFLD                                                 
         GOTO1 AFVAL,(R2)                                                       
         BNE   ROUTE                                                            
*                                                                               
         LA    R3,VALOPTTB         OPTIONS TABLE                                
         CLI   0(R3),X'40'                                                      
         BNL   VALOPT5                                                          
VALOPT2  GOTO1 DICTATE,BCDMCB,C'SU  ',(R3),0                                    
         LA    R3,L'VALOPTTB(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   VALOPT2                                                          
*                                                                               
VALOPT5  L     RE,AIOBC            CLEAR SCANNER AREA                           
         LA    RF,L'BCIO                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R4,AIOBC                                                         
         GOTO1 SCANNER,BCDMCB,(20,BCIFLDH),(8,(R4))                             
         CLI   BCDMCB+4,0                                                       
         BE    VALOPTE                                                          
*                                                                               
         USING SCANBLKD,R4                                                      
         L     R4,AIOBC                                                         
VALOPT10 OC    SCLINE,SCLINE                                                    
         BZ    VALOPTX                                                          
*                                                                               
         LA    R3,VALOPTTB         OPTIONS TABLE                                
VALOPT20 CLI   0(R3),X'FF'                                                      
         BE    VALOPTE                                                          
         SR    R1,R1                                                            
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALOPT90                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),0(R3)                                                
         BE    VALOPT30                                                         
         LA    R3,L'VALOPTTB(R3)                                                
         B     VALOPT20                                                         
*                                                                               
VALOPT30 SR    RF,RF                                                            
         IC    RF,10(R3)                                                        
         LA    RE,VALOPT90         SET RETURN ADDRESS                           
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALOCLI             CLIENT FILTER                                
         B     VALOPJ              PRODUCT/JOB FILTER                           
         B     VALOMOA             MOA FILTER                                   
         B     VALOTSK             TASK FILTER                                  
         B     VALOSRT             SORT OPTION                                  
         B     VALOTYPE            TYPE FILTER                                  
         B     VALOXD              EXTRA DETAIL (XD=Y)                          
         B     VALOCA              CONTRA ACCOUNT                               
*&&UK*&& B     VALOALL             ALL (FOR DAILY TIME)                         
VALOPT90 BH    VALOPTE                                                          
VALOPT95 LA    R4,L'SCLINE+10(R4)                                               
         B     VALOPT10                                                         
*                                                                               
VALOPTX  B     ROUTE                                                            
*                                                                               
VALOPTE  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ROUTH                                                            
*                                                                               
VALOPTE2 MVC   GERROR,=AL2(ACELSHO)    INPUT TOO SHORT                          
         B     ROUTH                                                            
*                                                                               
* VALIDATE CLIENT/PRODUCT/JOB OPTION                                            
*                                                                               
VALOCLI  SR    RF,RF               FORCE USER TO TYPE 2 LETTERS                 
         IC    RF,SC1STLEN         LENGTH OF 1ST FIELD                          
         CHI   RF,1                DO NOT ALLOW C= FOR CLI/CONTRA               
         BH    VALOPJ              CONTINUE TO CHECK PROD/JOB                   
         MVC   GERROR,=AL2(ACELSHO)                                             
         B     ROUTH                                                            
*                                                                               
VALOPJ   MVC   BCFLTCLI(L'BCFLTCLI+L'BCFLTPRO+L'BCFLTJOB),BCSPACES              
         LA    R2,SC2NDFLD                                                      
         SR    RF,RF               WHAT LEVEL DID THEY ENTERED UP TO            
         ICM   RF,1,SC2NDLEN       LENGTH OF 2ND FIELD                          
         BZ    VALOPTE                                                          
*                                                                               
         SR    R1,R1               CLIENT CODE SHOULD ALWAYS BE ENTERED         
         IC    R1,BCSJLEV1         LENGTH OF 1ST LEVEL                          
         SR    RF,R1               DECREASE SCAN LEN BY 1ST LEV LENGTH          
         BNM   VALOC10                                                          
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOC10  BCTR  R1,0                                                             
         MVC   BCFLTCLI(0),0(R2)                                                
         EX    R1,*-6                                                           
         CHI   RF,1                DO WE STILL HAVE ANYTHING?                   
         BL    VALOPT95                                                         
         LA    R2,1(R1,R2)                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BCSJLEV2         LENGTH OF 2ND LEVEL                          
         SR    RF,R1               DECREASE SCAN LEN BY 2ND LEV LENGTH          
         BNM   VALOC20                                                          
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOC20  BCTR  R1,0                                                             
         MVC   BCFLTPRO(0),0(R2)                                                
         EX    R1,*-6                                                           
         CHI   RF,1                DO WE STILL HAVE ANYTHING?                   
         BL    VALOPT95                                                         
         LA    R2,1(R1,R2)                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BCSJLEV3         LENGTH OF 3RD LEVEL                          
         SR    RF,R1               DECREASE SCAN LEN BY LST LEV LENGTH          
         BNM   VALOC30                                                          
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOC30  BCTR  R1,0                                                             
         MVC   BCFLTJOB(0),0(R2)                                                
         EX    R1,*-6                                                           
         B     VALOPT95                                                         
*                                                                               
* VALIDATE TASK OPTION                                                          
*                                                                               
VALOTSK  SR    RF,RF               FORCE USER TO TYPE 2 LETTERS                 
         IC    RF,SC1STLEN         LENGTH OF 1ST FIELD                          
         CHI   RF,1                DO NOT ALLOW T= FOR TASK                     
         BNH   VALOPTE2            CONTINUE TO CHECK PROD/JOB                   
*                                                                               
         SR    RF,RF               MAKE SURE THAT W/C IS 2 BYTES-NOT 1          
         IC    RF,SC2NDLEN         LENGTH OF 2ND FIELD                          
         CHI   RF,2                                                             
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACETSKCD)                                            
         B     ROUTH                                                            
         MVC   BCFLTTSK,SC2NDFLD                                                
         B     VALOPT95                                                         
*                                                                               
* VALIDATE MOA OPTION                                                           
*                                                                               
VALOMOA  NTR1                                                                   
         XC    BCFSTMOA,BCFSTMOA                                                
         XC    BCFENMOA,BCFENMOA                                                
         LA    R2,SC2NDFLD         MOA DATE ENTERED                             
         SR    R3,R3                                                            
         IC    R3,SC2NDLEN         LENGTH OF ENTERED FIELD                      
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'20'                                                       
         GOTO1 PERVAL,DMCB,((R3),(R2)),(BYTE,BLOCK)                             
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID                               
         BE    ROUTH                                                            
         CLI   DMCB+4,PVRCONE      ONLY ENTERED IN ONE DATE                     
         BE    *+12                                                             
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID                               
         BE    ROUTH                                                            
*                                                                               
         LR    RE,R2                                                            
         CLI   0(RE),C'-'          ARE WE DOING EVERYTHING BEFORE?              
         BE    VALOM10                                                          
         MVC   BCFSTMOA,PVALPSTA                                                
         IC    R3,SC2NDLEN         LENGTH OF ENTERED FIELD                      
         AR    RE,R3                                                            
         BCTR  RE,0                                                             
         CLI   0(RE),C'-'          ARE WE DOING EVERYTHING AFTER?               
         BNE   VALOM10                                                          
         MVC   BCFENMOA,=X'FFFF'   END DATE IS INFINITY                         
         B     *+10                                                             
VALOM10  MVC   BCFENMOA,PVALPEND                                                
         B     ROUTE                                                            
         DROP  R1                                                               
*                                                                               
* VALIDATE SORT OPTION                                                          
*                                                                               
VALOSRT  MVI   BCOPT1,C'Y'                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
* VALIDATE TYPE OPTION                                                          
*                                                                               
VALOTYPE SR    RF,RF               FORCE USER TO TYPE 2 LETTERS                 
         IC    RF,SC1STLEN         LENGTH OF 1ST FIELD                          
         CHI   RF,1                DO NOT ALLOW T= FOR TYPE                     
         BNH   VALOPTE                                                          
*                                                                               
         SR    RF,RF               MAKE SURE THAT USER ONLY                     
         IC    RF,SC2NDLEN         INPUTTED ONE BYTE                            
         CHI   RF,1                DO NOT ALLOW TYPE=BT OR BA                   
         BNH   *+14                                                             
         MVC   GERROR,=AL2(ACELLONG)                                            
         B     ROUTH                                                            
*                                                                               
         CLI   SC2NDFLD,C'B'                                                    
         BNE   *+12                                                             
         MVI   BCFLTTYP,TIMTCB                                                  
         B     VALOPT95                                                         
         CLI   SC2NDFLD,C'R'                                                    
         BNE   *+12                                                             
         MVI   BCFLTTYP,TIMTCR                                                  
         B     VALOPT95                                                         
         CLI   SC2NDFLD,C'N'                                                    
         BNE   VALOPTE                                                          
         MVI   BCFLTTYP,TIMTCN                                                  
         B     VALOPT95                                                         
*                                                                               
* VALIDATE EXTRA DETAILS OPTION                                                 
*                                                                               
VALOXD   NTR1                                                                   
         CLC   SC2NDFLD(1),AC@YES                                               
         BNE   ROUTH                                                            
         MVI   BCOPT2,C'Y'                                                      
         B     ROUTE                                                            
*                                                                               
* VALIDATE CONTRA ACCOUNT OPTION                                                
*                                                                               
VALOCA   SR    RF,RF               FORCE USER TO TYPE 2 LETTERS                 
         IC    RF,SC1STLEN         LENGTH OF 1ST FIELD                          
         CHI   RF,1                DO NOT ALLOW C= FOR CLI/CONTRA               
         BNH   VALOPTE                                                          
*                                                                               
         CLC   =C'1C',SC2NDFLD     MAKE SURE THAT THEY ENTERED U/L              
         BE    VALOCA10                                                         
         CLC   =C'1N',SC2NDFLD     NO NEED CHECKING LEVELS ON 1N                
         BE    VALOCA50                                                         
         MVC   GERROR,=AL2(ACEIVUL)    INVALID U/L                              
         B     ROUTH                                                            
*                                                                               
VALOCA10 LA    R2,SC2NDFLD                                                      
         SR    RF,RF               WHAT LEVEL DID THEY ENTERED UP TO            
         ICM   RF,1,SC2NDLEN       LENGTH OF 2ND FIELD                          
         BZ    VALOPTE                                                          
         SHI   RF,2                                                             
         BNP   VALOPTE             MUST HAVE MORE THAN U/L                      
         LA    R2,2(R2)            BUMP PAST U/L                                
*                                                                               
         SR    R1,R1               CLIENT CODE SHOULD ALWAYS BE ENTERED         
         IC    R1,BC1CLEV1         LENGTH OF 1ST LEVEL                          
         SR    RF,R1               DECREASE SCAN LEN BY 1ST LEV LENGTH          
         BNM   VALOCA20                                                         
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOCA20 CHI   RF,1                DO WE STILL HAVE ANYTHING?                   
         BL    VALOCA50                                                         
         AR    R2,R1                                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,BC1CLEV2       LENGTH OF 2ND LEVEL                          
         BZ    VALOCA50                                                         
         SR    RF,R1               DECREASE SCAN LEN BY 2ND LEV LENGTH          
         BNM   VALOCA30                                                         
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOCA30 CHI   RF,1                DO WE STILL HAVE ANYTHING?                   
         BL    VALOCA50                                                         
         AR    R2,R1                                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,BC1CLEV3       LENGTH OF 3RD LEVEL                          
         BZ    VALOCA50                                                         
         SR    RF,R1               DECREASE SCAN LEN BY LST LEV LENGTH          
         BNM   VALOCA40                                                         
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOCA40 CHI   RF,1                DO WE STILL HAVE ANYTHING?                   
         BL    VALOCA50                                                         
         AR    R2,R1                                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,BC1CLEV4       LENGTH OF 4TH LEVEL                          
         BZ    VALOCA50                                                         
         SR    RF,R1               DECREASE SCAN LEN BY LST LEV LENGTH          
         BNM   VALOCA50                                                         
         LR    R0,R1               MAKE SURE THERE AREN'T ANY '?'               
         LR    RE,R2               RE=START OF ACCOUNT CODE                     
         CLI   0(RE),C'?'          IF '?'-IT MUST BE FULL LEVEL                 
         BE    VALOPTE2                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALOCA50 MVC   BCFLTCA,SC2NDFLD                                                 
         B     VALOPT95                                                         
*                                                                               
* VALIDATE ALL OPTION                                                           
*                                                                               
*&&UK                                                                           
VALOALL  CLI   ACTEQU,ACTADD                                                    
         BE    VALOAL02                                                         
         CLI   ACTEQU,ACTCHA                                                    
         BE    VALOAL02                                                         
         CLI   ACTEQU,ACTLIST                                                   
         BE    VALOAL02                                                         
         CLI   ACTEQU,ACTDEL                                                    
         BE    VALOAL02                                                         
         CLI   ACTEQU,ACTSEL       ACTION SELECT                                
         BNE   VALOAL04                                                         
         CLC   BCTHISEL,AC@CHAU    IF CHANGE - ISSUE ERROR                      
         BNE   VALOAL04            MUST BE DISPLAY                              
VALOAL02 MVC   GERROR,=AL2(ACEOPIWA)   INVALID OPTION WITH THIS ACTION          
         B     ROUTH                                                            
VALOAL04 MVI   BCOPT3,C'Y'                                                      
         B     VALOPT95                                                         
*&&                                                                             
*                                                                               
VALOPTTB DS    0CL11                                                            
         DCDD  AC#CLIC,6           CLIENT                                       
         DC    CL(10-6)' '                                                      
         DC    AL1(OPT#CLI)                                                     
*                                                                               
         DCDD  AC#PROC,7           PRODUCT                                      
         DC    CL(10-7)' '                                                      
         DC    AL1(OPT#PJ)                                                      
*                                                                               
         DCDD  AC#JOBC,3           JOB                                          
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#PJ)                                                      
*                                                                               
         DCDD  AC#MOA,3            MOA                                          
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#MOA)                                                     
*                                                                               
         DCDD  AC#TASK,4           TASK                                         
         DC    CL(10-4)' '                                                      
         DC    AL1(OPT#TSK)                                                     
*                                                                               
         DCDD  AC#WC,3             WC (WORK CODE)                               
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#TSK)                                                     
*                                                                               
         DCDD  AC#RSTWO,3          WO (WORK CODE)                               
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#TSK)                                                     
*                                                                               
         DCDD  AC#SORT,4           SORT                                         
         DC    CL(10-4)' '                                                      
         DC    AL1(OPT#SORT)                                                    
*                                                                               
         DCDD  AC#TYPE,4           TYPE OF TIME                                 
         DC    CL(10-4)' '                                                      
         DC    AL1(OPT#TYPE)                                                    
*                                                                               
         DCDD  AC#XD,3             XD - EXTRA DETAIL                            
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#XD)                                                      
*                                                                               
         DCDD  AC#CTRA,3           C/A - CONTRA ACCOUNT                         
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#CTRA)                                                    
*                                                                               
         DCDD  AC#CTRA,7           CONTRA - COONTRA ACCOUNT                     
         DC    CL(10-7)' '                                                      
         DC    AL1(OPT#CTRA)                                                    
*                                                                               
*&&UK                                                                           
         DCDD  AC#ALL,3            ALL - ALL TIME FOR DAILY TIME                
         DC    CL(10-3)' '                                                      
         DC    AL1(OPT#ALL)                                                     
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
OPT#CLI  EQU   1                                                                
OPT#PJ   EQU   2                                                                
OPT#MOA  EQU   3                                                                
OPT#TSK  EQU   4                                                                
OPT#SORT EQU   5                                                                
OPT#TYPE EQU   6                                                                
OPT#XD   EQU   7                                                                
OPT#CTRA EQU   8                                                                
OPT#ALL  EQU   9                                                                
         EJECT                                                                  
***********************************************************************         
* ADJUST PERIOD DATE BY LOCATION                                      *         
*                                                                     *         
* NTRY - PARM1 = 1R ACCOUNT CODE    ZERO = O/D/S/P ALREADY SET        *         
*        PARM2 = PERIOD END DATE                                      *         
*                                                                     *         
* EXIT - CC=EQU - BCENDTE CONTAINS CORRECT PERIOD END DATE            *         
*      - CC=NEQ - SOMETHING IS WRONG                                  *         
***********************************************************************         
         SPACE 1                                                                
ADJDATE  DS    0H                                                               
         XC    BCYYMMDD,BCYYMMDD                                                
         ICM   RF,15,4(R1)         PERIOD END DATE                              
         BZ    ROUTH                                                            
         MVC   BCENDTE,0(RF)                                                    
*                                                                               
         ICM   RF,15,0(R1)         1R ACCOUNT CODE                              
         BZ    ADJ5                OR BCOFFC/DEPT/SDPT/PERSON SET               
         MVC   BCACCODE,0(RF)                                                   
         MVC   BCOFFC,BCSPACES                                                  
         MVC   BCDEPT,BCSPACES                                                  
         MVC   BCSDPT,BCSPACES                                                  
         MVC   BCPERSON,BCSPACES                                                
*                                                                               
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLEV1                                                      
         SH    R1,=H'1'                                                         
         MVC   BCOFFC(0),0(RF)     EXTRACT OFFICE CODE                          
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV2                                                      
         SH    R1,=H'1'                                                         
         MVC   BCDEPT(0),0(RF)     EXTRACT DEPT CODE                            
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV3                                                      
         SH    R1,=H'1'                                                         
         MVC   BCSDPT(0),0(RF)     EXTRACT SUBDEPT CODE                         
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV4                                                      
         SH    R1,=H'1'                                                         
         MVC   BCPERSON(0),0(RF)   EXTRACT PERSON CODE                          
         EX    R1,*-6                                                           
*                                                                               
ADJ5     LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING PERRECD,R6                                                       
         MVC   PERKEY,BCSPACES                                                  
         MVI   PERKTYP,PERKTYPQ    X'0F' PERSON RECORD                          
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,BCPERSON                                                
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                PERSON RECORD MUST BE THERE                  
*                                                                               
         USING LOCELD,R6                                                        
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
ADJ10    SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    ADJX                                                             
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   ADJ10                                                            
         CLC   LOCOFF,BCOFFC       SAME OFFICE                                  
         BNE   ADJ10                                                            
         CLC   LOCDEPT,BCDEPT      SAME DEPARTMENT                              
         BNE   ADJ10                                                            
         CLC   LOCSUB,BCSDPT       SAME SUBDEPARTMENT                           
         BNE   ADJ10                                                            
         OC    LOCEND,LOCEND       STILL IN THIS LOCATION IF ZERO               
         BNZ   *+10                                                             
         MVC   LOCEND,BCEFFS                                                    
         CLC   LOCSTART,BCENDTE    IF LOCATION START > PERIOD END               
         BH    ADJ10               THEN NOT IN LOCATION @ THIS TIME             
         CLC   LOCEND,BCYYMMDD                                                  
         BL    ADJ10                                                            
         MVC   BCYYMMDD,LOCEND                                                  
         B     ADJ10                                                            
*                                                                               
ADJX     OC    BCYYMMDD,BCYYMMDD                                                
         BZ    ROUTE                                                            
         CLC   BCYYMMDD,BCENDTE                                                 
         BH    ROUTE               ONLY ADJUST DATE BACKWARDS                   
         MVC   BCENDTE,BCYYMMDD                                                 
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX LOCALITY                                               *         
*                                                                     *         
* NTRY - PARM1 = A(TAX LOCALITY FIELD)                                *         
*        PARM2 = PERIOD END DATE                                      *         
*        PARM3 = A(BYTE BASIS AMOUNT FIELD)                           *         
*                                                                     *         
* EXIT - CC=EQU - BCELEM = TAX BLOCK COVERED BY TXLOCALD              *         
*      - CC=NEQ - INVALID TAX LOCALITY                                *         
***********************************************************************         
         SPACE 1                                                                
VALLOC   DS    0H                                                               
*&&US                                                                           
         ICM   RF,15,0(R1)         TAX LOCALITY CODE                            
         BZ    ROUTH                                                            
         MVC   BCTAXLOC,0(RF)                                                   
         ICM   RF,15,4(R1)         PERIOD END DATE                              
         BZ    ROUTH                                                            
         MVC   BCYYMMDD,0(RF)                                                   
         ICM   RF,15,8(R1)         TAX BASIS AMOUNT                             
         BZ    ROUTH                                                            
         ZAP   BCTAXBAS,0(6,RF)                                                 
*                                                                               
         USING TXLOCALD,R4         R4=A(TAX OUTPUT BLOCK)                       
         LA    R4,BCELEM                                                        
         XC    BCELEM,BCELEM                                                    
         MVI   BCLAP,0             INITIALIZE LAP COUNTER                       
*                                                                               
VALLOC5  LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING SUTRECD,R6                                                       
         MVC   SUTKEY,BCSPACES                                                  
         MVI   SUTKTYP,SUTKTYPQ    X'2D' SALES TAX RECORD                       
         MVI   SUTKSUB,SUTKSUBQ    X'01'                                        
         MVC   SUTKCPY,CMPY                                                     
         SR    R1,R1               1ST LEVEL TAX LOCALITY REQUIRED              
         ICM   R1,1,BCLAP                                                       
         BZ    VALLOC10                                                         
         MH    R1,=H'2'                                                         
         LA    RF,BCTAXLOC         LOCALITY OPTIONAL FOR LEVEL 2-4              
         AR    RF,R1                                                            
         CLC   0(2,RF),BCSPACES                                                 
         BE    VALLOCX                                                          
*                                                                               
VALLOC10 SR    R1,R1                                                            
         IC    R1,BCLAP                                                         
         LA    R1,1(R1)                                                         
         MH    R1,=H'2'            LAP * L'LEVEL = # BYTES TO MOVE              
         SH    R1,=H'1'                                                         
         MVC   SUTKLOC(0),BCTAXLOC                                              
         EX    R1,*-6                                                           
         GOTO1 AGETACT,0                                                        
         BNE   ROUTH               ERROR ON INVALID LOCALITY CODE               
*                                                                               
         XC    BCFLAG2,BCFLAG2     CLEAR FOUND FLAG                             
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
VALLOC15 CLI   0(R6),0                                                          
         BE    VALLOC40                                                         
         CLI   0(R6),SUTELQ        X'5F' - SALES/USE TAX ELEMENT                
         BE    VALLOC25                                                         
VALLOC20 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     VALLOC15                                                         
*                                                                               
         USING SUTELD,R6                                                        
VALLOC25 CLI   SUTLN,SUTLN2Q                                                    
         BL    *+10                                                             
         MVC   TXLACC,SUTACC       CREDIT ACCOUNT                               
         CLC   SUTEFF,BCYYMMDD     SKIP IF EFFECTIVE DATE > PERIOD              
         BH    VALLOC20                                                         
         CLC   SUTEFF,TXLEFF       SKIP IF < MORE CURRENT RATE                  
         BL    VALLOC20                                                         
         MVC   TXLEFF,SUTEFF       EFFECTIVE DATE                               
         ZAP   TXLRATE,SUTRTE      TAX RATE                                     
         ZAP   BCPL16,SUTRTE                                                    
         MP    BCPL16,BCTAXBAS     TAX$ = BASIS * TAX RATE                      
         SRP   BCPL16,64-6,5                                                    
         ZAP   TXLAMNT,BCPL16                                                   
         OI    BCFLAG2,BCFL2FND    EFFECTIVE TAX ITEM FOUND                     
         B     VALLOC20                                                         
*                                                                               
VALLOC40 CLI   BCFLAG2,BCFL2FND    WAS A RATE FOUND                             
         BNE   ROUTH                                                            
         LA    R4,TXLLNQ(R4)                                                    
         SR    R1,R1                                                            
         IC    R1,BCLAP            BUMP LAP #                                   
         LA    R1,1(R1)                                                         
         STC   R1,BCLAP                                                         
         IC    R1,BCELEM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,BCELEM           STORE # MINIS IN 1ST BYTE BCELEM             
         CLI   BCLAP,3             ONLY LOOP THROUGH 4 LEVELS                   
         BNH   VALLOC5                                                          
*&&                                                                             
*                                                                               
VALLOCX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX WORKCODE                                               *         
*                                                                     *         
* NTRY - R1=TAX WORKCODE                                              *         
*                                                                     *         
* EXIT - CC=EQU - VALID                                               *         
*      - CC=NEQ - INVALID                                             *         
***********************************************************************         
         SPACE 1                                                                
VALTXWC  DS    0H                                                               
*&&US                                                                           
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING WCORECD,R6          X'0A' WORK-CODE RECORD                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CMPY                                                     
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,0(R1)                                                    
         GOTO1 AGETACT,0                                                        
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEITXWC)                                            
         B     ROUTH                                                            
*&&                                                                             
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* TRANSMIT SCREEN FIELD - TRANSMITS ALL FIELDS BETWEEN PARM1 & PARM2  *         
*                       - TRANSMITS ENTIRE SCREEN IF PARM2 = 0        *         
*                                                                     *         
* NTRY - PARM1 = A(START FIELD) - REQUIRED                            *         
*        PARM2 = A(END FIELD)   - OPTIONAL                            *         
*                                                                     *         
* EXIT - CC=EQU - OK                                                  *         
*        CC=NEQ - IF (END FIELD < START FIELD)                        *         
***********************************************************************         
         SPACE 1                                                                
XMIT     DS    0H                                                               
         ICM   R2,15,0(R1)         R2=A(START FIELD)                            
         BNZ   *+6                                                              
         DC    H'0'                A(START FIELD) IS REQUIRED                   
         ICM   R4,15,4(R1)         R4=A(END FIELD)                              
         BZ    XMIT10                                                           
         CR    R4,R2                                                            
         BL    ROUTH               ERROR IF END < START                         
*                                                                               
XMIT10   CLI   0(R2),0             END OF SCREEN                                
         BE    ROUTE                                                            
         CLI   1(R2),X'FF'         SKIP NOP FIELDS                              
         BE    *+8                                                              
         OI    6(R2),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         OR    R4,R4                                                            
         BZ    XMIT10              2ND PARAMETER NOT PASSED                     
         CR    R2,R4                                                            
         BNH   XMIT10                                                           
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT DISPLAY LINE                                                 *         
*                                                                     *         
* NTRY - BCFLD1 = FIRST FIELD                                         *         
*        BCFLD2 = SECOND FIELD                                        *         
*                                                                     *         
* EXIT - CC=EQU - BCFLD1 CONTAINS "FIELD1=FIELD2,"                    *         
*                 BCFLD1LQ = LENGTH OF INPUT RETURNED IN BCFLD1       *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   DS    0H                                                               
         CLC   BCFLD2,BCSPACES     DONT INSERT '=' IF NO SECOND FLD             
         BNH   FORM10                                                           
*                                                                               
         LA    R0,L'BCFLD1         *** INSERT '=' SIGN ***                      
         LA    R6,BCFLD1+L'BCFLD1-1                                             
         CLI   0(R6),X'40'                                                      
         BH    *+12                                                             
         SH    R6,=H'1'                                                         
         BCT   R0,*-12                                                          
         MVI   1(R6),C'='                                                       
         LA    R6,2(R6)                                                         
         LA    R1,BCFLD1+L'BCFLD1  *** COPY 2ND FIELD AFTER '=' ***             
         SR    R1,R6                                                            
         SH    R1,=H'1'                                                         
         MVC   0(0,R6),BCFLD2                                                   
         EX    R1,*-6                                                           
*                                                                               
FORM10   LA    R0,L'BCFLD1         *** INSERT TRAILING COMMA ***                
         LA    R6,BCFLD1+L'BCFLD1-1                                             
         CLI   0(R6),X'40'                                                      
         BH    *+12                                                             
         SH    R6,=H'1'                                                         
         BCT   R0,*-12                                                          
         MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         LA    R1,BCFLD1                                                        
         SR    R6,R1                                                            
         STC   R6,BCFLDSLQ         STORE LENGTH OF ENTIRE LINE                  
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT DISPLAY LINE                                                 *         
*                                                                     *         
* NTRY - PARM1 = WEEK ENDING DATE                                     *         
*        PARM2 = AREA FOR LIST OF DATES                               *         
*                                                                     *         
* EXIT                                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
GETDTES  DS    0H                                                               
         MVC   BCFULL,AIO          SAVE AIO FOR RETURN                          
         LM    R2,R3,0(R1)         R2=A(PERIOD END DATE) R3=A(LIST)             
         MVC   BCYYMMDD,0(R2)      SAVE PERIOD END DATE                         
         XC    0(7*L'TIMXTDDT,R3),0(R3) CLEAR LIST                              
         LR    R4,R3               R4=A(LIST)                                   
*                                                                               
         USING TSWRECD,R6          READ TIMESHEET WEEKLY PASSIVE PTR            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,BCACKCPY    COMPANY CODE                                 
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSWKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSWKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSWKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSWKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
*                                                                               
GTDT10   CLC   TSWKEY(TSWKODS+L'TSWKODS-TSWKEY),KEYSAVE                         
         BNE   GTDT30              NO DATA FOR THIS PERSON/WEEK/ODS             
         MVC   AIO,AIOBC                                                        
         MVC   BCDA,TSWKDA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETTMEL                                                       
*                                                                               
GTDT20   GOTO1 SEQ                                                              
         LA    R6,BIGKEY                                                        
         B     GTDT10                                                           
*                                                                               
         USING TSSRECD,R6          READ TIMESHEET SAVE RECORDS                  
GTDT30   LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         MVI   TSSKSUB,TSSKSUBQ    X'11'                                        
         MVC   TSSKCPY,BCACKCPY    COMPANY CODE                                 
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSSKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSSKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSSKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSSKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSSKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
*                                                                               
GTDT40   CLC   TSSKEY(TSSKODS+L'TSSKODS-TSSKEY),KEYSAVE                         
         BNE   GTDTX                                                            
         MVC   AIO,AIOBC                                                        
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETTMEL                                                       
*                                                                               
GTDT50   GOTO1 SEQ                                                              
         LA    R6,BIGKEY                                                        
         B     GTDT40                                                           
*                                                                               
GTDTX    MVC   AIO,BCFULL          RESTORE AIO TO ORIGINAL                      
         B     ROUTE                                                            
***********************************************************************         
* GET TIME ELEMENT INFORMATION INTO TABLE IN R3 AND R4                *         
***********************************************************************         
         SPACE 1                                                                
GETTMEL  NTR1  ,                                                                
         USING TIMELD,R6                                                        
         L     R6,AIOBC                                                         
         AH    R6,=Y(ACCRFST-ACCKEY)                                            
GTEL10   CLI   0(R6),0             END OF RECORD                                
         BE    GTELX                                                            
         CLI   0(R6),TIMELQ        X'8B'                                        
         BNE   GTEL20                                                           
         CLI   TIMETYP,TIMEXTRA    ONLY BRANCH ON START OF CLUSTER              
         BE    GTEL30              & SKIP TAX/NARRATIVE ITEMS                   
GTEL20   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GTEL10                                                           
*                                                                               
GTEL30   LA    R1,7                                                             
         LR    R3,R4                                                            
GTEL40   OC    0(L'TIMXTDDT,R3),0(R3)                                           
         BZ    GTEL70                                                           
         CLC   TIMXTDDT,0(R3)                                                   
         BE    GTEL20                                                           
         BL    GTEL50                                                           
         LA    R3,L'TIMXTDDT(R3)                                                
         BCT   R1,GTEL40                                                        
         DC    H'0'                                                             
*                                                                               
GTEL50   SHI   R1,1                                                             
         MHI   R1,L'TIMXTDDT                                                    
         MVC   BCWORK(0),0(R3)                                                  
         EX    R1,*-6                                                           
         MVC   0(L'TIMXTDDT,R3),TIMXTDDT                                        
         MVC   L'TIMXTDDT(0,R3),BCWORK                                          
         EX    R1,*-6                                                           
         B     GTEL20                                                           
GTEL70   MVC   0(L'TIMXTDDT,R3),TIMXTDDT                                        
         B     GTEL20                                                           
*                                                                               
GTELX    B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* LOCAL STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SV1RODS  DS    0CL8                SAVED AREA FOR LOCATION O/D/S                
SV1ROFF  DS    CL2                 SAVED AREA FOR LOCATION OFFICE               
SV1RDPT  DS    CL3                 SAVED AREA FO LOCATION DEPARTMENT            
SV1RSUB  DS    CL3                 SAVED AREA FOR LOCATION SUB-DEPART           
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACCAP32   12/11/09'                                      
         END                                                                    
