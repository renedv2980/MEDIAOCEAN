*          DATA SET PRSFM00S   AT LEVEL 168 AS OF 05/01/02                      
*PHASE T41C00A,+0     ************ NOTE "A" APPENDED TO PHASE                   
*INCLUDE PUBVAL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE SRCHCALL                                                   L06         
*INCLUDE KHDUMMY                                                                
         TITLE 'T41C00 - CHANGE LOG'                                            
*                                                                               
* BPLA      10/98 ACTIVATE BILLING FORMULA RECORDS                              
*                                                                               
* SMYE     10/97  GETINS MADE CORE-RESIDENT                                     
*                                                                               
* SMYE     08/97  USED CIRC "SELECT" LOGIC FOR PUBLIST (GENCON MSG FIX)         
*                                                                               
* BOBY      7/96  NEW RECORD FOR PUBLISTS (9/2/97 - NEW "VERSION")              
*                                                                               
* SMYE      4/96  NEW RECORD FOR BUDGETS (LIVE 09/02/97)                        
*                                                                               
* BPLA      10/95  CLT/PRD/PUB GROUPS - CHANGE RECORD NAMES                     
*                                                                               
* BOBY      8/95  CHECK FOR GLOBBER CALL                                        
*                                                                               
* BPLA      5/95  CLIENT/PRODUCT/PUB GROUPS                                     
*                                                                               
* BPLA      2/95  NEW RECORD FOR BILLING FORMULAS                               
*                                                                               
* SMUR      8/94  NEW RECORD FOR SPACE DESCRIPTION RECORDS                      
*                                                                               
* BOBY      2/94  NEW RECORD FOR EDR INKAGE (SECOND TRY)                        
*                                                                               
* BOBY  96 11/93  NEW RECORD FOR CIRCULATION                                    
*                                                                               
* BOBY  89 10/93  SCROLLING FOR CURECS                                          
*                                                                               
* BOBY  87  9/93  NEW RECORD FOR EDR LINKAGE                                    
*                                                                               
* BPLA 8/93      NEW RECORD FOR ISSUE DATES                                     
*                                                                               
* BPLA 8/93      NEW RECORD FOR SPACE CU                                        
*                                                                               
* LWEI 01/06/93  NEW RECORD FOR USER DEFINITIONS                                
*                                                                               
* LWEI 11/10/92  NEW RECORD FOR FSI                                             
*                                                                               
* LWEI 08/28/92  NEW RECORD FOR PG COST                                         
*                                                                               
* ABEA 06/29/92  NEW RECORD FOR CLEARANCE LIST                                  
*                                                                               
* BPLA 11/12/90  SET NTWA TO X'01' FOR GENCON                                   
*                                                                               
* BPLA 10/4/90   SET GENCON BIT TO PRESERVE KEY ON LISTS                        
*                                                                               
* BPLA 8/28/90   DIV/REG/DST FIX                                    L08         
*                                                                               
* BPLA 4/16/90   ADD OFFICE AND CLIENT SECURITY                     L07         
*                                                                               
* BPLA 2/14/90   GET AND SAVE ADDRESS OF USCAN IN VUSCAN            L06         
*                GETINS SAVED IN VGETINS                            L06         
*                                                                               
* ROSA 2/9/90    ADD CLIENT SECURITY                                L05         
*                                                                   L04         
* BPLA 7/31/89   ADD CODE FOR DIV/REG/DST                           L04         
*                                                                               
* ROSA 5/19/89   MODIFY TO ACCECPT NOW FUNCTION                     L03         
*                                                                   L03         
* ROSA 8/2/88    MODIFY TO ACCECPT NEW AGENCY NAME RECORD           L02         
*                                                                               
* ROSA 7/27/88   MODIFY TO ACCECPT NEW AOR FUNCTION                 L01         
*                                                                               
         TITLE 'T41C00 - PRINT SFM - CONTROLLER'                                
T41C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T41C00,RA,RR=R2,CLEAR=YES                              
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    RA,BASERA                                                        
*                                                                               
         L     R2,SYSPARMS                                                      
         L     R2,4(R2)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R2                                                   
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,0(RF)            A(TIOB)                                      
         USING TIOBD,RF                                                         
*                                                                               
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BE    *+8                 SKIP SWAP IF NOT FOUND                       
         CLI   TIOBAID,24          LOOK FOR PFKEY 24 HIT                        
         BNE   SFMPF12X            SKIP SWAP IF NOT FOUND                       
*                                                                               
         CLC   =C'ISSUE',CONREC    SKIP UNLESS ISSUE RECORD                     
         BNE   SFMPF12X                                                         
*                                                                               
         SR    RF,RF                                                            
         LA    RE,CONHEADH         POINT TO FIRST FLD ON SCREEN                 
         IC    RF,CONHEADH         LENGTH OF 1ST FIELD ON SCREEN                
         LA    RE,0(RF,RE)         POINT TO SERVICE REQUEST FIELD               
         MVC   8(17,RE),=CL17'=SW ' SET FOR RETURN TO MATCH                     
*                                                                               
         B     EXIT                RETURN WITH SWAP                             
*                                                                               
         DROP  RF                                                               
*                                                                               
SFMPF12X DS    0H                                                               
*                                                                               
*        CHECK IF THIS IS A GLOBBER CALL                                        
*                                                                               
         MVI   TRANSSW,0           INIT TRANSFER SWITCH                         
*                                                                               
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    SFMGLBX                                                          
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL  GET XCTL ELM             
*                                                                               
         CLI   DMCB+8,GLEGNF       SKIP IF NO ELM FOUND                         
         BE    SFMGLBX                                                          
*                                                                               
         CLI   DMCB+8,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,WORK             ESTABLISH XCTL ELM                           
         USING GLVXFRSY,R1                                                      
*                                                                               
         CLC   =C'PRI',GLVXTOSY    MAKE SURE PRINT/SFM WANTED                   
         BNE   SFMGLBX                                                          
         CLC   =C'SFM',GLVXTOPR                                                 
         BNE   SFMGLBX                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'  DELETE TRANSFER ELM                          
*                                                                               
         MVI   TRANSSW,C'Y'        INDICATE WE ARE IN MIDST OF TRANSFER         
*                                                                               
         L     R2,SYSPARMS                                                      
         L     R2,4(R2)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R2                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONRECH,,GLVXREC  GET RECORD              
         GOTO1 (RF),(R1),=C'DELE'  DELETE RECORD                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONACTH,,GLVXACT  GET ACTION              
         GOTO1 (RF),(R1),=C'DELE'  DELETE ACTION                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONKEYH,,GLVPRKEY GET KEY                 
         GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
SFMGLBX  DS    0H                                                               
*                                                                               
*        ANALYZE INPUT TO SEE IF WE NEED TO INTERVENE                           
*        ON SELECT ACTIONS                                                      
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,4(RF)            ESTABLISH SCREEN                             
         USING CONHEADH-64,RF                                                   
*                                                                               
         CLI   TWASCR,X'FC'        LOOKING FOR CUREC MAINTENANCE                
         BNE   CURECN                                                           
*                                                                               
         CLC   =C'CU',CONREC       DOING 'CUREC' MAINTENANCE                    
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
CURECN   DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C8'        LOOKING FOR PUBLIST LIST                     
         BNE   PUBLN                                                            
*                                                                               
         LA    R2,PLLSEL1H         POINT TO FIRST SELECT FIELD                  
*                                                                               
PUBLLP   DS    0H                                                               
*                                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
PUBLCN   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   PUBLLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
PUBLN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'D8'        LOOKING FOR PUBLIST MAINTENANCE              
         BNE   PUBLX                                                            
*                                                                               
         CLC   =C'PUBL',CONREC     DOING 'PUBLIST' MAINTENANCE                  
         BE    PUBLTST                                                          
         CLC   =C'SMPU',CONREC     DOING 'PUBLIST' MAINTENANCE                  
         BNE   *+10                                                             
*                                                                               
PUBLTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
PUBLX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C0'        LOOKING FOR CIRC LIST                        
         BNE   CIRCLN                                                           
*                                                                               
         LA    R2,CRLSEL1H         POINT TO FIRST SELECT FIELD                  
*                                                                               
CIRCLLP  DS    0H                                                               
*                                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
CIRCLCN  DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   CIRCLLP                                                          
*                                                                               
         B     GOGENCON                                                         
*                                                                               
CIRCLN   DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'D0'        LOOKING FOR CIRC MAINTENANCE                 
         BNE   CIRCN                                                            
*                                                                               
         CLC   =C'CI',CONREC       DOING 'CIRC' MAINTENANCE                     
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
CIRCN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'D5'        LOOKING FOR SPACE MAINTENANCE                
         BNE   SPACEN                                                           
*                                                                               
         CLC   =C'SP',CONREC       DOING 'SPACE' MAINTENANCE                    
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
SPACEN   DS    0H                                                               
*                                                                               
         B     GOGENCON                                                         
*                                                                               
CHASEL   DS    0H                  SET ACTION TO 'CHANGE'                       
*                                                                               
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            A(TIOB)                                      
         USING TIOBD,RE                                                         
*                                                                               
         CLI   TIOBAID,12          SKIP IF PF12 OR PF24                         
         BE    *+8                                                              
         CLI   TIOBAID,24                                                       
         BE    CHASELX                                                          
*                                                                               
****     SR    R0,R0                                                            
****     ICM   R0,1,TIOBAID        PFKEY                                        
****     BZ    CHASELX             NO PFKEY HIT                                 
*                                                                               
         MVC   CONACT(6),=C'CHANGE'   CHANGE ACTION                             
         MVI   CONACTH+5,6                                                      
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
CHASELX  DS    0H                                                               
*                                                                               
GOGENCON DS    0H                                                               
*                                                                               
         GOTO1 GENCON,DMCB,(R8)    GOTO GENERAL CONTROLLER                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
*                                                                               
SYSINIT  NTR1                                                                   
*                                                                               
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES                                                         
         LA    R4,COREFACS         ADDRESS AREA                                 
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)                                                         
*                                                                               
         MVC   VUSCAN,44(R1)                                     L06            
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)                                                        
         USING COMFACSD,R1                                                      
         MVC   VGLOBBER,CGLOBBER   SAVE GLOBBER ADDRESS                         
         L     RF,CCALLOV                                                       
         DROP  R1                                                               
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS6     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS6                                                          
*                          GET AND STORE GETINS CORE-RESIDENT ADDRESS           
         MVI   DMCB+7,QGETINS                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETINS,DMCB        SAVE GETINS MODULE ADDRESS                   
         EJECT                                                                  
* SET SYSTEM DEPENDENT VALUES *                                                 
*                                                                               
         MVI   NTWA,X'01'          TELL GENCON CON I NEED 1 TWA                 
*                                                                               
         MVI   SYSTEM,C'P'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'25'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'     USUALLY PRINT FILE                           
         MVC   SYSFIL,=C'PRTFILE '                                              
         MVC   SYSDIR,=C'PRTDIR  '                                              
         MVI   GETMSYS,25          USES GETMSG FOR SYSTEM 25                    
         MVC   LWORK,=Y(LENWORK)   SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9041C00'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START ADDR                 
         ST    R0,ASTARTSV                                                      
         OI    GENSTAT1,USKYMRG                                                 
         OI    GENSTAT2,DISTHSPG   STAY ON SAME LIST PAGE                       
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,4(RF)            ESTABLISH SCREEN                             
         USING CONHEADH-64,RF                                                   
*                                                                               
         CLC   =C'EDR',CONREC      ID EDR  RECORD TYPE                          
         BE    *+10                                                             
         CLC   =C'LOADEDR',CONREC  LOAD EDRFILE TO CTFILE                       
         BNE   *+12                                                             
         MVI   USEIO,C'Y'             INDICATE IT DOES OWN I/O                  
         MVI   IOOPT,C'Y'             OVERLAY DOES I/O                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     RA,BASERA                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VAGY                                                             
         B     VCURSERR                                                         
         B     VMED                                                             
         B     VCLT                                                             
         B     VPRD                                                             
         B     VEST                                                             
         B     VDIV                                                             
         B     VREG                                                             
         B     VDST                                                             
         B     VSTD                                                             
         B     VEND                                                             
         B     VDTYP                                                            
         B     VPER                                                             
         B     VPUB                                                             
         B     GETFLD                                                           
         B     VADC                                                             
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
BASERA   DC    A(0)                                                             
         EJECT                                                                  
VAGY     B     EXIT                GET AGY REC IN MEDIA VALIDATION              
         SPACE 2                                                                
* VALIDATE MEDIA CODE *                                                         
         SPACE 1                                                                
VMED     XC    SVKEY,SVKEY         CLEAR SAVED KEY AREA                         
         GOTO1 VGETFLD                                                          
*                                                                               
         MVI   ERROR,INVMED                                                     
         CLI   FLDH+5,1            INPUT LEN MUST BE 1                          
         BNE   TRAPERR                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING PAGYKEY,R4                                                       
         MVC   PAGYKAGY,AGENCY                                                  
         MVC   PAGYKMED,FLD        MEDIA                                        
         MVI   PAGYKRCD,X'01'                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVMED                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING PAGYELEM,R6                                                      
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
*                                                                               
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         MVC   SVUSER,USERNAME        SAVE FOR FUTURE REF                       
         MVC   QMED,8(R2)             SAVE INPUT MEDIA CODE                     
         MVC   MEDNM,PAGYMED          MEDIA NAME                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLIENT *                                                             
         SPACE 1                                                                
VCLT     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         MVC   QCLT,FLD                                                         
*                                                                               
         XC    KEY,KEY             GET CLIENT RECORD                            
         LA    R6,KEY                                                           
         USING PCLTRECD,R6                                                      
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,QCLT                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVCLI                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                 L05           
         L     RF,ATWA                                            L05           
*                                                                               
         OC    6(2,RF),6(RF)     CHK FOR LIMIT ACCESS             L07           
         BZ    VCLT10                                             L07           
         CLI   6(RF),C'$'                                         L05           
         BNE   NODOL                                              L05           
         BAS   RE,PPCLIVER                                        L05           
         BE    VCLT10                                             L07           
*                                                                               
ACCERR   MVI   ERROR,SECLOCK        SECURITY LOCK-OUT             L05           
         B     TRAPERR                                            L05           
*                                                                               
NODOL    DS    0H                                                 L05           
         CLI   6(RF),C'*'        CHK OFFICE LIMIT ACCESS          L07           
         BNE   VCLT5                                              L07           
         CLC   7(1,RF),PCLTOFF                                    L07           
         BNE   ACCERR                                             L07           
         B     VCLT10                                             L07           
*                                                                 L07           
VCLT5    CLC   6(3,RF),PCLTKCLT                                   L07           
         BNE   ACCERR                                             L07           
*                                                                               
VCLT10   MVC   CLTNM,PCLTNAME                                                   
         MVC   SVCPROF,PCLTPROF                                                 
         B     EXIT                                                             
*          DATA SET PPACCTEST  AT LEVEL 006 AS OF 02/07/90                      
         SPACE 2                                                                
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                  / **************************\                                
PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                           
*                  \ **************************/                                
         SPACE 2                                                                
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         L     RF,ATWA                                                          
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTOFF                                                   
         DROP  R1                                                               
         L     RF,DMCB                                                          
         LA    R8,DATAMGR                                                       
         GOTO1  (RF),DMCB,WORK,(R8)                                             
         CLI   0(R1),0                                                          
         XIT1                                                                   
         DROP  R6                                                               
*******************************************************                         
*******************************************************                         
         EJECT                                                                  
* VALIDATE PRODUCT                                                              
*                                                                               
VPRD     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         MVC   QPRD,FLD                                                         
*                                                                               
         XC    KEY,KEY             GET PRODUCT RECORD                           
         LA    R4,KEY                                                           
         USING PPRDKEY,R4                                                       
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,QCLT                                                    
         MVC   PPRDKPRD,QPRD                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVPRD                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PPRDELEM,R6                                                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
         MVC   PRDNM,PPRDNAME                                                   
         MVC   HOLDDIV,PPRDDIV                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE *                                                           
         SPACE 1                                                                
VEST     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         MVI   ERROR,INVEST                                                     
         STH   R0,BEST             CONTAINS BINARY AFTER GETFLD                 
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
         CVD   R0,DUB                                           L08             
         OI    DUB+7,X'0F'                                      L08             
         UNPK  QEST,DUB                                         L08             
*                                                                               
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R4,KEY                                                           
         USING PESTKEY,R4                                                       
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,QCLT                                                    
         MVC   PESTKPRD,QPRD                                                    
         MVC   PESTKEST,BEST                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ESTNM,PESTNAME                                                   
         MVC   ESTDTST,PESTST      SAVE ESTIMATE START DATE                     
         MVC   ESTDTEND,PESTEND    SAVE ESTIMATE END DATE                       
*****    EDIT  (2,BEST),(3,QEST)         QEST SET ABOVE                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE DIVISION                                                             
*                                                                               
VDIV     XC    QDIV,QDIV                                                        
         XC    DIVNM,DIVNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   QDIV,FLD                                                         
         CLC   QDIV,=C'000'                                                     
         BE    VDIV5                                                            
         LTR   R0,R0                                                            
         BZ    VDIVERR                                                          
         CVD   R0,DUB                                           L08             
         OI    DUB+7,X'0F'                                      L08             
         UNPK  QDIV,DUB                                         L08             
*                                                                               
VDIV5    XC    KEY,KEY             GET DIVISION RECORD                          
         LA    R4,KEY                                                           
         USING PDIVKEY,R4                                                       
         MVC   PDIVKAGY,AGENCY                                                  
         MVC   PDIVKMED,QMED                                                    
         MVI   PDIVKRCD,X'03'                                                   
         MVC   PDIVKCLT,QCLT                                                    
         MVC   PDIVKDIV,QDIV                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   EXIT              LEAVES DIVNM AS ZEROS                          
*                                IF DIVISION NOT FOUND                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PDIVELEM,R6                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DIVNM,PDIVNAME                                                   
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
VDIVERR  MVI   ERROR,INVDIV                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
* VALIDATE REGION *                                                             
         SPACE 1                                                                
VREG     XC    QREG,QREG                                                        
         XC    REGNM,REGNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         LTR   R0,R0                                                            
         BZ    VREGERR                                                          
         CVD   R0,DUB                                           L08             
         OI    DUB+7,X'0F'                                      L08             
         UNPK  QREG,DUB                                         L08             
         OC    QDIV,QDIV           THEN DIVISION MUST = NNN                     
         BNZ   VREG5                                                            
VREGERR  MVI   ERROR,INVREG                                                     
         B     TRAPERR                                                          
*                                                                               
VREG5    XC    KEY,KEY             GET REGION RECORD                            
         LA    R4,KEY                                                           
         USING PREGKEY,R4                                                       
         MVC   PREGKAGY,AGENCY                                                  
         MVC   PREGKMED,QMED                                                    
         MVI   PREGKRCD,X'04'                                                   
         MVC   PREGKCLT,QCLT                                                    
         MVC   PREGKDIV,QDIV                                                    
         MVC   PREGKREG,QREG                                                    
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   EXIT                   LEAVES REGNM AS ZEROS                     
*                                     IF REGION NOT FOUND                       
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PREGELEM,R6                                                      
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   REGNM,PREGNAME                                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE DISTRICT                                                             
*                                                                               
VDST     XC    QDST,QDST                                                        
         XC    DSTNM,DSTNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         LTR   R0,R0                                                            
         BZ    VDSTERR                                                          
         CVD   R0,DUB                                             L08           
         OI    DUB+7,X'0F'                                        L08           
         UNPK  QDST,DUB                                           L08           
         OC    QREG,QREG           THEN REGION MUST = NNN                       
         BNZ   VDST5                                                            
VDSTERR  MVI   ERROR,INVDST                                                     
         B     TRAPERR                                                          
*                                                                               
VDST5    XC    KEY,KEY             GET DISTRICT RECORD                          
         LA    R4,KEY                                                           
         USING PDSTKEY,R4                                                       
         MVC   PDSTKAGY,AGENCY                                                  
         MVC   PDSTKMED,QMED                                                    
         MVI   PDSTKRCD,X'05'                                                   
         MVC   PDSTKCLT,QCLT                                                    
         MVC   PDSTKDIV,QDIV                                                    
         MVC   PDSTKREG,QREG                                                    
         MVC   PDSTKDST,QDST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   EXIT                LEAVES DSTNM AS ZEROS                        
*                                  IF DISTRICT NOT FOUND                        
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PDSTELEM,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSTNM,PDSTNAME                                                   
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* VALIDATE START DATE                                                           
*                                                                               
VSTD     DS    0H                                                               
         GOTO1 VGETFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QSTART (YYMMDD)                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(3,BSTART) (YMD)                              
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* VALIDATE END DATE                                                             
*                                                                               
VEND     DS    0H                                                               
         GOTO1 VGETFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QEND (YYMMDD)                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   QEND,QSTART         SEL END/SEL START                            
         BNL   *+12                                                             
         MVI   ERROR,INVDTSEQ      END DATE BEFORE START DATE                   
         B     TRAPERR                                                          
         GOTO1 DATCON,DMCB,QEND,(3,BEST) (YMD)                                  
*                                                                               
VENDX    B     EXIT                                                             
         EJECT                                                                  
* VALIDATE DATE TYPE                                                            
*                                                                               
VDTYP    MVI   DATETYPE,C'I'       INSERTION DATE (DEFAULT)                     
         GOTO1 VGETFLD                                                          
         BZ    DTYPX                                                            
         CLI   FLD,C'I'            INSERTION                                    
         BE    DTYPX                                                            
         CLI   FLD,C'P'                                                         
         BNE   *+12                                                             
         MVI   DATETYPE,C'P'       PAYABLE                                      
         B     DTYPX                                                            
         CLI   FLD,C'B'            BILLABLE                                     
         BNE   *+12                                                             
         MVI   DATETYPE,C'B'                                                    
         B     DTYPX                                                            
         CLI   FLD,C'C'            CLOSING                                      
         BNE   *+12                                                             
         MVI   DATETYPE,C'C'                                                    
         B     DTYPX                                                            
         CLI   FLD,C'S'            ON SALE DATE                                 
         BNE   DTYPERR                                                          
         MVI   DATETYPE,C'S'                                                    
DTYPX    B     EXIT                                                             
*                                                                               
DTYPERR  MVI   ERROR,INVDTYP                                                    
         B     TRAPERR                                                          
         SPACE 5                                                                
* VALIDATE PUBLICATION                                                          
*                                                                               
VPUB     DS    0H                                                               
         CLI   8(R2),C'='          IF INPUT STARTS WITH '='                     
         BNE   VPUBSCHX                                                         
*                                     NEED TO DO NAME SEARCHING                 
         S     R2,ATWA                DISPLACEMENT OF FIELD INTO TWA            
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',ATWA),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
*                                                                               
         A     R2,ATWA             RESTORE FIELD POINTER                        
*                                                                               
         DROP  R3                                                               
*                                                                               
VPUBSCHX DS    0H                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
*                                                                               
         MVI   ERROR,INVPUB                                                     
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ERROR,INVPUB                                                     
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ADCODE *                                                             
         SPACE 1                                                                
VADC     XC    QADCODE,QADCODE                                                  
         MVI   FLDOPT,C'Y'                                                      
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   QADCODE,FLD                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PJOBRECD,R4                                                      
         MVC   PJOBKAGY,AGENCY                                                  
         MVC   PJOBKMED,QMED                                                    
         MVI   PJOBKRCD,X'15'                                                   
         MVC   PJOBKCLT,QCLT                                                    
         MVC   PJOBKPRD,QPRD                                                    
         MVC   PJOBKJOB,QADCODE                                                 
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VPER     DS    0H                                                               
         MVI   FLDOPT,C'Y'                                                      
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
*                                                                               
         LA    R3,FLD                                                           
         GOTO1 DATVAL,DMCB,FLD,QSTART (YYMMDD)                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R7,FLD                                                           
         A     R7,DMCB                                                          
         CLI   0(R7),C'-'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         LA    R7,1(R7)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(3,BSTART) (YMD)                              
*                                                                               
         GOTO1 DATVAL,DMCB,(R7),QEND (YYMMDD)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   QEND,QSTART         SEL END/SEL START                            
         BNL   *+12                                                             
         MVI   ERROR,INVDTSEQ      END DATE BEFORE START DATE                   
         B     TRAPERR                                                          
         GOTO1 DATCON,DMCB,QEND,(3,BEND) (YMD)                                  
*                                                                               
VPERX    B     EXIT                                                             
         EJECT                                                                  
* SET CURSOR TO ERROR POSITION AND EXIT TO ERROR ROUTINE                        
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
*                                                                               
VCURSERR SR    R4,R4                                                            
         ICM   R4,7,1(R1)          A(SCANNER BLOCK)                             
         ZIC   RE,0(R1)            LENGTH OF 2ND HALF OF SCANNER FIELDS         
         LA    RE,22(RE)           TOTAL LENGTH OF EACH SCANNER FIELD           
         LA    R3,8(R2)            A(NEW CURSOR POSITION)                       
         SR    R5,R5                                                            
         MVC   BYTE,4(R1)          ERROR ROUTINE SWITCH                         
         ICM   R5,7,5(R1)          A(INVALID FIELD NUMBER)                      
         CLI   0(R5),1             TEST FIRST FIELD IS INVALID                  
         BE    VC100               CURSOR NEED NOT BE ADJUSTED                  
         LA    RF,1                                                             
*                                                                               
VC20     ZIC   R1,0(R4)            LENGTH OF FIRST HALF OF FIELD                
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS ','              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    VC40                LENGTH OF SECOND HALF OF FIELD               
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS '='              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
VC40     LA    R4,0(RE,R4)         NEXT SCANNER ENTRY                           
         B     VC20                                                             
*                                                                               
VC100    L     R1,SYSPARMS                                                      
         L     R1,0(R1)            A(TIOB)                                      
         USING TIOBD,R1                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    R3,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   R3,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
         CLI   BYTE,0              GO TO PROPER ERROR ROUTINE                   
         BE    VC200                                                            
         CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
VC200    GOTO1 ERREX                                                            
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                               
*                                                                               
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  FLDOPT=1 FIELD IS OPTIONAL                   
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER                  
*                                  FLD   FIELD DATA SPACE FILLED                
*                                  R0    BINARY VALUE IF FIELD NUMERIC          
*                                  R1    FIELD LENGTH                           
*                                  CONDITION CODE ZERO IF R1=0                  
*                                                                               
GETFLD   DS    0H                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         CLI   5(R2),0             TEST NO INPUT                                
         BE    GTFERRCK                                                         
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SH    R1,=H'8'            (SUBTRACT 8 MORE)                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R2)                                                     
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
*                                                                               
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
*                                                                               
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
*                                                                               
GETFLD4  LR    RE,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  RE,0                                                             
         CH    RE,=H'7'            LIMIT TO MAX 8 DIGITS                        
         BH    GTNOTNUM                                                         
         MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GTNOTNUM                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         B     GETFLDX                                                          
*                                                                               
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
*                                                                               
GTFERRCK MVI   ERROR,MISSING                                                    
         CLI   FLDOPT,C'Y'         IS THIS OK?                                  
         BNE   TRAPERR                                                          
         XC    FLD,FLD                                                          
         B     GETFLDX                                                          
*                                                                               
GETFLDX  MVI   FLDOPT,C'N'         RESET OPTIONAL BITS                          
         LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
* CONSTANTS TABLES, ETC *                                                       
*                                                                               
RELO     DS    A                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(PUBVAL)                                                        
         DC    V(RECUP)                                                         
*****    DC    V(GETINS)                                         L06            
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QLINUP)                                                      
         DC    AL1(QPRHELP)                                                     
         DC    AL1(QPRVAL)                                                      
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
         DC    X'01',C'USERP   ',AL1(02),X'00C2'                                
         DC    X'01',C'BUY     ',AL1(03),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'AOR     ',AL1(04),X'00CF'                  L01           
         DC    X'01',C'OAN     ',AL1(05),X'00CF'                  L02           
         DC    X'01',C'DIVISION',AL1(07),X'00CF'                  L04           
         DC    X'01',C'REGION  ',AL1(08),X'00CF'                  L04           
         DC    X'01',C'DISTRICT',AL1(09),X'00CF'                  L04           
         DC    X'01',C'DSTREC  ',AL1(09),X'00CF'                                
         DC    X'01',C'CLRST   ',AL1(10),X'00CF'                                
         DC    X'01',C'COST    ',AL1(11),X'00CF'                                
         DC    X'01',C'FSI     ',AL1(12),X'00CF'                                
         DC    X'01',C'UDEF    ',AL1(13),X'00CF'                                
         DC    X'01',C'CUREC   ',AL1(14),X'00CF'                                
         DC    X'01',C'ISSUE   ',AL1(15),X'00CF'                                
         DC    X'01',C'EDR     ',AL1(18),X'00CF'                                
         DC    X'01',C'CIRC    ',AL1(17),X'00CF'                                
         DC    X'01',C'2EDR    ',AL1(18),X'00CF'                                
         DC    X'01',C'LOADEDR ',AL1(19),X'00CF'                                
         DC    X'01',C'SPACE   ',AL1(20),X'00CF'                                
         DC    X'01',C'SPACE   ',AL1(20),X'00CF'                                
         DC    X'01',C'BILLFORM',AL1(21),X'00CF'    BILLING FORMULA             
******   DC    X'01',C'BPLA    ',AL1(21),X'00CF'    BILLING FORMULA             
         DC    X'01',C'BUDGET  ',AL1(22),X'00CF'    BUDGET                      
*****    DC    X'01',C'SMYEREC ',AL1(22),X'00CF'    BUDGET TEST NAME            
         DC    X'01',C'CGROUP  ',AL1(34),X'00CF'    CLIENT GROUPS               
         DC    X'01',C'CGASSIGN',AL1(35),X'00CF'    CLIENT ASSIGNMENTS          
         DC    X'01',C'CGDEF   ',AL1(36),X'00CF'    CLIENT GROUP DEF            
         DC    X'01',C'PGROUP  ',AL1(37),X'00CF'    PRODUCT GROUPS              
         DC    X'01',C'PGASSIGN',AL1(38),X'00CF'    PRODUCT ASSIGNMENTS         
         DC    X'01',C'PGDEF   ',AL1(39),X'00CF'    PRODUCT GROUP DEF           
         DC    X'01',C'PBGROUP ',AL1(40),X'00CF'    PUB GROUPS                  
         DC    X'01',C'PBASSIGN',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PBDEF   ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PUBLIST ',AL1(43),X'00CF'    PUB LIST                    
*****    DC    X'01',C'SMPUBL  ',AL1(43),X'00CF'    PUB LIST                    
         DC    X'01',C'JWTCPP  ',AL1(44),X'00CF'    JWT CPP PUT TO Q            
**                                                                              
** BELOW ARE ALTERNATE SPELLINGS FOR PUB GROUP RECORDS                          
**                                                                              
         DC    X'01',C'PUBGROUP',AL1(40),X'00CF'    PUB GROUPS                  
         DC    X'01',C'PUBGASSI',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBASSIG',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBGDEF ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PBGDEF  ',AL1(42),X'00CF'    PUB GROUP DEF               
         SPACE 3                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'COPY    ',AL1(13,13,00)                                  
         DC    X'02',C'MOVE    ',AL1(14,14,00)                                  
         DC    X'02',C'MAKELIVE',AL1(15,15,00)                                  
         DC    X'02',C'CANCEL  ',AL1(16,16,00)                                  
         DC    X'02',C'MAINTAIN',AL1(17,17,00)                                  
         DC    X'02',C'TOTALS  ',AL1(18,18,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*               X'03' ENTRIES ARE OK REC/ACT COMBOS                             
*                |       CL1 RECORD NUMBER                                      
*                |       |  CL1 ACTION EQUATE                                   
*                |       |  |     CL1 PHASE NUMBER FOR SCREEN                   
*                |       |  |     | CL1 PHASE NUMBER FOR EDIT                   
*                |       |  |     | | CL1 PHASE NUMBER FOR SPECS                
*                |       |  |     | | | CL1 PHASE NUMBER FOR REPORT             
*                |       |  |     | | | | CL1 WHEN OK BITS 80=SCREEN            
*                |       |  |     | | | | | 40=NOW 20=SOON 10=OV 08=DDS         
*                |       |  |     | | | | |     CL2 CODE FOR REPORTS            
*                |       |  |     | | | | |     | CL2 CODE FOR EOD-             
*                |       |  |     | | | | |     | |         HANDLING            
*                |       |  |     SC| SP| AC    | |                             
*                |       |  |     | OV| RP|     | |                             
         DC    X'03',AL1(02,01),X'F2510000C0',C'    '  USERP    MAINT           
         DC    X'03',AL1(02,10),X'E2510051F8',C'USSF'           LIST            
         DC    X'03',AL1(02,12),X'E251005178',C'USSF'           REPORT          
         DC    X'03',AL1(03,13),X'F3030003F8',C'BXBX'  BUY      COPY            
         DC    X'03',AL1(03,14),X'F3030003F8',C'BXBX'  BUY      MOVE            
         DC    X'03',AL1(03,15),X'F3030003F8',C'BXBX'  BUY      MKLV            
         DC    X'03',AL1(03,16),X'F3030003F8',C'BXBX'  BUY      CANCEL          
         DC    X'03',AL1(04,01),X'F1110000C0',C'    '  AOR      MAINT           
         DC    X'03',AL1(04,10),X'E1110011C0',C'AOSF'           LIST            
         DC    X'03',AL1(04,12),X'E111001178',C'AOSF'           REPORT          
         DC    X'03',AL1(05,01),X'F6160000C0',C'    '  OAN      MAINT           
         DC    X'03',AL1(05,10),X'E6160016F8',C'OASF'           LIST            
         DC    X'03',AL1(05,12),X'E616001678',C'OASF'           REPORT          
         DC    X'03',AL1(07,01),X'F7070000C0',C'    '  DIV      MAINT           
         DC    X'03',AL1(07,10),X'E7070007F8',C'DVSF'           LIST            
         DC    X'03',AL1(07,12),X'E707000778',C'DVSF'           REPORT          
         DC    X'03',AL1(08,01),X'F8080000C0',C'    '  REG      MAINT           
         DC    X'03',AL1(08,10),X'E8080008F8',C'RGSF'           LIST            
         DC    X'03',AL1(08,12),X'E808000878',C'RGSF'           REPORT          
         DC    X'03',AL1(09,01),X'F9090000C0',C'    '  DIS      MAINT           
         DC    X'03',AL1(09,10),X'E9090009F8',C'DSSF'           LIST            
         DC    X'03',AL1(09,12),X'E909000978',C'DSSF'           REPORT          
         DC    X'03',AL1(10,10),X'F4040000C0',C'    '  CLR      LIST            
         DC    X'03',AL1(11,01),X'FA0A0000C0',C'    '  PGCOST   MAINT           
         DC    X'03',AL1(11,10),X'EA0A000AF8',C'PGPG'           LIST            
         DC    X'03',AL1(12,01),X'F5050000C0',C'    '  FSI      MAINT           
         DC    X'03',AL1(12,10),X'E5050005F8',C'FSSF'           LIST            
         DC    X'03',AL1(12,12),X'E505000578',C'FSSF'           REPORT          
         DC    X'03',AL1(13,01),X'FB0B0000C0',C'    '  UDEF     MAINT           
         DC    X'03',AL1(13,10),X'EB1B001BF8',C'UDUD'           LIST            
         DC    X'03',AL1(14,01),X'FC0C0000C0',C'    '  CUREC    MAINT           
         DC    X'03',AL1(14,10),X'EC0C000CF8',C'CUSF'           LIST            
         DC    X'03',AL1(14,12),X'EC0C000C78',C'CUSF'           REPORT          
         DC    X'03',AL1(15,01),X'FD0D0000C0',C'    '  ISSUE    MAINT           
         DC    X'03',AL1(15,10),X'ED0D000DF8',C'ISSF'           LIST            
         DC    X'03',AL1(15,12),X'ED0D000D78',C'ISSF'           REPORT          
         DC    X'03',AL1(17,01),X'D0100000C0',C'    '  CIRC     MAINT           
         DC    X'03',AL1(17,10),X'C0100010F8',C'CRSF'           LIST            
         DC    X'03',AL1(17,12),X'C010001078',C'CRSF'           REPORT          
         DC    X'03',AL1(18,01),X'D3130000C1',C'    '  EDR      MAINT           
         DC    X'03',AL1(18,12),X'D313001318',C'EDSF'           REPORT          
         DC    X'03',AL1(19,01),X'D4140000C1',C'    '  LOADEDR  MAINT           
         DC    X'03',AL1(19,12),X'D414001418',C'EDSF'           REPORT          
         DC    X'03',AL1(20,01),X'D5150000C0',C'    '  SPACE    MAINT           
         DC    X'03',AL1(20,10),X'C5150015F8',C'SPSF'           LIST            
         DC    X'03',AL1(20,12),X'C515001578',C'SPSF'           REPORT          
         DC    X'03',AL1(21,17),X'D717000081',C'    '  BILLF    MAINT           
*                                                                               
         DC    X'03',AL1(22,01),X'D1120000C0',C'    '  BUDGET   ADD             
         DC    X'03',AL1(22,02),X'D1120000C0',C'    '           CHANGE          
         DC    X'03',AL1(22,03),X'D1120000C0',C'    '           DISPLAY         
         DC    X'03',AL1(22,05),X'D1120000C0',C'    '           SELECT          
         DC    X'03',AL1(22,10),X'C1120000C0',C'    '           LIST            
         DC    X'03',AL1(22,18),X'D1120000C0',C'    '           TOTALS          
         DC    X'03',AL1(22,12),X'D112000078',C'BUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(34,01),X'B2260000C0',C'    '  CGROUP   ADD             
         DC    X'03',AL1(34,02),X'B2260000C0',C'    '           CHANGE          
         DC    X'03',AL1(34,03),X'B2260000C0',C'    '           DISPLAY         
         DC    X'03',AL1(34,04),X'B2260000C0',C'    '           DELETE          
         DC    X'03',AL1(34,05),X'B2260000C0',C'    '           SELECT          
         DC    X'03',AL1(34,06),X'B2260000C0',C'    '           RESTORE         
         DC    X'03',AL1(34,10),X'B9260000C0',C'    '           LIST            
         DC    X'03',AL1(34,12),X'BA26000078',C'CGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(35,01),X'B3270000C1',C'    '  CGASSIGN ADD             
         DC    X'03',AL1(35,02),X'B3270000C1',C'    '           CHANGE          
         DC    X'03',AL1(35,03),X'B3270000C1',C'    '           DISPLAY         
         DC    X'03',AL1(35,04),X'B3270000C1',C'    '           DELETE          
         DC    X'03',AL1(35,05),X'B3270000C1',C'    '           SELECT          
         DC    X'03',AL1(35,06),X'B3270000C1',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(36,01),X'B1250000C0',C'    '  CGDEF    ADD             
         DC    X'03',AL1(36,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(36,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(36,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(36,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(36,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(36,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(37,01),X'B4290000C0',C'    '  PGROUP   ADD             
         DC    X'03',AL1(37,02),X'B4290000C0',C'    '           CHANGE          
         DC    X'03',AL1(37,03),X'B4290000C0',C'    '           DISPLAY         
         DC    X'03',AL1(37,04),X'B4290000C0',C'    '           DELETE          
         DC    X'03',AL1(37,05),X'B4290000C0',C'    '           SELECT          
         DC    X'03',AL1(37,06),X'B4290000C0',C'    '           RESTORE         
         DC    X'03',AL1(37,10),X'BD290000C0',C'    '           LIST            
         DC    X'03',AL1(37,12),X'BE29000078',C'PGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(38,01),X'B52A0000C1',C'    '  PGASSIGN ADD             
         DC    X'03',AL1(38,02),X'B52A0000C1',C'    '           CHANGE          
         DC    X'03',AL1(38,03),X'B52A0000C1',C'    '           DISPLAY         
         DC    X'03',AL1(38,04),X'B52A0000C1',C'    '           DELETE          
         DC    X'03',AL1(38,05),X'B52A0000C1',C'    '           SELECT          
         DC    X'03',AL1(38,06),X'B52A0000C1',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(39,01),X'BB280000C0',C'    '  PGDEF    ADD             
         DC    X'03',AL1(39,02),X'BB280000C0',C'    '           CHANGE          
         DC    X'03',AL1(39,03),X'BB280000C0',C'    '           DISPLAY         
         DC    X'03',AL1(39,04),X'BB280000C0',C'    '           DELETE          
         DC    X'03',AL1(39,05),X'BB280000C0',C'    '           SELECT          
         DC    X'03',AL1(39,06),X'BB280000C0',C'    '           RESTORE         
         DC    X'03',AL1(39,10),X'BC280000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(40,01),X'B6260000C0',C'    ' PUB GROUP ADD             
         DC    X'03',AL1(40,02),X'B6260000C0',C'    '           CHANGE          
         DC    X'03',AL1(40,03),X'B6260000C0',C'    '           DISPLAY         
         DC    X'03',AL1(40,04),X'B6260000C0',C'    '           DELETE          
         DC    X'03',AL1(40,05),X'B6260000C0',C'    '           SELECT          
         DC    X'03',AL1(40,06),X'B6260000C0',C'    '           RESTORE         
         DC    X'03',AL1(40,10),X'B9260000C0',C'    '           LIST            
         DC    X'03',AL1(40,12),X'BA26000078',C'BGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(41,01),X'B7270000C1',C'    ' PUB ASSGN ADD             
         DC    X'03',AL1(41,02),X'B7270000C1',C'    '           CHANGE          
         DC    X'03',AL1(41,03),X'B7270000C1',C'    '           DISPLAY         
         DC    X'03',AL1(41,04),X'B7270000C1',C'    '           DELETE          
         DC    X'03',AL1(41,05),X'B7270000C1',C'    '           SELECT          
         DC    X'03',AL1(41,06),X'B7270000C1',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(42,01),X'B1250000C0',C'    ' PUB G DEF ADD             
         DC    X'03',AL1(42,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(42,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(42,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(42,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(42,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(42,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(43,01),X'D8180000C0',C'    '  PUBLIST  MAINT           
         DC    X'03',AL1(43,10),X'C8180018F8',C'PLSF'           LIST            
         DC    X'03',AL1(43,12),X'C818001878',C'PLSF'           REPORT          
*                                                                               
         DC    X'03',AL1(44,12),X'BF8F008F18',C'CPSF'           REPORT          
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
RECACT2  DS    0D                  ALTERNATE ACTION TABLE FOR SCROLLING         
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(02,01,00) DISPLAY =CHANGE                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'COPY    ',AL1(13,13,00)                                  
         DC    X'02',C'MOVE    ',AL1(14,14,00)                                  
         DC    X'02',C'MAKELIVE',AL1(15,15,00)                                  
         DC    X'02',C'CANCEL  ',AL1(16,16,00)                                  
         DC    X'02',C'TOTALS  ',AL1(18,18,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC8D                                                       
         EJECT                                                                  
** DDGENTWA                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
** DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
** DDCOREQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
** FATIOB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
** PPSRCHPARM                                                                   
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
         PRINT ON                                                               
** DDGLOBEQUS                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
** DDGLVXCTLD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PDIVRECD DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
PREGRECD DSECT                                                                  
       ++INCLUDE PREGREC                                                        
         EJECT                                                                  
PDSTRECD DSECT                                                                  
       ++INCLUDE PDSTREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
         EJECT                                                                  
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
         SPACE 1                                                                
       ++INCLUDE PUBNAMEL                                                       
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'168PRSFM00S  05/01/02'                                      
         END                                                                    
