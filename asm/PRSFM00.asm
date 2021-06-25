*          DATA SET PRSFM00    AT LEVEL 203 AS OF 07/07/20                      
*PHASE T41C00A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE KHDUMMY                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C00 - PRINT SFM - CONTROLLER'                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN MAR/2020 SPEC-39079 FREEZE INACTIVE CLIENT CODES                         
*                                                                               
* SMUR 06/22/18 SPEC-24453 ADD DELETE/RESTORE FOR BILLADR (18.3)                
*                                                                               
* BOBY 08/11    ADD DROP DOWN MENU CODE DEFINITIONS                             
*                                                                               
* KWAN 11/03/10 SAVE PUB LOCK SWITCH FROM PUB RECORD INTO SVPUBLSW              
*                                                                               
* BPLA 05/09    BFORM REPORTING                                                 
*                                                                               
* BOBY 09/00/08 NEW RECORD, DCOM                   - T41C39                     
*                                                                               
* BOBY 06/00/08 NEW RECORD, EXCHANGE               - T41C38                     
*                                                                               
* BPLA 05/30/08 REMOVE ACCESS TO BILLFORM - THEY MUST USE BFORM NOW             
*                                                                               
* SMYE 02/05/08 FIX GLOBBER DELETE CALL AT LABEL SFMPF12X                       
*                                                                               
* KWAN 07/03/07 FOR ADBUYER UPLOADS, DISPLAY ERROR FOR OLD SECURITY             
*                                                                               
* BOBY 03/00/07 NEW RECORD, CENTRAL PUBFILE - CPUB - T41C37                     
*                                                                               
* KWAN 10/10/06 INITIALIZE PARAMETERS FOR GENERALIZED UPLOAD (GENNEW)           
*                                                                               
* BOBY 09/09/06 NEW RECORD, STANDARD CUSTOM COLUMN (STDCOL) - T41C22            
*                                                                               
* SMYE 08/25/06 NEW RECORD, PURCHASE ORDER NUMBER (PO#) - T41C36                
*                                                                               
* SMYE 07/22/05 NEW RECORDS, SRVCCLST (T41C41) AND SRACCLST (T41C42)            
*                                                                               
* KWAN 06/27/05 NEW RECORD, CLIADV (T41C2E)                                     
*                                                                               
* KWAN 11/11/04 NEW RECORD, SUBMEDIA (T41C34)                                   
*                                                                               
* SMYE 09/07/04 ALLOW CLT, PRD & EST IF DDS TERMINAL                            
*                                                                               
* SMYE  08/04   DISALLOW CLT, PRD & EST BY POINTING TO DIFFERENT RECACT         
*               TABLE IF "NEW SECURITY" (TWA+4) NOT IN USE                      
*                                                                               
* KWAN 06/02/04 LEGAL WARNING FIELD ACCESS                                      
*                                                                               
* BOBY 05/28/04 ACCLIST                                                         
*                                                                               
* BOBY 05/03/04 VCCLIST TO USE PUBFILE FOR RECORDS                              
*                                                                               
* SMYE 04/14/04 GET QSORT CORE-RESIDENT ADDRESS AND MODIFY RECACT TABLE         
*               FOR NEW PPS SECURITY, ETC.                                      
*                                                                               
* KWAN 09/04/02 CONVERT CLT/PRD/EST/PGEST/GFEST FROM FIL TO SFM                 
*                                                                               
* SMYE 05/02    NEW LIMIT ACCESS SECURITY                                       
*                                                                               
* SMYE 09/24/01 IN VCLT, SAVE PCLTOFF IN SVCPROF+30                             
*                                                                               
* SMYE 08/01    NEW RECORD CUSTCOL ("CUSTOM COLUMNS")                           
*                                                                               
* SMYE 06/01    NEW RECORD STDSPACE ("STANDARD SPACE DESCRIPTION")              
*                                                                               
* SMYE 02/01    NEW RECORD FOR "SPECIAL CHARGES"                                
*                                                                               
* SMYE 09/00    NEW RECORD FOR USER COMMENT                                     
*                                                                               
* SMYE 12/98    NEW RECORD FOR SRDS LINKAGE                                     
*                                                                               
* BPLA 10/98    ACTIVATE BILLING FORMULA RECORDS                                
*                                                                               
* SMYE 11/97    USED CIRC "SELECT" LOGIC FOR CUREC AND SPACE RECORDS            
*                                                                               
* SMYE 10/97    GETINS MADE CORE-RESIDENT                                       
*                                                                               
* SMYE 08/97    USED CIRC "SELECT" LOGIC FOR PUBLIST (GENCON MSG FIX)           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41C00   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T41C00,RA,RR=R2,CLEAR=YES                              
*                                                                               
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
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    RA,BASERA                                                        
*                                                                               
         L     R2,SYSPARMS                                                      
         L     R2,4(R2)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R2                                                   
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   SFM_20              THEN SKIP DDLINK STUFF                       
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(R2)                                         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,10,GLVDLUWF                             
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         BNE   SFM_20                                                           
*                                                                               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
*                                                                               
         L     RF,CGENNEW-COMFACSD(R2)                                          
         MVI   GENSTAT6,GES$UPLD   ENABLE DDLINK AUTO UPLOAD FOR                
         MVI   MODE,TESTGLOB       PRESTO JOB MAINTENANCE                       
         GOTO1 (RF),DMCB,SPOOLD                                                 
         DC    H'0'                GENNEW SHOULD RETURN TO MONITOR              
*                                                                               
SFM_20   L     RF,SYSPARMS                                                      
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
*                                                                               
* SET FOR RETURN TO MATCH                                                       
*                                                                               
         MVC   8(17,RE),=CL17'=SW '                                             
*                                                                               
         B     EXIT                RETURN WITH SWAP                             
*                                                                               
         DROP  RF                                                               
*                                                                               
SFMPF12X DS    0H                                                               
*                                                                               
* CHECK IF THIS IS A GLOBBER CALL                                               
*                                                                               
         MVI   TRANSSW,0           INIT TRANSFER SWITCH                         
*                                                                               
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    SFMGLBX                                                          
*                                                                               
* GET XCTL ELEM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL                           
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
         GOTO1 (RF),DMCB,=C'DELE'  DELETE TRANSFER ELM                          
*                                                                               
         MVI   TRANSSW,C'Y'        INDICATE WE ARE IN MIDST OF TRANSFER         
*                                                                               
         L     R2,SYSPARMS                                                      
         L     R2,4(R2)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R2                                                   
*                                                                               
* GET RECORD                                                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONRECH,,GLVXREC                          
         GOTO1 (RF),(R1),=C'DELE'  DELETE RECORD                                
*                                                                               
* GET ACTION                                                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONACTH,,GLVXACT                          
         GOTO1 (RF),(R1),=C'DELE'  DELETE ACTION                                
*                                                                               
* GET KEY                                                                       
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONKEYH,,GLVPRKEY                         
         GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
SFMGLBX  DS    0H                                                               
*                                                                               
* ANALYZE INPUT TO SEE IF INTERVENE ON SELECT ACTIONS ARE NEEDED                
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,4(RF)            ESTABLISH SCREEN                             
         USING CONHEADH-64,RF                                                   
*                                                                               
         BRAS  RE,CKRECSCR         CK FOR REC SCR (CLT/PRD/EST ETC...)          
         BE    GOGENCON                                                         
*                                                                               
         CLI   TWASCR,X'EC'        LOOKING FOR CUREC LIST                       
         BNE   CURECN                                                           
*                                                                               
         LA    R2,FSLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
CURECLP  DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
CURECCN  DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   CURECLP                                                          
*                                                                               
         B     GOGENCON                                                         
*                                                                               
CURECN   DS    0H                                                               
         CLI   TWASCR,X'FC'        LOOKING FOR CUREC MAINTENANCE                
         BNE   CURECX                                                           
*                                                                               
         CLC   =C'CU',CONREC       DOING 'CUREC' MAINTENANCE                    
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
CURECX   DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C8'        LOOKING FOR PUBLIST LIST                     
         BNE   PUBLN                                                            
*                                                                               
         LA    R2,PLLSEL1H         POINT TO FIRST SELECT FIELD                  
*                                                                               
PUBLLP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
PUBLCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   PUBLLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
PUBLN    DS    0H                                                               
         CLI   TWASCR,X'D8'        LOOKING FOR PUBLIST MAINTENANCE              
         BNE   PUBLX                                                            
*                                                                               
         CLC   =C'PUBL',CONREC     DOING 'PUBLIST' MAINTENANCE                  
         BNE   *+10                                                             
*                                                                               
PUBLTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
PUBLX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'9C'        LOOKING FOR MENU LIST                        
         BNE   MENUN                                                            
*                                                                               
         LA    R2,SCLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
MENULP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
MENUCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   MENULP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
MENUN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'9D'        LOOKING FOR MENU MAINTENANCE                 
         BNE   MENUX                                                            
*                                                                               
         CLC   =C'CUSTMENU',CONREC     DOING 'CUSTMENU' MAINT                   
         BE    *+10                                                             
         CLC   =C'STDMENU',CONREC      DOING 'STDMENU'  MAINT                   
         BNE   *+10                                                             
*                                                                               
MENUTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
MENUX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'A7'        LOOKING FOR VCCLIST LIST                     
         BNE   VCCLN                                                            
*                                                                               
         LA    R2,SCLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
VCCLLP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
VCCLCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   VCCLLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
VCCLN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'A6'        LOOKING FOR VCCLIST MAINTENANCE              
         BNE   VCCLX                                                            
*                                                                               
         CLC   =C'VCCL',CONREC     DOING 'VCCLIST' MAINTENANCE                  
         BNE   *+10                                                             
*                                                                               
VCCLTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
VCCLX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'A9'        LOOKING FOR ACCLIST LIST                     
         BNE   ACCLN                                                            
*                                                                               
         LA    R2,SCLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
ACCLLP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
ACCLCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   ACCLLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
ACCLN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'A8'        LOOKING FOR ACCLIST MAINTENANCE              
         BNE   ACCLX                                                            
*                                                                               
         CLC   =C'ACCL',CONREC     DOING 'ACCLIST' MAINTENANCE                  
         BNE   *+10                                                             
*                                                                               
ACCLTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
ACCLX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'DD'        LOOKING FOR PO# MAINTENANCE                  
         BNE   PO#LX                                                            
*                                                                               
         CLC   =C'PO#',CONREC      DOING 'PO#' MAINTENANCE                      
         BNE   *+10                                                             
*                                                                               
PO#LTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
PO#LX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C3'        LOOKING FOR SRVCCLST LIST                    
         BNE   SRVLN                                                            
*                                                                               
         LA    R2,SCLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
SRVLLP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
SRVLCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   SRVLLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
SRVLN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C2'        LOOKING FOR SRVCCLST MAINTENANCE             
         BNE   SRVLX                                                            
*                                                                               
         CLC   =C'SRVC',CONREC     DOING 'SRVCCLST' MAINTENANCE                 
         BNE   *+10                                                             
*                                                                               
SRVLTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
SRVLX    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'AC'        LOOKING FOR SRACCLST LIST                    
         BNE   SRALN                                                            
*                                                                               
         LA    R2,SCLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
SRALLP   DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
SRALCN   DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   SRALLP                                                           
*                                                                               
         B     GOGENCON                                                         
*                                                                               
SRALN    DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'C6'        LOOKING FOR SRACCLST MAINTENANCE             
         BNE   SRALX                                                            
*                                                                               
         CLC   =C'SRAC',CONREC     DOING 'SRACCLST' MAINTENANCE                 
         BNE   *+10                                                             
*                                                                               
SRALTST  CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
SRALX    DS    0H                                                               
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
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   CIRCLLP                                                          
*                                                                               
         B     GOGENCON                                                         
*                                                                               
CIRCLN   DS    0H                                                               
         CLI   TWASCR,X'D0'        LOOKING FOR CIRC MAINTENANCE                 
         BNE   CIRCX                                                            
*                                                                               
         CLC   =C'CI',CONREC       DOING 'CIRC' MAINTENANCE                     
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
CIRCX    DS    0H                                                               
         CLI   TWASCR,X'C5'        LOOKING FOR SPACE LIST                       
         BNE   SPACLN                                                           
*                                                                               
         LA    R2,SPLSELH          POINT TO FIRST SELECT FIELD                  
*                                                                               
SPACLLP  DS    0H                                                               
         CLI   8(R2),C'S'          CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
*                                                                               
SPACLCN  DS    0H                                                               
         BAS   RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   SPACLLP                                                          
*                                                                               
         B     GOGENCON                                                         
*                                                                               
SPACLN   DS    0H                                                               
         CLI   TWASCR,X'D5'        LOOKING FOR SPACE MAINTENANCE                
         BNE   SPACEX                                                           
*                                                                               
         CLC   =C'SP',CONREC       DOING 'SPACE' MAINTENANCE                    
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
SPACEX   DS    0H                                                               
         CLI   TWASCR,X'D2'        LOOKING FOR SPECIAL CHARGE MAINT             
         BNE   SPLCGX                                                           
*                                                                               
         CLC   =C'CH',CONREC       DOING 'SPLCHG' MAINTENANCE                   
         BNE   *+10                                                             
         CLC   =C'SEL',CONACT      CHANGE SELECT TO 'CHA'                       
         BNE   GOGENCON                                                         
*                                                                               
         B     CHASEL              CHANGE 'SELECT' TO 'CHANGE'                  
*                                                                               
SPLCGX   DS    0H                                                               
         B     GOGENCON                                                         
*                                                                               
CHASEL   DS    0H                  SET ACTION TO 'CHANGE'                       
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            A(TIOB)                                      
         USING TIOBD,RE                                                         
*                                                                               
         CLI   TIOBAID,12          SKIP IF PF12 OR PF24                         
         BE    *+8                                                              
         CLI   TIOBAID,24                                                       
         BE    CHASELX                                                          
*                                                                               
* CHANGE ACTION                                                                 
*                                                                               
         MVC   CONACT(6),=C'CHANGE'                                             
         MVI   CONACTH+5,6                                                      
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
CHASELX  DS    0H                                                               
*                                                                               
GOGENCON DS    0H                                                               
         GOTO1 GENCON,DMCB,(R8)    GOTO GENERAL CONTROLLER                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETIONUM DS    0H                  SETTING WHICH AIO TO USE                     
         MVC   AIO,AIO1                                                         
         CLI   USEIONUM,1                                                       
         BNH   SETIOX                                                           
         MVC   AIO,AIO2                                                         
         CLI   USEIONUM,2                                                       
         BE    SETIOX                                                           
         MVC   AIO,AIO3                                                         
         CLI   USEIONUM,3                                                       
         BE    SETIOX                                                           
         DC    H'0'                                                             
SETIOX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SYSINIT  NTR1                                                                   
*                                                                               
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,VDUMMY                                                        
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
         MVC   VUSCAN,44(R1)                                                    
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)                                                        
         USING COMFACSD,R1                                                      
*                                                                               
         MVC   VRECUP,CRECUP       USED BY "OLDER" PRGMS                        
         MVC   VGLOBBER,CGLOBBER   SAVE GLOBBER ADDRESS                         
         MVC   SWITCH,CSWITCH      RESET BY GENCON BUT WE NEED IT               
         MVC   GETFACT,CGETFACT    RESET BY GENCON BUT WE NEED IT               
*                                                                               
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
*                                                                               
         MVI   DMCB+7,QGETINS                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETINS,DMCB        SAVE GETINS ADDRESS (CORE RES)               
*                                                                               
         MVI   DMCB+7,QPUBVAL                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBVAL,DMCB        SAVE PUBVAL ADDRESS (CORE RES)               
*                                                                               
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QSORT,DMCB          SAVE QSORT ADDRESS (CORE RES)                
*                                                                               
* SET SYSTEM DEPENDENT VALUES                                                   
*                                                                               
         MVI   NTWA,X'01'          TELL GENCON CON 1 TWA IS NEEDED              
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
*                                                                               
         MVI   USEIONUM,1          DEFAULT TO AIO1                              
*                                                                               
         OI    GENSTAT1,USKYMRG                                                 
         OI    GENSTAT2,DISTHSPG   STAY ON SAME LIST PAGE                       
         OI    GENSTAT4,NEWSCRM    PASS BACK NEWSCR MODE                        
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,4(RF)            ESTABLISH SCREEN                             
         USING CONHEADH-64,RF                                                   
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         BNZ   SYS20                                                            
*                                                                               
         OC    TWASAGN,TWASAGN     TEST NEW SECURITY                            
         BNZ   SYS20               HAS "NEW SECURITY"                           
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL ?                               
         BE    SYS20               YES - ALLOW ACCESS TO ALL RECORDS            
*                                                                               
*  DOES NOT HAVE NEW SECURITY SO DISALLOW CLT, PRD, EST ACCESS                  
         LHI   R1,RECACTX-T41C00   REC/ACT DIRECTORY (NO CLT,PRD, EST)          
         AR    R1,RB                                                            
         ST    R1,ARECACT          POINT TO "NO CLT" RECACT DIRECTORY           
*                                                                               
SYS20    DS    0H                                                               
*                                                                               
         CLC   =C'VCCLIST',CONREC  IF EIO VENDOR CONTCT LIST                    
         BE    SYS25                                                            
         CLC   =C'SRVCCLST',CONREC  IF ESR VENDOR CONTCT LIST                   
         BNE   SYS30                                                            
SYS25    DS    0H                                                               
         OI    GENSTAT5,VRAHOOK    GET MODE CALL AFTER REC/ACT VALID            
         OI    GENSTAT3,MULTFILS   PROGRAM USES MULTIPLE FILES                  
*                                                                               
SYS30    DS    0H                                                               
         CLC   =C'SRDS',CONREC     ID SRDS RECORD TYPE?                         
         BE    *+10                                                             
         CLC   =C'LOADEDR',CONREC  LOAD EDRFILE TO CTFILE?                      
         BNE   *+12                                                             
         MVI   USEIO,C'Y'          INDICATE IT DOES OWN I/O                     
         MVI   IOOPT,C'Y'          OVERLAY DOES I/O                             
*                                                                               
         CLC   =C'STDCOL',CONREC   STANDARD COLUMNS ON GENFILE                  
         BE    *+10                                                             
         CLC   =C'STDMENU',CONREC  STANDARD COLUMNS ON GENFILE                  
         BNE   SYS40                                                            
*                                                                               
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'     USUALLY PRINT FILE                           
         MVC   SYSFIL,=C'GENFILE '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
SYS40    DS    0H                                                               
*                                                                               
         CLC   =C'CPUB',CONREC     CENTRAL PUB FILE                             
         BNE   SYS45                                                            
*                                                                               
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'     USUALLY PRINT FILE                           
         MVC   SYSFIL,=C'GENFILE '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
*        GET CONNECTING SYSTEM NUMBER                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0      SAVE SE NUM                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   CONNSYS,FASYS       SET CONNECTING SYS NUM                       
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH                       
*                                                                               
         DROP  R1                                                               
*                                                                               
SYS45    DS    0H                                                               
*                                                                               
         CLC   =C'EXC',CONREC      EXCHANGE RECORD                              
         BNE   SYSEXCX                                                          
*                                                                               
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'     USUALLY PRINT FILE                           
         MVC   SYSFIL,=C'GENFILE '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
*        GET CONNECTING SYSTEM NUMBER                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0      SAVE SE NUM                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   CONNSYS,FASYS       SET CONNECTING SYS NUM                       
*                                                                               
******   GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
******   CLI   DMCB+4,0                                                         
******   B     *+6                                                              
******   DC    H'0'                MUST BE ABLE TO SWITCH                       
******                                                                          
         DROP  R1                                                               
*                                                                               
SYSEXCX  DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
*                                                                               
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VAGY     DS    0H                  GET AGY REC IN MEDIA VALIDATION              
         L     RF,ATWA                                                          
         USING T41CFFD,RF                                                       
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    VAGYSECX                                                         
*                                                                               
         LH    R6,=Y(SECBLK-T41CFFD)                                            
         AR    R6,RF                                                            
         ST    R6,ASECBLK                                                       
         DROP  RF                                                               
*                                                                               
         L     R0,ASECBLK          CLEAR SECRET BLOCK                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*                                                                               
* CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE                      
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         MVI   TRFAGSW,0           CLEAR                                        
         BAS   RE,CKTRAFID         TRAFFIC ID?                                  
         BNE   VAGYSECX            NO                                           
         MVI   TRFAGSW,C'Y'        YES - TRAFFIC ID SIGN-ON                     
*                                                                               
VAGYSECX DS    0H                                                               
*                                                                               
*        VCCLIST  RECORD READS RECORDS FROM PUBFILE                             
*        SRVCCLST RECORD READS RECORDS FROM PUBFILE                             
*                                                                               
         CLI   MODE,VALRA          SKIP UNLESS AFTER RECORD/ACT VAL             
         BNE   VAGYVCCX                                                         
*                                                                               
         L     RF,ATWA                                                          
         USING T41CFFD,RF                                                       
*                                                                               
         CLC   =C'VCCLIST',CONREC    SKIP UNLESS EIO VENDOR CC LIST REC         
         BE    VAGYFILX                                                         
         CLC   =C'SRVCCLST',CONREC            OR ESR VENDOR CC LIST REC         
         BNE   VAGYVCCX                                                         
*                                                                               
VAGYFILX DS    0H                                                               
         MVC   SYSFIL,=C'PUBFILE '                                              
         MVC   SYSDIR,=C'PUBDIR  '                                              
*                                                                               
         DROP  RF                                                               
*                                                                               
VAGYVCCX DS    0H                                                               
*                                                                               
         BRAS  RE,CKRECSCR         CK IF SCR FLDS NEED TO BE MODIFIED           
*                                                                               
VAGYX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1                      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         L     R0,AIO2                                                          
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO2                                                          
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         L     RF,ATWA                                                          
         MVC   CTIKNUM,10(RF)      ID NUMBER                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VMED     DS    0H                  VALIDATE MEDIA CODE                          
         XC    SVKEY,SVKEY         CLEAR SAVED KEY AREA                         
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
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         USING PAGYELEM,R6                                                      
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         MVC   SVUSER,USERNAME     SAVE FOR FUTURE REF                          
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   MEDNM,PAGYMED       MEDIA NAME                                   
         MVC   SVAGYPF,PAGYPROF    SAVE AGENCY PROFILE                          
         MVC   WNATION,PAGYNAT     NATIONALITY                                  
         MVC   SVAGPINI,PAGYPINI   2 BYTES BINARY RFP ID NUMBER                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        ACC AGY LIST ELEM                            
         BAS   RE,GETEL                                                         
         BNE   VMED40                                                           
         ZIC   R1,1(R6)                                                         
         AHI   R1,-3               2 FROM ELEM OVERHEAD AND 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVACCAGY(0),2(R6)   EXECUTED                                     
*                                                                               
VMED40   L     R6,AIO                                                           
         MVI   ELCODE,X'05'        CONTROL FILE ID ELEM                         
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         MVC   SVCTAGY,2(R6)                                                    
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VCLT     DS    0H                  VALIDATE CLIENT                              
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
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        RUNNING OFFLINE REPORT ?                     
         BE    VCLT50              YES - SKIP LIMIT ACCESS                      
*                                                                               
         L     RF,ATWA                                                          
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    VCLT50                                                           
*                                                                               
         MVC   TSTOFF,PCLTOFF      SV PCLTOFF FOR LIMIT ACCESS TEST             
*                                                                               
         CLI   TRFAGSW,C'Y'        TRAFFIC ID SIGN-ON ?                         
         BNE   VCLT20              NO                                           
*                                                                               
* SEE IF TRAFFIC OFFICE EXISTS                                                  
*                                                                               
         L     R6,AIO              POINT TO CLIENT REC                          
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BAS   RE,GETEL                                                         
         BNE   VCLT20              NO TRAFFIC OFFICE FOUND                      
*                                                                               
* REPLACE PCLTOFF SAVED IN TSTOFF WITH CLIENT TRAFFIC OFFICE CODE               
*                                                                               
         MVC   TSTOFF,2(R6)                                                     
*                                                                               
VCLT20   DS    0H                  LIMIT ACCESS TESTING                         
         XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
*                                                                               
         L     RF,ATWA                                                          
         L     R6,AIO              POINT TO CLT REC                             
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,TSTOFF       CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RF)                                                  
         MVC   OFCSECD,ASECBLK     A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 CALLOV,DMCB         GET OFFICER ADDRESS                          
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    VCLT50              OK                                           
*                                                                               
ACCERR   MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
         B     TRAPERR                                                          
*                                                                               
VCLT50   MVC   CLTNM,PCLTNAME                                                   
         MVC   SVCPROF,PCLTPROF                                                 
         MVC   SVCLSTAT,PCLTSTAT   CLT STATUS BYTE                              
*                                                                               
* NOTE USE OF SVCPROF+30 TO STORE PCLTOFF                                       
*                                                                               
         MVC   SVCPROF+30(1),PCLTOFF                                            
*                                                                               
* READ FOR F0 PROFILE AND SAVE IN F0PROF                                        
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   WORK+00(12),SPACES                                               
         MVC   WORK+00(04),=C'P0F0'                                             
         MVC   WORK+04(02),AGENCY                                               
         MVC   WORK+06(01),PCLTKMED                                             
         MVC   WORK+07(03),PCLTKCLT                                             
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,F0PROF,DATAMGR                                 
*                                                                               
         DROP  R6                  DONE USING PCLTRECD                          
*                                                                               
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        USER DEFINITION FLDS ELEM CODE               
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVP1USER(SVULNQ),2(R6)                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
TSTOFF   DS    X                   FOR LIMIT ACCESS OFFICE TEST                 
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPRD     DS    0H                  VALIDATE PRODUCT                             
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
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VEST     DS    0H                  VALIDATE ESTIMATE                            
         GOTO1 VGETFLD                                                          
         MVI   ERROR,INVEST                                                     
         STH   R0,BEST             CONTAINS BINARY AFTER GETFLD                 
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
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
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VDIV     DS    0H                  VALIDATE DIVISION                            
         XC    QDIV,QDIV                                                        
         XC    DIVNM,DIVNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   QDIV,FLD                                                         
         CLC   QDIV,=C'000'                                                     
         BE    VDIV5                                                            
         LTR   R0,R0                                                            
         BZ    VDIVERR                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QDIV,DUB                                                         
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
         BNE   EXIT                                                             
*                                                                               
* LEAVES DIVNM AS ZEROS IF DIVISION NOT FOUND                                   
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREG     DS    0H                  VALIDATE REGION                              
         XC    QREG,QREG                                                        
         XC    REGNM,REGNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         LTR   R0,R0                                                            
         BZ    VREGERR                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QREG,DUB                                                         
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
         BNE   EXIT                                                             
*                                                                               
* LEAVES REGNM AS ZEROS IF REGION NOT FOUND                                     
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VDST     DS    0H                  VALIDATE DISTRICT                            
         XC    QDST,QDST                                                        
         XC    DSTNM,DSTNM                                                      
         MVI   FLDOPT,C'Y'         SET OPTIONAL                                 
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         LTR   R0,R0                                                            
         BZ    VDSTERR                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QDST,DUB                                                         
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
         BNE   EXIT                                                             
*                                                                               
* LEAVES DSTNM AS ZEROS IF DISTRICT NOT FOUND                                   
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VSTD     DS    0H                  VALIDATE START DATE                          
         GOTO1 VGETFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QSTART                                           
         OC    DMCB(4),DMCB        YYMMDD?                                      
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(3,BSTART)                                    
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VEND     DS    0H                  VALIDATE END DATE                            
         GOTO1 VGETFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,FLD,QEND                                             
         OC    DMCB(4),DMCB        YYMMDD?                                      
         BNZ   *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   QEND,QSTART         SEL END/SEL START                            
         BNL   *+12                                                             
         MVI   ERROR,INVDTSEQ      END DATE BEFORE START DATE                   
         B     TRAPERR                                                          
         GOTO1 DATCON,DMCB,QEND,(3,BEST)                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VDTYP    DS    0H                  VALIDATE DATE TYPE                           
         MVI   DATETYPE,C'I'       INSERTION DATE (DEFAULT)                     
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPUB     DS    0H                  VALIDATE PUBLICATION                         
         CLI   8(R2),C'='          IF INPUT STARTS WITH '='                     
         BNE   VPUBSCHX                                                         
*                                                                               
* NEED TO DO NAME SEARCHING DISPLACEMENT OF FIELD INTO TWA                      
*                                                                               
         S     R2,ATWA                                                          
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',ATWA),ACOMFACS,       +        
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
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
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
         MVC   SVPUBLSW,PUBLOCSW   PUB LOCK SWITCH                              
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VADC     DS    0H                  VALIDATE ADCODE                              
         XC    QADCODE,QADCODE                                                  
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPER     DS    0H                  VALIDATE PERIOD                              
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SET CURSOR TO ERROR POSITION AND EXIT TO ERROR ROUTINE                        
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                               
*                                                                               
* INPUTS       R2     = A(FIELD HEADER)                                         
*              FLDOPT = 1 FIELD IS OPTIONAL                                     
*                                                                               
* OUTPUTS      FLDH     CONTAINS FIELD HEADER                                   
*              FLD      FIELD DATA SPACE FILLED                                 
*              R0       BINARY VALUE IF FIELD NUMERIC                           
*              R1       FIELD LENGTH                                            
*              CONDITION CODE ZERO IF R1=0                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
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
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RELO     DS    A                                                                
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QLINUP)                                                      
         DC    AL1(QPRHELP)                                                     
         DC    AL1(QPRVAL)                                                      
         DC    AL1(QOFFICER)                                                    
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DIRECTORY OF RECORDS AND ACTIONS                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
* X'01'  ENTRIES ARE AVAILABLE RECORDS                                          
* CL8    EXPANDED RECORD NAME                                                   
* CL1    RECORD NUMBER                                                          
* CL1    PHASE NUMBER FOR DATA DICTIONARY                                       
* CL1    PHASE NUMBER FOR HELP SCREEN                                           
*                                                                               
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'USERP   ',AL1(02),X'00C2'                                
         DC    X'01',C'BUY     ',AL1(03),X'00C2'                                
         DC    X'01',C'AOR     ',AL1(04),X'00CF'                                
         DC    X'01',C'OAN     ',AL1(05),X'00CF'                                
         DC    X'01',C'DIVISION',AL1(07),X'00CF'                                
         DC    X'01',C'REGION  ',AL1(08),X'00CF'                                
*                                                                               
         DC    X'01',C'DISTRICT',AL1(09),X'00CF'                                
         DC    X'01',C'DSTREC  ',AL1(09),X'00CF'                                
*                                                                               
         DC    X'01',C'CLRST   ',AL1(10),X'00CF'                                
         DC    X'01',C'COST    ',AL1(11),X'00CF'                                
         DC    X'01',C'FSI     ',AL1(12),X'00CF'                                
         DC    X'01',C'UDEF    ',AL1(13),X'00CF'                                
         DC    X'01',C'CUREC   ',AL1(14),X'00CF'                                
         DC    X'01',C'ISSUE   ',AL1(15),X'00CF'                                
         DC    X'01',C'CIRC    ',AL1(17),X'00CF'                                
*                                                                               
         DC    X'01',C'EDR     ',AL1(18),X'00CF'                                
         DC    X'01',C'2EDR    ',AL1(18),X'00CF'                                
*                                                                               
         DC    X'01',C'LOADEDR ',AL1(19),X'00CF'                                
*                                                                               
         DC    X'01',C'SPACE   ',AL1(20),X'00CF'                                
         DC    X'01',C'SPACE   ',AL1(20),X'00CF'                                
*                                                                               
*OLD*    DC    X'01',C'BILLFORM',AL1(21),X'00CF'    BILLING FORMULA             
         DC    X'01',C'BUDGET  ',AL1(22),X'00CF'    BUDGET                      
         DC    X'01',C'CGROUP  ',AL1(34),X'00CF'    CLIENT GROUPS               
         DC    X'01',C'CGASSIGN',AL1(35),X'00CF'    CLIENT ASSIGNMENTS          
         DC    X'01',C'CGDEF   ',AL1(36),X'00CF'    CLIENT GROUP DEF            
         DC    X'01',C'PGROUP  ',AL1(37),X'00CF'    PRODUCT GROUPS              
         DC    X'01',C'PGASSIGN',AL1(38),X'00CF'    PRODUCT ASSIGNMENTS         
         DC    X'01',C'PGDEF   ',AL1(39),X'00CF'    PRODUCT GROUP DEF           
*                                                                               
         DC    X'01',C'PBGROUP ',AL1(40),X'00CF'    PUB GROUPS                  
         DC    X'01',C'PUBGROUP',AL1(40),X'00CF'    PUB GROUPS                  
*                                                                               
         DC    X'01',C'PBASSIGN',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBGASSI',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBASSIG',AL1(41),X'00CF'    PUB ASSIGNMENTS             
*                                                                               
         DC    X'01',C'PBDEF   ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PUBGDEF ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PBGDEF  ',AL1(42),X'00CF'    PUB GROUP DEF               
*                                                                               
         DC    X'01',C'PUBLIST ',AL1(43),X'00CF'    PUB LIST                    
         DC    X'01',C'JWTCPP  ',AL1(44),X'00CF'    JWT CPP PUT TO Q            
         DC    X'01',C'SRDS    ',AL1(45),X'00CF'    SRDS SIZING                 
         DC    X'01',C'UCOMM   ',AL1(46),X'00CF'    USER COMMENT                
         DC    X'01',C'CHARGE  ',AL1(47),X'00CF'    SPECIAL CHARGES             
         DC    X'01',C'MSHCPP  ',AL1(48),X'00CF'    MSHR CPP PUT TO Q           
         DC    X'01',C'STDSPACE',AL1(49),X'00CF'    STANDARD SPACE DESC         
         DC    X'01',C'CUSTCOL ',AL1(50),X'00CF'    CUSTOM COLUMNS              
         DC    X'01',C'PGEST   ',AL1(51),X'00CF'    PGEST                       
         DC    X'01',C'GFEST   ',AL1(52),X'00CF'    GFEST                       
*                                                                               
         DC    X'01',C'CLT     ',AL1(53),X'00CF'    CLIENT                      
         DC    X'01',C'CLIENT  ',AL1(53),X'00CF'    CLIENT                      
*                                                                               
         DC    X'01',C'CL2     ',AL1(54),X'00CF'    CLIENT 2 (OPTIONS)          
         DC    X'01',C'CLIENT2 ',AL1(54),X'00CF'    CLIENT 2 (OPTIONS)          
*                                                                               
         DC    X'01',C'PRD     ',AL1(55),X'00CF'    PRODUCT                     
         DC    X'01',C'PRODUCT ',AL1(55),X'00CF'    PRODUCT                     
*                                                                               
         DC    X'01',C'PRBILL  ',AL1(56),X'00CF'    PRD BILL                    
         DC    X'01',C'PRDBILL ',AL1(56),X'00CF'    PRD BILL                    
*                                                                               
         DC    X'01',C'EST     ',AL1(57),X'00CF'    ESTIMATE                    
         DC    X'01',C'ESTIMATE',AL1(57),X'00CF'    ESTIMATE                    
*                                                                               
         DC    X'01',C'ESBILL  ',AL1(58),X'00CF'    EST BILL                    
         DC    X'01',C'ESTBILL ',AL1(58),X'00CF'    EST BILL                    
*                                                                               
         DC    X'01',C'ES2     ',AL1(59),X'00CF'    EST 2 (OPTIONS)             
         DC    X'01',C'ESTIMAT2',AL1(59),X'00CF'    EST 2 (OPTIONS)             
*                                                                               
         DC    X'01',C'LEGAL   ',AL1(60),X'00CF'    LEGAL WARNINGS              
         DC    X'01',C'LEGALW  ',AL1(60),X'00CF'    LEGAL WARNINGS              
*                                                                               
         DC    X'01',C'VCCLIST ',AL1(61),X'00CF'    EIO VENDOR CC LIST          
*                                                                               
         DC    X'01',C'ACCLIST ',AL1(62),X'00CF'    EIO AGENCY CC LIST          
*                                                                               
         DC    X'01',C'SETUP   ',AL1(63),X'00CF'    SETUP (WAS SCHEMA)          
*                                                                               
         DC    X'01',C'SUBMEDIA',AL1(64),X'00CF'    SUBMEDIA                    
*                                                                               
         DC    X'01',C'BFORM   ',AL1(65),X'00CF'    NEW BILL FORM MAINT         
*                                                                               
         DC    X'01',C'CLIADV  ',AL1(66),X'00CF'    CLIADV                      
         DC    X'01',C'CLTADV  ',AL1(66),X'00CF'    CLTADV                      
*                                                                               
*                                                                               
         DC    X'01',C'SRVCCLST',AL1(67),X'00CF'    ESR VENDOR CC LIST          
*                                                                               
         DC    X'01',C'SRACCLST',AL1(68),X'00CF'    ESR AGENCY CC LIST          
*                                                                               
         DC    X'01',C'PO#     ',AL1(69),X'00CF'    PURCHASE ORDER NO.          
*                                                                               
         DC    X'01',C'STDCOL  ',AL1(70),X'00CF'    STANDARD CUST COLS          
*                                                                               
*******  DC    X'01',C'CPUB    ',AL1(71),X'00CF'    CENTRAL PUBFILE             
*                                                                               
         DC    X'01',C'EXCHANGE',AL1(72),X'00CF'    EXCHANGE                    
*                                                                               
         DC    X'01',C'DCOMMENT',AL1(73),X'00CF'    EXCHANGE                    
*                                                                               
         DC    X'01',C'CUSTMENU',AL1(74),X'00CF'    CUST DROP DOWN MENU         
*                                                                               
         DC    X'01',C'STDMENU ',AL1(75),X'00CF'    STD  DROP DOWN MENU         
*                                                                               
         DC    X'01',C'AUTOPAY ',AL1(76),X'00CF'    AUTOPAY                     
*                                                                               
         DC    X'01',C'BILLADR ',AL1(77),X'00CF'    BILL ADDRESS REC            
*                                                                               
         DC    X'01',C'CLFRZ   ',AL1(78),X'00CF'    FRZ INACTIVE CLT            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* X'02'  ENTRIES ARE AVAILABLE ACTIONS                                          
* CL8    EXPANDED ACTION NAME                                                   
* CL1    ACTION NUMBER                                                          
* CL1    ACTION EQUATE                                                          
* CL1    SPARE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        PHASE USED UP BY SYSTEM SO FAR (PRSFMXX)                               
*        P = PROGRAM, S= SCREEN, . = FREE                                       
*                                                                               
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     P  .  .  P  P  P  .  P  P  P  P  P  P  P  P  P                  
*        1X     P  P  P  P  P  P  P  P  P  P  P  P  P  P  P  P                  
*        2X     P  P  P  P  P  P  P  P  P  P  P  P  P  P  P  .                  
*        3X     P  P  P  P  P  P  P  P  P  P  .  .  .  .  .  .                  
*        4X     P  P  P  P  P  P  .  .  .  .  .  .  .  .  .  .                  
*        5X     .  P  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        6X     .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        7X     .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        8X     .  .  .  .  .  .  .  .  .  .  .  .  S  S  P  P                  
*        9X     .  .  .  .  .  .  .  .  .  S  S  S  S  S  S  S                  
*        AX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        BX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        CX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        DX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        EX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        FX     .  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         DC    X'03',AL1(02,01),X'F2510000C0',C'    ' USERP     ADD             
         DC    X'03',AL1(02,02),X'F2510000C0',C'    '           CHANGE          
         DC    X'03',AL1(02,03),X'F2510000C0',C'    '           DISPLAY         
         DC    X'03',AL1(02,04),X'F2510000C0',C'    '           DELETE          
         DC    X'03',AL1(02,05),X'F2510000C0',C'    '           SELECT          
         DC    X'03',AL1(02,06),X'F2510000C0',C'    '           RESTORE         
         DC    X'03',AL1(02,10),X'E2510051F8',C'USSF'           LIST            
         DC    X'03',AL1(02,12),X'E251005178',C'USSF'           REPORT          
*                                                                               
         DC    X'03',AL1(03,13),X'F3030003F8',C'BXBX' BUY       COPY            
         DC    X'03',AL1(03,14),X'F3030003F8',C'BXBX'           MOVE            
         DC    X'03',AL1(03,15),X'F3030003F8',C'BXBX'           MKLV            
         DC    X'03',AL1(03,16),X'F3030003F8',C'BXBX'           CANCEL          
*                                                                               
         DC    X'03',AL1(04,01),X'F1110000C0',C'    ' AOR       ADD             
         DC    X'03',AL1(04,02),X'F1110000C0',C'    '           CHANGE          
         DC    X'03',AL1(04,03),X'F1110000C0',C'    '           DISPLAY         
         DC    X'03',AL1(04,04),X'F1110000C0',C'    '           DELETE          
         DC    X'03',AL1(04,05),X'F1110000C0',C'    '           SELECT          
         DC    X'03',AL1(04,06),X'F1110000C0',C'    '           RESTORE         
         DC    X'03',AL1(04,10),X'E1110011C0',C'AOSF'           LIST            
         DC    X'03',AL1(04,12),X'E111001178',C'AOSF'           REPORT          
*                                                                               
         DC    X'03',AL1(05,01),X'F6160000C0',C'    ' OAN       ADD             
         DC    X'03',AL1(05,02),X'F6160000C0',C'    '           CHANGE          
         DC    X'03',AL1(05,03),X'F6160000C0',C'    '           DISPLAY         
         DC    X'03',AL1(05,04),X'F6160000C0',C'    '           DELETE          
         DC    X'03',AL1(05,05),X'F6160000C0',C'    '           SELECT          
         DC    X'03',AL1(05,06),X'F6160000C0',C'    '           RESTORE         
         DC    X'03',AL1(05,10),X'E6160016F8',C'OASF'           LIST            
         DC    X'03',AL1(05,12),X'E616001678',C'OASF'           REPORT          
*                                                                               
         DC    X'03',AL1(07,01),X'F7070000C0',C'    ' DIV       ADD             
         DC    X'03',AL1(07,02),X'F7070000C0',C'    '           CHANGE          
         DC    X'03',AL1(07,03),X'F7070000C0',C'    '           DISPLAY         
         DC    X'03',AL1(07,04),X'F7070000C0',C'    '           DELETE          
         DC    X'03',AL1(07,05),X'F7070000C0',C'    '           SELECT          
         DC    X'03',AL1(07,06),X'F7070000C0',C'    '           RESTORE         
         DC    X'03',AL1(07,10),X'E7070007F8',C'DVSF'           LIST            
         DC    X'03',AL1(07,12),X'E707000778',C'DVSF'           REPORT          
*                                                                               
         DC    X'03',AL1(08,01),X'F8080000C0',C'    ' REG       ADD             
         DC    X'03',AL1(08,02),X'F8080000C0',C'    '           CHANGE          
         DC    X'03',AL1(08,03),X'F8080000C0',C'    '           DISPLAY         
         DC    X'03',AL1(08,04),X'F8080000C0',C'    '           DELETE          
         DC    X'03',AL1(08,05),X'F8080000C0',C'    '           SELECT          
         DC    X'03',AL1(08,06),X'F8080000C0',C'    '           RESTORE         
         DC    X'03',AL1(08,10),X'E8080008F8',C'RGSF'           LIST            
         DC    X'03',AL1(08,12),X'E808000878',C'RGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(09,01),X'F9090000C0',C'    ' DIS       ADD             
         DC    X'03',AL1(09,02),X'F9090000C0',C'    '           CHANGE          
         DC    X'03',AL1(09,03),X'F9090000C0',C'    '           DISPLAY         
         DC    X'03',AL1(09,04),X'F9090000C0',C'    '           DELETE          
         DC    X'03',AL1(09,05),X'F9090000C0',C'    '           SELECT          
         DC    X'03',AL1(09,06),X'F9090000C0',C'    '           RESTORE         
         DC    X'03',AL1(09,10),X'E9090009F8',C'DSSF'           LIST            
         DC    X'03',AL1(09,12),X'E909000978',C'DSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(10,10),X'F4040000C0',C'    ' CLR       LIST            
*                                                                               
         DC    X'03',AL1(11,01),X'FA0A0000C0',C'    ' PGCOST    ADD             
         DC    X'03',AL1(11,02),X'FA0A0000C0',C'    '           CHANGE          
         DC    X'03',AL1(11,03),X'FA0A0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(11,04),X'FA0A0000C0',C'    '           DELETE          
         DC    X'03',AL1(11,05),X'FA0A0000C0',C'    '           SELECT          
         DC    X'03',AL1(11,06),X'FA0A0000C0',C'    '           RESTORE         
         DC    X'03',AL1(11,10),X'EA0A000AF8',C'PGPG'           LIST            
*                                                                               
         DC    X'03',AL1(12,01),X'F5050000C0',C'    ' FSI       ADD             
         DC    X'03',AL1(12,02),X'F5050000C0',C'    '           CHANGE          
         DC    X'03',AL1(12,03),X'F5050000C0',C'    '           DISPLAY         
         DC    X'03',AL1(12,04),X'F5050000C0',C'    '           DELETE          
         DC    X'03',AL1(12,05),X'F5050000C0',C'    '           SELECT          
         DC    X'03',AL1(12,06),X'F5050000C0',C'    '           RESTORE         
         DC    X'03',AL1(12,10),X'E5050005F8',C'FSSF'           LIST            
         DC    X'03',AL1(12,12),X'E505000578',C'FSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(13,01),X'FB0B0000C0',C'    ' UDEF      ADD             
         DC    X'03',AL1(13,02),X'FB0B0000C0',C'    '           CHANGE          
         DC    X'03',AL1(13,03),X'FB0B0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(13,04),X'FB0B0000C0',C'    '           DELETE          
         DC    X'03',AL1(13,05),X'FB0B0000C0',C'    '           SELECT          
         DC    X'03',AL1(13,06),X'FB0B0000C0',C'    '           RESTORE         
         DC    X'03',AL1(13,10),X'EB1B001BF8',C'UDUD'           LIST            
*                                                                               
         DC    X'03',AL1(14,01),X'FC0C0000C0',C'    ' CUREC     ADD             
         DC    X'03',AL1(14,02),X'FC0C0000C0',C'    '           CHANGE          
         DC    X'03',AL1(14,03),X'FC0C0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(14,04),X'FC0C0000C0',C'    '           DELETE          
         DC    X'03',AL1(14,05),X'FC0C0000C0',C'    '           SELECT          
         DC    X'03',AL1(14,06),X'FC0C0000C0',C'    '           RESTORE         
         DC    X'03',AL1(14,10),X'EC0C000CF8',C'CUSF'           LIST            
         DC    X'03',AL1(14,12),X'EC0C000C78',C'CUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(15,01),X'FD0D0000C0',C'    ' ISSUE     ADD             
         DC    X'03',AL1(15,02),X'FD0D0000C0',C'    '           CHANGE          
         DC    X'03',AL1(15,03),X'FD0D0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(15,04),X'FD0D0000C0',C'    '           DELETE          
         DC    X'03',AL1(15,05),X'FD0D0000C0',C'    '           SELECT          
         DC    X'03',AL1(15,06),X'FD0D0000C0',C'    '           RESTORE         
         DC    X'03',AL1(15,10),X'ED0D000DF8',C'ISSF'           LIST            
         DC    X'03',AL1(15,12),X'ED0D000D78',C'ISSF'           REPORT          
*                                                                               
         DC    X'03',AL1(17,01),X'D0100000C0',C'    ' CIRC      ADD             
         DC    X'03',AL1(17,02),X'D0100000C0',C'    '           CHANGE          
         DC    X'03',AL1(17,03),X'D0100000C0',C'    '           DISPLAY         
         DC    X'03',AL1(17,04),X'D0100000C0',C'    '           DELETE          
         DC    X'03',AL1(17,05),X'D0100000C0',C'    '           SELECT          
         DC    X'03',AL1(17,06),X'D0100000C0',C'    '           RESTORE         
         DC    X'03',AL1(17,10),X'C0100010F8',C'CRSF'           LIST            
         DC    X'03',AL1(17,12),X'C010001078',C'CRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(18,01),X'D3130000C1',C'    ' EDR       ADD             
         DC    X'03',AL1(18,02),X'D3130000C1',C'    '           CHANGE          
         DC    X'03',AL1(18,03),X'D3130000C1',C'    '           DISPLAY         
         DC    X'03',AL1(18,04),X'D3130000C1',C'    '           DELETE          
         DC    X'03',AL1(18,05),X'D3130000C1',C'    '           SELECT          
         DC    X'03',AL1(18,06),X'D3130000C1',C'    '           RESTORE         
         DC    X'03',AL1(18,12),X'D313001318',C'EDSF'           REPORT          
*                                                                               
         DC    X'03',AL1(19,01),X'D4140000C1',C'    ' LOADEDR   ADD             
         DC    X'03',AL1(19,02),X'D4140000C1',C'    '           CHANGE          
         DC    X'03',AL1(19,03),X'D4140000C1',C'    '           DISPLAY         
         DC    X'03',AL1(19,04),X'D4140000C1',C'    '           DELETE          
         DC    X'03',AL1(19,05),X'D4140000C1',C'    '           SELECT          
         DC    X'03',AL1(19,06),X'D4140000C1',C'    '           RESTORE         
         DC    X'03',AL1(19,12),X'D414001418',C'EDSF'           REPORT          
*                                                                               
         DC    X'03',AL1(20,01),X'D5150000C0',C'    ' SPACE     ADD             
         DC    X'03',AL1(20,02),X'D5150000C0',C'    '           CHANGE          
         DC    X'03',AL1(20,03),X'D5150000C0',C'    '           DISPLAY         
         DC    X'03',AL1(20,04),X'D5150000C0',C'    '           DELETE          
         DC    X'03',AL1(20,05),X'D5150000C0',C'    '           SELECT          
         DC    X'03',AL1(20,06),X'D5150000C0',C'    '           RESTORE         
         DC    X'03',AL1(20,10),X'C5150015F8',C'SPSF'           LIST            
         DC    X'03',AL1(20,12),X'C515001578',C'SPSF'           REPORT          
*                                                                               
*OLD*    DC    X'03',AL1(21,17),X'D717000081',C'    ' BILLF     MAINT           
*                                                                               
         DC    X'03',AL1(22,01),X'D1120000C0',C'    ' BUDGET    ADD             
         DC    X'03',AL1(22,02),X'D1120000C0',C'    '           CHANGE          
         DC    X'03',AL1(22,03),X'D1120000C0',C'    '           DISPLAY         
         DC    X'03',AL1(22,04),X'D1120000C0',C'    '           DELETE          
         DC    X'03',AL1(22,05),X'D1120000C0',C'    '           SELECT          
         DC    X'03',AL1(22,06),X'D1120000C0',C'    '           RESTORE         
         DC    X'03',AL1(22,10),X'C1120000C0',C'    '           LIST            
         DC    X'03',AL1(22,18),X'D1120000C0',C'    '           TOTALS          
         DC    X'03',AL1(22,12),X'D112000078',C'BUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(34,01),X'B2260000C0',C'    ' CGROUP    ADD             
         DC    X'03',AL1(34,02),X'B2260000C0',C'    '           CHANGE          
         DC    X'03',AL1(34,03),X'B2260000C0',C'    '           DISPLAY         
         DC    X'03',AL1(34,04),X'B2260000C0',C'    '           DELETE          
         DC    X'03',AL1(34,05),X'B2260000C0',C'    '           SELECT          
         DC    X'03',AL1(34,06),X'B2260000C0',C'    '           RESTORE         
         DC    X'03',AL1(34,10),X'B9260000C0',C'    '           LIST            
         DC    X'03',AL1(34,12),X'BA26000078',C'CGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(35,03),X'B3270000C1',C'    ' CGASSIGN  DISPLAY         
*                                                      (CLT GRP ASSIGN)         
         DC    X'03',AL1(36,01),X'B1250000C0',C'    ' CGDEF     ADD             
         DC    X'03',AL1(36,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(36,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(36,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(36,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(36,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(36,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(37,01),X'B4290000C0',C'    ' PGROUP    ADD             
         DC    X'03',AL1(37,02),X'B4290000C0',C'    '           CHANGE          
         DC    X'03',AL1(37,03),X'B4290000C0',C'    '           DISPLAY         
         DC    X'03',AL1(37,04),X'B4290000C0',C'    '           DELETE          
         DC    X'03',AL1(37,05),X'B4290000C0',C'    '           SELECT          
         DC    X'03',AL1(37,06),X'B4290000C0',C'    '           RESTORE         
         DC    X'03',AL1(37,10),X'BD290000C0',C'    '           LIST            
         DC    X'03',AL1(37,12),X'BE29000078',C'PGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(38,03),X'B52A0000C1',C'    ' PGASSIGN  DISPLAY         
*                                                      (PRD GRP ASSIGN)         
         DC    X'03',AL1(39,01),X'BB280000C0',C'    ' PGDEF     ADD             
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
         DC    X'03',AL1(41,03),X'B7270000C1',C'    ' PUB ASSGN DISPLAY         
*                                                      (PUB GRP ASSIGN)         
         DC    X'03',AL1(42,01),X'B1250000C0',C'    ' PUB G DEF ADD             
         DC    X'03',AL1(42,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(42,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(42,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(42,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(42,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(42,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(43,01),X'D8180000C0',C'    ' PUBLIST   ADD             
         DC    X'03',AL1(43,02),X'D8180000C0',C'    '           CHANGE          
         DC    X'03',AL1(43,03),X'D8180000C0',C'    '           DISPLAY         
         DC    X'03',AL1(43,04),X'D8180000C0',C'    '           DELETE          
         DC    X'03',AL1(43,05),X'D8180000C0',C'    '           SELECT          
         DC    X'03',AL1(43,06),X'D8180000C0',C'    '           RESTORE         
         DC    X'03',AL1(43,10),X'C8180018F8',C'PLSF'           LIST            
         DC    X'03',AL1(43,12),X'C818001878',C'PLSF'           REPORT          
*                                                                               
         DC    X'03',AL1(44,12),X'BF8F008F18',C'CPSF' JWTCPP    REPORT          
*                                                                               
         DC    X'03',AL1(45,01),X'D3130000C1',C'    ' SRDS      ADD             
         DC    X'03',AL1(45,02),X'D3130000C1',C'    '           CHANGE          
         DC    X'03',AL1(45,03),X'D3130000C1',C'    '           DISPLAY         
         DC    X'03',AL1(45,04),X'D3130000C1',C'    '           DELETE          
         DC    X'03',AL1(45,05),X'D3130000C1',C'    '           SELECT          
         DC    X'03',AL1(45,06),X'D3130000C1',C'    '           RESTORE         
         DC    X'03',AL1(45,12),X'D313001318',C'SRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(46,01),X'D9190000C0',C'    ' UCOMM     ADD             
         DC    X'03',AL1(46,02),X'D9190000C0',C'    '           CHANGE          
         DC    X'03',AL1(46,03),X'D9190000C0',C'    '           DISPLAY         
         DC    X'03',AL1(46,04),X'D9190000C0',C'    '           DELETE          
         DC    X'03',AL1(46,05),X'D9190000C0',C'    '           SELECT          
         DC    X'03',AL1(46,06),X'D9190000C0',C'    '           RESTORE         
         DC    X'03',AL1(46,10),X'C9190019C0',C'UCSF'           LIST            
*                                                                               
         DC    X'03',AL1(47,01),X'D2200000C0',C'    ' CHARGE    ADD             
         DC    X'03',AL1(47,02),X'D2200000C0',C'    '           CHANGE          
         DC    X'03',AL1(47,03),X'D2200000C0',C'    '           DISPLAY         
         DC    X'03',AL1(47,04),X'D2200000C0',C'    '           DELETE          
         DC    X'03',AL1(47,05),X'D2200000C0',C'    '           SELECT          
         DC    X'03',AL1(47,06),X'D2200000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(48,12),X'BF8E008E18',C'CPSF' MSHCPP    REPORT          
*                                                                               
         DC    X'03',AL1(49,17),X'D6210000C0',C'    ' STDSPACE  MAINT           
         DC    X'03',AL1(49,12),X'D621002178',C'DSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(50,01),X'DA220000C0',C'    ' CUSTCOL   ADD             
         DC    X'03',AL1(50,02),X'DA220000C0',C'    '           CHANGE          
         DC    X'03',AL1(50,03),X'DA220000C0',C'    '           DISPLAY         
         DC    X'03',AL1(50,04),X'DA220000C0',C'    '           DELETE          
         DC    X'03',AL1(50,05),X'DA220000C0',C'    '           SELECT          
         DC    X'03',AL1(50,06),X'DA220000C0',C'    '           RESTORE         
         DC    X'03',AL1(50,10),X'CA220022F8',C'CCSF'           LIST            
         DC    X'03',AL1(50,12),X'CA22002278',C'CCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(51,01),X'CB230000C0',C'    ' PGEST     ADD             
         DC    X'03',AL1(51,02),X'CB230000C0',C'    '           CHANGE          
         DC    X'03',AL1(51,03),X'CB230000C0',C'    '           DISPLAY         
         DC    X'03',AL1(51,04),X'CB230000C0',C'    '           DELETE          
         DC    X'03',AL1(51,05),X'CB230000C0',C'    '           SELECT          
         DC    X'03',AL1(51,06),X'CB230000C0',C'    '           RESTORE         
         DC    X'03',AL1(51,10),X'CC230000C0',C'    '           LIST            
         DC    X'03',AL1(51,12),X'CC23002378',C'PGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(52,01),X'CD240000C0',C'    ' GFEST     ADD             
         DC    X'03',AL1(52,02),X'CD240000C0',C'    '           CHANGE          
         DC    X'03',AL1(52,03),X'CD240000C0',C'    '           DISPLAY         
         DC    X'03',AL1(52,04),X'CD240000C0',C'    '           DELETE          
         DC    X'03',AL1(52,05),X'CD240000C0',C'    '           SELECT          
         DC    X'03',AL1(52,06),X'CD240000C0',C'    '           RESTORE         
         DC    X'03',AL1(52,10),X'CE240000C0',C'    '           LIST            
         DC    X'03',AL1(52,12),X'CE24002478',C'GFSF'           REPORT          
*                                                                               
         DC    X'03',AL1(53,01),X'B01C0000C0',C'    ' CLIENT    ADD             
         DC    X'03',AL1(53,02),X'B01C0000C0',C'    '           CHANGE          
         DC    X'03',AL1(53,03),X'B01C0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(53,04),X'B01C0000C0',C'    '           DELETE          
         DC    X'03',AL1(53,05),X'B01C0000C0',C'    '           SELECT          
         DC    X'03',AL1(53,06),X'B01C0000C0',C'    '           RESTORE         
         DC    X'03',AL1(53,10),X'C41C0000C0',C'    '           LIST            
         DC    X'03',AL1(53,12),X'C41C001C78',C'CLSF'           REPORT          
*                                                                               
         DC    X'03',AL1(54,01),X'C71D0000C0',C'    ' CL2       ADD             
         DC    X'03',AL1(54,02),X'C71D0000C0',C'    '           CHANGE          
         DC    X'03',AL1(54,03),X'C71D0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(54,04),X'C71D0000C0',C'    '           DELETE          
         DC    X'03',AL1(54,05),X'C71D0000C0',C'    '           SELECT          
         DC    X'03',AL1(54,06),X'C71D0000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(55,01),X'A01E0000C0',C'    ' PRODUCT   ADD             
         DC    X'03',AL1(55,02),X'A01E0000C0',C'    '           CHANGE          
         DC    X'03',AL1(55,03),X'A01E0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(55,04),X'A01E0000C0',C'    '           DELETE          
         DC    X'03',AL1(55,05),X'A01E0000C0',C'    '           SELECT          
         DC    X'03',AL1(55,06),X'A01E0000C0',C'    '           RESTORE         
         DC    X'03',AL1(55,10),X'A11E0000C0',C'    '           LIST            
         DC    X'03',AL1(55,12),X'A11E001E78',C'PRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(56,01),X'A21F0000C0',C'    ' PRBILL    ADD             
         DC    X'03',AL1(56,02),X'A21F0000C0',C'    '           CHANGE          
         DC    X'03',AL1(56,03),X'A21F0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(56,04),X'A21F0000C0',C'    '           DELETE          
         DC    X'03',AL1(56,05),X'A21F0000C0',C'    '           SELECT          
         DC    X'03',AL1(56,06),X'A21F0000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(57,01),X'A31A0000C0',C'    ' ESTIMATE  ADD             
         DC    X'03',AL1(57,02),X'A31A0000C0',C'    '           CHANGE          
         DC    X'03',AL1(57,03),X'A31A0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(57,04),X'A31A0000C0',C'    '           DELETE          
         DC    X'03',AL1(57,05),X'A31A0000C0',C'    '           SELECT          
         DC    X'03',AL1(57,06),X'A31A0000C0',C'    '           RESTORE         
         DC    X'03',AL1(57,10),X'A41A0000C0',C'    '           LIST            
         DC    X'03',AL1(57,12),X'A41A001A78',C'ESSF'           REPORT          
         DC    X'03',AL1(57,13),X'E02D002DC1',C'    '           COPY            
*                                                                               
         DC    X'03',AL1(58,01),X'A21F0000C0',C'    ' ESBILL    ADD             
         DC    X'03',AL1(58,02),X'A21F0000C0',C'    '           CHANGE          
         DC    X'03',AL1(58,03),X'A21F0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(58,04),X'A21F0000C0',C'    '           DELETE          
         DC    X'03',AL1(58,05),X'A21F0000C0',C'    '           SELECT          
         DC    X'03',AL1(58,06),X'A21F0000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(59,01),X'A52C0000C0',C'    ' ES2       ADD             
         DC    X'03',AL1(59,02),X'A52C0000C0',C'    '           CHANGE          
         DC    X'03',AL1(59,03),X'A52C0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(59,04),X'A52C0000C0',C'    '           DELETE          
         DC    X'03',AL1(59,05),X'A52C0000C0',C'    '           SELECT          
         DC    X'03',AL1(59,06),X'A52C0000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(60,01),X'A01E0000C0',C'    ' LEGAL     ADD             
         DC    X'03',AL1(60,02),X'A01E0000C0',C'    '           CHANGE          
         DC    X'03',AL1(60,03),X'A01E0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(60,04),X'A01E0000C0',C'    '           DELETE          
         DC    X'03',AL1(60,05),X'A01E0000C0',C'    '           SELECT          
         DC    X'03',AL1(60,06),X'A01E0000C0',C'    '           RESTORE         
         DC    X'03',AL1(60,10),X'A11E0000C0',C'    '           LIST            
         DC    X'03',AL1(60,12),X'A11E001E78',C'PRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(61,01),X'A6310000C0',C'    ' VCCLIST   ADD             
         DC    X'03',AL1(61,02),X'A6310000C0',C'    '           CHANGE          
         DC    X'03',AL1(61,03),X'A6310000C0',C'    '           DISPLAY         
         DC    X'03',AL1(61,04),X'A6310000C0',C'    '           DELETE          
         DC    X'03',AL1(61,05),X'A6310000C0',C'    '           SELECT          
         DC    X'03',AL1(61,06),X'A6310000C0',C'    '           RESTORE         
         DC    X'03',AL1(61,10),X'A7310000C0',C'    '           LIST            
         DC    X'03',AL1(61,12),X'A731003178',C'VCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(62,01),X'A8320000C0',C'    ' ACCLIST   ADD             
         DC    X'03',AL1(62,02),X'A8320000C0',C'    '           CHANGE          
         DC    X'03',AL1(62,03),X'A8320000C0',C'    '           DISPLAY         
         DC    X'03',AL1(62,04),X'A8320000C0',C'    '           DELETE          
         DC    X'03',AL1(62,05),X'A8320000C0',C'    '           SELECT          
         DC    X'03',AL1(62,06),X'A8320000C0',C'    '           RESTORE         
         DC    X'03',AL1(62,10),X'A9320000C0',C'    '           LIST            
         DC    X'03',AL1(62,12),X'A932003278',C'ACSF'           REPORT          
*                                                                               
         DC    X'03',AL1(63,01),X'AA330000C0',C'    ' SETUP     ADD             
         DC    X'03',AL1(63,02),X'AA330000C0',C'    ' (WAS      CHANGE          
         DC    X'03',AL1(63,03),X'AA330000C0',C'    '  SCHEMA)  DISPLAY         
         DC    X'03',AL1(63,05),X'AA330000C0',C'    '           SELECT          
         DC    X'03',AL1(63,10),X'AB330000C0',C'    '           LIST            
         DC    X'03',AL1(63,12),X'AB33003378',C'SUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(64,01),X'AD340000C0',C'    ' SUBMEDIA  ADD             
         DC    X'03',AL1(64,02),X'AD340000C0',C'    '           CHANGE          
         DC    X'03',AL1(64,03),X'AD340000C0',C'    '           DISPLAY         
         DC    X'03',AL1(64,04),X'AD340000C0',C'    '           DELETE          
         DC    X'03',AL1(64,05),X'AD340000C0',C'    '           SELECT          
         DC    X'03',AL1(64,06),X'AD340000C0',C'    '           RESTORE         
         DC    X'03',AL1(64,10),X'AE340000C0',C'    '           LIST            
         DC    X'03',AL1(64,12),X'AE34003478',C'SMSF'           REPORT          
*                                                                               
         DC    X'03',AL1(65,01),X'DB350000C0',C'    ' BFORM     ADD             
         DC    X'03',AL1(65,02),X'DB350000C0',C'    '           CHANGE          
         DC    X'03',AL1(65,03),X'DB350000C0',C'    '           DISPLAY         
         DC    X'03',AL1(65,04),X'DB350000C0',C'    '           DELETE          
         DC    X'03',AL1(65,05),X'DB350000C0',C'    '           SELECT          
         DC    X'03',AL1(65,06),X'DB350000C0',C'    '           RESTORE         
         DC    X'03',AL1(65,10),X'DC350000F8',C'BFSF'           LIST            
         DC    X'03',AL1(65,12),X'DC35003578',C'BFSF'           REPORT          
*                                                                               
         DC    X'03',AL1(66,01),X'AF2E0000C0',C'    ' CLIADV    ADD             
         DC    X'03',AL1(66,02),X'AF2E0000C0',C'    '           CHANGE          
         DC    X'03',AL1(66,03),X'AF2E0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(66,04),X'AF2E0000C0',C'    '           DELETE          
         DC    X'03',AL1(66,05),X'AF2E0000C0',C'    '           SELECT          
         DC    X'03',AL1(66,06),X'AF2E0000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(67,01),X'C2410000C0',C'    ' SRVCCLST  ADD             
         DC    X'03',AL1(67,02),X'C2410000C0',C'    '           CHANGE          
         DC    X'03',AL1(67,03),X'C2410000C0',C'    '           DISPLAY         
         DC    X'03',AL1(67,04),X'C2410000C0',C'    '           DELETE          
         DC    X'03',AL1(67,05),X'C2410000C0',C'    '           SELECT          
         DC    X'03',AL1(67,06),X'C2410000C0',C'    '           RESTORE         
         DC    X'03',AL1(67,10),X'C3410000C0',C'    '           LIST            
         DC    X'03',AL1(67,12),X'C341004178',C'SVSF'           REPORT          
*                                                                               
         DC    X'03',AL1(68,01),X'C6420000C0',C'    ' SRACCLST  ADD             
         DC    X'03',AL1(68,02),X'C6420000C0',C'    '           CHANGE          
         DC    X'03',AL1(68,03),X'C6420000C0',C'    '           DISPLAY         
         DC    X'03',AL1(68,04),X'C6420000C0',C'    '           DELETE          
         DC    X'03',AL1(68,05),X'C6420000C0',C'    '           SELECT          
         DC    X'03',AL1(68,06),X'C6420000C0',C'    '           RESTORE         
         DC    X'03',AL1(68,10),X'AC420000C0',C'    '           LIST            
         DC    X'03',AL1(68,12),X'AC42004278',C'SASF'           REPORT          
*                                                                               
         DC    X'03',AL1(69,01),X'DD360000C0',C'    ' PO#       ADD             
         DC    X'03',AL1(69,02),X'DD360000C0',C'    '           CHANGE          
         DC    X'03',AL1(69,03),X'DD360000C0',C'    '           DISPLAY         
*******  DC    X'03',AL1(69,04),X'DD360000C0',C'    '           DELETE          
         DC    X'03',AL1(69,05),X'DD360000C0',C'    '           SELECT          
*******  DC    X'03',AL1(69,06),X'DD360000C0',C'    '           RESTORE         
         DC    X'03',AL1(69,10),X'DE360000C0',C'POSF'           LIST            
         DC    X'03',AL1(69,12),X'DE36003678',C'POSF'           REPORT          
*                                                                               
         DC    X'03',AL1(70,01),X'DA220000C0',C'    ' STDCOL    ADD             
         DC    X'03',AL1(70,02),X'DA220000C0',C'    '           CHANGE          
         DC    X'03',AL1(70,03),X'DA220000C0',C'    '           DISPLAY         
         DC    X'03',AL1(70,04),X'DA220000C0',C'    '           DELETE          
         DC    X'03',AL1(70,05),X'DA220000C0',C'    '           SELECT          
         DC    X'03',AL1(70,06),X'DA220000C0',C'    '           RESTORE         
         DC    X'03',AL1(70,10),X'CA220000F8',C'SCSF'           LIST            
         DC    X'03',AL1(70,12),X'CA22002278',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(71,01),X'DF370000C0',C'    ' CPUB      ADD             
         DC    X'03',AL1(71,02),X'DF370000C0',C'    '           CHANGE          
         DC    X'03',AL1(71,03),X'DF370000C0',C'    '           DISPLAY         
         DC    X'03',AL1(71,04),X'DF370000C0',C'    '           DELETE          
         DC    X'03',AL1(71,05),X'DF370000C0',C'    '           SELECT          
         DC    X'03',AL1(71,06),X'DF370000C0',C'    '           RESTORE         
         DC    X'03',AL1(71,10),X'EF370000F8',C'SCSF'           LIST            
         DC    X'03',AL1(71,12),X'EF37003778',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(72,01),X'E3380000C0',C'    ' EXCHANGE  ADD             
         DC    X'03',AL1(72,02),X'E3380000C0',C'    '           CHANGE          
         DC    X'03',AL1(72,03),X'E3380000C0',C'    '           DISPLAY         
         DC    X'03',AL1(72,04),X'E3380000C0',C'    '           DELETE          
         DC    X'03',AL1(72,05),X'E3380000C0',C'    '           SELECT          
         DC    X'03',AL1(72,06),X'E3380000C0',C'    '           RESTORE         
         DC    X'03',AL1(72,10),X'E4380000F8',C'SCSF'           LIST            
         DC    X'03',AL1(72,12),X'E438003878',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(73,01),X'9E390000C0',C'    ' DCOMMENT  ADD             
         DC    X'03',AL1(73,02),X'9E390000C0',C'    '           CHANGE          
         DC    X'03',AL1(73,03),X'9E390000C0',C'    '           DISPLAY         
         DC    X'03',AL1(73,05),X'9E390000C0',C'    '           SELECT          
         DC    X'03',AL1(73,10),X'9F390000F8',C'DCSF'           LIST            
         DC    X'03',AL1(73,12),X'9F39003978',C'DCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(74,02),X'9D430000C0',C'    ' CUSTMENU  CHANGE          
         DC    X'03',AL1(74,03),X'9D430000C0',C'    '           DISPLAY         
         DC    X'03',AL1(74,05),X'9D430000C0',C'    '           SELECT          
         DC    X'03',AL1(74,10),X'9C430000F8',C'DMSF'           LIST            
         DC    X'03',AL1(74,12),X'9C43004378',C'DMSF'           REPORT          
*                                                                               
         DC    X'03',AL1(75,02),X'9D430000C0',C'    ' STDMENU   CHANGE          
         DC    X'03',AL1(75,03),X'9D430000C0',C'    '           DISPLAY         
         DC    X'03',AL1(75,05),X'9D430000C0',C'    '           SELECT          
         DC    X'03',AL1(75,10),X'9C430000F8',C'DMSF'           LIST            
         DC    X'03',AL1(75,12),X'9C43004378',C'DMSF'           REPORT          
*                                                                               
         DC    X'03',AL1(76,03),X'9A440000C0',C'    ' AUTOPAY   DISPLAY         
         DC    X'03',AL1(76,05),X'9A440000C0',C'    ' AUTOPAY   SELECT          
         DC    X'03',AL1(76,10),X'9B440000F8',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(77,01),X'8C400000C0',C'    ' BILLADR   ADD             
         DC    X'03',AL1(77,02),X'8C400000C0',C'    '           CHANGE          
         DC    X'03',AL1(77,03),X'8C400000C0',C'    '           DISPLAY         
         DC    X'03',AL1(77,04),X'8C400000C0',C'    '           DELETE          
         DC    X'03',AL1(77,05),X'8C400000C0',C'    '           SELECT          
         DC    X'03',AL1(77,06),X'8C400000C0',C'    '           RESTORE         
         DC    X'03',AL1(77,10),X'8D400000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(78,12),X'9945004530',C'CZCZ' CLFRZ     REPORT          
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACT2  DS    0D                  ALTERNATE ACTION TABLE FOR SCROLLING         
*                                                                               
* X'02'  ENTRIES ARE AVAILABLE ACTIONS                                          
* CL8    EXPANDED ACTION NAME                                                   
* CL1    ACTION NUMBER                                                          
* CL1    ACTION EQUATE                                                          
* CL1    SPARE                                                                  
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(02,02,00) DISPLAY =CHANGE                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DIRECTORY OF RECORDS AND ACTIONS (NO CLIENT, PRODUCT OR ESTIMATE)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECACTX  DS    0D                                                               
*                                                                               
* X'01'  ENTRIES ARE AVAILABLE RECORDS                                          
* CL8    EXPANDED RECORD NAME                                                   
* CL1    RECORD NUMBER                                                          
* CL1    PHASE NUMBER FOR DATA DICTIONARY                                       
* CL1    PHASE NUMBER FOR HELP SCREEN                                           
*                                                                               
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'USERP   ',AL1(02),X'00C2'                                
         DC    X'01',C'BUY     ',AL1(03),X'00C2'                                
         DC    X'01',C'AOR     ',AL1(04),X'00CF'                                
         DC    X'01',C'OAN     ',AL1(05),X'00CF'                                
         DC    X'01',C'DIVISION',AL1(07),X'00CF'                                
         DC    X'01',C'REGION  ',AL1(08),X'00CF'                                
*                                                                               
         DC    X'01',C'DISTRICT',AL1(09),X'00CF'                                
         DC    X'01',C'DSTREC  ',AL1(09),X'00CF'                                
*                                                                               
         DC    X'01',C'CLRST   ',AL1(10),X'00CF'                                
         DC    X'01',C'COST    ',AL1(11),X'00CF'                                
         DC    X'01',C'FSI     ',AL1(12),X'00CF'                                
         DC    X'01',C'UDEF    ',AL1(13),X'00CF'                                
         DC    X'01',C'CUREC   ',AL1(14),X'00CF'                                
         DC    X'01',C'ISSUE   ',AL1(15),X'00CF'                                
         DC    X'01',C'CIRC    ',AL1(17),X'00CF'                                
*                                                                               
         DC    X'01',C'EDR     ',AL1(18),X'00CF'                                
         DC    X'01',C'2EDR    ',AL1(18),X'00CF'                                
*                                                                               
         DC    X'01',C'LOADEDR ',AL1(19),X'00CF'                                
*                                                                               
         DC    X'01',C'SPACE   ',AL1(20),X'00CF'                                
*                                                                               
*OLD*    DC    X'01',C'BILLFORM',AL1(21),X'00CF'    BILLING FORMULA             
         DC    X'01',C'BUDGET  ',AL1(22),X'00CF'    BUDGET                      
         DC    X'01',C'CGROUP  ',AL1(34),X'00CF'    CLIENT GROUPS               
         DC    X'01',C'CGASSIGN',AL1(35),X'00CF'    CLIENT ASSIGNMENTS          
         DC    X'01',C'CGDEF   ',AL1(36),X'00CF'    CLIENT GROUP DEF            
         DC    X'01',C'PGROUP  ',AL1(37),X'00CF'    PRODUCT GROUPS              
         DC    X'01',C'PGASSIGN',AL1(38),X'00CF'    PRODUCT ASSIGNMENTS         
         DC    X'01',C'PGDEF   ',AL1(39),X'00CF'    PRODUCT GROUP DEF           
*                                                                               
         DC    X'01',C'PBGROUP ',AL1(40),X'00CF'    PUB GROUPS                  
         DC    X'01',C'PUBGROUP',AL1(40),X'00CF'    PUB GROUPS                  
*                                                                               
         DC    X'01',C'PBASSIGN',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBGASSI',AL1(41),X'00CF'    PUB ASSIGNMENTS             
         DC    X'01',C'PUBASSIG',AL1(41),X'00CF'    PUB ASSIGNMENTS             
*                                                                               
         DC    X'01',C'PBDEF   ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PUBGDEF ',AL1(42),X'00CF'    PUB GROUP DEF               
         DC    X'01',C'PBGDEF  ',AL1(42),X'00CF'    PUB GROUP DEF               
*                                                                               
         DC    X'01',C'PUBLIST ',AL1(43),X'00CF'    PUB LIST                    
         DC    X'01',C'JWTCPP  ',AL1(44),X'00CF'    JWT CPP PUT TO Q            
         DC    X'01',C'SRDS    ',AL1(45),X'00CF'    SRDS SIZING                 
         DC    X'01',C'UCOMM   ',AL1(46),X'00CF'    USER COMMENT                
         DC    X'01',C'CHARGE  ',AL1(47),X'00CF'    SPECIAL CHARGES             
         DC    X'01',C'MSHCPP  ',AL1(48),X'00CF'    MSHR CPP PUT TO Q           
         DC    X'01',C'STDSPACE',AL1(49),X'00CF'    STANDARD SPACE DESC         
         DC    X'01',C'CUSTCOL ',AL1(50),X'00CF'    CUSTOM COLUMNS              
*                                                                               
* RECORD NUMBERS 51 THROUGH 60 FROM RECACT ARE NOT INCLUDED IN RECACTX          
*                                                                               
         DC    X'01',C'VCCLIST ',AL1(61),X'00CF'    EIO VENDOR CC LIST          
*                                                                               
         DC    X'01',C'ACCLIST ',AL1(62),X'00CF'    EIO AGENCY CC LIST          
*                                                                               
         DC    X'01',C'SETUP   ',AL1(63),X'00CF'    SETUP (WAS SCHEMA)          
*                                                                               
         DC    X'01',C'SUBMEDIA',AL1(64),X'00CF'    SUBMEDIA                    
*                                                                               
         DC    X'01',C'BFORM   ',AL1(65),X'00CF'    NEW BILLFORM MAINT          
*                                                                               
         DC    X'01',C'SRVCCLST',AL1(67),X'00CF'    ESR VENDOR CC LIST          
*                                                                               
         DC    X'01',C'SRACCLST',AL1(68),X'00CF'    ESR AGENCY CC LIST          
*                                                                               
         DC    X'01',C'PO#     ',AL1(69),X'00CF'    PURCHASE ORDER NO.          
*                                                                               
         DC    X'01',C'STDCOL  ',AL1(70),X'00CF'    STANDARD CUST COL           
*                                                                               
         DC    X'01',C'CPUB    ',AL1(71),X'00CF'    CENTRAL PUBFILE             
*                                                                               
         DC    X'01',C'EXCHANGE',AL1(72),X'00CF'    EXCHANGE                    
*                                                                               
         DC    X'01',C'DCOMMENT',AL1(73),X'00CF'    DISCREPANCY COMMENT         
*                                                                               
         DC    X'01',C'CUSTMENU',AL1(74),X'00CF'    CUST DROP DOWN MENU         
*                                                                               
         DC    X'01',C'STDMENU ',AL1(74),X'00CF'    STD  DROP DOWN MENU         
*                                                                               
         DC    X'01',C'AUTOPAY ',AL1(76),X'00CF'    AUTOPAY                     
*                                                                               
         DC    X'01',C'BILLADR ',AL1(77),X'00CF'    BILL ADDRESS                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* X'02'  ENTRIES ARE AVAILABLE ACTIONS                                          
* CL8    EXPANDED ACTION NAME                                                   
* CL1    ACTION NUMBER                                                          
* CL1    ACTION EQUATE                                                          
* CL1    SPARE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        PHASE USED UP BY SYSTEM SO FAR (PRSFMXX)                               
*        P = PROGRAM, S= SCREEN, . = FREE                                       
*                                                                               
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     P  .  .  P  P  P  .  P  P  P  P  P  P  P  P  P                  
*        1X     P  P  P  P  P  P  P  P  P  P  P  P  P  P  P  P                  
*        2X     P  P  P  P  P  P  P  P  P  P  P  P  P  P  .  .                  
*        3X     P  P  P  P  .  P  P  P  .  .  .  .  .  .  .  .                  
*        4X     .  .  .  P  .  .  .  .  .  .  .  .  .  .  .  .                  
*        5X     .  P  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        6X     .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        7X     .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .                  
*        8X     .  .  .  .  .  .  .  .  .  .  .  .  .  .  P  P                  
*        9X     .  .  .  .  .  .  .  .  .  .  .  .  S  S  S  S                  
*        AX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        BX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        CX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        DX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        EX     S  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*        FX     .  S  S  S  S  S  S  S  S  S  S  S  S  S  S  S                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         DC    X'03',AL1(02,01),X'F2510000C0',C'    ' USERP     ADD             
         DC    X'03',AL1(02,02),X'F2510000C0',C'    '           CHANGE          
         DC    X'03',AL1(02,03),X'F2510000C0',C'    '           DISPLAY         
         DC    X'03',AL1(02,04),X'F2510000C0',C'    '           DELETE          
         DC    X'03',AL1(02,05),X'F2510000C0',C'    '           SELECT          
         DC    X'03',AL1(02,06),X'F2510000C0',C'    '           RESTORE         
         DC    X'03',AL1(02,10),X'E2510051F8',C'USSF'           LIST            
         DC    X'03',AL1(02,12),X'E251005178',C'USSF'           REPORT          
*                                                                               
         DC    X'03',AL1(03,13),X'F3030003F8',C'BXBX' BUY       COPY            
         DC    X'03',AL1(03,14),X'F3030003F8',C'BXBX'           MOVE            
         DC    X'03',AL1(03,15),X'F3030003F8',C'BXBX'           MKLV            
         DC    X'03',AL1(03,16),X'F3030003F8',C'BXBX'           CANCEL          
*                                                                               
         DC    X'03',AL1(04,01),X'F1110000C0',C'    ' AOR       ADD             
         DC    X'03',AL1(04,02),X'F1110000C0',C'    '           CHANGE          
         DC    X'03',AL1(04,03),X'F1110000C0',C'    '           DISPLAY         
         DC    X'03',AL1(04,04),X'F1110000C0',C'    '           DELETE          
         DC    X'03',AL1(04,05),X'F1110000C0',C'    '           SELECT          
         DC    X'03',AL1(04,06),X'F1110000C0',C'    '           RESTORE         
         DC    X'03',AL1(04,10),X'E1110011C0',C'AOSF'           LIST            
         DC    X'03',AL1(04,12),X'E111001178',C'AOSF'           REPORT          
*                                                                               
         DC    X'03',AL1(05,01),X'F6160000C0',C'    ' OAN       ADD             
         DC    X'03',AL1(05,02),X'F6160000C0',C'    '           CHANGE          
         DC    X'03',AL1(05,03),X'F6160000C0',C'    '           DISPLAY         
         DC    X'03',AL1(05,04),X'F6160000C0',C'    '           DELETE          
         DC    X'03',AL1(05,05),X'F6160000C0',C'    '           SELECT          
         DC    X'03',AL1(05,06),X'F6160000C0',C'    '           RESTORE         
         DC    X'03',AL1(05,10),X'E6160016F8',C'OASF'           LIST            
         DC    X'03',AL1(05,12),X'E616001678',C'OASF'           REPORT          
*                                                                               
         DC    X'03',AL1(07,01),X'F7070000C0',C'    ' DIV       ADD             
         DC    X'03',AL1(07,02),X'F7070000C0',C'    '           CHANGE          
         DC    X'03',AL1(07,03),X'F7070000C0',C'    '           DISPLAY         
         DC    X'03',AL1(07,04),X'F7070000C0',C'    '           DELETE          
         DC    X'03',AL1(07,05),X'F7070000C0',C'    '           SELECT          
         DC    X'03',AL1(07,06),X'F7070000C0',C'    '           RESTORE         
         DC    X'03',AL1(07,10),X'E7070007F8',C'DVSF'           LIST            
         DC    X'03',AL1(07,12),X'E707000778',C'DVSF'           REPORT          
*                                                                               
         DC    X'03',AL1(08,01),X'F8080000C0',C'    ' REG       ADD             
         DC    X'03',AL1(08,02),X'F8080000C0',C'    '           CHANGE          
         DC    X'03',AL1(08,03),X'F8080000C0',C'    '           DISPLAY         
         DC    X'03',AL1(08,04),X'F8080000C0',C'    '           DELETE          
         DC    X'03',AL1(08,05),X'F8080000C0',C'    '           SELECT          
         DC    X'03',AL1(08,06),X'F8080000C0',C'    '           RESTORE         
         DC    X'03',AL1(08,10),X'E8080008F8',C'RGSF'           LIST            
         DC    X'03',AL1(08,12),X'E808000878',C'RGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(09,01),X'F9090000C0',C'    ' DIS       ADD             
         DC    X'03',AL1(09,02),X'F9090000C0',C'    '           CHANGE          
         DC    X'03',AL1(09,03),X'F9090000C0',C'    '           DISPLAY         
         DC    X'03',AL1(09,04),X'F9090000C0',C'    '           DELETE          
         DC    X'03',AL1(09,05),X'F9090000C0',C'    '           SELECT          
         DC    X'03',AL1(09,06),X'F9090000C0',C'    '           RESTORE         
         DC    X'03',AL1(09,10),X'E9090009F8',C'DSSF'           LIST            
         DC    X'03',AL1(09,12),X'E909000978',C'DSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(10,10),X'F4040000C0',C'    ' CLR       LIST            
*                                                                               
         DC    X'03',AL1(11,01),X'FA0A0000C0',C'    ' PGCOST    ADD             
         DC    X'03',AL1(11,02),X'FA0A0000C0',C'    '           CHANGE          
         DC    X'03',AL1(11,03),X'FA0A0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(11,04),X'FA0A0000C0',C'    '           DELETE          
         DC    X'03',AL1(11,05),X'FA0A0000C0',C'    '           SELECT          
         DC    X'03',AL1(11,06),X'FA0A0000C0',C'    '           RESTORE         
         DC    X'03',AL1(11,10),X'EA0A000AF8',C'PGPG'           LIST            
*                                                                               
         DC    X'03',AL1(12,01),X'F5050000C0',C'    ' FSI       ADD             
         DC    X'03',AL1(12,02),X'F5050000C0',C'    '           CHANGE          
         DC    X'03',AL1(12,03),X'F5050000C0',C'    '           DISPLAY         
         DC    X'03',AL1(12,04),X'F5050000C0',C'    '           DELETE          
         DC    X'03',AL1(12,05),X'F5050000C0',C'    '           SELECT          
         DC    X'03',AL1(12,06),X'F5050000C0',C'    '           RESTORE         
         DC    X'03',AL1(12,10),X'E5050005F8',C'FSSF'           LIST            
         DC    X'03',AL1(12,12),X'E505000578',C'FSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(13,01),X'FB0B0000C0',C'    ' UDEF      ADD             
         DC    X'03',AL1(13,02),X'FB0B0000C0',C'    '           CHANGE          
         DC    X'03',AL1(13,03),X'FB0B0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(13,04),X'FB0B0000C0',C'    '           DELETE          
         DC    X'03',AL1(13,05),X'FB0B0000C0',C'    '           SELECT          
         DC    X'03',AL1(13,06),X'FB0B0000C0',C'    '           RESTORE         
         DC    X'03',AL1(13,10),X'EB1B001BF8',C'UDUD'           LIST            
*                                                                               
         DC    X'03',AL1(14,01),X'FC0C0000C0',C'    ' CUREC     ADD             
         DC    X'03',AL1(14,02),X'FC0C0000C0',C'    '           CHANGE          
         DC    X'03',AL1(14,03),X'FC0C0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(14,04),X'FC0C0000C0',C'    '           DELETE          
         DC    X'03',AL1(14,05),X'FC0C0000C0',C'    '           SELECT          
         DC    X'03',AL1(14,06),X'FC0C0000C0',C'    '           RESTORE         
         DC    X'03',AL1(14,10),X'EC0C000CF8',C'CUSF'           LIST            
         DC    X'03',AL1(14,12),X'EC0C000C78',C'CUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(15,01),X'FD0D0000C0',C'    ' ISSUE     ADD             
         DC    X'03',AL1(15,02),X'FD0D0000C0',C'    '           CHANGE          
         DC    X'03',AL1(15,03),X'FD0D0000C0',C'    '           DISPLAY         
         DC    X'03',AL1(15,04),X'FD0D0000C0',C'    '           DELETE          
         DC    X'03',AL1(15,05),X'FD0D0000C0',C'    '           SELECT          
         DC    X'03',AL1(15,06),X'FD0D0000C0',C'    '           RESTORE         
         DC    X'03',AL1(15,10),X'ED0D000DF8',C'ISSF'           LIST            
         DC    X'03',AL1(15,12),X'ED0D000D78',C'ISSF'           REPORT          
*                                                                               
         DC    X'03',AL1(17,01),X'D0100000C0',C'    ' CIRC      ADD             
         DC    X'03',AL1(17,02),X'D0100000C0',C'    '           CHANGE          
         DC    X'03',AL1(17,03),X'D0100000C0',C'    '           DISPLAY         
         DC    X'03',AL1(17,04),X'D0100000C0',C'    '           DELETE          
         DC    X'03',AL1(17,05),X'D0100000C0',C'    '           SELECT          
         DC    X'03',AL1(17,06),X'D0100000C0',C'    '           RESTORE         
         DC    X'03',AL1(17,10),X'C0100010F8',C'CRSF'           LIST            
         DC    X'03',AL1(17,12),X'C010001078',C'CRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(18,01),X'D3130000C1',C'    ' EDR       ADD             
         DC    X'03',AL1(18,02),X'D3130000C1',C'    '           CHANGE          
         DC    X'03',AL1(18,03),X'D3130000C1',C'    '           DISPLAY         
         DC    X'03',AL1(18,04),X'D3130000C1',C'    '           DELETE          
         DC    X'03',AL1(18,05),X'D3130000C1',C'    '           SELECT          
         DC    X'03',AL1(18,06),X'D3130000C1',C'    '           RESTORE         
         DC    X'03',AL1(18,12),X'D313001318',C'EDSF'           REPORT          
*                                                                               
         DC    X'03',AL1(19,01),X'D4140000C1',C'    ' LOADEDR   ADD             
         DC    X'03',AL1(19,02),X'D4140000C1',C'    '           CHANGE          
         DC    X'03',AL1(19,03),X'D4140000C1',C'    '           DISPLAY         
         DC    X'03',AL1(19,04),X'D4140000C1',C'    '           DELETE          
         DC    X'03',AL1(19,05),X'D4140000C1',C'    '           SELECT          
         DC    X'03',AL1(19,06),X'D4140000C1',C'    '           RESTORE         
         DC    X'03',AL1(19,12),X'D414001418',C'EDSF'           REPORT          
*                                                                               
         DC    X'03',AL1(20,01),X'D5150000C0',C'    ' SPACE     ADD             
         DC    X'03',AL1(20,02),X'D5150000C0',C'    '           CHANGE          
         DC    X'03',AL1(20,03),X'D5150000C0',C'    '           DISPLAY         
         DC    X'03',AL1(20,04),X'D5150000C0',C'    '           DELETE          
         DC    X'03',AL1(20,05),X'D5150000C0',C'    '           SELECT          
         DC    X'03',AL1(20,06),X'D5150000C0',C'    '           RESTORE         
         DC    X'03',AL1(20,10),X'C5150015F8',C'SPSF'           LIST            
         DC    X'03',AL1(20,12),X'C515001578',C'SPSF'           REPORT          
*                                                                               
*OLD*    DC    X'03',AL1(21,17),X'D717000081',C'    ' BILLF     MAINT           
*                                                                               
         DC    X'03',AL1(22,01),X'D1120000C0',C'    ' BUDGET    ADD             
         DC    X'03',AL1(22,02),X'D1120000C0',C'    '           CHANGE          
         DC    X'03',AL1(22,03),X'D1120000C0',C'    '           DISPLAY         
         DC    X'03',AL1(22,04),X'D1120000C0',C'    '           DELETE          
         DC    X'03',AL1(22,05),X'D1120000C0',C'    '           SELECT          
         DC    X'03',AL1(22,06),X'D1120000C0',C'    '           RESTORE         
         DC    X'03',AL1(22,10),X'C1120000C0',C'    '           LIST            
         DC    X'03',AL1(22,18),X'D1120000C0',C'    '           TOTALS          
         DC    X'03',AL1(22,12),X'D112000078',C'BUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(34,01),X'B2260000C0',C'    ' CGROUP    ADD             
         DC    X'03',AL1(34,02),X'B2260000C0',C'    '           CHANGE          
         DC    X'03',AL1(34,03),X'B2260000C0',C'    '           DISPLAY         
         DC    X'03',AL1(34,04),X'B2260000C0',C'    '           DELETE          
         DC    X'03',AL1(34,05),X'B2260000C0',C'    '           SELECT          
         DC    X'03',AL1(34,06),X'B2260000C0',C'    '           RESTORE         
         DC    X'03',AL1(34,10),X'B9260000C0',C'    '           LIST            
         DC    X'03',AL1(34,12),X'BA26000078',C'CGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(35,03),X'B3270000C1',C'    ' CGASSIGN  DISPLAY         
*                                                      (CLT GRP ASSIGN)         
         DC    X'03',AL1(36,01),X'B1250000C0',C'    ' CGDEF     ADD             
         DC    X'03',AL1(36,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(36,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(36,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(36,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(36,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(36,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(37,01),X'B4290000C0',C'    ' PGROUP    ADD             
         DC    X'03',AL1(37,02),X'B4290000C0',C'    '           CHANGE          
         DC    X'03',AL1(37,03),X'B4290000C0',C'    '           DISPLAY         
         DC    X'03',AL1(37,04),X'B4290000C0',C'    '           DELETE          
         DC    X'03',AL1(37,05),X'B4290000C0',C'    '           SELECT          
         DC    X'03',AL1(37,06),X'B4290000C0',C'    '           RESTORE         
         DC    X'03',AL1(37,10),X'BD290000C0',C'    '           LIST            
         DC    X'03',AL1(37,12),X'BE29000078',C'PGSF'           REPORT          
*                                                                               
         DC    X'03',AL1(38,03),X'B52A0000C1',C'    ' PGASSIGN  DISPLAY         
*                                                      (PRD GRP ASSIGN)         
         DC    X'03',AL1(39,01),X'BB280000C0',C'    ' PGDEF     ADD             
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
         DC    X'03',AL1(41,03),X'B7270000C1',C'    ' PUB ASSGN DISPLAY         
*                                                      (PUB GRP ASSIGN)         
         DC    X'03',AL1(42,01),X'B1250000C0',C'    ' PUB G DEF ADD             
         DC    X'03',AL1(42,02),X'B1250000C0',C'    '           CHANGE          
         DC    X'03',AL1(42,03),X'B1250000C0',C'    '           DISPLAY         
         DC    X'03',AL1(42,04),X'B1250000C0',C'    '           DELETE          
         DC    X'03',AL1(42,05),X'B1250000C0',C'    '           SELECT          
         DC    X'03',AL1(42,06),X'B1250000C0',C'    '           RESTORE         
         DC    X'03',AL1(42,10),X'B8250000C0',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(43,01),X'D8180000C0',C'    ' PUBLIST   ADD             
         DC    X'03',AL1(43,02),X'D8180000C0',C'    '           CHANGE          
         DC    X'03',AL1(43,03),X'D8180000C0',C'    '           DISPLAY         
         DC    X'03',AL1(43,04),X'D8180000C0',C'    '           DELETE          
         DC    X'03',AL1(43,05),X'D8180000C0',C'    '           SELECT          
         DC    X'03',AL1(43,06),X'D8180000C0',C'    '           RESTORE         
         DC    X'03',AL1(43,10),X'C8180018F8',C'PLSF'           LIST            
         DC    X'03',AL1(43,12),X'C818001878',C'PLSF'           REPORT          
*                                                                               
         DC    X'03',AL1(44,12),X'BF8F008F18',C'CPSF' JWTCPP    REPORT          
*                                                                               
         DC    X'03',AL1(45,01),X'D3130000C1',C'    ' SRDS      ADD             
         DC    X'03',AL1(45,02),X'D3130000C1',C'    '           CHANGE          
         DC    X'03',AL1(45,03),X'D3130000C1',C'    '           DISPLAY         
         DC    X'03',AL1(45,04),X'D3130000C1',C'    '           DELETE          
         DC    X'03',AL1(45,05),X'D3130000C1',C'    '           SELECT          
         DC    X'03',AL1(45,06),X'D3130000C1',C'    '           RESTORE         
         DC    X'03',AL1(45,12),X'D313001318',C'SRSF'           REPORT          
*                                                                               
         DC    X'03',AL1(46,01),X'D9190000C0',C'    ' UCOMM     ADD             
         DC    X'03',AL1(46,02),X'D9190000C0',C'    '           CHANGE          
         DC    X'03',AL1(46,03),X'D9190000C0',C'    '           DISPLAY         
         DC    X'03',AL1(46,04),X'D9190000C0',C'    '           DELETE          
         DC    X'03',AL1(46,05),X'D9190000C0',C'    '           SELECT          
         DC    X'03',AL1(46,06),X'D9190000C0',C'    '           RESTORE         
         DC    X'03',AL1(46,10),X'C9190019C0',C'UCSF'           LIST            
*                                                                               
         DC    X'03',AL1(47,01),X'D2200000C0',C'    ' CHARGE    ADD             
         DC    X'03',AL1(47,02),X'D2200000C0',C'    '           CHANGE          
         DC    X'03',AL1(47,03),X'D2200000C0',C'    '           DISPLAY         
         DC    X'03',AL1(47,04),X'D2200000C0',C'    '           DELETE          
         DC    X'03',AL1(47,05),X'D2200000C0',C'    '           SELECT          
         DC    X'03',AL1(47,06),X'D2200000C0',C'    '           RESTORE         
*                                                                               
         DC    X'03',AL1(48,12),X'BF8E008E18',C'CPSF' MSHCPP    REPORT          
*                                                                               
         DC    X'03',AL1(49,17),X'D6210000C0',C'    ' STDSPACE  MAINT           
         DC    X'03',AL1(49,12),X'D621002178',C'DSSF'           REPORT          
*                                                                               
         DC    X'03',AL1(50,01),X'DA220000C0',C'    ' CUSTCOL   ADD             
         DC    X'03',AL1(50,02),X'DA220000C0',C'    '           CHANGE          
         DC    X'03',AL1(50,03),X'DA220000C0',C'    '           DISPLAY         
         DC    X'03',AL1(50,04),X'DA220000C0',C'    '           DELETE          
         DC    X'03',AL1(50,05),X'DA220000C0',C'    '           SELECT          
         DC    X'03',AL1(50,06),X'DA220000C0',C'    '           RESTORE         
         DC    X'03',AL1(50,10),X'CA220022F8',C'CCSF'           LIST            
         DC    X'03',AL1(50,12),X'CA22002278',C'CCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(61,01),X'A6310000C0',C'    ' VCCLIST   ADD             
         DC    X'03',AL1(61,02),X'A6310000C0',C'    '           CHANGE          
         DC    X'03',AL1(61,03),X'A6310000C0',C'    '           DISPLAY         
         DC    X'03',AL1(61,04),X'A6310000C0',C'    '           DELETE          
         DC    X'03',AL1(61,05),X'A6310000C0',C'    '           SELECT          
         DC    X'03',AL1(61,06),X'A6310000C0',C'    '           RESTORE         
         DC    X'03',AL1(61,10),X'A7310000C0',C'    '           LIST            
         DC    X'03',AL1(61,12),X'A731003178',C'VCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(62,01),X'A8320000C0',C'    ' ACCLIST   ADD             
         DC    X'03',AL1(62,02),X'A8320000C0',C'    '           CHANGE          
         DC    X'03',AL1(62,03),X'A8320000C0',C'    '           DISPLAY         
         DC    X'03',AL1(62,04),X'A8320000C0',C'    '           DELETE          
         DC    X'03',AL1(62,05),X'A8320000C0',C'    '           SELECT          
         DC    X'03',AL1(62,06),X'A8320000C0',C'    '           RESTORE         
         DC    X'03',AL1(62,10),X'A9320000C0',C'    '           LIST            
         DC    X'03',AL1(62,12),X'A932003278',C'ACSF'           REPORT          
*                                                                               
         DC    X'03',AL1(63,01),X'AA330000C0',C'    ' SETUP     ADD             
         DC    X'03',AL1(63,02),X'AA330000C0',C'    ' (WAS      CHANGE          
         DC    X'03',AL1(63,03),X'AA330000C0',C'    '  SCHEMA)  DISPLAY         
         DC    X'03',AL1(63,04),X'AA330000C0',C'    '           DELETE          
         DC    X'03',AL1(63,05),X'AA330000C0',C'    '           SELECT          
         DC    X'03',AL1(63,06),X'AA330000C0',C'    '           RESTORE         
         DC    X'03',AL1(63,10),X'AB330000C0',C'    '           LIST            
         DC    X'03',AL1(63,12),X'AB33003378',C'SUSF'           REPORT          
*                                                                               
         DC    X'03',AL1(64,01),X'AD340000C0',C'    ' SUBMEDIA  ADD             
         DC    X'03',AL1(64,02),X'AD340000C0',C'    '           CHANGE          
         DC    X'03',AL1(64,03),X'AD340000C0',C'    '           DISPLAY         
         DC    X'03',AL1(64,04),X'AD340000C0',C'    '           DELETE          
         DC    X'03',AL1(64,05),X'AD340000C0',C'    '           SELECT          
         DC    X'03',AL1(64,06),X'AD340000C0',C'    '           RESTORE         
         DC    X'03',AL1(64,10),X'AE340000C0',C'    '           LIST            
         DC    X'03',AL1(64,12),X'AE34003478',C'SMED'           REPORT          
*                                                                               
         DC    X'03',AL1(65,01),X'DB350000C0',C'    ' BFORM     ADD             
         DC    X'03',AL1(65,02),X'DB350000C0',C'    '           CHANGE          
         DC    X'03',AL1(65,03),X'DB350000C0',C'    '           DISPLAY         
         DC    X'03',AL1(65,04),X'DB350000C0',C'    '           DELETE          
         DC    X'03',AL1(65,05),X'DB350000C0',C'    '           SELECT          
         DC    X'03',AL1(65,06),X'DB350000C0',C'    '           RESTORE         
         DC    X'03',AL1(65,10),X'DC350000F8',C'BFSF'           LIST            
         DC    X'03',AL1(65,12),X'DC35003578',C'BFSF'           REPORT          
*                                                                               
         DC    X'03',AL1(67,01),X'C2410000C0',C'    ' SRVCCLST  ADD             
         DC    X'03',AL1(67,02),X'C2410000C0',C'    '           CHANGE          
         DC    X'03',AL1(67,03),X'C2410000C0',C'    '           DISPLAY         
         DC    X'03',AL1(67,04),X'C2410000C0',C'    '           DELETE          
         DC    X'03',AL1(67,05),X'C2410000C0',C'    '           SELECT          
         DC    X'03',AL1(67,06),X'C2410000C0',C'    '           RESTORE         
         DC    X'03',AL1(67,10),X'C3410000C0',C'    '           LIST            
         DC    X'03',AL1(67,12),X'C341004178',C'SVSF'           REPORT          
*                                                                               
         DC    X'03',AL1(68,01),X'C6420000C0',C'    ' SRACCLST  ADD             
         DC    X'03',AL1(68,02),X'C6420000C0',C'    '           CHANGE          
         DC    X'03',AL1(68,03),X'C6420000C0',C'    '           DISPLAY         
         DC    X'03',AL1(68,04),X'C6420000C0',C'    '           DELETE          
         DC    X'03',AL1(68,05),X'C6420000C0',C'    '           SELECT          
         DC    X'03',AL1(68,06),X'C6420000C0',C'    '           RESTORE         
         DC    X'03',AL1(68,10),X'AC420000C0',C'    '           LIST            
         DC    X'03',AL1(68,12),X'AC42004278',C'SASF'           REPORT          
*                                                                               
         DC    X'03',AL1(69,01),X'DD360000C0',C'    ' PO#       ADD             
         DC    X'03',AL1(69,02),X'DD360000C0',C'    '           CHANGE          
         DC    X'03',AL1(69,03),X'DD360000C0',C'    '           DISPLAY         
*******  DC    X'03',AL1(69,04),X'DD360000C0',C'    '           DELETE          
         DC    X'03',AL1(69,05),X'DD360000C0',C'    '           SELECT          
*******  DC    X'03',AL1(69,06),X'DD360000C0',C'    '           RESTORE         
         DC    X'03',AL1(69,10),X'DE360000C0',C'POSF'           LIST            
         DC    X'03',AL1(69,12),X'DE36003678',C'POSF'           REPORT          
*                                                                               
         DC    X'03',AL1(70,01),X'DA220000C0',C'    ' STDCOL    ADD             
         DC    X'03',AL1(70,02),X'DA220000C0',C'    '           CHANGE          
         DC    X'03',AL1(70,03),X'DA220000C0',C'    '           DISPLAY         
         DC    X'03',AL1(70,04),X'DA220000C0',C'    '           DELETE          
         DC    X'03',AL1(70,05),X'DA220000C0',C'    '           SELECT          
         DC    X'03',AL1(70,06),X'DA220000C0',C'    '           RESTORE         
         DC    X'03',AL1(70,10),X'CA220000F8',C'SCSF'           LIST            
         DC    X'03',AL1(70,12),X'CA22002278',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(71,01),X'DF370000C0',C'    ' CPUB      ADD             
         DC    X'03',AL1(71,02),X'DF370000C0',C'    '           CHANGE          
         DC    X'03',AL1(71,03),X'DF370000C0',C'    '           DISPLAY         
         DC    X'03',AL1(71,04),X'DF370000C0',C'    '           DELETE          
         DC    X'03',AL1(71,05),X'DF370000C0',C'    '           SELECT          
         DC    X'03',AL1(71,06),X'DF370000C0',C'    '           RESTORE         
         DC    X'03',AL1(71,10),X'EF370000F8',C'SCSF'           LIST            
         DC    X'03',AL1(71,12),X'EF37003778',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(72,01),X'E3380000C0',C'    ' CPUB      ADD             
         DC    X'03',AL1(72,02),X'E3380000C0',C'    '           CHANGE          
         DC    X'03',AL1(72,03),X'E3380000C0',C'    '           DISPLAY         
         DC    X'03',AL1(72,04),X'E3380000C0',C'    '           DELETE          
         DC    X'03',AL1(72,05),X'E3380000C0',C'    '           SELECT          
         DC    X'03',AL1(72,06),X'E3380000C0',C'    '           RESTORE         
         DC    X'03',AL1(72,10),X'E4380000F8',C'SCSF'           LIST            
         DC    X'03',AL1(72,12),X'E438003878',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(72,01),X'E3380000C0',C'    ' EXCHANGE  ADD             
         DC    X'03',AL1(72,02),X'E3380000C0',C'    '           CHANGE          
         DC    X'03',AL1(72,03),X'E3380000C0',C'    '           DISPLAY         
         DC    X'03',AL1(72,04),X'E3380000C0',C'    '           DELETE          
         DC    X'03',AL1(72,05),X'E3380000C0',C'    '           SELECT          
         DC    X'03',AL1(72,06),X'E3380000C0',C'    '           RESTORE         
         DC    X'03',AL1(72,10),X'E4380000F8',C'SCSF'           LIST            
         DC    X'03',AL1(72,12),X'E438003878',C'SCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(73,01),X'9E390000C0',C'    ' DCOMMENT  ADD             
         DC    X'03',AL1(73,02),X'9E390000C0',C'    '           CHANGE          
         DC    X'03',AL1(73,03),X'9E390000C0',C'    '           DISPLAY         
         DC    X'03',AL1(73,04),X'9E390000C0',C'    '           DELETE          
         DC    X'03',AL1(73,05),X'9E390000C0',C'    '           SELECT          
         DC    X'03',AL1(73,06),X'9E390000C0',C'    '           RESTORE         
         DC    X'03',AL1(73,10),X'9F390000F8',C'DCSF'           LIST            
         DC    X'03',AL1(73,12),X'9F39003978',C'DCSF'           REPORT          
*                                                                               
         DC    X'03',AL1(74,02),X'9D430000C0',C'    ' CUSTMENU  CHANGE          
         DC    X'03',AL1(74,03),X'9D430000C0',C'    '           DISPLAY         
         DC    X'03',AL1(74,05),X'9D430000C0',C'    '           SELECT          
         DC    X'03',AL1(74,10),X'9C430000F8',C'DMSF'           LIST            
         DC    X'03',AL1(74,12),X'9C43004378',C'DMSF'           REPORT          
*                                                                               
         DC    X'03',AL1(75,02),X'9D430000C0',C'    ' STDMENU   CHANGE          
         DC    X'03',AL1(75,03),X'9D430000C0',C'    '           DISPLAY         
         DC    X'03',AL1(75,05),X'9D430000C0',C'    '           SELECT          
         DC    X'03',AL1(75,10),X'9C430000F8',C'DMSF'           LIST            
         DC    X'03',AL1(75,12),X'9C43004378',C'DMSF'           REPORT          
*                                                                               
         DC    X'03',AL1(76,03),X'9A440000C0',C'    ' AUTOPAY   DISPLAY         
         DC    X'03',AL1(76,05),X'9A440000C0',C'    ' AUTOPAY   SELECT          
         DC    X'03',AL1(76,10),X'9B440000F8',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(77,01),X'8C400000C0',C'    ' BILLADR   ADD             
         DC    X'03',AL1(77,02),X'8C400000C0',C'    '           CHANGE          
         DC    X'03',AL1(77,03),X'8C400000C0',C'    '           DISPLAY         
         DC    X'03',AL1(77,04),X'8C400000C0',C'    '           DELETE          
         DC    X'03',AL1(77,05),X'8C400000C0',C'    '           SELECT          
         DC    X'03',AL1(77,06),X'8C400000C0',C'    '           RESTORE         
         DC    X'03',AL1(77,10),X'8D400000C0',C'    '           LIST            
*                                                                               
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKRECSCR NTR1  BASE=*,LABEL=*      CK FOR REC SCR (CLT/PRD/EST ETC...)          
*                                                                               
         L     R4,SYSPARMS                                                      
         L     R4,4(R4)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R4                                                   
*                                                                               
         LA    R5,CKRSTAB          POINT TO SCR/PRG TABLE                       
CKRSC20  CLI   0(R5),0             END OF TABLE?                                
         JE    SETCCNEQ                                                         
         CLC   TWASCR,0(R5)        SCR IS DEFINED IN TABLE?                     
         BE    CKRSC50                                                          
         LA    R5,2(R5)            NEXT ENTRY IN TABLE                          
         B     CKRSC20                                                          
*                                                                               
CKRSC50  CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         BNE   CKRSCX                                                           
*                                                                               
* FOR PRGS SHARE COMMON SCRS, SET PRG OVERLAY HERE                              
*                                                                               
         CLI   0(R5),X'A2'         PRBILL OR ESBILL SCR?                        
         BNE   *+16                                                             
         CLI   RECNUM,56           PRBILL?                                      
         BNE   *+08                                                             
         MVI   1(R5),X'1F'         SET PRG OVERLAY TO BE CALLED                 
*                                                                               
         CLI   1(R5),0             NEED TO CALL PROGRAM OVERLAY?                
         BE    CKRSCX                                                           
         CLI   WNATION,0           NATIONALITY ALREADY KNOWN?                   
         BNE   *+8                                                              
         BRAS  RE,CKRS_NAT         GET NATIONALITY                              
*                                                                               
         XC    DMCB(12),DMCB       PREPARE FOR PRG OVERLAY CALL                 
         MVC   DMCB(1),1(R5)       PROGRAM NUMBER                               
         GOTO1 CALLOV,DMCB         GET OVERLAY ADDRESS                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                PHASE NOT FOUND                              
         L     RF,DMCB             EXECUTE OVERLAY                              
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
CKRSCX   J     SETCCEQ                                                          
*                                                                               
* ROUTINE TO GET NATIONAILTY (WILL READ AGENCY RECORD)                          
*                                                                               
CKRS_NAT ST    RE,FULL                                                          
         XC    KEY,KEY             NO NEED TO SAVE, NOT BEING USED YET          
         LA    RE,KEY                                                           
         USING PAGYKEY,RE                                                       
         MVC   PAGYKAGY,AGENCY                                                  
         MVI   PAGYKMED,C'I'       FIRST MEDIA TYPE                             
         MVI   PAGYKRCD,X'01'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLI   KEY+3,X'01'         MEDIA RECORD FOUND?                          
         BNE   CKRS_NAX            NO, STOP EVERYTHING                          
*                                                                               
         MVC   DUB(L'AIO),AIO      SAVE AIO POINTER                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,DUB             RESTORE AIO POINTER                          
         L     R6,AIO2                                                          
         USING PAGYELEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL            MAIN ELEM FOUND?                             
         BNE   CKRS_NAX            NO, STOP EVERYTHING                          
         MVC   WNATION,PAGYNAT     NATIONALITY                                  
         DROP  R6                                                               
*                                                                               
CKRS_NAX L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* 1ST BYTE = SCREEN NUMBER, 2ND BYTE = PROGRAM NUMBER                           
* IF PROGRAM NUMBER IS X'00' THEN NO NEED TO CALL IT                            
*                                                                               
CKRSTAB  DS    0H                                                               
         DC    X'B01C'             CLIENT   MAINT                               
         DC    X'C400'             CLIENT   LIST                                
         DC    X'C71D'             CLIENT2  MAINT                               
*                                                                               
         DC    X'A01E'             PRODUCT  MAINT                               
         DC    X'A100'             PRODUCT  LIST                                
*                                                                               
         DC    X'A300'             ESTIMATE MAINT                               
         DC    X'A400'             ESTIMATE LIST                                
         DC    X'A500'             EST2     MAINT                               
         DC    X'E000'             ESTIMATE COPY                                
*                                                                               
         DC    X'A200'             PRD BILL MAINT (ALSO EST BILL)               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         DROP  R4,RB               FROM CKRECSCR ROUTINE                        
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC0D          CIRC LIST SCREEN                             
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC8D          PUBLIST LIST SCREEN                          
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMECD          CUREC LIST SCREEN                            
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC5D          SPACE LIST SCREEN                            
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA7D          EIO VCCLIST IST SCREEN                       
         EJECT                                                                  
*                                                                               
*****    ORG   CONTAGH             SAME DSECT AS PRSFMA7D ABOVE                 
*********INCLUDE PRSFMC3D          ESR VCCLIST IST SCREEN                       
*****    EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
* PROGRAM SAVED STORAGE AT BOTTOM OF TWA0                                       
*                                                                               
         ORG   T41CFFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
*                                                                               
STSAVE   EQU   *                                                                
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SVSPARE  DS    CL(TWAMXLEN-(*-STSAVE))  SPARE - DEFINE NEW AREAS ABOVE          
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
*                                                                               
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
*                                                                               
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*                                                                               
PDIVRECD DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
*                                                                               
PREGRECD DSECT                                                                  
       ++INCLUDE PREGREC                                                        
         EJECT                                                                  
*                                                                               
PDSTRECD DSECT                                                                  
       ++INCLUDE PDSTREC                                                        
         EJECT                                                                  
*                                                                               
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
*                                                                               
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
*                                                                               
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
         EJECT                                                                  
*                                                                               
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
*                                                                               
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'203PRSFM00   07/07/20'                                      
         END                                                                    
