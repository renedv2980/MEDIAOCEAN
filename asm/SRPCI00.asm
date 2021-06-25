*          DATA SET SRPCI00    AT LEVEL 023 AS OF 12/22/20                      
*PHASE T12100A                                                                  
*INCLUDE ACCESS                                                                 
*INCLUDE FAVIRTRM                                                               
*INCLUDE OFFLAL                                                                 
*INCLUDE PWDGEN                                                                 
*&&US                                                                           
*INCLUDE MEDGET                    CALLED BY ACCESS                             
*&&                                                                             
*INCLUDE KHDUMMY                                                                
*                                                                               
         TITLE 'SRPCI00 - MAINFRAME TO PC INTERFACE SERVICE REQUEST'            
         PRINT NOGEN                                                            
T12100   CSECT                                                                  
         NMOD1 LENWORK,**$PC***,RA,R9,R8,CLEAR=YES,RR=RE                        
         ST    RE,RELO                                                          
*                                                                               
         USING CONTROLD,RC                                                      
         ST    R1,APARMS           SAVE PARAMETER LIST ADDRESS                  
         L     RF,0(R1)            SAVE A(SYSFACS)                              
         ST    RF,ASYSFACS                                                      
         L     RE,VSSB-SYSFACD(RF)                                              
         ST    RE,ASSB             SAVE A(SSB)                                  
         L     RF,SSBTKADR-SSBD(RE)                                             
         ST    RF,ATCB             SAVE A(TCB)                                  
         L     RF,4(R1)            SAVE A(TIA)                                  
         ST    RF,ATIA                                                          
         L     RF,8(R1)            SAVE A(UTL)                                  
         ST    RF,AUTL                                                          
         L     RF,12(R1)           SAVE A(COMFACS)                              
         ST    RF,ACOMFACS                                                      
         L     RF,20(R1)           SAVE A(TWA)                                  
         ST    RF,ATWA                                                          
         L     RF,28(R1)           SAVE A(TIOB)                                 
         ST    RF,ATIOB                                                         
         ST    RD,SAVERD           SAVE STACK ADDRESS FOR EXIT                  
                                                                                
* MAIN (HIGHEST LEVEL ROUTINE)                                                  
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INITNMOD         INITIALIZE ADDRS OF NMOD RESOURCES           
         BAS   RE,INITRTNS                    ROUTINE ADDRESSES                 
         BAS   RE,INITMISC                    OTHER VARIABLES                   
         BAS   RE,READOLD          READ SAVED STORAGE                           
*&&UK                                                                           
         BAS   RE,VALACCES         VALIDATE PC ACCESS TO $PC (UK ONLY)          
         BNE   M90                                                              
*&&                                                                             
         CLI   TRACEIN,C'Y'        LOG INPUT VALUE                              
         BNE   *+8                                                              
         BRAS  RE,WTOERR                                                        
*                                                                               
         BAS   RE,SCANTWA          SCAN CONTROL AND DATA FRAMES                 
         BNE   M90                                                              
*                                                                               
         MVC   PCIACT,PCACTION     COPY PC ACTION TO MAD                        
*                                                                               
M02      CLC   PCACTION,=Y(0)      DEFAULT ACTION IS CLEAR SCREEN               
         BE    M90                                                              
*                                  ALSO IF ACTION IS ABORT                      
M10      CLC   PCACTION,=Y(ACABORT)                                             
         BE    M90                                                              
*                                                                               
M30      BAS   RE,PROCSTRT         ELSE START NEW ACTION                        
*                                                                               
M90      BAS   RE,BLDTWA           BUILD CONTROL AND DATA FRAMES                
*                                                                               
         BAS   RE,SAVESTR          WRITE SAVED STORAGE                          
*                                                                               
MX       L     R7,ATWA             SET SPECIAL FLAGS IN TWA                     
         USING T121FFD,R7                                                       
         OI    SRVSRVH+6,X'81'     SET MOD AND TRANS FLAGS ON SR FLD            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR TO LAST FIELD ENTERED             
         MVI   TIOBCURI,X'00'                                                   
         OI    SRVPC2MH+6,X'01'    SET MODIFY FLAG ON PC CONTROL FLD            
*                                                                               
         B     XDIRECT             EXIT MODULE                                  
         DROP  R7,RF                                                            
                                                                                
* SAVE ADDRESSES OF SECTIONS OF MEMORY CREATED BY THE CONTROLLER'S              
* NMOD INSTRUCTION.  ADDITIONALLY, SPLIT UP MEMORY USED FOR IO AREAS            
* INTO TWO AND SAVE THE ADDRESS OF EACH.                                        
*                                                                               
INITNMOD NTR1                                                                   
         ST    RB,ABASE1           SAVE A(FIRST BASE)                           
         ST    R9,ABASE2           SAVE A(SECOND BASE)                          
         ST    R8,ABASE3           SAVE A(THIRD BASE)                           
*                                                                               
         LR    RF,RC               BUMP PAST FIRST HALF OF CONTROLD             
         A     RF,=A(LENCON)                                                    
*                                                                               
         ST    RF,ASAVE            SAVE A(CONTROLLER SAVED MEMORY)              
         A     RF,=A(LENSAVE)      BUMP PAST                                    
*                                                                               
         ST    RF,AINFRM           SAVE A(INPUT FRAME)                          
         A     RF,=A(LENINF)       BUMP PAST                                    
*                                                                               
         ST    RF,AOUTFRM          SAVE A(OUTPUT FRAME)                         
         A     RF,=A(LENOUTF)      BUMP PAST                                    
*                                                                               
         ST    RF,AIOS             SAVE A(DATAMGR IO AREAS)                     
         A     RF,=A(LENIOS)       BUMP PAST                                    
*                                                                               
         ST    RF,APCTWASV         A(PC SCREEN TWA SAVE AREA)                   
         A     RF,=A(LENTWASV)     BUMP PAST                                    
*                                                                               
         ST    RF,AITSVTAB         SAVE A(ITEM DATA SAVE TABLE)                 
*                                                                               
         L     RF,=A(ITCVTAB)      SAVE A(ITEM DATA CONVERSION TABLE)           
         A     RF,RELO                                                          
         ST    RF,AITCVTAB                                                      
                                                                                
* SPLIT UP IO AREA MEMORY INTO TWO AND SAVE THE ADDRESS OF EACH                 
* HALF AS AIO1 AND AIO2.                                                        
*                                                                               
         MVC   AIO1,AIOS           SAVE A(IOAREA #1)                            
         L     RF,AIO1                                                          
         AHI   RF,(LENIOS/2)       TWO 4K IO AREAS                              
         ST    RF,AIO2             SAVE A(IOAREA #2)                            
         MVC   AIO,AIO1            SAVE A(CURRENT IO AREA)                      
         B     XIT                                                              
                                                                                
* SAVE ADDRESSES OF THE ROUTINES USED BY $PC.  THERE ARE FOUR                   
* TYPES OF ROUTINES:                                                            
*                                                                               
* 1) ROUTINES FOUND IN FACPAK'S COMFACS DSECT                                   
* 2) ROUTINES LINKED WITH THE CONTROLLER                                        
* 3) ROUTINES FOUND IN FACPAK'S CORE                                            
* 4) ROUTINES FOUND IN THE CONTROLLER                                           
*                                                                               
* ROUTINES LINKED WITH THE CONTROLLER AND ROUTINES IN THE CONTROLLER            
* ITSELF MUST BE DYNAMICALLY RELOCATED BY ADDING THE RELOCATION                 
* CONSTANT (RELO) TO EACH ROUTINE ADDRESS.                                      
*                                                                               
INITRTNS NTR1                                                                   
         L     RF,ACOMFACS         SAVE ADDRESSES OF COMFACS ROUTINES           
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   SCANNER,CSCANNER                                                 
         MVC   TERMVAL,CTERMVAL                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   GETRET,CGETRET                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   HEXOUT,CHEXOUT                                                   
         DROP  RF                                                               
                                                                                
* SAVE ADDRESSES OF LINKED ROUTINES                                             
*                                                                               
         LA    R2,LINKED           R2=A(FIRST VTYPE ADDRESS)                    
         LA    R3,EXTERNS          R3=A(FIRST RELOCATED ADDRESS)                
         LA    R4,NLINKED          R4=NUMBER OF LINKED ROUTINES                 
         LTR   R4,R4               IF NO LINKED ROUTINES THEN DONE              
         BZ    IR20                                                             
*                                                                               
IR10     L     R1,0(R2)            R1=V-TYPE ADDRESS                            
         A     R1,RELO             ADD RELOCATION CONSTANT                      
         ST    R1,0(R3)            SAVE RELOCATED ADDRESS                       
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR10             LOOP BACK                                    
                                                                                
* SAVE ADDRESSES OF CORE RESIDENT ROUTINES                                      
*                                                                               
IR20     XC    DMCB,DMCB           SET UP PARAMETERS TO CALLOV                  
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
         LA    R2,CORERES          R2=A(FIRST CORERES ROUTINE EQUATE)           
         LA    R3,COREADRS         R3=A(FIRST CORERES ROUTINE ADDR)             
         LA    R4,NCORERES         R4=NUMBER OF CORERES ROUTINES                
         LTR   R4,R4               IF NO CORERES ROUTINES THEN DONE             
         BZ    IR40                                                             
*                                                                               
IR30     MVC   DMCB+7(1),0(R2)     INSERT OVERLAY NUMBER INTO PARAMETER         
         GOTO1 CALLOV,DMCB         CALL CALLOV                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE IF CALLOV RETURNS ERROR                  
         MVC   0(4,R3),DMCB        SAVE ROUTINE ADDRESS                         
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR30             LOOP BACK                                    
                                                                                
* SAVE ADDRESSES OF CONTROLLER ROUTINES                                         
* ALL CONTROLLER ROUTINES ARE ENTERED THROUGH AN INTERMEDIATE                   
* ROUTINE CALLED VCOMMON.  VCOMMON SETS UP THE BASE REGISTERS                   
* AND CALLS THE DESIRED ROUTINE.  THE WAY THIS WORKS IS THAT ALL                
* CONTOLLER ROUTINE ADDRESSES ARE REALLY THE ADDRESS OF VCOMMON WITH            
* A ROUTINE NUMBER IN THE HIGH ORDER BYTE TO TELL VCOMMON WHICH ROUTINE         
* TO BRANCH TO.                                                                 
*                                                                               
IR40     LA    R2,VCOMMON          R2=A(VCOMMON)                                
         SR    R3,R3               R3=0                                         
         LA    R4,COMMADRS         R4=A(FIRST ROUTINE ADDRESS)                  
         LA    R5,VCOUNT           R5=NUMBER OF CONTROLLER ROUTINES             
         LTR   R5,R5               IF NO CONTROLLER ROUTINES THEN DONE          
         BZ    IR60                                                             
*                                                                               
IR50     ST    R2,0(R4)            SAVE A(VCOMMON) IN LAST 3 BYTES              
         STC   R3,0(R4)            SAVE ROUTINE NUMBER IN FIRST BYTE            
         LA    R3,1(R3)            BUMP ROUTINE NUMBER                          
         LA    R4,4(R4)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R5,IR50             LOOP BACK                                    
*                                                                               
IR60     DS    0H                                                               
*                                                                               
IRX      B     XIT                                                              
                                                                                
* INITIALIZE OTHER MISCELLANEOUS VARIABLES                                      
*                                                                               
INITMISC NTR1                                                                   
         L     RF,ASSB             RF=A(SSB)                                    
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL      GET TEMPSTR RECORD LENGTH                    
         CLC   RECLEN,=H'18432'    18K TEMPSTR DISPLACEMENTS                    
         BNE   INIM010                                                          
         MVC   GLODSP,=Y(CHKPTGLD) DISPLACEMENT TO GLOBALS                      
         MVC   CHKDSP,=Y(CHKPTDSP) DISPLACEMENT TO CHECK POINT                  
         B     INIM030                                                          
INIM010  CLC   RECLEN,=H'14336'    14K TEMPSTR DISPLACEMENTS                    
         BNE   INIM020                                                          
         MVC   GLODSP,=H'12888'                                                 
         MVC   CHKDSP,=H'12800'                                                 
         B     INIM030                                                          
INIM020  DC    H'0'                                                             
         DROP  RF                                                               
                                                                                
* INITIALIZE VARIABLES FROM UTL                                                 
*                                                                               
INIM030  L     RF,AUTL             RF=A(UTL)                                    
         USING UTLD,RF                                                          
         OI    TFLAG,TFLAGIRB      INHIBIT BROADCAST MESSAGES                   
         MVC   CTUSER,TUSER        SET $CT USER ID                              
         DROP  RF                                                               
                                                                                
* INITIALIZE VARIABLES USED IN CALLSUB                                          
*                                                                               
         MVC   CSPHASE,=X'D9012100'   PHASE PARAMETER FOR CALLOV CALL           
         MVI   CSSP,0                 OVERLAY STACK POINTER                     
         MVC   CSNXTLOD,DUMMY         A(NEXT PLACE TO LOAD OVERLAY)             
                                                                                
* INITIALIZE PCI CONTROL FRAME VARIABLES                                        
* ASSUME MAXIMIMUM OF ONE FRAME IN AND OUT PER DIALOGUE                         
*                                                                               
         MVC   PCIVRS,=H'001'      PCI VERSION NUMBER                           
         MVC   PCIFRAME,=F'1'      PCI FRAME NUMBER                             
         MVI   PCILAST,C'Y'        LAST FRAME FLAG = 'Y'                        
         MVI   PCIDEBUG,C'N'       DEBUG FLAG = 'N'                             
                                                                                
* INITIALIZE GETITEM AND PUTITEM VARIABLES                                      
*                                                                               
         MVI   GETINIT,C'N'        GETITEM NOT YET INITIALIZED                  
         MVI   PUTINIT,C'N'        PUTITEM NOT YET INITIALIZED                  
*                                                                               
INX      B     XIT                                                              
                                                                                
*&&UK                                                                           
* THIS ROUTINE DETERMINES THE PC'S AUTHORIZATION TO USE $PC BY CALLING          
* TERMVAL AND LOOKING AT THE TERMINAL DEFINITION ELEMENT IT RETURNS.            
* IF THE TERMINAL ATTRIBUTE BYTE INDICATES THAT THE PC IS NOT                   
* AUTHORIZED TO USE $PC, THEN THE ROUTINE SETS PCIACT TO 'NOT                   
* AUTHORIZED' AND RETURNS 'NO'.  OTHERWISE IT RETURNS 'YES'.                    
*                                                                               
VALACCES NTR1                                                                   
         GOTO1 TERMVAL,DMCB,(X'30',0),AUTL,0,0                                  
         ICM   RE,7,13(R1)         ERROR IF NO TERMINAL DEFINITION EL           
         BZ    VAAUTH                                                           
         TM    CTTRMAT1-CTTRMD(RE),X'80'                                        
         BNO   VAAUTH              ERR IF NOT AUTHORIZED TO ACCESS $PC          
         B     VAYES                                                            
*                                                                               
VAAUTH   MVC   PCIACT,=Y(NOTAUTH)  PC NOT AUTHORIZED TO USE $PC                 
*                                                                               
VANO     B     NO                                                               
VAYES    B     YES                                                              
*&&                                                                             
                                                                                
* SCAN INCOMING PC MESSAGE.  EXTRACT SERVICE REQUEST FIELD VALUE INTO           
* CONTROLLER WORK AREA AND EXTRACT DATA FRAME FIELDS INTO FRAME AREA.           
*                                                                               
SCANTWA  NTR1                                                                   
         L     R7,ATWA             R7=A(TWA)                                    
         USING T121FFD,R7                                                       
*                                                                               
STPCI    LA    R3,SRVSRVH          R3=A(SERVICE REQUEST FIELD)                  
         ZIC   R4,5(R3)            EXTRACT LENGTH INPUT FIELD                   
         LA    R3,8(R3)            R3=A(DATA)                                   
         SR    R2,R2                                                            
*                                  SEARCH FOR ACTION PARAMETER                  
STPL1    LA    R2,1(R2)            AFTER =PC, OR SYNONYM, IF PRESENT            
         CR    R2,R4                                                            
         BH    STDF                NOT FOUND ASSUME DEFAULT ACTION              
         CLI   0(R3),C','          COMMA IS SEPARATOR                           
         BE    STPL1X                                                           
         LA    R3,1(R3)            NEXT CHARACTER                               
         B     STPL1                                                            
*                                                                               
STPL1X   LA    R3,1(R3)            INTERPRET PARAMETERS                         
         SR    R4,R2                                                            
         SR    R2,R2                                                            
         MVC   GETIT,GETITEM       DEFAULT GET ITEM ROUTINE                     
*                                                                               
STPL2    LA    R2,1(R2)            SEARCH FOR NEXT SEPARATOR                    
         CR    R2,R4               UPTO END OF FIELD                            
         BH    STPL2X                                                           
         CLI   0(R3),C','          IF FOUND TEST FOR SECOND PARAMETER           
         BNE   STPL2A                                                           
         CLI   1(R3),C'M'                                                       
         BNE   STACT               ELSE INVALID                                 
         MVC   GETIT,GEMITEM       MANUAL GET ITEM ROUTINE                      
         B     STPL2X                                                           
*                                                                               
STPL2A   CLI   0(R3),C'0'          TEST ACTION CODE NUMERIC                     
         BL    STACT                                                            
         CLI   0(R3),C'9'                                                       
         BH    STACT                                                            
         LA    R3,1(R3)                                                         
         B     STPL2                                                            
*                                                                               
STPL2X   C     R2,=F'4'            MAXIMIMUM 3 DIGIT ACTION CODE                
         BH    STACT                                                            
         BCTR  R2,0                CONVERT ACTION CODE VALUE                    
         LTR   R2,R2                                                            
         BNP   STACT                                                            
         SR    R3,R2                                                            
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R0,DUB                                                           
         STH   R0,PCACTION         AND SAVE IT                                  
         CLC   PCACTION,=Y(ACWHOAMI) IF STATUS DATA REQUEST                     
         BE    STDF                IGNORE PC CONTROL FRAME DATA                 
         B     STPCCF                                                           
                                                                                
* EXTRACT AND CHECK PC CONTROL FRAME INCOMING DATA                              
*                                                                               
STPCCF   LA    R3,SRVPC2M          R3=A(PC CONTROL FRAME)                       
         USING COMHDRD,R3                                                       
                                                                                
* EXTRACT ALINK VERSION NUMBER                                                  
*                                                                               
         GOTO1 TESTNUM,DMCB,COMVRS,3                                            
         BNE   STVRS                                                            
         PACK  DUB,COMVRS          ELSE PACK NUMBER INTO PCVRS                  
         CVB   R0,DUB                                                           
         STH   R0,PCVRS                                                         
                                                                                
* EXTRACT DATA FRAME INTO INPUT FRAME AREA                                      
*                                                                               
STDF     L     R3,AINFRM           R3=A(INPUT FRAME AREA)                       
         LA    R2,SRVDATAH         R2=A(FIRST FIELD IN DATA FRAME)              
*                                                                               
ST30     ZIC   RF,0(R2)            RF=LENGTH OF FIELD FOR MOVE                  
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8              MOVE DATA TO INPUT FRAME AREA                
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         LA    R3,1(R3,RF)         BUMP R3 PAST DATA                            
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),0             IF NOT END OF SCREEN THEN LOOP BACK          
         BNE   ST30                                                             
*                                                                               
         L     RF,AINFRM           COMPUTE FRAME SIZE                           
         SR    R3,RF                                                            
         ST    R3,PCIFRMSZ                                                      
         B     STYES               RETURN YES                                   
*                                                                               
STVRS    MVC   PCIACT,=Y(INVVRS)   INVALID VERSION NUMBER SYNTAX                
         B     STNO                                                             
*                                  INVALID ACTION NUMBER SYNTAX                 
STACT    MVC   PCIACT,=Y(INVACT)                                                
         B     STNO                                                             
*                                  INVALID ACTION NUMBER ZERO                   
STACT0   MVC   PCIACT,=Y(INVACT0)                                               
         B     STNO                                                             
*                                  INVALID FRAME NUMBER SYNTAX                  
STFRM    MVC   PCIACT,=Y(INVFRM)                                                
         B     STNO                                                             
*                                  INVALID LAST FRAME INDICATOR                 
STLAST   MVC   PCIACT,=Y(INVLAST)                                               
         B     STNO                                                             
*                                  INVALID DEBUG INDICATOR SYNTAX               
STDEBUG  MVC   PCIACT,=Y(INVDEBUG)                                              
         B     STNO                                                             
*                                                                               
STNO     B     NO                                                               
STYES    B     YES                                                              
         DROP  R3,R7                                                            
                                                                                
* DUMMY ROUTINE IN PLACE OF READ SAVED STORAGE                                  
*                                                                               
READOLD  NTR1                                                                   
ROX      B     XIT                                                              
                                                                                
* START NEW ACTION. IF THE NEW ACTION IS NOT VALID RETURN ERROR                 
*                                                                               
PROCSTRT NTR1                                                                   
         USING ACTTABD,R3                                                       
         GOTO1 VALACT,DMCB,PCACTION                                             
         BNE   PSX                 IF NOT VALID THEN ERROR                      
         L     R3,AACTION          GET ACTION TABLE ENTRY                       
*                                                                               
         BAS   RE,PULLIN           VALIDATE INPUT FRAME ITEMS                   
         BNE   PSX                 IF UNSUCCESFUL CODE IN PCIACT                
*                                                                               
         ICM   RF,15,ACTIN         DO ACTION INPUT ROUTINE                      
         BZ    PS10                IF REQUIRED                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   PSX                                                              
*                                                                               
PS10     BAS   RE,PUTOUT           FORMAT OUTPUT FRAME ITEMS                    
         BNE   PSX                                                              
         ICM   RF,15,ACTOUT        DO ACTION OUTPUT ROUTINE                     
         BZ    PSX                 IF REQUIRED                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
PSX      B     XIT                                                              
                                                                                
* THIS ROUTINE LOOKS UP THE ACTION PASSED IN PARAMTER ONE IN THE ACTION         
* TABLE. IF THE ACTION IS VALID, IT RETURNS A(ACTION TABLE ENTRY)               
* AND RETURNS 'YES'. IF THE ACTION IS NOT VALID, IT RETURNS                     
* 'INVALID ACTION' IN PCIACT AND RETURNS 'NO'.                                  
*                                                                               
VALACT   NTR1                                                                   
         L     R4,0(R1)            R4=A(ACTION NUMBER)                          
         LA    R3,ACTTAB           R3=A(FIRST ACTION IN TABLE)                  
*                                                                               
VAC10    CLC   ACTNUM,=Y(ACABORT)  WHILE NOT END OF ACTION TABLE                
         BE    VAC20                                                            
         CLC   ACTNUM,0(R4)        IF MATCH THEN DONE                           
         BE    VAC30                                                            
         LA    R3,ACITEM           ELSE BUMP TO NEXT ACTION                     
VAC12    CLC   0(2,R3),=Y(0)       WHILE NOT END OF TABLE ENTRY                 
         BE    VAC14                                                            
         LA    R3,L'ACITEM(R3)                                                  
         B     VAC12                                                            
VAC14    LA    R3,L'ACITEM(R3)                                                  
         B     VAC10                                                            
*                                  RETURN 'ACTION NOT FOUND' PCIACT             
VAC20    MVC   PCIACT,=Y(ER00ANF)                                               
         B     VACNO               RETURN 'NO'                                  
*                                                                               
VAC30    ST    R3,AACTION          SAVE A(ACTION TABLE ENTRY)                   
         B     VACYES              RETURN 'YES'                                 
*                                                                               
VACNO    B     NO                                                               
VACYES   B     YES                                                              
                                                                                
* PROCESS INPUT DATA FRAME                                                      
*                                                                               
PULLIN   NTR1                                                                   
PL10     GOTO1 GETIT               USING CURRENT GET ITEM ROUTINE               
         BNE   PLNO                EXIT IF UNSUCCESFUL CODE IN PCIACT           
         CLI   EOFFLAG,C'Y'        SUCCESFUL EXIT AT END OF FRAME               
         BE    PLYES                                                            
         CLI   LENTYPE,0           CHECK TYPE NUMERIC                           
         BNE   PLERRX                                                           
         CLC   ATYPE(2),=Y(0)      AND IN HALF WORD RANGE                       
         BNE   PLERRX                                                           
*                                                                               
         LA    R4,ACITEM           PROCESS FOR EACH ITEM TYPE                   
PL20     CLC   ACITNUM-ACITEM(2,R4),=Y(0)                                       
         BE    PLERRX              ITEM TYPE NOT IN ACTION TABLE                
         TM    ACITIND-ACITEM(R4),ITI                                           
         BZ    PL30                INPUT VALIDATION NOT REQUIRED                
         CLC   ATYPE+2(2),ACITNUM-ACITEM(R4)                                    
         BNE   PL30                NOT THIS ITEM, GET NEXT                      
*                                                                               
         LH    R0,ATYPE+2          GET DATA ITEM TABLE ENTRIES                  
         BAS   RE,SITTAB                                                        
         BZ    PLERRX                                                           
         MVC   0(4,RF),DATALEN     ITEM LENGTH FROM SAVE TABLE                  
         MVC   4(4,RF),ADATA       ADDRESS ITEM IN FRAME                        
*                                                                               
         TM    ITCINDS-ITCVTABD(R1),ICONVUP                                     
         BZ    *+8                                                              
         BAS   RE,CONVUP           CONVERT DATA TO UPPER CASE                   
         TM    ITCINDS-ITCVTABD(R1),ICONESC                                     
         BZ    *+8                                                              
         BAS   RE,CONSPIN          CONVERT ESCAPE SEQUENCES                     
*                                                                               
PL22     ICM   RF,15,ITCIN-ITCVTABD(R1)                                         
         BZ    PL10                IF REQUIRED                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   PLNO                                                             
         B     PL10                                                             
*                                                                               
PL30     LA    R4,L'ACITEM(R4)     PROCESS NEXT ITEM TYPE                       
         B     PL20                                                             
*                                  INVALID ITEM TYPE                            
PLERRX   MVC   PCIACT,=Y(PIETYPE)                                               
PLNO     B     NO                                                               
PLYES    B     YES                                                              
                                                                                
* PROCESS OUTPUT DATA FRAME                                                     
*                                                                               
PUTOUT   NTR1                                                                   
         LA    R4,ACITEM           PROCESS FOR EACH ITEM TYPE                   
PT20     CLC   ACITNUM-ACITEM(2,R4),=Y(0)                                       
         BE    PTYES               UNTIL LAST ENTRY                             
         TM    ACITIND-ACITEM(R4),ITO                                           
         BZ    PT22                IF OUTPUT PROCESS REQUIRED                   
*NOP*    BZ    PTYES               IF OUTPUT PROCESS REQUIRED                   
*                                                                               
         LH    R0,ACITNUM-ACITEM(R4)                                            
         BAS   RE,SITTAB           GET ITEM CONVERSION TABLE ENTRY              
         BZ    PTERRX                                                           
         ICM   RF,15,ITCOUT-ITCVTABD(R1)                                        
         BZ    PT22                                                             
         XC    ATYPE,ATYPE         FORMAT ITEM FOR OUTPUT                       
         MVC   ATYPE+2(2),ACITNUM-ACITEM(R4)                                    
         XC    DATALEN,DATALEN     SET DEFAULT                                  
         A     RF,RELO                                                          
         BASR  RE,RF               RETURN ITEM IN ADATA, LEN DATALEN            
*                                                                               
         CLC   PCACTION,=Y(ACECHO) AVOID NOSPACE FOR ECHO ACTION                
         BE    PT21                                                             
         TM    ITCINDS-ITCVTABD(R1),ICONESC                                     
         BZ    *+8                                                              
         BAS   RE,CONSPOUT         CONVERT SPECIAL CHARACTERS                   
         BAS   RE,NOTERM           REMOVE TERMCHAR CHARACTERS IN DATA           
         BAS   RE,NOSPACE          REMOVE TRAILING SPACES                       
*                                                                               
PT21     L     R6,DATALEN          MOVE ITEM TO OUTPUT FRAME                    
         GOTO1 PUTITEM,DMCB,ATYPE,(R6),ADATA                                    
*                                                                               
PT22     LA    R4,L'ACITEM(R4)     GET NEXT ITEM                                
         B     PT20                                                             
*                                  INVALID ITEM TYPE                            
PTERRX   MVC   PCIACT,=Y(PIEBASER)                                              
         B     PTNO                                                             
*                                                                               
PTNO     B     NO                                                               
PTYES    B     YES                                                              
         DROP  R3                                                               
                                                                                
* BUILD MESSAGE TO PC.  BUILD =PC CONTROL FRAME FROM VALUES IN                  
* CONTROLLER WORK AREA AND BUILD DATA FRAME FROM OUTPUT FRAME AREA              
*                                                                               
BLDTWA   NTR1                                                                   
         L     R7,ATWA             R7=A(TWA)                                    
         USING T121FFD,R7                                                       
         LA    R3,SRVM2PC          R3=A(PCI CONTROL FRAME)                      
         USING COMHDRD,R3                                                       
         EDIT  (2,PCIVRS),(3,COMVRS),FILL=0 FILL PCI CONTROL FRAME              
*                                                                               
*NOP     LH    RF,PCIACT                                                        
*NOP     CHI   RF,9416             316 INVALID PERSONAL ID - FIELD &I           
*NOP     BNE   *+12                                                             
*NOP     LHI   RF,9231             131 INVALID PERSONAL PASSWORD                
*NOP     STH   RF,PCIACT                                                        
*                                                                               
         EDIT  (2,PCIACT),(4,COMACT),FILL=0                                     
         EDIT  (4,PCIFRAME),(7,COMFRM),FILL=0                                   
         MVC   COMLAST,PCILAST                                                  
         MVC   COMDEBUG,PCIDEBUG                                                
         EDIT  (4,PCIFRMSZ),(7,COMFRS),FILL=0                                   
*                                                                               
         OI    SRVM2PCH+6,X'80'    TRANSMIT FIELD                               
         DROP  R3                                                               
*                                                                               
         L     R3,AOUTFRM          R3=A(OUTPUT FRAME AREA)                      
         LA    R2,SRVDATAH         R2=A(FIRST FIELD IN DATA FRAME)              
*                                                                               
BT10     ZIC   RF,0(R2)            RF=LENGTH OF FIELD FOR MOVE                  
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8              MOVE DATA TO FIELD                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         LA    R3,1(R3,RF)         BUMP PAST DATA                               
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),0             IF NOT END OF SCREEN THEN LOOP BACK          
         BNE   BT10                                                             
*                                                                               
BTX      B     XIT                                                              
         DROP  R7                                                               
                                                                                
* DUMMY ROUTINE IN PLACE OF ONE TO WRITE SAVED STORAGE TO TWA                   
*                                                                               
SAVESTR  NTR1                                                                   
SSX      B     XIT                                                              
                                                                                
* ACTION SUBROUTINES CALLED DURRING INPUT PROCESSING PHASE                      
* REFERRED TO FROM ENTRIES IN THE ACTION TABLE                                  
                                                                                
* DISPLAY DATA ITEM. CHECKS CORRECT DATA ITEM TYPE WAS ENTERED                  
*                                                                               
PIDISP   NTR1                                                                   
         LHI   R0,ITTYPE           FOR TYPE TYPE ITEM                           
         BAS   RE,SITTAB           GET TABLE ENTRIES                            
         BZ    PIERRX              ERROR IF NOT PRESENT                         
         ICM   RF,15,0(RF)         OR DATA LENGTH ZERO                          
         BZ    PIERRX                                                           
         B     PIYES                                                            
                                                                                
* CHANGE DATA ITEM. CHECKS CORRECT DATA ITEMS INPUT                             
* CALLS DATA ITEM UPDATE ROUTINE                                                
*                                                                               
PICHGE   NTR1                                                                   
         LHI   R0,ITTYPE           FOR TYPE TYPE ITEM                           
         BAS   RE,SITTAB           GET TABLE ENTRIES                            
         BZ    PIERRX              INVALID IF NULL                              
         ICM   RF,15,0(RF)                                                      
         BZ    PIERRX                                                           
*                                                                               
         LHI   R0,ITDATA           FOR DATA TYPE ITEM                           
         BAS   RE,SITTAB           GET TABLE ENTRIES ELSE NULL                  
         BZ    PIERRX                                                           
         ICM   RE,15,0(RF)                                                      
         BZ    PIERRX                                                           
         ST    RE,DATALEN          SAVE DATA VALUE AND LENGTH                   
         MVC   ADATA,4(RF)                                                      
*                                                                               
         LH    R0,ITEMSAVE         GET SAVED TYPE OF ITEM                       
         BAS   RE,SITTAB           AND GET TABLE ENTRY                          
         BZ    PIERRX                                                           
         ICM   RF,15,ITCUP-ITCVTABD(R1)                                         
         BZ    PIERRX                                                           
         A     RF,RELO             DO ITEM UPDATE ROUTINE                       
         BASR  RE,RF                                                            
         BNE   PINO                RETURN UNSUCCESFUL CODE IN PCIACT            
         B     PIYES               ELSE OK                                      
                                                                                
* PHYSICAL CONNECT. CHECKS CORRECT DATA ITEMS INPUT                             
* CALLS OVERLAYS AND SETS SYSTEM DATA FOR =CT CONNECT CALL                      
* EXIT TO FACPAK MONITOR IF SUCCESFUL CONNECT,                                  
* ELSE SYSTEM DATA AND PC TWA RESTORED AND NORMAL ERROR EXIT                    
*                                                                               
PICTPH   NTR1                                                                   
         L     RE,ATWA             SAVE PC SCREEN TWA IN WORKING STORE          
         L     R0,APCTWASV                                                      
         LA    R1,SRVLAST-T121FFD                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ATWA             LOAD CONNECT BASE SCREEN                     
         LA    R6,64(R6)                                                        
         GOTO1 CALLOV,DMCB,(R6),X'D90110FF'                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 LOADFLD,DMCB,=C'=CT',3,1                                         
*                                                                               
         LHI   R0,ITUSER           CHECK USER TYPE ITEM INPUT                   
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PCP10               ELSE DO NEXT ITEM                            
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PCP10                                                            
         L     R2,4(RF)            MOVE DATA TO USER ID TWA FIELD               
         GOTO1 LOADFLD,DMCB,(R2),(R3),2                                         
*                                                                               
PCP10    LHI   R0,ITSYS            CHECK SYSTEM TYPE ITEM INPUT                 
         BAS   RE,SITTAB           GET DATA TABLE ENTRY                         
         BZ    PCP20                                                            
         ICM   R3,15,0(RF)                                                      
         BZ    PCP20                                                            
         L     R2,4(RF)            MOVE DATA TO SYSTEM FIELD                    
         GOTO1 LOADFLD,DMCB,(R2),(R3),3                                         
*                                                                               
PCP20    LHI   R0,ITPROG           CHECK PROGRAM TYPE ITEM INPUT                
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PCP30                                                            
         ICM   R3,15,0(RF)                                                      
         BZ    PCP30                                                            
         L     R2,4(RF)            MOVE TO TWA FIELD                            
         GOTO1 LOADFLD,DMCB,(R2),(R3),4                                         
*                                                                               
         LR    RF,R2               IF PROGRAM HAS STEREO FLAGS (,S=???)         
         AR    RF,R3                                                            
PCP25    BCTR  RF,0                                                             
         CR    RF,R2                                                            
         BL    PCP30                                                            
         CLC   0(3,RF),=C',S='                                                  
         BNE   PCP25                                                            
         L     RF,AUTL             THEN DONT INHIBIT BROADCAST MSGS             
         USING UTLD,RF                                                          
         NI    TFLAG,X'FF'-TFLAGIRB                                             
         DROP  RF                                                               
*                                                                               
PCP30    LHI   R0,ITPASS           CHECK PASSWORD TYPE ITEM INPUT               
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PCP40                                                            
         ICM   R3,15,0(RF)                                                      
         BZ    PCP40                                                            
         L     R2,4(RF)            MOVE TO TWA FIELD                            
         GOTO1 LOADFLD,DMCB,(R2),(R3),5                                         
*                                                                               
PCP40    LHI   R0,ITCDATA          CHECK CONNECT DATA INPUT                     
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PCP100                                                           
         ICM   R3,15,0(RF)                                                      
         BZ    PCP100                                                           
         L     R2,4(RF)            MOVE TO TWA FIELD                            
         GOTO1 LOADFLD,DMCB,(R2),(R3),6                                         
*                                                                               
PCP100   MVC   DMCB+4(4),=X'D9011000'                                           
         MVC   DMCB(4),CSNXTLOD    IN CASE NOT CORE RESIDENT ??                 
         GOTO1 CALLOV,DMCB,,,0                                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
* SET SYSTEM STATUS TABLE VALUES AS IF FOR ENTRY DIRECT FROM FACPAK             
* MONITOR AND GOTO CONNECT ROUTINE                                              
*                                                                               
         L     R2,AUTL             ADJUST UTL VALUES                            
         USING UTLD,R2                                                          
         L     R3,ATCB             ADJUST TCB VALUES                            
         USING TCBD,R3                                                          
         MVC   TSRSAVE(2),TSVCREQ  SAVE CURRENT UTL PROGRAM STATE               
         MVI   TSVCREQ,X'01'       INDICATE CT SERVICE REQUEST STATE            
         MVI   TSVCREQ+1,$CT                                                    
         MVI   TCBPRG,$CT          INDICATE CT SERVICE REQUEST STATE            
         L     RF,0(R1)            GET CODE ENTRY POINT                         
         L     R1,APARMS           POINT TO ENTRY PARAMETERS                    
         BASR  RE,RF               GOTO CONNECT ROUTINE                         
         OC    TSVCREQ,TSVCREQ     TEST IF CONNECT SUCCESFUL                    
         BZ    XDIRECT             IF SO EXIT DIRECT TO MONITOR                 
         MVC   TSVCREQ(2),TSRSAVE  ELSE RESTORE UTL PROGRAM STATE               
         MVC   TCBPRG(1),TSVCREQ+1 AND RESTORE TCB PROGRAM STATE                
         DROP  R2,R3                                                            
*                                                                               
         BAS   RE,TRERROR          INTERPRET ERROR MSG IN TWA LINE 1            
*                                                                               
         L     R6,ATWA             RESTORE PCI TWA SCREEN                       
         LA    R6,64(R6)           LOAD OVERLAY                                 
         GOTO1 CALLOV,DMCB,(R6),X'D90121FF'                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,ATWA             MOVE SAVED TWA DATA FROM STORAGE             
         L     RE,APCTWASV                                                      
         LA    R1,SRVLAST-T121FFD                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     PINO                RETURN PCIACT ALREADY SET FOR ERROR          
*                                                                               
$CT      EQU   X'10'               CONNECT PROGRAM #                            
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PIERRX   MVC   PCIACT,=Y(PIENOTIN)                                              
PINO     B     NO                                                               
PIYES    B     YES                                                              
                                                                                
* WEB IP CHECK.                                                                 
*                                                                               
PIWIPCHK NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         LHI   R0,ITIPADR          CHECK IP ADDRESS INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PIPCERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PIPCERRX                                                         
         STC   R3,ADIPADRL                                                      
         L     R2,4(RF)            MOVE DATA TO IP ADDRESS FIELD                
         XC    ADIPADR,ADIPADR     (ACTUALLY FILL WITH SPACES ??)               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADIPADR(0),0(R2)                                                 
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCPIP                                                  
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PIPCOK                                                           
*                                                                               
PIPCERRX MVC   PCIACT,=Y(PIENOTIN)       9002-REQUIRED ITEM NOT INPUT           
PIPCNO   B     NO                                                               
PIPCOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
*======================================================================         
* WEB USER AUTHENTICATION                                                       
*======================================================================         
PIWUAUT  NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         LHI   R0,ITUAUID          CHECK USER TYPE ITEM INPUT                   
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWUAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWUAERRX                                                         
         L     R2,4(RF)            MOVE DATA TO USER ID FIELD                   
         MVC   ADUAUID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUID(0),0(R2)                                                 
*                                                                               
PWUA10   LHI   R0,ITUAPID          CHECK PERSON ID TYPE ITEM INPUT              
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWUAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWUAERRX                                                         
         CHI   R3,L'SAPEPID                                                     
         BH    PWUAERIX                                                         
         L     R2,4(RF)            MOVE DATA TO PERSON ID FIELD                 
         MVC   ADUAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPID(0),0(R2)                                                 
*                                                                               
PWUA20   LHI   R0,ITUAPWD          CHECK PASSWORD TYPE ITEM INPUT               
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWUAERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PWUAERRX                                                         
         CHI   R3,L'ADUAPWD        CHECK FOR MAXIMUM PASSWORD (10)              
         BH    PWUAERPX                                                         
         L     R2,4(RF)            MOVE TO PWD FIELD                            
         MVC   ADUAPWD,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPWD(0),0(R2)                                                 
*                                                                               
PWUA30   LHI   R0,ITIPADR          CHECK IP ADDRESS INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWUAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWUAERRX                                                         
         STC   R3,ADIPADRL                                                      
         L     R2,4(RF)            MOVE DATA TO IP ADDRESS FIELD                
         XC    ADIPADR,ADIPADR     (ACTUALLY FILL WITH SPACES ??)               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADIPADR(0),0(R2)                                                 
*                                                                               
PWUA40   MVC   ADPAPID,SPACES                                                   
         LHI   R0,ITPAPID          CHECK PROGRAM ID INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWUA100                                                          
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWUA100             (OPTIONAL)                                   
         L     R2,4(RF)            MOVE DATA TO PROGAM ID FIELD                 
         MVC   ADPAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADPAPID(0),0(R2)                                                 
*                                                                               
PWUA100  XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCPUSER                                                
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PWUAOK                                                           
*                                                                               
PWUAERRX MVC   PCIACT,=Y(PIENOTIN)     9002-REQUIRED ITEM NOT INPUT             
         B     NO                                                               
PWUAERPX MVC   PCIACT,=Y(PIEERMSG+131) 9231-INVALID PERSONAL PASSWORD           
         B     NO                                                               
PWUAERIX MVC   PCIACT,=Y(PIEERMSG+316) 9416-INVALID PERSONAL ID                 
PWUANO   B     NO                                                               
PWUAOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
*======================================================================         
* WEB PROGRAM AUTHORISATION                                                     
*======================================================================         
PIWPAUT  NTR1                                                                   
*                                                                               
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         LHI   R0,ITPAPID          CHECK PROGRAM ID INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPAERRX                                                         
         L     R2,4(RF)            MOVE DATA TO PROGAM ID FIELD                 
         MVC   ADPAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADPAPID(0),0(R2)                                                 
*                                                                               
         LHI   R0,ITUAUIN          CHECK USER ID FOR SPECIAL VALIDATION         
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPA10                                                           
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPA10                                                           
         CHI   R3,L'ADUAUIN+11     CHECK FOR MAXIMUM LENGTH                     
         BH    PWPA10                                                           
         L     R2,4(RF)            START OF DATA                                
         LA    R1,0(R3,R2)         END OF DATA                                  
         AHI   R1,-11              BUMP BACK TO SPECIAL KEYWORD                 
         CLC   =C' V A L I D',0(R1)                                             
         BNE   PWPA10              NOTHING SPECIAL                              
         OI    ACCDINDS,ACCDIUVR   INDICATE USER VALIDATION REQUIRED            
         AHI   R3,-11                                                           
         STCM  R3,15,0(RF)         ADJUST INPUT FIELD LENGTH                    
*                                                                               
PWPA10   LHI   R0,ITUAPIN          CHECK PERSON PIN TYPE ITEM INPUT             
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPA12                                                           
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPA12                                                           
         L     R2,4(RF)            MOVE DATA TO PIN FIELD                       
         MVC   ADUAPIN,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPIN(0),0(R2)                                                 
         B     PWPA20                                                           
*                                                                               
PWPA12   LHI   R0,ITUAPID          CHECK PERSON ID TYPE ITEM INPUT              
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPA14                                                           
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPA14                                                           
         L     R2,4(RF)            MOVE DATA TO PERSON ID FIELD                 
         MVC   ADUAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPID(0),0(R2)                                                 
         B     PWPA20                                                           
*                                                                               
PWPA14   LHI   R0,ITUAPWD          CHECK PASSWORD ITEM INPUT                    
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPAERRX                                                         
         CHI   R3,L'ADUAPWD        CHECK FOR MAXIMUM PASSWORD (10)              
         BH    PWPAERPX                                                         
         L     R2,4(RF)            MOVE DATA TO PASSWORD FIELD                  
         MVC   ADUAPWD,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPWD(0),0(R2)                                                 
*                                                                               
PWPA20   LHI   R0,ITUAUIN          CHECK USERID # ITEM INPUT                    
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWPA22                                                           
         ICM   R3,15,0(RF)                                                      
         BZ    PWPA22                                                           
         L     R2,4(RF)            MOVE TO USERID # FIELDS                      
         MVC   ADUAUIN,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUIN(0),0(R2)                                                 
         B     PWPA30                                                           
*                                                                               
PWPA22   LHI   R0,ITUAUID          CHECK USER TYPE ITEM INPUT                   
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPAERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPAERRX                                                         
         L     R2,4(RF)            MOVE DATA TO USER ID FIELD                   
         MVC   ADUAUID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUID(0),0(R2)                                                 
*                                                                               
PWPA30   LHI   R0,ITUAAGC          CHECK ACCESS GROUP #ITEM INPUT               
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWPA40                                                           
         MVC   ADUAAGNU,SPACES                                                  
         ICM   R3,15,0(RF)                                                      
         BZ    PWPA40                                                           
         L     R2,4(RF)            MOVE TO ACCESS GROUP CODE FIELD              
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAAGNU(0),0(R2)                                                
*                                                                               
PWPA40   LHI   R0,ITUASAID         CHECK SEC AGENCY ID INPUT                    
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWPA100                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PWPA100                                                          
         L     R2,4(RF)            MOVE TO ACCESS GROUP CODE FIELD              
         MVC   ADUASAID,SPACES                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUASAID(0),0(R2)                                                
*                                                                               
PWPA100  XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCPAUTH                                                
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PWPAOK                                                           
*                                                                               
PWPAERRX MVC   PCIACT,=Y(PIENOTIN)     9002-REQUIRED ITEM NOT INPUT             
         B     NO                                                               
PWPAERPX MVC   PCIACT,=Y(PIEERMSG+131) 9231-INVALID PERSONAL PASSWORD           
PWPANO   B     NO                                                               
PWPAOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
*======================================================================         
* WEB PROGRAM ACCESS                                                            
*======================================================================         
PIWPACC  NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         LHI   R0,ITPAPID          CHECK PROGRAM ID INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPCERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPCERRX                                                         
         L     R2,4(RF)            MOVE DATA TO PROGAM ID FIELD                 
         MVC   ADPAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADPAPID(0),0(R2)                                                 
*                                                                               
PWPC10   LHI   R0,ITUAPIN          CHECK PERSON PIN TYPE ITEM INPUT             
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PWPCERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PWPCERRX                                                         
         L     R2,4(RF)            MOVE DATA TO PIN FIELD                       
         MVC   ADUAPIN,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPIN(0),0(R2)                                                 
*                                                                               
PWPC20   LHI   R0,ITUAUIN          CHECK USERID # ITEM INPUT                    
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWPCERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PWPCERRX                                                         
         L     R2,4(RF)            MOVE TO USERID # FIELDS                      
         MVC   ADUAUIN,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUIN(0),0(R2)                                                 
*                                                                               
PWPC30   LHI   R0,ITUASAID         CHECK SEC AGENCY ID INPUT                    
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PWPCERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PWPCERRX                                                         
         L     R2,4(RF)            MOVE TO ACCESS GROUP CODE FIELD              
         MVC   ADUASAID,SPACES                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUASAID(0),0(R2)                                                
*                                                                               
PWPC100  XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCPPACC                                                
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PWPCOK                                                           
*                                                                               
PWPCERRX MVC   PCIACT,=Y(PIENOTIN)       9002-REQUIRED ITEM NOT INPUT           
PWPCNO   B     NO                                                               
PWPCOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
*======================================================================         
* CHANGE PASSWORD                                                               
*======================================================================         
PICPWD   NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         XC    ADUAUID,ADUAUID                                                  
         LHI   R0,ITUAUID          CHECK USER TYPE ITEM INPUT                   
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PICPERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PICPERRX                                                         
         L     R2,4(RF)            MOVE DATA TO USER ID FIELD                   
         MVC   ADUAUID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUID(0),0(R2)                                                 
*                                                                               
PICP10   XC    ADUAPID,ADUAPID                                                  
         LHI   R0,ITUAPID          CHECK PERSON ID TYPE ITEM INPUT              
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PICP20                                                           
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PICP20                                                           
         L     R2,4(RF)            MOVE DATA TO PERSON ID FIELD                 
         MVC   ADUAPID,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPID(0),0(R2)                                                 
*                                                                               
PICP20   XC    ADUAPWD,ADUAPWD                                                  
         LHI   R0,ITUAPWD          CHECK PASSWORD TYPE ITEM INPUT               
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PICPERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PICPERRX                                                         
         CHI   R3,L'ADUAPWD        CHECK FOR MAXIMUM PASSWORD (10)              
         BH    PICPERPX                                                         
         L     R2,4(RF)            MOVE TO PWD FIELD                            
         MVC   ADUAPWD,SPACES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPWD(0),0(R2)                                                 
*                                                                               
PICP30   XC    ADNPWD,ADNPWD                                                    
         LHI   R0,ITNPWD           CHECK NEW PASSWORD TYPE ITEM INPUT           
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PICPERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PICPERRX                                                         
         CHI   R3,L'ADNPWD         CHECK FOR MAXIMUM PASSWORD (10)              
         BH    PICPPTLX                                                         
         L     R2,4(RF)            MOVE TO NEW PWD FIELD                        
         MVC   ADNPWD,SPACES                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADNPWD(0),0(R2)                                                  
*                                                                               
PICP40   XC    ADIPADR,ADIPADR                                                  
         XC    ADIPADRL,ADIPADRL                                                
         LHI   R0,ITIPADR          CHECK IP ADDRESS INPUT                       
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PICP50                                                           
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PICP50                                                           
         STC   R3,ADIPADRL                                                      
         L     R2,4(RF)            MOVE DATA TO IP ADDRESS FIELD                
         XC    ADIPADR,ADIPADR     (ACTUALLY FILL WITH SPACES ??)               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADIPADR(0),0(R2)                                                 
*                                                                               
PICP50   XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCPCP                                                  
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PICPOK                                                           
*                                                                               
PICPERRX MVC   PCIACT,=Y(PIENOTIN)     9002-REQUIRED ITEM NOT INPUT             
         B     NO                                                               
PICPERPX MVC   PCIACT,=Y(PIEERMSG+131) 9231-INVALID PERSONAL PASSWORD           
         B     NO                                                               
PICPPTLX MVC   PCIACT,=Y(PIEERMSG+303) 9403-PASSWORD IS TOO LONG                
PICPNO   B     NO                                                               
PICPOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
*======================================================================         
* SYSTEM SERVICE                                                                
*======================================================================         
PISSRV   NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         XC    ADSSRV,ADSSRV                                                    
         LHI   R0,ITSSRV           CHECK SYSTEM SERVICE ID INPUT                
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PISSERRX                                                         
         ICM   R3,15,0(RF)         CHECK DATA LENGTH NOT ZERO                   
         BZ    PISSERRX                                                         
         L     R2,4(RF)            MOVE DATA TO USER ID FIELD                   
         MVC   ADSSRV,SPACES                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADSSRV(0),0(R2)                                                  
*                                                                               
PISS10   XC    ADUC,ADUC                                                        
         LHI   R0,ITUC             CHECK USER CONTROL                           
         BAS   RE,SITTAB           AND GET VALUE FROM SAVE TABLE                
         BZ    PISSERRX                                                         
         ICM   R3,15,0(RF)                                                      
         BZ    PISSERRX                                                         
         L     R2,4(RF)                                                         
         MVC   ADUC,SPACES                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUC(0),0(R2)                                                    
*                                                                               
PISS20   XC    ADADATA,ADADATA                                                  
         LHI   R0,ITDATA           CHECK DATA INPUT                             
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    PISS40                                                           
         XC    ADADATA,ADADATA                                                  
         STCM  RF,15,ADADATA                                                    
*                                                                               
PISS40   XC    DMCB(24),DMCB                                                    
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
         MVI   ACCPACTN,ACCSSRV                                                 
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PISSOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PISSERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PISSNO   B     NO                                                               
PISSOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
* ACCESS AUTHORIZATION VALIDATION (=PC,32 AND =PC,33 INPUT PROCESSING)          
*                                                                               
PIACSA   NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         ICM   RF,15,=V(OFFLAL)                                                 
         A     RF,RELO                                                          
         STCM  RF,15,ACCDAOFL                                                   
*                                                                               
         MVC   ADPAPID,SPACES      PROGRAM ID (SYSTM/PRGRM HEX VALUE)           
         LHI   R0,ITPAPID                                                       
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADPAPID(0),0(R2)                                                 
*                                                                               
         XC    ADPASEN,ADPASEN     FACPAK SYSTEM SE NUMBER                      
         LHI   R0,ITPASEN                                                       
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADPASEN(0),0(R2)                                                 
*                                                                               
         MVC   ADUAUAID,SPACES     AGENCY ALPHA ID                              
         LHI   R0,ITUAUAID                                                      
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUAID(0),0(R2)                                                
*                                                                               
         MVC   ADUASAID,SPACES     SECURITY AGENCY ALPHA ID                     
         LHI   R0,ITUASAID                                                      
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUASAID(0),0(R2)                                                
*                                                                               
         MVC   ADUAPAID,SPACES     PERSON SECURITY AGENCY ALPHA ID              
         LHI   R0,ITUAPAID                                                      
         BAS   RE,SITTAB                                                        
         BZ    PIAC020             (NOT REQUIRED)                               
         ICM   R3,15,0(RF)                                                      
         BZ    PIAC020                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPAID(0),0(R2)                                                
         B     *+10                                                             
PIAC020  MVC   ADUAPAID,ADUASAID   USE SEC AGY IF NO PER SEC SUPPLIED           
*                                                                               
         MVC   ADUAUIN,SPACES      USER ID NUMBER                               
         LHI   R0,ITUAUIN                                                       
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAUIN(0),0(R2)                                                 
*                                                                               
         MVC   ADUAPIN,SPACES      PERSON ID # (PASSWORD #) PIN                 
         LHI   R0,ITUAPIN                                                       
         BAS   RE,SITTAB                                                        
         BZ    PIACERX                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIACERX                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAPIN(0),0(R2)                                                 
*                                                                               
         MVC   ADUAAGNU,SPACES     ACCESS GROUP NUMBER                          
         LHI   R0,ITUAAGC                                                       
         BAS   RE,SITTAB                                                        
         BZ    PIAC030                                                          
         ICM   R3,15,0(RF)                                                      
         BZ    PIAC030                                                          
         L     R2,4(RF)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ADUAAGNU(0),0(R2)                                                
*                                                                               
PIAC030  CLC   PCACTION,=Y(ACCLAA) CLIENT ACCESS INFORMATION? (PC,33)           
         BNE   PIAC040             NO-SKIP CLIENT DATA                          
         XC    ADADATA,ADADATA     CLIENT DATA (PC,33)                          
         LHI   R0,ITDATA                                                        
         BAS   RE,SITTAB                                                        
         BZ    PIAC040                                                          
         XC    ADADATA,ADADATA                                                  
         STCM  RF,15,ADADATA       SAVE ADDRESS                                 
*                                                                               
PIAC040  XC    DMCB(24),DMCB        CALL DDACCESS TO PROCESS FIELDS             
         LA    R2,DMCB                                                          
         USING ACCPARMD,R2                                                      
*                                                                               
         MVI   ACCPACTN,ACCGAIN    GENERAL ACCESS INFORMATION                   
         CLC   PCACTION,=Y(ACCLAA) CLIENT ACCESS AUTHORIZATION (PC,33)          
         BNE   *+8                 NO                                           
         MVI   ACCPACTN,ACCCLAA    YES                                          
*                                                                               
         STCM  R4,15,ACCPDATA                                                   
         MVC   ACCPSYS,APARMS                                                   
         MVC   ACCPSAVE,APCTWASV                                                
         MVC   ACCPLODA,CSNXTLOD                                                
         LA    R1,SRVLAST-T121FFD                                               
         STCM  R1,7,ACCPSLEN                                                    
         MVI   ACCPSCRN,X'FF'                                                   
         GOTO1 =V(ACCESS),(R2),RR=RELO                                          
         BNE   PACCER                                                           
         B     PIACOK                                                           
*                                                                               
PIACERX  MVC   PCIACT,=Y(PIENOTIN) INPUT ACTION ROUTINE RETURN POINTS           
PIACNO   B     NO                                                               
PIACOK   B     YES                                                              
         DROP  R4                                                               
                                                                                
* ACQUIRE FIXED VLU                                                             
*                                                                               
PIVLUAFX NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(6,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         L     R4,AIO2                                                          
         MVC   0(10,R4),0(RE)                                                   
         B     PAFXOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PAFXERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PAFXNO   B     NO                                                               
PAFXOK   B     YES                                                              
                                                                                
* ACQUIRE FLOATING VLU                                                          
*                                                                               
PIVLUAFL NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(4,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(6,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         L     R4,AIO2                                                          
         MVC   0(10,R4),0(RE)                                                   
         B     PAFLOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PAFLERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PAFLNO   B     NO                                                               
PAFLOK   B     YES                                                              
                                                                                
* MAKE THIS LU HAVE FLOATING LUS                                                
*                                                                               
PIVLUMFL NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(4,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         B     PMFLOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PMFLERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PMFLNO   B     NO                                                               
PMFLOK   B     YES                                                              
                                                                                
* RELEASE THE CURRENT VLU                                                       
*                                                                               
PIVLURLS NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(7,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         B     PRLSOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PRLSERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PRLSNO   B     NO                                                               
PRLSOK   B     YES                                                              
                                                                                
* RELEASE MULTIPLE VLUS                                                         
*                                                                               
PIVLURLM NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(7,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         B     PRLMOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PRLMERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PRLMNO   B     NO                                                               
PRLMOK   B     YES                                                              
                                                                                
* RELEASE VLU RESOURCES                                                         
*                                                                               
PIVLURSC NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(VIRTRM),DMCB,(8,0),0,ASYSFACS,RR=RELO                         
         CLI   0(R1),0                                                          
         BNE   PVLUER                                                           
         B     PRSCOK                                                           
                                                                                
* INPUT ACTION ROUTINE RETURN POINTS                                            
*                                                                               
PRSCERRX MVC   PCIACT,=Y(PIENOTIN)                                              
PRSCNO   B     NO                                                               
PRSCOK   B     YES                                                              
                                                                                
* ACTION SUBROUTINES CALLED DURRING OUTPUT PROCESSING PHASE                     
* REFERRED TO FROM ENTRIES IN THE ACTION TABLE                                  
                                                                                
* OUTPUT ACTION ROUTINE RETURN POINTS                                           
*                                                                               
POERRX   MVC   PCIACT,=Y(PIEBASER)                                              
PONO     B     NO                                                               
POYES    B     YES                                                              
                                                                                
* PROCESS DDACCESS ERROR RETURN CODE                                            
*                                                                               
         USING ACCPARMD,R2                                                      
PACCER   CLI   TRACE,C'Y'                                                       
         BNE   *+8                                                              
         BRAS  RE,WTOERR                                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,ACCPRS                                                      
         CLI   ACCPRC,ACRCERR1                                                  
         BE    PACEER1                                                          
         CLI   ACCPRC,ACRCERR2                                                  
         BE    PACEER2                                                          
         CLI   ACCPRC,ACRCERR3                                                  
         BE    PACEER3                                                          
PACEER1  AHI   RF,PIEACCEB                                                      
         STH   RF,PCIACT                                                        
         B     NO                                                               
PACEER2  AHI   RF,PIEERMSG                                                      
         STH   RF,PCIACT                                                        
         B     NO                                                               
PACEER3  AHI   RF,PIEACCEB                                                      
         STH   RF,PCIACT                                                        
         B     NO                                                               
         DROP  R2                                                               
                                                                                
* PROCESS FAVIRTRM ERROR RETURN CODE                                            
*                                                                               
PVLUER   LHI   RF,PIEACCEB                                                      
         STH   RF,PCIACT                                                        
         B     NO                                                               
                                                                                
* DATA ITEM INPUT VALIDATION SUBROUTINES                                        
*                                                                               
                                                                                
* VALIDATE 'TYPE' TYPE ITEM                                                     
*                                                                               
ITITYPE  NTR1                                                                   
         ICM   R2,15,DATALEN       GET LENGTH DATA                              
         BZ    IIERRX              MUST BE 1 TO 4                               
         C     R2,=F'4'                                                         
         BH    IIERRX                                                           
         GOTO1 TESTNUM,DMCB,ADATA,(R2)                                          
         BNE   IIERRX              EXIT ERROR IF NOT NUMERIC                    
*                                                                               
         L     RE,ADATA            GET DATA ADDRESS                             
         BCTR  R2,0                CONVERT TO BINARY                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         CVB   RF,DUB                                                           
         STH   RF,ITEMSAVE         SAVE VALUE FOR LATER                         
         CLC   ITEMSAVE,=Y(ITPCBASE)                                            
         BNH   IIERRX              ERROR IF TO LOW                              
         CLC   ITEMSAVE,=Y(ITPCMAX)                                             
         BH    IIERRX              ERROR IF TO HIGH                             
         CLC   ITEMSAVE,=Y(ITTYPE)                                              
         BE    IIERRX              CAN NOT BE TYPE TYPE VALUE                   
         B     IIYES                                                            
                                                                                
* INPUT VALIDATION ROUTINE RETURN POINTS                                        
*                                                                               
IIERRX   MVC   PCIACT,=Y(PIEDATA)                                               
IINO     B     NO                                                               
IIYES    B     YES                                                              
                                                                                
* DATA ITEM UPDATE SUBROUTINES                                                  
*                                                                               
                                                                                
* UPDATE SYSTEM COUNTRY CODE                                                    
*                                                                               
ITUCTRY  NTR1                                                                   
         LA    RF,3                CHECK LENGTH DATA > MINIMUM                  
         C     RF,DATALEN                                                       
         BL    IUERRX                                                           
         L     R1,ADATA                                                         
*                                                                               
         L     RE,ASYSFACS         GET COUNTRY TABLE                            
         L     RE,VSSB-SYSFACD(RE)                                              
         L     RE,SSBACTRY-SSBD(RE)                                             
         LA    RE,6(RE)                                                         
         L     RF,AUTL                                                          
         USING CTRYTABD,RE                                                      
*                                                                               
IUCT10   CLI   CTRYCODE,X'FF'      SEARCH FOR COUNTRY NAME                      
         BE    IUERRX              NOT FOUND                                    
         CLC   CTRYSHRN(2),0(R1)                                                
         BE    IUCT20              FOUND NATIVE                                 
         CLC   CTRYSHR(2),0(R1)                                                 
         BE    IUCT20              FOUND ENGLISH                                
IUCT30   LA    RE,CTRYTABL(RE)     BUMP TO NEXT                                 
         CLI   CTRYCODE,1                                                       
*&&US*&& CLI   CTRYCODE,2                                                       
         BE    IUCT30                                                           
         B     IUCT10                                                           
*                                  UPDATE COUNTRY CODE IN UTL                   
IUCT20   MVC   TCTRY-UTLD(1,RF),CTRYCODE                                        
         B     IUYES                                                            
         DROP  RE                                                               
                                                                                
* UPDATE SYSTEM LANGUAGE CODE                                                   
*                                                                               
ITULANG  NTR1                                                                   
         LA    RF,L'LANGSHRN       CHECK LENGTH DATA > MINIMUM                  
         C     RF,DATALEN                                                       
         BL    IUERRX                                                           
         L     R1,ADATA                                                         
*                                                                               
         L     RE,ASYSFACS         GET LANGUAGE TABLE                           
         L     RE,VSSB-SYSFACD(RE)                                              
         L     RE,SSBALANG-SSBD(RE)                                             
         LA    RE,6(RE)                                                         
         L     RF,AUTL                                                          
         USING LANGTABD,RE                                                      
*                                                                               
IULA10   CLI   LANGCODE,X'FF'      SEARCH FOR LANGUAGE NAME                     
         BE    IUERRX              NOT FOUND                                    
         CLC   LANGSHRN,0(R1)                                                   
         BE    IULA20              NATIVE NAME FOUND                            
         CLC   LANGSHR,0(R1)                                                    
         BE    IULA20              ENGLISH NAME FOUND                           
         LA    RE,LANGTABL(RE)     BUMP TO NEXT                                 
         B     IULA10                                                           
*                                  UPDATE LANGUAGE CODE IN UTL                  
IULA20   MVC   TLANG-UTLD(1,RF),LANGCODE                                        
         B     IUYES                                                            
         DROP  RE                                                               
                                                                                
* UPDATE ITEM ROUTINE RETURN POINTS                                             
*                                                                               
IUERRX   MVC   PCIACT,=Y(PIEUPDAT)                                              
IUNO     B     NO                                                               
IUYES    B     YES                                                              
                                                                                
* DATA ITEM OUTPUT FORMATTING SUBROUTINES                                       
* RETURN:  ADATA=ADDRESS(DATA VALUE), DATALEN=LENGTH OF DATA                    
*                                                                               
                                                                                
* OUTPUT VALUE FOR 'TYPE' TYPE ITEM                                             
* VALUE DERIVED INDIRECTLY BY CALLING OUTPUT ROUTINE FOR                        
* THE TYPE OF THE GIVEN VALUE                                                   
*                                                                               
ITOTYPE  NTR1                                                                   
         MVC   ATYPE+2(2),ITEMSAVE GET VALUE SAVED ON INPUT                     
         LH    R0,ITEMSAVE                                                      
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    IONO                EXIT IF NO ENTRY                             
         ICM   RF,15,ITCOUT-ITCVTABD(R1) GET OUTPUT ROUTINE                     
         BZ    IONO                                                             
         XC    DATALEN,DATALEN     SET DEFAULT                                  
         A     RF,RELO                                                          
         BASR  RE,RF               RETURN ITEM IN ADATA,LEN DATALEN             
         BNE   IONO                UNLESS UNSUCCESFUL                           
         B     IOYES                                                            
                                                                                
* OUTPUT VALUE FOR 'DATA' TYPE ITEM                                             
* VALUE DERIVED FROM INPUT SAVE TABLE                                           
*                                                                               
ITODATA  NTR1                                                                   
         LHI   R0,ITDATA                                                        
         BAS   RE,SITTAB           GET ITEM TABLE ENTRIES                       
         BZ    IONO                EXIT IF NO ENTRY                             
         ICM   RE,15,0(RF)                                                      
         BZ    IONO                                                             
         ST    RE,DATALEN          SAVE DATA VALUE AND LENGTH                   
         MVC   ADATA,4(RF)                                                      
         B     IOYES                                                            
                                                                                
* OUTPUT VALUE FOR FACPAK SYSTEM ID TYPE ITEM                                   
*                                                                               
ITOFPID  NTR1                                                                   
         L     RF,ASYSFACS         GET ID FROM SSB                              
         L     RF,VSSB-SYSFACD(RF)                                              
         LA    RF,SSBVTID-SSBD(RF)                                              
         ST    RF,ADATA                                                         
         LA    RF,L'SSBVTID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
                                                                                
* OUTPUT VALUE FOR UTL LUID TYPE ITEM                                           
*                                                                               
ITOLUID  NTR1                                                                   
         GETLA AUTL,SAVELINE,ADDR=ALPHA                                         
         LA    RF,SAVELINE         VALUE RETURNED IN WORKING STORE              
         ST    RF,ADATA                                                         
         LA    RF,8                                                             
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
                                                                                
* OUTPUT VALUE FOR UTL COUNTRY TYPE ITEM                                        
*                                                                               
ITOCTRY  NTR1                                                                   
         L     RF,ASYSFACS         GET COUNTRY TABLE                            
         L     RF,VSSB-SYSFACD(RF)                                              
         L     R5,SSBACTRY-SSBD(RF)                                             
         L     RF,AUTL                                                          
         BAS   RE,SETBXLE                                                       
         USING CTRYTABD,R5                                                      
         CLC   CTRYCODE,TCTRY-UTLD(RF)                                          
         BE    IOCT10              FIND ENTRY                                   
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
*                                  SAVE NAME IN WORKING STORE                   
IOCT10   MVC   SAVELINE(3),CTRYSHR                                              
*                                                                               
         LA    RF,SAVELINE                                                      
         ST    RF,ADATA                                                         
         LA    RF,3                                                             
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* OUTPUT VALUE FOR CONNECT LANGUAGE TYPE ITEM                                   
*                                                                               
ITOLANG  NTR1                                                                   
         L     RF,ASYSFACS         GET LANGUAGE TABLE                           
         L     RF,VSSB-SYSFACD(RF)                                              
         L     R5,SSBALANG-SSBD(RF)                                             
         L     RF,AUTL                                                          
         BAS   RE,SETBXLE                                                       
         USING LANGTABD,R5                                                      
         CLC   LANGCODE,TLANG-UTLD(RF)                                          
         BE    IOLA10              FIND LANGUAGE ENTRY                          
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
*                                                                               
IOLA10   MVC   SAVELINE(L'LANGSHRN),LANGSHRN                                    
         LA    RF,SAVELINE         SAVE VALUE IN WORKING STORE                  
         ST    RF,ADATA                                                         
         LA    RF,L'LANGSHRN                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* OUTPUT VALUE FOR CURRENT CONNECT USER ID TYPE ITEM                            
*                                                                               
ITOUSER  NTR1                                                                   
         L     R4,AUTL             GET USER ID # FROM UTL                       
         USING UTLD,R4                                                          
         OC    TUSER,TUSER                                                      
         BZ    IONO                UNLESS NOT CONNECTED                         
         MVC   USER,TUSER                                                       
         B     IOUS00              EXTRACT USER ID                              
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 1 CONNECT USER ID TYPE ITEM                    
*                                                                               
ITOUSER1 NTR1                                                                   
         LA    R4,SVSESS1          GET USER ID # FROM WORKING STORE             
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,1,(R4)                                              
         OC    SVUSER,SVUSER                                                    
         BZ    IONO                UNLESS NOT CONNECTED                         
         MVC   USER,SVUSER                                                      
         B     IOUS00              EXTRACT USER ID                              
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 2 CONNECT USER ID TYPE ITEM                    
*                                                                               
ITOUSER2 NTR1                                                                   
         LA    R4,SVSESS2          GET USER ID # FROM WORKING STORE             
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,2,(R4)                                              
         OC    SVUSER,SVUSER                                                    
         BZ    IONO                UNLESS NOT CONNECTED                         
         MVC   USER,SVUSER                                                      
         B     IOUS00              EXTRACT USER ID                              
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 3 CONNECT USER ID TYPE ITEM                    
*                                                                               
ITOUSER3 NTR1                                                                   
         LA    R4,SVSESS3          GET USER ID # FROM WORKING STORE             
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,3,(R4)                                              
         OC    SVUSER,SVUSER                                                    
         BZ    IONO                UNLESS NOT CONNECTED                         
         MVC   USER,SVUSER                                                      
         B     IOUS00              EXTRACT USER ID                              
         DROP  R4                                                               
                                                                                
* EXTRACT USER ID GIVEN # IN WORKING STORE AREA 'USER'                          
*                                                                               
IOUS00   LA    R5,KEY              BUILD CONTROL FILE USER ID KEY               
         USING CTIKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,USER        USER ID # SAVED IN WORKING STORE             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     R5,AIO              R4=A(RECORD)                                 
         CLC   KEY(25),0(R5)       IF KEY DOESN'T MATCH RECORD                  
         BE    *+6                                                              
         DC    H'0'                THEN ERROR RECORD NOT FOUND                  
         LA    R5,CTIDATA          R4=A(RECORD DATA)                            
         USING CTDSCD,R5                                                        
*                                                                               
IOUS10   CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),CTDSCELQ      ELSE IF FOUND DESC ELEMENT THEN DONE         
         BE    IOUS20                                                           
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     IOUS10                                                           
*                                                                               
IOUS20   LA    RF,CTDSC            POINT TO VALUE                               
         ST    RF,ADATA                                                         
         LA    RF,10                                                            
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* OUTPUT VALUE FOR CURRENT CONNECT SYSTEM ID TYPE ITEM                          
*                                                                               
ITOSYS   NTR1                                                                   
         L     R4,AUTL                                                          
         USING UTLD,R4                                                          
         CLI   TSYS,0              CHECK SYSTEM CONNECTED IN UTL                
         BE    IONO                                                             
         MVC   SYSTEM,TOVSYS                                                    
         B     IOSY00              EXTRACT SYSTEM ID                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 1 CONNECT SYSTEM ID TYPE ITEM                  
*                                                                               
ITOSYS1  NTR1                                                                   
         LA    R4,SVSESS1                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,1,(R4)                                              
         CLI   SVSYS,0             CHECK SYSTEM CONNECTED                       
         BE    IONO                                                             
         MVC   SYSTEM,SVOVSYS                                                   
         B     IOSY00              EXTRACT SYSTEM ID                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 2 CONNECT SYSTEM ID TYPE ITEM                  
*                                                                               
ITOSYS2  NTR1                                                                   
         LA    R4,SVSESS2                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,2,(R4)                                              
         CLI   SVSYS,0             CHECK SYSTEM CONNECTED                       
         BE    IONO                                                             
         MVC   SYSTEM,SVOVSYS                                                   
         B     IOSY00              EXTRACT SYSTEM ID                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 3 CONNECT SYSTEM ID TYPE ITEM                  
*                                                                               
ITOSYS3  NTR1                                                                   
         LA    R4,SVSESS3                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,3,(R4)                                              
         CLI   SVSYS,0             CHECK SYSTEM CONNECTED                       
         BE    IONO                                                             
         MVC   SYSTEM,SVOVSYS                                                   
         B     IOSY00              EXTRACT SYSTEM ID                            
         DROP  R4                                                               
                                                                                
* GET CONNECT SYSTEM ID USING SYS OV # IN WORKING STORE AREA 'SYSTEM'           
*                                                                               
IOSY00   GOTO1 GETFACT,DMCB,0                                                   
         L     R5,0(R1)                                                         
         L     R5,FASYSLST-FACTSD(R5)                                           
         BAS   RE,SETBXLE                                                       
         USING SYSLSTD,R5          R5=A(SYSTEM LIST)                            
         CLC   SYSTEM,SYSLNUM                                                   
         BE    IOSY10                                                           
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
*                                                                               
IOSY10   LA    RF,SYSLNAME         POINT TO VALUE                               
         ST    RF,ADATA                                                         
         LA    RF,L'SYSLNAME                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* OUTPUT VALUE FOR CURRENT CONNECT PROGRAM ID TYPE ITEM                         
*                                                                               
ITOPROG  NTR1                                                                   
         L     R4,AUTL                                                          
         USING UTLD,R4                                                          
         CLI   TSYS,0              CHECK PROGRAM CONNECTED IN UTL               
         BE    IONO                                                             
         MVC   SYSTEM,TSYS                                                      
         MVC   PROGRAM,TPRG                                                     
         B     IOPR00              EXTRACT PROGRAM ID                           
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 1 CONNECT PROGRAM ID TYPE ITEM                 
*                                                                               
ITOPROG1 NTR1                                                                   
         LA    R4,SVSESS1                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,1,(R4)                                              
         CLI   SVSYS,0             CHECK PROGRAM CONNECTED                      
         BE    IONO                                                             
         MVC   SYSTEM,SVSYS                                                     
         MVC   PROGRAM,SVPROG                                                   
         B     IOPR00              EXTRACT PROGRAM ID                           
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 2 CONNECT PROGRAM ID TYPE ITEM                 
*                                                                               
ITOPROG2 NTR1                                                                   
         LA    R4,SVSESS2                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,2,(R4)                                              
         CLI   SVSYS,0             CHECK PROGRAM CONNECTED                      
         BE    IONO                                                             
         MVC   SYSTEM,SVSYS                                                     
         MVC   PROGRAM,SVPROG                                                   
         B     IOPR00              EXTRACT PROGRAM ID                           
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 3 CONNECT PROGRAM ID TYPE ITEM                 
*                                                                               
ITOPROG3 NTR1                                                                   
         LA    R4,SVSESS3                                                       
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,3,(R4)                                              
         CLI   SVSYS,0             CHECK PROGRAM CONNECTED                      
         BE    IONO                                                             
         MVC   SYSTEM,SVSYS                                                     
         MVC   PROGRAM,SVPROG                                                   
         B     IOPR00              EXTRACT PROGRAM ID                           
         DROP  R4                                                               
                                                                                
* GET CONNECT PROGRAM ID USING VALUES IN 'SYSTEM' AND 'PROGRAM'                 
*                                                                               
IOPR00   L     R5,ASYSFACS         FIND ENTRY IN SELIST                         
         L     R5,VSELIST-SYSFACD(R5)                                           
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
         CLC   SYSTEM,SESYS                                                     
         BE    IOPR10                                                           
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
*                                                                               
IOPR10   L     R5,SEPGMS           FIND ENTRY IN PROGRAM LIST                   
         BAS   RE,SETBXLE                                                       
         USING PGMLSTD,R5                                                       
         CLC   PGMNUM,PROGRAM                                                   
         BE    IOPR20                                                           
         BXLE  R5,R6,*-10                                                       
         MVC   SAVELINE(8),=C'PGM=    '                                         
         GOTO1 HEXOUT,DMCB,PROGRAM,SAVELINE+4,1,=C'TOG'                         
         LA    RF,SAVELINE         IF NOT FOUND SAVE HEX CODE                   
         ST    RF,ADATA            IN WORKING STORE                             
         LA    RF,8                                                             
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
*                                                                               
IOPR20   LA    RF,PGMNAME          IF FOUND POINT TO VALUE                      
         ST    RF,ADATA                                                         
         LA    RF,L'PGMNAME                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
                                                                                
* OUTPUT VALUE FOR CURRENT CONNECT PASSWORD TYPE ITEM                           
*                                                                               
ITOPASS  NTR1                                                                   
         L     R4,AUTL             GET PASSWORD # FROM UTL                      
         USING UTLD,R4                                                          
         OC    TPASSWD,TPASSWD                                                  
         BZ    IONO                UNLESS NO ENTRY                              
         MVC   PASSWD,TPASSWD                                                   
         MVC   AGYID,TAGYSEC       SECURITY AGENCY ID                           
         TM    TFLAG,TFLAGSEC                                                   
         BNZ   IOPS00              EXTRACT SECRET AUTHORITY WORD                
         B     IOPA00              EXTRACT TERMINAL PASSWORD                    
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 1 CONNECT PASSWORD TYPE ITEM                   
*                                                                               
ITOPASS1 NTR1                                                                   
         LA    R4,SVSESS1          GET PASSWORD # FROM WORKING STORE            
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,1,(R4)                                              
         OC    SVPASS,SVPASS                                                    
         BZ    IONO                UNLESS NO ENTRY                              
         MVC   PASSWD,SVPASS                                                    
         MVC   AGYID,SVAGY         SECURITY AGENCY ID                           
         TM    SVFLAG,TFLAGSEC                                                  
         BNZ   IOPS00              EXTRACT SECRET AUTHORITY WORD                
         B     IOPA00              EXTRACT PASSWORD                             
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 2 CONNECT PASSWORD TYPE ITEM                   
*                                                                               
ITOPASS2 NTR1                                                                   
         LA    R4,SVSESS2          GET PASSWORD # FROM WORKING STORE            
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,2,(R4)                                              
         OC    SVPASS,SVPASS                                                    
         BZ    IONO                UNLESS NO ENTRY                              
         MVC   PASSWD,SVPASS                                                    
         MVC   AGYID,SVAGY         SECURITY AGENCY ID                           
         TM    SVFLAG,TFLAGSEC                                                  
         BNZ   IOPS00              EXTRACT SECRET AUTHORITY WORD                
         B     IOPA00              EXTRACT PASSWORD                             
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SAVED SESSION 3 CONNECT PASSWORD ITEM                        
*                                                                               
ITOPASS3 NTR1                                                                   
         LA    R4,SVSESS3          GET PASSWORD # FROM WORKING STORE            
         USING SVSESSD,R4                                                       
         GOTO1 GETSESS,DMCB,3,(R4)                                              
         OC    SVPASS,SVPASS                                                    
         BZ    IONO                UNLESS NO ENTRY                              
         MVC   PASSWD,SVPASS                                                    
         MVC   AGYID,SVAGY         SECURITY AGENCY ID                           
         TM    SVFLAG,TFLAGSEC                                                  
         BNZ   IOPS00              EXTRACT SECRET AUTHORITY WORD                
         B     IOPA00              EXTRACT PASSWORD                             
         DROP  R4                                                               
                                                                                
* EXTRACT PASSWORD GIVEN # IN WORKING STORE AREA 'PASSWD'                       
* FROM CONTROL FILE TERMINAL RECORD KEY TYPE 'T' RECORDS                        
*                                                                               
IOPA00   LA    R5,KEY              BUILD CONTROL FILE TERMINAL REC KEY          
         USING CTTKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKPASS+8(2),PASSWD   PASSWORD# IS PASSIVE RECORD#              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     R5,AIO              R4=A(RECORD)                                 
         CLC   KEY(25),0(R5)       IF KEY DOESN'T MATCH RECORD                  
         BE    *+6                                                              
         DC    H'0'                THEN ERROR RECORD NOT FOUND                  
         LA    R5,CTTDATA          R4=A(RECORD DATA)                            
         USING CTPASD,R5                                                        
*                                                                               
IOPA10   CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),CTPASELQ      ELSE IF FOUND PASSIVE EL THEN DONE           
         BE    IOPA20                                                           
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     IOPA10                                                           
*                                                                               
IOPA20   LA    RF,CTPASDTA+L'CTTKTID  POINT TO PASSWORD IN ELEMENT              
         ST    RF,ADATA                                                         
         LA    RF,L'CTTKPASS                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* EXTRACT SECRET AUTHORITY WORD GIVEN # IN WORKING STORE AREA 'PASSWD'          
* FROM CONTROL FILE AUTHORITY RECORD KEY TYPE '0' RECORDS                       
*                                                                               
IOPS00   LA    R5,KEY              BUILD CONTROL FILE AUTHORITY REC KEY         
         USING CT0KEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,AGYID       MOVE SECURITY AGENCY ID TO KEY               
         MVC   CT0KNUM,PASSWD      MOVE PERSON PASSWORD # TO KEY                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     R5,AIO              R4=A(RECORD)                                 
         CLC   KEY(25),0(R5)       IF KEY DOESN'T MATCH RECORD                  
         BE    *+6                                                              
         DC    H'0'                THEN ERROR RECORD NOT FOUND                  
         LA    R5,CT0DATA          R4=A(RECORD DATA)                            
         USING CTPASD,R5                                                        
*                                                                               
IOPS10   CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),CTPASELQ      ELSE IF FOUND PASSIVE ELEMENT                
         BNE   *+12                                                             
         CLI   CTPASLEN,2+L'CT0KCODE  AND LENGTH OF SECRET WORD                 
         BE    IOPS20              THEN DONE                                    
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     IOPS10                                                           
*                                                                               
IOPS20   LA    RF,CTPASDTA         POINT TO SECRET CODE IN ELEMENT              
         ST    RF,ADATA                                                         
         LA    RF,L'CT0KCODE       SECRET CODE LENGTH                           
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R5                                                               
                                                                                
* OUTPUT VALUE FOR IP CHECK FLAG                                                
*                                                                               
ITOIPFLG NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPFLAG                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPFLAG                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP ADDRESS                                                   
*                                                                               
ITOIPADR NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPADR                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPADR                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP AGENCY ID                                                 
*                                                                               
ITOIPAID NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPAID                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPAID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP AGENCY PERSON ID FLAG                                     
*                                                                               
ITOIPPFL NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPPFLG                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPPFLG                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP USER ID                                                   
*                                                                               
ITOIPUID NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPUID                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPUID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR IP PU ID                                                     
*                                                                               
ITOIPPUI NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPPUID                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPPUID                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR IP RESOURCE NAME                                             
*                                                                               
ITOIPRNA NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPRNAM                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPRNAM                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP AGENCY NAME                                               
*                                                                               
ITOIPANM NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPANAM                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPANAM                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP DESTINATION NAME                                          
*                                                                               
ITOIPDNM NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPDNAM                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPDNAM                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP ADDRESS LINE 1                                            
*                                                                               
ITOIPAD1 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPADL1                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPADL1                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP ADDRESS LINE 2                                            
*                                                                               
ITOIPAD2 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPADL2                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPADL2                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR IP ADDRESS LINE 3                                            
*                                                                               
ITOIPAD3 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADIPADL3                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADIPADL3                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER AUTHENTICATION FLAG                                     
*                                                                               
ITOUAFLG NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAFLAG                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAFLAG                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR AUTHENTICATION USER ID                                       
*                                                                               
ITOUAUID NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAUID                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAUID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON ID                                                    
*                                                                               
ITOUAPID NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAPID                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAPID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PASSWORD                                                     
*                                                                               
ITOUAPWD NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAPWD                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAPWD                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON PASSWORD NUMBER                                       
*                                                                               
ITOUAPIN NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAPIN                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAPIN                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID NUMBER                                               
*                                                                               
ITOUAUIN NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAUIN                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAUIN                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON OFFICE CODE                                           
*                                                                               
ITOUAOFF NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAOFF                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAOFF                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON DEPARTMENT CODE                                       
*                                                                               
ITOUADEP NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUADEP                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUADEP                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER COUNTRY                                                 
*                                                                               
ITOUACTR NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUACTRY                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUACTRY                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON HIRE DATE                                             
*                                                                               
ITOUADHI NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUADHI                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUADHI                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON TERMINATION DATE                                      
*                                                                               
ITOUADTE NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUADTE                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUADTE                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON ACCESS GROUP CODE                                     
*                                                                               
ITOUAAGC NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAAGNU                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAAGNU                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON FIRST NAME                                            
*                                                                               
ITOUAFNA NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAFNA                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAFNA                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON MIDDLE NAME                                           
*                                                                               
ITOUAMNA NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAMNA                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAMNA                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PERSON LAST NAME                                             
*                                                                               
ITOUALNA NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUALNA                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADUALNA                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID AGENCY NAME                                          
*                                                                               
ITOUAUAN NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAUANM                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAUANM                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID AGENCY ID                                            
*                                                                               
ITOUAUAI NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAUAID                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAUAID                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SECURITY AGENCY ID                                           
*                                                                               
ITOUASAI NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUASAID                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUASAID                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR SECURITY AGENCY ID FOR PERSON                                
*                                                                               
ITOUAPAI NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAPAID                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAPAID                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID DESTINATION NAME                                     
*                                                                               
ITOUADNM NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUADNAM                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUADNAM                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID ADDRESS LINE 1                                       
*                                                                               
ITOUAAD1 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAADL1                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAADL1                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID ADDRESS LINE 2                                       
*                                                                               
ITOUAAD2 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAADL2                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAADL2                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER ID ADDRESS LINE 3                                       
*                                                                               
ITOUAAD3 NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAADL3                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAADL3                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR USER AUTHENTICATION PROGAM ACCESS LIST                       
*                                                                               
ITOUAPAC NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAPACL                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAPACL                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR WEB CONTROLS                                                 
*                                                                               
ITOUWCTL NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAWCTL                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAWCTL                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR ACCESS CODE                                                  
*                                                                               
ITOUACOD NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUAACOD                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADUAACOD                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PROGRAM ID                                                   
*                                                                               
ITOPAPID NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAPID                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAPID                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR PROGAM AUTHORISATION FLAG                                    
*                                                                               
ITOPAFLG NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAFLAG                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAFLAG                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR PROGAM AUTHORISATION CODE                                    
*                                                                               
ITOPAAUT NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAAUTH                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAAUTH                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR PROGAM FACPAK ADV VTAM ID                                    
*                                                                               
ITOPAADV NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAADV                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAADV                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR PROGAM FACPAK SYSTEM SE NUMBER                               
*                                                                               
ITOPASEN NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPASEN                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADPASEN                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
* OUTPUT VALUE FOR PROGAM FACPAK LIMIT CODE ACCESS LIST                         
*                                                                               
ITOPALAC NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
*&&US*&& LA    RF,ADPALACC                                                      
*&&UK*&& LA    RF,SPACES           TEMPORARILY RETURN BLANKS                    
         ST    RF,ADATA                                                         
*&&US*&& LA    RF,L'ADPALACC                                                    
*&&UK*&& LA    RF,1                                                             
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PROGAM AUTHORISATION ACTION ACCESS LIST                      
*                                                                               
ITOPAACL NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAACL                                                       
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAACL                                                     
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR PROGAM AUTHORISATION FIELD CONTROLS                          
*                                                                               
ITOPAFCO NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAFCON                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAFCON                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
                                                                                
* OUTPUT VALUE FOR PROGAM AUTHORISATION OPTION CONTROLS                         
*                                                                               
ITOPAOCO NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADPAOCON                                                      
         ST    RF,ADATA                                                         
         LA    RF,L'ADPAOCON                                                    
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR USER CONTROL DATA                                            
*                                                                               
ITOUC    NTR1                                                                   
         L     R4,AIO2                                                          
         USING ACCDATAD,R4                                                      
         LA    RF,ADUC                                                          
         ST    RF,ADATA                                                         
         LA    RF,L'ADUC                                                        
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
         DROP  R4                                                               
                                                                                
* OUTPUT VALUE FOR VLU TOKEN                                                    
*                                                                               
ITOVLUT  NTR1                                                                   
         L     R4,AIO2                                                          
         ST    R4,ADATA                                                         
         LA    RF,10                                                            
         ST    RF,DATALEN                                                       
         B     IOYES                                                            
                                                                                
* OUTPUT ITEM ROUTINE RETURN POINTS                                             
*                                                                               
IOERRX   MVC   PCIACT,=Y(PIEBASER)                                              
IONO     B     NO                                                               
IOYES    B     YES                                                              
                                                                                
* SUBROUTINE TO GET SAVED SESSION DATA INTO WORKING STORAGE                     
* PARAM 1: SAVED SESSION NUMBER                                                 
* PARAM 2: ADDRESS OF WORKING STORAGE SAESSION DATA SAVE AREA                   
*                                                                               
GETSESS  NTR1                                                                   
         LM    R3,R4,0(R1)         R3=SAVED SESS #, R4=A(WORK AREA)             
         L     RF,AUTL             CHECK CURRENT SAVED SESSIONS                 
         USING UTLD,RF                                                          
         CLI   TSYS,0              MUST BE CONNECTED TO SESSION                 
         BE    GSX                                                              
         TM    TFLAG,TFLAGSVS                                                   
         BZ    GSX                 SAVED SESSION NOT AVAILABLE                  
         DROP  RF                                                               
*                                                                               
         USING SVSESSD,R4                                                       
         CLI   SVIND,0             TEST FIRST TIME                              
         BNE   GSX                                                              
         MVI   SVIND,1                                                          
*                                                                               
         LA    RF,SESSTWAP         GET TWA PAGE # FOR SESSION                   
*                                                                               
GSL1     CLI   0(RF),0             TEST END OF TABLE                            
         BE    GSX                                                              
         BCT   R3,GSL1A                                                         
         B     GSL1X                                                            
*                                                                               
GSL1A    LA    RF,1(RF)                                                         
         B     GSL1                                                             
*                                                                               
GSL1X    SR    R3,R3                                                            
         ICM   R3,8,0(RF)                                                       
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R3),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ATIA                                                          
         AH    R3,CHKDSP                                                        
         USING CHKPTD,R3                                                        
         MVC   SVFLAG,CHUTLSV1+(TFLAG-TSVDATA1)                                 
         MVC   SVUSER,CHUTLSV+(TUSER-TSVDATA)                                   
         MVC   SVSYS,CHUTLSV+(TSYS-TSVDATA)                                     
         MVC   SVOVSYS,CHUTLSV+(TOVSYS-TSVDATA)                                 
         MVC   SVPROG,CHUTLSV+(TPRG-TSVDATA)                                    
         MVC   SVPASS,CHUTLSV+(TPASSWD-TSVDATA)                                 
         MVC   SVAGY,CHUTLSV2+(TAGYSEC-TSVDATA2)                                
*                                                                               
GSX      B     XIT                                                              
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
SESSTWAP DS    0H                  SESSION SAVE TWA PAGE TABLE                  
         DC    X'0600'                                                          
SESSTWAX DS    0H                                                               
                                                                                
* SUBROUTINE TO INTERPRET ERROR MESSAGE IN TWA FIELD ONE                        
* SET VALUE OF PCIACT ON RETURN                                                 
*                                                                               
TRERROR  NTR1                                                                   
         L     R7,ATWA                                                          
         USING T121FFD,R7                                                       
*                                                                               
         LA    R3,SRVM2PC                                                       
         CLC   0(2,R3),=C'ED'      TEST FOR ERROR CODE IN FIELD                 
         BNE   TRERUND1            UNDEFINED ERROR CODE                         
*                                                                               
         LA    R2,4                READ 4 DIGIT CODE                            
         LA    R3,3(R3)                                                         
         GOTO1 TESTNUM,DMCB,(R3),(R2)                                           
         BNE   TRERUND1                                                         
*                                                                               
         BCTR  R2,0                CONVERT VALUE TO BINARY                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R2,DUB                                                           
*                                                                               
         LA    R3,TRERTAB          TRANSLATE VALUE FROM TABLE                   
TRERL1   CH    R2,0(R3)                                                         
         BE    TRERLX              FOUND                                        
         CLC   0(2,R3),=Y(0)                                                    
         BE    TRERUND2            NOT FOUND                                    
         LA    R3,L'TRERTAB(R3)                                                 
         B     TRERL1                                                           
*                                                                               
TRERLX   MVC   PCIACT,2(R3)        EXTRACT TRANSLATION                          
         B     TRERX                                                            
*                                                                               
TRERUND1 SR    R2,R2               SET DEFAULT VALUE                            
         B     TRERUND2                                                         
*                                                                               
TRERUND2 AHI   R2,PIEERMSG         SET EXIT CODE                                
         STH   R2,PCIACT                                                        
         B     TRERX                                                            
*                                                                               
TRERX    B     XIT                                                              
*                                                                               
         DROP  R7                                                               
*                                                                               
TRERTAB  DS    0F                  ERROR CODE TRANSLATION TABLE                 
         DC    YL2(0031,PIEERMSG+0031)                                          
         DC    YL2(0032,PIEERMSG+0032)                                          
         DC    YL2(0033,PIEERMSG+0033)                                          
         DC    YL2(0)                                                           
                                                                                
* SUBROUTINE TO FIND DATA ITEM ENTRY IN CONVERT AND SAVE TABLES                 
* AT AITCVTAB AND AITSVTAB                                                      
* ON ENTRY R0=ITEM TYPE,RE=A(RETURN)                                            
* ON EXIT  R1=A(ITCVTAB ENTRY),RF=A(ITSVTAB ENTRY)                              
*                                                                               
SITTAB   L     RF,AITSVTAB         ITEM TABLE ADDRESS                           
         L     R1,AITCVTAB         ITEM CONVERT TABLE ADDRESS                   
         USING ITCVTABD,R1                                                      
*                                                                               
STL1     CH    R0,ITCTYPE          SEARCH CONVERT TABLE FOR ENTRY               
         BE    STOKX               FOUND OK                                     
         CLC   ITCTYPE,=Y(0)                                                    
         BE    STNOX               NOT FOUND                                    
         LA    RF,8(RF)            BUMP POINTER TO SAVE TABLE                   
         LA    R1,ITCVTLEN(R1)     BUMP TO NEXT CONVERT ENTRY                   
         B     STL1                                                             
*                                                                               
STNOX    SR    RF,RF               FLAG NOT FOUND                               
*                                                                               
STOKX    LTR   RF,RF               FLAG FOUND                                   
         BR    RE                  AND EXIT                                     
         DROP  R1                                                               
                                                                                
* SUBROUTINE TO REMOVE TRAILING SPACES FROM DATA ITEM                           
* ADDRESS IN ADATA,LENGTH IN DATALEN                                            
*                                                                               
NOSPACE  L     RF,ADATA            POINT TO DATA STRING                         
         ICM   R1,15,DATALEN                                                    
         BZ    NOSPX                                                            
         BCTR  RF,0                                                             
         AR    RF,R1               POINT TO LAST CHARACTER IN STRING            
*                                                                               
NOSPL1   CLI   0(RF),C' '          FIND FIRST NON SPACE                         
         BE    *+12                                                             
         CLI   0(RF),0             OR NULL                                      
         BNE   NOSPX                                                            
         C     RF,ADATA                                                         
         BL    NOSPX               UPTO START OF STRING                         
         BCTR  RF,0                                                             
         BCTR  R1,0                DECREMENT DATA LENGTH                        
         B     NOSPL1              BUMP BACK TO NEXT POSITION                   
*                                                                               
NOSPX    ST    R1,DATALEN                                                       
         BR    RE                                                               
                                                                                
* SUBROUTINE TO REPLACE ANY TERMCHARS IN DATA WITH BLANKS                       
*                                                                               
NOTERM   L     RF,ADATA            POINT TO DATA STRING                         
         ICM   R1,15,DATALEN                                                    
         BZ    NOTMX                                                            
         BCTR  RF,0                                                             
         AR    RF,R1               POINT TO LAST CHARACTER IN STRING            
*                                                                               
NOTML1   CLI   0(RF),TERMCHAR      BLANK OUT TERMINATOR CHARACTER               
         BNE   NOTML2                                                           
         MVI   0(RF),C' '                                                       
NOTML2   BCTR  RF,0                                                             
         BCT   R1,NOTML1                                                        
*                                                                               
NOTMX    BR    RE                                                               
                                                                                
SETBXLE  LH    R6,0(R5)            SET BXLE SUBROUTINE                          
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
                                                                                
* SUBROUTINE TO CONVERT DATA ITEM TO UPPER CASE                                 
* ADDRESS IN ADATA,LENGTH IN DATALEN                                            
*                                                                               
CONVUP   L     RF,ADATA            POINT TO DATA STRING                         
         L     R0,DATALEN                                                       
         AR    RF,R0               POINT TO END+1 OF STRING                     
*                                                                               
COUP10   C     RF,ADATA                                                         
         BNH   COUPX                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),X'81'         LITTLE A                                     
         BL    COUP10                                                           
         CLI   0(RF),X'A9'         LITTLE Z                                     
         BH    COUP10                                                           
         OI    0(RF),X'40'         CONVERT TO UPPER CASE                        
         B     COUP10                                                           
*                                                                               
COUPX    BR    RE                                                               
                                                                                
* CONVERT INPUT DATA ITEM SPECIAL CHARACTERS TO PROPER ESCAPE SEQUENCE          
* ADDRESS IN ADATA, LENGTH IN DATALEN                                           
*                                                                               
CONSPIN  NTR1                                                                   
         L     R1,ADATA            R1=A(DATA STRING)                            
         LR    R2,R1                                                            
         L     R6,DATALEN          R6=LENGTH OF DATA                            
         AR    R2,R6               R2=A(END+1 OF STRING)                        
*                                                                               
COSI010  CR    R1,R2               END OF DATA STRING?                          
         BNL   COSIX               YES                                          
         CLI   0(R1),ESCSEQC       NO, CHECK FOR ESCAPE CHARACTER               
         BE    *+12                                                             
COSI020  LA    R1,1(,R1)           MOVE ON                                      
         B     COSI010                                                          
*                                                                               
         LA    R3,ESCTAB           R3=A(ESCAPE SEQUENCE TABLE)                  
COSI030  CLI   0(R3),X'FF'         END OF ESCAPE SEQ TABLE?                     
         BE    COSI020             YES,THEN THIS CHARACTER IS GOOD              
*                                                                               
         CLC   1(2,R3),1(R1)       DATA CHARS AN ESCAPE SEQUENCE?               
         BE    *+12                YES                                          
         LA    R3,L'ESCTAB(R3)     NO,TRY NEXT SEQUENCE                         
         B     COSI030                                                          
                                                                                
         MVC   0(1,R1),0(R3)       REPLACE ESCAPE SEQUENCE WITH CHAR            
                                                                                
         LR    R4,R1                                                            
COSI040  CR    R4,R2               MOVE DATA OVER 2 BYTES                       
         BNL   COSI050                                                          
         MVC   1(1,R4),3(R4)                                                    
         LA    R4,1(,R4)                                                        
         B     COSI040                                                          
                                                                                
COSI050  SHI   R2,2                                                             
         XC    0(2,R2),0(R2)                                                    
         SHI   R6,2                                                             
         ST    R6,DATALEN                                                       
         B     COSI020                                                          
*                                                                               
COSIX    J     XIT                                                              
                                                                                
* CONVERT OUTPUT DATA ITEM SPECIAL CHARACTERS TO PROPER ESCAPE SEQUENCE         
* ADDRESS IN ADATA,LENGTH IN DATALEN                                            
*                                                                               
CONSPOUT NTR1                                                                   
         L     R1,ADATA            R1=A(DATA STRING)                            
         L     R6,DATALEN                                                       
         LA    R2,0(R6,R1)         R2=A(END+1 OF STRING)                        
*                                                                               
COSO010  CR    R1,R2               END OF DATA STRING?                          
         BNL   COSOX               YES                                          
*                                                                               
         LA    R3,ESCTAB           R3=A(ESCAPE SEQU CHARACTER TABLE)            
COSO020  CLI   0(R3),X'FF'         END OF ESCAPE CHARACTERS TABLE?              
         BNE   *+12                NO                                           
         LA    R1,1(,R1)           YES,NEXT DATA STRING CHARACTER               
         B     COSO010             CHECK REACHED END OF DATA                    
         CLC   0(1,R1),0(R3)       SPECIAL CHARACTER NEEDS ESCAPE?              
         BE    *+12                YES                                          
         LA    R3,L'ESCTAB(R3)     NO,BUMP TO NEXT ESCAPE CHARACTER             
         B     COSO020             CHECK REACHED END OF ESCAPE TABLE            
*                                                                               
         LR    R4,R2               R2=A(END+1 OF STRING)                        
         SHI   R4,1                R4=A(END OF DATA STRING)                     
         LR    R5,R4                                                            
         SHI   R5,2                R5=A(END OF DATA STRING-2)                   
COSO030  CR    R1,R5               0(R1) IS CHAR NEED TO REMOVE                 
         BNL   COSO040             REACHED THAT CHARACTER?, YES                 
         MVC   0(1,R4),0(R5)       NO,MOVE DATA OVER 2 BYTES                    
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         B     COSO030             CHECK NEXT CHARACTER TO MOVE                 
*                                                                               
COSO040  MVI   0(R1),ESCSEQC       REPLACE BAD CHAR WITH ESCAPE CHAR            
         MVC   1(2,R1),1(R3)       AND THEN SPECIFIC ESCAPE SEQUENCE            
         AHI   R2,2                ADD 2 TO END OF DATA                         
         LA    R1,L'ESCTAB(,R1)    BUMP PAST ESCAPE SEQUENCE                    
*                                                                               
         AHI   R6,2                ADD 2 TO DATALEN                             
         ST    R6,DATALEN                                                       
         B     COSO020             AND CONTINUE                                 
*                                                                               
COSOX    J     XIT                                                              
*                                                                               
ESCSEQC  EQU   C'?'                ESCAPE SEQUENCE CHARACTER                    
ESCTAB   DS    0CL3                ESCAPE SEQUENCE TABLE                        
*NOP*    DC    C'^',C'0A'                                                       
         DC    C'+',C'0B'                                                       
*NOP*    DC    C'?',C'0C'                                                       
         DC    X'FF'                                                            
                                                                                
* OUTPUT CALL INFORMATION                                                       
*                                                                               
WTOERR   NTR1                                                                   
         L     R3,ATWA                                                          
         USING T121FFD,R3                                                       
         MVC   LINE1T(6),SRVSRV                                                 
         MVC   LINE2T,SRVDATA                                                   
         MVC   LINE3T,SRVDATA+26                                                
*                                                                               
         USING ACCPARMD,R2                                                      
         XR    R1,R1                                                            
         IC    R1,ACCPRC                                                        
         CVD   R1,DUB                                                           
         UNPK  WORK(16),DUB                                                     
         OI    WORK+15,X'F0'                                                    
         MVC   LINE1T+8(4),WORK+12                                              
         XR    R1,R1                                                            
         ICM   R1,7,ACCPRS                                                      
         CVD   R1,DUB                                                           
         UNPK  WORK(16),DUB                                                     
         OI    WORK+15,X'F0'                                                    
         MVC   LINE1T+16(8),WORK+8                                              
         DROP  R2                                                               
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((LINE1,D),(LINE2,D),(LINE3,DE))                            
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
LINE1    DC    AL2(L'LINE1T)                                                    
LINE1T   DC    CL24' '                                                          
LINE2    DC    AL2(L'LINE2T)                                                    
LINE2T   DC    CL18' '                                                          
LINE3    DC    AL2(L'LINE3T)                                                    
LINE3T   DC    CL79' '                                                          
                                                                                
* ENTRY POINT FOR THE CONTROLLER ROUTINES AVAILABLE TO CONTROLLER AND           
* OVERLAYS.UPON ENTRY,RF HOLDS A(VCOMMON) IN ITS LOW ORDER THREE                
* BYTES AND THE ROUTINE NUMBER IN ITS HIGH ORDER BYTE.VCOMMON WILL              
* USE THE ROUTINE NUMBER TO BRANCH TO THE DESIRED ROUTINE.                      
*                                                                               
VCOMMON  NTR1  BASE=ABASE1         RB=A(FIRST BASE)                             
         L     R9,ABASE2           R9=A(SECOND BASE)                            
         L     R8,ABASE3           R8=A(THIRD BASE)                             
*                                                                               
         SRL   RF,24               BRANCH TO DESIRED ROUTINE                    
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
                                                                                
* TABLE OF BRANCH ADDRESSES TO CONTROLLER ROUTINES                              
*                                                                               
VBRANCH  B     VCALLSUB                                                         
         B     VGETITEM                                                         
         B     VGEMITEM                                                         
         B     VPUTITEM                                                         
         B     VTESTNUM                                                         
         B     VREADTWA                                                         
         B     VWRTTWA                                                          
         B     VHXTOCHR                                                         
         B     VCHRTOHX                                                         
         B     VCONVOFF                                                         
         B     VLOADFLD                                                         
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
                                                                                
* READ OVERLAY INTO PROGRAMS AREA, BAL TO IT, AND WHEN IT RETURNS, PASS         
* CONTROL BACK TO THE CALLING OVERLAY.                                          
*                                                                               
VCALLSUB CLI   CSSP,4              DIE IF STACK OVERFLOWS                       
         BL    *+6                                                              
         DC    H'0'                                                             
         L     R2,0(R1)            R2=A(OVERLAY NUMBER)                         
         CLI   0(R2),0             EXIT IF OVERLAY NUMBER IS ZERO               
         BE    CSX                                                              
*                                                                               
         L     R3,CSNXTLOD         STORE ADDR IN 1ST PARM TO CALLOV             
         ST    R3,DMCB                                                          
         MVC   DMCB+4(3),CSPHASE   STORE PHASE IN 2ND PARM TO CALLOV            
         MVC   DMCB+7(1),0(R2)                                                  
*                                                                               
         GOTO1 CALLOV,DMCB,,,0     CALL CALLOV                                  
         OC    DMCB+9(3),DMCB+9    DIE IF CAN'T FIND OVERLAY                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             DIE IF OVERLAY NOT LOADED WHERE              
         CR    RF,R3               IT WAS WANTED                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    DMCB+10(2),DMCB+10  DIE IF PHASE LENGTH IS ZERO                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R4,R3               R4=ADDRESS TO LOAD NEXT OVERLAY              
         AH    R4,DMCB+10                                                       
         LA    R4,8(R4)                                                         
         SRL   R4,3                                                             
         SLL   R4,3                ROUND UP TO NEAREST DOUBLE WORD              
*                                                                               
         ZIC   RF,CSSP             SAVE CURRENT LOAD ADDRESS ON STACK           
         SLL   RF,2                                                             
         LA    RF,CSSTACK(RF)                                                   
         ST    R3,0(RF)                                                         
         ST    R4,CSNXTLOD         SAVE NEXT LOAD ADDRESS IN CSNXTLOD           
*                                                                               
         ZIC   RF,CSSP             INCREMENT STACK POINTER                      
         LA    RF,1(RF)                                                         
         STC   RF,CSSP                                                          
*                                                                               
         GOTO1 (R3),DMCB,(RC)      PASS CONTROL TO OVERLAY                      
*                                                                               
         ZIC   RF,CSSP             DECREMENT STACK POINTER                      
         BCTR  RF,0                                                             
         STC   RF,CSSP                                                          
*                                                                               
         SLL   RF,2                RESTORE NEXT LOAD ADDRESS FROM STACK         
         LA    RF,CSSTACK(RF)                                                   
         MVC   CSNXTLOD,0(RF)                                                   
*                                                                               
CSX      B     XIT                 EXIT BACK TO CALLER                          
                                                                                
* GET NEXT ITEM FROM INPUT FRAME WORK AREA.                                     
*                                                                               
* GETITEM RETURNS:                                                              
* LENTYPE  : LENGTH OF TYPE DATA OR ZERO FOR NUMERIC TYPES                      
* ATYPE    : A(TYPE DATA) OR TYPE NUMBER IF LENTYPE IS ZERO                     
* DATALEN  : LENGTH OF ITEM DATA                                                
* ADATA    : A(ITEM DATA)                                                       
*                                                                               
* GETITEM MAINTAINS INTERNALLY:                                                 
* AINPUT   : A(NEXT ITEM TO BE RETRIEVED)                                       
* INPLEFT  : SPACE REMAINING IN DATA FRAME                                      
*                                                                               
VGETITEM DS    0H                                                               
         CLI   GETINIT,C'N'        IF GETITEM NOT YET INITIALIZED               
         BNE   GI10                                                             
         MVC   AINPUT,AINFRM       A(BEGINNING OF DATA FRAME)                   
         MVC   INPLEFT,PCIFRMSZ    SPACE REMAINING IN DATA FRAME                
         MVI   EOFFLAG,C'N'                                                     
         MVI   GETINIT,C'Y'                                                     
*                                                                               
GI10     CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET THEN ERROR          
         BE    GIEOF                                                            
         L     R6,AINPUT           POINT R6 TO BEGIN OF NEXT ITEM               
         CLI   0(R6),C' '          IF ITEM TYPE IS BLANK THEN SET END           
         BH    GI20                OF FRAME FLAG AND RETURN                     
         MVI   EOFFLAG,C'Y'                                                     
         B     GINOERR                                                          
*                                                                               
GI20     GOTO1 TESTNUM,DMCB,(R6),2 TEST FIRST TWO BYTES NUMERIC                 
         BNE   GIINV                                                            
         ZIC   R3,0(R6)            EXTRACT LENGTH OF TYPE                       
         ZIC   RF,=C'0'            R3=LENGTH OF TYPE                            
         SR    R3,RF                                                            
         STC   R3,LENTYPE                                                       
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ZIC   R4,0(R6)            EXTRACT LENGTH OF LENGTH                     
         ZIC   RF,=C'0'            R4=LENGTH OF TYPE                            
         SR    R4,RF                                                            
         STC   R4,LENLEN                                                        
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ST    R6,ATYPE            RETURN A(TYPE)                               
         LA    R6,0(R6,R3)         BUMP PAST TYPE                               
*                                                                               
         BAS   RE,GETNUM           TEST IF NUMERIC                              
*                                                                               
         GOTO1 TESTNUM,DMCB,(R6),(R4) TEST ALL LENGTH BYTES NUMERIC             
         BNE   GIINV                                                            
         LR    RF,R4               EXTRACT LENGTH                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)                                                      
         CVB   R5,DUB                                                           
         ST    R5,DATALEN          R5=LENGTH OF DATA                            
         LA    R6,0(R6,R4)         BUMP PAST                                    
*                                                                               
         LA    R2,2                ADD 2 FOR L'TYPE AND L'LENGTH                
         LA    R2,0(R2,R3)         AND LENGTH OF TYPE                           
         LA    R2,0(R2,R4)         ADD LENGTH OF LENGTH OF DATA                 
         LA    R2,0(R3,R5)         ADD LENGTH OF DATA                           
         CLC   PCACTION,=Y(ACECHO) AVOID TERMINATOR FOR ECHO                    
         BE    GI100                                                            
         LA    R2,1(R2)            ADD ONE FOR TERMCHAR                         
                                                                                
* TEST FOR ITEM LENGTH PAST END OF DATA FRAME.                                  
* IF SO,AUTHOR OF DATA OBJECT MADE FATAL ERROR.                                 
*                                                                               
GI100    L     RF,INPLEFT          COMPUTE NEW FRAME AVAILABLE                  
         S     RF,ITEMLEN          !! SURELY DATALEN HERE ?                     
         ST    RF,INPLEFT                                                       
         BM    GIFRMOV             IF NEGATIVE THEN ERROR                       
*                                                                               
         ST    R6,ADATA            RETURN A(DATA)                               
         LA    R6,0(R6,R5)         BUMP PAST DATA                               
*                                                                               
         CLC   PCACTION,=Y(ACECHO) AVOID TERMINATOR FOR ECHO                    
         BE    GI110                                                            
         CLI   0(R6),TERMCHAR      VERIFY TERMINATOR IS PRESENT                 
         BNE   GITRMNF                                                          
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
GI110    ST    R6,AINPUT           SAVE A(NEXT ITEM TO GET)                     
*                                                                               
GINOERR  B     GIYES               NO ERROR                                     
*                                  END OF FRAME ON GET ERROR                    
GIEOF    MVC   PCIACT,=Y(EOFONGET)                                              
         B     GINO                                                             
*                                                                               
GIINV    MVC   PCIACT,=Y(INVITEM)  INVALID ITEM ERROR                           
         B     GINO                                                             
*                                                                               
GIFRMOV  MVC   PCIACT,=Y(FRMOVER)  FRAME OVERFLOW ERROR                         
         B     GINO                                                             
*                                                                               
GITRMNF  MVC   PCIACT,=Y(TRMNOFND) TERMINATOR NOT FOUND ERROR                   
         B     GINO                                                             
*                                                                               
GINO     B     NO                                                               
GIYES    B     YES                                                              
                                                                                
* ROUTINE DETERMINES IF ITEM TYPE FOUND IN DATA FRAME ITEM IS VALID             
* NUMERIC.  IF SO, IT CONVERTS THE EBCDIC FORM OF THE TYPE TO 4-BYTE            
* BINARY FORM AND SETS LENTYPE TO ZERO TO INFORM THE CALLER OF GETITEM          
* THAT THE ITEM TYPE IS IN 4-BYTE BINARY FORM.                                  
*                                                                               
GETNUM   NTR1                                                                   
         ZIC   R2,LENTYPE          IF TYPE NOT NUMERIC                          
         GOTO1 TESTNUM,DMCB,ATYPE,(R2)                                          
         BNE   GNX                 THEN RETURN                                  
*                                                                               
         L     R3,ATYPE            ELSE CONVERT EBCDIC TYPE TO NUMERIC          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,ATYPE                                                      
         MVI   LENTYPE,0           SET LENTYPE TO ZERO FOR NUMERIC TYPE         
*                                                                               
GNX      B     XIT                                                              
                                                                                
* GET NEXT ITEM FROM INPUT FRAME WORK AREA. MANUAL ENTRY FORMAT                 
* I.E.: TYPE1=VALUE1,TYPE2=VALUE2,.....,TYPEN=VALUEN                            
*                                                                               
* GEMITEM RETURNS:                                                              
* LENTYPE  : LENGTH OF TYPE DATA OR ZERO FOR NUMERIC TYPES                      
* ATYPE    : A(TYPE DATA) OR TYPE NUMBER IF LENTYPE IS ZERO                     
* DATALEN  : LENGTH OF ITEM DATA                                                
* ADATA    : A(ITEM DATA)                                                       
*                                                                               
* GEMITEM MAINTAINS INTERNALLY:                                                 
* AINPUT   : A(NEXT ITEM TO BE RETRIEVED)                                       
* INPLEFT  : SPACE REMAINING IN DATA FRAME                                      
*                                                                               
VGEMITEM CLI   GETINIT,C'N'        IF GEMITEM NOT YET INITIALIZED               
         BNE   GM10                                                             
*                                                                               
         MVC   AINPUT,AINFRM       A(BEGINNING OF DATA FRAME)                   
         MVC   INPLEFT,PCIFRMSZ    SPACE REMAINING IN DATA FRAME                
         MVI   EOFFLAG,C'N'                                                     
         MVI   GETINIT,C'Y'                                                     
*                                                                               
GM10     CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET THEN ERROR          
         BE    GMEOF                                                            
         L     R6,AINPUT           POINT R6 TO BEGIN OF NEXT ITEM               
*                                                                               
         CLI   0(R6),C' '          IF ITEM TYPE IS BLANK THEN SET END           
         BH    GM20                OF FRAME FLAG AND RETURN                     
         MVI   EOFFLAG,C'Y'                                                     
         B     GMNOERR                                                          
*                                                                               
GM20     L     R4,INPLEFT          READ TYPE CHARACTERS                         
         SR    R4,R5                                                            
         BM    GMFRMOV                                                          
         MVI   LENTYPE,4           ASSUME 4 DIGITS                              
         ST    R6,ATYPE                                                         
         BAS   RE,GETNUM                                                        
         BNE   GMINV               ELSE INVALID                                 
*                                                                               
         CLI   4(R6),C'='          TEST TYPE/VALUE SEPARATOR                    
         BNE   GMINV                                                            
         LA    R6,5(R6)            READ FOLLOWING DATA STRING                   
         SR    R2,R2               KEEP CHARACTER COUNT                         
         ST    R6,ADATA            SAVE ADDRESS OF START                        
*                                                                               
GML1     BCTR  R4,0                CHECK END OF FRAME                           
         LTR   R4,R4                                                            
         BM    GML1X                                                            
         CLI   0(R6),C','          CECK SEPARATOR CHARACTER                     
         BNE   GML1A                                                            
         LA    R6,1(R6)                                                         
         BCTR  R4,0                                                             
         B     GML1X                                                            
*                                                                               
GML1A    CLI   0(R6),C' '          CHECK END OF DATA IN FRAME                   
         BNH   GML1X                                                            
*                                                                               
         LA    R6,1(R6)            BUMP DATA POINTER                            
         LA    R2,1(R2)            AND LENGTH                                   
         B     GML1                                                             
*                                                                               
GML1X    ST    R6,AINPUT           SAVE NEXT FRAME POSITION                     
         ST    R2,DATALEN          !! WHAT IF ZERO ??                           
         ST    R4,INPLEFT          !!                                           
*                                                                               
GMNOERR  B     GMYES               NO ERROR                                     
*                                                                               
GMEOF    MVC   PCIACT,=Y(EOFONGET) END OF FRAME ON GET ERROR                    
         B     GMNO                                                             
*                                                                               
GMINV    MVC   PCIACT,=Y(INVITEM)  INVALID ITEM ERROR                           
         B     GMNO                                                             
*                                                                               
GMFRMOV  MVC   PCIACT,=Y(FRMOVER)  FRAME OVERFLOW ERROR                         
         B     GMNO                                                             
*                                                                               
GMTRMNF  MVC   PCIACT,=Y(TRMNOFND) TERMINATOR NOT FOUND ERROR                   
*                                                                               
GMNO     B     NO                                                               
GMYES    B     YES                                                              
                                                                                
* PUT ITEM INTO OUTPUT FRAME WORK AREA.                                         
* CALLER PASSES:                                                                
* PARAM 1 : BYTE 0    - LEN OF TYPE OR ZERO (NUMERIC TYPE)                      
*           BYTES 1-3 - TYPE DATA (BINARY IF BYTE 0 IS 0)                       
* PARAM 2 : LENGTH OF ITEM DATA                                                 
* PARAM 3 : A(ITEM DATA)                                                        
*                                                                               
* PUTITEM MAINTAINS INTERNALLY:                                                 
* AOUTPUT : A(NEXT ITEM TO BE ADDED)                                            
* OUTLEFT : SPACE REMAINING IN DATA FRAME                                       
*                                                                               
VPUTITEM MVC   LENTYPE,0(R1)       EXTRACT PARMS INTO WORKING STORAGE           
         MVC   ATYPE,0(R1)                                                      
         MVC   DATALEN,4(R1)                                                    
         MVC   ADATA,8(R1)                                                      
*                                                                               
         CLI   PUTINIT,C'N'        IF PUTITEM NOT YET INITIALIZED               
         BNE   PI10                                                             
*                                  INIT LOCAL VARIABLES                         
         MVC   AOUTPUT,AOUTFRM     A(BEGINNING OF DATA FRAME)                   
         MVC   OUTLEFT,PCIFRMSZ    SPACE LEFT TO ADD ITEMS                      
         MVI   EOFFLAG,C'N'        END OF FRAME FLAG                            
         MVI   PUTINIT,C'Y'                                                     
*                                                                               
PI10     CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET THEN ERROR          
         BE    PIEOF                                                            
* IF LENTYPE IS ZERO THEN ATYPE IS A 4-BYTE BINARY TYPE NUMBER. EDIT            
* NUMBER INTO TYPE DATA WORK AREA,POINT ADATA TO IT AND SET LENTYPE             
* ITS LENGTH.                                                                   
*                                                                               
         BAS   RE,PUTNUM                                                        
         L     R6,AOUTPUT          POINT R6 TO NEXT AREA TO ADD                 
         ZIC   R3,LENTYPE          R3=LENGTH OF TYPE                            
         L     R5,DATALEN          R5=LENGTH OF DATA                            
*                                                                               
         LR    RF,R5               COMPUTE LENGTH OF LENGTH                     
         BAS   RE,NUMDIGS                                                       
         LR    R4,RF                                                            
         STC   R4,LENLEN           R4=LENGTH OF LENGTH                          
*                                                                               
         LA    R2,2                ADD 2 FOR L'TYPE+L'LENGTH                    
         LA    R2,0(R2,R3)         ADD LENGTH OF THE TYPE                       
         LA    R2,0(R2,R4)         ADD LENGTH OF THE LENGTH OF THE DATA         
         LA    R2,0(R2,R5)         ADD LENGTH OF THE DATA                       
         LA    R2,1(R2)            ADD ONE FOR TERMCHAR                         
         ST    R2,ITEMLEN          SAVE FOR DUMP DIAGNOSTICS                    
*                                                                               
         L     RF,OUTLEFT          COMPUTE NEW FRAME AVAILABLE                  
         SR    RF,R2                                                            
         ST    RF,OUTLEFT                                                       
*                                                                               
         BP    PI20                IF NEW FRAME AVAILABLE WENT NEGITIVE         
         MVI   EOFFLAG,C'Y'        SET END OF FRAME FLAG                        
         B     PINOERR             AND RETURN                                   
*                                                                               
PI20     ZIC   RF,=C'0'            SAVE LENGTH OF TYPE                          
         LA    RF,0(R3,RF)                                                      
         STC   RF,0(R6)                                                         
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ZIC   RF,=C'0'            SAVE LENGTH OF LENGTH                        
         LA    RF,0(R4,RF)                                                      
         STC   RF,0(R6)                                                         
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         L     RE,ATYPE            SAVE TYPE                                    
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
         LA    R6,0(R6,R3)         BUMP PAST                                    
*                                                                               
         CVD   R5,DUB              SAVE LENGTH                                  
         OI    DUB+7,X'0F'                                                      
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R6),DUB                                                      
         LA    R6,0(R6,R4)         BUMP PAST                                    
*                                                                               
         L     R7,DATALEN          SAVE DATA                                    
         L     RE,ADATA                                                         
         LR    RF,R7                                                            
         MVCL  R6,RE               MVCL AUTOMATICALLY BUMPS PAST                
*                                                                               
         MVI   0(R6),TERMCHAR      SAVE TERMINATOR BYTE                         
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ST    R6,AOUTPUT          SAVE A(NEXT PLACE TO ADD ITEM)               
*                                                                               
PINOERR  B     YES                 NO ERROR RETURN CC EQUAL                     
*                                                                               
PIEOF    MVC   PCIACT,=Y(EOFONPUT) END OF FRAME ON PUT ERROR                    
         B     NO                                                               
                                                                                
NUMDIGS  LR    R1,RF               CALCULATE NUMBER OF DIGITS FOR VALUE         
         LA    RF,1                FOUND IN RF AND RETURN IN RF                 
*                                                                               
ND10     SR    R0,R0               DIVIDE BY TEN UNTIL ZERO                     
         D     R0,=F'10'                                                        
         LTR   R1,R1                                                            
         BZR   RE                                                               
         LA    RF,1(RF)            BUMP NUMBER OF DIGITS                        
         B     ND10                                                             
                                                                                
* ROUTINE WILL EDIT, IF LENTYPE IS ZERO, THE TYPE NUMBER FOUND IN ATYPE         
* INTO THE TYPE DATA WORK AREA CALLED TYPEWORK.  IT WILL THEN ASSIGN            
* ATYPE TO THE ADDRESS OF THIS WORK AREA AND SET LENTYPE TO ITS LENGTH.         
*                                                                               
PUTNUM   NTR1                                                                   
         CLI   LENTYPE,0           IF LENTYPE NOT ZERO THEN DON'T               
         BNE   PNX                 CONVERT TO EBCDIC                            
*                                                                               
         EDIT  (4,ATYPE),(9,TYPEWORK),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         STC   R0,LENTYPE          SAVE NEW TYPE LENGTH IN LENTYPE              
         LA    RF,TYPEWORK         SET A(TYPE DATA) TO WORK AREA                
         ST    RF,ATYPE                                                         
*                                                                               
PNX      B     XIT                                                              
                                                                                
* TEST IF SOME NUMBER OF BYTES PAST A GIVEN ADDRESS ARE ALL NUMERIC             
*                                                                               
* PARAM 1 - A(FIRST BYTE)                                                       
* PARAM 2 - NUMBER OF BYTES                                                     
*                                                                               
* ON RETURN,COND CODE IS EQ IF ALL BYTES ARE NUMERIC,NEQ OTHERWISE              
*                                                                               
VTESTNUM LM    R2,R3,0(R1)         R2=A(FIRST BYTE),R3=# OF BYTES               
*                                                                               
TN10     CLI   0(R2),C'0'          TEST BYTE IS NOT NUMERIC                     
         BL    TN20                                                             
         CLI   0(R2),C'9'                                                       
         BH    TN20                                                             
         LA    R2,1(R2)            BUMP TO NEXT BYTE AND TRY AGAIN              
         BCT   R3,TN10                                                          
*                                                                               
         CR    RF,RF               ALL BYTES NUMERIC - RETURN EQ                
         B     TNX                                                              
*                                                                               
TN20     LTR   RF,RF               SOME BYTE NOT NUMERIC - RETURN NEQ           
*                                                                               
TNX      B     XIT                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE TWA RECORD NUMBER SPECIFIED BY PARAMETER 2.                               
*                                                                               
VREADTWA LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY)                        
*                                  R3=TWA RECORD NUMBER                         
         SLL   R3,32-8             MOVE TO HIGH ORDER BYTE                      
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R3),(R2),0              
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
* WRITE FROM THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 INTO THE TWA           
* RECORD NUMBER SPECIFIED BY PARAMETER 2.                                       
*                                                                               
VWRTTWA  LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY)                        
*                                  R3=TWA RECORD NUMBER                         
         SLL   R3,32-8             MOVE TO HIGH ORDER BYTE                      
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R3),(R2),0               
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
* THIS ROUTINE CONVERTS THE BINARY NUMBER IN PARAMETER ONE TO AN EIGHT          
* DIGIT '0' FILLED HEX CHARACTER STRING WHICH IS RETURNED IN DUB.  THE          
* ROUTINE RETURNS THE NUMBER OF SIGNIFIGANT CHARACTERS (I.E. 0-F = 1,           
* 10-FF = 2) IN PARAMETER ONE.                                                  
*                                                                               
VHXTOCHR L     R3,0(R1)            R3=BINARY NUMBER                             
*                                                                               
         MVC   DUB,=C'00000000'    FILL DUB WITH '0'S                           
         LA    R4,DUB+7            R4=A(LAST BYTE IN DUB)                       
*                                                                               
HC10     SR    R2,R2               R3=R3/16,R2=REMAINDER                        
         D     R2,=F'16'                                                        
*                                                                               
         C     R2,=F'10'           IF R2>= 10                                   
         BL    *+12                                                             
         LA    R2,C'A'-10(R2)      THEN R2=R2+C'A'-10                           
         B     *+8                                                              
         LA    R2,C'0'(R2)         ELSE R2=R2+C'0'                              
*                                                                               
         STC   R2,0(R4)            SAVE R2 IN DUB                               
*                                                                               
         LTR   R3,R3               IF R3 = 0                                    
         BZ    HC90                THEN DONE                                    
         BCTR  R4,0                ELSE BACK UP POINTER TO DUB                  
         B     HC10                LOOP BACK                                    
*                                                                               
HC90     LA    RF,DUB+8            RETURN LENGTH OF STRING                      
         SR    RF,R5                                                            
         ST    RF,0(R1)                                                         
*                                                                               
HCX      B     XIT                                                              
                                                                                
* THIS ROUTINE CONVERTS THE HEX CHARACTER STRING WHOSE ADDRESS IS IN            
* PARAMETER ONE AND LENGTH IN PARAMETER TWO TO ITS BINARY VALUE WHICH           
* IT RETURNS IN PARAMETER ONE.  IF ANY OF THE CHARACTERS IN THE STRING          
* ARE NOT VALID HEX CHARACTERS, THE ROUTINE RETURNS 'NO'.  OTHERWISE,           
* IT RETURNS 'YES'.                                                             
*                                                                               
VCHRTOHX LM    R2,R3,0(R1)         R2=A(STRING),R3=LEN(STRING)                  
         SR    R5,R5               R5=0 (COMPUTE ANSWER HERE)                   
*                                                                               
CH10     M     R4,=F'16'           R5=R5*16 (SHIFT DIGITS OVER ONE) 03          
         ZIC   RF,0(R2)            RF=CURRENT CHARACTER TO TRANSLATE            
         CLM   RF,1,=C'A'          IF RF < 'A'                                  
         BL    CHNO                THEN ERROR                                   
*                                                                               
         CLM   RF,1,=C'F'          ELSE IF RF <='F'                             
         BH    CH20                                                             
         LA    R0,C'A'-10          THEN R0='A'-10                               
         B     CH90                                                             
*                                                                               
CH20     CLM   RF,1,=C'0'          ELSE IF RF<'0' THEN ERROR                    
         BL    CHNO                                                             
         CLM   RF,1,=C'9'          ELSE IF RF>'9' THEN ERROR                    
         BH    CHNO                                                             
         LA    R0,C'0'             ELSE R0=RF-'0'                               
*                                                                               
CH90     SR    RF,R0               R5=R5+RF-R0 (ADD DIGIT IN)                   
         AR    R5,RF                                                            
         LA    R2,1(R2)            BUMP TO NEXT CHARACTER                       
         BCT   R3,CH10             REPEAT UNTIL NO MORE CHARACTERS              
*                                                                               
         ST    R5,0(R1)            RETURN BINARY NUMBER                         
*                                                                               
CHYES    B     YES                                                              
CHNO     B     NO                                                               
                                                                                
* THIS ROUTINE CONVERTS AN OFFICE CODE FROM 2 TO 8 BYTE FORMAT OR FROM          
* 8 TO 2 BYTE FORMAT.  IT DOES SO BY READING THE CONTROL FILE FOR THE           
* OFFICE RECORD.  IF IT CANNOT FIND THE OFFICE RECORD IT RETURNS 'NO'.          
* OTHERWISE IT DOES THE CONVERSION AND RETURNS 'YES'.                           
*                                                                               
* PARAM1 1 - A(INPUT OFFICE CODE)                                               
*            BYTE 0 0=CONVERT FROM 2 TO 8 BYTE FORMAT                           
*                   1=CONVERT FROM 8 TO 2 BYTE FORMAT                           
* PARAM2 2 - A(OUTPUT OFFICE CODE)                                              
*                                                                               
VCONVOFF LM    R2,R3,0(R1)         R2=A(INPUT OFFICE CODE)                      
*                                  R3=A(OUTPUT OFFICE CODE)                     
         MVC   BYTE,0(R1)          BYTE=CONVERSION TYPE                         
         LA    R4,KEY              BUILD CONTROL FILE USER ID KEY               
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
*                                                                               
         CLI   BYTE,0              IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO10                                                             
         XC    CTIKID(8),CTIKID    THEN PUT 2 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),0(R2)                                                
         B     CO20                                                             
*                                                                               
CO10     MVC   CTIKID(8),0(R2)     ELSE PUT 8 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),=CL2' '                                              
*                                                                               
CO20     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                   
         CLI   8(R1),0                                                          
         BNE   CONO                DISK ERROR                                   
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         CLC   KEY(25),0(R4)       IF KEY DOESN'T MATCH RECORD                  
         BNE   CONO                THEN ERROR RECORD NOT FOUND                  
*                                                                               
         LA    R4,CTIDATA          R4=A(RECORD DATA)                            
         USING CTDSCD,R4                                                        
*                                                                               
CO30     CLI   0(R4),0             IF REACHED END OF RECORD THEN ERROR          
         BE    CONO                                                             
         CLI   0(R4),X'02'         ELSE IF FOUND DESC ELEMENT THEN DONE         
         BE    CO40                                                             
         ZIC   R0,1(R4)            ELSE BUMP TO NEXT ELEMENT                    
         AR    R4,R0                                                            
         B     CO30                LOOP BACK                                    
*                                                                               
CO40     CLI   BYTE,0              IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO50                                                             
         MVC   0(8,R3),CTDSC       RETURN 8 BYTE FORMAT IN PARM 2               
         B     COYES                                                            
*                                                                               
CO50     MVC   0(2,R3),CTDSC       ELSE RETURN 2 BYTE FORMAT IN PARM 2          
         DROP  R4                                                               
*                                                                               
COYES    B     YES                                                              
CONO     B     NO                                                               
                                                                                
* LOAD STRING OF DATA INTO UNPROTECTED TWA FIELD AT GIVEN RELATIVE              
* POSITION SETTING VALUES IN FIELD HEADER                                       
*                                                                               
* PARAM 1 - A(DATA)                                                             
* PARAM 2 - LENGTH DATA                                                         
* PARAM 3 - FIELD NUMBER                                                        
*                                                                               
VLOADFLD LM    R2,R4,0(R1)         R2=A(DATA),R3=LENGTH,R4=FLD#                 
         SR    RF,RF               FIND UNPROTECTED FIELD IN TWA                
         L     RE,ATWA                                                          
         LA    RE,64(RE)                                                        
         USING FLDHDRD,RE                                                       
*                                                                               
LF10     TM    FLDATB,FATBPROT                                                  
         BZ    LF30                                                             
LF20     IC    RF,FLDLEN           BUMP TO NEXT FIELD                           
         AR    RE,RF                                                            
         B     LF10                                                             
LF30     BCT   R4,LF20             BUMP TO NEXT FIELD                           
*                                                                               
         IC    R4,FLDLEN           R4=MAX FIELD DATA LENGTH                     
         AHI   R4,-8                                                            
         TM    FLDATB,FATBXHDR                                                  
         BZ    *+8                                                              
         AHI   R4,-8                                                            
         CR    R3,R4               TEST IF DATA LENGTH GT FIELD MAX             
         BNH   *+6                 NO                                           
         LR    R3,R4               YES TRUNCATE LENGTH TO FIELD MAX             
*                                                                               
         OI    FLDIIND,X'80'       INPUT THIS TIME                              
         STC   R3,FLDILEN          INPUT LENGTH                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),0(R2)    MOVE IN DATA                                 
*                                                                               
         SR    R2,R2               SET NUM/ALPHA/HEX BITS                       
         IC    R2,FLDILEN                                                       
         LTR   R2,R2                                                            
         BZ    LFX                                                              
         LA    R3,FLDDATA                                                       
         OI    FLDIIND,B'00001110' SET BITS ON                                  
*                                                                               
LF40     TM    FLDATB,FATBLC       LOWER CASE TESTS                             
         BZ    LF50                                                             
         CLI   0(R3),X'81'         TEST LITTLE A                                
         BL    LF50A                                                            
         CLI   0(R3),X'A9'         TEST LITTLE Z                                
         BH    LF50                                                             
         NI    FLDIIND,B'11110101' FLD NOT NUM/HEX                              
         B     LF60                                                             
*                                                                               
LF50     CLI   0(R3),C'A'                                                       
         BNL   *+12                                                             
LF50A    NI    FLDIIND,B'11110001' FLD NOT NUM/ALPHA/HEX                        
         B     LF70                                                             
LF50B    CLI   0(R3),C'Z'                                                       
         BNH   LF50C                                                            
         NI    FLDIIND,B'11111011' FLD NOT ALPHA                                
         B     LF60                                                             
LF50C    NI    FLDIIND,B'11110111' FLD NOT NUM                                  
         CLI   0(R3),C'F'                                                       
         BNH   *+8                                                              
         NI    FLDIIND,B'11111101' FLD NOT HEX                                  
*                                                                               
LF60     LA    R3,1(R3)                                                         
         BCT   R2,LF40                                                          
*                                                                               
LF70     TM    FLDATB,FATBNUM      REQUIRED NUM FIELD                           
         BZ    LFX                 NO                                           
         TM    FLDIIND,X'08'       IS IT NUM                                    
         BO    LFX                 YES                                          
         OI    FLDIIND,X'10'       NO SET FLD INV                               
         B     LFX                                                              
*                                                                               
LFX      B     XIT                                                              
         DROP  RE                                                               
                                                                                
* CONSTANTS USED FOR DATAMGR CALLS                                              
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READL    DC    CL8'READ'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
                                                                                
* TABLE OF LINKED ROUTINES ADDRESSES                                            
*                                                                               
LINKED   DS    0F                                                               
         DC    V(DUMMY)            AREA TO LOAD OVERLAYS                        
NLINKED  EQU   (*-LINKED)/4                                                     
                                                                                
* TABLE OF CORE RESIDENT ROUTINE OVERLAY NUMBER EQUATES                         
*                                                                               
CORERES  DS    0X                                                               
NCORERES EQU   (*-CORERES)                                                      
*                                                                               
RELO     DS    A                   RELOCATION CONSTANT                          
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
*                                                                               
XDIRECT  L     RD,SAVERD           EXIT MODULE                                  
         XMOD1 1                                                                
                                                                                
         LTORG                                                                  
                                                                                
TERMCHAR EQU   C'+'                                                             
TRACE    DC    C'N'                                                             
TRACEIN  DC    C'N'                                                             
SPACES   DC    255C' '                                                          
                                                                                
* TABLE OF ACTIONS ACCESSED FROM VALACT.                                        
* VARIABLE LENGTH ENTRIES FOR EACH ACTION:                                      
* YL2    ACTION CODE                                                            
* AL4    INPUT ACTION ROUTINE                                                   
* AL4    OUTPUT ACTION ROUTINE                                                  
* YL2    (ITEM IO ROUTINE,CONVERSION INDICATOR)                                 
* AL4    (0) END OF ENTRY                                                       
*                                                                               
* TABLE TERMINATED BY CODE ACABEND                                              
*                                                                               
ACTTAB   DS    0D                                                               
*                                  DISPLAY ITEM VALUE                           
         DC    YL2(ACDISP)         =PC,1                                        
         DC    AL4(PIDISP)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITTYPE,ITI+ITO) 9001 - ITEM TYPE NUMBER                      
         DC    AL4(0)                                                           
*                                  CHANGE ITEM VALUE                            
         DC    YL2(ACCHGE)         =PC,2                                        
         DC    AL4(PICHGE)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITTYPE,ITI+ITO) 9001 - ITEM TYPE NUMBER                      
         DC    YL2(ITDATA,ITI)     9002 - ITEM DATA VALUE                       
         DC    AL4(0)                                                           
*                                  DISPLAY SYSTEM STATUS INFO                   
         DC    YL2(ACWHOAMI)       =PC,3                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    YL2(ITFPID,ITO)     9003 - FACPAK SYSTEM ID                      
         DC    YL2(ITLUID,ITO)     9004 - UTL LUID                              
         DC    YL2(ITCTRY,ITO)     9005 - UTL COUNTRY                           
         DC    YL2(ITLANG,ITO)     9006 - UTL LANGUAGE                          
         DC    YL2(ITUSER,ITO)     9101 - CURRENT SESSION USER ID               
         DC    YL2(ITSYS,ITO)      9102 - CURRENT SESSION SYSTEM ID             
         DC    YL2(ITPROG,ITO)     9103 - CURRENT SESSION PROGRAM ID            
         DC    YL2(ITPASS,ITO)     9104 - CURRENT SESSION PASSWORD              
         DC    YL2(ITUSER1,ITO)    9111 - SAVED SESSION 1 USER ID               
         DC    YL2(ITSYS1,ITO)     9112 - SAVED SESSION 1 SYSTEM                
         DC    YL2(ITPROG1,ITO)    9113 - SAVED SESSION 1 PROGRAM               
         DC    YL2(ITPASS1,ITO)    9114 - SAVED SESSION 1 PASSWORD              
         DC    AL4(0)                                                           
*                                  PHYSICAL CONNECT                             
         DC    YL2(ACCONPHY)       =PC,4                                        
         DC    AL4(PICTPH)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITUSER,ITI)     9101 - CURRENT SESSION USER ID               
         DC    YL2(ITSYS,ITI)      9102 - CURRENT SESSION SYSTEM ID             
         DC    YL2(ITPROG,ITI)     9103 - CURRENT SESSION PROGRAM ID            
         DC    YL2(ITPASS,ITI)     9104 - CURRENT SESSION PASSWORD              
         DC    YL2(ITCDATA,ITI)    9007 - CONNECT DATA                          
         DC    AL4(0)                                                           
*                                  LOGICAL CONNECT                              
         DC    YL2(ACCONLOG)       =PC,5                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                  GENERAL TWA FIELD INPUT                      
         DC    YL2(ACFIELDS)       =PC,6                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                  ECHO DATA ITEM                               
         DC    YL2(ACECHO)         =PC,7                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    YL2(ITDATA,ITI+ITO) 9002 - ITEM DATA VALUE                       
         DC    AL4(0)                                                           
*                                  WEB IP CHECK                                 
         DC    YL2(ACWIPCHK)       =PC,8                                        
         DC    AL4(PIWIPCHK)                                                    
         DC    AL4(0)                                                           
         DC    YL2(ITIPADR,ITI)    9202 - IP ADDRESS                            
         DC    YL2(ITIPFLAG,ITO)   9201 - WEB IP CHECK FLAG                     
         DC    YL2(ITIPAID,ITO)    9203 - IP AGENCY ALPHA ID                    
         DC    YL2(ITIPPFLG,ITO)   9210 - IP AGENCY PERSON ID FLAG              
         DC    YL2(ITIPUID,ITO)    9204 - IP USER ID                            
         DC    YL2(ITIPPUID,ITO)   9211 - IP PU ID                              
         DC    YL2(ITIPRNAM,ITO)   9212 - IP RESOURCE NAME                      
         DC    YL2(ITIPANAM,ITO)   9205 - IP AGENCY NAME                        
         DC    YL2(ITIPDNAM,ITO)   9206 - IP DESTINATION NAME                   
         DC    YL2(ITIPADL1,ITO)   9207 - IP DEST ADDRESS LINE1                 
         DC    YL2(ITIPADL2,ITO)   9208 - IP DEST ADDRESS LINE1                 
         DC    YL2(ITIPADL3,ITO)   9209 - IP DEST ADDRESS LINE1                 
         DC    AL4(0)                                                           
*                                  WEB USER AUTHENTICATION                      
         DC    YL2(ACWUAUT)        =PC,9                                        
         DC    AL4(PIWUAUT)                                                     
         DC    AL4(0)                                                           
         DC    YL2(ITUAUID,ITI)    9221 - USER ID                               
         DC    YL2(ITUAPID,ITI)    9222 - PERSON ID                             
         DC    YL2(ITUAPWD,ITI)    9223 - PASSWORD CODE                         
         DC    YL2(ITIPADR,ITI)    9202 - IP ADDRESS                            
         DC    YL2(ITPAPID,ITI)    9310 - WEB PGM ID(FCPK SYS/PGM HEX)          
         DC    YL2(ITUAFLAG,ITO)   9220 - WEB USER AUTHENTICATION FLAG          
         DC    YL2(ITUAPIN,ITO)    9224 - PERSON PASSWORD NUMBER                
         DC    YL2(ITUAUIN,ITO)    9225 - USER ID NUMBER                        
         DC    YL2(ITUAOFF,ITO)    9226 - PERSON OFFICE CODE                    
         DC    YL2(ITUADEP,ITO)    9227 - PERSON DEPARTMENT CODE                
         DC    YL2(ITUACTRY,ITO)   9228 - USER ID COUNTRY CODE                  
         DC    YL2(ITUADHI,ITO)    9229 - PERSON HIRE DATE                      
         DC    YL2(ITUADTE,ITO)    9230 - PERSON TERMINATION DATE               
         DC    YL2(ITUAAGC,ITO)    9231 - PERSON ACCESS GROUP CODE              
         DC    YL2(ITUAFNA,ITO)    9232 - PERSON FIRST NAME                     
         DC    YL2(ITUAMNA,ITO)    9233 - PERSON MIDDLE NAME                    
         DC    YL2(ITUALNA,ITO)    9234 - PERSON LAST NAME                      
         DC    YL2(ITUAUANM,ITO)   9235 - USER ID AGENCY NAME                   
         DC    YL2(ITUAUAID,ITO)   9236 - USER ID AGENCY ID                     
         DC    YL2(ITUASAID,ITO)   9237 - SECURITY AGENCY ID                    
         DC    YL2(ITUADNAM,ITO)   9238 - USER ID DESTINATION NAME              
         DC    YL2(ITUAADL1,ITO)   9239 - USER ID ADDRESS LINE 1                
         DC    YL2(ITUAADL2,ITO)   9240 - USER ID ADDRESS LINE 2                
         DC    YL2(ITUAADL3,ITO)   9241 - USER ID ADDRESS LINE 3                
         DC    YL2(ITUAPACL,ITO)   9300 - PROGRAM ACCESS LIST                   
         DC    YL2(ITUAWCTL,ITO)   9242 - WEB CONTROLS                          
         DC    YL2(ITUAACOD,ITO)   9243 - ACCESS CODE                           
         DC    AL4(0)                                                           
*                                  WEB PROGRAM AUTHORISATION                    
         DC    YL2(ACWPAUT)        =PC,10                                       
         DC    AL4(PIWPAUT)                                                     
         DC    AL4(0)                                                           
         DC    YL2(ITPAPID,ITI)    9310 - WEB PGM ID (FCPK SYS/PGM HEX)         
         DC    YL2(ITUAPIN,ITI)    9224 - PERSON PASSWORD NUMBER                
         DC    YL2(ITUAUIN,ITI)    9225 - USER ID NUMBER                        
         DC    YL2(ITUAAGC,ITI)    9231 - PERSON ACCESS GROUP CODE              
         DC    YL2(ITUAUID,ITI)    9221 - USER ID                               
         DC    YL2(ITUAPID,ITI)    9222 - PERSON ID                             
         DC    YL2(ITUASAID,ITI)   9237 - SECURITY AGENCY ID                    
         DC    YL2(ITUAPWD,ITI)    9223 - PASSWORD CODE                         
         DC    YL2(ITPAFLAG,ITO)   9311 - WEB PGM AUTHORISATION FLAG            
         DC    YL2(ITPAAUTH,ITO)   9315 - WEB PGM AUTHORISATION CODE            
         DC    YL2(ITPAADV,ITO)    9316 - WEB PGM ADV VTAM ID                   
         DC    YL2(ITPASEN,ITO)    9317 - WEB PGM FACPAK SYSTEM SE#             
         DC    YL2(ITPALACC,ITO)   9318 - WEB PGM LIMIT ACCESS LIST             
         DC    YL2(ITPAACL,ITO)    9312 - PROGRAM ACTION ACCESS LIST            
         DC    YL2(ITPAFCON,ITO)   9313 - FIELD CONTROLS                        
         DC    YL2(ITPAOCON,ITO)   9314 - OPTION CONTROLS                       
         DC    AL4(0)                                                           
*                                  WEB PROGRAM ACCESS CHECK                     
         DC    YL2(ACWPACC)        =PC,11                                       
         DC    AL4(PIWPACC)                                                     
         DC    AL4(0)                                                           
         DC    YL2(ITPAPID,ITI)    9310 - WEB PGM ID (FCPK SYS/PGM HEX)         
         DC    YL2(ITUAPIN,ITI)    9224 - PERSON PASSWORD NUMBER                
         DC    YL2(ITUAUIN,ITI)    9225 - USER ID NUMBER                        
         DC    YL2(ITUASAID,ITI)   9237 - SECURITY AGENCY ID                    
         DC    YL2(ITUAPACL,ITO)   9300 - PROGRAM ACCESS LIST                   
         DC    AL4(0)                                                           
*                                  ACQUIRE FIXED VLU TOKEN                      
         DC    YL2(ACVLUAFX)       =PC,20                                       
         DC    AL4(PIVLUAFX)                                                    
         DC    AL4(0)                                                           
         DC    YL2(ITVLUT,ITO)     9100 - VLU TOKEN                             
         DC    AL4(0)                                                           
*                                  ACQUIRE FLOATING VLU TOKEN                   
         DC    YL2(ACVLUAFL)       =PC,21                                       
         DC    AL4(PIVLUAFL)                                                    
         DC    AL4(0)                                                           
         DC    YL2(ITVLUT,ITO)     9100 - VLU TOKEN                             
         DC    AL4(0)                                                           
*                                  MAKE THIS LU HAVE FLOATING VLUS              
         DC    YL2(ACVLUMFL)       =PC,22                                       
         DC    AL4(PIVLUMFL)                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                  RELEASE THE CURRENT VLU                      
         DC    YL2(ACVLURLS)       =PC,23                                       
         DC    AL4(PIVLURLS)                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                  RELEASE MULTIPLE VLU TOKENS                  
         DC    YL2(ACVLURLM)       =PC,24                                       
         DC    AL4(PIVLURLM)                                                    
         DC    AL4(0)                                                           
         DC    YL2(ITVLUT,ITI)     9100 - VLU TOKEN                             
         DC    AL4(0)                                                           
*                                  RELEASE VLU RESOURCES                        
         DC    YL2(ACVLURSC)       =PC,25                                       
         DC    AL4(PIVLURSC)                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                  PASSWORD CHANGE                              
         DC    YL2(ACCPWD)         =PC,30                                       
         DC    AL4(PICPWD)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITUAUID,ITI)    9221 - USER ID                               
         DC    YL2(ITUAPID,ITI)    9222 - PERSON ID                             
         DC    YL2(ITUAPWD,ITI)    9223 - PASSWORD CODE                         
         DC    YL2(ITIPADR,ITI)    9202 - IP ADDRESS                            
         DC    YL2(ITNPWD,ITI)     9010 - NEW PASSWORD CODE                     
         DC    AL4(0)                                                           
*                                  SYSTEM SERVICE                               
         DC    YL2(ACSSRV)         =PC,31                                       
         DC    AL4(PISSRV)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITSSRV,ITI)     9008 - SYSTEM SERVICE ID                     
         DC    YL2(ITUC,ITI)       9011 - USER CONTROL DATA                     
         DC    YL2(ITDATA,ITI)     9002 - ITEM DATA VALUE                       
         DC    YL2(ITDATA,ITO)     9002 - ITEM DATA VALUE                       
         DC    AL4(0)                                                           
*                                  GENERAL ACCESS INFORMATION                   
         DC    YL2(ACGAIN)         =PC,32                                       
         DC    AL4(PIACSA)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITPAPID,ITI)    9310 - PGM ID (FCPK SYS/PGM HEX)             
         DC    YL2(ITPASEN,ITI)    9317 - PGM FACPAK SYSTEM SE#                 
         DC    YL2(ITUAUAID,ITI)   9236 - AGENCY ID                             
         DC    YL2(ITUASAID,ITI)   9237 - SECURITY AGENCY ID                    
         DC    YL2(ITUAPAID,ITI)   9244 - SECURITY AGY FOR PERSON (DDS)         
         DC    YL2(ITUAUIN,ITI)    9225 - USER ID NUMBER                        
         DC    YL2(ITUAPIN,ITI)    9224 - PERSON ID # (PIN) PASSWORD #          
         DC    YL2(ITUAAGC,ITI)    9231 - ACCESS GROUP NUMBER                   
         DC    YL2(ITUC,ITO)       9011 - USER CONTROL DATA                     
         DC    AL4(0)                                                           
*                                  CLIENT ACCESS AUTHORIZATION                  
         DC    YL2(ACCLAA)         =PC,33                                       
         DC    AL4(PIACSA)                                                      
         DC    AL4(0)                                                           
         DC    YL2(ITPAPID,ITI)    9310 - PGM ID (FCPK SYS/PGM HEX)             
         DC    YL2(ITPASEN,ITI)    9317 - PGM FACPAK SYSTEM SE#                 
         DC    YL2(ITUAUAID,ITI)   9236 - AGENCY ID                             
         DC    YL2(ITUASAID,ITI)   9237 - SECURITY AGENCY ID                    
         DC    YL2(ITUAPAID,ITI)   9244 - SECURITY AGY FOR PERSON (DDS)         
         DC    YL2(ITUAUIN,ITI)    9225 - USER ID NUBMER                        
         DC    YL2(ITUAPIN,ITI)    9224 - PERSON ID # (PIN) PASSWORD #          
         DC    YL2(ITUAAGC,ITI)    9231 - ACCESS GROUP NUMBER                   
         DC    YL2(ITDATA,ITI)     9002 - ITEM DATA VALUE (MEDIA/CLNT)          
         DC    YL2(ITDATA,ITO)     9002 - ITEM DATA VALUE                       
         DC    AL4(0)                                                           
*                                  END OF TABLE                                 
         DC    YL2(ACABORT)                                                     
*                                                                               
* ITEM CONVERSION INDICATOR EQUATES                                             
*                                                                               
ITI      EQU   X'01'               INPUT VALIDATION REQUIRED                    
ITO      EQU   X'02'               OUTPUT FORMATTING RQUIRED                    
                                                                                
* ITEM DATA IO VALIDATION AND FORMATTING TABLE                                  
* FIXED LENGTH ENTRIES FOR EACH ITEM TYPE:                                      
* YL2    ITEM TYPE CODE                                                         
* AL4    INPUT VALIDATION ROUTINE                                               
* AL4    OUTPUT FORMATTING ROUTINE                                              
* AL4    ITEM UPDATE ROUTINE                                                    
* XL2    VALIDATION INDICATORS,(TO BE DEVELOPED...)                             
*                                                                               
* TABLE TERMINATED BY TYPE=0                                                    
*                                                                               
ICONVUP  EQU   X'80'               CONVERT TO UPPER CASE FLAG                   
ICONESC  EQU   X'40'               CONVERT ESCAPE SEQUENCES FOR I/O             
*                                                                               
ITCVTAB  DS    0XL(ITCVTLEN)                                                    
         DC    YL2(ITTYPE),AL4(ITITYPE,ITOTYPE,0),AL1(0,0)                      
         DC    YL2(ITDATA),AL4(0,ITODATA,0),AL1(0,0)                            
         DC    YL2(ITFPID),AL4(0,ITOFPID,0),AL1(ICONVUP,0)                      
         DC    YL2(ITLUID),AL4(0,ITOLUID,0),AL1(ICONVUP,0)                      
         DC    YL2(ITCTRY),AL4(0,ITOCTRY,ITUCTRY),AL1(ICONVUP,0)                
         DC    YL2(ITUSER),AL4(0,ITOUSER,0),AL1(ICONVUP,0)                      
         DC    YL2(ITSYS),AL4(0,ITOSYS,0),AL1(ICONVUP,0)                        
         DC    YL2(ITPROG),AL4(0,ITOPROG,0),AL1(ICONVUP,0)                      
         DC    YL2(ITPASS),AL4(0,ITOPASS,0),AL1(0,0)                            
         DC    YL2(ITLANG),AL4(0,ITOLANG,ITULANG),AL1(ICONVUP,0)                
         DC    YL2(ITUSER1),AL4(0,ITOUSER1,0),AL1(ICONVUP,0)                    
         DC    YL2(ITSYS1),AL4(0,ITOSYS1,0),AL1(ICONVUP,0)                      
         DC    YL2(ITPROG1),AL4(0,ITOPROG1,0),AL1(ICONVUP,0)                    
         DC    YL2(ITPASS1),AL4(0,ITOPASS1,0),AL1(0,0)                          
         DC    YL2(ITUSER2),AL4(0,ITOUSER2,0),AL1(ICONVUP,0)                    
         DC    YL2(ITSYS2),AL4(0,ITOSYS2,0),AL1(ICONVUP,0)                      
         DC    YL2(ITPROG2),AL4(0,ITOPROG2,0),AL1(ICONVUP,0)                    
         DC    YL2(ITPASS2),AL4(0,ITOPASS2,0),AL1(0,0)                          
         DC    YL2(ITUSER3),AL4(0,ITOUSER3,0),AL1(ICONVUP,0)                    
         DC    YL2(ITSYS3),AL4(0,ITOSYS3,0),AL1(ICONVUP,0)                      
         DC    YL2(ITPROG3),AL4(0,ITOPROG3,0),AL1(ICONVUP,0)                    
         DC    YL2(ITPASS3),AL4(0,ITOPASS3,0),AL1(0,0)                          
         DC    YL2(ITCDATA),AL4(0,0,0),AL1(ICONVUP,0)                           
*                                                                               
         DC    YL2(ITIPFLAG),AL4(0,ITOIPFLG,0),AL1(0,0)                         
         DC    YL2(ITIPADR),AL4(0,ITOIPADR,0),AL1(0,0)                          
         DC    YL2(ITIPAID),AL4(0,ITOIPAID,0),AL1(0,0)                          
         DC    YL2(ITIPPFLG),AL4(0,ITOIPPFL,0),AL1(0,0)                         
         DC    YL2(ITIPUID),AL4(0,ITOIPUID,0),AL1(0,0)                          
         DC    YL2(ITIPPUID),AL4(0,ITOIPPUI,0),AL1(0,0)                         
         DC    YL2(ITIPRNAM),AL4(0,ITOIPRNA,0),AL1(0,0)                         
         DC    YL2(ITIPANAM),AL4(0,ITOIPANM,0),AL1(0,0)                         
         DC    YL2(ITIPDNAM),AL4(0,ITOIPDNM,0),AL1(0,0)                         
         DC    YL2(ITIPADL1),AL4(0,ITOIPAD1,0),AL1(0,0)                         
         DC    YL2(ITIPADL2),AL4(0,ITOIPAD2,0),AL1(0,0)                         
         DC    YL2(ITIPADL3),AL4(0,ITOIPAD3,0),AL1(0,0)                         
*                                                                               
         DC    YL2(ITUAFLAG),AL4(0,ITOUAFLG,0),AL1(0,0)                         
         DC    YL2(ITUAUID),AL4(0,ITOUAUID,0),AL1(0,0)                          
         DC    YL2(ITUAPID),AL4(0,ITOUAPID,0),AL1(ICONVUP,0)                    
         DC    YL2(ITUAPWD),AL4(0,ITOUAPWD,0),AL1(0,0)                          
         DC    YL2(ITUAPIN),AL4(0,ITOUAPIN,0),AL1(0,0)                          
         DC    YL2(ITUAUIN),AL4(0,ITOUAUIN,0),AL1(0,0)                          
         DC    YL2(ITUAOFF),AL4(0,ITOUAOFF,0),AL1(0,0)                          
         DC    YL2(ITUADEP),AL4(0,ITOUADEP,0),AL1(0,0)                          
         DC    YL2(ITUACTRY),AL4(0,ITOUACTR,0),AL1(0,0)                         
         DC    YL2(ITUADHI),AL4(0,ITOUADHI,0),AL1(0,0)                          
         DC    YL2(ITUADTE),AL4(0,ITOUADTE,0),AL1(0,0)                          
         DC    YL2(ITUAAGC),AL4(0,ITOUAAGC,0),AL1(0,0)                          
         DC    YL2(ITUAFNA),AL4(0,ITOUAFNA,0),AL1(0,0)                          
         DC    YL2(ITUAMNA),AL4(0,ITOUAMNA,0),AL1(0,0)                          
         DC    YL2(ITUALNA),AL4(0,ITOUALNA,0),AL1(0,0)                          
         DC    YL2(ITUAUANM),AL4(0,ITOUAUAN,0),AL1(0,0)                         
         DC    YL2(ITUAUAID),AL4(0,ITOUAUAI,0),AL1(ICONESC,0)                   
         DC    YL2(ITUASAID),AL4(0,ITOUASAI,0),AL1(ICONESC,0)                   
         DC    YL2(ITUAPAID),AL4(0,ITOUAPAI,0),AL1(ICONESC,0)                   
         DC    YL2(ITUADNAM),AL4(0,ITOUADNM,0),AL1(0,0)                         
         DC    YL2(ITUAADL1),AL4(0,ITOUAAD1,0),AL1(0,0)                         
         DC    YL2(ITUAADL2),AL4(0,ITOUAAD2,0),AL1(0,0)                         
         DC    YL2(ITUAADL3),AL4(0,ITOUAAD3,0),AL1(0,0)                         
*                                                                               
         DC    YL2(ITUAPACL),AL4(0,ITOUAPAC,0),AL1(0,0)                         
         DC    YL2(ITUAWCTL),AL4(0,ITOUWCTL,0),AL1(0,0)                         
         DC    YL2(ITUAACOD),AL4(0,ITOUACOD,0),AL1(0,0)                         
*                                                                               
         DC    YL2(ITPAPID),AL4(0,ITOPAPID,0),AL1(0,0)                          
*                                                                               
         DC    YL2(ITPAFLAG),AL4(0,ITOPAFLG,0),AL1(0,0)                         
         DC    YL2(ITPAAUTH),AL4(0,ITOPAAUT,0),AL1(0,0)                         
         DC    YL2(ITPAADV),AL4(0,ITOPAADV,0),AL1(0,0)                          
         DC    YL2(ITPASEN),AL4(0,ITOPASEN,0),AL1(0,0)                          
         DC    YL2(ITPALACC),AL4(0,ITOPALAC,0),AL1(0,0)                         
         DC    YL2(ITPAACL),AL4(0,ITOPAACL,0),AL1(0,0)                          
         DC    YL2(ITPAFCON),AL4(0,ITOPAFCO,0),AL1(0,0)                         
         DC    YL2(ITPAOCON),AL4(0,ITOPAOCO,0),AL1(0,0)                         
*                                                                               
         DC    YL2(ITVLUT),AL4(0,ITOVLUT,0),AL1(0,0)                            
*                                                                               
         DC    YL2(ITVER),AL4(0,0,0),AL1(0,0)                                   
         DC    YL2(ITSSRV),AL4(0,0,0),AL1(0,0)                                  
         DC    YL2(ITNPWD),AL4(0,0,0),AL1(0,0)                                  
         DC    YL2(ITUC),AL4(0,ITOUC,0),AL1(0,0)                                
*                                                                               
         DC    YL2(0)                                                           
ITCVTABX EQU   *                                                                
                                                                                
* SRPCIFFD                                                                      
       ++INCLUDE SRPCIFFD                                                       
                                                                                
* SRPCIWORKD                                                                    
       ++INCLUDE SRPCIWORKD                                                     
                                                                                
* SRPCIDSECT                                                                    
       ++INCLUDE SRPCIDSECT                                                     
                                                                                
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
                                                                                
* DDGETRETD                                                                     
       ++INCLUDE DDGETRETD                                                      
                                                                                
* FADSECTS                                                                      
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* FAFACTS                                                                       
* FASYSLSTD                                                                     
* FACHKPT                                                                       
* SRDDEQUS                                                                      
* DDACCESSD                                                                     
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FACHKPT                                                        
       ++INCLUDE SRDDEQUS                                                       
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
       ++INCLUDE DDACCESSD                                                      
                                                                                
LENCON   EQU   2048                FIRST HALF OF CONTROLLER STORAGE             
*                                  WHICH IS COVERED BY CONTROLD                 
*                                  THIS MEMORY IS NOT SAVED                     
LENSAVE  EQU   2048                SECOND HALF OF CONTROLLER STORAGE            
*                                  ALSO COVERED BY CONTROLD BUT                 
*                                  SAVED IN TWA1 EACH TRANSACTION               
*                                  OF CONTROLLER'S STORAGE                      
LENINF   EQU   2048                INPUT FRAME AREA - NOT SAVED                 
LENOUTF  EQU   2048                OUTPUT FRAME AREA - NOT SAVED                
LENIOS   EQU   8*K                 TWO 4096 BYTE IO AREAS FOR USE IN            
*                                  DATAMGR CALLS - NOT SAVED                    
LENTWASV EQU   4096                SPACE FOR PC SCREEN TWA SAVE ON              
*                                  TRANSFER OF CONTROL TO ANOTHER PGM           
*                                                                               
LENTAB   EQU   8*(ITPCMAX-ITPCBASE)  SPACE FOR ITEM DATA SAVE AREA              
*                                                                               
LENWORK  EQU   LENCON+LENSAVE+LENINF+LENOUTF+LENIOS+LENTAB+LENTWASV             
*                                                                               
K        EQU   1024                1 KILOBYTE=1024 BYTES                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SRPCI00   12/22/20'                                      
         END                                                                    
