*          DATA SET ACREPJS02  AT LEVEL 001 AS OF 08/10/20                      
*PHASE ACJS02C                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE XSORT                                                                  
         TITLE 'JOB STATUS REPORT'                                              
*---------------------------------------------------------------------*         
* THIS MODULE PROVIDES THE JOB STATUS REPORT BASED ON THE REQUEST     *         
* OPTIONS AND START/END DATE PROVIDED. START/END DATE FILTER CRITERIA *         
* IS BY THE JOB OPEN DATE.                                            *         
* THIS REPORT COULD BE RUN FOR ANY LEVEL OF SJ ACCOUNT.               *         
*                                                                     *         
* REQUEST OPTIONS:-                                                   *         
*                                                                     *         
* QOPT1 - SHOW DETAILS OF THE LINKS OF JOB (Y/N)                      *         
* QOPT2 - SHOW ELEMENT LEVEL DETAILS OF TRANSACTIONS (Y/N)            *         
* QOPT3 - FILTER BY ELIGIBLE FOR DELETION FLAG (Y/N)                  *         
* QOPT4 - SHOW CLOSED JOBS ONLY (Y/N)                                 *         
*                                                                     *         
* START DATE - FILTER BY JOB OPEN DATE                                *         
* END DATE   - FILTER BY JOB OPEN DATE                                *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* RKEJ 001 10AUG20 <SPEC-45996> NEW PROGRAM TO SHOW JOB STATUS AND    *         
*                               LINKS                                 *         
*---------------------------------------------------------------------*         
ACJS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACJS**,R9       BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACJSD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,PROCLEVA       PROCESS LEVEL A                              
         BE    PLEVA                                                            
         CLI   MODE,PROCLEVB       PROCESS LEVEL B                              
         BE    PLEVB                                                            
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,PROCSBAC       PROCESS CONTRA ACCOUNT                       
         BE    PSUB                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PTRN                                                             
         CLI   MODE,SBACLAST       SUB-ACCOUNT LAST                             
         BE    SUBL                                                             
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
         USING CPXELD,R7                                                        
         XC    SVCPSTA6,SVCPSTA6                                                
         L     R7,ADCMPEL                                                       
         MVI   ELCODE,CPXELQ       X'39' - COMPANY XTRA ELEMENT                 
         JAS   RE,NEXTEL                                                        
         JNE   RUNFX                                                            
         MVC   SVCPSTA6(1),CPXSTAT6                                             
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
*                                                                               
* READ CONTROL FILE CT5 RECORD                                                  
*                                                                               
         USING CT5REC,R3                                                        
         LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   CT5KTYP,CT5KTYPQ    C'5' - SYSTEM ACCESS RECORDS                 
         MVC   CT5KALPH,ALPHAID                                                 
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(CT5LEN-CT5KEY),SVKEY                                       
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
*                                                                               
         LA    R3,IO                                                            
         LA    R2,CT5DATA                                                       
         MVC   SVAID,ALPHAID       DEFAULT TO ALPHA ID                          
REQF10   CLI   0(R2),0                                                          
         BE    REQFX                                                            
         CLI   0(R2),X'B8'         SECURITY ALPHA ID ELM                        
         BE    REQF20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     REQF10                                                           
*                                                                               
         USING CTSEAD,R2                                                        
REQF20   MVC   SVAID,CTSEAAID      SECURITY ID                                  
         DROP  R2                                                               
*                                                                               
REQFX    DS    0H                                                               
         CLC   QSTART,SPACES                                                    
         JNH   REQFX10                                                          
         GOTO1 DATCON,DMCB,(0,QSTART),(X'20',SVQSRT) QSTART DATE                
REQFX10  CLC   QEND,SPACES                                                      
         JNH   EXIT                                                             
         GOTO1 DATCON,DMCB,(0,QEND),(X'20',SVQEND)   QEND DATE                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING LDGELD,R2                                                        
         L     R2,ADLDGEL          ADDRESS LEDGER ELEMENT                       
         MVC   OFFPOS,LDGOPOS      LOCATION OF OFFICE                           
*                                                                               
LDGFX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
         SPACE 1                                                                
PLEVA    DS    0H                                                               
         MVC   SVCLI,SPACES        CLEAR SAVED AREA FOR CLIENT CODE             
         MVC   SVCLINM,SPACES      CLEAR SAVED AREA FOR CLIENT NAME             
         MVC   SVOFF,SPACES                                                     
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRA          R2=A(LEVAL A ACCOUNT RECORD)                 
         MVC   SVCLI,ACTKACT       SAVE CLIENT CODE                             
*                                                                               
         USING NAMELD,R2                                                        
         L     R2,ADLVANAM         R2=A(CLIENT NAME)                            
         CLI   NAMEL,NAMELQ        X'20' - ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCLINM(0),NAMEREC  SAVE CLIENT NAME                             
*                                                                               
         USING PPRELD,R2                                                        
         L     R2,ADLVASUP         R2=A(PRODUCTION PROFILE ELEMENT)             
         MVC   SVOFF,PPRGAOFF      SAVE CLIENT OFFICE                           
         MVC   SVCOST,PPRCOST      COSTING ACCOUNT                              
PLEVAX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL B                                                     *         
***********************************************************************         
         SPACE 1                                                                
PLEVB    DS    0H                                                               
         MVC   SVPRD,SPACES        CLEAR SAVED AREA FOR CLIENT CODE             
         MVC   SVPRDNM,SPACES      CLEAR SAVED AREA FOR CLIENT NAME             
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRB          R2=A(LEVAL B ACCOUNT RECORD)                 
         MVC   SVPRD,ACTKACT+3     SAVE PRODUCT CODE                            
*                                                                               
         USING NAMELD,R2                                                        
         L     R2,ADLVBNAM         R2=A(PRODUCT NAME)                           
         CLI   NAMEL,NAMELQ        X'20' - ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVPRDNM(0),NAMEREC  SAVE CLIENT NAME                             
*                                                                               
         USING PPRELD,R2                                                        
         L     R2,ADLVBSUP         R2=A(PRODUCTION PROFILE ELEMENT)             
         CLC   PPRCOST,XSPACES     ANY COSTING?                                 
         BNH   *+10                                                             
         MVC   SVCOST,PPRCOST      COSTING ACCOUNT                              
         CLC   PPRGAOFF,SPACES     ANY OFFICE?                                  
         BNH   PLEVBX                                                           
         MVC   SVOFF,PPRGAOFF      SAVE PRODUCT OFFICE                          
*                                                                               
PLEVBX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   FCRDTRNS,C'Y'      READ TRANS FOR THIS ACCOUNT                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   FLAG,C'N'                                                        
         MVI   BALFLG,C'Y'         ACCOUNT BALANCED BY DEFAULT!                 
         MVI   MANDELF,C'Y'        JOB MANUAL DELETION FLAG!                    
         XC    SVDRAFT,SVDRAFT                                                  
         XC    SVJBADV,SVJBADV                                                  
         USING ACTRECD,R2                                                       
         L     R2,ADACC            R2=A(ACCOUNT RECORD)                         
         CLC   ACTKUNT(2),=C'SJ'   MAKE SURE WE ARE ONLY DOING SJ               
         BE    *+8                                                              
         MVI   FCRDTRNS,C'N'                                                    
*                                                                               
         MVC   SVJOB,ACTKACT+6     SAVE JOB NAME                                
*                                                                               
         MVC   SVJOBNM,SPACES      CLEAR NAME FIELD                             
         USING NAMELD,R2                                                        
         L     R2,ADACCNAM         R2=A(ACCOUNT NAME)                           
         CLI   NAMEL,NAMELQ        X'20' - ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVJOBNM(0),NAMEREC  SAVE JOB NAME                                
*                                                                               
PACC10   CLI   0(R2),0                                                          
         BE    PACCX                                                            
         CLI   0(R2),PPRELQ       X'24' - PRODUCTION PROFILE ELEMENT            
         BE    PACCGT24                                                         
         CLI   0(R2),JOBELQ       X'26' - PRODUCTION JOB ELEMENT                
         BE    PACCGT26                                                         
         CLI   0(R2),RSTELQ       X'30' - RECORD STATUS ELEMENT                 
         BE    PACCGT30                                                         
         CLI   0(R2),ABLELQ       X'32' - ACCOUNT BALANCE ELEMENT               
         BE    PACCGT32                                                         
         CLI   0(R2),APOELQ       X'33' - ACCOUNT PEEL-OFF ELEMENT              
         BE    PACCGT33                                                         
         CLI   0(R2),ASTELQ       X'31' - ACCOUNT STATUS ELEMENT                
         JE    PACCGT34                                                         
         CLI   0(R2),JCBELQ       X'CB' - JOB CLIENT BILLING ELEMENT            
         JE    PACCGT36                                                         
*                                                                               
PACC20   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     PACC10                                                           
*                                                                               
         USING PPRELD,R2                                                        
PACCGT24 DS    0H                                                               
         CLC   PPRCOST,XSPACES     ANY COSTING?                                 
         BNH   *+10                                                             
         MVC   SVCOST,PPRCOST      COSTING ACCOUNT                              
         B     PACC20                                                           
*                                                                               
         USING JOBELD,R2                                                        
PACCGT26 DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(20,SVCDTE)   CLOSING  DATE             
         GOTO1 (RF),(R1),(1,JOBODATE),(20,SVODTE)     OPENING  DATE             
         GOTO1 (RF),(R1),(1,JOBRDATE),(20,SVRDTE)     REVISION DATE             
         B     PACC20                                                           
*                                                                               
         USING RSTELD,R2                                                        
PACCGT30 DS    0H                                                               
         MVC   ACSTAT,RSTSTAT1                                                  
         B     PACC20                                                           
*                                                                               
         USING ABLELD,R2                                                        
PACCGT32 DS    0H                                                               
         ZAP   PACK8,ABLDR         DEBITS SINCE BBF                             
         SP    PACK8,ABLCR                                                      
         ZAP   PACK16,ABLFRWD      BALANCE BROUGHT FORWARD                      
         AP    PACK16,PACK8                                                     
         EDIT  PACK16,CURRBAL,2,FLOAT=$,MINUS=YES                               
*                                                                               
         CP    ABLCR,=P'0'                                                      
         JNE   *+14                                                             
         CP    ABLDR,=P'0'                                                      
         JE    PACC20              ACCOUNT BALANCED!                            
         MVI   BALFLG,C'N'         ACCOUNT NOT BALANCED!                        
         MVI   MANDELF,C'N'        CANNOT BE MANUALLY DELETED!                  
*                                                                               
         B     PACC20                                                           
*                                                                               
         USING APOELD,R2                                                        
PACCGT33 DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,APOPLDT),(20,SVPDTE)    PEELED  DATE              
         ZAP   PACK8,APODR         DEBITS B/F IN THIS PEEL                      
         SP    PACK8,APOCR                                                      
         EDIT  PACK8,PEELBAL,2,FLOAT=$,MINUS=YES                                
         B     PACC20                                                           
*                                                                               
         USING ASTELD,R2                                                        
PACCGT34 DS    0H                                                               
         MVC   SVDRAFT,ASTDRAFT                                                 
         J     PACC20                                                           
*                                                                               
         USING JCBELD,R2                                                        
PACCGT36 DS    0H                                                               
         MVC   SVJBADV,JCBADV                                                   
         J     PACC20                                                           
*                                                                               
PACCX    DS    0H                                                               
         CLI   QOPT4,C'Y'         SHOW ONLY CLOSED JOBS?                        
         JNE   PACCX1                                                           
                                                                                
         TM    ACSTAT,RSTSACIC    ACCOUNT CLOSED ?                              
         JZ    PACCX3             SKIP TRANS IF JOB NOT CLOSED!                 
*                                                                               
PACCX1   CLC   QSTART,SPACES      YEAR YY MAY BE FUNNY FOR 21ST CEN             
         JNH   PACCX2                                                           
         CLC   SVQSRT,SVODTE+2    QSTART IN YYMMDD FORMAT-NO FUNNY YRS          
         JH    PACCX3                                                           
*                                                                               
PACCX2   CLC   QEND,SPACES        YEAR YY MAY BE FUNNY FOR 21ST CEN             
         JNH   PACCX4                                                           
         CLC   SVQEND,SVODTE+2    QEND IN YYMMDD FORMAT-NO FUNNY YRS            
         JNL   PACCX4                                                           
PACCX3   MVI   FCRDTRNS,C'N'      SKIP TRANS FOR THIS ACCOUNT                   
         J     EXIT                                                             
*                                                                               
PACCX4   SR    RF,RF                                                            
         ICM   RF,7,SVDRAFT        SET RF=#DRAFT TRANSACTIONS                   
         SR    R1,R1               SET R1=#ADVANCE BILLS                        
         ICM   R1,3,SVJBADV                                                     
         CR    RF,R1                                                            
         JH    *+14                                                             
         XC    SVDRAFT,SVDRAFT     CLEAR DRAFT SINCE ALL ARE ADVANCES           
         J     *+8                                                              
         MVI   MANDELF,C'N'        CANNOT BE MANUALLY DELETED!                  
*                                                                               
         LAY   R0,CNTRS                                                         
         LHI   R1,RECMAX*CNTLQ                                                  
         XR    R3,R3                                                            
         MVCL  R0,R2              CLEAR CNTRS COUNTERS AREA                     
         MVC   SVCPY,RCCOMPFL     MOVE TO PROGRAM STORAGE                       
         GOTO1 =A(PTRN802)        PROCESS RECD TABLE AND GET COUNTS             
         GOTO1 =A(PEXP000)        PROCESS EXPENSE RECORD SEPARATELY             
*                                                                               
         CLI   QOPT3,C'Y'         SHOW RECORDS ELIGIBLE FOR DELETION?           
         JNE   PACCX10                                                          
         CLI   MANDELF,C'Y'       THIS ACC CAN BE MANUALLY DELETED?             
         JE    PACCX20                                                          
         MVI   FCRDTRNS,C'N'      SKIP TRANS FOR THIS ACCOUNT                   
         J     PACCX20                                                          
*                                                                               
PACCX10  CLI   QOPT3,C'N'         SHOW RECS NOT ELIGIBLE FOR DELETION?          
         JNE   PACCX20                                                          
         CLI   MANDELF,C'N'       THIS ACC CANNOT BE MANUALLY DELETED?          
         JE    PACCX20                                                          
         MVI   FCRDTRNS,C'N'      SKIP TRANS FOR THIS ACCOUNT                   
*                                                                               
PACCX20  CLI   QOPT1,C'Y'         SHOW DETAIL RECORDS FOR COUNTRS?              
         JNE   EXIT                                                             
         MVI   FLAG,C'Y'                                                        
*                                                                               
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTRA ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R2                                                       
PSUB     DS    0H                                                               
         L     R2,ADSUBAC          R2=A(CONTRA ACCOUNT RECORD)                  
*                                                                               
         MVC   SVWRKCD,CHDKWRK     SAVE WORK CODE TO PRINT LATER                
         MVC   SVSUBULA,CHDKULC    SAVE SUB ACCOUNT                             
         MVC   SVSUBANM,XSPACES    CLEAR CLIENT NAME                            
         DROP  R2                                                               
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   ACTKULA,SVSUBULA    CONTRA U/L/A                                 
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         CLC   SVKEY(ACTKEND),IOKEY                                             
         BNE   PSUBX                                                            
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         LA    R2,IO                                                            
*                                                                               
         USING NAMELD,R7                                                        
         LR    R7,R2               R5=A(IO)                                     
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVSUBANM(0),NAMEREC  SAVE CLIENT NAME                            
*                                                                               
PSUBX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
*         THIS ROUTINE USES R5 AND R6 TO POINT TO THE INFO FIELD      *         
*         IN THE PRINT LINE.  DO NOT USE R5 OR R6 EVER IN ANY RTE     *         
*         CONTAINED WITHIN THE PTRN ROUTINE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
PTRN     DS    0H                                                               
*                                                                               
         CLI   FLAG,C'Y'           SHOW DETAIL RECORDS FOR COUNTRS              
         JNE   PTRN00                                                           
         GOTO1 =A(PRPT000)         PRINT REPORT                                 
         MVI   FLAG,C'N'           SHOW ONCE FOR EACH ACCOUNT                   
PTRN00   EQU *                                                                  
*                                                                               
         L     R3,ADTRANS          R3=A(TRANSACTION ELEMENT)                    
         LR    R2,R3                                                            
         SH    R2,DATADISP         R2=A(TRANSACTION RECORD)                     
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVC   XP,XSPACES                                                       
*                                                                               
         XC    SV60EL,SV60EL                                                    
*                                                                               
         USING TRSELD,R7                                                        
         LR    R7,R3                                                            
         MVI   ELCODE,TRSELQ       X'60' - TRANSACTION STATUS ELEMNT            
         BAS   RE,NEXTEL                                                        
         BNE   PTRN05                                                           
         ST    R7,SV60EL                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,TRSDATE),(X'20',SVADTE) ACTIVITY DATE             
*                                                                               
PTRN05   CLC   SVWRKCD,XSPACES     ANY WORK CODE?                               
         BE    PTRN10                                                           
*                                                                               
         MVC   PTITLE,=CL15'WORK CODE   :'                                      
         MVC   PWRKCD,SVWRKCD      MOVE WORK CODE TO PRINT LINE                 
         GOTO1 ACREPORT                                                         
         MVC   PTITLE,=CL15'CONTRA ACCT :'                                      
         MVC   PSUBULA,SVSUBULA    PRINT IT                                     
         MVC   PSUBANM,SVSUBANM                                                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   XP(50),HEADER                                                    
         GOTO1 UNDERLIN,DMCB,(50,XP+2),XPSECOND+2                               
         GOTO1 ACREPORT                                                         
         MVC   SVWRKCD(SVSUBLNQ),XSPACES                                        
*                                                                               
PTRN10   GOTO1 DATCON,DMCB,(1,TRNDATE),(X'20',PTRNDTE) TRNS DATE                
         MVC   PREF,TRNREF         REFERENCE                                    
         EDIT  TRNTYPE,PTYP                                                     
         EDIT  TRNAMNT,PAMNT,2,FLOAT=$,MINUS=YES                                
*                                                                               
         CLI   QOPT2,C'Y'          CLIENT REQUEST - ONLY POINTERS               
         BNE   PTRN500                                                          
*                                                                               
         MVC   PELNM,=C'TRNELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,TRNSTAT        SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,TRNSTTAB,TRNSTQ  GET STAT BITS IN INFO LINE         
*                                                                               
         CLI   TRNLN,TRNLN2Q       DO WE HAVE BILLABLE INFO?                    
         BL    PTRN30                                                           
         CLC   TRNANAL,=C'99'      IF NOT 99 SKIP                               
         BNE   PTRN30                                                           
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(L'TRNBLTYP),TRNBLTYP  BILLING TYPE DESCRIPTION             
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
*                                                                               
         TM    TRNBLCOM+5,X'0C'    PATCH FOR BAD RECORDS                        
         BZ    PTRN15                                                           
*                                                                               
         CP    TRNBLCOM,=P'0'                                                   
         BE    PTRN15                                                           
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(6),=C'BLCOM='                                              
         EDIT  TRNBLCOM,(10,SVFLD+6),2,FLOAT=$,MINUS=YES                        
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN15   CP    TRNBLCD,=P'0'                                                    
         BE    PTRN20                                                           
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(5),=C'BLCD='                                               
         EDIT  TRNBLCD,(10,SVFLD+5),2,FLOAT=$,MINUS=YES,ALIGN=LEFT              
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN20   CP    TRNBLPAY,=P'0'                                                   
         BE    PTRN30                                                           
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(6),=C'BLPAY='                                              
         EDIT  TRNBLPAY,(10,SVFLD+6),2,FLOAT=$,MINUS=YES,ALIGN=LEFT             
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
         USING TRSELD,R7                                                        
PTRN30   ICM   R7,15,SV60EL        RESTORE TRSEL (ACTIVITY ELEMENT)             
         BZ    PTRN60                                                           
         MVC   PTRSDTE,SVADTE      ACTIVITY DATE                                
         EDIT  TRSBSEQ,PSEQ,ZERO=NOBLANK                                        
         CLC   PINFO,XSPACES       DONT PRINT UNLESS IT HAS DATA                
         BNH   PTRN40                                                           
         GOTO1 ACREPORT                                                         
PTRN40   MVC   PDETL,XSPACES                                                    
*                                                                               
         MVC   PELNM,=C'TRSELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,TRSSTAT        SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,TRSSTTAB,TRSSTQ  GET STAT BITS IN INFO LINE         
*                                                                               
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,TRSSTAT2       SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,TRSS2TAB,TRSS2Q  GET STAT BITS IN INFO LINE         
*                                                                               
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,TRSSTAT3       SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,TRSS3TAB,TRSS3Q  GET STAT BITS IN INFO LINE         
*                                                                               
         CLC   PINFO,XSPACES     DONT PRINT UNLESS IT HAS DATA                  
         BNH   PTRN50                                                           
         GOTO1 ACREPORT                                                         
PTRN50   MVC   PDETL,XSPACES                                                    
*                                                                               
PTRN60   DS    0H                                                               
         USING PTAELD,R7                                                        
         LR    R7,R3                                                            
         MVI   ELCODE,PTAELQ       X'77' - PROD TRANSACTION ACTIVITY            
*                                                                               
PTRN70   BAS   RE,NEXTEL                                                        
         BNE   PTRN300                                                          
*                                                                               
         MVC   PELNM,=C'PTAELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(4),=C'A/D='                                                
         GOTO1 DATCON,DMCB,(2,PTADATE),(X'20',SVFLD+4) ACTIVITY DATE            
         GOTO1 MOVEIT,DMCB,SVFLD,10                                             
*                                                                               
         OC    PTAPERS,PTAPERS     ANY PERSON CODE?                             
         BZ    PTRN80                                                           
         MVC   SVFLD(4),=C'PER='                                                
         EDIT  PTAPERS,(5,SVFLD+4),ALIGN=LEFT    PERSON CODE                    
         GOTO1 MOVEIT,DMCB,SVFLD,9                                              
*                                                                               
PTRN80   TM    PTATYPE,PTATREVS    IS IT A TRANSFER/WRITE-OFF REVERSAL          
         BNO   PTRN90                                                           
         MVC   SVFLD(17),=C'TRAN-W/O REV POST'                                  
         GOTO1 MOVEIT,DMCB,SVFLD,17                                             
*                                                                               
PTRN90   BAS   RE,GETTYP           GET PTA TYPE                                 
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,PTASTAT1       SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,PTAST1TB,PTAST1Q   GET STAT BITS INFO LINE          
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   BYTE,PTASTAT2       SAVE STAT FOR GETSTAT RTE                    
         GOTO1 GETSTAT,DMCB,PTAST2TB,PTAST2Q   GET STAT BITS INFO LINE          
*                                                                               
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         TM    PTASTAT2,PTASWRUP   IS IT NET OR WRITEUP AMNT?                   
         BO    PTRN100                                                          
         MVC   SVFLD(7),=C'PTANET='                                             
         EDIT  PTANET,(13,SVFLD+7),2,MINUS=YES,FLOAT=$                          
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(4),=C'SEQ='                                                
         EDIT  PTASEQN,(5,SVFLD+4),ZERO=NOBLANK                                 
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         B     PTRN120                                                          
*                                                                               
PTRN100  CP    PTAWUAMT,=P'0'                                                   
         BE    PTRN110                                                          
         MVC   SVFLD(7),=C'PTAWUA='                                             
         EDIT  PTAWUAMT,(13,SVFLD+8),2,MINUS=YES,FLOAT=$                        
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN110  OC    PTAWUHRS,PTAWUHRS   ANY HOURS?                                   
         BZ    PTRN120                                                          
         MVC   SVFLD(7),=C'PTAHRS='                                             
         EDIT  PTAWUHRS,(5,SVFLD+8),ZERO=NOBLANK                                
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN120  DS    0H                                                               
         GOTO1 MOVEIT,DMCB,PTACUR,L'PTACUR                                      
         CLC   PTAOFFC,XSPACES                                                  
         BNH   PTRN130                                                          
         MVC   SVFLD(4),=C'OFF='                                                
         MVC   SVFLD+4(2),PTAOFFC                                               
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN130  CLC   PTAMOA,XSPACES                                                   
         BNH   PTRN140                                                          
         MVC   SVFLD(4),=C'MOA='                                                
         MVC   WORK(2),PTAMOA                                                   
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(X'20',SVFLD+4)   MOA                       
         GOTO1 MOVEIT,DMCB,SVFLD,8   DONT INCLUDE DAYS                          
PTRN140  CP    PTACDSC,=P'0'       ANY CASH DISCOUNTS?                          
         BE    PTRN150                                                          
         MVC   SVFLD(7),=C'CSHDIS='                                             
         EDIT  PTACDSC,(13,SVFLD+7),2,MINUS=YES,FLOAT=$                         
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN150  OC    PTAHOURS,PTAHOURS   ANY HOURS?                                   
         BZ    PTRN160                                                          
         MVC   SVFLD(6),=C'HOURS='                                              
         EDIT  PTAHOURS,(5,SVFLD+6),ZERO=NOBLANK                                
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN160  CLI   PTALN,PTALN1Q       DO WE HAVE ANY MORE INFO?                    
         BNH   PTRN70                                                           
         CLI   PTALN,PTARLN1Q      REGULAR ALLOCATION VALUES                    
         BE    PTRN170                                                          
         CLI   PTALN,PTARLN2Q      FOREIGN CURRENCY BILLING                     
         BE    PTRN250                                                          
         CLI   PTALN,PTATLN2Q      TRANSFER TO VALUES                           
         BE    PTRN260                                                          
         CLI   PTALN,PTAFLN1Q      TRANSFER FROM VALUES                         
         BE    PTRN270                                                          
         CLI   PTALN,PTAWLN1Q      WRITE OFF VALUES                             
         BE    PTRN280                                                          
*                                                                               
* REGULAR ALLOCATION VALUES                                                     
*                                                                               
PTRN170  CLI   PTATYPE,PTATWOF     W/O AND ALLOCATE HAS SAME LEN-JUST           
         BE    PTRN280             MAKE SURE YOU ARE IN THE RIGHT RTE           
         CLI   PTATYPE,PTATWOFR                                                 
         BE    PTRN280                                                          
         MVC   SVFLD(8),=C'COM-RTE='                                            
         EDIT  PTARCORT,(13,SVFLD+8),2,MINUS=YES,FLOAT=$                        
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(4),=C'COM='                                                
         EDIT  PTARCOM,(13,SVFLD+4),2,MINUS=YES,FLOAT=$                         
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(10),=C'BLLNG-DTE='                                         
         GOTO1 DATCON,DMCB,(2,PTARDATE),(X'20',SVFLD+10) BILLING DATE           
         GOTO1 MOVEIT,DMCB,SVFLD,16                                             
*                                                                               
         OC    PTARPERS,PTARPERS     ANY PERSON CODE?                           
         BZ    PTRN180                                                          
         MVC   SVFLD(4),=C'PER='                                                
         EDIT  PTARPERS,(5,SVFLD+4)                PERSON CODE                  
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN180  MVC   SVFLD(6),=C'BILL#='                                              
         MVC   SVFLD+6(L'PTARBLNO),PTARBLNO                                     
         GOTO1 MOVEIT,DMCB,SVFLD,6+L'PTARBLNO                                   
         MVC   SVFLD(9),=C'BILL-DTE='                                           
         GOTO1 DATCON,DMCB,(2,PTARDATE),(X'20',SVFLD+9) BILL DATE               
         GOTO1 MOVEIT,DMCB,SVFLD,15                                             
         OC    PTARCODE,PTARCODE   ANY FORMAT TO CODE                           
         BZ    PTRN190                                                          
         MVC   SVFLD(7),=C'FRM-TO='                                             
         EDIT  PTARCODE,(3,SVFLD+7),2,MINUS=YES,FLOAT=$                         
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN190  OC    PTARFORM,PTARFORM   ANY FORMAT/GROUP LEVEL?                      
         BZ    PTRN200                                                          
         MVC   SVFLD(8),=C'FRM/GRP='                                            
         MVC   SVFLD+8(L'PTARFORM),PTARFORM                                     
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN200  OC    PTARTYPE,PTARTYPE   ANY WORKCODE TYPE?                           
         BZ    PTRN210                                                          
         MVC   SVFLD(8),=C'W/C-TYP='                                            
         MVC   SVFLD+8(L'PTARTYPE),PTARTYPE                                     
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN210  OC    PTARGSTC,PTARGSTC   ANY GST CODE?                                
         BZ    PTRN220                                                          
         MVC   SVFLD(4),=C'GST='                                                
         MVC   SVFLD+4(L'PTARGSTC),PTARGSTC                                     
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN220  OC    PTARPSTC,PTARPSTC   ANY PST CODE?                                
         BZ    PTRN230                                                          
         MVC   SVFLD(4),=C'PST='                                                
         MVC   SVFLD+4(L'PTARPSTC),PTARPSTC                                     
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN230  OC    PTARPRVC,PTARPRVC   ANY PROVINCE CODE?                           
         BZ    PTRN240                                                          
         MVC   SVFLD(4),=C'PRV='                                                
         EDIT  PTARPRVC,(5,SVFLD+4),ALIGN=LEFT     PROVINCE CODE                
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
PTRN240  OC    PTARORGB,PTARORGB   ANY ORIGINAL BILL INFO?                      
         BZ    PTRN290                                                          
         MVC   SVFLD(10),=C'ORG-BILL#='                                         
         MVC   SVFLD+10(L'PTARORGB),PTARORGB                                    
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(12),=C'ORG-BILL-DT='                                       
         MVC   SVFLD+12(L'PTARORGD),PTARORGD                                    
         GOTO1 DATCON,DMCB,(2,PTARORGD),(X'20',SVFLD+12) ORG BILL DATE          
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         B     PTRN290                                                          
*                                                                               
* FOREIGN CURRENCY BILLING                                                      
*                                                                               
PTRN250  BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD(5),=C'FNET='                                               
         EDIT  PTANETF,(13,SVFLD+5),2,MINUS=YES,FLOAT=$                         
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(5),=C'FCOM='                                               
         EDIT  PTARFCOM,(13,SVFLD+5),2,MINUS=YES,FLOAT=$                        
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         B     PTRN290                                                          
*                                                                               
* TRANSFER TO VALUES                                                            
*                                                                               
PTRN260  BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD(7),=C'TO JOB='                                             
         MVC   SVFLD+7(L'PTATJOB),PTATJOB       TRANSFER TO JOB                 
         GOTO1 MOVEIT,DMCB,SVFLD,19                                             
         MVC   SVFLD(6),=C'TO WC='                                              
         MVC   SVFLD+6(L'PTATWRK),PTATWRK       TRANSFER TO W/C                 
         GOTO1 MOVEIT,DMCB,SVFLD,8                                              
         MVC   SVFLD(3),=C'SK='                                                 
         MVC   SVFLD+3(L'PTATSKAC),PTATSKAC     OVERRIDE SK ACCOUNT             
         GOTO1 MOVEIT,DMCB,SVFLD,15                                             
         B     PTRN290                                                          
*                                                                               
* TRANSFER FROM VALUES                                                          
*                                                                               
PTRN270  BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD(7),=C'FR JOB='                                             
         MVC   SVFLD+7(L'PTAFJOB),PTAFJOB       TRANSFER FROM JOB               
         GOTO1 MOVEIT,DMCB,SVFLD,19                                             
         MVC   SVFLD(6),=C'FR WC='                                              
         MVC   SVFLD+6(L'PTAFWRK),PTAFWRK       TRANSFER FROM W/C               
         GOTO1 MOVEIT,DMCB,SVFLD,8                                              
         B     PTRN290                                                          
*                                                                               
* WRITE-OFF VALUES                                                              
*                                                                               
PTRN280  BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD(4),=C'REF='                                                
         MVC   SVFLD+4(L'PTAWREF),PTAWREF       TRANSACTION REFERENCE           
         GOTO1 MOVEIT,DMCB,SVFLD,10                                             
         MVC   SVFLD(5),=C'DATE='                                               
         GOTO1 DATCON,DMCB,(1,WORK),(X'20',SVFLD+5)   MOA                       
         GOTO1 MOVEIT,DMCB,SVFLD,11                                             
         MVC   SVFLD(4),=C'EXP='                                                
         MVC   SVFLD+4(L'PTAWEXPA),PTAWEXPA    EXPENSE ACCOUNT                  
         GOTO1 MOVEIT,DMCB,SVFLD,18                                             
         MVC   SVFLD(8),=C'W/O TYP='                                            
         MVC   SVFLD+8(L'PTAWWOT),PTAWWOT      WRITE OFF TYPE                   
         GOTO1 MOVEIT,DMCB,SVFLD,18                                             
         CLI   PTALN,PTAWLN1Q          DO WE ALSO HAVE W/O ACCOUNT?             
         BE    PTRN290                    NO - PRINT AND CHECK FOR NEXT         
         MVC   SVFLD(8),=C'W/O ACC='                                            
         MVC   SVFLD+8(L'PTAWWOFA),PTAWWOFA    WRITE OFF ACCOUNT                
         GOTO1 MOVEIT,DMCB,SVFLD,22                                             
*                                                                               
PTRN290  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         MVC   PDETL,XSPACES                                                    
         B     PTRN70                                                           
*                                                                               
         USING PXDELD,R7                                                        
PTRN300  LR    R7,R3                                                            
         MVI   ELCODE,PXDELQ       X'4E' - TRANSFER DETAIL ELEMENT              
*                                                                               
PTRN320  BAS   RE,NEXTEL                                                        
         BNE   PTRN340                                                          
         MVC   PELNM,=C'PXDELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(7),=C'TO JOB='                                             
         CLI   PXDTYPE,PXDTTO                                                   
         BE    PTRN330                                                          
         MVC   SVFLD(8),=C'ORG JOB='                                            
         CLI   PXDTYPE,PXDTORG                                                  
         BE    *+10                                                             
         MVC   SVFLD(9),=C'FROM JOB='                                           
*                                                                               
PTRN330  MVC   SVFLD+9(12),PXDFRTOA                                             
         CLI   PXDLN,PXDLNQ                                                     
         BL    PTRN335                                                          
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(3),=C'WC='                                                 
         MVC   SVFLD+3(2),PXDFRTOW                                              
         CLI   PXDLN,PXDLN2Q                                                    
         BL    PTRN335                                                          
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(4),=C'BAT='                                                
         MVC   SVFLD+4(6),PXDOBAT                                               
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(4),=C'SEQ='                                                
         EDIT  PXDOSEQ,(2,SVFLD+4),FILL=0                                       
*                                                                               
PTRN335  BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         GOTO1 ACREPORT                                                         
         MVC   PDETL,XSPACES                                                    
         B     PTRN320                                                          
*                                                                               
         USING PAKELD,R7                                                        
PTRN340  LR    R7,R3                                                            
         MVI   ELCODE,PAKELQ       X'D4' - PAYABLE KEY ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BNE   PTRN360                                                          
*                                                                               
         MVC   PELNM,=C'PAKELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(16),=C'PAYABLE ACCOUNT='                                   
         MVC   SVFLD+16(14),PAKACC+1                                            
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(12),=C'OFFICE CODE='                                       
         MVC   SVFLD+12(2),PAKOFF                                               
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         MVC   SVFLD(15),=C'CONTRA ACCOUNT='                                    
         MVC   SVFLD+15(14),PAKCON+1                                            
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         GOTO1 ACREPORT                                                         
         MVC   PDETL,XSPACES                                                    
*                                                                               
         USING CPJELD,R7                                                        
PTRN360  LR    R7,R3                                                            
         MVI   ELCODE,CPJELQ       X'4F' - CLIENT/PRODUCT/JOB ELEMENT           
*                                                                               
PTRN380  BAS   RE,NEXTEL                                                        
         BNE   PTRN400                                                          
         MVC   PELNM,=C'CPJELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
*                                                                               
         CLI   CPJTYPE,CPJTREC     RECEIVABLE ONLY                              
         BNE   PTRN380                                                          
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(19),=C'RECEIVABLE ACCOUNT='                                
         MVC   SVFLD+19(14),CPJRUNT                                             
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
         GOTO1 ACREPORT                                                         
         MVC   PDETL,XSPACES                                                    
*                                                                               
         USING BSCELD,R7                                                        
PTRN400  LR    R7,R3                                                            
         MVI   ELCODE,BSCELQ       X'E3' - BILLING SOURCE ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   PTRN410                                                          
         MVC   PELNM,=C'BSCELD-'                                                
         BAS   RE,FNDNXT           FIND NEXT POSITION IN INFO LINE              
*                                                                               
         MVC   SVFLD,XSPACES                                                    
         MVC   SVFLD(15),=C'BILLING SOURCE='                                    
         MVC   SVFLD+15(12),BSCBSRC                                             
         MVC   SVFLD+27(7),=C'OFFICE='                                          
         MVC   SVFLD+34(2),BSCBOFF                                              
         BAS   RE,SQSVFLD          SQUASH AND MOVE SVFLD TO INFO LINE           
*                                                                               
PTRN410  GOTO1 ACREPORT                                                         
*                                                                               
PTRN500  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*        MVC   SVIOKEY,0(R2)       SAVE OFF SEQUENCE KEY                        
*                                                                               
*TRNXIT  GOTO1 =A(DMREADAC),DMCB,(RC)   RESTORE READ SEQUENCE                   
*        GOTO1 ACREPORT                                                         
PTRNXIT  B     EXIT                                                             
         DROP  R2,R3,R4,R7                                                      
         EJECT                                                                  
**********************************************************************          
* SUB ACCOUNT LAST                                                   *          
**********************************************************************          
         SPACE 1                                                                
SUBL     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
SUBLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SQUASH INFO IN SVFLD AND MOVE IT TO INFO LINE                       *         
***********************************************************************         
         SPACE 1                                                                
SQSVFLD  NTR1                                                                   
         GOTO1 SQUASHER,DMCB,SVFLD,L'SVFLD                                      
         L     R2,4(R1)                                                         
         GOTO1 MOVEIT,DMCB,SVFLD,(R2)                                           
         MVC   SVFLD,XSPACES                                                    
         XIT1  REGS=(R5,R6)        SAVE R5 AND R6                               
         EJECT                                                                  
***********************************************************************         
* FIND NEXT AVAILABLE POSITION IN THE INFO LINE                       *         
*      ON EXIT - R5 IS POSITION AND R6 IS REMAINING SPACES            *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R4                                                        
FNDNXT   NTR1                                                                   
         LA    R4,XP               R4=PRINT LINE                                
         LA    R5,PINFO            POINT R1 TO INFO SEG                         
         LA    R6,L'PINFO          LENGTH OF INFO LINE                          
         CLC   0(5,R5),XSPACES                                                  
         BE    FNDNX                                                            
         LA    R5,1(R5)                                                         
         BCT   R6,*-14                                                          
         GOTO1 ACREPORT                                                         
         LA    R5,PINFO            POINT R1 TO INFO SEG                         
         LA    R6,L'PINFO          LENGTH OF INFO LINE                          
         MVC   XP,XSPACES                                                       
*                                                                               
FNDNX    XIT1  REGS=(R5,R6)        SAVE R5 AND R6                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET STAT BITS INTO INFO LINE                                        *         
*     ON ENTRY-R2 POINTS TO STATUS TABLE                              *         
*              R5 IS FIRST AVAILABLE EMPTY SPACE IN INFO LINE         *         
*              R6 IS REMAINING SPACES IN INFO LINE                    *         
*              BYTE IS TRANSACTION STATUS BYTE                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNSTD,R2                                                        
         USING PLINED,R4                                                        
GETSTAT  NTR1                                                                   
         LA    R4,XP               R4=PRINT LINE                                
         L     R2,0(R1)            POINT R2 TO STATUS TABLE                     
         L     R0,4(R1)            PUT LENGTH OF INFO LINE IN R0                
GETS10   MVC   SVBYTE,BYTE                                                      
         NC    SVBYTE,TRNSTBIT                                                  
         BZ    GETS30                                                           
*                                                                               
         SR    R3,R3                                                            
         IC    R3,TRNSTLN                                                       
         CR    R3,R6               DO WE HAVE ENOUGH ROOM TO INSERT             
         BNH   GETS20                                                           
         GOTO1 ACREPORT                                                         
         LA    R5,PINFO            POINT R1 TO INFO SEG                         
         LA    R6,L'PINFO          LENGTH OF INFO LINE                          
         MVC   XP,XSPACES                                                       
*                                                                               
GETS20   BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),TRNSTDSC                                                 
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R5,R3                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)            ADD 1 FOR THE COMMA                          
         SR    R6,R3                                                            
         BZ    *+12                                                             
*                                                                               
GETS30   LA    R2,TRNSTLNQ(R2)                                                  
         BCT   R0,GETS10                                                        
*                                                                               
GETSX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET STAT BITS INTO STATUS PRINT FIELD                               *         
*     ON ENTRY  - 0(R1) POINTS TO STATUS TABLE                        *         
*                 4(R1) POINTS TO STATUS BYTE                         *         
***********************************************************************         
         SPACE 1                                                                
GTASTAT  NTR1                                                                   
         L     R2,0(R1)            POINT R2 TO STATUS TABLE                     
         L     R3,4(R1)            POINT R3 TO STATUS BYTE                      
         MVC   APPSTAT,0(R3)                                                    
*                                                                               
         MVI   SVBYTE,0                                                         
         LA    R1,WORK                                                          
         MVC   WORK,SPACES                                                      
*        LA    RE,STATTAB                                                       
         LA    RE,0(R2)                                                         
         LA    RF,0                                                             
GTAS010  CLI   0(RE),EOF                                                        
         JE    GTASX                                                            
         MVC   BYTE,APPSTAT        SAVE OFF STATUS BYTE                         
         CLI   BYTE,0                                                           
         JNE   *+8                                                              
         MVI   BYTE,X'04'                                                       
         NC    BYTE,0(RE)          IS THE BYTE SET?                             
         JZ    GTAS020                                                          
         TM    SVBYTE,X'80'        DID WE ADD TO THE LINE ALREADY?              
         JNO   GTAS015                                                          
         CLI   0(R1),X'40'         ARE WE AT A SIGNIFICANT CHARACTER?           
         JH    *+12                                                             
         AHI   R1,-1                                                            
         J     *-12                                                             
         MVI   1(R1),C'/'          SEPARATE THE TWO STATUS                      
         LA    R1,2(R1)                                                         
GTAS015  MVC   0(14,R1),1(RE)                                                   
         OI    SVBYTE,X'80'                                                     
         LA    R1,15(R1)                                                        
         AHI   RF,1                                                             
         CHI   RF,4                MAX 4 STATUS                                 
         JL    GTAS020                                                          
         MVI   0(R1),C'*'                                                       
         J     GTASX                                                            
GTAS020  LA    RE,L'STATTAB(RE)                                                 
         J     GTAS010                                                          
*                                                                               
GTASX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* MOVE DATA FROM SVFLD TO PINFO IN PRINT LINE                         *         
*      ON ENTRY-PARM1 - LENGTH OF CURRENT ENTRY                       *         
*               PARM2 - ADDRESS OF FIELD TO BE MOVED INTO INFO LINE   *         
*               R5 IS FIRST AVAILABLE EMPTY SPACE IN INFO LINE        *         
*               R6 IS REMAINING SPACES IN INFO LINE                   *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R4                                                        
MOVEIT   NTR1                                                                   
         LA    R4,XP               R4=PRINT LINE                                
         L     R2,0(R1)            R2=A(ENTRY TO BE MOVED)                      
         L     R3,4(R1)            R3=LENGTH OF ENTRY TO BE MOVED               
         CR    R3,R6               DO WE HAVE ENOUGH ROOM TO INSERT             
         BNH   MVIT10                                                           
         GOTO1 ACREPORT                                                         
         LA    R5,PINFO            POINT R1 TO INFO SEG                         
         LA    R6,L'PINFO          LENGTH OF INFO LINE                          
         MVC   XP,XSPACES                                                       
*                                                                               
MVIT10   BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R2)                                                    
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R5,R3                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)            ADD 1 FOR THE COMMA                          
         SR    R6,R3                                                            
*                                                                               
MVITX    MVC   SVFLD,XSPACES                                                    
         XIT1  REGS=(R5,R6)                                                     
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET PTA TYPE                                                        *         
* ON ENTRY - R7 POINTS TO PTAEL                                       *         
*            R4 POINTS TO PRINT LINE                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R4                                                        
         USING PTAELD,R7                                                        
GETTYP   NTR1                                                                   
         MVC   SVFLD,XSPACES                                                    
         USING TRNSTD,R2                                                        
         LA    R2,PTATYTAB                                                      
         LA    R0,PTATYQ                                                        
GETT10   CLC   PTATYPE,0(R2)                                                    
         BNE   GETT30                                                           
         SR    R3,R3                                                            
         IC    R3,TRNSTLN                                                       
         CR    R3,R6               DO WE HAVE ENOUGH ROOM TO INSERT             
         BNH   GETT20                                                           
         GOTO1 ACREPORT                                                         
         LA    R5,PINFO            POINT R1 TO INFO SEG                         
         LA    R6,L'PINFO          LENGTH OF INFO LINE                          
         MVC   XP,XSPACES                                                       
*                                                                               
GETT20   BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),TRNSTDSC                                                 
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R5,R3                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)            ADD 1 FOR THE COMMA                          
         SR    R6,R3                                                            
         BZ    *+12                                                             
GETT30   LA    R2,TRNSTLNQ(R2)                                                  
         BCT   R0,GETT10                                                        
*                                                                               
GETTX    DS    0H                                                               
         XIT1  REGS=(R5,R6)                                                     
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R7,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R7,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
*                                                                               
HEADER   DC    CL50'  T-DTE   A-DTE   REF     SEQ  TYP   AMOUNT   INFO'         
PTHEADER DC    CL50'APPLICATION  1R ACCOUNT      END DATE    STATUS   '         
PEHEADER DC    CL50'APPLICATION  CLAIM#  PID       CLAIM DATE  STATUS '         
POHEADER DC    CL50'APPLICATION  NUMBER  STATUS                       '         
PESTHEDR DC    CL50'APPLICATION  NUMBER                               '         
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(BINADD)           BINSEARCH ROUTINE                            
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(UNDERLIN)         UNDERLINE HEADER                             
         DC    V(SQUASHER)         SQUASH INFO LINE                             
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
BTYPTAB  DS    0CL16                                                            
         DC    AL1(TRNBTALL),CL15'ALLOCATED'                                    
         DC    AL1(TRNBTCLI),CL15'CLIENT'                                       
         DC    AL1(TRNBTMAN),CL15'MANUAL'                                       
         DC    AL1(TRNBTONE),CL15'ONE-TIME'                                     
         DC    AL1(TRNBTPER),CL15'% OF ESTIMATE'                                
         DC    AL1(TRNBTPRO),CL15'PROGRESSIVE'                                  
         DC    AL1(TRNBTSPE),CL15'SPECIAL'                                      
         DC    AL1(TRNBTTOT),CL15'TOTAL'                                        
         DC    AL1(EOF)                                                         
*                                                                               
TRNSTTAB DS    0C                                                               
         DC    XL1'80',XL1'02',CL10'DR'                                         
         DC    XL1'40',XL1'08',CL10'URG-TRNS'                                   
         DC    XL1'20',XL1'03',CL10'REV'                                        
         DC    XL1'08',XL1'05',CL10'CAN$'                                       
         DC    XL1'04',XL1'09',CL10'TAL-B/PAY'                                  
         DC    XL1'02',XL1'09',CL10'SEL-4 PAY'                                  
         DC    XL1'01',XL1'07',CL10'NON-COM'                                    
TRNSTQ   EQU   (*-TRNSTTAB)/TRNSTLNQ                                            
*                                                                               
TRSSTTAB DS    0C                                                               
         DC    XL1'80',XL1'0A',CL10'UPD-TO-G/L'                                 
         DC    XL1'40',XL1'0A',CL10'O+M-CONVRS'                                 
         DC    XL1'20',XL1'03',CL10'G/L'                                        
         DC    XL1'10',XL1'06',CL10'PEELED'                                     
         DC    XL1'08',XL1'07',CL10'OFF-PAY'                                    
         DC    XL1'04',XL1'09',CL10'PD-BY-CHK'                                  
         DC    XL1'02',XL1'08',CL10'REV-TRNS'                                   
         DC    XL1'01',XL1'0A',CL10'P/C-VOIDED'                                 
TRSSTQ   EQU   (*-TRSSTTAB)/TRNSTLNQ                                            
*                                                                               
TRSS2TAB DS    0C                                                               
         DC    XL1'80',XL1'09',CL10'PAID-AC5P'                                  
         DC    XL1'40',XL1'08',CL10'ANAL-PAY'                                   
         DC    XL1'20',XL1'07',CL10'ACCRUAL'                                    
         DC    XL1'10',XL1'09',CL10'ACCRL-REV'                                  
         DC    XL1'08',XL1'07',CL10'T/S-ADJ'                                    
         DC    XL1'04',XL1'09',CL10'T/S-MISSG'                                  
         DC    XL1'02',XL1'09',CL10'PAID-MARK'                                  
         DC    XL1'01',XL1'07',CL10'T/S-REG'                                    
TRSS2Q   EQU   (*-TRSSTTAB)/TRNSTLNQ                                            
*                                                                               
TRSS3TAB DS    0C                                                               
         DC    XL1'80',XL1'09',CL10'XJOB-POST'                                  
         DC    XL1'40',XL1'08',CL10'COKE-EXP'                                   
         DC    XL1'20',XL1'09',CL10'NOT-ON-21'                                  
         DC    XL1'0F',XL1'09',CL10'PROG/MASK'                                  
         DC    XL1'01',XL1'05',CL10'BATCH'                                      
         DC    XL1'02',XL1'05',CL10'CLOSE'                                      
         DC    XL1'03',XL1'07',CL10'DEBTORS'                                    
         DC    XL1'04',XL1'03',CL10'WFM'                                        
         DC    XL1'05',XL1'0A',CL10'MARK(CR/M)'                                 
         DC    XL1'06',XL1'0A',CL10'MARK(BK/V)'                                 
         DC    XL1'07',XL1'0A',CL10'MARK(MD/R)'                                 
         DC    XL1'08',XL1'0A',CL10'MARK(CR/B)'                                 
TRSS3Q   EQU   (*-TRSSTTAB)/TRNSTLNQ                                            
*                                                                               
PTATYTAB DS    0C                                                               
         DC    XL1'01',XL1'0A',CL10'AL.TO-BILL'                                 
         DC    XL1'02',XL1'0A',CL10'TRANS-FROM'                                 
         DC    XL1'03',XL1'08',CL10'TRANS-TO'                                   
         DC    XL1'04',XL1'09',CL10'WRITE-OFF'                                  
         DC    XL1'05',XL1'07',CL10'W/O-REC'                                    
PTATYQ   EQU   (*-PTATYTAB)/TRNSTLNQ                                            
*                                                                               
PTAST1TB DS    0C                                                               
         DC    XL1'80',XL1'07',CL10'PENDING'                                    
         DC    XL1'40',XL1'0A',CL10'COMM-OVERR'                                 
         DC    XL1'20',XL1'09',CL10'AL-BY-HRS'                                  
         DC    XL1'10',XL1'0A',CL10'AL-BY-CASH'                                 
         DC    XL1'08',XL1'0A',CL10'REVS-ALLOC'                                 
         DC    XL1'04',XL1'08',CL10'REVERSED'                                   
         DC    XL1'02',XL1'08',CL10'REVS-UPD'                                   
         DC    XL1'01',XL1'0A',CL10'UPD-OFFLNE'                                 
PTAST1Q  EQU   (*-PTAST1TB)/TRNSTLNQ                                            
*                                                                               
PTAST2TB DS    0C                                                               
         DC    XL1'80',XL1'0A',CL10'TRNS-COMMS'                                 
         DC    XL1'40',XL1'0A',CL10'COMM-AS-NT'                                 
         DC    XL1'20',XL1'0A',CL10'TRNS-ALLOC'                                 
         DC    XL1'10',XL1'0A',CL10'AUTO-REV-L'                                 
         DC    XL1'08',XL1'09',CL10'W-UP-AMNT'                                  
         DC    XL1'04',XL1'0A',CL10'TOTAL-BILL'                                 
         DC    XL1'02',XL1'0A',CL10'2D-RET-PST'                                 
PTAST2Q  EQU   (*-PTAST2TB)/TRNSTLNQ                                            
*                                                                               
STATTAB  DS    0CL15                                                            
         DC    AL1(4),CL14'IN PROGRESS'                                         
         DC    AL1(TIMSFAPP),CL14'FULLY APPROVED'                               
         DC    AL1(TIMSSUBM),CL14'SUBMITTED'                                    
         DC    AL1(TIMSPAPP),CL14'PART APPROVED'                                
         DC    AL1(TIMSAWAP),CL14'AWAIT LINE MAN'                               
         DC    AL1(TIMSREJE),CL14'REJECTED'                                     
         DC    AL1(TIMSMAAP),CL14'LINE MNGR APPR'                               
         DC    AL1(EOF)                                                         
*                                                                               
EXPCTAB  DS    0CL15                                                            
         DC    AL1(EXCSDELT),CL14'DELETED'                                      
         DC    AL1(EXCSSUBM),CL14'SUBMITTED'                                    
         DC    AL1(EXCSPAPP),CL14'PART APPROVED'                                
         DC    AL1(EXCSCOMP),CL14'COMPLETED'                                    
         DC    AL1(EXCSREJE),CL14'REJECTED'                                     
         DC    AL1(EXCSLOGD),CL14'LOGICALLY DELD'                               
         DC    AL1(EXCSFNTA),CL14'AWAIT FINANCE'                                
         DC    AL1(EOF)                                                         
*                                                                               
ORDRTAB  DS    0CL15                                                            
         DC    AL1(ORDSDRFT),CL14'DRAFT ORDER'                                  
         DC    AL1(ORDSPAPP),CL14'PART APPROVED'                                
         DC    AL1(ORDSAPPR),CL14'APPROVED ORDER'                               
         DC    AL1(ORDSOREJ),CL14'REJECTED'                                     
         DC    AL1(ORDSEXEX),CL14'EXTENSION RECD'                               
         DC    AL1(ORDSSTAT),CL14'STATUS CHANGE'                                
         DC    AL1(ORDSSUBM),CL14'SUBMITTED'                                    
         DC    AL1(EOF)                                                         
*                                                                               
ESTATAB  DS    0CL15                                                            
         DC    AL1(4),CL14'ARCHIVED'                                            
         DC    AL1(ESTKDELT),CL14'DELETED'                                      
         DC    AL1(ESTKREJE),CL14'REJECTED'                                     
         DC    AL1(ESTKCAPP),CL14'CLIENT APPRVED'                               
         DC    AL1(ESTKCREA),CL14'CREATED'                                      
         DC    AL1(ESTKLOGD),CL14'LOGICALLY DELD'                               
         DC    AL1(ESTKSUBM),CL14'SUBMITTED'                                    
         DC    AL1(ESTKINTA),CL14'INTERNALLY APR'                               
         DC    AL1(EOF)                                                         
*                                                                               
*---------------------------------------------------------------------*         
* EACH ENTRY OF THE RECDTAB REFERS TO ONE SET OF RECORDS THAT IS READ *         
* BASED ON THE KEY PROVIDED IN THE ENTRY AND THE REQUIRED FIELDS IS   *         
* COMPARED WITH THE RECORD READ AND THE ACTUAL JOB FIELDS.            *         
* THE REQUIRED FIELD IF PRESENT IN THE ELEMENT OF THE RECORD, THE     *         
* ELEMENT DETAILS AND FIELD OFFSET MUST BE MENTIONED IN THE ENTRY.    *         
*                                                                     *         
* FOLLOWING IS THE BYTE WISE EXPLANATION OF THE FIELDS OF EACH ENTRY:-*         
* EACH BLOCK OF ENTRY CAN HAVE MULTIPLE ENTRIES                       *         
*                                                                     *         
*    1ST SET OF ENTRIES HAS THE SET OF KEY FIELDS TO READ THE RECORD  *         
*    AND EACH FIELD COULD BE A 3 OR 4 BYTE FIELD DEPENDING ON WHETEHR *         
*    IMMEDIATE FIELD OR A FIELD OFFSET                                *         
*           XL1 - LENGTH OF IMMEDIATE OR FIELD OFFSET                 *         
*                 (X'80' BIT MUST BE ON FOR ABOVE LENGTH FIELD FOR AN *         
*                  OFFICE FIELD SINCE SOME RECORDS MAY HAVE BINARY 0'S*         
*                  FOR OFFICE AND SOME MAY HAVE OFFSET FIELDS SO X'80'*         
*                  INDICATES TO CHECK FOR BOTH THE SCANARIOS)         *         
*           CL1 - 'I' IMMEDIATE , 'O' FIELD OFFSET                    *         
*           AL1 - IMMEDIATE VALUE, AL2 - OFFSET FIELD                 *         
*      X'00' - INDICATE END OF KEY FIELDS                             *         
*      AL1 - LENGTH OF KEY TO BE COMPARED WITH RECORD READ            *         
*      XL1 - INDEX TO THE COUNTER TABLE THAT HAS COUNTS FOR THIS RECD *         
*      X'00' - INDICATES THE ELEMENT DATA PRESENT                     *         
*      XL1   - ELEMENT TYPE LEN                                       *         
*      XL1   - ELEMENT TYPE                                           *         
*      XL1   - OFFSET FIELD LENGTH                                    *         
*      AL2   - ELEMENT DATA FIELD OFFSET                              *         
*      AL2   - ACTUAL RECORD DATA FIELD OFFSET                        *         
*      X'00' - INDICATES END OF ELEMENTS DATA                         *         
*                                                                     *         
*---------------------------------------------------------------------*         
RECDTAB  DS    0H                                                               
*                                                                               
TIMEPAS  DC    X'01',C'I',AL1(TSJPTYPQ) TIMESHEET PASSIVE                       
         DC    X'01',C'I',AL1(TSJPSUBQ)      SUB TYPE                           
         DC    X'01',C'O',AL2(SVCPY-ACJSD)   COMPANY CODE                       
         DC    X'02',C'I',AL1(0)             BINARY ZEROS                       
         DC    X'01',C'I',AL1(TSJPSJAQ)      OFFICE,CLIENT,PROD,JOB,MED         
         DC    X'82',C'O',AL2(SVOFF-ACJSD)   OFFICE                             
         DC    X'0C',C'O',AL2(SVSJACT-ACJSD) CLIENT/PRODUCT/JOB                 
         DC    X'01',C'O',AL2(SVJOB-ACJSD)   MEDIA                              
         DC    X'00'                    -END OF KEY ARGUMENTS                   
         DC    AL1(TSJPMED-TSJPAS+L'TSJPMED) 21 BYTES                           
         DC    X'01'                         INDEX TO CNTR TABLE                
*                                                                               
ORDER    DC    X'01',C'I',AL1(OSJPTYPQ) ORDER SJ PASSIVE                        
         DC    X'01',C'I',AL1(OSJPSUBQ)      SUB TYPE                           
         DC    X'01',C'O',AL2(SVCPY-ACJSD)   COMPANY CODE                       
         DC    X'14',C'I',AL1(0)             BINARY ZEROS                       
         DC    X'0C',C'O',AL2(SVSJACT-ACJSD) CLIENT/PRODUCT/JOB                 
         DC    X'00'                    -END OF KEY ARGUMENTS                   
         DC    AL1(OSJPMEM-OSJPAS)           35 BYTES                           
         DC    X'02'                         INDEX TO CNTR TABLE                
*                                                                               
PRDADJ   DC    X'01',C'I',AL1(PARKTYPQ) PRODUCTION ADJUSTMENT RATE TYPE         
         DC    X'01',C'O',AL2(SVCPY-ACJSD)   COMPANY CODE                       
         DC    X'82',C'O',AL2(SVOFF-ACJSD)   OFFICE                             
         DC    X'0C',C'O',AL2(SVSJACT-ACJSD) CLIENT/PRODUCT/JOB                 
         DC    X'00'                    -END OF KEY ARGUMENTS                   
         DC    AL1(PARKDEP-PARKEY)      16 BYTES                                
         DC    X'03'                         INDEX TO CNTR TABLE                
*                                                                               
ESTREC   DC    X'01',C'I',AL1(ESTKTYPQ)    ESTIMATE TYPE                        
         DC    X'01',C'I',AL1(ESTKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'18',C'I',AL1(0)            BINARY ZEROS                        
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'01',C'I',AL1(0)            N/D                                 
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(ESTKLNO-ESTKEY)         40 BYTES                             
         DC    X'04'                         INDEX TO CNTR TABLE                
*                                                                               
EVEREC   DC    X'01',C'I',AL1(EVEKTYPQ)    ESTIMATE VER TYPE                    
         DC    X'01',C'I',AL1(EVEKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(EVEKTYPE-EVEKEY)        23 BYTES                             
         DC    X'05'                         INDEX TO CNTR TABLE                
*                                                                               
AUDREC   DC    X'01',C'I',AL1(AUDKTYPQ)    AUDIT TYPE                           
         DC    X'01',C'I',AL1(AUDKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',AL1(AUDKEST)      AUDIT ESTIMATE TYPE                 
         DC    X'18',C'I',AL1(0)            BINARY ZEROS                        
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(AUDKELNO-AUDKEY)        40 BYTES                             
         DC    X'06'                         INDEX TO CNTR TABLE                
*                                                                               
POPREC   DC    X'01',C'I',AL1(POPKTYPQ)    PROD OPTIONS RECORD                  
         DC    X'01',C'I',AL1(POPKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'01',C'I',AL1(0)            OFFICE GRP                          
         DC    X'82',C'O',AL2(SVOFF-ACJSD)  OFFICE                              
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(POPKMGR-POPKEY)         26 BYTES                             
         DC    X'07'                         INDEX TO CNTR TABLE                
*                                                                               
JCBREC   DC    X'01',C'I',AL1(JCBKTYPQ)    JOB CYCLE BILL RECORD                
         DC    X'01',C'I',AL1(JCBKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(JCBKJOB-JCBKEY+L'JCBKJOB)   23 BYTES                         
         DC    X'08'                         INDEX TO CNTR TABLE                
*                                                                               
PRCREC   DC    X'01',C'I',AL1(PRCKTYPQ)    UNIT PRICING RECORD                  
         DC    X'01',C'I',AL1(PRCKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'01',C'I',AL1(0)            OFFICE GRP                          
         DC    X'82',C'O',AL2(SVOFF-ACJSD)  OFFICE                              
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(PRCKMGR-PRCKEY)         26 BYTES                             
         DC    X'09'                         INDEX TO CNTR TABLE                
*                                                                               
UFSREC   DC    X'01',C'I',AL1(UFSKTYPQ)    USER FIELD SEL RECORD                
         DC    X'01',C'I',AL1(UFSKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'01',C'I',AL1(0)            OFFICE GRP                          
         DC    X'82',C'O',AL2(SVOFF-ACJSD)  OFFICE                              
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'02',C'I',AL1(0)            MEDIA GRP/CODE                      
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(UFSKJOB-UFSKEY+L'UFSKJOB) 28 BYTES                           
         DC    X'0A'                         INDEX TO CNTR TABLE                
*                                                                               
TXTREC   DC    X'01',C'I',AL1(TXTKTYPQ)    TEXT RECORD                          
         DC    X'01',C'I',AL1(TXTKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'01',C'I',AL1(0)            OFFICE GRP                          
         DC    X'82',C'O',AL2(SVOFF-ACJSD)  OFFICE                              
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(TXTKMG-TXTKEY)          26 BYTES                             
         DC    X'0B'                        INDEX TO CNTR TABLE                 
*                                                                               
SESREC   DC    X'01',C'I',AL1(SESKTYPQ)    SESSION RECORD                       
         DC    X'01',C'I',AL1(SESKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'06',C'O',AL2(SVJOB-ACJSD)  JOB                                 
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(SESKTYPE-SESKEY)        23 BYTES                             
         DC    X'0C'                        INDEX TO CNTR TABLE                 
*                                                                               
REBREC   DC    X'01',C'I',AL1(REBKTYPQ)    REB INVOICE RECORD                   
         DC    X'01',C'I',AL1(REBKSUBQ)    SUB TYPE                             
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(REBKREM-REBKEY)          3 BYTES                             
         DC    X'0D'                        INDEX TO CNTR TABLE                 
         DC    X'00'                        START OF RBDELD                     
         DC    X'01',AL1(RBDELQ)            RBDEL TYPE                          
         DC    X'03',AL2(RBDRCPJ-RBDEL),AL2(SVCLI-ACJSD)                        
         DC    X'03',AL2(RBDRCPJ-RBDEL+3),AL2(SVPRD-ACJSD)                      
         DC    X'06',AL2(RBDRCPJ-RBDEL+6),AL2(SVJOB-ACJSD)                      
         DC    X'00'                        END OF RBDELD                       
*                                                                               
APPREC   DC    X'01',C'I',AL1(APPKTYPQ)     APPROVER RECORD                     
         DC    X'01',C'I',AL1(APPKSUBQ)     SUB TYPE                            
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(APPKREM-APPKEY)          3 BYTES                             
         DC    X'0E'                        INDEX TO CNTR TABLE                 
         DC    X'00'                        START OF CIDELD                     
         DC    X'01',AL1(LIDELQ)            LIDEL TYPE                          
         DC    X'03',AL2(LIDLACT-LIDEL),AL2(SVCLI-ACJSD)                        
         DC    X'03',AL2(LIDLACT-LIDEL+3),AL2(SVPRD-ACJSD)                      
         DC    X'06',AL2(LIDLACT-LIDEL+6),AL2(SVJOB-ACJSD)                      
         DC    X'00'                        END OF CIDELD                       
*                                                                               
JFNREC   DC    X'01',C'I',AL1(FUNKTYPQ)     JOB FUND RECORD                     
         DC    X'01',C'I',AL1(FUNKSUBQ)     SUB TYPE                            
         DC    X'01',C'O',AL2(SVCPY-ACJSD)  COMPANY CODE                        
         DC    X'01',C'I',CL1'S'            UNIT S                              
         DC    X'01',C'I',CL1'J'            LEDGER J                            
         DC    X'01',C'I',AL1(0)            OFFICE GRP                          
         DC    X'82',C'O',AL2(SVOFF-ACJSD)  OFFICE                              
         DC    X'03',C'O',AL2(SVCLI-ACJSD)  CLIENT                              
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'03',C'O',AL2(SVPRD-ACJSD)  PRODUCT                             
         DC    X'03',C'I',CL1' '            SPACES                              
         DC    X'00'                       -END OF KEY ARGUMENTS                
         DC    AL1(FUNKMGR-FUNKEY)          3 BYTES                             
         DC    X'0F'                        INDEX TO CNTR TABLE                 
         DC    X'00'                        START OF FJNELD                     
         DC    X'01',AL1(FJNELQ)            FJNEL TYPE                          
         DC    X'06',AL2(FJNJOB-FJNEL),AL2(SVJOB-ACJSD)                         
         DC    X'00'                        END OF FJNELD                       
*                                                                               
STUDIO   DC    X'01',C'I',AL1(SACKTYPQ) STUDIO RECORD                           
         DC    X'01',C'I',AL1(SACKSUBQ)      SUB-TYPE                           
         DC    X'01',C'O',AL2(SVCPY-ACJSD)   COMPANY CODE                       
         DC    X'0C',C'O',AL2(SVSJACT-ACJSD) CLIENT/PRODUCT/JOB                 
         DC    X'00'                    -END OF KEY ARGUMENTS                   
         DC    AL1(SACKSTY-SACKEY)      15 BYTES                                
         DC    X'10'                         INDEX TO CNTR TABLE                
*                                                                               
EORT     DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GET ELEMENT AND MATCH THE REQUIRED C/P/J FOR A GIVEN COMPANY        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GETELM   DS 0H                                                                  
         NMOD1 0,*GETELM*                                                       
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACJSD,RC                                                         
*                                                                               
         LAY   R7,IO               R7=PRINT LINE                                
         MVC   ELCODE,4(R2)                                                     
         LA    R2,5(R2)            R2 TO FIRST ENTRY OF ELM FIELDS              
         LR    R3,R2               SAVE R2 FOR NEXT PASS                        
         JAS   RE,GETEL2                                                        
         JNE   GETELMX                                                          
GETELM10 LLH   RE,1(R2)            DISPLACEMENT FOR ELM FIELD!                  
         LLH   RF,3(R2)            DISPLACEMENT FOR WS FIELD!                   
         LA    RE,0(RE,R7)                                                      
         LA    RF,0(RF,RC)                                                      
         LLC   R1,0(R2)            LEN OF COMPARE                               
         BCTR  R1,0                                                             
         EX    R1,EXCLC2                                                        
         JNE   GETELM20                                                         
         LA    R2,5(R2)                                                         
         CLI   0(R2),0                                                          
         JNE   GETELM10                                                         
         LA    R2,1(R2)                                                         
         J     GETELMX                                                          
*                                                                               
GETELM20 LR    R2,R3                                                            
         JAS   RE,NEXTEL2                                                       
         JE    GETELM10                                                         
*                                                                               
GETELMX  XIT1                                                                   
*                                                                               
EXCLC2   CLC   0(0,RE),0(RF)       SAME CLI,PRD OR JOB ?                        
*        LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PRINT REPORT - RECORD DETAILS FOR MANUAL DELETION                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PRPT000  DS 0H                                                                  
         NMOD1 0,**PRPT**                                                       
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         LA    RC,SPACEND                                                       
         USING ACJSD,RC            RC = A(SAVE W/S)                             
*                                                                               
         USING EXCRECD,R2                                                       
PRPT010  LA    R2,IOKEY                                                         
         LHI   R7,0                EXPENSE TABLE ENTRY (00) IN CNTRS            
*        MHI   R7,CNTLQ                                                         
         LA    R7,CNTRS(R7)                                                     
         L     R0,0(R7)            R0-> COUNT OF EXPC RECORDS                   
         CHI   R0,0                                                             
         JNH   PRPT040                                                          
         LA    R7,4(R7)            R7-> RECORD DA                               
         CHI   R0,10                                                            
         JNH   *+8                                                              
         LHI   R0,10               SET MAX TO 10                                
         MVC   PAPPL(50),PEHEADER MOVE IN HEADER                                
         LA    R5,XPSECOND                                                      
         AHI   R5,PAPPL-PLINED                                                  
         GOTO1 UNDERLIN,DMCB,(50,PAPPL),(R5)                                    
         GOTO1 ACREPORT                                                         
         MVC   PAPPL,=CL10'EXPENSES'                                            
PRPT015  GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',0(R7),IO,DMWORK                   
         MVI   PDASH,C'-'                                                       
         MVC   PECLM,EXCKREF       CLAIM NUMBER                                 
*        MVC   SVPID,EXJPPID                                                    
         SR    RF,RF                                                            
         ICM   RF,3,EXCKDATE                                                    
         LNR   RF,RF                                                            
         STCM  RF,3,HALF                                                        
         GOTO1 DATCON,DMCB,(2,HALF),(21,PECDTE)  CLAIM DATE                     
         GOTO1 GTASTAT,DMCB,EXPCTAB,EXCRSTAT GET STAT BITS IN STAT FLD          
         MVC   PESTAT,WORK                                                      
         MVC   SVIOKEY2,IOKEY      SAVE OFF IOKEY                               
*                                                                               
* READ SECURITY SA0REC AND SAPEREC                                              
*                                                                               
         USING SA0REC,R3                                                        
         LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ    C'0'-PERSONAL AUTHORIZATION RECORDS          
         MVC   SA0KAGY,SVAID       ALPHA/SECURITY ID                            
         MVC   SA0KNUM,EXCKPIDB                                                 
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(SA0LEN-SA0KEY),SVKEY                                       
         JNE   PRPT030                                                          
*                                                                               
         LA    R3,IO                                                            
         LA    R2,SA0DATA                                                       
PRPT020  CLI   0(R2),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'C3'         PERSON PERSONAL-ID                           
         JE    PRPT025                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     PRPT020                                                          
*                                                                               
         USING SAPALD,R2                                                        
PRPT025  MVC   PEPID,SAPALPID      PERSON ID NAME                               
         DROP  R2                                                               
*                                                                               
PRPT030  GOTO1 ACREPORT                                                         
         MVC   SVKEY,SVIOKEY2          RESTORE SEQUENCE                         
         GOTO1 =A(DMREADDR),DMCB,(RC)  READ                                     
         LA    R7,4(R7)                                                         
         JCT   R0,PRPT015          READ NEXT                                    
         GOTO1 ACREPORT            BLANK LINE                                   
*                                                                               
         USING TIMRECD,R2                                                       
PRPT040  LA    R2,IOKEY                                                         
         LHI   R7,1                TIME TABLE ENTRY (01) IN CNTRS               
         MHI   R7,CNTLQ                                                         
         LA    R7,CNTRS(R7)                                                     
         L     R0,0(R7)            R0-> COUNT OF TIME RECORDS                   
         CHI   R0,0                                                             
         JNH   PRPT050                                                          
         LA    R7,4(R7)            R7-> RECORD DA                               
         CHI   R0,10                                                            
         JNH   *+8                                                              
         LHI   R0,10               SET MAX TO 10                                
         MVC   PAPPL(50),PTHEADER  MOVE IN HEADER                               
         LA    R5,XPSECOND                                                      
         AHI   R5,PAPPL-PLINED                                                  
         GOTO1 UNDERLIN,DMCB,(50,PAPPL),(R5)                                    
         GOTO1 ACREPORT                                                         
         MVC   PAPPL,=CL10'TIME'                                                
PRPT045  GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',0(R7),IO,DMWORK                   
         MVI   PDASH,C'-'                                                       
         MVC   PT1RUL,=C'1R'                                                    
         MVC   PT1RACT,TIMKACT                                                  
         ICM   RF,7,TIMKPEDT                                                    
*        LNR   RF,RF                                                            
         STCM  RF,7,WORK                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(21,PTPED)  PERIOD END DATE                 
         GOTO1 GTASTAT,DMCB,STATTAB,TIMRSTAT GET STAT BITS IN STAT FLD          
         MVC   PTSTAT,WORK                                                      
         GOTO1 ACREPORT                                                         
         LA    R7,4(R7)                                                         
         JCT   R0,PRPT045          READ NEXT                                    
         GOTO1 ACREPORT            BLANK LINE                                   
*                                                                               
         USING ORDRECD,R2                                                       
PRPT050  LA    R2,IOKEY                                                         
         LHI   R7,2               ORDER TABLE ENTRY (02) IN CNTRS               
         MHI   R7,CNTLQ                                                         
         LA    R7,CNTRS(R7)                                                     
         L     R0,0(R7)            R0-> COUNT OF ORDER RECORDS                  
         CHI   R0,0                                                             
         JNH   PRPT060                                                          
         LA    R7,4(R7)            R7-> RECORD DA                               
         CHI   R0,10                                                            
         JNH   *+8                                                              
         LHI   R0,10               SET MAX TO 10                                
         MVC   PAPPL(50),POHEADER MOVE IN HEADER                                
         LA    R5,XPSECOND                                                      
         AHI   R5,PAPPL-PLINED                                                  
         GOTO1 UNDERLIN,DMCB,(50,PAPPL),(R5)                                    
         GOTO1 ACREPORT                                                         
         MVC   PAPPL,=CL10'ORDER'                                               
PRPT055  GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',0(R7),IO,DMWORK                   
         MVI   PDASH,C'-'                                                       
         MVC   PORDNUM,ORDKORD     ORDER NUMBER                                 
         GOTO1 GTASTAT,DMCB,ORDRTAB,ORDRSTA2 GET STAT BITS IN STAT FLD          
         MVC   POSTAT,WORK                                                      
         GOTO1 ACREPORT                                                         
         LA    R7,4(R7)                                                         
         JCT   R0,PRPT055          READ NEXT                                    
         GOTO1 ACREPORT            BLANK LINE                                   
*                                                                               
         USING ESTRECD,R2                                                       
PRPT060  LA    R2,IOKEY                                                         
         LHI   R3,4                EST TABLE ENTRY (04) IN CNTRS                
         MHI   R3,CNTLQ                                                         
         LA    R3,CNTRS(R3)                                                     
         L     R0,0(R3)            R0-> COUNT OF EST RECORDS                    
         CHI   R0,0                                                             
         JNH   PRPT070                                                          
         LA    R3,4(R3)            R3-> RECORD DA                               
         CHI   R0,10                                                            
         JNH   *+8                                                              
         LHI   R0,10               SET MAX TO 10                                
         MVC   PAPPL(50),PESTHEDR  MOVE IN HEADER                               
         LA    R5,XPSECOND                                                      
         AHI   R5,PAPPL-PLINED                                                  
         GOTO1 UNDERLIN,DMCB,(50,PAPPL),(R5)                                    
         GOTO1 ACREPORT                                                         
         MVC   PAPPL,=CL10'ESTIMATE'                                            
PRPT065  GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',0(R3),IO,DMWORK                   
         MVI   PDASH,C'-'                                                       
         LAY   R7,IO                                                            
         MVI   ELCODE,EMDELQ       X'A9' - EMDEL ELEMENT                        
         JAS   RE,GETEL2                                                        
         JNE   *+2                 NO EMDEL A9 ELEMENT!                         
         USING EMDELD,R7                                                        
         MVC   PEST#,EMDGNO      ESTIMATE NUMBER                                
         DROP  R7                                                               
         GOTO1 GTASTAT,DMCB,ESTATAB,ESTRSTA1 GET STAT BITS IN STAT FLD          
         MVC   PESTA,WORK                                                       
         GOTO1 ACREPORT                                                         
PRPT066  LA    R3,4(R3)                                                         
         JCT   R0,PRPT065          READ NEXT                                    
         GOTO1 ACREPORT            BLANK LINE                                   
*                                                                               
PRPT070  EQU *                                                                  
         CLI   BALFLG,C'N'         ACCOUNT NOT BALANCED!                        
         JNE   PRPT999                                                          
         MVC   PAPPL(21),=C'ACCOUNT NOT BALANCED!'                              
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
PRPT999  EQU *                                                                  
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PROCESS EXPENSE RECORD                                              *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PEXP000  DS 0H                                                                  
         NMOD1 0,**PEXP**                                                       
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACJSD,RC            RC = A(SAVE W/S)                             
         USING EXJPASD,R2                                                       
         LA    R2,SVKEY                                                         
         XR    R7,R7                                                            
         LAY   R8,CNTRS+4                                                       
         XC    EXJPAS,EXJPAS                                                    
         MVI   EXJPTYP,EXJPTYPQ    X'37'                                        
         MVI   EXJPSUB,EXJPSUBQ    X'1B'                                        
         MVC   EXJPCPY,RCCOMPFL    COMPANY CODE                                 
         MVI   EXJPVIEW,EXJPCLI1                                                
         TM    SVCPSTA6,CPX2LAEI                                                
         JZ    *+8                                                              
         MVI   EXJPVIEW,EXJPCBL1                                                
         MVC   EXJPCOFF,SVOFF                                                   
         MVC   EXJPCPJ,SVSJACT                                                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         J     PEXP020                                                          
PEXP010  GOTO1 =A(DMSEQDR),DMCB,(RC)        READ SEQ                            
*                                                                               
PEXP020  CLC   SVKEY(EXJPMED-EXJPASD),IOKEY                                     
         JNE   PEXP040                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         AHI   R7,1                                                             
         MVI   MANDELF,C'N'        CANNOT BE MANUALLY DELETED!                  
         CHI   R7,10                                                            
         JH    PEXP010                                                          
         MVC   0(4,R8),EXJPDA                                                   
         LA    R8,4(R8)                                                         
         J     PEXP010                                                          
PEXP040  EQU *                                                                  
         ST    R7,CNTRS                                                         
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PROCESS RECORD TABLE                                                *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PTRN802  DS 0H                                                                  
         NMOD1 0,*PTRN8**                                                       
         LAY   R2,RECDTAB          RECORD TABLE                                 
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING PLINED,R4                                                        
         USING ACJSD,RC            RC = A(SAVE W/S)                             
*                                                                               
PTRN805  CLI   0(R2),X'FF'         END OF RECORD TABLE ?                        
         JE    PTRN802X                                                         
         XR    R7,R7               COUNT OF RECORDS FOUND!                      
         XC    SAVER3,SAVER3                                                    
         LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
PTRN810  CLI   0(R2),0             ALL FIELDS PROCESSED?                        
         JE    PTRN815                                                          
         CLI   1(R2),C'I'          DO WE HAVE IMMEDIATE VALUE?                  
         JNE   PTRN812                                                          
         LLC   R1,0(R2)                                                         
         MVC   0(1,R3),2(R2)                                                    
         LA    R3,1(R3)                                                         
         JCT   R1,*-10                                                          
         LA    R2,3(R2)            BUMP OVER RECORD KEY FIELDS                  
         J     PTRN810                                                          
*                                                                               
PTRN812  LLH   RF,2(R2)            WE HAVE A DISPLACEMENT FOR FIELD!            
         LA    RF,0(RF,RC)                                                      
         LLC   R1,0(R2)                                                         
         TM    0(R2),X'80'                                                      
         JNO   PTRN813                                                          
         NILL  R1,X'7F'                                                         
         ST    R3,SAVER3                                                        
PTRN813  BCTR  R1,0                                                             
         EX    R1,EXMVC                                                         
         LA    R3,1(R1,R3)                                                      
         LA    R2,4(R2)            BUMP OVER RECORD KEY FIELDS                  
         J     PTRN810                                                          
EXMVC    MVC   0(0,R3),0(RF)                                                    
*                                                                               
PTRN815  LLC   R5,2(R2)                                                         
         LR    R8,R5                                                            
         MHI   R8,CNTLQ                                                         
         LA    R8,CNTRS(R8)                                                     
         LA    R8,4(R8)                     R8 --> 1ST DA IN TABLE              
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         J     PTRN825                                                          
PTRN820  GOTO1 =A(DMSEQDR),DMCB,(RC)        READ SEQ                            
*                                                                               
PTRN825  LLC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,EXCLC                     COMPARE KEY READ!                   
         JNE   PTRN830                                                          
*                                                                               
         CLI   2(R2),X'02'                  ORDER RECORD?                       
         JNE   *+12                                                             
         USING OSJPASD,IOKEY                                                    
         TM    OSJPSTAT,ORDSDEL+ORDSFMCH+ORDSLDEL+ORDCLOSE                      
         JNZ   PTRN820             GET NEXT RECORD                              
*                                                                               
         LA    RF,IO                                                            
         USING ACCRECD,RF                                                       
         MVC   DA,ACCKDA                                                        
         DROP  RF                                                               
         CLI   3(R2),0                                                          
         JNE   PTRN828                                                          
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         GOTOR =A(GETELM)                    CHECK ELEMENT!                     
         JNE   PTRN820                                                          
PTRN828  AHI   R7,1                                                             
         CHI   R7,10                                                            
         JH    PTRN820                                                          
         MVC   0(4,R8),DA                                                       
         LA    R8,4(R8)                                                         
         J     PTRN820                                                          
EXCLC    CLC   SVKEY(0),IOKEY                                                   
*                                                                               
PTRN830  CHI   R7,0                         ANY MATCHED RECORD?                 
         JH    PTRN834                                                          
         OC    SAVER3,SAVER3                                                    
         JZ    PTRN832                                                          
         L     R3,SAVER3                                                        
         MVC   0(2,R3),=XL2'00'                                                 
         XC    SAVER3,SAVER3                                                    
         J     PTRN815                                                          
PTRN832  CLI   3(R2),0             ANY ELEMENTS IN TABLE ENTRY?                 
         JNE   PTRN833                                                          
         LA    R2,6(R2)            LOOP AND SKIP THE ELEMENT ENTRY              
         CLI   0(R2),0                                                          
         JE    *+12                                                             
         LA    R2,5(R2)                                                         
         J     *-12                                                             
         LA    R2,1(R2)                                                         
         J     PTRN805                                                          
*                                                                               
PTRN833  LA    R2,3(R2)                                                         
         J     PTRN805                                                          
*                                                                               
PTRN834  MVI   MANDELF,C'N'        CANNOT BE MANUALLY DELETED!                  
         CLI   3(R2),0                                                          
         JNE   PTRN835                                                          
         LA    R2,6(R2)                                                         
         CLI   0(R2),0                                                          
         JE    *+16                                                             
         LA    R2,5(R2)                                                         
         J     *-12                                                             
PTRN835  LA    R2,2(R2)                                                         
         LA    R2,1(R2)                                                         
         MHI   R5,CNTLQ                                                         
         LA    R5,CNTRS(R5)                                                     
         ST    R7,0(R5)                                                         
         J     PTRN805             NEXT RECDTAB ENTRY                           
*                                                                               
PTRN802X XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA10   AP    0(8,R4),0(8,R3)     ADD TO BUCKET                                
         LA    R3,8(R3)            BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,8(R4)            BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADAC NMOD1 0,RDAC              READ ACCOUNT                                 
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SVIOKEY,IO,0                     
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         MVC   XHEAD2+11(L'SVCLI),SVCLI            CLIENT  CODE                 
         MVC   XHEAD2+21(L'SVCLINM),SVCLINM        CLIENT  NAME                 
         MVC   XHEAD2+69(L'SVODTE),SVODTE          OPENED DATE                  
*        MVC   XHEAD2+92(L'SVBTYPE),SVBTYPE        BILL TYPE                    
*        MVC   XHEAD2+97(L'SVBTYDES),SVBTYDES      BILL TYPE DESC               
*                                                                               
         TM    ACSTAT,RSTSACIC                     IS ACCOUNT CLOSED?           
         BNO   *+10                                                             
         MVC   XHEAD2+129(10),=C'**CLOSED**'                                    
         TM    ACSTAT,RSTSACIL                     IS ACCOUNT LOCKED?           
         BNO   *+10                                                             
         MVC   XHEAD2+129(10),=C'**LOCKED**'                                    
*                                                                               
         MVC   XHEAD3+11(L'SVPRD),SVPRD            PRODUCT CODE                 
         MVC   XHEAD3+21(L'SVPRDNM),SVPRDNM        PRODUCT NAME                 
         MVC   XHEAD3+69(L'SVRDTE),SVRDTE          REVISED DATE                 
         MVC   XHEAD3+92(L'SVCOST-1),SVCOSTU       COSTING U/L/ACCOUNT          
*                                                                               
         MVC   XHEAD4+11(L'SVJOB),SVJOB            JOB     CODE                 
         MVC   XHEAD4+21(L'SVJOBNM),SVJOBNM        JOB     NAME                 
         MVC   XHEAD4+69(L'SVCDTE),SVCDTE          CLOSED DATE                  
         MVC   XHEAD4+92(L'CURRBAL),CURRBAL        ACCOUNT BALANCE              
*                                                                               
         MVC   XHEAD5+28(L'MANDELF),MANDELF        MANUAL DELETION?             
         MVC   XHEAD5+69(L'SVPDTE),SVPDTE          PEELED DATE                  
         MVC   XHEAD5+92(L'PEELBAL),PEELBAL        ACCOUNT PEELED BAL           
*                                                                               
         LAY   R5,CNTRS                                                         
         USING CNTD,R5                                                          
         EDIT  (B4,CNTVAL),(6,XHEAD6+15),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD6+32),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD6+49),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD6+70),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD6+90),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD6+107),ZERO=NOBLANK                          
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+15),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+32),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+49),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+70),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+90),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD7+107),ZERO=NOBLANK                          
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD8+15),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD8+32),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD8+49),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD8+70),ZERO=NOBLANK                           
         LA    R5,CNTLQ(R5)                                                     
         EDIT  (B4,CNTVAL),(6,XHEAD8+90),ZERO=NOBLANK                           
*        LA    R5,CNTLQ(R5)                                                     
         EDIT  (B3,SVDRAFT),(6,XHEAD8+107),ZERO=NOBLANK     DRAFT TRANS         
         DROP  R5                                                               
         MVC   XHEAD8+124(1),BALFLG        ACCOUNT BALANCE FLAG                 
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+8,C'T'                                                   
*        MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
*        MVI   BOXCOLS+(PSUBACC-PRTLNE-1),C'C'                                  
*        MVI   BOXCOLS+(PSUBANM-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACJSD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ABINADD  DS    A                   BINADD ROUTINE                               
PRNTBL   DS    V                   PRINT DATA                                   
UNDERLIN DS    V                   UNDERLINE HEADER                             
SQUASHER DS    V                   SQUASH INFO LINE                             
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SV60EL   DS    A                   SAVED AREA FOR 60 ELEMENT (TRSEL)            
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
*                                                                               
SAVER3   DS    F                                                                
SVBYTE   DS    XL1                                                              
SVKEY    DS    CL42                                                             
SVIOKEY  DS    CL42                                                             
SVIOKEY2 DS    CL42                                                             
SVSJACT  DS    0CL12               SJ ACCOUNT                                   
SVCLI    DS    CL3                 CLIENT  CODE                                 
SVPRD    DS    CL3                 PRODUCT CODE                                 
SVJOB    DS    CL6                 JOB     CODE                                 
SVCLINM  DS    CL36                CLIENT  NAME                                 
SVPRDNM  DS    CL36                PRODUCT NAME                                 
SVJOBNM  DS    CL36                JOB     NAME                                 
SVOFF    DS    CL2                 CLIENT OFFICE                                
SVOFFNM  DS    CL36                CLIENT OFFICE NAME                           
SVFLD    DS    CL40                SAVED AREA FOR INFO LINE                     
SVFLD1   DS    CL10                SAVED AREA FOR INFO LINE                     
SVPID    DS    XL2                                                              
SVAID    DS    CL2                 SAVED AREA FOR ALPHA/SECURITY ID             
SVPERNAM DS    CL8                 PERSONAL ID NAME                             
SVCPY    DS    CL1                 COMPANY                                      
SVCPSTA6 DS    XL1                 COMPANY XTRA ELEMENT STATUS 6                
SVQSRT   DS    CL6                 QSTART DATE IN YYMMDD-NO FUNNY YEAR          
SVQEND   DS    CL6                 QSTART DATE IN YYMMDD-NO FUNNY YEAR          
SVDRAFT  DS    XL3                 DRAFT TRANS                                  
SVJBADV  DS    XL2                 JOB BILLING ADVANCE                          
*                                                                               
SVWRKCD  DS    CL2                 WORK CODE                                    
SVSUBULA DS    0CL14                                                            
SVSUBUL  DS    CL2                 CONTRA U/L                                   
SVSUBACC DS    CL12                CONTRA ACCOUNT                               
SVSUBANM DS    CL36                CONTRA ACCOUNT NAME                          
SVSUBLNQ EQU   *-SVWRKCD                                                        
*                                                                               
SVCOST   DS    0CL15               COSTING ACCOUNT                              
SVCOSTC  DS    XL1                 COMPANY CODE                                 
SVCOSTU  DS    CL1                 UNIT CODE                                    
SVCOSTL  DS    CL1                 LEDGER CODE                                  
SVCOSTA  DS    CL12                ACCOUNT CODE                                 
*                                                                               
SVADTE   DS    CL6                 ACTIVITY DATE                                
SVCDTE   DS    CL8                 CLOSING  DATE                                
SVODTE   DS    CL8                 OPENED   DATE                                
SVRDTE   DS    CL8                 REVISED  DATE                                
SVPDTE   DS    CL8                 PEELED   DATE                                
*                                                                               
PACK16   DS    PL16                                                             
PACK8    DS    PL8                                                              
*                                                                               
CURRBAL  DS    CL10                CURRENT BALANCE AMOUNT                       
PEELBAL  DS    CL10                PEEL BALANCE                                 
*                                                                               
CLIOFF   DS    CL2                 CLIENT OFFICE                                
OFFPOS   DS    CL1                 SJ CLIENT OFFICE POSITION                    
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
FLAG     DS    CL1                 SPECIFIC TO EACH REQUEST                     
BALFLG   DS    CL1                 ACCOUNT BALANCED FLAG                        
MANDELF  DS    CL1                 MANUAL JOB DELETION FLAG                     
ACSTAT   DS    XL1                 ACCOUNT STATUS BYTE                          
APPSTAT  DS    XL1                 APPLICATIONS STATUS                          
*                                                                               
MSG      DS    CL10                                                             
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
ALL      EQU   X'FF'               EVERYTHING                                   
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
         DS    0F                                                               
CNTRS    DS    (RECMAX)CL(CNTLQ)                                                
RECMAX   EQU   20                                                               
         EJECT                                                                  
***********************************************************************         
* COUNTERS DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
CNTD     DSECT                                                                  
CNTVAL   DS    XL4                                                              
CNTDA1   DS    XL4                                                              
CNTDA2   DS    XL4                                                              
CNTDA3   DS    XL4                                                              
CNTDA4   DS    XL4                                                              
CNTDA5   DS    XL4                                                              
CNTDA6   DS    XL4                                                              
CNTDA7   DS    XL4                                                              
CNTDA8   DS    XL4                                                              
CNTDA9   DS    XL4                                                              
CNTDA10  DS    XL4                                                              
CNTLQ    EQU   *-CNTD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* TRANSACTION STATUS BYTE TABLE DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
TRNSTD   DSECT                                                                  
TRNSTBIT DS    XL1                                                              
TRNSTLN  DS    XL1                                                              
TRNSTDSC DS    CL10                                                             
TRNSTLNQ EQU   *-TRNSTD                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE # CLIENT CODES                    
         DS    CL2                                                              
PTITLE   DS    CL15                FIELD DESCRIPTION                            
         DS    CL2                                                              
PWRKCD   DS    CL2                                                              
         ORG   PWRKCD                                                           
PSUBULA  DS    0CL14                                                            
PSUBUL   DS    CL2                 CONTRA U/L                                   
PSUBACC  DS    CL12                CONTRA ACCOUNT                               
         DS    CL1                                                              
PSUBANM  DS    CL36                CONTRA ACCOUNT NAME                          
         ORG   PTITLE                                                           
PTRNDTE  DS    CL6                 TRANSACTION DATE                             
         DS    CL2                                                              
PTRSDTE  DS    CL6                 ACTIVITY    DATE                             
         DS    CL2                                                              
PREF     DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL2                                                              
PSEQ     DS    CL3                 SEQUENCE NUMBER                              
         DS    CL2                                                              
PTYP     DS    CL2                 TRANSACTION TYPE                             
         DS    CL2                                                              
PAMNT    DS    CL10                TRANSACTION AMOUNT                           
         DS    CL2                                                              
PDETL    DS    0CL107                                                           
PELNM    DS    CL7                 ELEMENT NAME AND (-)                         
PINFO    DS    0CL100              STATUS/INFO                                  
         ORG   PREF                                                             
PAPPL    DS    CL10                APPLICATION                                  
PDASH    DS    CL1                 DASH                                         
         DS    CL2                                                              
PT1RULA  DS    0CL14               1R U/L/A                                     
PT1RUL   DS    CL2                 1R U/L                                       
PT1RACT  DS    CL12                1R     A                                     
         DS    CL2                                                              
PTPED    DS    CL10                PERIOD END DATE                              
         DS    CL2                                                              
PTSTAT   DS    CL64                STATUS                                       
         ORG   PT1RULA                                                          
PECLM    DS    CL6                 EXPENSE CLAIM                                
         DS    CL2                                                              
PEPID    DS    CL8                 EXPENSE PID                                  
         DS    CL2                                                              
PECDTE   DS    CL10                EXPENSE DATE                                 
         DS    CL2                                                              
PESTAT   DS    CL64                STATUS                                       
         ORG   PT1RULA                                                          
PORDNUM  DS    CL6                 ORDER NUMBER                                 
         DS    CL2                                                              
POSTAT   DS    CL64                STATUS                                       
         ORG   PT1RULA                                                          
PEST#    DS    CL6                 ESTIMATE NUMBER                              
         DS    CL2                                                              
PESTA    DS    CL64                STATUS                                       
         ORG   PINFO+L'PINFO                                                    
         DS    CL2                                                              
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPJS02 08/10/20'                                      
         END                                                                    
