*          DATA SET SPTRA4A    AT LEVEL 108 AS OF 05/20/19                      
*PHASE T2164AC                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PQPROF                                                                 
         TITLE 'T2164A - SPOT DETAIL INSTRUCTION GENERATION'                    
***********************************************************************         
* PUTS OUT 30 ELEMS IN INST RECAP RECS (LIKE DEALER INST)             *         
***********************************************************************         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*                                                                     *         
*             AIO2 - HEADLINES, FOOTNOTES, COMMERCIAL LIST            *         
*                                                                     *         
*             AIO3 - TEXT, INSTR, TBA                                 *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER TO STATBLE AND SPTABLE                          *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE REG                                         *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
* TSPFUSER 000 - 007 ID                                               *         
*          008 - 135 ERRFILE DCB                                      *         
*          136 - 139 CT TO FORCE CHANGE TO REMOTKEY                   *         
*          140       EASYLINK STUFF PRINTED SWITCH                    *         
*                                                                     *         
*                                                                     *         
* TSAR USES ELEM AS AN I/O AREA                                       *         
*      0 - 1  LENGTH OF RECORD                                        *         
*      2 - 3  KEY - BINARY NUMBER 1 TO N                              *         
*      4      FORCEHED OR, IF BINARY, SPACE BEFORE                    *         
*      5      SPACING                                                 *         
*      6 - N  PRINT LINE (OR HEADING OR MIDLINE)                      *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* LEV 95 BGRI JAN03/05 AD-ID                                          *         
* LEV 96 SMUR MAR23/06 USE DEFAULT TIME FOR PQ RETAINS                *         
* LEV 98 ABEA JUN20/07 SKIP SWITCH AND OPEN CHECK IF OFFLINE          *         
* LEV 99 SMUR DEC07/07 LANDSCAPE                                      *         
*             DEC14/07 BYPASS TRAFFIC=NO BUYS                         *         
* LEV101 MNAS AUG13/08 ARCHIVING NOW REPORTS                          *         
* LEV102 MHER JUL08/09 ADID SUPPORT                                   *         
* LEV104 MNAS MAR03/11 HONOR TW PROFILE FOR ID FDMFDA WHEN COMING     *         
*             FROM ANOTHER SCREEN - ELSE VALUES GET CARRIED OVER      *         
* LEV105 MNAS JAN22/13 MORE BANDS                                     *         
* LEV106 SMUR JAN05/16 NEW BAND CM FOR IHEART RADIO                   *         
* LEV107 SMUR JAN20/18 SAVE ESTIMATE ON SHIP RECORD SPEC-19499 (18.2) *         
* SPEC-33109 SMUR 02/25/18 OBTAIN STORAGE FOR LARGER SPTABLE FOR 499  *         
***********************************************************************         
         EJECT                                                                  
T2164A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2164A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTRRR                                                        
                                                                                
         L     RF,=V(PQPROF)                                                    
         AR    RF,R3                                                            
         ST    RF,PQPROF                                                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERM                                     
         BE    INIT10                                                           
*                                                                               
* FIND DCB ADDRESS AND SEE IF READ ONLY                                         
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    INIT10                                                           
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DTFAD',=C'SPTDIR'                                
*                                                                               
         L     RE,12(,R1)          GET ADDR OF DCB                              
*                                                                               
         TM    ISFOPEN-ISDTF(RE),ISFORO+ISFONOP  READ ONLY OR NOP               
         BNZ   WRITERR                                                          
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO SPOT TRAFFIC SYSTEM           
*                                                                               
INIT10   LA    RE,ASVNEXT                                                       
         ST    RE,ASVSTOR                                                       
*                                                                               
* MAKE SURE NOT EXCEEDING SYSD STORAGE *                                        
*                                                                               
         AH    RE,=H'6144'        LENGTH OF SAVED AREA                          
         LR    RF,R9               START OF SAVED AREA                          
         A     RF,LSYSD                                                         
         CR    RE,RF              SEE IF PAST END OF SYSD                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,STABLE                                                        
         LR    R1,R0                                                            
         LA    R1,L'STABLE(,R1)                                                 
         LA    RE,SPTABLE                                                       
         LR    RF,RE                                                            
         AHI   RF,L'SPTABLE                                                     
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         STM   RE,RF,ASPTABL       SAVE ADDRESS FOR ONLINE                      
         B     INIT20                                                           
*                                                                               
         L     RE,TWADCONS                MAKE SURE OUTPUT DCB ADDRESS          
         L     R2,TSPFUSER-TWADCOND(,RE)  SET FOR OFFLINE JOBS                  
         LA    R2,8(,R2)                                                        
         ST    R2,AERRFIL          WILL BE DCB ADDRESS                          
*                                                                               
         L     RF,VADUMMY                                                       
         MVC   0(8,RF),=C'*SHPLST*'                                             
         LA    RE,8(RF)                                                         
         AHI   RF,21000                                                         
         STM   RE,RF,ASHPLIST                                                   
*                                                                               
         LR    R1,RF                                                            
         MVC   0(8,R1),=C'*STATBL*'                                             
         LA    R0,8(,R1)                                                        
         AHI   R1,30000                                                         
*                                                                               
         BAS   RE,GETSTROF         GET STORAGE OFFLINE FOR STA TABLE            
*                                                                               
INIT20   STM   R0,R1,ASTABL                                                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE INSTRUCTIONS                         
         BE    LRR                                                              
         CLI   MODE,RUNFRST                                                     
         BE    GENAUTO                                                          
         CLI   MODE,RUNLAST                                                     
         BE    CLSAUTO                                                          
EXIT     XIT1                                                                   
         EJECT                                                                  
* OPEN AUTOREQ FILE AT RUNFRST *                                                
*                                                                               
GENAUTO  CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         TM    WHEN,X'C0'          TEST IMMED/NOW                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   SEQNUM,=H'1'                                                     
         L     RE,ATWA                                                          
         MVI   29(RE),X'02'        INDICATE RUNLAST HOOK REQUIRED               
*                                                                               
         L     RE,TWADCONS         MAKE SURE OUTPUT DCB ADDRESS SET             
         USING TWADCOND,RE          FOR OFFLINE JOBS                            
         L     R2,TSPFUSER                                                      
         DROP  RE                                                               
         MVC   0(8,R2),=C'*ERRFIL*'                                             
         LA    R2,8(,R2)                                                        
         ST    R2,AERRFIL          WILL BE DCB ADDRESS                          
         L     RF,=A(ERRFILE)                                                   
         A     RF,SPTRRR                                                        
         MVC   0(128,R2),0(RF)     MOVE ERRFILE DCB                             
*                                                                               
         ST    R2,AERRFIL          SET DCB ADDRESSES                            
*                                                                               
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF               OPEN OK                                      
         BZ    EXIT                                                             
         DC    H'0'                OPEN FAILED                                  
*                                                                               
* CLOSE AUTOREQ FILE AND PROCESS AT RUNLAST *                                   
*                                                                               
CLSAUTO  L     R2,AERRFIL                                                       
         CLOSE ((2),)                                                           
         FREEPOOL (R2)                                                          
*                                                                               
*  CALL OVLY 96 TO GENERATE AUTO REQUESTS *                                     
*                                                                               
*        LA    R1,DMCB                                                          
*        L     RE,VADUMMY                                                       
*        A     RE,SPTRRR                                                        
*        ST    RE,0(R1)                                                         
*        MVC   4(4,R1),=X'D9021696'                                             
*                                                                               
*        GOTO1 CALLOV              CALL OVERLAY TO PROCESS                      
*        CLI   4(R1),X'FF'                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        L     RF,0(R1)                                                         
*        GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
* CK IF ANY EASYLINK STUFF THAT MIGHT GET LOST *                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
                                                                                
         TM    WHEN,X'C0'          TEST IMMED/NOW                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,TWAMASTC                                                   
         BZ    EXIT                                                             
         OC    MCREMOTE-MASTD(,RF),MCREMOTE-MASTD(RF)                           
         BNZ   EXIT                                                             
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(RE)                                         
         CLI   140(RE),C'Y'        SEE IF ANY EASYLINK STUFF PRINTED            
         BNE   EXIT                 NO                                          
*                                                                               
* OPEN DIRECT ENTRY (WHICH WILL BE LOST)  TO PRESERVE REAL QUE ENTRY *          
*                                                                               
         L     R4,MCVREMOT-MASTD(,RF)                                           
         USING REMOTED,R4                                                       
*                                                                               
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVC   REMOTJID,=C'STN'                                                 
         MVC   REMOTDST,TWAORIG                                                 
         MVI   REMOTCLS,C'A'                                                    
         MVI   REMOTCPY,1                                                       
*                                                                               
         GOTO1 OPENPQ                                                           
         MVC   P(5),=C'DUMMY'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPMODE,X'FE'        DELETE THIS ENTRY                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
         XC    REMOTKEY,REMOTKEY                                                
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
* CHECK FOR ANY CHANGES TO KEY FIELDS                                           
*                                                                               
VK       DS    0H                                                               
         MVI   SVQLTYP1,0                                                       
         MVI   SVFAXARC,0                                                       
         XC    WORK,WORK                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'S'                                                     
         MVC   PROFKEY+1(2),=C'TQ'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 PQPROF,DMCB,(X'80',PROFKEY),(0,WORK),ACOMFACS                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WORK                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP1,REMOTTY1                                                
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   GEN12                                                            
         CLI   REMOTSUB+1,C'N'                                                  
         BE    GEN12A                                                           
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   GEN12                                                            
         OI    SVFAXARC,REMOTARQ                                                
         B     GEN12A                                                           
GEN12    OI    SVFAXARC,REMOTAEQ                                                
GEN12A   DS    0H                                                               
         DROP  R4                                                               
*                                                                               
         MVI   PQSW,1              SUPPRESS AUTO PRTQUE OPEN                    
         CLI   TRLSTPGM,X'4A'      WAS THIS LAST PROGRAM ACTIVE                 
         BNE   VK10                  NO, FORCE REVALIDATE                       
         TM    TRAMEDH+4,X'20'     MEDIA CHANGED                                
         BZ    VK10                  YES REVALIDATE                             
         TM    TRACLTH+4,X'20'     CLIENT CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPRDH+4,X'20'     PRODUCT CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPTRH+4,X'20'     PARTNER CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAMKTH+4,X'20'     MARKET CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAESTH+4,X'20'     ESTIMATE CHANGED                             
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPERH+4,X'20'     PERIOD CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAOPTH+4,X'20'     OPTIONS CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRASHIPH+4,X'20'    SHIP CHANGED                                 
         BZ    VK10                  YES REVALIDATE                             
         TM    TRACONTH+4,X'20'    CONTACT CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAFAXH+4,X'20'     FAX CHANGED?                                 
         BZ    VK10                  YES REVALIDATE                             
         EJECT                                                                  
         BAS   RE,RDTWA            RESTORE STATION TABLE, ETC                   
*                                                                               
         TM    TRACONTH+4,X'20'    CONTACT CHANGED                              
         BO    CSEL                  NO                                         
*                                                                               
         BAS   RE,VCON             REVALIDATE CONTACT                           
*                                                                               
* TSAR CLEAR HERE                                                               
*                                                                               
CSEL     DS    0H                                                               
         XC    TRAPQID,TRAPQID     CLEAR COPY ID                                
         OI    TRAPQIDH+6,X'80'                                                 
*                                                                               
         CLI   SVTWPR09,C'2'       REQUIRE AGENCY COPY                          
         BNE   CSEL05                                                           
*                                                                               
         XC    PRTCT,PRTCT                                                      
         XC    TSARBYTE,TSARBYTE                                                
*                                                                               
         LH    RE,=AL2(TSARBLK-SYSD)                                            
         AR    RE,R9                                                            
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R2                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,2                                                         
         OI    TSINDS,TSINODSK                                                  
         MVI   TSRECL+1,138                                                     
         OI    TSRECI,TSRVAR                                                    
         MVI   TSPAGN,28        NUMBER OF TEMPSTR PAGES TO BE USED              
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
*                                                                               
         MVI   TSACTN,TSAINI       SET 1ST TSAR FOR INIT                        
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVREMUSR,REMUSER                                                 
         DROP  R2                                                               
*                                                                               
* CHECK FOR UNPROTECTED SELECT FIELDS                                           
*                                                                               
CSEL05   LA    R2,TRASEL1H                                                      
         L     R3,ASTABL                                                        
         USING STABLED,R3                                                       
         NI    UPDSW,X'FF'-X'10'   SET OFF RETRANSMIT THIS SCREEN               
CSEL10   TM    1(R2),X'20'         PROTECTED                                    
         BO    CSEL14              YES, DONE BEFORE OR NTH LINE FOR STA         
         CLI   5(R2),0             ANY ENTRY                                    
         BE    CSEL30                                                           
         CLI   8(R2),C'S'          SELECT (PRINT INSTR)                         
         BNE   CSEL14              NO                                           
         OI    UPDSW,X'10'         SET TO RETRANSMIT THIS SCREEN                
*                                                                               
         CLC   TSARBYTE,TSARBMAX    MORE THAN MAX ALLOWED?                      
         BNL   CSEL30                                                           
         CLC   PRTCT,PRTCTMAX       MORE THAN MAX ALLOWED?                      
         BNL   CSEL30                                                           
*                                                                               
         XC    GERROR,GERROR                                                    
*                                                                               
         BAS   RE,GEN              GO GENERATE INSTRUCTIONS                     
         BNE   CSEL30                                                           
*                                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   MAXIOERR             YES                                         
*                                                                               
* ARE WE ABOUT TO EXCEED MAX IO COUNT                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R4,R4                                                            
         ICM   R4,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         SH    R4,=H'1000'                                                      
         CLM   R4,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BNH   MAXIOERR             YES                                         
         DROP  R1                                                               
*                                                                               
CSEL14   LA    R2,NEXTLINE(,R2)                                                 
         CLI   0(R2),9                                                          
         BNH   CSEL15                                                           
         ZIC   RE,0(R2)                                                         
         LA    RF,0(RE,R2)                                                      
         OC    8(L'TRADSP1,RF),8(RF)   ANYTHING HERE                            
         BNZ   CSEL10                   NO, NO SEL CODE NEEDED                  
*                                                                               
* TSAR PRINT HERE                                                               
*                                                                               
CSEL15   OC    PRTCT,PRTCT         ANY COPIES TO PRINT?                         
         BZ    CSEL16               NO                                          
*                                                                               
         GOTO1 =A(PRTC),RR=SPTRRR                                               
*                                                                               
         BAS   RE,DCPY                                                          
*                                                                               
CSEL16   A     R3,ASVNEXT          GET STARTING POINT IN TABLE                  
         OC    0(4,R3),0(R3)       AT END OF TABLE                              
         BNZ   CSEL18                                                           
         MVC   GERROR,=Y(ENDMSG2)                                               
         MVI   GMSGTYPE,C'I'                                                    
         LA    R2,TRAMEDH                                                       
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALIDATED                          
         B     TRAPERR2                                                         
*                                                                               
CSEL18   TM    UPDSW,X'10'         SET TO RETRANSMIT THIS SCREEN                
         BZ    LRL                  NO - GO ON TO NEXT SCREEN IF ANY            
*                                                                               
         MVC   GERROR,=Y(SELMSG2)                                               
         MVI   GMSGTYPE,C'I'                                                    
         OI    TRATABH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATABH+6,X'80'     AND TRANSMIT                                 
         B     COMXIT                                                           
*                                                                               
* TSAR PRINT HERE *                                                             
*                                                                               
CSEL30   OC    PRTCT,PRTCT                                                      
         BZ    MISSELER                                                         
*                                                                               
         GOTO1 =A(PRTC),RR=SPTRRR                                               
*                                                                               
         BAS   RE,DCPY                                                          
*                                                                               
         OI    TRATABH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATABH+6,X'80'     AND TRANSMIT                                 
*                                                                               
         OC    GERROR,GERROR       ANY ERROR OCCUR                              
         BNZ   TRAPERR2                                                         
*                                                                               
         CLC   TSARBYTE,TSARBMAX    MORE THAN MAX ALLOWED?                      
         BNL   MAXTSAR                                                          
         CLC   PRTCT,PRTCTMAX       MORE THAN MAX ALLOWED?                      
         BNL   MAXTSAR                                                          
*                                                                               
         B     MISSELER                                                         
*                                                                               
MAXIOERR DS    0H                                                               
         OI    TRATABH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATABH+6,X'80'     TRANSMIT                                     
         MVC   GERROR,=Y(MOPROCES)                                              
         B     TRAPERR2                                                         
*=============================================================                  
* GET STORAGE FOR OFFLINE                                                       
*=============================================================                  
*                                                                               
GETSTROF NTR1                                                                   
         CLI   MODE,RUNLAST                                                     
         JE    EXIT                                                             
*                                                                               
         ICM   R1,15,ASPTABL       ANY BUFFER ACQUIRED YET                      
         JNZ   EXIT                                                             
*                                                                               
         MVC   SPTTSIZE,=F'500000'                                              
*                                                                               
         L     R0,SPTTSIZE         WAS 60000                                    
         STORAGE OBTAIN,LENGTH=(R0),LOC=24,COND=YES                             
         LTR   RF,RF               ERROR?                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R0,R1               R0=LEN OF STORAGE                            
         ST    R0,ASPTABLX                                                      
         MVC   0(8,R1),=C'*SPTTBL*'  R1=A(STORAGE)                              
         LA    R1,8(R1)                                                         
         ST    R1,ASPTABL                                                       
         J     EXIT                                                             
*                                                                               
*                                                                               
*=============================================================                  
* RELEASE STORAGE FOR OFFLINE                                                   
*=============================================================                  
*                                                                               
RELSTR   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         L     R0,SPTTSIZE                                                      
         L     R1,ASPTABL                                                       
         LTR   R1,R1                                                            
         JZ    EXIT                                                             
         SHI   R1,8                                                             
         STORAGE RELEASE,LENGTH=(R0),ADDR=(R1),COND=YES                         
         LTR   RF,RF               ERROR?                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    ASPTABL,ASPTABL                                                  
         J     EXIT                                                             
*                                                                               
DCPY     NTR1                                                                   
         OI    TRAPQIDH+6,X'80'                                                 
         MVC   TRAPQID(3),REMUSER                                               
         MVI   TRAPQID+3,C','                                                   
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         LA    R4,TRAPQID+4                                                     
         EDIT  (R0),(4,(R4)),ALIGN=LEFT                                         
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VK10     DS   0H                                                                
         XC    ELDATE(ROTDAYS+2-ELDATE),ELDATE                                  
         LA    R0,CLEARAR                                                       
         LA    R1,CLEARARX-CLEARAR                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BZ    *+18                                                             
         OC    MCREMOTE-MASTD(,RE),MCREMOTE-MASTD(RE)                           
         BZ    *+8                                                              
         MVI   DIRPRTSW,C'Y'                                                    
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
*                                                                               
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GET PROFILE RECORD(S)                        
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   PRDNM,WORK+5                                                     
         OI    4(R2),X'20'         SET ON VALIDATED                             
         EJECT                                                                  
VK20     LA    R2,TRAPTRH          PARTNER                                      
         MVC   QPRD2,SPACES                                                     
         MVI   BPRD2,0                                                          
         MVC   PRD2NM,SPACES                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                                                             
         CLC   =C'NONE',TRAPTR     NO PIGGYBACKS                                
         BNE   VK24                                                             
         MVI   BPRD2,X'FF'                                                      
         B     VK30                                                             
*                                                                               
VK24     GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',QPRD                                                     
         BNE   *+14                                                             
         MVC   GERROR,=Y(PTRINV)                                                
         B     TRAPERR2                                                         
*                                                                               
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   PRD2NM,WORK+5                                                    
VK30     OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* EDIT MARKET *                                                                 
*                                                                               
         LA    R2,TRAMKTH          MARKET                                       
         XC    BMKTSTA,BMKTSTA                                                  
         XC    QMKT,QMKT                                                        
         XC    RQBMKT,RQBMKT                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK36                                                             
*                                                                               
         GOTO1 VALIMKT                                                          
         MVC   RQBMKT,BMKT         REQUESTED MKT (CANADA MKT = 0 VALID)         
*                                                                               
VK36     OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* EDIT ESTIMATE *                                                               
*                                                                               
VK40     LA    R2,TRAESTH                                                       
*                                                                               
         XC    SVESTAB,SVESTAB                                                  
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
         CLI   5(R2),0                                                          
         BNE   VK44                                                             
VK42     CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BNE   VK50                                                             
         MVC   GERROR,=Y(ESTREQD)                                               
         BAS   RE,CLRSCR                                                        
         B     TRAPERR2                                                         
*                                                                               
VK44     GOTO1 ANY                                                              
         CLC   =C'NO ',WORK                                                     
         BE    VK42                                                             
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    *+12                 NEEDS EST                                   
         CLI   SVPROF15,C'Y'       TRAFFIC BUYS DON'T USE ESTIMATES             
         BE    VK50                   UNLESS COPY CODE = ESTIMATE               
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   QBEST,ACTUAL        SET AS EST                                   
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   NOESTER                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BE    *+14                                                             
         MVC   GERROR,=Y(ESTHASCC)                                              
         B     TRAPERR2                                                         
*                                                                               
VK46     GOTO1 DATCON,DMCB,(0,ESTART),(3,SVPERST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVPEREND)                                  
         MVC   ESTDESC,EDESC                                                    
VK50     OI    4(R2),X'20'                                                      
         DROP  R6                                                               
         EJECT                                                                  
* EDIT PERIOD *                                                                 
*                                                                               
         LA    R2,TRAPERH          PERIOD                                       
         MVI   TABLESW,0           RESET TABLE BUILT                            
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
*                                                                               
         GOTO1 =A(VPER),RR=SPTRRR    VALIDATE DATE(S) AND FLIGHT                
*                                                                               
VK60     LA    R2,TRAOPTH                                                       
*                                                                               
         GOTO1 =A(VOPT),RR=SPTRRR    GO VALIDATE OPTIONS                        
         OI    4(R2),X'20'                                                      
*                                                                               
VK70     LA    R2,TRASHIPH                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK74                 NO                                          
         CLI   8(R2),C'Y'          YES                                          
         BE    VK76                                                             
         CLI   8(R2),C'N'          NO SHIP                                      
         BE    VK76                                                             
         CLI   8(R2),C'A'          AUTO-SHIP                                    
         BE    VK76                                                             
         MVC   GERROR,=Y(BADSHIP)                                               
         BAS   RE,CLRSCR                                                        
         B     TRAPERR2                                                         
*                                                                               
VK74     MVC   TRASHIP(1),SVPROF6  MOVE PROFILE OPTION                          
         OI    6(R2),X'80'                                                      
VK76     MVC   SHIPYORN,TRASHIP                                                 
         OI    4(R2),X'20'                                                      
*                                                                               
* CK FORMAT HERE WHEN IT IS USED *                                              
*                                                                               
VK80     LA    R2,TRACONTH                                                      
*                                                                               
         BAS   RE,VCON             REVALIDATE CONTACT                           
*                                                                               
* VALIDATE FAX *                                                                
*                                                                               
         GOTO1 =A(VFAX),RR=SPTRRR                                               
*                                                                               
* READ ESTIMATE HEADERS AND BUILD LIST OF ELIGIBLE ESTIMATES *                  
*                                                                               
         GOTO1 =A(BLEST),RR=SPTRRR                                              
         BE     VK84                                                            
*                                                                               
         CLI   SVTBPR04,C'Y'       ARE NO ESTIMATES ALLOWED                     
         BNE   NOESTER                                                          
         MVI   SVESTAB,X'FF'                                                    
         MVC   SVESTAB+1(L'SVESTAB-1),SVESTAB                                   
*                                                                               
* READ BUYS, LOOKING FOR ALL BUYS, AND BUILD STATION TABLE *                    
*                                                                               
VK84     DS    0H                                                               
         GOTO1 =A(BLSTA),RR=SPTRRR                                              
         BNE   NOSPTER             IF NONE FOUND, ERROR                         
*                                                                               
         OI    TRAPERH+4,X'20'     SET ON VALIDATED                             
*                                                                               
         MVI   TRLSTPGM,X'4A'      SET AS LAST PROGRAM ACTIVE                   
*                                                                               
         L     R3,ASTABL                                                        
         MVC   QUWHEN,CONWHEN                                                   
         TM    WHEN,X'38'          TEST OVERNIGHT/SOON/DDS                      
         BZ    LRL                 GO BUILD SCREEN                              
         CLI   OFFLINE,C'Y'                                                     
         BE    VKX                                                              
         EJECT                                                                  
         TM    SVOPT1,OPTSEED      OPT SEED TO BE RUN, ACCEPT REQUEST           
         BO    VK94                                                             
*                                                                               
* SEE IF ANY UNASSIGNED SPOTS                                                   
*                                                                               
         USING STABLED,R3                                                       
VK90     CLC   STANOASG,STASPTS    THIS STATION HAVE ALL UNASSGND SPOTS         
         BE    NOASGERA             YES                                         
         LA    R3,STANEXT                                                       
         OC    0(4,R3),0(R3)       EOL                                          
         BNZ   VK90                NO                                           
*                                                                               
VK94     NI    TRAMEDH+4,X'FF'-X'20'  SET OFF VALID                             
*                                                                               
VKX      DS    0H                                                               
         BAS   RE,CLRSCR                                                        
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         CLI   SVTWPR09,C'2'                                                    
         BNE   EXIT                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         L     R2,=A(PRTFILO)                                                   
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
* ONLINE INSTRUCTIONS - DISPLAY 1 LINE PER STATION FROM STATION TABLE *         
*                                                                               
         USING STABLED,R3                                                       
LRL      DS   0H                                                                
         BAS   RE,CLRSCR                                                        
*                                                                               
         LA    R2,TRASEL1H         1ST DISPLAY LINE                             
*                                                                               
* DISPLAY LINE FOR STATION ENTRY *                                              
*                                                                               
         USING LSTLINE,R2                                                       
LRL10    NI    1(R2),X'FF'-X'20'   SET OFF PROTECT BIT                          
         OI    6(R2),X'80'          ON TRANSMIT                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 MSUNPK,DMCB,(X'80',STAMSTA),QMKT,DUB                             
*                                                                               
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
* FORMAT STATION FOR PRINTING *                                                 
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    LRL20                                                            
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    LRL20                                                            
         MVI   3(RE),C' '                                                       
*                                                                               
LRL20    MVC   LSTA,STAPRNT                                                     
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   LSTA(8),STANET                                                   
*                                                                               
         MVC   LMKT,QMKT                                                        
*                                                                               
LRL24    MVC   LPRDSLN(3),STAPROD                                               
         LA    R4,LPRDSLN+2        POINT TO END OF PRD                          
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'-'                                                       
         ZIC   R0,STASLN                                                        
         EDIT  (R0),(3,2(R4)),ALIGN=LEFT                                        
*                                                                               
         OC    STAPROD2,STAPROD2                                                
         BZ    LRL30                                                            
         MVC   LPTRSLN(3),STAPROD2                                              
         LA    R4,LPTRSLN+2        POINT TO END OF PRD                          
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'-'                                                       
*                                                                               
         ZIC   R0,STASLN2                                                       
         EDIT  (R0),(3,2(R4)),ALIGN=LEFT                                        
LRL30    EDIT  (B2,STASPTS),(4,LSPOTS)                                          
         GOTO1 DATCON,DMCB,(2,STAFTD),(8,LFTLCST)                               
         GOTO1 (RF),(R1),(2,STALTD),(8,LLTLCST)                                 
         OC    STANOASG,STANOASG   ANY UNASSIGNED SPOTS                         
         BZ    LRL36                                                            
         EDIT  (B2,STANOASG),(4,LPRTID)                                         
         MVC   LPRTID+5(13),=CL13'UNASSGND SPTS'                                
LRL36    OI    6(R2),X'80'                                                      
         MVC   SVMKTSTA,STAMSTA                                                 
         LA    R3,STANEXT                                                       
         OC    0(4,R3),0(R3)       END OF TABLE                                 
         BZ    LRL50               YES                                          
         C     R3,ASTABLX                                                       
         BNL   LRL50                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             END OF SCREEN                                
         BNH   LRL40               YES                                          
         CLC   SVMKTSTA,STAMSTA                                                 
         BNE   LRL10                                                            
*                                                                               
         OI    6(R2),X'80'+X'20'   SET TRANSMIT & PROTECTED                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     LRL24                                                            
*                                                                               
* AT END OF SCREEN, SEE IF MORE FOR THIS STATION *                              
*                                                                               
LRL40    CLC   SVMKTSTA,STAMSTA                                                 
         BNE   LRL46                                                            
         SR    R2,R0               BACK UP TO PREV                              
         MVC   LPRTID-9(26),=C'** MORE FOR THIS STATION **'                     
LRL44    LA    R3,STANEXT                                                       
         OC    0(4,R3),0(R3)       END OF TABLE                                 
         BZ    LRL50               YES                                          
         CLC   SVMKTSTA,STAMSTA                                                 
         BE    LRL44                                                            
LRL46    L     R0,ASTABL           END OF SCREEN, MORE STATIONS                 
         SR    R3,R0                                                            
         ST    R3,ASVNEXT          SAVE 1ST SPOT FOR NEXT SCREEN                
         B     MOREX                                                            
*RL50    SR    R3,R3               SAVE                                         
LRL50    DS   0H                                                                
         L     R0,ASTABL           END OF SCREEN, MORE STATIONS                 
         SR    R3,R0                                                            
         ST    R3,ASVNEXT          SO CSEL WILL END                             
         B     DONEX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
* GENERATE OFFLINE INSTRUCTIONS HERE                                            
*                                                                               
LRR      L     R3,ASTABL          START OF STATION TABLE                        
         USING STABLED,R3                                                       
*                                                                               
LRR10    CLC   STANOASG,STASPTS    THIS STATION HAVE ALL UNASSGND SPOTS         
         BE    LRR20                                                            
         MVC   SVMKTSTA,STAMSTA    MKT/STA THIS INSTR                           
         GOTO1 =A(BLACT),RR=SPTRRR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(PRT),RR=SPTRRR   PRINT SPOT DETAIL INSTR                      
LRR20    LA    R3,STANEXT          LOOK FOR NEXT STA                            
         OC    0(4,R3),0(R3)       END                                          
         BZ    LRR30                                                            
         C     R3,ASTABLX          TEST END OF TABLE                            
         BNL   LRR30                                                            
         CLC   SVMKTSTA,STAMSTA    SAME MKT/STA LAST INSTR                      
         BE    LRR20                                                            
         B     LRR10                                                            
*                                                                               
LRR30    CLI   SVTWPR09,C'2'       IS AGENCY COPY REQUIRED                      
         BE    *+12                                                             
         BRAS  RE,RELSTR                                                        
         B     EXIT                 NO                                          
*                                                                               
         GOTO1 =A(PRTEZ),RR=SPTRRR PRINT FAX (IF ANY)                           
         BRAS  RE,RELSTR                                                        
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
* PRINT INSTRUCTIONS FOR THIS MARKET/STATION, USER HAS SELECTED *               
* INSTRUCTIONS, NOW FIND FIRST TABLE ENTRY FOR THIS STATION,    *               
* THEN BUILD SPOT TABLE AND PRINT INSTRUCTIONS FOR THIS STATION *               
*****************************************************************               
*                                                                               
         USING STABLED,R3                                                       
         DS    0H                                                               
GEN      NTR1                                                                   
         ZIC   R5,0(R2)                                                         
         LA    R5,0(R2,R5)                                                      
         USING LSTLINE,R5                                                       
         MVC   STANET,SPACES                                                    
         MVC   STANET(4),LSTA                                                   
*                                                                               
         CLI   LSTA+4,C'/'         CABLE HEAD                                   
         BNE   *+14                                                             
         MVC   STANET+4(4),LSTA+4                                               
         B     GEN03                                                            
*                                                                               
         LA    R1,LSTA+5                                                        
         CLI   STANET+3,C'-'                                                    
         BNE   GEN02                                                            
         MVI   STANET+3,C' '                                                    
         BCTR  R1,0                                                             
GEN02    MVC   STANET+4(1),0(R1)                                                
*                                                                               
GEN03    GOTO1 MSPACK,DMCB,=C'0000',STANET,SVMKTSTA                             
*                                                                               
GEN04    CLC   STAMSTA+2(3),SVSTA                                               
         BE    GEN06                                                            
*                                                                               
         CLI   SPOTCAN,C'C'        THIS CANADIAN                                
         BNE   GEN05                NO                                          
*                                                                               
         MVC   DUB(3),STAMSTA+2                                                 
         MVC   DUB+3(3),SVSTA                                                   
         NI    DUB+2,X'80'                                                      
         NI    DUB+5,X'80'                                                      
         CLC   DUB(3),DUB+3                                                     
         BE    GEN06                                                            
*                                                                               
GEN05    LA    R3,STANEXT                                                       
         OC    0(4,R3),0(R3)       AT END OF TABLE                              
         BNZ   GEN04                                                            
         DC    H'0'                                                             
GEN06    CLC   STANOASG,STASPTS    ALL UNASSIGNED SPOTS                         
         BE    NOASGER              YES, CAN'T PRINT INSTRUCTIONS               
         MVC   SVMKTSTA,STAMSTA                                                 
         GOTO1 =A(BLACT),RR=SPTRRR                                              
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,SVTWA                                                         
         BAS   RE,PRT                                                           
*                                                                               
* DISPLAY SPOOL ID FOR USER *                                                   
*                                                                               
         OI    6(R5),X'80'         SET XMT                                      
         MVC   LPRTID(20),SPACES                                                
         MVC   LPRTID(3),=C'ID='                                                
         MVC   LPRTID+3(3),REMUSER                                              
         CLI   TRAFAX,C'N'                                                      
         BE    *+10                                                             
         MVC   LPRTID+3(3),=C'SWX'                                              
         MVI   LPRTID+6,C','                                                    
         EDIT  (B2,SPOOLRPN),(4,LPRTID+7),ALIGN=LEFT                            
*                                                                               
         TM    INSPRTSW,INSPRTNO   DID WE GENERATE INSTRUCTIONS                 
         BZ    GEN10                                                            
         MVC   LPRTID(17),=CL17'NO DATA GENERATED'                              
*                                                                               
* MODIFY SELECT FIELD *                                                         
*                                                                               
GEN10    XC    8(3,R2),8(R2)       BLANK IT                                     
         MVI   8(R2),C'*'          MARK DONE                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'20'         AND CHANGE TO PROTECTED                      
*                                                                               
         CR    RE,RE               SET CONDITION CODE                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES CONTACT AND READS CONTACT REC IF CON=  *                 
***************************************************************                 
*                                                                               
         DS    0H                                                               
VCON     NTR1                                                                   
         LA    R2,TRACONTH                                                      
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         XC    CONFAX,CONFAX                                                    
         GOTO1 ANY                                                              
         MVC   QUESTOR(L'TRACONT),TRACONT                                       
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   VCON20                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
*                                                                               
         CLI   5(R2),12            NO MORE THAN 8 CHARS ALLOWED                 
         BNH   *+12                                                             
         LA    R1,7                                                             
         B     VCON05                                                           
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         AHI   R1,-5               MINUS 4 (CON=) 1 (FOR EX)                    
*                                                                               
VCON05   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTKNAME(0),12(R2)                                               
*                                                                               
         MVC   CNTKNAME,12(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCON10                                                           
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOTFND                                                           
*                                                                               
VCON10   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   VCON20                                                           
         USING CNTFAXEL,R6                                                      
         MVC   CONFAX,CNTFTEL                                                   
VCON20   OI    TRACONTH+4,X'20'                                                 
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
*                                                                               
         DS    0H                                                               
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         B     COMTWA                                                           
*                                                                               
* RESTORE STABLE                                                                
*                                                                               
         DS    0H                                                               
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMREAD'                                                    
COMTWA   ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCR   LA    RF,TRASEL1H                                                      
*                                                                               
CLRSCR10 OC    8(L'TRASEL1,RF),8(RF)                                            
         BZ    CLRSCR20                                                         
         CLC   8(L'TRASEL1,RF),SPACES                                           
         BE    CLRSCR20                                                         
         XC    8(L'TRASEL1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         OI    1(RF),X'20'         SET PROTECT BIT                              
CLRSCR20 ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
*                                                                               
         OC    8(L'TRADSP1,RF),8(RF)                                            
         BZ    CLRSCR30                                                         
         CLC   8(L'TRADSP1,RF),SPACES                                           
         BE    CLRSCR30                                                         
         XC    8(L'TRADSP1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
CLRSCR30 IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),9                                                          
         BH    CLRSCR10                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
* GET PROFILE REC(S)                                                            
*                                                                               
         DS    0H                                                               
FPRO     NTR1                                                                   
*                                                                               
* READ TW PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TW'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         MVC   SVTWPR09,SVPROF+8   SPOT GEN FAX - Y,N,2 (AUTH)                  
*                                                                               
* READ T1 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),WORK,SVT1PROF,DATAMGR                                  
*                                                                               
* READ TZ PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'Z'                                                      
         GOTO1 (RF),(R1),WORK,SVPROF,DATAMGR                                    
         MVC   SVTZPR01,SVPROF     FLEXIBLE DATES PROFILE                       
         MVC   SVTZPR03,SVPROF+2   SORT BY DATE/TIME                            
         MVC   SVTZPR04,SVPROF+3   SHOW PRODUCTS IN SAME ORDER AS BUY           
*                                                                               
* READ TB PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'B'                                                      
         GOTO1 (RF),(R1),WORK,SVPROF,DATAMGR                                    
*                                                                               
         MVC   SVTBPR04,SVPROF+3   NO ESTIMATES REQUIRED                        
*MNMB                                                                           
*                                                                               
* READ T3 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),WORK,SVPROF,DATAMGR                                    
         MVC   SVT3PR06,SVPROF+5                                                
*MNMB                                                                           
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'0'                                                      
         GOTO1 (RF),(R1),WORK,SVPROF,DATAMGR                                    
         B     EXIT                                                             
         EJECT                                                                  
*        ERROR ROUTINES                                                         
*                                                                               
NOASGERA LA    R2,TRAMEDH                                                       
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALID                              
NOASGER  MVC   GERROR,=Y(UNASSPOT)                                              
         B     COMXIT                                                           
*                                                                               
NOSPTER  MVC   GERROR,=Y(NOSPTSEL)                                              
         LA    R2,TRAMEDH                                                       
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALIDATED                          
         BAS   RE,CLRSCR                                                        
         B     TRAPERR2                                                         
*                                                                               
MOREX    MVC   GERROR,=Y(MOREMSG)                                               
         B     BOTHX                                                            
*                                                                               
MAXTSAR  MVC   GERROR,=Y(MXTSARMS)                                              
         B     TRAPERR2                                                         
*                                                                               
WRITERR  MVC   GERROR,=Y(SPTRDONL) SPOT FILE ARE READ ONLY                      
         B     TRAPERR2                                                         
*                                                                               
DONEX    MVC   GERROR,=Y(DONEMSG)                                               
*                                                                               
BOTHX    MVI   GMSGTYPE,C'I'                                                    
*                                                                               
         LA    R2,TRASEL1H                                                      
COMXIT   BAS   RE,SVTWA                                                         
*                                                                               
TRAPERR2 GOTO1 VTRAERR                                                          
         EJECT                                                                  
NOTFND   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
NOESTER  MVI   ERROR,NOESTS                                                     
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALIDATED                          
         B     TRAPERR                                                          
*                                                                               
MISSELER TM    UPDSW,X'10'         ANY UPDATES DONE                             
         BZ    *+8                  NO                                          
         BAS   RE,SVTWA            SAVE UPDATED SPOTTABLE                       
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
*                                                                               
TRAPERR  DS   0H                                                                
         BAS   RE,CLRSCR                                                        
         GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
*                                                                               
TSARBMAX DC    F'120000'            MAX BYTES FOR TSAR                          
PRTCTMAX DC    H'1400'              MAX PRINT LINES TSAR                        
         EJECT                                                                  
* DCB FOR OFF-LINE PRINT FILE, PRINT ONCE TO EASYLINK, ONCE TO                  
* NORMAL SPOOL FROM PRTFILE                                                     
*                                                                               
         DS    0D                                                               
PRTFILO  DCB   DDNAME=PRTFILE,DSORG=PS,RECFM=FB,BLKSIZE=13200,         X        
               LRECL=132,MACRF=PM                                               
*                                                                               
PRTFILI  DCB   DDNAME=PRTFILE,DSORG=PS,RECFM=FB,BLKSIZE=13200,         X        
               LRECL=132,MACRF=GM,EODAD=ENDEZPRT                                
*                                                                               
* MODEL DCB FOR AUTO REQUEST FILE. COPIED INTO CORE RESIDENT AREA               
* BEFORE OPEN                                                                   
*                                                                               
ERRFILE  DCB   DDNAME=ERRFILE,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=64,MACRF=PM                                                
         EJECT                                                                  
*********************************************************************           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON REQUESTED DATES *           
*********************************************************************           
*                                                                               
         DROP  RB,R7,RC                                                         
BLEST    NMOD1 0,**BLEST*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    SVESTAB,SVESTAB                                                  
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD AND BCLT                                         
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
         CLI   KEY+7,0                                                          
         BNE   *+8                                                              
         MVI   KEY+7,1                                                          
*                                                                               
BLEST2   DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLEST10                                                          
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLEST4                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,SVQEND       EST START AFTER REQ END                      
         BH    BLEST4                                                           
         CLC   EEND,SVQSTART       EST END BEFORE REQ START                     
         BL    BLEST4                                                           
         ZIC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),X'FF'                                                      
*                                                                               
BLEST4   MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         CLI   QBEST,0             TEST NO ESTIMATE REQUEST                     
         BE    BLEST2              YES - CONTINUE                               
         EJECT                                                                  
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
*                                                                               
BLEST10  GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         OC    SVESTAB,SVESTAB     TEST ANY DATA FOUND                          
         BZ    NEQXIT              NO - RETURN WITH CC SET                      
*                                                                               
         MVC   BEST(2),QBESTEND    SET ACTUAL ESTIMATE NUMBERS                  
*                                                                               
         CLI   QBEST,0             TEST EST=NO REQUEST                          
         BNE   EQXIT                                                            
*                                                                               
         LA    RE,SVESTAB                                                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         LA    R1,SVESTAB                                                       
         SR    RE,R1                                                            
         STC   RE,BEST                                                          
*                                                                               
         LA    RE,SVESTAB+255                                                   
         LA    R0,255                                                           
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,BESTEND                                                       
EQXIT    CR    RE,RE                                                            
         B     BLESTX                                                           
NEQXIT   CR    R7,RB                                                            
BLESTX   XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE FAX                                                                  
*                                                                               
VFAX     NMOD1 0,**VFAX**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         LA    R2,TRAFAXH                                                       
*MN                                                                             
         CLC   TWAORIG,=X'335D'    FDMFDA                                       
         BNE   VF04                                                             
         CLI   TRLSTPGM,X'4A'                                                   
         BNE   VF05                                                             
*MN                                                                             
VF04     CLI   5(R2),0             ANY ENTRY?                                   
         BNE   VF10                                                             
VF05     CLI   SVTWPR09,0          NO ENTRY                                     
         BNE   *+12                                                             
         MVI   8(R2),C'N'                                                       
         B     *+10                                                             
         MVC   8(1,R2),SVTWPR09    USE PROFILE DEFAULT                          
         OI    6(R2),X'80'         FORCE XMIT                                   
         MVI   5(R2),1                                                          
*MN                                                                             
         MVI   TRLSTPGM,X'4A'                                                   
*MN                                                                             
         B     VFX                                                              
*                                                                               
VF10     CLI   8(R2),C'Y'                                                       
         BE    VF20                                                             
         CLI   8(R2),C'2'                                                       
         BE    VF20                                                             
         CLI   8(R2),C'N'                                                       
         BE    VFX                                                              
         MVC   GERROR,=Y(BADFAX)                                                
         B     VFTRAP                                                           
*                                                                               
VF20     CLI   SVTWPR09,C'N'       FAX ALLOWED?                                 
         BE    *+12                 NO                                          
         CLI   SVTWPR09,0          NO FAX IF NO ENTRY                           
         BNE   VFX                                                              
         MVC   GERROR,=Y(NOTWAUTH)                                              
VFTRAP   GOTO1 VTRAERR                                                          
VFX      OI    4(R2),X'20'         SET PRE-VAL                                  
         MVC   SVTWPR09,8(R2)                                                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
****************************************************************                
*                                                              *                
* SUBROUTINE TO PRINT INSTRUCTIONS FOR 1 MARKET/STATION ENTRY  *                
*                                                              *                
* IO1                                                          *                
* IO2 HEAD LINES, FOOTNOTES, COMMERCIAL LIST                   *                
* IO3 USED TO READ COMMERCIALS                                 *                
*                                                              *                
* AFTER HEADLINES PRINTED,                                     *                
* IO2 + IO3 USED TO HOLD DEALER TEXT + CMML LIST               *                
*                                                              *                
****************************************************************                
*                                                                               
* READ PREVIOUS INSTRUCTIONS AND COMPARE *                                      
*                                                                               
PRT      NMOD1 0,**PRT***,R7                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         USING STABLED,R3                                                       
*                                                                               
* READ MARKET REC (IF NEW MARKET) FOR NAME                                      
*                                                                               
         CLC   SVBMKT,BMKT         ON SAME MKT?                                 
         BE    PRT0A                                                            
         MVC   BMKT,SVBMKT                                                      
         ZICM  R0,BMKT,2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(15),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MKTNM,MKTNAME-MKTRECD(R6)                                        
*                                                                               
PRT0A    BAS   RE,RINS             GO READ INSTR REC                            
*                                                                               
         MVC   NEXTADDR,AIO2       ALL QUEUES BUILT IN AIO2                     
*                                                                               
         TM    SVOPT1,OPTREV       TEST REV INST ONLY                           
         BZ    PRT01                                                            
         TM    INSPRTSW,INSPRTCH   TEST CHANGED SPOTS FOUND                     
         BO    PRT04                                                            
         B     PRT02                                                            
*                                                                               
PRT01    TM    SVOPT1,OPTNEW       TEST NEW INST ONLY                           
         BZ    PRT04                                                            
         TM    INSPRTSW,INSPRTPR   TEST PREV INST FOUND                         
         BO    PRT04                                                            
*                                                                               
PRT02    OI    INSPRTSW,INSPRTNO   SET DID NOT PRINT INSTRUCTIONS               
         B     PREXIT                                                           
*                                                                               
* READ STATION ADDRESS RECORD AND SAVE IT *                                     
*                                                                               
PRT04    MVC   AIO,AIO1                                                         
         GOTO1 =A(RSTA),RR=SPTRRR                                               
*                                                                               
         L     RE,NEXTADDR         AND SET AS NEXT ADDRESS                      
         MVC   0(8,RE),=C'FOOTNOTE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASVFTNT          ADVANCE POINTER                              
         MVI   0(RE),0             SET EOL FLAG                                 
         LA    RE,1(RE)                                                         
         ST    RE,NEXTADDR         AND SET AS NEXT ADDRESS                      
*                                                                               
* READ FOOTNOTE TEXT RECORD *                                                   
*                                                                               
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(3),QPRD                                                    
*                                                                               
         MVI   KEY+8,C'F'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    PRT06                                                            
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT06                                                            
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRT10                                                            
*                                                                               
PRT06    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NONE ',2(R6)     TEST SUPPRESS COMMENT                        
         BE    PRT10                                                            
*                                                                               
* FORMAT COMMENT *                                                              
*                                                                               
         L     R4,ASVFTNT                                                       
*                                                                               
PRT08    MVC   0(60,R4),SPACES                                                  
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,60(R4)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    PRT08                                                            
*                                                                               
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         ST    R4,NEXTADDR         SET POINTER                                  
*                                                                               
PRT10    GOTO1 DATCON,DMCB,(3,SVGENST),(X'20',USERQSTR)                         
         GOTO1 (RF),(R1),(3,SVGENEND),(X'20',USERQEND)                          
*                                                                               
         CLI   PQSW,1              TEST PRTQUE OPEN                             
         BE    PRT11                NO                                          
*                                                                               
         CLI   SVTWPR09,C'N'       FAXING?                                      
         BE    PRT12                NO - ONLY 1 PQ ENTRY                        
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   SVTWPR09,C'2'       AGY COPY?                                    
         BE    PRT12                                                            
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)     FORCE CLOSE OF SPOOL                         
         MVI   PQSW,1                                                           
*                                                                               
PRT11    LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
*                                                                               
         CLI   TRAFAX,C'N'                                                      
         BNE   *+14                                                             
         MVC   QLTYP1,SVQLTYP1                                                  
         B     PRT11A                                                           
         TM    WHEN,X'40'                                                       
         BZ    *+10                                                             
         MVC   QLTYP1,SVFAXARC                                                  
PRT11A   DS    0H                                                               
*                                                                               
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
*                                                                               
         CLI   SVTWPR09,C'N'       NO EASYLINK                                  
         BE    EASY05                                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EASY00                                                           
         CLI   SVTWPR09,C'2'       AGY COPY?                                    
         BE    EASY05                                                           
         B     EASY03                                                           
         EJECT                                                                  
* SPECIAL CLASS/DESC FOR EASYLINK                                               
*                                                                               
EASY00   DS    0H                                                               
         MVC   QLDESC(3),QCLT                                                   
         MVC   QLDESC+3(3),QPRD                                                 
         MVC   QLDESC+6(5),QSTA                                                 
         MVI   PLCLASS,C'G'        WESTERN UNION                                
         MVI   QLCLASS,C'G'        ALSO                                         
*                                                                               
         OC    QLTYP1,SVFAXARC                                                  
*                                                                               
         MVC   REMUSER,=C'SWX'                                                  
         B     EASY05                                                           
         DROP  R1                                                               
*                                                                               
EASY03   L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
*                                                                               
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(,RE)                                        
         MVI   140(RE),C'Y'        SET EASYLINK STUFF PRINTED SW                
*                                                                               
         L     R1,136(RE)          PAST SAVED DCB                               
         LA    R1,1(R1)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REMOTFRM+1(3),DUB                                                
         ST    R1,136(RE)                                                       
*                                                                               
         MVC   REMOTJID,=C'SWX'                                                 
*                                                                               
         OC    REMOTTY1,SVFAXARC                                                
*                                                                               
         MVC   REMOTDST,TWAORIG                                                 
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
*                                                                               
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(RE)                                         
         SR    R1,R1                                                            
         ICM   R1,15,136(RE)                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,REMOTSUB       FORCE PRT QUE KEY CHANGE                     
         STCM  R1,15,136(RE)                                                    
         MVI   REMOTCLS,C'G'       SHOULD BE CLASS G                            
         MVI   REMOTCPY,1                                                       
         DROP  RF                                                               
*                                                                               
EASY05   GOTO1 OPENPQ                                                           
*                                                                               
         CLI   SVTWPR09,C'N'       THIS AN EASYLINK TRANSMISSION?               
         BE    PRT12                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        IF ONLINE, SEND EASYLINK                     
         BNE   *+12                                                             
         CLI   SVTWPR09,C'2'       AGY COPY?                                    
         BE    PRT12                YES - DO IT FIRST                           
*                                                                               
* SEND 2 SPECIAL LINES FOR EASYLINK *                                           
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=C'*HDR*'                                                 
*                                                                               
         MVC   EDIDESID(5),QSTA                                                 
*                                                                               
         CLI   QSTA,C'0'           IS THIS A CABLE STATION                      
         BL    *+16                                                             
         MVC   EDIDESID(1),QSTA+4  SEND MEDIA,                                  
         MVC   EDIDESID+1(4),QSTA  THEN 4 DIGITS OF CABLE STATION               
*                                                                               
         MVI   EDIWIDE,C'L'                                                     
         MVI   EDIPAGE,C'P'                                                     
         MVC   EDIFDEST(4),QSTA                                                 
         LA    RE,EDIFDEST+3                                                    
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    EASY10                                                           
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    EASY10                                                           
         MVI   3(RE),C' '                                                       
*                                                                               
EASY10   MVC   EDIBILL(4),QMED    MEDIA & CLIENT                                
         MVC   EDIBILL+4(3),QPRD                                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPINSTRN'                                  
*                                                                               
         MVC   EDISTTMD(4),QMED    MEDIA & CLIENT                               
         MVC   EDISTTPR,QPRD       PRODUCT                                      
         MVC   EDISTTP2,QPRD2      PARTNER                                      
         MVC   EDISTTES,=C'NO '    SET EST=NO UNLESS EST IS USED                
         ZICM  R0,QBEST,1                                                       
         BZ    *+18                LEAVE EST=NO                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDISTTES,DUB                                                     
*                                                                               
         MVC   EDISTTDT(6),USERQSTR    START DATE                               
         MVC   EDISTTDT+6(6),USERQEND  END DATE                                 
         MVC   EDISTTST(5),QSTA    STATION                                      
         MVC   EDISTTCT(16),QUESTOR CONTACT                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
         EJECT                                                                  
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN) *                          
*                                                                               
PRT12    LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         L     R1,=A(HDHK)         HEADING ROUTINE FOR REPORT                   
         A     R1,SPTRRR                                                        
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         L     R1,=A(FTHK)         HEADING ROUTINE FOR REPORT                   
         A     R1,SPTRRR                                                        
         ST    R1,FOOTHOOK         STORE FOR CONTROLLER                         
*NOP     MVI   MAXLINES,58         SET PAGE SIZE                                
         MVI   MAXLINES,42         SET PAGE SIZE (WAS 44)                       
         MVI   FOOTLNS,5           ALLOW 5 LINES OF FOOTLINES                   
         MVI   FOOTSW,0            RESET SWITCH                                 
         MVI   CLEARHED,C'N'       DO NOT CLEAR HEADLINES                       
         MVI   CONTINUE,C'Y'       DO NOT CLEAR HEADLINES                       
         NI    UPDSW,X'FF'-PTCMLTXT                                             
*                                                                               
* BLANK THE HEADLINES NOW *                                                     
*                                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVI   LINE,99             FORCE NEW PAGE WITHOUT FORCEHED              
         MVC   PAGE,=H'1'          WHICH CAUSES FOOTLINES TO REPRINT            
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    PRT15               THEN SHPLIST ADDRESS IS SET                  
*                                                                               
         L     RE,NEXTADDR         POINT TO SHIPPING LIST                       
         LA    RE,7(RE)                                                         
         SRL   RE,3                                                             
         SLL   RE,3                ROUND TO DBLWD                               
         MVC   0(8,RE),=C'*SHPLST*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASHPLIST                                                      
         AHI   RE,50*112           MAX 50 ENTRIES                               
         ST    RE,ASHPLISX                                                      
         LA    RE,1(RE)            ALLOW ROOM FOR 15 FILMS + X'00'              
         ST    RE,NEXTADDR         SAVE UPDATED LIST ADDR                       
*                                                                               
PRT15    L     R0,ASHPLIST                                                      
         L     R1,ASHPLISX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
* READ HEADLINE TEXT RECORD *                                                   
*                                                                               
         L     RE,NEXTADDR                                                      
         XC    0(24,RE),0(RE)      PRECLEAR                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(3),QPRD                                                    
*                                                                               
         MVI   KEY+8,C'H'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    PRT20                                                            
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT20                                                            
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRT30                                                            
*                                                                               
* PRINT COMMENTS (08 = DO GETREC THERE, CK FOR BOXES)                           
PRT20    OI    UPDSW,X'08'                                                      
*                                                                               
         GOTO1 =A(PCMT),DMCB,(60,NEXTADDR),RR=SPTRRR                            
*                                                                               
* PRINT THE HEADLINES *                                                         
*                                                                               
PRT30    BRAS  RE,GOSPL                                                         
*                                                                               
         TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BO    PRT32                                                            
         CLI   SVPROF12,C'*'       TEST SPECIAL MKT/STA COMMENTS                
         BNE   PRT48                                                            
*                                                                               
* FIND AND PRINT SPECIAL MARKET, STATION TEXT OR STATION TYPE *                 
*                                                                               
PRT32    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPROF16                                                
         TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BO    *+10                                                             
         MVC   KEY+5(1),SVPROF12                                                
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   PRT34                                                            
         CLI   QBEST,0                                                          
         BE    PRT34                                                            
         ZIC   R0,QBEST                                                         
         CVB   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   KEY+6(3),=C'ES='                                                 
         UNPK  KEY+9(3),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST ESTIMATE TEXT FOUND                     
         BE    PRT36                                                            
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         XC    KEY+6(7),KEY+6                                                   
*                                                                               
PRT34    GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),FULL,DUB                            
         MVC   KEY+6(5),DUB                                                     
         CLI   QMED,C'T'                                                        
         BNE   *+20                                                             
         MVI   KEY+10,0                                                         
         CLI   KEY+9,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+9,0                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     TEST STATION TEXT FOUND                      
         BE    PRT36                                                            
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         XC    KEY+6(7),KEY+6                                                   
         MVC   KEY+6(5),=C'TYPE='                                               
         MVC   KEY+11(1),SVSTATYP  STATION TYPE                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST STATION TYPE TEXT FOUND                 
         BE    PRT36                                                            
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVBMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+6(4),DUB                                                     
         XC    KEY+10(3),KEY+10                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     TEST MARKET TEXT FOUND                       
         BE    PRT36                                                            
*                                                                               
         MVC   KEY(13),KEYSAVE    RESTORE KEY                                   
         XC    KEY+6(4),KEY+6     CK FOR ALL MKTS/STA                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    TEST ALL MKT/STA STEXT FOUND                  
         BNE   PRT48                                                            
*                                                                               
PRT36    L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRT40    BRAS  RE,NEXTEL                                                        
         BNE   PRT44                                                            
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'                                                         
         EX    RE,PRT40EX                                                       
*                                                                               
         BRAS  RE,GOSPL                                                         
         B     PRT40                                                            
PRT40EX  MVC   P+3(0),3(R6)                                                     
*                                                                               
PRT44    ZIC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT36                                                            
         BRAS  RE,GOSPL                                                         
         EJECT                                                                  
* PRINT MIDLINES HERE FOR PAGE 1 ONLY AS PRINT LINES *                          
*                                                                               
PRT48    MVI   P,0                                                              
*                                                                               
         L     RE,=A(MIDLINE)                                                   
         A     RE,SPTRRR                                                        
         MVC   P2(L'MIDLINE),0(RE)                                              
         MVC   P3(L'MIDLINE),L'MIDLINE(RE)                                      
*                                                                               
         BRAS  RE,GOSPL            PRINT THE 'MIDLINES'                         
*                                                                               
         OI    UPDSW,X'20'         SET ON PRINT SPOTS SW                        
         XC    BLOCK(240),BLOCK    PRODUCT NAME SAVE AREA                       
         XC    BLOCK+240(240),BLOCK+240                                         
*                                                                               
* PRINT THIS SPOT ON INSTRUCTIONS *                                             
*                                                                               
PRT50    BAS   RE,BLIN             GO BUILD PRT LINE(S)/CMML SHIP LIST          
*                                                                               
* WRITE SHIPPING LIST ENTRIES AS REQUIRED *                                     
*                                                                               
         L     R5,ASHPLIST                                                      
         USING SVCMLD,R5                                                        
*                                                                               
         L     R4,NEXTADDR         TO BE SHIPPED LIST HERE                      
         ST    R4,ACMLSHIP                                                      
         XC    0(17,R4),0(R4)                                                   
*                                                                               
* CREATE NEW ELEMENT IN ELEM *                                                  
*                                                                               
PRT60    NI    UPDSW,X'FF'-UPDEST   INIT UPDATED ESTIMATE ELEM                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         USING SHPDTAEL,R6                                                      
*                                                                               
         MVI   0(R6),X'10'                                                      
         MVI   1(R6),SHPDTAX-SHPDTAEL                                           
         MVC   SHPCMML,SVCMLCOD    MOVE 8 BYTE CODE HERE                        
         TM    SVCMLST,X'01'                                                    
         BZ    *+8                                                              
         OI    SHPNOSHP,SHPISADI   SET PACKED FLAG                              
*                                                                               
         CLI   SVCMLPIG,0                                                       
         BE    PRT62X                                                           
         MVC   SHPCMML2,SVCMLCOD+L'SVCMLDTA                                     
                                                                                
* NEED TO MAKE SURE BOTH CODES ARE PACKED IF EITHER CODE IS                     
                                                                                
         TM    SVCMLST,X'01'       TEST FIRST IS PACKED                         
         BZ    PRT62                                                            
         TM    SVCMLST+L'SVCMLDTA,X'01'  TEST SECOND IS PACKED                  
         BO    PRT62X                    YES - FINE                             
         MVC   WORK(8),SHPCMML2                                                 
         MVC   WORK+8(4),SPACES                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SHPCMML2                                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PRT62X                                                           
* FIRST IS NOT PACKED                                                           
PRT62    TM    SVCMLST+L'SVCMLDTA,X'01'  TEST SECOND IS PACKED                  
         BZ    PRT62X                    NO - FINE                              
         OI    SHPNOSHP,SHPISADI                                                
         MVC   WORK(8),SHPCMML                                                  
         MVC   WORK+8(4),SPACES                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SHPCMML                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRT62X   GOTO1 DATCON,DMCB,(2,SVCMLFTD),(3,SHPFTD)                              
         GOTO1 (RF),(R1),(2,SVCMLLTD),(3,SHPLTD)                                
*                                                                               
         GOTO1 (RF),(R1),(5,0),(3,SHPQDATE)                                     
*                                                                               
         CLI   SHIPYORN,C'N'                                                    
         BNE   PRT64                                                            
         OI    SHPNOSHP,X'80'                                                   
         MVC   SHPSHPDT,SHPQDATE   SET SHIP DATE = TODAY                        
*                                                                               
PRT64    CLI   SVCMLPIG,1                                                       
         BNE   *+8                                                              
         MVI   SHPPIG,C'Y'         SET PASSIVE PIGGYBACK                        
*                                                                               
* READ SHIPPING RECAP RECORD *                                                  
*                                                                               
         NI    UPDSW,X'FF'-UPDRECSW    PRESET 'UPDATED' SW TO NO SHIP           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(5),SVMKTSTA                                                
         MVC   KEY+11(2),SVCMLSEQ                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT66                                                            
*                                                                               
* NOT FOUND - CREATE NEW RECORD *                                               
*                                                                               
         L     R6,AIO                                                           
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   13(2,R6),=H'24'                                                  
         MVC   20(2,R6),AGENCY                                                  
         OI    UPDSW,UPDRECSW      SET NEEDS SHIP                               
         B     PRT80                                                            
*                                                                               
PRT66    GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         USING SHPDTAEL,R6                                                      
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRT70    BRAS  RE,NEXTEL                                                        
         BNE   PRT80                                                            
         CLC   SHPCMML(16),ELEM+2  MATCH BOTH CMMLS                             
         BNE   PRT70                                                            
*                                                                               
* MATCHING ELEMENT FOUND - TEST TO UPDATE TELECAST DATES *                      
*                                                                               
         XC    DUB,DUB             CLEAR LAST TLCST SAVE AREA                   
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST SHIPPED YET                             
         BNZ   PRT74               YES - FIRST TLCST DATE NOT RELEVANT          
*                                                                               
         LA    RE,ELEM+SHPFTD-SHPDTAEL                                          
         CLC   SHPFTD,0(RE)        TEST LOWER FIRST TLCST DATE                  
         BNH   PRT74                                                            
         MVC   SHPFTD,0(RE)                                                     
*                                                                               
         OI    UPDSW,UPDRECSW      SET NEEDS SHIP                               
*                                                                               
PRT74    MVC   DUB(6),SHPFTD       SAVE OLD FIRST/LAST TLCST DATES              
*                                                                               
         LA    RE,ELEM+SHPLTD-SHPDTAEL                                          
         CLC   SHPLTD,0(RE)        HIGHER LAST TLCST DATE                       
         BNL   PRT75                                                            
         MVC   SHPLTD,0(RE)                                                     
         OI    UPDSW,UPDRECSW      SET NEEDS SHIP                               
*                                                                               
* SAVE ELEMENT AND DELETE FROM RECORD *                                         
*                                                                               
PRT75    IC    R0,ELEM+SHPNOSHP-SHPDTAEL   SAVE PACKED CMML FLAG                
         MVC   ELEM,0(R6)                                                       
         XC    BLOCK(256),BLOCK                                                 
         MVC   BLOCK(SHPDTAX-SHPDTAEL),0(R6) SAVE OLD ELEM                      
         STC   R0,ELEM+SHPNOSHP-SHPDTAEL   AND SET IN SAVED ELEMENT             
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST SHIPPED YET                             
         BZ    PRT80                                                            
         TM    SVOPT1,OPTRERUN     TEST RERUN                                   
         BZ    PRT76               NO                                           
         CLC   SVOPTDTE,SHPSHPDT   COMPARE RERUN DATE TO SHIP DATE              
         BNH   PRT80               IF NOT HIGH, SHIP IT                         
*                                                                               
* SHIPPED PREVIOUSLY - TEST NEEDS RESHIP *                                      
*                                                                               
PRT76    GOTO1 DATCON,DMCB,(3,DUB+3),WORK                                       
*                                                                               
         ZIC   R0,SVPROF1          USER SPECIFIED RESHIP PERIOD                 
         CLI   SVPROF1,0                                                        
         BNE   *+8                                                              
         LA    R0,30               DEFAULT IS 30                                
*                                                                               
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
*                                                                               
         GOTO1 DATCON,(R1),(5,0),WORK+12                                        
*                                                                               
         CLC   WORK+6(6),WORK+12   COMPARE TO TODAY                             
         BNL   PRT88               DO NOT ADD TO SHIP LIST                      
*                                                                               
         MVC   SHPFTD,DUB          RESET FIRST TLCST DATE FOR RESHIP            
         XC    SHPSHPDT,SHPSHPDT   AND CLEAR DATE SO IT WILL HAPPEN             
*                                                                               
PRT80    CLI   SVCMLPIG,2          TEST PRD2 REC                                
         BE    PRT88                                                            
*                                                                               
* ADD COMMERCIAL(S) TO SHIPLIST IF NOT THERE ALREADY *                          
*                                                                               
         L     R4,ACMLSHIP                                                      
*                                                                               
PRT84    OC    0(8,R4),0(R4)       TEST EOL                                     
         BZ    PRT86                                                            
         CLC   0(16,R4),ELEM+2     TEST EQUAL ENTRY                             
         BE    PRT88                                                            
         LA    R4,17(R4)                                                        
         L     R1,AIO3             SEE IF REACHED END OF AIO3                   
         AHI   R1,6000                                                          
         CR    R1,R4                                                            
         BH    PRT84                                                            
         DC    H'0'                                                             
*                                                                               
PRT86    MVC   0(16,R4),ELEM+2                   MOVE CMMLS                     
         MVC   16(1,R4),ELEM+SHPNOSHP-SHPDTAEL   AND FLAGS                      
         XC    17(17,R4),17(R4)    EOL                                          
*                                                                               
PRT88    TM    UPDSW,UPDRECSW      TEST UPDATE REQUIRED                         
         BZ    PRT89F               NO                                          
*                                                                               
         L     RE,AIO                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(RE)         GET REC LEN                                  
         AR    RE,R0               POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       AND CLEAR                                    
*                                                                               
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
         MVC   BLOCK(SHPDTAX-SHPDTAEL),ELEM  SAVE ELEM FOR P/B                  
*                                                                               
         CLI   QBEST,0             EST = NO                                     
         BE    PRT89B              YES                                          
*                                                                               
         MVI   ELCODE,X'20'        ESTIMATE ELEMENT                             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRT89                                                            
*                                                                               
         BAS   RE,ESTMASK          TEST ESTIMATE MASK/SET ON IF NOT             
         B     PRT89B                                                           
*                                                                               
* CREATE NEW ESTIMATE ELEMENT                                                   
PRT89    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING SHPESTEL,R6                                                      
*                                                                               
         MVI   SHPESTEL,X'20'                                                   
         MVI   SHPESTLN,SHPESTX-SHPESTEL                                        
         BAS   RE,ESTMASK                                                       
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM                       
*                                                                               
PRT89B   L     RF,PUTREC                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         L     RF,ADDREC                                                        
*                                                                               
PRT89D   TM    SVOPT1,OPTTEST      TEST TEST RUN                                
         BO    *+6                                                              
         BASR  RE,RF                                                            
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM FOR P/B               
*                                                                               
PRT89F   TM    UPDSW,UPDEST        UPDATED ESTIMATE ALREADY                     
         BO    PRT90               YES, DONE                                    
*                                                                               
         TM    UPDSW,UPDRECSW      TEST UPDATE REQUIRED                         
         BO    PRT90                                                            
*                                                                               
         CLI   QBEST,0             EST = NO                                     
         BE    PRT90               YES                                          
*                                                                               
         MVC   BLOCK(SHPDTAX-SHPDTAEL),ELEM   SAVE ELEM                         
*                                                                               
         BAS   RE,AESTELEM         ADD ESTIMATE ELEM IF NEEDED                  
         TM    UPDSW,UPDEST        UPDATED EST ELEM?                            
         BO    PRT89B                                                           
*                                                                               
*                                                                               
* NEED TO ADD RECORD UNDER PASSIVE PRD FOR PIGGYBACKS *                         
*                                                                               
PRT90    CLI   SVCMLPIG,1          TEST P/B PRD1                                
         BNE   PRT92               NO                                           
         NI    UPDSW,X'FF'-UPDEST  INIT UPDATED ESTIMATE                        
         LA    R5,SVCMLNXT         POINT TO PRD2 DATA                           
         CLI   SVCMLPIG,2                                                       
         BE    PRT64                                                            
         DC    H'0'                                                             
*                                                                               
PRT92    LA    R5,SVCMLNXT         NEXT COMMERCIAL ENTRY                        
*                                                                               
         OC    0(2,R5),0(R5)       TEST EOL                                     
         BZ    PRT100                                                           
         CLI   SVCMLPIG,2          TEST SOLO OR ACTIVE PB                       
         BE    PRT92               NO - SKIP                                    
         B     PRT60               ELSE GO PROCESS                              
         DROP  R5                                                               
*                                                                               
* PRINT ALL COMMERCIAL TEXT COMMENTS (IN COMMERCIAL ORDER) *                    
*                                                                               
PRT100   ST    R4,NEXTADDR                                                      
*                                                                               
         NI    UPDSW,X'FF'-X'20'   SET OFF PRINTING SPOTS                       
         SR    R0,R0                                                            
         L     R1,ASHPLIST                                                      
         LR    R2,R1                                                            
PRT102   OC    0(8,R1),0(R1)                                                    
         BZ    PRT104                                                           
         BCTR  R0,0                                                             
         LA    R1,L'SVCMLDTA(R1)                                                
         B     PRT102                                                           
PRT104   LCR   R0,R0                                                            
         BZ    PRT120                                                           
*                                                                               
         GOTO1 =V(XSORT),DMCB,ASHPLIST,(R0),L'SVCMLDTA,8,0,RR=SPTRRR            
*                                                                               
         BAS   RE,RCT              GO READ COMML TEXT                           
*                                                                               
* COUNT AND SORT SHIPPING LIST ENTRIES *                                        
*                                                                               
PRT120   SR    R0,R0               CLEAR COUNTER                                
         L     R4,ACMLSHIP                                                      
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,17(R4)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0               SET NUMBER OF ENTRIES                        
         BZ    PRT144              SKIP SORT/PRINT IF NONE                      
*                                                                               
         GOTO1 =V(XSORT),DMCB,ACMLSHIP,(R0),17,17,0,RR=SPTRRR                   
*                                                                               
* FORMAT SHIPPING LIST TO PRINT BUFFER (IO3) *                                  
*                                                                               
         CLI   SHIPYORN,C'N'       IF USER SAID NOSHIP,                         
         BE    PRT144              ADD RECORDS BUT DO NOT PRINT                 
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   PRT124                                                           
         TM    AUTOQSW,X'80'       TEST AUTO SHIPPING GENERATED YET             
         BO    PRT124                                                           
*                                                                               
* GENERATE AUTO SHIPPING REQ *                                                  
*                                                                               
         OI    AUTOQSW,X'80'       SET FLAG                                     
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
*                                                                               
         MVI   XTYPE,C'S'                                                       
         MVC   XQUESTOR,QUESTOR                                                 
         MVC   XWHEN,QUWHEN                                                     
         MVC   XMED,QMED                                                        
         MVC   XCLT,QCLT                                                        
         TM    SVOPT1,OPTRERUN     TEST RERUN                                   
         BZ    PRT122                                                           
         GOTO1 DATCON,DMCB,(3,SVOPTDTE),XRERUN                                  
*                                                                               
PRT122   CLI   SHIPYORN,C'A'       TEST AUTO SHIP OPTION                        
         BNE   PRT124              NO                                           
         L     R1,AERRFIL                                                       
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
*                                                                               
PRT124   CLI   SVT1PR6,C'E'       ELIMINATE SHIP MSG                            
         BE    PRT144                                                           
*                                                                               
         L     R4,ACMLSHIP                                                      
*                                                                               
         L     R5,AIO3                                                          
*                                                                               
         ST    R5,FULL                                                          
         MVC   0(128,R5),SPACES                                                 
         MVC   00(38,R5),=C'YOU SHOULD HAVE ALL COMMERCIALS EXCEPT'             
         MVC   64(35,R5),=C'THE FOLLOWING WHICH WILL BE SHIPPED'                
*                                                                               
         CLI   SVT1PR6,C'Y'       PRINT ENCLOSED, NOT SHIPPED                   
         BNE   *+10                                                             
         MVC   84(15,R5),=CL15'ARE ENCLOSED   '                                 
*                                                                               
         LA    R5,128(R5)                                                       
         B     PRT138                                                           
*                                                                               
PRT126   CLI   8(R4),0             TEST PIGGYBACK                               
         BNE   PRT130                                                           
*                                                                               
* SOLO *                                                                        
*                                                                               
         LA    RF,12(R5)                                                        
         CR    RF,R0               TEST PAST EOL                                
         BH    PRT136                                                           
         MVC   0(8,R5),0(R4)                                                    
         TM    16(R4),X'01'        TEST CODE IS PACKED                          
         BZ    PRT134                                                           
         GOTO1 VTRPACK,DMCB,(C'U',(R4)),(R5)                                    
         B     PRT134                                                           
*                                                                               
* PIGGYBACK *                                                                   
*                                                                               
PRT130   LA    RF,25(R5)                                                        
         CR    RF,R0                                                            
         BH    PRT136                                                           
         MVC   0(8,R5),0(R4)                                                    
         MVI   8(R5),C'-'                                                       
         MVC   9(8,R5),8(R4)                                                    
*                                                                               
         TM    16(R4),X'01'        TEST CMMLS ARE PACKED                        
         BZ    PRT134                                                           
*                                                                               
         MVC   0(17,R5),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'U',(R4)),(R5)                                    
*                                                                               
         LA    RE,13(R5)           FIND LAST OUTPUT CHAR                        
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   1(RE),C'-'                                                       
         LA    RE,2(RE)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 VTRPACK,DMCB,(C'U',8(R4))                                        
*                                                                               
PRT134   LA    R5,25(R5)           POINT PAST END OF ADID-ADID                  
         CLI   17(R4),0            TEST MORE DATA IN LIST                       
         BE    PRT140              NO - GO PRINT                                
*                                                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
         LA    R4,17(R4)                                                        
         B     PRT126                                                           
*                                                                               
PRT136   L     R5,FULL                                                          
         LA    R5,64(R5)                                                        
PRT138   ST    R5,FULL                                                          
         MVC   0(64,R5),SPACES                                                  
         LA    R0,63(R5)           SET EOL ADDRESS                              
         B     PRT126                                                           
*                                                                               
* COUNT AND PRINT THE SHIPPING LIST ENTRIES *                                   
*                                                                               
PRT140   L     RE,FULL                                                          
         LA    RE,64(RE)                                                        
         MVI   0(RE),0             SET EOL FLAG                                 
*                                                                               
* COUNT THE LINES *                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R4,AIO3                                                          
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,64(R4)                                                        
         BCT   R0,*-12                                                          
         LPR   R0,R0                                                            
         STC   R0,ALLOWLIN                                                      
*                                                                               
         L     R4,AIO3                                                          
PRT142   MVC   P+2(64),0(R4)                                                    
         BRAS  RE,GOSPL                                                         
         LA    R4,64(R4)                                                        
         CLI   0(R4),0                                                          
         BNE   PRT142                                                           
         EJECT                                                                  
* NEED TO PRINT FOOTLINES AT END OF REPORT *                                    
*                                                                               
PRT144   MVI   CONTINUE,C'N'                                                    
         MVI   FORCEFUT,C'Y'                                                    
         MVC   P,SPACES                                                         
         BRAS  RE,GOSPL                                                         
*                                                                               
* UPDATE INSTRUCTION RECAP, BUY ACTIVITY, AND SPOT BUY RECORDS *                
*                                                                               
         BRAS  RE,UPDI                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   PREXIT                                                           
*                                                                               
* GENERATE SPOT RECAP FOR DEALERS *                                             
*                                                                               
         MVC   ELEM,SPACES                                                      
         LA    R2,ELEM                                                          
         USING XRECD,R2                                                         
*                                                                               
         MVI   XTYPE,C'R'                                                       
         MVC   XQUESTOR,QUESTOR                                                 
         MVC   XWHEN,QUWHEN                                                     
         MVC   XMED,QMED                                                        
         MVC   XCLT,QCLT                                                        
         MVC   XPRD,QPRD                                                        
         MVC   XMKT,QMKT                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVGENST),XFLTST                                   
         GOTO1 (RF),(R1),(3,SVGENEND),XFLTEND                                   
*                                                                               
         L     R1,AERRFIL                                                       
         LA    R0,ELEM                                                          
         PUT   (1),(0)                                                          
PREXIT   XIT1                                                                   
         EJECT                                                                  
*================================================                               
* TEST ESTIMATE MASK                                                            
* TURN ON ESTIMATE MASK IF NOT ON YET                                           
*================================================                               
         USING SHPESTEL,R6                                                      
ESTMASK  NTR1                                                                   
         SR    R0,R0                                                            
         LLC   R0,QBEST            ESTIMATE                                     
         LR    RF,R0                                                            
         SR    RE,RE                                                            
         SLDL  RE,29               GET MASK BYTE NO. INTO RE                    
         SRL   RF,29               GET MASK BIT  NO. INTO RF                    
         LA    R1,SHPESMSK(RE)     R1 = A(CORRECT BYTE IN MASK)                 
         LA    RF,BITLIST(RF)                                                   
         MVC   BYTE,0(R1)          TEST FOR THIS ESTIMATE                       
         NC    BYTE,0(RF)                                                       
         CLI   BYTE,0              ANY ESTIMATE?                                
         BNE   ESTX                 YES                                         
         OC    0(1,R1),0(RF)       TURN BIT ON IN ESTIMATE MASK                 
         CR    R1,R1               SET CC EQ                                    
ESTX     XIT1                                                                   
                                                                                
BITLIST  DC    X'8040201008040201'                                              
*                                                                               
*                                                                               
*===============================================================                
* ADD ESTIMATE ELEMENT OR ADD NEW ESTIMATE MASK TO EXISTING ELEM                
*===============================================================                
*                                                                               
AESTELEM NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'     RESTORE X'10' ELEM IF REMOVED EARLIER           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
AEST10   BRAS  RE,NEXTEL                                                        
         BNE   AEST20              ADD BACK THE X'10' ELEM                      
*                                                                               
         CLC   0(SHPDTAX-SHPDTAEL,R6),BLOCK  SAME ELEM?                         
         BNE   AEST10                                                           
         B     AEST30              ELEMENT IN RECORD                            
                                                                                
AEST20   MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK                                     
         L     R6,AIO                                                           
         LR    RE,R6                                                            
         SR    R0,R0                                                            
         ICM   R0,3,13(RE)         GET REC LEN                                  
         AR    RE,R0               POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       AND CLEAR                                    
*                                                                               
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
*                                                                               
AEST30   MVI   ELCODE,X'20'        ESTIMATE ELEMENT                             
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   AEST40                                                           
*                                                                               
         BAS   RE,ESTMASK          TEST EST MASK/SET ON IF NOT FND              
         BNE   AESTX               ESTIMATE FOUND IN ELEM                       
         OI    UPDSW,UPDEST        TURN ON UPDATED ESTIMATE ELEM                
         B     AESTX                                                            
*                                                                               
* CREATE NEW ESTIMATE ELEMENT                                                   
AEST40   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING SHPESTEL,R6                                                      
*                                                                               
         MVI   SHPESTEL,X'20'                                                   
         MVI   SHPESTLN,SHPESTX-SHPESTEL                                        
         BAS   RE,ESTMASK                                                       
         GOTO1 ADDELEM             INSERT NEW ELEM IN RECORD                    
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SHPDTAX-SHPDTAEL),BLOCK  RESTORE ELEM                       
*                                                                               
         OI    UPDSW,UPDEST        TURN ON UPDATED ESTIMATE ELEM                
AESTX    XIT1                                                                   
*                                                                               
*                                                                               
* BUILD PRINT LINE(S) FOR 1 SPOT                                                
*                                                                               
         DS    0H                                                               
BLIN     NTR1                                                                   
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         L     R3,ASPTABL                                                       
         USING SPTABLED,R3                                                      
*                                                                               
BLIN00   TM    SPTFLAG,X'40'       WAS THIS SPOT PRINTED                        
         BO    *+12                                                             
         OI    UPDSW2,UPDPRTSW     SET SPOT NEVER PRTD NOW PRTD                 
         OI    SPTFLAG,X'40' FLAG AS PRINTED                                    
*                                                                               
         GOTO1 UNDAY,DMCB,SPTDAY,PDAY                                           
*                                                                               
         GOTO1 UNTIME,(R1),SPTTIME,PTIME                                        
*                                                                               
         CLC   SVDSKAD,SPTDSKAD                                                 
         BNE   BLIN10                                                           
         MVC   PPNAME,SVPNAME                                                   
         B     BLIN12                                                           
*                                                                               
BLIN10   XC    KEY,KEY                                                          
         MVC   KEY+14(4),SPTDSKAD                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING BUYRECD,R4                                                       
         MVC   PPNAME,BDPROGRM                                                  
         MVC   SVPNAME,BDPROGRM                                                 
         MVC   SVDSKAD,SPTDSKAD                                                 
         DROP  R4                                                               
*                                                                               
BLIN12   MVC   PDAYPT,SPTDPT                                                    
*                                                                               
         ZIC   R4,SPTSLN                                                        
         OC    SPTCMLS2,SPTCMLS2                                                
         BZ    BLIN14                                                           
         CLC   SPTCMLSQ,SPTCMLS2                                                
         BNE   BLIN14                                                           
         ZIC   RF,SPTSLN2                                                       
         AR    R4,RF                                                            
*                                                                               
BLIN14   EDIT  (R4),(3,PSLN)                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,SPTFTD),(4,PDATE)                                 
         CLC   SPTFTD,SPTLTD                                                    
         B     BLIN16                                                           
*        BE    BLIN16                                                           
         MVI   PDATE+5,C'-'                                                     
         GOTO1 (RF),(R1),(2,SPTLTD),(4,PDATE+6)                                 
*                                                                               
BLIN16   MVC   PPROD,SPTPROD                                                    
         LA    R4,SPTPROD                                                       
         BAS   RE,FPNM             GO FIND PRODUCT NAME                         
         MVC   PPRODNM,WORK                                                     
*                                                                               
         LA    R4,SPTCMLSQ                                                      
         MVI   BYTE,0                                                           
*                                                                               
         OC    SPTCMLS2,SPTCMLS2                                                
         BZ    BLIN20                                                           
         CLC   SPTCMLSQ,SPTCMLS2                                                
         BE    BLIN20                                                           
         MVI   BYTE,1                                                           
*                                                                               
BLIN20   BAS   RE,FCMLN            GET COMMERCIAL NAME                          
         MVC   PCML,PTCMLADI       PRINT ADI OR ISCI                            
*                                                                               
         LA    R1,PCMLTI                                                        
*                                                                               
BLIN22   OC    PTCMLDSC,PTCMLDSC                                                
         BZ    *+14                                                             
         MVC   0(L'PCMLTI,R1),PTCMLDSC                                          
         AHI   R1,132                                                           
*                                                                               
         OC    PTCMLDS2,PTCMLDS2                                                
         BZ    *+14                                                             
         MVC   0(20,R1),PTCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    PTCMLDS3,PTCMLDS3                                                
         BZ    *+10                                                             
         MVC   0(20,R1),PTCMLDS3                                                
*                                                                               
         MVI   ALLOWLIN,5                                                       
*                                                                               
         BRAS  RE,GOSPL                                                         
*                                                                               
         LA    R1,PCMLTI                                                        
*                                                                               
         OC    PTCMLCLT,PTCMLCLT   SAVED CLIENT COMML #                         
         BZ    BLIN23                                                           
         MVC   0(20,R1),PTCMLCLT                                                
*                                                                               
         BRAS  RE,GOSPL                                                         
*                                                                               
BLIN23   DS    0H                                                               
         OC    SPTPROD2,SPTPROD2   PIGGYBACK PROD                               
         BZ    BLIN30               NO                                          
*                                                                               
         MVC   PPROD,SPTPROD2                                                   
         LA    R4,SPTPROD2                                                      
         BAS   RE,FPNM             GO FIND PRODUCT NAME                         
         MVC   PPRODNM,WORK                                                     
*                                                                               
         OC    SPTCMLS2,SPTCMLS2                                                
         BZ    BLIN24                                                           
*                                                                               
         CLC   SPTCMLSQ,SPTCMLS2                                                
         BE    BLIN30                                                           
*                                                                               
BLIN24   MVI   PSLN+3,C'/'                                                      
         EDIT  (B1,SPTSLN2),(3,PSLN+4),ALIGN=LEFT                               
         OC    SPTCMLS2,SPTCMLS2                                                
         BZ    BLIN30                                                           
*                                                                               
BLIN26   LA    R4,SPTCMLS2                                                      
         MVI   BYTE,2                                                           
         BAS   RE,FCMLN            GET COMMERCIAL NAME                          
         MVC   PCML,PTCMLADI                                                    
*                                                                               
         LA    R1,PCMLTI                                                        
*                                                                               
         OC    PTCMLDSC,PTCMLDSC                                                
         BZ    *+14                                                             
         MVC   0(L'PCMLTI,R1),PTCMLDSC                                          
         AHI   R1,132                                                           
*                                                                               
         OC    PTCMLDS2,PTCMLDS2                                                
         BZ    *+14                                                             
         MVC   0(20,R1),PTCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    PTCMLDS3,PTCMLDS3                                                
         BZ    *+14                                                             
         MVC   0(20,R1),PTCMLDS3                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         LA    RE,P+396            P4?                                          
         CR    R1,RE                                                            
         BL    *+12                                                             
         BRAS  RE,GOSPL                                                         
*                                                                               
         LA    R1,PCMLTI                                                        
*                                                                               
         OC    PTCMLCLT,PTCMLCLT   SAVED CLIENT COMML #                         
         BZ    *+10                                                             
         MVC   0(20,R1),PTCMLCLT                                                
*                                                                               
BLIN30   MVI   SPACING,2                                                        
*                                                                               
         BRAS  RE,GOSPL                                                         
*                                                                               
* CHECK IF ALL SPOTS PRINTED                                                    
*                                                                               
         LA    R3,SPTNEXT          NEXT SPOT                                    
         OC    SPTFTD,SPTFTD       ANY MORE                                     
         BNZ   BLIN00                                                           
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         B     PREXIT                                                           
         EJECT                                                                  
* SEARCH TABLE FOR COMMERCIAL RECORD  NEEDED FOR THIS SPOT *                    
*                                                                               
FCMLN    NTR1                                                                   
*                                                                               
         MVI   HALF,0                                                           
*                                                                               
         XC    PTCMLCOD,PTCMLCOD                                                
         XC    PTCMLADI,PTCMLADI                                                
         XC    PTCMLDSC,PTCMLDSC                                                
         XC    PTCMLDS2,PTCMLDS2                                                
         XC    PTCMLDS3,PTCMLDS3                                                
         XC    PTCMLCLT,PTCMLCLT                                                
*                                                                               
         OC    0(2,R4),0(R4)       IS THIS A TBA COMML                          
         BZ    FCMLN50             YES                                          
*                                                                               
         L     RE,ASHPLIST         START                                        
         L     RF,ASHPLISX         AND OF TABLE                                 
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         LA    R1,L'SVCMLDTA                                                    
         DR    RE,R1                   GET MAX ENTRIES ALLOWED                  
*                                                                               
         L     R5,ASHPLIST                                                      
         USING SVCMLD,R5                                                        
FCMLN10  OC    0(L'SVCMLDTA,R5),0(R5) EMPTY SLOT                                
         BZ    FCMLN20                                                          
         CLC   SVCMLSEQ,0(R4)                                                   
         BNE   FCMLN14                                                          
         CLC   SVCMLPIG,BYTE       THIS THE SAME STATUS?                        
         BNE   FCMLN14                                                          
         CLI   BYTE,2              THIS PTR PROD                                
         BL    FCMLN30              NO                                          
*                                                                               
         LR    R1,R5                                                            
         SHI   R1,L'SVCMLDTA       LOOK AT PREV ENTRY                           
         C     R1,ASHPLIST                                                      
         BNL   *+6                                                              
         DC    H'0'                CAN'T BE BELOW START                         
         OC    PRCMLSQ1,PRCMLSQ1                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  PREV ENTRY 1ST OF PAIR                       
         CLI   SVCMLPIG-SVCMLDTA(R1),1                                          
         BNE   FCMLN14                                                          
*                                  PREV ENTRY = TO 1ST OF THIS PAIR             
         CLC   PRCMLSQ1,SVCMLSEQ-SVCMLDTA(R1)                                   
         BE    FCMLN30              YES                                         
*                                                                               
FCMLN14  LA    R5,SVCMLNXT                                                      
         BCT   RF,FCMLN10                                                       
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE - DIE                                
         BNE   ACTSZERR            ONLINE - GIVE MSG                            
         DC    H'0'                CML LIST TABLE IS TOO SMALL                  
*                                                                               
FCMLN20  XC    KEY,KEY             READ PASSIVE POINTER                         
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+6(2),0(R4)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         USING CMLKEY,R6                                                        
         MVC   SVCMLCOD,CMLKCML                                                 
         MVC   SVCMLADI,SVCMLCOD   SAVE HERE TOO IN CASE NO ADID                
*                                                                               
         TM    15(R6),CMLKSTA_PCKD                                              
         BZ    FCMLN22                                                          
         MVI   SVCMLST,X'01'       SET FLAG CMML IS PACKED                      
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),SVCMLADI                             
*                                                                               
FCMLN22  MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BO    DELCMLER            NO                                           
         MVC   SVCMLDSC(15),CMLTITLE                                            
         MVC   SVCMLTYP,CMLTYPE                                                 
         MVC   SVCMLCLT,CMLCLTNO   CLT COMMERCIAL #                             
         CLC   0(2,R4),CMLSEQ+1                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCMLSEQ,CMLSEQ+1                                                
         MVC   SVCMLPIG,BYTE       0=1 PROD, 1=PROD1, 2=PROD2                   
         MVC   SVCMLFTD,SPTFTD                                                  
         MVC   SVCMLLTD,SPTLTD                                                  
*                                                                               
         XC    SVCMLDS2,SVCMLDS2   CLEAR COMMERCIAL DESC 2 AND 3                
         XC    SVCMLDS3,SVCMLDS3                                                
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FCMLN26                                                          
         MVC   SVCMLDS2(20),3(R6)  COMMERCIAL DESC 2                            
         BRAS  RE,NEXTEL                                                        
         BNE   FCMLN26                                                          
         MVC   SVCMLDS3(20),3(R6)  COMMERCIAL DESC 3                            
*                                                                               
FCMLN26  DS   0H                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLN28                                                          
*                                                                               
         USING CMLADIEL,R6                                                      
         MVC   SVCMLADI,CMLADID                                                 
         DROP  R6                                                               
*                                                                               
FCMLN28  CLI   HALF,X'FF'                                                       
         BNE   *+12                                                             
         MVI   HALF,0                                                           
         B     FCMLN40                                                          
*                                                                               
         LA    R1,L'SVCMLDTA(,R5)                                               
         XC    0(L'SVCMLDTA,R1),0(R1)                                           
         B     FCMLN40                                                          
*                                                                               
FCMLN30  CLC   SVCMLFTD,SPTFTD                                                  
         BNH   *+10                                                             
         MVC   SVCMLFTD,SPTFTD                                                  
*                                                                               
         CLC   SVCMLLTD,SPTLTD                                                  
         BNL   *+10                                                             
         MVC   SVCMLLTD,SPTLTD                                                  
*                                                                               
         MVI   HALF,X'FF'                                                       
         B     FCMLN20                                                          
*                                                                               
FCMLN40  DS   0H                                                                
         MVC   PTCMLCOD,SVCMLCOD                                                
         MVC   PTCMLDSC,SVCMLDSC                                                
         MVC   PTCMLDS2,SVCMLDS2                                                
         MVC   PTCMLDS3,SVCMLDS3                                                
         MVC   PTCMLADI,SVCMLADI                                                
         MVC   PTCMLCLT,SVCMLCLT                                                
         XC    PRCMLSQ1,PRCMLSQ1                                                
         CLI   BYTE,1              THIS PTR PROD                                
         BNE   FCMLNX               NO                                          
*                                                                               
         MVC   PRCMLSQ1,0(R4)      SAVE 1ST COMML SEQ OF PAIR                   
         B     FCMLNX                                                           
*                                                                               
FCMLN50  MVC   PTCMLCOD,SPACES                                                  
         MVC   PTCMLCOD(3),=C'TBA'                                              
         XC    PTCMLADI,PTCMLADI                                                
         MVC   PTCMLDSC,=CL15'TO BE ANNOUNCED'                                  
         XC    PTCMLDS2,PTCMLDS2                                                
         XC    PTCMLDS3,PTCMLDS3                                                
         XC    PTCMLCLT,PTCMLCLT                                                
*                                                                               
FCMLNX   B     PREXIT                                                           
         DROP  R3                                                               
*                                                                               
ACTSZERR MVC   GERROR,=Y(MANYSPOT)                                              
         MVI   GMSGTYPE,C'E'                                                    
         CLI   OFFLINE,C'Y'        SET COND CODE                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
*============================================================                   
* READ PRODUCT HEADERS AND SAVE PRODUCT NAMES IN BUFFER *                       
*============================================================                   
                                                                                
FPNM     NTR1                                                                   
*                                                                               
         LA    R5,BLOCK                                                         
         LA    R6,BLOCK+460                                                     
*                                                                               
FPNM10   CLI   0(R5),0                                                          
         BE    FPNM20                                                           
         CLC   0(3,R5),0(R4)       MATCH                                        
         BE    FPNMX                                                            
         LA    R5,23(,R5)                                                       
         CR    R5,R6                                                            
         BL    FPNM10                                                           
         DC    H'0'                                                             
*                                                                               
* READ PRDHDR *                                                                 
*                                                                               
FPNM20   XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),0(R4)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   0(3,R5),0(R4)                                                    
         MVC   3(20,R5),PNAME                                                   
*                                                                               
FPNMX    MVC   WORK(20),3(R5)                                                   
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
DELCMLER MVC   GERROR,=Y(CMLDEL2)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,9              L'SUBST TEXT + 1                             
         L     RE,AIO1                                                          
         MVC   ELEM+1(8),5(RE)                                                  
         GOTO1 VTRAERR                                                          
         DROP  R5                                                               
         EJECT                                                                  
* READ INSTRUCTION RECAP RECS FOR REVISION/RERUN *                              
*                                                                               
         USING SPTABLED,R3                                                      
RINS     NTR1                                                                   
*                                                                               
         NI    INSPRTSW,X'FF'-INSPRTPR-INSPRTCH SET PREV INSTR/CHGE OFF         
*                                                                               
         XC    SVINSDT(3),SVINSDT AND SVINSREV                                  
*                                                                               
         L     R3,ASPTABL                                                       
         USING SPTABLED,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD BCLT                                             
         MVC   KEY+5(1),SPTPRD                                                  
         MVC   KEY+6(5),SVMKTSTA                                                
         MVC   KEY+11(1),QBEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RINS40              IF NOT FOUND, NO DUPES                       
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
RINS10   MVC   WORK(1),SPTPRD                                                   
         MVC   WORK+1(1),SPTSLN                                                 
         MVC   WORK+2(1),SPTPRD2                                                
         MVC   WORK+3(1),SPTSLN2                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
RINS20   BRAS  RE,NEXTEL                                                        
         BNE   RINS30                                                           
         USING INSDTAEL,R6                                                      
         MVC   BSLN,SPTSLN                                                      
         CLC   INSPRD1(4),WORK     PRD1/SLN1/PRD2/SLN2                          
         BNE   RINS20                                                           
         CLC   SVGENEDP,INSFTD     FLIGHT END BEFORE INST START                 
         BL    RINS20                                                           
         CLC   SVGENSTP,INSLTD     FLIGHT START AFTER INST END                  
         BH    RINS20                                                           
         MVC   SVINSDT,INSDATE     SAVE LAST INST DATE                          
         MVC   SVINSREV,INSREV                                                  
*                                                                               
         OI    INSPRTSW,INSPRTPR   SET FLAG THERE WERE PREVIOUS INST            
*                                                                               
         B     RINS20              GET ALL ELEMENTS                             
*                                                                               
RINS30   TM    SPTPREF,X'80'       WAS THIS PRINTED ON INST                     
         BZ    RINS44                                                           
*                                                                               
         OI    INSPRTSW,INSPRTCH   SET ON CHANGED SW                            
*                                                                               
RINS44   LA    R3,SPTNEXT                                                       
*                                                                               
         OC    SPTFTD,SPTFTD       AT END                                       
         BZ    RINS40               YES                                         
*                                                                               
* SEE IF NEED TO LOOK UP ANOTHER ELEMENT *                                      
*                                                                               
         CLC   WORK(1),SPTPRD                                                   
         BNE   RINS10                                                           
         CLC   WORK+1(1),SPTSLN                                                 
         BNE   RINS10                                                           
         CLC   WORK+2(1),SPTPRD2                                                
         BNE   RINS10                                                           
         CLC   WORK+3(1),SPTSLN2                                                
         BNE   RINS10                                                           
         B     RINS30                                                           
*                                                                               
RINS40   B     PREXIT                                                           
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* RTN TO READ COMMERCIAL TEXT RECORDS *                                         
*                                                                               
         DS    0H                                                               
RCT      NTR1                                                                   
*                                                                               
* READ COMMERCIAL TEXT RECORD *                                                 
*                                                                               
         USING SVCMLD,R2                                                        
RCT40    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CMTKEY,R4                                                        
         MVC   CMTKID,=X'0A35'                                                  
         MVC   CMTKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMTKPRD,SVCMLPRD                                                 
         MVC   CMTKMKT(5),SVMKTSTA                                              
         MVC   CMTKSEQ,SVCMLSEQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCT44                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKSTA,CMTKSTA                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCT44                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKMKT,CMTKMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCT44                                                            
         MVC   KEY(13),KEYSAVE                                                  
         MVI   CMTKPRD,0                                                        
         MVC   CMTKMKT(5),SVMKTSTA                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCT44                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKSTA,CMTKSTA                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCT44                                                            
         MVC   KEY(13),KEYSAVE                                                  
         XC    CMTKMKT,CMTKMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RCT70                                                            
         DROP  R4                                                               
RCT44    L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'40'                                                     
         SR    R3,R3                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RCT46    LA    R3,1(,R3)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    RCT46                                                            
         STC   R3,ALLOWLIN                                                      
         MVC   PDAY(15),=CL15'COMMERCIAL TEXT'                                  
         MVC   PDAY+15+3(8),0(R2)         COMML CODE                            
         OC    SVINSDT,SVINSDT     IF ORIGINAL, NO CHANGE POSSIBLE              
         BZ    RCT50                                                            
         CLI   SVINSREV,0          IF REVSION ZERO                              
         BNE   *+12                                                             
         TM    SVOPT1,OPTRERUN      AND RERUN                                   
         BO    RCT50                                                            
         L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(2,SVINSDT),(3,FULL)                                 
         CLC   FULL(3),2(R6)                                                    
         BNL   *+20                                                             
         MVC   PDAY+29(6),=C'(ADDED'                                            
         MVC   PDAY+36(24),=C'SINCE LAST INSTRUCTIONS)'                         
         B     RCT50                                                            
*                                                                               
         CLC   FULL(3),8(R6)                                                    
         BNL   RCT50                                                            
         MVC   PDAY+29(8),=C'(CHANGED'                                          
         MVC   PDAY+38(24),=C'SINCE LAST INSTRUCTIONS)'                         
*                                                                               
RCT50    BRAS  RE,GOSPL                                                         
*                                                                               
         L     R6,AIO3             USE IO3 FOR COMMERCIALS                      
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMTTXTEL,R6                                                      
RCT60    ZIC   R1,CMTTXTLN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,RCTMVC                                                        
         BRAS  RE,GOSPL                                                         
         BRAS  RE,NEXTEL                                                        
         BE    RCT60                                                            
         BRAS  RE,GOSPL                                                         
RCT70    LA    R2,SVCMLNXT                                                      
         CLI   SVCMLCOD,0                                                       
         BNE   RCT40                                                            
         LTR   RB,RB               SET NE COND CODE FOR NOW                     
         B     PREXIT                                                           
RCTMVC   MVC   PDAY(0),CMTTXT                                                   
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
HEADING  SSPEC H1,3,C'CONTACT -'                                                
         SSPEC H2,3,AGYNAME                                                     
         SSPEC H3,3,AGYADD                                                      
*                                                                               
*NOTE*   SSPEC H1,39,C'SPOT TELEVISION COMMERCIAL INSTRUCTIONS'                 
*NOTE*   SSPEC H2,39,C'---------------------------------------'                 
         SSPEC H3,46,PERIOD                                                     
         SSPEC H4,39,C' (SUPERSEDES INSTRUCTIONS OF OCT10/83)'                  
*                                                                               
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H3,85,C'PURCHASE ORDER'                                          
         SSPEC H4,85,C'REVISION'                                                
         SSPEC H5,85,PAGE                                                       
         SSPEC H5,93,REQUESTOR                                                  
*                                                                               
         DC    X'00'               END MARKER FOR SSPECS                        
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
************************************************************                    
*                              PROGRAM= FOR VK RTN         *                    
* SUBROUTINE VALIDATES OPTIONS CLEAR, SAVE, TOTAL, PATTERN *                    
*                              CPAT=#, CLEAR=ALL           *                    
************************************************************                    
*                                                                               
VOPT     NMOD1 0,**VOPT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    SVOPTDTA,SVOPTDTA                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VOPT00               YES                                         
         BNE   VOPT12                                                           
         CLI   SVPROF16,0          ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX                NO                                          
         CLI   SVPROF16,C'0'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX                NO                                          
         CLI   SVPROF16,C'N'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX                NO                                          
         TM    SVOPT2,OP2SPCMT     WAS CMT= ENTERED                             
         BO    VOPTX                YES                                         
         MVC   GERROR,=Y(CMTREQD)                                               
         B     VOPTTRAP                                                         
VOPT00   CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
*                                                                               
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,BLOCK+64)                                
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA            NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VOPT12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VOPT14              NO, NETHER                                   
VOPT12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VOPT14   DS    0H                                                               
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
         LA    RF,OPTTABLE                                                      
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    RF,L'OPTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VOPTHLP                                                          
*                                                                               
VOPTCLC  CLC   12(0,R4),0(RF)                                                   
VOPTGO   L     RE,10(RF)                                                        
         A     RE,SPTRRR                                                        
         BR    RE                                                               
         EJECT                                                                  
VOPTTEST DS    0H                                                               
         L     R1,ATWA                                                          
         CLI   1(R1),C'*'          THIS A DDS TERMINAL                          
         BE    VOPTEST2             YES                                         
         LA    R2,TRAFAXH                                                       
         CLI   8(R2),C'N'                                                       
         BNE   TSTFAXER                                                         
         LA    R2,TRAOPTH                                                       
VOPTEST2 OI    SVOPT1,OPTTEST      TEST                                         
         B     VOPT90                                                           
*                                                                               
VOPTRERN TM    SVOPT1,X'38'        RERUN                                        
         BNZ   BDKYWDER                                                         
         OI    SVOPT1,OPTRERUN                                                  
         CLI   1(R4),0                                                          
         BE    NODATER                                                          
         GOTO1 DATVAL,DMCB,22(R4),DUB                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),DUB,(3,SVOPTDTE)                                     
         B     VOPT90                                                           
*                                                                               
VOPTREV  TM    SVOPT1,X'58'        REV                                          
         BNZ   BDKYWDER                                                         
         OI    SVOPT1,OPTREV                                                    
         B     VOPT90                                                           
*                                                                               
VOPTNEW  TM    SVOPT1,X'68'        NEW                                          
         BNZ   BDKYWDER                                                         
         OI    SVOPT1,OPTNEW                                                    
         B     VOPT90                                                           
*                                                                               
* CONSTRUCT FLDHDR FOR VALISLN *                                                
*                                                                               
VOPTLEN  DS    0H                  LEN(GTH)                                     
         XC    ELEM,ELEM                                                        
         CLI   1(R4),0             ANY ENTRY                                    
         BE    LENERR              NO, ERROR                                    
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
         GOTO1 VALISLN                                                          
*                                                                               
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          RESTORE R2                                   
         CLI   ERROR,0                                                          
         BNE   TRAPERRA                                                         
         MVC   SVOPTLEN,WORK+4                                                  
         B     VOPT90                                                           
*                                                                               
VOPTSTA  DS    0H                  STATION=                                     
*                                                                               
* CONSTRUCT FLDHDR FOR VALISTA *                                                
*                                                                               
         MVC   SVMKTSTA,BMKTSTA    SAVE MARKET                                  
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
*                                                                               
         GOTO1 VALISTA                                                          
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          POINT TO REAL FLDHDR FOR ERROR RTN           
         CLI   ERROR,0                                                          
         BNE   TRAPERRA                                                         
*                                                                               
         OC    SVMKTSTA(2),SVMKTSTA   WAS MARKET ENTERED                        
         BZ    VOPT66                  NO                                       
         CLC   BMKT,SVMKTSTA          IS STA IN MARKET                          
         BNE   MKTSTAER                                                         
*                                                                               
VOPT66   MVC   SVOPTSTA,BSTA                                                    
         OI    SVOPT1,OPT1STA                                                   
         XC    SVMKTSTA,SVMKTSTA                                                
         B     VOPT90                                                           
*                                                                               
VOPTNAGY OI    SVOPT1,OPTNOAGY     NOAGY                                        
         B     VOPT90                                                           
*                                                                               
VOPTNPRG DS    0H                                                               
         CLI   1(R4),0             MUST BE SECOND ENTRY                         
         BE    MISSERRA                                                         
         CLI   1(R4),18            MAX LENGTH                                   
         BH    PRGSIZER                                                         
         MVC   FTPROGNM,22(R4)     SAVE REQUESTED PROG NAME                     
         MVC   FTPROGLN,1(R4)      SAVE LENGTH                                  
         B     VOPT90                                                           
*                                                                               
VOPTNADR OI    SVOPT2,OP2NOSTA     NOADDR-ACCEPT NO STA ADDR                    
         B     VOPT90                                                           
*                                                                               
VOPTSEED TM    WHEN,X'C0'          TEST IMMED/NOW                               
         BNZ   SEEDERR              ONLY FOR OVERNIGHT                          
         OI    SVOPT1,OPTSEED      SET SEED TO BE RUN, ACCEPT REQUEST           
         B     VOPT90                                                           
*                                                                               
VOPTCMT  CLI   SVPROF16,C'N'       CMT= GET NON-STD SPEC CMT CODE               
         BE    VOPTHLP                                                          
         CLI   SVPROF16,C'0'      DEFAULT                                       
         BE    VOPTHLP                                                          
         CLI   1(R4),1            ONLY 1 CHAR ALLOWED                           
         BNE   VOPT84                                                           
         CLI   SVPROF16,C'A'       ALL SPEC CMT CODES ALLOWED                   
         BE    VOPT86                                                           
         CLC   22(1,R4),SVPROF12                                                
         BE    VOPT88                                                           
         CLC   22(1,R4),SVPROF16                                                
         BE    VOPT88                                                           
VOPT84   MVC   GERROR,=Y(BADCMT2)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,2              L'SUBST TEXT + 1                             
         MVC   ELEM+1(1),SVPROF16                                               
         B     VOPTTRAP                                                         
VOPT86   LA    R0,7                                                             
         LA    R1,=C'$&&@#/*N'                                                  
         CLC   22(1,R4),0(R1)                                                   
         BE    *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-14                                                          
         B     CMTCDER                                                          
         MVC   SVPROF16(1),0(R1)                                                
         B     *+10                                                             
VOPT88   MVC   SVPROF16(1),22(R4)  SAVE SPECIAL COMMENT CODE                    
         OI    SVOPT2,OP2SPCMT                                                  
*                                                                               
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VOPT94   DS    0H                                                               
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPT96                                                           
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPT96                                                           
*                                                                               
         TM    SVOPT1,OPTTEST      TEST                                         
         BO    VOPT96                                                           
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VOPT96   DS    0H                                                               
         CLI   SVPROF16,0          ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX               NO                                           
         CLI   SVPROF16,C'0'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX               NO                                           
         CLI   SVPROF16,C'N'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPTX               NO                                           
         TM    SVOPT2,OP2SPCMT       WAS CMT= ENTERED                           
         BO    VOPTX               YES                                          
         MVC   GERROR,=Y(CMTREQD)                                               
         B     VOPTTRAP                                                         
VOPTX    XIT1                                                                   
         EJECT                                                                  
VOPTHLP  MVC   CONHEAD,OPTHLPMS                                                 
         B     XERREX2                                                          
*                                                                               
TSTFAXER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=CL32'* USE FAX=NO WITH TEST REQUEST *'              
         B     XERREX2                                                          
*                                                                               
LENERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'* ERROR * MUST ENTER THE LENGTH *'                
         B     XERREX2                                                          
*                                                                               
PRGSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGSIZMS),PRGSIZMS                                     
         B     XERREX2                                                          
*                                                                               
SEEDERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SEEDMS),SEEDMS                                         
XERREX2  GOTO1 ERREX2                                                           
*                                                                               
CMTCDER  MVC   GERROR,=Y(BADCMT)                                                
         B     VOPTTRAP                                                         
MKTSTAER MVC   GERROR,=Y(WRONGMKT)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R1),5             L'SUBST TEXT + 1                             
         MVC   1(4,R1),QMKT                                                     
         LA    R1,5(R1)                                                         
         MVI   0(R1),5             L'SUBST TEXT + 1                             
         SR    R0,R0                                                            
         ICM   R0,3,SVMKTSTA                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(4,R1),DUB                                                      
VOPTTRAP DS   0H                                                                
         BAS   RE,CLRSCN           GO CLEAR BOTTOM OF SCREEN                    
         GOTO1 VTRAERR                                                          
*                                                                               
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
*                                                                               
NODATER  MVI   ERROR,NODATE        MUST SPECIFY ORIGINAL INST DATE              
         B     TRAPERRA                                                         
*                                                                               
DATERRA  MVI   ERROR,INVDATE                                                    
         B     TRAPERRA                                                         
*                                                                               
BDKYWDER MVI   ERROR,BADKEYWD      KEY WORDS USED IN BAD COMBINATION            
*                                                                               
TRAPERRA DS   0H                                                                
         BAS   RE,CLRSCN           GO CLEAR BOTTOM OF SCREEN                    
         GOTO1 ERREX                                                            
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCN   LA    RF,TRASEL1H                                                      
*                                                                               
CLRSCN10 OC    8(L'TRASEL1,RF),8(RF)                                            
         BZ    CLRSCN20                                                         
         CLC   8(L'TRASEL1,RF),SPACES                                           
         BE    CLRSCN20                                                         
         XC    8(L'TRASEL1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         OI    1(RF),X'20'         SET PROTECT BIT                              
CLRSCN20 ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
*                                                                               
         OC    8(L'TRADSP1,RF),8(RF)                                            
         BZ    CLRSCN30                                                         
         CLC   8(L'TRADSP1,RF),SPACES                                           
         BE    CLRSCN30                                                         
         XC    8(L'TRADSP1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
CLRSCN30 IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),9                                                          
         BH    CLRSCN10                                                         
         BR    RE                                                               
*                                                                               
OPTHLPMS DC    CL60'* ERROR * OPTNS=TEST/STATION/NOAGY/NOADDR/LEN/SEED C        
               *'                                                               
PRGSIZMS DC    C'* ERROR * MAX LENGTH FOR PROGRAM IS 18 *'                      
SEEDMS   DC    C'* ERROR * OPTION SEED ONLY FOR OVERNIGHT REQUESTS *'           
*                                                                               
OPTTABLE DS    0CL14                                                            
         DC    CL10'CMT       ',AL4(VOPTCMT)                                    
         DC    CL10'LEN       ',AL4(VOPTLEN)                                    
         DC    CL10'NEW       ',AL4(VOPTNEW)                                    
         DC    CL10'NOADDR    ',AL4(VOPTNADR)                                   
         DC    CL10'NOAGY     ',AL4(VOPTNAGY)                                   
         DC    CL10'PROGRAM   ',AL4(VOPTNPRG)                                   
         DC    CL10'RERUN     ',AL4(VOPTRERN)                                   
         DC    CL10'REV       ',AL4(VOPTREV)                                    
         DC    CL10'SEED      ',AL4(VOPTSEED)                                   
         DC    CL10'STATION   ',AL4(VOPTSTA)                                    
         DC    CL10'TEST      ',AL4(VOPTTEST)                                   
         DC    CL10'HELP      ',AL4(VOPTHLP)                                    
         DC    X'FF'                                                            
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
********************************************                                    
* READ THROUGH BUYS AND BUILD STATION LIST *                                    
********************************************                                    
*                                                                               
BLSTA    NMOD1 0,**BLSTA*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
* CLEAR STATION TABLE BUILD AREA *                                              
*                                                                               
         LM    RE,RF,ASTABL                                                     
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),BAGYMD       A-M/CLT/PRD                                  
         MVC   KEY+4(5),BMKTSTA                                                 
*                                                                               
         GOTO1 HIGH                                                             
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   BLS05                                                            
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    BLS10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    BLS10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART                                
         BE    BLS10                                                            
BLS05    DS    0H                                                               
*MNMB                                                                           
         SR    R5,R5                                                            
         B     BLS11                                                            
*                                                                               
BLS10    GOTO1 SEQ                                                              
*                                                                               
BLS11    DS    0H                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   BLS11C                                                           
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    BLS10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    BLS10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART                                
         BE    BLS10                                                            
BLS11C   DS    0H                                                               
*MNMB                                                                           
         OC    SVOPTSTA,SVOPTSTA   WAS STA ENTERED                              
         BZ    BLS12                NO                                          
*                                                                               
         CLC   KEY(9),KEYSAVE      A-M/CLT/PRD/MKT/STA                          
         BE    BLS20                                                            
         B     BLS14                                                            
*                                                                               
BLS12    CLI   TRAMKTH+5,0         WAS MARKET ENTERED                           
         BE    BLS13                NO                                          
*                                                                               
         CLC   KEY(6),KEYSAVE      A-M/CLT/PRD/MKT                              
         BE    BLS20                                                            
         B     BLS14                                                            
*                                                                               
BLS13    CLC   KEY(4),KEYSAVE      A-M/CLT/PRD                                  
         BE    BLS20                                                            
*                                                                               
* SORT ENTRIES IN STATION ORDER                                                 
*                                                                               
BLS14    GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         ZIC   R2,STACT            GET NUMBER OF STATIONS                       
         LTR   R2,R2               IF NO STATIONS FOUND                         
         BNZ   BLS16                                                            
         CR    R2,RB               SET CC NE FOR NO SPOTS FOUND                 
         B     BLSX                                                             
*                                                                               
* SORT BY: MARKET AND STATION                                                   
*                                                                               
BLS16    GOTO1 =V(XSORT),DMCB,ASTABL,(R2),L'STADATA,13,0,RR=SPTRRR              
*                                                                               
BLS18    CR    R2,R2               SET COND CODE                                
*                                                                               
BLSX     XIT1                                                                   
*                                                                               
BLS20    CLI   KEY+3,X'FF'         READING POL (ALL PRODS)                      
         BE    BLS22                                                            
*                                                                               
         CLI   KEY+10,X'FF'        ONLY READ POL BUYS                           
         BNE   BLS10                                                            
*                                                                               
BLS22    ZIC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BE    BLS10                NO                                          
*                                                                               
         CLI   TRAMKTH+5,0         WAS MARKET ENTERED                           
         BE    BLS24                NO                                          
         CLC   KEY(6),KEYSAVE      A-M/CLT/PRD/MKT                              
         BE    BLS26                                                            
         B     BLS14                                                            
BLS24    CLC   KEY(3),KEYSAVE      A-M/CLT/PRD                                  
         BNE   BLS14                                                            
*                                                                               
BLS26    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
         TM    15(R4),X'80'        TEST DELETED                                 
         BO    BLS10               YES - IGNORE                                 
*                                                                               
         CLC   KEY+4(2),4(R4)      TEST SAME MARKET                             
         BNE   BLS10                NO - SPILL, BYPASS                          
*                                                                               
* ONLY POL PROCESSING *                                                         
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLS10                                                            
         MVC   KEYSAVE+6(3),KEY+6  SAVE NEW STATION                             
*                                                                               
* FILTER ON PROGRAM NAME IF REQUESTED *                                         
*                                                                               
         CLI   FTPROGLN,0          FILTERING ON PROGRAM NAME                    
         BE    BLS28                                                            
         ZIC   RF,FTPROGLN                                                      
         BCTR  RF,0                                                             
         EX    RF,CKPRGNM                                                       
         BNE   BLS10                                                            
         B     BLS28                                                            
*                                                                               
CKPRGNM  CLC   FTPROGNM(0),BDPROGRM                                             
*                                                                               
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
*                                                                               
BLS28    DS   0H                                                                
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
*                                                                               
*                                                                               
* LOOK FOR TRAFFIC=NO                                                           
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLS29    BAS   RE,BUYEL                                                         
         BNE   BLS29X                                                           
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    BLS10               BYPASS THIS BUY                              
         B     BLS29                                                            
*                                                                               
*                                                                               
BLS29X   MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         XC    ELDATE,ELDATE                                                    
*                                                                               
BLS30    BAS   RE,BUYEL                                                         
         BNE   BLS10                                                            
         USING REGELEM,R6                                                       
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSED                        
         BNZ   BLS30                                                            
         CLI   RLEN,10             TEST UNALL                                   
         BNH   BLS30                                                            
         CLC   10(2,R6),SVPRD      SAME PROD AS PREV                            
         BE    BLS32                                                            
         XC    SVPRODS,SVPRODS                                                  
         MVC   SVPRD(2),10(R6)                                                  
         LA    R1,SVPROD                                                        
         BAS   RE,FPRODS                                                        
BLS32    CLI   1(R6),18            THIS PIGGYBACK PROD                          
         BE    *+14                                                             
         XC    SVPROD2(5),SVPROD2                                               
         B     BLS34                                                            
         CLC   14(2,R6),SVPRD2     SAME PROD AS PREV                            
         BE    BLS34                                                            
         MVC   SVPRD2(2),14(R6)                                                 
         LA    R1,SVPROD2                                                       
         BAS   RE,FPRODS                                                        
         CLC   SVPROD,SVPROD2      SEE IF PRODS IN ALPHA ORDER                  
         BL    BLS34                                                            
*                                                                               
* REVERSE PRODUCT ORDER                                                         
*                                                                               
         MVC   DUB(5),SVPROD                                                    
         MVC   SVPROD(5),SVPROD2                                                
         MVC   SVPROD2(5),DUB                                                   
*                                                                               
BLS34    CLI   BPRD,255            PROD POL ENTERED                             
         BE    *+14                                                             
         CLC   BPRD,SVPRD          SAME PROD                                    
         BNE   BLS30                                                            
*                                                                               
         CLI   BPRD2,0             PROD2 ENTERED                                
         BE    BLS36                NO                                          
         CLI   BPRD2,X'FF'         PROD2 NONE ENTERED                           
         BNE   *+16                                                             
         CLI   RLEN,18             TEST PIGGYBACK                               
         BNE   BLS36                                                            
         B     BLS30                                                            
*                                                                               
         CLI   RLEN,18             TEST PIGGYBACK                               
         BNE   BLS30                                                            
         CLC   BPRD2,SVPRD2        SAME PROD                                    
         BNE   BLS30                                                            
*                                                                               
BLS36    CLI   SVOPTLEN,0                                                       
         BE    BLS37                                                            
         ZIC   RE,SVSLN                                                         
         ZIC   RF,SVSLN2                                                        
         AR    RE,RF                                                            
         CLM   RE,1,SVOPTLEN                                                    
         BNE   BLS30                                                            
BLS37    MVC   ELDATE,RDATE                                                     
         CLC   ELDATE,GENENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    BLS30                                                            
         MVC   ELDATEX,RDATE       AND PRESET ELEM END DATE                     
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLS38                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
         DROP  R6                                                               
*                                                                               
BLS38    CLC   ELDATEX,GENSTP      TEST BEFORE FLIGHT/TELECAST                  
         BL    BLS30                                                            
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R3,STAWORK                                                       
         USING STABLED,R3                                                       
         MVC   STAMSTA,KEY+4                                                    
         MVC   STAPROD,SVPROD                                                   
         MVC   STASLN,SVSLN                                                     
         MVC   STAPROD2,SVPROD2                                                 
         MVC   STASLN2,SVSLN2                                                   
         MVC   STAFTD,=X'FFFF'                                                  
*                                                                               
* TEST ENTRY IN TABLE ALREADY *                                                 
*                                                                               
         L     R3,ASTABL                                                        
         USING STABLED,R3                                                       
BLS40    OC    STAMSTA,STAMSTA                                                  
         BZ    BLS44                                                            
         CLC   STACOMP,STAWORK     THIS ENTRY IN TABLE                          
         BE    BLS46                                                            
         LA    R3,STANEXT                                                       
         C     R3,ASTABLX                                                       
         BL    BLS40                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, KEEP ON GOING                    
         BE    BLS44                                                            
         TM    WHEN,X'38'          TEST OVERNIGHT/SOON/DDS                      
         BNZ   BLS18                YES, EXIT AS THOUGH OK                      
*                                                                               
         MVC   GERROR,=Y(MANYSTA)                                               
         B     BLSERX2                                                          
         EJECT                                                                  
* ADD NEW ENTRY TO TABLE                                                        
*                                                                               
BLS44    MVC   STADATA,STAWORK                                                  
         ZIC   R1,STACT                                                         
         LA    R1,1(,R1)                                                        
         STC   R1,STACT                                                         
         XC    STANEXT(L'STADATA),STANEXT                                       
*                                                                               
BLS46    CLC   STAFTD,ELDATE                                                    
         BNH   *+10                                                             
         MVC   STAFTD,ELDATE                                                    
         CLC   STALTD,ELDATEX                                                   
         BNL   *+10                                                             
         MVC   STALTD,ELDATEX                                                   
         SR    RF,RF                                                            
         ICM   RF,3,STASPTS                                                     
         LA    RF,1(,RF)                                                        
         STCM  RF,3,STASPTS                                                     
*                                                                               
         LR    RE,R6                                                            
BLS50    ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),13            ANOTHER BUY ELEM                             
         BNH   BLS54                YES                                         
         CLI   0(RE),24            THIS A SPOT ASSIGN ELEMENT                   
         BNE   BLS50                NO, KEEP ON LOOKING                         
         CLI   1(RE),8             THIS A DEALER TAG ELEMENT                    
         BH    DLRTAGER             YES, PROBLEM                                
*                                                                               
         OC    2(2,RE),2(RE)       WAS THIS SPOT ASSIGNED                       
         BNZ   BLS30                YES                                         
*                                                                               
BLS54    SR    R1,R1                                                            
         ICM   R1,3,STANOASG                                                    
         LA    R1,1(,R1)                                                        
         STCM  R1,3,STANOASG                                                    
         B     BLS30                                                            
         DROP  R3,R4                                                            
*                                                                               
BUYEL    CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
* FIND 3 CHAR PROD CODE FROM 1 BINARY CODE                                      
*                                                                               
FPRODS   L     RF,ASVCLIST                                                      
FPRODS10 CLC   3(1,R1),3(RF)                                                    
         BE    FPRODS20                                                         
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    FPRODS10                                                         
         DC    H'0'                                                             
FPRODS20 MVC   0(3,R1),0(RF)                                                    
         BR    RE                                                               
*                                                                               
DLRTAGER MVC   GERROR,=Y(SPDLRTAG)                                              
BLSERX2  LA    R2,TRAMEDH                                                       
         GOTO1 VTRAERR                                                          
         DROP  RB,RC                                                            
         EJECT                                                                  
****************************************************************                
* READ THROUGH BUYS AND BUILD SPOT ASSIGNED LIST FOR 1 STATION *                
****************************************************************                
*                                                                               
BLACT    NMOD1 0,**BLACT*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
* CLEAR FIRST SPOT ACTIVITY LIST BUILD AREA *                                   
*                                                                               
         LM    RE,RF,ASPTABL                                                    
         SR    RF,RE                                                            
         XCEF                                                                   
         XC    SPOTCT,SPOTCT       AND SPOT COUNT                               
         NI    ANYFLAG,X'FF'-PRD2FSW INIT PROD 2 FOUND SW                       
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO MEDIA SPOT SYSTEM                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),BAGYMD A/M, CLT, BPRD                                     
         MVC   KEY+4(5),SVMKTSTA                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     BLA12                                                            
*                                                                               
BLA10    GOTO1 SEQ                                                              
*                                                                               
BLA12    CLC   KEY(9),KEYSAVE      A-M/CLT/PRD/MKT/STA                          
         BNE   BLA14               DONE BUILD                                   
*                                                                               
         CLI   KEY+3,X'FF'         PROD POL                                     
         BE    *+12                                                             
         CLI   KEY+10,X'FF'                                                     
         BNE   BLA10                                                            
*                                                                               
         ZIC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BE    BLA10               NO                                           
         B     BLA18               YES                                          
*                                                                               
* SORT SPOTS IN DATE ORDER                                                      
*                                                                               
BLA14    GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         MVI   SPOTNUMB,0                                                       
         LH    R2,SPOTCT           GET NUMBER OF SPOTS                          
         LTR   R2,R2               IF NO SPOTS FOUND                            
         BNZ   BLA16                                                            
         CR    R2,RB               SET CC NE FOR NO SPOTS FOUND                 
         B     BLAX                                                             
*                                                                               
* SORT BY: PROD/LEN/PROD2/LEN2/FTD/TIME                                         
*                                                                               
BLA16    CLI   SVTZPR03,C'Y'       SORT BY DATE/TIME/PRD/PTR                    
         BE    BLA17                YES                                         
         CLI   SVTZPR04,C'Y'       SHOW BUYS IN BUYLINE ORDER                   
         BE    BLA17                YES                                         
*                                                                               
         GOTO1 =V(XSORT),DMCB,ASPTABL,(R2),L'SPTDATA,L'SPTSORT,0,      C        
               RR=SPTRRR                                                        
*                                                                               
         B     BLAX                                                             
*                                                                               
BLA17    GOTO1 =V(XSORT),DMCB,ASPTABL,(R2),L'SPTDATA,4,                C        
               SPTPROD2-SPTDATA,RR=SPTRRR                                       
*                                                                               
         GOTO1 (RF),(R1),,,,4,SPTPROD-SPTDATA                                   
*                                                                               
         CLI   SVTZPR04,C'Y'       SHOW BUYS IN SPOT LINE ORDER                 
         BNE   BLA17F               NO                                          
         GOTO1 (RF),(R1),,,,1,SPTSPTN-SPTDATA  (SPOT LINE)                      
*                                                                               
BLA17F   GOTO1 (RF),(R1),,,,6,SPTFTD-SPTDATA  (FTD/TIME)                        
*                                                                               
BLAX     CR    R2,R2               SET COND CODE                                
         XIT1                                                                   
*                                                                               
BLA18    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
         TM    15(R4),X'80'        TEST DELETED                                 
         BO    BLA10               YES - IGNORE                                 
*                                                                               
         CLC   KEY+4(2),4(R4)      TEST SAME MARKET                             
         BNE   BLA10                NO - SPILL, BYPASS                          
*                                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   BLA18C                                                           
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    BLA10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    BLA10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART                                
         BE    BLA10                                                            
BLA18C   DS    0H                                                               
*MNMB                                                                           
* ONLY POL PROCESSING *                                                         
*                                                                               
         CLI   KEY+3,X'FF'         READING POL (ALL PRODS)                      
         BE    *+12                                                             
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLA10                                                            
*                                                                               
* FILTER ON PROGRAM NAME IF REQUESTED *                                         
*                                                                               
         CLI   FTPROGLN,0          FILTERING ON PROGRAM NAME                    
         BE    BLA19                                                            
         ZIC   RF,FTPROGLN                                                      
         BCTR  RF,0                                                             
         EX    RF,CKPRGNMB                                                      
         BNE   BLA10                                                            
         B     BLA19                                                            
*                                                                               
CKPRGNMB CLC   FTPROGNM(0),BDPROGRM                                             
*                                                                               
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
*                                                                               
BLA19    DS   0H                                                                
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
*                                                                               
         SR    R5,R5               RESET SPOT NUMBER                            
*                                                                               
* LOOK FOR TRAFFIC=NO                                                           
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLA19C   BAS   RE,BUYEL1                                                        
         BNE   BLA19X                                                           
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    BLA10               BYPASS THIS BUY                              
         B     BLA19C                                                           
*                                                                               
*                                                                               
BLA19X   MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         XC    ELDATE,ELDATE                                                    
*                                                                               
BLA20    BAS   RE,BUYEL1                                                        
         BNE   BLA10                                                            
         USING REGELEM,R6                                                       
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSED                        
         BNZ   BLA20                                                            
         CLI   RLEN,10            TEST UNALL                                    
         BNH   BLA20                                                            
         CLC   ELDATE,RDATE        SAVE ELEM START DATE                         
         BE    BLA22                                                            
         SR    R5,R5               RESET SPOT NUMBER                            
BLA22    MVC   ELDATE,RDATE        SAVE ELEM START DATE                         
         LA    R5,1(,R5)           ADD TO ELEMENT CTR                           
         XC    SVPRODS,SVPRODS                                                  
         MVC   SVPRD(2),10(R6)                                                  
         LA    R1,SVPROD                                                        
         BAS   RE,FPRODA                                                        
         MVI   BYTE,0                                                           
         CLI   1(R6),18            THIS PIGGYBACK PROD                          
         BNE   BLA22A                                                           
*                                                                               
         OI    ANYFLAG,PRD2FSW     PROD 2 FOUND SW                              
*                                                                               
         MVC   SVPRD2(2),14(R6)                                                 
         LA    R1,SVPROD2                                                       
         BAS   RE,FPRODA                                                        
*                                                                               
         CLI   SVTZPR04,C'Y'       SHOW PRODS IN SAME ORDER AS BUY              
         BE    BLA22A                                                           
*                                                                               
         CLC   SVPROD,SVPROD2      SEE IF PRODS IN ALPHA ORDER                  
         BL    BLA22A                                                           
*                                                                               
* REVERSE PRODUCT ORDER                                                         
*                                                                               
         MVC   DUB(5),SVPROD                                                    
         MVC   SVPROD(5),SVPROD2                                                
         MVC   SVPROD2(5),DUB                                                   
         MVI   BYTE,1                                                           
*                                                                               
BLA22A   CLI   BPRD,255            POL PROD                                     
         BE    *+14                                                             
         CLC   BPRD,SVPRD          SAME PROD                                    
         BNE   BLA20                BYPASS                                      
*                                                                               
         CLI   BPRD2,255           PTR PROD NONE                                
         BNE   BLA22C                                                           
         CLI   SVPRD2,0            ANY PTR PROD                                 
         BNE   BLA20                YES, BYPASS                                 
         B     BLA22E                                                           
*                                                                               
BLA22C   CLI   BPRD2,0             PTR PROD SPECIFIC                            
         BE    BLA22E               NO                                          
         CLC   BPRD2,SVPRD2        SAME PROD                                    
         BNE   BLA20                                                            
*                                                                               
BLA22E   MVC   ELDATEX,RDATE       AND PRESET ELEM END DATE                     
*                                                                               
         CLC   ELDATE,GENENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    BLA20                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLA24                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
         DROP  R6                                                               
*                                                                               
BLA24    CLC   ELDATEX,GENSTP      TEST BEFORE FLIGHT/TELECAST                  
         BL    BLA20                                                            
*                                                                               
         XC    SPTWORK,SPTWORK                                                  
         LA    R3,SPTWORK                                                       
         USING SPTABLED,R3                                                      
         MVC   SPTPROD,SVPROD                                                   
         MVC   SPTSLN,SVSLN                                                     
         MVC   SPTPROD2,SVPROD2                                                 
         MVC   SPTSLN2,SVSLN2                                                   
         MVC   SPTFTD,ELDATE                                                    
         MVC   SPTLTD,ELDATEX                                                   
*                                                                               
         STC   R5,SPTSPTN                                                       
         MVC   SPTDAY,BDDAY                                                     
         MVC   SPTTIME,BDTIMST                                                  
         MVC   SPTEST,BUYKEST                                                   
*                                                                               
         LLC   R0,BUYKEY+10                                                     
         TM    BUYRCNTL,BUYRLN2                                                 
         BZ    *+8                                                              
         ICM   R0,3,BUYKEY+10                                                   
         STCM  R0,3,SPTLINE                                                     
*                                                                               
         MVC   SPTDPT,BDDAYPT                                                   
         MVC   SPTDSKAD,KEY+14                                                  
         MVC   SPTPRD,SVPRD                                                     
         MVC   SPTPRD2,SVPRD2                                                   
         LR    RE,R6                                                            
BLA26    ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),13            THIS ANOTHER SPOT                            
         BNH   BLA30                YES, ADD AS UNASSIGNED                      
         CLI   0(RE),24            THIS A CMML ASSIGN ELEMENT                   
         BNE   BLA26                NO, LOOK FURTHUR                            
         MVC   SPTPREF,6(RE)         ALSO POSSIBLE PATTERN REF                  
         MVC   SPTCMLSQ(4),2(RE)    SAVE CMML SEQ 1 & 2                         
         CLI   BYTE,0              WAS PRODUCT SEQ FLIPPED                      
         BE    BLA30                NO                                          
         MVC   SPTCMLS2,2(RE)                                                   
         MVC   SPTCMLSQ,4(RE)                                                   
*                                                                               
* TEST DATA IN TABLE ALREADY *                                                  
*                                                                               
BLA30    L     R3,ASPTABL                                                       
*                                                                               
BLA32    OC    SPTFTD,SPTFTD       AT END OF SPOTS                              
         BZ    BLA40                YES                                         
*                                                                               
         CLC   SPTDATA(L'SPTDATA),SPTWORK THIS ENTRY TO ALL OTHERS              
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SPTNEXT                                                       
         B     BLA32                                                            
*                                                                               
BLA40    C     R3,ASPTABLX                                                      
         BNL   ACTSIZER                                                         
*                                                                               
         LH    R1,SPOTCT           UPDATE TOTAL SPOTS COUNTER                   
         LA    R1,1(R1)                                                         
         STH   R1,SPOTCT                                                        
*                                                                               
         MVC   SPTDATA,SPTWORK                                                  
         XC    L'SPTDATA(L'SPTDATA,R3),L'SPTDATA(R3)                            
         B     BLA20                                                            
*                                                                               
         DROP  R3,R4                                                            
BUYEL1   CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL1                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL1                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
* FIND 3 CHAR PROD CODE FROM 1 BINARY CODE                                      
*                                                                               
FPRODA   L     RF,ASVCLIST                                                      
FPRODA10 CLC   3(1,R1),3(RF)                                                    
         BE    FPRODA20                                                         
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    FPRODA10                                                         
         DC    H'0'                                                             
FPRODA20 MVC   0(3,R1),0(RF)                                                    
         BR    RE                                                               
*                                                                               
ACTSIZER MVC   GERROR,=Y(MANYSPOT)                                              
         MVI   GMSGTYPE,C'E'                                                    
         CLI   OFFLINE,C'Y'        SET COND CODE                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VTRAERR                                                          
         DROP  RB,RC                                                            
         EJECT                                                                  
* READ STATION ADDRESS RECORD AND SAVE IT *                                     
*                                                                               
RSTA     NMOD1 0,**RSTA                                                         
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),QMKT,DUB                            
*                                                                               
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
* READ STATION MASTER RECORD FOR TYPE *                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
*                                                                               
         MVC   SVSTATYP,STYPE                                                   
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),QSTA                                                    
         GOTO1 HIGH                                                             
         L     R6,AIO1                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    RSTA10                                                           
*                                                                               
         TM    SVOPT2,OP2NOSTA    IGNORE MISSING ADDRESS                        
         BO    *+12                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   NOSTADER                                                         
         MVC   0(100,R6),SPACES                                                 
         MVC   2(5,R6),QSTA                                                     
         B     RSTA20                                                           
*                                                                               
RSTA10   GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSTA20   L     RE,NEXTADDR                                                      
         MVC   0(8,RE),=C'STA ADDR'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASVSTAD                                                       
*                                                                               
         MVC   0(24,RE),=CL24'** TV OPERATIONS DESK **'                         
         CLI   QMED,C'T'                                                        
         BE    RSTA24                                                           
         MVC   0(24,RE),=CL24'RADIO OPERATIONS DESK'                            
         CLI   QMED,C'R'                                                        
         BE    RSTA24                                                           
         MVC   0(24,RE),=CL24'**  OPERATIONS DESK **'                           
RSTA24   MVC   24(96,RE),2(R6)                                                  
         LA    RE,120(RE)                                                       
         ST    RE,NEXTADDR         AND SET AS NEXT ADDRESS                      
         XIT1                                                                   
NOSTADER MVI   ERROR,NOSTAADR                                                   
         GOTO1 ERREX               IN OPTION HEADING FIELD                      
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* PRINT COMMENTS, BOXED OR NOT                                                  
*                                                                               
PCMT     NMOD1 0,**PCMT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         L     R4,0(R1)            OUTPUT AREA ADDR                             
         ZIC   R7,0(R1)            LENGTH OF OUTPUT AREA                        
         SR    R2,R2               ZERO LINE CT                                 
         L     R6,AIO                                                           
         TM    UPDSW,X'08'         NEED TO GETREC/CK FOR BOX                    
         BZ    PCMT04              NO, DONE                                     
         GOTO1 GETREC                                                           
*                                                                               
PCMT04   MVI   HALF,0                                                           
         TM    UPDSW,X'04'         NEED TO CK FOR BOXES                         
         BO    PCMT06              NO, NOT ALLOWED                              
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   HALF,2(R6)          SAVE IND BYTE                                
*                                                                               
PCMT06   L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NONE ',3(R6)     TEST SUPPRESS COMMENT                        
         BE    PCMT40                                                           
         CLC   =C'ID=',3(R6)                                                    
         BNE   PCMT08                                                           
         BRAS  RE,NEXTEL                                                        
PCMT08   TM    HALF,X'80'          TEST TO BOX COMMENT                          
         BZ    PCMT10              NO                                           
         GOTO1 BOXER,DMCB,((R7),(R4))                                           
         B     PCMT40                                                           
*                                                                               
* FORMAT UNBOXED COMMENT *                                                      
*                                                                               
PCMT10   L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
PCMT12   BAS   RE,CEP              GO CK FOR END OF PAGE                        
         MVC   0(59,R4),SPACES                                                  
         MVI   60(R4),0            FORCE BLANK COMMENTS TO PRINT                
         TM    UPDSW,X'02'         CK FOR =NAME                                 
         BZ    PCMT30                                                           
         CLC   =C'=NAME',3(R6)     DOES DEALER NAME GO HERE                     
         BNE   PCMT30              NO                                           
         MVC   0(24,R4),0(R5)      DEALER NAME                                  
         LA    R1,24(,R4)                                                       
PCMT14   CLI   0(R1),C' '                                                       
         BH    PCMT16                                                           
         BCTR  R1,0                                                             
         B     PCMT14                                                           
PCMT16   MVI   1(R1),C','                                                       
         MVC   2(24,R1),24(R5)     DEALER ADDRESS                               
         LA    R4,0(R4,R7)                                                      
         LA    R2,1(,R2)                                                        
         BAS   RE,CEP              CK END OF PAGE                               
         OC    48(48,R5),48(R5)    DEALER 2 NAME                                
         BZ    PCMT34                                                           
         MVC   0(24,R4),48(R5)                                                  
         LA    R1,24(,R4)                                                       
PCMT20   CLI   0(R1),C' '                                                       
         BH    PCMT24                                                           
         BCTR  R1,0                                                             
         B     PCMT20                                                           
PCMT24   MVI   1(R1),C','                                                       
         MVC   2(24,R1),72(R5)     DEALER 2 ADDRESS                             
         LA    R4,0(R4,R7)                                                      
         LA    R2,1(,R2)                                                        
         B     PCMT34                                                           
PCMT30   ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
PCMT34   LA    R4,0(R4,R7)                                                      
         LA    R2,1(,R2)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    PCMT12                                                           
         CH    R7,=H'132'          PRINTING                                     
         BE    PCMT40               BYPASS EOL FLAG                             
         MVI   0(R4),0             SET EOL FLAG                                 
PCMT40   NI    UPDSW,X'FF'-X'08'-X'04'-X'02'                                    
*                                                                               
         BRAS  RE,GOSPL                                                         
PCMTX    XIT1                                                                   
*                                                                               
         DS    0H                                                               
CEP      NTR1                                                                   
         CH    R7,=H'132'          PRINTING                                     
         BNE   CEP10                                                            
         CH    R2,=H'4'                                                         
         BL    CEP10                                                            
*                                                                               
         BRAS  RE,GOSPL                                                         
         MVI   ALLOWLIN,0          RESET                                        
         SR    R2,R2                                                            
         LR    R1,R7                                                            
         SLL   R1,2                                                             
         SR    R4,R1                                                            
CEP10    XIT1  REGS=(R2,R4)                                                     
         EJECT                                                                  
*********************************************                                   
* SUBROUTINE TO PRINT BOXES AROUND TEXT     *                                   
* AIO MUST HAVE RECORD ADDRESS              *                                   
* ELCODE MUST CONTAIN COMMENT ELEM CODE     *                                   
* P1  (1) = MAXIMUM LEN OF EXPANDED COMMENT *                                   
* P1+1(3) = EXPANDED COMMENT OUTPUT AREA    *                                   
*********************************************                                   
*                                                                               
         DS    0H                                                               
BOXER    NTR1                                                                   
*                                                                               
* FIRST - FIND LENGTH OF LONGEST COMMENT *                                      
*                                                                               
         L     R3,0(R1)            GET OUTPUT AREA ADDRESS                      
         ZIC   R4,0(R1)            GET OUTPUT RECORD SIZE                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
*                                                                               
BOX2     ZIC   RE,1(R6)                                                         
         CLC   =C'BOX=',3(R6)                                                   
         BNE   *+8                                                              
         SH    RE,=H'4'                                                         
         CR    R5,RE                                                            
         BH    *+6                                                              
         LR    R5,RE                                                            
         BRAS  RE,NEXTEL                                                        
         BE    BOX2                                                             
*                                                                               
* LENGTH IN R5 INCLUDES 3 FOR ELCODE/LEN/SEQ - NOW ADJUST FOR                   
* '*/SP' AND 'SP/*' AT EITHER END                                               
*                                                                               
         LA    R5,1(R5)                                                         
*                                                                               
* CREATE ROW OF *'S THIS LENGTH                                                 
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R3),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R3,R4               POINT TO NEXT OUTPUT LINE                    
         LA    R5,2(R5)            RESTORE LENGTH                               
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
BOX4     EX    R4,BOXSPC                                                        
*                                                                               
         ZIC   RE,1(R6)                                                         
         LA    RF,3(R6)                                                         
*                                                                               
         CLC   =C'BOX=',0(RF)                                                   
         BNE   *+12                                                             
         SH    RE,=H'4'                                                         
         LA    RF,4(RF)                                                         
*                                                                               
         MVI   0(R3),C'*'                                                       
         SH    RE,=H'4'            SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),0(RF) *EXECUTED*                                         
*                                                                               
         LA    RE,0(R3,R5)         POINT TO END OF LINE                         
         BCTR  RE,0                BACK UP                                      
         MVI   0(RE),C'*'                                                       
         AR    R3,R4               POINT TO NEXT LINE                           
         BRAS  RE,NEXTEL                                                        
         BE    BOX4                                                             
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R3),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R3,R4                                                            
         MVI   0(R3),0             SET END OF BUFFER FLAG                       
         XIT1                                                                   
*                                                                               
BOXR7    MVC   1(0,R3),0(R3)  *EXECUTED*                                        
BOXSPC   MVC   0(0,R3),SPACES *EXECUTED*                                        
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD             *                 
***************************************************************                 
*                                                                               
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
*                                                                               
         CLI   SVPROF11,C'E'       BY EST                                       
         BE    VPER26                                                           
*                                                                               
         LA    R1,DMCB                                                          
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04               NO                                          
         GOTO1 DATVAL,(R1),9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVGENST)                             
         B     VPER06                                                           
VPER04   GOTO1 DATCON,(R1),(5,0),(3,SVGENST)                                    
*                                                                               
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVGENST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         GOTO1 HIGH                                                             
*                                                                               
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VPER14   BRAS  RE,NEXTEL                                                        
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         GOTO1 ERREX2                                                           
*                                                                               
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVPERST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVPEREND),(4,CONHEAD+21)                            
         GOTO1 ERREX2                                                           
*                                                                               
VPER30   XC    SVGENDTS,SVGENDTS                                                
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER32                                                           
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         GOTO1 DATCON,DMCB,(3,SVPERST),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVPEREND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVPERST),SVQSTART                                   
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
         MVC   SVGENDTS,SVPERDTS                                                
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER46                                                           
*                                                                               
VPER32   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVGENST)                             
*                                                                               
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40               YES                                         
*                                                                               
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRB                                                          
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVGENEND)                              
         EJECT                                                                  
* CK DATES ENTERED TO BE FLIGHT OR ESTIMATE DATES *                             
*                                                                               
VPER40   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER50                                                           
         BAS   RE,FFLT                                                          
*                                                                               
* GET FLIGHT/TELECAST DATES IN 2 BYTE FORM *                                    
*                                                                               
VPER46   GOTO1 DATCON,DMCB,(3,SVGENST),(2,GENSTP)                               
         GOTO1 (RF),(R1),(3,SVGENEND),(2,GENENDP)                               
         CLC   SVGENST,SVGENEND                                                 
         BH    DATERRB                                                          
         CLI   SVTZPR01,C'Y'       FLEXIBLE DATES OPTION ON?                    
         BNE   VPERX                                                            
*                                                                               
         CLC   SVGENST,SVPERST     IS START DATE = PERIOD START                 
         BE    VPER48                                                           
         GOTO1 GETDAY,DMCB,SVQSTART,WORK                                        
         CLI   DMCB,1              IS START MONDAY                              
         BNE   MONERR                                                           
         CLC   WORK(3),SPACES      BETTER NOT BE ERROR                          
         BNE   VPER48                                                           
         DC    H'0'                                                             
*                                                                               
VPER48   CLC   SVGENEND,SVPEREND   IS END DATE = PERIOD END                     
         BE    VPERX                                                            
         GOTO1 GETDAY,DMCB,SVQEND,WORK                                          
         CLI   DMCB,7              IS END SUNDAY                                
         BNE   SUNERR                                                           
         CLC   WORK(3),SPACES      BETTER NOT BE ERROR                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VPERX    DS   0H                                                                
         GOTO1 DATCON,DMCB,(3,SVPERST),(2,SVPERSTP)                             
         GOTO1 (RF),(R1),(3,SVPEREND),(2,SVPEREDP)                              
         GOTO1 (RF),(R1),(3,SVGENST),(2,SVGENSTP)                               
         GOTO1 (RF),(R1),(3,SVGENEND),(2,SVGENEDP)                              
         XIT1                                                                   
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER50   CLC   SVGENST,SVPEREND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVGENST,SVPERST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVGENEND,SVGENEND   ANY END DATE ENTERED                         
         BNZ   VPER54                                                           
         MVC   SVGENEND,SVPEREND   USE EST END DATE                             
         LA    R3,TRAPER+9                                                      
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,SVPEREND),(5,2(R3))                               
         OI    TRAPERH+6,X'80'                                                  
         B     VPER56                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH FLIGHT/ESTIMATE END *                        
*                                                                               
VPER54   CLI   SVTZPR01,C'Y'       FLEXIBLE DATES OPTION ON?                    
         BNE   VPER55                                                           
         CLC   SVGENEND,SVPEREND   AFTER EST END                                
         BH    ESTDTERR                                                         
         CLC   SVGENEND,SVPERST    BEFORE EST START                             
         BL    ESTDTERR                                                         
         B     VPER56                                                           
*                                                                               
VPER55   CLC   SVGENEND,SVPEREND   LAST TLCST MUST BE EST END                   
         BNE   ESTENDER                                                         
*                                                                               
VPER56   GOTO1 DATCON,DMCB,(3,SVGENST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVGENEND),SVQEND                                    
         B     VPER46                                                           
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
*                                                                               
         DS    0H                                                               
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         CLI   BPRD,X'FF'          TEST POL INST                                
         BE    *+10                                                             
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVGENST,KEY+6       FIRST INST DATE TO RECORD END DATE           
         BNH   CHKF4                                                            
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKF6    BRAS  RE,NEXTEL                                                        
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         OC    SVGENEND,SVGENEND   TEST END DATE GIVEN                          
         BZ    CHKF10                                                           
*                                                                               
         CLC   SVGENST,FLTEND       INST START AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   SVGENEND,FLTSTART    INSTR END BEFORE FLIGHT START               
         BL    CHKF6                                                            
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVGENEND,FLTEND      INSTR END DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   SVGENST,FLTSTART                                                 
         BL    FLTOVLER                                                         
         MVC   SVPERDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVGENDTS,FLTSTART   TEST FLIGHT = INST DATES                     
         BE    CHKF20                                                           
*                                                                               
         CLI   SVTZPR01,C'Y'       FLEXIBLE DATES OPTION ON?                    
         BE    CHKF20                                                           
         CLC   FLTEND,SVGENEND     END MUST MATCH FLT END                       
         BNE   FLTDTER                                                          
*                                                                               
CHKF8    MVC   SVGENDTS,SVPERDTS   FORCE FLIGHT DATES = TELECAST                
         B     CHKF20                                                           
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF10   CLC   SVGENST,FLTSTART                                                 
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVPEREND,SVGENEND                FORCE END DATE                  
         GOTO1 DATCON,DMCB,(3,SVPEREND),SVQEND  AND REQ END DATE                
CHKF20   B     VPERX                                                            
*                                                                               
FLTDTER  MVI   ERROR,NOTFLTDT      PER END NO MATCH TO FLIGHT END DATE          
         B     VPERERR                                                          
FLTRECER MVI   ERROR,NOFLTREC                                                   
         B     VPERERR                                                          
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     VPERERR                                                          
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     VPERERR                                                          
DATERRB  MVI   ERROR,INVDATE                                                    
VPERERR  BRAS  RE,RELSTR                                                        
         GOTO1 ERREX                                                            
SUNERR   MVC   GERROR,=Y(SUNEDDT)                                               
         B     *+10                                                             
MONERR   MVC   GERROR,=Y(MONSTDT)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         MVI   ELEM,4                                                           
         MVC   ELEM+1(3),WORK                                                   
         B     VPERERR2                                                         
ESTDTERR MVC   GERROR,=Y(DTNINEST)                                              
         B     VPERERR2                                                         
ESTENDER MVC   GERROR,=Y(BDESTEND)                                              
VPERERR2 BRAS  RE,RELSTR                                                        
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* UPDATE INSTRUCTION RECAP RECORDS *                                            
*                                                                               
UPDI     NMOD1 0,**UPDI**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         L     R3,ASPTABL          GET 1ST SPOT THIS STATION                    
         USING SPTABLED,R3                                                      
*                                                                               
         MVC   AIO,AIO1            USE IO1                                      
*                                                                               
UPDI00   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(1),SPTPRD                                                  
         MVC   KEY+6(5),SVMKTSTA                                                
         MVC   KEY+11(1),QBEST                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    UPDI10                                                           
*                                                                               
* CREATE NEW INSTRUCTION RECAP RECORD *                                         
*                                                                               
         L     R6,AIO                                                           
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   13(2,R6),=H'24'                                                  
         MVC   20(2,R6),AGENCY                                                  
         XC    ELEM,ELEM                                                        
         B     UPDI20                                                           
*                                                                               
* INST RECAP REC FOUND - REMOVE ALL ELEMS FOR THIS PERIOD *                     
*                                                                               
UPDI10   DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
UPDI14   BRAS  RE,NEXTEL                                                        
         BNE   UPDI20                                                           
         USING INSDTAEL,R6                                                      
UPDI16   CLC   INSPRD1,SPTPRD      MUST BE SAME PRD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   INSSLN1,SPTSLN      SAME SPOT LEN                                
         BNE   UPDI14                                                           
         CLC   INSPRD2,SPTPRD2     SAME PTR PRD                                 
         BNE   UPDI14                                                           
         CLC   INSSLN2,SPTSLN2     SAME SPOT LEN                                
         BNE   UPDI14                                                           
         CLC   INSFTD,SVGENEDP     TEST START AFTER THIS                        
         BH    UPDI14                                                           
         CLC   INSLTD,SVGENSTP     TEST LAST SUBEL END BEFORE INST ST           
         BL    UPDI14                                                           
         XC    ELEM,ELEM           IF 2 ELEMENTS, SAVE LAST ONLY                
         ZIC   R5,1(R6)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8              SAVE ELEMENT                                 
         B     *+10                                                             
         MVC   ELEM(0),0(R6) *EXECUTED*                                         
*                                                                               
         GOTO1 VRECUP,DMCB,AIO,(R6)  DELETE THIS ELEM                           
*                                                                               
         CLI   0(R6),0             TEST EOR                                     
         BE    UPDI20                                                           
         CLC   0(1,R6),ELCODE                                                   
         BE    UPDI16                                                           
         B     UPDI14                                                           
*                                                                               
***** NEED CODE HERE TO REMOVE MORE ELEMENTS IF NEW                             
***** ONES WON'T FIT - BUT DON'T NEED THEM RIGHT NOW                            
*                                                                               
* CREATE OR UPDATE RECAP DATA ELEMENTS *                                        
* FIRST GET INST START DATE-1 AND SAVE IT *                                     
*                                                                               
UPDI20   GOTO1 DATCON,DMCB,(3,SVGENST),WORK                                     
         LA    R0,1                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,FULL)  SAVE INST START - 1                 
*                                                                               
         LA    R6,ELEM                                                          
         USING INSDTAEL,R6                                                      
*                                                                               
         CLI   0(R6),0             TEST PREVIOUS ELEM FOUND                     
         BE    UPDI36              NO                                           
*                                                                               
* NEED TO FIND IF CURRENT INST REPLACE OR AMEND PREVIOUS INST *                 
*                                                                               
         LA    R5,INSPTTN          POINT TO FIRST RECAP SUB-EL                  
*                                                                               
         CLC   3(2,R5),SVPERSTP    SUB-EL END TO FLT/EST START                  
         BH    UPDI32              OVERWRITE ALL PREV                           
*                                                                               
UPDI30   CLC   5(3,R5),SVPERSTP    SUB-EL END TO FLT/EST START                  
         BNL   UPDI32              IF HIGH OR EQUAL, GOT IT                     
         LA    R5,INSSUBEL(R5)     ELSE TRY NEXT                                
         CLI   0(R5),0                                                          
         BNE   UPDI30                                                           
         B     UPDI38                                                           
*                                                                               
UPDI32   CLC   3(2,R5),SVGENSTP    SUB-EL START TO INSTR START                  
         BE    UPDI38              EQUAL - OVERWRITE                            
         MVC   5(2,R5),FULL        ELSE SET NEW END DATE (= START -1)           
         LA    R5,INSSUBEL(,R5)    POINT TO NEXT SUBEL                          
         B     UPDI38                                                           
*                                                                               
* ADD 7 BYTES OF PATTERN DATA ONCE ONLY *                                       
*                                                                               
UPDI36   MVI   0(R6),X'30'             SET CODE (BUT NOT LEN)                   
         MVI   1(R6),INSBSCEL+INSSUBEL ELEM LEN                                 
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   *+8                                                              
         OI    INSFLAG,X'20'                                                    
*                                                                               
UPDI38   MVC   INSPRD1,SPTPRD      PRD1                                         
         MVC   INSSLN1,SPTSLN      SLN1                                         
         MVC   INSPRD2,SPTPRD2     PRD2                                         
         MVC   INSSLN2,SPTSLN2     SLN2                                         
*                                                                               
         MVC   INSPERST(4),SVPERSTP  FLT/EST START/END DATES                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,INSDATE)                                    
*                                                                               
         ZIC   RF,SVINSREV                                                      
         OC    SVINSDT,SVINSDT     TEST NO PREVIOUS INST                        
         BZ    UPDI40                                                           
         TM    SVOPT1,OPTRERUN                                                  
         BO    *+8                                                              
         LA    RF,1(,RF)                                                        
         STC   RF,INSREV                                                        
*                                                                               
* FILL IN 7 BYTES OF PATTERN DATA *                                             
*                                                                               
UPDI40   MVC   INSPTTN,=X'000000' *WOULD BE PATTERN REF/SUB FOR INSTR           
         MVC   INSFTD(4),SVPERSTP  MOVE IN FLT/EST START/END DATES              
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         L     RF,PUTREC                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         L     RF,ADDREC                                                        
*                                                                               
         TM    SVOPT1,OPTTEST                                                   
         BO    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         MVC   WORK(1),SPTPRD                                                   
         MVC   WORK+1(1),SPTSLN                                                 
         MVC   WORK+2(1),SPTPRD2                                                
         MVC   WORK+3(1),SPTSLN2                                                
*                                                                               
UPDI50   LA    R3,SPTNEXT                                                       
         OC    SPTFTD,SPTFTD       AT END                                       
         BZ    UPDI60               YES                                         
*                                                                               
* SEE IF ANOTHER ELEM TO UPDATE *                                               
*                                                                               
         CLC   WORK(1),SPTPRD                                                   
         BNE   UPDI00                                                           
         CLC   WORK+1(1),SPTSLN                                                 
         BNE   UPDI00                                                           
         CLC   WORK+2(1),SPTPRD2                                                
         BNE   UPDI00                                                           
         CLC   WORK+3(1),SPTSLN2                                                
         BNE   UPDI00                                                           
         B     UPDI50                                                           
*                                                                               
UPDI60   L     R3,ASPTABL          GET 1ST SPOT THIS STATION                    
*                                                                               
         BAS   RE,TBAUP            GO UPDATE TBA RECORDS                        
*                                                                               
         SR    R4,R4                                                            
UPDI64   OC    SPTFTD,SPTFTD       END OF TABLE                                 
         BZ    UPDI66                                                           
         LA    R4,1(,R4)                                                        
         LA    R3,SPTNEXT                                                       
         B     UPDI64                                                           
UPDI66   LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,ASPTABL          GET 1ST SPOT THIS STATION                    
*                                                                               
* SORT ON DISK ADDRESS AND SPOT NUMBER (SORTED ON DATE ALREADY) *               
*                                                                               
         GOTO1 =V(XSORT),DMCB,ASPTABL,(R4),L'SPTDATA,5,                C        
               SPTDSKAD-SPTDATA,RR=SPTRRR                                       
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
UPDI70   MVC   KEY+14(4),SPTDSKAD                                               
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,KEY+14,(R6),     C        
               DMWORK                                                           
         NI    UPDSW,X'FF'-UPDRECSW SET RECORD UPDATE SW OFF                    
*                                                                               
UPDI80   XC    WORK,WORK                                                        
         MVC   WORK(1),SPTPRD                                                   
         MVC   WORK+1(1),SPTSLN                                                 
         MVC   WORK+2(1),SPTPRD2                                                
         MVC   WORK+3(1),SPTSLN2                                                
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
         XC    ELDATE,ELDATE                                                    
         EJECT                                                                  
* UPDATE ALL ELEMS FOR THIS BUY *                                               
*                                                                               
UPDI82   BAS   RE,BUYEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    6(R6),X'C0'                                                      
         BNZ   UPDI82                                                           
         CLI   1(R6),10            UNALLOCATED                                  
         BNH   UPDI82                                                           
*                                                                               
         CLC   2(2,R6),ELDATE                                                   
         BE    UPDI84                                                           
         MVI   SPOTNUMB,0                                                       
         MVC   ELDATE,2(R6)                                                     
UPDI84   ZIC   RE,SPOTNUMB                                                      
         LA    RE,1(,RE)                                                        
         STC   RE,SPOTNUMB                                                      
         CLI   SPTPRD2,0           IS THERE A PIGGYBACK PROD                    
         BE    UPDI88               NO                                          
         CLI   1(R6),18                                                         
         BL    UPDI82                                                           
         CLC   WORK(2),10(R6)      SAME BPRD/SLN                                
         BNE   *+18                                                             
         CLC   WORK+2(2),14(R6)    SAME BPRD2/SLN2                              
         BNE   UPDI82                                                           
         B     UPDI90                                                           
*                                                                               
* CK IF PRODS WERE OUT OF ORDER *                                               
*                                                                               
         CLC   WORK+2(2),10(R6)    SAME BPRD2/SLN2                              
         BNE   UPDI82                                                           
         CLC   WORK(2),14(R6)      SAME BPRD/SLN                                
         BNE   UPDI82                                                           
         B     UPDI90                                                           
*                                                                               
UPDI88   CLI   1(R6),14                                                         
         BH    UPDI82                                                           
         CLC   WORK(2),10(R6)      SAME BPRD/SLN                                
         BNE   UPDI82                                                           
*                                                                               
UPDI90   CLC   2(2,R6),SPTFTD      SAME DATE                                    
         BNE   UPDI82               NO                                          
         CLC   SPTSPTN,SPOTNUMB    SAME SPOT NUMBER                             
         BNE   UPDI82               NO                                          
         LR    R4,R6                                                            
UPDI92   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),13            THIS ANOTHER SPOT                            
         BL    UPDI94               YES, NO TRAFFIC ELEM                        
*                                                                               
         CLI   0(R4),0             THIS END OF RECORD                           
         BE    UPDI94               YES, NO TRAFFIC ELEM                        
*                                                                               
         CLI   0(R4),X'18'         THIS A CML ASSIGN ELEMENT                    
         BNE   UPDI92               NO, BYPASS                                  
*                                                                               
         USING TRACID,R4                                                        
         TM    TRACREF,X'80'       WAS PRINTED FLAG SET                         
         BO    UPDI94                                                           
*                                                                               
         OI    TRACREF,X'80'       SET PRINTED FLAG                             
         OI    UPDSW,UPDRECSW      ***INDICATE RECORD NEEDS UPDATE***           
         DROP  R4                                                               
*                                                                               
UPDI94   LA    R3,SPTNEXT                                                       
         CLC   SPTDSKAD,KEY+14     SAME REC                                     
         BE    UPDI80              YES                                          
*                                                                               
         TM    UPDSW,UPDRECSW      TEST RECORD NEEDS UPDATE                     
         BZ    UPDI96                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   *+12                 ALWAYS WRITE                                
         CLI   TWAWRITE,C'Y'       WRITE ALLOWED                                
         BNE   UPDI96                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',SYSFIL,KEY+14,AIO1,DMWORK                
*                                                                               
UPDI96   CLI   0(R3),0             AT END OF SPOT TABLE                         
         BNE   UPDI70               NO                                          
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
UPDIX    XIT1                                                                   
*                                                                               
BUYEL2   CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL2                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL2                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
** SUBROUTINE TO UPDATE TBA RECORDS *                                           
*                                                                               
         DS    0H                                                               
TBAUP    NTR1                                                                   
         L     R3,ASPTABL          START OF SPOT TABLE-THIS MKT/STA             
         USING SPTABLED,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(5),SVMKTSTA   MKT/STA                                      
         MVC   KEY+10(1),SPTPRD    PRD                                          
         MVC   KEY+11(1),QBEST                                                  
         MVC   KEY+12(1),SPTPRD2     PTNR                                       
*                                                                               
         GOTO1 HIGH                                                             
         B     TBAUP04                                                          
TBAUP02  L     R3,ASPTABL          START OF SPOT TABLE-THIS MKT/STA             
         GOTO1 SEQ                                                              
TBAUP04  CLC   KEY(10),KEYSAVE                                                  
         BNE   TBAUP22                                                          
         CLI   QBEST,01            THIS BY EST                                  
         BE    TBAUP06              NO                                          
         CLC   KEY+11(1),QBEST                                                  
         BNE   TBAUP22                                                          
TBAUP06  CLC   BPRD2,KEY+12                                                     
         BNE   TBAUP02                                                          
         GOTO1 GETREC                                                           
*                                                                               
         NI    UPDSW,X'FF'-UPDRECSW  SET RECORD UPDATE SW OFF                   
*                                                                               
         USING TBADTAEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'05'                                                     
TBAUP08  L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
TBAUP10  BRAS  RE,NEXTEL                                                        
         BNE   TBAUP20                                                          
*                                                                               
         CLC   TBADTASL,SPTSLN    SAME SLN                                      
         BNE   TBAUP10                                                          
         CLC   TBADTAS2,SPTSLN2   SAME SLN2                                     
         BNE   TBAUP10                                                          
         GOTO1 DATCON,DMCB,(2,TBADTAWK),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'6'                                      
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WORK)                                  
         CLC   TBADTAWK,SPTFTD     PRIOR TO WEEK START                          
         BH    TBAUP10                                                          
         CLC   WORK(2),SPTLTD      AFTER WEEK END                               
         BL    TBAUP10                                                          
         TM    TBADTAAC,X'80'      IS ACTIVITY FLAG SET                         
         BZ    TBAUP16                                                          
         NI    TBADTAAC,X'7F'      *** RESET ACTIVITY FLAG ***                  
         OI    UPDSW,UPDRECSW      ***INDICATE RECORD NEEDS UPDATE***           
*                                                                               
TBAUP16  LA    R3,SPTNEXT                                                       
         OC    SPTFTD,SPTFTD       TEST MORE DATA                               
         BNZ   TBAUP08                                                          
*                                                                               
TBAUP20  TM    UPDSW,UPDRECSW      TEST RECORD NEEDS UPDATE                     
         BZ    TBAUP02                                                          
         TM    SVOPT1,OPTTEST                                                   
         BO    TBAUP02                                                          
         GOTO1 PUTREC                                                           
         B     TBAUP02             UPDATE ALL ESTIMATES                         
*                                                                               
TBAUP22  B     UPDIX                                                            
         DROP  R3,RB,RC                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* PRINT AGY COPY TO PRTFILE OFFLINE, TSAR ONLINE. FAX IS PRODUCED 1ST *         
*                                                                               
GOSPL    NMOD1 0,**GOSPL*                                                       
*                                                                               
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   SVTWPR09,C'2'       DO COPY ALSO                                 
         BNE   GOSPL10              NO                                          
*                                                                               
         LA    RE,P                                                             
         LA    RF,528                                                           
         LR    R1,RF                                                            
         L     R0,AIO1                                                          
         AH    R0,=H'1472'                                                      
         LR    R3,R0                                                            
         MVCL  R0,RE                                                            
         MVC   SVSPACE,SPACING                                                  
*                                                                               
GOSPL10  GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   SVTWPR09,C'2'       DO COPY ALSO                                 
         BNE   GOSPLX               NO                                          
*                                                                               
         CLI   OFFLINE,C'Y'        PUT TO PRTFILE                               
         BNE   GOSPL20              NO, CK TSAR                                 
*                                                                               
         L     R4,=A(PRTFILO)                                                   
         MVC   131(1,R3),SVSPACE   SAVE SPACING                                 
*                                                                               
         LA    R2,5                MAX LINES TO MOVE                            
*                                                                               
GOSPL14  PUT   (R4),(R3)                                                        
         LA    R3,132(,R3)                                                      
         CLC   0(132,R3),SPACES                                                 
         BE    GOSPLX                                                           
         MVI   131(R3),0           SPACING                                      
         BCT   R2,GOSPL14                                                       
         B     GOSPLX                                                           
*                                                                               
*        R0 = SIZE OF PRINT LINE WITHOUT LEADING BLANKS                         
*        R1 = 1S NON=BLANK CHAR IN PRINT LINE                                   
*        R2 = TSAR BASE                                                         
*        R3 = CURRENT PRINT LINE                                                
*        R4 = CT OF PRINT LINES                                                 
*        R5 = CT OF SKIPPED BLANK LINES                                         
*        R6 = LAST LINE MARKER                                                  
*        RE = CT OF NON-BLANK CHARACTERS                                        
*        RF = FINDS LAST NON BLANK CHAR ON LINE                                 
         EJECT                                                                  
* TSAR WRITE TO HIGH STORAGE HERE                                               
*                                                                               
GOSPL20  DS    0H                                                               
         LH    R2,=AL2(TSARBLK-SYSD)                                            
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         LA    R4,4                                                             
         SR    R5,R5                                                            
*                                                                               
* FIND ADDRESS OF LAST LINE TO PRINT                                            
*                                                                               
         LR    R1,R4                                                            
         LA    R6,396(,R3)                                                      
GOSPL24  OC    0(132,R6),0(R6)                                                  
         BZ    GOSPL26                                                          
         CLC   0(132,R6),SPACES                                                 
         BNE   GOSPL30                                                          
GOSPL26  SH    R6,=H'132'                                                       
         BCT   R1,GOSPL24                                                       
*                                                                               
GOSPL30  XC    ELEM,ELEM                                                        
*                                                                               
         LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
         LR    R1,R3               POINT TO START OF PRINT LINE                 
         LA    RF,131(,R3)         POINT TO END OF PRINT LINE                   
*                                                                               
         CLI   0(R1),0             FORCE LINE                                   
         BE    GOSPL35              YES                                         
*                                                                               
GOSPL32  CLI   0(R1),C' '          FIRST NON-BLANK                              
         BH    GOSPL34              GET OUT                                     
         LA    R1,1(,R1)                                                        
         BCTR  RE,0                                                             
         BCT   R0,GOSPL32                                                       
*                                                                               
         B     GOSPL38             NOTHING TO PRINT                             
*                                                                               
GOSPL34  CH    R0,=H'128'                                                       
         BL    GOSPL36                                                          
*                                                                               
GOSPL35  LR    R1,R3               POINT TO START OF PRINT LINE                 
         LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
*                                                                               
* NOW LOOK FOR TRAILING BLANKS                                                  
*                                                                               
GOSPL36  CLI   0(RF),C' '                                                       
         BH    GOSPL40                                                          
         CLI   0(RF),0             FORCE LINE                                   
         BE    GOSPL40              YES                                         
         BCTR  RF,0                                                             
         BCT   RE,GOSPL36                                                       
*                                                                               
GOSPL38  LA    R5,1(,R5)           ADD TO SKIPPED LINE CT                       
         B     GOSPL50             BYPASS EMPTY LINE                            
*                                                                               
GOSPL40  LTR   RE,RE                                                            
         BZ    *+6                                                              
         BCTR  RE,0                                                             
*                                                                               
         CR    R1,R3               ARE THERE LEADING BLANKS TO DROP             
         BNE   GOSPL41              YES                                         
         EX    RE,GOSPLMVC                                                      
         B     GOSPL42                                                          
*                                                                               
GOSPL41  MVI   ELEM+6,1            MARK LEADING BLANK COMPRESSION               
         LR    RF,R1                                                            
         SR    RF,R3               GET DISPLACEMENT                             
         STC   RF,ELEM+7                                                        
         EX    RE,GOSPLMVA                                                      
         LA    RE,2(,RE)                                                        
*                                                                               
GOSPL42  STC   R5,ELEM+4           SAVE ANY SKIPPED LINE CT                     
         SR    R5,R5               SET TO ZERO                                  
*                                                                               
GOSPL44  LA    RE,7(,RE)                                                        
         STCM  RE,3,ELEM                                                        
         LH    R1,PRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,PRTCT                                                         
         STCM  R1,3,ELEM+2                                                      
         LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
*                                                                               
         CR    R3,R6               THIS LAST LINE TO PRINT                      
         BL    GOSPL46              NO                                          
         MVC   ELEM+5(1),SVSPACE   SAVE SPACING                                 
*                                                                               
GOSPL46  MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    GOSPL50                                                          
         DC    H'0'                                                             
*                                                                               
GOSPL50  SR    R1,R1                                                            
         ICM   R1,3,ELEM                                                        
         A     R1,TSARBYTE                                                      
         ST    R1,TSARBYTE                                                      
*                                                                               
         LA    R3,132(,R3)                                                      
         BCT   R4,GOSPL30                                                       
         DROP  R2                                                               
*                                                                               
GOSPLX   XIT1                                                                   
*                                                                               
GOSPLMVC MVC   ELEM+6(0),0(R3)                                                  
GOSPLMVA MVC   ELEM+8(0),0(R1)                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* PRINT HARDCOPY USING TSAR *                                                   
*                                                                               
PRTC     NMOD1 0,**PRTC**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    FORCE CLOSE OF SPOOL                          
         MVI   PQSW,1                                                           
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
*NOP     MVI   QLRETNL+1,36                                                     
*****    MVI   QLRETND+1,12                                                     
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
*                                                                               
         TM    WHEN,X'40'                                                       
         BZ    *+10                                                             
         MVC   QLTYP1,SVQLTYP1                                                  
*                                                                               
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
*                                                                               
* RESTORE ORIGINAL REMOTE PRINT SETTINGS *                                      
*                                                                               
         MVC   REMUSER,SVREMUSR                                                 
*                                                                               
         GOTO1 OPENPQ                                                           
         SR    R0,R0                                                            
         ST    R0,SPECS            NO HEADING SPECS                             
         ST    R0,HEADHOOK            HDHK                                      
         ST    R0,FOOTHOOK            FTHK                                      
*                                                                               
         LH    R2,=AL2(TSARBLK-SYSD)  GET ADDR OF TSAR TABLE                    
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
*                                                                               
         LA    R0,1                                                             
         STCM  R0,3,TSRNUM                                                      
*                                                                               
PRTC10   MVI   TSACTN,TSAGET                                                    
         GOTO1 ATSAR,TSARD                                                      
*=====>                                                                         
*        XC    BLOCK(128),BLOCK                                                 
*        MVI   BLOCK,128                                                        
*        LA    R1,BLOCK+1                                                       
*        MVC   0(64,R1),0(R2)                                                   
*        MVC   64(64,R1),ELEM                                                   
*        GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',BLOCK                          
*=====>                                                                         
         TM    TSERRS,X'90'        END OF FILE                                  
         BNZ   PRTCX                                                            
         CLI   TSERRS,0                                                         
         BE    PRTC20                                                           
         DC    H'0'                                                             
PRTC20   CLI   ELEM+4,C'Y'         IS THERE ANY FORCEHED                        
         BE    PRTC30               YES                                         
         CLI   ELEM+4,0            IS THERE ANY SPACING BEFORE                  
         BE    PRTC30               NO                                          
         MVC   SPACING,ELEM+4                                                   
         MVI   ELEM+4,0            SET OFF FOR LATER                            
         MVI   P,0                                                              
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTC30   SR    R1,R1                                                            
         ICM   R1,3,ELEM                                                        
         SH    R1,=H'7'                                                         
         LTR   R1,R1               ONLY 1 CHAR TO PRINT                         
         BZ    PRTC40               YES                                         
*                                                                               
         CLI   ELEM+6,1            WERE LEADING BLANKS DROPPED                  
         BNE   PRTC40                                                           
         MVC   P,SPACES                                                         
         BCTR  R1,0                SUBTRACT 2 FOR EXTRA INDICATOR/DISPL         
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         IC    RE,ELEM+7                                                        
         LA    RF,P(RE)                                                         
         EX    R1,PRTCMVCA                                                      
         B     PRTC44                                                           
PRTCMVCA MVC   0(0,RF),ELEM+8                                                   
*                                                                               
PRTC40   EX    R1,PRTCMVC                                                       
*                                                                               
PRTC44   CLI   ELEM+4,0            IS THERE ANY FORCEHED                        
         BE    *+10                                                             
         MVC   FORCEHED,ELEM+4                                                  
*                                                                               
         CLI   ELEM+5,0            IS THERE ANY SPACING                         
         BE    *+10                                                             
         MVC   SPACING,ELEM+5                                                   
*                                                                               
         CLC   P,SPACES                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    P,P                                                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TSRNUM                                                      
         LA    RF,1(,RF)                                                        
         STCM  RF,3,TSRNUM                                                      
         B     PRTC10                                                           
*                                                                               
PRTCX    XIT1                                                                   
PRTCMVC  MVC   P(0),ELEM+6                                                      
         DROP  R2,RB,RC                                                         
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   HDHK02                                                           
         LH    RE,SEQNUM                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H1+104(4),DUB                                                    
         LA    RE,1(RE)                                                         
         STH   RE,SEQNUM                                                        
*                                                                               
HDHK02   MVC   H1+12(L'QUESTOR),QUESTOR                                         
*                                                                               
         MVC   H4+2(12),CONTEL                                                  
         OC    CONTEL+13(5),CONTEL+13 TEL EXTENSION                             
         BZ    HDHK04                                                           
         MVC   H4+15(3),=C'EXT'                                                 
         MVC   H4+19(5),CONTEL+13                                               
*                                                                               
HDHK04   OC    CONFAX,CONFAX   TEL+IS THERE A FAX                               
         BZ    HDHK06                                                           
         MVC   H5+2(3),=C'FAX'                                                  
         MVC   H5+6(12),CONFAX                                                  
         OC    CONFAX+13(5),CONFAX+13 TEL EXTENSION                             
         BZ    HDHK06                                                           
         MVC   H5+19(3),=C'EXT'                                                 
         MVC   H5+23(5),CONFAX+13                                               
*                                                                               
HDHK06   MVC   H1+38(40),SPACES                                                 
         MVC   H1+39(10),=C'SPOT RADIO'                                         
         CLI   QMED,C'R'                                                        
         BE    HDHK08                                                           
         MVC   H1+39(13),=C'NETWORK RADIO'                                      
         CLI   QMED,C'X'                                                        
         BE    HDHK08                                                           
         MVC   H1+38(15),=C'SPOT TELEVISION'                                    
         CLI   QMED,C'T'                                                        
         BE    HDHK08                                                           
         MVC   H1+36(18),=C'NETWORK TELEVISION'                                 
         CLI   QMED,C'N'                                                        
         BE    HDHK08                                                           
         DC    H'0'                                                             
HDHK08   MVC   H1+55(23),=C'COMMERCIAL INSTRUCTIONS'                            
         GOTO1 SQUASHER,DMCB,H1+38,40                                           
         GOTO1 CENTER,(R1),,40                                                  
         GOTO1 UNDERLIN,(R1),(40,H1+38),H2+38                                   
*                                                                               
         OC    SVINSDT,SVINSDT     TEST PREVIOUS INST                           
         BZ    HDHK10                                                           
         GOTO1 DATCON,DMCB,(2,SVINSDT),(8,H4+67)                                
*                                                                               
HDHK10   MVC   H3+99(5),QSTA       CREATE PURCHASE ORDER NUM                    
         GOTO1 DATCON,DMCB,(5,0),WORK                                           
         MVC   H3+104(4),WORK+2    MOVE MMDD                                    
*                                                                               
         CLI   SVPROF4,C'N'        TEST SUPPRESS PURCHASE ORDER                 
         BNE   *+10                                                             
         MVC   H3+84(26),SPACES                                                 
*                                                                               
         ZIC   RE,SVINSREV                                                      
         TM    SVOPT1,OPTRERUN                                                  
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H4+94(3),DUB                                                     
*                                                                               
         OC    SVINSDT,SVINSDT     TEST NO PREVIOUS INST                        
         BZ    HDHK12              THESE ARE ORIGINAL                           
         CLI   SVINSREV,0          TEST PREVIOUS INST WERE ORIGINAL             
         BNE   HDHK14              NO                                           
         TM    SVOPT1,OPTRERUN     TEST THIS IS A RERUN                         
         BZ    HDHK14              YES - TREAT THESE AS ORIGINAL                
*                                                                               
HDHK12   MVC   H4+38(40),SPACES    OVERWRITE SUPERSEDES LINE                    
         MVC   H4+84(21),=C'ORIGINAL INSTRUCTIONS'                              
*                                                                               
HDHK14   TM    SVOPT1,OPTTEST      TEST OPTION TEST                             
         BZ    *+10                                                             
         MVC   H5+2(16),=C'*** TEST RUN ***'                                    
*                                                                               
         TM    SVOPT1,OPTNOAGY     TEST BLANK AGENCY NAME/ADDRESS               
         BZ    *+16                                                             
         MVC   H2+2(33),SPACES     AGENCY NAME                                  
         MVC   H3+2(33),SPACES     ADDRESS                                      
*                                                                               
         CLI   SVPROF5,C'N'        TEST SUPPRESS REQUESTOR/REPORT               
         BNE   *+16                                                             
         MVC   H1+84(14),SPACES    REPORT (LEAVE SEQUENCE NUMBER)               
         MVC   H5+92(18),SPACES    REQUESTOR                                    
*                                                                               
         CLC   PAGE,=H'1'                                                       
         BE    *+10                                                             
         MVC   H6+85(21),=C'***** CONTINUED *****'                              
*                                                                               
         MVC   H7+67(6),=C'CLIENT'                                              
         MVC   H7+76(3),QCLT                                                    
         MVC   H7+81(20),CLTNM                                                  
*                                                                               
         MVC   H8+67(7),=C'PRODUCT'                                             
*                                                                               
         MVC   H8+76(3),QPRD                                                    
         MVC   H8+81(20),PRDNM                                                  
*                                                                               
         MVC   H9+67(6),=C'MARKET'                                              
         MVC   H9+76(4),QMKT                                                    
         MVC   H9+81(24),MKTNM                                                  
*                                                                               
         CLI   QBEST,0             RUNNING BY EST                               
         BE    HDHK28               NO                                          
         MVC   H10+67(8),=C'ESTIMATE'                                           
         ZIC   R0,QBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H10+76(3),DUB                                                    
         CLI   SVT1PR14,C'N'       DON'T PRINT EST DESC                         
         BE    *+10                                                             
         MVC   H10+81(20),ESTDESC                                               
*                                                                               
HDHK28   TM    ANYFLAG,PRD2FSW     PROD 2 FOUND SW                              
         BZ    *+10                                                             
         MVC   H11+67(26),=C'*** PIGGYBACKS PRESENT ***'                        
*                                                                               
* FORMAT STANDARD TEXT TO PRINT LINES *                                         
*                                                                               
         CLC   PAGE,=H'1'                                                       
         BNE   HDHK40              ONLY PRINT ON PAGE 1                         
         L     R4,NEXTADDR         FORMATTED COMMENTS ARE HERE                  
*                                                                               
HDHK30   LA    R0,9                                                             
         LA    R1,H6                                                            
*                                                                               
HDHK34   CLI   0(R4),0             TEST REACHED END OF COMMENTS                 
         BE    HDHK36                                                           
         MVC   3(60,R1),0(R4)                                                   
         LA    R1,132(R1)                                                       
         LA    R4,60(R4)                                                        
         BCT   R0,HDHK34                                                        
*                                                                               
HDHK36   CLI   0(R4),0             TEST DONE                                    
         BNE   HDHK30                                                           
*                                                                               
         BAS   RE,CKFIL                                                         
*                                                                               
         B     HDHKX               DO NOT PRINT MIDLINES FOR PAGE 1             
*                                                                               
* BLANK HEADLINES AFTER PAGE 1 *                                                
*                                                                               
HDHK40   LA    R0,9                                                             
         LA    R1,H6                                                            
*                                                                               
HDHK42   MVC   3(60,R1),SPACES                                                  
         LA    R1,132(R1)                                                       
         BCT   R0,HDHK42                                                        
*                                                                               
*NOP     MVI   MAXLINES,60         RESET PAGE SIZE                              
         MVI   MAXLINES,42         WAS 44                                       
*                                                                               
         BAS   RE,CKFIL                                                         
*                                                                               
* PRINT TITLES *                                                                
*                                                                               
         TM    UPDSW,X'20'         PRINTING SPOTS                               
         BZ    HDHKX               NO, NO MIDLINES                              
         MVC   MID1(L'MIDLINE),MIDLINE                                          
         MVC   MID2(L'MIDLINE),MIDLINE+L'MIDLINE                                
*                                                                               
         CLI   SVTWPR09,C'2'       SEE IF FAXING                                
         BNE   HDHKX                                                            
         CLI   OFFLINE,C'Y'        ONLY OFFLINE                                 
         BNE   HDHK44                                                           
*                                                                               
         L     R4,=A(PRTFILO)                                                   
         MVI   MID1+131,1          FOR SPACING                                  
         MVI   MID2+131,1          FOR SPACING                                  
         PUT   (R4),MID1                                                        
         PUT   (R4),MID2                                                        
         B     HDHKX                                                            
*                                                                               
* TSAR SAVE MIDLINES                                                            
*                                                                               
HDHK44   BAS   RE,CKMID                                                         
*                                                                               
HDHKX    XIT1                                                                   
         EJECT                                                                  
* IF FAX WITH COPY REQUEST, WRITE TO TSAR ONLINE, PRTFILO OFFLINE *             
*                                                                               
CKFIL    NTR1                                                                   
         CLI   SVTWPR09,C'2'       IS AGENCY COPY REQUIRED                      
         BNE   HDHKX                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   CKFIL50              NO, USE TSAR                                
*                                                                               
         L     R4,=A(PRTFILO)                                                   
         MVC   ELEM(5),=C'/PAGE'                                                
         XC    ELEM+5(127),ELEM+5                                               
         MVC   ELEM+112(L'QSTA),QSTA                                            
*                                                                               
CKFIL16  LA    R3,ELEM                                                          
         PUT   (R4),(R3)                                                        
*                                                                               
         ICM   R5,15,H1+104        GET SEQNUM                                   
         MVC   H1+104(4),SPACES    BLK SEQ                                      
         MVC   H5+50(19),SPACES    BLK AGENCY COPY                              
         LA    R6,14                                                            
         LA    R1,H14                                                           
CKFIL20  CLC   SPACES,0(R1)                                                     
         BNE   CKFIL24                                                          
         SH    R1,=H'132'                                                       
         BCT   R6,CKFIL20                                                       
         DC    H'0'                                                             
*                                                                               
CKFIL24  LA    R3,H1                                                            
*                                                                               
CKFIL30  MVI   131(R3),1           FOR SPACING                                  
         PUT   (R4),(R3)                                                        
         LA    R3,132(,R3)                                                      
         BCT   R6,CKFIL30                                                       
*                                                                               
         MVI   H1+131,C' '                                                      
         STCM  R5,15,H1+104        RESTORE SEQNUM                               
         MVC   H5+50(19),=C'*** AGENCY COPY ***'                                
         B     HDHKX                                                            
*                                                                               
* TSAR WRITE TO HIGH STORAGE HERE                                               
*                                                                               
CKFIL50  LA    R3,H1                                                            
         LA    R4,14                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM+4,C'Y'         SET FORCEHED                                 
         MVC   H5+50(19),=C'*** AGENCY COPY ***'                                
         B     CKFIL54                                                          
*                                                                               
CKMID    NTR1                                                                   
*                                                                               
         LA    R3,MID1                                                          
         LA    R4,2                                                             
         XC    ELEM,ELEM                                                        
*                                                                               
*                                                                               
*        R0 = SIZE OF PRINT LINE WITHOUT LEADING BLANKS                         
*        R1 = 1S NON=BLANK CHAR IN PRINT LINE                                   
*        R2 = TSAR BASE                                                         
*        R3 = CURRENT PRINT LINE                                                
*        R4 = CT OF PRINT LINES                                                 
*        R5 = CT OF SKIPPED BLANK LINES                                         
*        R6 = LAST LINE MARKER                                                  
*        RE = CT OF NON-BLANK CHARACTERS                                        
*        RF = FINDS LAST NON BLANK CHAR ON LINE                                 
*                                                                               
CKFIL54  LH    R2,=AL2(TSARBLK-SYSD)                                            
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         SR    R5,R5                                                            
         B     CKFIL62                                                          
*                                                                               
CKFIL60  XC    ELEM,ELEM                                                        
*                                                                               
CKFIL62  LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
         LR    R1,R3               POINT TO START OF PRINT LINE                 
         LA    RF,131(,R3)         POINT TO END OF PRINT LINE                   
*                                                                               
CKFIL64  CLI   0(R1),C' '                                                       
         BH    CKFIL66                                                          
         LA    R1,1(,R1)                                                        
         BCTR  RE,0                                                             
         BCT   R0,CKFIL64                                                       
*                                                                               
         B     CKFIL72                                                          
*                                                                               
CKFIL66  CH    R0,=H'128'                                                       
         BL    CKFIL70                                                          
*                                                                               
CKFIL68  LR    R1,R3               POINT TO START OF PRINT LINE                 
         LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
CKFIL70  CLI   0(RF),C' '                                                       
         BH    CKFIL74                                                          
         BCTR  RF,0                                                             
         BCT   RE,CKFIL70                                                       
*                                                                               
CKFIL72  LA    R5,1(,R5)           ADD TO SKIPPED LINE COUNT                    
         B     CKFIL94                                                          
*                                                                               
CKFIL74  LTR   RE,RE                                                            
         BZ    *+6                                                              
         BCTR  RE,0                                                             
*                                                                               
         CR    R1,R3               DROPPING LEADING SPACES AS WELL              
         BNE   CKFIL80              YES                                         
*                                                                               
         EX    RE,CKFILMVC                                                      
         B     CKFIL84                                                          
*                                                                               
CKFIL80  MVI   ELEM+6,1                                                         
         LR    RF,R1                                                            
         SR    RF,R3               GET DISPLACEMENT                             
         STC   RF,ELEM+7                                                        
         EX    RE,CKFILMVA                                                      
         LA    RE,2(,RE)                                                        
*                                                                               
CKFIL84  LA    RE,7(,RE)                                                        
         STCM  RE,3,ELEM                                                        
*                                                                               
         LH    R1,PRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,PRTCT                                                         
         STCM  R1,3,ELEM+2                                                      
*                                                                               
         CLI   ELEM+4,C'Y'         WAS THIS A FORCE HEAD                        
         BE    CKFIL90                                                          
*                                                                               
         LTR   R5,R5                                                            
         BZ    CKFIL90                                                          
         LA    R5,1(,R5)           ADD 1 TO SPACING                             
         STC   R5,ELEM+4                                                        
         SR    R5,R5               RESET                                        
*                                                                               
CKFIL90  LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
         MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    CKFIL94                                                          
         DC    H'0'                                                             
*                                                                               
CKFIL94  SR    R1,R1                                                            
         ICM   R1,3,ELEM                                                        
         A     R1,TSARBYTE                                                      
         ST    R1,TSARBYTE                                                      
*                                                                               
         LA    R3,132(,R3)                                                      
         BCT   R4,CKFIL60                                                       
         MVC   H5+50(19),SPACES     BLK AGENCY COPY                             
*                                                                               
* FORCE SPACE AFTER LAST LINE OF HEADING *                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+1,7                                                         
         LH    R1,PRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,PRTCT                                                         
         STCM  R1,3,ELEM+2                                                      
*                                                                               
         MVI   ELEM+5,1            FORCE 1 LINE OF SPACING                      
         MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,ELEM                                                        
         A     R1,TSARBYTE                                                      
         ST    R1,TSARBYTE                                                      
         B     HDHKX                                                            
*                                                                               
CKFILMVC MVC   ELEM+6(0),0(R3)                                                  
CKFILMVA MVC   ELEM+8(0),0(R1)                                                  
         DROP  R2                                                               
MIDLINE  DC    CL110'DAY      TIME        PROGRAM          D LEN  DATE C        
                       PRD PRODUCT NAME       COMML        COMMERCIAL NC        
               AME'                                                             
         DC    CL110'---      ----        -------          P ---  ---- C        
                       --- ------------       --------     ------------C        
               --'                                                              
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* FOOT HOOK ROUTINE                                                             
*                                                                               
FTHK     NMOD1 0,**FTHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   FOOTSW,5            TEST DONE 5 LINES YET                        
         BE    FTHK24              YES - DONE                                   
         ZIC   RE,FOOTSW                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FOOTSW                                                        
*                                                                               
         MVC   P,SPACES            FTHK DOES NOT CLEAR P                        
         MVI   P+1,0               FORCE RETURN                                 
*                                                                               
         CLI   FOOTSW,1            TEST FIRST LINE                              
         BNE   FTHK10                                                           
*                                                                               
         OC    STANET,STANET       THIS CABLE HEAD STATION                      
         BZ    *+10                                                             
         MVC   P+50(L'STANET),STANET                                            
*                                                                               
         CLI   CONTINUE,C'N'       TEST FINISHED                                
         BE    FTHK10              YES                                          
         MVC   P+2(40),=C'** INSTRUCTIONS CONTINUE ON NEXT PAGE **'             
*                                                                               
FTHK10   CLI   FOOTSW,3            REACHED LINE 3 YET                           
         BL    FTHK16              NO - SKIP FOOTNOTE                           
*                                                                               
* CHECK FOR FOOTNOTE LINE *                                                     
*                                                                               
         CLI   CONTINUE,C'N'       TEST LAST PAGE                               
         BNE   FTHK16              NO, FOOTNOTE ONLY LAST PAGE                  
         L     R2,ASVFTNT                                                       
         ZIC   R0,FOOTSW                                                        
         SH    R0,=H'2'                                                         
         B     FTHK14                                                           
FTHK12   CLI   0(R2),0             TEST MORE DATA                               
         BE    FTHK16                                                           
         LA    R2,60(R2)                                                        
FTHK14   BCT   R0,FTHK12                                                        
         CLI   0(R2),0                                                          
         BE    FTHK16                                                           
         MVC   P+2(60),0(R2)                                                    
*                                                                               
FTHK16   CLI   CONTINUE,C'N'       TEST LAST PAGE                               
         BE    FTHK18              YES - PRINT STATION ADDRESS                  
*                                                                               
         CLI   FOOTSW,2            ONLY PRINT 2 STATION ADDRESS LINES           
         BH    FTHK26              IF NOT LAST PAGE                             
         EJECT                                                                  
* FORMAT STATION ADDRESS DATA *                                                 
*                                                                               
FTHK18   L     R2,ASVSTAD                                                       
         ZIC   R0,FOOTSW                                                        
         BCTR  R0,0                                                             
         MH    R0,=H'24'                                                        
         AR    R2,R0                                                            
         MVC   P+75(24),0(R2)                                                   
         B     FTHK50                                                           
*                                                                               
FTHK24   MVI   FOOTSW,0                                                         
         MVC   P,SPACES                                                         
         B     FTHKX                                                            
*                                                                               
FTHK26   MVI   P+75,C'*'                                                        
         MVC   P+76(23),P+75                                                    
*                                                                               
FTHK50   BAS   RE,CKFOOT                                                        
FTHKX    XIT1                                                                   
         EJECT                                                                  
* PUT FOOTLINE TO TSAR/PRTFILO                                                  
*                                                                               
CKFOOT   NTR1                                                                   
         CLI   OFFLINE,C'Y'        THIS OFFLINE                                 
         BE    CKFT60                                                           
         CLI   SVTWPR09,C'2'       REQUIRE AGENCY COPY                          
         BNE   CKFX                                                             
*                                                                               
         LH    R2,=AL2(TSARBLK-SYSD)                                            
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
         LA    R1,P                POINT TO START OF PRINT LINE                 
         LR    R3,R1                                                            
         LA    RF,P+131                                                         
*                                                                               
CKFT10   CLI   0(R1),C' '                                                       
         BH    CKFT14                                                           
         LA    R1,1(,R1)                                                        
         BCTR  RE,0                                                             
         BCT   R0,CKFT10                                                        
*                                                                               
         B     CKFT24                                                           
*                                                                               
CKFT14   CH    R0,=H'128'                                                       
         BL    CKFT20                                                           
*                                                                               
CKFT16   LR    R1,R3               POINT TO START OF PRINT LINE                 
         LA    R0,132              MAX PRINT POSITIONS                          
         LR    RE,R0                                                            
CKFT20   CLI   0(RF),C' '                                                       
         BH    CKFT26                                                           
         BCTR  RF,0                                                             
         BCT   RE,CKFT20                                                        
*                                                                               
CKFT24   MVI   ELEM+6,0                                                         
         MVI   ELEM+1,7                                                         
         B     CKFT50                                                           
*                                                                               
CKFT26   LTR   RE,RE                                                            
         BZ    *+6                                                              
         BCTR  RE,0                                                             
*                                                                               
         CH    R0,=H'128'          DROPPING LEADING SPACES AS WELL              
         BL    CKFT30                                                           
*                                                                               
         EX    RE,CKFTMVC                                                       
         B     CKFT40                                                           
*                                                                               
CKFT30   MVI   ELEM+6,1                                                         
         LR    RF,R1                                                            
         SR    RF,R3                                                            
         STC   RF,ELEM+7                                                        
         EX    RE,CKFTMVCA                                                      
         LA    RE,2(,RE)                                                        
*                                                                               
CKFT40   LA    RE,7(,RE)                                                        
         STCM  RE,3,ELEM                                                        
*                                                                               
CKFT50   LH    R1,PRTCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,PRTCT                                                         
         STCM  R1,3,ELEM+2                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,ELEM                                                        
         A     R1,TSARBYTE                                                      
         ST    R1,TSARBYTE                                                      
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
         MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    CKFX                                                             
         DC    H'0'                                                             
CKFTMVC  MVC   ELEM+6(0),P                                                      
CKFTMVCA MVC   ELEM+8(0),0(R1)                                                  
*                                                                               
CKFT60   CLI   SVTWPR09,C'2'       SEE IF FAXING                                
         BNE   CKFX                                                             
*                                                                               
         L     R4,=A(PRTFILO)                                                   
         MVI   P1+131,1            FOR SPACING                                  
         PUT   (R4),P1                                                          
CKFX     B     FTHKX                                                            
         DROP  R2                                                               
         EJECT                                                                  
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* PRINT OUT PRTFILE OFFLINE                                                     
*                                                                               
PRTEZ    NMOD1 0,**PREZ**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
* CLOSE OUTPUT PRTFILE, OPEN INPUT, AND PRINT FAX COPY OF INSTR *               
*                                                                               
         L     R2,=A(PRTFILO)                                                   
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
         L     R2,=A(PRTFILI)                                                   
         OPEN  ((2),INPUT)                                                      
*                                                                               
         LTR   RF,RF               CHECK OPEN WAS OK                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,ENDEZPRT                                                      
         STCM  R0,7,33(R2)                                                      
*                                                                               
         L     R4,AIO1                                                          
*                                                                               
PRTEZ10  XC    0(132,R4),0(R4)                                                  
         GET   (R2),(R4)                                                        
         CLC   0(120,R4),SPACES                                                 
         BE    PRTEZ10                                                          
         CLC   =C'/PAGE',0(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    CLOSE OLD QUE ENTRY                           
         MVI   PQSW,1                                                           
*                                                                               
* RE-OPEN OR OPEN DIRECT TO AGENCY PRINT QUE ENTRY *                            
*                                                                               
         LA    R1,ELEM                                                          
         XC    ELEM(128),ELEM                                                   
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
*NOP     MVI   QLRETNL+1,36                                                     
******   MVI   QLRETND+1,12                                                     
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         DROP  R1                                                               
*                                                                               
* FORCE TO 'G' FOR EASYLINK TRANMISSION AND NEW QUE ENTRY *                     
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
*                                                                               
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
*                                                                               
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(,RE)                                        
         MVI   140(RE),C'Y'        SET EASYLINK STUFF PRINTED SW                
*                                                                               
         L     R1,136(,RE)         PAST SAVED DCB                               
         LA    R1,1(,R1)                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REMOTFRM+1(3),DUB                                                
         ST    R1,136(,RE)                                                      
*                                                                               
         MVC   REMOTJID,=C'SWX'                                                 
*                                                                               
         OC    REMOTTY1,SVFAXARC                                                
*                                                                               
         MVC   REMOTDST,TWAORIG                                                 
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
*                                                                               
         L     RE,TWADCONS                                                      
         L     RE,TSPFUSER-TWADCOND(RE)                                         
         SR    R1,R1                                                            
         ICM   R1,15,136(RE)                                                    
         LA    R1,1(,R1)                                                        
         STCM  R1,3,REMOTSUB       FORCE PRT QUE KEY CHANGE                     
         STCM  R1,15,136(RE)                                                    
         MVI   REMOTCLS,C'G'       SHOULD BE CLASS G                            
         MVI   REMOTCPY,1                                                       
         DROP  RF                                                               
         GOTO1 OPENPQ             OPEN NEW QUE ENTRY                            
*                                                                               
* KILL HEADLINES, POSSIBLE FUTURE FOOTLINES, AND SPECS *                        
*                                                                               
         SR    R1,R1                                                            
         ST    R1,HEADHOOK         NO HEADHOOK                                  
         ST    R1,FOOTHOOK         OR FOOTHOOK                                  
         ST    R1,SPECS            OR SPECS                                     
*                                                                               
*  EASYLINK GETS A PRINT LINE WITH SOME FIXED INFO, AND FAX NUMBER *            
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
PRTEZ20  MVI   LINE,2                                                           
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=C'*HDR*'                                                 
*                                                                               
         MVC   QSTA,112(R4)                                                     
         MVC   SVLFAX,QSTA                                                      
         MVC   EDIDESID(5),QSTA                                                 
         MVI   EDIWIDE,C'L'                                                     
         MVI   EDIPAGE,C'P'                                                     
         MVC   EDIFDEST(4),QSTA                                                 
         LA    RE,EDIFDEST+3                                                    
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    PRTEZ30                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    PRTEZ30                                                          
         MVI   3(RE),C' '                                                       
*                                                                               
PRTEZ30  MVC   EDIBILL(4),QMED    MEDIA & CLIENT                                
         MVC   EDIBILL+4(3),QPRD                                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPINSTRN'                                  
*                                                                               
         MVC   EDISTTMD(4),QMED        MEDIA & CLIENT                           
         MVC   EDISTTPR,QPRD                                                    
         MVC   EDISTTP2,QPRD2                                                   
         MVC   EDISTTES,=C'NO '    SET EST=NO UNLESS EST IS USED                
         ZICM  R0,QBEST,1                                                       
         BZ    *+18                LEAVE EST=NO                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDISTTES,DUB                                                     
*                                                                               
         MVC   EDISTTDT(6),USERQSTR     START DATE                              
         MVC   EDISTTDT+6(6),USERQEND   END DATE                                
         MVC   EDISTTST(5),QSTA         STATION                                 
         MVC   EDISTTCT(16),QUESTOR     CONTACT                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
PRTEZ40  XC    0(132,R4),0(R4)                                                  
         GET   (R2),(R4)                                                        
*                                                                               
         CLC   =C'/PAGE',0(R4)     THIS NEW PAGE                                
         BNE   PRTEZ50              NO                                          
*                                                                               
         OC    112(L'QSTA,R4),112(R4)  THIS A START OF NEW FAX                  
         BZ    PRTEZ50                                                          
*                                                                               
         CLC   SVLFAX,112(R4)      SAME AS LAST FAX NUMBER (STATION)            
         BE    PRTEZ46                                                          
*                                                                               
         MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTEZ20                                                          
*                                                                               
PRTEZ46  XC    112(L'QSTA,R4),112(R4)  ZERO OUT SO ESYLNK RECOGNIZES IT         
*                                      IF SAME OR NO FAX NUMBER                 
*                                                                               
PRTEZ50  MVC   P(130),0(R4)                                                     
*                                                                               
         MVC   SPACING(1),131(R4)                                               
*         MVI   SPACING,1                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,2               I MUST RESET LINE                           
         B     PRTEZ40                                                          
*                                                                               
ENDEZPRT CLOSE ((2),)              PRTFILE                                      
         FREEPOOL (R2)                                                          
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         MVI   PQSW,X'FF'                                                       
         GOTO1 SPOOL,DMCB,(R8)    CLOSE OLD QUE ENTRY                           
         MVI   PQSW,1                                                           
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
*NOP     MVI   QLRETNL+1,36                                                     
****     MVI   QLRETND+1,12                                                     
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         DROP  R1                                                               
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
*                                                                               
* SEE IF ORIGINALLY PRINTING DDS SHOP                                           
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
*                                                                               
         OC    MCREMOTE-MASTD(,RE),MCREMOTE-MASTD(RE)                           
         BZ    ENDEZP10            YES                                          
         CLI   DIRPRTSW,C'Y'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*        MVC   REMOTKEY,MCREMOTE-MASTD(RE) RESTORE ORIGINAL DIRECT              
         LA    R1,MCREMOTE-MASTD(,RE)                                           
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVC   REMOTPRG,REMOTPRG-REMOTED(R1)                                    
         MVC   REMOTFRM,REMOTFRM-REMOTED(R1)                                    
         MVC   REMOTJID,REMOTJID-REMOTED(R1)                                    
         MVC   REMOTDST,REMOTDST-REMOTED(R1)                                    
         MVC   REMOTCLS,REMOTCLS-REMOTED(R1)                                    
         MVC   REMOTCPY,REMOTCPY-REMOTED(R1)                                    
         B     ENDEZP20                                                         
*                                                                               
ENDEZP10 XC    REMOTKEY,REMOTKEY                                                
         DROP  RF                                                               
*                                                                               
ENDEZP20 GOTO1 OPENPQ             OPEN FOR END LOGOS                            
*                                                                               
         XIT1                                                                   
         DROP  RB,RC                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
         EJECT                                                                  
       ++INCLUDE SPTRTBAE                                                       
         TITLE 'T2164A - SPOT DETAIL INSTRUCTIONS - DSECTS'                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRADAD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
NEXTLINE EQU   TRASEL2H-TRASEL1H                                                
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
DATELIST DS    367X                                                             
* FROM BUY ELEMENT                                                              
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
STAWORK  DS   0CL22                                                             
SPTWORK  DS    XL(L'SPTDATA)                                                    
ROTDAYS  DS    H                                                                
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTRRR   DS    A                                                                
PQPROF   DS    A                                                                
ASVSTOR  DS    A                                                                
ASTABL   DS    A                                                                
ASTABLX  DS    A                                                                
ASPTABL  DS    A                                                                
ASPTABLX DS    A                                                                
AERRFIL  DS    A                                                                
ASVSTAD  DS    A                                                                
ASVFTNT  DS    A                                                                
ASHPLIST DS    A                   LIST OF COMMLS TO BE SHIPPED                 
ASHPLISX DS    A                   LIST OF COMMLS TO BE SHIPPED                 
*                                  (ALSO USED FOR TITLE, TYPE, SEQ)             
ACMLSHIP DS    A                   COMMERCIAL SHIP LIST                         
SPTTSIZE DS    F                                                                
*                                                                               
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL2)                                                      
*                                                                               
PROFKEY  DS    CL5                 READ PROGRAM PROFILE RECORD                  
SVQLTYP1 DS    CL1                 STORE ARCHIVE SETTING                        
SVFAXARC DS    CL1                 ARCHIVE SETTING FOR FAX COPIES               
CLEARAR  EQU   *                   EVERYTHING BET CLEARAR & CLEARARX            
*                                  CLEARED AT VK10                              
*                                                                               
SVLFAX   DS    CL(L'QSTA)                                                       
SVREMUSR DS    CL3                                                              
SVTWPR09 DS    C                   TW SP GEN FAX DEFAULT                        
SVTZPR01 DS    C                   FLEXIBLE DATES PROFILE                       
SVTZPR03 DS    C                   SORT BY DATE/TIME/PRD/PTR                    
SVTZPR04 DS    C                   SHOW PRODUCTS IN SAME ORDER AS BUY           
SVTBPR04 DS    C                   NO ESTIMATES REQUIRED (TB PROFILE)           
*MNMB                                                                           
SVT3PR06 DS    C                   EXCLUDES DV/SM/CM FROM TRAFFIC               
*MNMB                                                                           
PRCMLSQ1 DS    CL2                                                              
ATSAR    DS    A                   TSAR CORE RESIDENT T00A5D                    
*                                                                               
TSARBYTE DS    F                   BYTES SENT TO TSAR THIS SESSION              
PRTCT    DS    H                   CT OF PRINT LINES SAVED                      
*                                                                               
DIRPRTSW DS    C                   DIRECT CARD FOR THESE REQUESTS               
*                                                                               
VTRPACK  DS    A                                                                
NEXTADDR DS    A                                                                
SEQNUM   DS    H                                                                
SVINSDT  DS    XL2                 LAST INST DATE                               
SVINSREV DS    XL1                                                              
FOOTSW   DS    C                                                                
CONTINUE DS    C                                                                
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SVDSKAD  DS    XL4                                                              
SVPNAME  DS    CL16                                                             
ESTDESC  DS    CL20                                                             
*                                                                               
PTCMLCOD DS    CL12                CODE                                         
PTCMLADI DS    CL12                AD-ID                                        
PTCMLDSC DS    CL15                SAVED CLIENT COMML DESC                      
PTCMLDS2 DS    CL20                SAVED CLIENT COMML DESC 2                    
PTCMLDS3 DS    CL20                SAVED CLIENT COMML DESC 3                    
PTCMLCLT DS    CL20                SAVED CLIENT COMML #                         
*                                                                               
         DS    0D                                                               
SVESTAB  DS    XL256                                                            
ASVNEXT  DS    A                   1ST TABLE ENTRY ON NEXT SCREEN               
*                                                                               
* FROM FLIGHT RECORD OR ENTERED TELECAST DATES                                  
*                                                                               
GENSTP   DS    H                   FLIGHT/TELECAST START DATE                   
GENENDP  DS    H                   FLIGHT/TELECAST END DATE                     
*                                                                               
* FROM FLIGHT OR ESTIMATE RECORD *                                              
*                                                                               
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START OF FLT/EST                             
SVPEREND DS    XL3                 END OF FLT/EST                               
*                                                                               
SVPERDTP DS    0XL4                                                             
SVPERSTP DS    XL2                 START OF FLT/EST                             
SVPEREDP DS    XL2                 END OF FLT/EST                               
*                                                                               
* FROM VPER RTN                                                                 
*                                                                               
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 INSTRUCTION START DATE                       
SVGENEND DS    XL3                 INSTRUCTION END DATE                         
*                                                                               
SVGENDTP DS    0XL4                                                             
SVGENSTP DS    XL2                 INSTRUCTION START DATE                       
SVGENEDP DS    XL2                 INSTRUCTION END DATE                         
*                                                                               
SPOTCT   DS    H                                                                
RECCT    DS    H                                                                
HOLDSIGN DS    CL1                                                              
SVSPACE  DS    CL1                                                              
SPOTNUMB DS    XL1                                                              
INSPRTSW DS    XL1                                                              
INSPRTPR EQU   X'80'               THERE WERE PERVIOUS INSTR GENERATED          
INSPRTCH EQU   X'40'               SPOTS WERE CHANGED SINCE LAST INSTR          
INSPRTNO EQU   X'20'               INSTR NOT PRINTED                            
TABLESW  DS    XL1                 Y=TABLE BUILT                                
*                                                                               
ANYFLAG  DS    XL1                                                              
PRD2FSW  EQU   X'80'               PROD 2 FOUND SWITCH                          
*                                                                               
UPDSW    DS    XL1                                                              
UPDRECSW EQU   X'80'               UPDATE REC - SHIP REC & TBA REC              
UPDEST   EQU   X'40'               ESTIMATE ELEMENT UPDATED                     
*                                20 PRINTING SPOTS, PRT MIDLNS IN HDHK          
*                                10 INSTR PRINTED FOR STATION SEND DIP          
*                                08 DO GETREC IN PCMT RTN                       
*                                04 NO BOXES ALLOWED IN PCMT RTN                
*                                02 CK FOR =NAME IN PCMT RTN                    
PTCMLTXT EQU   X'01'             01 COMMERCIAL TEXT FOUND                       
*                                                                               
UPDSW2   DS    XL1                                                              
UPDBUYSW EQU   X'80'               UPDATE BUY- SPOT PRINTED                     
UPDPRTSW EQU   X'80'               SPOT PRTD - NOT PRTD BEFORE, UPDATE          
*                                                                               
SHIPYORN DS    CL1                                                              
AUTOQSW  DS    CL1                                                              
QUWHEN   DS    CL8                                                              
QUESTOR  DS    CL24                1ST 17 FROM SCREEN, ONLY FULL 24 IF          
*                                         FROM AGENCY CONTACT REC               
CONTEL   DS    CL18                TEL NUMBER FROM AGENCY CONTACT REC           
CONFAX   DS    CL18                FAX NUMBER FROM AGENCY CONTACT REC           
SVMKTSTA DS   0XL5                                                              
SVBMKT   DS    XL2                                                              
SVSTA    DS    XL3                                                              
SVSTATYP DS    CL1                                                              
STACT    DS    XL1                                                              
*                                                                               
RQBMKT   DS    XL2                 REQUESTED MKT (CANADA MKT = 0 VALID)         
*                                                                               
* KEEP ALL SVPROD TO SVSLN2 TOGETHER AND IN ORDER *                             
*                                                                               
SVPRODS  DS   0XL10                                                             
SVPROD   DS    CL3                                                              
SVPRD    DS    XL1                                                              
SVSLN    DS    XL1                                                              
SVPROD2  DS    CL3                                                              
SVPRD2   DS    XL1                                                              
SVSLN2   DS    XL1                                                              
*                                                                               
* SAVE AREA FOR OPTIONS                                                         
*                                                                               
SVOPTDTA DS    0CL28                                                            
SVOPTSTA DS    XL3                                                              
SVOPTDTE DS    XL3                                                              
SVOPTLEN DS    XL1                                                              
*                                                                               
SVOPT1   DS    XL1                                                              
*                                                                               
OPTTEST  EQU   X'80'                                                            
OPTRERUN EQU   X'40'                                                            
OPTREV   EQU   X'20'                                                            
OPTNEW   EQU   X'10'                                                            
OPTSEED  EQU   X'08'                                                            
OPTNOAGY EQU   X'04'                                                            
*        EQU   X'02'               UNUSED                                       
OPT1STA  EQU   X'01'                                                            
*                                                                               
SVOPT2   DS    XL1                                                              
*                                                                               
OP2NOSTA EQU   X'80'                                                            
OP2SPCMT EQU   X'40'                                                            
*                                                                               
FTPROGLN DS    XL1                           PROGRAM NAME LENGTH                
FTPROGNM DS    CL18                FILTER ON PROGRAM NAME                       
*                                                                               
         DS    0D                                                               
STABLE   DS    XL880               40 STATION ENTRIES AT 22 BYTES EACH          
*                                                                               
CLEARARX EQU   *                   EVERYTHING BET CLEARAR & CLEARARX            
*                                  CLEARED AT VK10                              
*                                                                               
SPTABLE  DS    XL9000              SPOT TABLE BUILD AREA                        
ENDSYSD  DS    0C                  IF ADDR OF ENDSYSD MORE THAN 2F10            
*                                  BIG TROUBLE                                  
         EJECT                                                                  
* DSECT FOR STATION TABLE *                                                     
*                                                                               
STABLED  DSECT                                                                  
STADATA  DS    0XL22                                                            
STACOMP  DS    0XL13                                                            
STAMSTA  DS    0XL5                                                             
STAMKT   DS    XL2                                                              
STASTA   DS    XL3                                                              
STAPROD  DS    CL3                                                              
STASLN   DS    XL1                 SPOT LENGTH                                  
STAPROD2 DS    CL3                                                              
STASLN2  DS    XL1                 SPOT LENGTH                                  
STASPTS  DS    XL2                 SPOTS                                        
STANOASG DS    XL2                 COUNT OF UNASSIGNED SPOTS                    
STAFTD   DS    XL2                 STATION FIRST TELECAST DATE                  
STALTD   DS    XL2                 STATION LAST TELECAST DATE                   
STAFLAG  DS    XL1                 80 INSTRUCTIONS PREVIOUSLY GENERATED         
STAFLGPI EQU   X'80'               INSTRUCTIONS PREVIOUSLY GENERATED            
*                                  40 INSTR GENERATED, PRT ALL REPORTS          
STANEXT  EQU   *                                                                
*                                                                               
* DSECT FOR SPOT ACTIVITY LIST ENTRIES *                                        
*                                                                               
SPTABLED DSECT                                                                  
SPTDATA  DS    0XL37                                                            
SPTSORT  DS    0XL14                                                            
SPTPROD  DS    CL3                 PROD 1                                       
SPTSLN   DS    XL1                 SPOT LENGTH                                  
SPTPROD2 DS    CL3                 PROD 2                                       
SPTSLN2  DS    XL1                 SPOT LENGTH 2                                
SPTFTD   DS    XL2                 SPOT TELECAST DATE                           
SPTTIME  DS    XL4                 START/END TIMES                              
*                                                                               
SPTDAY   DS    XL1                 DAYS                                         
SPTEST   DS    XL1                 ESTIMATE                                     
SPTLINE  DS    XL2                 BUY LINE                                     
SPTCMLSQ DS    XL2                 CML SEQ NUMBER                               
SPTCMLS2 DS    XL2                 CML SEQ NUMBER 2                             
SPTDPT   DS    XL1                 DAY PART                                     
SPTFLAG  DS    XL1                 80 INSTRUCTIONS PREVIOUSLY GENERATED         
SPTFLGPI EQU   X'80'               INSTR PREVIOUSLY GENERATED                   
SPTFLGIG EQU   X'40'                                                            
*                                  40 INSTR GENERATED, PRT ALL REPORTS          
SPTPREF  DS    XL2                 PATTERN REF - 1ST BIT IS PRINTED FLG         
SPTDSKAD DS    XL4                 BUY REC DISK ADDRESS                         
SPTSPTN  DS    XL1                 SPOT NUMBER                                  
SPTPRD   DS    XL1                                                              
SPTPRD2  DS    XL1                                                              
SPTLTD   DS    XL2                 SPOT TELECAST DATE                           
         DS    XL2                 SPARE                                        
SPTNEXT  EQU   *                                                                
         EJECT                                                                  
* DSECT FOR COMMERCIAL SAVE TABLE ENTRY *                                       
*                                                                               
SVCMLD   DSECT                                                                  
SVCMLDTA DS    0CL112                                                           
SVCMLCOD DS    CL8                                                              
SVCMLTYP DS    CL4                                                              
SVCMLPIG DS    CL1                 0 FOR SOLO - 1 OR 2 FOR P/B                  
SVCMLPRD DS    CL1                                                              
SVCMLSEQ DS    XL2                                                              
SVCMLFTD DS    XL2                 FIRST TELECAST DATE                          
SVCMLLTD DS    XL2                 LAST TELECAST DATE                           
SVCMLDSC DS    CL15                COMML DESCRIPTION                            
SVCMLDS2 DS    CL20                COMML DESC 2                                 
SVCMLDS3 DS    CL20                COMML DESC 3                                 
SVCMLCLT DS    CL20                LAST TELECAST DATE                           
SVCMLADI DS    CL12                ADID CODE                                    
SVCMLST  DS    XL1                 X'01'=CMML CODE IS PACKED                    
         DS    XL4                                                              
SVCMLNXT EQU   *                                                                
*                                                                               
* DSECT FOR PATTERN RECAP AND SHIP GEN AUTO REQUESTS *                          
*                                                                               
XRECD    DSECT                                                                  
XREC     DS    0XL64                                                            
*                                                                               
XTYPE    DS    CL1                 P=PTN RCP/S=SHP ORDER                        
XQUESTOR DS    CL12                REQUESTOR (FOR SHIP ORDER)                   
*                                  PTN RCP USES 'WHEN' FIELD HERE               
XMED     DS    CL1                 MEDIA                                        
XCLT     DS    CL3                 CLIENT                                       
XPRD     DS    CL7                 PRD1-SLN1                                    
XPRD2    DS    CL7                 PRD2-SLN2                                    
XCOPY    DS    CL1                 COPY CODE                                    
XMKT     DS    CL4                 MARKET (DEALER RECAP ONLY)                   
XRERUN   DS    0CL6                RERUN DATE (SHP ORDER ONLY)                  
XFLTST   DS    CL6                 FLIGHT START                                 
XFLTEND  DS    CL6                 FLIGHT END                                   
XWHEN    DS    CL7                 PRINT WHEN                                   
         DS    CL9                 SPARE                                        
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LSTLINE  DSECT                                                                  
         DS    CL8                 FIELD HEADER                                 
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LMKT     DS    CL4                                                              
         DS    CL2                                                              
LPRDSLN  DS    CL7                                                              
         DS    C                                                                
LPTRSLN  DS    CL7                                                              
         DS    C                                                                
LSPOTS   DS    CL3                                                              
         DS    CL2                                                              
LFTLCST  DS    CL8                                                              
         DS    CL2                                                              
LLTLCST  DS    CL8                                                              
         DS    CL1                                                              
LPRTID   DS    CL17                                                             
*                                                                               
* DSECT FOR PRINT LINE DATA *                                                   
*                                                                               
SPOOLD   DSECT                                                                  
*                                                                               
         ORG   P                                                                
*                                                                               
PDAY     DS    CL8                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PPNAME   DS    CL16                                                             
         DS    CL1                                                              
PDAYPT   DS    CL1                                                              
         DS    CL1                                                              
PSLN     DS    CL3                                                              
         DS    CL2                                                              
PDATE    DS    CL11                                                             
         DS    CL2                                                              
PPROD    DS    CL3                                                              
         DS    CL1                                                              
PPRODNM  DS    CL18                                                             
         DS    CL1                                                              
PCML     DS    CL12                                                             
         DS    CL1                                                              
PCMLTI   DS    CL15                                                             
*                                                                               
         ORG   P                                                                
       ++INCLUDE EDIDESTD                                                       
         ORG   P                                                                
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108SPTRA4A   05/20/19'                                      
         END                                                                    
