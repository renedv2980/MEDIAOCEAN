*          DATA SET DDRECOIL   AT LEVEL 002 AS OF 09/20/11                      
*PHASE RECOILA                                                                  
*INCLUDE ENDOFMOD                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE FILTABL                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'RECOIL - OFFLINE RECOVERY'                                      
**********************************************************                      
* TITLE:      RECOIL - OFFLINE RECOVERY                  *                      
* COMMENTS:   RECOVER OR RERUN OFFLINE JOB BY READING    *                      
*             UPDATES FROM SYSTEM RECOVERY FILE          *                      
* PARAMETERS: JOB CONTROL CARDS CAN BE INPUT AS FOLLOWS; *                      
*                                                        *                      
*    SYSTEM=<FACPAK SYSTEM NAME>=<SYSTEM FILE CODE)      *                      
*      E.G.  SYSTEM=ACCOUNT=5  FOR ACC5                  *                      
*      OR                                                *                      
*    SYSTEM=<FACPAK SYSTEM NAME>                         *                      
*    AGENCY=<AGENCY ALPHA ID FOR SYSTEM FILE CODE)       *                      
*      E.G.  SYSTEM=ACCOUNT                              *                      
*            AGENCY=D5                                   *                      
*    JOBNAME=<NAME OF OFFLINE JOB>                       *                      
*      AND                                               *                      
*    JESID=<JES-ID OF OFFLINE JOB> - ELSE ALL JOBS       *                      
*      OR                                                *                      
*    JESID=THIS - RECOVER THIS JOB WITHIN CURRENT 'EXEC' *                      
*    MODE=RECOVER - RECOVER ANY UPDATES MADE BY JOB      *                      
*      OR                                                *                      
*    MODE=RERUN   - RERUN ANY UPDATES MADE BY JOB        *                      
*    INPUT=TAPE - OPTIONAL, RECOVERY FILE IS ON TAPE     *                      
*    PATCH=NNNNNN NN - OPTIONAL TEST CARD                *                      
*    DDSIO=<DDSIO> - OPTIONAL TO SET DDSIO               *                      
*    DSPACE=<X> - OPTIONAL TO SET DSPACE                 *                      
**********************************************************                      
                                                                                
RECOIL   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKX-WORKD,**RECO**,RA,R9,R8,WORK=A(WORKC),CLEAR=YES,  X        
               RR=RE                                                            
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         MVI   MVSPARM,0                                                        
         L     R1,0(R1)            GET MVS PARMS                                
         SR    RF,RF                                                            
         LH    RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   MVSPARM,1                                                        
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         MVC   TITLE(20),=C'OFFLINE JOB RECOVERY'                               
         ICM   RE,15,=V(ENDOFMOD)  SET STXIT IF END OF MODULE CSECT             
         USING RECDS,R6                                                         
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     EQU   *                                                                
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         GOTO1 VDMOD000,P1,A(DMODFLS)                                           
         L     R1,0(R1)            GET A(DATA MANAGER FILES TABLE)              
         ST    R1,VDMGRFLS                                                      
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IOL                            
*                                                                               
         BAS   RE,AGYSYS           SET UP AGENCY SYSTEM DATA                    
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,SYSFLIST         BUILD SYSFLES LIST                           
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,OPENFIL          OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         CLI   INTAPE,C'Y'                                                      
         BE    MJOB                                                             
         BAS   RE,READLAST         READ LAST RECORD ON RECOVERY FILE            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,INITBLK          INITIALISE FOR BLOCKED/UNBLOCKED             
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
MJOB     EQU   *                   PROCESS JOB IN RECOVERY FILE                 
         TM    MODE,RRUNQ          TEST EXECUTION MODE                          
         BO    MRRUN                                                            
MRECV    BAS   RE,JOBRECV          PROCESS JOB IN RECOVERY MODE                 
         BNE   MERR                  EXIT IF ERROR                              
         B     MCLOS                                                            
MRRUN    BAS   RE,JOBRRUN          PROCESS JOB IN RE-RUN MODE                   
         BNE   MERR                  EXIT IF ERROR                              
         B     MCLOS                                                            
*                                                                               
MCLOS    BAS   RE,CLOSEFIL         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         B     MXIT                EXIT OK                                      
*                                                                               
MERR     EQU   *                   EXIT HERE IF ERROR                           
         MVI   RETCODE,0                                                        
         CLI   ERROR,0                                                          
         BNE   *+8                                                              
         MVI   ERROR,UNDEFERQ                                                   
         BAS   RE,ERRPRT                                                        
         B     MXIT                                                             
*                                  EXIT HERE IF OK                              
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME                                                                   
         ST    R0,MVSTIME                                                       
         ST    R1,MVSDATE                                                       
         MVC   CENTURY,=CL2'19'                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAY+2)                                   
         MVC   TODAY(2),CENTURY                                                 
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYC)                                    
         MVI   MODE,RCOVQ                                                       
         MVI   INTAPE,C'N'                                                      
         MVI   RETCODE,X'FF'                                                    
         MVI   SYSCHAR,C'0'                                                     
         L     RF,=A(INPUT)                                                     
         ST    RF,AINPUT                                                        
         L     RF,=A(TRKBUFF)                                                   
         ST    RF,ATRKBUFF                                                      
         L     R6,=A(RECBUFF)                                                   
         ST    R6,ARECBUFF                                                      
         ST    R6,ARCVHDR                                                       
         L     RF,=A(RCVSAVE)                                                   
         ST    RF,ARCVSAVE                                                      
         L     RF,=A(RCVCOPY)                                                   
         ST    RF,ARCVCOPY                                                      
         L     RF,=V(FILTABL)                                                   
         ST    RF,AFILTAB                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VCSYSTEM                                                         
         B     VCAGENCY                                                         
         B     VCINPUT                                                          
         B     VCPATCH                                                          
         B     VCDDSIO                                                          
         B     VCJOBNAM                                                         
         B     VCJESID                                                          
         B     VCMODE                                                           
         B     VCDSPACE                                                         
*                                  EXIT/ERROR CONDITIONS                        
*                                  CHECK REQUIRED INPUT                         
VCEND    CLI   SYSTEM,0            CHECK SYSTEM ENTERED                         
         BE    VCERR2                                                           
         B     VCOK                                                             
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   MVI   ERROR,VCER1Q        INVALID CARD                                 
         B     VCNO                                                             
VCERR2   MVI   ERROR,VCER2Q        MISSING SYSTEM DEFINITION                    
         B     VCNO                                                             
VCERR3   MVI   ERROR,VCER3Q        INVALID SYSTEM NAME                          
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCOK     B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
VCSYSTEM EQU    *                   SYSTEM=                                     
         LA    R4,P+7                                                           
         SR    RF,RF                                                            
*                                                                               
VCSY010  CLI   0(R4),C' '                                                       
         BE    VCSY012                                                          
         CLI   0(R4),C'='                                                       
         BE    VCSY012                                                          
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         B     VCSY010                                                          
*                                                                               
VCSY012  LTR   RF,RF                                                            
         BZ    VCERR3                                                           
         BCTR  RF,0                                                             
         LA    R3,SYSLST                                                        
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VCSY020  CLI   0(R3),0             TEST E-O-T                                   
         BE    VCERR3                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VCSY022                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),P+7                                                  
         BE    VCSY030                                                          
VCSY022  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VCSY020                                                          
*                                                                               
VCSY030  MVC   SYSCODE,SYSLRPLT    RETURN SYSTEM CODE                           
         MVC   SYSTEM,SYSLNUM        AND SYSTEM NUMBER                          
*                                                                               
         CLI   0(R4),C' '                                                       
         BE    VCLP1                                                            
         CLI   0(R4),C'='                                                       
         BNE   VCERR1                                                           
*                                                                               
VCSY100  EQU   *                                                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    VCERR1                                                           
*                                                                               
         CLI   0(R4),C'A'                                                       
         BL    VCERR1                                                           
         CLI   0(R4),C'Z'                                                       
         BNH   VCSY120                                                          
         CLI   0(R4),C'0'                                                       
         BL    VCERR1                                                           
         CLI   0(R4),C'9'                                                       
         BH    VCERR1                                                           
VCSY120  MVC   SYSCHAR,0(R4)                                                    
         B     VCLP1                                                            
         EJECT                                                                  
VCAGENCY EQU   *                   AGENCY=                                      
         MVC   AGYID,P+7                                                        
         B     VCLP1                                                            
                                                                                
VCINPUT  EQU   *                   INPUT=                                       
         CLC   P+6(4),=C'TAPE'                                                  
         BNE   VCERR1                                                           
         MVI   INTAPE,C'Y'                                                      
         B     VCLP1                                                            
                                                                                
VCPATCH  EQU   *                   PATCH=111111 11                              
         XC    FULL,FULL           GET DISPLACEMENT INTO FULL                   
         GOTO1 VHEXIN,DMCB,P+6,FULL+1,6                                         
         CLC   DMCB+12(4),=F'3'                                                 
         BNE   VCERR1              MUST BE 6 HEX DIGITS                         
         LA    R4,P+71             FIND LENGTH OF PATCH DATA                    
         LH    RE,=H'-1'                                                        
         LA    RF,P+12                                                          
         CLI   0(R4),C' '                                                       
         BNE   *+12                                                             
         BXH   R4,RE,*-8                                                        
         B     VCERR1                                                           
         SR    R4,RF               L'PATCH DATA IN R4                           
         GOTO1 VHEXIN,DMCB,P+13,WORK,(R4)                                       
         ICM   R1,15,DMCB+12       GET L'HEX PATCH DATA IN R1                   
         BZ    VCERR1              ZERO IS NOT ALLOWED                          
         BCTR  R1,0                                                             
         L     RF,FULL             PATCH DISPLACEMENT IN RF                     
         AR    RF,RB               RF = A(AREA TO BE PATCHED)                   
         EX    R1,*+8              MOVE IN THE PATCH DATA                       
         B     VCLP1                                                            
         MVC   0(0,RF),WORK                                                     
                                                                                
VCDDSIO  EQU   *                   DDSIO=                                       
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    VCERR1                                                           
         MVC   0(8,RF),P+6                                                      
         B     VCLP1                                                            
                                                                                
VCJOBNAM EQU  *                   JOBNAME=                                      
         OC    JOBNAME,JOBNAME    CHECK JOBNAME NOT ALREADY SET BY              
         BNZ   VCERR1               JESID=THIS CARD                             
         MVC   JOBNAME,P+8                                                      
         B     VCLP1                                                            
                                                                                
VCJESID  EQU  *                   JESID=                                        
         CLC   P+6(4),=CL4'THIS'                                                
         BNE   VCJE010                                                          
         OC    JOBNAME,JOBNAME    CHECK JOBNAME NOT ALREADY SET BY              
         BNZ   VCERR1               JOBNAME= CARD                               
         LA    R3,EXPARMS                                                       
         EXTRACT (3),'S',FIELDS=TIOT                                            
         L     R3,EXPARMS                                                       
         MVC   JOBNAME,0(R3)                                                    
         ST    RD,SAVERD                                                        
         LINK  EP=FWRGETJB,PARAM=(RETJOBID)                                     
         L     RD,SAVERD                                                        
         MVC   JESID,RETJOBID                                                   
         B     VCLP1                                                            
*                                                                               
VCJE010  MVC   JESID,P+6                                                        
         B     VCLP1                                                            
                                                                                
VCMODE   EQU   *                   MODE=                                        
         MVI   MODE,0                                                           
         LA    RE,MODETAB                                                       
VCMO010  CLI   0(RE),X'FF'                                                      
         BE    VCERR1                                                           
         CLC   0(8,RE),P+5                                                      
         BE    VCMO020                                                          
         LA    RE,L'MODETAB(RE)                                                 
         B     VCMO010                                                          
VCMO020  OC    MODE,8(RE)          OR IN THE MODE BIT                           
         B     VCLP1                                                            
                                                                                
VCDSPACE EQU   *                   DSPACE=                                      
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     VCLP1                                                            
                                                                                
PACKIN   EQU   *                                                                
         SR    R1,R1               GET STRING LENGTH                            
         LA    R0,8                                                             
*                                                                               
PIN010   CLI   0(RF),C' '                                                       
         BE    PINOK                                                            
         CLI   0(RF),C'0'                                                       
         BL    PINNO                                                            
         CLI   0(RF),C'9'                                                       
         BH    PINNO                                                            
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PIN010                                                        
         B     PINNO                                                            
*                                                                               
PINNO    SR    R1,R1                                                            
PINOK    LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN SYSTEM FILES                                                   *         
***********************************************************************         
OPENFIL  NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'UPDID'                                          
         L     RE,12(R1)                                                        
         MVC   0(2,RE),=C'XX'                                                   
*                                                                               
         CLI   INTAPE,C'Y'         OPEN TAPE FILE                               
         BNE   OFIL010                                                          
         OPEN  (RCVTAPE,(INPUT))                                                
         B     OFIL020                                                          
*                                  OPEN SYSTEM DISK FILES                       
OFIL010  EQU   *                                                                
         GOTO1 VDMOD000,P1,A(DMODOSYS),(SENUM,IOL)                              
*                                                                               
OFIL020  EQU   *                                                                
         B     OFILOK                                                           
*                                                                               
OFILNO   MVI   ERROR,OFILERQ                                                    
         B     NO                                                               
OFILOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CLOSE SYSTEM FILES                                                  *         
***********************************************************************         
CLOSEFIL NTR1                                                                   
         CLI   INTAPE,C'Y'         CLOSE TAPE FILE                              
         BNE   CFIL010                                                          
         CLOSE (RCVTAPE)                                                        
         B     CFIL020                                                          
*                                  CLOSE SYSTEM DISK FILES                      
CFIL010  EQU   *                                                                
         GOTO1 VDMOD000,P1,A(DMODCSYS),(SENUM,IOL)                              
*                                                                               
CFIL020  EQU   *                                                                
         B     CFILOK                                                           
*                                                                               
CFIL030  EQU   *                                                                
         B     CFILOK                                                           
*                                                                               
CFIL040  B     CFILOK                                                           
*                                                                               
CFILNO   MVI   ERROR,CFILERQ                                                    
         B     NO                                                               
CFILOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD SYSFLES LIST AND SAVE A(DTF SYSTEM RECOVERY FILE)             *         
***********************************************************************         
SYSFLIST NTR1                                                                   
         XC    DMCB+8(4),DMCB+8                                                 
         ZIC   RF,SENUM                                                         
         GOTO1 VDATAMGR,DMCB,DMREAD,SYSFLES,(RF)                                
         L     R4,12(R1)           THIS IS A(SYSFLES) ON RETURN                 
         ST    R4,SVSYSFL                                                       
         MVC   SVSYSFLH,0(R4)      SAVE SYSFLES HEADER                          
         MVC   SVSYSFL(1),SENUM    SAVE SYSTEM NUMBER                           
         ZIC   R0,3(R4)            GET NUMBER OF FILES                          
         LA    R4,4(R4)            POINT TO FIRST FILE                          
*                                                                               
SLIS010  TM    0(R4),X'40'         IS IT THE RECOVERY FILE                      
         BNZ   SLIS020             YES                                          
         LA    R4,8(R4)                                                         
         BCT   R0,SLIS010                                                       
         B     SLISNO              EXIT IF NO RECOVERY FILE                     
*                                                                               
SLIS020  L     RF,4(R4)            SAVE A(DTF) OF SYSTEM RECOVERY FILE          
         ST    RF,SVRECDTF                                                      
         B     SLISOK                                                           
*                                                                               
SLISNO   MVI   ERROR,SLISERQ                                                    
         B     NO                                                               
SLISOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ LAST RECORD ON FILE                                            *         
***********************************************************************         
READLAST NTR1                                                                   
         L     R3,ARECBUFF                                                      
         L     R5,SVRECDTF                                                      
         USING DTFPHD,R5                                                        
         TM    DTFOPEN,X'80'       EXIT IF READ ONLY FILE                       
         BO    RLASNO                                                           
         CLC   DNEXT,=X'00010000'  TEST NO RECS ON FILE                         
         BE    RLASNO                                                           
         MVC   RECDA,DNEXT         SET LAST REC ADDRESS                         
         MVI   RECDA+3,0                                                        
         XC    Q1(24),Q1                                                        
         LA    RF,RECDA                                                         
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R3),,(R5),(RF)                          
         OC    8(2,R1),8(R1)                                                    
         BNZ   RLASDER             DISK READ ERROR RCVR FILE                    
         MVC   SVBLKSZ,DBLKSZ                                                   
         OC    SVBLKSZ,SVBLKSZ     TEST IF BLOCKED RECOVERY FILE                
         BZ    RLASOK                OR UNBLOCKED                               
         CLC   RECDA(3),0(R3)      CHECK DA IN BLOCK AGREES WITH DNEXT          
         BNE   RLASDER             GET HIGH RECORD NUM IN BLOCK                 
         MVC   BRECNUM,3(R3)       SAVE NUMBER OF RECORDS IN BLOCK              
         LR    R1,R3                                                            
         AH    R1,4(R3)                                                         
         ST    R1,BLOCKEND         SAVE END OF BLOCK DISK ADDRESS               
         MVC   RECDA+3(1),BRECNUM  SET RECORD IN DA TO LAST IN BLOCK            
         MVC   LASTRDA,RECDA       SAVE LAST RECORD ADDRESS                     
         B     RLASOK                                                           
*                                                                               
RLASDER  MVI   ERROR,RLASDERQ                                                   
         B     NO                                                               
*                                                                               
RLASNO   MVI   ERROR,RLASERQ                                                    
         B     NO                                                               
RLASOK   B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR BLOCKED OR UNBLOCKED RECOVERY                        *         
***********************************************************************         
INITBLK  NTR1                                                                   
         L     R6,ARECBUFF                                                      
         ST    R6,ARCVHDR                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SVBLKSZ        TEST IF BLOCKED                              
         BZ    IBLKOK                                                           
         MVC   P1(24),Q1           SAVE PARM LIST FOR BLOCKED I/O               
         L     RF,SVRECDTF                                                      
*                                  AND CALL DADDS FOR BLKSIZE                   
         GOTO1 VDADDS,Q1,A(DARPT),,(R0),(RF)                                    
         ICM   R0,3,Q3+2                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STH   R0,BLKSTRK          SAVE BLOCKS PER TRACK                        
*                                                                               
         MVC   Q1(24),P1           RESET PARM LIST FOR BLOCKED I/O              
         B     IBLKOK                                                           
*                                                                               
IBLKNO   MVI   ERROR,IBLKERQ                                                    
         B     NO                                                               
IBLKOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS JOBS IN RECOVERY MODE                                       *         
***********************************************************************         
JOBRECV  NTR1                                                                   
         MVC   P(24),=C'RECOVER JOBS            '                               
         GOTO1 VPRINTER                                                         
         MVI   JOBFOUND,C'N'       INITIALISE RECOVERY JOB FOUND FLAG           
         MVI   RWRTPNDG,X'00'      CLEAR BLOCK WRITE PENDING                    
         MVI   FILEFLAG,0          CLEAR FILE FLAG                              
         MVI   ISRCVFLG,ISRCVOFF   AUTO IS RECOVERY FLAG                        
         MVC   RECDA,LASTRDA       READ FROM LAST RECORD ON FILE                
         BAS   RE,READDA                                                        
         BNE   JRECNO                                                           
         MVC   EOJDA,RECDA         INITIALISE END OF JOB DISK ADDRESS           
*                                                                               
JREC010  L     R6,ARCVHDR          POINT TO RECOVERY RECORD HEADER              
         LA    R3,RCVREC           POINT TO RECOVERY RECORD DATA                
         CLI   JOBFOUND,C'Y'       TEST JOB FOUND                               
         BE    JREC020                                                          
*                                                                               
*                                  HERE IF IN JOB-NOT-FOUND MODE                
*                                                                               
         CLC   0(8,R3),ENDJOB      TEST FOR JOB TRAILER TEXT                    
         BNE   JREC012                                                          
         BAS   RE,FILTJOB          CHECK JOBNAME AND JESID                      
         BNE   JREC012                                                          
*                                  END OF JOB TRAILER MATCHES FILTERS           
         BAS   RE,READBACK         READ BACK FOR NEXT RECOVERY RECORD           
         BNE   JRECNO                EXIT IF ERROR                              
         CLI   FILEFLAG,0          CHECK IF START OF FILE                       
         BNE   JREC032                                                          
         MVC   EOJDA,RECDA         SAVE THIS DISK ADDRESS TO MARK EOJ           
         B     JREC016                                                          
*                                                                               
JREC012  CLC   0(8,R3),STARTJOB    TEST FOR JOB HEADER TEXT                     
         BNE   JREC012A                                                         
         BAS   RE,FILTJOB          CHECK JOBNAME AND JESID                      
         BE    JREC014                                                          
         B     JREC012B              SKIP HEADER RECORD                         
JREC012A CLI   RTASKID,X'00'       OFFLINE RECOVERY RECORD ??                   
         BE    JREC030               YES - CONTINUE                             
         B     JREC012B              ELSE SKIP IT                               
*                                                                               
JREC012B BAS   RE,READBACK         READ BACK FOR NEXT RECOVERY RECORD           
         BNE   JRECNO                EXIT IF ERROR                              
         CLI   FILEFLAG,0          CHECK IF START OF FILE                       
         BNE   JREC032                                                          
         MVC   EOJDA,RECDA         SAVE THIS DISK ADDRESS TO MARK EOJ           
         B     JREC010                                                          
*                                  START OF JOB HEADER MATCHES FILTERS          
JREC014  CLC   RECDA,EOJDA         CHECK RECORDS IN JOB                         
         BE    JREC030                                                          
         MVC   P,SPACES                                                         
         MVC   RECDA,EOJDA         GET END OF JOB DISK ADDRESS                  
         BAS   RE,READDA           RE-READ LAST RECORD FOR JOB                  
         BNE   JRECNO                                                           
*                                                                               
JREC016  GOTO1 VPRINTER                                                         
         MVC   P(24),=C'START OF JOB            '                               
         GOTO1 VPRINTER                                                         
         MVI   JOBFOUND,C'Y'       SET JOB FOUND MODE                           
         B     JREC010               AND PROCESS THIS JOB                       
         EJECT                                                                  
*                                  HERE IF IN JOB-FOUND MODE                    
*                                                                               
JREC020  CLI   ISRCVFLG,ISRCVPDG   CHECK AUTO IS RECOVERY PENDING               
         BNE   JREC022                                                          
         BAS   RE,PISDA            PROCESS IS DA PAIR                           
         BNE   JRECNO                                                           
         MVI   ISRCVFLG,ISRCVOFF                                                
*                                                                               
JREC022  CLC   0(8,R3),ENDJOB      TEST FOR SPURIOUS JOB TRAILER TEXT           
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   0(8,R3),STARTJOB    TEST FOR JOB HEADER TEXT                     
         BE    JREC024               IF SO ASSUME END OF CURRENT JOB            
         CLI   RTASKID,X'FF'       DIE IF LOGICALLY DELETED RECORD ??           
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   RTASKID,X'00'       TEST IF OFFLINE RECOVERY RECORD              
         BE    JREC040             PROCESS RECOVERY RECORD                      
         DC    H'00'               DIE IF ONLINE RECOVERY RECORD                
*                                                                               
JREC024  MVC   P(24),=C'END OF JOB              '                               
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVI   JOBFOUND,C'N'       RESET JOB NOT FOUND FLAG                     
         BAS   RE,READBACK         READ BACK FOR NEXT RECOVERY RECORD           
         BNE   JRECNO                EXIT IF ERROR                              
         CLI   FILEFLAG,0          CHECK IF START OF FILE                       
         BNE   JREC032                                                          
         MVC   EOJDA,RECDA         SAVE THIS DISK ADDRESS TO MARK EOJ           
         B     JREC010                                                          
*                                                                               
JREC030  BAS   RE,READBACK         READ BACK FOR NEXT RECOVERY RECORD           
         BNE   JRECNO                EXIT IF ERROR                              
         CLI   FILEFLAG,0          CHECK IF START OF FILE                       
         BE    JREC010                                                          
*                                                                               
JREC032  MVC   P(24),=C'BOTTOM OF FILE FLAG     '                               
         GOTO1 VPRINTER                                                         
         CLI   ISRCVFLG,ISRCVPDG   CHECK AUTO IS RECOVERY PENDING               
         BNE   JRECOK                                                           
         BAS   RE,PISDA            PROCESS IS DA PAIR                           
         BNE   JRECNO                                                           
         MVI   ISRCVFLG,ISRCVOFF                                                
         TM    RWRTPNDG,X'01'      TEST IF BLOCK WRITE PENDING                  
         BNO   *+8                                                              
         BAS   RE,WRTRCV                                                        
         B     JRECOK                                                           
*                                                                               
*                                  HERE TO PROCESS JOB RECOVERY RECORD          
JREC040  EQU   *                                                                
         MVC   P(24),=C'NEXT RECOVERY RECORD    '                               
         GOTO1 VPRINTER                                                         
         BAS   RE,PROCRCV                                                       
         BNE   JRECNO                                                           
*                                  CONTINUE TO NEXT RECOVERY RECORD             
JREC100  EQU   *                                                                
         B     JREC300                                                          
*                                                                               
JREC300  CLI   ISRCVFLG,ISRCVON                                                 
         BE    JREC030                                                          
         LH    R1,RECLN                                                         
         LA    RE,RECDS                                                         
         CLI   TRECTY,COPY                                                      
         BE    JREC310                                                          
         L     RF,ARCVSAVE                                                      
         CLI   TRECTY,CHANGE                                                    
         BE    JREC312                                                          
         MVI   ISRCVFLG,ISRCVPDG                                                
         B     JREC312                                                          
JREC310  L     RF,ARCVCOPY                                                      
         MVI   ISRCVFLG,ISRCVPDG                                                
JREC312  MOVE  ((RF),(R1)),(RE)    SAVE THIS RECOVERY RECORD                    
         B     JREC030                                                          
*                                                                               
JRECNO   B     NO                                                               
JRECOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS JOB IN RERUN MODE                                           *         
***********************************************************************         
JOBRRUN  NTR1                                                                   
         MVC   P(24),=C'RERUN JOB               '                               
         GOTO1 VPRINTER                                                         
         MVI   JOBFOUND,C'N'       INITIALISE RECOVERY JOB FOUND FLAG           
         MVI   RWRTPNDG,X'00'      CLEAR BLOCK WRITE PENDING                    
         MVI   FILEFLAG,0          CLEAR END OF FILE FLAG                       
         MVI   ISRCVFLG,ISRCVOFF   AUTO IS RECOVERY FLAG                        
         MVC   RECDA,=X'00010101'  READ FROM FIRST RECORD ON FILE               
         BAS   RE,READDA                                                        
         BNE   JRECNO                                                           
*                                                                               
JRUN010  L     R6,ARCVHDR          POINT TO RECOVERY RECORD HEADER              
         LA    R3,RCVREC           POINT TO RECOVERY RECORD DATA                
         CLI   JOBFOUND,C'Y'       TEST IF JOB FOUND MODE                       
         BE    JRUN020                                                          
*                                                                               
*                                  HERE IF IN JOB-NOT-FOUND MODE                
*                                                                               
         CLC   0(8,R3),STARTJOB    TEST FOR JOB HEADER TEXT                     
         BNE   JRUN030               IF FOUND                                   
         BAS   RE,FILTJOB            CHECK JOBNAME AND JESID                    
         BNE   JRUN030                 IF MATCH                                 
         MVI   JOBFOUND,C'Y'           SET JOB FOUND MODE                       
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'START OF JOB            '                               
         GOTO1 VPRINTER                                                         
         B     JRUN030             GET NEXT RECOVERY RECORD                     
         EJECT                                                                  
*                                                                               
*                                  HERE IF IN JOB-FOUND MODE                    
*                                                                               
JRUN020  CLI   ISRCVFLG,ISRCVPDG   CHECK AUTO IS RECOVERY PENDING               
         BNE   JRUN021                                                          
         BAS   RE,PISDA            PROCESS IS DA PAIR                           
         BNE   JRUNNO                                                           
         MVI   ISRCVFLG,ISRCVOFF                                                
*                                                                               
JRUN021  CLC   0(8,R3),ENDJOB      CHECK FOR JOB TRAILER RECORDS                
         BNE   JRUN022                                                          
         BAS   RE,FILTJOB          CHECK JOBNAME AND JOBID MATCH                
         BE    *+6                                                              
         DC    H'00'                                                            
         B     JRUN024                                                          
*                                                                               
JRUN022  CLC   0(8,R3),STARTJOB    TEST FOR JOB HEADER TEXT                     
         BE    JRUN024               IF SO ASSUME END OF CURRENT JOB            
         CLI   RTASKID,X'FF'       LOGICALLY DELETED RECORD ??                  
         BNE   JRUN023                                                          
         TM    MODE,RRUNQ          OK IF RERUN MODE ONLY                        
         BO    JRUN040                                                          
         DC    H'00'                                                            
JRUN023  CLI   RTASKID,X'00'       OFFLINE RECOVERY RECORD ??                   
         BE    JRUN040               PROCESS RECOVERY RECORD                    
         B     JRUN024               END OF JOB IF FACPAK TASK RECOVERY         
*                                                                               
JRUN024  MVC   P(24),=C'END OF JOB              '                               
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVI   JOBFOUND,C'N'       RESET JOB NOT FOUND FLAG                     
         B     JRUN010                                                          
*                                                                               
JRUN030  BAS   RE,READFWD          READ FORWARD NEXT RECOVERY RECORD            
         BNE   JRUNNO                EXIT IF ERROR                              
         CLI   FILEFLAG,0          CHECK IF END OF FILE                         
         BE    JRUN010                                                          
         MVC   P(24),=C'END OF FILE FLAG        '                               
         GOTO1 VPRINTER                                                         
         CLI   ISRCVFLG,ISRCVPDG   CHECK AUTO IS RECOVERY PENDING               
         BNE   JRUNOK                                                           
         BAS   RE,PISDA            PROCESS IS DA PAIR                           
         BNE   JRUNNO                                                           
         MVI   ISRCVFLG,ISRCVOFF                                                
         TM    RWRTPNDG,X'01'      TEST IF BLOCK WRITE PENDING                  
         BNO   *+8                                                              
         BAS   RE,WRTRCV                                                        
         B     JRUNOK                                                           
*                                                                               
*                                  HERE TO PROCESS JOB RECOVERY RECORD          
JRUN040  EQU   *                                                                
         MVC   P(24),=C'NEXT RECOVERY RECORD    '                               
         GOTO1 VPRINTER                                                         
         BAS   RE,PROCRCV                                                       
         BNE   JRUNNO                                                           
*                                  CONTINUE TO NEXT RECOVERY RECORD             
JRUN100  EQU   *                                                                
         B     JRUN300                                                          
*                                                                               
JRUN300  CLI   ISRCVFLG,ISRCVON                                                 
         BE    JRUN030                                                          
         LH    R1,RECLN                                                         
         LA    RE,RECDS                                                         
         CLI   TRECTY,COPY                                                      
         BE    JRUN310                                                          
         L     RF,ARCVSAVE                                                      
         MVI   ISRCVFLG,ISRCVPDG                                                
         B     *+8                                                              
JRUN310  L     RF,ARCVCOPY                                                      
         MOVE  ((RF),(R1)),(RE)    SAVE THIS RECOVERY RECORD                    
         B     JRUN030                                                          
*                                                                               
JRUNNO   B     NO                                                               
JRUNOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY FILE RECORD                                        *         
***********************************************************************         
PROCRCV  NTR1                                                                   
         MVC   TRECTY,RRECTY       DON'T CHANGE IN RECORD                       
         NI    TRECTY,X'7F'        TURN OFF 'POINTER' BIT                       
         TM    MODE,RCOVQ          TEST RECOIL MODE                             
         BZ    PRCV010                                                          
         CLI   TRECTY,CHANGE       IF RECOVERY MODE THEN                        
         BE    PRCVDEL               DELETE CHANGES FROM RECOVERY FILE          
         B     PRCV020                                                          
*                                                                               
PRCV010  TM    MODE,RRUNQ                                                       
         BO    *+6                                                              
         DC    H'00'                                                            
         CLI   TRECTY,COPY         IF RERUN MODE THEN                           
         BE    PRCVUND               UNDELETE COPIES IN RECOVERY FILE           
         B     PRCV020                                                          
*                                                                               
PRCV020  L     RF,SVSYSFL          SEARCH FOR FILE IN SYSFLES LIST              
         SR    R1,R1                                                            
         IC    R1,3(RF)                                                         
         LA    RF,4(RF)                                                         
PRCV030  CLC   RFILTY,3(RF)        MATCH FILE NUMBER                            
         BE    PRCV040                                                          
         LA    RF,8(RF)                                                         
         BCT   R1,PRCV030                                                       
         DC    H'00'                                                            
*                                                                               
PRCV040  L     R0,4(RF)            GET DTF ADDRESS                              
         LA    R2,DMCB             SET A(DMOD000 DMCB PARAMS)                   
         XC    DMCB(24),DMCB                                                    
         TM    0(RF),X'01'         TEST FILE TYPE                               
         BO    PRCVIS              IS FILE                                      
         B     PRCVDA              DA FILE                                      
*                                                                               
PRCVDEL  EQU   *                   DELETE THIS RECOVERY RECORD                  
         MVI   RTASKID,X'FF'                                                    
         OI    RWRTPNDG,X'01'      SET BLOCK WRITE PENDING                      
         B     PRCVOK                                                           
*                                                                               
PRCVUND  EQU   *                   UNDELETE THIS RECOVERY RECORD                
         MVI   RTASKID,X'00'                                                    
         OI    RWRTPNDG,X'01'      SET BLOCK WRITE PENDING                      
         B     PRCVOK                                                           
*                                                                               
PRCVER1  MVI   ERROR,PRCVER1Q      PRCV ERROR 1                                 
         B     PRCVNO                                                           
PRCVER2  MVI   ERROR,PRCVER2Q      PRCV ERROR 2                                 
         B     PRCVNO                                                           
PRCVER3  MVI   ERROR,PRCVER3Q      PRCV ERROR 3                                 
         B     PRCVNO                                                           
PRCVER4  MVI   ERROR,PRCVER4Q      PRCV ERROR 4                                 
         B     PRCVNO                                                           
PRCVER5  MVI   ERROR,PRCVER5Q      PRCV ERROR 5                                 
         B     PRCVNO                                                           
PRCVER6  MVI   ERROR,PRCVER6Q      PRCV ERROR 6                                 
         B     PRCVNO                                                           
PRCVER7  MVI   ERROR,PRCVER7Q      PRCV ERROR 7                                 
         B     PRCVNO                                                           
PRCVER8  MVI   ERROR,PRCVER8Q      PRCV ERROR 8                                 
         B     PRCVNO                                                           
PRCVER9  MVI   ERROR,PRCVER9Q      PRCV ERROR 9                                 
         B     PRCVNO                                                           
PRCVERA  MVI   ERROR,PRCVERAQ      PRCV ERROR A                                 
         B     PRCVNO                                                           
PRCVERB  MVI   ERROR,PRCVERBQ      PRCV ERROR B                                 
         B     PRCVNO                                                           
PRCVERC  MVI   ERROR,PRCVERCQ      PRCV ERROR C                                 
         B     PRCVNO                                                           
*                                                                               
PRCVNO   B     NO                                                               
PRCVOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DIRECT ACCESS FILE                                                  *         
***********************************************************************         
PRCVDA   EQU   *                   READ DA FILE RECORD INTO INPUT AREA          
         L     R5,AINPUT                                                        
         LA    RF,RVCHR                                                         
         GOTO1 VDMOD000,P1,A(DMODREAD),(R5),,(R0),(RF)                          
         TM    MODE,RRUNQ          TEST RECOVERY/RERUN MODE                     
         BO    PRCVDARR                                                         
         B     PRCVDARC                                                         
*                                                                               
PRCVDARC EQU   *                   DA FILE IN RECOVERY MODE                     
         CLI   DMCB+8,0                                                         
         BE    PRDA010                                                          
         TM    DMCB+8,X'90'                                                     
         BZ    PRCVER1             DISK READ ERROR DA FILE                      
         CLI   TRECTY,ADD                                                       
         BE    PRCVDEL             EOF AND NOTFOUND OK FOR ADD                  
         B     PRCVER2                                                          
*                                                                               
PRDA010  CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   PRDA020                                                          
         LA    R0,RCVREC                                                        
         GOTO1 VDMOD000,P1,A(DMODWRI),(R0),,,,                                  
         B     PRCVDEL                                                          
*                                                                               
PRDA020  CLI   TRECTY,ADD          LOGICALLY DELETE DA FILE ADDED REC           
         BNE   PRDA030                                                          
         BAS   RE,LDELETE                                                       
         GOTO1 VDMOD000,P1,A(DMODWRI),,,,,                                      
         B     PRCVDEL                                                          
*                                                                               
PRDA030  B     PRCVDEL             ERASE INVALID RCVR REC                       
*                                                                               
*                                  DA FILE IN RERUN MODE                        
*                                                                               
PRCVDARR EQU   *                                                                
         CLI   DMCB+8,0                                                         
         BE    PRDA110                                                          
         TM    DMCB+8,X'90'                                                     
         BZ    PRCVER3             DISK READ ERROR DA FILE                      
         B     PRCVER4             RECORD NOT FOUND                             
*                                                                               
PRDA110  CLI   TRECTY,ADD          HERE IF RECORD FOUND OK                      
         BE    PRDA120                                                          
         CLI   TRECTY,CHANGE       OVERWRITE WITH RECOVERY CHANGE REC.          
         BNE   PRCVER5                                                          
         LA    R0,RCVREC                                                        
         GOTO1 VDMOD000,P1,A(DMODWRI),(R0),,,,                                  
         B     PRCVUND                                                          
*                                                                               
PRDA120  CLI   TRECTY,ADD          ADD DA FILE RECORD                           
         BNE   PRCVER6                                                          
         BAS   RE,LUNDEL                                                        
         GOTO1 VDMOD000,P1,A(DMODWRI),,,,,                                      
         B     PRCVUND                                                          
         EJECT                                                                  
***********************************************************************         
* INDEX SEQUENTIAL FILE                                               *         
***********************************************************************         
PRCVIS   EQU   *                   READ IS FILE RECORD INTO INPUT AREA          
         MVI   ISRCVFLG,ISRCVON    FLAG IS FILE RECOVERY ON                     
         L     R5,AINPUT           KEY IS IN RECOVERY RECORD                    
         LA    RF,RCVREC           READ IS FILE RECORD INTO INPUT AREA          
         ST    RF,DMCB+8                                                        
         GOTO1 VDMOD000,P1,A(DMODRKEY),(R5),,(R0),(RF)                          
         TM    MODE,RRUNQ          TEST RECOVERY/RERUN MODE                     
         BO    PRCVISRR                                                         
         B     PRCVISRC                                                         
*                                                                               
PRCVISRC EQU   *                   IS FILE IN RECOVERY MODE                     
         CLI   DMCB+8,0                                                         
         BNE   PRCVER7             DISK READ ERROR IS FILE                      
         L     RE,P1+12                                                         
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC     COMPARE KEYS                                  
         BE    PRIS010                                                          
         CLI   TRECTY,ADD                                                       
         BE    PRCVDEL             NOTFOUND OK FOR ADD                          
         B     PRCVER8                                                          
*                                                                               
PRIS010  CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   PRIS020                                                          
         GOTO1 VDMOD000,P1,A(DMODWKEY),RCVREC,,,,                               
         B     PRCVDEL                                                          
*                                                                               
PRIS020  CLI   TRECTY,ADD          ERASE IS FILE ADDED RECORD                   
         BNE   PRIS030                                                          
         GOTO1 VDMOD000,P1,A(DMODEKEY),,,,,                                     
         B     PRCVDEL                                                          
*                                                                               
PRIS030  B     PRCVDEL             ERASE INVALID RCVR REC                       
*                                                                               
*                                  IS FILE IN RERUN MODE                        
*                                                                               
PRCVISRR EQU   *                                                                
         CLI   DMCB+8,0                                                         
         BNE   PRCVER9             DISK READ ERROR IS FILE                      
         L     RE,P1+12                                                         
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC      COMPARE KEYS                                 
         BE    PRIS110                                                          
         CLI   TRECTY,ADD                                                       
         BE    PRIS120             NOT FOUND OK FOR ADD                         
         B     PRCVERA                                                          
*                                                                               
PRIS110  CLI   TRECTY,ADD          HERE IF RECORD FOUND                         
         BE    PRCVERB               ERROR IF ADD                               
         CLI   TRECTY,CHANGE       OVERWRITE WITH RECOVERY CHANGE REC.          
         BNE   PRCVERC                                                          
         GOTO1 VDMOD000,P1,A(DMODWKEY),RCVREC,,,,                               
         B     PRCVUND                                                          
*                                                                               
PRIS120  CLI   TRECTY,ADD          ADD IS FILE ADDED RECORD                     
         BNE   PRIS130                                                          
         LA    R5,RCVREC                                                        
         GOTO1 VDMOD000,P1,A(DMODAKEY),(R5),,,,                                 
         B     PRCVUND                                                          
*                                                                               
PRIS130  B     PRCVUND             ERASE INVALID RCVR REC                       
         EJECT                                                                  
***********************************************************************         
* SEARCH FILE TABLE TO FIND LOGICAL DELETE FIELD AND SET TO FF'S      *         
***********************************************************************         
LDELETE  EQU   *                                                                
         L     R3,VDMGRFLS         POINT TO DMGRFLES TABLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
*                                                                               
LDEL2    CLI   0(R3),0             SEARCH FILE TABLE FOR FILE                   
         BE    *+14                                                             
         CLC   RFILTY,8(R3)                                                     
         BE    LDEL4                                                            
         BXLE  R3,R4,LDEL2                                                      
         B     LDEL6                                                            
*                                                                               
LDEL4    SR    R5,R5                                                            
         IC    R5,9(R3)            R5=L'LOGICAL DELETE FIELD                    
         LTR   R5,R5                                                            
         BZ    LDEL6                                                            
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         IC    R4,10(R3)                                                        
         A     R4,AINPUT                                                        
*                                  R4=A(LOGICAL DELETE FIELD)                   
         EX    R5,*+8                                                           
         B     LDELX                                                            
         MVC   0(0,R4),=8X'FF'     SET FIELD TO ALL FF'S                        
*                                                                               
LDEL6    B     PRCVDEL             ERASE INVALID RCVR REC                       
*                                                                               
LDELX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SEARCH FILE TABLE TO FIND LOGICAL DELETE FIELD AND SET TO 00'S      *         
***********************************************************************         
LUNDEL   EQU   *                                                                
         L     R3,VDMGRFLS         POINT TO DMGRFLES TABLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
*                                                                               
LUND2    CLI   0(R3),0             SEARCH FILE TABLE FOR FILE                   
         BE    *+14                                                             
         CLC   RFILTY,8(R3)                                                     
         BE    LUND4                                                            
         BXLE  R3,R4,LUND2                                                      
         B     LUND6                                                            
*                                                                               
LUND4    SR    R5,R5                                                            
         IC    R5,9(R3)            R5=L'LOGICAL DELETE FIELD                    
         LTR   R5,R5                                                            
         BZ    LUND6                                                            
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         IC    R4,10(R3)                                                        
         A     R4,AINPUT                                                        
*                                  R4=A(LOGICAL DELETE FIELD)                   
         EX    R5,*+8                                                           
         B     LUNDX                                                            
         MVC   0(0,R4),=8X'00'     SET FIELD TO ALL 00'S                        
*                                                                               
LUND6    B     PRCVUND             ERASE INVALID RCVR REC                       
*                                                                               
LUNDX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PREVIOUS DA RECORD FROM RECOVERY FILE SAVED IN BUFFER       *         
***********************************************************************         
PISDA    NTR1                                                                   
         L     R6,ARCVSAVE                                                      
         L     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
         ZIC   RF,RFILTY                                                        
         SLL   RF,5                                                             
         AR    R3,RF                                                            
         TM    DMFLTYP,DMFLIS                                                   
         BO    PISDOK                                                           
         TM    DMFLSTYP,DMFLDAL                                                 
         BZ    PISDOK                                                           
         MVC   TRECTY,RRECTY       DON'T CHANGE IN RECORD                       
         NI    TRECTY,X'7F'        TURN OFF 'POINTER' BIT                       
         CLI   TRECTY,ADD                                                       
         BE    PISDADD                                                          
         B     PISDCHG                                                          
*                                                                               
*                                  PROCESS CHANGE TYPE RECOVERY RECORD          
*                                                                               
PISDCHG  EQU   *                                                                
         L     R4,ARCVCOPY                                                      
         ZIC   RF,DMFLKEYL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RCVREC(0),RCVREC-RECDS(R4)                                       
         BE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,DMFLDAD                                                       
         ZIC   RF,DMFLCTRL                                                      
         SR    RE,RF                                                            
         TM    MODE,RRUNQ          TEST RECOVERY/RERUN MODE                     
         BO    *+16                                                             
         LA    R1,RCVREC-RECDS(RE,R4)                                           
         LA    RE,RCVREC-RECDS(RE,R6)                                           
         B     *+12                                                             
         LA    R1,RCVREC-RECDS(RE,R6)                                           
         LA    RE,RCVREC-RECDS(RE,R4)                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(R1)                                                    
         BE    PISDOK                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R1)                                                    
*                                  READ IS FILE RECORD INTO INPUT AREA          
         ZIC   RF,DMFLPAIR                                                      
         L     R3,AFILTAB                                                       
         SLL   RF,5                                                             
         AR    R3,RF                                                            
         TM    DMFLTYP,DMFLIS                                                   
         BO    *+6                                                              
         DC    H'00'                                                            
*                                  READ IS FILE RECORD INTO INPUT AREA          
         L     RF,SVSYSFL          SEARCH FOR FILE IN SYSFLES LIST              
         SR    R1,R1                                                            
         IC    R1,3(RF)                                                         
         LA    RF,4(RF)                                                         
PISC010  CLC   DMFLNUM,3(RF)       MATCH FILE NUMBER                            
         BE    PISC020                                                          
         LA    RF,8(RF)                                                         
         BCT   R1,PISC010                                                       
         DC    H'00'                                                            
*                                                                               
PISC020  LR    R6,R4               POINT TO RECOVERY COPY RECORD                
         L     R0,4(RF)            GET DTF ADDRESS                              
         LA    R2,DMCB             SET A(DMOD000 DMCB PARAMS)                   
         XC    DMCB(24),DMCB                                                    
         L     R5,AINPUT           KEY IS IN RECOVERY RECORD                    
         LA    RF,RCVREC           READ IS FILE RECORD INTO INPUT AREA          
         ST    RF,DMCB+8                                                        
         GOTO1 VDMOD000,P1,A(DMODRKEY),(R5),,(R0),(RF)                          
         CLI   DMCB+8,0                                                         
         BNE   PISCER1             DISK READ ERROR IS FILE                      
         L     RE,P1+12                                                         
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC      COMPARE KEYS                                 
         BNE   PISCER2                                                          
*                                                                               
PISC030  EQU   *                   WRITE IS FILE COPY RECORD                    
         ZIC   RE,DMFLDAD                                                       
         ZIC   RF,DMFLCTRL                                                      
         SR    RE,RF                                                            
         LA    R1,0(RE,R5)                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),WORK                                                     
         GOTO1 VDMOD000,P1,A(DMODWKEY),,,,,                                     
         B     PISDOK                                                           
         EJECT                                                                  
PISDADD  EQU   *                   PROCESS ADD TYPE RECOVERY RECORD             
         ZIC   RF,DMFLPAIR                                                      
         L     R3,AFILTAB                                                       
         SLL   RF,5                                                             
         AR    R3,RF                                                            
         TM    DMFLTYP,DMFLIS                                                   
         BO    *+6                                                              
         DC    H'00'                                                            
*                                  READ IS FILE RECORD INTO INPUT AREA          
         L     RF,SVSYSFL          SEARCH FOR FILE IN SYSFLES LIST              
         SR    R1,R1                                                            
         IC    R1,3(RF)                                                         
         LA    RF,4(RF)                                                         
PISA010  CLC   DMFLNUM,3(RF)       MATCH FILE NUMBER                            
         BE    PISA020                                                          
         LA    RF,8(RF)                                                         
         BCT   R1,PISA010                                                       
         DC    H'00'                                                            
*                                                                               
PISA020  EQU   *                                                                
         L     R0,4(RF)            GET DTF ADDRESS                              
         LA    R2,DMCB             SET A(DMOD000 DMCB PARAMS)                   
         XC    DMCB(24),DMCB                                                    
         L     R5,AINPUT           KEY IS IN RECOVERY RECORD                    
         LA    RF,RCVREC           READ IS FILE RECORD INTO INPUT AREA          
         ST    RF,DMCB+8                                                        
         GOTO1 VDMOD000,P1,A(DMODRKEY),(R5),,(R0),(RF)                          
         TM    MODE,RRUNQ          TEST RECOVERY/RERUN MODE                     
         BO    PISARR                                                           
         B     PISARC                                                           
*                                                                               
*                                  HERE IF RECOVERY MODE                        
*                                                                               
PISARC   CLI   DMCB+8,0                                                         
         BNE   PISAER1             DISK READ ERROR IS FILE                      
         L     RE,P1+12                                                         
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC      COMPARE KEYS                                 
         BE    PISA030                                                          
         B     PISDOK              NOT FOUND EXIT OK                            
*                                                                               
PISA030  EQU   *                   ERASE IS FILE FOR ADDED RECORD               
         GOTO1 VDMOD000,P1,A(DMODEKEY),,,,,                                     
         B     PISDOK                                                           
*                                                                               
*                                  HERE IF RERUN MODE                           
*                                                                               
PISARR   CLI   DMCB+8,0                                                         
         BNE   PISAER2             DISK READ ERROR IS FILE                      
         L     RE,P1+12                                                         
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC      COMPARE KEYS                                 
         BE    PISAER3             ERROR IF FOUND                               
*                                                                               
PISA130  EQU   *                   ADD IS FILE FOR ADDED RECORD                 
         LA    R5,RCVREC           SET KEY FROM DA RECORD                       
         ZIC   RF,DMFLDAD          FIND OFFSET TO DISK ADDRESS                  
         AR    RF,R5                                                            
         MVC   0(4,RF),RVCHR       SET DISK ADDRESS OF DA RECORD                
         GOTO1 VDMOD000,P1,A(DMODAKEY),(R5),,,,                                 
         B     PISDOK                                                           
*                                                                               
PISCER1  MVI   ERROR,PISCER1Q                                                   
         B     PISDNO                                                           
PISCER2  MVI   ERROR,PISCER2Q                                                   
         B     PISDNO                                                           
PISAER1  MVI   ERROR,PISAER1Q                                                   
         B     PISDNO                                                           
PISAER2  MVI   ERROR,PISAER2Q                                                   
         B     PISDNO                                                           
PISAER3  MVI   ERROR,PISAER3Q                                                   
         B     PISDNO                                                           
*                                                                               
PISDNO   B     NO                                                               
PISDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ RECOVERY FILE RECORD AT DISK ADDRESS DEFINED IN RECDA          *         
* R6 = A(RECOVERY RECORD HEADER) = ARCVHDR                            *         
***********************************************************************         
READDA   NTR1                                                                   
*                                                                               
         L     R6,ARECBUFF         POINT TO START OF BLOCK BUFFER               
         L     R0,SVRECDTF         GET A(DTF)                                   
         OC    SVBLKSZ,SVBLKSZ     TEST IF BLOCKED                              
         BZ    *+14                                                             
         CLC   RECDA(3),0(R6)      IF SO CHECK IF BLOCK IN BUFFER               
         BE    RDDA010                                                          
*                                  READ RECORD/BLOCK FROM DISK                  
         MVC   BYTE,RECDA+3        SAVE BLOCK RECORD COUNTER                    
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R6),,(R0),(RF)                          
         OC    Q3(2),Q3                                                         
         BNZ   RDDAER1             DISK READ ERROR                              
         OC    SVBLKSZ,SVBLKSZ     EXIT IF NOT BLOCKED                          
         BZ    RDDA030                                                          
         CLC   RECDA(3),0(R6)      CHECK DA IN BLOCK OK                         
         BNE   RDDAER1             GET HIGH RECORD NUM IN BLOCK                 
         MVC   BRECNUM,3(R6)       SAVE NUMBER OF RECORDS IN BLOCK              
         LR    R1,R6                                                            
         AH    R1,4(R6)                                                         
         ST    R1,BLOCKEND         SAVE END OF BLOCK DISK ADDRESS               
         MVC   RECDA+3(1),BYTE     RESTORE RECORD IN DISK ADDRESS               
*                                                                               
*                                  FIND RECORD IN BLOCK                         
RDDA010  LA    R6,6(R6)            FIRST RECORD IN BLOCK                        
         SR    R2,R2               SET RECORD COUNT                             
         ICM   R2,1,RECDA+3                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
RDDA020  C     R6,BLOCKEND         TEST END OF BLOCK                            
         BNE   *+6                                                              
         DC    H'00'                                                            
         BCT   R2,*+8              DECREMENT RECORD COUNT                       
         B     RDDA030                                                          
         SR    R0,R0                                                            
         ICM   R0,3,0(R6)                                                       
         AR    R6,R0                                                            
         B     RDDA020                                                          
*                                                                               
RDDA030  ST    R6,ARCVHDR                                                       
         B     RDDAOK                                                           
*                                                                               
RDDAER1  MVI   ERROR,RDDAER1Q      DISK READ ERROR                              
         B     RDDANO                                                           
*                                                                               
RDDANO   B     NO                                                               
RDDAOK   B     YES                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* READ FORWARD FOR NEXT RECORD FROM RECOVERY FILE                     *         
***********************************************************************         
READFWD  NTR1                                                                   
         CLI   INTAPE,C'Y'         READ RECORD FROM TAPE                        
         BNE   RFWD010                                                          
         L     R6,ARCVHDR                                                       
         LR    R0,R6                                                            
         GET   RCVTAPE,(0)                                                      
         B     RFWDOK                                                           
*                                  READ RECORD FROM SYSTEM DISK FILE            
RFWD010  OC    SVBLKSZ,SVBLKSZ     TEST IF BLOCKED                              
         BZ    RFWD020                                                          
         BAS   RE,RFWDBLK          READ FORWARD BLOCKED RECOVERY FILE           
         BNE   RFWDNO                ERROR EXIT                                 
         B     RFWDOK              RECORD FOUND OR END OF FILE                  
*                                                                               
RFWD020  BAS   RE,RFWDUNB          READ FORWARD UNBLOCKED RECOVERY FILE         
         BNE   RFWDNO                ERROR EXIT                                 
         B     RFWDOK              RECORD FOUND OR END OF FILE                  
*                                                                               
RFWDNO   B     NO                                                               
RFWDOK   B     YES                                                              
*                                                                               
EODADRCV MVI   FILEFLAG,1          FLAG END OF TAPE FILE                        
         B     YES                   AND EXIT                                   
         EJECT                                                                  
***********************************************************************         
* READ FORWARD NEXT RECORD FROM BLOCKED RECOVERY FILE                 *         
* R6 POINTS TO LAST RECORD HEADER                                     *         
***********************************************************************         
RFWDBLK  NTR1                                                                   
         L     R6,ARCVHDR                                                       
         AH    R6,RECLN            POINT TO NEXT RECORD IN BLOCK BUFFER         
         C     R6,BLOCKEND         CHECK IF END OF BLOCK                        
         BNL   RFBL010                                                          
         ZIC   RF,RECDA+3          BUMP RECORD NUMBER IN DA                     
         LA    RF,1(RF)                                                         
         STC   RF,RECDA+3                                                       
         B     RFBLOK                                                           
*                                  START OF, BLOCK REACHED                      
RFBL010  CLC   BRECNUM,RECDA+3     CHECK RECORD COUNT MATCHES BLOCK MAX         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    RWRTPNDG,X'01'      TEST IF BLOCK WRITE PENDING                  
         BNO   *+8                                                              
*                                  WRITE BACK UPDATED RECOVERY BLOCK            
         BAS   RE,WRTRCV                                                        
*                                                                               
         CLC   RECDA(3),LASTRDA    TEST IF LAST RECORD ON FILE                  
         BNE   *+12                                                             
         MVI   FILEFLAG,1          FLAG END OF FILE                             
         B     RFBLOK                                                           
         CLC   RECDA+2(1),BLKSTRK+1  TEST IF LAST BLOCK ON TRACK                
         BE    RFBLNEXT                IF SO BUMP TO NEXT TRACK                 
         SR    R1,R1                                                            
         IC    R1,RECDA+2          BUMP BLOCK NUMBER IN DISK ADDRESS            
         LA    R1,1(R1)                                                         
         STC   R1,RECDA+2                                                       
         B     RFBLREAD            READ NEXT BLOCK                              
*                                                                               
RFBLNEXT SR    R1,R1               BUMP TRACK NUMBER IN DISK ADDRESS            
         ICM   R1,3,RECDA                                                       
         LA    R1,1(R1)                                                         
         XC    RECDA,RECDA                                                      
         STCM  R1,3,RECDA                                                       
         MVI   RECDA+2,X'01'       RESET BLOCK NUMBER                           
*                                                                               
RFBLREAD EQU   *                   READ NEXT RECOVERY FILE BLOCK                
         L     R6,ARECBUFF         POINT TO START OF BLOCK BUFFER               
         L     R0,SVRECDTF         GET A(DTF)                                   
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R6),,(R0),(RF)                          
         OC    Q3(2),Q3                                                         
         BNZ   RFBLER1             DISK READ ERROR                              
         CLC   RECDA(3),0(R6)      CHECK DA IN BLOCK OK                         
         BNE   RFBLER1             GET HIGH RECORD NUM IN BLOCK                 
         MVC   BRECNUM,3(R6)       SAVE NUMBER RECORDS IN BLOCK                 
         LR    R1,R6                                                            
         AH    R1,4(R6)                                                         
         ST    R1,BLOCKEND         SAVE END OF BLOCK DISK ADDRESS               
         LA    R6,6(R6)            POINT TO FIRST RECOVERY HEADER               
         MVI   RECDA+3,X'01'       SET DA TO FIRST RECORD IN BLOCK              
         B     RFBLOK                                                           
*                                                                               
RFBLER1  MVI   ERROR,RFBLER1Q      DISK READ ERROR                              
         B     RFBLNO                                                           
*                                                                               
RFBLNO   B     NO                                                               
RFBLOK   EQU   *                                                                
         ST    R6,ARCVHDR          UPDATE NEXT RECOVERY HEADER ADDRESS          
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ FORWARD NEXT RECORD FROM UNBLOCKED RECOVERY FILE              *          
***********************************************************************         
RFWDUNB  NTR1                                                                   
         L     R6,ARCVHDR                                                       
*                                                                               
         TM    RWRTPNDG,X'01'      TEST IF BLOCK WRITE PENDING                  
         BNO   *+8                                                              
*                                  WRITE BACK UPDATED RECOVERY BLOCK            
         BAS   RE,WRTRCV                                                        
*                                                                               
         CLC   RECDA(3),LASTRDA    TEST IF LAST RECORD ON FILE                  
         BNE   *+12                                                             
         MVI   FILEFLAG,1          FLAG END OF FILE                             
         B     RFUBOK                                                           
         CLC   RECDA+2(1),BLKSTRK+1  TEST IF LAST BLOCK ON TRACK                
         BE    RFUBNEXT                IF SO BUMP TO NEXT TRACK                 
         SR    R1,R1                                                            
         IC    R1,RECDA+2          BUMP BLOCK NUMBER IN DISK ADDRESS            
         LA    R1,1(R1)                                                         
         STC   R1,RECDA+2                                                       
         B     RFUBREAD            READ NEXT BLOCK                              
*                                                                               
RFUBNEXT SR    R1,R1               BUMP TRACK NUMBER IN DISK ADDRESS            
         ICM   R1,3,RECDA                                                       
         LA    R1,1(R1)                                                         
         XC    RECDA,RECDA                                                      
         STCM  R1,3,RECDA                                                       
         MVI   RECDA+2,X'01'       RESET BLOCK NUMBER                           
*                                                                               
RFUBREAD EQU   *                   READ NEXT RECOVERY FILE BLOCK                
         L     R6,ARECBUFF         POINT TO START OF BLOCK BUFFER               
         L     R0,SVRECDTF         GET A(DTF)                                   
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R6),,(R0),(RF)                          
         OC    Q3(2),Q3                                                         
         BNZ   RFUBER1             DISK READ ERROR                              
         CLC   RECDA(3),0(R6)      CHECK DA IN BLOCK OK                         
         BNE   RFUBER1             GET HIGH RECORD NUM IN BLOCK                 
         B     RFUBOK                                                           
*                                                                               
RFUBER1  MVI   ERROR,RFUBER1Q      DISK READ ERROR                              
         B     RFUBNO                                                           
*                                                                               
RFUBNO   B     NO                                                               
RFUBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ BACKWARD FOR NEXT RECORD FROM RECOVERY FILE                    *         
***********************************************************************         
READBACK NTR1                                                                   
         CLI   INTAPE,C'Y'         READ RECORD FROM TAPE                        
         BNE   RBAC010                                                          
         L     R6,ARCVHDR                                                       
         LR    R0,R6                                                            
         GET   RCVTAPE,(0)                                                      
         B     RBACOK                                                           
*                                  READ RECORD FROM SYSTEM DISK FILE            
RBAC010  OC    SVBLKSZ,SVBLKSZ     TEST IF BLOCKED                              
         BZ    RBAC020                                                          
         BAS   RE,RBACBLK          READ FORWARD BLOCKED RECOVERY FILE           
         BNE   RBACNO                ERROR EXIT                                 
         B     RBACOK              RECORD FOUND OR END OF FILE                  
*                                                                               
RBAC020  BAS   RE,RBACUNB          READ FORWARD UNBLOCKED RECOVERY FILE         
         BNE   RBACNO                ERROR EXIT                                 
         B     RBACOK              RECORD FOUND OR END OF FILE                  
*                                                                               
RBACNO   B     NO                                                               
RBACOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ BACKWARD FOR NEXT RECORD FROM BLOCKED RECOVERY FILE            *         
* R6 POINTS TO LAST RECORD HEADER                                     *         
***********************************************************************         
RBACBLK  NTR1                                                                   
         L     R6,ARCVHDR                                                       
         SR    R1,R1                                                            
         IC    R1,RECDA+3          DECREMENT RECORD COUNTER                     
         SH    R1,=H'1'                                                         
         BNP   RBBL010                                                          
         STC   R1,RECDA+3                                                       
         CLI   RECDA+3,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         BAS   RE,READDA           GET PREVIOUS RECORD IN BLOCK                 
         BNE   RBBLNO                                                           
         B     RBBLOK                AND EXIT OK                                
*                                                                               
RBBL010  EQU   *                   HERE AT START OF BLOCK                       
         TM    RWRTPNDG,X'01'      TEST IF BLOCK WRITE PENDING                  
         BNO   *+8                                                              
*                                  WRITE BACK UPDATED RECOVERY BLOCK            
         BAS   RE,WRTRCV                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,RECDA+2          BACK TO PREVIOUS BLOCK ON TRACK              
         SH    R1,=H'1'                                                         
         BNP   RBBL020             NO MORE BLOCKS ON THIS TRACK                 
         STC   R1,RECDA+2                                                       
         B     RBBLREAD            READ LAST BLOCK                              
*                                                                               
RBBL020  SR    R1,R1               BACK TO LAST RECORD ON PREV. TRACK           
         ICM   R1,3,RECDA                                                       
         SH    R1,=H'1'                                                         
         BP    *+12                                                             
         MVI   FILEFLAG,1          FLAG START OF FILE                           
         B     RBBLOK                                                           
         XC    RECDA,RECDA                                                      
         STCM  R1,3,RECDA                                                       
         MVC   RECDA+2(1),BLKSTRK+1   RESET BLOCK NUMBER                        
*                                                                               
RBBLREAD EQU   *                   READ RECOVERY FILE BLOCK                     
         L     R6,ARECBUFF         POINT TO START OF BLOCK BUFFER               
         L     R0,SVRECDTF         GET A(DTF)                                   
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R6),,(R0),(RF)                          
         OC    Q3(2),Q3                                                         
         BNZ   RBBLER1             DISK READ ERROR                              
         CLC   RECDA(3),0(R6)      CHECK DA IN BLOCK OK                         
         BNE   RBBLER1             GET HIGH RECORD NUM IN BLOCK                 
         MVC   BRECNUM,3(R6)       SAVE NUMBER RECORDS IN BLOCK                 
         LR    R1,R6                                                            
         AH    R1,4(R6)                                                         
         ST    R1,BLOCKEND         SAVE END OF BLOCK DISK ADDRESS               
         MVC   RECDA+3(1),BRECNUM  SET RECORD IN DA TO LAST IN BLOCK            
         BAS   RE,READDA           READ THE LAST RECORD IN BLOCK                
         BNE   RBBLNO                                                           
         B     RBBLOK                                                           
*                                                                               
RBBLER1  MVI   ERROR,RBBLER1Q      DISK READ ERROR                              
         B     RBBLNO                                                           
*                                                                               
RBBLNO   B     NO                                                               
RBBLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ BACKWARD FOR NEXT RECORD FROM UNBLOCKED RECOVERY FILE          *         
***********************************************************************         
RBACUNB  NTR1                                                                   
         L     R6,ARCVHDR                                                       
*                                                                               
         TM    RWRTPNDG,X'01'      TEST IF WRITE PENDING                        
         BNO   *+8                                                              
*                                  WRITE BACK UPDATED RECORD                    
         BAS   RE,WRTRCV                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,RECDA+2          BACK TO PREVIOUS BLOCK ON TRACK              
         SH    R1,=H'1'                                                         
         BNP   RBUB020             NO MORE BLOCKS ON THIS TRACK                 
         STC   R1,RECDA+2                                                       
         B     RBUBREAD            READ LAST BLOCK                              
*                                                                               
RBUB020  SR    R1,R1               BACK TO LAST RECORD ON PREV. TRACK           
         ICM   R1,3,RECDA                                                       
         SH    R1,=H'1'                                                         
         BP    *+12                                                             
         MVI   FILEFLAG,1          FLAG START OF FILE                           
         B     RBUBOK                                                           
         XC    RECDA,RECDA                                                      
         STCM  R1,3,RECDA                                                       
         MVC   RECDA+2(1),BLKSTRK+1   RESET BLOCK NUMBER                        
*                                                                               
RBUBREAD EQU   *                   READ RECOVERY FILE BLOCK                     
         L     R6,ARECBUFF         POINT TO START OF BLOCK BUFFER               
         L     R0,SVRECDTF         GET A(DTF)                                   
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODREAD),(R6),,(R0),(RF)                          
         OC    Q3(2),Q3                                                         
         BNZ   RBUBER1             DISK READ ERROR                              
         B     RBUBOK                                                           
*                                                                               
RBUBER1  MVI   ERROR,RBUBER1Q      DISK READ ERROR                              
         B     RBUBNO                                                           
*                                                                               
RBUBNO   B     NO                                                               
RBUBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE BACK UPDATED RECOVERY RECORD BLOCK                            *         
***********************************************************************         
WRTRCV   NTR1                                                                   
         CLI   INTAPE,C'Y'         EXIT IF TAPE RECOVERY FILE                   
         BE    WRCVOK                                                           
         OC    SVBLKSZ,SVBLKSZ     TEST IF UNBLOCKED FILE                       
         BZ    WRCV010                                                          
         L     R6,ARECBUFF         POINT TO START OF BLOCKED BUFFER             
         L     R5,SVRECDTF                                                      
         USING DTFPHD,R5                                                        
         L     RE,DBLK                                                          
         CLC   0(3,RE),0(R6)       IS THIS BLOCK THE CURRENT BUFFER             
         BNE   WRCV010             NO                                           
         SR    RF,RF               YES MOVE UPDATED BLOCK BACK TO BUFF          
         ICM   RF,3,DBLKSZ                                                      
         LR    R1,RF                                                            
         LR    R0,R6                                                            
         MVCL  RE,R0                                                            
         DROP  R5                                                               
*                                                                               
WRCV010  L     R0,SVRECDTF         GET A(DTF)                                   
         MVC   BYTE,RECDA+3                                                     
         MVI   RECDA+3,0                                                        
         LA    RF,RECDA            POINT TO DISK ADDRESS                        
         GOTO1 VDMOD000,Q1,A(DMODWRI),(R6),,(R0),(RF)                           
         OC    Q3(2),Q3                                                         
         BZ    *+6                                                              
         DC    H'00'                                                            
         MVC   RECDA+3(1),BYTE     RESTORE RECDA                                
         NI    RWRTPNDG,X'FF'-X'01'  CLEAR WRITE PENDING FLAG                   
         B     WRCVOK                                                           
*                                                                               
WRCVNO   B     NO                                                               
WRCVOK   B     YES                                                              
                                                                                
***********************************************************************         
* CHECK JOBNAME AND JESID FILTERS ON RECOVERY HEADER/TRAILER AT R3    *         
***********************************************************************         
                                                                                
FILTJOB  NTR1                                                                   
         OC    JOBNAME,JOBNAME     TEST JOBNAME FILTER ENTERED                  
         BZ    *+14                                                             
         CLC   8(8,R3),JOBNAME     IF SO COMPARE WITH RECOVERY RECORD           
         BNE   FJOBNO                                                           
         OC    JESID,JESID         TEST  JESID FILTER ENTERED                   
         BZ    *+14                                                             
         CLC   16(8,R3),JESID      IF SO COMPARE WITH RECOVERY RECORD           
         BNE   FJOBNO                                                           
         B     FJOBOK                                                           
*                                                                               
FJOBNO   B     NO                                                               
FJOBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SET AGENCY SYSTEM DATA                                              *         
***********************************************************************         
AGYSYS   NTR1                                                                   
         OC    AGYID,AGYID                                                      
         BZ    ASYS010             IF AGENCY ALPHA ID PASSED THEN               
         BAS   RE,GETACC             GET SYSTEM INFO FROM ACCESS RECORD         
         B     ASYS030                                                          
*                                    ELSE USE SYSTEM FILE CHARACTER             
ASYS010  LA    RE,SNUMLIST           TO GET SYSTEM NUMBER FROM SNUMLIST         
ASYS020  CLI   0(RE),0             TEST E-O-L                                   
         BE    ASYSER1                                                          
         CLC   SYSCHAR,0(RE)       MATCH INPUT TO TABLE                         
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     ASYS020                                                          
         LA    RF,SNUMLIST                                                      
         SR    RE,RF               RE=SYSTEM FILE SET NUMBER                    
         STC   RE,SNUMB                                                         
*                                                                               
* READ & PROCESS SYSTEM LIST RECORD                                             
*                                                                               
ASYS030  LA    R3,IOL                                                           
         USING CTWREC,R3           R3=A(RECORD)                                 
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ RECORD                      
*                                                                               
         LA    R3,CTWDATA          FIND SYSTEM ELEMENT ON RECORD                
         USING SYSELD,R3           R3=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
ASYS032  CLI   SYSEL,0             TEST E-O-R                                   
         BE    ASYSER2                                                          
         CLI   SYSEL,SYSELQ                                                     
         BNE   ASYS036                                                          
         CLC   SYSSYS,SYSTEM       MATCH SYSTEM NUMBER                          
         BNE   ASYS036                                                          
         OC    SENUM,SENUM         TEST IF SYSTEM SE# EXTRACTED                 
         BZ    ASYS034                                                          
         CLC   SYSSEN,SENUM        MATCH SYSTEM SE NUMBER                       
         BE    ASYS040                                                          
         B     ASYS036                                                          
ASYS034  CLC   SYSNUM,SNUMB        ELSE MATCH SYSTEM RELATIVE NUMBER            
         BE    ASYS050                                                          
ASYS036  IC    R0,SYSLEN                                                        
         AR    R3,R0                                                            
         B     ASYS032                                                          
*                                                                               
ASYS040  LA    RE,SNUMLIST         GET SUB SYSTEM CHARACTER                     
         ZIC   R1,SYSNUM                                                        
         STC   R1,SNUMB                                                         
ASYS042  CLI   0(RE),0             TEST E-O-L                                   
         BE    ASYSER3                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,ASYS042                                                       
         MVC   SYSCHAR,0(RE)                                                    
         B     ASYS060                                                          
*                                                                               
ASYS050  MVC   SENUM,SYSSEN        GET SYSTEM SE NUMBER                         
*                                                                               
ASYS060  EQU   *                                                                
         L     RE,=A(UTL)          SET SE NUMBER IN UTL                         
         MVC   4(1,RE),SENUM                                                    
         B     ASYSOK                                                           
*                                                                               
ASYSER1  MVI   ERROR,ASYSER1Q                                                   
         B     NO                                                               
ASYSER2  MVI   ERROR,ASYSER2Q                                                   
         B     NO                                                               
ASYSER3  MVI   ERROR,ASYSER3Q                                                   
         B     NO                                                               
ASYSNO   B     NO                                                               
ASYSOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET AGENCY ACCESS RECORD DATA FROM CONTROL FILE                     *         
***********************************************************************         
GETACC   NTR1                                                                   
         LA    R3,IOL                                                           
         USING CT5REC,R3           R3=A(RECORD)                                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYID                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                        
         CLI   8(R1),0                                                          
         BE    GACC010                                                          
         CLI   8(R1),X'10'                                                      
         BE    GACCNO                                                           
         DC    H'0'                DIE IF IO ERROR                              
*                                                                               
GACC010  LA    R3,CT5DATA          FIND SYSTEM ELEMENT ON RECORD                
         USING CTSYSD,R3           R3=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
GACC020  CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    GACCNO                                                           
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   GACC030                                                          
         CLC   CTSYSNUM,SYSTEM     MATCH SYSTEM NUMBER                          
         BNE   GACC030                                                          
         MVC   SENUM,CTSYSSE                                                    
         B     GACCOK                                                           
GACC030  IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     GACC020                                                          
*                                                                               
GACCNO   B     NO                                                               
GACCOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
ERRPRT   NTR1                                                                   
         LA    R3,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    R3,RF                                                            
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+13(10),=C'ERROR CODE'                                          
         MVC   P+23(L'ERRTAB),0(R3)                                             
         GOTO1 VPRINTER                                                         
         DC    H'00'                                                            
         XIT1                                                                   
                                                                                
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
RETJOBID DC    CL8'        '       RETURN AREA FOR JES JOB ID                   
                                                                                
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
ADD      EQU   X'03'               RECOVERY RECORD ADD CODE                     
CHANGE   EQU   X'02'               RECOVERY RECORD CHANGE CODE                  
COPY     EQU   X'01'               RECOVERY RECORD COPY CODE                    
         LTORG                                                                  
         EJECT                                                                  
SNUMLIST DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                    
                                                                                
STARTJOB DC    CL8'**SJOB**'                                                    
ENDJOB   DC    CL8'**EJOB**'                                                    
                                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'SYSTEM='                                   
         DC    AL1(07,02),X'00',CL11'AGENCY='                                   
         DC    AL1(06,03),X'00',CL11'INPUT='                                    
         DC    AL1(06,04),X'00',CL11'PATCH='                                    
         DC    AL1(06,05),X'00',CL11'DDSIO='                                    
         DC    AL1(08,06),X'00',CL11'JOBNAME='                                  
         DC    AL1(06,07),X'00',CL11'JESID='                                    
         DC    AL1(05,08),X'00',CL11'MODE='                                     
         DC    AL1(07,09),X'00',CL11'DSPACE='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
                                                                                
* TABLE OF VALID MODES INPUT VIA MODE=CARD                                      
*                                                                               
MODETAB  DS    0CL9                                                             
         DC    C'RECOVER ',AL1(RCOVQ)                                           
         DC    C'RERUN   ',AL1(RRUNQ)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         EJECT                                                                  
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VPERVAL  DC    V(PERVAL)                                                        
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
GETREC   DC    C'GETREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
DMDA     DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
*                                                                               
         DS    0H                                                               
ERRTAB   DS    0CL40               ERROR REPORT STRINGS                         
         DC    CL40'000 UNDEFINED ERROR'                                        
         DC    CL40'001 INVALID CONTROL CARD INPUT'                             
         DC    CL40'002 MISSING SYSTEM DEFINITION'                              
         DC    CL40'003 INVALID SYSTEM NAME'                                    
         DC    CL40'004 UNDEFINED ERROR'                                        
         DC    CL40'005 OPEN FILES ERROR'                                       
         DC    CL40'006 CLOSE FILES ERROR'                                      
         DC    CL40'007 SYSFLES ERROR'                                          
         DC    CL40'008 READ LAST RECOVERY RECORD ERROR'                        
         DC    CL40'009 READ LAST RECOVERY RECORD DISK ERROR'                   
         DC    CL40'010 INITIALISE RECORD ERROR'                                
         DC    CL40'011 PROCESS IS CHANGE RECORD ERROR 1'                       
         DC    CL40'012 PROCESS IS CHANGE RECORD ERROR 2'                       
         DC    CL40'013 PROCESS IS ADD RECORD ERROR 1'                          
         DC    CL40'014 PROCESS IS ADD RECORD ERROR 2'                          
         DC    CL40'015 PROCESS IS ADD RECORD ERROR 3'                          
         DC    CL40'016 READ BLOCKED RECOVERY FILE ERROR 1'                     
         DC    CL40'017 AGENCY SYSTEM ERROR 1'                                  
         DC    CL40'018 AGENCY SYSTEM ERROR 2'                                  
         DC    CL40'019 AGENCY SYSTEM ERROR 3'                                  
         DC    CL40'020 RECOVERY RECORD ERROR 1'                                
         DC    CL40'021 RECOVERY RECORD ERROR 2'                                
         DC    CL40'022 RECOVERY RECORD ERROR 3'                                
         DC    CL40'023 RECOVERY RECORD ERROR 4'                                
         DC    CL40'024 RECOVERY RECORD ERROR 5'                                
         DC    CL40'025 RECOVERY RECORD ERROR 6'                                
         DC    CL40'026 RECOVERY RECORD ERROR 7'                                
         DC    CL40'027 RECOVERY RECORD ERROR 8'                                
         DC    CL40'028 RECOVERY RECORD ERROR 9'                                
         DC    CL40'029 RECOVERY RECORD ERROR A'                                
         DC    CL40'030 RECOVERY RECORD ERROR B'                                
         DC    CL40'031 RECOVERY RECORD ERROR C'                                
         DC    CL40'032 READ BACK BLOCK ERROR 1'                                
         DC    CL40'033 READ DISK ADDRESS ERROR 1'                              
         DC    CL40'034 READ BACK UNBLOCKED ERROR 1'                            
         DC    CL40'035 READ FORWARD UNBLOCKED ERROR 1'                         
VCER1Q   EQU   1                                                                
VCER2Q   EQU   2                                                                
VCER3Q   EQU   3                                                                
UNDEFERQ EQU   4                                                                
OFILERQ  EQU   5                                                                
CFILERQ  EQU   6                                                                
SLISERQ  EQU   7                                                                
RLASERQ  EQU   8                                                                
RLASDERQ EQU   9                                                                
IBLKERQ  EQU   10                                                               
PISCER1Q EQU   11                                                               
PISCER2Q EQU   12                                                               
PISAER1Q EQU   13                                                               
PISAER2Q EQU   14                                                               
PISAER3Q EQU   15                                                               
RFBLER1Q EQU   16                                                               
ASYSER1Q EQU   17                                                               
ASYSER2Q EQU   18                                                               
ASYSER3Q EQU   19                                                               
PRCVER1Q EQU   20                                                               
PRCVER2Q EQU   21                                                               
PRCVER3Q EQU   22                                                               
PRCVER4Q EQU   23                                                               
PRCVER5Q EQU   24                                                               
PRCVER6Q EQU   25                                                               
PRCVER7Q EQU   26                                                               
PRCVER8Q EQU   27                                                               
PRCVER9Q EQU   28                                                               
PRCVERAQ EQU   29                                                               
PRCVERBQ EQU   30                                                               
PRCVERCQ EQU   31                                                               
RBBLER1Q EQU   32                                                               
RDDAER1Q EQU   33                                                               
RBUBER1Q EQU   34                                                               
RFUBER1Q EQU   35                                                               
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=8200,            *        
               BLKSIZE=0,MACRF=(GM),EODAD=EODADRCV                              
*                                                                               
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'0A',XL3'00',XL56'00'                                      
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
                                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
                                                                                
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
                                                                                
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
                                                                                
* DMFILTABD                                                                     
       ++INCLUDE DMFILTABD                                                      
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 RECOVERY RECORD LENGTH                       
       ++INCLUDE DMRCVRHDR                                                      
RCVREC   DS    10000C              RECOVERY RECORD DATA                         
         EJECT                                                                  
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                   PARAM LIST FOR FILE I/O                      
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
Q1       DS    F                   PARAM LIST FOR RECOVERY FILE I/O             
Q2       DS    F                                                                
Q3       DS    F                                                                
Q4       DS    F                                                                
Q5       DS    F                                                                
Q6       DS    F                                                                
*                                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
VDMGRFLS DS    A                   DATA MANAGER FILES TABLE                     
SAVERD   DS    F                                                                
EXPARMS  DS    8F                  MVS EXTRACT MACRO PARAMETER LIST             
IOKEY    DS    CL32                                                             
DMWORK   DS    12D                                                              
BYTE     DS    XL1                                                              
ERROR    DS    XL1                 ERROR CODE                                   
FILEFLAG DS    XL1                 RECOVERY FILE FLAG                           
*                                                                               
ISRCVFLG DS    XL1                 FLAG FOR AUTO IS RECORD RECOVERY             
ISRCVOFF EQU   0                   IS RECORD NOT IN RECOVERY FILE               
ISRCVPDG EQU   1                   AUTO IS RECOVERY IS PENDING                  
ISRCVON  EQU   2                   IS RECORDS ARE IN RECOVERY FILE              
*                                                                               
MVSPARM  DS    XL1                 MVS INPUT PARAMETERS                         
MVSTIME  DS    F                   IBM TIME BINARY 100S SECS                    
MVSDATE  DS    F                   IBM DATE JULIAN                              
CENTURY  DS    CL2                 CURRENT CENTURY EBCDIC VALUE                 
TODAY    DS    CL8                 TODAYS DATE EBCDIC                           
TODAYC   DS    XL2                 TODAYS DATE COMPRESSED                       
TIMENOW  DS    CL8                 TIME NOW EBCDIC                              
RETCODE  DS    XL1                 XBASE RETURN CODE                            
*                                                                               
SYSTEM   DS    XL1                 SYSTEM BINARY CODE                           
SYSCODE  DS    CL1                 SYSTEM CHARACTER CODE                        
SYSCHAR  DS    CL1                 SUB SYSTEM CHARACTER CODE                    
SNUMB    DS    XL1                 SUB SYSTEM BINARY NUMBER                     
*                                                                               
SENUM    DS    XL1                 SYSTEM SE NUMBER                             
AGYID    DS    CL2                 AGENCY ALPHA                                 
*                                                                               
JOBFOUND DS    XL1                 RECOVERY JOB FOUND FLAG (N/Y)                
*                                                                               
*                                  RECOVERY DISK FILE DA PARAMETERS             
RECDA    DS    A                   RECORD DISK ADDRESS (TTTTBBXX)               
LASTRDA  DS    A                   END OF FILE DISK ADDRESS                     
EOJDA    DS    A                   END OF JOB DISK ADDRESS                      
BLKSTRK  DS    H                   BLOCKS PER TRACK                             
BLOCKEND DS    A                   A(CURRENT BLOCK END)                         
BRECNUM  DS    XL1                 NUMBER OF RECORDS IN CURRENT BLOCK           
SVBLKSZ  DS    XL2                 SAVE DBLKSZ                                  
SVRECDTF DS    A                   SAVE A(DTF)                                  
SVSYSFL  DS    A                   SAVE A(SYSFLES)                              
SVSYSFLH DS    XL4                 SAVE SYSTEM FILES HEADER                     
TRECTY   DS    XL41                WORK AREA FOR RECOVERY RECORD HEADER         
RWRTPNDG DS    XL1                 RECOVERY BLOCK WRITE PENDING FLAG            
*                                                                               
ARCVHDR  DS    A                   A(RECOVERY RECORD)                           
ATRKBUFF DS    A                   A(DISK IO BUFFER)                            
ARECBUFF DS    A                   A(RECOVERY RECORD BLOCK BUFFER)              
ARCVSAVE DS    A                   A(LAST RECOVERY DA CHANGE RECORD)            
ARCVCOPY DS    A                   A(LAST RECOVERY DA COPY RECORD)              
AINPUT   DS    A                   A(SYSTEM FILE INPUT BUFFER)                  
AFILTAB  DS    A                   A(DMFILTAB)                                  
*                                                                               
*                                  JCL INPUT CONTROL CARD PARAMETERS            
JOBNAME  DS    CL8                 JES JOB NAME FOR RECOVERY                    
JESID    DS    CL8                 JES JOB ID FOR RECOVERY                      
INTAPE   DS    CL1                 RECOVERY FILE ON TAPE FLAG                   
MODE     DS    XL1                 OFFLINE RECOVERY MODE FLAG                   
RCOVQ    EQU   X'01'               RECOVER JOBS MODE                            
RRUNQ    EQU   X'02'               RERUN JOBS MODE                              
*                                                                               
WORK     DS    XL256               GENERAL WORK AREA                            
IOL      DS    F                   GENERAL IO BUFFER                            
IO       DS    4096X                                                            
WORKX    DS    0D                                                               
         EJECT                                                                  
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    10000D                                                           
                                                                                
RECBUFF  CSECT                     RECOVERY RECORD IO BUFFER                    
         DS    10000C                                                           
RECBUFFX EQU   *                                                                
                                                                                
RCVSAVE  CSECT                     RECOVERY DA CHANGE RECORD SAVE AREA          
         DS    10000C                                                           
RCVSAVEX EQU   *                                                                
                                                                                
RCVCOPY  CSECT                     RECOVERY DA COPY RECORD SAVE AREA            
         DS    10000C                                                           
RCVCOPYX EQU   *                                                                
                                                                                
INPUT    CSECT                     SYSTEM FILE INPUT IO BUFFER                  
         DS    10000C                                                           
INPUTX   EQU   *                                                                
                                                                                
TRKBUFF  CSECT                     DISK IO BUFFER                               
         DS    (60000)XL1                                                       
TRKBUFFX EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDRECOIL  09/20/11'                                      
         END                                                                    
