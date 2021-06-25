*          DATA SET CTMAD00    AT LEVEL 011 AS OF 07/20/20                      
*PHASE TA0C00A                                                                  
*&&US                                                                           
*INCLUDE EQVRD                                                                  
*INCLUDE DPTRD                                                                  
*&&                                                                             
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE WORKER                                                                 
*INCLUDE CRYPT                                                                  
*INCLUDE MADAPPL                                                                
*INCLUDE MADUSUK                                                                
TA0C00   TITLE 'CTMAD00 - MAINFRAME TO PC LINK - CONTROLLER'                    
TA0C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,**MAD0**,RA,R9,CLEAR=YES,RR=RE                           
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
*                                                                               
         LR    RF,RC               RF = A(NMOD WORK AREA)                       
*                                                                               
         L     RC,20(R1)           RC = A(CONTROLLER COMMON STORAGE)            
         AHI   RC,4096                = ATWA + X'1000' (4096)                   
*                                                                               
         LR    R0,RC               AOVER = A(OVERLAY SAVED STORAGE)             
         AHI   R0,4096                   = ATWA + X'2000' (8192)                
         ST    R0,AOVER                                                         
*                                                                               
         ST    RB,ABASE1           SAVE A(FIRST BASE)                           
         ST    RA,ABASE2           SAVE A(SECOND BASE)                          
         ST    R9,ABASE3           SAVE A(THIRD BASE)                           
         ST    RE,RELO                                                          
***********************************************************************         
*        SPLIT THE WORK AREA ALLOCATED BY THE NMOD INSTRUCTION UP INTO          
*        SEPARATE BLOCKS AND SAVE ADDRESSES IN GLOBAL STORAGE.                  
***********************************************************************         
         ST    RF,AINFRM           SAVE A(INPUT FRAME)                          
         AHI   RF,LENINF           BUMP PAST                                    
*                                                                               
         ST    RF,AOUTFRM          SAVE A(OUTPUT FRAME)                         
         AHI   RF,LENOUTF          BUMP PAST                                    
*                                                                               
         ST    RF,ACOMP            SAVE A(COMPRESSION BUFFER)                   
         AHI   RF,LENCOMP          BUMP PAST                                    
*                                                                               
         ST    RF,AIOS             SAVE A(DATAMGR IO AREAS)                     
         AHI   RF,LENIOS           BUMP PAST                                    
*                                                                               
         ST    RF,AFREE            SAVE A(FREE MEMORY FOR CONTROLLER)           
         AHI   RF,LENFREE          BUMP PAST                                    
*                                                                               
         LR    R7,RF               R7 = A(FIRST APPL COMMON STORAGE)            
         ST    R7,AAPPL            SAVE ADDR IN CONTROLLER STORAGE              
         AHI   RF,LENAPPL          BUMP PAST                                    
*                                                                               
         LR    R8,RF               R8 = A(US/UK APPL COMMON STORAGE)            
         ST    R8,AUSUK            SAVE ADDR IN CONTROLLER STORAGE              
***********************************************************************         
*        SPLIT UP IO AREA MEMORY BLOCK INTO THREE SEPARATE BLOCKS AND           
*        SAVE EACH OF THEIR ADDRESSES IN AIO1, AIO2, AND AADDIO.                
***********************************************************************         
         SPACE 1                                                                
         MVC   AIO1,AIOS           SAVE A(IO AREA #1)                           
         L     RF,AIO1                                                          
         AHI   RF,LEN1IO                                                        
         ST    RF,AIO2             SAVE A(IO AREA #2)                           
         AHI   RF,LEN1IO                                                        
         ST    RF,AADDIO           SAVE A(ADDREC IO AREA)                       
         MVC   AIO,AIO1            SAVE A(CURRENT IO AREA)                      
*                                                                               
         MVC   ASYSFACS,0(R1)      SAVE PARAMETERS FROM MONITOR                 
         MVC   ATIA,4(R1)                                                       
         MVC   AUTL,8(R1)                                                       
         MVC   ACOMFACS,12(R1)                                                  
         MVC   ATWA,20(R1)                                                      
         MVC   ATIOB,28(R1)                                                     
                                                                                
***********************************************************************         
* NOTE THE LITTLE GEM BELOW -                                         *         
* WE USE THE END OF THE PROGRAMS AREA (FOR 18K) TO HOUSE THE TEMP     *         
* DATA BUFFER - WATCH YOUR PROGRAM SIZE OR YOU WILL HAVE PROBLEMS     *         
***********************************************************************         
         SPACE 1                                                                
         L     RF,ASYSFACS                                                      
         L     RF,VSSB-SYSFACD(RF)                                              
         L     RF,SSBTKADR-SSBD(RF)                                             
         USING TCBD,RF                                                          
         L     RE,TCBPGMX-TCBD(RF)                                              
         AHI   RE,-(LENTMP)                                                     
         ST    RE,ATMPBUF          SAVE A(GETTMP/PUTTMP RECORD AREA)            
         AHI   RE,-32                                                           
         MVC   00(16,RE),=CL16'*MADTMP**MADTMP*'                                
         MVC   16(16,RE),=CL16'*MADTMP**MADTMP*'                                
         EJECT                                                                  
***********************************************************************         
* MAIN (HIGHEST LEVEL ROUTINE)                                        *         
***********************************************************************         
         SPACE 1                                                                
MAIN     BRAS  RE,INITRTNS         INITIALIZE ROUTINE ADDRESSES                 
         BRAS  RE,INITFRST         INITIALIZE FIRST TRANS VARIABLES             
         BNE   M02                                                              
*                                                                               
         BRAS  RE,INITALL          INITIALIZE ALL TRANSACTION VARIABLES         
         BNE   M02                                                              
         BRAS  RE,SCANTWA          SCAN CONTROL AND DATA FRAMES                 
         BNE   M02                                                              
*                                                                               
         OC    PCFRAME,PCFRAME     IF PC REQUESTS SAME SCREEN AS LAST           
         BZ    M04                 THEN STOP $MAD - RETURN SAME SCREEN          
         BRAS  RE,PROCACT          PROCESS SCANNED ACTION                       
*                                                                               
M02      BRAS  RE,BLDTWA           BUILD CONTROL AND DATA FRAMES                
         BRAS  RE,SECUREIT         SECURITY CHECK                               
         BRAS  RE,SAVEALL          SAVE ALL TRANSACTION VARIABLES               
*                                                                               
M04      BRAS  RE,EXITPROC         HANDLE EXIT CODE                             
         BRAS  RE,DOZIP            SEE IF COMPRESSING                           
         BE    MAIN                YES - AND CAN HANDLE MORE                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE HANDLES EXIT PROCESSING                                *         
*                                                                     *         
* 1) SET UP TWA FOR THE NEXT TRANSACTION                              *         
* 2) DETECT FLAG TO UNWIND TRANSACTION                                *         
***********************************************************************         
         SPACE 1                                                                
EXITPROC NTR1  ,                                                                
         L     R4,ATWA             SET SPECIAL FLAGS IN TWA                     
         USING TA0CFFD,R4                                                       
         OI    MADSRVH+6,X'81'     PREVENT 'NO DATA RECEIVED' MESSAGE           
         OI    MADPC2MH+6,X'40'    POSITION CURSOR TO PC CONTROL FRAME          
***********************************************************************         
*        IF OVERLAY SETUP AUTOSWITCH, THEN SAVE PC TO MAD CONTROL               
*        FRAME AND SET FLAG THAT INDICATES THAT FAMONTIOR WILL                  
*        REQUEUE BACK TO MAD BEFORE FINISHING THIS TRANSACTION                  
***********************************************************************         
         CLI   MADSRV,C'='         IF SERVICE REQUEST FIELD HAS '='             
         BNE   EP20                                                             
         MVC   SVPC2M,MADPC2M      THEN SAVE PC TO MAD CONTROL FRAME            
         MVI   REQUEUE,C'Y'        AND FLAG THAT FAMONITOR WILL REQUEUE         
***********************************************************************         
*        RECORD FIRST AND LAST TRANSACTIONS OF EACH ACTION TO         *         
*        LOGGER PASSING THE FOLLOWING BLOCK:                          *         
*                                                                     *         
*        BYTES 0-7    '$MADSTRT'  -  FIRST TRANSACTION, NOT LAST      *         
*                     '$MADSF  '  -  FIRST AND LAST TRANSACTION       *         
*                     '$MADFIN '  -  LAST TRANSACTION                 *         
*                     '$MADSE  '  -  FIRST TRANSACTION AND ERROR      *         
*                     '$MADERR '  -  ERROR AFTER FIRST TRANSACTION    *         
*        BYTES 8-15   LUID                                            *         
*        BYTES 16-17  ACTION NUMBER                                   *         
*        BYTES 18-21  ESTIMATED NUMBER OR TOTAL FRAMES                *         
*        BYTES 22-24  DATE (YMD BINARY)                               *         
*        BYTES 25-28  TIME (HHMMSSTH)                                 *         
*        BYTE  29     OVERLAY NUMBER                                  *         
*        BYTES 30-33  SYSTEM INPUT NUMBER                             *         
***********************************************************************         
EP20     XC    BLOCK(34),BLOCK     PRE-CLEAR TRANSACTION LOG BLOCK              
*                                                                               
         CLC   MDACTION,=H'999'    ERROR TRANSACTIONS                           
         BL    EP30                                                             
         CLI   OVERMODE,C'S'       ERROR DURING FIRST TRANSACTION               
         BNE   EP22                                                             
         MVC   BLOCK(8),=CL8'$MADSE'                                            
         B     EP40                                                             
*                                  ERROR AFTER FIRST TRANSACTION                
EP22     MVC   BLOCK(8),=CL8'$MADERR'                                           
         B     EP40                                                             
*                                                                               
EP30     CLI   OVERMODE,C'S'       FIRST TRANSACTION                            
         BNE   EP34                                                             
         CLI   MDLAST,C'Y'         ALSO LAST TRANSACTION                        
         BNE   EP32                                                             
         MVC   BLOCK(8),=CL8'$MADSF'                                            
         B     EP40                                                             
*                                  FIRST BUT NOT LAST TRANS                     
EP32     MVC   BLOCK(8),=CL8'$MADSTRT'                                          
         MVC   BLOCK+18(4),MDFRMEST                                             
         B     EP40                                                             
*                                                                               
EP34     CLI   MDLAST,C'Y'         LAST TRANSACTION                             
         BNE   EP50                                                             
         MVC   BLOCK(8),=CL8'$MADFIN'                                           
         MVC   BLOCK+18(4),MDFRAME                                              
*                                  HANDLE DATA FOR BOTH FIRST AND LAST          
EP40     GOTO1 GETFACT,CMDMCB,(X'02',0)                                         
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   BLOCK+8(8),FASYM                                                 
         MVC   BLOCK+16(2),MDACTION                                             
         MVC   BLOCK+22(3),FADATEB                                              
         MVC   BLOCK+30(4),FASIN                                                
         TIME  DEC                                                              
         STCM  R0,15,BLOCK+25                                                   
         MVC   BLOCK+29(1),OVERLAY                                              
*                                                                               
         GOTO1 LOGGER,BLOCK        CALL LOGGER PASSING DATA                     
*                                                                               
         DROP  R1                                                               
*                                                                               
EP50     DS    0H                                                               
***********************************************************************         
*        THIS CODE MUST HAPPEN LAST!                                  *         
***********************************************************************         
         SPACE 1                                                                
EP90     CLI   UNWIND,C'Y'         TEST UNWIND TRANSACTION                      
         BNE   XIT                                                              
         DC    H'0',C'$ABEND'      RESTORE FROM RECOVERY                        
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        THIS CHECKS THE VALIDITY OF THE PC CALLING PROGRAM           *         
***********************************************************************         
         SPACE 1                                                                
SECUREIT NTR1  ,                                                                
         L     R4,ATWA                                                          
         USING TA0CFFD,R4                                                       
         LA    R3,MADM2PC          R3 = A(PC CONTROL FRAME)                     
         USING COMHDRD,R3                                                       
         XC    COMCRYPT,COMCRYPT                                                
         L     RF,AUTL             GET RANDOM NUMBER FROM UTL                   
         USING UTLD,RF                                                          
         CLC   PCVRS,=H'330'       ONLY IF PC VERSION = 330                     
         BNE   XIT                                                              
         TM    TSIN+3,X'07'        ONLY ON AN 8TH TRANSACTION                   
         BNZ   XIT                                                              
*                                                                               
         L     R1,TSIN             USE LAST 4 OF DEC TSIN                       
         A     R1,TBUFF            THIS SHOULD REALLY CONFUSE THINGS            
         CVD   R1,DUB                                                           
         UNPK  WORK(4),DUB                                                      
         OI    WORK+3,X'F0'                                                     
         L     R1,TTIME            AND LAST 4 OF DEC TIME                       
         A     R1,TBUFF            THIS SHOULD REALLY CONFUSE THINGS            
         CVD   R1,DUB                                                           
         UNPK  WORK+4(4),DUB                                                    
         OI    WORK+7,X'F0'                                                     
         GOTO1 CRYPT,DMCB,8,WORK,=C'ALANELKNER'                                 
         MVC   COMCRYPT,WORK                                                    
         DROP  R3,R4,RF                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SAVE ADDRESSES OF THE ROUTINES USED BY $MAD.  THERE ARE FOUR        *         
* TYPES OF ROUTINES:                                                  *         
*                                                                     *         
*        1) ROUTINES FOUND IN FACPAK'S COMFACS DSECT                  *         
*        2) ROUTINES LINKED WITH THE CONTROLLER                       *         
*        3) ROUTINES FOUND IN FACPAK'S CORE                           *         
*        4) ROUTINES FOUND IN THE CONTROLLER                          *         
*                                                                     *         
* ROUTINES LINKED WITH THE CONTROLLER AND ROUTINES IN THE CONTROLLER  *         
* ITSELF MUST BE DYNAMICALLY RELOCATED BY ADDING THE RELOCATION       *         
* CONSTANT (RELO) TO EACH ROUTINE ADDRESS.                            *         
***********************************************************************         
         SPACE 1                                                                
INITRTNS NTR1  ,                                                                
         L     RF,ACOMFACS         SAVE ADDRESS OF COMFACS ROUTINES             
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   SCANNER,CSCANNER                                                 
         MVC   TERMVAL,CTERMVAL                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   VBLDCUR,CBLDCUR                                                  
         MVC   GETRET,CGETRET                                                   
         MVC   SWITCH,CSWITCH                                                   
         MVC   HEXIN,CHEXIN                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   GETFACT,CGETFACT                                                 
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
*&&US*&& MVC   DEMAND,CDEMAND                                                   
*                                                                               
         L     RF,ASYSFACS         SAVE ADDRESS OF SYSFACS ROUTINES             
         USING SYSFACD,RF                                                       
         MVC   LOGGER,VLOGGER                                                   
         MVC   SHIPIT,VSHIPIT                                                   
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVE ADDRESSES OF LINKED ROUTINES                                   *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,LINKED           R2 = A(FIRST VTYPE ADDRESS)                  
         LA    R3,EXTERNS          R3 = A(FIRST RELOCATED ADDRESS)              
         LA    R4,NLINKED          R4 = NUMBER OF LINKED ROUTINES               
*                                                                               
         LTR   R4,R4               IF NO LINKED ROUTINES THEN DONE              
         BZ    IR10                                                             
*                                                                               
IR5      L     R1,0(R2)            R1 = V-TYPE ADDRESS                          
         A     R1,RELO             ADD RELOCATION CONSTANT                      
         ST    R1,0(R3)            SAVE RELOCATED ADDRESS                       
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR5              LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF US/UK LINKED ROUTINES                                
***********************************************************************         
IR10     LA    R2,LINKUSUK         R2 = A(FIRST VTYPE ADDRESS)                  
         LA    R3,EXTUSUK          R3 = A(FIRST RELOCATED ADDRESS)              
         LA    R4,NLNKUSUK         R4 = NUMBER OF LINKED ROUTINES               
*                                                                               
         LTR   R4,R4               IF NO LINKED ROUTINES THEN DONE              
         BZ    IR20                                                             
*                                                                               
IR15     L     R1,0(R2)            R1 = V-TYPE ADDRESS                          
         A     R1,RELO             ADD RELOCATION CONSTANT                      
         ST    R1,0(R3)            SAVE RELOCATED ADDRESS                       
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR15             LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF CORE RESIDENT ROUTINES                               
***********************************************************************         
IR20     XC    CMDMCB,CMDMCB       SET UP PARAMETERS TO CALLOV                  
         MVC   CMDMCB+4(3),=X'D9000A'                                           
*                                                                               
         LA    R2,CORERES          R2 = A(FIRST CORERES ROUTINE EQUATE)         
         LA    R3,COREADRS         R3 = A(FIRST CORERES ROUTINE ADDR)           
         LA    R4,NCORERES         R4 = NUMBER OF CORERES ROUTINES              
*                                                                               
         LTR   R4,R4               IF NO CORERES ROUTINES THEN DONE             
         BZ    IR30                                                             
*                                                                               
IR25     MVC   CMDMCB+7(1),0(R2)   INSERT OVERLAY NUMBER INTO PARAMETER         
*                                                                               
         GOTO1 CALLOV,CMDMCB       CALL CALLOV                                  
         CLI   CMDMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                DIE IF CALLOV RETURNS ERROR                  
*                                                                               
         MVC   0(4,R3),CMDMCB      SAVE ROUTINE ADDRESS                         
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR25             LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF US/UK CORE RESIDENT ROUTINES                         
***********************************************************************         
         SPACE 1                                                                
IR30     XC    CMDMCB,CMDMCB       SET UP PARAMETERS TO CALLOV                  
         MVC   CMDMCB+4(3),=X'D9000A'                                           
*                                                                               
         LA    R2,COREUSUK         R2 = A(FIRST CORERES ROUTINE EQUATE)         
         LA    R3,CORAUSUK         R3 = A(FIRST CORERES ROUTINE ADDR)           
         LA    R4,NCORUSUK         R4 = NUMBER OF CORERES ROUTINES              
*                                                                               
         LTR   R4,R4               IF NO CORERES ROUTINES THEN DONE             
         BZ    IR40                                                             
*                                                                               
IR35     MVC   CMDMCB+7(1),0(R2)   INSERT OVERLAY NUMBER INTO PARAMETER         
*                                                                               
         GOTO1 CALLOV,CMDMCB       CALL CALLOV                                  
         CLI   CMDMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                DIE IF CALLOV RETURNS ERROR                  
*                                                                               
         MVC   0(4,R3),CMDMCB      SAVE ROUTINE ADDRESS                         
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR35             LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF CONTROLLER COMMON ROUTINES                           
*                                                                               
* ALL CONTROLLER COMMON ROUTINES ARE ENTERED THROUGH AN INTERMEDIATE            
* ROUTINE CALLED CONTCOMM. CONTCOMM SETS UP THE BASE REGISTERS                  
* AND CALLS THE DESIRED ROUTINE.  THE WAY THIS WORKS IS THAT ALL                
* CONTOLLER ROUTINE COMMON ADDRESSES ARE REALLY THE ADDRESS OF CONTCOMM         
* WITH A ROUTINE NUMBER IN THE HIGH ORDER BYTE TO TELL CONTCOMM WHICH           
* ROUTINE TO BRANCH TO.                                                         
***********************************************************************         
IR40     LA    R2,CONTCOMM         R2 = A(CONTCOMM)                             
         SR    R3,R3               R3 = ROUTINE NUMBER = 0                      
         LA    R4,CONTADRS         R4 = A(FIRST ROUTINE ADDRESS)                
         LA    R5,CONTADRN         R5 = NUMBER OF CONTROLLER ROUTINES           
*                                                                               
         LTR   R5,R5               IF NO CONTROLLER ROUTINES THEN DONE          
         BZ    IR60                                                             
*                                                                               
IR50     ST    R2,0(R4)            SAVE A(CONTCOMM) IN LAST 3 BYTES             
         STC   R3,0(R4)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    R3,1(R3)            BUMP ROUTINE NUMBER                          
         LA    R4,4(R4)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R5,IR50             LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF APPLICATION COMMON ROUTINES                          
*                                                                               
* ALL NON-US/UK SPECIFIC APPLICATION COMMON ROUTINES ARE ALSO ENTERED           
* THROUGH AN INTERMEDIATE ROUTINE, IN THIS CASE =V(APPLCOMM), LOCATED           
* IN THE PAN BOOK, CTMADAPPL.  APPLCOMM WILL BRANCH TO THE DESIRED              
* ROUTINE.  THE FOLLOWING CODE SEGMENT WORKS THE SAME WAY AS THE                
* CONTCOMM CODE ABOVE.                                                          
***********************************************************************         
IR60     L     R2,=V(APPLCOMM)     R2 = A(APPLCOMM)                             
         A     R2,RELO                                                          
         SR    R3,R3               R3 = ROUTINE NUMBER = 0                      
         LA    R4,APPLADRS         R4 = A(FIRST ROUTINE ADDRESS)                
         LA    R5,APPLADRN         R5 = NUMBER OF APPLICATION ROUTINES          
*                                                                               
         LTR   R5,R5               IF NO APPL ROUTINES THEN DONE                
         BZ    IR80                                                             
*                                                                               
IR70     ST    R2,0(R4)            SAVE A(APPLCOMM) IN LAST 3 BYTES             
         STC   R3,0(R4)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    R3,1(R3)            BUMP ROUTINE NUMBER                          
         LA    R4,4(R4)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R5,IR70             LOOP BACK                                    
***********************************************************************         
*        SAVE ADDRESSES OF US/UK APPLICATION COMMON ROUTINES                    
*                                                                               
* ALL US/UK SPECIFIC APPLICATION COMMON ROUTINES ARE ALSO ENTERED               
* THROUGH AN INTERMEDIATE ROUTINE, IN THIS CASE =V(USUKCOMM), LOCATED           
* IN EITHER CTMADUS OR CTMADUK.  USUKCOMM WILL BRANCH TO THE DESIRED            
* ROUTINE.  THE FOLLOWING CODE SEGMENT WORKS THE SAME WAY AS THE                
* CONTCOMM CODE ABOVE.                                                          
***********************************************************************         
IR80     L     R2,=V(USUKCOMM)     R2 = A(USUKCOMM)                             
         A     R2,RELO                                                          
         SR    R3,R3               R3 = ROUTINE NUMBER = 0                      
         LA    R4,USUKADRS         R4 = A(FIRST ROUTINE ADDRESS)                
*                                                                               
*&&US                                                                           
         LA    R5,USADRN           R5 = NUMBER OF US APPL ROUTINES              
*&&                                                                             
*&&UK                                                                           
         LA    R5,UKADRN           R5 = NUMBER OF UK APPL ROUTINES              
*&&                                                                             
*                                                                               
         LTR   R5,R5               IF NO APPL ROUTINES THEN DONE                
         BZ    IR100                                                            
*                                                                               
IR90     ST    R2,0(R4)            SAVE A(USUKCOMM) IN LAST 3 BYTES             
         STC   R3,0(R4)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    R3,1(R3)            BUMP ROUTINE NUMBER                          
         LA    R4,4(R4)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R5,IR90             LOOP BACK                                    
*                                                                               
IR100    DS    0H                                                               
*                                                                               
IRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISE FIRST TIME VARIABLES                                     *         
* FIRED FROM TWAUSER(1) - IF 0 FIRST TIME, IF 1 NOT FIRST TIME        *         
* EXIT: CC EQ  - INITIALISED OK                                       *         
*       CC NEQ - ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
INITFRST NTR1  ,                                                                
         L     R4,ATWA             ONLY FIRST TRANSACTION                       
         CLI   TWAUSER-TWAD(R4),0                                               
         BNE   YES                                                              
*                                                                               
         XC    SVPCACT,SVPCACT     RESET STATUS VARIABLES                       
         XC    SVMDFRM,SVMDFRM                                                  
         MVI   SVMDLAST,C' '                                                    
*                                                                               
         MVI   TMPOFLAG,C'N'       SET TEMP FILE NOT OPEN                       
         XC    TMPMAX,TMPMAX       CLEAR LARGEST REC PUT TO TEMP FILE           
*                                                                               
         BRAS  RE,VALUSER          VALIDATE USER SIGNON INFO                    
         BNE   NO                                                               
*                                                                               
         MVI   TWAUSER-TWAD(R4),1  SET FIRST TIME FLAG = FALSE                  
         MVC   LENTWA,=Y(TWAMAX)   SET LENGTH OF TWA (18K)                      
         MVI   REQUEUE,C'N'        SET FAMONITOR REQUEUE FLAG                   
*                                                                               
         GOTO1 GETFACT,CMDMCB,0    GET TRANSLATE TABLE FOR THIS COUNTRY         
         L     RF,0(R1)                                                         
         L     RE,FAXLATES-FACTSD(RF)                                           
*                                                                               
         L     RF,AUTL                                                          
         XR    R1,R1                                                            
         IC    R1,TCTRY-UTLD(RF)   R1 = COUNTRY NUMBER                          
         MHI   R1,16               R1 = INDEX INTO TRANSLATE TABLE              
         AR    RE,R1                                                            
         L     RE,12(RE)           RE = A(XLATE TABLE FOR THIS COUNTRY)         
         MVC   VALOCHRS,0(RE)      MOVE XLATE TABLE TO CONTROL STORAGE          
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INITIALIZE VARIABLES THAT NEED TO INITIALIZED ON EVERY           
* TRANSACTION.  IF ALL GOES WELL, THE ROUTINE RETURNS 'YES'.  OTHERWISE         
* IT RETURNS 'NO'.                                                              
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1  ,                                                                
         GOTO1 READTWA,CMDMCB,ATIA,1  READ TEMPSTR RECORD 1 INTO ATIA           
*                                                                               
         LR    RE,R7               RESTORE 1ST COMMON STORAGE TO R7             
         LHI   RF,4096                                                          
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LR    RE,R8               RESTORE 2ND COMMON STORAGE TO R8             
         LHI   RF,4096                                                          
         L     R0,ATIA                                                          
         AHI   R0,4096                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
***********************************************************************         
*        SINCE RC WILL BE LOST DURING THE NMOD1 INSTRUCTIONS OF THE             
*        APPLICATION COMMON ROUTINES AND THE OVERLAYS, IT IS SAVED              
*        HERE IN APPLD.  THE OVERLAYS CAN THEN RESTORE IT IMMEDIATELY           
*        FOLLOWING THE NMOD1 INSTRUCTION.                                       
***********************************************************************         
         ST    RC,ACONTD           SAVE RC IN APPLD                             
***********************************************************************         
*        INITIALIZE VARIABLES FROM UTL                                          
***********************************************************************         
         GOTO1 PROTOFF             DISSABLE STORAGE PROTECTION                  
         L     RF,AUTL             RF = A(UTL)                                  
         USING UTLD,RF                                                          
         OI    TFLAG,TFLAGIRB      INHIBIT BROADCAST MESSAGES                   
         DROP  RF                                                               
         GOTO1 PROTON              RESTORE STORAGE PROTECTION                   
***********************************************************************         
*        INITIALIZE VARIABLES USED IN CALLSUB                                   
***********************************************************************         
         MVC   CSPHASE,=X'D90A0C00'   PHASE PARAMETER FOR CALLOV CALL           
         MVI   CSSP,0                 OVERLAY STACK POINTER                     
***********************************************************************         
*        INITIALIZE MAD CONTROL FRAME VARIABLES                                 
***********************************************************************         
         MVC   MDFRAME,=F'0'       MAD FRAME NUMBER                             
         MVI   MDLAST,C'N'         LAST FRAME FLAG = 'N'                        
         MVI   MDDEBUG,C'N'        DEBUG FLAG = 'N'                             
***********************************************************************         
*        IF FAMONITOR HAS JUST REQUEUED BACK TO MAD THEN THIS                   
*        TRANSACTION WILL GO THROUGH MAD TWICE.  TO PREVENT THE FRAME           
*        NUMBER FROM INCREMENTING TWICE WE WILL DECREMENT IT HERE.              
*        ALSO WE NEED TO RESTORE THE PC TO MAD CONTROL FRAME.                   
***********************************************************************         
         CLI   REQUEUE,C'Y'        IF FAMONTIOR REQUEUED                        
         BNE   IA10                                                             
         L     RF,SVMDFRM          THEN DECREMENT MAD FRAME NUMBER              
         BCTR  RF,0                                                             
         ST    RF,SVMDFRM                                                       
         L     RF,ATWA                                                          
         USING TA0CFFD,RF                                                       
         MVC   MADPC2M,SVPC2M      RESTORE PC TO MAD CONTROL FRAME              
         MVI   REQUEUE,C'N'        CLEAR FAMONTIOR REQUEUE FLAG                 
         DROP  RF                                                               
*                                                                               
IA10     MVI   UNWIND,C'N'         RESET UNWIND SWITCH                          
***********************************************************************         
*        INITIALIZE GETITEM AND PUTIEM VARIABLES                                
***********************************************************************         
         MVC   AINPUT,AINFRM       A(BEGINNING OF INPUT DATA FRAME)             
         MVC   AOUTPUT,AOUTFRM     A(BEGINNING OF OUTPUT DATA FRAME)            
         MVI   EIFFLAG,C'N'        END OF INPUT DATA FRAME FLAG                 
         MVI   EOFFLAG,C'N'        END OF OUTPUT DATA FRAME FLAG                
         XC    APPLERR,APPLERR     CLEAR APPLICATION ERROR NUMBER               
***********************************************************************         
*        RESTORE TEMPORARY FILE BUFFER (IF TEMPSTR FILE OPEN)                   
***********************************************************************         
         CLI   TMPOFLAG,C'N'       IF TEMP FILE OPEN                            
         BE    IA20                                                             
*                                  THEN READ TWA INTO TMPBUF                    
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 READTWA,CMDMCB,ATMPBUF,CURRTWA                                   
***********************************************************************         
*        INITIALIZE THE SYSTEM SE NUMBER AND FILE NAMES TO THE                  
*        CONTROL SYSTEM, 'CTFILE'.  THIS WAY, AN OVERLAY NEED NOT               
*        CALL SETSYS IF IT WISHES ONLY TO USE THE CONTROL FILE.                 
***********************************************************************         
IA20     MVI   SYSNUM,0            SYSNUM WILL BECOME CONTROL SYSTEM            
         GOTO1 SETSYS,CMDMCB,=C'CONTROL',0,=CL8'CTFILE'                         
         BNE   NO                                                               
         B     YES                                                              
*                                                                               
IAYES    B     YES                                                              
*                                                                               
IANO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF PC IS AUTHORISED TO USE REQUESTED MAD APPLICATION          *         
* DDS TERMINALS ALWAYS AUTHORISED                                     *         
* OTHER TERMINAL AUTHORISATION FROM TERMINAL DEFINITION ELEMENT       *         
* EXIT: CC EQ  - TERMINAL AUTHORISED                                  *         
* EXIT: CC NEQ - MDACTION = NOT AUTHORISED                            *         
***********************************************************************         
         SPACE 1                                                                
VALACCES NTR1  ,                                                                
         L     RF,AUTL             DDS TERMINALS ARE ALWAYS ALLOWED             
         TM    TSTAT-UTLD(RF),TSTATDDS                                          
         BNZ   YES                                                              
*                                  CALL TERMINAL VALIDATION ROUTINE             
         GOTO1 PROTOFF                                                          
         GOTO1 TERMVAL,CMDMCB,(X'30',0),AUTL,0,0                                
         GOTO1 PROTON                                                           
         ICM   RE,7,13(R1)         IF TERMINAL DEFINITION ELEMENT               
         BNZ   VA10                    FOUND THEN CHECK BITS                    
*                                                                               
         CLI   APPLID,AIALINK      ELSE ERROR IF ALINK ACTION                   
         BNE   YES                                                              
         MVC   MDACTION,=Y(NOTAUTH)                                             
         B     NO                                                               
*                                                                               
VA10     CLI   APPLID,AIALINK      IF ALINK ACTION CHECK ALINK BIT              
         BNE   YES                                                              
         TM    CTTRMAT1-CTTRMD(RE),X'80'                                        
         BO    YES                                                              
         MVC   MDACTION,=Y(NOTAUTH)                                             
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE USER SIGNON INFORMATION                                  *         
* EXIT: CC EQ  - INITIALISED OK                                       *         
*       CC NEQ - ERROR IN MDACTION                                    *         
***********************************************************************         
         SPACE 1                                                                
VALUSER  NTR1  ,                                                                
         L     R4,ATWA             GET 2 BYTE HEX SIGNON ID AND 2 BYTE          
         USING TWAD,R4                                                          
         MVC   SIGNON2,TWAUSRID    CHAR SIGNON ID FROM TWA                      
         MVC   SIGNON2C,TWAAGY                                                  
         DROP  R4                                                               
*                                  CONVERT TO 8 BYTE CHAR - CONTROL             
*                                  FILE USER ID RECORD WILL BE IN AIO           
         GOTO1 CONVOFF,CMDMCB,(0,SIGNON2),SIGNON8                               
         BNE   NO                                                               
*                                                                               
         LA    R3,SYSLIST          R3 = A(SYSTEM LIST)                          
         USING SYSLISTD,R3                                                      
*                                                                               
         L     R4,AIO              R4 = A(FIRST ELEMENT IN ID RECORD)           
         USING CTIKEY,R4                                                        
         LA    R4,CTIDATA                                                       
         USING CTSYSD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VU10     CLI   CTSYSEL,0                                                        
         BNE   *+14                                                             
         MVC   MDACTION,=Y(BADIDREC)                                            
         B     NO                                                               
*                                                                               
         CLI   CTSYSEL,CTSYSELQ                                                 
         BE    VU20                                                             
         IC    RF,CTSYSLEN                                                      
         BXH   R4,RF,VU10                                                       
*                                                                               
VU20     CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   VU30                                                             
         MVC   SYSNUMB,CTSYSNUM    ADD ENTRY TO SYSLIST                         
         MVC   SYSSE,CTSYSSE                                                    
         AHI   R3,SYSLISTL         BUMP R3 TO NEXT SYSLIST ENTRY                
         IC    RF,CTSYSLEN                                                      
         BXH   R4,RF,VU20                                                       
*                                                                               
VU30     MVI   0(R3),0             MARK END OF SYSLIST                          
         B     YES                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SCAN INCOMING PC MESSAGE.  EXTRACT PC CONTROL FRAME VALUES INTO               
* CONTROLLER WORK AREA AND EXTRACT DATA FRAME FIELDS INTO INPUT FRAME           
* AREA.                                                                         
***********************************************************************         
         SPACE 1                                                                
SCANTWA  NTR1  ,                                                                
         L     R4,ATWA             R4 = A(TWA)                                  
         USING TA0CFFD,R4                                                       
*                                                                               
         LA    R3,MADM2PC          R3 = A(PC CONTROL FRAME)                     
         USING COMHDRD,R3                                                       
         XC    DUB,DUB                                                          
         OC    COMCRYPT,COMCRYPT                                                
         BZ    ST1                                                              
         MVC   DUB,COMCRYPT                                                     
*                                                                               
ST1      LA    R3,MADPC2M          R3 = A(PC CONTROL FRAME)                     
         USING COMHDRD,R3                                                       
         GOTO1 DECIN,CMDMCB,COMVRS,3   EXTRACT PC VERSION NUMBER                
         BNE   STVRS               NOT NUMERIC                                  
         MVC   PCVRS,2(R1)                                                      
         BRAS  RE,SETVRS           SET VERSION DEPENDENT VARIABLES              
*                                                                               
         CLC   PCVRS,=H'320'       IF VERSION >= 320 TEST COMCRYPT              
         BL    ST1A                                                             
         OC    DUB,DUB                                                          
         BZ    ST1A                                                             
         GOTO1 CRYPT,DMCB,8,COMCRYPT,=C'ALANELKNER'                             
         CLC   DUB,COMCRYPT                                                     
         B     *+6                 NOP THIS DUMP                                
         DC    H'0'                                                             
*                                                                               
ST1A     GOTO1 DECIN,CMDMCB,COMACT,4  EXTRACT REQUESTED PC ACTION #             
         BNE   STACT                                                            
         MVC   PCACTION,2(R1)      SAVE IN PCACTION                             
*                                                                               
         OC    PCACTION,PCACTION   IF ACTION IS ZERO THEN ERROR                 
         BZ    STACT0                                                           
***********************************************************************         
*        EXTRACT REQUESTED FRAME NUMBER                                         
***********************************************************************         
         SPACE 1                                                                
         CLI   COMFRM,C'+'         IF '+' ENTERED                               
         BNE   ST2                                                              
         MVC   PCFRAME,=F'1'       THEN PC FRAME = 1                            
         B     ST7                                                              
*                                                                               
ST2      CLI   COMFRM,C'?'         ELSE IF '?' ENTERED                          
         BNE   ST5                                                              
         MVC   PCFRAME,=F'0'       THEN PC FRAME = 0                            
         B     ST7                                                              
*                                                                               
ST5      B     STFRM               ELSE INVALID FRAME NUMBER SYNTAX             
***********************************************************************         
* EXTRACT LAST FRAME INDICATOR                                        *         
***********************************************************************         
         SPACE 1                                                                
ST7      MVI   PCLAST,C'N'         IF 'N' OR NOTHING ENTERED THEN SET           
         CLI   COMLAST,C' '            TO 'N'                                   
         BNH   ST10                                                             
         CLI   COMLAST,C'N'                                                     
         BE    ST10                                                             
*                                                                               
         CLI   COMLAST,C'Y'        ELSE MUST ENTER 'Y'                          
         BNE   STLAST                                                           
         MVI   PCLAST,C'Y'         SET TO 'Y'                                   
***********************************************************************         
* EXTRACT DEBUG INDICATOR                                             *         
***********************************************************************         
         SPACE 1                                                                
ST10     MVI   PCDEBUG,C'N'        IF 'N' OR NOTHING ENTERED THEN SET           
         CLI   COMDEBUG,C' '           TO 'N'                                   
         BNH   ST20                                                             
         CLI   COMDEBUG,C'N'                                                    
         BE    ST20                                                             
*                                                                               
         CLI   COMDEBUG,C'Y'       ELSE MUST ENTER 'Y'                          
         BNE   STDEBUG                                                          
         MVI   PCDEBUG,C'Y'        SET TO 'Y'                                   
*                                                                               
ST20     L     R3,AINFRM           EXTRACT DATA INTO INPUT FRAME AREA           
         LA    R2,MADDATAH         R2 = A(FIRST FIELD IN DATA FRAME)            
         USING FHD,R2                                                           
         XR    RF,RF                                                            
ST30     ICM   RF,1,FHLN           END OF SCREEN?                               
         BZ    ST32                YES                                          
         LHI   R0,FHDAD+1          NORMAL                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LHI   R0,FHDAD+FHDAD+1    EXTENDED HEADER                              
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FHDA        MOVE IN DATA                                 
         LA    R3,1(RF,R3)         GO TO NEXT DATA IN OUTPUT FRAME              
         AR    RF,R0                                                            
         BXH   R2,RF,ST30          BUMP TO NEXT FIELD                           
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
ST32     S     R3,AINFRM           COMPUTE FRAME SIZE                           
         ST    R3,MDFRMSIZ                                                      
         MVC   INPLEFT,MDFRMSIZ    SPACE REMAINING IN INPUT DATA FRAME          
         MVC   OUTLEFT,MDFRMSIZ    SPACE REMAINING IN OUTPUT DATA FRAME         
         B     YES                                                              
*                                                                               
STVRS    MVC   MDACTION,=Y(INVVRS)   INVALID VERSION NUMBER SYNTAX              
         B     NO                                                               
STACT    MVC   MDACTION,=Y(INVACT)   INVALID ACTION NUMBER SYTAX                
         B     NO                                                               
STACT0   MVC   MDACTION,=Y(INVACT0)  INVALID ACTION NUMBER ZERO                 
         B     NO                                                               
STFRM    MVC   MDACTION,=Y(INVFRM)   INVALID FRAME NUMBER SYNTAX                
         B     NO                                                               
STLAST   MVC   MDACTION,=Y(INVLAST)  INVALID LAST FRM INDICATOR SYNTAX          
         B     NO                                                               
STDEBUG  MVC   MDACTION,=Y(INVDEBUG) INVALID DEBUG INDICATOR SYNTAX             
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS THOSE VARAIBLES WHICH ARE AFFECTED BY THE PC      *         
* VERSION NUMBER.                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETVRS   NTR1  ,                                                                
         MVI   TERMCHR,DINKCHR                                                  
         CLC   PCVRS,=H'310'                                                    
         BL    *+8                                                              
         MVI   TERMCHR,C'+'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD MESSAGE TO PC.  BUILD $MAD CONTROL FRAME FROM VALUES IN                 
* CONTROLLER WORK AREA AND BUILD DATA FRAME FROM OUTPUT FRAME AREA              
***********************************************************************         
         SPACE 1                                                                
BLDTWA   NTR1  ,                                                                
         L     R4,ATWA             R4 = A(TWA)                                  
         USING TA0CFFD,R4                                                       
         LA    R3,MADM2PC          R3 = A(MAD CONTROL FRAME)                    
         USING COMHDRD,R3                                                       
*                                  FILL MAD CONTROL FRAME                       
         EDIT  (2,PCVRS),(3,COMVRS),FILL=0                                      
         EDIT  (2,MDACTION),(4,COMACT),FILL=0                                   
         EDIT  (4,MDFRAME),(7,COMFRM),FILL=0                                    
         MVC   COMLAST,MDLAST                                                   
         MVC   COMDEBUG,MDDEBUG                                                 
         EDIT  (4,MDFRMSIZ),(7,COMFRS),FILL=0                                   
         EDIT  (4,MDFRMEST),(7,COMEST),FILL=0                                   
*                                                                               
         OI    MADM2PCH+6,X'80'    TRANSMIT FIELD                               
         DROP  R3                                                               
*                                                                               
         L     R3,AOUTFRM          R3 = A(OUTPUT FRAME AREA)                    
         LA    R2,MADDATAH         R2 = A(FIRST FIELD IN DATA FRAME)            
         USING FHD,R2                                                           
         XR    RF,RF                                                            
*                                                                               
BT10     LA    RE,MADWORK          STOP TWA OVERFLOW                            
         CR    RE,R2                                                            
         BH    *+6                                                              
         DC    H'0'                LET'S SEE IF THIS HELPS                      
*                                                                               
         ICM   RF,1,FHLN           RF = LENGTH OF FIELD                         
         BZ    XIT                                                              
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         LHI   R0,FHDAD+1          NORMAL                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LHI   R0,FHDAD+FHDAD+1    EXTENDED HEADER                              
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),0(R3)       MOVE OUT DATA                                
         LA    R3,1(RF,R3)         GO TO NEXT DATA IN OUTPUT FRAME              
         AR    RF,R0                                                            
         BXH   R2,RF,BT10          BUMP TO NEXT FIELD                           
         DC    H'0'                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE REQUESTED ACTION READ BY SCANTWA.                  
***********************************************************************         
         SPACE 1                                                                
PROCACT  NTR1  ,                                                                
         MVC   MDACTION,PCACTION   COPY PC ACTION TO MAD                        
         BRAS  RE,TESTCHG          TEST ACTION CHANGE AND FINISH OLD            
         BNE   XIT                                                              
*                                  IF ACTION IS ABORT                           
         CLC   PCACTION,=Y(ACABORT)                                             
         BNE   *+12                                                             
         MVI   MDLAST,C'Y'         THEN SET LAST FRAME TO 'Y'                   
         B     XIT                                                              
*                                                                               
         CLC   PCACTION,SVPCACT    ELSE IF ACTION IS SAME AS OLD ACT            
         BNE   PA02                                                             
         CLI   SVMDLAST,C'Y'       AND PREVIOUS FRAME WAS NOT LAST              
         BE    PA02                                                             
         BRAS  RE,PROCCONT         THEN CONTINUE OLD ACTION                     
         B     XIT                                                              
*                                                                               
PA02     CLI   TMPOFLAG,C'N'       ELSE IF TEMPORARY FILE OPEN                  
         BE    PA04                                                             
         GOTO1 TMPCLOSE            THEN CLOSE IT                                
*                                                                               
PA04     BRAS  RE,PROCSTRT         START NEW ACTION                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK IF PC  IS ASKING FOR A NEW ACTION                             *         
* IF SO AND WE DID SOMETHING BEFORE - CALL OVERLAY TO END ACTION      *         
*       - IF ERROR DURING CALL RETURN CC NEQ                          *         
* ELSE  - RETURN CC OK                                                *         
***********************************************************************         
         SPACE 1                                                                
TESTCHG  NTR1  ,                                                                
         OC    SVPCACT,SVPCACT     IF OLD ACTION IS NOT ZERO                    
         BZ    TCH04                                                            
         CLC   PCACTION,SVPCACT    AND ACTION HAS CHANGED                       
         BNE   TCH02                                                            
         CLI   SVMDLAST,C'Y'       OR PREVIOUS FRAME WAS LAST                   
         BNE   TCH04                                                            
*                                  THEN LOOK UP OLD ACTION OVERLAY NUM          
TCH02    GOTO1 VALACT,CMDMCB,SVPCACT                                            
         BNE   TCH04                                                            
         MVI   OVERMODE,C'E'       CALL OVERLAY WITH MODE 'END'                 
         GOTO1 CALLSUB,CMDMCB,OVERLAY                                           
*                                                                               
TCH04    CLC   MDACTION,=Y(EQERROR)                                             
         BL    YES                 OK                                           
         B     NO                  ERROR                                        
         EJECT                                                                  
***********************************************************************         
* START NEW ACTION.  IF THE NEW ACTION IS NOT VALID OR THE NEW FRAME            
* NUMBER IS NOT ONE THEN RETURN AN ERROR.  OTHERWISE CALL THE OVERLAY           
* FOR THE NEW ACTION WITH THE MODE 'START'.                                     
***********************************************************************         
         SPACE 1                                                                
PROCSTRT NTR1  ,                                                                
*                                  LOOK UP NEW ACTION                           
PS10     GOTO1 VALACT,CMDMCB,PCACTION                                           
         BNE   PSX                 IF ERROR THEN DONE                           
*                                                                               
         GOTO1 PROTOFF                                                          
         L     RF,ASYSFACS                                                      
         L     RF,VSSB-SYSFACD(RF)                                              
         L     RF,SSBTKADR-SSBD(RF)                                             
         MVC   TCBUDATA-TCBD(2,RF),PCACTION                                     
         GOTO1 PROTON                                                           
*                                                                               
         MVC   MDFRAME,=F'1'       MAD FRAME = 1                                
         MVC   MDFRMEST,=F'0'      MAD ESTIMATED NUMBER OF FRAMES = 0           
*                                                                               
         MVI   OVERMODE,C'S'       CALL OVERLAY WITH MODE 'START'               
*                                                                               
* DO NOT CLEAR SAVED STORAGE FOR CTMAD20                                        
* TO DO SO WOULD BE PATHETICALLY STUPID (AS WE NOW KNOW)                        
*                                                                               
         CLI   OVERLAY,X'20'                                                    
         BE    PS20                                                             
*                                                                               
         L     RE,AOVER            CLEAR OVERLAY SAVE STORAGE                   
         LHI   RF,4096                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COMPTAB(26*8),COMPTAB   AND COMPTAB                              
*                                                                               
PS20     EQU   *                       IN UK CLEAR COMPTAB FOR 20               
         XC    COMPTAB(26*8),COMPTAB                                            
*                                                                               
         GOTO1 CALLSUB,CMDMCB,OVERLAY                                           
*                                                                               
PSX      CLC   MDACTION,=Y(EQERROR) IF ALL WENT WELL                            
         BL    YES                 THEN RETURN 'YES'                            
         B     NO                  ELSE RETURN 'NO'                             
         EJECT                                                                  
***********************************************************************         
* CONTINUE PROCESSING OLD ACTION.  IF THE NEW FRAME NUMBER IS NOT ONE           
* MORE THAN THE OLD FRAME NUMBER THEN RETURN AN ERROR.  ALSO IF THE             
* OLD FRAME WAS THE LAST FRAME THEN RETURN AN ERROR.  OTHERWISE                 
* CALL THE OVERLAY FOR THE OLD ACTION.                                          
***********************************************************************         
         SPACE 1                                                                
PROCCONT NTR1  ,                                                                
         CLI   SVMDLAST,C'Y'       OLD FRAME WAS LAST FRAME?                    
         BNE   *+14                NO                                           
         MVC   MDACTION,=Y(LASTFRM)                                             
         B     NO                                                               
*                                  LOOKUP ACTION                                
         GOTO1 VALACT,CMDMCB,PCACTION                                           
*                                                                               
         L     RF,SVMDFRM          MAD FRAME NUMBER = PREVIOUS + 1              
         AHI   RF,1                                                             
         ST    RF,MDFRAME                                                       
*                                                                               
         MVI   OVERMODE,C'M'       CALL OVERLAY WITH MODE 'MIDDLE'              
         GOTO1 CALLSUB,CMDMCB,OVERLAY                                           
         CLC   MDACTION,=Y(EQERROR)                                             
         BL    YES                                                              
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* LOOK UP THE ACTION PASSED IN P1 IN THE ACTION TABLE                 *         
* NTRY: P1     = ACTION NUMBER                                        *         
* EXIT: CC EQ  - OVERLAY  = OVERLAY NUMBER                            *         
*              - APPLID   = APPLICATION ID                            *         
*       CC NEQ - MDACTION = INVALID ACTION MESSAGE                    *         
***********************************************************************         
         SPACE 1                                                                
VALACT   NTR1  ,                                                                
         L     R4,0(R1)            R4 = A(ACTION NUMBER)                        
         LA    R3,ACTTAB           R3 = A(FIRST ACTION IN TABLE)                
         USING ACTTABD,R3                                                       
         LHI   RF,ACTTABL                                                       
*                                                                               
VAC02    CLC   ACTNUM,=AL2(0)      EOT?                                         
         BNZ   *+14                                                             
         MVC   MDACTION,=Y(ER00ANF)                                             
         B     NOEXIT              ACTION NOT FOUND                             
*                                                                               
         CLC   ACTNUM,0(R4)        TEST MATCH                                   
         BE    *+8                                                              
         BXH   R3,RF,VAC02         NEXT ENTRY                                   
*                                                                               
         MVC   OVERLAY,ACTOVLY     RETURN OVERLAY NUMBER                        
         MVC   APPLID,ACTAPPL             APPLICATION ID                        
*                                                                               
         BRAS  RE,VALACCES         VALIDATE ACCESS TO MAD APPLICATON            
         BNE   NOEXIT                                                           
         B     YESEXIT                                                          
*                                                                               
NOEXIT   B     NO                                                               
*                                                                               
YESEXIT  B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE OF ACTIONS AND OVERLAYS THAT PROCESS THEM                     *         
***********************************************************************         
         SPACE 1                                                                
ACTTAB   DC    Y(ACDNLREP),X'01'   DOWNLOAD REPORT                              
         DC    AL1(AIALINK)                                                     
         DC    Y(ACDETINQ),X'02'   DETAILED REPORTS INQUIRY                     
         DC    AL1(AIALINK)                                                     
         DC    Y(ACSUMINQ),X'02'   SUMMARIZED REPORT INQUIRY                    
         DC    AL1(AIALINK)                                                     
         DC    Y(ACDNLSTA),X'03'   SPOT DOWNLOAD STATIONS                       
         DC    AL1(0)                                                           
         DC    Y(ACDNLMKT),X'03'   SPOT DOWNLOAD MARKETS                        
         DC    AL1(0)                                                           
         DC    Y(ACDNLCOM),X'03'   SPOT DOWNLOAD STATION COMBOS                 
         DC    AL1(0)                                                           
         DC    Y(ACDNNSTA),X'03'   SPOT DOWNLOAD STATIONS (NEW)                 
         DC    AL1(0)                                                           
         DC    Y(ACDNLDEM),X'04'   RADIO DOWNLOAD DEMOS                         
         DC    AL1(0)                                                           
         DC    Y(ACDNLGOL),X'05'   RADIO DOWNLOAD GOALS                         
         DC    AL1(0)                                                           
         DC    Y(ACUPLBUY),X'06'   RADIO UPLOAD BUYLINES                        
         DC    AL1(0)                                                           
         DC    Y(ACFBRATC),X'26'   RADIO UPLOAD BUYLINES - NEW                  
         DC    AL1(0)                                                           
         DC    Y(ACDATAMG),X'07'   DATAMGR CALLS                                
         DC    AL1(0)                                                           
         DC    Y(ACDNLRCS),X'08'   REP DOWNLOAD CONTRACT SUPPORT DATA           
         DC    AL1(0)                                                           
         DC    Y(ACUPLRCS),X'09'   REP UPLOAD CONTRACTS                         
         DC    AL1(0)                                                           
         DC    Y(ACRXFRDM),X'0A'   REP TRANSFER DEMOS                           
         DC    AL1(0)                                                           
         DC    Y(ACDNLRIN),X'0B'   REP DOWNLOAD INVENTORY DATA                  
         DC    AL1(0)                                                           
         DC    Y(ACDNLRPT),X'0C'   REP DOWNLOAD PAV/TIME PERIOD DATA            
         DC    AL1(0)                                                           
         DC    Y(ACVALSCH),X'0D'   RADIO DOWNLOAD ESTIMATE RECORD               
         DC    AL1(0)                                                           
         DC    Y(ACVALSC2),X'0D'   NEW RADIO DOWNLOAD ESTIMATE RECORD           
         DC    AL1(0)                                                           
         DC    Y(ACVALSC3),X'0D'   NEW RADIO DOWNLOAD ESTIMATE RECORD           
         DC    AL1(0)                                                           
         DC    Y(ACUPLCAB),X'0E'   NET UPLOAD CABLE BUYS                        
         DC    AL1(0)                                                           
         DC    Y(ACDNLMBI),X'0F'   MEDIABASE- DOWNLOAD INDEX                    
         DC    AL1(0)                                                           
         DC    Y(ACDNLMBS),X'0F'   MEDIABASE- DOWNLOAD SUPPLIER                 
         DC    AL1(0)                                                           
         DC    Y(ACUPLMEB),X'10'   MEDLINE - UPLOAD BUYS                        
         DC    AL1(0)                                                           
         DC    Y(ACDNLCAL),X'11'   RADIO DOWNLOAD CALL LETTER DATA              
         DC    AL1(0)                                                           
         DC    Y(ACDNLMEM),X'12'   MEDLINE - DOWNLOAD MASTER DATA               
         DC    AL1(0)                                                           
         DC    Y(ACUPLBUD),X'13'   ACC - UPLOAD BUDGETS                         
         DC    AL1(0)                                                           
         DC    Y(ACUPPINS),X'14'   PRINT - UPLOAD INSERTIONS                    
         DC    AL1(0)                                                           
         DC    Y(ACPRMNTR),X'15'   PRESTO - MONITOR SUPPORT RECORDS             
         DC    AL1(0)                                                           
         DC    Y(ACDNLTMP),X'16'   GENERAL - DOWNLOAD TEMP FILE                 
         DC    AL1(0)                                                           
         DC    Y(ACUPLTMP),X'16'   GENERAL - UPLOAD TEMP FILE                   
         DC    AL1(0)                                                           
         DC    Y(ACDNLAUR),X'17'   RADIO - DOWNLOAD AUR DATA                    
         DC    AL1(0)                                                           
         DC    Y(ACUPSBUY),X'18'   SPOT - UPLOAD BUYS                           
         DC    AL1(0)                                                           
         DC    Y(ACUPNBUY),X'19'   NET - UPLOAD BUYS                            
         DC    AL1(0)                                                           
         DC    Y(ACEXTINQ),X'02'   EXTENDED REPORTS ENQUIRY                     
         DC    AL1(0)                                                           
         DC    Y(ACCOMDNL),X'01'   COMMTEST DOWNLOAD REPORT                     
         DC    AL1(0)                                                           
         DC    Y(ACCOMINQ),X'02'   COMMTEST REPORTS ENQUIRY                     
         DC    AL1(0)                                                           
         DC    Y(ACUPSCRP),X'20'   UPLOAD SCRIPT REQUEST                        
         DC    AL1(0)                                                           
         DC    Y(ACDNSCRP),X'20'   DOWNLOAD SCRIPT RESULT                       
         DC    AL1(0)                                                           
         DC    Y(ACSCRSTA),X'20'   SCRIPT STATUS REQUEST                        
         DC    AL1(0)                                                           
         DC    Y(ACSCRIPT),X'20'   ONE SHOT SCRIPT                              
         DC    AL1(0)                                                           
         DC    Y(ACRNSCRP),X'20'   EXECUTE SCRIPT                               
         DC    AL1(0)                                                           
         DC    Y(ACUPLJOB),X'21'   PRESTO UPLOAD JOB                            
         DC    AL1(0)                                                           
         DC    Y(ACUPLORD),X'22'   PRESTO UPLOAD ORDER                          
         DC    AL1(0)                                                           
         DC    Y(ACORDRES),X'23'   PRESTO ORDER RESERVATION                     
         DC    AL1(0)                                                           
         DC    Y(ACSQLREP),X'24'   SQL REPORT DOWNLOAD                          
         DC    AL1(0)                                                           
         DC    Y(ACTOTINQ),X'02'   TOTAL REPORTS ENQUIRY                        
         DC    AL1(0)                                                           
         DC    Y(ACDISREP),X'01'   VIEW REPORT ONLY                             
         DC    AL1(0)                                                           
         DC    Y(ACSENREP),X'01'   MARK REPORT AS SENT                          
         DC    AL1(0)                                                           
         DC    Y(ACDISALL),X'01'   VIEW REPORT WITH ALL LINES SHOWN             
         DC    AL1(0)                                                           
         DC    Y(ACCBLGOL),X'05'   CABLE DOWNLOAD GOALS                         
         DC    Y(0)                                                             
         EJECT                                                                  
***********************************************************************         
* SAVE ALL VARIABLES AT THE END OF A TRANSACTION                      *         
***********************************************************************         
         SPACE 1                                                                
SAVEALL  NTR1  ,                                                                
         BRAS  RE,SAVEACT          SAVE ACTION, FRAME, ETC.                     
*                                                                               
         L     RE,ATIA             SAVE FIRST APPL COMMON STORAGE               
         LHI   RF,4096              FROM R7                                     
         LR    R0,R7                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA             SAVE SECOND APPL COMMON STORAGE              
         AHI   RE,4096                 FROM R8                                  
         LHI   RF,4096                                                          
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                  WRITE ATIA TO TEMPSTR RECORD 1               
         GOTO1 WRTTWA,CMDMCB,ATIA,1                                             
***********************************************************************         
*        FLUSH TEMPORARY FILE BUFFER IF OPEN FOR PUT                            
***********************************************************************         
         CLI   TMPOFLAG,C'P'       IF TEMP FILE OPEN FOR PUT                    
         BNE   SVX                                                              
*                                  THEN SAVE TMPBUF INTO CURRENT TWA            
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 WRTTWA,CMDMCB,ATMPBUF,CURRTWA                                    
*                                                                               
SVX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SAVES THE CURRENT ACTION, FRAME NUMBER, ETC. FOR THE             
* NEXT TRANSACTION.  IF AN ERROR OCCURED DURING THIS TRANSACTION,               
* THE ROUTINE WILL REINITIALIZE THE ACTION, FRAME NUMBER, ETC.                  
***********************************************************************         
         SPACE 1                                                                
SAVEACT  NTR1  ,                                                                
         CLC   MDACTION,=Y(EQERROR) IF ACTION IS AN ERROR OR ABORT              
         BNL   SA10                                                             
         CLC   MDACTION,=Y(ACABORT)                                             
         BNE   SA20                                                             
*                                                                               
SA10     MVC   SVPCACT,=F'0'       THEN INITIALIZE SAVED PC ACTION              
         MVC   SVMDFRM,=F'0'                             MAD FRAME NUM          
         MVI   SVMDLAST,C' '                             MAD LAST FLAG          
         B     XIT                                                              
*                                                                               
SA20     MVC   SVPCACT,PCACTION    ELSE SAVE PC ACTION                          
         MVC   SVMDFRM,MDFRAME               MAD FRAME NUMBER                   
         MVC   SVMDLAST,MDLAST               MAD LAST FLAG                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DECIDES WHETHER WE CAN USE COMPRESSION FOR MAD SCREENS *         
***********************************************************************         
         SPACE 1                                                                
DOZIP    NTR1  ,                                                                
         L     RF,AUTL             TEST PKZIP + MAD COMPRESSION                 
         TM    TSTAT8-UTLD(RF),TST8BINT                                         
         BZ    NO                                                               
         TM    TSTAT9-UTLD(RF),TST9CMAD                                         
         BZ    NO                                                               
*                                                                               
         CLI   INZIP,C'Y'          DOING A PKZIP?                               
         BE    DOZ02               YES                                          
*                                                                               
         CLC   MDACTION,=Y(ACDNLTMP)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACDNLREP)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACCOMDNL)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACDISALL)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACDISREP)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACDETINQ)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACSUMINQ)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACCOMINQ)                                            
         BE    DOZ02                                                            
         CLC   MDACTION,=Y(ACTOTINQ)                                            
         BE    DOZ02                                                            
         B     NO                                                               
*                                                                               
DOZ02    MVI   INZIP,C'Y'          SET DOING A PKZIP                            
         LA    R1,DMCB                                                          
         USING SHIPBLKD,R1                                                      
         LA    RE,SAVESHIP                                                      
         ST    RE,SHIPSHIP                                                      
*                                                                               
         CLI   OVERMODE,C'S'       FIRST TRANSACTION                            
         BNE   *+8                                                              
         OI    SHIPFLAG,SHIPFFST                                                
         CLC   MDACTION,=H'999'    ERROR TRANSACTIONS                           
         BL    *+8                                                              
         OI    SHIPFLAG,SHIPFEND   LAST FOR THIS COMPRESSION                    
         CLI   MDLAST,C'Y'         LAST TRANSACTION                             
         BNE   *+8                                                              
         OI    SHIPFLAG,SHIPFEND   LAST FOR THIS COMPRESSION                    
         OI    SHIPFLAG,SHIPFVGC   WE HOPE                                      
         GOTO1 SHIPIT,(R1)                                                      
         BE    *+12                                                             
         MVI   INZIP,C'N'          OUT OF ZIP                                   
         B     NO                  AND EXIT                                     
*                                                                               
         L     R0,AOUTFRM          RESET OUTPUT FRAME                           
         LHI   R1,LENOUTF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ACOMP            RESET COMPRESSION BUFFER                     
         LHI   R1,LENCOMP                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ATWA                                                          
         USING TA0CFFD,R4                                                       
         LA    R2,MADDATAH         R2 = A(FIRST FIELD IN DATA FRAME)            
         USING FHD,R2                                                           
         XR    RF,RF                                                            
DOZ04    ICM   RF,1,FHLN           END OF SCREEN?                               
         BZ    YES                 YES                                          
         LHI   R0,FHDAD+1                                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LHI   R0,FHDAD+FHDAD+1                                                 
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA                                                     
         AR    RF,R0                                                            
         MVI   FHOL,0                                                           
         MVI   FHIL,0                                                           
         BXH   R2,RF,DOZ04                                                      
         DC    H'0'                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT FOR THE CONTROLLER ROUTINES AVAILABLE TO CONTROLLER AND           
* OVERLAYS.  UPON ENTRY, RF HOLDS A(CONTCOMM) IN ITS LOW ORDER THREE            
* BYTES AND THE ROUTINE NUMBER IN ITS HIGH ORDER BYTE.  CONTCOMM WILL           
* USE THE ROUTINE NUMBER TO BRANCH TO THE DESIRED ROUTINE.                      
***********************************************************************         
         SPACE 1                                                                
CONTCOMM NTR1  BASE=ABASE1         RB = A(FIRST BASE)                           
         L     RA,ABASE2           RA = A(SECOND BASE)                          
         L     R9,ABASE3           R9 = A(THIRD BASE)                           
*                                                                               
         SRL   RF,24               BRANCH TO DESIRED ROUTINE                    
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
***********************************************************************         
* TABLE OF BRANCH ADDRESSES TO CONTROLLER ROUTINES                              
***********************************************************************         
         SPACE 1                                                                
VBRANCH  B     VCALLSUB                                                         
         B     VGETITEM                                                         
         B     VPUTITEM                                                         
         B     VDECIN                                                           
         B     VREADTWA                                                         
         B     VWRTTWA                                                          
         B     VCONVOFF                                                         
         B     VRD                                                              
         B     VHIGH                                                            
         B     VSEQ                                                             
         B     VWRT                                                             
         B     VADD                                                             
         B     VGETREC                                                          
         B     VPUTREC                                                          
         B     VADDREC                                                          
         B     VSETSYS                                                          
         B     VGETELEM                                                         
         B     VSRCHGET                                                         
         B     VNEXTEL                                                          
         B     VDELELEM                                                         
         B     VSRCHDEL                                                         
         B     VADDELEM                                                         
         B     VTMPOPEN                                                         
         B     VTMPCLOS                                                         
         B     VPUTTMP                                                          
         B     VGETTMP                                                          
         B     VESTFRM                                                          
         B     VWRKCRE                                                          
         B     VWRKPUT                                                          
         B     VWRKCLOS                                                         
         B     VWRKREPT                                                         
         B     VWRKLOC                                                          
         B     VWRKGET                                                          
         B     VWRKREGT                                                         
         B     VWRKSENT                                                         
         B     VWRKCLOT                                                         
         B     VWRKCLOE                                                         
         EJECT                                                                  
***********************************************************************         
* READ OVERLAY INTO PROGRAMS AREA, BAL TO IT, AND WHEN IT RETURNS, PASS         
* CONTROL BACK TO THE CALLING OVERLAY.                                          
***********************************************************************         
         SPACE 1                                                                
VCALLSUB L     R2,0(R1)            R2 = A(OVERLAY NUMBER)                       
         CLI   0(R2),0             EXIT IF OVERLAY NUMBER IS ZERO               
         BE    XIT                                                              
*                                                                               
         XC    CMDMCB(24),CMDMCB   DO A NORMAL FUCKING CALL TO CALLOV           
         MVC   CMDMCB(1),0(R2)                                                  
         GOTO1 CALLOV,CMDMCB                                                    
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                WTFITFP                                      
*                                                                               
         L     RF,CMDMCB                                                        
         BASR  RE,RF               PASS CONTROL TO OVERLAY                      
         B     XIT                 EXIT BACK TO CALLER                          
         EJECT                                                                  
***********************************************************************         
* GET NEXT ITEM FROM INPUT FRAME WORK AREA.                                     
*                                                                               
* GETITEM RETURNS:                                                              
*        TYPENUM  : TYPE NUMBER                                                 
*        DATALEN  : LENGTH OF ITEM DATA                                         
*        ADATA    : A(ITEM DATA)                                                
*                                                                               
* GETITEM MAINTAINS INTERNALLY:                                                 
*        AINPUT    : A(NEXT ITEM TO BE RETRIEVED)                               
*        INPLEFT   : SPACE REMAINING IN DATA FRAME                              
***********************************************************************         
VGETITEM DS    0H                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME FLAG SET THEN ERROR          
         BE    GIEOF                                                            
*                                                                               
         L     R6,AINPUT           POINT R6 TO BEGIN OF NEXT ITEM               
*                                                                               
         CLI   0(R6),C' '          IF TYPE LENGTH IS BLANK THEN SET END         
         BH    GI10                    OF FRAME FLAG AND RETURN                 
         MVI   EIFFLAG,C'Y'                                                     
         B     GINOERR                                                          
*                                  LENGTH OF TYPE MUST BE NUMERIC               
GI10     GOTO1 DECIN,CMDMCB,(R6),1                                              
         BNE   GIINV                                                            
         L     R3,0(R1)            R3 = LENGTH OF TYPE                          
*                                                                               
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
*                                  LENGTH OF LENGTH MUST BE NUMERIC             
         GOTO1 DECIN,CMDMCB,(R6),1                                              
         BNE   GIINV                                                            
         L     R4,0(R1)            R4 = LENGTH OF LENGTH                        
*                                                                               
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
*                                  ALL TYPE BYTES MUST BE NUMERIC               
         GOTO1 DECIN,CMDMCB,(R6),(R3)                                           
         BNE   GIINV                                                            
         MVC   TYPENUM,0(R1)       SAVE TYPE NUMBER                             
*                                                                               
         LA    R6,0(R6,R3)         BUMP PAST                                    
*                                                                               
*                                  ALL LENGTH BYTES MUST BE NUMERIC             
         GOTO1 DECIN,CMDMCB,(R6),(R4)                                           
         BNE   GIINV                                                            
         MVC   DATALEN,0(R1)       SAVE DATA LENGTH                             
*                                                                               
         LA    R6,0(R6,R4)         BUMP PAST                                    
*                                                                               
         MVC   COMPLEN,DATALEN     INIT LEN OF COMP DATA TO DATALEN             
*                                                                               
         ST    R6,ADATA            SET A(DATA)                                  
*                                                                               
         CLC   PCVRS,=H'310'       IF PC VERSION >= 310                         
         BL    *+12                                                             
         BRAS  RE,DECOMP           THEN DECOMPRESS DATA                         
         BNE   GINO                                                             
*                                                                               
         A     R6,COMPLEN          BUMP PAST COMPRESSED DATA                    
*                                                                               
         CLC   0(1,R6),TERMCHR     VERIFY TERMINATOR IS PRESENT                 
         BNE   GITRMNF                                                          
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ST    R6,AINPUT           SAVE A(NEXT ITEM TO GET)                     
*                                                                               
*                                  R2 = TOTAL ITEM LENGTH                       
         LA    R2,2                ADD 2 FOR L'TYPE AND L'LENGTH                
         LA    R2,0(R2,R3)         AND LENGTH OF TYPE                           
         LA    R2,0(R2,R4)         ADD LENGTH OF LENGTH OF DATA                 
         A     R2,COMPLEN          ADD LENGTH OF COMPRESSED DATA                
         LA    R2,1(R2)            ADD ONE FOR '^' TERMINATOR                   
*                                                                               
*                                  TEST FOR ITEM LENGTH PAST END OF             
*                                  DATA FRAME.  IF SO, AUTHOR OF DATA           
*                                  OBJECT MADE FATAL ERROR                      
*                                                                               
         L     RF,INPLEFT          COMPUTE NEW FRAME AVAILABLE                  
         SR    RF,R2                                                            
         ST    RF,INPLEFT                                                       
         BM    GIFRMOV             IF NEGATIVE THEN ERROR                       
*                                                                               
GINOERR  B     GIYES               NO ERROR                                     
*                                                                               
*                                  END OF FRAME ON GET ERROR                    
GIEOF    MVC   MDACTION,=Y(EOFONGET)                                            
         B     GINO                                                             
*                                  INVALID ITEM ERROR                           
GIINV    MVC   MDACTION,=Y(INVITEM)                                             
         B     GINO                                                             
*                                  FRAME OVERFLOW ERROR                         
GIFRMOV  MVC   MDACTION,=Y(FRMOVER)                                             
         B     GINO                                                             
*                                  TERMINATOR NOT FOUND ERROR                   
GITRMNF  MVC   MDACTION,=Y(TRMNOFND)                                            
*                                                                               
GINO     B     NO                                                               
*                                                                               
GIYES    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DECOMPRESSES THE DATA POINTED TO BY ADATA INTO THE               
* COMPRESSION BUFFER, ACOMP.  IN THE PROCESS, THE LENGTH OF THE                 
* COMPRESSED DATA WILL BE DETERMINED AND STORED IN COMPLEN.  ALSO,              
* IF THE SOURCE DATA LENGTH, DETERMINED BY FINDING THE TERMINATOR               
* CHARACTER AT THE END,  DOES NOT MATCH THE DATA LENGTH PART OF THE             
* OBJECT, AN ERROR IS REPORTED.  THE ROUTINE DOES NOT ALLOW DATA                
* LENGTHS GREATER THAT 4096, THE SIZE OF THE COMPRESSION BUFFER.                
* THE ROUTINE RETURNS 'NO' IF AN ERROR HAS OCCURED.  OTHERWISE IT               
* RETURNS 'YES'.                                                                
***********************************************************************         
DECOMP   NTR1                                                                   
         OC    DATALEN,DATALEN     IF NO LENGTH                                 
         BZ    DCYES               THEN NOTHING TO COMPRESS                     
*                                                                               
*                                  ERROR IF DATA LENGTH TOO BIG                 
         CLC   DATALEN,=A(LENCOMP)                                              
         BH    DCOVER                                                           
*                                                                               
         L     R4,ADATA            R4 = A(SOURCE DATA)                          
         L     R3,ACOMP            R3 = A(DEST DATA - COMP BUFFER)              
         LR    R2,R3               R2 = A(END OF COMP BUFFER)                   
         A     R2,=A(LENCOMP)                                                   
*                                                                               
DC10     CR    R3,R2               IF NO MORE ROOM THEN OVERFLOW ERROR          
         BNL   DCOVER                                                           
*                                                                               
         CLI   0(R4),C'?'          IF CHAR NOT ESCAPE CHAR ('?')                
         BE    DC20                                                             
         MVC   0(1,R3),0(R4)       THEN COPY IT TO DEST DATA                    
         LA    R4,1(R4)            BUMP SOURCE AND DEST                         
         LA    R3,1(R3)                                                         
         B     DC300                                                            
*                                                                               
DC20     CLC   1(2,R4),=C'0A'      ELSE IF '?0A'                                
         BNE   DC30                                                             
         MVI   0(R3),DINKCHR       THEN STORE 'DINK' IN DEST DATA               
         B     DC90                                                             
*                                                                               
DC30     CLC   1(2,R4),=C'0B'      ELSE IF '?0B'                                
         BNE   DC40                                                             
         MVI   0(R3),C'+'          THEN STORE '+' IN DEST DATA                  
         B     DC90                                                             
*                                                                               
DC40     CLC   1(2,R4),=C'0C'      ELSE IF '?0C'                                
         BNE   DC50                                                             
         MVI   0(R3),C'?'          THEN STORE '?' IN DEST DATA                  
         B     DC90                                                             
*                                                                               
DC50     B     DC100               ELSE (MUST BE REPEAT SEQUENCE)               
*                                                                               
DC90     LA    R3,1(R3)            BUMP DEST DATA FOR '?' ESCAPES               
         B     DC290               BUMP SOURCE AND LOOP BACK                    
*                                                                               
DC100    ZIC   R1,1(R4)            R1 = REPEAT CODE                             
*                                                                               
         CLI   1(R4),C'4'          IF REPEAT CODE IS '4'-'9'                    
         BL    DC110                                                            
         CLI   1(R4),C'9'                                                       
         BH    DC110                                                            
         LA    R0,C'0'             THEN REPEAT COUNT IS 4-9                     
         B     DC190                                                            
*                                                                               
DC110    CLI   1(R4),C'A'          ELSE IF REPEAT CODE IS 'A'-'I'               
         BL    DC120                                                            
         CLI   1(R4),C'I'                                                       
         BH    DC120                                                            
         LA    R0,C'A'-10          THEN REPEAT COUNT IS 10-18                   
         B     DC190                                                            
*                                                                               
DC120    CLI   1(R4),C'J'          ELSE IF REPEAT CODE IS 'J'-'R'               
         BL    DC130                                                            
         CLI   1(R4),C'R'                                                       
         BH    DC130                                                            
         LA    R0,C'J'-19          THEN REPEAT COUNT IS 19-27                   
         B     DC190                                                            
*                                                                               
DC130    CLI   1(R4),C'S'          ELSE IF REPEAT CODE IS 'S'-'Z'               
         BL    DC140                                                            
         CLI   1(R4),C'Z'                                                       
         BH    DC140                                                            
         LA    R0,C'S'-28          THEN REPEAT COUNT IS 28-35                   
         B     DC190                                                            
*                                                                               
DC140    CLI   1(R4),X'81'         ELSE IF REPEAT CODE IS LOWER 'A'-'I'         
         BL    DC150                                                            
         CLI   1(R4),X'89'                                                      
         BH    DC150                                                            
         LA    R0,X'81'-36         THEN REPEAT COUNT IS 36-44                   
         B     DC190                                                            
*                                                                               
DC150    CLI   1(R4),X'91'         ELSE IF REPEAT CODE IS LOWER 'J'-'R'         
         BL    DC160                                                            
         CLI   1(R4),X'99'                                                      
         BH    DC160                                                            
         LA    R0,X'91'-45         THEN REPEAT COUNT IS 45-53                   
         B     DC190                                                            
*                                                                               
DC160    CLI   1(R4),X'A2'         ELSE IF REPEAT CODE IS LOWER 'S'-'Z'         
         BL    DC170                                                            
         CLI   1(R4),X'A9'                                                      
         BH    DC170                                                            
         LA    R0,X'A2'-54         THEN REPEAT COUNT IS 54-61                   
         B     DC190                                                            
*                                                                               
DC170    B     DCSERR              ELSE DECOMPRESSION SYNTAX ERROR              
*                                                                               
DC190    SR    R1,R0               R1 = REPEAT COUNT                            
*                                                                               
         LR    RF,R3               IF NOT ENOUGH ROOM THEN OVERFLOW ERR         
         AR    RF,R1                                                            
         CR    RF,R2                                                            
         BNL   DCOVER                                                           
*                                                                               
         CLC   2(1,R4),TERMCHR     IF REPEAT CHAR IS TERM CHAR THEN ERR         
         BE    DCSERR                                                           
*                                                                               
DC195    MVC   0(1,R3),2(R4)       ELSE LOOP AND STORE REPEATED CHAR IN         
         LA    R3,1(R3)                DEST BUFFER                              
         BCT   R1,DC195                                                         
*                                                                               
DC290    LA    R4,3(R4)            BUMP SOURCE DATA                             
*                                                                               
DC300    CLC   0(1,R4),TERMCHR     IF NO END OF SOURCE THEN LOOP BACK           
         BNE   DC10                                                             
*                                                                               
         L     RF,ACOMP            IF DECOMPRESSED LENGTH NOT EQUAL             
         SR    R3,RF                   TO OBJECT LENGTH THEN ERROR              
         C     R3,DATALEN                                                       
         BNE   DCDLEN                                                           
*                                                                               
         L     RF,ADATA            SET COMPRESSED LENGTH                        
         SR    R4,RF                                                            
         ST    R4,COMPLEN                                                       
*                                                                               
         MVC   ADATA,ACOMP         SET A(DATA) TO A(COMP)                       
*                                                                               
         B     DCYES               NO ERRORS - RETURN 'YES'                     
*                                                                               
*                                  DECOMPRESSION OVERFLOW                       
DCOVER   MVC   MDACTION,=Y(ER00DCOV)                                            
         B     DCNO                                                             
*                                  DECOMPRESSION SYNTAX ERROR                   
DCSERR   MVC   MDACTION,=Y(ER00DCSE)                                            
         B     DCNO                                                             
*                                  DECOMPRESSED LENGTH INVALID                  
DCDLEN   MVC   MDACTION,=Y(ER00DCDL)                                            
         B     DCNO                                                             
*                                                                               
DCYES    B     YES                                                              
*                                                                               
DCNO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* PUT ITEM INTO OUTPUT FRAME WORK AREA.                                         
*                                                                               
* CALLER PASSES:                                                                
*        PARAMETER 1 : TYPE NUMBER                                              
*        PARAMETER 2 : LENGTH OF ITEM DATA                                      
*        PARAMETER 3 : A(ITEM DATA)                                             
*                                                                               
* PUTITEM MAINTAINS INTERNALLY:                                                 
*        AOUTPUT   : A(NEXT ITEM TO BE ADDED)                                   
*        OUTLEFT   : SPACE REMAINING IN DATA FRAME                              
***********************************************************************         
VPUTITEM DS    0H                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET THEN ERROR          
         BE    PIEOF                                                            
*                                                                               
         MVC   TYPENUM,0(R1)       EXTRACT PARMS INTO WORKING STORAGE           
         MVC   DATALEN,4(R1)                                                    
         MVC   ADATA,8(R1)                                                      
*                                                                               
         MVC   COMPLEN,DATALEN     INIT LEN OF COMP DATA TO DATALEN             
*                                                                               
         OC    DATALEN,DATALEN     IF ZERO DATA LENGTH                          
         BZ    PI10                THEN NO NEED FOR TRANS AND COMP              
*                                                                               
         BRAS  RE,BADCHARS         TRANSLATE ALL BAD CHARS TO ' '               
*                                                                               
         CLC   PCVRS,=H'310'       IF PC VERSION >= 310                         
         BL    *+8                                                              
         BRAS  RE,COMPRESS         THEN COMPRESS DATA                           
*                                                                               
PI10     L     R6,AOUTPUT          POINT R6 TO NEXT AREA TO ADD ITEM            
*                                                                               
*                                  EDIT BINARY TYPENUM INTO WORK AREA           
         EDIT  (4,TYPENUM),(9,TYPEWORK),ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
         LR    R3,R0               R3 = LENGTH OF TYPE                          
*                                                                               
         L     RF,DATALEN          R4 = LENGTH OF DATA LENGTH                   
         BRAS  RE,NUMDIGS                                                       
         LR    R4,RF                                                            
*                                                                               
         CLC   PCVRS,=H'350'       IF PC VERSION >= 350                         
         BNL   *+14                DO NOT COMPRESS TYPE CODES                   
         CLC   PCVRS,=H'320'       IF PC VERSION >= 320                         
         BNL   PI40                COMPRESS TYPE CODES                          
*                                                                               
*                                  R2 = TOTAL ITEM LENGTH                       
         LA    R2,2                ADD 2 FOR L'TYPE + L'LENGTH                  
         LA    R2,0(R2,R3)         ADD LENGTH OF THE TYPE                       
         LA    R2,0(R2,R4)         ADD LENGTH OF THE LENGTH OF THE DATA         
         A     R2,COMPLEN          ADD LENGTH OF THE COMPRESSED DATA            
         LA    R2,1(R2)            ADD ONE FOR '^' TERMINATOR                   
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
         ZIC   RF,=C'0'            SAVE LENGTH OF DATA LENGTH                   
         LA    RF,0(R4,RF)                                                      
         STC   RF,0(R6)                                                         
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         LA    RE,TYPEWORK         SAVE TYPE                                    
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
         LA    R6,0(R6,R3)         BUMP PAST                                    
         B     PI50                                                             
*                                                                               
PI40     EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,=C'0'            SAVE LENGTH OF TYPE                          
         LA    RF,0(R3,RF)                                                      
         STC   RF,WORK                                                          
         ZIC   RF,=C'0'            SAVE LENGTH OF DATA LENGTH                   
         LA    RF,0(R4,RF)                                                      
         STC   RF,WORK+1                                                        
         LA    RE,TYPEWORK         SAVE TYPE                                    
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),0(RE)                                                  
*                                                                               
         LA    R1,2(R3)            R1=LEN OF WORK FIELD                         
         LR    R2,R1                                                            
         BRAS  RE,FINDCOMP                                                      
         TM    WORK,X'F0'          TEST FOR CHR OR NUMERIC                      
         BO    *+8                                                              
         LA    R2,1                                                             
         LR    R1,R2               SET R1 TO EXECUTE LEN OF TYPE                
         BCTR  R1,0                                                             
*                                                                               
         LA    R2,0(R2,R4)         ADD LENGTH OF THE LENGTH OF THE DATA         
         A     R2,COMPLEN          ADD LENGTH OF THE COMPRESSED DATA            
         LA    R2,1(R2)            ADD ONE FOR '^' TERMINATOR                   
*                                                                               
         L     RF,OUTLEFT          COMPUTE NEW FRAME AVAILABLE                  
         SR    RF,R2                                                            
         ST    RF,OUTLEFT                                                       
*                                                                               
         BP    PI45                IF NEW FRAME AVAILABLE WENT NEGATIVE         
         ICM   RF,15,FULL          CLEAR ANY NEW COMPRESSION ENTRY              
         BZ    *+10                                                             
         XC    0(8,RF),0(RF)                                                    
         MVI   EOFFLAG,C'Y'        SET END OF FRAME FLAG                        
         B     PINOERR             AND RETURN                                   
*                                                                               
PI45     EX    R1,*+8              OUTPUT TYPE AND BUMP                         
         B     *+10                                                             
         MVC   0(0,R6),WORK                                                     
         LA    R6,1(R6,R1)                                                      
*                                                                               
PI50     L     RF,DATALEN          SAVE DATA LENGTH                             
         CVD   RF,CMDUB                                                         
         OI    CMDUB+7,X'0F'                                                    
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R6),CMDUB                                                    
         LA    R6,0(R6,R4)         BUMP PAST                                    
*                                                                               
         LR    RE,R6               SAVE DATA                                    
         L     RF,COMPLEN                                                       
         L     R0,ADATA                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         LR    R6,RE               BUMP PAST                                    
*                                                                               
         MVC   0(1,R6),TERMCHR     SAVE TERMINATOR BYTE                         
         LA    R6,1(R6)            BUMP PAST                                    
*                                                                               
         ST    R6,AOUTPUT          SAVE A(NEXT PLACE TO ADD ITEM)               
*                                                                               
PINOERR  B     YES                 NO ERROR RETURN CC EQUAL                     
*                                                                               
*                                  END OF FRAME ON PUT ERROR                    
PIEOF    MVC   MDACTION,=Y(EOFONPUT)                                            
         B     NO                                                               
*                                                                               
*                                                                               
*                                                                               
NUMDIGS  LR    R1,RF               CALCULATE NUMBER OF DIGITS FOR VALUE         
         LA    RF,1                    FOUND IN RF AND RETURN IN RF             
*                                                                               
ND10     SR    R0,R0               DIVIDE BY TEN UNTIL ZERO                     
         D     R0,=F'10'                                                        
         LTR   R1,R1                                                            
         BZR   RE                                                               
         LA    RF,1(RF)            BUMP NUMBER OF DIGITS                        
         B     ND10                                                             
         EJECT                                                                  
***********************************************************************         
*        THIS LOOKS FOR A TYPE MATCH IN THE COMPTAB                             
***********************************************************************         
         SPACE 1                                                                
FINDCOMP NTR1                                                                   
         XC    FULL,FULL           CLEAR FULL FIRST                             
         LA    RF,COMPTAB SEARCH COMPTAB                                        
         LA    R0,26                                                            
         BCTR  R1,0                                                             
FC010    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(RF)       DO WE HAVE A MATCH                           
         BE    FC020                                                            
         OC    0(8,RF),0(RF)       FREE ENTRY                                   
         BZ    FC030                                                            
         LA    RF,8(RF)                                                         
         BCT   R0,FC010            EXIT AFTER 26 TRIES                          
         B     XIT                                                              
*                                                                               
FC020    LA    R1,26               WORK OUT WHICH LETTER AND EXIT               
         SR    R1,R0                                                            
         LA    R1,C'A'(R1)                                                      
         XC    WORK,WORK                                                        
         STC   R1,WORK                                                          
         B     XIT                                                              
*                                                                               
FC030    MVC   0(8,RF),WORK        SAVE THIS ENTRY                              
         ST    RF,FULL             SET FULL TO A(ENTRY)                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS REPLACES ALL NON-PRINTABLE CHARACTERS TO SPACES.                         
***********************************************************************         
BADCHARS NTR1                                                                   
         L     R2,ADATA            R2 = A(DATA TO BE REPLACED)                  
         L     R3,DATALEN          R3 = L(DATA)                                 
*                                                                               
         MVC   CMBLOCK,VALOCHRS    MOVE TRANS TABLE TO CMBLOCK                  
*                                                                               
         CLC   PCVRS,=H'300'       IF VERSION = 300                             
         BNE   *+8                 THEN FILTER THE 'DINK'                       
         MVI   CMBLOCK+X'5F',X'40'                                              
*                                                                               
BC10     LTR   R3,R3               IF NO MORE CHARS LEFT THAN DONE              
         BZ    BCX                                                              
*                                                                               
         C     R3,=F'256'          ELSE IF LESS THAN 256 CHARS LEFT             
         BH    BC20                                                             
         BCTR  R3,0                THEN TRANSLATE WHAT'S LEFT                   
         EX    R3,*+8                                                           
         B     BCX                 AND RETURN                                   
         TR    0(0,R2),CMBLOCK                                                  
*                                                                               
BC20     TR    0(256,R2),CMBLOCK   ELSE TRANSLATE 256 CHARS                     
*                                                                               
         S     R3,=F'256'          R3 = # OF CHARACTERS LEFT TO REPLACE         
         A     R2,=F'256'          R2 = A(NEXT SET TO REPLACE)                  
         B     BC10                LOOP BACK UNTIL NO MORE CHARACTERS           
*                                                                               
BCX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE COMPRESSES OUTGOING DATA OF THE ITEM ABOUT TO BE PUT.            
*                                                                               
* 'DINK' WILL BE REPLACED BY A '?0A'                                            
* '+' WILL BE REPLACED BY A '?0B'                                               
* '?' WILL BE REPLACED BY A '?0C'                                               
*                                                                               
* COMPRESSIONS WILL HAVE THE FOLLOWING FORMAT: ?N*.                             
* ? IS THE ESCAPE CHARACTER                                                     
* N IS THE CHARACTER (4-9, A-Z, AND LOWER CASE A-Z) SIGNIFYING                  
*      THE NUMBER OF TIMES THE CHARACTER IS REPEATED (4-61 MAX)                 
* * IS THE CHARACTER TO BE REPEATED                                             
***********************************************************************         
COMPRESS NTR1                                                                   
         OC    DATALEN,DATALEN     IF NO LENGTH                                 
         BZ    CMX                 THEN NOTHING TO COMPRESS                     
*                                                                               
         L     R5,ADATA            R5 = A(FIRST CHAR IN SOURCE DATA)            
         L     R4,DATALEN          R4 = LENGTH(SOURCE DATA)                     
         L     R3,ACOMP            R3 = A(FIST CHAR IN DEST DATA)               
         SR    R2,R2               R2 = REPEATING CHARACTER                     
         SR    R1,R1               R1 = NUMBER OF REPETITIONS                   
*                                                                               
CM10     CLM   R2,1,0(R5)          IF NEW CHARACTER                             
         BE    CM100                                                            
*                                                                               
         BRAS  RE,COMPREV          THEN COMPRESS PREV REPEATED CHARS            
*                                                                               
         CLI   0(R5),DINKCHR       IF NEW CHARACTER IS A 'DINK'                 
         BNE   CM20                                                             
         MVI   CMBYTE,C'A'         THEN EXPAND IT TO '?0A'                      
         B     CM200                                                            
*                                                                               
CM20     CLI   0(R5),C'+'          ELSE IF NEW CHARACTER IS A '+'               
         BNE   CM30                                                             
         MVI   CMBYTE,C'B'         THEN EXPAND IT TO '?0B'                      
         B     CM200                                                            
*                                                                               
CM30     CLI   0(R5),C'?'          ELSE IF NEW CHARACTER IS A '?'               
         BNE   CM40                                                             
         MVI   CMBYTE,C'C'         THEN EXPAND IT TO '?0C'                      
         B     CM200                                                            
*                                                                               
CM40     IC    R2,0(R5)            ELSE R2 = NEW CHAR                           
         LA    R1,1                     R1 = NUM OF REPS = 1                    
         B     CM300                                                            
*                                                                               
CM100    LA    R1,1(R1)            ELSE (NOT NEW CHAR) BUMP REP COUNT           
*                                                                               
         CH    R1,=H'61'           IF REP COUNT >= 61                           
         BL    CM300                                                            
         BRAS  RE,COMPREV          THEN COMPRESS PREV REPEATED CHARS            
         B     CM300                                                            
*                                                                               
CM200    MVC   0(2,R3),=C'?0'      STORE ESCAPE SEQUENCE ?0* WHERE              
         MVC   2(1,R3),CMBYTE          * IS STORED IN CMBYTE                    
         LA    R3,3(R3)                                                         
*                                                                               
CM300    LA    R5,1(R5)            BUMP R5 TO NEXT BYTE OF SOURCE DATA          
         BCT   R4,CM10             REPEAT UNTIL NO MORE SOURCE DATA             
*                                                                               
         LTR   R1,R1               IF CHARS LEFT OVER TO BE PUT                 
         BZ    *+8                                                              
         BRAS  RE,COMPREV          THEN COMPRESS LEFT OVER DATA                 
*                                                                               
         MVC   ADATA,ACOMP         SET A(DATA) TO A(COMPRESSED BUFFER)          
*                                                                               
         L     RF,ACOMP            SET LEN OF COMPRESSED DATA                   
         SR    R3,RF                                                            
         ST    R3,COMPLEN                                                       
*                                                                               
CMX      B     XIT                                                              
***********************************************************************         
* THIS ROUTINE STORES THE APPROPRIATE COMPRESSION SEQUENCE FOR THE              
* PREVIOUSLY REPEATED CHARACTERS IN THE DESTINATION DATA.                       
***********************************************************************         
COMPREV  DS    0H                                                               
         LTR   R1,R1               IF NO PREVIOUS CHAR                          
         BZ    CPX                 THEN RETURN                                  
*                                                                               
         CH    R1,=H'4'            ELSE IF NUMBER OF REPS < 4                   
         BNL   CP20                                                             
*                                                                               
CP10     STC   R2,0(R3)            THEN LOOP AND SAVE REPEATED CHAR             
         LA    R3,1(R3)                TO DEST DATA                             
         BCT   R1,CP10                                                          
         B     CPX                 AND RETURN                                   
*                                                                               
CP20     CH    R1,=H'10'           ELSE IF NUMBER OF REPS < 10                  
         BNL   CP30                                                             
         LA    R0,C'0'             THEN REPEAT CODE IS FROM '4'-'9'             
         B     CP100                                                            
*                                                                               
CP30     CH    R1,=H'19'           ELSE IF NUMBER OF REPS < 19                  
         BNL   CP40                                                             
         LA    R0,C'A'-10          THEN REPEAT CODE IS FROM 'A'-'I'             
         B     CP100                                                            
*                                                                               
CP40     CH    R1,=H'28'           ELSE IF NUMBER OF REPS < 28                  
         BNL   CP50                                                             
         LA    R0,C'J'-19          THEN REPEAT CODE IS FROM 'J'-'R'             
         B     CP100                                                            
*                                                                               
CP50     CH    R1,=H'36'           ELSE IF NUMBER OF REPS < 36                  
         BNL   CP60                                                             
         LA    R0,C'S'-28          THEN REPEAT CODE IS FROM 'S'-'Z'             
         B     CP100                                                            
*                                                                               
CP60     CH    R1,=H'45'           ELSE IF NUMBER OF REPS < 45                  
         BNL   CP70                                                             
         LA    R0,X'81'-36         THEN REPEAT CODE IS LOWER 'A'-'I'            
         B     CP100                                                            
*                                                                               
CP70     CH    R1,=H'54'           ELSE IF NUMBER OF REPS < 54                  
         BNL   CP80                                                             
         LA    R0,X'91'-45         THEN REPEAT CODE IS LOWER 'J'-'R'            
         B     CP100                                                            
*                                                                               
CP80     LA    R0,X'A2'-54         ELSE REPEAT CODE IS LOWER 'S'-'Z'            
*                                                                               
CP100    MVI   0(R3),C'?'          STORE COMP SEQUENCE IN DEST DATA             
         AR    R0,R1                                                            
         STC   R0,1(R3)                                                         
         STC   R2,2(R3)                                                         
*                                                                               
         LA    R3,3(R3)            BUMP R3 TO NEXT POS IN DEST DATA             
*                                                                               
         SR    R1,R1               CLEAR REPEAT CHAR AND COUNT                  
         SR    R2,R2                                                            
*                                                                               
CPX      BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONVERTS A DECIMAL CHARACTER STRING TO BINARY.  IF ANY           
* OF THE CHARACTERS ARE NOT DECIMAL DIGITS, THE ROUTINE RETURNS 'NO'.           
* OHERWISE, IT RETURNS 'YES'.                                                   
*                                                                               
*     PARAMETER 1 - A(INPUT STRING)                                             
*                   UPON RETURN: FULL WORD BINARY VALUE                         
*     PARAMETER 2 - NUMBER OF BYTES IN INPUT STRING                             
***********************************************************************         
VDECIN   DS    0H                                                               
         LM    R2,R3,0(R1)         R2 = A(INPUT STRING)                         
*                                  R3 = NUMBER OF BYTES IN INPUT STRING         
*                                                                               
DI10     CLI   0(R2),C'0'          TEST BYTE IS NOT NUMERIC                     
         BL    DINO                                                             
         CLI   0(R2),C'9'                                                       
         BH    DINO                                                             
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT BYTE AND TRY AGAIN              
         BCT   R3,DI10                                                          
*                                                                               
         LM    R2,R3,0(R1)         R2 = A(INPUT STRING)                         
*                                  R3 = NUMBER OF BYTES IN INPUT STRING         
*                                                                               
         BCTR  R3,0                CONVERT EBCDIC TO BINARY                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  CMDUB,0(0,R2)                                                    
         CVB   R0,CMDUB                                                         
         ST    R0,0(R1)            SAVE FULL WORD BINARY IN PARAMETER 1         
*                                                                               
DIYES    B     YES                                                              
*                                                                               
DINO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE TWA RECORD NUMBER SPECIFIED BY PARAMETER 2.                               
***********************************************************************         
VREADTWA DS    0H                                                               
         LM    R2,R3,0(R1)         R2 = A(BLOCK OF MEMORY)                      
*                                  R3 = TWA RECORD NUMBER                       
*                                                                               
         SLL   R3,32-8             SHIFT TWA NUM TO R3 HIGH ORDER BYTE          
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
         MVC   CMDMCB+20(2),=C'L=' SET DATAMGR PARM 6 TO READ THE NEW           
         MVC   CMDMCB+22(2),LENTWA     LARGE TEMPSTR RECORD SIZE                
*                                                                               
*                                  CALL DATAMGR TO READ TEMPSTR RECORD          
         GOTO1 DATAMGR,CMDMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R3),(R2)              
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                SERIOUS BUG - NEED CORE DUMP                 
         EJECT                                                                  
***********************************************************************         
* WRITE FROM THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 INTO THE TWA           
* RECORD NUMBER SPECIFIED BY PARAMETER 2.                                       
***********************************************************************         
VWRTTWA  DS    0H                                                               
         LM    R2,R3,0(R1)         R2 = A(BLOCK OF MEMORY)                      
*                                  R3 = TWA RECORD NUMBER                       
*                                                                               
         SLL   R3,32-8             SHIFT TWA NUM TO R3 HIGH ORDER BYTE          
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
*                                  CALL DATAMGR TO WRITE TEMPSTR RECORD         
         GOTO1 DATAMGR,CMDMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R3),(R2),0             
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                SERIOUS BUG - NEED CORE DUMP                 
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONVERTS AN OFFICE CODE FROM 2 TO 8 BYTE FORMAT OR FROM          
* 8 TO 2 BYTE FORMAT.  IT DOES SO BY READING THE CONTROL FILE FOR THE           
* OFFICE RECORD.  IF IT CANNOT FIND THE OFFICE RECORD IT RETURNS 'NO'.          
* OTHERWISE IT DOES THE CONVERSION AND RETURNS 'YES'.                           
*                                                                               
*        PARM 1 - A(INPUT OFFICE CODE)                                          
*                 BYTE 0    0 = CONVERT FROM 2 TO 8 BYTE FORMAT                 
*                           1 = CONVERT FROM 8 TO 2 BYTE FORMAT                 
*        PARM 2 - A(OUTPUT OFFICE CODE)                                         
***********************************************************************         
VCONVOFF DS    0H                                                               
         LM    R2,R3,0(R1)         R2 = A(INPUT OFFICE CODE)                    
*                                  R3 = A(OUTPUT OFFICE CODE)                   
         MVC   CMBYTE,0(R1)        CMBYTE = CONVERSION TYPE                     
*                                                                               
         LA    R4,KEY              BUILD CONTROL FILE USER ID KEY               
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
*                                                                               
         CLI   CMBYTE,0            IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO10                                                             
         XC    CTIKID(8),CTIKID    THEN PUT 2 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),0(R2)                                                
         B     CO20                                                             
*                                                                               
CO10     MVC   CTIKID(8),0(R2)     ELSE PUT 8 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),=CL2' '                                              
*                                  READ RECORD                                  
CO20     GOTO1 DATAMGR,CMDMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                 
         CLI   8(R1),0                                                          
         BNE   COIDREAD            DISK ERROR                                   
*                                                                               
         L     R4,AIO              R4 = A(RECORD)                               
*                                                                               
         CLC   KEY(25),0(R4)       IF KEY DOESN'T MATCH RECORD                  
         BNE   COIDREAD            THEN ERROR RECORD NOT FOUND                  
*                                                                               
         LA    R4,CTIDATA          R4 = A(RECORD DATA)                          
         USING CTDSCD,R4                                                        
*                                                                               
CO30     CLI   0(R4),0             IF REACHED END OF RECORD THEN ERROR          
         BE    COBADID                                                          
         CLI   0(R4),X'02'         ELSE IF FOUND DESC ELEMENT THEN DONE         
         BE    CO40                                                             
         ZIC   R0,1(R4)            ELSE BUMP TO NEXT ELEMENT                    
         AR    R4,R0                                                            
         B     CO30                LOOP BACK                                    
*                                                                               
CO40     CLI   CMBYTE,0            IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO50                                                             
         MVC   0(8,R3),CTDSC       RETURN 8 BYTE FORMAT IN PARM 2               
         B     COYES                                                            
*                                                                               
CO50     MVC   0(2,R3),CTDSC       ELSE RETURN 2 BYTE FORMAT IN PARM 2          
         DROP  R4                                                               
*                                                                               
COYES    B     YES                                                              
*                                  DISK ERROR WHILE READING ID RECORD           
COIDREAD MVC   MDACTION,=Y(IDREAD)                                              
*                                                                               
*                                  BAD ID RECORD                                
COBADID  MVC   MDACTION,=Y(BADIDREC)                                            
*                                                                               
CONO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THE FOLLOWING FUNCTIONS ALLOW THE CALLER TO INTERFACE WITH DATAMGR.           
* THEY REQUIRE NO ARGUMENTS, BUT INSTEAD USE THE GLOBAL DATA AREAS SUCH         
* AS KEY, AIO, SYSDIR, AND SYSFIL AS ARGUMENTS TO DATAMGR.                      
***********************************************************************         
VRD      DS    0H                  SET COMMAND TO 'DMREAD'                      
         MVC   COMMAND,=CL8'DMREAD'                                             
         MVC   KEYSAVE,KEY         BACK UP REQUEST KEY IN KEYSAVE               
         CLC   SYSFIL,=CL8'ACCMST'                                              
         BNE   ALLDIR                                                           
         MVC   KEYSAVE,BIGKEY      USE BIGKEY FOR ACCDIR                        
         B     ALLDIR                                                           
*                                                                               
VHIGH    DS    0H                  SET COMMAND TO 'DMRDHI'                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY         BACK UP REQUEST KEY IN KEYSAVE               
         CLC   SYSFIL,=CL8'ACCMST'                                              
         BNE   ALLDIR                                                           
         MVC   KEYSAVE,BIGKEY      USE BIGKEY FOR ACCDIR                        
         B     ALLDIR                                                           
*                                                                               
VSEQ     DS    0H                  SET COMMAND TO 'DMRSEQ'                      
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     ALLDIR                                                           
*                                                                               
VWRT     DS    0H                  SET COMMAND TO 'DMWRT'                       
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     ALLDIR                                                           
*                                                                               
VADD     DS    0H                  SET COMMAND TO 'DMADD'                       
         MVC   COMMAND,=CL8'DMADD'                                              
*                                                                               
ALLDIR   NI    DMINBTS,X'7F'       IF CALLER WANTS READ FOR UPDATE THEN         
         CLI   RDUPDATE,C'Y'           SET BIT IN DMINBTS                       
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
*                                                                               
         LA    R4,KEY              R4 = A(KEY)                                  
*                                                                               
         CLC   SYSFIL,=CL8'ACCMST' SET PARAMETERS SPECIAL FOR ACCDIR            
         BNE   AD5                                                              
         LA    R4,BIGKEY                                                        
         LA    R2,SYSDIR                                                        
         LA    R3,BIGKEY                                                        
         B     AD20                                                             
*                                                                               
AD5      CLI   ISDAFILE,C'Y'       IF THE FILE IS A DIRECT ACCESS FILE          
         BNE   AD10                    (DIRECTORY/FILE) THEN USE THE            
         LA    R2,SYSDIR                DIRECTORY NAME AND RETURN THE           
         LA    R3,KEY                   DATA IN KEY                             
         B     AD20                                                             
*                                                                               
AD10     LA    R2,SYSFIL           ELSE USE THE FILE NAME AND RETURN            
         L     R3,AIO                  THE DATA IN AIO                          
*                                                                               
*                                  CALL DATAMGR                                 
AD20     GOTO1 DATAMGR,CMDMCB,(DMINBTS,COMMAND),(R2),(R4),(R3),0,0              
*                                                                               
         CLI   ISDAFILE,C'Y'       IF FILE IS ISDA THEN COPY KEY FROM           
         BE    *+14                    IO AREA INTO KEY                         
         L     R3,AIO                                                           
         MVC   KEY,0(R3)                                                        
*                                                                               
         B     DMCHECK             TEST DATAMGR RETURN CODES                    
*                                                                               
VGETREC  DS    0H                  SET COMMAND TO 'GETREC'                      
         MVC   COMMAND,=CL8'GETREC'                                             
         LA    R3,KEY              R3 = A(DISK ADDRESS)                         
         CLC   SYSFIL,=CL8'ACCMST'                                              
         BNE   *+8                                                              
         LA    R3,BIGKEY           USE BIGKEY FOR ACCDIR                        
         ZIC   R0,DISPDSKA                                                      
         AR    R3,R0                                                            
         B     ALLFIL                                                           
*                                                                               
VPUTREC  DS    0H                  SET COMMAND TO 'PUTREC'                      
         MVC   COMMAND,=CL8'PUTREC'                                             
         LA    R3,DMWORK+4         R3 = A(DISK ADDRESS)                         
         B     ALLFIL                                                           
*                                                                               
VADDREC  DS    0H                  SET COMMAND TO 'ADDREC'                      
         MVC   COMMAND,=CL8'ADDREC'                                             
         LA    R3,KEY              R3 = A(WHERE TO RETURN DISK ADRESS)          
*                                                                               
ALLFIL   NI    DMINBTS,X'7F'       IF CALLER WANTS READ FOR UPDATE THEN         
         CLI   RDUPDATE,C'Y'           SET BIT IN DMINBTS                       
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
*                                  CALL DATAMGR                                 
         GOTO1 DATAMGR,CMDMCB,(DMINBTS,COMMAND),SYSFIL,(R3),AIO,DMWORK,X        
               APRDLIST                                                         
         XC    APRDLIST,APRDLIST   TRY TO PROTECT AGAINST GARBAGE               
*                                                                               
DMCHECK  TM    CMDMCB+8,X'20'      IF DATAMGR ERROR IS DUPLICATE KEY            
         BZ    *+6                     ON ADD THEN CAUSE A PROGRAM              
         DC    H'0'                    FAILURE TO UNWIND TRANSACTION            
*                                                                               
DMX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ACCEPTS AS ARGUMENTS A SYSTEM NAME IN EITHER LONG OR             
* SHORT FORM, A DIRECTORY NAME, AND A FILE NAME.  IT SWITCHES TO THE            
* SYSTEM WITH THE GIVEN SYSTEM NAME AND SAVES THE DIRECTORY AND FILE            
* NAMES IN GLOBAL STORAGE FOR USE IN DATAMGR CALLS.  IF THE DIRECTORY           
* NAME GIVEN IS ZERO, THEN THE ISDAFILE FLAG IS SET TO 'N' TO INDICATE          
* THAT THE FILE IS NOT DIRECT ACCESS.  THIS FLAG IS USED IN DATAMGR             
* DIRECTORY CALLS.  IF THE DIRECTORY NAME IS NOT ZERO, THEN ISDAFILE            
* IS SET TO 'Y'.  THE ROUTINE WILL VALIDATE THE FILE NAME AND THE               
* DIRECTORY NAME AND SAVE INFORMATION ABOUT THEM IN GLOBAL STORAGE.             
* IF THERE IS ANY PROBLEM, THE ROUTINE RETURNS 'NO'.  OTHERWISE, IT             
* RETURNS 'YES'.                                                                
*                                                                               
*        PARAMETER 1 - A(SYSTEM NAME - SHORT OR LONG)                           
*                      BYTE 0 - LENGTH OF SYSTEM NAME                           
*        PARAMETER 2 - A(DIRECTORY NAME) OS ZEROS IF NOT DIRECT ACCESS          
*        PARAMETER 3 - A(FILE NAME)                                             
***********************************************************************         
VSETSYS  DS    0H                                                               
         LM    R2,R4,0(R1)         R2 = A(SYSTEM NAME)                          
*                                  R3 = A(DIRECTORY NAME)                       
*                                  R4 = A(DIRECTORY NAME)                       
         MVC   CMBYTE,0(R1)        CMBYTE = LENGTH OF SYSTEM NAME               
*                                                                               
         LA    R5,SYSLST           R5 = A(FIRST ENTRY IN TABLE OF               
         LA    R5,6(R5)                   SYSTEM NAMES AND NUMBERS)             
         USING SYSLSTD,R5                                                       
*                                                                               
SY10     CLI   0(R5),0             IF END OF TABLE REACHED THEN ERROR           
         BE    SYSYSNAM                                                         
*                                                                               
         ZIC   RF,CMBYTE         RF = LENGTH TO COMPARE SYSTEM NAME             
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,SYCMPLNG         IF SYSTEM NAME MATCHES LONG NAME             
         BE    SY20                    THEN DONE                                
         EX    RF,SYCMPSHT         IF SYSTEM NAME MATCHES SHORT NAME            
         BE    SY20                    THEN DONE                                
*                                                                               
         LA    R5,SYSLLEN(R5)      BUMP R5 TO NEXT SYSTEM NAME ENTRY            
         B     SY10                                                             
*                                                                               
SYCMPLNG CLC   0(0,R2),SYSLNAME    COMPARE INSTRUCTION FOR LONG NAME            
SYCMPSHT CLC   0(0,R2),SYSLSHRT                            SHORT NAME           
*                                                                               
SY20     MVC   CMBYTE,SYSLNUM      SAVE GLOBAL SYSTEM NUMBER IN CMBYTE          
         DROP  R5                                                               
*                                                                               
         LA    R5,SYSLIST          R5 = A(FIRST SYSTEM SE LIST ENTRY)           
         USING SYSLISTD,R5                                                      
*                                                                               
SY30     CLI   0(R5),0             IF END OF TABLE REACHED THEN ERROR           
         BE    SYINVSYS                                                         
*                                                                               
         CLC   SYSNUMB,CMBYTE      IF MATCHES GLOBAL SYSTEM NUMBER              
         BE    SY40                    THEN DONE                                
*                                                                               
         LA    R5,SYSLISTL(R5)     BUMP R5 TO NEXT SYSTEM SE LIST ENTRY         
         B     SY30                                                             
*                                                                               
SY40     CLC   SYSNUM,SYSSE        IF SYSTEM SE NUMBER IS NOT THE SAME          
         BE    SY50                    AS THE CURRENT SYSTEM                    
*                                                                               
         MVC   SYSNUM,SYSSE        THEN SET CURRENT TO NEW SYSTEM SE            
         DROP  R5                                                               
*                                                                               
         MVC   CMDMCB(1),SYSNUM    SWITCH TO NEW SYSTEM                         
         MVC   CMDMCB+1(3),=X'FFFFFF'                                           
         GOTO1 SWITCH,CMDMCB,,0                                                 
         CLI   4(R1),0                                                          
         BNE   SYSWITCH                                                         
*                                                                               
SY50     LTR   R3,R3               IF DIRECTORY NAME GIVEN                      
         BZ    SY60                                                             
         MVI   ISDAFILE,C'Y'       THEN SET ISDAFILE TO 'Y'                     
         MVC   SYSDIR,0(R3)        SAVE DIRECTORY NAME IN GLOBAL                
         B     SY70                                                             
*                                                                               
SY60     MVI   ISDAFILE,C'N'       ELSE SET ISDAFILE TO 'N'                     
*                                                                               
SY70     MVC   SYSFIL,0(R4)        SAVE FILE NAME IN GLOBAL                     
*                                                                               
         L     R5,HELEN            R5 = A(FILE NAME TABLE)                      
*                                                                               
SY100    CLI   0(R5),X'FF'         IF END OF TABLE THEN ERROR                   
         BE    SYBADFIL                                                         
*                                                                               
         CLC   SYSFIL,1(R5)        IF MATCH FOUND THEN DONE                     
         BE    SY110                                                            
*                                                                               
         LA    R5,13(R5)           BUMP R5 TO NEXT FILE NAME ENTRY              
         B     SY100                                                            
*                                                                               
SY110    MVC   MAXRSIZE,9(R5)      SAVE MAX RECORD SIZE IN GLOBAL               
         MVC   DISPELEM,11(R5)          DISPLACEMENT TO FIRST ELEMENT           
         MVC   DISPRLEN,12(R5)          DISPLACEMENT TO RECORD LENGTH           
*                                                                               
         CLI   ISDAFILE,C'Y'       IF FILE IS DIRECT ACCESS                     
         BNE   SY140                                                            
*                                                                               
         L     R5,HELENDIR         THEN R5 = A(DIRECTORY NAME TABLE)            
*                                                                               
SY120    CLI   0(R5),X'FF'         IF END OF TABLE THEN ERROR                   
         BE    SYBADDIR                                                         
*                                                                               
         CLC   SYSDIR,1(R5)        IF MATCH FOUND THEN DONE                     
         BE    SY130                                                            
*                                                                               
         LA    R5,11(R5)           BUMP R5 TO NEXT DIR NAME ENTRY               
         B     SY120                                                            
*                                                                               
SY130    MVC   LENKEY,9(R5)        SAVE LENGTH OF KEY IN GLOBAL                 
         MVC   DISPDSKA,10(R5)          DISPLACEMENT TO DISK ADDRESS            
*                                                                               
SY140    DS    0H                                                               
*                                                                               
SYYES    B     YES                                                              
*                                  BAD SYSTEM NAME                              
SYSYSNAM MVC   MDACTION,=Y(SYSNAM)                                              
         B     SYNO                                                             
*                                  BAD SYSTEM NUMBER                            
SYINVSYS MVC   MDACTION,=Y(INVSYS)                                              
         B     SYNO                                                             
*                                  ERROR WHILE SWITCHING SYSTEMS                
SYSWITCH MVC   MDACTION,=Y(ERSWITCH)                                            
         B     SYNO                                                             
*                                  BAD FILE NAME                                
SYBADFIL MVC   MDACTION,=Y(BADFIL)                                              
         B     SYNO                                                             
*                                  BAD DIRECTORY NAME                           
SYBADDIR MVC   MDACTION,=Y(BADDIR)                                              
*                                                                               
SYNO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THESE TWO ROUTINES POINT R6 TO THE FIRST ELEMENT IN AIO WITH THE              
* ELEMENT CODE SPECIFIED IN PARAMETER ONE.  SRCHGET FURTHER RESTRICTS           
* THE SEARCH TO MATCH THE ELEMENT'S DATA (STARTING AFTER THE ELEMENT            
* LENGTH) WITH THE SEARCH ARGUMENT SPECIFIED IN PARAMETER TWO.  FOR             
* EITHER ROUTINE, IF THE ELEMENT IS FOUND, IT RETURNS 'YES'.                    
* OTHERWISE, IT RETURNS 'NO'.                                                   
*                                                                               
*        PARAMETER 1 - ELEMENT CODE                                             
*        PARAMETER 2 (FOR SRCHGET ONLY) - A(SEARCH ARGUMENT)                    
*                                         BYTE 0 IS L(SEARCH ARGUMENT)          
*                                                                               
* ADDED 6/2/93:                                                                 
*    CODE NOW SAVES FOR SRCHLEN AND SRCHDATA IF CALL IS TO SRCHGET.             
*    NEXTELEM WILL USE THESE VALUE TO FILTER ELEMENTS.                          
***********************************************************************         
VGETELEM DS    0H                                                               
         SR    R3,R3               CLEAR R3 AND R4 TO MAKE HELLO USE NO         
         SR    R4,R4                   SEARCH ARGUMENT                          
         MVI   SRCHLEN,0           CLEAR SRCHLEN SO NEXTELEM DOESN'T            
         B     ALLGET                  FILTER                                   
*                                                                               
VSRCHGET DS    0H                                                               
         L     R3,4(R1)            R3 = A(SEARCH ARGMENT)                       
         ZIC   R4,4(R1)            R4 = LENGTH OF SEARCH ARGUMENT               
*                                                                               
         MVC   SRCHLEN,4(R1)       SAVE SEARCH LENGTH AND DATA SO               
         LR    RE,R3                   NEXTELEM FILTERS                         
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCHDATA(0),0(RE)                                                
*                                                                               
ALLGET   L     R2,0(R1)            R2 = ELEMENT CODE                            
         STC   R2,CMELCODE         SAVE CODE IN CONTROLLER GLOBAL               
*                                                                               
*                                  CALL HELLO TO GET ELEMENT                    
         GOTO1 HELLO,CMDMCB,(C'G',SYSFIL),((R2),AIO),((R4),(R3))                
*                                                                               
         L     R6,12(R1)           RETURN A(ELEMENT) IN R6                      
*                                                                               
         CLI   12(R1),0            RETURN 'YES' IF ELEMENT FOUND AND            
         B     XIT6                    'NO' IF NOT FOUND                        
         EJECT                                                                  
***********************************************************************         
* R6 SHOULD POINT TO A VALID RECORD ELEMENT.  UPON RETURN, R6 WILL              
* POINT TO THE NEXT ELEMENT.                                                    
*                                                                               
* THIS ROUTINE MUST BE USED ONLY AFTER FIRST MAKING AN INITIAL CALL TO          
* GETELEM.  THE ELCODE PASSED TO GETELEM, WHICH IS SAVED INTERNALLY, IS         
* USED IN THIS ROUTINE TO DECIDE WHICH ELEMENT IS NEXT.  IF THE ELCODE          
* WAS ZERO, THEN THE VERY NEXT ELEMENT IS RETURNED.  OTHERWISE, IT              
* CONTINUES BUMPING THROUGH THE RECORD UNTIL AN ELEMENT WITH THE SAME           
* CODE IS FOUND.  IF THE END OF THE RECORD IS REACHED BEFORE THE                
* ELEMENT IS FOUND, THE ROUTINE RETURNS 'NO'.  OTHERWISE, IT RETURNS            
* 'YES'.                                                                        
*                                                                               
* ADDED 6/2/93:                                                                 
*    CODE NOW CHECKS FOR SRCHLEN AND SRCHDATA, VALUES SET IF ORIGINAL           
*    CALL WAS SRCHGET.  SRCHGET SETS THESE VALUE FROM THE VALUES                
*    PASSED TO IT.  THIS ROUTINE USES THEM AS A FILTER.                         
***********************************************************************         
VNEXTEL  DS    0H                                                               
         CLI   0(R6),0             IF END OF RECORD THEN RETURN 'NO'            
         BZ    NENO                                                             
*                                                                               
NE10     ZIC   R0,1(R6)            BUMP R6 TO NEXT ELEMENT                      
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             IF END OF RECORD REACHED THEN RETURN         
         BE    NENO                    'NO'                                     
*                                                                               
         CLI   CMELCODE,0          ELSE IF ELCODE IS ZERO THEN DONE             
         BE    NEYES                                                            
*                                                                               
         CLC   CMELCODE,0(R6)      ELSE IF MATCH NOT FOUND THEN BUMP            
         BNE   NE10                    TO NEXT ELEMENT                          
*                                                                               
         CLI   SRCHLEN,0           IF ORIGINAL SEARCH WAS SRCHGET               
         BE    NEYES                                                            
         ZIC   RF,SRCHLEN          THEN SKIP ELEMENT IF DATA DOES               
         BCTR  RF,0                    NOT MATCH FILTER                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SRCHDATA(0),2(R6)                                                
         BNE   NE10                                                             
*                                                                               
NEYES    B     YES6                ELSE RETURN 'YES' AND R6                     
*                                                                               
NENO     B     NO6                                                              
         EJECT                                                                  
***********************************************************************         
* THESE TWO ROUTINES DELETE THE ELEMENT IN AIO WITH THE ELEMENT CODE            
* SPECIFIED IN PARAMETER ONE.  SRCHDEL DELETES ONLY THE ELEMENT                 
* WHOSE ELEMENT DATA (STARTING AFTER THE ELEMENT LENGTH) MATCHES THE            
* SEARCH ARGUMENT SPECIFIED IN PARAMETER TWO.  DELELEM DELETES ALL              
* ELEMENTS WITH THE SPECIFIED ELEMENT CODE.                                     
*                                                                               
*        PARAMETER 1 - ELEMENT CODE                                             
*        PARAMETER 2 (FOR SRCHDEL ONLY) - A(SEARCH ARGUMENT)                    
***********************************************************************         
VDELELEM DS    0H                                                               
         SR    R3,R3               CLEAR R3 AND R4 TO MAKE HELLO USE NO         
         SR    R4,R4                   SEARCH ARGUMENT                          
         B     ALLDEL                                                           
*                                                                               
VSRCHDEL DS    0H                                                               
         L     R3,4(R1)            R3 = A(SEARCH ARGMENT)                       
         ZIC   R4,4(R1)            R4 = LENGTH OF SEARCH ARGUMENT               
*                                                                               
ALLDEL   L     R2,0(R1)            R2 = ELEMENT CODE                            
*                                                                               
*                                  CALL HELLO TO DELETE ELEMENT                 
         GOTO1 HELLO,CMDMCB,(C'D',SYSFIL),((R2),AIO),((R4),(R3))                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADDS THE ELEMENT FOUND IN PARAMETER ONE TO THE RECORD            
* IN AIO.  IF THE RECORD OVERFLOWS, THE ROUTINE CAUSES A PROGRAM                
* FAILURE TO PREVENT ANY BAD RECORDS FROM ENDING UP ON THE FILE.                
* (IF THE ROUTINE RETURNED A CONDITION CODE, THE PROGRAMMER WOULD               
* PROBABLY IGNORE IT).                                                          
***********************************************************************         
VADDELEM DS    0H                                                               
         L     R2,0(R1)            R2 = A(ELEMENT TO ADD)                       
*                                                                               
*                                  CALL HELLO TO ADD ELEMENT                    
         GOTO1 HELLO,CMDMCB,(C'P',SYSFIL),AIO,(R2),0,0                          
*                                                                               
         CLI   12(R1),0            DIE IF RECORD OVERFLOWS TO PREVENT           
         BE    XIT                     ACCIDENTALLY ADDING BAD RECORDS          
         DC    H'0'                    TO THE FILE                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE OPENS THE TEMPORARY FILE FOR GETS/PUTS BASED ON THE              
* FIRST PARAMETER.  IF THE FIRST BYTE OF PARAMETER IS ANYTHING OTHER            
* THAN A C'S', THEN PUTTMP IS ALLOWED TO WRITE RECORDS ACROSS THE               
* TEMPSTR RECORD.  THE TEMPORARY FILE METHOD (TMPMETHD) IS SET TO THE           
* FIRST BYTE OF PARAMETER 1.  IT ALSO MAKES SURE THAT THE TEMP FILE             
* HASN'T ALREADY BEEN OPENED, REWINDS OUR FILE POINTER (CURRTWA AND             
* DNEXTTMP), AND SETS THE TEMPORARY FILE OPEN FLAG TO C'G' OR C'P' TO           
* INDICATE WHAT THE TEMPORARY FILE WAS OPENED FOR.                              
*                                                                               
* IF THE FILE WAS OPENED FOR A GET, THE PARAMETER SHOULD CONTAIN THE            
* SIZE OF THE BUFFER TO BE USED FOR THE GETTMP CALLS.  AN ERROR WILL            
* BE RETURNED IF THE BUFFER IS SMALLER THAN THE SIZE OF THE LARGEST             
* RECORD (TMPMAX) THAT WAS 'PUT' IN OUR TEMPORARY FILE.  THIS PROTECTS          
* THE CALLER FROM OVERFLOWING THE BUFFER AND KILLING THE PROCESS.               
*                                                                               
* ON ENTRY:    PARAM 1   BYTE 0    C'S' - SPECIAL METHOD FOR PUTTMP             
*                                         SO RECORDS WILL NOT GO ACROSS         
*                                         TEMPSTR RECORD BOUNDARY               
*                                                                               
*                        BYTE 1-3  A(COMMAND) : =C'GET'  OR  =C'PUT'            
*                                               =C'GTX'  OR  =C'PTX'            
*                                                                               
*              PARAM 2  (FOR GET)  SIZE OF BUFFER TO BE USED                    
*                                                                               
*              PARAM 3  BYTE 0     START TWA PAGE NUMBER (IF COMMAND            
*                                  IS 'GTX' OR 'PTX')                           
*                                                                               
* ON EXIT:     TMPMETHD      C     C'S' - SPECIAL METHOD FOR PUTTMP             
*                                         SO RECORDS WILL NOT GO ACROSS         
*                                         TEMPSTR RECORD BOUNDARY               
*                                                                               
*              CURRTWA       F     FILE POINTER: CURRENT TWA                    
*                                                                               
*              DNEXTTMP      F     FILE POINTER: DISP IN CURRTWA                
*                                                                               
*              TMPMAX        F     LENGTH OF LARGEST RECORD PUT,                
*                                   CLEARED WHEN 1ST OPENED FOR PUT             
*                                                                               
*              EOTFLAG       C     END OF TEMP FILE FLAG,                       
*                                   SET TO 'N' WHEN 1ST OPENED FOR GET          
*                                                                               
*              TMPOFLAG      C     TEMP FILE OPEN FOR (G)ET OR (P)UT            
***********************************************************************         
VTMPOPEN DS    0H                                                               
         CLI   TMPOFLAG,C'N'       ERROR IF TEMP FILE ALREADY OPEN              
         BNE   TOALROP                                                          
*                                                                               
         L     R2,0(R1)            R2 = PARAMETER 1                             
         L     R3,4(R1)            R3 = PARAMETER 2                             
*                                                                               
         MVC   TMPMETHD,0(R1)      COPY WHICH PUT METHOD TO USE, SO FAR         
*                                  C'S' IS THE ONLY USED PUT METHOD             
*                                                                               
         MVC   CURRTWA,=F'2'       REWIND FILE TWA NUMBER                       
         XC    DNEXTTMP,DNEXTTMP               DISP WITHIN TEMP BUFFER          
*                                                                               
         CLC   0(3,R2),=C'GTX'     IF SPECIAL GET OR PUT OPEN                   
         BE    *+14                                                             
         CLC   0(3,R2),=C'PTX'                                                  
         BNE   TO5                                                              
*                                                                               
         XC    CURRTWA,CURRTWA     THEN GET START TWA NUMBER FROM PARM3         
         MVC   CURRTWA+3(1),8(R1)                                               
*                                                                               
TO5      CLC   0(3,R2),=C'GET'     IF OPENING FOR GET                           
         BE    *+14                                                             
         CLC   0(3,R2),=C'GTX'                                                  
         BNE   TO10                                                             
*                                                                               
         OC    TMPMAX,TMPMAX       THEN ERROR IF TEMP FILE EMPTY                
         BZ    TOEMPTY                                                          
*                                                                               
         C     R3,TMPMAX           ERROR IF BUFFER SIZE TOO SMALL               
         BL    TOBUFSM                                                          
*                                  READ FIRST TWA INTO TMPBUF                   
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 READTWA,CMDMCB,ATMPBUF,CURRTWA                                   
*                                                                               
         MVI   EOTFLAG,C'N'        SET END OF TEMP FILE TO FALSE                
         MVI   TMPOFLAG,C'G'       TEMP FILE OPEN FOR GET                       
         B     TOYES                                                            
*                                                                               
TO10     CLC   0(3,R2),=C'PUT'     ELSE IF OPENING FOR PUT                      
         BE    *+14                                                             
         CLC   0(3,R2),=C'PTX'                                                  
         BNE   TOPARM1                                                          
*                                                                               
         XC    TMPMAX,TMPMAX       THEN CLEAR LARGEST RECORD PUT                
         MVI   TMPOFLAG,C'P'       TEMP FILE OPEN FOR PUT                       
*                                                                               
         B     TOYES               RETURN 'YES'                                 
*                                                                               
*                                  TEMP FILE ALREADY OPEN                       
TOALROP  MVC   MDACTION,=Y(ER00TOAO)                                            
         B     TONO                                                             
*                                  TEMP FILE EMPTY ON GET                       
TOEMPTY  MVC   MDACTION,=Y(ER00TOEM)                                            
         B     TONO                                                             
*                                  USER TEMP FILE BUFFER SIZE TOO SMALL         
TOBUFSM  MVC   MDACTION,=Y(ER00TOBS)                                            
         B     TONO                                                             
*                                  TMPOPEN PARAMETER 1 BAD                      
TOPARM1  MVC   MDACTION,=Y(ER00TOP1)                                            
*                                                                               
TONO     B     NO                                                               
*                                                                               
TOYES    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CLOSES THE TEMPORARY FILE.                                       
***********************************************************************         
VTMPCLOS DS    0H                                                               
         CLI   TMPOFLAG,C'N'       ERROR IF TEMP FILE NOT OPEN                  
         BE    TCNOTOP                                                          
*                                                                               
         CLI   TMPOFLAG,C'P'       IF CLOSING TEMP FILE FOR PUT                 
         BNE   TC10                                                             
*                                  THEN PUT END OF FILE OBJECT                  
         GOTO1 PUTTMP,CMDMCB,CMBYTE,0                                           
*                                  FLUSH TEMP FILE BUFFER                       
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 WRTTWA,CMDMCB,ATMPBUF,CURRTWA                                    
         BNE   TCNO                                                             
*                                                                               
TC10     MVI   TMPOFLAG,C'N'       SET TEMP FILE NOT OPEN ANYMORE               
*                                                                               
         B     TCYES               RETURN 'YES'                                 
*                                                                               
*                                  TEMP FILE NOT OPEN                           
TCNOTOP  MVC   MDACTION,=Y(ER00TCNO)                                            
*                                                                               
TCNO     B     NO                                                               
*                                                                               
TCYES    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS DATA INTO THE TEMPORARY FILE BUFFER, ATMPBUF.  THE          
* ROUTINE MOVES THROUGH THE BUFFER TO PUT EACH DATA CHUNK, AND EACH             
* TIME THE END IS REACHED, IT WRITES THE BUFFER OUT TO THE NEXT TEMPSTR         
* RECORD IN THE TEMPORARY FILE.  IF THE ROUTINE IS SUCCESSFUL, IT               
* RETURNS 'YES', ELSE IT RETURNS 'NO'.                                          
*                                                                               
* ON ENTRY:    PARAMETER 1         A(DATA TO BE PUT)                            
*              PARAMETER 2         L(DATA)                                      
***********************************************************************         
VPUTTMP  DS    0H                                                               
         CLI   TMPOFLAG,C'P'       IF TMP BUFFER OPEN FOR PUT                   
         BNE   PTNOTPUT                                                         
*                                                                               
         L     R2,0(R1)            R2 = A(DATA TO BE PUT)                       
         L     R3,4(R1)            R3 = L(DATA)                                 
*                                                                               
         C     R3,TMPMAX           IF LENGTH > CURRENT MAX(REC LENGTH)          
         BNH   *+8                                                              
         ST    R3,TMPMAX           THEN STORE NEW MAX(REC LENGTH)               
*                                                                               
         L     R4,DNEXTTMP         R4 = D(NEXT DATA IN TMP BUFFER)              
*                                                                               
         CLI   TMPMETHD,C'S'       REC CAN'T CROSS TMPSTR REC BOUNDARY?         
         BNE   PT00                IT CAN, NO THE SPECIAL PUTTMP METHOD         
*                                                                               
         LA    R1,4(R4)            IT CAN'T CROSS, MAKE  SURE THERE'S           
*                                      ROOM FOR L(DATA), DATA, AND A            
         AR    R1,R3                   POSSIBLE END OF BLOCK (X'FFFF')          
         B     PT05                                                             
*                                                                               
PT00     LA    R1,2(R4)            MAKE SURE ROOM FOR LENGTH                    
*                                                                               
PT05     CH    R1,LENTWA           IF WE CAN PUT IN TMP BUFFER                  
         BNH   PT20                THEN PUT INTO THE TMP BUFFER                 
*                                                                               
         CLI   TMPMETHD,C'S'       IF SPECIAL PUTTMP METHOD                     
         BNE   PT07                                                             
*                                                                               
         A     R4,ATMPBUF                                                       
         MVC   0(2,R4),=X'FFFF'    THEN PUT AN END OF BLOCK (X'FFFF')           
*                                                                               
*                                  ELSE WRITE TEMP BUFFER TO TEMPSTR            
PT07     DS    0H                                                               
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 WRTTWA,CMDMCB,ATMPBUF,CURRTWA                                    
         BNE   PTNO                                                             
*                                                                               
PT10     L     RF,CURRTWA          BUMP TO NEXT TEMPSTR NUMBER                  
         LA    RF,1(RF)                                                         
         ST    RF,CURRTWA                                                       
*                                                                               
         CLC   CURRTWA,=F'5'       IF TEMPSTR RECORD 5                          
         BE    PT10                THEN SKIP TO NEXT                            
*                                                                               
         CLC   CURRTWA,=F'11'      IF OVERFLOW                                  
         BNL   PTOVER              THEN ERROR                                   
*                                                                               
         SR    R4,R4               NEW D(NEXT DATA IN TMP BUFFER)               
         ST    R4,DNEXTTMP                                                      
*                                                                               
PT20     A     R4,ATMPBUF          STORE LENGTH IN TEMP BUFFER                  
         STCM  R3,3,0(R4)                                                       
*                                                                               
         L     R4,DNEXTTMP         R4 = D(NEXT DATA IN TMP BUFFER)              
         LA    R4,2(R4)                                                         
         LR    R5,R3               R5 = LENGTH OF DATA                          
*                                                                               
         LA    R1,0(R3,R4)         IF DATA FITS IN TMP BUFFER                   
         A     R4,ATMPBUF              R4 = A(NEXT DATA IN TMP BUFFER)          
*                                                                               
         CLI   TMPMETHD,C'S'       IF SPECIAL PUTTMP METHOD                     
         BE    PT40                THEN WE KNOW IT FITS IN TWA                  
*                                                                               
         CH    R1,LENTWA                                                        
         BNH   PT40                THEN COPY DATA TO TMP BUFFER                 
*                                                                               
         LH    R0,LENTWA           R5 = # OF BYTES THAT WON'T FIT               
         SR    R0,R1                                                            
         LPR   R5,R0                                                            
*                                                                               
         LR    R1,R3               R1 = # OF BYTES THAT WILL FIT                
         AR    R1,R0                                                            
         BZ    PT25                IF NONE THEN GOTO NEXT TEMPSTR REC           
*                                                                               
         STM   R0,R1,CMDMCB        SAVE R0 AND R1                               
*                                                                               
         LR    RE,R4               ELSE PUT WHAT WILL FIT                       
         LR    RF,R1                                                            
         LR    R0,R2                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LM    R0,R1,CMDMCB        RESTORE R0 AND R1                            
*                                                                               
         LR    R0,R2               R2 = A(DATA THAT WASN'T COPIED)              
         AR    R0,R1                                                            
         LR    R1,R5                                                            
         L     RE,AFREE                                                         
         LR    RF,R5                                                            
         MVCL  RE,R0                                                            
         L     R2,AFREE                                                         
*                                                                               
         LM    R0,R1,CMDMCB        RESTORE R0 AND R1                            
*                                                                               
*                                  WRITE OUT TEMP BUFFER                        
PT25     DS    0H                                                               
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 WRTTWA,CMDMCB,ATMPBUF,CURRTWA                                    
         BNE   PTNO                                                             
*                                                                               
PT30     L     RF,CURRTWA          BUMP TO NEXT TEMPSTR NUMBER                  
         LA    RF,1(RF)                                                         
         ST    RF,CURRTWA                                                       
*                                                                               
         CLC   CURRTWA,=F'5'       IF TEMPSTR RECORD 5                          
         BE    PT30                THEN SKIP TO NEXT                            
*                                                                               
         CLC   CURRTWA,=F'11'      IF OVERFLOW                                  
         BNL   PTOVER              THEN ERROR                                   
*                                                                               
         L     R4,ATMPBUF          NEW D(NEXT DATA IN TMP BUFFER)               
*                                                                               
PT40     STM   R0,R1,CMDMCB        SAVE R0 AND R1                               
*                                                                               
         LR    R0,R4               COPY ELEMENT INTO TMP BUFFER                 
         LR    R1,R5                                                            
         LR    RE,R2                                                            
         LR    RF,R5                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LM    R0,R1,CMDMCB        RESTORE R0 AND R1                            
*                                                                               
         AR    R4,R5               SAVE D(NEXT ELEMENT IN TMP BUFFER)           
         S     R4,ATMPBUF                                                       
         ST    R4,DNEXTTMP                                                      
*                                                                               
         B     PTYES               RETURN 'YES'                                 
*                                                                               
*                                  TEMPSTR NOT OPEN FOR PUT                     
PTNOTPUT MVC   MDACTION,=Y(ER00NPUT)                                            
         B     PTNO                                                             
*                                  TEMPSTR OVERFLOW ERROR                       
PTOVER   MVC   MDACTION,=Y(ER00PTMP)                                            
         B     PTNO                                                             
*                                                                               
PTNO     B     NO                                                               
*                                                                               
PTYES    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETRIEVES THE NEXT DATA CHUNK FROM THE TEMPORARY FILE            
* BUFFER.  THE TEMPORARY FILE USES TEMPSTR TO STORE ITS RECORDS, AND            
* ATMPBUF AS ITS BUFFER.  THE ROUTINE MOVES THROUGH THE BUFFER TO               
* READ EACH ELEMENT, AND EACH TIME THE END IS REACHED, IT READS IN THE          
* NEXT TEMPSTR RECORD.  IF THE ROUTINE IS SUCCESSFUL, IT RETURNS 'YES',         
* ELSE IT RETURNS 'NO'.                                                         
*                                                                               
* ON ENTRY:    PARAMETER 1         A(WHERE TO STORE DATA)                       
*                                                                               
* RETURNS:     PARAMETER 2         LENGTH OF DATA                               
*                                      0 - END OF TEMP FILE                     
***********************************************************************         
VGETTMP  DS    0H                                                               
         CLI   TMPOFLAG,C'G'       IF TEMP FILE OPEN FOR GET                    
         BNE   GTNOTGET                                                         
*                                                                               
         L     R2,0(R1)            R2 = A(WHERE TO STORE DATA)                  
         L     R4,DNEXTTMP         R4 = A(NEXT DATA IN TMP BUFFER)              
*                                                                               
         LA    R1,2(R4)            IF LENGTH FITS IN TMP BUFFER                 
         CH    R1,LENTWA                                                        
         BNH   GT10                THEN GET THE LENGTH AND DATA                 
*                                                                               
GT5      DS    0H                                                               
         L     RF,CURRTWA          ELSE BUMP TO NEXT TEMPSTR NUMBER             
         LA    RF,1(RF)                                                         
         ST    RF,CURRTWA                                                       
*                                                                               
         CLC   CURRTWA,=F'5'       IF TEMPSTR RECORD 5                          
         BE    GT5                 THEN SKIP TO NEXT                            
*                                                                               
         CLC   CURRTWA,=F'11'      IF OVERFLOW                                  
         BNL   GTOVER              THEN ERROR                                   
*                                  FILL TMPBUF W/ NEXT TEMPSTR REC              
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 READTWA,CMDMCB,ATMPBUF,CURRTWA                                   
         BNE   GTNO                                                             
*                                                                               
         SR    R4,R4               RESET DNEXTTMP                               
         ST    R4,DNEXTTMP                                                      
*                                                                               
GT10     A     R4,ATMPBUF          GET LENGTH FROM TEMP BUF                     
*                                                                               
         CLC   =X'FFFF',0(R4)                                                   
         BE    GT5                 END OF BLOCK, READ NEXT TWA                  
*                                                                               
         ZICM  R3,0(R4),(3)                                                     
         BNZ   *+12                IF LENGTH IS ZERO                            
         MVI   EOTFLAG,C'Y'        THEN SET END OF TEMP FLAG                    
         B     GTYES               AND RETURN 'YES'                             
*                                                                               
         LA    R4,2(R4)            R4 = A(DATA STORED IN TMP BUFFER)            
         LR    R5,R3               R5 = LENGTH OF DATA                          
*                                                                               
         L     R1,DNEXTTMP         IF DATA FITS IN TIA                          
         LA    R1,2(R1,R3)                                                      
         CH    R1,LENTWA                                                        
         BNH   GT30                THEN COPY DATA TO ADDRESS GIVEN              
*                                                                               
         LH    R0,LENTWA           R5 = # OF BYTES THAT DIDN'T FIT              
         SR    R0,R1                                                            
         LPR   R5,R0                                                            
*                                                                               
         LR    R1,R3               R1 = # OF BYTES THAT DID FIT                 
         AR    R1,R0                                                            
         BZ    GT20                IF NONE THEN GOTO NEXT TEMPSTR REC           
*                                                                               
         STM   R0,R1,CMDMCB        SAVE R0 AND R1                               
*                                                                               
         LR    RE,R2               ELSE GET WHAT DID FIT                        
         LR    RF,R1                                                            
         LR    R0,R4                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LM    R0,R1,CMDMCB        RESTORE R0 AND R1                            
*                                                                               
         AR    R2,R1               R2 = A(REST OF DATA FOR TMPELEM)             
*                                                                               
GT20     L     RF,CURRTWA          BUMP TO NEXT TEMPSTR NUMBER                  
         LA    RF,1(RF)                                                         
         ST    RF,CURRTWA                                                       
*                                                                               
         CLC   CURRTWA,=F'5'       IF TEMPSTR RECORD 5                          
         BE    GT20                THEN SKIP TO NEXT                            
*                                                                               
         CLC   CURRTWA,=F'11'      IF OVERFLOW                                  
         BNL   GTOVER              THEN ERROR                                   
*                                                                               
*                                  ELSE FILL TMPBUF W/ NEXT TEMPSTR REC         
         OC    CURRTWA,CURRTWA     MAKE SURE ITS NOT TWA0                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 READTWA,CMDMCB,ATMPBUF,CURRTWA                                   
         BNE   GTNO                                                             
*                                                                               
         L     R4,ATMPBUF          R4 = A(1ST ELEMENT FOR NEW TMPBUF)           
*                                                                               
GT30     STM   R0,R1,CMDMCB        SAVE R0 AND R1                               
*                                                                               
         LR    RE,R2               GET THE DATA                                 
         LR    RF,R5                                                            
         LR    R0,R4                                                            
         LR    R1,R5                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LM    R0,R1,CMDMCB        RESTORE R0 AND R1                            
*                                                                               
         AR    R4,R5               SAVE D(NEXT ELEMENT IN TMP BUFFER)           
         S     R4,ATMPBUF                                                       
         ST    R4,DNEXTTMP                                                      
*                                                                               
         B     GTYES               RETURN 'YES'                                 
*                                                                               
*                                  TEMPSTR NOT OPEN FOR GET                     
GTNOTGET MVC   MDACTION,=Y(ER00NGET)                                            
         B     GTNO                                                             
*                                  TEMPSTR OVERFLOW ERROR                       
GTOVER   MVC   MDACTION,=Y(ER00GTMP)                                            
*                                                                               
GTNO     B     NO                                                               
*                                                                               
GTYES    ST    R3,DMCB+4           RETURN THE LENGTH                            
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DETERMINES THE PROPER VALUE THAT SHOULD APPEAR IN THE            
* ESTIMATE FRAMES FOR REQUEST FIELD IN THE MAD CONTROL FRAME.  IT TAKES         
* THE CURRENT FRAME NUMBER, WHICH IS GLOBAL, AND THE ESTIMATE FRAMES            
* REMAINING, WHICH IS PASSED AS THE FIRST PARAMETER, AND DECIDES WHAT           
* THE TOTAL FRAMES FOR THE REQUEST SHOULD BE.  THIS NUMBER WILL NEVER           
* INCREASE BY MORE THAN ONE FROM TRANSACTION TO TRANSACTION, OR ELSE            
* THE PERCENTAGE COMPLETE DISPLAYED ON THE PC SCREEN COULD ACTUALLY             
* DROP.  THIS WOULD NOT BE ACCEPTABLE.                                          
***********************************************************************         
VESTFRM  DS    0H                                                               
         L     RF,0(R1)            RF = ESTIMATED FRAMES REMAINING              
*                                                                               
         A     RF,MDFRAME          RF = ESTIMATE TOTAL NUMBER OF FRAMES         
*                                                                               
         CLC   MDFRAME,=F'1'       IF FIRST FRAME THEN SAVE ESTIMATE            
         BE    EF10                                                             
*                                                                               
         L     RE,MDFRMEST         IF OLD ESTIMATE + 1 < NEW ESTIMATE           
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    EF20                THEN NEW ESTIMATE = OLD ESTIMATE + 1         
*                                                                               
EF10     LR    RE,RF               ELSE USE NEW ESTIMATE                        
*                                                                               
EF20     ST    RE,MDFRMEST         SAVE NEW ESTIMATE                            
*                                                                               
EFX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE OPENS A NEW WORKER FILE FOR CREATE.                              
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FILE ID) OR ZERO (USE DEFAULT)             
***********************************************************************         
VWRKCRE  DS    0H                                                               
         L     R2,0(R1)            R2 = A(FILE ID) OR ZERO                      
*                                                                               
         L     R4,AFREE            INITIALIZE HEADER RECORD                     
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,SIGNON2                                                  
*                                                                               
         LTR   R2,R2               IF OVERRIDE FILE ID PASSED                   
         BZ    WCR10                                                            
         MVC   WLFILEID,0(R2)      THEN SET FILE ID FROM OVERRIDE               
         B     WCR20                                                            
*                                  ELSE DEFAULT TO 'MADWKT'                     
WCR10    MVC   WLFILEID(5),=C'MADWK'                                            
         MVI   WLCLASS,C'T'                                                     
*                                                                               
WCR20    LTR   R2,R2               IF OVERRIDE TYPE PASSED                      
         BZ    WCR30                                                            
         MVC   WLTYPE,6(R2)        THEN SET TO OVERRIDE                         
*                                                                               
         CLI   WLTYPE,C'A'         IF TYPE IS A OR L                            
         BE    *+12                                                             
         CLI   WLTYPE,C'L'                                                      
         BNE   WCR50                                                            
         L     RF,ASYSFACS         WLSUBPRG MUST BE FAC 1CHR ID                 
         L     RF,VSSB-SYSFACD(RF)                                              
         MVC   WLSUBPRG,SSBSYSN1-SSBD(RF)                                       
         B     WCR50                                                            
*                                                                               
WCR30    MVI   WLTYPE,0            ELSE DEFAULT TO 0 FOR NOW                    
*                                                                               
WCR50    MVI   WLATTB,WLATOBJ      SET OBJECT CODED DATA FLAG                   
         MVC   WLDESC,=CL16' '     FILL IN DESC '$MAD LUIDLUID   '              
         MVC   WLDESC(4),=C'$MAD'                                               
         L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   WLDESC+5(8),TSYM                                                 
         DROP  RF                                                               
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR TO CREATE FILE                  
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),ATMPBUF           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WRKFILNO,WLREPRNO   EXTRACT FILE NUMBER                          
*                                                                               
WCRYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS A RECORD TO THE WORKER FILE.                                
*                                                                               
* ON ENTRY:    PARAMETER 1         A(RECORD - WITH 4 BYTE MVS HEADER)           
***********************************************************************         
VWRKPUT  DS    0H                                                               
         L     R2,0(R1)            R2 = A(RECORD)                               
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR TO PUT RECORD                   
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R2),ATMPBUF           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WPTYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CLOSES THE WORKER FILE.                                          
***********************************************************************         
VWRKCLOT DS    0H                                                               
         L     R4,AFREE            INITIALIZE HEADER RECORD                     
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFTMP*'                                            
         B     WRKCLO1             TEMPORARY CLOSE                              
*                                                                               
VWRKCLOE DS    0H                                                               
         L     R4,AFREE            INITIALIZE HEADER RECORD                     
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFERR*'                                            
         B     WRKCLO1             ERROR CLOSE                                  
*                                                                               
VWRKCLOS DS    0H                                                               
         L     R4,AFREE            INITIALIZE HEADER RECORD                     
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
WRKCLO1  LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR TO CLOSE FILE                   
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),ATMPBUF           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WCLYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE REOPENS THE WORKER FILE FOR APPEND.                              
***********************************************************************         
VWRKREPT DS    0H                                                               
         L     R4,AFREE            INITIALIZE HEADER RECORD                     
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVI   WLFLAG,WLFLREFN+WLFLMOD+WLFLRCOP                                 
         MVC   WLFILENO,WRKFILNO                                                
         MVC   WLUSRID,SIGNON2                                                  
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR REOPEN FILE                     
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),ATMPBUF           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRPYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THE TWO ENTRY POINTS HERE ARE FOR INITIALLY LOCATING A WORKER FILE            
* (WRKLOC) AND FOR RELOCATING THE WORKER FILE FROM THE PREVIOUS                 
* TRANSACTION AND SEEKING TO THE RECORD WHERE WE LEFT OFF (WRKREGT).            
***********************************************************************         
VWRKLOC  DS    0H                                                               
         XC    WRKRECNO,WRKRECNO   RESET RECORD NUMBER TO BEGIN OF FILE         
         B     WLO50                                                            
*                                                                               
VWRKREGT DS    0H                  LEAVE RECORD NUMBER FOR RELOCATING           
*                                                                               
WLO50    LA    R3,ELEMENT          FILL INDEX KEY WITH FILE NUMBER              
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKFILENO,WRKFILNO                                                
         MVI   UKFLAG,UKFLDAT                                                   
         MVC   UKUSRID,SIGNON2                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',=C'WRKFILE',(R3),AFREE,ATMPBUF            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    WRKRECNO,WRKRECNO   IF BEGIN OF FILE THEN DONE                   
*NOP     BZ    WLOYES                                                           
*                                                                               
         L     R4,AFREE            ELSE SET RECORD NUMBER FOR RANDOM            
         USING WLHDRD,R4              READ                                      
         XC    0(12,R4),0(R4)                                                   
         MVC   0(4,R4),WRKRECNO                                                 
         MVC   4(4,R4),=C'REC '                                                 
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR TO DO RANDOM READ               
         GOTO1 DATAMGR,DMCB,=C'RANDOM',=C'WRKFILE',(R3),AFREE,ATMPBUF           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WLOYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS A RECORD FROM THE WORKER FILE.                              
*                                                                               
* ON ENTRY:    PARAMETER 1         A(BUFFER TO RETURN RECORD IN)                
***********************************************************************         
VWRKGET  DS    0H                                                               
         L     R2,0(R1)            R2 = A(RECORD BUFFER)                        
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                                                               
*                                  CALL DATAMGR TO GET RECORD                   
         GOTO1 DATAMGR,DMCB,=C'READ',=C'WRKFILE',(R3),(R2),ATMPBUF              
         TM    8(R1),X'80'                                                      
         BO    WGTEOF                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,WRKRECNO         BUMP RECORD NUMBER                           
         LA    R1,1(R1)                                                         
         ST    R1,WRKRECNO                                                      
         B     WGTYES                                                           
*                                  EOF                                          
WGTEOF   MVC   0(8,R2),=C'*EOFEOF*'                                             
*                                                                               
WGTYES   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SET THE WORKER FILE STATUS TO SENT                                  *         
***********************************************************************         
         SPACE 1                                                                
VWRKSENT GOTO1 WRKLOC              LOCATE FILE FIRST                            
*                                                                               
         LA    R3,ELEMENT          FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVC   UKUSRID,SIGNON2                                                  
*                                  CALL DATAMGR TO MARK REPORT SENT             
         GOTO1 DATAMGR,DMCB,=C'SENRET',=C'WRKFILE',(R3),AFREE,ATMPBUF           
         CLI   8(R1),0                                                          
         BE    YES                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF LINKED ROUTINES ADDRESSES                                            
***********************************************************************         
         SPACE 1                                                                
LINKED   DS    0F                                                               
         DC    V(HELLO)            RECORD UPDATE ROUTINE                        
         DC    V(HELEN)            FILE NAME TABLE                              
         DC    V(WORKER)           WORKER FILE MANAGER                          
         DC    V(CRYPT)                                                         
         DC    A(0)                                                             
         DC    A(0)                32000 BYTE AREA TO LOAD OVERLAYS             
         DC    V(BINSRCH)          BINARY SEARCH TABLE ROUTINE                  
         DC    A(0)                                                             
         DC    A(0)                                                             
*                                                                               
NLINKED  EQU   (*-LINKED)/4                                                     
***********************************************************************         
* TABLE OF US/UK LINKED ROUTINES ADDRESSES                                      
***********************************************************************         
LINKUSUK DS    0F                                                               
*&&US                                                                           
         DC    V(HELENDIR)         DIRECTORY NAME TABLE                         
         DC    V(DPTRD)            DAYPART READ                                 
         DC    V(EQVRD)            EQUIVALENCE READ                             
*&&                                                                             
*&&UK                                                                           
         DC    V(HELENDIR)         DIRECTORY NAME TABLE                         
*&&                                                                             
*                                                                               
NLNKUSUK EQU   (*-LINKUSUK)/4                                                   
***********************************************************************         
* TABLE OF CORE RESIDENT ROUTINE OVERLAY NUMBER EQUATES                         
***********************************************************************         
CORERES  DS    0X                                                               
         DC    AL1(QPADDLE)                                                     
NCORERES EQU   (*-CORERES)                                                      
         EJECT                                                                  
***********************************************************************         
* TABLE OF US/UK CORE RESIDENT ROUTINE OVERLAY NUMBER EQUATES                   
***********************************************************************         
         SPACE 1                                                                
COREUSUK DS    0X                                                               
*&&US*&& DC    AL1(QDDISP)                                                      
*&&US*&& DC    AL1(QCLPACK)                                                     
*&&US*&& DC    AL1(QSTAPACK)                                                    
*&&US*&& DC    AL1(QQSORT)                                                      
*&&US*&& DC    AL1(QDEFINE)                                                     
*&&US*&& DC    AL1(QDEMOCON)                                                    
*&&US*&& DC    AL1(QDEMOVAL)                                                    
*&&US*&& DC    AL1(QGETBROD)                                                    
*&&US*&& DC    AL1(QMOBILE)                                                     
*&&US*&& DC    AL1(QSQUASH)                                                     
*&&US*&& DC    AL1(QJOBBER)                                                     
*&&US*&& DC    AL1(QGETOPT)                                                     
*&&US*&& DC    AL1(QSPSLNTB)                                                    
*                                                                               
*&&UK*&& DC    AL1(QJOBBER)                                                     
*&&UK*&& DC    AL1(QGETOPT)                                                     
*                                                                               
NCORUSUK EQU   (*-COREUSUK)                                                     
         EJECT                                                                  
***********************************************************************         
* VARIOUS CONSTANTS AND EXIT POINTS                                             
***********************************************************************         
         SPACE 1                                                                
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
*                                                                               
YES6     SR    RC,RC               RETURN CC EQUAL                              
NO6      LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT6     XIT1  REGS=(R6)                                                        
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SYSTEM LIST                                                         *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* CTMADFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTMADFFD                                                       
         PRINT ON                                                               
* CTMADWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTMADWORKD                                                     
         PRINT ON                                                               
* CTMADDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTMADDSECT                                                     
         PRINT ON                                                               
* CTMADEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMADEQUS                                                      
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* DMWRKFL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFL                                                        
         PRINT ON                                                               
* DMWRKFK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
* FASHIPBLK                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASHIPBLK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATED LENGTHS                                                     *         
***********************************************************************         
         SPACE 1                                                                
LENINF   EQU   2048                LENGTH OF INPUT FRAME AREA                   
LENOUTF  EQU   2048                LENGTH OF OUTPUT FRAME AREA                  
LENCOMP  EQU   4096                LENGTH OF COMPRESSION BUFFER                 
LEN1IO   EQU   6000                                                             
LENIOS   EQU   3*LEN1IO            THREE 4K BYTE IO AREAS FOR USE IN            
*                                      DATAMGR CALLS                            
LENFREE  EQU   4096                SPARE MEMORY FOR CONTROLLER TO USE           
LENTMP   EQU   18432               GETTMP/PUTTMP RECORD AREA                    
LENAPPL  EQU   4096                LEN OF FIRST APPL COMMON STORAGE             
LENUSUK  EQU   4096                LEN OF US/UK APPL COMMON STORAGE             
LENWORK  EQU   LENINF+LENOUTF+LENCOMP+LENIOS+LENFREE+LENAPPL+LENUSUK            
*                                                                               
DINKCHR  EQU   X'5F'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011CTMAD00   07/20/20'                                      
         END                                                                    
