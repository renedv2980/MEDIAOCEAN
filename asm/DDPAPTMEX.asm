*          DATA SET DDPAPTMEX  AT LEVEL 084 AS OF 10/19/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTMEXA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE DYNALLOC     *NOT DMDMGRL (DON'T LOAD DDSIO IN THE FOREGROUND)         
*                                                                               
PAPTMEX  TITLE 'VERIFY MEMBER EXISTENCE IN A PANVALET LIBRARY'                  
*                                                                               
************************ < DESCRIPTION BEGIN      > *******************         
*                                                                     *         
* NAME       : DDPAPTMEX (LOAD MODULE PAPTMEX)                        *         
*        ******* NOTE: SEE SOURCE MODULE APAS0222 FOR THE *********   *         
*        *******  CA-DELIVERED PANVALET MEMBER EXISTENCE EXIT *****   *         
*                                                                     *         
* PRODUCT    : PANAPT                                                 *         
* TYPE       : ASSEMBLER SOURCE PROGRAM                               *         
*                                                                     *         
* DESCRIPT.  : VERIFY THE EXISTENCE OF A MEMBER IN A PANVALET LIBRARY *         
*                                                                     *         
* NOTICES    : THIS MODULE IS PART OF THE DISTRIBUTED SOURCE          *         
*              CODE FOR PANAPT.                                       *         
*                                                                     *         
*              COPYRIGHT (C) 1992 COMPUTER ASSOCIATES                 *         
*              INTERNATIONAL INC.  ALL RIGHTS RESERVED.               *         
*                                                                     *         
*              THIS SOFTWARE IS PROPRIETARY INFORMATION AND ITS       *         
*              USE BY UNAUTHORIZED PERSONS IS PROHIBITED.             *         
*                                                                     *         
************************ < DESCRIPTION END        > *******************         
         EJECT                                                                  
************************ < DOCUMENTATION BEGIN    > *******************         
*                                                                     *         
* FUNCTION   : TO VERIFY THE EXISTENCE OF A MEMBER IN A PANVALET      *         
*              LIBRARY.                                               *         
*                                                                     *         
* ENTRY COND.                                                         *         
*                                                                     *         
*    LINKAGE : STANDARD OS LINKAGE                                    *         
*    PARMS   : (USAGE IS IN=INPUT, OUT=OUTPUT, MOD=MODIFIED)          *         
*              (TYPE IS STRUCTURE = A COLLECTION OF RELATED DATA      *         
*               FIELDS USED TOGETHER AS A DATA STRUCTURE)             *         
*                                                                     *         
*      PARAMETER  USAGE  TYPE      DESCRIPTION                        *         
*      ---------  -----  --------  ---------------------------------- *         
*      APAM02XX    MOD   STRUCTURE MEMBER EXISTENCE PARAMETER BLOCK   *         
*      APAMDIB2    IN    STRUCTURE MEMBER INVENTORY PARAMETER BLOCK   *         
*      APAMLIB2    IN    STRUCTURE LIBRARY CODE RECORD         013A*GRS         
*      APAMMMBR    IN    STRUCTURE PENDING FILE RECORD         013A*GRS         
*      APAMMDES    IN    STRUCTURE MEMBER DESCRIPTION RECORD   013A*GRS         
*                                                                     *         
* EXIT COND.                                                          *         
*                                                                     *         
*    RETURN  :  0 <===> MEMBER FOUND, THE MOVE REQUEST MAY BE CLOSED. *         
*    CODES   :          THE VERIFY FLAG WILL BE CLEARED.              *         
*                                                                     *         
*               4 <===> MEMBER NOT FOUND, THE MOVE REQUEST MAY NOT BE *         
*                       CLOSED. THE VERIFY FLAG WILL REMAIN SET TO    *         
*                       "V" INDICATING VERIFICATION IS STILL REQUIRED *         
*                                                                     *         
*               8 <===> INDICATES NO PROCESSING WAS DONE BY THE EXIT. *         
*                       THE VERIFY FLAG WILL NOT BE MODIFIED.         *         
*                                                                     *         
*              12 <===> USER PROVIDED DATA ERRORS OR                  *         
*                       ALLOCATION/DEALLOCATION ERROR OR              *         
*                       PAMS PROCESSING ERRORS                        *         
*                                                                     *         
* EXTERNAL                                                            *         
* CALLS      : PAM                                                    *         
*                                                                     *         
* COPYBOOKS  : NONE.                                                  *         
*                                                                     *         
* MACROS     : IEFZB4D0, IEFZB4D2, DYNALLOC.                          *         
*                                                                     *         
* COMMENTS   : NOTE:   UPON ENTRY TO THIS MODULE, PANAPT WILL SET THE *         
*                      VERIFY FLAG TO "V" FOR ADDS AND MAKECOPY.      *         
*                                                                     *         
************************ < DOCUMENTATION END      > *******************         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        PARAMETERS FROM CALLER                                                 
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
APAM02XX DSECT                                                                  
         COPY APAM02XX                                                          
APAMDIB2 DSECT                                                  012*BTK         
         COPY APAMDIB2                                                          
         COPY APAMLIB2                                                          
APAMMMBR DSECT                                                 013A*GRS         
         APAMMMBR                                                               
         APAMMDES                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        PROCESSING VERIFICATION TABLE                                          
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PROCESSD DSECT ,                   PROCESSING CRITERIA                          
PROCESS  DS    0CL3                                                             
VERFLG   DS    CL1                   VERIFY FLAG                                
SOURCE   DS    CL1                   SOURCE OF EXIT CALLING                     
AUTOCHKO DS    CL1                   AUTO CHECKOUT FLAG                         
PROCESSL EQU   *-PROCESS           TABLE ENTRY LENGTH                           
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        REGISTER DEFINITIONS                                                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*             R0                   WORK                                         
*             R1                   PARAMETER LIST                               
*             R2                   WORK                                         
*             R3                   MEMBER DESCRIPTION RECORD   013A*GRS         
*             R4                   PROCESSING VERIFICATION TABLE                
*             R5                   PENDING FILE RECORD DSECT   013A*GRS         
*             R6                   LIBRARY CODE RECORD DSECT   013A*GRS         
*             R7                   INVENTORY RECORD DSECT                       
*             R8                   COUNTER/TABLE POINTER                        
*             R9                   LOOP CONTROL                                 
*             RA                   SECOND PROGRAM BASE                          
*             RB                   3RD PROGRAM BASE                             
*             RC                   BASE                                         
*             RD                   SAVEAREA                                     
*             RE                   BRANCH AND LINK                              
*             RF                   ENTRY POINT                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        MAIN ROUTINE                                                           
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PAPTMEX  CSECT                                                                  
         USING PAPTMEX,RC,RA,RB    ESTABLISH BASE ADDRESSABILITY                
         SAVE  (14,12),,PAPTMEX-&SYSDATE-&SYSTIME                               
         LR    RC,RF               LOAD BASE ADDRESS                            
         LR    RA,RC               ESTABLISH SECOND BASE REGISTER               
         AHI   RA,4096                                                          
         LR    RB,RA               ESTABLISH 3RD BASE REGISTER                  
         AHI   RB,4096                                                          
         LARL  RF,SAVEAREA         FETCH POINTER TO SAVEAREA                    
         ST    RD,4(RF)            SAVE LINKAGE TO CALLING PROGRAM              
         ST    RF,8(RD)            CHAIN SAVEAREAS                              
         LR    RD,RF               POINT TO CURRENT SAVEAREA                    
         USING SAVEAREA,RD                                                      
*                                                                               
**       *----------------------------------------------------*013A*GRS         
**       *  ESTABLISH ADDRESSABILITY TO PASSING PARAMETERS.   *013A*GRS         
**       *----------------------------------------------------*013A*GRS         
*                                                                               
         LM    R3,R7,0(R1)         FETCH PARM POINTERS                          
** (PARM #1) -------------------->  USER REQUEST BLOCK.        013A*GRS         
         USING APAM02XX,R3         ESTABLISH ADDRESSABILITY.   013A*GRS         
*                                                                               
** (PARM #2) -------------------->  INVENTORY RECORD.          013A*GRS         
         USING APAMDIB2,R4         ESTABLISH ADDRESSABILITY.   013A*GRS         
*                                                                               
** (PARM #3) ---------------------> LIBRARY CODE RECORD.       013A*GRS         
         USING APAMLIB2,R5         ESTABLISH ADDRESSABILITY.   013A*GRS         
*                                                                               
** (PARM #4) ---------------------> PENDING FILE RECORD.       013A*GRS         
         USING APAMMMBR,R6         ESTABLISH ADDRESSABILITY.   013A*GRS         
*                                                                               
** (PARM #5) -------------------->  MEMBER DESCRIPTION RECORD. 013A*GRS         
         TM    16(R1),X'80'        CHECK "VL" PARMLIST BIT.    013A*GRS         
         BO    @ASSERT             BIT OFF, ABEND.             013A*GRS         
         ABEND 2020,DUMP           *** ASSERT FAILURE ***      013A*GRS         
@ASSERT  DS    0H ---------------- PASSED ASSERT CHECK.        013A*GRS         
         USING APAMMDES,R7         ESTABLISH ADDRESSABILITY.   013A*GRS         
         SLL   R7,1                                            013A*GRS         
         SRL   R7,1                SHIFT OUT VL BIT.           013A*GRS         
*                                                                               
RQBLK    USING S99RB,REQBLOCK      ESTABLISH BASE ADDRESSABILITY                
*                                                                               
         MVI   M02XXMSG,C' '       CLEAR ERROR MESSAGE FIELD                    
         MVC   M02XXMSG+1(L'M02XXMSG-1),M02XXMSG                                
         XC    RTNCODE,RTNCODE     SET NORMAL RETURN CODE                       
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        DETERMINE IF PROCESSING IS NECESSARY                                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*********OPEN  (JMCMSG,OUTPUT)                                                  
*                                                                               
         CLC   MRSAVE,DESNUMB      STARTING A MOVE REQUEST?                     
         BNE   NTRY20              YES: INITIALIZE                              
*                                                                               
         CLC   SRCSAVE,M02XXSRC    STARTING A NEW EXIT POINT?                   
         BE    MEMBR010            NO: PROCEED WITH MEMBER PROCESSING           
*                                                                               
* GLOBAL MOVE REQUEST VALIDATION                                                
*                                                                               
NTRY20   DS    0H                                                               
         MVC   MRSAVE,DESNUMB      YES: SAVE NEW MR NUMBER                      
         MVC   SRCSAVE,M02XXSRC    SAVE EXIT POINT CODE NUMBER                  
         MVC   LIBCDGRP,BLANKS     INITIALIZE "LIBCODE GROUP"                   
         MVI   PRKSFLAG,C' '       INITIALIZE /K LIBCODE INDICATOR FLAG         
         XC    MBRCOUNT,MBRCOUNT   CLEAR LOCAL MEMBER COUNT                     
         XC    SRCECNT,SRCECNT     CLEAR "SRCE" LIBCODE MEMBER COUNTER          
*                                                                               
         MVI   BKOTFLAG,C'Y'       POSIT BACKOUT                                
         CLI   M02XXSRC,BACKOUT    BACKOUT PROCESSING?                          
         BE    NTRY25              YES                                          
         CLC   DESCLSTA,=AL2(DESSTAPB) BACKOUT PROCESSING IN BATCH?             
         BE    NTRY25              YES                                          
         CLC   DESCLSTA,=AL2(DESSTSLB) BACKOUT PROCESSING IN BATCH?             
         BE    NTRY25              YES                                          
         MVI   BKOTFLAG,C'N'       NOT BACKOUT                                  
*                                                                               
NTRY25   DS    0H                                                               
         CLI   M02XXSRC,BACKOUT    BACKOUT PROCESSING?                          
         BNE   MEMBR010            NO                                           
*                                                                               
* COPY FOR REWORK MOVE REQUESTS SHOULD NEVER BE BACKED OUT. WHEN CR IS          
* USED AS INTENDED, THE REWORK IS CREATED FROM AN ORIGINAL MOVE                 
* REQUEST, AND THE ASSUMPTION IS THAT IF A BACKOUT IS NECESSARY, THAT           
* ORIGINAL MR WILL BE BACKED OUT, NOT THE REWORK.                               
*                                                                               
* WE IDENTIFY A REWORK MR BY LOOKING AT FIELD DESNAME, WHICH WILL BE OF         
* THE FORMAT "RW OOOOOO:RRRRRR" WHERE OOOOOO IS THE ORIGINAL MR #, AND          
* RRRRRR IS THE REWORK MR #. THAT IS A RESULT OF CUSTOM CODE IN THE             
* PANAPT APIP110 PANEL, NOT BECAUSE THE FIELD IS SET THAT WAY BY THE            
* PANAPT BASE PRODUCT. WE HAVE TO DO IT THIS WAY BECAUSE THE "REAL"             
* REWORK INFO IS NOT MAPPED BY APAMMDES.                                        
*                                                                               
         CLC   =C'RW ',DESNAME     IS THIS A COPY FOR REWORK MR?                
         BNE   NTRY40                                                           
         CLI   DESNAME_DELIMITER,C':'                                           
         BNE   NTRY40              NO                                           
*                                                                               
         CLC   DESESTOP,=C'PROD'   REWORK OF PROD MR?                           
         BNE   *+14                                                             
         CLC   M02XXLSN,=C'STGE'   BACKOUT FROM STGE?                           
         BE    NTRY40              YES: THIS IS ACTUALLY OKAY                   
*                                                                               
         MVC   MXERR04M,DESNAME_ORIG_MR#  PUT ORIGINAL MR# IN ERROR MSG         
         MVC   M02XXMSG(MXERR04L),MXERR04    ...AND GENERATE AN ERROR           
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
NTRY40   DS    0H                                                               
         CLC   M02XXLSN,=C'PROD'   BACKOUT FROM PROD?                           
         BNE   MEMBR010            NO                                           
         CLI   MRSUB,C'K'          YES: DID WE PROMOTE VIA PRKS LEVEL?          
         BNE   MEMBR010            NO                                           
*                                                                               
         MVC   M02XXMSG(MXERR12L),MXERR12  PROD BACKOUT VIA PRKS!               
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
         EJECT                                                                  
* MEMBER PROCESSING BEGINS HERE                                                 
*                                                                               
MEMBR010 DS    0H                                                               
*&&DO                                                                           
**** FOR DEBUGGING                                                              
         TPUT  =C'APAM02XX BLOCK:',15,EDIT                                      
         TPUT  M02XXACT,M02XXMSG-M02XXACT,EDIT                                  
         TPUT  =C'MOVE REQUEST BLOCK:',19,EDIT                                  
         TPUT  APAMMDES,DESEXMAX-APAMMDES,EDIT                                  
         TPUT  =C'MEMBER BLOCK:',13,EDIT                                        
         TPUT  MRMBR,MRMBRL,EDIT                                                
         TPUT  =C'INVENTORY BLOCK:',16,EDIT                                     
         TPUT  INVREC,D2RECLNG,EDIT                                             
*&&                                                                             
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BE    *+14                YES                                          
         CLC   M02XXLSN,=C'PRKS'   NO: ARE WE AT THE PRKS LEVEL?                
         BE    GETOUT              YES: NOTHING GETS MOVED                      
*                                                                               
         CLI   INVUSE04,C'Y'       FLAGGED AS PERMANENT TEST MODULE?            
         BNE   MEMBR015            NO: PROCEED                                  
         CLC   MRLIB,=C'SRCE'      YES: SRCE/T LIBCODE SPECIFIED?               
         BNE   *+12                                                             
         CLI   MRSUB,C'T'                                                       
         BE    MEMBR050            YES: PROCEED                                 
*                                                                               
         MVC   MX5MEM,M02XXMEM                                                  
         MVC   M02XXMSG(MXERR05L),MXERR05                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR015 DS    0H                                                               
*&&DO                                                                           
* IF WE EVER DECIDE TO IMPLEMENT THIS, UNCOMMENT IT!                            
*                                                                               
         CLI   INVUSE01,C'Y'       FLAGGED AS CRITICAL MODULE?                  
         BNE   MEMBR020            NO: PROCEED                                  
         CLI   MRSUB,C'C'          /C LIBCODE SPECIFIED?                        
         BE    MEMBR050            YES: PROCEED                                 
*                                                                               
         MVC   MX7MEM,M02XXMEM                                                  
         MVC   M02XXMSG(MXERR07L),MXERR07                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR020 DS    0H                                                               
*&&                                                                             
         IF (CLI,M02XXSRC,EQ,ADD),OR,  MEMBER IS ABOUT TO BE ADDED...           
            (CLI,M02XXSRC,EQ,MAKECOPY) ...OR COPIED INTO A MR?                  
           IF (CLI,INVUSE01,EQ,C'Y')   FLAGGED AS CRITICAL MODULE?              
             BRAS  RE,CRITWARN           YES: SEND A WARNING E-MAIL             
           ENDIF ,                                                              
*                                                                               
* When a developer adds a member to a MR which is already assigned to           
* someone else, a conversation must take place between both developers          
* regarding the concurrent development. As a reminder of this, we send          
* an e-mail to both developers to prompt that conversation. Note: we            
* only do this for Inventory records which were last updated in 2017            
* and beyond. This is because:                                                  
*  a) this is approximately when we began our process of mainframe QA,          
*     as well as major and interim code releases, which had the effect          
*     of keeping Pan members from being promoted as quickly as they             
*     used to be (and therefore exacerbated the concurrent developement         
*     issue), and                                                               
*  b) we still have many very old Move Requests which we don't want to          
*     delete, but which don't require a warning when assignment is              
*     transferred away from them.                                               
           IF (CLI,INVASFLG,EQ,C'Y'),AND,  IF MEMBER IS ASSIGNED TO             
              (CLC,DESADDID,NE,INVASSUR),AND,   SOMEONE ELSE, AND...            
              (CLC,INVDATE,GE,=C'20170101')     ISN'T "OLD", THEN...            
             BRAS  RE,ASSGNWRN                 ...SEND A WARNING E-MAIL         
           ENDIF ,                                                              
         ENDIF ,                                                                
*                                                                               
         CLC   MRLIB,=C'SRCE'      LIBCODE SRCE?                                
         BNE   MEMBR050            NO: CONTINUE                                 
         CLI   MRSUB,C'T'          SUBCODE T?                                   
         BNE   MEMBR040            NO: IT'S LIBCODE SRCE                        
*                                                                               
         MVC   MX3MEM,M02XXMEM     UNAUTHORIZED FOR SRCE/T                      
         MVC   MX3LIBC,L2LIBC                                                   
         MVC   MX3SUBC,L2SUBC                                                   
         MVI   MX3SLASH,C' '                                                    
         CLC   L2SUBC,BLANKS                                                    
         BE    *+8                                                              
         MVI   MX3SLASH,C'/'                                                    
         MVC   M02XXMSG(MXERR03L),MXERR03                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR040 DS    0H                                                               
         LH    R0,SRCECNT          INCREMENT "SRCE" MEMBER COUNTER              
         AHI   R0,1                                                             
         STH   R0,SRCECNT                                                       
         CH    R0,MAXIMUM_SRCE_MEMBERS  TOO MANY RVP JOB STEPS?                 
         BNH   MEMBR050            NO                                           
*                                                                               
         LH    R0,MAXIMUM_SRCE_MEMBERS                                          
         CVD   R0,DBLWRD                                                        
         OI    DBLWRD+7,X'0F'                                                   
         UNPK  MXERR13N,DBLWRD                                                  
         MVC   M02XXMSG(MXERR13L),MXERR13                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR050 DS    0H                                                               
         CLI   PRKSFLAG,C' '       /K CONSISTENCY FLAG IS ALREADY SET?          
         BNE   MEMBR055            YES                                          
         MVI   PRKSFLAG,C'N'       ASSUME THIS LIBCODE ISN'T /K                 
         CLI   MRSUB,C'K'          IS THIS A /K LIBCODE?                        
         BNE   *+8                                                              
         MVI   PRKSFLAG,C'K'       YES                                          
         B     MEMBR060                                                         
*                                                                               
MEMBR055 DS    0H                                                               
         MVI   BYTE,C'N'           ASSUME THIS ISN'T A /K LIBCODE               
         CLI   MRSUB,C'K'          IS IT?                                       
         BNE   *+8                                                              
         MVI   BYTE,C'K'           YES                                          
         CLC   PRKSFLAG,BYTE       IS THIS CONSISTENT WITH PREVIOUS?            
         BNE   MEMBR065            NO: LIBCODES ARE INCONSISTENT                
*                                                                               
MEMBR060 DS    0H                                                               
         CLC   =C'@* LIBCODE GROUP = "',L2MOD1  "LIBCODE GROUP" RECORD?         
         BNE   MEMBR100            NO: ASSUME NO CONSISTENCY ISSUES             
         CLC   LIBCDGRP,BLANKS     FIRST LIBCODE SEEN YET?                      
         BNE   *+14                YES: CHECK CONSISTENCY                       
         MVC   LIBCDGRP,L2MOD1+20  NO: SAVE THIS GROUP VALUE                    
         B     MEMBR070                                                         
         CLC   LIBCDGRP,L2MOD1+20  IS THIS IN THE SAME LIBCODE GROUP?           
         BE    MEMBR070            YES: LIBCODES ARE CONSISTENT                 
*                                                                               
MEMBR065 DS    0H                                                               
         MVC   MX1MEM,M02XXMEM     INCONSISTENT LIBCODES IN THIS MR             
         MVC   MX1LIBC,L2LIBC                                                   
         MVC   MX1SUBC,L2SUBC                                                   
         MVI   MX1SLASH,C' '                                                    
         CLC   L2SUBC,BLANKS                                                    
         BE    *+8                                                              
         MVI   MX1SLASH,C'/'                                                    
         MVC   M02XXMSG(MXERR01L),MXERR01                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR070 DS    0H                                                               
         CLI   MRSUB,C'P'          IS THIS A /P LIBCODE?                        
         BNE   MEMBR100                                                         
         CLI   INVUSE03,C'Y'       YES: STGE BYPASS ALLOWED?                    
         BE    MEMBR100            YES                                          
*                                                                               
* FOR MEMBERS WHOSE NAME BEGINS WITH A KNOWN AND AUTHORIZED PREFIX,             
* DON'T REQUIRE THAT THE STGE-BYPASS FLAG BE TURNED ON IN THE INVENTORY         
* RECORD.                                                                       
*                                                                               
         LA    RE,STGE_BYPASS_MEMBERS_OK   TABLE OF MEMBER PREFIXES             
         USING STGEBPD,RE                                                       
MEMBR080 DS    0H                                                               
         CLI   0(RE),X'FF'         EOT?                                         
         BE    MEMBR090            YES: STGE-BYPASS UNAUTHORIZED                
         LLC   R1,STGEPFXL         NUMBER OF SIGNIFICANT CHARACTERS             
         BCTR  R1,0                                                             
         EX    R1,*+8              EXECUTED CLC ALLOWS FOR PREFIXES             
         B     *+10                                                             
         CLC   M02XXMEM(0),STGEPFX IS THIS ONE OF THE MEMBERS?                  
         BE    MEMBR100            YES (OR AT LEAST, PROBABLY)                  
         LA    RE,STGEBPDQ(RE)                                                  
         B     MEMBR080                                                         
         DROP  RE                                                               
*                                                                               
MEMBR090 DS    0H                                                               
         MVC   MX2MEM,M02XXMEM     NO: GENERATE ERROR                           
         MVC   MX2LIBC,L2LIBC                                                   
         MVC   MX2SUBC,L2SUBC                                                   
         MVI   MX2SLASH,C' '                                                    
         CLC   L2SUBC,BLANKS                                                    
         BE    *+8                                                              
         MVI   MX2SLASH,C'/'                                                    
         MVC   M02XXMSG(MXERR02L),MXERR02                                       
         MVC   RTNCODE,=F'4'       SET VERIFY ERROR RETURN CODE                 
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR100 DS    0H                                                               
         CLI   BKOTFLAG,C'Y'       BACKOUT PROCESSING?                          
         BNE   MEMBR120            NOT BACKOUT: CHECK FOR CLOSE                 
*                                                                               
         CLC   M02XXLSN,=C'TEST'   TEST LEVEL?                                  
         BE    GETOUT              YES: WE SHOULDN'T BE HERE                    
         B     MEMBR140            NO: PROCEED                                  
*                                                                               
MEMBR120 DS    0H                                                               
         CLI   M02XXSRC,CLOSE      CLOSE PROCESSING?                            
         BE    MEMBR140            YES.. PROCESS MEMBER                         
*                                                                               
         LA    R8,TABLE            FETCH POINTER TO VERIFY TABLE                
P        USING PROCESSD,R8         ESTABLISH BASE ADDRESSABILITY                
         LA    R9,TABLECNT         FETCH NUMBER OF TABLE ENTRIES                
*                                                                               
CKPROCES DS    0H                                                               
         CLC   P.PROCESS,M02XXVER  PROCESS THIS MEMBER?                         
         BE    MEMBR140            YES.. CONTINUE                               
         LA    R8,PROCESSL(,R8)    POINT TO NEXT TABLE ENTRY                    
         BCT   R9,CKPROCES         GO... CHECK TABLE ENTRY                      
         DROP  P                   DROP BASE ADDRESSABLITY                      
*                                                                               
GETOUT   DS    0H                                                               
         MVC   RTNCODE,=F'8'       SET NO PROCESSING RETURN CODE                
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR140 DS    0H                                                               
         CLC   M02XXACT,=CL8'MEMBER' VALID ACTION REQUESTED                     
         BE    MEMBR160            YES.. CONTINUE PROCESSING                    
         MVC   PVACT01,M02XXACT    SET INVALID ACTION IN MESSAGE                
         MVC   M02XXMSG(PVERR01L),PVERR01 RETURN INVALID ACTION MSG             
         MVC   RTNCODE,=F'12'      SET USER ERROR RETURN CODE                   
         B     RETURN              RETURN TO CALLER                             
*                                                                               
MEMBR160 DS    0H                                                               
         CLC   =C'DOESNT.MATTER',M02XXDSN IF IT'S JUST A PLACEHOLDER...         
         BE    RETURN                     ...DON'T BOTHER TO LOOK               
         CLC   M02XXDDN,=C'$USERLIB'  IF ORIGIN IS PERSONAL PANLIB...           
         BE    MEMBR170                                                         
         CLC   M02XXDDN,=C'$USERLBK'  OR BACKUP IS PERSONAL PANLIB...           
         BE    MEMBR170               THEN CONSTRUCT THE DSN                    
*                                                                               
         CLI   BKOTFLAG,C'Y'       DSNS COME FROM LIBCODE DEFINITION...         
         BNE   MEMBR200            BUT PRKS DSNS ARE AT THE PROD LEVEL          
         CLC   M02XXLSN,=C'PRKS'                                                
         BNE   MEMBR200                                                         
*                                                                               
         LR    RF,R5               A(APAMLIB2 DSECT)                            
         AHI   RF,L2LVLDTA-APAMLIB2                                             
         USING L2LVLDAT,RF                                                      
         CLC   L2LVLSNM,=C'TEST'   CHECK SHORT LEVEL NAME                       
         BE    *+6                                                              
         DC    H'0'                'TEST' IS ALWAYS THE FIRST LEVEL!            
*                                                                               
         LA    RF,L2NXTLVL         BUMP TO NEXT LEVEL                           
         CLC   L2LVLSNM,=C'PROD'   FIND PROD SHORT LEVEL NAME                   
         BNE   *-10                                                             
         MVC   M02XXDSN,L2BKDSN    PRODUCTION BACKUP PANLIB DSN                 
         B     MEMBR200                                                         
*                                                                               
MEMBR170 DS    0H                                                               
         MVC   M02XXDSN,BLANKS        BUILD THE ORIGIN LIBRARY DSN              
         MVC   M02XXDSN(4),=C'PAN.'   PROVIDE PREFIX                            
         CLC   =C'ENV=SBX',M02XXPRM   ARE WE IN THE SANDBOX?                    
         BNE   *+16                   NO: BYPASS ADJUSTMENT                     
         MVC   M02XXDSN(L'SANDBOX),SANDBOX    PROVIDE 'PANAPT.SBX.'             
         MVC   M02XXDSN+L'SANDBOX(4),=C'PAN.'                                   
*                                                                               
         LA    R8,M02XXDSN+4          SET POINTER                               
         CLC   SANDBOX,M02XXDSN       ARE WE IN THE SANDBOX?                    
         BNE   *+8                    NO: BYPASS ADJUSTMENT                     
         LA    R8,L'SANDBOX(R8)       YES: ADVANCE POINTER                      
*                                                                               
         LA    R1,DESADDID                                                      
MEMBR180 DS    0H                                                               
         MVC   0(1,R8),0(R1)          MOVE NEXT CHAR                            
         LA    R1,1(R1)               ADVANCE POINTERS                          
         LA    R8,1(R8)               ADVANCE POINTERS                          
         CLI   0(R1),C' '             END OF ID?                                
         BNE   MEMBR180               NO: LOOP                                  
*                                                                               
         MVI   0(R8),C'.'             PLACE TRAILING PERIOD                     
         MVC   1(7,R8),=C'LIBRARY'    COMPLETE DSN                              
*                                                                               
MEMBR200 DS    0H                                                               
         MVC   PANLIBSV,M02XXDSN   SAVE PAN LIBRARY DSN                         
         MVC   MEMSAVE,M02XXMEM    SAVE MEMBER NAME                             
         XC    TXTMBKEY,TXTMBKEY   DON'T ALLOCATE BY MEMBER                     
         BAS   RE,ALLOC             ALLOCATE LIBRARY                            
         ST    RF,RTNCODE                                                       
         LTR   RF,RF               ALLOCATION ERRORS                            
         BNZ   RETURN              YES.. RETURN TO CALLER                       
         BAS   RE,OPENLIB           OPEN LIBRARY                                
         ST    RF,RTNCODE                                                       
         LTR   RF,RF               OPEN ERRORS                                  
         BNZ   RETURN              YES.. RETURN TO CALLER                       
         MVC   AUDIT,=C'AUDIT   '  TELL PAM TO RETURN AUDIT INFO                
         BAS   RE,SEARCH            LOCATE MEMBER IN LIBRARY                    
         ST    RF,RTNCODE                                                       
         LTR   RF,RF               MEMBER NOT FOUND IN LIBRARY                  
         BNZ   MEMBR220            YES.. RETURN TO CALLER                       
*                                                                               
* CONSTRUCT DDS AUDIT STAMP                                                     
*                                                                               
         MVC   AUDMEM,MEMSAVE      BOOK=                                        
         LA    R8,SAVENTRY                                                      
         USING DIRENTRY,R8                                                      
         MVC   AUDLVL,DLEVEL       LEVEL=                                       
*                                                                               
*                                  CONVERT DATE TO MMMDD/YY FORMAT              
         GOTOR ,DMCB,(4,DDATEM),(11,AUDDATE)                                    
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         DROP  R8                                                               
*                                                                               
* NOTE: LUPDTIM COULD BE BLANKS IF IT'S A VERY OLD PAN MEMBER.                  
         LARL  R1,ALLAUDT                      TIME OF LAST UPDATE...           
         MVC   AUDTIME,LUPDTIM-ALLAUDT(R1)     ...FROM AUDIT INFO               
*                                                                               
         BAS   RE,USERRTN           CLASSIFY MEMBER AND GET OUTNAME             
         ST    RF,RTNCODE                                                       
*                                                                               
MEMBR220 DS    0H                                                               
         BAS   RE,CLOSLIB                                                       
         BAS   RE,DEALLOC                                                       
*                                                                               
* TAKE ADDITIONAL ACTION WHEN WE CAN PREDICT IN ADVANCE THAT PANAPT             
* WILL BE GENERATING SPECIFIC BACKOUT WARNINGS OF INTEREST TO US.               
*                                                                               
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BNE   MEMBR280            NO                                           
         CLC   M02XXLSN,=C'PROD'   YES: BACKOUT FROM PROD?                      
         BE    MEMBR260            YES                                          
*                                                                               
* THERE IS A BUG IN PANAPT. IF A MOVE REQUEST HAS BEEN PROMOTED TO AN           
* INTERMEDIATE LEVEL, AND THE "C" (CONCURRENT DEVELOPMENT) FLAG IS              
* POSTED ON A MEMBER BECAUSE ANOTHER MR HAS TRANSFERRED ASSIGNMENT AND          
* PROMOTED IT TO PROD, THE BAK WILL NOT GENERATE A BACKOUT WARNING, BUT         
* THE 'SUB' WILL FAIL BECAUSE OF THE "C" FLAG. THAT CAN'T POSSIBLY BE           
* CORRECT PANAPT BEHAVIOR.                                                      
*                                                                               
* IT IS LEGITIMATE TO BACK OUT A MR FROM STGE IF A "C" FLAG IS POSTED,          
* SO IF THAT HAPPENS, WE ISSUE A HELPFUL WARNING MESSAGE TO EXPLAIN TO          
* THE PROGRAMMER WHAT NEEDS TO BE DONE. BUT IF THE BACKOUT IS FROM              
* PRKS, THEN SOMETHING IS VERY WRONG, BECAUSE A MR WHICH HAS BEEN               
* PROMOTED TO PRKS IS EFFECTIVELY IN PRODUCTION. A BACKOUT FROM PRKS            
* WHEN THERE IS A "C" FLAG POSTED PRESENTS A VERY REAL RISK OF                  
* REGRESSING LEGITIMATE CHANGES. SO WE ISSUE A DIFFERENT (MORE SCARY)           
* WARNING IN THAT CASE.                                                         
*                                                                               
* NOTE: THE MRCDFLG FIELD IN THE MEMBER RECORD IS DOCUMENTED AS BEING           
* THE CONCURRENT DEVELOPMENT FLAG. UNFORTUNATELY, THERE IS NO                   
* DOCUMENTATION IN THE APAMMMBR DSECT (OR ELSEWHERE) OF THE *VALUE* OF          
* THAT FIELD WHEN THE FLAG IS ON. EMPIRICAL OBSERVATION HAS SHOWN THAT          
* A "Y" IN THAT FIELD MEANS THAT THE "C" FLAG IS ON, SO THAT'S WHAT'S           
* BEING TESTED HERE. (WE DON'T HAVE MUCH OF A CHOICE, BECAUSE WE AREN'T         
* REALLY GETTING MUCH SUPPORT FROM CA ON PANAPT ANY LONGER, AND THIS            
* BUG WILL NEVER BE FIXED.)                                                     
*                                                                               
         CLI   MRCDFLG,C'Y'        CONCURRENT DEVELOPMENT FLAG ON?              
         BNE   MEMBR280            NO                                           
*                                                                               
         MVC   RTNCODE,=F'4'       SET RC=4 (REGARDLESS OF THE MESSAGE)         
         CLC   M02XXLSN,=C'PRKS'   YES: BACKOUT FROM PRKS?                      
         BNE   *+20                NO: MUST BE BACKOUT FROM STGE                
         MVC   MX10MR#,INVLSTMR    LAST MR TO MOVE THIS MEMBER TO PROD          
         MVC   M02XXMSG(MXERR10L),MXERR10                                       
         B     RETURN              USER ERROR EXIT                              
*                                                                               
         MVC   M02XXMSG(MXERR11L),MXERR11 STGE BACKOUT WITH "C" FLAG ON         
         B     RETURN              USER ERROR EXIT                              
*                                                                               
*                                                                               
* PANAPT GENERATES AN APCS1180-16 WARNING WHEN ANOTHER MR HAS                   
* ASSIGNMENT OF THE MEMBER BEING BACKED OUT. ON A BACKOUT OF PROD, THIS         
* MEANS THAT THE OWNER OF THE OTHER MR NEEDS TO BE WARNED WHEN THE              
* BACKOUT HAPPENS, BECAUSE IT MAY BE NECESSARY TO REMOVE BAD CODE FROM          
* THAT VERSION. RATHER THAN RELY SOLELY ON VERBAL COMMUNCATION BETWEEN          
* BOTH PROGRAMMERS, WE PUSH AN E-MAIL TO THE PROGRAMMER WHO HAS                 
* ASSIGNMENT OF THE MEMBER WHEN THE BACKOUT ACTUALLY OCCURS (I.E.,              
* WHEN THE BATCH JOB RUNS).                                                     
*                                                                               
MEMBR260 DS    0H                                                               
         CLI   INVASFLG,C'Y'       IS MEMBER ASSIGNED TO ANOTHER MR?            
         BNE   MEMBR280            NO                                           
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   MEMBR280            NO: DON'T DO THIS ON A BAK COMMAND           
         BRAS  RE,BKOTWARN         SEND THE WARNING E-MAIL                      
*                                                                               
MEMBR280 DS    0H                                                               
*&&DO                                                                           
*                                                                               
* THE CODE THAT FOLLOWS HERE IS OBSOLETE, BECAUSE THE "SEC" LIBCODE             
* NO LONGER EXISTS. HOWEVER, IT IS A WORKING EXAMPLE OF HOW TO ENFORCE          
* A MEMBER NAMING CONVENTION REQUIRING THAT A PARTICULAR MEMBER NAME            
* PREFIX BE ASSOCIATED WITH A GIVEN LIBCODE. THIS WOULD NORMALLY ONLY           
* BE NECESSARY IF THERE ARE RACF PROFILES SET UP WHICH ARE ASSOCIATED           
* WITH THAT PREFIX.                                                             
*                                                                               
* MEMBERS BEGINNING WITH 'DDPAPTS' MUST HAVE LIBCODE SEC/P, AND                 
* MEMBERS OF LIBCODE SEC/P MUST BEGIN WITH 'DDPAPTS'                            
*                                                                               
         MVC   PVCOND12,=C'MUST '                                               
         CLC   =C'DDPAPTS',MEMSAVE MEMBER NAME BEGINS WITH 'DDPAPTS*'?          
         BNE   *+18                NO                                           
         CLC   MRLIBC,=C'SEC P  '  IS IT THE SEC(URITY) LIBCODE?                
         BE    RETURN              YES: CONTINUE                                
         B     *+20                NO: ERROR                                    
         MVC   PVCOND12,=C'CAN''T'                                              
         CLC   MRLIBC,=C'SEC P  '  IS IT THE SEC(URITY) LIBCODE?                
         BNE   MEMBR300            NO: CONTINUE                                 
*                                                                               
         MVC   PVMEM12,MEMSAVE     LIBCODE SEC/P <==> MEMBER 'DDPAPTS*'         
         MVC   M02XXMSG(PVERR12L),PVERR12   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     RETURN              USER ERROR EXIT                              
*                                                                               
MEMBR300 DS    0H                                                               
*&&                                                                             
         ICM   RF,15,RTNCODE       MEMBER CLASSIFICATION CORRECT?               
         BNZ   RETURN              NO.. RETURN TO CALLER                        
*                                                                               
* DDSCRGEN PUTS A SPECIFIC STRING TO THE PANVALET MEMBER COMMENT FIELD          
* OF AUTO-GENERATED SCREEN DSECT MEMBERS. IF WE SEE THAT COMMENT IN THE         
* MEMBER, WE KNOW THAT THE PROGRAMMER HAS ADDED A SCREEN DSECT MEMBER           
* TO HIS/HER MOVE REQUEST. WE ALSO CHECK FOR A MEMBER NAME BEGINNING            
* WITH "RM". BOTH CASES REPRESENT OUTPUT OBJECTS WHICH ARE MOVED                
* AUTOMATICALLY BY THE PANAPT MODELS, AND (UNLESS THE LIBCODE IS                
* SPECIFICALLY INTENDED TO MOVE SUCH MEMBERS, SUCH AS APPL/DEL), WE             
* KNOW THAT THIS REPRESENTS A PROGRAMMER ERROR WHICH WOULD OTHERWISE            
* RESULT IN A "SUB" FAILURE DURING PANVALET MEMBER MOVES.                       
*                                                                               
         CLC   MEMTYPE,=C'DELE'    IF THE LIBCODE IS APPL/DEL, THEN...          
         BE    MEMBR310            ...ANY MEMBER MAY BE INCLUDED IN MR          
*                                                                               
         LARL  R1,ALLAUDT          IS THE MEMBER A SCREEN DSECT?                
         CLC   =C'SCRGEN-GENERATED SCREEN DSECT',REALCMT-ALLAUDT(R1)            
         BE    *+14                YES: ERROR                                   
         CLC   =C'RM',MEMSAVE      IS IT AN OBJECT MODULE?                      
         BNE   MEMBR305            YES: ERROR                                   
*                                                                               
         MVC   MX9MEM,MEMSAVE      MEMBER WRONGLY INCLUDED IN MR                
         MVC   M02XXMSG(MXERR09L),MXERR09   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     RETURN              USER ERROR EXIT                              
*                                                                               
MEMBR305 DS    0H                                                               
*&&US                                                                           
* ALL DC* MEMBERS IN THE US ARE MANAGED BY ALFRESCO NOW. THEY ARE               
* PERMANENTLY LOCKED ON 'PAN.APPL.LIBRARY'.                                     
         CLC   =C'DC',MEMSAVE      IS IT A DC* MEMBER?                          
         BNE   MEMBR310            NO                                           
         MVC   MX19MEM,MEMSAVE     YES: MEMBER WRONGLY INCLUDED IN MR           
         MVC   M02XXMSG(MXERR19L),MXERR19   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     RETURN              USER ERROR EXIT                              
*                                                                               
*&&                                                                             
MEMBR310 DS    0H                                                               
         CLC   MEMTYPE,=C'INCL'    IS IT INCL?                                  
         BE    RETURN              YES: RETURN                                  
         CLC   MEMTYPE,=C'MACR'    IS IT MACR?                                  
         BE    RETURN              YES: RETURN                                  
         CLC   MEMTYPE,=C'ACAT'    IS IT ACAT?                                  
         BE    RETURN              YES: RETURN                                  
         CLC   MEMTYPE,=C'SRCE'    IS IT SRCE (ONLY)?                           
         BE    RETURN              YES: RETURN                                  
         CLC   MEMTYPE,=C'DELE'    IS IT APPL/DEL?                              
         BE    RETURN              YES: RETURN                                  
*                                                                               
         CLC   MEMTYPE,=C'DICT'    IS IT A DATA DICTIONARY EQU?                 
         BE    MEMBR312            YES: MUST CHECK FOR MULTIPLE PHASES          
         MVC   M02XXMEM,BLANKS                                                  
         MVC   M02XXMEM(8),BASENAME  SET THE *BASE* PHASE NAME                  
         BAS   RE,CHKTESTLIB       WARN IF LIVE PHASE EXISTS ON TESTLIB         
         OC    RTNCODE,RTNCODE     OKAY OPENING AND ALLOCATING TESTLIB?         
         BNZ   RETURN              NO: JUST GIVE UP!                            
         B     MEMBR315                                                         
*                                                                               
MEMBR312 DS    0H                                                               
         LH    R2,DICTCNT          NUMBER OF DICTIONARY PHASES                  
         MVC   M02XXMEM,BLANKS                                                  
         DO    FROM=(R2)           PROCESS EACH DICTIONARY TABLE ENTRY          
           LAY   R1,-1(,R2)          R1 IS TABLE INDEX                          
           MHI   R1,8*2              X TABLE ENTRY LENGTH                       
           LA    R1,DICTTBL(R1)      POINT TO TABLE ENTRY                       
           MVC   M02XXMEM(8),8(R1)   SET THE *BASE* DICT. MEMBER NAME           
           BAS   RE,CHKTESTLIB       WARN IF LIVE PHASE IS ON TESTLIB           
           OC    RTNCODE,RTNCODE     OK OPENING AND ALLOCATING TESTLIB?         
           BNZ   RETURN              NO: JUST GIVE UP!                          
         ENDDO ,                   PROCESS NEXT DICTIONARY ENTRY                
*                                                                               
MEMBR315 DS    0H                                                               
         LR    RF,R5               A(APAMLIB2 DSECT)                            
         AHI   RF,4096             REQUIRES A SECOND BASE REGISTER              
         USING APAMLIB2+4096,RF                                                 
         LA    RE,WORK             BUILD RECORD KEY IN FIELD 'WORK'             
         USING RELLIBCD,RE                                                      
         MVC   RLLIBCOD,L2RELLOD   FIND THE EXECUTABLE RELATED LIBCODE          
         MVC   RLLEVEL,M02XXLSN    AND LEVEL SHORT NAME                         
         DROP  RF                                                               
*                                                                               
         LARL  RE,RELLIBC          RELATED LIBCODE TABLE                        
MEMBR320 DS    0H                                                               
         CLI   0(RE),X'FF'         EOT?                                         
         BE    MEMBR340            LIBCODE/LEVEL NOT FOUND: IMPOSSIBLE          
         CLC   0(RLKEYLQ,RE),WORK  MATCH ON LIBCODE/LEVEL?                      
         BE    MEMBR360            YES                                          
         LA    RE,RLLENQ(RE)       NO: BUMP TO NEXT TABLE ENTRY                 
         B     MEMBR320                                                         
*                                                                               
MEMBR340 DS    0H                                                               
         MVC   PVERR#14,=C'01'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     RETURN              ERROR EXIT                                   
*                                                                               
MEMBR360 DS    0H                                                               
         MVC   M02XXDSN,RLDSN      SAVE THE RELATED LIBCODE DSN                 
         CLC   =C'ENV=SBX',M02XXPRM   ARE WE IN THE SANDBOX?                    
         BNE   *+16                NO: BYPASS ADJUSTMENT                        
         MVC   M02XXDSN(L'SANDBOX),SANDBOX                                      
         MVC   M02XXDSN+L'SANDBOX(L'SANDBOX),RLDSN                              
         DROP  RE                                                               
*                                                                               
         MVC   M02XXDDN,=CL8'NEWDD'                                             
         XC    TXTMBKEY,TXTMBKEY   DON'T ALLOCATE BY MEMBER                     
         BAS   RE,ALLOC            ALLOCATE THE LOAD LIBRARY                    
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   RETURN                                                           
*                                                                               
         BAS   RE,OPENLOD          OPEN THE LOAD LIBRARY                        
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   RETURN                                                           
*                                                                               
         CLC   MEMTYPE,=C'DICT'    IS IT A DATA DICTIONARY EQU?                 
         BE    MEMBR400            YES: PROCESS ALL DICTIONARY PHASES           
*                                                                               
         MVC   M02XXMEM,BLANKS                                                  
         MVC   M02XXMEM,OUTNAME                                                 
         BAS   RE,BLDL             LOOK FOR THE LOAD MODULE                     
         ST    RF,RTNCODE                                                       
         LTR   RF,RF               LOAD MODULE WAS FOUND?                       
         BNZ   MEMBR500            NO                                           
         B     MEMBR420                                                         
*                                                                               
MEMBR400 DS    0H                                                               
         LH    R2,DICTCNT          NUMBER OF DICTIONARY PHASES                  
         MVC   M02XXMEM,BLANKS                                                  
         DO    FROM=(R2)           PROCESS EACH DICTIONARY TABLE ENTRY          
           LAY   R1,-1(,R2)          R1 IS TABLE INDEX                          
           MHI   R1,8*2              X TABLE ENTRY LENGTH                       
           LA    R1,DICTTBL(R1)      POINT TO TABLE ENTRY                       
           MVC   M02XXMEM(8),0(R1)   SET THE *TEST* DICT. MEMBER NAME           
           BAS   RE,BLDL                                                        
           ST    RF,RTNCODE                                                     
           LTR   RF,RF               LOAD MODULE WAS FOUND?                     
           BNZ   MEMBR500            NO                                         
         ENDDO ,                   PROCESS NEXT TABLE ENTRY                     
*                                                                               
MEMBR420 DS    0H                                                               
* DETERMINE IF WE NEED TO VALIDATE THE AUDIT STAMP                              
         CLC   MEMTYPE,=C'LNK '    IS IT AN LM* OR LK* LINK BOOK?               
         BE    *+14                YES                                          
         CLC   MEMTYPE,=C'ALNK'    IS IT ALNK?                                  
         BNE   MEMBR470            NO                                           
*                                                                               
         BAS   RE,AUDVAL           COMPARE LEVEL STAMPS                         
         LTR   RF,RF               DO THEY MATCH?                               
         BZ    MEMBR470            YES                                          
*                                                                               
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BNE   *+14                                                             
         NILF  GRF,X'00FF'         YES: CLEAR REASON CODE (JUST ASSUME          
         B     MEMBR440            ...THE LEVEL STAMPS ARE OUT-OF-SYNC)         
*                                                                               
         LR    RE,RF               RETURN CODE FROM AUDVAL                      
         SRL   RE,24                                                            
         CASENTRY RE               RE = REASON CODE FROM AUDVAL                 
           CASE 1                    DYNAMIC ALLOCATION FAILURE                 
             MVC   PDREQ07,PREQ                                                 
             MVC   PDDDN07,PDDNAM                                               
             MVC   PDRTN07,PRETURN                                              
             MVC   M02XXMSG(PDERR07L),PDERR07                                   
           CASE 2                    LOAD MODULE IS NX (NOT EXECUTABLE)         
             MVC   PDPHA11,OUTNAME     LOAD MODULE NAME                         
             MVC   M02XXMSG(PDERR11L),PDERR11                                   
           CASE 3                    LEVEL STAMP IS OUT-OF-SYNC                 
             MVC   PVMEM08,MEMSAVE     PAN MEMBER NAME                          
             MVC   M02XXMSG(PVERR08L),PVERR08                                   
         ENDCASE ,                                                              
*                                                                               
         NILF  GRF,X'00FF'         CLEAR REASON CODE FROM RF                    
         ST    RF,RTNCODE                                                       
         B     MEMBR500            USER ERROR EXIT                              
*                                                                               
MEMBR440 DS    0H                  WE *MIGHT* BE OUT-OF-SYNC ON BACKOUT         
*                                                                               
* WE *MIGHT* BE OUT-OF-SYNC. BECAUSE THE FORMAT OF THE LEVEL STAMPS             
* HAS CHANGED OVER THE YEARS, WE MIGHT NOT BE ABLE TO FIND A MATCH,             
* BUT THAT DOESN'T NECESSARILY MEAN THAT THE BACKUP SOURCE IS OUT-OF-           
* SYNC WITH ITS ASSOCIATED LOAD MODULE.                                         
*                                                                               
* ON FEB13/06, A VERSION OF PANACEA WAS INSTALLED IN THE US WHICH               
* GENERATES LEVEL STAMPS THAT INCLUDE THE *TIME* THAT THE MEMBER WAS            
* LAST UPDATED. IF WE ALLOW TIME FOR THIS NEW LEVEL STAMP FORMAT TO             
* WORK ITS WAY INTO THE LOAD MODULES, IT'S PRESUMABLY SAFE TO SAY               
* THAT IF THE BACKUP SOURCE MODULE WAS LAST UPDATED IN OR AFTER 2007,           
* THE LEVEL STAMPS REALLY SHOULD MATCH.                                         
*                                                                               
* IN THE WORST CASE, WE MIGHT GENERATE A FALSE POSITIVE WARNING, OR             
* FAIL TO GENERATE AN APPROPRIATE WARNING, BUT THIS HEURISTIC IS                
* PROBABLY GOOD ENOUGH 95% OF THE TIME, WHILE ELIMINATING A VAST NUMBER         
* OF FALSE POSITIVES FOR LOAD MODULES WITH AN OLD-STYLE LEVEL STAMP.            
*                                                                               
         LR    R0,RF               SAVE RETURN CODE FROM AUDVAL                 
*&&UK                                                                           
         MVC   WORK+0(2),AUDDATE+3   DD      FROM MMMDD/YY                      
         MVC   WORK+2(3),AUDDATE+0     MMM                                      
         MVC   WORK+5(2),AUDDATE+6        YY                                    
         GOTOR ,DMCB,WORK,(X'40',DBLWRD)                                        
*&&                                                                             
*&&US*&& GOTOR ,DMCB,AUDDATE,(X'40',DBLWRD)                                     
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB(4),DMCB        RETURNED P1 FROM DATVAL                      
         BNZ   MEMBR460            RESULT IS OKAY                               
*                                                                               
         MVC   PVERR#14,=C'02'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     MEMBR500            ERROR EXIT                                   
*                                                                               
MEMBR460 DS    0H                                                               
         GOTOR ,DMCB,DBLWRD,(15,FULL)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CP    FULL,=X'0107001F'   JAN1/2007 (FULLWORD PACKED JULIAN)           
         BL    MEMBR500            LEVEL STAMP IS OLD-STYLE: ASSUME OK          
*                                                                               
* IF THE BACKUP SOURCE MODULE (THAT WE'RE ABOUT TO RESTORE) WAS                 
* PROMOTED VIA LIBCODE "SRCE", THEN WE *EXPECT* TO BE OUT-OF-SYNC,              
* BECAUSE THE SOURCE CODE WAS PROMOTED WITHOUT ITS LOAD MODULE.                 
*                                                                               
         CLC   MRLIBCOD,=C'SRCE'   PREVIOUS PROMOTION WAS VIA SRCE?             
         BE    MEMBR500            YES: DON'T COMPLAIN!                         
*                                                                               
         ST    R0,RTNCODE          SAVED RETURN CODE FROM AUDVAL                
*                                                                               
         CLC   M02XXLSN,=C'STGE'   BACKOUT FROM STGE?                           
         BNE   MEMBR465            NO                                           
*                                                                               
         MVC   PVMEM15,OUTNAME     STAGED LOAD MODULE NAME                      
         MVC   PVDSN15,PANLIBSV    PRESUMABLY, A PERSONAL PAN LIBRARY           
         MVC   M02XXMSG(PVERR15L),PVERR15   RETURN ERROR MESSAGE                
         B     MEMBR500                                                         
*                                                                               
MEMBR465 DS    0H                                                               
         MVC   PVOUTM13,OUTNAME    BACKUP LOAD MODULE NAME                      
         MVC   M02XXMSG(PVERR13L),PVERR13   RETURN ERROR MESSAGE                
         B     MEMBR500                                                         
*                                                                               
MEMBR470 DS    0H                                                               
         CLI   BKOTFLAG,C'Y'       BACKOUT?                                     
         BE    MEMBR500            YES                                          
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   MEMBR500            NO - EXIT                                    
*                                                                               
         BAS   RE,CLOSLOD          CLOSE THE LOAD LIBRARY                       
         BAS   RE,DEALLOC          UNALLOCATE THE LOAD LIBRARY                  
*                                                                               
         MVC   M02XXDDN,=CL8'NEWDD'                                             
         MVC   TXTMBKEY,=AL2(DALMEMBR) ASK FOR ALLOCATION BY MEMBER             
         BAS   RE,ALLOC            ALLOCATE THE SPECIFIC LOAD MODULE            
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   RETURN                                                           
*                                                                               
         BAS   RE,OPENLOD          REOPEN THE LOAD LIBRARY FOR MEMBER           
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   RETURN                                                           
*                                                                               
         MVI   IDRDONE,C'N'        LAST IDR RECORD NOT YET SEEN                 
         MVI   ZAPCOUNT,0          ASSUME LOAD MODULE IS NOT ZAPPED             
*                                                                               
MEMBR480 DS    0H                                                               
         CLI   IDRDONE,C'Y'        DID WE ALREADY READ ALL IDR RECS?            
         BE    MEMBR500            YES                                          
*                                                                               
         GET   PDSDCB              EXTRACT IDR INFO FROM LOAD MODULE            
*                                                                               
         CLI   0(R1),X'80'         IDR RECORD?                                  
         BNE   MEMBR480            NO: TRY AGAIN                                
*                                                                               
         TM    2(R1),X'80'         LAST IDR RECORD IN LOAD MODULE?              
         BZ    *+8                                                              
         MVI   IDRDONE,C'Y'        YES: REMEMBER THAT                           
*                                                                               
         TM    2(R1),X'01'         ZAP DATA?                                    
         BNO   MEMBR490                                                         
         MVC   ZAPCOUNT,3(R1)      YES                                          
         NI    ZAPCOUNT,X'3F'      THESE BITS CONTAIN THE ZAP COUNT             
         IF (CLI,ZAPCOUNT,NE,0)    IF THE ZAP COUNT ISN'T ZERO:                 
           MVC   WORK+1(3),6(R1)     ZAP DATE: YYDDDF                           
           MVI   WORK,0              ALWAYS FORCE 20TH CENTURY                  
           GOTOR ,DMCB,(6,WORK),(X'20',WORK+4)                                  
           L     RF,=V(DATCON)                                                  
           BRAS  RE,CALL_DDS_SUBRTN                                             
           GOTOR ,DMCB,WORK+4,(20,ZAPDATE)  CONVERT TO YYYYMMDD                 
           L     RF,=V(DATCON)                                                  
           BRAS  RE,CALL_DDS_SUBRTN                                             
         ENDIF ,                                                                
         B     MEMBR480            LOOK FOR MORE IDR RECORDS                    
*                                                                               
MEMBR490 DS    0H                                                               
         TM    2(R1),X'02'         BINDER DATA                                  
         BNO   MEMBR480                                                         
*                                                                               
         MVC   WORK+1(3),15(R1)    LINK DATE: YYDDDF                            
         MVI   WORK,0              ALWAYS FORCE 20TH CENTURY *Y2K*              
         LR    R0,R1               SAVE R1                                      
         GOTOR ,DMCB,(6,WORK),(X'20',DBLWRD)                                    
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         GOTOR ,DMCB,DBLWRD,(20,WORK)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         LR    R1,R0               RESTORE R1                                   
         UNPK  WORK+8(6),18(4,R1)  LINK TIME: 0HHMMSS+                          
         OI    WORK+13,X'F0'                                                    
*                                                                               
* PREVENT THE USER FROM PROMOTING A LOAD MODULE THAT WAS LINKED AFTER           
* THE EARLIEST RVP FOR THE CURRENT LEVEL.                                       
*                                                                               
         MVC   DUB,MRPRDATE        TODAY'S DATE: YYYYMMDD                       
         BAS   RE,CHKRVPDT         ENFORCE RVP DATE RULES?                      
         LTR   RF,RF                                                            
         BP    MEMBR500            NO                                           
         BZ    MEMBR495            YES                                          
*                                                                               
         MVC   PVERR#14,=C'05'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         MVC   RTNCODE,=F'4'       RC = 4                                       
         B     MEMBR500                                                         
*                                                                               
MEMBR495 DS    0H                                                               
         CLC   FIRSTRVP,WORK       MEMBER LINKED AFTER RVP?                     
         BH    MEMBR497            NO                                           
*                                                                               
         MVC   RTNCODE,=F'12'      YES: SET USER ERROR RETURN CODE              
         MVC   PDPHA10,M02XXMEM    LOAD MODULE NAME                             
         MVC   M02XXMSG(PDERR10L),PDERR10   RETURN ERROR MESSAGE                
         B     MEMBR500                                                         
*                                                                               
* PREVENT THE USER FROM PROMOTING A ZAPPED LOAD MODULE (EXCEPT IN THE           
* RARE CASE WHERE THE MODULE WAS ZAPPED, BUT THE ZAP WAS PRIOR TO THE           
* BIND DATE, WHICH CAN HAPPEN WITH THINGS LIKE PANACEA WHICH INCLUDE            
* VENDOR-SUPPLIED MODULES).                                                     
*                                                                               
MEMBR497 DS    0H                                                               
         CLI   ZAPCOUNT,0          WAS THE LOAD MODULE ZAPPED?                  
         BE    MEMBR480            NO                                           
         CLC   ZAPDATE,WORK        YES: WAS THE ZAP BEFORE THE BIND?            
         BL    MEMBR480                                                         
         MVC   RTNCODE,=F'12'      NO: SET USER ERROR RETURN CODE               
         MVC   PDPHA09,M02XXMEM    LOAD MODULE NAME                             
         MVC   M02XXMSG(PDERR09L),PDERR09   RETURN ERROR MESSAGE                
*                                                                               
MEMBR500 DS    0H                  LOAD MODULE EODAD                            
         BAS   RE,CLOSLOD          CLOSE THE LOAD LIBRARY                       
         BAS   RE,DEALLOC          UNALLOCATE THE LOAD LIBRARY                  
*                                                                               
RETURN   DS    0H                  EXIT MODULE                                  
         OC    PAM@,PAM@           WAS PAM MODULE LOADED                        
         BZ    RETURNX             NO... DO NOT DELETE MODULE                   
         DELETE EPLOC=PAM          DELETE PAM MODULE                            
         XC    PAM@,PAM@           CLEAR PAM POINTER                            
*                                                                               
RETURNX  DS    0H                  EXIT MODULE                                  
         LH    R1,MBRCOUNT                                                      
         LA    R1,1(R1)            INCREMENT MEMBER COUNT                       
         STH   R1,MBRCOUNT                                                      
         C     R1,DES#MEM          LAST MEMBER?                                 
         BL    RETURNY             NO: PROCEED                                  
         MVC   MRSAVE,BLANKS                                                    
         MVC   SRCSAVE,BLANKS                                                   
*                                                                               
RETURNY  EQU   *                                                                
         BRAS  RE,SNDMAIL          SEND EMAIL IF ANY ERROR                      
*                                                                               
*********CLOSE JMCMSG                                                           
         ICM   RF,15,RTNCODE       RETRIEVE RETURN CODE                         
         L     RD,4(,RD)           RESTORE LINKAGE TO CALLING PROGRAM           
         RETURN (14,12),RC=(15)    RETURN TO CALLING PROGRAM                    
         EJECT                                                                  
*&&DO                                                                           
* JMCMSG IS A MESSAGE DATASET FOR DEBUGGING.                                    
* BUILD ANY LINE OF TEXT IN FIELD JMCMSG1, AND THEN:                            
*        PUT   JMCMSG,JMCMSG1                                                   
JMCMSG   DCB   DDNAME=JMCMSG,DSORG=PS,RECFM=FB,LRECL=79,BLKSIZE=79,    X        
               MACRF=PM                                                         
JMCMSG1  DC    CL79' '                                                          
*&&                                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        OPEN ROUTINE                                                           
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
OPENLIB  DS    0H                                                               
*                                                                               
         ST    RE,OPENLIBL         SAVE RETURN LINKAGE                          
         LOAD  EPLOC=PAM           LOAD PAM MODULE                              
         ST    R0,PAM@             SAVE POINTER TO PAM ENTRY POINT              
         XC    PACB,PACB           CLEAR PAM CONTROL BLOCK                      
         MVI   FUNCTION,C'O'       SET OPEN FUNCTION CODE                       
         LA    R9,M02XXDDN         FETCH POINTER TO DDNAME                      
         L     RF,PAM@             FETCH POINTER TO PAM                         
         CALL  (15),(PACB,(R9),BACKUP),VL                                       
         LTR   RF,RF               OPEN SUCCESSFUL                              
         BZ    OPENLIBX            YES.. CONTINUE                               
         ST    RF,OPENLIBR         SAVE RETURN CODE                             
         BAS   RE,DEALLOC           DEALLOCATE LIBRARY                          
         L     RF,OPENLIBR         RESTORE ORIGINAL RETURN CODE                 
         MVC   PVFUNC04,=CL6'POPEN' SET PAM FUNCTION IN MESSAGE                 
         BAS   RE,PVMSG             FORMAT PAM ERROR MESSAGE                    
*                                                                               
OPENLIBX DS    0H                                                               
         L     RE,OPENLIBL         RESTORE RETURN LINKAGE                       
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        OPEN LOADLIB                                                           
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
OPENLOD  DS    0H                                                               
         ST    RE,OPENLIBL          SAVE RETURN LINKAGE                         
*                                                                               
         LA    R9,PDSDCB           FETCH POINTER TO DCB                         
         USING IHADCB,R9           ESTABLISH BASE ADDRESSABILITY                
*                                                                               
         MVC   PDSDCB(PDSDCBL-PDSDCB),DUMMYDCB REFRESH DCB                      
         MVC   DCBDDNAM,M02XXDDN   SET DDNAME IN DCB                            
         CLC   M02XXACT,=C'MEMPURGE'   PURGE?                                   
         BE    OPENLOD1            YES.. MUST OPEN FOR UPDATE.                  
         OPEN  (PDSDCB,(INPUT))    OPEN LIBRARY FOR INPUT                       
         B     OPENLOD2                                                         
OPENLOD1 DS    0H                                                               
         OPEN  (PDSDCB,(UPDAT))    OPEN LIBRARY FOR UPDATE                      
OPENLOD2 DS    0H                                                               
         TM    DCBOFLGS,X'10'      OPEN SUCCESSFUL?                             
         BO    OPENLODX            YES.. CONTINUE                               
         BAS   RE,DEALLOC           DEALLOCATE LIBRARY                          
         MVC   PDDSN04,M02XXDSN    MOVE DSN TO MESSAGE                          
         MVC   M02XXMSG(PDERR04L),PDERR04 RETURN OPEN FAILED MSG                
         LA    RF,12               SET PROCESSING ERROR RETURN CODE             
*                                                                               
OPENLODX DS    0H                                                               
         DROP  R9                                                               
         L     RE,OPENLIBL          RESTORE RETURN LINKAGE                      
         BR    RE                  EXIT                                         
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        SEARCH ROUTINE                                                         
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
SEARCH   DS    0H                                                               
         ST    RE,SEARCHL          SAVE RETURN LINKAGE                          
*                                                                               
         XC    ACTION,ACTION       CLEAR ACTION CODE                            
         MVI   FUNCTION,C'S'       SET SEARCH FUNCTION CODE                     
         MVC   NAME1(L'M02XXMEM),M02XXMEM    FETCH MEMBER NAME                  
         L     RF,PAM@             FETCH POINTER TO PAM                         
         CALL  (15),(PACB,DIRNTRY,NAME1,NAME2,ALLAUDT,SUBSET),VL                
         MVC   SAVENTRY,DIRNTRY    SAVE FOR AUDIT VALIDATION                    
         LTR   RF,RF               MEMBER FOUND IN LIBRARY                      
         BZ    SEARCHXT            YES.. CONTINUE                               
         CHI   RF,8                GENERAL ERROR?                               
         BNE   SEARCHE             NO... CONTINUE                               
         CLC   ACTION,=X'00000017' MEMBER NOT FOUND IN LIBRARY                  
         BNE   SEARCHE             NO... PROCESS OTHER ERROR                    
         MVC   PVMEM05,M02XXMEM    MOVE MEMBER TO MESSAGE                       
         MVC   PVDSN05,M02XXDSN    MOVE DSN TO MESSAGE                          
         MVC   M02XXMSG(PVERR05L),PVERR05 RETURN MEMBER NOT FOUND MSG           
         LA    RF,4                SET MEMBER NOT FOUND RETURN CODE             
*&&DO                                                                           
*** REMOVED MAR/2020 BY DEIS. WE BELIEVE THAT WE *SHOULD* GENERATE A            
*** BACKOUT WARNING IF A PAN MEMBER ISN'T FOUND. IN PARTICULAR, IF A MR         
*** MEMBER HAS AN ASSOCIATED OUTPUT OBJECT, THEN WE CAN'T POSSIBLY KNOW         
*** THAT OBJECT'S *NAME* WITHOUT ITS SOURCE MODULE (BECAUSE WE NEED TO          
*** EXAMINE THE *PHASE, OR *CATALP, ETC CARD TO DERIVE THE NAME).               
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BNE   SEARCHXT            NO: PROCEED TO EXIT                          
         LA    RF,8                SET PASSIVE RETURN CODE                      
*&&                                                                             
         B     SEARCHXT            EXIT SEARCH ROUTINE                          
*                                                                               
SEARCHE  DS    0H                                                               
         MVC   SEARCHA,ACTION      SAVE ACTION CODE                             
         MVC   ACTION,SEARCHA      RESTORE ORIGINAL ACTION CODE                 
         MVC   PVFUNC04,=CL6'PSRCH' SET PAM FUNCTION IN MESSAGE                 
         BAS   RE,PVMSG            FORMAT PAMS ERROR MESSAGE                    
         B     SEARCHXT            EXIT SEARCH ROUTINE                          
*                                                                               
SEARCHXT DS    0H                                                               
         L     RE,SEARCHL          RESTORE RETURN LINKAGE                       
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        PHASE SEARCH ROUTINE                                                   
*                                                                               
*----------------------------------------------------------------------         
BLDL     DS    0H                                                               
         ST    RE,SEARCHL          SAVE RETURN LINKAGE                          
*                                                                               
         MVC   BLDLMBR,M02XXMEM    SET MEMBER NAME IN BLDL PARM LIST            
         BLDL  PDSDCB,BLDLLIST     DETERMINE IF MEMBER IS IN LIBRARY            
         LTR   RF,RF               MEMBER FOUND?                                
         BZ    BLDLEX              YES.. CONTINUE                               
         CHI   RF,4                MEMBER NOT FOUND IN LIBRARY                  
         BNE   BLDLERR             NO... PROCESS OTHER ERROR                    
         MVC   PDMEM05,M02XXMEM    MOVE MEMBER TO MESSAGE                       
         MVC   PDDSN05,M02XXDSN    MOVE DSN TO MESSAGE                          
         MVC   M02XXMSG(PDERR05L),PDERR05 RETURN MEMBER NOT FOUND MSG           
         LA    RF,4                SET MEMBER NOT FOUND RETURN CODE             
*&&DO                                                                           
*** REMOVED JAN/2016 BY DEIS. WE BELIEVE THAT THERE IS NO LEGITIMATE            
*** REASON TO PREVENT A BACKOUT WARNING IF A LOAD MODULE ISN'T FOUND,           
*** ESPECIALLY BECAUSE THE "LOAD" JOB WILL FAIL ON THE "SUB"!                   
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BNE   BLDLEX              NO: PROCEED TO EXIT                          
         LA    RF,8                SET PASSIVE RETURN CODE                      
*&&                                                                             
         B     BLDLEX              EXIT BLDL ROUTINE                            
*                                                                               
BLDLERR  DS    0H                                                               
         CVD   R0,DBLWRD           SAVE REASON CODE                             
         OI    DBLWRD+7,X'0F'      ENSURE VALID SIGN                            
         UNPK  PDREA06,DBLWRD      MOVE REASON CODE TO MESSAGE                  
         MVC   M02XXMSG(PDERR06L),PDERR06 RETURN BLDL ERROR MESSAGE             
         LA    RF,12               SET BLDL ERROR RETURN CODE                   
         B     BLDLEX              EXIT BLDL ROUTINE                            
*                                                                               
BLDLEX   DS    0H                                                               
         L     RE,SEARCHL          RESTORE RETURN LINKAGE                       
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* SEARCH TO SEE IF A LIVE PHASE EXISTS IN THE TESTLIB                           
*                                                                               
*----------------------------------------------------------------------         
CHKTESTLIB DS  0H                                                               
         ST    RE,SRCTSTL          SAVE RETURN LINKAGE                          
*                                                                               
* DEIS JUN/2018:                                                                
*  PREVIOUSLY, WE ONLY CHECKED THE "LIVE PHASE ON TESTLIB" CONDITION            
*  WHEN THE ORIGIN LEVEL WAS "TEST". THAT'S BECAUSE "TEST" IS THE ONLY          
*  PANAPT LEVEL FOR WHICH 'DDS.TESTLIB' IS THE ORIGIN LOAD LIBRARY.             
*  'DDS.TESTLIB' HAD BEEN DYNAMICALLY ALLOCATED BY THIS EXIT ALREADY            
*  (BECAUSE IT HAD TO EXAMINE THE "A" PHASES), SO IT WAS EASY TO                
*  ADD THE BLDL TO CHECK FOR THE "LIVE" PHASES ON TESTLIB. BUT THIS             
*  MEANT THAT WE NEVER SENT THE "LIVE ON TESTLIB" WARNING E-MAILS ON A          
*  PROMOTION TO PROD (UNLESS THE /P LIBCODES WERE USED).                        
*  GOING FORWARD, WE NOW UNCONDITIONALLY ALLOCATE 'DDS.TESTLIB' AND             
*  LOOK FOR THE LIVE PHASES REGARDLESS OF THE ORIGIN LEVEL.                     
*                                                                               
*  THE ROUTINE ALLOCATES AND OPENS TESTLIB, USES BLDL TO CHECK FOR THE          
*  LIVE PHASES, THEN CLOSES AND DEALLOCATES TESTLIB, THEN EXITS. THE            
*  IDEA IS THAT THIS ROUTINE SHOULD DO ITS THING WITHOUT DISTURBING             
*  ANYTHING DOWNSTREAM HAVING TO DO WITH THE "REAL" BUSINESS OF THIS            
*  EXIT, WHICH IS TO EXAMINE THE COMPONENTS THAT ARE ACTUALLY GOING TO          
*  BE MOVED.                                                                    
*                                                                               
         XC    RTNCODE,RTNCODE     CLEAR RETURN CODE                            
*                                                                               
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BE    CHKTSTLX            YES - EXIT                                   
*                                                                               
         CLI   M02XXSRC,CLOSE      CLOSE PROCESSING?                            
         BE    *+12                YES: OKAY                                    
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   CHKTSTLX            NO - EXIT                                    
*                                                                               
         USING RELLIBCD,RE                                                      
         LARL  RE,TESTLIB          GET TESTLIB DSN                              
         MVC   M02XXDSN,RLDSN      SAVE THE RELATED LIBCODE DSN                 
         CLC   =C'ENV=SBX',M02XXPRM   ARE WE IN THE SANDBOX?                    
         BNE   *+16                NO: BYPASS ADJUSTMENT                        
         MVC   M02XXDSN(L'SANDBOX),SANDBOX                                      
         MVC   M02XXDSN+L'SANDBOX(L'SANDBOX),RLDSN                              
         DROP  RE                                                               
*                                                                               
         MVC   M02XXDDN,=CL8'TESTLIB'  USE DDNAME 'TESTLIB '                    
         XC    TXTMBKEY,TXTMBKEY   DON'T ALLOCATE BY MEMBER                     
         BAS   RE,ALLOC            ALLOCATE THE LOAD LIBRARY                    
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   CHKTSTLX            UNSUCCESSFUL ALLOCATE (WHY ???)              
*                                                                               
         BAS   RE,OPENLOD          OPEN THE LOAD LIBRARY                        
         ST    RF,RTNCODE                                                       
         LTR   RF,RF                                                            
         BNZ   CHKTSTL8            UNSUCCESSFUL OPEN (WHY ???)                  
*                                                                               
         CLC   M02XXMEM,BLANKS     ANY BASE PHASE NAME?                         
         BNH   CHKTSTL6            NO - EXIT                                    
*                                                                               
         MVC   BLDLMBR,M02XXMEM    SET MEMBER NAME IN BLDL PARM LIST            
         BLDL  PDSDCB,BLDLLIST     DETERMINE IF MEMBER IS IN LIBRARY            
         LTR   RF,RF               MEMBER FOUND?                                
         BNZ   CHKTSTL6            NO, EXIT                                     
*                                                                               
*                                  BUFFER THIS LIVE PHASE NAME                  
         LHI   R0,WRNEMQ                                                        
         LARL  R1,WRNEMAIL                                                      
CHKTSTL2 CLC   0(L'WRNEMAIL,R1),BLANKS     EMPTY SLOT?                          
         BNH   CHKTSTL4                    YES - ADD IT                         
         AHI   R1,L'WRNEMAIL                                                    
         BCT   R0,CHKTSTL2                 NO  - TRY NEXT SLOT                  
         B     CHKTSTL6                    NO MORE ROOM                         
CHKTSTL4 MVC   0(L'BASENAME,R1),M02XXMEM                                        
*                                                                               
CHKTSTL6 DS    0H                                                               
         BAS   RE,CLOSLOD          CLOSE THE LOAD LIBRARY                       
CHKTSTL8 DS    0H                                                               
         BAS   RE,DEALLOC          UNALLOCATE THE LOAD LIBRARY                  
*                                                                               
CHKTSTLX DS    0H                                                               
         L     RE,SRCTSTL          RESTORE RETURN LINKAGE                       
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        BLDL PARAMETER LIST                                                    
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
BLDLLIST DC    H'1'                NUMBER OF ENTRIES IN THIS BLDL LIST          
         DC    H'12'               LENGTH OF BLDL LIST                          
BLDLMBR  DC    CL08' '             MEMBER NAME                                  
BLDLTTRK DC    XL04'00'            TTR ,K                                       
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        USER SUBROUTINE                                                        
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
USERRC   DC    F'0'                                                             
USERRTN  DS    0H                                                               
*                                                                               
         ST    RE,USERRTNL         SAVE ROUTINE RETURN LINKAGE                  
         XC    USERRC,USERRC       POSIT GOOD RETURN                            
*                                                                               
*        USER CODE HERE                                                         
*                                                                               
         BAS   RE,CLSSFY                                                        
         L     RF,CLSSRC                                                        
         ST    RF,USERRC                                                        
         LTR   RF,RF                                                            
         BNZ   USERRTNX                                                         
*                                                                               
         CLC   MEMTYPE,=C'DELE'    APPL/DEL?                                    
         BE    USER05              YES: OKAY                                    
*                                                                               
         CLC   L2LIBC,MEMTYPE      MEMBER TYPE MATCH?                           
         BE    USER00              YES: CONTINUE                                
*                                                                               
         MVC   PVMEM06,MEMSAVE                                                  
         MVC   PVTYP06,MEMTYPE                                                  
         MVC   PVLIB06,L2LIBC                                                   
         MVC   PVSUB06,L2SUBC                                                   
         MVI   PV6SLASH,C' '                                                    
         CLC   L2SUBC,BLANKS                                                    
         BE    *+8                                                              
         MVI   PV6SLASH,C'/'                                                    
         MVC   M02XXMSG(PVERR06L),PVERR06   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER00   EQU   *                                                                
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BE    USER05              YES                                          
*                                                                               
* THE CODE BELOW, FROM HERE TO LABEL USER05, PREVENTS A USER FROM               
* PROMOTING A MEMBER WHICH WAS EDITED AFTER THE EARLIEST RVP FOR THE            
* CURRENT LEVEL.                                                                
*                                                                               
         LARL  R8,ALLAUDT                                                       
A        USING ALLAUDT,R8                                                       
*                                                                               
* NOTE: CA MUST HAVE ADDED THE "LAST UPDATE DATE/TIME" TO THE AUDIT             
*       DATA AREA AT SOME POINT *AFTER* DDS BEGAN USING PANVALET.               
*       I.E., A VERY OLD PAN MEMBER MIGHT NOT HAVE THIS INFORMATION IN          
*       THE AUDIT AREA. SO WE NE NEED TO ALLOW FOR THAT POSSIBILITY.            
*                                                                               
         CLC   LUPDDAT,BLANKS      "LAST UPDATE DATE" IS PRESENT?               
         BE    USER05              NO: IT MUST BE A VERY OLD MEMBER             
*                                                                               
* CONSTRUCT PAN MEMBER LAST UPDATE DATE/TIME IN FIELD WORK                      
* IN YYYYMMDDHHMMSS FORMAT.                                                     
* LUPDDAT IS MM/DD/YY REGARDLESS OF CTRY SO MUST BE RESHUFFLED FOR UK           
*                                                                               
*&&UK                                                                           
         MVC   WORK+0(3),A.LUPDDAT+3  DD/      FROM MM/DD/YY                    
         MVC   WORK+3(3),A.LUPDDAT+0     MM/                                    
         MVC   WORK+6(2),A.LUPDDAT+6        YY                                  
         GOTOR ,DMCB,WORK,(X'40',WORK+8)                                        
*&&                                                                             
*&&US*&& GOTOR ,DMCB,A.LUPDDAT,(X'40',WORK+8)                                   
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB(4),DMCB        RETURNED P1 FROM DATVAL                      
         BNZ   USER02              RETURN IS OKAY                               
*                                                                               
         MVC   PVERR#14,=C'03'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER02   DS    0H                                                               
         GOTOR ,DMCB,WORK+8,(20,WORK)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         MVC   WORK+8(2),A.LUPDTIM    HH                                        
         MVC   WORK+10(2),A.LUPDTIM+3 MM                                        
         MVC   WORK+12(2),A.LUPDTIM+6 SS                                        
         DROP  A                                                                
*                                                                               
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BE    USER03              YES: CHECK AGAINST RVP TIME                  
*                                                                               
* IT'S A CLOSE: MAKE SURE THAT THE LAST UPDATE TIME ISN'T *AFTER* THE           
* CURRENT TIME. IF IT IS, THIS ALMOST CERTAINLY MEANS THAT THE MEMBER           
* WAS TRANSFERRED FROM THE UK, AND WASN'T EDITED IN THE US BEFORE THE           
* CLOSE. SO THE MEMBER WILL STILL HAVE THE *UK* LAST UPDATE TIME, AND           
* ANY ATTEMPT TO PROMOTE THE MEMBER ON THE SAME DAY AS THE TRANSFER             
* FROM THE UK WILL CAUSE THE SUB TO FAIL ON THE CHECK AT LABEL USER03.          
* SO WE CATCH THAT CONDITION ON THE CLO HERE, RATHER THAN WAITING FOR           
* THE SUB TO FAIL.                                                              
*                                                                               
*                                  CONSTRUCT CURRENT DATE/TIME IN               
*                                   WORK+14(14) (FORMAT YYYYMMDDHHMMSS)         
         XC    TIMEWORK+12(4),TIMEWORK+12  PREPARE FOR TIME MACRO               
         TIME  DEC,TIMEWORK,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD                    
         MVI   TIMEWORK+12,X'0F'                                                
         UNPK  WORK+14(9),TIMEWORK+8(5)                                         
         MVI   TIMEWORK+3,X'0F'                                                 
         UNPK  WORK+22(7),TIMEWORK(4)                                           
*                                                                               
         MVC   DUB,WORK+14         CURRENT DATE: YYYYMMDD ONLY                  
         BAS   RE,CHKRVPDT         ENFORCE RVP DATE RULES?                      
         LTR   RF,RF                                                            
         BP    USER05              NO                                           
         BZ    USER02E             YES                                          
*                                                                               
         MVC   PVERR#14,=C'06'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         B     USER02H             USER ERROR EXIT                              
*                                                                               
USER02E  DS    0H                                                               
         CLC   WORK(14),WORK+14    COMPARE DATE/TIME PAIRS                      
         BNH   USER05                                                           
*                                                                               
         MVC   PVMEM10,M02XXMEM                                                 
         MVC   M02XXMSG(PVERR10L),PVERR10   RETURN ERROR MESSAGE                
*                                                                               
USER02H  DS    0H                                                               
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER03   DS    0H                                                               
* READ DDPAPTMRR OUTPUT RECORD TO GET THE EARLIEST VERIFICATION                 
* DATE/TIME (YYYYMMDDHHMMSS) FOR THE CURRENT LEVEL. WE MUST CONFIRM             
* THAT THE MEMBER WASN'T UPDATED AFTER IT'S BEEN VERIFIED.                      
*                                                                               
         OPEN  RVPDCB                                                           
         GET   RVPDCB,RECORD                                                    
         CLOSE RVPDCB                                                           
*                                                                               
         MVC   FIRSTRVP,RECORD     YYYYMMDDHHMMSS OF EARLIEST RVP               
         MVC   DUB,FIRSTRVP        YYYYMMDD ONLY                                
         BAS   RE,CHKRVPDT         ENFORCE RVP DATE RULES?                      
         LTR   RF,RF                                                            
         BP    USER05              NO                                           
         BZ    USER04              YES                                          
*                                                                               
         MVC   PVERR#14,=C'07'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         B     USER04E             USER ERROR EXIT                              
*                                                                               
USER04   DS    0H                                                               
         CLC   FIRSTRVP,WORK       COMPARE DATE/TIME PAIRS                      
         BH    USER05                                                           
*                                                                               
         MVC   PVMEM11,M02XXMEM    MEMBER WAS EDITED AFTER VERIFICATION         
         MVC   M02XXMSG(PVERR11L),PVERR11   RETURN ERROR MESSAGE                
*                                                                               
USER04E  DS    0H                                                               
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER05   DS    0H                                                               
         MVC   M02XXMEM,OUTNAME    POSIT: ACAT                                  
         CLC   MEMTYPE,=C'ACAT'    IS IT?                                       
         BE    USER10              YES: FIND OBJECT MEMBER                      
         CLC   MEMTYPE,=C'GEN '    IS IT A SCRGEN?                              
         BNE   USERRTNX            NO: EXIT                                     
*                                                                               
         MVC   M02XXMEM,MEMSAVE    YES: CONSTRUCT SCREEN DSECT NAME             
         LA    R1,M02XXMEM                                                      
*                                                                               
         CLI   0(R1),C' '          END OF NAME?                                 
         BE    *+12                YES: PROCEED                                 
         LA    R1,1(R1)            ADVANCE POINTER                              
         B     *-12                LOOP                                         
*                                                                               
         MVI   0(R1),C'D'          PLACE SUFFIX "D"                             
*                                                                               
* NOTE: THIS CODE ASSUMES THAT PAN *SOURCE* MEMBERS OF LIBCODES GEN AND         
*       ACAT RESIDE ON THE SAME PAN LIBRARY AS THEIR ASSOCIATED PAN             
*       OUTPUT OBJECTS (SCREEN DSECTS AND RELOS, RESPECTIVELY).                 
*                                                                               
USER10   EQU   *                                                                
         MVC   AUDIT,=C'NO-ENTRY'  TELL PAM NOT TO RETURN AUDIT INFO            
         BAS   RE,SEARCH           SEARCH FOR OUTPUT MEMBER                     
         ST    RF,USERRC                                                        
         LTR   RF,RF               FOUND?                                       
         BZ    USER20              YES: VALIDATE AUDIT STAMP                    
         MVC   PVMEM07,M02XXMEM                                                 
         MVC   PVDSN07,M02XXDSN                                                 
         MVC   M02XXMSG(PVERR07L),PVERR07   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER20   EQU   *                                                                
         CLC   MEMTYPE,=C'ACAT'    IS IT ACAT?                                  
         BNE   USERRTNX            NO: EXIT                                     
*                                                                               
USER22   EQU   *                                                                
         BAS   RE,PREAD                 GET A RECORD FROM THE MEMBER            
         CLC   =C'$*',RECORD            END OF MEMBER?                          
         BNE   USER25                   NO: CHECK FOR OBJECT .END               
*                                                                               
         MVC   PVMEM09,M02XXMEM         NO LEVEL STAMP FOUND                    
         MVC   M02XXMSG(PVERR09L),PVERR09   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER25   EQU   *                                                                
         CLC   OBJEND,RECORD       END CARD SEEN?                               
         BE    USER30              YES: CHECK AUDIT STAMP                       
*                                                                               
         CLC   OBJTXT,RECORD       IS IT A .TXT RECORD?                         
         BNE   USER22              NO.. FLUSH IT                                
         MVC   RECSAV1,RECSAV2     SAVE PREV .TXT RECORD                        
         MVC   RECSAV2,RECSAV3     SAVE PREV .TXT RECORD                        
         MVC   RECSAV3,RECORD+16   SAVE CURRENT .TXT RECORD                     
         B     USER22              GET NEXT                                     
*                                                                               
USER30   EQU   *                                                                
         LA    R8,RECSAV1                                                       
         LA    R9,129              MAX ITERATIONS                               
*                                                                               
         CLC   AUDSTAMP(AUDITLNQ),0(R8)   GOT A MATCH?                          
         BE    USERRTNX            YES                                          
         LA    R8,1(R8)            NO: BUMP TO NEXT CHARACTER                   
         BCT   R9,*-14             IF WE FALL THROUGH, NO MATCH FOUND           
*                                                                               
         CLI   BKOTFLAG,C'Y'       PROCESSING BACKOUT?                          
         BE    USER50              YES                                          
         MVC   PVMEM08,MEMSAVE     NO: ATTEMPT TO PROMOTE OUT-OF-SYNC           
         MVC   M02XXMSG(PVERR08L),PVERR08   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER50   DS    0H                  WE *MIGHT* BE OUT-OF-SYNC ON BACKOUT         
*                                                                               
* WE *MIGHT* BE OUT-OF-SYNC. BECAUSE THE FORMAT OF THE LEVEL STAMPS             
* HAS CHANGED OVER THE YEARS, WE MIGHT NOT BE ABLE TO FIND A MATCH,             
* BUT THAT DOESN'T NECESSARILY MEAN THAT THE BACKUP SOURCE IS OUT-OF-           
* SYNC WITH ITS ASSOCIATED OBJECT MODULE.                                       
*                                                                               
* ON FEB13/06, A VERSION OF PANACEA WAS INSTALLED IN THE US WHICH               
* GENERATES LEVEL STAMPS THAT INCLUDE THE *TIME* THAT THE MEMBER WAS            
* LAST UPDATED. IF WE ALLOW TIME FOR THIS NEW LEVEL STAMP FORMAT TO             
* WORK ITS WAY INTO THE OBJECT MODULES, IT'S PRESUMABLY SAFE TO SAY             
* THAT IF THE BACKUP SOURCE MODULE WAS LAST UPDATED IN OR AFTER 2007,           
* THE LEVEL STAMPS REALLY SHOULD MATCH.                                         
*                                                                               
* IN THE WORST CASE, WE MIGHT GENERATE A FALSE POSITIVE WARNING, OR             
* FAIL TO GENERATE AN APPROPRIATE WARNING, BUT THIS HEURISTIC IS                
* PROBABLY GOOD ENOUGH 95% OF THE TIME, WHILE ELIMINATING A VAST NUMBER         
* OF FALSE POSITIVES FOR OBJECT MODULES WITH AN OLD-STYLE LEVEL STAMP.          
*                                                                               
*&&UK                                                                           
         MVC   WORK+0(2),AUDDATE+3   DD      FROM MMMDD/YY                      
         MVC   WORK+2(3),AUDDATE+0     MMM                                      
         MVC   WORK+5(2),AUDDATE+6        YY                                    
         GOTOR ,DMCB,WORK,(X'40',DBLWRD)                                        
*&&                                                                             
*&&US*&& GOTOR ,DMCB,AUDDATE,(X'40',DBLWRD)                                     
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB(4),DMCB        RETURNED P1 FROM DATVAL                      
         BNZ   USER60              RETURN IS OKAY                               
*                                                                               
         MVC   PVERR#14,=C'04'     UNIQUE INTERNAL ERROR CODE                   
         MVC   M02XXMSG(PVERR14L),PVERR14   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
         B     USERRTNX            USER ERROR EXIT                              
*                                                                               
USER60   DS    0H                                                               
         GOTOR ,DMCB,DBLWRD,(15,FULL)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CP    FULL,=X'0107001F'   JAN1/2007 (FULLWORD PACKED JULIAN)           
         BL    USERRTNX            LEVEL STAMP IS OLD-STYLE: ASSUME OK          
*                                                                               
* IF THE BACKUP SOURCE MODULE (THAT WE'RE ABOUT TO RESTORE) WAS                 
* PROMOTED VIA LIBCODE "SRCE", THEN WE *EXPECT* TO BE OUT-OF-SYNC,              
* BECAUSE THE SOURCE CODE WAS PROMOTED WITHOUT ITS OBJECT MODULE.               
*                                                                               
         CLC   MRLIBCOD,=C'SRCE'   PREVIOUS PROMOTION WAS VIA SRCE?             
         BE    USERRTNX            YES: DON'T COMPLAIN!                         
*                                                                               
         MVC   PVOUTM13,OUTNAME    BACKUP OBJECT MODULE NAME                    
         MVC   M02XXMSG(PVERR13L),PVERR13   RETURN ERROR MESSAGE                
         LA    RF,4                SET VERIFY FAIL RETURN CODE                  
         ST    RF,USERRC                                                        
*                                                                               
USERRTNX DS    0H                                                               
         L     RF,USERRC           RESTORE RETURN CODE                          
         L     RE,USERRTNL         RESTORE RETURN LINKAGE                       
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        CLOSE SUBROUTINE                                                       
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CLOSLIB  DS    0H                                                               
         ST    RE,CLOSLIBL         SAVE RETURN LINKAGE                          
*                                                                               
         XC    ACTION,ACTION       CLEAR ACTION CODE                            
         MVI   FUNCTION,C'C'       SET CLOSE FUNCTION CODE                      
         L     RF,PAM@             FETCH POINTER TO PAM                         
         CALL  (15),(PACB),VL                                                   
         LTR   RF,RF               CLOSE SUCCESSFUL                             
         BZ    CLOSLIBX            YES.. CONTINUE                               
         MVC   PVFUNC04,=C'PCLOSE' SET PAM FUNCTION IN MESSAGE                  
         BAS   RE,PVMSG            FORMAT PAM ERROR MESSAGE                     
*                                                                               
CLOSLIBX DS    0H                                                               
         L     RE,CLOSLIBL         RESTORE RETURN LINKAGE                       
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        CLOSE LOADLIB                                                          
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CLOSLOD  DS    0H                                                               
         ST    RE,CLOSLIBL                                                      
*                                                                               
         CLOSE (PDSDCB)            CLOSE LIBRARY                                
         FREEPOOL PDSDCB                                                        
         L     RE,CLOSLIBL                                                      
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        PANVALET ERROR MESSAGES                                                
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PVMSG    DS    0H                                                               
*                                                                               
         CVD   RF,DBLWRD           CONVERT RC TO DECIMAL                        
         OI    DBLWRD+7,X'0F'      ENSURE VALID SIGN                            
         UNPK  PVRTN04,DBLWRD+6(2) SET RC IN ERROR MESSAGE                      
         MVC   PVTYPE04,=C'PV'     SET PANVALET MESSAGE ID                      
         CLI   ACTCODE,X'00'       PANVALET ERROR?                              
         BE    PVMSGC              YES.. CONTINUE                               
         MVC   PVTYPE04,=C'PM'     SET LIBRARY ACCESS MESSAGE ID                
         CLI   ACTCODE,X'01'       LIBRARY ACCESS ERROR?                        
         BE    PVMSGC              YES.. CONTINUE                               
         MVC   PVTYPE04,=C'MP'     SET MULTIPLE PARTITION MESSAGE ID            
         CLI   ACTCODE,X'02'       MULTIPLE PARTITION ERROR?                    
         BE    PVMSGC              YES.. CONTINUE                               
         MVC   PVTYPE04,=C'GE'     SET GENERLIZED EXIT MESSAGE ID               
*                                                                               
PVMSGC   DS    0H                                                               
         LLC   RF,ACTMSG           FETCH ERROR MESSAGE NUMBER                   
         CVD   RF,DBLWRD           CONVERT NUMBER TO DECIMAL                    
         OI    DBLWRD+7,X'0F'      ENSURE VALID SIGN                            
         UNPK  PVNUM04,DBLWRD+6(2) SET MESSAGE NUMBER IN ERROR MESSAGE          
         MVC   M02XXMSG(PVERR04L),PVERR04 RETURN PAM ERROR MESSAGE              
         LA    RF,12               SET PAM ERROR RETURN CODE                    
         BR    RE                  EXIT                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        ALLOCATION ROUTINE                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
ALLOC    DS    0H                                                               
*                                                                               
         ST    RE,ALLOCL           SAVE RETURN LINKAGE                          
         MVI   RQBLK.S99RBLN,20    LENGTH OF REQUEST BLOCK                      
         LA    R1,TXTPTRS          SET TEXT LIST POINTER                        
         ST    R1,RQBLK.S99TXTPP   *                                            
         MVI   RQBLK.S99VERB,S99VRBAL    REQUEST FOR ALLOCATION                 
         MVC   TXTDDN,M02XXDDN     MOVE DDNAME TO TEXT UNIT                     
         MVC   TXTDSN,M02XXDSN     MOVE DATASET NAME TO TEXT UNIT               
         MVC   TXTMEMBR,M02XXMEM   MOVE MEMBER NAME TO TEXT UNIT                
         MVC   TXTSTKEY,=AL2(DALSTATS) SET DATASET STATUS                       
         BAS   RE,DYNAM            ALLOCATE DATASET                             
         L     RE,ALLOCL           RESTORE RETURN LINKAGE                       
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        DEALLOCATION ROUTINE                                                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
DEALLOC  DS    0H                                                               
*                                                                               
         ST    RE,DEALLOCL         SAVE RETURN LINKAGE                          
         MVI   RQBLK.S99VERB,S99VRBUN    REQUEST FOR DEALLOCATION               
         MVC   TXTSTKEY,=AL2(DUNOVDSP) SET DATASET STATUS                       
         BAS   RE,DYNAM            DEALLOCATE DATASET                           
         L     RE,DEALLOCL         RESTORE RETURN LINKAGE                       
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        DYNALLOC SUBROUTINE                                                    
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
DYNAM    DS    0H                                                               
*                                                                               
         LA    R1,REQBKPTR         FETCH REQUEST BLOCK POINTER                  
         DYNALLOC                                                               
*                                                                               
         LTR   RF,RF               SUCCESSFUL DYNALLOC REQUEST?                 
         BZ    DYNAMXIT            YES.. EXIT                                   
         CHI   RF,4                RETURN CODE OF 4                             
         BNE   DYNAMMSG            NO... FORMAT ERROR MESSAGE                   
         CLC   RQBLK.S99ERROR,=X'1708'   DATASET NOT FOUND                      
         BNE   DYNAMMSG            NO... FORMAT ERROR MESSAGE                   
         MVC   PVDSN02,M02XXDSN    MOVE DSN TO ERROR MESSAGE                    
         MVC   M02XXMSG(PVERR02L),PVERR02 RETURN DSN NOT FOUND MSG              
         B     DYNAMXIT            RETURN TO CALLER                             
*                                                                               
DYNAMMSG DS    0H                                                               
         MVC   PVVERB03,=C'ALOC'   ASSUME WE'RE ALLOCATING                      
         CLI   RQBLK.S99VERB,S99VRBAL  REQUEST FOR ALLOCATION?                  
         BE    *+10                YES                                          
         MVC   PVVERB03,=C'UNAL'   NO: UNALLOCATION REQUEST                     
         MVC   PVDDN03,M02XXDDN    MOVE DDNAME TO ERROR MESSAGE                 
         MVC   PVDSN03,M02XXDSN    MOVE DSN TO ERROR MESSAGE                    
         LH    R0,RQBLK.S99ERROR   FETCH DYNALLOC ERROR CODE                    
         LA    R9,4                MAXIMUM TIMES THRU LOOP                      
*                                                                               
DYNAMMSH DS    0H                  ISOLATE EACH DIGIT OF ERROR CODE             
         SRDL  R0,4                ISOLATE NUMERIC NIBBLE OF DIGIT              
         SRL   R1,4                ADD ZONE NIBBLE TO DIGIT                     
         BCT   R9,DYNAMMSH         PROCESS NEXT DIGIT OF ERROR CODE             
         STCM  R1,B'1111',PVS9903  SET ERROR CODE IN MESSAGE                    
         TR    PVS9903,DYNAMCHR    TRANSLATE CODE TO READABLE FORMAT            
         MVC   M02XXMSG(PVERR03L),PVERR03 RETURN DYNALLOC ERROR MSG             
         LA    RF,12               SET ALLOCATION/DEALLOCATION RC               
*                                                                               
DYNAMXIT DS    0H                                                               
         BR    RE                  EXIT                                         
         DROP  RQBLK                                                            
         TITLE 'VERIFY MEMBER EXISTENCE IN A PANVALET LIBRARY - LOCAL E-        
               NVIRONMENT'                                                      
*----------------------------------------------------------------------         
*                                                                               
*        AUDIT STAMP VALIDATION ROUTINE                                         
*                                                                               
*----------------------------------------------------------------------         
AUDVALSV DC    F'0'                                                             
AUDR15   DC    F'0'                                                             
AUDVALID DC    CL8'AUDVAL'                                                      
AUDVAL   EQU   *                                                                
         ST    RE,AUDVALSV                                                      
*                                                                               
         LOAD  EP=APAS1S99            DYNAMIC ALLOCATION ROUTINE                
         ST    R0,ADDR1S99                                                      
*                                                                               
         MVC   PARMLIST,BLANKS                                                  
         MVC   PDSN,BLANKS                                                      
         MVC   PREQ,FREE                                                        
         MVC   PDDNAM,=CL8'OUTDD'                                               
         L     RF,ADDR1S99                                                      
         CALL  (15),PARMLIST                                                    
*                                                                               
         MVC   PREQ,DUMM                                                        
         MVC   PDDNAM,=CL8'OUTDD'                                               
         L     RF,ADDR1S99                                                      
         CALL  (15),PARMLIST                                                    
         LTR   RF,RF                                                            
         BZ    AUDVAL10                                                         
*                                                                               
         MVC   AUDR15,=F'4'        UNSUCCESSFUL ALLOCATE OF LOADMOD             
         MVI   AUDR15,1            SET REASON CODE IN HOB                       
         B     AUDVALX             USER ERROR EXIT                              
*                                                                               
AUDVAL10 EQU   *                                                                
         LA    R9,OUTNAME                                                       
         LA    R8,PDSDCB                                                        
         LA    R2,FULL                                                          
*                                                                               
* LOAD THE LOAD MODULE INTO CORE SO WE CAN SEARCH FOR ITS EMBEDDED              
* LEVEL STAMP. NOTE: MODULES WITH "RMODE ANY" WILL BE LOADED ABOVE THE          
* LINE, SO IT BEHOOVES US TO BE IN XA MODE WHILE WE SEARCH FOR THE              
* LEVEL STAMP!                                                                  
*                                                                               
         SAM31 ,                   ENTER XA MODE                                
*                                                                               
         LOAD  EPLOC=(R9),DCB=(R8),LOADPT=(R2),ERRET=AUDVAL12                   
AUDVAL12 DS    0H                                                               
         LTR   RF,RF               SUCCESSFUL LOAD OF LOAD MODULE?              
         BZ    AUDVAL15            YES                                          
         C     R1,=X'00000706'     S706 ("NX" NON-EXECUTABLE MODULE)?           
         JNE   *+2                 NO: UNSUCCESSFUL LOAD OF LOAD MODULE         
         MVC   AUDR15,=F'4'        LOAD MODULE IS NON-EXECUTABLE                
         MVI   AUDR15,2            SET REASON CODE IN HOB                       
         B     AUDVALX                                                          
*                                                                               
AUDVAL15 DS    0H                                                               
         L     RE,FULL             RE = A(LOAD MODULE)                          
         N     R1,=X'00FFFFFF'     HOB OF R1 = APF AUTHORIZATION CODE           
*                                  R1 = L'LOADMOD IN DOUBLEWORDS                
         BNZ   *+6                 MAKE SURE A LENGTH WAS RETURNED              
         DC    H'0'                A DEATH HERE MEANS THAT THE LOADMOD          
*                                   IS > 16M LONG, OR HAS RMODE(SPLIT).         
*                                   WE'LL NEED TO INVOKE THE CSVQUERY           
*                                   MACRO IF THIS EVER HAPPENS.                 
*                                                                               
         MHI   R1,8                R1 = L'LOADMOD IN BYTES                      
         AR    R1,RE               R1 = A(END OF LOAD MODULE)                   
         MVC   AUDR15,=F'4'        ASSUME NO MATCH WILL BE FOUND                
         MVI   AUDR15,3            SET REASON CODE IN HOB                       
*                                                                               
AUDVAL20 DS    0H                                                               
         LR    R0,R1               R0 = A(END OF LOAD MODULE)                   
         SR    R0,RE               R0 = # BYTES REMAINING TO COMPARE            
         CHI   R0,AUDITLNQ         IF LEVEL STAMP IS LONGER...                  
         BL    AUDVAL30            ...WE DON'T HAVE A MATCH                     
*                                                                               
         CLC   AUDSTAMP(AUDITLNQ),0(RE)   FOUND A MATCH?                        
         BE    *+12                YES                                          
         LA    RE,1(RE)            NO: TRY NEXT BYTE                            
         B     AUDVAL20                                                         
*                                                                               
         XC    AUDR15,AUDR15       SET RC = 0                                   
*                                                                               
AUDVAL30 DS    0H                                                               
         DELETE EPLOC=OUTNAME                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL UNLOAD                          
*                                                                               
AUDVALX  DS    0H                                                               
         SAM24 ,                   EXIT XA MODE                                 
*                                                                               
         MVC   PARMLIST,BLANKS                                                  
         MVC   PDSN,BLANKS                                                      
         MVC   PREQ,FREE                                                        
         MVC   PDDNAM,=CL8'OUTDD'                                               
         L     RF,ADDR1S99                                                      
         CALL  (15),PARMLIST                                                    
*                                                                               
         DELETE EPLOC=APAS1S99     DELETE DYNAMIC ALLOCATION ROUTINE            
         L     RF,AUDR15           REASON CODE IS IN HOB                        
         L     RE,AUDVALSV                                                      
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        DETERMINE IF WE SHOULD ENFORCE THE RVP DATE CHECK.                     
*                                                                               
* INPUT:  YYYYMMDD DATE IN FIELD "DUB"                                          
* OUTPUT: RETURN CODE IN RF                                                     
*          0 = ENFORCE THE RULES                                                
*          1 = DO NOT ENFORCE                                                   
*         -1 = INTERNAL ERROR                                                   
*                                                                               
* WE DO NOT PERMIT THE PROMOTION OF ANY MODULE IN WHICH:                        
*                                                                               
*  1. THE SOURCE WAS EDITED AFTER THE EARLIEST RVP FOR THE CURRENT              
*      LEVEL, OR                                                                
*  2. THE LOAD MODULE WAS LINKED AFTER THE EARLIEST RVP FOR THE CURRENT         
*      LEVEL.                                                                   
*                                                                               
* THE CHECK IS SENSITIVE TO THE MACHINE CLOCK TIME. SPECIFICALLY, IT            
* ASSUMES THAT THE CLOCK ONLY MOVES IN A FORWARD DIRECTION. THAT                
* ASSUMPTION IS INCORRECT DURING THE CLOCK CHANGE FROM DAYLIGHT                 
* SAVING TIME TO STANDARD TIME IN THE FALL. SO ON THAT DATE ONLY, IF            
* THE DATE CHECK FAILS, WE DON'T FAIL THE SUB. (I.E., WE GIVE THE               
* PROGRAMMER THE BENEFIT OF ANY DOUBT, ON THE ASSUMPTION THAT A DATE            
* ERROR WILL BE DUE TO THE CLOCK CHANGE.)                                       
*                                                                               
*----------------------------------------------------------------------         
CHKRVPSV DC    F'0'                                                             
CHKRVP15 DC    F'0'                                                             
CHKRVPWK DS    CL12                WORK AREA                                    
CHKRVPID DC    CL8'CHKRVP'                                                      
CHKRVPDT EQU   *                                                                
         ST    RE,CHKRVPSV                                                      
*                                                                               
         XC    CHKRVP15,CHKRVP15   ASSUME WE SHOULD ENFORCE THE RULES           
*                                                                               
* IF DUB CONTAINS HIGH VALUES, THEN DDPAPTMRR SET IT THAT WAY, BECAUSE          
* THERE WERE NO VERIFICATION RECORDS FOUND FOR THIS MR (WHICH IS                
* LEGITIMATE FOR LIBCODES SUCH AS SRCE/T).                                      
*                                                                               
         CLC   DUB,=8X'FF'         DATE IS SET TO HIGH VALUES?                  
         BE    CHKRVPDX            YES: IT WILL VALIDATE OKAY                   
*                                                                               
* IN THE US, DAYLIGHT SAVING TIME ENDS ON THE FIRST SUNDAY IN NOVEMBER.         
* IN THE UK, SUMMER TIME ENDS ON THE LAST SUNDAY IN OCTOBER.                    
*                                                                               
         GOTOR ,DMCB,(9,DUB),(0,DBLWRD)  INPUT YYYYMMDD (Y2K !)                 
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   CHKRVPWK(2),DBLWRD  EBCDIC YEAR                                  
*&&US*&& MVC   CHKRVPWK+2(4),=C'1031' US: START LOOKING AT OCTOBER 31           
*&&UK*&& MVC   CHKRVPWK+2(4),=C'1101' UK: START LOOKING AT NOVEMBER 1           
*                                                                               
CHKRVP10 DS    0H                                                               
*&&US*&& GOTOR ,DMCB,CHKRVPWK,CHKRVPWK,F'1'  US: FIND 1ST  SUN. IN NOV.         
*&&UK*&& GOTOR ,DMCB,CHKRVPWK,CHKRVPWK,F'-1' UK: FIND LAST SUN. IN OCT.         
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
         GOTOR ,DMCB,CHKRVPWK,CHKRVPWK+6                                        
         L     RF,=V(GETDAY)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLC   CHKRVPWK+6(3),BLANKS GETDAY CALL WAS SUCCESSFUL?                 
         BNE   *+14                                                             
         MVC   CHKRVP15,=F'-1'     NO: RETURN -1                                
         B     CHKRVPDX                                                         
*                                                                               
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   CHKRVP10            NO                                           
*                                                                               
         CLC   DBLWRD,CHKRVPWK     IS TODAY THE START OF STANDARD TIME?         
         BNE   CHKRVPDX                                                         
*                                                                               
         MVC   CHKRVP15,=F'1'      YES: RETURN 1                                
*                                                                               
CHKRVPDX DS    0H                                                               
         L     RF,CHKRVP15         RETURN CODE                                  
         L     RE,CHKRVPSV                                                      
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        CLASSIFICATION ROUTINE                                                 
*                                                                               
*----------------------------------------------------------------------         
CLSSRC   DC    F'0'                                                             
CLSSFYSV DC    F'0'                                                             
CLSSFY49 DC    6F'0'                    REG SAVE                                
CLSSFYID DC    CL8'CLSSFY'                                                      
CLSSFY   EQU   *                                                                
         ST    RE,CLSSFYSV                                                      
         STM   R4,R9,CLSSFY49                                                   
*                                                                               
         XC    CLSSRC,CLSSRC                                                    
         MVI   SRCEOKSW,C'N'            POSIT: SRCE LIBCODE NOT OKAY            
*                                                                               
         MVC   MEMTYPE,=C'DELE'         POSIT:  APPL/DEL                        
         CLC   L2LIBS,=C'APPLDEL'       APPL/DEL LIBCODE?                       
         BE    CLSSFYXX                 YES: APPL/DEL CONFIRMED                 
*                                                                               
         MVC   MEMTYPE,=C'MACR'         POSIT:  TYPE IS MACRO                   
         CLC   MEMSAVE(2),=C'MC'        MEMBER NAME STARTS WITH "MC"?           
         BE    CLSSFYEX                 YES: MACRO CONFIRMED                    
*                                                                               
         MVC   OUTNAME,BLANKS                                                   
         MVC   BASENAME,BLANKS                                                  
         MVC   MEMTYPE,BLANKS                                                   
         MVI   DICTSW,C'N'              *DICT CARD NOT FOUND YET                
         MVI   GENSW,C'N'               POSIT:  NOT A PANGEN                    
         MVI   LNKSW,C'N'               POSIT:  NOT A PANLNK                    
         MVI   XXGSW,C'N'               POSIT:  NOT APG/DPG/RRG                 
*                                                                               
GETPAN   EQU   *                                                                
         BAS   RE,PREAD                 GET A RECORD FROM THE MEMBER            
         CLC   =C'$*',RECORD            END OF MEMBER?                          
         BNE   CLASS                    NO: PROCESS RECORD                      
         CLC   MEMTYPE,BLANKS           TYPE DETERMINED YET?                    
         BNE   CLSSFYEX                 YES: EXIT                               
         MVC   MEMTYPE,=C'INCL'         NO: DECLARE IT INCL                     
         B     CLSSFYEX                 EXIT                                    
*                                                                               
CLASS    EQU   *                                                                
         CLI   GENSW,C'Y'                PANGEN PENDING?                        
         BE    GENCF                     YES: SEE IF WE CONFIRM                 
         CLI   LNKSW,C'Y'                PANLNK PENDING?                        
         BE    LNKCF                     YES: SEE IF WE CONFIRM                 
         CLI   XXGSW,C'Y'                APG/DPG/RRG PENDING PHASE?             
         BE    XXGPHASE                  YES: SEE IF WE FIND IT                 
         CLI   DICTSW,C'Y'               *DICT CARD FOUND?                      
         BE    DICTNXT                   YES: LOOK FOR ANOTHER                  
*                                                                               
         CLC   =C'*CATALP ',RECORD       IS IT A *CATALP ?                      
         BNE   CHKPHASE                  NO: CHECK FOR *PHASE                   
*                                                                               
         MVC   MEMTYPE,=C'ACAT'          RETURN MEMBER TYPE                     
         LA    R8,RECORD+8                                                      
ACAT00   EQU   *                                                                
         CLI   0(R8),C' '                START OF NAME?                         
         BNE   ACAT10                    YES: EXTRACT IT                        
         LA    R8,1(R8)                  NO: ADVANCE POINTER                    
         B     ACAT00                                                           
ACAT10   EQU   *                                                                
         TRT   0(9,R8),TRTBLANK          FIND BLANK OR COMMA                    
         SR    R1,R8                     LENGTH OF NAME                         
         STH   R1,NAMELN                 SAVE LENGTH                            
         MVC   OUTNAME(2),=C'RM'         RM PREFIX FOR OBJECT                   
         BCTR  R1,0                      DECREMENT FOR EX                       
         MVC   OUTNAME+2(0),0(R8)                                               
         EX    R1,*-6                                                           
         MVI   SRCEOKSW,C'Y'             SRCE OK FOR ACAT MEMBERS               
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
CHKPHASE EQU   *                                                                
         CLC   =C'*PHASE ',RECORD        IS IT A "PHASE" CARD?                  
         BNE   CHKGEN                    NO: CHECK FOR GEN                      
*                                                                               
         MVC   MEMTYPE,=C'LNK '          POSIT PANLNK                           
         MVI   LNKSW,C'Y'                SHOW LNK PENDING                       
         LA    R8,RECORD+7               ADVANCE PAST *PHASE                    
         BAS   RE,PHASERTN               EXTRACT PHASE NAME                     
         ICM   RF,15,CLSSRC              PHASE NAME ERROR?                      
         BNZ   CLSSFYXX                  YES: EXIT                              
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
CHKGEN   EQU   *                                                                
         CLC   =C'*GEN ',RECORD          IS IT A "*GEN " CARD?                  
         BNE   CHKDICT                   NO: CHECK FOR DICT                     
*                                                                               
         MVC   MEMTYPE,=C'GEN '                                                 
         MVI   GENSW,C'Y'                SHOW GEN PENDING                       
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
CHKDICT  EQU   *                                                                
         CLC   =C'*DICT ',RECORD         IS IT A "*DICT " CARD?                 
         BNE   CHKAPG                    NO: CHECK FOR APG                      
*                                                                               
         MVC   MEMTYPE,=C'DICT'          ASSIGN IT "DICT"                       
         MVI   DICTSW,C'Y'               SHOW DICT CARD FOUND                   
         LA    R8,RECORD+6               ADVANCE PAST *DICT                     
         BAS   RE,PHASERTN               EXTRACT PHASE NAME                     
         ICM   RF,15,CLSSRC              PHASE NAME ERROR?                      
         BNZ   CLSSFYXX                  YES: EXIT                              
*                                                                               
         MVC   DICTTBL,BLANKS            INITIALIZE TABLE                       
         MVC   DICTTBL(8),OUTNAME        STORE FIRST PHASE NAME                 
         MVC   DICTTBL+8(8),BASENAME     STORE FIRST BASE PHASE NAME            
         MVC   DICTCNT,=H'1'             SET ENTRY COUNT                        
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
DICTNXT  EQU   *                                                                
         CLC   =C'*DICT ',RECORD         IS IT ANOTHER  "*DICT " CARD?          
         BNE   CLSSFYEX                  DONE: EXIT                             
*                                                                               
         LA    R8,RECORD+6               ADVANCE PAST *DICT                     
         BAS   RE,PHASERTN               EXTRACT PHASE NAME                     
         ICM   RF,15,CLSSRC              PHASE NAME ERROR?                      
         BNZ   CLSSFYXX                  YES: EXIT                              
         LH    R1,DICTCNT                PICK UP PREVIOUS COUNT                 
         LA    R1,1(R1)                  INCREMENT COUNT                        
         STH   R1,DICTCNT                UPDATE ENTRY COUNT                     
         BCTR  R1,0                      DECREMENT, AND...                      
         MHI   R1,8*2                    ...X (L'TESTNAME+L'BASENAME)           
         LA    R1,DICTTBL(R1)            NEXT EMPTY SLOT                        
         MVC   0(8,R1),OUTNAME           STORE PHASE NAME                       
         MVC   8(8,R1),BASENAME          STORE BASE PHASE NAME                  
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
CHKAPG   EQU   *                                                                
         CLC   =C'*APG ',RECORD          IS IT APG?                             
         BNE   CHKDPG                    NO: CHECK FOR DPG                      
         MVC   MEMTYPE,=C'APG '          YES: MARK IT                           
         LA    R0,APGRTN                 TABLE ENTRY                            
         ST    R0,XXGADDR                                                       
         MVI   XXGSW,C'Y'                LOOK FOR PHASE CARD NEXT               
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
CHKDPG   EQU   *                                                                
         CLC   =C'*DPG ',RECORD          IS IT DPG?                             
         BNE   CHKRRG                    NO: CHECK FOR RRG                      
         MVC   MEMTYPE,=C'DPG '          YES: MARK IT                           
         LA    R0,DPGRTN                 TABLE ENTRY                            
         ST    R0,XXGADDR                                                       
         MVI   XXGSW,C'Y'                LOOK FOR PHASE CARD NEXT               
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
CHKRRG   EQU   *                                                                
         CLC   =C'*RRG ',RECORD          IS IT RRG?                             
         BNE   GETPAN                    NO: UNCLASSIFIED SO FAR                
         MVC   MEMTYPE,=C'RRG '          YES: MARK IT                           
         LA    R0,RRGRTN                 TABLE ENTRY                            
         ST    R0,XXGADDR                                                       
         MVI   XXGSW,C'Y'                LOOK FOR PHASE CARD NEXT               
         B     GETPAN                    GET NEXT RECORD                        
*                                                                               
LNKCF    EQU   *                         CONFIRM PANLNK                         
         CLC   MEMSAVE(2),=C'LK'         LK MEMBER NAME?                        
         BE    LNKCF10                   YES: ASSIGN PANLNK                     
         CLC   MEMSAVE(2),=C'LM'         LM MEMBER NAME?                        
         BE    LNKCF10                   YES: ASSIGN PANLNK                     
         B     ALNKCF                    NO: IT'S PANALNK                       
*                                                                               
LNKCF10  EQU   *                         CONFIRM PANLNK                         
         MVC   MEMTYPE,=C'LNK '          YES: IT'S PANLNK                       
         MVI   LNKSW,C'N'                NO LONGER PENDING                      
         MVI   SRCEOKSW,C'Y'             SRCE OK FOR LNK MEMBERS                
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
ALNKCF   EQU   *                         CONFIRM PANLNK                         
         MVC   MEMTYPE,=C'ALNK'          NO: IT'S PANALNK                       
         MVI   LNKSW,C'N'                NO LONGER PENDING                      
         MVI   SRCEOKSW,C'Y'             SRCE OK FOR ALNK MEMBERS               
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
GENCF    EQU   *                                                                
         CLI   RECORD,C'S'               SCREEN CARD?                           
         BNE   GETPAN                    NO: NEXT CARD                          
*                                                                               
         MVC   GENREC,RECORD             SAVE SCREEN CARD                       
*                                                                               
GEN10    EQU   *                                                                
         MVI   GENSW,C'N'                NO LONGER PENDING                      
         LA    R8,GENREC+4               START AT "T"                           
         LA    R1,1(R8)                  LOOK AT NEXT CHAR                      
GEN12    EQU   *                                                                
         MVC   WORKNAME,BLANKS           CLEAR WORK AREA                        
         CLI   0(R1),C' '                BLANK? (END OF ROOT NAME)              
         BE    GEN15                     YES: EXTRACT IT                        
         LA    R1,1(R1)                  ADVANCE POINTER                        
         B     GEN12                                                            
GEN15    EQU   *                                                                
         SR    R1,R8                     NAME LENGTH                            
         STH   R1,NAMELN                 SAVE IT                                
         BCTR  R1,0                      DECREMENT FOR MVC                      
         MVC   WORKNAME(0),0(R8)                                                
         EX    R1,*-6                                                           
         LH    R1,NAMELN                 RESTORE ACTUAL LENGTH                  
         LA    R8,WORKNAME(R1)           POINT TO END OF ROOT NAME              
         MVC   0(2,R8),GENREC+1          BRING IN SUFFIX                        
         LA    R8,2(R8)                  ADVANCE OUTNAME POINTER                
         LA    R1,3(R1)                  ADJUST NAME LENGTH                     
* EXTRA BYTE IS FOR TEST SUFFIX WE ARE ABOUT TO ADD.                            
         STH   R1,NAMELN                 SAVE IT                                
         MVI   0(R8),C'A'                DEFAULT TEST SUFFIX                    
         CLI   GENREC+3,C' '             TEST SUFFIX DEFAULTED?                 
         BE    *+10                      YES: KEEP DEFAULT                      
         MVC   0(1,R8),GENREC+3          NO: USE SUPPLIED SUFFIX                
         LA    R8,WORKNAME               PREP FOR PHASERTN                      
         BAS   RE,PHASERTN                                                      
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
XXGPHASE EQU   *                         SCAN FOR "PHASE"                       
         CLI   RECORD,C'*'               COMMENT CARD?                          
         BE    GETPAN                    YES: SKIP IT                           
*                                                                               
         MVC   WORKNAME,BLANKS                                                  
         LA    R8,WORKNAME                                                      
         L     R4,XXGADDR                ADDR OF TYPE-SPECIFIC RTN              
         BR    R4                                                               
*                                                                               
APGRTN   EQU   *                                                                
         CLC   =C'PHASE    ',RECORD      APG PHASE RECORD?                      
         BNE   GETPAN                    NO: GET NEXT                           
*                                                                               
         MVC   WORKNAME(2),=C'AC'                                               
         MVC   WORKNAME+2(6),RECORD+9    CONSTRUCT TEST PHASE NAME              
         BAS   RE,PHASERTN               CONSTRUCT OTHER PHASE NAMES            
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
DPGRTN   EQU   *                                                                
         CLC   =C'         PHASE ',RECORD  DPG PHASE RECORD?                    
         BNE   GETPAN                    NO: GET NEXT                           
*                                                                               
         MVC   WORKNAME,RECORD+15        RETRIEVE PHASE NAME                    
         BAS   RE,PHASERTN               CONSTRUCT OTHER PHASE NAMES            
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
RRGRTN   EQU   *                                                                
         CLC   =C'PHASE    ',RECORD      RRG PHASE RECORD?                      
         BNE   GETPAN                    NO: GET NEXT                           
*                                                                               
         MVC   WORKNAME(4),=C'RERG'      CONSTRUCT PHASE NAME                   
         MVC   WORKNAME+4(3),RECORD+9                                           
         BAS   RE,PHASERTN               CONSTRUCT OTHER PHASE NAMES            
         B     CLSSFYEX                  DONE: EXIT                             
*                                                                               
CLSSFYEX EQU   *                                                                
*&&DO                                                                           
*===================================================================            
         WTO   'MESSAGE FROM DEIS'                                              
         MVC   AREA,BLANKS                                                      
         MVC   AREA(7),L2LIBC                                                   
         MVC   AREA+11(4),MEMTYPE                                               
         MVC   AREA+20(1),SRCEOKSW                                              
         LA    R9,AREAL                                                         
         WTO   TEXT=(R9)                                                        
         WTO   'END OF MESSAGE FROM DEIS'                                       
*===================================================================            
*&&                                                                             
         CLC   L2LIBC,=C'SRCE'     SRCE(ONLY) LIBCODE?                          
         BNE   CLSSFYXX                                                         
         CLI   SRCEOKSW,C'Y'       YES: OKAY FOR THIS COMPILE TYPE?             
         BNE   CHKSRCI                                                          
         CLC   L2SUBC,=C'I  '      LIBCODE SRCE/I ?                             
         BE    SRCEINVL            NO: CAN'T USE LIBCODE SRCE/I                 
         MVC   MEMTYPE,=C'SRCE'    YES: USE OF SRCE LIBCODE IS OKAY             
         B     CLSSFYXX                                                         
*                                                                               
CHKSRCI  DS    0H                                                               
         CLC   L2SUBC,=C'I  '      LIBCODE SRCE/I ?                             
         BNE   SRCEINVL            NO: CAN'T USE LIBCODE SRCE                   
         CLC   MEMTYPE,=C'INCL'    BUT INCL MEMBERS ARE OK WITH SRCE/I          
         BNE   SRCEINVL                                                         
         MVC   MEMTYPE,=C'SRCE'                                                 
         B     CLSSFYXX                                                         
*                                                                               
SRCEINVL DS    0H                                                               
         MVC   MX14CTYP,MEMTYPE    SRCE (OR SRCE/I) NOT ALLOWED HERE            
         MVC   MX14MEM,MEMSAVE                                                  
         MVC   MX14LIBC,L2LIBC                                                  
         MVC   MX14SUBC,L2SUBC                                                  
         MVI   MX14SLASH,C' '                                                   
         CLC   L2SUBC,BLANKS                                                    
         BE    *+8                                                              
         MVI   MX14SLASH,C'/'                                                   
         MVC   M02XXMSG(MXERR14L),MXERR14   RETURN ERROR MESSAGE                
         MVC   CLSSRC,=F'4'                                                     
*                                                                               
CLSSFYXX DS    0H                                                               
         LM    R4,R9,CLSSFY49                                                   
         L     RE,CLSSFYSV                                                      
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
PHASESV  DC    F'0'                                                             
PHASEID  DC    CL8'PHASE'                                                       
PHASERTN EQU   *                                                                
         ST    RE,PHASESV                                                       
         MVC   OUTNAME,BLANKS                                                   
         MVC   BASENAME,BLANKS                                                  
         MVC   BKUPNAME,BLANKS                                                  
         MVC   TESTNAME,BLANKS                                                  
*                                                                               
PHASE00  EQU   *                                                                
         CLI   0(R8),C' '                                                       
         BNE   PHASE10                   FOUND FIRST NONBLANK                   
         LA    R8,1(R8)                  ADVANCE POINTER                        
         B     PHASE00                   LOOP                                   
PHASE10  EQU   *                         START OF NAME                          
         CLI   0(R8),C'0'                SANITY CHECK ON FIRST CHAR             
         BNL   PHASERR                   INSANE! FAIL VALIDATION                
*                                                                               
         SR    R1,R1                     CLEAR FOR TRT                          
         TRT   0(10,R8),TRTBLANK         FIND BLANK OR COMMA                    
         SR    R1,R8                     LENGTH OF NAME                         
         STH   R1,NAMELN                 SAVE LENGTH                            
         CHI   R1,8                      LONGER THAN 8?                         
         BH    PHASERR                   YES: FAIL VALIDATION                   
*                                                                               
         LR    R9,R1                     COPY TO R9                             
         BCTR  R9,0                      DECREMENT FOR TRT                      
         SR    R1,R1                     CLEAR FOR TRT                          
         TRT   0(0,R8),TRTNAME           VALIDATE NAME                          
         EX    R9,*-6                                                           
         LTR   R1,R1                     STOPPED BEFORE END?                    
         BNZ   PHASERR                   INVALID CHAR: FAIL                     
*                                                                               
         LH    R1,NAMELN                 RESTORE NAME LENGTH TO R1              
         BCTR  R1,0                      DECREMENT FOR MVC                      
         MVC   TESTNAME(0),0(R8)                                                
         EX    R1,*-6                    CAPTURE THE TEST PHASE NAME            
*                                                                               
         BCTR  R1,0                      CUT OFF TEST LEVEL LETTER              
         MVC   BASENAME(0),0(R8)                                                
         EX    R1,*-6                    CAPTURE BASE NAME                      
*                                                                               
         MVC   BKUPNAME,BASENAME         FORM ROOT OF BKUPNAME                  
         LH    R1,NAMELN                                                        
         LA    R1,BKUPNAME(R1)           POINT BEYOND NAME                      
         BCTR  R1,0                      BACK UP TO LAST CHAR                   
         MVI   0(R1),C'S'                CREATE "SAVE" NAME                     
*                                                                               
         MVC   OUTNAME(8),TESTNAME       POSIT USING TESTNAME                   
         CLI   BKOTFLAG,C'Y'             IS IT A BACKOUT?                       
         BNE   PHASEEX                   NO: EXIT PHASERTN                      
         CLC   M02XXLSN,=C'PROD'         YES: FROM PROD LEVEL?                  
         BE    *+14                                                             
         CLC   M02XXLSN,=C'PRKS'         ...OR PRKS LEVEL?                      
         BNE   PHASEEX                   NO: EXIT PHASERTN                      
         MVC   OUTNAME(8),BKUPNAME       YES: USE BKUPNAME                      
         B     PHASEEX                   NOW EXIT PHASERTN                      
*                                                                               
PHASERR  EQU   *                         CAPTURE NAME                           
         MVC   PDPHA08,OUTNAME                                                  
         MVC   PDMEM08,MEMSAVE                                                  
         MVC   M02XXMSG(PDERR08L),PDERR08   RETURN ERROR MESSAGE                
         LA    RF,4                                                             
         ST    RF,CLSSRC                                                        
         B     PHASEEX                   EXIT WITH ERROR                        
*                                                                               
PHASEEX  EQU   *                                                                
         L     RE,PHASESV                                                       
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
*************************************************************                   
PREADSV  DC    F'0'                     LINK REG SAVE                           
PREADID  DC    CL8'PREAD'               SUBROUTINE ID                           
PREAD    EQU   *                                                                
         ST    RE,PREADSV               SAVE LINK REG                           
*                                                                               
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         MVI   FUNCTION,C'R'            REQUEST READ                            
         L     RF,PAM@                                                          
         CALL  (15),(PACB,RECORD,NAME1,INCLUDES,COMMENT),VL                     
         L     RF,ACTION                RETRIEVE RETURN CODE                    
         L     RE,PREADSV               RETRIEVE LINK REG                       
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
         LTORG                                                                  
         EJECT                                                                  
* THE MEMBER NAME PREFIXES IN THE TABLE BELOW ARE FOR MEMBERS WHICH ARE         
* PERMITTED TO BYPASS THE STGE LEVEL (VIA THE /P LIBCODES) WITHOUT              
* THEIR ASSOCIATED INVENTORY RECORDS BEING SPECIFICALLY FLAGGED AS              
* "STGE-BYPASS" AUTHORIZED. THIS IS INTENDED FOR GROUPS OF MEMBERS              
* (E.G., TALENT IN THE US) WHICH ARE TYPICALLY NEVER STAGED.                    
* EACH TABLE ENTRY CONSISTS OF THE EXECUTED COMPARE LENGTH, FOLLOWED BY         
* THE MEMBER NAME PREFIX CHARACTERS. (WE DON'T NEED MORE THAN 9 PREFIX          
* CHARACTERS, BECAUSE THE MAXIMUM MEMBER NAME LENGTH IS 10.)                    
*                                                                               
STGE_BYPASS_MEMBERS_OK DS 0H                                                    
*&&US                                                                           
         DC    AL1(2),CL9'TA'      TALENT SOURCE MODULES                        
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*      EXIT ERROR MESSAGES                                                      
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
* NOTE: WHEN THIS MODULE PRODUCES ITS OWN BACKOUT WARNING MESSAGE               
*       (I.E., EXITS WITH RC=4 ON A BAK), PANAPT PRECEDES OUR MESSAGE           
*       WITH ITS OWN APCS1180-12 MESSAGE CONTAINING THE MEMBER NAME (SO         
*       WE DON'T NEED TO INCLUDE THE MEMBER NAME IN OUR OWN BACKOUT             
*       WARNING MESSAGE).                                                       
*                                                                               
*       FOR NON-BACKOUT MESSAGES, IF WE LEAVE OUT THE MEMBER NAME, THE          
*       PROGRAMMER WOULD NEED TO LOOK AT THE "V" FLAG ON THE MEMBER(S)          
*       TO KNOW WHICH ONE(S) HAD FAILED MEMBER EXISTENCE VERIFICATION.          
*                                                                               
MXERR01  DS    0C                  LIBCODE INCONSISTENT                         
MX1MEM   DC    CL10' '                                                          
         DC    CL2' '                                                           
MX1LIBC  DC    CL4' '                                                           
MX1SLASH DC    C' '                IS SET TO C'/' IF THERE IS A SUBCODE         
MX1SUBC  DC    CL3' '                                                           
         DC    C' NOT PERMITTED DUE TO LIBCODE INCONSISTENCY'                   
MXERR01L EQU   *-MXERR01                                                        
*                                                                               
MXERR02  DS    0C                  STGE BYPASS NOT ALLOWED                      
MX2MEM   DC    CL10' '                                                          
         DC    CL2' '                                                           
MX2LIBC  DC    CL4' '                                                           
MX2SLASH DC    C' '                IS SET TO C'/' IF THERE IS A SUBCODE         
MX2SUBC  DC    CL3' '                                                           
         DC    C' STGE BYPASS UNAUTHORIZED'                                     
MXERR02L EQU   *-MXERR02                                                        
*                                                                               
MXERR03  DS    0C                  UNAUTHORIZED USE OF LIBCODE                  
MX3MEM   DC    CL10' '                                                          
         DC    CL2' '                                                           
MX3LIBC  DC    CL4' '                                                           
MX3SLASH DC    C' '                IS SET TO C'/' IF THERE IS A SUBCODE         
MX3SUBC  DC    CL3' '                                                           
         DC    C' UNAUTHORIZED USE OF LIBCODE'                                  
MXERR03L EQU   *-MXERR03                                                        
*                                                                               
MXERR04  DS    0C                  NO BACKOUT OF COPY FOR REWORK MR             
         DC    C'BACKOUT OF COPY FOR REWORK PROHIBITED. BACKOUT ORIGINA+        
               L MR '                                                           
MXERR04M DS    CL6                 MOVE REQUEST NUMBER                          
MXERR04L EQU   *-MXERR04                                                        
*                                                                               
MXERR05  DS    0C                  SRCE/T LIBCODE REQUIRED                      
MX5MEM   DC    CL10' '                                                          
         DC    CL2' '                                                           
         DC    C'MUST BE PROMOTED WITH LIBCODE SRCE/T'                          
MXERR05L EQU   *-MXERR05                                                        
*                                                                               
*&&DO                                                                           
* IF WE EVER DECIDE TO IMPLEMENT THIS, UNCOMMENT IT!                            
MXERR07  DS    0C                  /C LIBCODE REQUIRED                          
MX7MEM   DC    CL10' '                                                          
         DC    C' '                                                             
         DC    C'IS A CRITICAL MEMBER. MUST BE PROMOTED VIA "/C" LIBCOD+        
               E.'                                                              
MXERR07L EQU   *-MXERR07                                                        
*&&                                                                             
*                                                                               
MXERR09  DS    0C                  INVALID MEMBER INCLUDED IN MR                
         DC    C'MEMBER '                                                       
MX9MEM   DC    CL10' '                                                          
         DC    C' IS AN OUTPUT OBJECT. DELETE IT FROM THIS MOVE REQUEST+        
               .'                                                               
MXERR09L EQU   *-MXERR09                                                        
*                                                                               
MXERR19  DS    0C                  "DC*" MEMBER PROMOTION IS DISALLOWED         
         DC    C'DELETE '                                                       
MX19MEM  DC    CL10' '                                                          
         DC    C' FROM THIS MR. DOCUMENTATION MUST BE MAINTAINED IN ALF+        
               RESCO.'                                                          
MXERR19L EQU   *-MXERR19                                                        
*                                                                               
MXERR10  DS    0C                  "C" FLAG ON PRKS BACKOUT: DANGEROUS!         
         DC    C'*WARNING*: DON''T BACKOUT THIS MR! IT WAS LAST PROMOTE+        
               D TO PROD BY MR '                                                
MX10MR#  DC    CL6' '                                                           
         DC    C'!'                                                             
MXERR10L EQU   *-MXERR10                                                        
*                                                                               
MXERR11  DS    0C                  STGE BKOT WITH "C" FLAG ON                   
         DC    C'**DO NOT OVERRIDE** HIT PF3 NOW, CHG THE MR, CLEAR THE+        
                "C" FLAG, REDO THE BAK.'                                        
MXERR11L EQU   *-MXERR11                                                        
*                                                                               
MXERR12  DS    0C                  PROD BACKOUT VIA THE PRKS LEVEL              
         DC    C'THIS MR WAS PROMOTED VIA THE PRKS LEVEL. A BACKOUT MAY+        
                BE VERY DANGEROUS!'                                             
MXERR12L EQU   *-MXERR12                                                        
*                                                                               
MXERR13  DS    0C                  TOO MANY SRCE LIBCODE MEMBERS                
         DC    C'MAXIMUM OF '                                                   
MXERR13N DC    C'NN'                                                            
         DC    C' "SRCE" LIBCODE MEMBERS EXCEEDED IN THIS MR'                   
MXERR13L EQU   *-MXERR13                                                        
*                                                                               
MXERR14  DS    0C                  SRCE (OR SRCE/I) LIBCODE NOT ALLOWED         
         DC    C'MEMBER '                                                       
MX14MEM  DC    CL10' '                                                          
         DC    C' OF COMPILE TYPE '                                             
MX14CTYP DC    CL4' '                                                           
         DC    C' CAN''T BE USED WITH LIBCODE '                                 
MX14LIBC DC    CL4' '                                                           
MX14SLASH DC   C' '                IS SET TO C'/' IF THERE IS A SUBCODE         
MX14SUBC DC    CL3' '                                                           
MXERR14L EQU   *-MXERR14                                                        
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        PAN ERROR MESSAGES                                                     
*                                                                               
*----------------------------------------------------------------------         
* NOTE: THESE MESSAGES WILL BE SENT TO THE ISPF LOG.           012A*JMM         
*       ISPF WILL TRUNCATE THEM TO 78 CHARACTERS.              012A*JMM         
*                                                              012A*JMM         
*                                                                               
PVERR01  DS    0C                  INVALID ACTION==>  MEMBER ONLY!              
PVACT01  DC    CL08' '                                                          
         DC    C' IS AN INVALID ACTION'                                         
PVERR01L EQU   *-PVERR01                                                        
*                                                                               
PVERR02  DS    0C                  DATA SET DOES NOT EXIST                      
         DC    C'DATASET '                                                      
PVDSN02  DC    CL44' '                                                          
         DC    C' NOT FOUND.'                                  012A*JMM         
PVERR02L EQU   *-PVERR02                                                        
*                                                                               
PVERR03  DS    0C                  INTERNAL SVC 99 ERROR                        
PVVERB03 DC    CL04' '             SVC 99 VERB CODE                             
         DC    C' S99ERR='                                     012A*JMM         
PVS9903  DC    CL04' '               ERROR CODE RETURNED BY SVC 99              
         DC    C' DDN='                                                         
PVDDN03  DC    CL08' '                                                          
         DC    C' DSN='                                                         
PVDSN03  DC    CL44' '                                                          
PVERR03L EQU   *-PVERR03                                                        
*                                                                               
PVERR04  DS    0C                  PANVALET ERROR                               
         DC    C'PANVALET '                                                     
PVFUNC04 DC    CL06' '                                                          
         DC    C' FAILED, RC='                                                  
PVRTN04  DC    C'NN'                                                            
         DC    C', ERROR MESSAGE '                                              
PVTYPE04 DC    C'CC'                                                            
PVNUM04  DC    C'NNN'                                                           
PVERR04L EQU   *-PVERR04                                                        
*                                                                               
PVERR05  DS    0C                  MEMBER NOT FOUND                             
PVMEM05  DC    CL10' '                                                          
         DC    C' NOT IN '                                                      
PVDSN05  DC    CL44' '                                                          
PVERR05L EQU   *-PVERR05                                                        
*                                                                               
PVERR06  DS    0C                  INCORRECT LIBCODE                            
PVMEM06  DC    CL10' '                                                          
         DC    C' WITH TYPE '                                                   
PVTYP06  DC    CL4' '                                                           
         DC    C' DOES NOT MATCH LIBCODE '                                      
PVLIB06  DC    CL4' '                                                           
PV6SLASH DC    C' '                IS SET TO C'/' IF THERE IS A SUBCODE         
PVSUB06  DC    CL3' '                                                           
         DC    C'.'                                                             
PVERR06L EQU   *-PVERR06                                                        
*                                                                               
PVERR07  DS    0C                  OUTPUT NOT FOUND                             
         DC    C'OUTPUT MEMBER '                                                
PVMEM07  DC    CL10' '                                                          
         DC    C' NOT IN '                                                      
PVDSN07  DC    CL30' '                                                          
PVERR07L EQU   *-PVERR07                                                        
*                                                                               
PVERR08  DS    0C                  AUDIT STAMP UNMATCHED                        
         DC    C'LEVEL/DATE/TIME STAMP MISMATCH FOR MEMBER '                    
PVMEM08  DC    CL10' '                                                          
PVERR08L EQU   *-PVERR08                                                        
*                                                                               
PVERR09  DS    0C                  AUDIT STAMP NOT FOUND                        
         DC    C'NO LEVEL STAMP FOUND FOR MEMBER '                              
PVMEM09  DC    CL10' '                                                          
PVERR09L EQU   *-PVERR09                                                        
*                                                                               
PVERR10  DS    0C                  AUDIT DATE/TIME IS PRIOR TO NOW              
*                                  (PROBABLE MEMBER TRANSFER FROM UK).          
PVMEM10  DC    CL10' '                                                          
         DC    C' PROBABLY TRANSFERRED FROM UK. TOUCH IN =P.2 AND RELIN+        
               K.'                                                              
PVERR10L EQU   *-PVERR10                                                        
*                                                                               
PVERR11  DS    0C                  MEMBER WAS EDITED AFTER VERIFICATION         
         DC    C'MEMBER '                                                       
PVMEM11  DC    CL10' '                                                          
         DC    C' WAS EDITED *AFTER* RVP (VERIFICATION) PROCESSING'             
PVERR11L EQU   *-PVERR11                                                        
*                                                                               
*&&DO                                                                           
PVERR12  DS    0C                  DDPAPTS* MEMBERS MUST BE SEC/P               
         DC    C'MEMBER '                                                       
PVMEM12  DC    CL10' '                                                          
         DC    C' '                                                             
PVCOND12 DS    CL5                 WILL BE "MUST " OR "CAN'T"                   
         DC    C' BE ASSIGNED LIBCODE SEC/P'                                    
PVERR12L EQU   *-PVERR12                                                        
*&&                                                                             
*                                                                               
PVERR13  DS    0C                  AUDIT STAMP MISMATCH ON BACKOUT              
         DC    C'LEVEL/DATE/TIME STAMP MISMATCH WITH BACKED UP MEMBER '         
PVOUTM13 DC    CL10' '                                                          
PVERR13L EQU   *-PVERR13                                                        
*                                                                               
PVERR14  DS    0C                  PAPTMEX INTERNAL ERROR                       
         DC    C'PAPTMEX-14 '                                                   
*&&US*&& DC    C'CONTACT DAVID EISENBERG. INTERNAL ERROR #'                     
*&&UK*&& DC    C'CONTACT TERRY CLEEVE. INTERNAL ERROR #'                        
PVERR#14 DC    CL2' '                                                           
         DC    C'.'                                                             
PVERR14L EQU   *-PVERR14                                                        
*                                                                               
PVERR15  DS    0C                  AUDIT STAMP MISMATCH ON STGE BACKOUT         
         DC    C'MEMBER '                                                       
PVMEM15  DC    CL8' '              LOAD MODULE NAME                             
         DC    C' LEVEL/TIME STAMP DOESN''T MATCH SOURCE IN '                   
PVDSN15  DC    CL21' '             PRESUMABLY: PERSONAL PAN LIBRARY DSN         
PVERR15L EQU   *-PVERR15                                                        
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        PDS ERROR MESSAGES                                                     
*                                                                               
*----------------------------------------------------------------------         
PDERR04  DS    0C                  OPEN FAILED ERROR                            
         DC    C'OPEN FAILED, DATASET '                                         
PDDSN04  DC    CL44' '                                                          
PDERR04L EQU   *-PDERR04                                                        
*                                                                               
PDERR05  DS    0C                  MEMBER NOT FOUND                             
PDMEM05  DC    CL08' '                                                          
         DC    C' NOT IN '                                                      
PDDSN05  DC    CL44' '                                                          
PDERR05L EQU   *-PDERR05                                                        
*                                                                               
PDERR06  DS    0C                  INTERNAL BLDL ERROR                          
         DC    C'BLDL ERROR - RC=8 REASON CODE='                                
PDREA06  DC    C' '                                                             
PDERR06L EQU   *-PDERR06                                                        
*                                                                               
PDERR07  DS    0C                  INTERNAL DYNALLOC ERROR                      
         DC    C'ALLOCATION ERROR ON REQUEST '                                  
PDREQ07  DS    CL4                                                              
         DC    C' FOR FILE '                                                    
PDDDN07  DS    CL8                                                              
         DC    C'. RC = '                                                       
PDRTN07  DS    CL8                                                              
PDERR07L EQU   *-PDERR07                                                        
*                                                                               
PDERR08  DS    0C                  INVALID PHASE NAME                           
         DC    C'INVALID PHASE NAME '                                           
PDPHA08  DS    CL9                                                              
         DC    C' FOR MEMBER '                                                  
PDMEM08  DS    CL10                                                             
PDERR08L EQU   *-PDERR08                                                        
*                                                                               
PDERR09  DS    0C                  ZAPPED LOAD MODULE                           
         DC    C'LOAD MODULE '                                                  
PDPHA09  DS    CL8                                                              
         DC    C' HAS BEEN ZAPPED'                                              
PDERR09L EQU   *-PDERR09                                                        
*                                                                               
PDERR10  DS    0C                  POST-RVP LINK                                
         DC    C'LOAD MODULE '                                                  
PDPHA10  DS    CL8                                                              
         DC    C' WAS LINKED *AFTER* RVP (VERIFICATION) PROCESSING'             
PDERR10L EQU   *-PDERR10                                                        
*                                                                               
PDERR11  DS    0C                  LOAD MODULE IS "NX" (NOT EXECUTABLE)         
         DC    C'LOAD MODULE '                                                  
PDPHA11  DS    CL8                                                              
         DC    C' IS NOT EXECUTABLE'                                            
PDERR11L EQU   *-PDERR11                                                        
*                                                                               
         EJECT                                                                  
* THIS MODULE HAS ACCESS TO THE NAMES OF ANY RELATED LIBCODES WHICH ARE         
* ASSOCIATED WITH THE CURRENT LIBCODE. HOWEVER, WE DO NOT HAVE ACCESS           
* TO THE DSNS CONTAINED IN THE RELATED LIBCODES. THEREFORE, IT IS               
* REQUIRED TO MAINTAIN A TABLE OF ANY RELATED LIBCODE DSNS WHICH ARE            
* REFERENCED WITHIN THIS MODULE. THIS TABLE IS ESSENTIALLY A CLONE OF           
* THE DATA CONTAINED IN THE RELATED LIBCODE RECORDS THEMSELVES. THE             
* TABLE ENTRIES ARE KEYED BY RELATED LIBCODE AND LEVEL. A LINEAR                
* SEARCH IS PERFORMED, AND THE DSN IS EXTRACTED.                                
* NOTE: IN THE SBX ENVIRONMENT, THE DSNS BELOW ARE ALL PREFIXED BY              
* THE FIXED STRING 'PANAPT.SBX.'                                                
*                                                                               
RELLIBC  DS    0H                                                               
TESTLIB  DC    C'ZRELLOD',C'TEST',CL44'DDS.TESTLIB'                             
         DC    C'ZRELLOD',C'STGE',CL44'DDS.LOADLIB'                             
         DC    C'ZRELLOD',C'PRKS',CL44'DDS.LOADLIB'                             
         DC    C'ZRELLOD',C'PROD',CL44'DDS.LOADLIB'                             
         DC    X'FF'               EOT                                          
         SPACE 2                                                                
         DC    ((3*4096)-(*-PAPTMEX))X'00'   GRAB A FULL 12K                    
         EJECT                                                                  
*************************************************************                   
*----------------------------------------------------------------------         
*                                                                               
*        CONSTANTS AND WORKAREAS                                                
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
SAVEAREA DC    18F'0'              LOCAL SAVEAREA                               
*                                                                               
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
*                                                                               
RTNCODE  DC    F'0'                                                             
*                                                                               
DICTSW   DC    C'N'                                                             
GENSW    DC    C'N'                                                             
LNKSW    DC    C'N'                                                             
XXGSW    DC    C'N'                                                             
SRCEOKSW DC    C'N'                'Y' IF SRCE LIBCODE IS OK FOR MEMBER         
*                                                                               
OBJEND   DS    0XL4                                                             
         DC    X'02',C'END'                                                     
OBJTXT   DS    0XL4                                                             
         DC    X'02',C'TXT'                                                     
*                                                                               
MEMTYPE  DC    CL4' '                                                           
*                                                                               
XXGADDR  DS    F                                                                
*                                                                               
DICTCNT  DC    H'0'                NUMBER OF ENTRIES IN DICTTBL                 
*                                                                               
* EACH DICTTBL ENTRY IS THE TESTNAME(8), FOLLOWED BY THE BASENAME(8)            
DICTTBL  DC    CL(2*8*MAXDICTS)' ' TABLE OF DICTIONARY PHASES                   
MAXDICTS EQU   10                  MAX NUMBER OF *DICT CARDS ALLOWED            
*                                                                               
BKOTFLAG DC    C' '                IS THIS A BACKOUT? Y OR N                    
LIBCDGRP DC    C'        '         TO ENSURE LIBCODE CONSISTENCY IN MR          
PRKSFLAG DC    C' '                TO ENSURE /K LIBCODE CONSISTENCY             
MRSAVE   DC    CL6' '              PREVIOUS MR NUMBER                           
SRCSAVE  DC    C' '                PREVIOUS EXIT POINT CODE                     
IDRDONE  DC    C' '                LAST IDR RECORD SEEN: Y/N                    
ZAPCOUNT DC    X'0'                FROM IDR RECORD                              
ZAPDATE  DS    CL8                 FROM IDR RECORD (YYYYMMDD FORMAT)            
MBRCOUNT DC    H'0'                MEMBER COUNT SINCE INIT                      
*                                                                               
SRCECNT  DC    H'0'                NO. OF MEMBERS WITH LIBCODE "SRCE"           
* THERE IS A LIMIT TO THE NUMBER OF LIBCODE "SRCE" MEMBERS WHICH CAN BE         
* PRESENT IN THE MR. THAT'S BECAUSE THE "SRCVRFY" VERIFICATION MODEL            
* GENERATES MANY JOB STEPS FOR EACH MEMBER, AND THERE IS A VERY REAL            
* POSSIBILITY OF EXCEEDING THE LIMIT OF 255 STEPS PER JOB. BY KEEPING           
* THE NEXT TWO EQU STATEMENTS CURRENT, WE CAN CATCH THIS POTENTIAL              
* JCL ERROR DURING A CLO.                                                       
SRCE_VERIF_MODEL_MEMBER_STEPS EQU 11   # OF STEPS REQUIRED FOR EACH MBR         
SRCE_VERIF_MODEL_OVERHEAD_STEPS EQU 5  # OF FIXED OVERHEAD JOB STEPS            
*                                        (1 TO START, AND 4 AT THE END)         
*                                                                               
* AS TIME GOES ON, IF "MAXIMUM_SRCE_MEMBERS" GETS TOO LOW FOR COMFORT,          
* WE CAN LOOK AT THE SRCVRFY MODEL AND SEE IF WE CAN REDUCE THE NUMBER          
* OF STEPS (POSSIBLY BY WRITING A NEW PROGRAM THAT INVOKES SUPERC AND           
* ICETOOL PROGRAMMATICALLY, OR BY COMBINING STEPS SOMEHOW, ETC).                
MAXIMUM_SRCE_MEMBERS DC AL2((255-SRCE_VERIF_MODEL_OVERHEAD_STEPS)/SRCE_+        
               VERIF_MODEL_MEMBER_STEPS)                                        
*                                                                               
PANLIBSV DS    CL44                                                             
MEMSAVE  DS    CL10                                                             
OUTNAME  DS    CL10                                                             
WORKNAME DS    CL10                                                             
BASENAME DS    CL8                                                              
BKUPNAME DS    CL8                                                              
TESTNAME DS    CL8                                                              
NAMELN   DS    H                                                                
*                                                                               
* THIS MODULE ASSUMES THAT ALL SOURCE AND OUTPUT DATASETS FOR THE               
* SBX ENVIRONMENT ARE PREFIXED BY 'PANAPT.SBX.' !!!                             
SANDBOX  DC    C'PANAPT.SBX.'                                                   
GENREC   DS    CL80                                                             
BLANKS   DC    CL80' '                                                          
*                                                                               
         DS    0D                                                               
TRTBLANK DC    256X'00'                                                         
         ORG   TRTBLANK+C' '                                                    
         DC    X'FF'                                                            
         ORG   TRTBLANK+C','                                                    
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
TRTNAME  DC    256X'FF'                                                         
         ORG   TRTNAME+C'@'                                                     
         DC    X'00'                                                            
         ORG   TRTNAME+C'$'                                                     
         DC    X'00'                                                            
         ORG   TRTNAME+C'#'                                                     
         DC    X'00'                                                            
         ORG   TRTNAME+C'A'                                                     
         DC    9X'00'                                                           
         ORG   TRTNAME+C'J'                                                     
         DC    9X'00'                                                           
         ORG   TRTNAME+C'S'                                                     
         DC    8X'00'                                                           
         ORG   TRTNAME+C'0'                                                     
         DC    10X'00'                                                          
         ORG                                                                    
*                                                                               
ADDR1S99 DC    F'0'                                                             
APAS1S99 DC    CL8'APAS1S99'                                                    
*                                                                               
AUDSTAMP DS    0C                                                               
         DC    C'BOOK='                                                         
AUDMEM   DS    CL10                PAN MEMBER NAME                              
         DC    C' '                PANACEA WILL PUT A NON-BLANK HERE TO         
*                                   FORCE A MISMATCH IF APPROPRIATE             
         DC    C'LEVEL='                                                        
AUDLVL   DS    CL3                 PAN LEVEL NUMBER                             
         DC    C' DATE='           " DATE="                                     
AUDDATE  DS    CL8                 MMMDD/YY (IN BOTH US AND UK)                 
         DC    C' TIME='           " TIME="                                     
AUDTIME  DS    CL8                 HH:MM:SS                                     
AUDITLNQ EQU   *-AUDSTAMP                                                       
*                                                                               
OPENLIBL DC    F'0'                OPEN ROUTINE LINKAGE SAVEAREA                
CLOSLIBL DC    F'0'                CLOSE ROUTINE LINKAGE SAVEAREA               
SEARCHL  DC    F'0'                SEARCH ROUTINE LINKAGE SAVEAREA              
SRCTSTL  DC    F'0'                SEARCH TESTLIB RTN LINKAGE SAVEAREA          
USERRTNL DC    F'0'                USER ROUTINE LINKAGE SAVEAREA                
ALLOCL   DC    F'0'                ALLOCATION ROUTINE LINKAGE SAVEAREA          
DEALLOCL DC    F'0'                DEALLOCATION ROUTINE LINKAGE SAVEARE         
*                                                                               
SNDMAILL DS    16F                 CALLER'S SAVE 16 GPRS (SNDMAIL)              
GPRSAVE  DS    15F                 CALLER'S SAVE RE-RC                          
*                                                                               
DUB      DS    D                                                                
DBLWRD   DS    D                   WORK FIELD                                   
FULL     DS    F                                                                
BYTE     DS    X                                                                
WORK     DS    CL29                                                             
TIMEWORK DS    CL16                WORKAREA FOR TIME MACRO                      
*                                                                               
AREAL    DC    AL2(80)                                                          
AREA     DS    CL80                                                             
*                                                                               
DYNAMCHR DC    C'0123456789ABCDEF' TRANSLATE TO CHARACTERS                      
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        PAM INTERFACE                                                          
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PAM@     DC    A(0)                ADDRESS OF PAM ENTRY POINT                   
PAM      DC    CL8'PAM'            PAM MODULE NAME                              
BACKUP   DC    CL8'NO-ENTRY'       PAM PARM (NOT USED)                          
DIRNTRY  DC    CL80' '             PAM PARM                                     
SAVENTRY DC    CL80' '             SAVE ENTRY FOR SOURCE MEMBER                 
NAME1    DC    CL22' '             PAM PARM - MEMBER NAME                       
NAME2    DC    CL11'NO-ENTRY'      PAM PARM (NOT USED)                          
COMMENT  DC    CL52'NO-ENTRY'      PAM PARM (NOT USED)                          
SUBSET   DC    CL27'NO-ENTRY'      PAM PARM (NOT USED)                          
INCLUDES DC    CL8'NO-ENTRY'       PAM PARM (NOT USED)                          
FIRSTRVP DS    CL14                EARLIEST RVP (YYYYMMDDHHMMSS)...             
*                                   ...FOR THE CURRENT LEVEL                    
RECORD   DC    CL80' '                                                          
RECSAV1  DC    CL56' '                                                          
RECSAV2  DC    CL56' '                                                          
RECSAV3  DC    CL56' '                                                          
*                                                                               
         DS    0F                                                               
PACB     DS    0XL12               PAM ACCESS CONTROL BLOCK                     
*                                                                               
ACTION   DS    F                   RETURN CODE SET BY PAM                       
         ORG   ACTION                                                           
ACTCODE  DS    XL1                 COMPONENT-ID                                 
         DS    XL2                                                              
ACTMSG   DS    XL1                 ERROR MSG NUMBER                             
         DS    XL3                 RESERVED                                     
FUNCTION DS    CL1                 FUNCTION INDICATOR                           
         DS    F                   PAM ANCHOR BLOCK POINTER                     
*                                                                               
OPENLIBR DS    F                   OPEN ROUTINE ORIGINAL RETURN CODE            
SEARCHR  DS    F                   SEARCH ROUTINE ORIGINAL RETURN CODE          
SEARCHA  DS    F                   SEARCH ROUTINE ORIGINAL ACTION CODE          
CLOSLIBR DS    F                   CLOSE ROUTINE ORIGINAL RETURN CODE           
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
*        ALLOCATION CONTROL BLOCK                                               
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
REQBKPTR DS    0F                  REQUEST BLOCK POINTER                        
         DC    X'80',AL3(REQBLOCK)                                              
REQBLOCK DC    10H'0'              DYNAMIC ALLOCATION REQUEST BLOCK             
*                                                                               
TXTPTRS  DS    0F                                                               
         DC    A(DDNTEXT)          DDNAME TEXT POINTER                          
         DC    A(DSNTEXT)          DATASET NAME TEXT POINTER                    
         DC    A(STATEXT)          STATUS/DISPOSITION TEXT POINTER              
         DC    A(MBRTEXT)          MEMBER NAME TEXT POINTER                     
         ORG   *-4                                                              
         DC    X'80'               EOL                                          
         ORG                                                                    
*                                                                               
DDNTEXT  DS    0F                                                               
         DC    AL2(DALDDNAM)       DDNAME ALLOCATION/DEALLOCATION               
         DC    AL2(1)              NUMBER OF PARMS                              
         DC    AL2(8)              LENGTH OF THE DDNAME                         
TXTDDN   DC    CL08' '             DDNAME                                       
*                                                                               
DSNTEXT  DS    0F                                                               
         DC    AL2(DALDSNAM)       DATASET ALLOCATION/DEALLOCATION              
         DC    AL2(1)              NUMBER OF PARMS                              
         DC    AL2(44)             LENGTH OF THE DATASET NAME                   
TXTDSN   DC    CL44' '             DATASET NAME                                 
*                                                                               
STATEXT  DS    0F                                                               
TXTSTKEY DC    AL2(DALSTATS)       STATUS, DISPOSITION KEY                      
         DC    AL2(1)              NUMBER OF PARMS                              
         DC    AL2(1)              STATUS, DISPOSITION LENGTH                   
         DC    XL01'08'            SHR, KEEP                                    
*                                                                               
MBRTEXT  DS    0F                                                               
TXTMBKEY DS    AL2                 MEMBER NAME (DALMEMBR OR NULLS)              
         DC    AL2(1)              NUMBER OF PARMS                              
         DC    AL2(8)              L'MEMBER NAME                                
TXTMEMBR DC    CL8' '              MEMBER NAME                                  
         EJECT                                                                  
         COPY APAMXTBL                                                          
         EJECT                                                                  
         COPY APAMRQ99                                                          
         EJECT                                                                  
         COPY APAM1S99                                                          
         SPACE 2                                                                
*----------------------------------------------------------------------         
*                                                                               
*        PDSDCB IS THE DCB USED IN THE BLDL.                                    
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PDSDCB   DCB   DDNAME=XXXXXXXX,DSORG=PS,MACRF=GL,EODAD=MEMBR500                 
PDSDCBL  EQU   *                   DCB LENGTH                                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        DUMMYDCB IS USED TO REFRESH PDSDCB                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
DUMMYDCB DCB   DDNAME=DDDDDDDD,DSORG=PS,MACRF=GL,EODAD=MEMBR500                 
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*        RVPDCB IS USED TO EXAMINE RVP DATE/TIME                                
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
RVPDCB   DCB   DDNAME=RVPDTTIM,DSORG=PS,MACRF=GM                                
         EJECT                                                                  
       ++INCLUDE DDPANAUDIT                                                     
         SPACE 3                                                                
         DC    (4096-(*-SAVEAREA))X'00'   GRAB A FULL 4K                        
         EJECT                                                                  
* DSECT TO COVER TABLE OF RELATED LIBCODES                                      
*                                                                               
RELLIBCD DSECT                                                                  
RLLIBCOD DS    CL7                 RELATED LIBCODE AND SUBCODE                  
RLLEVEL  DS    CL4                 PANAPT LEVEL                                 
RLKEYLQ  EQU   *-RELLIBCD                                                       
RLDSN    DS    CL44                LIBRARY DSN                                  
RLLENQ   EQU   *-RELLIBCD                                                       
         SPACE 3                                                                
STGEBPD  DSECT                                                                  
STGEPFXL DS    AL1                 MEMBER NAME COMPARE PREFIX LENGTH            
STGEPFX  DS    CL9                 MEMBER NAME PREFIX CHARACTERS                
STGEBPDQ EQU   *-STGEBPD                                                        
         SPACE 3                                                                
*----------------------------------------------------------------------         
*                                                                               
*        DCB DSECT                                                              
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DDOPERLSTD                                                     
         EJECT                                                                  
       ++INCLUDE DDPAN0UPD                                                      
         EJECT                                                                  
         PUSH  PRINT                                                            
         PRINT NOGEN                                                            
*----------------------------------------------------------------------         
*                                                                               
*        DYNAMIC ALLOCATION (SVC 99) REQUEST BLOCK DSECT                        
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
         IEFZB4D0                                                               
         IEFZB4D2                                                               
*                                                                               
         CVT   DSECT=YES                                                        
         IEESMCA                                                                
*                                                                               
         POP   PRINT                                                            
*----------------------------------------------------------------------         
*                                                                               
*        SEND E-MAIL ROUTINE                                                    
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
PAPTMEX  CSECT                                                                  
SNDMAIL  DS    0H                                                               
         STM   RE,RD,SNDMAILL      SAVE ALL 16 GPRS                             
         BASR  RB,0                                                             
         AHI   RB,-6                                                            
         USING SNDMAIL,RB,RA       ESTABLISH BASE ADDRESSABILITY                
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
*                                                                               
         USING APAM02XX,R3                                                      
         USING APAMDIB2,R4                                                      
         USING APAMLIB2,R5                                                      
         USING APAMMMBR,R6                                                      
         USING APAMMDES,R7                                                      
*                                                                               
         LA    RF,SETSMTP          MUST USE RF AS BRANCH ADDRESS                
         BASR  RE,RF               SET SYSOUT PARAMETERS FOR DYNALLOC           
*                                                                               
         CLI   M02XXSRC,CLOSE      CLOSE PROCESSING?                            
         BE    SNDM50              YES: SEND E-MAILS                            
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   SNDMAILX            NO: NO E-MAILS NOW                           
*                                                                               
*                                  READ DDPAPTMRR OUTPUT RECORD                 
         OPEN  RVPDCB                                                           
         GET   RVPDCB,RECORD                                                    
         MVC   RACFUSER,RECORD+14  TSO SUBMITTER'S USERID                       
         MVC   ITMFWHO,RECORD+22   ITMF TICKET SUBMITTER'S TSO USERID           
         CLOSE RVPDCB                                                           
         B     SNDM60                                                           
*                                                                               
SNDM50   DS    0H                                                               
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =C'ACEE',0(RF)           VALID ACEE?                             
         BE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         MVC   RACFUSER,(ACEEUSRI-ACEE)(RF)                                     
*                                                                               
SNDM60   DS    0H                                                               
         LOAD  EP=OPERLST          LIST OF COMPUTER ROOM OPERATOR IDS           
         LTR   RF,RF                                                            
         JNZ   *+2                 CAN'T LOAD THE LOAD MODULE ?!?               
         LR    R8,R0               R8 = A(LIST OF OPERATOR USERIDS)             
*                                                                               
         USING OPERLSTD,R8                                                      
         LH    RE,0(,R8)           L'ENTRY                                      
         L     RF,2(,R8)           A(LAST BYTE OF TABLE)                        
         LA    R8,6(,R8)           A(FIRST ENTRY)                               
SNDM65   DS    0H                                                               
         CLC   RACFUSER,OPRUSRID   IS SUBMITTER AN OPERATOR?                    
         BNE   *+12                                                             
         MVI   OPERATOR,C'Y'       YES: REMEMBER THAT                           
         B     *+8                 STOP TABLE SEARCH                            
         BXLE  R8,RE,SNDM65        NEXT TABLE ENTRY                             
         DROP  R8                                                               
*                                                                               
         DELETE EP=OPERLST         UNLOAD OPERATOR LIST                         
         LTR   RF,RF                                                            
         JNZ   *+2                 CAN'T UNLOAD THE LOAD MODULE ?!?             
*                                                                               
         MVI   FLUSH,C'N'                                                       
         LH    R0,COUNTER                                                       
         AHI   R0,1                                                             
         STH   R0,COUNTER                                                       
         C     R0,DES#MEM          LAST MEMBER?                                 
         BNE   *+14                NO - BUFFER ONLY, NOT SMTP YET               
         MVI   FLUSH,C'Y'                                                       
         XC    COUNTER,COUNTER     CLEAR MEMBER COUNTER                         
*                                                                               
         ICM   R0,15,RTNCODE       RETRIEVE RETURN CODE                         
         BNP   SNDM90              WARNING MESSAGE ONLY: IGNORE IT              
*                                                                               
*                                  BUFFER THIS ERROR                            
         LHI   R0,ERREMQ                                                        
         LARL  R1,ERREMAIL                                                      
         CLC   0(L'ERREMAIL,R1),BLANKS     EMPTY SLOT?                          
         BNH   *+16                        YES - ADD IT                         
         AHI   R1,L'ERREMAIL                                                    
         BCT   R0,*-14                     NO  - TRY NEXT SLOT                  
         B     SNDM90                      NO MORE ROOM                         
*                                                                               
         MVC   0(L'ERREMAIL,R1),BLANKS                                          
         LA    RE,L'M02XXMSG       (=7.5) ERROR MESSAGE                         
         LA    RF,L'ERREMAIL                                                    
         CR    RE,RF               WHICH LENGTH IS SHORTER?                     
         BNH   *+6                                                              
         LR    RE,RF                                                            
*                                                                               
         BCTR  RE,0                                                             
         MVC   0(0,R1),M02XXMSG    STORE THIS ERROR MESSAGE                     
         EX    RE,*-6                                                           
*                                                                               
SNDM90   DS    0H                                                               
*                                                                               
* WE CAN'T BUFFER THE E-MAILS ON A SUB, BECAUSE WE DON'T NECESSARILY            
* GET CALLED FOR EVERY MEMBER IN THE MOVE REQUEST UNLESS WE ARE                 
* PROMOTING TO LEVEL PROD. THEREFORE, WE HAVE NO EASY WAY TO KNOW WHEN          
* WE'VE PROCESSED THE LAST MEMBER.                                              
*                                                                               
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BE    *+12                YES: DON'T BUFFER THE E-MAILS                
         CLI   FLUSH,C'Y'          LAST MEMBER?                                 
         BNE   SNDMAILX            NO - BUFFER ONLY, NOT SMTP YET               
*                                                                               
         LARL  R1,ERREMAIL                                                      
         CLC   0(L'ERREMAIL,R1),BLANKS     ANY ERROR?                           
         BNH   SNDM200                     NO, CHECK ANY WARNING                
*                                                                               
         GOTOR ,DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                              
         L     RF,=V(DYNALLOC)                                                  
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
         MVC   WORKOWNR,DESADDID   MR OWNER                                     
         MVC   WORKUSER,RACFUSER   CURRENT USER                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'HELO),HELO                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'MAILFROM),MAILFROM                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
* SEND E-MAILS TO BOTH USER AND MR OWNER                                        
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKOWNR         MR OWNER                                     
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLC   WORKUSER,WORKOWNR   DON'T SEND TWICE TO THE SAME USER            
         BE    SNDM100                                                          
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKUSER         CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM100  DS    0H                                                               
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   SNDM103                                                          
         CLC   =C'ENV=SBX',M02XXPRM   ARE WE IN THE SANDBOX?                    
         BE    SNDM103                YES: DON'T CC COMPUTER ROOM               
         MVC   LINE,BLANKS         YES: CC THE ENTIRE COMPUTER ROOM             
         MVC   LINE(L'RCPTTO),RCPTTO                                            
*&&US*&& MVC   LINE+L'RCPTTO(33),=C'US-ComputerRoomNY@MEDIAOCEAN.COM>'          
*&&UK*&& MVC   LINE+L'RCPTTO(28),=C'UK-Operators@MEDIAOCEAN.COM>'               
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM103  DS    0H                                                               
         CLC   ITMFWHO,BLANKS                                                   
         BE    SNDM105                                                          
         CLC   ITMFWHO,WORKOWNR                                                 
         BE    SNDM105                                                          
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,ITMFWHO          ITMF TICKET SUBMITTER                        
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM105  DS    0H                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'DATA),DATA                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'FROM),FROM                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,WORKOWNR         MR OWNER                                     
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLC   WORKUSER,WORKOWNR   DON'T CONFUSE THE RECIPIENT WITH A           
         BE    SNDM110              DUPLICATE CC INDICATOR                      
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'CC),CC                                                    
         LA    RF,LINE+L'CC                                                     
         LA    RE,WORKUSER         CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM110  DS    0H                                                               
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   SNDM112                                                          
         CLC   =C'ENV=SBX',M02XXPRM   ARE WE IN THE SANDBOX?                    
         BE    SNDM112                YES: DON'T CC COMPUTER ROOM               
         MVC   LINE,BLANKS         YES: CC THE ENTIRE COMPUTER ROOM             
         MVC   LINE(L'CC),CC                                                    
*&&US*&& MVC   LINE+L'CC(32),=C'US-ComputerRoomNY@MEDIAOCEAN.COM'               
*&&UK*&& MVC   LINE+L'CC(27),=C'UK-Operators@MEDIAOCEAN.COM'                    
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM112  DS    0H                                                               
         CLC   ITMFWHO,BLANKS                                                   
         BE    SNDM115                                                          
         CLC   ITMFWHO,WORKOWNR                                                 
         BE    SNDM115                                                          
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'CC),CC                                                    
         LA    RF,LINE+L'CC                                                     
         LA    RE,ITMFWHO          ITMF TICKET SUBMITTER                        
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM115  DS    0H                                                               
         MVC   LINE,BLANKS                                                      
         MVC   SBJ1MR#,DESNUMB     MOVE REQUEST NUMBER                          
         MVC   SBJ1TXT,=CL40'CANNOT BE CLOSED BECAUSE:'                         
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   *+10                YES: ADJUST SUBJECT                          
         MVC   SBJ1TXT,=CL40'*** FATAL ERROR *** MR SUBMIT FAILED'              
         CLC   DESCLSTA,=AL2(DESSTAPB) BACKOUT PROCESSING IN BATCH?             
         BE    *+14                                                             
         CLC   DESCLSTA,=AL2(DESSTSLB) BACKOUT PROCESSING IN BATCH?             
         BNE   *+10                                                             
         MVC   SBJ1TXT,=CL40'*** BACKOUT WARNING ***'                           
         PUT   SMTPOUT,SUBJECT1                                                 
*                                                                               
*&&DO                                                                           
*THIS IS JUST FOR TRACING IN CASE I NEED IT IN THE FUTURE. YYUN, 6/7/06         
         MVC   LINE,BLANKS                                                      
         MVC   LINE(3),=C'RC='                                                  
         L     R0,RTNCODE           RETRIEVE RETURN CODE                        
         CVD   R0,DBLWRD                                                        
         OI    DBLWRD+7,X'0F'                                                   
         UNPK  LINE+3(5),DBLWRD                                                 
         MVC   LINE+20(L'BASENAME),BASENAME                                     
         MVC   LINE+30(L'OUTNAME),OUTNAME                                       
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(10),=CL10'MEMBER='                                          
         MVC   LINE+10(L'M02XXMEM),M02XXMEM                                     
         MVC   LINE+20(10),=CL10'MEMTYPE='                                      
         MVC   LINE+30(L'MEMTYPE),MEMTYPE                                       
         MVC   LINE+40(10),=CL10'L2LIBC='                                       
         MVC   LINE+50(L'L2LIBC),L2LIBC                                         
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(10),=CL10'ACTION='                                          
         MVC   LINE+10(L'M02XXSRC),M02XXSRC                                     
         PUT   SMTPOUT,LINE                                                     
*&&                                                                             
*                                                                               
         MVC   LINE,BLANKS                                                      
         PUT   SMTPOUT,LINE        NEED A BLANK LINE AS LINE 1                  
         LHI   R8,ERREMQ                                                        
         LARL  R9,ERREMAIL                 (=7.5) ERROR MESSAGES                
SNDM120  CLC   0(L'ERREMAIL,R9),BLANKS     EMPTY SLOT?                          
         BNH   SNDM130                     YES - DONE                           
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'ERREMAIL),0(R9)                                           
         PUT   SMTPOUT,LINE                                                     
         AHI   R9,L'ERREMAIL                                                    
         BCT   R8,SNDM120                                                       
*                                                                               
SNDM130  DS    0H                                                               
         MVC   LINE,BLANKS                                                      
         MVI   LINE,C'.'                                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'QUIT),QUIT                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLOSE SMTPOUT                                                          
*                                                                               
SNDM200  DS    0H                                                               
         LARL  R1,WRNEMAIL                                                      
         CLC   0(L'WRNEMAIL,R1),BLANKS     ANY WARNING?                         
         BNH   SNDM500                     NO, EXIT                             
*                                                                               
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   *+16                                                             
         CLI   WARN1FLG,C'Y'       YES: ONLY SEND ONE E-MAIL ON A SUB           
         BE    SNDM500                                                          
         MVI   WARN1FLG,C'Y'                                                    
*                                                                               
         GOTOR ,DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                              
         L     RF,=V(DYNALLOC)                                                  
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
         MVC   WORKOWNR,DESADDID   MR OWNER                                     
         MVC   WORKUSER,RACFUSER   CURRENT USER                                 
         LAY   RF,WARN1MR#                                                      
         MVC   0(L'WARN1MR#,RF),DESNUMB    MR #                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'HELO),HELO                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'MAILFROM),MAILFROM                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKOWNR         MR OWNER                                     
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLI   OPERATOR,C'Y'       DON'T SEND THIS E-MAIL TO OPERATORS          
         BE    SNDM208                                                          
         CLC   WORKUSER,WORKOWNR   DON'T CONFUSE THE RECIPIENT WITH A           
         BE    SNDM205              DUPLICATE CC INDICATOR                      
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKUSER         CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM205  DS    0H                                                               
         CLC   ITMFWHO,BLANKS                                                   
         BE    SNDM208                                                          
         CLC   ITMFWHO,WORKOWNR                                                 
         BE    SNDM208                                                          
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,ITMFWHO          ITMF TICKET SUBMITTER                        
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM208  DS    0H                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'DATA),DATA                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'FROM),FROM                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,WORKOWNR         MR OWNER                                     
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLI   OPERATOR,C'Y'       DON'T SEND THIS E-MAIL TO OPERATORS          
         BE    SNDM215                                                          
         CLC   WORKUSER,WORKOWNR   DON'T CONFUSE THE RECIPIENT WITH A           
         BE    SNDM210              DUPLICATE CC INDICATOR                      
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'CC),CC                                                    
         LA    RF,LINE+L'CC                                                     
         LA    RE,WORKUSER         CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM210  DS    0H                                                               
         CLC   ITMFWHO,BLANKS                                                   
         BE    SNDM215                                                          
         CLC   ITMFWHO,WORKOWNR    DON'T CONFUSE THE RECIPIENT WITH A           
         BE    SNDM215              DUPLICATE CC INDICATOR                      
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'CC),CC                                                    
         LA    RF,LINE+L'CC                                                     
         LA    RE,ITMFWHO          CURRENT USER                                 
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
SNDM215  DS    0H                                                               
         MVC   LINE,BLANKS                                                      
         MVC   SBJ2MR#,DESNUMB     MOVE REQUEST NUMBER                          
         PUT   SMTPOUT,SUBJECT2                                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         PUT   SMTPOUT,LINE        NEED A BLANK LINE AS LINE 1                  
         LHI   R8,WRNEMQ                                                        
         LARL  R9,WRNEMAIL                                                      
SNDM220  CLC   0(L'WRNEMAIL,R9),BLANKS     EMPTY SLOT?                          
         BNH   SNDM230                     YES - DONE                           
         MVC   WARNLPHS,0(R9)                                                   
         MVC   LINE(L'WARNLINE),WARNLINE                                        
         PUT   SMTPOUT,LINE                                                     
         AHI   R9,L'WRNEMAIL                                                    
         BCT   R8,SNDM220                                                       
*                                                                               
SNDM230  DS    0H                                                               
         LARL  R9,WARNEXP1         START OF BOILERPLATE                         
         CLI   M02XXSRC,BATCH      ARE WE IN THE 'SUB' ACTION?                  
         BNE   SNDM240                                                          
         LARL  R9,WARNEXP1B        START OF BOILERPLATE FOR BATCH               
         MVC   WARNEXP1B_MR#-WARNEXP1B(,R9),DESNUMB   MR#                       
*                                                                               
SNDM240  MVC   LINE,0(R9)          PUT ONE LINE OF TEXT AT A TIME               
         PUT   SMTPOUT,LINE                                                     
         AHI   R9,L'WARNEXP1                                                    
         CLI   0(R9),X'FF'         EOT?                                         
         BNE   SNDM240             PUT ENTIRE BOILERPLATE                       
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVI   LINE,C'.'                                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'QUIT),QUIT                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLOSE SMTPOUT                                                          
*                                                                               
SNDM500  DS    0H                                                               
         LHI   R0,ERREMQ           CLEAR ERROR MESSAGE TABLE                    
         LARL  R1,ERREMAIL                                                      
         MVC   0(L'ERREMAIL,R1),BLANKS                                          
         AHI   R1,L'ERREMAIL                                                    
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,WRNEMQ           CLEAR WARNING PHASE TABLE                    
         LARL  R1,WRNEMAIL                                                      
         MVC   0(L'WRNEMAIL,R1),BLANKS                                          
         AHI   R1,L'WRNEMAIL                                                    
         BCT   R0,*-10                                                          
*                                                                               
SNDMAILX DS    0H                                                               
         LM    RE,RD,SNDMAILL      RESTORE ALL 16 GPRS                          
         BSM   0,RE                EXIT                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB,RA                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* ON A BACKOUT OF MR CONTAINING AN ASSIGNED MEMBER, SEND A WARNING              
* E-MAIL TO THE OWNER OF THE MOVE REQUEST WHICH HAS ASSIGNMENT.                 
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
BKOTWARN DS    0H                                                               
         STM   RE,RD,SNDMAILL      SAVE ALL 16 GPRS                             
         BASR  RB,0                                                             
         AHI   RB,-6                                                            
         USING BKOTWARN,RB,RA      ESTABLISH BASE ADDRESSABILITY                
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
*                                                                               
         USING APAM02XX,R3                                                      
         USING APAMDIB2,R4                                                      
         USING APAMLIB2,R5                                                      
         USING APAMMMBR,R6                                                      
         USING APAMMDES,R7                                                      
*                                                                               
         LA    RF,SETSMTP          MUST USE RF AS BRANCH ADDRESS                
         BASR  RE,RF               SET SYSOUT PARAMETERS FOR DYNALLOC           
*                                                                               
         GOTOR ,DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                              
         L     RF,=V(DYNALLOC)                                                  
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
         MVC   WORKOWNR,INVASSUR   MR OWNER                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'HELO),HELO                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'MAILFROM),MAILFROM                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'DATA),DATA                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'FROM),FROM                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   SBJ2MR#,INVASSMR    MOVE REQUEST NUMBER                          
         PUT   SMTPOUT,SUBJECT2                                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         PUT   SMTPOUT,LINE        NEED A BLANK LINE AS LINE 1                  
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(69),=C'MEMBER XXXXXXXXXX WAS JUST BACKED OUT VIA MR+        
               # NNNNNN BY USER UUUUUUUU'                                       
         MVC   LINE+7(10),MEMSAVE                                               
         MVC   LINE+46(6),DESNUMB  MR# BEING BACKED OUT                         
         MVC   LINE+61(8),DESADDID OWNER OF MR BEING BACKED OUT                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         LARL  R9,WARNEXP2         START OF BOILERPLATE                         
         MVC   WARNEXP2_MEM_A-WARNEXP2(,R9),MEMSAVE     MEMBER NAME             
         MVC   WARNEXP2_MEM_B-WARNEXP2(,R9),MEMSAVE                             
         MVC   WARNEXP2_MR#_O-WARNEXP2(,R9),INVASSMR MR# W/ ASSIGNMENT          
         MVC   WARNEXP2_MR#_B-WARNEXP2(,R9),DESNUMB  BACKED OUT MR#             
         MVC   WARNEXP2_USER-WARNEXP2(,R9),DESADDID  SUBMITTER'S USERID         
*                                                                               
         LHI   R8,WARNXP2Q         NUMBER OF LINES IN BOILERPLATE               
BKOTW10  MVC   LINE,0(R9)          PUT ONE LINE OF TEXT AT A TIME               
         PUT   SMTPOUT,LINE                                                     
         AHI   R9,L'WARNEXP2                                                    
         BCT   R8,BKOTW10          PUT ENTIRE BOILERPLATE                       
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVI   LINE,C'.'                                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'QUIT),QUIT                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLOSE SMTPOUT                                                          
*                                                                               
         LM    RE,RD,SNDMAILL      RESTORE ALL 16 GPRS                          
         BSM   0,RE                EXIT                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB,RA                                                            
         DROP  R3,R4,R5,R6,R7                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* SEND AN E-MAIL WHEN A CRITICAL MEMBER IS ADDED TO A MOVE REQUEST.             
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CRITWARN DS    0H                                                               
         STM   RE,RD,SNDMAILL      SAVE ALL 16 GPRS                             
         BASR  RB,0                                                             
         AHI   RB,-6                                                            
         USING CRITWARN,RB,RA      ESTABLISH BASE ADDRESSABILITY                
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
*                                                                               
         USING APAM02XX,R3                                                      
         USING APAMMMBR,R6                                                      
         USING APAMMDES,R7                                                      
*                                                                               
         LA    RF,SETSMTP          MUST USE RF AS BRANCH ADDRESS                
         BASR  RE,RF               SET SYSOUT PARAMETERS FOR DYNALLOC           
*                                                                               
         GOTOR ,DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                              
         L     RF,=V(DYNALLOC)                                                  
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
         MVC   WORKOWNR,DESADDID   MR OWNER                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'HELO),HELO                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'MAILFROM),MAILFROM                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'DATA),DATA                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'FROM),FROM                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   SBJ3MEM,M02XXMEM    MEMBER NAME                                  
         MVC   SBJ3MR#,DESNUMB     MOVE REQUEST NUMBER                          
         PUT   SMTPOUT,SUBJECT3                                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         PUT   SMTPOUT,LINE        NEED A BLANK LINE AS LINE 1                  
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVI   LINE,C'.'                                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'QUIT),QUIT                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLOSE SMTPOUT                                                          
*                                                                               
         LM    RE,RD,SNDMAILL      RESTORE ALL 16 GPRS                          
         BSM   0,RE                EXIT                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB,RA                                                            
         DROP  R3,R6,R7                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* SEND AN E-MAIL WHEN A MEMBER IS ADDED TO A RELATIVELY RECENT MOVE             
* REQUEST, IF THE MEMBER IS ASSIGNED TO SOMEONE ELSE IN ANOTHER MOVE            
* REQUEST.                                                                      
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
ASSGNWRN DS    0H                                                               
         STM   RE,RD,SNDMAILL      SAVE ALL 16 GPRS                             
         BASR  RB,0                                                             
         AHI   RB,-6                                                            
         USING ASSGNWRN,RB,RA      ESTABLISH BASE ADDRESSABILITY                
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
*                                                                               
         USING APAM02XX,R3                                                      
         USING APAMDIB2,R4                                                      
         USING APAMMMBR,R6                                                      
         USING APAMMDES,R7                                                      
*                                                                               
         LA    RF,SETSMTP          MUST USE RF AS BRANCH ADDRESS                
         BASR  RE,RF               SET SYSOUT PARAMETERS FOR DYNALLOC           
*                                                                               
         GOTOR ,DMCB,(X'FD',=C'SMTPOUT '),SMTPINFO                              
         L     RF,=V(DYNALLOC)                                                  
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         OPEN  (SMTPOUT,OUTPUT)                                                 
         MVC   WORKOWNR,DESADDID   MR OWNER                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'HELO),HELO                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'MAILFROM),MAILFROM                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'RCPTTO),RCPTTO                                            
         LA    RF,LINE+L'RCPTTO                                                 
         LA    RE,INVASSUR         CURRENT MEMBER ASSIGNEE                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(16,RF),=C'@MEDIAOCEAN.COM>'                                    
*&&UK*&& MVC   0(20,RF),=C'DDLO@MEDIAOCEAN.COM>'                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'DATA),DATA                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'FROM),FROM                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,WORKOWNR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'TO),TO                                                    
         LA    RF,LINE+L'TO                                                     
         LA    RE,INVASSUR         CURRENT MEMBER ASSIGNEE                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&US*&& MVC   0(15,RF),=C'@MEDIAOCEAN.COM'                                     
*&&UK*&& MVC   0(19,RF),=C'DDLO@MEDIAOCEAN.COM'                                 
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   SBJ4MEM,M02XXMEM    MEMBER NAME                                  
         MVC   SBJ4MR#,DESNUMB     MOVE REQUEST NUMBER                          
         MVC   SBJ4ASSUR,INVASSUR                                               
         MVC   SBJ4ASSMR,INVASSMR                                               
         PUT   SMTPOUT,SUBJECT4                                                 
*                                                                               
         MVC   LINE,BLANKS                                                      
         PUT   SMTPOUT,LINE        NEED A BLANK LINE AS LINE 1                  
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVI   LINE,C'.'                                                        
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         MVC   LINE,BLANKS                                                      
         MVC   LINE(L'QUIT),QUIT                                                
         PUT   SMTPOUT,LINE                                                     
*                                                                               
         CLOSE SMTPOUT                                                          
*                                                                               
         LM    RE,RD,SNDMAILL      RESTORE ALL 16 GPRS                          
         BSM   0,RE                EXIT                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB,RA                                                            
         DROP  R3,R4,R6,R7                                                      
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* SET THE DYNALLOC PARAMETERS FOR A SYSOUT DATASET FOR AN SMTP EMAIL.           
* THIS ROUTINE MUST BE CALLED WITH RF CONTAINING THE BRANCH ADDRESS.            
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
SETSMTP  DS    0H                                                               
         USING *,RF                RF = A(THIS ROUTINE)                         
         ST    RE,SETSMTP_SAVERE   SAVE RETURN ADDRESS                          
*                                                                               
* SET UP TO DYNAMICALLY ALLOCATE:                                               
*   //SMTPOUT  DD  SYSOUT=(T,SMTP0x),FREE=CLOSE              - US               
*   //SMTPOUT  DD  SYSOUT=A,DEST=(DDLMVS,TCPSMTP),FREE=CLOSE - UK               
*                                                                               
         MVC   SMTPINFO(SMTPINFO_LEN),BLANKS                                    
*&&US                                                                           
         MVI   SMTPCLAS,C'T'                                                    
         L     RE,X'10'(,0)        COMMUNICATION VECTOR TABLE                   
         L     RE,CVTSMCA-CVT(,RE) SYSTEM MANAGEMENT CONTROL AREA               
         LA    RE,SMCASID-SMCABASE(,RE)    CPU ID (SMF)                         
         MVC   SMTPWRTR,=C'SMTP01  ' ASSUME WE'RE ON SY1                        
         CLC   =C'SYC ',0(RE)      SYC = SY1                                    
         JE    *+10                                                             
         MVC   SMTPWRTR,=C'SMTP07  ' WE'RE NOT: ASSUME WE'RE ON SY7             
*&&                                                                             
*&&UK                                                                           
         MVI   SMTPCLAS,C'A'                                                    
         MVC   SMTPNODE,=C'DDLMVS  '                                            
         MVC   SMTPUSER,=C'TCPSMTP '                                            
*&&                                                                             
         L     RE,SETSMTP_SAVERE   RESTORE RETURN ADDRESS                       
         BSM   0,RE                RETURN TO CALLER                             
*                                                                               
SETSMTP_SAVERE DS A                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A PANAPT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL DDS EXTERNAL SUBROUTINE                 
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
SMTPOUT  DCB   DDNAME=SMTPOUT,LRECL=80,RECFM=FB,DSORG=PS,MACRF=PM               
         SPACE 3                                                                
*                                                                               
RACFUSER DS    CL8                                                              
ITMFWHO  DC    CL8' ',C' '                                                      
WORKOWNR DC    CL8' ',C' '                                                      
WORKUSER DC    CL8' ',C' '                                                      
COUNTER  DC    H'0'                                                             
OPERATOR DC    C'N'                IS SUBMITTER AN OPERATOR?                    
FLUSH    DS    C                                                                
WARN1FLG DC    C'N'                'Y' = SEND WARNING E-MAIL ON A SUB           
*                                                                               
SMTPINFO DS    0C                  SYSOUT INFO FOR SMTP E-MAILS                 
SMTPCLAS DS    C                   CLASS                                        
SMTPNODE DS    CL8                 NODEID                                       
SMTPUSER DS    CL8                 USERID                                       
         ORG   SMTPUSER                                                         
SMTPWRTR DS    CL8                 WRITER-NAME                                  
SMTPFCDE DS    CL4                 FORM CODE                                    
SMTPINFO_LEN EQU *-SMTPCLAS                                                     
*                                                                               
LINE     DS    CL80                                                             
*&&US                                                                           
HELO     DC    C'HELO DDNMVS'                                                   
MAILFROM DC    C'MAIL FROM:<PANAPT@DDNMVS>'                                     
*&&                                                                             
*&&UK                                                                           
HELO     DC    C'HELO DDLMVS'                                                   
MAILFROM DC    C'MAIL FROM:<PANAPT@DDLMVS>'                                     
*&&                                                                             
RCPTTO   DC    C'RCPT TO: <'                                                    
FROM     DC    C'FROM: PanAPT'                                                  
TO       DC    C'TO: '                                                          
CC       DC    C'CC: '                                                          
DATA     DC    C'DATA'                                                          
QUIT     DC    C'QUIT'                                                          
*                                                                               
SUBJECT1 DC    CL80' '                                                          
         ORG   SUBJECT1                                                         
         DC    C'SUBJECT: PanAPT MR# '                                          
SBJ1MR#  DC    C'NNNNNN'                                                        
         DC    C' '                                                             
SBJ1TXT  DS    CL40                                                             
         ORG                                                                    
*                                                                               
SUBJECT2 DC    CL80' '                                                          
         ORG   SUBJECT2                                                         
         DC    C'SUBJECT: PanAPT MR# '                                          
SBJ2MR#  DC    C'NNNNNN'                                                        
         DC    C' '                                                             
         DC    C'WARNING! '                                                     
         ORG                                                                    
*                                                                               
SUBJECT3 DC    CL80' '                                                          
         ORG   SUBJECT3                                                         
         DC    C'SUBJECT: Reminder: member '                                    
SBJ3MEM  DC    C'XXXXXXXXXX'                                                    
         DC    C' in PanAPT MR# '                                               
SBJ3MR#  DC    C'NNNNNN'                                                        
         DC    C' requires code review.'                                        
         ORG                                                                    
*                                                                               
SUBJECT4 DS    0CL80                                                            
SBJ4SBJ  DC    C'SUBJECT: Member '                                              
SBJ4MEM  DC    C'XXXXXXXXXX'                                                    
         DC    C' in PanAPT MR '                                                
SBJ4MR#  DC    C'NNNNNN'                                                        
         DC    C' is assigned to '                                              
SBJ4ASSUR DC   C'XXXXXXXX'                                                      
         DC    C' MR '                                                          
SBJ4ASSMR DC   C'NNNNNN'                                                        
         DC    (L'SUBJECT4-(*-SBJ4SBJ))C' '                                     
*                                                                               
WARNLINE DC    CL80' '                                                          
         ORG   WARNLINE                                                         
         DC    C'LIVE PHASE '                                                   
WARNLPHS DC    CL7' '              (8-CHAR. LIVE NAMES ARE PROHIBITED)          
         DC    C' EXISTS ON DDS.TESTLIB'                                        
         ORG                                                                    
*                                                                               
         DS    0H                                                               
WRNEMAIL DC    (WRNEMQ)CL10' '                                                  
ERREMAIL DC    (ERREMQ)CL80' '                                                  
*                                                                               
WRNEMQ   EQU   10                                                               
ERREMQ   EQU   100                                                              
*                                                                               
         EJECT                                                                  
         DS    0H                  FOR SMTP, EACH REC MUST BE 80 BYTES          
WARNEXP1B DC   CL80' '                                                          
         ORG  WARNEXP1B                                                         
         DC   C'(there *may* be more live phases on TESTLIB: see MR# '          
WARNEXP1B_MR# DS CL6               MR#                                          
         DC   C')'                                                              
         ORG                                                                    
WARNEXP1 DC    CL80' '                                                          
 DC CL80'--------------------------------------------------------------+        
               ------------------'                                              
 DC CL80' '                                                                     
 DC CL80'DO NOT IGNORE THIS WARNING. YOU MAY NEED TO TAKE ACTION!'              
 DC CL80' '                                                                     
         DC    CL80' '                                                          
         ORG   *-80                                                             
         DC    C'Examine the live phase(s) on DDS.TESTLIB in MR# '              
WARN1MR# DS    CL6                                                              
         DC    C' and delete them if they'                                      
         ORG                                                                    
         DC    CL80'are no longer needed.'                                      
 DC CL80' '                                                                     
 DC CL80'Phases with live names which remain on DDS.TESTLIB after Move +        
               Request promotion'                                               
 DC CL80'may be problematic. Anything that concatenates TESTLIB ahead o+        
               f LOADLIB (i.e.,'                                                
 DC CL80'our entire test environment) will pick up the live *TESTLIB* v+        
               ersions until'                                                   
 DC CL80'they are deleted. This has the potential to cause enormous con+        
               fusion the *next*'                                               
 DC CL80'time the phase is promoted!'                                           
         DC    X'FF'               EOT                                          
*                                                                               
         DS    0H                                                               
WARNEXP2 DS    0CL80               FOR SMTP, EACH REC MUST BE 80 BYTES          
         DC    CL80' '                                                          
         DC    CL80'---------------------------------------------------+        
               -----------------------------'                                   
         DC    CL80' '                                                          
         DC    CL80'DO NOT IGNORE THIS WARNING. YOU MAY NEED TO TAKE AC+        
               TION!'                                                           
         DC    CL80' '                                                          
         DC    CL80' '                                                          
         ORG   *-80                                                             
WARNEXP2_MEM_A DS CL10             MEMBER NAME                                  
         DC    C' is assigned to you, in MR# '                                  
WARNEXP2_MR#_O DS CL6              MR# THAT HAS ASSIGNMENT                      
         DC    C'. This member is also'                                         
         ORG                                                                    
         DC    CL80' '                                                          
         ORG   *-80                                                             
         DC    C'included in MR# '                                              
WARNEXP2_MR#_B DS CL6              MR# THAT WAS BACKED OUT                      
         DC    C', which was just *backed out* by '                             
WARNEXP2_USER DS CL8               USER THAT SUBMITTED THE BACKOUT              
         DC    C'.'                                                             
         ORG                                                                    
         DC    CL80' '                                                          
         DC    CL80' '                                                          
         ORG   *-80                                                             
         DC    C'It is possible that the version of '                           
WARNEXP2_MEM_B DS CL10                                                          
         DC    C' in your personal'                                             
         ORG                                                                    
         DC    CL80'Pan library contains the same code that prompted th+        
               e backout.'                                                      
         DC    CL80'Consider whether you need to modify your code befor+        
               e you promote it!'                                               
WARNXP2Q EQU   (*-WARNEXP2)/L'WARNEXP2    NO. OF LINES IN BOILERPLATE           
*                                                                               
         EJECT                                                                  
         IHAPSA                                                                 
         IHAACEE                                                                
         IHAASCB                                                                
         IHAASXB                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084DDPAPTMEX 10/19/20'                                      
         END                                                                    
