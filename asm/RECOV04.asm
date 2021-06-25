*          DATA SET RECOV04    AT LEVEL 038 AS OF 08/31/00                      
*          DATA SET RECOV04    AT LEVEL 037 AS OF 03/19/98                      
*&&      SET   NOP=N                                                            
*PHASE T82304A                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'RECOV04 - REP COVERSHEET PROGRAM LEVEL OBJECTS'                 
COV04    CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,RECOV4**,R6,R7,RR=R3,CLEAR=YES                           
         USING RTWORKD,RC                                                       
         ST    R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         ST    R3,RTRELO                                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
*                                                                               
         B     OBJECT                                                           
*                                                                               
A#WRKADD EQU   18                  WORK/ADD EQUATE                              
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT                                                           
         DC    12XL4'00'                                                        
*                                                                               
EXITL    MVI   BCDUB,0             SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   BCDUB,2             SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   BCDUB,1             SET CC EQUAL                                 
EXITCC   CLI   BCDUB,1                                                          
*                                                                               
EXIT     L     R1,RTPARMA          RETURN PARAMS TO CALLER                      
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* OBJECT - PROVIDES INTERFACE TO OBJECTS                              *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   LA    R1,RTPARMS                                                       
         TM    0(R1),GCBOVER       CAN WE OVERRIDE THIS CALL?                   
         BZ    OO02                                                             
         L     RF,AOLY                                                          
         TM    0(R1),GCBPS         FILTERING?                                   
         BZ    *+8                                                              
         L     RF,APSOLY           PREVIOUS SESSION OVERLAY REQUIRED            
         BASR  RE,RF                                                            
         BNH   EXIT                OVERRIDDEN AT A LOWER LEVEL                  
*                                                                               
OO02     LA    R5,TABLEOO          OBJECTS KNOWN AT THIS LEVEL                  
         USING OBJTABD,R5                                                       
         L     RE,0(R1)                                                         
*                                                                               
OO04     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    OO06                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     OO04                ITERATE KNOWN OBJECTS                        
*                                                                               
OO06     ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                  INVOKE OBJECT                                
         DROP  R5                                                               
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OIO),AL1(0,0,0),AL4(IOOBJ)                                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION FOR INTERNAL VERBS                              *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R3,4(R1)                                                      
         LA    R5,KEYTABL                                                       
         USING OBJTABD,R5                                                       
*                                                                               
KEY02    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    KEY04               MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     KEY02               ITERATE                                      
*                                                                               
KEY04    ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
KEYTABL  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R1,R3,4(R1)                                                      
         LA    R5,TABLREC                                                       
         USING OBJTABD,R5                                                       
*                                                                               
REC02    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    REC04               MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     REC02               BUMP & LOOP                                  
*                                                                               
REC04    ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R5                                                               
*                                                                               
TABLREC  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR ZERO                            *         
* P3 BYTE  0   HOLDS GLOBAL ACTION IF P2 IS ZERO                      *         
* P3 BYTES 1-3 HOLD EQUATED VERB                                      *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
***********************************************************************         
         SPACE 1                                                                
DATA     L     RE,4(R1)            RE HOLDS DATA IDENTIFIER                     
         LTR   RE,RE               NO GLOBAL OVERRIDES (SO FAR)                 
         BZ    EXITH                                                            
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA02   CLC   KNOWID,=AL2(EOT)    E.O.T                                        
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,3,KNOWID         MATCH ON DATA TYPE                           
         BE    DATA04                                                           
         LA    RF,KNOWLQ(RF)       ITERATE THE TABLE                            
         B     DATA02                                                           
*                                                                               
DATA04   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,RTRELO           RELOCATE IT                                  
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DS    0H                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
COV04    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD VERB                            
***********************************************************************         
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,RTPARMS                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* IO OBJECT - PROVIDES INTERFACE TO IO ROUTINE                                  
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
IOOBJ    DS    0H                                                               
         LM    R0,R2,RTPARMS                                                    
         LA    RF,TABLEIO                                                       
         B     ITER                                                             
*                                                                               
TABLEIO  DC    AL1(IDIRGET),AL1(0,0,0),AL4(GETDIR)                              
         DC    AL1(IRECGET),AL1(0,0,0),AL4(GETREC)                              
         DC    AL1(IRECRD),AL1(0,0,0),AL4(RDREC)                                
         DC    AL1(IRECADD),AL1(0,0,0),AL4(ADDREC)                              
         DC    AL1(IRECWRT),AL1(0,0,0),AL4(WRTREC)                              
         DC    AL1(EOT)                                                         
***********************************************************************         
* GETS DIRECTORY RECORD AND FORMAT IT INTO GSRECKEY/STA/DA                      
*                                                                               
* ON ENTRY:    GSRECKEY            MINIO MASTER KEY                             
*                                                                               
* ON EXIT:     GSRECKEY            1ST DIRECTORY KEY OF MINIO SET               
*              GSRECSTA            STATUS BYTES OF THE DIRECTORY KEY            
*              GSRECDA             DISK ADDRESS OF 1ST RECORD IN SET            
***********************************************************************         
GETDIR   DS    0H                                                               
         L     R1,=AL4(XOHID+XIO11)   READ HIGH INTO IOAREA 11                  
         LH    RF,GSDIRDSP               DELETED RECORDS ARE OKAY               
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         RTFILDIR=A(DIRECTORY TABLE ENTRY)            
         USING NFITABD,RF                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF DIRECTORY KEY                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   IOKEY(0),GSRECKEY                                                
*                                                                               
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                MULTIPLY IT BY 256 FOR EQUATE                
         LA    R1,0(RE,R1)                                                      
         GOTOX (XIO,AGROUTS)       DO THE I/O                                   
*                                                                               
         CLI   IOERR,0             TEST RECORD READ SUCCESSFULLY                
         BE    GETDIR10                                                         
         CLI   IOERR,IOEDEL        TEST RECORD DELETED                          
         BE    GETDIR10                                                         
         DC    H'0'                SOME OTHER ERROR, DIE                        
*                                                                               
GETDIR10 CLC   IOKEY(L'RCOVKMST),IOKEYSAV   CHECK FOR L(MASTER KEY)             
         BE    GETDIR20                                                         
GETDIR15 XC    GSRECSTA,GSRECSTA   SAVE THE DIRECTORY STATUS DETAILS            
         XC    GSRECDA,GSRECDA     CLEAR D/A FOR NOT ON FILE                    
         B     GETDIRX                                                          
*                                                                               
GETDIR20 DS    0H                                                               
         L     RF,RTFILDIR                                                      
         XC    GSRECSTA,GSRECSTA   SAVE THE DIRECTORY STATUS DETAILS            
         XR    RE,RE                                                            
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         XR    R1,R1                                                            
         IC    R1,NFIKEYL          LENGTH OF THE KEY                            
         LA    R1,IOKEY(R1)        START OF THE CONTROL INFORMATION             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R1)                                                
*                                                                               
         MVC   GSRECDA,IODA        SAVE THE D/A                                 
         MVC   GSRECMSK,BCEFFS     SET MASK FROM DIRECTORY                      
         GOTOX APRG,BOPARM,('GCBOVER',OKEY),KMASK,GSRECKEY,GSRECMSK             
*                                                                               
GETDIRX  B     EXITOK                                                           
         DROP  RF                                                               
***********************************************************************         
* INTIIALIZE MINIO                                                              
*                                                                               
* NTRY: P3=XOLOCK TO READ FOR UPDATE, OR 0                                      
* EXIT: CC=HIGH IF RECORD HAS CHANGED SINCE LAST READ                           
***********************************************************************         
GETREC   DS    0H                                                               
         L     R0,AIO7             IO7 IS USED FOR MINBLK & MINELEM             
         LA    R1,IOAREALN                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR THEM BOTH                              
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,=V(RECUP)                                                     
         A     RF,RTRELO                                                        
         ST    RF,MINRECUP                                                      
         MVC   MINCOMF,ACOM                                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'REPFIL'                                              
         MVC   MINDIR,=CL8'REPDIR'                                              
         MVI   MINFKLEN,L'RCOVKEY    KEY LENGTH <- FIX THESE ROB                
         MVI   MINEKLEN,L'RCOVKMEL   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'RCOVKMST   DISPLACEMENT TO ELEMENT KEY                
         MVC   MINAGYC,CUAALF             AGENCY                                
         MVI   MINAGYD,5                  DISP TO POWER CODE IN KEY             
         MVI   MINNCTL,1             NUMBER OF CONTROL BYTES                    
         MVC   MINFRCLM,=AL2(1925)   MAXIMUM RECORD LENGTH                      
         MVC   MINBUFF,AIO8        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           NUMBER OF BUFFERS AVAILABLE                  
         MVC   MINRTAB,ARECOV02    <=== SET IN RECOVINI                         
         MVC   MINRTABL,=Y(LENMINIO)  L(MINIO RECORD TABLE)                     
*                                                                               
***>     MVI   MINNKLO,RPRDYELQ    ELEMENTS IN CLUSTER THAT DON'T NEED          
***>     MVI   MINNKHI,RPRNDELQ       A MINIO ELEM KEY (X'61'-X'8F')            
*                                                                               
         L     R1,AIO7             USE 2ND HALF OF IO7 FOR MINIO ELEM           
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)  MAXLEN(ELEM OR CLUSTER)           
*                                                                               
         XC    MINMKEY,MINMKEY     SET MASTER KEY FOR MINIO                     
         LA    R4,MINMKEY                                                       
         MVC   0(L'RCOVKMST,R4),GSRECKEY                                        
*                                                                               
MNOINIX  B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* GET FILE RECORD FROM DISK ADDRESS                                   *         
*                                                                     *         
* NTRY: P3=XOLOCK TO READ FOR UPDATE, OR 0                            *         
* NTRY: P4=DISK ADDRESS                                               *         
* NTRY: P5=EQUATE FOR IOAREA, OR 0 FOR AIOREC                         *         
* EXIT: CC=HIGH IF RECORD HAS CHANGED SINCE LAST READ                 *         
***********************************************************************         
RDREC    DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* ADD RECORD TO FILE                                                  *         
***********************************************************************         
ADDREC   DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* WRITE RECORD ROUTINE                                                *         
***********************************************************************         
WRTREC   DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO  &VBTAB,&ID,&LB                                                  
         AIF   (T'&ID NE 'O').L1                                                
         DC    CL8' '                                                           
         AGO  .L2                                                               
.L1      ANOP                                                                   
         DC    CL8'&ID'                                                         
.L2      ANOP                                                                   
         DS    0H                                                               
         USING *,RB                                                             
&NTRDO   NTR1  BASE=(RF)                                                        
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,&VBTAB           TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
&LB.03   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    &LB.H                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    *+12                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     &LB.03              BUMP & LOOP                                  
*                                                                               
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
&LB.XL   MVI   RTPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     &LB.E                                                            
&LB.XE   MVI   RTPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     &LB.E                                                            
&LB.XH   MVI   RTPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     &LB.E                                                            
&LB.XX   MVI   RTPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     &LB.E                                                            
*                                                                               
&LB.L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     &LB.CC                                                           
&LB.H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
&LB.E    MVI   RTBYTE1,1           SET CC EQUAL                                 
&LB.CC   CLI   RTBYTE1,1                                                        
*                                                                               
*                                                                               
&LB.X    XIT1                                                                   
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMO  DS    F                                                                
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
*                                                                               
RTFILDIR DS    A                   A(THIS SYSTEM DIRECTORY ENTRY)               
RTFILREC DS    A                   A(THIS SYSTEM FILE ENTRY)                    
*                                                                               
RTVFINDS DS    XL1                                                              
RTVFIVAL EQU   X'80'                                                            
RTVFINOT EQU   X'40'                                                            
RTDATA   DS    XL300                                                            
RTWORK   DS    XL80                                                             
RTBYTE1  DS    X                                                                
*                                                                               
RTWORKL  EQU   *-RTWORKD                                                        
*                                                                               
COV04    CSECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* RECOVWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE RECOVWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
COV04    CSECT                                                                  
         ORG   COV04+(((*-COV04)/2048)+1)*2048                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038RECOV04   08/31/00'                                      
         END                                                                    
