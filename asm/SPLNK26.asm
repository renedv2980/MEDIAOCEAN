*          DATA SET SPLNK26    AT LEVEL 005 AS OF 09/12/17                      
*PHASE T21E26A                                                                  
*INCLUDE DPTRD                                                                  
*                                                                               
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* SMUR  28OCT14 001 INITIAL DEVELOPMENT - COMMERCIAL UPLOAD                     
* SMUR  05JAN15 002 PATTERN UPLOAD                                              
* HWON  12SEP17 005 SPEC-13049-RELINK FOR LARGER IOAREAS                        
*                                                                               
*=====================================================================*         
*                                                                               
SPLNK26  TITLE 'SPOT/LINK TRAFFIC COMMERCIAL UPLOAD'                            
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=500,REQUEST=*,WORKERKEY=SPTU,    +        
               LINKIO=Y,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),          +        
               SYSTEM=SPTSYSQ                                                   
         EJECT                                                                  
ENTRY    NMOD1 0,**SL26**,RR=RE,CLEAR=Y                                         
         LR    R6,R1               R1=A(LP_D)                                   
         USING LP_D,R6                                                          
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     RF,LP_ARUNP                                                      
         MVC   BYTE1,RUNPMODE-RUNPARMD(RF)                                      
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         L     RA,AGLOBALS                                                      
         A     RA,SRVRRELO                                                      
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   BYTE1,RRUNSTRQ      TEST 'FIRST' MODE                            
         JE    FIRST                                                            
         CLI   BYTE1,RINIREQQ      TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   BYTE1,RRUNREQQ      TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         CLI   BYTE1,RRUNENDQ      TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
*                                                                               
         J     EXITY                                                            
*                                                                               
AGLOBALS DC    A(GLOBALS)                                                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RUN FIRST                                                                     
***********************************************************************         
*                                                                               
FIRST    DS    0H                                                               
*                                                                               
         LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,BTODAY)                                    
*                                                                               
         MVC   LP_BLKS+((B#CMLREC-1)*L'LP_BLKS)(AIOLAST-AIO3),AIO3              
*                                                                               
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* INIT - NOTE FIRST REC PROCESSED *BEFORE* INIT CALL!                           
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         MVI   DATADISP+1,24                                                    
*                                                                               
         MVC   WVALUES(WVALUESL),LVALUES                                        
         LHI   R0,ADCONSN                                                       
         CHI   R0,0                                                             
         JE    INIT10                                                           
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
*                                                                               
INIT10   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         LA    RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         OI    LIOBINDS,LIOBINRM   INHIBIT AUTO RETURN MAP ELEMENT              
*                                                                               
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         DROP  RF                                                               
*                                                                               
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                  EXTRACT A(RECUP) FROM COMFACS                
         MVC   ARECUP,CRECUP-COMFACSD(RF)                                       
*                                  EXTRACT A(DATVAL) FROM COMFACS               
         MVC   VDATVAL,CDATVAL-COMFACSD(RF)                                     
*                                  EXTRACT A(DATVAL) FROM COMFACS               
         MVC   VADDAY,CADDAY-COMFACSD(RF)                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A0E'    GET ADDRESS OF TIMVAL                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTIMVAL,0(R1)                                                    
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS AN UPLOAD RECORD                                                      
***********************************************************************         
*                                                                               
INPUT    LA    RE,RECTAB                                                        
         USING RECTABD,RE                                                       
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   RECTMAP#,LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE             
         JE    INPUT20                                                          
         AHI   RE,RECTABL                                                       
         JCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  CLI   RECTFLAG,X'FF'        CLEAR ALL ERRORS?                          
         JNE   INPUT30               YES                                        
         MVI   MISCFLG3,0                                                       
*                                                                               
INPUT30  CLI   MISCFLG3,0                                                       
         JNE   EXITY                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,RECTPROG                                                    
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR                                                       
*                                                                               
         GOTOR RECADDR                                                          
         J     EXITY               EXIT BACK TO DDLINK                          
*                                                                               
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTPROG DS    AL3                 A(ROUTINE)                                   
RECTFLAG DS    X                                                                
RECTABL  EQU   *-RECTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* COMMERCIAL UPLOAD                                                             
***********************************************************************         
CMLUPLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,I$CAM                                                      
         JZ    ERR00258            MISSING MEDIA                                
*                                                                               
         USING LW_D,RE                                                          
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
         GOTOR (#VALMED,AVALMED),DMCB,LW_DATA1,(R3),SVBAGM                      
         JNE   ERR01023            INVALID MEDIA                                
*                                                                               
         ICM   RE,15,I$CCLT                                                     
         JZ    ERR00966            MISSING CLIENT                               
*                                                                               
         MVC   QCLTA,LW_DATA1                                                   
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
*                                                                               
         CHI   R3,2                                                             
         JL    ERR00014            INVALID CLIENT                               
         JH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
*                                                                               
         GOTOR (#VALCLT,AVALCLT),DMCB,LW_DATA1,(R3),SVBCLT                      
         JNE   ERR00014            INVALID CLIENT                               
*                                                                               
         BRAS  RE,GPROF                                                         
*                                                                               
         LA    R2,QCOMML                                                        
         XC    NXTCML,NXTCML       INIT ADDRESS OF NEXT CML                     
         CLI   RECACT,C'X'         DELETE                                       
         JNE   CMLU004                                                          
                                                                                
         MVI   ERRFLG,0                                                         
         BRAS  RE,BCMLLST           YES, BUILD LIST OF CMLS IF ANY              
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
                                                                                
         LA    R2,ELEM2                                                         
         LA    RF,12(R2)                                                        
         CLI   0(R2),X'40'         SINGLE CML REQUEST                           
         JNH   CMLU004                                                          
         ST    RF,NXTCML           SAVE ADDRESS OF NEXT CML                     
*                                                                               
CMLU004  MVI   ERRFLG,0                                                         
         BRAS  RE,VCML                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
CMLU005  LA    R4,IOKEY                                                         
         USING CMLRECD,R4                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVI   CMLKID,X'0A'        X'0A21'                                      
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,SVBAGM                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTRFDIR+#CMLREC'                        
         JE    CMLU010                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'      Try passive pointer                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTRFDIR+#CMLREC'                        
         JNE   CMLU015                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
CMLU010  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+#CMLREC'                    
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ACMLREC          PREPARE IOAREA FOR THE ADD                   
*                                                                               
         CLI   RECACT,C'X'         DELETE                                       
         JE    CMLU012                                                          
         CLI   RECACT,C'R'         RESTORE                                      
         JE    CMLU012                                                          
         CLI   RECACT,C'C'                                                      
         JNE   ERR00049            RECORD ALREADY EXISTS                        
*                                                                               
CMLU012  L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     RE,ACMLREC          SAVE ORIGINAL RECORD IN IO8                  
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIO8                                                          
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
*                                                                               
         OC    QCKSM,QCKSM         ANY CHECKSUM PASSED                          
         JNZ   *+16                 YES, CHECK                                  
         CLI   RECACT,C'C'                                                      
         JE    ERR00814            NEED CHECK SUM FOR ACTION CHANGE             
         J     CMLU018                                                          
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCKSUM           VALIDATE CHECK SUM                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         J     CMLU018                                                          
*                                                                               
CMLU015  CLI   RECACT,C'A'                                                      
         JNE   ERR00971            RECORD NOT FOUND                             
*                                                                               
         L     RE,ACMLREC          PREPARE IOAREA FOR THE ADD                   
         LHI   RF,ACMLREQ                                                       
         XCEFL                                                                  
*                                                                               
         L     R4,ACMLREC                                                       
         USING CMLRECD,R4                                                       
         MVI   CMLKID,X'0A'        X'0A21'                                      
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,SVBAGM                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK        PACKED CML                                   
         MVI   CMLRLEN+1,24        RECORD LENGTH                                
         MVC   CMLAGYA,LP_AGY      SET AGENCY ALPHA ID                          
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),0(R4)                                                  
*                                                                               
CMLU018  MVC   HOLDCML,WORK                                                     
         MVC   SVKEY,IOKEY                                                      
*                                                                               
         CLI   RECACT,C'X'         IF ACTION DELETE                             
         JE    CMLU025                                                          
         CLI   RECACT,C'R'         OR RESTORE                                   
         JE    CMLU025             PROCESS X'10' ELEM                           
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VPRDL            VALIDATE PROD LIST & BUILD ELEM              
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    CMLU020                                                          
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'        PROD LIST ELEM CODE                          
         BRAS  RE,GETEL            DELETE ELEMENT PRESERVING ELEM               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
CMLU020  MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
*===========================================================                    
* COMMERCIAL TITLES                                                             
*===========================================================                    
*                                                                               
CMLU025  L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    CMLU030                                                          
*                                                                               
         CLI   RECACT,C'A'                                                      
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                         1             
         MVI   ELEM+1,CMLDTAX-CMLDTAEL                                          
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     CMLU025             GO FIND ELEM JUST INSERTED                   
*                                                                               
         USING CMLDTAEL,R4                                                      
CMLU030  CLC   HOLDCML,=8C'9'       CML ID ALL 9'S (PROD HSE KEY)               
         JE    ERR00002             INVALID ENTRY                               
*                                                                               
         OC    CMLCOVCT,CMLCOVCT   IS THIS A COVD (ACT) CML?                    
         JZ    *+8                                                              
         OI    FLAGS,ISCOVERD                                                   
*                                                                               
         CLI   RECACT,C'X'         SOFT DELETE THIS CML                         
         JNE   CMLU040               NO                                         
*                                                                               
CMLU035  TM    FLAGS,ISCOVERD      IS CMML COVERED?                             
         JO    ERR00661             NO                                          
*                                                                               
         OI    CMLSTAT,X'80'       SOFT DELETE RECORD                           
         OI    FLAGS,SOFTDEL                                                    
         J     CMLU102                                                          
*                                                                               
CMLU040  CLI   RECACT,C'R'         RESTORE SOFT DELETE?                         
         JNE   CMLU045               NO                                         
         TM    CMLSTAT,X'80'       WAS CML SOFT DELETED                         
         JZ    ERR00581            CAN'T RESTORE                                
         NI    CMLSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         OI    FLAGS,SOFTREST                                                   
         J     CMLU102                                                          
*                                                                               
CMLU045  MVI   ERRFLG,0                                                         
*                                                                               
         OC    QCMLTTLE,QCMLTTLE   ANY CML TITLE?                               
         JZ    ERR00001                                                         
*                                                                               
         BRAS  RE,VALTTL                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
CMLU050  L     R4,ACMLREC          MOVE X'10' ELEM OUT OF RECORD                
         MVI   ELCODE,X'10'        IT NOW CONTAINS NEW TITLE1                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         LA    R4,ELEM                                                          
*                                                                               
*==================================================================             
* VALIDATE COMMERCIAL LEN                                                       
*==================================================================             
*                                                                               
CMLU055  CLI   QCMLSLN,0           ANY ENTRY                                    
         JE    ERR00243            INVALID                                      
*                                                                               
         MVC   SVSLN,QCMLSLN                                                    
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALISLN                                                       
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         CLC   CMLSLN,SVSLN                                                     
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SLN                                               
         MVC   CMLSLN,SVSLN                                                     
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VOVRD            GO GET PRINTABLE OVERRIDE SPOT LENS          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*==================================================================             
* VALIDATE START (RELEASE) AND END (RECALL) DATES                               
*==================================================================             
*                                                                               
         CLC   CMLRLSE,QCMLRLSE                                                 
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SDAT                                              
         MVC   CMLRLSE,QCMLRLSE                                                 
*                                                                               
         CLC   =C'99/99/99',QCMLRCL  UFN                                        
         JNE   CMLU060             NO, PROCESS IT                               
         MVC   WORK(3),=X'FFFFFF'  FORCE IT                                     
         J     CMLU065                                                          
*                                                                               
CMLU060  GOTOR VDATVAL,DMCB,(0,QCMLRCL),DATE                                    
         OC    DMCB(4),DMCB                                                     
         JZ    ERR00020                                                         
         GOTOR VDATCON,(R1),(0,DATE),(3,WORK)                                   
*                                                                               
CMLU065  CLC   CMLRCL,WORK                                                      
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_EDAT                                              
         MVC   CMLRCL,WORK                                                      
*                                                                               
         CLC   CMLRLSE,CMLRCL      CAN'T RECALL BEFORE RELEASE                  
         JH    ERR00020                                                         
*                                                                               
         MVC   WORK(3),CMLRLSE                                                  
         MVC   WORK+3(3),CMLRCL                                                 
*                                                                               
* >>>> FUTURE DEV <<<<<                                                         
*NOP     BRAS  RE,CHKPRSOV         SEE IF PRD SUB ELEMS IN PERIOD               
*                                                                               
         XC    CMLLMTCD(11),CMLLMTCD CLEAR RUN LIMIT CODE                       
*                                                                               
*============================================================                   
* CLIENT COMMERCIAL NUMBER                                                      
*============================================================                   
*                                                                               
         CLC   QCMLCLTN,SPACES                                                  
         JNE   *+10                                                             
         XC    QCMLCLTN,QCMLCLTN                                                
*                                                                               
         CLC   CMLCLTNO,QCMLCLTN                                                
         JE    CMLU070                                                          
         OI    CHGFLAG2,CMLCH_CLNO                                              
         MVC   CMLCLTNO,QCMLCLTN                                                
         OC    CMLCLTNO,SPACES                                                  
*                                                                               
*==================================================================             
* VALIDATE P/J-SOLO OPTION                                                      
*==================================================================             
*                                                                               
CMLU070  CLI   QCMLSOLO,0                                                       
         JE    CMLU072                                                          
         CLI   QCMLSOLO,C'S'                                                    
         JE    CMLU072                                                          
         CLI   QCMLSOLO,C'P'                                                    
         JNE   ERR00580                                                         
*                                                                               
CMLU072  CLC   CMLSOLO,QCMLSOLO                                                 
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_OTHR                                              
         MVC   CMLSOLO,QCMLSOLO                                                 
*                                                                               
         NI    CMLSTAT,X'FF'-X'40' INIT COMMENT EXISTS FLAG                     
*                                                                               
*==================================================================             
* TALENT TRANSFER EXCLUDE OPTION                                                
*==================================================================             
*                                                                               
         CLI   QCMLTALX,0                                                       
         JE    CMLU075                                                          
         CLI   QCMLTALX,C'Y'       YES, EXCLUDE FROM TALENT TRANSFER            
         JE    CMLU075                                                          
         CLI   QCMLTALX,C'N'                                                    
         JNE   ERR00742                                                         
*                                                                               
* REMOVE BELOW PER RAMUNE                                                       
*NOP     CLI   SVTTPR3,C'Y'        IF SPOT TALENT USER, NOT ALLOWED             
****     JNE   CMLU075                                                          
*                                                                               
* MAKE SURE THAT THIS IS NOT PRD=ALL                                            
*                                                                               
*        LR    R5,R4                                                            
*        L     R4,AIO                                                           
*        MVI   ELCODE,X'20'                                                     
*        BRAS  RE,GETEL                                                         
*        JE    *+6                                                              
*        DC    H'0'                                                             
*        CLI   2(R4),X'FF'         THIS ALL PRODUCTS                            
*        JE    PRDALLRR                                                         
*******  LR    R4,R5                                                            
*                                                                               
CMLU075  CLC   CMLTALEX,QCMLTALX                                                
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_TAL                                               
         MVC   CMLTALEX,QCMLTALX                                                
*                                                                               
CMLU080  XC    CMLCLASS,CMLCLASS  CLEAR FOR NOW                                 
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+12                                                             
         OI    CMLOPFLG,CMLOPADD   CML ADDED IN OPTICA                          
         J     CMLU085                                                          
*                                                                               
         OI    CMLOPFLG,CMLOPCHG   CML CHANGED IN OPTICA                        
*                                                                               
CMLU085  MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL            INSERT X'10' ELEMENT NOW!                    
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   CMLU090                                                          
         MVI   ERRFLG,0                                                         
         BRAS  RE,DUPCHK           MAKE SURE THIS CML DOES NOT EXIST            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         CLI   ADIDFLAG,C'Y'       TEST CMML IS AN ADID                         
         JNE   *+8                 NO                                           
         BRAS  RE,AADIDEL          ELSE ADD ADID ELEM FOR CMML                  
*                                                                               
*==================================================================             
* VALIDATE MATCH DATES, TIMES                                                   
*==================================================================             
*                                                                               
CMLU090  MVI   ERRFLG,0                                                         
         BRAS  RE,VALMAT                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*=================================================================              
* VALIDATE HIDEF COMMERCIAL                                                     
* PER RAMUNE 08FEB08, JUST ACCEPT ANY COMMERCIAL-LIKE INPUT                     
* PER RAMUNE 21FEB08, DO NOT ALLOW ANY SPECIAL CHARACTERS                       
*=================================================================              
*                                                                               
VRHD     DS    0H                                                               
         MVI   BYTE,0              INIT INPUT LEN                               
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVHIDEFX,SVHIDEFX                                                
         XC    SVHDADDK,SVHDADDK   CLEAR HIDEF KEY TO ADD                       
         XC    SVHDDELK,SVHDDELK   HIDEF KEY TO DELETE                          
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   VRHD003                                                          
*                                                                               
         CLI   QFORMAT,0           IS THERE A HIDEF FLAG                        
         JE    VRPC                 NO, DONE                                    
*                                                                               
VRHD003  XC    WORK,WORK                                                        
         LA    RF,QCOMML                                                        
         LA    RE,L'QCOMML-1                                                    
         BRAS  R1,FLEN             FIND INPUT LEN-1                             
*                                                                               
         STC   RE,BYTE             SAVE IT                                      
*                                                                               
         CLI   QFORMAT,0           IS THERE A HIDEF FLAG                        
         JE    VRHD005                                                          
*                                                                               
         CLI   QFORMAT,C'H'        H IS THE ONLY VALID ENTRY                    
         JNE   ERR00815                                                         
*                                                                               
         CLI   BYTE,11             CML LEN MUST BE LESS THAN 12                 
         JNL   ERR00765                                                         
*                                                                               
         BASR  R1,0                                                             
         MVC   WORK(0),QCOMML      MOVE CML TO WORK                             
         EX    RE,0(R1)                                                         
         OC    WORK(12),SPACES                                                  
*                                                                               
         LA    RF,WORK                                                          
         AHI   RE,1                ACTUAL INPUT LEN                             
         STC   RE,BYTE                                                          
         AR    RF,RE               POINT TO END OF CML                          
         MVI   0(RF),C'H'          H TO THE END OF CML FOR HIDEF                
*                                                                               
         MVC   SVHIDEF,WORK                                                     
         OC    SVHIDEF,SPACES                                                   
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    VRHD010                                                          
*                                                                               
VRHD005  BRAS  RE,GETHIDEF         LOOK UP HIDEF IN EXISTING REC                
         JE    VRPC                PASSIVE ALREADY MAINTAINED                   
*                                                                               
         CLI   QFORMAT,0                                                        
         JE    VRHD050             DELETE OLD HIDEF                             
*                                                                               
VRHD010  XC    NWHIDEFX,NWHIDEFX   INITIALIZE NEW HIDEF CODE                    
         LA    R2,WORK                                                          
         CLI   BYTE,0              INPUT LEN?                                   
         JE    VRHD050                                                          
         CLI   BYTE,8                                                           
         JL    ERR00765                                                         
*                                                                               
         LLC   R0,BYTE             MAKE SURE NO SPCL CHARS                      
         LA    R1,WORK                                                          
*                                                                               
VRHD020  CLI   0(R1),C'A'                                                       
         JL    ERR00734                                                         
         CLI   0(R1),C'Z'                                                       
         JNH   VRHD030                                                          
         CLI   0(R1),C'0'                                                       
         JL    ERR00734                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR00734                                                         
VRHD030  LA    R1,1(R1)                                                         
         JCT   R0,VRHD020                                                       
*                                                                               
         GOTOR VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         JNE   ERR00734                                                         
*                                                                               
         MVC   NWHIDEFX,WORK+12                                                 
*                                                                               
         BRAS  RE,HDDUPS                                                        
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00756                                                         
*                                                                               
VRHD050  DS    0H                                                               
         OC    SVHIDEF,SVHIDEF                                                  
         JZ    VRHD070                                                          
         MVC   SVHDDELK(2),=X'0AC2'                                             
         MVC   SVHDDELK+2(3),SVBAGM                                             
         MVC   SVHDDELK+5(8),SVHIDEFX                                           
*                                                                               
VRHD070  DS    0H                                                               
         CLI   QFORMAT,0                                                        
         JE    VRPC                NO HIDEF                                     
*                                                                               
         MVC   SVHDADDK(2),=X'0AC2'                                             
         MVC   SVHDADDK+2(3),SVBAGM                                             
         MVC   SVHDADDK+5(8),WORK+12                                            
*                                                                               
*=================================================================              
* VALIDATE PARENT COMMERCIAL                                                    
*=================================================================              
*                                                                               
VRPC     DS    0H                                                               
         OC    QCMLPRNT,QCMLPRNT   ANY PARENT CML                               
         JZ    VRPCX                                                            
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALCMML                                                       
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VRPCX    DS    0H                                                               
*                                                                               
*======================================================================         
* VALIDATE TELECASTER NUMBER (CANADA ONLY)                                      
*======================================================================         
*&&DO                                                                           
VRSWAP   DS    0H                                                               
         MVI   SVSWAP,0                                                         
         LA    R2,TRASWAPH                                                      
         CLI   5(R2),0                                                          
         JE    VRTLCST                                                          
         MVI   SVSWAP,C'N'                                                      
         CLI   TRASWAP,C'N'                                                     
         JE    VRTLCST                                                          
         MVI   SVSWAP,C'Y'                                                      
         CLI   TRASWAP,C'Y'                                                     
         JE    VRTLCST                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*======================================================================         
* VALIDATE TELECASTER NUMBER (CANADA ONLY)                                      
*======================================================================         
*                                                                               
VRTLCST  DS    0H                                                               
*                                                                               
*&&                                                                             
*                                                                               
*======================================================================         
* VALIDATE CLASS                                                                
*======================================================================         
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCLASS                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*======================================================================         
* VALIDATE DESTROY DATE AND TIME                                                
*======================================================================         
*                                                                               
         XC    DSTRYDAT(5),DSTRYDAT   CLEAR DATE/TIME SAVE AREA                 
*                                                                               
         OC    QCMLDSDT,QCMLDSDT                                                
         JZ    CMLU100                                                          
*                                                                               
         MVC   DSTRYDAT,QCMLDSDT                                                
*                                                                               
         OC    QCMLDSTM,QCMLDSTM                                                
         JZ    CMLU100                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    RF,QCMLDSTM         FIND INPUT LEN                               
         LA    RE,L'QCMLDSTM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                                                             
         STC   RE,BYTE             SAVE LEN                                     
*                                                                               
         LA    R4,QCMLDSTM                                                      
         ICM   R4,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R4),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747                                                         
         MVC   DSTRYTIM,WORK                                                    
         OC    DSTRYTIM,DSTRYTIM   TEST FOR MIDNIGHT                            
         JNZ   *+10                                                             
         MVC   DSTRYTIM,=H'2400'                                                
*                                                                               
CMLU100  L     R4,ACMLREC                                                       
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'24'        UPDATE EXTENDED DATA ELEM                    
         BRAS  RE,GETEL                                                         
         JNE   CMLU100C                                                         
*                                                                               
         BAS   RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
CMLU100C LA    R4,ELEM                                                          
         USING CMLXDTEL,R4                                                      
*                                                                               
         CLC   CMLXPRNT,QCMLPRNT                                                
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_PRNT                                              
*                                                                               
         CLC   CMLXHDEF,SVHIDEF                                                 
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_HDEF                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   CMLXDTEL,X'24'                                                   
         MVI   CMLXDTLN,CMLXDTX-CMLXDTEL                                        
         MVC   CMLXHDEF,SVHIDEF                                                 
         MVC   CMLXPRNT,QCMLPRNT                                                
         MVC   CMLXDSDT,DSTRYDAT                                                
         MVC   CMLXDSTM,DSTRYTIM                                                
         MVC   CMLXHDPK,NWHIDEFX                                                
*                                                                               
         LLC   R1,CMLXDTLN                                                      
         SHI   R1,3                                                             
*                                                                               
         BASR  RF,0                                                             
         OC    ELEM+2(0),ELEM+2                                                 
         EX    R1,0(RF)                                                         
         JZ    CMLU102             DO NOT ADD AN EMPTY ELEMENT                  
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         DROP  R4                                                               
*                                                                               
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS                                                   
* IF DELETE, LEAVE TITLES ALONE                                                 
* IF RESTORE, VALIDATE ACT CMLS IF COV                                          
*======================================================================         
*                                                                               
CMLU102  TM    FLAGS,SOFTDEL       SOFT DELETE THIS CML                         
         JNZ   CMLU110             YES - NO OTHER CHANGES                       
         TM    FLAGS,SOFTREST      RESTORE SOFT DELETE?                         
         JZ    CMLU104             NO, CONTINUE                                 
         TM    FLAGS,ISCOVCML      IS THIS A COVER?                             
         JNZ   CMLU105             YES - MAKE SURE ACTUALS STILL VALID          
         J     CMLU110             NO - DISALLOW OTHER CHANGES                  
*                                                                               
* ACCEPT NO CHANGES ON A DELETED CMML                                           
*                                                                               
CMLU104  L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    CMLSTAT-CMLDTAEL(R4),X'80'                                       
         JNZ   ERR00535                                                         
*                                                                               
CMLU105  DS    0H                                                               
         MVC   BYTE4,ADIDFLAG                                                   
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALACT                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   ADIDFLAG,BYTE4                                                   
*                                                                               
CMLU110  MVC   IOKEY,SVKEY                                                      
*                                                                               
*===============================================================                
* ADD CHANGE ELEMENT TO RECORD                                                  
*===============================================================                
*                                                                               
CMLU115  BRAS  RE,SETCHGEL                                                      
*                                                                               
         L     RE,ACMLREC                                                       
         ST    RE,SVAREC                                                        
         BRAS  RE,AACTELEM         ADD ACTIVITY ELEMENT                         
*                                                                               
         L     R4,ACMLREC                                                       
         CLI   ADIDFLAG,C'Y'       TEST CMML PACKED                             
         JNE   *+12                NO                                           
         OI    15(R4),X'01'        TURN ON STATUS IN RECORD                     
         OI    IOKEY+13,X'01'      AND IN KEY FOR PACKED CML                    
*                                                                               
         CLI   RECACT,C'X'                                                      
         JE    CMLU117                                                          
         CLI   RECACT,C'R'                                                      
         JE    CMLU117                                                          
         CLI   RECACT,C'C'                                                      
         JNE   CMLU120                                                          
*                                                                               
CMLU117  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+#CMLREC'                    
         JNE   CLTIODIE                                                         
*                                                                               
         MVC   SVDSKADR,IODA       SAVE DISK ADDR OF CHANGED REC                
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,ACH               AFTER PUT CHK ACTUAL CML REWRITE            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         J     CMLU130                                                          
*                                                                               
CMLU120  MVC   SVIOKEY,IOKEY       SAVE KEY BEFORE ADDREC                       
*                                                                               
         BRAS  RE,PAREC            PRIOR TO ADD RECORD GET SEQ#                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOTRFFIL+#CMLREC'                    
         JNE   CLTIODIE                                                         
*                                                                               
         BRAS  RE,AAREC            AFTER ADD UPDATE SEQ# ADD PASSIVE            
*                                                                               
CMLU130  CLI   RECACT,C'X'         DELETE                                       
         JNE   CMLUX                                                            
         OC    NXTCML,NXTCML       SAVE ADDRESS OF NEXT CML                     
         JZ    CMLUX                                                            
         L     R2,NXTCML           ADDRESS OF NEXT CML TO PROCESS               
         LA    RF,QCMLLSTQ                                                      
         CR    R2,RF               END OF LIST?                                 
         JNL   CMLUX                                                            
*                                                                               
         LA    RF,12(R2)                                                        
         ST    RF,NXTCML           SAVE ADDRESS OF NEXT CML                     
*                                                                               
         CLC   0(L'QCOMML,R2),SPACES Any more cmls?                             
         JH    CMLU004                                                          
*                                                                               
CMLUX    J     EXITY                                                            
CMLUXN   J     EXITN                                                            
*                                                                               
CLTIODIE DC    H'0'                                                             
*                                                                               
         GETEL R4,24,ELCODE                                                     
*                                                                               
*---------------------------------------------------                            
* MOVE ELEMENT TO ELEM                                                          
*---------------------------------------------------                            
MVELEM   XC    ELEM,ELEM                                                        
         LLC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         MVC   ELEM(0),0(R4)                                                    
         EX    R1,0(RF)                                                         
         BR    RE                                                               
*---------------------------------------------------                            
* FIND INPUT LEN-1                                                              
* (ON ENTRY RF=FIELD, RE=MAX FIELD LEN                                          
*---------------------------------------------------                            
*                                                                               
FLEN     AR    RE,RF               POINT TO THE END OF FIELD                    
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         SR    RE,RF               LEN -1                                       
         BR    R1                                                               
*                                                                               
*                                                                               
*---------------------------------------------------                            
* VALIDATE RECORD CHECK SUM                                                     
*---------------------------------------------------                            
VCKSUM   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACMLREC          COMMERCIAL RECORD                            
         SR    RF,RF                                                            
         ICM   RF,3,CMLRLEN-CMLRECD(RE)                                         
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,FULL                                                       
*                                                                               
         CLC   FULL,QCKSM                                                       
         JNE   ERR00814                                                         
*                                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
* GET PROFILES                                                                  
***********************************************************************         
*                                                                               
GPROF    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,CPROFTAB         PRESET TO COMMERCIAL PROFILES                
         CLC   LP_QMAPN,=AL2(I#STCMLU)                                          
         JE    GPROF01                                                          
         LA    R4,PPROFTAB         PRESET TO PATTERN PROFILES                   
         CLC   LP_QMAPN,=AL2(I#STPATU)                                          
         JE    GPROF01                                                          
         DC    H'0'                BUG CATCHER                                  
*                                                                               
GPROF01  XC    FULL,FULL           INIT OUTPUT PROFILE NAME                     
         XC    WORK,WORK           INIT OUTPUT PROFILE VALUES                   
*                                                                               
         MVC   FULL(4),0(R4)       SET OUTPUT PROFILE NAME                      
         XC    TEMP,TEMP                                                        
         MVC   TEMP+00(2),=C'S0'                                                
         MVC   TEMP+02(2),0(R4)                                                 
         MVC   TEMP+04(2),LP_AGY                                                
         MVC   TEMP+06(L'QMEDA),QMEDA                                           
         MVC   TEMP+07(L'QCLTA),QCLTA                                           
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(L'SVCLTOFF),SVCLTOFF                                     
         GOTOR VGETPROF,DMCB,(FULL+3,TEMP),WORK,VDATAMGR                        
*                                                                               
         CLC   =C'T0',0(R4)                                                     
         JNE   GPROF10                                                          
         MVC   SVPROF14,WORK+13    COMCLASS                                     
         MVC   SVPROF10,WORK+9     AUTO T/A                                     
         MVC   SVPROF11,WORK+10    EST/DAYP/NONE                                
         J     GPROF30                                                          
*                                                                               
GPROF10  CLC   =C'TT',0(R4)                                                     
         JNE   GPROF20                                                          
         MVC   SVTTPR3,WORK+3      CLARUS                                       
         J     GPROF30                                                          
*                                                                               
GPROF20  CLC   =C'T2',0(R4)                                                     
         JNE   GPROF25                                                          
         MVC   SVT2PR3,WORK+2      ALLOW NON-STD %                              
         MVC   SVT2PR8,WORK+7      LIMIT 1 BRAND PER CML                        
         J     GPROF30                                                          
*                                                                               
GPROF25  CLC   =C'T1',0(R4)                                                     
         JE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         MVC   SVT1PR15,WORK+14    UFN IS INVALID                               
*                                                                               
GPROF30  LA    R4,PROFTABL(R4)     NEXT ENTRY IN TABLE                          
         CLC   0(2,R4),=X'0000'                                                 
         JNE   GPROF01                                                          
*                                                                               
         J     EXITY                                                            
*                                                                               
* COMMERCIAL PROFILES TABLE                                                     
*                                                                               
CPROFTAB DC    C'T0 ',X'C0'                                                     
PROFTABT DC    C'TT ',X'C0'                                                     
         DC    C'T2 ',X'C0'                                                     
         DC    X'0000'                                                          
PROFTABL EQU   PROFTABT-CPROFTAB                                                
*                                                                               
*                                                                               
* PATTERN PROFILES TABLE                                                        
*                                                                               
PPROFTAB DC    C'T0 ',X'C0'                                                     
         DC    C'T1 ',X'C0'                                                     
         DC    C'T2 ',X'C0'                                                     
         DC    X'0000'                                                          
*                                                                               
*                                                                               
*                                                                               
REMEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R7)),0,0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
*                                                                               
ADDEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'P',SYSFIL),(R7),ELEM,0                            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
*                                                                               
*---------------------------------------------------------                      
* GET HIDEF COMMERCIAL FROM EXISTING RECORD                                     
*---------------------------------------------------------                      
*                                                                               
GETHIDEF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO8                                                          
         MVI   ELCODE,X'24'        EXTENDED DATA ELEMENT                        
         BRAS  RE,GETEL                                                         
         JNE   EXITN                                                            
*                                                                               
         USING CMLXDTEL,R4                                                      
         OC    CMLXHDEF,CMLXHDEF   ANY HIDEF  ?                                 
         JZ    GETHI10                                                          
*                                                                               
         MVC   SVHIDEFX,CMLXHDPK   SAVE HEX HIDEF                               
*                                                                               
GETHI10  CLC   SVHIDEF,CMLXHDEF    SAME HIDEF CML?                              
         JE    EXITY                                                            
         JNE   EXITN                                                            
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE DATA IN COMMERCIAL FIELD AT 0(R2)                                    
* FOR NOW, 8 CHARS=ISCI, 9-12=ADID                                              
*============================================================                   
*                                                                               
VALCMML  NTR1  BASE=*,LABEL=*                                                   
         CLI   QCMLPRNT+8,X'40'                                                 
         JH    VALC10                                                           
*                                                                               
         MVC   DUB,QCMLPRNT        PRESUMING IT'S GOOD                          
         LA    R0,8                                                             
         LA    R1,QCMLPRNT                                                      
*                                                                               
VALC2    CLI   0(R1),C'A'                                                       
         JL    ERR00577                                                         
         LA    R1,1(,R1)                                                        
         JCT   R0,VALC2                                                         
         J     VALC20                                                           
*                                                                               
VALC10   DS   0H                                                                
         LA    RF,QCMLPRNT                                                      
         LA    RE,L'QCMLPRNT-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                                                             
         STC   RE,BYTE                                                          
         LA    R1,QCMLPRNT                                                      
*                                                                               
VALC14   CLI   0(R1),C'A'                                                       
         JL    ERR00783                                                         
         CLI   0(R1),C'Z'                                                       
         JNH   VALC16                                                           
*                                                                               
         CLI   0(R1),C'0'                                                       
         JL    ERR00783                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR00783                                                         
*                                                                               
VALC16   LA    R1,1(R1)                                                         
         JCT   RE,VALC14                                                        
*                                                                               
         OC    QCMLPRNT,SPACES                                                  
         GOTOR VTRPACK,DMCB,(C'P',QCMLPRNT),DUB                                 
         JNE   ERR00783                                                         
*                                                                               
VALC20   CLC   DUB,SVKEY+CMLKCML-CMLKEY   TEST SAME AS THIS CMML                
         JE    EXIT                                                             
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0A21'                                                
         MVC   IOKEY+2(3),SVBAGM     A-M/CLT                                    
         MVC   IOKEY+5(8),DUB                                                   
*                                                                               
         CLI   BYTE,8             TEST INPUT IS ADID                            
         JNH   *+10                                                             
         MVC   IOKEY(2),=X'0AC1'                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00053                                                         
*                                                                               
* NEED TO CHECK FOR SOFT DELETE                                                 
*                                                                               
VALC30   DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
*                                                                               
         L     RE,AIO7                                                          
         TM    CMLSTAT-CMLRECD(RE),X'80'   TEST SOFT DELETED                    
         JO    ERR00535                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*=======================================================                        
* VALIDATE PRODUCT LIST AND BUILD PROD LIST ELEMENT                             
*=======================================================                        
*                                                                               
VPRDL    NTR1  BASE=*,LABEL=*                                                   
         OC    QPRDLIST,QPRDLIST   ANY ENTRY                                    
         JZ    ERR00260             NO                                          
*                                                                               
         LA    R4,ELEM2                                                         
         USING CMLPRDEL,R4                                                      
         MVI   CMLPRDEL,X'20'      ELEM CODE                                    
         CLC   =C'PRD=ALL',QPRDLIST                                             
         JNE   VPRDL08                                                          
*                                                                               
         CLI   SVT2PR8,C'Y'         LIMIT ONE PRD PER ISCII                     
         JE    ERR00749             YES ERROR                                   
*                                                                               
         MVI   CMLPRDLN,3                                                       
         MVI   CMLPRDS,X'FF'                                                    
         J     VPRDL20                                                          
*                                                                               
VPRDL08  MVI   CMLPRDLN,2          ELEM LENGTH EMPTY                            
         XC    TEMP1,TEMP1         INIT PRODUCT LIST AREA (IN)                  
         LA    R2,QPRDLIST                                                      
*                                                                               
         LA    RF,L'QPRDLIST-1(R2) Get input length                             
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R2                                                            
         LA    RF,1(RF)            ACTUAL INPUT LEN                             
         STC   RF,TEMP1+5          FAKE HEADER FIELD                            
*                                                                               
         MVC   TEMP1+8(L'QPRDLIST),QPRDLIST                                     
*                                                                               
         CLI   TEMP1+5,4           SINGLE PROD                                  
         JNL   VPRDL09                                                          
*                                                                               
* SINGLE PROD REQUEST                                                           
         LA    R5,ELEM2+2                                                       
         CLC   =C'POL',QPRDLIST    THIS IS ILLEGAL                              
         JE    ERR00102                                                         
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,QPRDLIST,TEMP+5,QPRDX                     
         JNE   ERR00786            BAD PRODUCT                                  
*                                                                               
         MVC   0(1,R5),QPRDX       MOVE BINARY PRODUCT CODE TO ELEM             
         MVC   QPRDA,QPRDLIST      SAVE 3 CHAR PROD CODE                        
         IC    R1,ELEM2+1          GET ELEM LENGTH                              
         LA    R1,1(,R1)                                                        
         STC   R1,ELEM2+1                                                       
*                                                                               
*Per Ramune disable for now                                                     
*        CLI   SVTTPR3,C'Y'        IF SPOTTAL USER,                             
*        JNE   *+8                                                              
******   BRAS  RE,GPROD            SEE IF PRODUCT HAS TALENT AGENCY             
         J     VPRDL20                                                          
*                                                                               
VPRDL09  GOTOR VSCANNER,DMCB,TEMP1,AIO6                                         
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3                                                            
         JZ    ERR00260            MISSING PRODUCT                              
*                                                                               
         CLI   DMCB+4,1                                                         
         JE    *+12                                                             
         CLI   SVT2PR8,C'Y'        LIMIT ONE PRD PER ISCII                      
         JE    ERR00749            YES ERROR                                    
*                                                                               
         L     R4,AIO6             ADDRESS OF FIRST BLOCK                       
         LA    R5,ELEM2+2          SAVE PRODUCT CODE HERE                       
*                                                                               
VPRDL10  CLC   12(3,R4),=C'POL'    THIS IS ILLEGAL                              
         JE    ERR00102                                                         
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,12(R4),1(R4),QPRDX                        
         JNE   ERR00786            Bad product                                  
*                                                                               
         LA    RF,ELEM2+2                                                       
VPRDL14  CR    R5,RF               THIS FIRST/CURR ENTRY                        
         JE    VPRDL16             YES, NO DUPES                                
         CLC   0(1,RF),QPRDX       THIS A DUPE                                  
         JE    ERR00582                                                         
         LA    RF,1(,RF)                                                        
         J     VPRDL14                                                          
*                                                                               
VPRDL16  MVC   0(1,R5),QPRDX       MOVE BINARY PRODUCT CODE TO ELEM             
         MVC   QPRDA,12(R4)        SAVE 3 CHAR PROD CODE                        
         LA    R4,32(,R4)          NEXT SCANNER BLOCK                           
         LA    R5,1(,R5)           BUMP ELEMENT POINTER                         
         IC    R1,ELEM2+1          GET ELEM LENGTH                              
         LA    R1,1(,R1)                                                        
         STC   R1,ELEM2+1                                                       
*                                                                               
*Per Ramune disable for now                                                     
*        CLI   SVTTPR3,C'Y'        IF SPOTTAL USER,                             
*        JNE   *+8                                                              
*******  BRAS  RE,GPROD            SEE IF PRODUCT HAS TALENT AGENCY             
*                                                                               
         JCT   R3,VPRDL10                                                       
*                                                                               
         CLI   CMLPRDLN,2          WERE ANY PROD CODES FOUND                    
         JE    ERR00260            NO                                           
*                                                                               
VPRDL20  DS    0H                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    VPRDLX                                                           
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'        FIND ORIGINAL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,ELEM2                                                         
         SR    RF,RF                                                            
         IC    RF,ELEM2+1                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   ELEM2(0),0(R4)      COMPARE NEW/OLD PRD ELEMS                    
         EX    RF,0(R1)                                                         
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_PRDL                                              
*                                                                               
VPRDLX   MVC   ELEM,ELEM2                                                       
         J     EXIT                                                             
*                                                                               
*                                                                               
*=======================================================                        
* BUILD LIST OF COMMERCIALS TO DELETE                                           
*=======================================================                        
*                                                                               
BCMLLST  NTR1  BASE=*,LABEL=*                                                   
         OC    QCMLLST,QCMLLST     ANY ENTRY                                    
         JZ    EXITN                NO                                          
*                                                                               
         XC    ELEM1,ELEM1         INIT COMMERCIAL LIST AREA (IN)               
         XC    ELEM2,ELEM2         INIT COMEERCIAL LIST AREA (OUT)              
         LA    R2,QCMLLST                                                       
*                                                                               
         LA    RF,L'QCMLLST-1(R2)  GET INPUT LENGTH                             
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R2                                                            
         LA    RF,1(RF)            ACTUAL INPUT LEN                             
         STC   RF,ELEM1+5          FAKE HEADER FIELD                            
*                                                                               
         MVC   ELEM1+8(L'QCMLLST),QCMLLST                                       
*                                                                               
         CLI   ELEM1+5,12          SINGLE COMML                                 
         JNL   BCMLL09                                                          
*                                                                               
* Single cml request                                                            
         MVC   ELEM2(12),ELEM1+8   MOVE IN CML                                  
         MVI   ERRFLG,0                                                         
         LA    R2,ELEM2                                                         
         BRAS  RE,VCML                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         J     BCMLLX                                                           
*                                                                               
BCMLL09  GOTOR VSCANNER,DMCB,ELEM1,AIO6                                         
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3                                                            
         JZ    ERR00260            MISSING CMLS                                 
*                                                                               
         L     R4,AIO6             ADDRESS OF FIRST BLOCK                       
         LA    R5,ELEM2            SAVE CML CODE HERE                           
*                                                                               
BCMLL10  LA    RF,ELEM2                                                         
BCMLL14  CR    R5,RF               THIS FIRST/CURR ENTRY                        
         JE    BCMLL16             YES, NO DUPES                                
         CLC   0(12,RF),12(R4)     DUPE CML?                                    
         JE    ERR00582                                                         
         LA    RF,12(,RF)                                                       
         J     BCMLL14                                                          
*                                                                               
BCMLL16  MVC   0(12,R5),12(R4)     MOVE COMMERCIAL TO SAVE LIST AREA            
         LA    R4,32(,R4)          NEXT SCANNER BLOCK                           
         LA    R5,12(,R5)          BUMP IN CML LIST AREA                        
         JCT   R3,BCMLL10                                                       
*                                                                               
BCMLLX   J     EXIT                                                             
*                                                                               
*                                                                               
*===========================================================                    
* VALIDATE COMMERCIAL LENGTH                                                    
*===========================================================                    
*                                                                               
VALISLN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               POINT TO END OF TABLE                        
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMEDA,C'T'                                                       
         JE    VSLN10                                                           
         CLI   QMEDA,C'N'                                                       
         JE    VSLN10                                                           
         CLI   QMEDA,C'C'                                                       
         JE    VSLN10                                                           
*                                                                               
         LA    R0,C'R'                                                          
         CLI   QMEDA,C'R'                                                       
         JE    VSLN10                                                           
         CLI   QMEDA,C'X'                                                       
         JE    VSLN10                                                           
         DC    H'0'                                                             
*                                                                               
VSLN10   CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         JE    VSLN20                                                           
         CLC   LP_AGY,0(R1)        ELSE MATCH AGY                               
         JNE   *+12                                                             
VSLN20   CLM   R0,1,2(R1)          AND MEDIA                                    
         JE    VSLN30                                                           
*                                                                               
         BXLE  R1,RE,VSLN10        NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VSLN30   AHI   R1,4                POINT BEYOND HEADER                          
         SR    RE,RE                                                            
         IC    RE,SVSLN            GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         JNE   EXIT                                                             
         J     ERR00673            BAD LEN                                      
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE TITLE FIELDS - NOTE FIRST FIELD IS ONLY 15 BYTES                     
* NOTE THAT ON ENTRY R4 POINTS TO X'10' ELEMENT IN RECORD                       
*============================================================                   
*                                                                               
         USING CMLDTAEL,R4                                                      
VALTTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK+20(20),QCMLDSC2                                             
         OC    WORK+20(20),SPACES                                               
         MVC   WORK+40(20),QCMLDSC3                                             
         OC    WORK+40(20),SPACES                                               
*                                                                               
         OC    QCMLDSC2,QCMLDSC2   ANY TITLE 2 ENTERED                          
         JNZ   VALTTL2             YES                                          
         OC    QCMLDSC3,QCMLDSC3   IF NOT, SHOULD BE NO  TITLE 3                
         JNZ   ERR00537                                                         
*                                                                               
VALTTL2  CLC   CMLTITLE,QCMLTTLE   UPDATE TITLE IN CMLDTAEL                     
         JE    *+8                 BUT DO NOT INSERT IN RECORD YET              
         OI    CHGFLAG1,CMLCH_DESC                                              
         MVC   CMLTITLE,QCMLTTLE                                                
*                                                                               
         L     R4,ACMLREC          TEST FOR CHANGES IT TITLE2/3                 
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VALTTL10                                                         
         OC    3(24,R4),SPACES                                                  
         CLC   WORK+20(20),3(R4)                                                
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   VALTTL05                                                         
         OC    3(24,R4),SPACES                                                  
         CLC   WORK+40(20),3(R4)                                                
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
*                                                                               
VALTTL05 MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,REMEL            DELETE ELEMENT (X'30')                       
*                                                                               
VALTTL10 XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CMLDSCEL,R4                                                      
*                                                                               
         MVI   ELEM,X'30'          BUILD NEW X'30' ELEMS AND INSERT             
         MVI   ELEM+1,27                                                        
         MVI   ELEM+2,1            SEQUENCE NUMBER                              
         MVC   ELEM+3(L'QCMLDSC2),WORK+20                                       
         OC    ELEM+3(L'QCMLDSC2+4),SPACES                                      
*                                                                               
         LLC   R1,CMLDSCLN         ELEM LEN                                     
         SHI   R1,4                ADJ FOR EX (ELCODE/LEN/LIN#)                 
         BASR  RF,0                                                             
         CLC   ELEM+3(0),SPACES                                                 
         EX    R1,0(RF)                                                         
         JNH   EXIT                DO NOT ADD AN EMPTY ELEMENTS                 
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         MVI   ELEM+2,2                                                         
         MVC   ELEM+3(L'QCMLDSC3),WORK+40                                       
         OC    ELEM+3(L'QCMLDSC3+4),SPACES                                      
*                                                                               
         LLC   R1,CMLDSCLN         ELEM LEN                                     
         SHI   R1,4                ADJ FOR EX (ELCODE/LEN/LIN#)                 
*                                                                               
         BASR  RF,0                                                             
         CLC   ELEM+3(0),SPACES                                                 
         EX    R1,0(RF)                                                         
         JNH   EXIT                DO NOT ADD AN EMPTY ELEMENTS                 
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*=================================================                              
* GET PRODUCT RECORD                                                            
*=================================================                              
*                                                                               
GPROD    NTR1  BASE=*,LABEL=*                                                   
         MVC   MYKEY2,IOKEY                                                     
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+PKEYAM-PKEY(3),SVBAGM                                      
         MVC   IOKEY+PKEYPRD-PKEY(3),QPRDA                                      
         MVC   IOKEYSAV(13),IOKEY                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO7                                                          
         USING PRDRECD,R4                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
*                                                                               
         CLC   LP_QMAPN,=AL2(I#STCMLU) CML RECORD                               
         JNE   GPROD10                                                          
         OC    PTALAGY,PTALAGY     CHECK TALENT AGENCY                          
         JZ    ERR00744                                                         
         CLC   PTALAGY,SPACES                                                   
         JE    ERR00744                                                         
         J     GPROD40                                                          
*                                                                               
GPROD10  CLC   LP_QMAPN,=AL2(I#STPATU) PATTERN RECORD                           
         JE    *+6                                                              
         DC    H'0'                NOT PAT OR CML ???                           
*                                                                               
         TM    POPT1,POPT1_THTR    CHECK THEATRICAL                             
         JZ    *+8                                                              
         OI    FLAGS2,PRDTHTR                                                   
*                                                                               
GPROD40  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'MYKEY2),MYKEY2                                           
         J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE COMMERCIAL CLASS                                                     
*======================================================================         
*                                                                               
VCLASS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM2,ELEM2                                                      
*                                                                               
         LA    R3,ELEM2+200        USE AS SAVE AREA                             
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VCLS4                                                            
         USING CMLCLSEL,R4                                                      
*                                                                               
VCLS2    MVC   0(6,R3),CMLCLS      MOVE CLASS AND PERCENTAGE                    
         LA    R3,6(R3)                                                         
         BRAS  RE,NEXTEL                                                        
         JE    VCLS2                                                            
         DROP  R4                                                               
*                                                                               
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
VCLS4    OC    QCMLCLAS,QCMLCLAS                                                
         JNZ   VCLS10                                                           
         CLI   SVPROF14,C'Y'       IS COMMERCIAL CLASS REQUIRED                 
         JE    ERR00579                                                         
         CLI   SVPROF14,C'V'       IS COMML CLASS REQUIRED & VALIDATED          
         JE    ERR00579                                                         
         J     VCLSX                                                            
*                                                                               
VCLS10   LA    R4,ELEM2                                                         
         XC    CMLPRCNT,CMLPRCNT                                                
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
*                                                                               
         LA    RF,QCMLCLAS                                                      
         LA    RE,L'QCMLCLAS-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN-1                             
*                                                                               
         BASR  R1,0                                                             
         MVC   FAKEFLD(0),QCMLCLAS                                              
         EX    RE,0(R1)                                                         
*                                                                               
         AHI   RE,1                                                             
         STC   RE,FAKEFLDH+5       SET INPUT LENGTH IN OUR FAKE FLDH            
*                                                                               
         LA    R2,FAKEFLDH                                                      
         GOTOR VSCANNER,DMCB,(R2),(4,ELEM2),0                                   
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
         JZ    ERR00579            NONE                                         
         LR    R5,R3                                                            
*                                                                               
VCLS12   CLI   0(R4),4                                                          
         JH    ERR00579                                                         
*                                                                               
         CLI   SVPROF14,C'V'       IS COMMERCIAL CLASS VALIDATED                
         JNE   VCLS14                                                           
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,RDCLS             GO READ FOR CLASS                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VCLS14   CLI   1(R4),0                                                          
         JNE   VCLS16                                                           
         CHI   R5,1                                                             
         JE    VCLS30                                                           
         J     ERR00579             NO                                          
*                                                                               
VCLS16   LLC   R0,1(R4)                                                         
         GOTOR VCASHVAL,DMCB,(2,22(R4)),(R0),0                                  
         CLI   DMCB,X'FF'                                                       
         JE    ERR00735                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'21'                                                       
         MVI   ELEM+1,8                                                         
         MVC   ELEM+2(4),12(R4)                                                 
         L     RE,DMCB+4                                                        
         STCM  RE,3,ELEM+6                                                      
         AH    RE,CMLPRCNT                                                      
         STH   RE,CMLPRCNT                                                      
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         LA    R1,ELEM2+200        SEE IF THIS DATA IN SAVE AREA                
         LA    R0,4                                                             
*                                                                               
VCLS20   CLC   ELEM+2(6),0(R1)                                                  
         JE    VCLS30                                                           
         LA    R1,6(R1)                                                         
         JCT   R0,VCLS20                                                        
*                                                                               
         OI    CHGFLAG1,CMLCH_CLS  IF NOT THERE BEFORE, SET CHANGED             
*                                                                               
VCLS30   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         JCT   R3,VCLS12           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         CLC   CMLPRCNT,=H'10000'                                               
         JE    VCLSX                                                            
         CHI   R5,1                                                             
         JH    ERR00735                                                         
         OC    CMLPRCNT,CMLPRCNT                                                
         JNZ   VCLSX                                                            
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CMLCLASS-CMLDTAEL(4,R4),8(R2)                                    
*                                                                               
VCLSX    J     EXIT                                                             
*                                                                               
*============================================================                   
* SEE IF A CLASS RECORD EXISTS FOR SOME PRD OR THE CLIENT                       
* ON ENTRY R4 POINTS TO SCANNER BLOCK                                           
*============================================================                   
*                                                                               
RDCLS    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,IOKEY                                                      
         XC    IOKEY,IOKEY                                                      
         LR    R3,R4                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING CLSKEY,R2                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,SVBAGM                                                    
         MVC   CLSKCLAS,12(R3)     CLASS FROM SCANNER BLOCK                     
         OC    CLSKCLAS,SPACES                                                  
         MVC   CLSKCLT,SVBCLT                                                   
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'        GET PRODUCT ELEM                             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R4),X'FF'         THIS ALL PRODS                               
         JE    RDCLS30                                                          
*                                                                               
         LLC   R3,1(R4)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         LA    R5,2(,R4)                                                        
*                                                                               
RDCLS10  GOTOR (#EDTPRD,AEDTPRD),DMCB,0(R5),,SVQPRD  GET 3 CHAR PRD             
*                                                                               
RDCLS24  MVC   CLSKPROD,SVQPRD                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    RDCLS40                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R5,1(,R5)           POINT TO NEXT PRODUCT IN LIST                
         JCT   R3,RDCLS10                                                       
*                                                                               
         XC    CLSKPROD,CLSKPROD                                                
*                                                                               
RDCLS30  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    RDCLS40                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         XC    CLSKCLT,CLSKCLT                                                  
         DROP  R2                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00053            NOT FOUND                                    
*                                                                               
RDCLS40  MVC   IOKEY,SVKEY                                                      
*                                                                               
RDCLSX   J     EXIT                                                             
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE PRINT OVERRIDE                                                       
*============================================================                   
*                                                                               
         USING CMLDTAEL,R4                                                      
VOVRD    NTR1  BASE=*,LABEL=*                                                   
         XC    CMLOVRD1(2),CMLOVRD1                                             
*                                                                               
         OC    QCMLOVRD,QCMLOVRD   ANY INPUT?                                   
         JZ    VOVRDX               NO                                          
*                                                                               
         LA    R2,FAKEFLDH                                                      
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
*                                                                               
         LA    RF,QCMLOVRD                                                      
         LA    RE,L'QCMLOVRD-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN-1                             
*                                                                               
         BASR  R1,0                                                             
         MVC   FAKEFLD(0),QCMLOVRD                                              
         EX    RE,0(R1)                                                         
*                                                                               
         AHI   RE,1                                                             
         STC   RE,FAKEFLDH+5       SET INPUT LENGTH IN OUR FAKE FLDH            
*                                                                               
         GOTOR VSCANNER,DMCB,(R2),(2,ELEM2),C',=/='                             
         CLI   DMCB+4,2            MUST BE 2 BLOCKS                             
         JNE   ERR00574                                                         
*                                                                               
         LA    R1,ELEM2                                                         
         TM    2(R1),X'80'                                                      
         JZ    ERR00003            MUST BE NUMERIC                              
         L     RE,4(R1)                                                         
         STC   RE,CMLOVRD1                                                      
*                                                                               
         LA    R1,32(R1)           NEXT SCANNER BLOCK                           
         TM    2(R1),X'80'                                                      
         JZ    ERR00003            MUST BE NUMERIC                              
         L     RF,4(R1)                                                         
         STC   RF,CMLOVRD2                                                      
*                                                                               
* MAKE SURE (OVRD1 + OVRD2 = SLN)                                               
*                                                                               
         AR    RE,RF                                                            
         IC    RF,CMLSLN                                                        
         CR    RE,RF                                                            
         JNE   ERR00575                                                         
*                                                                               
VOVRDX   J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*NOP                                                                            
*========================================================                       
* MAKE SURE DATE CHANGE KEEPS SUBST ELEMS IN CML PERIOD                         
* WORK HAS CMLRLSE/CMLRCL                                                       
*========================================================                       
*                                                                               
*HKPRSOV NTR1  BASE=*,LABEL=*                                                   
*        L     R4,ACMLREC                                                       
*        MVI   ELCODE,X'B1'                                                     
*        BRAS  RE,GETEL                                                         
*        JNE   CHKPRSX                                                          
*                                                                               
*HKPRS02 CLC   CMLPRSDT-CMLPRSEL(3,R4),WORK     DATE PRIOR TO RELEASE           
*        JL    ERR00800                                                         
*        CLC   CMLPREDT-CMLPRSEL(3,R4),WORK+3   OR AFTER RECALL                 
*        JH    ERR00800                                                         
*        BRAS  RE,NEXTEL                                                        
*        JE    CHKPRS02                                                         
*                                                                               
*HKPRSX  J     EXIT                                                             
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
* MAKE SURE THIS ADID DOES NOT EXIST AS ADID OR HIDEF ON ANOTHER REC            
*---------------------------------------------------------------------          
*                                                                               
DUPCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0AC1'                                                
         MVC   IOKEY+2(3),SVBAGM                                                
         MVC   IOKEY+5(8),HOLDCML  PACKED CML                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00049                                                         
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C2'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00049                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*--------------------------------------------------------------                 
* FOR ACTION ADD, ADD AD-ID ELEMENT FOR PACKED CML                              
* SO ALWAYS HAVE PACKED AND UNPACKED ADID IN A0 ELEMENT FOR KATZ                
*--------------------------------------------------------------                 
*                                                                               
AADIDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'A0'        AD-ID  ELEM                                  
         LA    R4,ELEM                                                          
         USING CMLADIEL,R4                                                      
*                                                                               
         LA    R4,ELEM                                                          
         USING CMLADIEL,R4                                                      
         MVI   CMLADIEL,X'A0'                                                   
         MVI   CMLADILN,CMLADIDL-CMLADIEL                                       
         MVC   CMLADID,QCOMML                                                   
         OC    CMLADID,SPACES                                                   
*                                                                               
         GOTOR VTRPACK,DMCB,(C'P',CMLADID),CMLADIDP                             
         MVC   CMLADIDT,BTODAY        SAVE DATE                                 
*                                                                               
         TIME  DEC                                                              
         STCM  R0,15,CMLADITM      SAVE TIME                                    
         DROP  R4                                                               
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
VALMAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLAGS2,X'FF'-MATELEM                                             
         XC    FAKEFLD,FAKEFLD                                                  
         L     R4,ACMLREC                                                       
         MVI   ELCODE,CMLMATEQ     X'B0'                                        
         BRAS  RE,GETEL                                                         
         JNE   VALMAT05                                                         
*                                                                               
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
         MVC   FAKEFLD,ELEM        SAVE PREVIOUS ELEMENT                        
*                                                                               
*===================================================================            
* NOTE, THAT THIS LOOP RELIES ON 6 EQUAL LENGTH PERIOD FIELDS                   
* TAKE GREAT CARE WHEN CHANGING THE SCREEN, PLEASE                              
*===================================================================            
*                                                                               
VALMAT05 LA    R2,QCMLMDTE1        MATCH DATES                                  
         LA    R0,6                                                             
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CMLMATCH,R4                                                      
         MVI   CMLMATEL,CMLMATEQ                                                
         MVI   CMLMATLN,CMLMATDL                                                
         LA    R5,CMLMPER1                                                      
*                                                                               
* PERIOD VALIDATION LOOP                                                        
*                                                                               
VALMAT10 CLI   0(R2),0             ANY ENTRY                                    
         JE    VALMAT50            NO, GO TO NEXT FIELD                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         MVI   BYTE,17             INPUT LEN                                    
         LA    R3,0(R2)                                                         
         ICM   R3,8,BYTE                                                        
         GOTOR (RF),DMCB,(R3),PERVALST                                          
         CLI   DMCB+4,0                                                         
         JNE   ERR00746                                                         
*                                                                               
         MVC   0(4,R5),PERVALST+PVALCSTA-PERVALD                                
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT50 DS    0H                                                               
         LA    R2,L'QCMLMDTE1(R2)                                               
         LA    R5,L'CMLMPER1(R5)                                                
         JCT   R0,VALMAT10                                                      
*                                                                               
         XC    CMLMSTIM,CMLMSTIM                                                
         MVC   CMLMETIM,=X'FFFF'                                                
*                                                                               
         OC    QCMLMSTM,QCMLMSTM                                                
         JZ    VALMAT60                                                         
*                                                                               
         LA    RF,QCMLMSTM                                                      
         LA    RE,L'QCMLMSTM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
*                                                                               
         LA    R5,QCMLMSTM                                                      
         ICM   R5,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R5),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747            INVALID TIME                                 
         MVC   CMLMSTIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT60 DS    0H                                                               
         OC    QCMLMETM,QCMLMETM Any input                                      
         JZ    VALMAT70                                                         
*                                                                               
         LA    RF,QCMLMETM                                                      
         LA    RE,L'QCMLMETM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
*                                                                               
         LA    R5,QCMLMETM                                                      
         ICM   R5,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R5),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747                                                         
         MVC   CMLMETIM,WORK                                                    
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT70 DS    0H                                                               
         OC    QCMLMSTM,QCMLMSTM IF TIME ENTERED                                
         JNZ   VALMAT72            MUST INPUT DAILY FLAG                        
         CLC   QCMLMETM,=X'FFFF'                                                
         JE    VALMAT80                                                         
*                                                                               
VALMAT72 CLI   QDLYFLG,C'Y'                                                     
         JNE   VALMAT80                                                         
         OI    CMLMFLAG,CMLMFDAY                                                
         OI    FLAGS2,MATELEM                                                   
*                                                                               
VALMAT80 DS    0H                                                               
         TM    FLAGS2,MATELEM                                                   
         JNZ   VALMAT85                                                         
         OC    FAKEFLD,FAKEFLD                                                  
         JZ    VALMATX                                                          
*                                                                               
VALMAT85 DS    0H                                                               
E        USING CMLMATEL,FAKEFLD                                                 
         USING CMLMATEL,R4                                                      
         OC    E.CMLMETIM,E.CMLMETIM                                            
         JNZ   *+10                                                             
         MVC   E.CMLMETIM,=X'FFFF'                                              
*                                                                               
         CLC   E.CMLMSTIM(4),CMLMSTIM                                           
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_TIME                                              
*                                                                               
         CLC   E.CMLMFLAG,CMLMFLAG                                              
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_DLY                                               
*                                                                               
         CLC   E.CMLMPER1(24),CMLMPER1                                          
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_MDTS                                              
*                                                                               
         OI    FLAGS2,MATELEM                                                   
         JZ    VALMATX                                                          
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
VALMATX  J     EXIT                                                             
         DROP  E,R4                                                             
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF FIELD ABOUT TO BE ADDED                                
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
*----------------------------------------------------------------*              
HDDUPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=XL2'0AC1'                                              
         MVC   IOKEY+2(3),SVBAGM                                                
         MVC   IOKEY+5(8),WORK+12                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'                           
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    HDDXIT                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C2'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'                           
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    HDDXIT                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C3'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'                           
*                                                                               
HDDXIT   J     EXIT                                                             
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS AND INSERT ELEMENTS IN RECORD                     
*======================================================================         
*                                                                               
VALACT   NTR1  BASE=*,LABEL=*                                                   
         XC    OLDACTS(L'OLDACTS*NUMACTS),OLDACTS                               
         XC    NEWACTS(L'NEWACTS*NUMACTS),NEWACTS                               
         XC    ACTSLN,ACTSLN                                                    
*                                                                               
* SAVE OFF OLD ACTUALS                                                          
*                                                                               
         LA    R0,NUMACTS                                                       
         LA    R1,OLDACTS                                                       
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         USING CMLACTEL,R4                                                      
         BRAS  RE,GETEL                                                         
         JNE   VACT5                                                            
         J     VACT3                                                            
*                                                                               
VACT2    BRAS  RE,NEXTEL                                                        
         JNE   VACT4                                                            
VACT3    MVC   0(L'CMLACTID,R1),CMLACTID  8 CHAR CML                            
         CLI   CMLACTLN,CMLACTL1          OLD ELEM                              
         JE    *+10                                                             
         MVC   0(L'CMLACTCM,R1),CMLACTCM   NO, MOVE IN 12 CHAR CML              
         LA    R1,L'OLDACTS(R1)                                                 
         JCT   R0,VACT2                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
VACT4    DS    0H                                                               
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
* VALIDATE ID'S ON SCREEN & BUILD TABLE OF NEW ACTUAL ID'S                      
*                                                                               
VACT5    XC    ACTCT,ACTCT         COUNT OF ACTUAL CMMLS                        
         OC    QCMLACT1,QCMLACT1                                                
         JNZ   VACT5C                                                           
         OC    QCMLACT2,QCMLACT2                                                
         JNZ   VACT5C                                                           
         OC    QCMLACT3,QCMLACT3                                                
         JNZ   VACT5C                                                           
         OC    QCMLACT4,QCMLACT4                                                
         JNZ   VACT5C                                                           
         J     EXIT                                                             
*                                                                               
VACT5C   DS    0H                                                               
         CLC   QCMLACT1(7),=C'UNCOVER'                                          
         JNE   *+12                                                             
         OI    FLAGS,UNCOVER                                                    
         J     VACT24                                                           
*                                                                               
         LA    R2,QCMLACT1                                                      
         LA    R0,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
         SR    RF,RF                                                            
*                                                                               
VACT6    OC    0(L'QCMLACT1,R2),0(R2) ANY INPUT?                                
         JZ    VACT10               NO                                          
         TM    FLAGS,ISCOVERD      IS THIS CMML COVERED?                        
         JNZ   ERR00667             NO                                          
*                                                                               
         IC    RF,ACTCT            INC ACTUAL COUNT                             
         LA    RF,1(RF)                                                         
         STC   RF,ACTCT                                                         
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VID                                                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         MVC   0(L'NEWACTS,R5),0(R2)                                            
         OC    0(L'NEWACTS,R5),SPACES                                           
         LA    R5,L'NEWACTS(R5)                                                 
*                                                                               
VACT10   LA    R2,L'QCMLACT1(R2)                                                
         JCT   R0,VACT6                                                         
*                                                                               
* MAKE SURE 2+ ACTUALS USED                                                     
*                                                                               
         CLI   ACTCT,0              ANY ACTUALS?                                
         JNE   VACT12               YES                                         
         TM    FLAGS,ISCOVCML      WERE THERE ACTUALS BEFORE?                   
         JNZ   ERR00665             YES                                         
         J     VACT22                                                           
*                                                                               
VACT12   CLI   ACTCT,2                                                          
         JL    ERR00665                                                         
*                                                                               
* CHECK THAT ACCUM SLN'S = COV SLN                                              
*                                                                               
VACT16   L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACTSLN,CMLSLN-CMLDTAEL(R4)                                       
         JNE   ERR00666                                                         
*                                                                               
* ADD X'60' ELS                                                                 
*                                                                               
VACT20   XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING CMLACTEL,R1                                                      
*                                                                               
         ZIC   R0,ACTCT            COUNT OF ACT CMMLS                           
         LA    R2,NEWACTS                                                       
*                                                                               
VACT22   MVI   CMLACTEL,X'60'                                                   
         MVI   CMLACTLN,CMLACTL2                                                
         MVC   CMLACTCM,0(R2)                                                   
         OC    CMLACTCM,SPACES                                                  
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         LA    R2,L'NEWACTS(R2)                                                 
         JCT   R0,VACT22                                                        
*                                                                               
         DROP    R1                                                             
*                                                                               
VACT24   SR    R1,R1                                                            
         LA    R0,NUMACTS                                                       
*                                                                               
VACT26   LA    RE,OLDACTS(R1)                                                   
         LA    RF,NEWACTS(R1)                                                   
         CLC   0(L'NEWACTS,RE),0(RF)  NEW MATCH OLD                             
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_ACTS   SET CHANGE FLAG - NOT IN OLD LIST          
         LA    R1,L'OLDACTS(R1)                                                 
         JCT   R0,VACT26                                                        
         J     EXIT                                                             
*                                                                               
*                                                                               
* VALIDATE ACTUAL ID AND IT'S SLN TO ACTSLN                                     
*  R2 = POINTS TO ACTUL CML                                                     
*                                                                               
VID      NTR1  BASE=*,LABEL=*                                                   
         LR    RF,R2                                                            
         LA    RE,L'QCMLACT1-1                                                  
         BRAS  R1,FLEN             MUST BE AT LEAST 8 CHARS                     
         AHI   RE,1                                                             
         STC   RE,BYTE             SAVE INPUT LEN                               
         CLI   BYTE,8                                                           
         JL    ERR00783                                                         
                                                                                
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCML                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
* CMML CAN'T COVER ITSELF                                                       
*                                                                               
         L     RF,ACMLREC                                                       
         CLC   CMLKCML-CMLKEY(8,RF),WORK                                        
         JE    ERR00668                                                         
*                                                                               
* NOW READ REC                                                                  
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       VCML SETS THIS FLAG                          
         JNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),SVBAGM    A-M/CLT                                      
         MVC   CMLKCML,WORK        CMML FROM VCML                               
         DROP  R4                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00202                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
*                                                                               
* CHECK IF COV CMML                                                             
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         JE    ERR00662                                                         
*                                                                               
* CHECK IF CMML IS DELETED                                                      
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         TM    CMLSTAT,X'80'                                                    
         JNZ   ERR00535                                                         
*                                                                               
* CHECK IF CMML COVERS DATE SPREAD                                              
*                                                                               
         LR    R2,R4               SAVE PTR TO ACT X'10'                        
         L     R4,ACMLREC          GET X'10' FROM COV                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK IF CMML HAS SAME TYPE AS COVER                                          
*                                                                               
VID10    DS    0H                                                               
         CLC   CMLTYPE,CMLTYPE-CMLDTAEL(R2)                                     
         JNE   ERR00669                                                         
         LR    R4,R2                                                            
*                                                                               
* ADD SPOT LENGTH TO ACCUM                                                      
*                                                                               
         LLC   R1,ACTSLN                                                        
         LLC   RF,CMLSLN                                                        
         AR    R1,RF                                                            
         STC   R1,ACTSLN                                                        
         DROP  R4                                                               
*                                                                               
* CHECK IF CMML HAS PROD IN COV CMML                                            
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R4               SAVE ACTUAL'S X'20' EL                       
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         LLC   R1,1(R2)            OUTER LOOP COUNT                             
         SHI   R1,2                                                             
         LA    R2,2(R2)            R2 = A(1'ST PROD) IN ACTUAL                  
OUTER    IC    RF,1(R2)            INNER LOOP COUNT                             
         SHI   RF,2                                                             
         LA    RE,2(R4)            RE = A(1'ST PROD) IN COV                     
INNER    CLC   0(1,RE),0(R2)                                                    
         JE    HIT                                                              
         LA    RE,1(RE)                                                         
         JCT   RF,INNER                                                         
         LA    R2,1(R2)                                                         
         JCT   R1,OUTER                                                         
         J     ERR00664                                                         
*                                                                               
HIT      J     EXIT                                                             
*                                                                               
*                                                                               
*-----------------------------------------------                                
* ADD F1 ACTIVITY ELEMENT                                                       
*-----------------------------------------------                                
*                                                                               
AACTELEM NTR1  BASE=*,LABEL=*                                                   
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
         MVC   SVUID,TWAUSRID   SAVE IN BINARY SIGNON/ORIGIN ID                 
         DROP  RF                                                               
*                                                                               
         MVI   ELCODE,X'F1'                                                     
         L     R4,SVAREC                                                        
         BRAS  RE,GETEL                                                         
         JNE   ACTIV2                                                           
         USING ACTVD,R4                                                         
*                                                                               
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,SVAREC           POINT TO RECORD                              
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         LA    R4,ELEM                                                          
         MVC   ACTVCHDT,BTODAY                                                  
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
*                                                                               
         LA    R3,ACTVCHID                                                      
*                                                                               
         MVC   0(2,R3),SVUID      MOVE IN BINARY SIGNON/ORIGIN ID               
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
         J     ACTIV8                                                           
*                                                                               
ACTIV2   DS    0H                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING ACTVD,R4                                                         
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
*                                                                               
ACTIV6   MVC   0(2,R3),SVUID       MOVE IN ID                                   
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
*                                                                               
ACTIV8   L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVSCID,SECPID-SECD(R1)                                         
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
*                                                                               
*                                                                               
ACTIV10  MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,SVAREC                                                        
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* KEEP LAST 6 CHANGE ELEMENTS                                                   
* AIO3 HAS PREVIOUS VERSION OF RECORD                                           
* SAVE 8 PRODUCTS, START DATE, END DATE, AND SLN FROM IT                        
* ELEMENT LEN IS FIXED, SO FIELDS SAVED EVEN IF THEY DON'T CHANGE               
*====================================================================           
*                                                                               
SETCHGEL NTR1  BASE=*,LABEL=*                                                   
         OC    CHGFLAGS,CHGFLAGS    TEST ANY CHANGES                            
         JZ    EXIT                                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    EXIT                OBVIOUSLY THIS IS NOT A CHANGE!              
*                                                                               
         XC    ELEM,ELEM                                                        
C        USING CMLCHEL,ELEM                                                     
         MVI   C.CMLCHEL,X'C0'                                                  
         MVI   C.CMLCHLEN,CMLCHELX-CMLCHEL                                      
*                                                                               
         MVC   C.CMLCHDAT,BTODAY                                                
         BRAS  RE,GETTIME                                                       
         MVC   C.CMLCHTIM,FULL                                                  
*                                                                               
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
         MVC   C.CMLCHWHO,TWAUSRID   MOVE IN BINARY SIGNON/ORIGIN ID            
         DROP  RF                                                               
*                                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    SETCHG5                                                          
         MVC   C.CMLCHWHO(L'FAPASSWD),FAPASSWD YES SO USE THIS ID               
         OI    C.CMLCHFLG,X'80'    SET FLAG TO SAY THIS IS ID                   
         DROP  R1                                                               
*                                                                               
SETCHG5  DS    0H                                                               
         L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   C.CMLCHWHO,SECPID-SECD(R1)                                       
*                                                                               
SETCHG10 MVC   C.CMLCHDT1(3),CHGFLAGS MOVE CHANGE FLAGS                         
*                                                                               
         L     R4,AIO8             SAVE OLD DATA                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
*                                                                               
         MVC   C.CMLCHSDT,CMLRLSE                                               
         MVC   C.CMLCHEDT,CMLRCL                                                
         MVC   C.CMLCHSLN,CMLSLN                                                
*                                                                               
         L     R4,AIO8                                                          
         MVI   ELCODE,X'20'        PRODUCT LIST                                 
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         CHI   RF,8                                                             
         JNH   *+8                                                              
         LHI   RF,8                                                             
         AHI   RF,-3               SET FOR EX                                   
*                                                                               
         BASR  R1,0                                                             
         MVC   C.CMLCHPRD(0),2(R4)                                              
         EX    RF,0(R1)                                                         
*                                                                               
         L     R4,AIO8              SAVE OLD DATA                               
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         JNE   SETCHG18                                                         
         USING CMLMATEL,R4                                                      
*                                                                               
         MVC   C.CMLCHSTM,CMLMSTIM                                              
         MVC   C.CMLCHETM,CMLMETIM                                              
*                                                                               
SETCHG18 L     R4,ACMLREC                                                       
         MVI   ELCODE,X'C0'        FIND MOST RECENT CHANGEL                     
         BRAS  RE,GETEL                                                         
         JNE   SETCHG30                                                         
         USING CMLCHEL,R4                                                       
*                                                                               
         CLC   CMLCHDAT(11),C.CMLCHDAT   TEST SAME DATE/PERSON                  
         JNE   SETCHG20                                                         
         MVC   CMLCHTIM,C.CMLCHTIM       MOVE IN LATEST TIME                    
         OC    CMLCHDT1(3),C.CMLCHDT1    'OR' IN NEW ACTIVITY                   
         OI    CMLCHDT2,CMLCH_OPC  Changed in optica                            
         J     EXIT                                                             
*                                                                               
SETCHG20 L     R4,ACMLREC          IF > 6 ELEMENTS, DELETE OLDEST               
         SR    R5,R5                                                            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
*                                                                               
SETCHG22 BRAS  RE,NEXTEL                                                        
         JNE   SETCHG30                                                         
         AHI   R5,1                                                             
         CHI   R5,6                                                             
         JL    SETCHG22                                                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEM                                  
*                                                                               
SETCHG30 L     R4,ACMLREC          ADD NEW BEFORE OTHER CHGELS                  
         BRAS  RE,GETEL               OR AT EOR                                 
         OI    CMLCHDT2,CMLCH_OPC  CHANGED IN OPTICA                            
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL            ADD ELEM                                     
         J     EXIT                                                             
*                                                                               
GETTIME  NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL                                                        
         THMS                      TIME R1=0HHMMSS+                             
         LR    R0,R1               CONVERT TO BINARY HMS                        
         SRDL  R0,12                                                            
         SRL   R1,20                                                            
         XC    DUB,DUB                                                          
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+2                                                        
         SRDL  R0,8                                                             
         SRL   R1,20                                                            
         LA    RF,X'0C'                                                         
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+1                                                        
         SRDL  R0,28                                                            
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL                                                          
         J     EXIT                                                             
*                                                                               
*                                                                               
*===============================================================                
* GET NEXT SEQUENCE NUMBER FROM MASTER RECORD FOR ADDS                          
*===============================================================                
*                                                                               
PAREC    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,IOKEY                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV  IF CMML SEQ REC FOUND                        
         JE    PAR10               GO SAVE CMML SEQ NUMBER                      
         DROP  R4                                                               
*                                                                               
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA/CLT-ONCE FOR EACH A/M/CLT          
*                                                                               
         MVC   IOKEY(13),IOKEYSAV     RESTORE KEY                               
         L     R4,AIO7                                                          
         USING CMLKEY,R4                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,IOKEY                                                     
         MVC   CMLAGYA,LP_AGY      SET AGENCY ALPHA ID                          
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R4,ELEM                                                          
         USING CMLDTAEL,R4                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVI   CMLSEQ+2,X'01'      START SEQ NUMBER ONE IN MASTER               
         MVC   HOLDSEQ,CMLSEQ      START SEQ NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CMML SEQ RECORD'                                  
*                                                                               
         L     R7,AIO7                                                          
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL            ADD CMML DATA ELEMENT                        
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOTRFFIL+IO7'                        
         J     PAR20                                                            
*                                                                               
PAR10    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         MVC   HOLDSEQ,CMLSEQ                                                   
*                                                                               
PAR20    MVC   IOKEY(L'SVKEY),SVKEY  RESTORE KEY                                
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         MVC   CMLSEQ,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT               
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
*================================================================               
* AFTER ADDED RECORD - ADD PASSIVE KEYS, UPDATE SEQUENCE NUMBER                 
*================================================================               
*                                                                               
AAREC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVDSKAD,IODA        SAVE DISK ADDR OF ADDED REC                  
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING CMLKEY,R5                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOTRFDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO7'                        
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R4                                                      
         SR    R1,R1                                                            
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
*                                                                               
         L     RE,AIO7                                                          
         ST    RE,SVAREC                                                        
         BRAS  RE,AACTELEM         ADD ACTIVITY ELEMENT TO REC IN IO7           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+IO7'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CMLKID(2),=X'0AA1'                                               
         MVC   CMLKAM(3),SVBAGM                                                 
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
         CLI   ADIDFLAG,C'Y'                                                    
         JNE   *+8                                                              
         OI    CMLKSTAT,CMLKSTA_PCKD                                            
         MVC   IOKEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOTRFDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CREATE ADID POINTER FOR 8-12 CHAR CMML                                        
*                                                                               
         MVC   CMLKID(2),=X'0AC1'                                               
         MVC   CMLKAM(3),SVBAGM & BCLT                                          
         MVC   WORK(12),SPACES                                                  
         L     R4,ACMLREC                                                       
         MVC   CMLKCML,5(R4)       PRESET CML IN KEY                            
         TM    15(R4),X'01'        PACKED CML IN KEY                            
         JO    AAR02               YES, KEY IS SET                              
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),5(R4)       PRESET CML IN KEY                            
         GOTOR VTRPACK,DMCB,(C'P',WORK),CMLKCML                                 
         JNE   AAR05                                                            
*                                                                               
AAR02    GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOTRFDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AAR05    L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
AAR26    BRAS  RE,NEXTEL                                                        
         JNE   AAR30                                                            
         LA    R1,2(R4)                                                         
         LA    R0,1                                                             
*                                                                               
         USING CMLACTEL,R4                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         JE    AAR28                                                            
         CLI   8(R1),C' '          TEST ISCI                                    
         JNH   AAR28               YES                                          
         GOTOR VTRPACK,DMCB,(C'P',2(R4)),DUB   PACK 12 CHAR ADID                
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
AAR28    BRAS  RE,UPDACT                                                        
         J     AAR26                                                            
*                                                                               
AAR30    DS    0H                                                               
         XC    IOKEY,IOKEY                                                      
         OC    SVHDDELK,SVHDDELK                                                
         JZ    AAR35                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDDELK),SVHDDELK                                       
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
*                                                                               
AAR35    XC    IOKEY,IOKEY                                                      
         OC    SVHDADDK,SVHDADDK                                                
         JZ    AARX                                                             
         CLC   SVHDADDK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDADDK),SVHDADDK                                       
         MVC   IOKEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                    
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
*                                                                               
AARX     J     EXIT                                                             
         DROP  R4,R5,RB                                                         
*                                                                               
*                                                                               
*====================================================================           
* AFTER RE-WRITING RECORD - CHECK IF ACTUAL CMMLS NEED REWRITING                
*====================================================================           
*                                                                               
ACH      NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,1                                                             
         TM    FLAGS,SOFTDEL                                                    
         JZ    *+12                                                             
         LHI   R0,-1                                                            
         J     *+12                                                             
         TM    FLAGS,SOFTREST                                                   
         JZ    ACH20                                                            
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
*                                                                               
ACH10    BRAS  RE,NEXTEL                                                        
         JNE   ACH30                                                            
         LA    R3,2(R4)                                                         
         LA    R0,1                                                             
         LR    R1,R3               POINT TO CMML                                
*                                                                               
         USING CMLACTEL,R4                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         JE    ACH15                                                            
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   ACH15                                                            
*                                                                               
         OC    0(12,R3),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         JNE   ERR00783                                                         
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
ACH15    BRAS  RE,UPDACT                                                        
         J     ACH10                                                            
*                                                                               
ACH20    BRAS  RE,NPUT                                                          
*                                                                               
ACH30    DS    0H                                                               
         MVC   SVDSKAD,SVDSKADR                                                 
         XC    IOKEY,IOKEY                                                      
         OC    SVHDDELK,SVHDDELK                                                
         JZ    ACH40                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDDELK),SVHDDELK                                       
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
*                                                                               
ACH40    XC    IOKEY,IOKEY                                                      
         OC    SVHDADDK,SVHDADDK                                                
         JZ    ACHX                                                             
         CLC   SVHDADDK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDADDK),SVHDADDK                                       
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
*                                                                               
ACHX     J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* UPDATE CMMLS IF NEEDED - CHANGE WAS OTHER THAN DELETE OR RESTORE              
*====================================================================           
*                                                                               
NPUT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* FOR ANY ENTRY THAT IS IN OLDACTS AND NOT IN NEWACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'-1'                                                  
*                                                                               
         LHI   R0,-1               SET FLAG FOR UPDACT                          
         LA    R2,NUMACTS                                                       
         LA    R3,OLDACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         JZ    NP20                                                             
*                                                                               
NP01     LA    R4,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
*                                                                               
NP02     CLC   0(12,R3),0(R5)                                                   
         JE    NP10                                                             
         LA    R5,12(R5)                                                        
         JCT   R4,NP02                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   NP05                MUST BE 8 CHAR ISCII                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
NP05     BRAS  RE,UPDACT                                                        
*                                                                               
NP10     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         JZ    NP20                                                             
         JCT   R2,NP01                                                          
*                                                                               
* FOR ANY ENTRY THAT IS IN NEWACTS AND NOT IN OLDACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'1'                                                   
*                                                                               
NP20     DS    0H                                                               
         TM    FLAGS,UNCOVER                                                    
         JNZ   NPX                 NO NEWACTS                                   
         LA    R0,1                                                             
         LA    R2,NUMACTS                                                       
         LA    R3,NEWACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         JZ    NPX                                                              
*                                                                               
NP21     LA    R4,NUMACTS                                                       
         LA    R5,OLDACTS                                                       
*                                                                               
NP24     CLC   0(12,R3),0(R5)                                                   
         JE    NP30                                                             
         LA    R5,12(R5)                                                        
         JCT   R4,NP24                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   NP26                MUST BE 8 CHAR ISCII                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
NP26     BRAS  RE,UPDACT                                                        
*                                                                               
NP30     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         JZ    NPX                                                              
         JCT   R2,NP21                                                          
NPX      J     EXIT                                                             
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
DELPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'                           
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   EXITY                                                            
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOTRFDIR+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXITY                                                            
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF FIELD ABOUT TO BE ADDED                                
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
ADDPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOTRFDIR+IO7'                         
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ADDPSS                                                           
*                                                                               
WRTPSS   DS    0H                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         NI    IOKEY+13,X'FF'-X'80'                                             
         MVC   IOKEY+14(4),SVDSKADR                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+10                                                             
         MVC   IOKEY+14(4),SVDSKAD   USE THIS FOR ACTION ADD                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOTRFDIR+IO7'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
ADDPSS   DS    0H                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   IOKEY+14(4),SVDSKADR                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+10                                                             
         MVC   IOKEY+14(4),SVDSKAD   USE THIS FOR ACTION ADD                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOTRFDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
* EXPECTS:                                                                      
*  R0 = H'1' FOR INC, H'-1' FOR DEC                                             
*  R1 = A(CMML ID)                                                              
*                                                                               
*                                                                               
UPDACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       TEST TO READ ADID                            
         JNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
         MVC   CMLKCML,0(R1)                                                    
         DROP  R4                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOTRFDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO7'                        
*                                                                               
         L     R4,AIO7                                                          
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
*                                                                               
         USING CMLDTAEL,R4                                                      
         SR    RE,RE                                                            
         ICM   RE,3,CMLCOVCT                                                    
         AR    RE,R0                                                            
         STCM  RE,3,CMLCOVCT                                                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+IO7'                        
UAXIT    J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* VALIDATE 8 CHARACTER COMMERCIAL CODE                                          
* FIRST 4 CHARS MUST BE ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC *               
* ON EXIT, 8 CHARACTER CMNL WILL BE IN WORK!                                    
* (R2 POINTING TO COMML FIELD )                                                 
*====================================================================           
*                                                                               
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVI   ADIDFLAG,C'N'       ASSUME CML IS ISCII                          
*                                                                               
         CLI   8(R2),X'40'         MUST BE AT LEAST 8 CHARS                     
         JH    VCML50              MORE IS ADID                                 
*                                                                               
         CLI   RECACT,C'A'         IF ADD FULLY EDIT CML                        
         JNE   VCML10                                                           
         CLI   8(R2),X'40'         MUST BE LEN OF 8                             
         JNE   ERR00783            MUST BE 8 CHAR ISCII                         
*                                                                               
VCML10   LA    R0,8                                                             
         LR    R1,R2                                                            
VCML20   CLI   0(R1),C'A'                                                       
         JL    ERR00734            MUST BE ALPHA NUMERIC                        
         CLI   0(R1),C'Z'                                                       
         JNH   VCML36                                                           
         CLI   0(R1),C'0'                                                       
         JL    ERR00734                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR00734                                                         
VCML36   LA    R1,1(R1)                                                         
         JCT   R0,VCML20                                                        
*                                                                               
         MVC   WORK(L'QCOMML),0(R2)                                             
         J     VCMLX                                                            
*                                                                               
*=================================================================              
* See if this is an ad-id code                                                  
*=================================================================              
*                                                                               
VCML50   DS    0H                                                               
         GOTOR VTRPACK,DMCB,(C'P',0(R2)),WORK                                   
         JNE   ERR00722                                                         
*                                                                               
         MVI   ADIDFLAG,C'Y'       SET CMML IS ADID                             
         CLI   RECACT,C'A'         IF ADD                                       
         JE    VCMLX                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0AC1'                                                
         MVC   IOKEY+2(3),SVBAGM     A-M/CLT                                    
         MVC   IOKEY+5(8),WORK                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00202                                                         
*                                                                               
VCMLX    J     EXIT                                                             
*                                                                               
*                                                                               
***********************************************************************         
* PATTERN UPLOAD                                                                
***********************************************************************         
PATUPLD  NTR1  BASE=*,LABEL=*                                                   
         LR    R6,R1               R1=A(LP_D)                                   
         USING LP_D,R6                                                          
*                                                                               
         ICM   RE,15,I$CAM                                                      
         JZ    ERR00258            Missing media                                
*                                                                               
         USING LW_D,RE                                                          
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
         MVC   AMEDIA,LW_DATA1     Save media alpha                             
         GOTOR (#VALMED,AVALMED),DMCB,LW_DATA1,(R3),SVBAGM                      
         JNE   ERR01023            Invalid media                                
*                                                                               
         ICM   RE,15,I$CCLT                                                     
         JZ    ERR00966            MISSING CLIENT                               
*                                                                               
         MVC   QCLTA,LW_DATA1                                                   
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
*                                                                               
         CHI   R3,2                                                             
         JL    ERR00014            INVALID CLIENT                               
         JH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
*                                                                               
         GOTOR (#VALCLT,AVALCLT),DMCB,LW_DATA1,(R3),SVBCLT                      
         JNE   ERR00014            INVALID CLIENT                               
*                                                                               
PATU10   DS    0H                                                               
         BRAS  RE,GPROF                                                         
*                                                                               
         OC    QPROD1,QPROD1       Any product                                  
         JZ    ERR00260             No                                          
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,QPROD1,L'QPROD1,QPRDX                     
         JNE   ERR00786            Bad product                                  
*                                                                               
         BRAS  RE,GPROD            GET PROD REC FOR THEATRICAL                  
*                                                                               
         TM    FLAGS2,PRDTHTR                                                   
         JZ    PATU20                                                           
*                                                                               
*NOP     CLI   CONREC,C'B'         MUST USE BPAT                                
****     JNE   BPATERR                                                          
*        CLC   LP_QMAPN,=AL2(I#STPATU) PATTERN                                  
****     JE    BPATERR                                                          
****     J     PATU20                                                           
*                                                                               
*K21     CLC   =C'PAT',CONREC      MUST USE PAT FOR NON THTR PROD               
******   JNE   UPATERR                                                          
*K21     DS    0H                                                               
**       CLC   LP_QMAPN,=AL2(I#STPATU) PATTERN                                  
*****    JNE   UPATERR                                                          
*                                                                               
PATU20   CLC   =C'POL',QPROD1                                                   
         JE    PRDINV                                                           
         CLC   =C'AAA',QPROD1                                                   
         JE    PRDINV                                                           
*                                                                               
*==================================================================             
* VALIDATE PATTERN LEN                                                          
*==================================================================             
*                                                                               
PATU30   CLI   QLEN1,0             ANY ENTRY                                    
         JNE   PATU35                                                           
         MVI   QLEN1,30            DEFAULT SPOT LENGTH IS 30 SEC                
         J     PATU38                                                           
*                                                                               
PATU35   MVC   SVSLN,QLEN1                                                      
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALISLN                                                       
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   SVLEN1,SVSLN        SAVE LEN 1                                   
*                                                                               
PATU38   DS    0H                                                               
*NOP     CLI   SVPROF13,C'Y'       THIS CLT USE PROD EQUIV                      
*        JNE   PATU40               NO                                          
*****    BRAS  RE,CEP              GO CK EQUIV PRODS                            
*                                                                               
PATU40   DS    0H                                                               
         CLI   QPROD2,0            ANY P/B PROD                                 
         JE    PATU60              NO                                           
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,QPROD2,L'QPROD2,QPRDX2                    
         JNE   ERR00786            BAD PRODUCT                                  
*                                                                               
*NOP     TM    FLAGS2,PRDTHTR      THEATRICAL PRODUCT                           
*****    JO    NOPBERR             NO P/B ALLOWED                               
*                                                                               
PATU45   CLC   =C'POL',QPROD2                                                   
         JE    PRDINV                                                           
         CLC   =C'AAA',QPROD2                                                   
         JNE   PATU48                                                           
*                                                                               
PRDINV   LA    RF,L'INVPRDCD                                                    
         LA    R1,INVPRDCD                                                      
         J     ERR                 ERROR                                        
*                                                                               
PATU48   CLI   QLEN2,0             VALID SPOT LEN                               
         JNE   PATU50              YES                                          
         MVI   QLEN2,30            DEFAULT SPOT LENGTH IS 30 SEC                
*                                                                               
PATU50   MVC   SVSLN,QLEN2                                                      
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALISLN                                                       
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   SVLEN2,SVSLN        SAVE LEN 2                                   
*                                                                               
         CLC   QPROD1,QPROD2       COMPARE 2 PRODUCTS                           
         JL    PATU60                                                           
         LA    RF,L'INVPRDCD                                                    
         LA    R1,INVPRDCD                                                      
         J     ERR                 ERROR                                        
*                                                                               
*-------------------------------------                                          
* VALIDATE COPY CODE                                                            
*-------------------------------------                                          
PATU60   DS    0H                                                               
         MVI   ERRFLG,0                                                         
         LA    R2,QCODE            CODE                                         
         BRAS  RE,VCC                                                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*-------------------------------------                                          
* VALIDATE REFERENCE NUMBER                                                     
*-------------------------------------                                          
         MVI   ERRFLG,0                                                         
         BRAS  RE,VREF                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*--------------------                                                           
* NOW BUILD KEY                                                                 
*--------------------                                                           
*                                                                               
PATU70   CLI   RECACT,C'A'         IF ADD                                       
         JE    PATU80                                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING PATKEY,R4                                                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(3),SVBAGM  A/M/CLT                                        
         MVC   PATKPRD,QPRDX     BPRD                                           
         MVC   PATKSLN,SVLEN1    LEN                                            
         MVC   PATKPRD2,QPRDX2   PRD2                                           
         MVC   PATKSLN2,SVLEN2   LEN2                                           
         MVC   PATKCODE,CODE                                                    
         MVC   PATKREF,BREFSUB                                                  
*                                                                               
         CLI   RECACT,C'A'         IF ADD                                       
         JE    PATU80                                                           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOTRFDIR+#PATREC'                      
         JNE   ERR00971                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+#PATREC'                    
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,APATREC          PREPARE IOAREA FOR THE ADD                   
*                                                                               
         CLI   RECACT,C'X'         DELETE                                       
         JE    PATU75                                                           
         CLI   RECACT,C'R'         RESTORE                                      
         JE    PATU75                                                           
         CLI   RECACT,C'C'                                                      
         JNE   ERR00049            RECORD ALREADY EXISTS                        
*                                                                               
PATU75   L     RE,AIO5                                                          
         LHI   RF,IO5LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     RE,APATREC          SAVE ORIGINAL RECORD IN IO5                  
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIO5                                                          
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
*                                                                               
         OC    QCKSM,QCKSM         ANY CHECKSUM PASSED                          
         JNZ   *+16                 YES, CHECK                                  
         CLI   RECACT,C'C'                                                      
         JE    ERR00814            NEED CHECK SUM FOR ACTION CHANGE             
         J     PATU90                                                           
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCKSUM           VALIDATE CHECK SUM                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         J     PATU90                                                           
*                                                                               
PATU80   DS    0H                                                               
         L     RE,APATREC          PREPARE IOAREA FOR THE ADD                   
         LHI   RF,APATREQ                                                       
         XCEFL                                                                  
*                                                                               
         L     R4,APATREC                                                       
         USING PATRECD,R4                                                       
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(3),SVBAGM  A/M/CLT                                        
         MVC   PATKPRD,QPRDX     BPRD                                           
         MVC   PATKSLN,SVLEN1    LEN                                            
         MVC   PATKPRD2,QPRDX2   PRD2                                           
         MVC   PATKSLN2,SVLEN2   LEN2                                           
         MVC   PATKCODE,CODE                                                    
         MVC   PATKREF,BREFSUB                                                  
         MVI   14(R4),24         RECORD LENGTH                                  
         MVC   PATAGYA,LP_AGY    SET AGENCY ALPHA ID                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),0(R4)                                                  
         MVC   SVKEY,IOKEY         SAVE KEY                                     
*                                                                               
         CLI   RECACT,C'A'         ADD ?                                        
         JE    PATU95                                                           
*                                                                               
PATU90   DS    0H                                                               
*                                                                               
*KX      ICM   R0,7,PATKREF        GET REF/SUB                                  
*        N     R0,=X'000003FF'     DROP REF                                     
*        X     R0,=X'000003FF'     UNCOMPLEMENT                                 
*NOP     STH   R0,LATEST           SAVE MOST RECENT SUBLINE                     
*                                                                               
         NI    FLAGS,X'FF'-DELPATSW     INIT DELETE PATTERN                     
         NI    FLAGS,X'FF'-RESPATSW     INIT RESTORE PATTERN                    
         MVC   SVKEY,IOKEY         SAVE KEY                                     
*                                                                               
         USING PATKEY,R4                                                        
         MVC   SVBCLT,PATKCLT     CLT                                           
         MVC   QPRDX,PATKPRD      BPRD                                          
         MVC   SVLEN1,PATKSLN     LEN                                           
         MVC   QPRDX2,PATKPRD2    PRD2                                          
         MVC   SVLEN2,PATKSLN2    LEN2                                          
PATU95   ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         STH   R0,BREF                                                          
         SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
         MVI   NEWSUBSW,0                                                       
*                                                                               
         DROP  R4                                                               
*                                                                               
         OC    QDESCR,QDESCR       ANY DESCRIPTION                              
         LA    R2,QDESCR           DESCRIPTION                                  
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         CLI   RECACT,C'A'         ADD                                          
         JE    PATU100                                                          
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
*                                                                               
PATU100  L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         LA    R4,ELEM                                                          
         USING PATDTAEL,R4                                                      
         MVI   PATDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   ELEM+1,PATDTAX-PATDTAEL ELEMENT LENGTH                           
         MVC   SVOLDDPT,PATDPT-PATDTAEL(R4)                                     
*                                                                               
         CLI   RECACT,C'A'         ADD                                          
         JE    PATU110                                                          
*                                                                               
* IF DELETE JUST DELETE, BYPASS ALL OTHER CKS *                                 
*                                                                               
         CLI   RECACT,C'X'          DELETE                                      
         JNE   PATU110              NO                                          
         OI    FLAGS,DELPATSW       DELETE PATTERN REQUESTED                    
         OI    PATSTAT,X'80'        SET ON SOFT DELETE BIT                      
*                                                                               
         OI    PATSTAT1,PATOPCHG    PAT CHANGED IN OPTICA                       
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,APATREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         J     PATU300                                                          
*                                                                               
* IF COPY CODE = ESTIMATE, CAN'T CHANGE NON-ESTIMATE PATTERNS *                 
*                                                                               
PATU110  CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         JNE   PATU115                                                          
         CLI   QCODE,0             THIS BY ESTIMATE                             
         JNE   PATU115                                                          
*                                                                               
         LA    RF,L'NOESTER        NO ESTIMATE IN REQ PER                       
         LA    R1,NOESTER                                                       
         J     ERR                 ERROR                                        
*                                                                               
PATU115  CLI   CODESW,C'Y'         THIS A COPY CODE=EST                         
         JNE   *+8                                                              
         OI    PATSTAT,X'10'                                                    
*                                                                               
* SEE IF RESTORING A DELETED PATTERN *                                          
*                                                                               
         CLI   RECACT,C'R'         RESTORE                                      
         JNE   PATU125             NO                                           
         OI    FLAGS,RESPATSW      RESTORE PATTERN REQUESTED                    
*                                                                               
         TM    PATSTAT,X'80'       WAS PAT SOFT DELETED                         
         JO    PATU120                                                          
*                                                                               
         LA    RF,L'NOTDEL         PAT NOT DELETED                              
         LA    R1,NOTDEL                                                        
         J     ERR                 ERROR                                        
*                                                                               
PATU120  NI    PATSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         J     PATU130                                                          
PATU125  MVC   PATDESC,QDESCR                                                   
*                                                                               
* IF MAINTAINING AN AUTO P/B PATTERN, MAKE IT MANUAL *                          
*                                                                               
PATU130  TM    PATSTAT,X'40'       AUTO P/B PATTERN                             
         JZ    PATU140                                                          
         NI    PATSTAT,X'FF'-X'40'                                              
         CLC   PATDESC,=CL16'AUTO P/B PATTERN'                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   PATDESC,=CL16'AUTO P/B NOW MAN'                                  
*                                                                               
         OI    PATSTAT,X'20'       SET ON WAS AUTO P/B PATTERN                  
*                                                                               
PATU140  LA    R2,QINVPRD          INVERTED PROD/CMML FIELD                     
         OC    QINVPRD,QINVPRD     ANY INPUT?                                   
         JNZ   *+12                TO NEXT COMPARE                              
         MVI   QINVPRD,C'N'                                                     
         J     PATU160                                                          
         CLI   QINVPRD,C'N'                                                     
         JE    PATU160             GO TO NEXT VALIDATION                        
         CLI   QINVPRD,C'Y'                                                     
         JE    PATU150                                                          
*                                                                               
         LA    RF,L'INVPYN         INVERTED PROD VALID ENTRIES Y/N              
         LA    R1,INVPYN                                                        
         J     ERR                 ERROR                                        
*                                                                               
PATU150  CLI   QPROD2,0            WAS THERE PARTNER PROD                       
         JNE   PATU155                                                          
*                                                                               
         LA    RF,L'IPREQPB        INVERT = Y REQUIRES P/B PROD                 
         LA    R1,IPREQPB                                                       
         J     ERR                 ERROR                                        
*                                                                               
PATU155  OI    PATSTAT,X'04'       INDICATE YES FOR INVERTED                    
         J     *+8                                                              
PATU160  NI    PATSTAT,X'FB'       TURN OFF INVERTED ONLY                       
*                                                                               
PATU165  MVI   ERRFLG,0                                                         
         BRAS  RE,VPER                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*NOP     CLI   CONREC,C'B'         IS THIS BPAT                                 
*        JNE   *+8                                                              
****     OI    PATSTAT1,PATSBPAT   THIS IS BPAT RECORD                          
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+12                                                             
         OI    PATSTAT1,PATOPADD   PAT ADDED IN OPTICA                          
         J     PATU169                                                          
*                                                                               
         OI    PATSTAT1,PATOPCHG   PAT CHANGED IN OPTICA                        
*                                                                               
PATU169  L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   ELCODE,X'20'        MARKET/STATION LIST                          
         CLI   RECACT,C'A'         ADD                                          
         JE    PATU170                                                          
*                                                                               
         L     R4,APATREC                                                       
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
*                                                                               
PATU170  L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         MVC   ELEM2,ELEM          SAVE ELEM IN ELEM2                           
*                                                                               
*-----------------------------------------                                      
* VALIDATE MARKET/STATION/MARKET GROUP                                          
*-----------------------------------------                                      
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VMS              QMKT/QSTA/QMGROUP                            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         CLI   RECACT,C'A'                                                      
         JE    PATU175                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ELEM+1                                                        
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   ELEM2(0),ELEM                                                    
         JE    *+8                                                              
         MVI   NEWSUBSW,1          IF MKTLIST CHANGES, FORCE NEW SUBL           
*                                                                               
PATU175  DS    0H                                                               
*NOP     CLI   CONREC,C'B'         IS THIS BPAT                                 
*        JNE   PATU180                                                          
*                                                                               
*        LA    R2,TRADPTH                                                       
*        CLI   5(R2),0                                                          
*        JNE   INVALERR                                                         
*        LA    R2,TRAINVPH                                                      
*        CLI   8(R2),C'N'                                                       
*        JE    *+12                                                             
*        CLI   5(R2),0                                                          
*******  JNE   INVALERR                                                         
*                                                                               
PATU180  XC    DUB,DUB                                                          
         OC    QPATSTME,QPATSTME   TEST START TIME ENTERED                      
         JNZ   PATU185             YES                                          
         OC    QPATETME,QPATETME   ELSE MUST BE NO END TIME                     
         JZ    PATU210                                                          
*                                                                               
         LA    RF,L'NOSTMERR       NO END TIME WITHOUT START TIME               
         LA    R1,NOSTMERR                                                      
         J     ERR                 ERROR                                        
*                                                                               
PATU185  LA    RF,QPATSTME         PATTERN START TIME                           
         LA    RE,L'QPATSTME-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
         LA    R2,QPATSTME         PATTERN START TIME                           
         MVI   ERRFLG,0                                                         
         BRAS  RE,GOTIMVAL                                                      
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   DUB(2),FULL                                                      
*                                                                               
         CLI   QDPT,X'40'                                                       
         JH    ERR00771            ERROR, NO DAYPART AND TIME                   
*                                                                               
         CLC   ENDPAT,=X'FFFFFF'   TEST UFN PATTERN                             
         JNE   PATU190                                                          
         OC    QPATETME,QPATETME                                                
         JZ    PATU210                                                          
*                                                                               
         LA    RF,L'NOETMUFN       NO END TIME PAT UFN                          
         LA    R1,NOETMUFN                                                      
         J     ERR                 ERROR                                        
*                                                                               
PATU190  OC    QPATETME,QPATETME                                                
         JNZ   PATU200                                                          
*                                                                               
         LA    RF,L'ENDTERR        END TIME ERR                                 
         LA    R1,ENDTERR                                                       
         J     ERR                 ERROR                                        
*                                                                               
PATU200  LA    RF,QPATETME         PATTERN END TIME                             
         LA    RE,L'QPATETME-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
         LA    R2,QPATETME         PATTERN END TIME                             
         MVI   ERRFLG,0                                                         
         BRAS  RE,GOTIMVAL                                                      
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   DUB+2(2),FULL                                                    
         CLC   DUB+2(2),=H'2400'   MUST END BEFORE MIDNIGHT                     
         JL    PATU210                                                          
*                                                                               
         LA    RF,L'ENDTERR        END TIME ERR                                 
         LA    R1,ENDTERR                                                       
         J     ERR                 ERROR                                        
*                                                                               
PATU210  CLI   QDAILY,X'40'                                                     
         JNH   PATU230                                                          
         CLI   QDAILY,C'N'                                                      
         JE    PATU230                                                          
         CLI   QDAILY,C'Y'                                                      
         JE    PATU220                                                          
*                                                                               
         LA    RF,L'INVDAILY       NO END TIME WITHOUT START TIME               
         LA    R1,INVDAILY                                                      
         J     ERR                 ERROR                                        
*                                                                               
PATU220  CLI   QPATSTME,X'40'      TEST TIME WAS INPUT                          
         JH    PATU230                                                          
*                                                                               
         LA    RF,L'DLYTMER        NO END TIME WITHOUT START TIME               
         LA    R1,DLYTMER                                                       
         J     ERR                 ERROR                                        
*                                                                               
PATU230  LA    R2,QDPT                                                          
         CLI   0(R2),0             IF BOTH SCREEN AND RECORD                    
         JNE   PATU235             HAVE NO DAYPART SKIP VALIDATION              
         CLI   SVOLDDPT,0          ELSE CANNOT ADD OR DELETE DAYPART            
         JE    PATU255             ONCE THE PATTERN HAS BEEN ADDED              
         J     PATU240                                                          
*                                                                               
PATU235  CLI   QPATSTME,0          PATTERN START TIME                           
         JNE   ERR00771            ERROR, NO DAYPART AND TIME                   
*                                                                               
         MVC   DUB+4(1),QDPT       SAVE DAYPART                                 
*                                                                               
         CLI   RECACT,C'A'         IS THIS AN ADD                               
         JE    PATU242                                                          
*                                                                               
PATU240  CLC   SVOLDDPT,DUB+4      ELSE THEY CAN'T CHANGE IT                    
         JNE   ERR00779                                                         
*                                                                               
PATU242  CLI   SVPROF11,C'E'       TEST COPYCODE=EST                            
         JNE   ERR00771                                                         
*                                                                               
PATU244  LA    R1,DMCB             GET DAYPART MENU                             
         MVC   0(2,R1),LP_AGY      SET AGENCY ALPHA ID                          
         MVC   2(1,R1),AMEDIA                                                   
         MVC   3(1,R1),SVMENU                                                   
         GOTO1 =V(DPTRD),(R1),,AIO7,VDATAMGR,RR=SRVRRELO                        
*                                                                               
         CLI   8(R1),X'FF'                                                      
         JNE   PATU246                                                          
*                                                                               
         LA    RF,L'INVDPT         INVALID DAYPART                              
         LA    R1,INVDPT                                                        
         J     ERR                 ERROR                                        
*                                                                               
PATU246  TM    8(R1),X'08'                                                      
         JZ    PATU246C                                                         
*                                                                               
         LA    RF,L'INVDPT                                                      
         LA    R1,INVDPT                                                        
         J     ERR                 ERROR                                        
*                                                                               
PATU246C LA    R0,35               MAX DPTS IN MENU                             
         L     R1,AIO7                                                          
*                                                                               
PATU250  CLI   0(R1),0             TEST FOR EOT                                 
         JE    ERR00772                                                         
         CLC   DUB+4(1),0(R1)                                                   
         JE    PATU255                                                          
         LA    R1,5(R1)                                                         
         J     PATU250                                                          
*                                                                               
PATU255  MVI   ELCODE,X'10'                                                     
         L     R4,APATREC                                                       
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QDAILY,C'Y'         TEST DAILY TIMES                             
         JE    PATU260                                                          
         CLC   PATSTART,PATEND     PATTERN FOR ONE DAY                          
         JNE   PATU265                                                          
*                                                                               
PATU260  LH    R0,DUB              YES - MUST START BEFORE IT ENDS              
         J     *+16      <-----    NOP FOR CALENDAR DAYS                        
         CHI   R0,599                                                           
         JH    *+8                                                              
         AHI   R0,2400             FOR CALENDAR DAYS                            
*                                                                               
         CHI   R0,2400             FOR CALENDAR DAYS, CHECK FOR 12M             
         JL    *+6                                                              
         XR    R0,R0               MAKE IT A LOW TIME                           
*                                                                               
         LH    R1,DUB+2                                                         
         J     *+16      <------   NOP FOR CALENDAR DAYS                        
         CHI   R1,599                                                           
         JH    *+8                                                              
         AHI   R1,2400                                                          
*                                                                               
         CR    R0,R1               TEST START BEFORE END                        
         JH    ERR00773                                                         
*                                                                               
PATU265  CLC   PATSTIM(4),DUB                                                   
         JNE   PATU270                                                          
         CLC   PATDPT,DUB+4                                                     
         JE    PATU275                                                          
PATU270  MVI   NEWSUBSW,1          FORCE NEW SUBL IF NEW TIM OR DPT             
*                                                                               
PATU275  MVC   PATSTIM(4),DUB                                                   
         MVC   PATDPT,DUB+4                                                     
*                                                                               
         NI    PATSTAT1,X'FF'-PATSDLY                                           
         CLI   QDAILY,C'Y'                                                      
         JNE   *+8                                                              
         OI    PATSTAT1,PATSDLY    SET DAILY FLAG                               
*                                                                               
         OC    PATSTIM(4),PATSTIM  TEST TIME ENTERED                            
         JZ    PATU280             NO - GOOD                                    
         CLI   PATDPT,0            TEST DPT ENTERED                             
         JE    PATU280                                                          
         J     ERR00774                                                         
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
PATU280  DS    0H                                                               
         MVI   ELCODE,X'40'        PATTERN COMMENT ELEMENT                      
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         ICM   R2,15,QCMTIND       COMMENTS INDEX                               
         JZ    PATU285                                                          
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMT                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
PATU285  DS    0H                                                               
*                                                                               
*NOP                                                                            
*R60     CLI   MYSCREEN,X'5C'      TEST BPAT CMML SCREEN                        
*        JNE   VR62                                                             
*                                                                               
*R61     BRAS  RE,VBPAT                                                         
******** J     PATU310                                                          
*                                                                               
PATU290  MVI   DELFLAG,C'N'        DO NOT DISPLAY DELETED CMMLS                 
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMMLS                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*NOP     MVC   PRVPATDA,GLOBDA     SAVE LAST PATTERN WITH CMMLS!                
*        MVC   PRVPATMD,SVBAGM                                                  
*        MVC   PRVPATCL,BCLT                                                    
******   J     PATU310                                                          
*                                                                               
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLT/PRD WITH SAME                       
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
*                                                                               
PATU295  MVI   ERRFLG,0                                                         
         BRAS  RE,CHKOV                                                         
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*===================================================================            
* VALIDATE SPECIAL TEXT MUST BE DONE AFTER ANY POSSIBLE ERRORS                  
*===================================================================            
*                                                                               
PATU300  MVI   ERRFLG,0                                                         
         BRAS  RE,VSTX             GO VALIDATE SPECIAL TEXT (IF ANY)            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   IOKEY,SVKEY         RESTORE PATTERN KEY!                         
*                                                                               
PATU310  DS    0H                                                               
         L     RE,APATREC                                                       
         ST    RE,SVAREC                                                        
         BRAS  RE,AACTELEM         ADD ACTIVITY ELEMENT                         
*                                                                               
         NI    FLAGS,X'FF'-RECIOSW INIT RECORD I/O SWITCH                       
*                                                                               
         TM    FLAGS,DELPATSW      DELETE PATTERN                               
         JO    PATU348                                                          
*                                                                               
         CLI   SVPROF10,C'R'       AUTO TURNAROUND REMOTE                       
         JE    PATU320             YES                                          
         CLI   SVPROF10,C'D'       AUTO TURNAROUND                              
         JE    PATU320             NO                                           
         MVC   CHREASON,=C'NA'                                                  
         J     *+10                                                             
PATU320  MVC   CHREASON,=C'TA'     SET UP AS MAINT ADD                          
         CLI   RECACT,C'A'         IS THIS AN ADD                               
         JE    PATU340             YES                                          
         MVI   CHREASON+1,C'C'                                                  
         CLI   NEWSUBSW,0          IS A NEW REC CALLED FOR                      
         JE    PATU340             NO                                           
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
*NOP     CLI   CONREC,C'B'         IS THIS BPAT                                 
*        JNE   PATU330                                                          
*        OI    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
*        CLI   ADIDFLAG,C'Y'                                                    
*        JE    *+8                                                              
******   NI    PATSTAT1,X'FF'-PATSADID  UNSET FLAG                              
*                                                                               
PATU330  DS    0H                                                               
         OC    PATUSED,PATUSED     HAS THIS REC BEEN USED                       
         JNZ   *+12                                                             
         MVI   NEWSUBSW,0          NO, RESET NEW SUBLINE NEEDED                 
         J     PATU340             NO, NO NEW SUBLINE NEEDED                    
*                                                                               
         XC    PATUSED,PATUSED     CLEAR USED DATE FLD                          
*                                                                               
         NI    PATSTAT,X'FF'-X'08'-X'02'-X'01' SET OFF COMML TEXT,              
*                                         PATTERN SPECIAL TEXT CHANGE,          
*                                         USED IN JWT BOOKLET FLAGS             
         LH    R1,BSUB             GET SUBLINE                                  
         LA    R1,1(,R1)           ADD 1                                        
         STH   R1,BSUB             AND SAVE                                     
         X     R1,=XL4'000003FF'   GET 1'S COMPLEMENT                           
         LH    R0,BREF             GET REF                                      
         X     R0,=XL4'00003FFF'   COMPLEMENTED                                 
         SLL   R1,22                                                            
         SLDL  R0,10               COMBINE WITH REF                             
         STCM  R0,7,BREFSUB                                                     
*                                                                               
         L     R4,APATREC                                                       
         USING PATKEY,R4                                                        
         STCM  R0,7,PATKREF                                                     
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),PATKEY                                                 
*                                 NOW ADD REC WITH NEW SUBLINE                  
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOTRFFIL+#PATREC'                    
         OI    FLAGS,RECIOSW      DONE RECORD I/O SWITCH                        
         BRAS  RE,GENR             NOW ADD REQ FOR T/A                          
         J     PATU348                                                          
*                                                                               
PATU340  MVC   IOKEY(L'SVKEY),SVKEY  RESTORE ORIGINAL PAT KEY                   
*                                                                               
         CLI   RECACT,C'A'         IF THIS IS AN ADD, BYPASS                    
         JE    PATU350                                                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO5'                            
         CLC   IOKEY(13),IOKEYSAV  MUST HAVE FOUND REC                          
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO5             READ ORIGINAL RECORD                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO5'                           
*                                                                               
         L     RE,APATREC                                                       
         CLC   13(2,RE),13(R4)     SAME LENGTH                                  
         JNE   PATU345              NO, MUST BE A CHANGE                        
         LR    R0,R4                                                            
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         SH    R1,=H'20'           DON'T COMPARE ACTIVITY ELEM                  
         LR    RF,R1                                                            
         CLCL  RE,R0                                                            
         JE    PATU348                                                          
*                                                                               
PATU345  MVI   ELCODE,X'10'                                                     
         L     R4,APATREC                                                       
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
         NI    PATSTAT,X'FF'-X'02'  SET OFF PATTERN BOOKLET SW                  
*                                                                               
PATU348  TM    FLAGS,DELPATSW      DELETE PATTERN REQUESTED                     
         JO    PATU365                                                          
*                                                                               
PATU350  MVI   ELCODE,X'30'        PTTN CMML ELEMS                              
*                                                                               
*NOP     CLI   CONREC,C'B'         TEST BPAT                                    
*        JNE   PATU355             NO                                           
******   MVI   ELCODE,X'31'                                                     
*                                                                               
*        L     R4,APATREC                                                       
*        CLI   RECACT,C'A'         IF THIS IS NOT AN ADD                        
*        JNE   *+12                THEN BPAT FLAG IS ALREADY ON                 
*        OI    15(R4),X'01'        TURN ON BPAT RECORD                          
******** OI    IOKEY+13,X'01'                                                   
*                                                                               
PATU355  L     R4,APATREC                                                       
         BRAS  RE,GETEL            COMMERCIAL LIST ELEMENT                      
         JE    PATU360                                                          
         DC    H'0'                THIS SHOULD NOT BE                           
*                                                                               
* CML FOUND, RECORD IS COMPLETE                                                 
*                                                                               
PATU360  TM    IOKEY+13,X'02'      IS INCOMPLETE FLAG STILL ON                  
         JZ    PATU365             NO, BYPASS WRT                               
         L     R4,APATREC                                                       
         NI    15(R4),X'FF'-X'02'  TURN OFF INCOMPLETE RECORD                   
         NI    IOKEY+13,X'FF'-X'02' AND IN KEY                                  
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
         NI    PATSTAT1,X'FF'-PATSINC  RESET INCOMPLETE STAT IN ELEM            
*                                                                               
         CLI   RECACT,C'A'                                                      
         JE    PATU380                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOTRFDIR+#PATREC'                       
*                                                                               
PATU365  CLI   RECACT,C'X'         DELETE                                       
         JE    PATU370                                                          
         CLI   RECACT,C'R'         RESTORE                                      
         JE    PATU370                                                          
         CLI   RECACT,C'C'         CHANGE                                       
         JNE   PATU380              NO, MUST BE ADD                             
PATU370  CLI   NEWSUBSW,0          UNLESS ALREADY DONE                          
         JNE   EXITY                                                            
*                                                                               
         TM    FLAGS,RECIOSW       RECORD ALREADY CHANGED?                      
         JO    EXITY                                                            
*                                                                               
         BRAS  RE,PUT              ADD AUTO T/A REQUEST IF NEEDED               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+#PATREC'                    
         JNE   CLTIODIE                                                         
         J     EXITY                                                            
*                                                                               
PATU380  CLI   RECACT,C'A'         ADD                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,RECIOSW       RECORD ALREADY ADDED?                        
         JO    PATU390                                                          
*                                                                               
         MVI   NEWSUBSW,0          SET OFF NEW SUBLINE NEEDED SW                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOTRFFIL+#PATREC'                    
         JNE   CLTIODIE                                                         
*                                                                               
         BRAS  RE,GENR             GENERATE AUTO T/A                            
*                                                                               
PATU390  ICM   R1,3,SVBREF                                                      
         EDIT  (R1),(L'REFNUM,REFNUM),ALIGN=LEFT                                
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',002)                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',001),         +        
               ('LD_CHARQ',QCLTA),(L'QCLTA,0)                                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',002),         +        
               ('LD_CHARQ',REFNUM),(L'REFNUM,0)                                 
         J     EXITY               All done                                     
                                                                                
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
GOTIMVAL NTR1  BASE=*,LABEL=*                                                   
*        ICM   R0,8,BYTE                                                        
         LLC   R0,BYTE                                                          
         GOTOR VTIMVAL,DMCB,((R0),0(R2)),FULL                                   
         CLI   0(R1),X'FF'                                                      
         JNE   EXITY                                                            
*                                                                               
         LA    RF,L'INVTIME        NO END TIME WITHOUT START TIME               
         LA    R1,INVTIME                                                       
         J     ERR                 ERROR                                        
*                                                                               
*                                                                               
*==================================================================             
* TEST FOR GENUINE RECORD CHANGE - IF SO                                        
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*==================================================================             
*                                                                               
PUT      NTR1  BASE=*,LABEL=*                                                   
         L     R2,APATREC                                                       
         MVC   IOKEY,0(R2)                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO5'                            
         CLC   IOKEY(13),IOKEYSAV  MUST HAVE FOUND REC                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO5'                           
         L     R4,AIO5                                                          
         CLC   0(24,R2),0(R4)      COMPARE START OF 2 RECS FOR CHANGE           
         JNE   PUT20               IF DIFFERENT, GEN REQ                        
         LA    R2,24(,R2)                                                       
         LA    R4,24(,R4)                                                       
         SR    R1,R1                                                            
         ICM   R1,3,13(R2)         GET LEN                                      
         AHI   R1,-46              DON'T COMPARE ACT ELEM                       
*                                                                               
PUT10    CHI   R1,256              SEE IF REC LENGTH MORE THAN 256              
         JNH   PUT14                                                            
         CLC   0(256,R2),0(R4)                                                  
         JNE   PUT20                                                            
         LA    R2,256(,R2)                                                      
         LA    R4,256(,R4)                                                      
         AHI   R1,-256                                                          
         J     PUT10                                                            
*                                                                               
PUT14    EX    R1,PUTCLC                                                        
         JNE   PUT20               GEN REQ                                      
         J     PUTX                                                             
*                                                                               
PUT20    BRAS  RE,GENR             GO GENERATE T/A REPORT REQ                   
*                                                                               
PUTX     J     EXITY                                                            
*                                                                               
PUTCLC   CLC   0(0,R2),0(R4)                                                    
*                                                                               
*                                                                               
*                                                                               
*=============================================================                  
* VALIDATE MARKET/STATION LIST                                                  
* TYPE OF LIST CAN BE A = ALL MARKETS                                           
*                     M = MARKET LIST                                           
*                     S = STATION LIST                                          
*                     G = MARKET GROUP                                          
*=============================================================                  
*                                                                               
VMS      NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PATLSTEL,R4                                                      
         MVI   PATLSTEL,X'20'      ELEMENT IDENTIFIER                           
*                                                                               
         ICM   R2,15,QMKTIND                                                    
         JZ    VMS02                                                            
*                                                                               
         MVI   PATLSTTY,C'M'                                                    
*                                                                               
         USING LW_D,R2                                                          
         CLC   =C'ALL',LW_DATA2    MKT=ALL                                      
         JNE   VMS03                                                            
*                                                                               
         XC    PATLST(5),PATLST                                                 
         MVI   PATLSTLN,8                                                       
         J     VMSX                GO ADD ELEMENT                               
*                                                                               
         DROP  R2                                                               
*                                                                               
VMS02    ICM   R2,15,QSTAIND                                                    
         JZ    VMS04                                                            
*                                                                               
         MVI   PATLSTTY,C'S'                                                    
*                                                                               
*                                                                               
VMS03    MVI   ERRFLG,0                                                         
         BRAS  RE,VCOM                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         J     EXITY                                                            
*                                                                               
* MGROUP                                                                        
VMS04    ICM   R2,15,QMGRPIND      Any mgroup entered                           
         JZ    VMS10                                                            
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VMGR                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
VMSX     J     EXITY                                                            
*                                                                               
VMS10    LA    RF,L'MISSTYP                                                     
         LA    R1,MISSTYP                                                       
         J     ERR                                                              
*                                                                               
*                                                                               
*==================================                                             
* VALIDATE MARKETS/STATIONS                                                     
*==================================                                             
*                                                                               
VCOM     NTR1                                                                   
         LA    R3,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
*                                                                               
         CLI   PATLSTTY,C'M'       MARKET LIST PATTERN                          
         JNE   VCOM04                                                           
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,AMKT           LIST OF MARKETS                              
         USING LW_D,R2                                                          
         MVC   NUMMKT,LW_NUMN-LW_D(R2)                                          
*                                                                               
         CLI   NUMMKT+1,35                                                      
         JH    ERR00681            TOO MANY ENTRIES                             
*                                                                               
         AHI   R2,LW_LN2Q          POINT TO THE FIRST MKT IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMMKT         # OF MKTS                                    
         STC   R0,LOOPCT           # OF MKTS LEFT TO PROCESS                    
         J     VCOM32                                                           
*                                                                               
VCOM04   CLI   PATLSTTY,C'S'       STATION LIST PATTERN                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,ASTATION       LIST OF STATIONS                             
         USING LW_D,R2                                                          
         MVC   NUMSTA,LW_NUMN-LW_D(R2)                                          
*                                                                               
         CLI   NUMSTA+1,35                                                      
         JH    ERR00681            TOO MANY ENTRIES                             
*                                                                               
         AHI   R2,LW_LN2Q          POINT TO THE FIRST STA IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMSTA         # OF STAS                                    
         STC   R0,LOOPCT           # OF STAS LEFT TO PROCESS                    
         J     VCOM16                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
VCOM10   STC   R0,LOOPCT           # OF MKT/STA LEFT TO PROCESS                 
         CLI   PATLSTTY,C'M'       MARKET LIST PATTERN                          
         JE    VCOM32                                                           
*                                                                               
         CLI   PATLSTTY,C'S'       STATION LIST PATTERN                         
         JE    VCOM16                                                           
         DC    H'0'                                                             
*                                                                               
VCOM16   MVI   LP_VPARM,$VALSPAK                                                
         GOTOR (#VALSTA,AVALSTA),DMCB,0(R2),L'QSTA,WORK                         
         JE    VCOM20                                                           
*                                                                               
         LA    R1,INVSTA                                                        
         MVC   0(5,R1),0(R2)                                                    
         LA    RF,L'INVSTA                                                      
         LA    R1,INVSTA                                                        
         J     ERR                                                              
*                                                                               
VCOM20   MVC   QSTA,WORK+(STAPQSTA-STAPACKD)                                    
*                                                                               
*****    GOTO1 VALISTA                                                          
*                                                                               
         MVC   0(5,R3),QSTA                                                     
         J     VCOM40                                                           
*                                                                               
VCOMVN   MVN   DUB(0),0(R2)                                                     
VCOMCLC  CLC   DUB(0),0(R2)                                                     
*                                                                               
* MARKET                                                                        
VCOM32   MVC   DUB,EZEROS                                                       
*                                                                               
         LA    RF,0(R2)            POINT TO START OF ENTRY                      
         LA    RE,3                      ENTRY LEN-1                            
         BRAS  R1,FLEN             FIND LEN OF ENTRY-1                          
         STC   RE,INPLEN           INPUT LEN-1                                  
*                                                                               
         EX    RE,VCOMVN                                                        
         EX    RE,VCOMCLC                                                       
         JE    VCOM32C                                                          
*                                                                               
         LA    R1,INVMKT                                                        
         MVC   0(4,R1),0(R2)                                                    
         LA    RF,L'INVMKT                                                      
         LA    R1,INVMKT                                                        
         J     ERR                                                              
*                                                                               
VCOM32C  MVI   ERRFLG,0                                                         
         BRAS  RE,VALMKT                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         XC    0(3,R3),0(R3)                                                    
         MVC   3(2,R3),BMKT        MOVE MKT TO ELEM                             
*                                                                               
VCOM40   LA    R3,5(,R3)           BUMP IN ELEM                                 
         LA    RE,L'QMKT                                                        
         CLI   PATLSTTY,C'M'       MARKET LIST PATTERN                          
         JE    VCOM55                                                           
         LA    RE,L'QSTA                                                        
         CLI   PATLSTTY,C'S'       STATION LIST PATTERN                         
         JE    VCOM55                                                           
         DC    H'0'                                                             
VCOM55   AR    R2,RE               BUMP TO NEXT INDEX                           
         IC    R0,LOOPCT           # OF STATION LEFT TO PROCESS                 
         JCT   R0,VCOM10                                                        
*                                                                               
         LA    R0,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
         CR    R0,R3                                                            
         JNL   MISSERRT                                                         
         SR    R3,R4                                                            
         STC   R3,PATLSTLN                                                      
         LR    R1,R3               MOVE ELEM LENGTH                             
         SHI   R1,3                GET ENTRIES LEN ONLY                         
         SR    R0,R0               FOR DIVIDE                                   
         D     R0,=F'5'            DIVIDE BY ENTRY LEN                          
         LTR   R0,R0                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
VCOM60   J     VMSX                                                             
*                                                                               
*                                                                               
*--------------------------------------------                                   
* VALIDATE MARKET GROUP LIST                                                    
*--------------------------------------------                                   
*                                                                               
VMGR     NTR1                                                                   
*                                                                               
         MVI   PATLSTTY,C'G'                                                    
         LA    R3,PATLST           SLOT FOR 1ST MKT GRP IN ELEM                 
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,AMGRP          LIST OF MARKET GROUPS                        
         USING LW_D,R2                                                          
         MVC   NUMMGR,LW_NUMN-LW_D(R2)                                          
*                                                                               
         CLI   NUMMGR+1,25                                                      
         JH    ERR00681            TOO MANY ENTRIES                             
*                                                                               
         AHI   R2,LW_LN2Q          POINT TO THE FIRST MGR IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMMGR         # OF MGRS                                    
         STC   R0,LOOPCT           # OF MGRS LEFT TO PROCESS                    
         J     *+8                                                              
VMGR20   DS    0H                                                               
         STC   R0,LOOPCT                                                        
         CLI   0(R2),C'A'                                                       
         JL    MGRENTER                                                         
         CLI   0(R2),C'Z'                                                       
         JH    MGRENTER                                                         
*                                                                               
VMGR10   MVC   SVMGRP,0(R2)        MARKET GROUP (EX. BJ0001)                    
         MVC   SVMGID,0(R2)        MARKET GROUP ID                              
*                                                                               
         LA    RF,0(R2)            POINT TO START OF ENTRY                      
         LA    RE,L'SVMGRP-1       ENTRY LEN-1                                  
         BRAS  R1,FLEN             FIND LEN OF ENTRY-1                          
         STC   RE,INPLEN           INPUT LEN-1                                  
*                                                                               
         LA    RE,DUB                                                           
         MVC   DUB,EZEROS                                                       
*                                                                               
         LA    R1,1(R2)                                                         
         CLI   0(R1),C'A'          SEE IF 2 CHAR MARKET GROUP                   
         JL    VMGR30                                                           
         CLI   0(R1),C'Z'                                                       
         JH    VMGR30                                                           
         LA    R1,1(R1)                                                         
*                                                                               
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
*                                                                               
         LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VMGR22   CLC   0(2,R2),0(RE)      IS THIS IT                                    
         JE    VMGR24                                                           
         LA    RE,3(RE)                                                         
         JCT   RF,VMGR22                                                        
         J     MGRSNFND                                                         
*                                                                               
VMGR24   MVC   SVMGID,2(RE)        MOVE HEX VALUE FROM TABLE                    
*                                                                               
         LA    RE,DUB                                                           
*                                                                               
         LLC   RF,INPLEN           INPUT LEN-1                                  
         SHI   RF,1                MINUS 2 FOR MGRP ID                          
         STC   RF,INPLEN                                                        
*                                                                               
VMGR30   LLC   RF,INPLEN                                                        
*                                                                               
VMGR31   CLI   0(R1),C'0'                                                       
         JL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         JH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)       MOVE DIGIT TO DUB                            
         LA    R1,1(,R1)           BUMP IN INPUT FIELD                          
         LA    RE,1(,RE)           AND IN DUB                                   
         JCT   RF,VMGR31                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   0(1,R3),SVMGID      MARKET GROUP ID                              
         MVC   1(2,R3),WORK                                                     
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0D82'                                                
         MVC   IOKEY+2(3),SVBAGM A/M/CLT                                        
         MVC   IOKEY+8(3),0(R3)                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
         OC    IOKEY+5(3),IOKEY+5  FIND PRODUCT GROUP                           
         JZ    VMGR36                                                           
         CLC   IOKEY(5),IOKEYSAV                                                
         JE    VMGR34                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         XC    IOKEY+3(2),IOKEY+3  TRY ALL CLIENT                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
         CLC   IOKEY(5),IOKEYSAV                                                
         JNE   NOMGRPER                                                         
*                                                                               
VMGR34   CLC   IOKEY+8(3),0(R3)                                                 
         JE    VMGR36                                                           
         MVC   IOKEY+8(3),0(R3)                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
*                                                                               
VMGR36   CLC   IOKEY(11),IOKEYSAV  FIND A MARKET GROUP                          
         JE    VMGR38                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         XC    IOKEY+3(2),IOKEY+3  TRY ALL CLIENT                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
*                                                                               
VMGR38   CLC   IOKEY(11),IOKEYSAV  FIND A MARKET GROUP                          
         JNE   NOMGRPER                                                         
*                                                                               
         LA    R2,L'SVMGRP(R2)                                                  
         LA    R3,5(,R3)                                                        
         IC    R0,LOOPCT           # OF STATION LEFT TO PROCESS                 
         JCT   R0,VMGR20                                                        
*                                                                               
VMGR50   DS    0H                                                               
*                                                                               
         LA    R0,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
         CR    R0,R3                                                            
         JNL   MISSERRT                                                         
         SR    R3,R4                                                            
         STC   R3,PATLSTLN                                                      
         LR    R1,R3               MOVE ELEM LENGTH                             
         SHI   R1,3                GET ENTIRES LEN ONLY                         
         SR    R0,R0               FOR DIVIDE                                   
         D     R0,=F'5'            DIVIDE BY ENTRY LEN                          
         LTR   R0,R0                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VMGR60   J     VMSX                GO ADD ELEM                                  
*                                                                               
MISSERRT LA    RF,L'MISSMS                                                      
         LA    R1,MISSMS                                                        
         J     ERR                                                              
*                                                                               
*                                                                               
NOMGRPER LA    R1,MGRNFND                                                       
         MVC   0(5,R1),0(R2)                                                    
         LA    RF,L'MGRNFND                                                     
         LA    R1,MGRNFND                                                       
         J     ERR                                                              
*                                                                               
MGRENTER LA    R1,INVMGR                                                        
         MVC   0(6,R1),0(R2)                                                    
         LA    RF,L'INVMGR                                                      
         LA    R1,INVMGR                                                        
         J     ERR                                                              
*                                                                               
MGRSNFND LA    RE,MGRSNF                                                        
         MVC   17(2,RE),0(R2)                                                   
         LA    RF,L'MGRSNF                                                      
         LA    R1,MGRSNF                                                        
         J     ERR                                                              
*                                                                               
*************************************************************                   
* ROUTINE TO VALIDATE MARKET FOR BUY DOWNLOAD                                   
*************************************************************                   
*                                                                               
VALMKT   NTR1  BASE=*,LABEL=*                                                   
         IC    RF,INPLEN           INPUT LEN-1                                  
         BASR  RE,0                                                             
         PACK  DUB,0(0,R2)                                                      
         EX    RF,0(RE)                                                         
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         LA    R1,IOKEY                                                         
         USING MKTRECD,R1                                                       
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,LP_AGY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO6'                            
         JNE   ERR00689            BAD MARKET/STATION                           
         J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
*                                                                               
*===========================================================                    
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*===========================================================                    
*                                                                               
GENR     NTR1  BASE=*,LABEL=*                                                   
         J     EXITY    <<<<<< NOP THIS CODE FOR NOW.                           
*                       <<<<<< NOP THIS CODE FOR NOW.                           
*&&DO                                                                           
*NOP                                                                            
*        CLI   CONREC,C'B'         BPAT RECORD                                  
*NOP     JE    EXITY               NO T/A                                       
*                                                                               
         CLI   SVPROF10,C'R'       AUTO TURNAROUND REMOTE                       
         JE    GENR10              YES                                          
         CLI   SVPROF10,C'D'       AUTO TURNAROUND                              
         JNE   EXITY               NO                                           
*                                                                               
GENR10   XC    REQHDR(26),REQHDR                                                
         MVC   REQUEST(80),SPACES                                               
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),LP_AGY   Agency alpha                               
         MVC   REQUEST+4(23),=CL23'*.PAT.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMEDA                                              
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLTA                                              
         MVI   REQUEST+32,C'.'                                                  
         MVC   REQUEST+33(3),QPROD1                                             
         LA    R1,REQUEST+36                                                    
         CLI   QPRDX2,0                                                         
         JE    GENR20                                                           
*                                                                               
         MVI   0(R1),C'.'                                                       
         MVC   1(3,R1),QPROD2                                                   
         LA    R1,4(,R1)                                                        
*                                                                               
GENR20   MVC   0(2,R1),=C'.*'                                                   
         CLI   SVPROF10,C'D'                                                    
         JE    *+10                                                             
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                   
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
*&&                                                                             
*                                                                               
*                                                                               
*===============================================================                
* VALIDATE COMMERCIAL ROTATION                                                  
*===============================================================                
*                                                                               
VROT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* BUILD A TABLE IN VALTBL OF VALID COMMERCIAL LETTERS                           
* X'00'= INVALID X'01'=OK C'D'=DELETED                                          
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'30'        GET CMML ELEM                                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R4)                                                         
         SRL   R0,4                GIVES NUMBER OF CMMLS PRESENT                
         LA    R4,2(R4)                                                         
*                                                                               
         XC    VALTBL,VALTBL                                                    
         LA    R1,VALTBL                                                        
*                                                                               
VROT2    MVI   0(R1),1             ASSUME VALID                                 
         CLC   =X'5C00',0(R4)      TEST DELETED CMML                            
         JNE   *+8                                                              
         MVI   0(R1),C'D'                                                       
         LA    R4,16(R4)           NEXT CMML                                    
         LA    R1,1(R1)                                                         
         JCT   R0,VROT2                                                         
*                                                                               
VROT10   OC    QROT,QROT           ROTATION                                     
         JZ    VROT11                                                           
         CLI   PCTTOT,0            HAVE ROT - TEST ANY PCTS INPUT               
         JE    VROT12              NO - GO VALIDATE ROTATION                    
         J     ERR00768            EITHER ROTATION OR PCTS                      
*                                                                               
VROT11   CLI   PCTTOT,0            NO ROT INPUT, SHOULD HAVE PCTS               
         JNE   VROT30              GO BUILD ROTATION ELEMENT                    
         J     ERR00768            EITHER ROTATION OR PCTS                      
*                                                                               
VROT12   DS    0H                                                               
         LA    RF,QROT             FIND INPUT LEN                               
         LA    RE,L'QROT-1                                                      
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         LR    R0,RE               LEN                                          
         LA    R1,QROT                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+2                                                        
*                                                                               
VROT14   LLC   RE,0(R1)            GET INPUT CHAR                               
         AHI   RE,-192             A=192 =1                                     
         CHI   RE,9                                                             
         JNH   *+8                                                              
         AHI   RE,-7               J=209 = 10                                   
         LA    RF,VALTBL-1(RE)                                                  
*                                                                               
         CLI   0(RF),0             IF 0, INVALID CHAR                           
         JE    ERR00769                                                         
         CLI   0(RF),C'D'          TEST DELETED                                 
         JE    VROT16                                                           
*                                                                               
         LLC   RE,0(RF)            GET VALID CMML FLAG                          
         LA    RE,1(RE)            BUMP USAGE COUNT                             
         STC   RE,0(RF)                                                         
         MVC   0(1,R4),0(R1)       MOVE LETTER TO ELEM                          
         LA    R4,1(R4)            BUMP OUTPUT POSN                             
*                                                                               
VROT16   LA    R1,1(R1)                                                         
         JCT   R0,VROT14                                                        
*                                                                               
* MAKE SURE ALL CMMLS HAVE BEEN INPUT                                           
*                                                                               
         LA    R1,VALTBL                                                        
         LA    R0,15                                                            
VROT20   CLI   0(R1),1             TABLE SHOULD NOT BE 1 ANYMORE                
         JNE   VROT22                                                           
         LA    RF,L'MISSROT                                                     
         LA    R1,MISSROT                                                       
         J     ERR                 ERROR                                        
VROT22   LA    R1,1(R1)                                                         
         JCT   R0,VROT20                                                        
*                                                                               
         LR    R1,R4               END OF ELEM                                  
         LA    R4,ELEM             INSERT ROTATION ELEMENT IN RECORD            
         USING PATPTNEL,R4                                                      
*                                                                               
         MVI   PATPTNEL,X'32'                                                   
         LA    R0,ELEM                                                          
         SR    R1,R0                                                            
         STC   R1,PATPTNLN                                                      
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,APATREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
VROTX    XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
*                                                                               
*================================================================               
* BUILD ROTATION ELEMENT FROM INPUT PERCENTAGES                                 
*================================================================               
*                                                                               
VROT30   L     R4,APATREC                                                       
         MVI   ELCODE,X'34'        GET PCT ELEMENT PREVIOUSLY ADDED             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
* FIND MIN/MAX PCTS                                                             
         LA    R0,255                                                           
         ST    R0,MINPCT                                                        
         XC    MAXPCT,MAXPCT                                                    
*                                                                               
         XC    PCTTBL,PCTTBL                                                    
         LLC   RE,1(R4)                                                         
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   PCTTBL(0),2(R4)     MOVE PCTS TO TABLE AREA                      
*                                                                               
         LLC   R0,1(R4)                                                         
         AHI   R0,-2                                                            
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R1                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,PCTCT            SAVE NUMBER OF PERCENTS                      
         LA    R1,PCTTBL           POINT TO FIRST PCT                           
*                                                                               
VROT32   SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         C     RE,MINPCT                                                        
         JNL   *+8                                                              
         ST    RE,MINPCT                                                        
         C     RE,MAXPCT                                                        
         JNH   *+8                                                              
         ST    RE,MAXPCT                                                        
         LA    R1,3(R1)                                                         
         JCT   R0,VROT32                                                        
*                                                                               
         BAS   RE,DIV              GET DIVISOR                                  
         JE    VROT40                                                           
*                                                                               
         CLI   SVT2PR3,C'Y'                                                     
*NOP     JNE   ERR00638                                                         
         JNE   ABSERR                                                           
*                                                                               
         BAS   RE,ABSEL                                                         
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LLC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,VROTMVC                                                       
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,APATREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
* NOW BUILD TABLE OF LETTERS AND NUMBERS TO BUILD ROTATION *                    
*                                                                               
VROT40   XC    WORK,WORK                                                        
         LLC   R0,PCTCT                                                         
         LA    R3,PCTTBL                                                        
         LA    R4,WORK+2                                                        
         L     R2,COMDIV                                                        
         SR    R1,R1                                                            
         SR    RF,RF                                                            
*                                                                               
VROT60   MVC   0(1,R4),0(R3)       SAVE LETTER                                  
         ICM   RF,3,1(R3)                                                       
         SR    RE,RE                                                            
         DR    RE,R2                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         JZ    VROT66                                                           
         DC    H'0'                                                             
*                                                                               
VROT66   STCM  RF,3,1(R4)                                                       
         AR    R1,RF                                                            
         LA    R3,3(,R3)                                                        
         LA    R4,3(,R4)                                                        
         JCT   R0,VROT60                                                        
*                                                                               
         STH   R1,WORK             ROTATION TABLE SIZE                          
         CHI   R1,60               MAX ROT LEN                                  
         JNL   ABSERR                                                           
*                                                                               
VROT68   LR    R3,R1                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'32'                                                       
         LA    R0,2(,R3)                                                        
         STC   R0,ELEM+1                                                        
         LA    R4,ELEM+2                                                        
*                                                                               
VROT70   LLC   R0,PCTCT            GET NUMBER OF ENTRIES                        
         LA    R1,WORK+2                                                        
*                                                                               
VROT74   SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         JZ    VROT76                                                           
         MVC   0(1,R4),0(R1)                                                    
         BCTR  RE,0                                                             
         STCM  RE,3,1(R1)                                                       
         LA    R4,1(R4)                                                         
*                                                                               
VROT76   LA    R1,3(R1)                                                         
         JCT   R0,VROT74                                                        
         JCT   R3,VROT70                                                        
*                                                                               
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         L     R7,APATREC                                                       
         BRAS  RE,ADDEL                                                         
         J     EXITY                                                            
*                                                                               
*                                                                               
ABSERR   LA    RF,L'ABSMSG                                                      
         LA    R1,ABSMSG                                                        
         J     ERR                                                              
*                                                                               
*                                                                               
*=============================================================                  
* DEVELOP DIVISOR TO BUILD ROTATION TABLE                                       
*=============================================================                  
*                                                                               
DIV      NTR1                                                                   
         SR    RE,RE               SEE IF MAX DIVIDES BY MIN                    
         L     RF,MAXPCT                                                        
         D     RE,MINPCT                                                        
         MVC   COMDIV,MINPCT                                                    
         LTR   RE,RE               IF A REMAINDER, NG                           
         JNZ   DIV10                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EXITY                                                            
*                                                                               
* SEE IF MAX DIVIDES BY MIN/2 *                                                 
*                                                                               
DIV10    TM    COMDIV+3,1          IF ODD NUMBER, BYPASS                        
         JO    DIV14                                                            
         SR    RE,RE                                                            
         L     RF,MAXPCT                                                        
         L     R0,COMDIV                                                        
         SRL   R0,1                DIVIDE BY 2                                  
         ST    R0,COMDIV                                                        
         DR    RE,R0                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         JNZ   DIV14                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EXITY                                                            
*                                                                               
* DIVIDE BY SOME PRIME NOS & SEE IF RESULT IS COMMON DIVISOR *                  
*                                                                               
DIV14    LA    R2,PRIMETBL         TRY LIMITED PRIME NUMBERS                    
*                                                                               
DIV20    LH    R0,0(R2)                                                         
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    DIV30                                                            
         LA    R2,2(R2)                                                         
         CLI   0(R2),255                                                        
         JNE   DIV20                                                            
         J     EXITN                                                            
*                                                                               
* NOW DOUBLE DIVISOR AND TRY AGAIN *                                            
*                                                                               
DIV30    L     R0,COMDIV                                                        
         AR    R0,R0                                                            
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    DIV30                                                            
         L     R0,COMDIV                                                        
         SRL   R0,1                                                             
         ST    R0,COMDIV                                                        
         J     EXITY                                                            
*                                                                               
*                                                                               
*===================================================================            
* BUILD AND ADD PERCENTAGE ROTATION ELEMENT *                                   
* DEVELOP DIVISOR TO BUILD REAL ROTATION TABLE FROM ABSURD PERCENT              
*                                                                               
* METHOD - DIVIDE EACH ENTRY BY 2, 3, 5, 11, ADDING REMAINDERS                  
* DIVIDE TOTAL REMAINDER BY PRIME, SAVE Q & R, USE SMALLEST #                   
*===================================================================            
*                                                                               
ABSEL    NTR1                                                                   
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
*                                                                               
ABSEL10  LLC   R3,PCTCT            GET NUMBER OF PCTS                           
         SR    R4,R4                                                            
         LA    R5,PCTTBL           AND POINT TO FIRST                           
*                                                                               
ABSEL20  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R5)                                                       
         D     RE,0(R1)                                                         
         AR    R4,RE               ADD REMAINDER                                
         LA    R5,3(R5)                                                         
         JCT   R3,ABSEL20                                                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R4               GET TOTAL REMAINDERS                         
         D     RE,0(R1)                                                         
         STC   RE,1(R2)            REMAINDER                                    
         STC   RF,0(R2)            QUOTIENT                                     
         LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         JCT   R0,ABSEL10                                                       
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
         LA    R3,2047                                                          
         SR    R4,R4                                                            
*                                                                               
ABSEL30  CLM   R3,3,0(R2)                                                       
         JNH   ABSEL34                                                          
         LR    R4,R1               SAVE ADDRESS OF LOWEST Q/R                   
         ICM   R3,3,0(R2)                                                       
*                                                                               
ABSEL34  LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         JCT   R0,ABSEL30                                                       
*                                                                               
         LTR   R4,R4                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   COMDIV,0(R4)                                                     
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
*                                                                               
ABSEL40  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R2)                                                       
         D     RE,0(R4)                                                         
         STCM  RF,3,1(R2)          STORE QUOTIENT                               
         ST    RE,0(R3)            & REMAINDER                                  
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         JCT   R0,ABSEL40                                                       
*                                                                               
* NOW MULTIPLY EACH BY PRIME #                                                  
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
ABSEL50  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         MH    RE,2(R4)                                                         
         STCM  RE,3,1(R2)          STORE RESULT                                 
         LA    R2,3(R2)                                                         
         JCT   R0,ABSEL50                                                       
*                                                                               
ABSEL60  LLC   R0,PCTCT            SEE IF SUM 99 OR 100                         
         LA    R2,PCTTBL                                                        
         SR    RF,RF                                                            
*                                                                               
ABSEL64  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         AR    RF,RE                                                            
         LA    R2,3(,R2)                                                        
         JCT   R0,ABSEL64                                                       
*                                                                               
         CHI   RF,100                                                           
         JE    EXITY                                                            
         CHI   RF,99                                                            
         JE    EXITY                                                            
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
         SR    R1,R1                                                            
*                                                                               
ABSEL70  C     R1,0(R3)                                                         
         JNL   ABSEL74                                                          
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         L     R1,0(R3)                                                         
*                                                                               
ABSEL74  LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         JCT   R0,ABSEL70                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(RE)                                                       
         A     R0,0(R4)                                                         
         STCM  R0,3,1(RE)                                                       
         XC    0(4,RF),0(RF)                                                    
         J     ABSEL60                                                          
*                                                                               
PRCTAB   DC    F'2',F'3',F'5',F'11'                                             
PRCTABLN EQU   (*-PRCTAB)/4                                                     
*                                                                               
*                                                                               
*=========================================================                      
* CHECK COMMON DIVISOR AGAINST ALL ENTRIES                                      
*=========================================================                      
*                                                                               
CKD      LLC   RF,PCTCT                                                         
         LA    R3,PCTTBL           GET A(FIRST PCT IN ELEM)                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
CKD10    ICM   R1,3,1(R3)                                                       
         D     R0,COMDIV                                                        
         LTR   R0,R0                                                            
         BNZR  RE                                                               
         LA    R3,3(R3)                                                         
         JCT   RF,CKD10                                                         
         CR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
VROTMVC  MVC   ELEM+2(0),PCTTBL                                                 
         USING PATPTNEL,R4                                                      
VROTMVCA MVC   PATPTN(0),WORK                                                   
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
PRIMETBL DC    H'5',H'3',H'2',H'7',H'11',H'13',H'17',H'19',H'23',H'29'          
         DC    H'31',H'37'                                                      
         DC    X'FF'                                                            
         DROP  RB                                                               
*                                                                               
*                                                                               
*=================================================================              
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLIENT PROD WITH SAME                   
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
* AND START TIME, END TIME, AND DAYPART                                         
* IF PATSTAT1, PATSDLY, MOVE A * TO DAYPART                                     
* AND TESTS ARE DIFFERENT                                                       
*=================================================================              
*                                                                               
CHKOV    NTR1  BASE=*,LABEL=*                                                   
         XC    MYPTNSTM(5),MYPTNSTM                                             
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R4                                                      
         CLI   PATDTALN,38         IS THIS AN OLD PAT LEN (NO STAT)             
         JE    CHKOV04             YES, NOT DELETED                             
         TM    PATSTAT,X'80'       IS THIS PATTERN DELETED                      
         JO    EXITY               YES, NO DATE OVERLAP CK NEEDED               
*                                                                               
CHKOV04  MVC   MYPTNSTM(5),PATSTIM   SAVE ST/END TIMES + DPT                    
         TM    PATSTAT1,PATSDLY      TEST DAILY TIMES IN NEW REC                
         JZ    *+8                                                              
         MVI   MYPTNDPT,C'*'       SET SPECIAL DPT AS DAILY TIME FLAG           
*                                                                               
         LA    R2,PATSTART         R2 = THIS REC DATES                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R4               R3 = THIS REC MARKET/STAT LIST               
         MVC   IOKEY(10),SVKEY                                                  
         XC    IOKEY+10(3),IOKEY+10                                             
*                                                                               
CHKOV10  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(10),IOKEYSAV  ANY SIMILAR                                  
         JNE   EXIT                NO                                           
*                                                                               
         L     R1,APATREC                                                       
         CLC   IOKEY(13),0(R1)     SAME KEY AS THIS REC                         
         JE    CHKOV60             YES, BYPASS THIS                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R4                                                      
         CLI   PATDTALN,38         IS THIS AN OLD PAT LEN (NO STAT)             
         JE    CHKOV12             YES, NOT DELETED                             
         TM    PATSTAT,X'80'       IS THIS PATTERN DELETED                      
         JO    CHKOV60             YES, NO DATE OVERLAP CK NEEDED               
*                                                                               
CHKOV12  ST    R4,NEW10EL          SAVE 10 ELEM ADDR                            
*                                                                               
CHKOV20  LA    R5,PATSTART         SAVE IF ERROR                                
         CLC   =C'ES',0(R2)        THIS 'ES' DATES FROM ESTIMATE                
         BE    CHKOV24                                                          
         CLC   0(3,R5),3(R2)       THIS START TO END                            
         BH    CHKOV60             CAN'T OVERLAP                                
         CLC   3(3,R5),0(R2)       THIS END TO START                            
         BL    CHKOV60             CAN'T OVERLAP                                
         MVI   SAMEDTS,C'N'                                                     
         CLC   0(6,R5),0(R2)       TEST SAME DATES                              
         BNE   *+8                                                              
         MVI   SAMEDTS,C'Y'        SET FLAG FOR SAME DATES                      
*                                                                               
CHKOV24  CLI   PATDPT,0            TEST FILE HAS DPT                            
         BE    CHKOV23             NO                                           
         OC    MYPTNSTM,MYPTNSTM   TEST NEW HAS TIME                            
         BZ    CHKOV26                                                          
         B     CHKOV23X                                                         
*                                                                               
CHKOV23  OC    PATSTIM,PATSTIM     TEST FILE HAS TIME                           
         BZ    CHKOV26                                                          
         CLI   MYPTNDPT,C'*'                                                    
         BE    CHKOV26                                                          
         CLI   MYPTNDPT,0          TEST NEW HAS DPT                             
         BE    CHKOV26                                                          
*                                                                               
CHKOV23X SR    R5,R5                                                            
         J     ERR00778                                                         
*                                                                               
CHKOV26  MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(1,R4),2(R3)       IF LIST TYPES DIFF, CAN'T OVERLAP            
         JNE   CHKOV60                                                          
         LLC   R0,1(R4)            GET ELEM LEN                                 
         AHI   R0,-3               GET LIST LENL                                
         LA    R1,3(R4)            POINT TO START OF LIST-OLD PATTN             
*                                                                               
CHKOV28  LLC   RE,1(R3)            GET ELEM LEN                                 
         AHI   RE,-3               GET LIST LEN                                 
         LA    RF,3(R3)            POINT TO START OF LIST-NEW PATTN             
*                                                                               
CHKOV30  CLI   2(R4),C'C'          IF LIST TYPE = COMBINED CK FURTHER           
         JNE   CHKOV32              NO, GO ON                                   
*                                                                               
         CLI   0(RF),0             THIS A MARKET                                
         JNE   CHKOV60              NO, AFFILIATE, DONE                         
         CLI   0(R1),0             THIS A MARKET                                
         JNE   CHKOV60              NO, AFFILIATE, DONE                         
*                                                                               
CHKOV32  CLC   0(5,R1),0(RF)       CK ENTRY FOR EQ                              
         JNE   CHKOV50              NO PROBLEM                                  
*                                                                               
         CLI   2(R4),C'C'          IF LIST TYPE = COMBINED CK FURTHER           
         JNE   CHKOV70                                                          
*                                                                               
CHKOV33  STM   RE,R1,FLD                                                        
*                                                                               
* NOW JUMP TO AFFILIATES, AND SEE IF EQUAL THERE *                              
*                                                                               
CHKOV34  CLI   0(R1),0             THIS AN AFFILIATE                            
         JNE   CHKOV36              YES                                         
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         JP    CHKOV34                                                          
         DC    H'0'                MUST BE AFFILIATES                           
*                                                                               
CHKOV36  CLI   0(RF),0             THIS AN AFFILIATE                            
         JNE   CHKOV40              YES                                         
         LA    RF,5(,RF)           BUMP THRU OLD PATTN                          
         AHI   RE,-5                                                            
         JP    CHKOV36                                                          
         DC    H'0'                MUST BE AFFILIATES                           
*                                                                               
CHKOV40  STM   RE,RF,FLD+16                                                     
         J     CHKOV46                                                          
*                                                                               
CHKOV44  LM    RE,RF,FLD+16                                                     
*                                                                               
CHKOV46  CLC   0(5,R1),0(RF)       CK ENTRY FOR EQ AFFILIATE                    
         JE    CHKOV70             OVERLAP ERROR                                
         LA    RF,5(RF)            GO THRU LIST                                 
         AHI   RE,-5               UNTIL                                        
         JP    CHKOV46             END                                          
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         JP    CHKOV44                                                          
*                                                                               
         LM    RE,R1,FLD           RESTORE REGS                                 
*                                                                               
CHKOV50  LA    RF,5(,RF)           GO THRU LIST                                 
         AHI   RE,-5               UNTIL                                        
         JP    CHKOV30             END                                          
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         JP    CHKOV28                                                          
*                                                                               
CHKOV60  LA    R4,IOKEY                                                         
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         AHI   R1,-1               BUILD NEXT KEY                               
         JZ    EXIT                IF ZERO, DONE                                
         X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
         J     CHKOV10                                                          
*                                                                               
*==============================================================                 
* PATTERNS OVERLAP - BUT CHECK FOR START/END TIMES OR DPT                       
* DATES MAY OVERLAP IF DAYPARTS ARE DIFFERENT                                   
*==============================================================                 
*                                                                               
CHKOV70  L     R4,NEW10EL                                                       
         USING PATDTAEL,R4                                                      
*                                                                               
         CLI   MYPTNDPT,C'*'       TEST NEW REC HAS DAILY TIMES                 
         JE    CHKOV72             YES                                          
         TM    PATSTAT1,PATSDLY    NO - TEST OLD REC HAS DAILY TIMES            
         JO    ERR00791            YES - ERROR                                  
         J     CHKOV80                                                          
*                                                                               
CHKOV72  TM    PATSTAT1,PATSDLY    TEST FILE HAS DAILY TIMES                    
         JZ    ERR00792                                                         
         J     CHKOV76                                                          
*                                                                               
CHKOV74  CLI   MYPTNDPT,C'*'       TEST NEW HAS DAILY TIMES                     
         JE    ERR00792                                                         
*                                                                               
CHKOV76  MVI   SAMEDTS,C'Y'        SET FLAG TO FORCE ERR IF OVLP                
         J     CHKOV84                                                          
*                                                                               
CHKOV80  CLI   PATDPT,0            TEST FILE HAS DPT                            
         JE    CHKOV82             NO                                           
         CLI   MYPTNDPT,0          YES - TEST NEW HAS DPT                       
         JE    CHKOV60             NO - OK                                      
         CLC   PATDPT,MYPTNDPT     SEE IF THEY MATCH                            
         JE    DTOVERR                                                          
         CLI   SAMEDTS,C'Y'        TEST PATTERNS HAVE SAME DATES                
         JNE   ERR00775            TOO BAD - THEY SHOULD                        
         J     CHKOV60                                                          
*                                                                               
CHKOV82  CLI   MYPTNDPT,0          NO DPT IN NEW, ANY IN OLD                    
         JNE   CHKOV60             YES - THAT'S OK                              
*                                                                               
         OC    PATSTIM(4),PATSTIM     TEST TIME IN OLD                          
         JZ    DTOVERR                NO                                        
         OC    MYPTNSTM(4),MYPTNSTM   TIME IN OLD, TEST TIME IN NEW             
         JZ    DTOVERR                NO - THAT'S AN ERROR                      
*                                                                               
CHKOV84  MVC   MYADJSTM(4),MYPTNSTM   MOVE MY START/END TIMES                   
         MVC   MYADJSDT(6),0(R2)      MOVE MY START/END DATES                   
*                                                                               
         MVC   PTADJSTM(4),PATSTIM    MOVE FILE START/END TIMES                 
         MVC   PTADJSDT(6),0(R5)      MOVE FILE START/END DATES                 
*                                                                               
         LA    R0,4                                                             
         LA    R1,MYADJSTM                                                      
         LA    RF,MYADJSDT                                                      
*                                                                               
         BAS   RE,ADJTIM              ADJUST TO 24 HR CLOCK                     
         LA    R1,2(R1)                                                         
         LA    RF,3(RF)                                                         
         JCT   R0,*-12                                                          
*                                                                               
CHKOV86  CLI   MYPTNDPT,C'*'       TEST DAILY TIMES                             
         JNE   CHKOV87                                                          
         CLC   MYADJETM,PTADJSTM      THEN IT'S OK TO END BEFORE                
         JL    CHKOV60                                                          
         CLC   MYADJSTM,PTADJETM      OR TO START AFTER                         
         JH    CHKOV60                                                          
         J     DTOVERR                                                          
*                                                                               
CHKOV87  CLC   MYADJSDT,PTADJEDT      MY START DATE = FILE END DATE             
         JNE   CHKOV88                NO                                        
         CLC   MYADJSTM,PTADJETM      THEN I SHOULD START AFTER                 
         JH    CHKOV60                                                          
         CLC   PATSTART,PATEND        TEST PATTERN ALL ON 1 DAY                 
         JNE   DTOVERR                                                          
         CLC   MYADJETM,PTADJSTM      THEN IT'S OK TO END BEFORE                
         JL    CHKOV60                                                          
         J     DTOVERR                                                          
*                                                                               
CHKOV88  CLC   MYADJEDT,PTADJSDT      MY END DATE = FILE START DATE             
         JNE   CHKOV90                                                          
         CLC   MYADJETM,PTADJSTM      THEN I SHOULD END BEFORE                  
         JL    CHKOV60                                                          
         CLC   PATSTART,PATEND        TEST PATTERN ALL ON 1 DAY                 
         JNE   DTOVERR                                                          
         CLC   MYADJSTM,PTADJETM      THEN IT'S OK TO START AFTER               
         JL    CHKOV60                                                          
         J     DTOVERR                                                          
*                                                                               
CHKOV90  CLI   SAMEDTS,C'Y'                                                     
         JE    DTOVERR                                                          
         J     CHKOV60                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
ADJTIM   NTR1                                                                   
         LH    R0,0(R1)                                                         
         CHI   R0,2400                                                          
         JL    *+6                                                              
         XR    R0,R0               MIDNIGHT BECOMES 0                           
         STH   R0,0(R1)                                                         
         J     ADJTIMX             NOP FOR CALENDAR DAYS                        
*                                                                               
         LH    R0,0(R1)  <<NOP>>                                                
         CHI   R0,600                                                           
         JNL   ADJTIMX                                                          
         AHI   R0,2400                                                          
         STH   R0,0(R1)                                                         
* NOW NEED TO BACK UP DATE TO PREVIOUS DAY                                      
         LR    R2,RF                  SAVE ADDRESS OF DATE FIELD                
         GOTO1 VDATCON,DMCB,(3,(R2)),WORK                                       
         LHI   R0,-1                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(3,(R2))                                     
*                                                                               
ADJTIMX  J     EXITY                                                            
*                                                                               
*                                                                               
DTOVERR  DS    0H                                                               
         LA    R2,DTOVERL                                                       
*                                                                               
         L     R4,AIO7                                                          
         USING PATKEY,R4                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10                                                            
         X     R1,=XL4'00003FFF'                                                
         EDIT  (R1),(3,12(R2))                                                  
*                                                                               
         LTR   R5,R5               TEST NO DATES                                
         JZ    DTOVERRB                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,(R5)),(5,22(R2)) START DATE                      
         CLI   3(R5),X'FF'         IS THIS A UFN DATE                           
         JNE   DTOVERRA                                                         
         MVC   31(3,R2),=C'UFN'                                                 
         J     DTOVERRB                                                         
*                                                                               
DTOVERRA GOTO1 (RF),(R1),(3,3(R5)),(5,31(R2)) END DATE                          
*                                                                               
DTOVERRB LA    RF,L'DTOVERL        DATES OVERLAP                                
         LA    R1,DTOVERL                                                       
         J     ERR                                                              
*                                                                               
*                                                                               
*-----------------------------------------                                      
* VALIDATE STEXT FOR REQUEST                                                    
*-----------------------------------------                                      
*                                                                               
VSTX     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FLD,FLD                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   ELCODE,X'50'                                                     
         L     R4,APATREC                                                       
         BRAS  RE,GETEL                                                         
         JNE   *+8                                                              
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL                                                         
*                                                                               
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING DTXKEY,R4                                                        
         MVC   DTXKID,=X'0A2D'                                                  
         MVC   DTXKAM(3),SVBAGM     A-M/CLT                                     
         MVI   DTXKDESC,C'-'                                                    
         MVI   DTXKTYP,C'L'                                                     
*                                                                               
         OC    QSTEXT,QSTEXT                                                    
         JZ    VSTX40                                                           
*                                                                               
         MVC   DTXKDESC+1(6),QSTEXT                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFFIL+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00564                                                         
*                                                                               
         CLC   QSTEXT,PATTXTKY+1-PATTXTEL+ELEM                                  
         JNE   VSTX00                                                           
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         TM    FLAGS,DELPATSW      DELETE PATTERN                               
         JO    VSTX06                                                           
*                                                                               
         TM    FLAGS,RESPATSW      RESTORE PATTERN                              
         JO    VSTX06                                                           
*                                                                               
         J     VSTXX                                                            
*                                                                               
VSTX00   MVC   FLD(7),PATTXTKY-PATTXTEL+ELEM                                    
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    PATSTAT-PATDTAEL(R4),X'01'     SET ON PATTN TEXT CHANGE          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PATTXTEL,R4                                                      
         MVI   PATTXTEL,X'50'                                                   
         MVI   PATTXTLN,9                                                       
         MVI   PATTXTKY,C'-'                                                    
         MVC   PATTXTKY+1(6),QSTEXT                                             
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
VSTX06   GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO7'                        
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VSTX15                                                           
         USING DTXPATEL,R4                                                      
VSTX10   CLC   DTXPATP1,QPRDX                                                   
         JNE   VSTX14                                                           
         CLC   DTXPATS1,QLEN1                                                   
         JNE   VSTX14                                                           
         CLC   DTXPATP2,QPRDX2                                                  
         JNE   VSTX14                                                           
         CLC   DTXPATS2,QLEN2                                                   
         JNE   VSTX14                                                           
         CLC   DTXPATES,CODE                                                    
         JNE   VSTX14                                                           
         CLC   BREF,DTXPATRF                                                    
         JE    VSTX16                                                           
VSTX14   BRAS  RE,NEXTEL                                                        
         JE    VSTX10                                                           
*                                                                               
VSTX15   TM    FLAGS,DELPATSW      DELETE PATTERN (INSURANCE)                   
         JO    VSTXX               DO NOT CREATE NEW ELEM                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DTXPATEL,R4                                                      
         MVI   DTXPATEL,X'70'                                                   
         MVI   DTXPATLN,DTXPATX-DTXPATEL                                        
         MVC   DTXPATP1(4),QPRDX                                                
         MVC   DTXPATES,CODE                                                    
         MVC   DTXPATRF,BREF                                                    
         MVI   DTXPATST,1          MAKE ACTIVE                                  
*                                                                               
         L     R7,AIO7                                                          
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         J     VSTX18                                                           
*                                                                               
VSTX16   DS    0H                                                               
         TM    FLAGS,DELPATSW      DELETE PATTERN                               
         JZ    *+12                                                             
         MVI   DTXPATST,0          MAKE INACTIVE                                
         J     VSTX18                                                           
*                                                                               
         MVI   DTXPATST,1          MAKE ACTIVE                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
VSTX18   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+IO7'                        
         JNE   CLTIODIE                                                         
*                                                                               
         OC    FLD(7),FLD          WAS THERE AN OLD SPEC TEXT                   
         JZ    VSTXX                NO                                          
*                                                                               
         LA    R4,IOKEY                                                         
         USING DTXKEY,R4                                                        
         MVC   DTXKDESC,FLD                                                     
         DROP  R4                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOTRFDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO7'                        
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING DTXPATEL,R4                                                      
VSTX20   CLC   DTXPATKY(4),QPRDX                                                
         JNE   VSTX24                                                           
         CLC   DTXPATES,CODE                                                    
         JNE   VSTX24                                                           
         CLC   BREF,DTXPATRF                                                    
         JE    VSTX26                                                           
VSTX24   BRAS  RE,NEXTEL                                                        
         JE    VSTX20                                                           
         DC    H'0'                                                             
VSTX26   MVI   DTXPATST,0          MAKE INACTIVE                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+IO7'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VSTXX    J     EXITY                                                            
*                                                                               
* NO DATA ENTERED IN TEXT, SEE IF THERE WAS A SPEC TEXT BEFORE *                
*                                                                               
VSTX40   DS    0H                                                               
         CLI   RECACT,C'A'         ADD                                          
         JE    VSTXX                                                            
         L     R4,AIO5                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         NI    PATSTAT-PATDTAEL(R4),X'FE'     SET OFF PATTN TEXT CHGE           
*                                                                               
         OC    ELEM,ELEM           WAS THERE AN OLD SPEC TEXT                   
         JZ    VSTXX                NO, NO UPDATE NEEDED                        
*                                                                               
         LA    R4,IOKEY                                                         
         USING DTXKEY,R4                                                        
         MVC   DTXKDESC+1(6),PATTXTKY+1-PATTXTEL+ELEM                           
         DROP  R4                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOTRFDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO7'                        
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING DTXPATEL,R4                                                      
VSTX50   CLC   DTXPATKY(4),QPRDX                                                
         JNE   VSTX54                                                           
         CLC   DTXPATES,CODE                                                    
         JNE   VSTX54                                                           
         CLC   BREF,DTXPATRF                                                    
         JE    VSTX56                                                           
VSTX54   BRAS  RE,NEXTEL                                                        
         JE    VSTX50                                                           
         DC    H'0'                                                             
VSTX56   MVI   DTXPATST,0          MAKE INACTIVE                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOTRFFIL+IO7'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     VSTXX                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
*===========================================================                    
* VALIDATE COMMENTS                                                             
*===========================================================                    
*                                                                               
VCMT     NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,APATCMT        LIST OF COMMENTS                             
         MVC   NUMPATCM,LW_NUMN-LW_D(R2) NUMBER OF COMMENTS                     
         CLI   NUMPATCM+1,4                                                     
         JNH   VCMT05                                                           
*                                                                               
         LA    RF,L'CMT2MNY        TOO MANY COMMENT LINES                       
         LA    R1,CMT2MNY                                                       
         J     ERR                                                              
*                                                                               
VCMT05   AHI   R2,LW_LN2Q          POINT TO THE FIRST COMMENT                   
         SR    R5,R5                                                            
         ICM   R5,3,NUMPATCM       # OF COMMENTS                                
*                                                                               
         LA    R3,1                COMMENT NUMBER                               
         MVI   ELCODE,X'40'                                                     
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
VCMT10   LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PATCMTEL,R4                                                      
         MVI   PATCMTEL,X'40'                                                   
*                                                                               
         STC   R3,BYTE             SAVE COMMENT NUMBER                          
         BAS   RE,GETLN            GET INPUT LEN                                
         LTR   R3,R3               ANY INPUT?                                   
         JZ    VCMT20                                                           
         LR    RF,R3                                                            
*                                                                               
         LLC   R3,BYTE             RESTORE COMMENT NUMBER                       
*                                                                               
         BCTR  RF,0                LEN-1 FOR EX                                 
         EX    RF,VCMTMVC                                                       
*                                                                               
         STC   R3,PATCMTNO         COMMENT NUMBER                               
         AHI   RF,4                INPUT LEN+3+1                                
         STC   RF,PATCMTLN                                                      
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VCMT20   LA    R2,MAXCMLEN(R2)     POINT TO NEXT FIELD                          
         JCT   R5,VCMT10                                                        
         J     EXITY                                                            
VCMTMVC  MVC   PATCMT(0),0(R2)                                                  
*                                                                               
*                                                                               
*------------------------------------------------------------                   
* GET ACTUAL INPUT LEN                                                          
* R2 POINTS TO INPUT FIELD                                                      
* R3 WILL RETURN ACTUAL LENGTH OF INPUT FIELD                                   
*------------------------------------------------------------                   
*                                                                               
GETLN    NTR1  BASE=*,LABEL=*                                                   
         LA    RE,0(R2)                                                         
         LA    RF,MAXCMLEN                                                      
         BCTR  RF,0                                                             
         AR    RE,RF               POINT TO LAST CHARACTER IN COMMENT           
         LA    RF,MAXCMLEN         DEFAULT TO MAX INPUT LENGTH                  
GETFLN12 CLI   0(RE),C' '                                                       
         JH    GETFLN_X                                                         
         LTR   RF,RF               NO INPUT?                                    
         JZ    GETFLN_X                                                         
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     GETFLN12                                                         
GETFLN_X LR    R3,RF                                                            
         XIT1  REGS=(R3)                                                        
*                                                                               
MAXCMLEN EQU   53                                                               
*                                                                               
*                                                                               
*------------------------------------------------------------                   
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
*------------------------------------------------------------                   
*                                                                               
VCC      NTR1  BASE=*,LABEL=*                                                   
         MVI   CODE,0                                                           
         MVI   CODESW,C'N'                                                      
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
*                                                                               
         CLI   QCODE,0             ANY ENTRY                                    
         JNE   VCC10                                                            
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         JNE   VCCX                                                             
         LA    RF,L'MISSCOD                                                     
         LA    R1,MISSCOD                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCC10    CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         JNE   VCC30                                                            
*                                                                               
         LA    RF,QCODE                                                         
         LA    RE,L'QCODE-1                                                     
         BRAS  R1,FLEN             FIND INPUT LEN                               
         STC   RE,INPLEN           SAVE INPUT LEN-1                             
         LA    R2,QCODE                                                         
         MVI   ERRFLG,0                                                         
         BRAS  RE,VNUM             VALIDATE NUMERIC                             
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         MVC   CODE,ACTUAL                                                      
         MVI   CODESW,C'Y'                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING ESTRECD,R4                                                       
*                                                                               
         MVC   EKEYAM(3),SVBAGM                                                 
         MVC   EKEYPRD,QPROD1                                                   
         MVC   EKEYEST,CODE                                                     
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00643            ESTIMATE NOT FOUND                           
*                                                                               
         L     R4,AIO7                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
*                                                                               
         CLI   ECOPY,0             MUST NOT BE COPY CODE                        
         JNE   ERR00688                                                         
         GOTO1 VDATCON,DMCB,(0,ESTART),(3,ESTSTR)                               
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
*                                                                               
         MVC   QBEST,CODE          SAVE EST FOR VALILOC                         
         MVC   SVMENU,EDAYMENU     SAVE DAYPART MENU                            
*                                                                               
         J     VCCX                                                             
         DROP  R4                                                               
*                                                                               
VCC30    CLI   QCODE+1,X'40'                                                    
         JH    ERR00695                                                         
*                                                                               
         MVC   CODE,QCODE                                                       
VCCX     J     EXITY                                                            
*                                                                               
*                                                                               
*----------------------------------                                             
* VALIDATE NUMERIC                                                              
*----------------------------------                                             
*                                                                               
VNUM     NTR1  BASE=*,LABEL=*                                                   
         LLC   R1,INPLEN           LEN-1 OF INPUT                               
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVZ   WORK(0),0(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         JNE   VNUMERR                                                          
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         JZ    VNUMERR                                                          
         CHI   R1,255                                                           
         JH    VNUMERR                                                          
         STC   R1,ACTUAL                                                        
         J     EXITY                                                            
*                                                                               
VNUMERR  J     ERR00003                                                         
*                                                                               
*                                                                               
*----------------------------------                                             
* VALIDATE REFERENCE NUMBER                                                     
*----------------------------------                                             
*                                                                               
VREF     NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING PATKEY,R4                                                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(3),SVBAGM    A/M/CLT                                      
         MVC   PATKPRD,QPRDX       BPRD                                         
         MVC   PATKSLN,SVLEN1      LEN                                          
         MVC   PATKPRD2,QPRDX2     PRD2                                         
         MVC   PATKSLN2,SVLEN2     LEN2                                         
         MVC   PATKCODE,CODE                                                    
         CLI   RECACT,C'A'         ADD                                          
         JE    VREF60                                                           
         XC    BREFSUB,BREFSUB                                                  
*                                                                               
         OC    QREF,QREF                                                        
         JNZ   VREF08                                                           
*                                                                               
         LA    RF,L'MISSREF                                                     
         LA    R2,MISSREF                                                       
         J     ERR                                                              
*                                                                               
VREF08   MVC   SVBREF,QREF          REFERENCE                                   
         CLC   SVBREF,=X'16383'                                                 
         JNH   VREF12                                                           
*                                                                               
         LA    RF,L'REF2BIG                                                     
         LA    R1,REF2BIG                                                       
         J     ERR                                                              
*                                                                               
VREF12   ICM   R0,3,SVBREF                                                      
         STCM  R0,3,BREF                                                        
         X     R0,=XL4'00003FFF'                                                
         SLL   R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
         STCM  R0,7,PATKREF        COMBINED REF NUMBER/GENERATED SUBLIN         
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'  READ OF DELETED          
         CLC   IOKEY(10),IOKEYSAV                                               
         JE    VREF15                                                           
*                                                                               
         LA    RF,L'REFNOTF                                                     
         LA    R1,REFNOTF                                                       
         J     ERR                                                              
*                                                                               
VREF15   SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         CH    R0,BREF                                                          
         JE    VREF20                                                           
*                                                                               
         XC    ERRORMSG,ERRORMSG                                                
         LA    RF,L'REFNOTF                                                     
         LA    R1,REFNOTF                                                       
         J     ERR                                                              
*                                                                               
VREF20   SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
         J     VREFX                                                            
*                                                                               
*------------------------------------------                                     
* FIND REFERENCE NUMBER FOR ADDED RECORD                                        
*------------------------------------------                                     
*                                                                               
VREF60   DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO7'  READ OF DELETED          
         CLC   IOKEY(10),IOKEYSAV                                               
         JE    VREF62                                                           
         MVC   BREF(2),=XL2'0001'                                               
         MVC   BSUB(2),=XL2'0001'                                               
         MVC   BREFSUB,=XL3'FFFBFE'                                             
         MVC   SVBREF,BREF                                                      
         J     VREFX                                                            
VREF62   SR    R1,R1                                                            
         ICM   R1,7,IOKEY+10       GET THIS REF/SUB                             
         SRL   R1,10               NOW HAVE REF ONLY                            
         X     R1,=XL4'00003FFF'                                                
         LA    R1,1(,R1)           ADD 1                                        
         STH   R1,BREF                                                          
         X     R1,=XL4'00003FFF'                                                
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         SLL   R1,22                                                            
         SLDL  R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
         MVC   SVBREF,BREF         SAVE REF NUMBER OF NEW PAT                   
VREFX    J     EXITY                                                            
*                                                                               
*                                                                               
*                                                                               
*------------------------------                                                 
* VALIDATE PERIOD                                                               
*------------------------------                                                 
*                                                                               
VPER     NTR1                                                                   
         LA    R4,ELEM                                                          
         USING PATDTAEL,R4                                                      
*                                                                               
         MVC   STRTPAT,QPATSTR                                                  
*                                                                               
         CLC   =C'99/99/99',QPATEND  UFN                                        
         JNE   VPER20              NO, PROCESS IT                               
*                                                                               
         CLI   SVT1PR15,C'Y'       T1 PROFILE 15 PROHIBITS UFN END DATE         
         JE    ERR00677                                                         
*                                                                               
         MVC   ENDPAT,=X'FFFFFF'  FORCE IT                                      
         J     VPER30                                                           
*                                                                               
VPER20   GOTOR VDATVAL,DMCB,(0,QPATEND),DATE                                    
         OC    DMCB(4),DMCB                                                     
         JZ    ERR00020                                                         
         GOTOR VDATCON,(R1),(0,DATE),(3,ENDPAT)                                 
*                                                                               
         CLC   STRTPAT,ENDPAT    CAN'T END BEFORE START                         
         JH    ERR00020                                                         
*                                                                               
VPER30   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         JNE   VPER40                                                           
         CLI   CODE,0                                                           
         JE    VPER40                                                           
         CLC   ESTEND,STRTPAT                                                   
         JL    ERR00659                                                         
         CLC   ESTSTR,ENDPAT                                                    
         JH    ERR00659                                                         
*                                                                               
VPER40   CLC   PATDTS,PATSTART    WERE DATES CHANGED                            
         JE    *+8                                                              
         MVI   NEWSUBSW,1                                                       
*                                                                               
         MVC   PATSTART(6),PATDTS  SAVE DATES FOR CK TO CMML                    
*                                                                               
         CLI   RECACT,C'A'         ADD                                          
         JE    VPERX                                                            
*                                                                               
*=========================================================                      
* CHECK THAT ALL CMMLS FALL WITHIN NEW DATES                                    
*=========================================================                      
*                                                                               
         MVI   VCMLFLAG,C'P'       SET FLAG FOR VALIDATING PERIOD               
* 10 ELEM IS IN ELEM - IT HAS BEEN REMOVED FROM RECORD                          
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R4                                                      
         TM    PATSTAT1-PATDTAEL+ELEM,PATSADID   TEST CMMLS ARE ADIDS           
         JO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
         DROP  R4                                                               
*                                                                               
         L     R4,APATREC          COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         JE    VPER41                                                           
* FOR BPAT, LOOK FOR 31 ELEM                                                    
         L     R4,APATREC                                                       
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VPERX                                                            
*                                                                               
         USING PATCMLEL,R4                                                      
VPER41   LLC   R0,1(R4)                                                         
         SRL   R0,4                SET FOR JCT/DIVIDE BY 16                     
*                                                                               
         LA    R5,PATCML           FIRST CMML                                   
         CLI   ELCODE,X'31'        TEST BPAT                                    
         JNE   *+8                                                              
         LA    R5,1(R5)                                                         
*                                                                               
VPER42   DS    0H                                                               
         MVC   WORK(8),0(R5)                                                    
         CLC   =X'5C00',WORK                                                    
         JE    VPER44                                                           
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMLP                                                         
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VPER44   OC    8(8,R5),8(R5)                                                    
         JZ    VPER46                                                           
         MVC   WORK(8),8(R5)                                                    
         CLC   =X'5C00',WORK                                                    
         JE    VPER46                                                           
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMLP                                                         
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VPER46   LA    R5,16(R5)                                                        
         JCT   R0,VPER42                                                        
*                                                                               
VPERX    XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
*==========================================================                     
* CHECK THAT COMMERCIALS AND PATTERN PRDS/SLNS AGREE                            
*==========================================================                     
*                                                                               
CHKCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QPRDX2,0            TEST P/B PATTERN                             
         JNE   CHKCML20            YES                                          
*                                                                               
*================================================                               
* PATTERN IS FOR ONE PRODUCT ONLY                                               
*================================================                               
*                                                                               
         CLI   SVSOLO1,C'P'        TEST FOR P/B ONLY                            
         JE    ERR00696            YES - THEN ERROR                             
         CLI   SVSOLO2,C'P'                                                     
         JE    ERR00696                                                         
*                                                                               
         CLI   SVSLN2,0            TEST 2 CMMLS INPUT                           
         JNE   CHKCML10            YES                                          
*                                                                               
* ONE CMML INPUT - NON P/B                                                      
*                                                                               
         TM    SVFLAG1,X'01'       PRD IN CMML                                  
         JZ    MATPRDEV            NO - ERROR                                   
         CLC   QLEN1,SVSLN1        RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
*                                                                               
* TWO CMMLS INPUT - NON P/B                                                     
*                                                                               
CHKCML10 CLI   SVSOLO1,C'S'        TEST SOLO ONLY                               
         JE    ERR00696            YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'        TEST SOLO ONLY                               
         JE    ERR00696                                                         
*                                                                               
         CLI   SVFLAG1,1           TEST PRD IN BOTH CMMLS                       
         JNE   MATPRDEV                                                         
         CLI   SVFLAG2,1                                                        
         JNE   MATPRDEV                                                         
*                                                                               
         LLC   R0,SVSLN1                                                        
         LLC   R1,SVSLN2                                                        
         AR    R0,R1               GET TOTAL SLN                                
         CLM   R0,1,QLEN1          RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
*                                                                               
*============================================================                   
* PATTERN IS FOR PIGGYBACK                                                      
*============================================================                   
*                                                                               
CHKCML20 CLI   SVSOLO1,C'S'        TEST FOR SOLO ONLY                           
         JE    ERR00696            YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'                                                     
         JE    ERR00696                                                         
*                                                                               
         CLI   SVSLN2,0            TEST TWO CMMLS                               
         JNE   CHKCML30                                                         
*                                                                               
* ONE CMML INPUT - P/B                                                          
*                                                                               
         TM    SVFLAG1,X'02'+X'01' TEST BOTH PRDS IN CMML                       
         JNO   MATPRDEV            NO - ERROR                                   
         LLC   R0,SVSLN1                                                        
         SRL   R0,1                BOTH PRDS MUST BE HALF CMML SLN              
         CLM   R0,1,QLEN1                                                       
         JNE   INVSLNER                                                         
         CLM   R0,1,QLEN2                                                       
         JNE   INVSLNER                                                         
         J     EXITY                                                            
*                                                                               
* TWO CMMLS INPUT- P/B                                                          
*                                                                               
CHKCML30 TM    SVFLAG1,X'01'       TEST PRD1 IN CMML1                           
         JO    CHKCML32            YES                                          
         TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         JZ    MATPRDEV                                                         
*                                                                               
CHKCML32 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         JO    CHKCML40                                                         
         TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         JZ    MATPRDEV                                                         
*                                                                               
* FULL+0=PRD1/CMML1, FULL+1=PRD2/CMML2                                          
* FULL+2=PRD1/CMML2, FULL+3=PRD2/CMML1                                          
*                                                                               
CHKCML40 XC    FULL,FULL                                                        
         TM    SVFLAG1,X'01'       TEST PRD1 IN CMML 1                          
         JZ    CHKCML42                                                         
         MVI   FULL,X'10'          SET FLAG                                     
         CLC   SVSLN1,QLEN1        MATCH PRD1 SLN                               
         JNE   *+8                                                              
         MVI   FULL,X'11'          SET PRD1/SLN1 MATCH CMML1                    
*                                                                               
CHKCML42 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         JZ    CHKCML44                                                         
         MVI   FULL+3,X'10'                                                     
         CLC   SVSLN2,QLEN2        MATCH PRD2 SLN                               
         JNE   *+8                                                              
         MVI   FULL+3,X'11'                                                     
*                                                                               
CHKCML44 TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         JZ    CHKCML46                                                         
         MVI   FULL+2,X'10'                                                     
         CLC   SVSLN1,QLEN1        MATCH PRD1 SLN                               
         JNE   *+8                                                              
         MVI   FULL+2,X'11'                                                     
*                                                                               
CHKCML46 TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         JZ    CHKCML50                                                         
         MVI   FULL+1,X'10'                                                     
         CLC   SVSLN2,QLEN2        MATCH PRD2 SLN                               
         JNE   *+8                                                              
         MVI   FULL+1,X'11'                                                     
*                                                                               
CHKCML50 CLI   FULL,X'11'          TEST PRD1/SLN1 MATCH CMML1                   
         JNE   CHKCML52            NO                                           
         CLI   FULL+1,X'11'        TEST PRD2/SLN2 MATCH CMML2                   
         JE    CHKCMLX             YES - ALL IS GOOD                            
*                                                                               
CHKCML52 CLI   FULL+2,X'11'        TEST PRD1/SLN1 MATCH CMML2                   
         JNE   INVSLNER            NO - ERROR                                   
         CLI   FULL+3,X'11'        TEST PRD2/SLN2 MATCH CMML1                   
         JNE   INVSLNER            NO  - ERROR                                  
*                                                                               
CHKCMLX  J     EXITY                                                            
*                                                                               
MATPRDEV LA    RF,L'NOPRDPA                                                     
         LA    R1,NOPRDPA                                                       
         J     ERR                                                              
*                                                                               
INVSLNER LA    RF,L'PATCMLL                                                     
         LA    R1,PATCMLL                                                       
         J     ERR                                                              
*                                                                               
*===============================================================                
* VALIDATE CMMLS AND PCTS FOR NORMAL PATTERN                                    
*===============================================================                
*                                                                               
VCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   VCMLFLAG,0                                                       
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         MVI   HASDPT,C'N'                                                      
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
         OC    PATSTIM(4),PATSTIM                                               
         JZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
         CLI   PATDPT,0                                                         
         JE    *+8                                                              
         MVI   HASDPT,C'Y'                                                      
*                                                                               
         DROP  R4                                                               
*                                                                               
* NEED TO SEE IF ALL CMMLS ARE VALID ADIDS                                      
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,7,AFLDA                                                       
         JNZ   VCMML2                                                           
         LA    RF,L'MISSFLD                                                     
         LA    R1,MISSFLD                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML2   MVC   NUMFLDA,LW_NUMN-LW_D(R5) Number of cml alphas to process         
         SR    R4,R4                                                            
         ICM   R4,3,NUMFLDA        Number of cml fields to process              
         AHI   R5,LW_LN2Q          Point to first cml field                     
         ST    R5,ACOMML1          Set A(1st cml field)                         
*                                                                               
         USING CML_D,R5                                                         
VCMML4   CLI   FLDA,C' '                                                        
         JNE   VCMML4C                                                          
*                                                                               
         LA    RF,L'MISSFLD                                                     
         LA    R1,MISSFLD                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML4C  OC    CML1,CML1          TEST ANY CML                                  
         JNZ   VCMML4F                                                          
*                                                                               
         LA    RF,L'MISSCML                                                     
         LA    R1,MISSCML                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML4F  CLC   =C'HIATUS',CML1                                                  
         JE    VCMML6                                                           
         CLC   =C'DELETE',CML1                                                  
         JE    VCMML6                                                           
         CLI   CML1,C'*'                                                        
         JE    VCMML6                                                           
         OC    CML1,SPACES                                                      
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',CML1),SVCML1                                  
         JNE   VCMML8                                                           
*                                                                               
         OC    CML2,CML2          TEST P/B INPUT                                
         JZ    VCMML6              NO                                           
         GOTO1 VTRPACK,DMCB,(C'P',CML2),SVCML2                                  
         JNE   VCMML8                                                           
*                                                                               
VCMML6   DS    0H                                                               
         LA    R5,CMLLNQ(R5)                                                    
         JCT   R4,VCMML4                                                        
         DROP  R5                                                               
*                                                                               
VCMML7   MVI   ADIDFLAG,C'Y'                                                    
         J     VCMML10                                                          
*                                                                               
VCMML8   MVI   ADIDFLAG,C'N'       DO NOT USE ADIDS THIS REC                    
*                                                                               
VCMML10  XC    PCTTBL,PCTTBL       CLEAR PCT BUILD AREA                         
         LA    RE,PCTTBL                                                        
         ST    RE,PCTNXT           SET A(NEXT ENTRY)                            
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
         MVC   STRTPAT,PATSTART    PASS THESE TO VCMML                          
         MVC   ENDPAT,PATEND                                                    
         DROP  R4                                                               
*                                                                               
         L     R5,ACOMML1          First input cml                              
         USING CML_D,R5                                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PATCMLEL,R4                                                      
*                                                                               
         CLC   CML1(6),=CL6'HIATUS'                                             
         JNE   VCMML12                                                          
         MVC   PATCML(6),=CL6'HIATUS'                                           
         MVI   CMMLCT,1            SET ENTRY TO ONE                             
         LA    R3,PATCML+16        POINT BEYOND FIRST ENTRY                     
         J     VCMML70                                                          
*                                                                               
VCMML12  MVI   CMMLCT,0            CLEAR COUNTERS                               
         MVI   DELCT,0                                                          
         MVI   PCTTOT,0                                                         
         MVI   PRDMATSW,0                                                       
*                                                                               
         LA    R3,PATCML                                                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,NUMFLDA        Number of cmls fields to process             
VCMML14  STCM  RE,3,NUMCML                                                      
         L     RE,PCTNXT                                                        
         MVC   0(1,RE),FLDA        SAVE LETTER                                  
*                                                                               
         OC    CML1,CML1           TEST NO INPUT                                
         JZ    VCMML65                                                          
*                                                                               
VCMML22  XC    SVTYPE1(7),SVTYPE1  TYPE(4)/SLN(1)/SOLO(1)/FLAG(1)               
         XC    SVTYPE2(7),SVTYPE2                                               
*                                                                               
         OC    CML2,CML2           If no cml2                                   
         JNZ   VCMML26                                                          
         CLC   CML1(6),DELETE                                                   
         JE    VCMML24                                                          
*                                                                               
         CLC   CML1,DELCML         '*    '                                      
         JNE   VCMML26                                                          
*                                                                               
VCMML24  MVC   0(16,R3),DELCML                                                  
         LLC   R1,DELCT                                                         
         LA    R1,1(R1)            BUMP DELETED COUNT                           
         STC   R1,DELCT                                                         
         LA    R5,CMLLNQ(R5)       Point to next cml                            
         LA    R3,16(R3)                                                        
         SR    RE,RE                                                            
         ICM   RE,3,NUMCML         Number of cmls to process                    
         JCT   RE,VCMML14                                                       
*                                                                               
VCMML26  MVC   WORK(12),CML1       VALIDATE CML1                                
         LA    R1,SVTYPE1                                                       
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMLP                                                         
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   0(8,R3),WORK+12     MOVE COMPRESSED CODE TO ELEM                 
*                                                                               
         OC    CML2,CML2           TEST CMML2 INPUT                             
         JZ    VCMML30             NO                                           
*                                                                               
VCMML28  MVC   WORK(12),CML2       VALIDATE CML2                                
         LA    R1,SVTYPE2                                                       
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCMLP                                                         
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   8(8,R3),WORK+12                                                  
*NOP     L     R2,SVR2             Restore R2                                   
*                                                                               
         CLC   SVTYPE1,SVTYPE2     ARE BOTH CMML TYPES THE SAME                 
         JE    VCMML29                                                          
*                                                                               
         LA    RF,L'DIFTYPE        NO ERROR                                     
         LA    R1,DIFTYPE                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML29  CLC   0(8,R3),8(R3)       BOTH CMLS SHOULD NOT BE EQUAL                
         JE    ERR00621                                                         
*                                                                               
VCMML30  MVI   ERRFLG,0                                                         
         BRAS  RE,CHKCMLS          CHECK PRDS/SLNS                              
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*================================================================               
* CHECK FOR DUPLICATE INPUT                                                     
*================================================================               
*                                                                               
VCMML40  LA    R1,PATCML           FIRST COMMERCIAL                             
*                                                                               
VCMML42  CR    R1,R3               TEST THIS IS US                              
         JE    VCMML50             YES - DONE                                   
         CLC   0(16,R1),0(R3)      TEST EQUAL                                   
         JNE   VCMML44                                                          
*                                                                               
         LA    RF,L'DUPCML         YES ERROR                                    
         LA    R1,DUPCML                                                        
         J     ERR                 ERROR                                        
*                                                                               
VCMML44  LA    R1,16(R1)           POINT TO NEXT CMML(S)                        
         J     VCMML42                                                          
*                                                                               
VCMML50  LLC   R1,CMMLCT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,CMMLCT                                                        
*                                                                               
VCMML52  DS    0H                                                               
         CLI   PCT,0               Any percent entered?                         
         JE    VCMML54                                                          
*                                                                               
*NOP     ST    R2,SVR2             Save R2                                      
         LA    RF,PCT                                                           
         LA    RE,L'SVPCT-1                                                     
         BRAS  R1,FLEN             FIND INPUT LEN                               
         STC   RE,INPLEN           SAVE INPUT LEN-1                             
         LA    R2,PCT                                                           
         MVI   ERRFLG,0                                                         
         BRAS  RE,VNUM             Validate numeric                             
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*NOP     L     R2,SVR2             Restore R2                                   
*                                                                               
         LLC   R0,PCTTOT                                                        
         LLC   RE,ACTUAL                                                        
         AR    R0,RE                                                            
         STC   R0,PCTTOT                                                        
*                                                                               
         L     RE,PCTNXT                                                        
         MVC   2(1,RE),ACTUAL      SAVE PCT                                     
         LA    RE,3(RE)                                                         
         ST    RE,PCTNXT                                                        
         J     VCMML60                                                          
*                                                                               
VCMML54  CLI   PCTTOT,0            TEST PCTS PREVIOUSLY INPUT                   
         JE    VCMML60                                                          
*                                                                               
         LA    RF,L'MISSPCT        NO ERROR                                     
         LA    R1,MISSPCT                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML60  LA    R3,16(R3)                                                        
*                                                                               
VCMML62  LA    R5,CMLLNQ(R5)       Point to next cml                            
         SR    RE,RE                                                            
         ICM   RE,3,NUMCML         Number of cmls field to process              
         JCT   RE,VCMML14                                                       
*                                                                               
******************************************************                          
* CODE BELOW CHECKS FOR GAPS IN INPUT                                           
*******************************************************                         
VCMML65  DS    0H                                                               
*CMML65  LR    RE,R2               SHOULD BE NO MORE CMMLS                      
*NOP     SHI   RE,CMLLNQ           POINT TO LAST ALPHA THAT PROCESSED           
*        SR    R4,R4                                                            
*        ICM   R4,3,NUMFLDA        Number of cml fields to process              
*        LA    R1,L'ATBL           Max table len                                
*        CR    R4,R1                                                            
*        JE    VCMML66             All cml are there                            
*        LA    R1,ATBL             Alpha table                                  
*        SHI   R4,1                                                             
*        AR    R1,R4                                                            
*        CLC   0(1,RE),0(R1)       Should be same alpha                         
*        JE    VCMML66                                                          
*                                                                               
*        LA    RF,L'MISSCML                                                     
*        LA    R1,MISSCML                                                       
*******  J     ERR                 ERROR                                        
*                                                                               
VCMML66  CLI   CMMLCT,0           ANY ENTRIES FOUND                             
         JNE   VCMML70             NO                                           
*                                                                               
         LA    RF,L'MISSCML                                                     
         LA    R1,MISSCML                                                       
         J     ERR                 ERROR                                        
*                                                                               
VCMML70  MVI   ELEM,X'30'          SET ELEMENT CODE                             
         LA    R0,ELEM                                                          
         SR    R3,R0               GIVE ELEMENT LENGTH                          
         STC   R3,ELEM+1                                                        
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'30'        GET OLD CMML ELEM                            
         BRAS  RE,GETEL                                                         
         JNE   VCMML72                                                          
         SR    RE,RE                                                            
         IC    RE,1(R4)            GET OLD ELEM LENGTH                          
         BCTR  RE,0                                                             
         EX    RE,VCMMLCLC                                                      
         JE    VCMML74                                                          
         MVI   NEWSUBSW,1          SET TO ADD NEW REF/SUBL ON CHG               
*                                                                               
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
VCMML72  L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         L     R4,APATREC          SET FLAG THAT CMMLS ARE ADIDS                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
*                                                                               
         OI    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         CLI   ADIDFLAG,C'Y'                                                    
         JE    *+8                                                              
         NI    PATSTAT1,X'FF'-PATSADID  UNSET FLAG                              
         J     VCMML74                                                          
*                                                                               
VCMMLCLC CLC   0(0,R4),ELEM                                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
VCMML74  DS    0H                                                               
         MVI   ELCODE,X'32'                                                     
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         MVI   ELCODE,X'34'                                                     
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         MVI   ELCODE,X'36'                                                     
         L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         CLC   CML1(6),=CL6'HIATUS' NO ROTATION FOR HIATUS                      
         JE    VCMMLX                                                           
*                                                                               
         CLI   PCTTOT,0            TEST ANY PCTS INPUT                          
         JNE   VCMML76             YES                                          
         CLI   DELCT,0             TEST ANY DELETED CMMLS                       
         JNE   VCMML80             YES                                          
         CLI   CMMLCT,1            TEST EXACTLY 1 CMML                          
         JNE   VCMML80             NO                                           
         MVI   PCTTOT,100                                                       
         MVC   ELEM(5),=X'3405C10064'   SET A=100                               
         XC    QROT,QROT                AND IGNORE ROTATION INPUT               
         J     VCMML76X                                                         
*                                                                               
VCMML76  CLI   PCTTOT,99                                                        
         JE    *+12                                                             
         CLI   PCTTOT,100                                                       
         JNE   ERR00635            ERROR MUST BE 99/100 PCT                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'34'                                                       
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         MVI   0(RE),0             MARK END OF LIST                             
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
*                                                                               
VCMML76X L     R7,APATREC                                                       
         MVC   SYSFIL,=CL8'TRFFIL'                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
VCMML80  MVI   ERRFLG,0                                                         
         BRAS  RE,VROT                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*===============================================================                
* TURN OFF INCOMPLETE RECORD FLAG IN 10 ELEM                                    
*===============================================================                
*                                                                               
         L     R4,APATREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R4                                                      
         NI    PATSTAT1,X'FF'-PATSINC TURN OFF INCOMPLETE REC FLAG              
*                                                                               
VCMMLX   J     EXITY                                                            
*                                                                               
DELCML   DS    0XL16                                                            
         DC    C'*'                                                             
         DC    XL15'00'                                                         
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
ATBL     DC    CL15'ABCDEFGHIJKLMNO'  15 LETTERS FOR 15 COMMERCIALS             
*                                                                               
*                                                                               
*================================================================               
* VALIDATE COMMERCIAL                                                           
* ON ENTRY WORK(12) CONTAINS INPUT CMML                                         
*          R1 POINTS TO SVTYPE(4),SVSLN                                         
* ON EXIT  WORK+12(8) CONTAINS 8 BYTE VERSION                                   
*          AND ADIDFLAG IS 'Y' IF AN ADID WAS INPUT                             
*================================================================               
*                                                                               
VCMLP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'HIATUS',WORK                                                  
         JE    VCMLPX                                                           
*                                                                               
         LR    R5,R1               SAVE POINTER                                 
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0A21'                                                
         MVC   IOKEY+2(3),SVBAGM   A-M/CLT                                      
*                                                                               
         MVC   IOKEY+5(8),WORK     USE EBCDIC CMML                              
         MVC   WORK+12(8),WORK     SET AS TRUNPK OUTPUT                         
*                                                                               
         CLI   ADIDFLAG,C'Y'       TEST CAN USE ADIDS                           
         JNE   VCMLP4                                                           
*                                                                               
VCMLP2   MVC   IOKEY(2),=X'0AC1'                                                
         CLI   VCMLFLAG,C'P'       TEST VALIDATING PERIOD                       
         JE    VCMLP4              YES-THEN WORK IS PACKED ALREADY              
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         JNE   ERR00734                                                         
         MVC   IOKEY+5(8),WORK+12                                               
*                                                                               
VCMLP4   DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00708                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R4                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         JNZ   ERR00535                                                         
*                                                                               
         CLC   CMLRLSE,STRTPAT     SEE IF THIS CMML WILL START IN TIME          
         JH    CMLDTERR            NO, CMLRLSE AFTER PAT START                  
*                                                                               
         CLC   CMLRCL,STRTPAT      IS CML RECALL BEFORE PAT START               
         JL    CMLDTERR             YES, ERROR                                  
*                                                                               
         CLC   CMLRCL,ENDPAT       SEE IF THIS CMML WILL LAST THRU PAT          
         JNL   VCMLP10             YES, OK                                      
*                                                                               
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERR                                                         
*                                                                               
VCMLP10  CLI   VCMLFLAG,C'P'       TEST VALIDATING FOR PERIOD CHANGE            
         JE    VCMLPX              YES - EXIT NOW                               
*                                                                               
         MVC   SVCMLRCL,CMLRCL     SAVE COMMERCIAL RECALL DATE                  
*                                                                               
         MVC   0(4,R5),CMLTYPE     SAVE COMMERCIAL TYPE (SETS SVTYPE)           
         MVC   4(1,R5),CMLSLN      SAVE SPOT LEN (SVSLEN1)                      
         MVC   5(1,R5),CMLSOLO     SVSOLO1                                      
*                                                                               
         LA    R4,QPRDX                                                         
         BAS   RE,CHKPRD                                                        
         JNE   *+8                                                              
         OI    6(R5),X'01'       SET PRD1 IN CMML (SVFLAG1)                     
*                                                                               
         LA    R4,QPRDX2                                                        
         CLI   0(R4),0                                                          
         JE    VCMLP12                                                          
         BAS   RE,CHKPRD                                                        
         JNE   *+8                                                              
         OI    6(R5),X'02'       SET PRD2 IN CMML                               
*                                                                               
VCMLP12  TM    6(R5),X'03'       TEST EITHER PRD IN THIS CMML                   
         JZ    MATPRDEV                                                         
*                                                                               
         CLI   HASDPT,C'Y'         TEST PATTERN HAS DPT                         
         JE    *+12                YES                                          
         CLI   HASTIME,C'Y'        OR PATTERN HAS TIME                          
         JNE   VCMLP14             NO                                           
*                                                                               
         MVI   ELCODE,X'B0'                                                     
         L     R4,AIO7                                                          
         BRAS  RE,GETEL                                                         
         JNE   VCMLP14                                                          
         USING CMLMATEL,R4                                                      
         OC    CMLMSTIM,CMLMSTIM   TEST TIME IN CMML                            
         JNZ   ERR00777                                                         
*                                                                               
*                                                                               
*============================================================                   
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*============================================================                   
*                                                                               
VCMLP14  DS    0H                                                               
*&&DO                                                                           
* NO BRAND AGENCY (PER RAMUNE)                                                  
*                                                                               
VCMLP14  MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
         L     R4,AIO7                                                          
         BRAS  RE,GETEL                                                         
         JE    VCMLP20                                                          
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         JNE   VCMLPX                                                           
         J     NOAIRERR            YES, NOT APPROVED TO AIR                     
*                                                                               
         USING CMLBBEL,R4                                                       
*                                                                               
VCMLP20  DS    0H                                                               
         CLI   CMLBBBAG,C'N'       IS BRAND AGY=N                               
         JE    VCMLPX                                                           
         CLI   CMLBBBAG,C'Y'                                                    
         JE    *+12                                                             
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         JNE   VCMLPX                                                           
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         JZ    VCMLP30                                                          
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         JNH   VCMLP30                                                          
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
*                                                                               
         CLC   SVCMLRCL,STRTPAT    IS CML RECALL BEFORE PAT START               
         JL    CMLDTERC             YES, ERROR                                  
         CLC   SVCMLRCL,ENDPAT     SEE IF THIS CMML WILL LAST THRU PAT          
         JNL   VCMLP30              YES, OK                                     
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERB                                                         
*                                                                               
VCMLP30  DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         JNE   *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         JE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         JNE   VCMLPX               NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         JZ    CADTERR              NO, ERROR                                   
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         JE    VCMLP32             YES                                          
*                                                                               
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         JNE   NOAIRERR            NOT APPROVED TO AIR                          
         JE    VCMLPX               YES, DONE                                   
*                                                                               
VCMLP32  OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         JZ    NOAIRERR             NO, ERROR                                   
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         MVC   WORK(8),CMLBBREF                                                 
         BRAS  RE,FNDCML            GO FIND COMMERCIAL                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(8),SVCML       RESTORE ORIGINAL CML                         
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         JNE   NOAIRERR                                                         
*                                                                               
         USING CMLBBEL,R4                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         JNE   NOAIRERR                                                         
*                                                                               
*&&                                                                             
***LPX   MVC   AIO,AIO3            RESTORE AIO                                  
VCMLPX   J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
CMLDTERR LA    R2,BADCMLD                                                       
         MVC   0(8,R2),WORK                                                     
         CLI   ADIDFLAG,C'Y'                                                    
         JNE   CMLDTER2                                                         
         MVC   0(12,R2),WORK                                                    
*NOP         GOTO1 VTRPACK,DMCB,(C'U',WORK),0(R2)                               
*                                                                               
CMLDTER2 LA    RF,L'BADCMLD        BAD CML DATE                                 
         LA    R1,BADCMLD                                                       
         J     ERR                                                              
*                                                                               
         USING CMLPRDEL,R4                                                      
CHKPRD   NTR1                                                                   
         LR    R2,R4                                                            
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'20'        GET PRDLIST ELEMENT                          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =XL3'2003FF',CMLPRDEL   IS THIS CMML PRD=ALL                     
         JE    EXITY                   YES, COVERS ALL PRODUCTS                 
*                                                                               
         LLC   R0,CMLPRDLN                                                      
         AHI   R0,-2                                                            
         LA    R1,CMLPRDS          START OF PROD LIST                           
*                                                                               
CHKPRD2  CLC   0(1,R2),0(R1)       MATCH PRD                                    
         JE    EXITY                YES                                         
         LA    R1,1(,R1)                                                        
         JCT   R0,CHKPRD2                                                       
         J     EXITN                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
*-----------------------------------------                                      
* ISSUE CUSTOMIZED ERROR MESSAGE                                                
*-----------------------------------------                                      
*                                                                               
ERR      XC    ERRORMSG,ERRORMSG                                                
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ERRORMSG(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         BRAS  RE,PUTMYERR                                                      
         J     EXITN                                                            
*                                                                               
* VALIDATE COMBINED MARKET/AFFILIATE LIST                                       
*                                                                               
*                                                                               
*>>>>>> HERE  <<<<<<<                                                           
*                                                                               
ERR00020 GOTOR PUTERR,DMCB,('SE#SY002',SE#INDTE) INVALID DATE FORMAT            
         J     EXITN                                                            
ERR00151 GOTOR PUTERR,DMCB,('SE#SY002',151)  SPOT LEN NOT VALID                 
         J     EXITN                                                            
ERR00243 GOTOR PUTERR,DMCB,('SE#SY002',243)  SPOT LEN MISSING                   
         J     EXITN                                                            
ERR00258 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGMD)  MEDIA MISSING                 
         J     EXITN                                                            
ERR00260 GOTOR PUTERR,DMCB,('SE#SY002',260)  PRODUCT MISSING                    
         J     EXITN                                                            
ERR01023 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVMD)  INVALID MEDIA                 
         J     EXITN                                                            
ERR00966 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGCL)  MISSING CLIENT                
         J     EXITN                                                            
ERR00014 GOTOR PUTERR,DMCB,('SE#SY002',SE#INCLT)  INVALID CLIENT                
         J     EXITN                                                            
ERR00971 GOTOR PUTERR,DMCB,('SE#SY002',SE#RECNF)  RECORD NOT FOUND              
         J     EXITN                                                            
ERR00049 GOTOR PUTERR,DMCB,('SE#SY002',SE#RCXST)  REC ALREADY EXISTS            
         J     EXITN                                                            
ERR00053 GOTOR PUTERR,DMCB,('SE#SY002',53)  RECORD NOT FOUND                    
         J     EXITN                                                            
ERR00428 GOTOR PUTERR,DMCB,('SE#SY002',SE#ROPTM)  REQ OPTION &T MISSING         
         J     EXITN                                                            
ERR00485 GOTOR PUTERR,DMCB,('SE#SY002',485)  TOO MANY PRODUCTS                  
         J     EXITN                                                            
ERR01025 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVPR)  INVALID PRODUCT CODE          
         J     EXITN                                                            
ERR00537 GOTOR PUTERR,DMCB,('SE#SY002',537)  1ST CHAR MUST BE ALPHA             
         J     EXITN                                                            
ERR00538 GOTOR PUTERR,DMCB,('SE#SY002',538)  PRD CD MUST BE 2-3 CHARS           
         J     EXITN                                                            
ERR00002 GOTOR PUTERR,DMCB,('SE#SY002',SE#ININP)  INVALID INPUT FIELD           
         J     EXITN                                                            
ERR00544 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVOF)  INVALID OFFICE                
         J     EXITN                                                            
ERR00001 GOTOR PUTERR,DMCB,('SE#SY002',1) MISSING DATA                          
         J     EXITN                                                            
ERR00003 GOTOR PUTERR,DMCB,('SE#SY002',SE#NONUM) NOT VALID NUMERIC DATA         
         J     EXITN                                                            
ERR00063 GOTOR PUTERR,DMCB,('SE#SY022',SE#INVPR) INVALID PRODUCT                
         J     EXITN                                                            
ERR00573 GOTOR PUTERR,DMCB,('SE#SY013',573) DESC 2 REQ FOR DESC 3               
         J     EXITN                                                            
ERR00673 GOTOR PUTERR,DMCB,('SE#SY002',673) SPOT LENGTH NOT VALID               
         J     EXITN                                                            
ERR00688 GOTOR PUTERR,DMCB,('SE#SY002',688) ESTIMATE HAS COPY CODE              
         J     EXITN                                                            
ERR00786 GOTOR PUTERR,DMCB,('SE#SY002',786) INVALID PRODUCT CODE                
         J     EXITN                                                            
ERR00102 GOTOR PUTERR,DMCB,('SE#SY002',102) PRODUCT CANT BE POL                 
         J     EXITN                                                            
ERR00202 GOTOR PUTERR,DMCB,('SE#SY022',SE#NOCML) COMMERCIAL NOT FOUND           
         J     EXITN                                                            
*                                                                               
ERR00535 GOTOR PUTERR,DMCB,('SE#SY013',SE#CMLDL) DELETED CML                    
         J     EXITN                                                            
ERR00564 GOTOR PUTERR,DMCB,('SE#SY013',564) NO STEXT FOUND                      
         J     EXITN                                                            
ERR00574 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDOVR) OVRRID FORAMT IS NN/NN         
         J     EXITN                                                            
ERR00575 GOTOR PUTERR,DMCB,('SE#SY013',SE#INVOV) OVRD MUST= LEN                 
         J     EXITN                                                            
ERR00577 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDCML) CML 4 ALHPA/4 NUM              
         J     EXITN                                                            
ERR00579 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDCLS) CLASS MUST BE 1-4 CHAR         
         J     EXITN                                                            
ERR00580 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDSOL) BLANK/P/S                      
         J     EXITN                                                            
ERR00581 GOTOR PUTERR,DMCB,('SE#SY013',SE#NORES) CAN'T RESTORE DEL              
         J     EXITN                                                            
ERR00582 GOTOR PUTERR,DMCB,('SE#SY013',SE#DUPRD) DUP PRODS ENTERED              
         J     EXITN                                                            
ERR00621 GOTOR PUTERR,DMCB,('SE#SY013',621) P/B CML SAME                        
         J     EXITN                                                            
ERR00635 GOTOR PUTERR,DMCB,('SE#SY013',635) ALL % MUST = 99 OR 100              
         J     EXITN                                                            
ERR00638 GOTOR PUTERR,DMCB,('SE#SY013',638) LIST TOO LONG TO CREATE             
         J     EXITN                                                            
ERR00643 GOTOR PUTERR,DMCB,('SE#SY013',643) ESTIMATE NOT FOUND                  
         J     EXITN                                                            
ERR00644 GOTOR PUTERR,DMCB,('SE#SY013',SE#NOBLK) NO BLANKS IN CMLS CODE         
         J     EXITN                                                            
ERR00659 GOTOR PUTERR,DMCB,('SE#SY013',659) PAT DATES NOT IN EST                
         J     EXITN                                                            
ERR00661 GOTOR PUTERR,DMCB,('SE#SY013',SE#CCOVR) CML USED BY COVER              
         J     EXITN                                                            
ERR00662 GOTOR PUTERR,DMCB,('SE#SY013',SE#CISCV) CML IS COV CML                 
         J     EXITN                                                            
ERR00664 GOTOR PUTERR,DMCB,('SE#SY013',SE#NPRDM) PRDS DO NOT MATCH              
         J     EXITN                                                            
ERR00665 GOTOR PUTERR,DMCB,('SE#SY013',SE#COV2) MUST BE 2+ ACT CML              
         J     EXITN                                                            
ERR00666 GOTOR PUTERR,DMCB,('SE#SY013',SE#SLNEQ) ACT SLN <> COV SLN             
         J     EXITN                                                            
ERR00667 GOTOR PUTERR,DMCB,('SE#SY013',SE#NOCOV) COVD CML CANT BE               
         J     EXITN                                                            
ERR00668 GOTOR PUTERR,DMCB,('SE#SY013',SE#SMCML) CML CANT COV ITSELF            
         J     EXITN                                                            
ERR00669 GOTOR PUTERR,DMCB,('SE#SY013',SE#TYPER) CML TYPE <>                    
         J     EXITN                                                            
ERR00677 GOTOR PUTERR,DMCB,('SE#SY013',677) UFN INVALID T1 PROF 15              
         J     EXITN                                                            
ERR00681 GOTOR PUTERR,DMCB,('SE#SY013',681) MAX ENTRIES IN MKT/STA              
         J     EXITN                                                            
ERR00689 GOTOR PUTERR,DMCB,('SE#SY013',689) INV MKT/STA ENTRY                   
         J     EXITN                                                            
ERR00695 GOTOR PUTERR,DMCB,('SE#SY013',695) COPY CODE MORE THAT 1 CHAR          
         J     EXITN                                                            
ERR00696 GOTOR PUTERR,DMCB,('SE#SY013',696) CML P/B/SOLO CDE USAGE              
         J     EXITN                                                            
ERR00708 GOTOR PUTERR,DMCB,('SE#SY013',708)  PATTERN CML ERROR                  
         J     EXITN                                                            
ERR00722 GOTOR PUTERR,DMCB,('SE#SY013',SE#ADIDX)  INVALID ADID                  
         J     EXITN                                                            
ERR00734 GOTOR PUTERR,DMCB,('SE#SY013',SE#ANCML) ALPHA NUMERIC CML ONLY         
         J     EXITN                                                            
ERR00735 GOTOR PUTERR,DMCB,('SE#SY013',SE#PRCNT) PCT MUST SUM TO 100            
         J     EXITN                                                            
ERR00736 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDTEL) TLCSTR MUST BE 8 CHAR          
         J     EXITN                                                            
ERR00737 GOTOR PUTERR,DMCB,('SE#SY013',SE#DPTEL) TLCSTR ALREADY ON FILE         
         J     EXITN                                                            
ERR00739 GOTOR PUTERR,DMCB,('SE#SY013',SE#CMEDT) T2PROF7 ADD ONLY MED T         
         J     EXITN                                                            
ERR00741 GOTOR PUTERR,DMCB,('SE#SY013',SE#NOTAL) NO TAL TRNSFR PRD=ALL          
         J     EXITN                                                            
ERR00742 GOTOR PUTERR,DMCB,('SE#SY013',SE#YNTAL) TAL TRNSFR Y/N                 
         J     EXITN                                                            
ERR00744 GOTOR PUTERR,DMCB,('SE#SY013',SE#NOTAL) MISSING TALENT AGY             
         J     EXITN                                                            
ERR00745 GOTOR PUTERR,DMCB,('SE#SY013',SE#PRBAS) PRD SET UP AS BASE             
         J     EXITN                                                            
ERR00746 GOTOR PUTERR,DMCB,('SE#SY013',SE#INPER) INVALID PERIOD                 
         J     EXITN                                                            
ERR00747 GOTOR PUTERR,DMCB,('SE#SY013',SE#INTME) INVALID TIME                   
         J     EXITN                                                            
ERR00749 GOTOR PUTERR,DMCB,('SE#SY013',SE#ONEPR) ONE PRD PER CML                
         J     EXITN                                                            
ERR00756 GOTOR PUTERR,DMCB,('SE#SY013',SE#DUPHI) DUP HIDEF CML                  
         J     EXITN                                                            
ERR00765 GOTOR PUTERR,DMCB,('SE#SY013',765) CML S/B 9-12 CHAR                   
         J     EXITN                                                            
ERR00768 GOTOR PUTERR,DMCB,('SE#SY013',768) EITHER ROTATION OR PCTS             
         J     EXITN                                                            
ERR00769 GOTOR PUTERR,DMCB,('SE#SY013',769) INVALID ROTATION LETTER             
         J     EXITN                                                            
ERR00783 GOTOR PUTERR,DMCB,('SE#SY013',783) NOT 8 OR 9-12 ADID                  
         J     EXITN                                                            
ERR00771 GOTOR PUTERR,DMCB,('SE#SY013',771) EST TO USE DPT                      
         J     EXITN                                                            
ERR00772 GOTOR PUTERR,DMCB,('SE#SY013',772) INV DAYPART ENTRY                   
         J     EXITN                                                            
ERR00773 GOTOR PUTERR,DMCB,('SE#SY013',773) START TIME NOT BEFORE END           
         J     EXITN                                                            
ERR00774 GOTOR PUTERR,DMCB,('SE#SY013',774) ENTER TIME OR DPT NOT BOTH          
         J     EXITN                                                            
ERR00775 GOTOR PUTERR,DMCB,('SE#SY013',775) PTTNS HAVE DIFFERENT DATES          
         J     EXITN                                                            
ERR00777 GOTOR PUTERR,DMCB,('SE#SY013',777) NO TIME ON PTTN AND CML             
         J     EXITN                                                            
ERR00778 GOTOR PUTERR,DMCB,('SE#SY013',778) NO DPTS AND TIMES IN PTTNS          
         J     EXITN                                                            
ERR00779 GOTOR PUTERR,DMCB,('SE#SY013',779) CAN'T CHG DPT:DEL & READD           
         J     EXITN                                                            
ERR00791 GOTOR PUTERR,DMCB,('SE#SY013',791) FILE HAS DAILY,NEW DOES NOT         
         J     EXITN                                                            
ERR00792 GOTOR PUTERR,DMCB,('SE#SY013',792) NEW HAS DLY, FILE DOES NOT          
         J     EXITN                                                            
ERR00800 GOTOR PUTERR,DMCB,('SE#SY013',SE#SNPER) SUB DTES NO IN PER             
         J     EXITN                                                            
ERR00811 GOTOR PUTERR,DMCB,('SE#SY013',SE#NDADD) NO ADD DELETED REC             
         J     EXITN                                                            
ERR00814 GOTOR PUTERR,DMCB,('SE#SY013',814) CML CHKSUM VALIDATION ERR           
         J     EXITN                                                            
ERR00815 GOTOR PUTERR,DMCB,('SE#SY013',815) H IS THE ONLY VALID FORMAT          
         J     EXITN                                                            
*                                                                               
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                                     
***********************************************************************         
*                                                                               
PUTERR   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLG,1            SET ERROR FLAG ON                            
         MVC   WORK(2),2(R1)                                                    
*                                                                               
         L     RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         MVC   LIOBMSYS,0(R1)      GET ERROR MESSAGE FROM SYSTEM                
         DROP  RF                                                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',10),          +        
               ('LD_CHARQ',QCLTA),(L'QCLTA,0)                                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),    +        
               WORK,0                                                           
*                                                                               
PUTERRX  J     EXITY                                                            
*                                                                               
* PUT OUT MY ERROR MESSAGE                                                      
*                                                                               
PUTMYERR NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLG,1            SET ERROR FLAG ON                            
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',10),          +        
               ('LD_CHARQ',QCLTA),(L'QCLTA,0)                                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLERR),    +        
               ('LD_CHARQ',ERRORMSG),(L'ERRORMSG,0)                             
         J     EXITY                                                            
*                                                                               
*                                                                               
*&&DO                                                                           
GETEL    AH    R4,DATADISP                                                      
         J     NEXTEL2                                                          
NEXTEL   CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
NEXTEL2  CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R4)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R4)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*&&                                                                             
         DROP  RB                                                               
*                                                                               
*                                                                               
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
*                                                                               
LVALUES  DS    0F                                                               
         EJECT                                                                  
*                                                                               
RECTAB   DS    0XL(RECTABL)                ** RECORD TABLE **                   
         DC    AL2(I#STCMLU),AL3(CMLUPLD),X'FF'      X'0600'                    
         DC    AL2(I#STPATU),AL3(PATUPLD),X'FF'      X'0601'                    
*                                                                               
*                                                                               
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
         EJECT                                                                  
*                                                                               
*COMMERCIAL FIELD NAMES                                                         
*                                                                               
TCMLBRND DC    C'Brand(s)'                                                      
TCMLFMT  DC    C'Format'                                                        
TCMLTTL  DC    C'Title'                                                         
TCMLTTL2 DC    C'Title 2'                                                       
TCMLTTL3 DC    C'Title 3'                                                       
TCMLPBSO DC    C'Piggyback/Solo'                                                
TCMLPRNT DC    C'Parent commercial'                                             
TCMLFDTE DC    C'First air date'                                                
TCMLLDTE DC    C'Last air date'                                                 
TCMLSTME DC    C'Air date start time'                                           
TCMLETME DC    C'Air date end time'                                             
TCMLDDTE DC    C'Destroy date'                                                  
TCMLDTME DC    C'Destroy time'                                                  
TCMLLEN  DC    C'Total length'                                                  
TCMLOVRD DC    C'Override'                                                      
TCMLTDLY DC    C'Time is daily'                                                 
TCMLMDTE DC    C'Matching dates'                                                
TCMLCLTN DC    C'CLT#'                                                          
TCMLCLAR DC    C'Exclude from Clarus'                                           
TCMLACML DC    C'Actual commercials'                                            
TCMLCLAS DC    C'Class'                                                         
TCKSM    DC    C'Record check sum'                                              
*TCMLTEXT DC    C'Text present Y/N'                                             
*TCMLTLC# DC    C'Telecaster #'                                                 
*TCMLTLH# DC    C'Telecaster HD #'                                              
*TCMLCBC  DC    C'CBC/STC#'                                                     
*TCMLCYCL DC    C'Talent cycle'                                                 
*TCMLSPCO DC    C'Substitute product code'                                      
*TCMLSPNA DC    C'Substitute product name'                                      
*TCMLSSDT DC    C'Substitute start date'                                        
*TCMLSEDT DC    C'Substitute end date'                                          
*                                                                               
*                                                                               
*PATTERN FIELD NAMES                                                            
*                                                                               
**TPATPROD DC    C'Product'                                                     
TPATLEN  DC    C'Length'                                                        
TPATPTR  DC    C'Partner'                                                       
TPATLEN2 DC    C'Partner length'                                                
TPATCODE DC    C'Code'                                                          
TPATDESC DC    C'Description'                                                   
TPATREF  DC    C'Pattern reference'                                             
TPATSTR  DC    C'Period start'                                                  
TPATEND  DC    C'Period end'                                                    
TPATSTME DC    C'Start time'                                                    
TPATETME DC    C'End time'                                                      
TPATTDLY DC    C'Times Daily'      PATTERN TIMES ARE FOR                        
TPATDAYP DC    C'Daypart'                                                       
TPATSTXT DC    C'Special comments'                                              
TPATINVP DC    C'Invert p/b products'                                           
TPATTYP  DC    C'Type'                                                          
TPATMKT  DC    C'Market(s)'                                                     
TPATSTA  DC    C'Stations'                                                      
TPATMGRP DC    C'Market group(s)'                                               
TPATFLDA DC    C'Field alpha'                                                   
TPATCML1 DC    C'Commercial'                                                    
TPATCML2 DC    C'Piggyback commercial'                                          
TPATPCT  DC    C'Percent'                                                       
TPATROT  DC    C'Rotation'                                                      
TPATCMT  DC    C'Pattern comments'                                              
*TCKSM   DC    C'Record check sum'                                              
*                                                                               
*                                                                               
*------------------                                                             
* ERRROR MESSAGES                                                               
*------------------                                                             
*                                                                               
MLTCLTMS DC    C'MULTI-CLIENT FOR ACTION ADD ONLY'                              
MISSMS   DC    C'MISSING MKT/STA'                                               
MISSPCT  DC    C'MISSING/PERCENTAGE'                                            
MISSROT  DC    C'MISSING/EXTRA ROTATION'                                        
MISSFLD  DC    C'MISSING FIELD ALPHA'                                           
MISSCML  DC    C'MISSING COMMERCIAL'                                            
MISSCOD  DC    C'MISSING COPY CODE ESTIMATE'                                    
MISSREF  DC    C'MISSING PATTERN REFERENCE NUMBER'                              
MISSTYP  DC    C'MISSING PATTERN TYPE'                                          
REFNOTF  DC    C'REFERENCE NUMBER NOT FOUND'                                    
REF2BIG  DC    C'REF # LARGER THAN 16383'                                       
INVPRDCD DC    C'INVALID PRODUCT CODE'                                          
INVDPT   DC    C'INVALID DAYPART'                                               
MGRSNF   DC    C'MKT GROUP SCHEME XX DOES NOT EXIST'                            
NOESTER  DC    C'NO ESTIMATE IN REQUESTGED PERIOD'                              
NOTDEL   DC    C'PATTERN NOT DELETED'                                           
INVPYN   DC    C'Y/N VALID ENTRIES FOR INVERTED PRODUCT'                        
IPREQPB  DC    C'INVERT =Y REQUIRES P/B/ PRODUCT'                               
NOSTMERR DC    C'NO END TIME WITHOUT START TIME'                                
NOETMUFN DC    C'PATTERN END UFN, NO END TIME'                                  
ENDTERR  DC    C'END TIME ERROR'                                                
INVDAILY DC    C'Y/N VALID DAILY ENTRIES'                                       
DLYTMER  DC    C'TIME REQUIRED FOR DAILY PATTERN'                               
INVTIME  DC    C'INVALID TIME'                                                  
DTOVERL  DC    C'PATTERN REF     DATES MMMDD/YY-MMMDD/YY OVERLAP'               
BADCMLD  DC    C'             DATES OUTSIDE PATTERN PERIOD '                    
NOPRDPA  DC    C'PAT PROD(S) NOT IN COMMERCIAL'                                 
PATCMLL  DC    C'PAT/CML SPOT LENS DIFFERENT'                                   
DIFTYPE  DC    C'CML PAIR HAVE DIFFERENT TYPES'                                 
DUPCML   DC    C'DUPLICATE COMMERCIAL IN LIST'                                  
CMT2MNY  DC    C'TOO MANY COMMENT LINES, MAX 4'                                 
CMT2LNG  DC    C'COMMENT LINE TOO LONG, MAX 53'                                 
INVSTA   DC    C'      INVALID STATION'                                         
INVMKT   DC    C'     INVALID MARKET'                                           
INVMGR   DC    C'       INVALID MARKET GROUP'                                   
MGRNFND  DC    C'       NO MARKET GROUP FOUND'                                  
ABSMSG   DC    C'0638# LIST TOO LONG TO CREATE'                                 
*UPMSER  DC    C'DUPLICATE MKT/STA IN LIST'                                     
*                                                                               
*                                                                               
B#CMLREC EQU   3                   IO3 - COMMERCIAL RECORD                      
B#PATREC EQU   3                   IO3 - PATTERN RECORD                         
*                                                                               
***********************************************************************         
* KEY - STRAFFIC COMMERCIAL RECORD                                              
***********************************************************************         
*                                                                               
CMLHEAD  LKREQ H,I#STCMLU,0,NEWREC=Y         0600                               
RECACT   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=SP#ACTN,COL=*                 
MEDIA    LKREQ F,1,(I,B#SAVED,I$CAM),CHAR,                             +        
               MAXLEN=1,OLEN=1,TEXT=SP#MED,COL=*                                
CLIENT   LKREQ F,2,(I,B#SAVED,I$CCLT),VSTR,                            +        
               MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
CMLLST   LKREQ F,3,(D,B#SAVED,QCMLLST),CHAR,TEXT=SP#FILM,COL=*                  
*                                                                               
FORMAT   LKREQ F,20,(D,B#SAVED,QFORMAT),CHAR,TEXT=(*,TCMLFMT),COL=*             
TCMLBRND LKREQ F,21,(D,B#SAVED,QPRDLIST),CHAR,TEXT=(*,TCMLBRND),COL=*           
CMLTITLE LKREQ F,22,(D,B#SAVED,QCMLTTLE),CHAR,TEXT=(*,TCMLTTL),COL=*            
CMLDSC2  LKREQ F,23,(D,B#SAVED,QCMLDSC2),CHAR,TEXT=(*,TCMLTTL2),COL=*           
CMLDSC3  LKREQ F,24,(D,B#SAVED,QCMLDSC3),CHAR,TEXT=(*,TCMLTTL3),COL=*           
CMLSOLO  LKREQ F,25,(D,B#SAVED,QCMLSOLO),CHAR,TEXT=(*,TCMLPBSO),COL=*           
CMLXPRNT LKREQ F,26,(D,B#SAVED,QCMLPRNT),CHAR,TEXT=(*,TCMLPRNT),COL=*           
CMLRLSE  LKREQ F,27,(D,B#SAVED,QCMLRLSE),BDAT,TEXT=(*,TCMLFDTE),COL=*           
CMLRCL   LKREQ F,28,(D,B#SAVED,QCMLRCL),CHAR,TEXT=(*,TCMLLDTE),COL=*            
CMLMSTIM LKREQ F,29,(D,B#SAVED,QCMLMSTM),CHAR,TEXT=(*,TCMLSTME),COL=*           
CMLMETIM LKREQ F,30,(D,B#SAVED,QCMLMETM),CHAR,TEXT=(*,TCMLETME),COL=*           
CMLXDSDT LKREQ F,31,(D,B#SAVED,QCMLDSDT),BDAT,TEXT=(*,TCMLDDTE),COL=*           
CMLXDSTM LKREQ F,32,(D,B#SAVED,QCMLDSTM),CHAR,TEXT=(*,TCMLDTME),COL=*           
CMLSLN   LKREQ F,33,(D,B#SAVED,QCMLSLN),LBIN,TEXT=(*,TCMLLEN),COL=*             
CMLOVRD  LKREQ F,34,(D,B#SAVED,QCMLOVRD),CHAR,TEXT=(*,TCMLOVRD),COL=*           
CMLMFLAG LKREQ F,35,(D,B#SAVED,QDLYFLG),CHAR,TEXT=(*,TCMLTDLY),COL=*            
CMLMSDT1 LKREQ F,36,(D,B#SAVED,QCMLMDTE1),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLMSDT2 LKREQ F,37,(D,B#SAVED,QCMLMDTE2),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLMSDT3 LKREQ F,38,(D,B#SAVED,QCMLMDTE3),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLMSDT4 LKREQ F,39,(D,B#SAVED,QCMLMDTE4),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLMSDT5 LKREQ F,40,(D,B#SAVED,QCMLMDTE5),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLMSDT6 LKREQ F,41,(D,B#SAVED,QCMLMDTE6),CHAR,TEXT=(*,TCMLMDTE),COL=*          
CMLCLTNO LKREQ F,42,(D,B#SAVED,QCMLCLTN),CHAR,TEXT=(*,TCMLCLTN),COL=*           
CMLTALEX LKREQ F,43,(D,B#SAVED,QCMLTALX),CHAR,TEXT=(*,TCMLCLAR),COL=*           
CMLACTCM LKREQ F,44,(D,B#SAVED,QCMLACT1),CHAR,TEXT=(*,TCMLACML),COL=*           
CMLACTC2 LKREQ F,45,(D,B#SAVED,QCMLACT2),CHAR,TEXT=(*,TCMLACML),COL=*           
CMLACTC3 LKREQ F,46,(D,B#SAVED,QCMLACT3),CHAR,TEXT=(*,TCMLACML),COL=*           
CMLACTC4 LKREQ F,47,(D,B#SAVED,QCMLACT4),CHAR,TEXT=(*,TCMLACML),COL=*           
CMLCLAS  LKREQ F,48,(D,B#SAVED,QCMLCLAS),CHAR,TEXT=(*,TCMLCLAS),COL=*           
CKSUM    LKREQ F,50,(D,B#SAVED,QCKSM),HEXD,TEXT=(*,TCKSM),COL=*                 
*                                                                               
*MLTELNO LKREQ F,58,(D,B#SAVED,QCMLTELNO),CHAR,TEXT=(*,TCMLTLC#),COL=*          
*MLXTLHD LKREQ F,59,(D,B#SAVED,QCCMLXTLHD),CHAR,TEXT=(*,TCMLTLH#),COL=*         
*MLCBCNM LKREQ F,60,(D,B#SAVED,QCMLCBCNM),CHAR,TEXT=(*,TCMLCBC),COL=*           
*MLTCYC1 LKREQ F,61,(D,B#SAVED,QCMLTCYC1),CHAR,TEXT=(*,TCMLCYCL),COL=*          
*MLTCYC2 LKREQ F,62,(D,B#SAVED,QCMLTCYC2),CHAR,TEXT=(*,TCMLCYCL),COL=*          
*MLPRSPR LKREQ F,63,(D,B#SAVED,QCMLPRSPR),TEXT=(*,TCMLSPCO),COL=*               
*MLPRSNM LKREQ F,64,(D,B#SAVED,QCMLPRSNM),(R,PRDNME),                +          
*              TEXT=(*,TCMLSPNA),COL=*                                          
*MLTCYC2 LKREQ F,65,(D,B#SAVED,QCMLTCYC2),CHAR,TEXT=(*,TCMLCYCL),COL=*          
*MLPRSDT LKREQ F,66,(D,B#SAVED,QCMLPRSDT),BDAT,TEXT=(*,TCMLSSDT),COL=*          
*MLPREDT LKREQ F,67,(D,B#SAVED,QCMLPREDT),BDAT,TEXT=(*,TCMLSEDT),COL=*          
         LKREQ E                                                                
*                                                                               
*                                                                               
***********************************************************************         
* KEY - STRAFFIC PATTERN RECORD                                                 
***********************************************************************         
*                                                                               
PATHEAD  LKREQ H,I#STPATU,0,NEWREC=Y         0601                               
RECACT   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=SP#ACTN,COL=*                 
MEDIA    LKREQ F,1,(I,B#SAVED,I$CAM),CHAR,                             +        
               MAXLEN=1,OLEN=1,TEXT=SP#MED,COL=*                                
CLIENT   LKREQ F,2,(I,B#SAVED,I$CCLT),VSTR,                            +        
               MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
*LIENT   LKREQ F,2,(I,B#SAVED,CLTIND),CHAR,LIST=F,                              
*****          MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
PROD1    LKREQ F,3,(D,B#SAVED,QPROD1),CHAR,TEXT=SP#PRO,COL=*                    
LEN1     LKREQ F,4,(D,B#SAVED,QLEN1),LBIN,TEXT=(*,TPATLEN),COL=*                
PROD2    LKREQ F,5,(D,B#SAVED,QPROD2),CHAR,TEXT=(*,TPATPTR),COL=*               
LEN2     LKREQ F,6,(D,B#SAVED,QLEN2),LBIN,TEXT=(*,TPATLEN2),COL=*               
PATCODE  LKREQ F,7,(D,B#SAVED,QCODE),CHAR,TEXT=(*,TPATCODE),COL=*               
PATDESC  LKREQ F,8,(D,B#SAVED,QDESCR),CHAR,TEXT=(*,TPATDESC),COL=*              
PATREF   LKREQ F,9,(D,B#SAVED,QREF),LBIN,,TEXT=(*,TPATREF),COL=*                
PATSTR   LKREQ F,10,(D,B#SAVED,QPATSTR),BDAT,TEXT=(*,TPATSTR),COL=*             
PATEND   LKREQ F,11,(D,B#SAVED,QPATEND),CHAR,TEXT=(*,TPATEND),COL=*             
PATSTME  LKREQ F,12,(D,B#SAVED,QPATSTME),CHAR,TEXT=(*,TPATSTME),COL=*           
PATETME  LKREQ F,13,(D,B#SAVED,QPATETME),CHAR,TEXT=(*,TPATETME),COL=*           
PATTDLY  LKREQ F,14,(D,B#SAVED,QDAILY),CHAR,TEXT=(*,TPATTDLY),COL=*             
PATDPT   LKREQ F,15,(D,B#SAVED,QDPT),CHAR,TEXT=(*,TPATDAYP),COL=*               
PATSTEXT LKREQ F,16,(D,B#SAVED,QSTEXT),CHAR,TEXT=(*,TPATSTXT),COL=*             
INVPRD   LKREQ F,17,(D,B#SAVED,QINVPRD),CHAR,TEXT=(*,TPATINVP),COL=*            
PATTYPE  LKREQ F,18,(D,B#SAVED,QTYPE),CHAR,TEXT=(*,TPATTYP),COL=*               
PATMKT   LKREQ F,19,(I,B#SAVED,QMKTIND),CHAR,OLEN=L'QMKT,LIST=(F,NOD), +        
               MAXLEN=6,TEXT=(*,TPATMKT),SORT=N,COL=*                           
PATSTA   LKREQ F,20,(I,B#SAVED,QSTAIND),CHAR,OLEN=L'QSTA,LIST=(F,NOD), +        
               MAXLEN=L'QSTA,TEXT=(*,TPATSTA),COL=*                             
MGROUP   LKREQ F,21,(I,B#SAVED,QMGRPIND),CHAR,TEXT=(*,TPATMGRP),       +        
               LIST=(F,NOD),OLEN=L'SVMGRP,COL=*                                 
PATFLDA  LKREQ F,22,(I,B#SAVED,QFLDAIND),CHAR,OLEN=1,ARRAY=S,          +        
               SORT=N,TEXT=(*,TPATFLDA),COL=*                                   
PATCML1  LKREQ F,23,,CHAR,OLEN=L'SVCML1,                               +        
               SORT=N,TEXT=(*,TPATCML1),COL=*                                   
PATCML2  LKREQ F,24,,CHAR,OLEN=L'SVCML2,                               +        
               SORT=N,TEXT=(*,TPATCML2),COL=*                                   
PATPCT   LKREQ F,25,,CHAR,OLEN=L'SVPCT,                                +        
               SORT=N,TEXT=(*,TPATPCT),COL=*,ARRAY=E                            
*                                                                               
PATROT   LKREQ F,26,(D,B#SAVED,QROT),CHAR,TEXT=(*,TPATROT),COL=*                
PATCMT   LKREQ F,27,(I,B#SAVED,QCMTIND),CHAR,OLEN=L'SVPATCMT,          +        
               LIST=F,SORT=N,TEXT=(*,TPATCMT),COL=*,DELIM=C'|'                  
CKSUM    LKREQ F,50,(D,B#SAVED,QCKSM),HEXD,TEXT=(*,TCKSM),COL=*                 
*                                                                               
         LKREQ E                                                                
*                                                                               
         LKREQ X                                                                
*                                                                               
CML_D    DSECT                     Pattern cmls array                           
FLDA     DS    CL1                 field alpha                                  
CML1     DS    CL(L'SVCML1)        cml1                                         
CML2     DS    CL(L'SVCML2)        cml2                                         
PCT      DS    CL(L'SVPCT)         percentage                                   
CMLLNQ   EQU   *-CML_D             entry length                                 
*                                                                               
SVRDEF   CSECT                                                                  
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
EZEROS   DC    C'00000000'                                                      
*                                                                               
SAVED    DSECT                                                                  
WVALUES  DS    0X                                                               
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
*                                                                               
WVALUESL EQU   *-WVALUES                                                        
*                                                                               
AIOLIST  DS    XL20                10H'0'                                       
*                                                                               
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
*                                                                               
ALIOB    DS    A                   A(ALIOB)                                     
ALINKIO  DS    A                   A(LINKIO)                                    
ARECUP   DS    A                   A(ARECUP)                                    
VTRPACK  DS    A                   A(TRPACK)                                    
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
*                                                                               
PCTNXT   DS    A                   NEXT PERCENT SAVE AREA                       
ACOMML1  DS    A                   A(1ST CML)                                   
SVAREC   DS    A                   A(RECORD) TO PROCESS                         
*                                                                               
DATADISP DS    H                                                                
*                                                                               
SVR2     DS    F                                                                
*                                                                               
MKTSTACT DS    F                                                                
MAXPCT   DS    F                                                                
MINPCT   DS    F                                                                
COMDIV   DS    F                                                                
*                                                                               
FLDH     DS   0CL8                                                              
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FLD      DS   0CL64                                                             
FAKEFLD  DS    CL64                                                             
*                                                                               
JDTTODAY DS    PL4                 TODAY'S DATE (0CYYDDDF)                      
*                                                                               
SVBAGM   DS    X                                                                
SVBCLT   DS    CL2                                                              
SVQPRD   DS    CL3                                                              
*                                                                               
QSTA     DS    CL5                 STATION CALL LETTERS                         
QMKT     DS    CL4                 MARKET NUMBER EBCDIC                         
BMKT     DS    XL2                                                              
*                                                                               
         DS    0D                                                               
PCTTBL   DS    XL48                CMML LETTER(1)/CMML PCT(2)                   
REMTBL   DS    XL60                                                             
RELTAB   DS    XL8      4 ENTRIES OF 1 BYTE QUOTIENT, 1 BYTE REMAINDER          
         ORG   PCTTBL                                                           
NEW10EL  DS    A                                                                
MYPTNSTM DS    XL2                                                              
MYPTNETM DS    XL2                                                              
MYPTNDPT DS    C                                                                
SAMEDTS  DS    C                                                                
MYADJSTM DS    XL2                                                              
MYADJETM DS    XL2                                                              
PTADJSTM DS    XL2                                                              
PTADJETM DS    XL2                                                              
MYADJSDT DS    XL3                                                              
MYADJEDT DS    XL3                                                              
PTADJSDT DS    XL3                                                              
PTADJEDT DS    XL3                                                              
         ORG                                                                    
*                                                                               
VALTBL   DS    CL15                                                             
*                                                                               
INPLEN   DS    XL1                                                              
*                                                                               
LOOPCT   DS    XL1                                                              
MSCT     DS    XL1                 COUNT OF MKT/STA ENTRIES FOR ERR MSG         
SVMGRP   DS    CL6                 SAVE MGROUP (EX. BJ0001)                     
SVMGID   DS    XL1                 MGROUP ID HEX VALUE FROM TABLE               
SVPCT    DS    CL3            <<<<                                              
SVCML1   DS    CL12                 <<<<                                        
SVCML2   DS    CL12                   <<<<<<                                    
*                                                                               
SVCMLRCL DS    XL3                 SAVE COMMERCIAL RECALL DATE                  
*                                                                               
CMMLCT   DS    XL1                                                              
DELCT    DS    XL1                                                              
PCTCT    DS    XL1                 PERCENT COUNTER                              
PCTTOT   DS    XL1                 PERCENTAGE TOTAL                             
*                                                                               
SVTYPE1  DS    CL4               <-|                                            
SVSLEN1  DS    XL1                 |                                            
SVSLN1   EQU   SVSLEN1             |                                            
SVSOLO1  DS    CL1                 |                                            
SVFLAG1  DS    XL1                 | KEEP THESE TOGETHER                        
SVTYPE2  DS    CL4                 |   AND IN ORDER                             
SVSLEN2  DS    XL1                 |                                            
SVSLN2   EQU   SVSLEN2             |                                            
SVSOLO2  DS    CL1                 |                                            
SVFLAG2  DS    XL1               <-|                                            
*                                                                               
REFNUM   DS    CL5                 REF NUMBER OF ADDED PATTERN RECORD           
*                                                                               
CHREASON DS    CL2                 REASON FOR CHANGE                            
*                                                                               
NUMCML   DS    XL(L'LW_NUMN)                                                    
*                                                                               
PRDMATSW DS    XL1                 USED WITH VCML RTN                           
PRDMATPR EQU   X'80'               80 - PRODUCT FOUND IN COMMERCIAL             
PRDMATPT EQU   X'40'               40 - PARTNER FOUND IN CML                    
PRDMATBO EQU   X'C0'               C0 - BOTH PRODUCTS FOUND                     
PRDMATEQ EQU   X'08'               08 - PRD/PTR SPOT LENS=CML LEN               
*                                       -CML IS FOR PIGGYBACK PAIR              
*OCMLSW  EQU   X'04'               NO CMLS- INCOMPLETE BPAT                     
BPATSW   EQU   X'02'               IF ON, LIST BPAT RECORDS                     
LSTDELCM EQU   X'01'               01 - LIST - DELETED CMLS FOUND               
*                                                                               
NEWSUBSW DS    XL1                 SWITCH TO FORCE NEW REC, NEW SUBLINE         
*                                                                               
SVIOKEY  DS    XL(L'IOKEY)                                                      
*                                                                               
BTODAY   DS    CL3                                                              
SVUID    DS    CL2                                                              
DATE     DS    CL6                                                              
HOLDCML  DS    CL8                                                              
HOLDSEQ  DS    XL3                                                              
ADIDFLAG DS    C                                                                
NWHIDEFX DS    XL8                 NEW HIDEF HEX                                
SVHIDEFX DS    XL8                 ORIGINAL HIDEF HEX                           
SVHIDEF  DS    CL12                ORIGINAL HIDEF CHAR                          
SVHDDELK DS    XL13                                                             
SVHDADDK DS    XL13                                                             
SVDSKAD  DS    CL4                                                              
SVDSKADR DS    XL4                 SAVE DISK ADDR OF COMML REC                  
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
MYKEY2   DS    CL24                                                             
SVKEY    DS    CL24                                                             
CMLPRCNT DS    H                                                                
*                                                                               
SVLEN1   DS    X                   SAVE SPOT LEN 1                              
SVLEN2   DS    X                   SAVE SPOT LEN 2                              
*                                                                               
QPRDX2   DS    X                   PROD2 HEX CODE                               
*                                                                               
SVSLN    DS    X                   SAVE SPOT LEN                                
ACTCT    DS    X                   COUNT OF ACTUAL CMMLS USED                   
ACTSLN   DS    CL1                                                              
OLDACTS  DS    (NUMACTS)CL12                                                    
NEWACTS  DS    (NUMACTS)CL12                                                    
NUMACTS  EQU   4                   NUMBER OF ACTUALS ALLOWED                    
*                                                                               
DSTRYDAT DS    XL3                                                              
DSTRYTIM DS    XL2                                                              
*                                                                               
SYSFIL   DS    CL8                                                              
ELCODE   DS    XL1                                                              
*                                                                               
FLAGS    DS    X                   VARIOUS FLAGS                                
SOFTDEL  EQU   X'80'               JUST DID A SOFT DELETE                       
SOFTREST EQU   X'40'               JUST DID A SOFT RESTORE                      
ISCOVCML EQU   X'20'               THIS IS A COVER CMML                         
ISCOVERD EQU   X'10'               THIS CMML IS COVERED                         
UNCOVER  EQU   X'08'               ACTION UNCOVER                               
DELPATSW EQU   X'04'  was FLAGFTR  DELETE PATTERN REQUESTED                     
RESPATSW EQU   X'02'               RESTORE PATTERN REQUESTED                    
RECIOSW  EQU   X'01'               RECORD I/O DONE SWITCH                       
*                                                                               
*                                                                               
FLAGS2   DS    X                   VARIOUS FLAGS                                
PRDTHTR  EQU   X'80'               THEATRICAL PRODUCT                           
MATELEM  EQU   X'40'               ADDING MATCH ELEMENT (X'B0')                 
*        EQU   X'20'                                                            
*        EQU   X'10'                                                            
*        EQU   X'08'                                                            
*        EQU   X'04'                                                            
*        EQU   X'02'                                                            
*        EQU   X'01'                                                            
*                                                                               
CHGFLAGS DS    0XL3                                                             
CHGFLAG1 DS    XL1                                                              
CHGFLAG2 DS    XL1                                                              
CHGFLAG3 DS    XL1                                                              
*                                                                               
SVCLTOFF DS    CL1                 CLIENT OFFICE CODE                           
SVTTPR3  DS    CL1                 TALENT TRANSFER USET (TT PROFILE)            
SVT2PR8  DS    CL1                 LIMIT ONE PRD PER ISCII                      
SVPROF14 DS    CL1                 CLASS                                        
*                                                                               
SVPROF10 DS    CL1                 PATTERN AUTO T/A                             
SVPROF11 DS    CL1                 PATTERN EST/DAYP/NONE                        
SVT2PR3  DS    CL1                 PATTERN ALLOW NON-STD %                      
SVT1PR15 DS    CL1                 PATTERN UFN INVALID                          
*                                                                               
HASTIME  DS    C                                                                
HASDPT   DS    C                                                                
VCMLFLAG DS    XL1                                                              
DELFLAG  DS    C                   C'N'=DON'T SHOW DELETED CMMLS                
*                                                                               
BREF     DS    H                   REF NUM BINARY                               
BSUB     DS    H                   SUBLINE                                      
BREFSUB  DS    XL3                 REF NUM (14 BITS)/SUBLINE (10 BITS)          
REF      DS    CL5                                                              
CODE     DS    CL1                                                              
CODESW   DS    CL1                 Y-THIS IS A COPY CODE = EST PATTERN          
ESTSTR   DS    XL3                 ESTIMATE DATES FOR COPY CODE = EST           
ESTEND   DS    XL3                                                              
*                                                                               
PATDTS   DS    0XL6                                                             
STRTPAT  DS    XL3                 SAVED PATSTART FOR COMP TO CMML'ST           
ENDPAT   DS    XL3                 SAVED PATEND FOR COMP TO CMML'S              
*                                                                               
QBEST    DS    CL1                 REQUESTED START EST (BINARY)                 
QBESTEND DS    CL1                 REQUESTED END EST                            
ACTUAL   DS    2CL1                                                             
SVMENU   DS    XL1                                                              
SVBREF   DS    XL2                                                              
ERRORMSG DS    CL60                                                             
SVOLDDPT DS    C                                                                
AMEDIA   DS    C                   ALPHA MEDIA                                  
*                                                                               
SVPATCMT DS    CL53                PATTERN COMMENT                              
*                                                                               
ERRFLG   DS    CL1                 ERROR FLAG                                   
NXTCML   DS    F                   ADDRESS OF NEXT CML TO PROCESS               
ANXTCLT  DS    A                   ADDRESS OF NEXT CLT TO PROCESS               
*                                                                               
*COMMERCIAL FIELDS                                                              
*                                                                               
QFORMAT  DS    CL1                                                              
QPRDLIST DS    CL37                                                             
QCOMML   DS   0CL12                                                             
QCMLLST  DS    CL120                                                            
QCMLLSTQ EQU   *                                                                
QCMLTTLE DS    CL15                CMML TITLE                                   
QCMLDSC2 DS    CL20                                                             
QCMLDSC3 DS    CL20                                                             
QCMLSOLO DS    CL1                 C'S'=SOLO/C'P'=P/J                           
QCMLPRNT DS    CL12                PARENT COMMERCIAL                            
QCMLRLSE DS    CL11                RELEASE DATE (YMD)                           
QCMLRCL  DS    CL11                RECALL DATE  (YMD)                           
QCMLMSTM DS    CL6                 START TIME                                   
QCMLMETM DS    CL6                 END TIME                                     
MILTIME  DS    CL2                 MILITARY TIME                                
QCMLDSDT DS    CL11                DESTROY DATE                                 
QCMLDSTM DS    CL5                 DESTROY TIME (2400=12A,0=NONE)               
QCMLSLN  DS    XL1                 CMML LENGTH                                  
QCMLOVRD DS    CL6                                                              
QDLYFLG  DS    XL1                 CHECK TIMES DAILY (Y/N)                      
*                                                                               
QCMLMDTE1 DS   CL17                MATCH DATES                                  
QCMLMDTE2 DS   CL17                                                             
QCMLMDTE3 DS   CL17                                                             
QCMLMDTE4 DS   CL17                                                             
QCMLMDTE5 DS   CL17                                                             
QCMLMDTE6 DS   CL17                                                             
*                                                                               
QCMLCLTN DS    CL20                CLIENT CMML NUMBER                           
QCMLTALX DS    CL1                 EXCL TAL TRANSFER                            
QCMLCLAS DS    CL50                CLASS                                        
*                                                                               
QCMLACT1 DS    CL12                8-12 CHAR ACTUAL CML (SPACE PADDED)          
QCMLACT2 DS    CL12                                                             
QCMLACT3 DS    CL12                                                             
QCMLACT4 DS    CL12                                                             
*                                                                               
QCKSM    DS    XL4                 RECORD CHECK SUM                             
*                                                                               
*                                                                               
*PATTERN FIELDS                                                                 
*                                                                               
QPROD1   DS    CL3                                                              
QLEN1    DS    XL1                                                              
QPROD2   DS    CL3                                                              
QLEN2    DS    XL1                                                              
QCODE    DS    CL3                                                              
QDESCR   DS    CL17                PATTERN DESCRIPTION                          
QREF     DS    CL2                 PATTERN REF NUMBER                           
QPATSTR  DS    CL11                                                             
QPATEND  DS    CL11                                                             
QPATSTME DS    CL6                 START TIME                                   
QPATETME DS    CL6                 END TIME                                     
QDAILY   DS    CL1                 TIMES DAILY (Y/N)                            
QDPT     DS    CL1                 DAYPART                                      
QSTEXT   DS    CL6                                                              
QINVPRD  DS    CL1                 INVERT PRODS (Y/N)                           
QTYPE    DS    CL1                 TYPE (S)TATION, (M)ARKET (G)RP               
*                                                                               
*CLTIND   DS    X                   Client array                                
*ACLT     DS    AL3                                                             
*NUMCLT   DS    XL(L'LW_NUMN)       N'CLT TO PROCESS                            
*                                                                               
QMKTIND  DS    X                                                                
AMKT     DS    AL3                 ARRAY OF MARKETS                             
NUMMKT   DS    XL(L'LW_NUMN)       N'MARKETS TO PROCESS                         
*                                                                               
QSTAIND  DS    X                                                                
ASTATION DS    AL3                 ARRAY OF STATIONS                            
NUMSTA   DS    XL(L'LW_NUMN)       N'STATION TO PROCESS                         
*                                                                               
QMGRPIND DS    X                                                                
AMGRP    DS    AL3                 ARRAY OF MGROUPS                             
NUMMGR   DS    XL(L'LW_NUMN)       N'MGROUPS TO PROCESS                         
*                                                                               
QFLDAIND DS    X                                                                
AFLDA    DS    AL3                 ARRAY OF FIELD ALPHA                         
NUMFLDA  DS    XL(L'LW_NUMN)       N'FIELD ALPHAS TO PROCESS                    
*                                                                               
*                                                                               
QROT     DS    CL78                                                             
*                                                                               
QCMTIND  DS    X                                                                
APATCMT  DS    AL3                 ARRAY OF PATTERN COMMENTS                    
NUMPATCM DS    XL(L'LW_NUMN)       N'PATTERN COMMENTS TO PROCESS                
*                                                                               
*QCKSM    DS    XL4                 RECORD CHECK SUM                            
*                                                                               
*                                                                               
*  >>>>>>>>>>>>>>>>>>>> HERE  <<<<<<<<<<<<<<<<<<                                
*                                                                               
*PREFIX=Q                                                                       
       ++INCLUDE SPGENCLT                                                       
*PREFIX=                                                                        
         ORG   QCPROF                                                           
QCPROF1  DS    C                                                                
QCPROF2  DS    C                                                                
QCPROF3  DS    C                                                                
QCPROF4  DS    C                                                                
QCPROF5  DS    C                                                                
QCPROF6  DS    C                                                                
QCPROF7  DS    C                                                                
QCPROF8  DS    C                                                                
QCPROF9  DS    C                                                                
QCPROF10 DS    C                                                                
QCPROF11 DS    C                                                                
QCPROF12 DS    C                                                                
QCPROF13 DS    C                                                                
QCPROF14 DS    C                                                                
QCPROF15 DS    C                                                                
         ORG   QCEXTRA                                                          
QCEXTRA1 DS    C                                                                
QCEXTRA2 DS    C                                                                
QCEXTRA3 DS    C                                                                
QCEXTRA4 DS    C                                                                
QCEXTRA5 DS    C                                                                
QCEXTRA6 DS    C                                                                
QCEXTRA7 DS    C                                                                
QCEXTRA8 DS    C                                                                
QCEXTRA9 DS    C                                                                
QCEXTRA10 DS   C                                                                
QCEXTRA11 DS   C                                                                
QCEXTRA12 DS   C                                                                
QCEXTRA13 DS   C                                                                
QCEXTRA14 DS   C                                                                
QCEXTRA15 DS   C                                                                
         ORG                                                                    
*                                                                               
QCTOKEN  DS    CL8                 TOKEN                                        
RECACT   DS    C                   RECORD ACTION                                
QPTOKEN  DS    CL8                 TOKEN                                        
QPRECACT DS    C                   RECORD ACTION                                
*                                                                               
QBILLBAS DS    C                   BILL BASIS, X'00', C'G', OR C'N'             
QCOMMBIL DS    C                   COMMISSION ONLY BILLING   Y/N                
QCOMMBAS DS    C                   COMM BASIS, X'00', C'G', OR C'N'             
*                                                                               
         DS    0F                                                               
DISKADDR DS    0AL4                                                             
TDYDTCHR DS    CL6                 TODAYS DATE (CHAR)                           
*                                                                               
*ISCFLG1 DS    X                   MISCELLANEOUS FLAGS 1                        
*F1ERROR EQU   X'80'                                                            
*                                                                               
MISCFLG2 DS    X                   MISCELLANEOUS FLAGS 2                        
*                                                                               
MISCFLG3 DS    X                   ERROR FLAG                                   
*                                                                               
*  FIELD INDEX ADDRESSES                                                        
*                                                                               
I$CAM    DS    A                   A(CLIENT AGENCY MEDIA)                       
I$CCLT   DS    A                   A(CLIENT CLIENT CODE)                        
*                                                                               
I$PAM    DS    A                   A(PRODUCT AGENCY MEDIA)                      
I$PCLT   DS    A                   A(PRODUCT CLIENT CODE)                       
I$PPRD   DS    A                   A(PRODUCT CLIENT CODE)                       
APBILBAS DS    A                   A(PRODUCT BILLBASIS)                         
*                                                                               
SAVEL    EQU   *-SAVED                                                          
*                                                                               
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRDTXT                                                       
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
*                                                                               
WORKD    DSECT                     ** REDEFINE OVERWORK **                      
         ORG   OVERWORK                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPMSGEQUS                                                      
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
*                                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE DMPRTQK                                                        
       ++INCLUDE FATCB                                                          
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
*                                                                               
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVVALL   EQU   *-SVVALS                                                         
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPLNK26   09/12/17'                                      
         END                                                                    
