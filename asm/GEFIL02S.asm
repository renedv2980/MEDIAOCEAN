*          DATA SET GEFIL02S   AT LEVEL 060 AS OF 08/29/00                      
*PHASE T00AB2A                                                                  
* AATK 058 28JUL99 - SUPPRESS SECRET CALL IF ASKING FOR 'HELP' HELP             
* AATK 056 15JUN99 - GCIRCHG FLAG SUPPORT CODE                                  
* TCLE 055 10JUN99 - DIFFERENT ERROR MESSAGES FOR UK IF ALREADY DELETED         
*                    OR ALREADY RESTORED                                        
* TCLE 054 15MAR99 - SAVE STATUS ON ACTION CHANGE                               
         EJECT                                                                  
GEFIL02  TITLE 'NEW FILE - OBJECTS II'                                          
         PRINT NOGEN                                                            
GEFIL02  CSECT                                                                  
         NMODL RTWORKL,GEFIL02*,CLEAR=YES,RR=RE                                 
         USING RTWORKD,RC                                                       
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    RE,RTRELO                                                        
         MVC   RTPARMS,0(R1)                                                    
         ST    R1,RTPARMA                                                       
*                                                                               
         LA    R4,OBJTAB                                                        
         USING OBJTABD,R4                                                       
OBJ02    CLI   OBJTABD,EOT                                                      
         JE    EXITOK                                                           
         CLC   OBJVERB,RTPARMS1+3                                               
         JE    *+12                                                             
         LA    R4,OBJTABL(R4)                                                   
         J     OBJ02                                                            
         ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R4                                                               
*                                                                               
OBJTAB   DC    AL1(ORTYPE,0,0,0),AL4(RTYPE)                                     
         DC    AL1(OACT,0,0,0),AL4(ACT)                                         
         DC    AL1(OACTH,0,0,0),AL4(ACTH)                                       
         DC    AL1(OPFK,0,0,0),AL4(PFKEY)                                       
         DC    AL1(OPAGE,0,0,0),AL4(PAG)                                        
OBJTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRAELD,GSFRAEL                                                   
PSFRA    USING FRAELD,PSFRAEL                                                   
GSFRP    USING FRPELD,GSFRPEL                                                   
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITOPT  XR    RF,RF                                                            
         ICM   RF,3,GSDSPOPT       EXIT WITH CURSOR ON OPTION FIELD             
         JNZ   EXITSET             OR FALL THROUGH TO KEY FIELD 1               
*                                                                               
EXITKEY  XR    RF,RF                                                            
         ICM   RF,3,GS1STKEY       EXIT WITH CURSOR ON 1ST KEY FIELD            
         JZ    EXITACT             OR FALL THROUGH TO ACTION FIELD              
         CLI   TWASESNL,1                                                       
         BNH   EXITSET                                                          
         LR    R1,RF               IF FIELD PROTECTED WHILST NESTED             
         A     R1,ATWA             EXIT WITH CURSOR ON ACTION FIELD             
         TM    FHAT-FHD(R1),FHATPR                                              
         JO    EXITACT                                                          
         J     EXITSET                                                          
*                                                                               
EXITACT  XR    RF,RF                                                            
         ICM   RF,3,GSDSPACT       EXIT WITH CURSOR ON ACTION FIELD             
         JNZ   EXITSET             OR FALL THROUGH TO RECORD FIELD              
*                                                                               
EXITREC  LH    RF,GS1STREC         EXIT WITH CURSOR ON 1ST RECORD FIELD         
         JNZ   EXITSET                                                          
*                                                                               
EXITSET  A     RF,ATWA             SET CURSOR POSITION                          
         ST    RF,FVADDR                                                        
         J     EXITOK                                                           
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
EXIT     L     R1,RTPARMA                                                       
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE - EXPECTS VERB TO JE IN R1                  *         
*                         - EXPECTS A(TABLE TO JE IN RF)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T - NOT KNOWN AT ANY LEVEL               
         JE    EXITH                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         JE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         J     ITER                ITERATE VERB TABLE                           
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,RTRELO                                                        
         LA    R1,RTPARMS          GET BACK PARAMETER LIST                      
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR SCROLLING                                       *         
*                                                                     *         
* EXIT: CC = EQUAL IF SCROLLED                                        *         
*       CC = HIGH IF DID NOT                                          *         
*       CC = LOW IF ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1  ,                                                                
         NI    GCINDS1,FF-GCISSCRL TURN OFF SCROLL BY PAGE FLAG                 
         GOTOX AGEN,RTPARM,OPAGE,PGVAL                                          
         JH    SCROLL02                                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         J     EXITOK                                                           
*                                                                               
SCROLL02 CLI   GSFRP.FRPTYPE,FRPTMFWD                                           
         JNE   SCROLL04                                                         
         GOTOX AGEN,RTPARM,OSCRN,SFRWD                                          
         J     EXITOK                                                           
*                                                                               
SCROLL04 CLI   GSFRP.FRPTYPE,FRPTMBAC                                           
         JNE   EXITH                                                            
         GOTOX AGEN,RTPARM,OSCRN,SBACK                                          
         J     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  RECORD TYPE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RTYPE    LR    R7,RF                                                            
         L     R1,RTPARMS2                                                      
         LA    RF,RTYTAB                                                        
         J     ITER                                                             
*                                                                               
RTYTAB   DC    AL1(RTVAL,0,0,0),AL4(RTYVAL)                                     
         DC    AL1(RTDIS,0,0,0),AL4(RTYDIS)                                     
         DC    AL1(RTTST,0,0,0),AL4(RTYTST)                                     
         DC    AL1(RTSET,0,0,0),AL4(RTYSET)                                     
         DC    AL1(RTREF,0,0,0),AL4(RTYREF)                                     
         DC    AL1(RTXVAL,0,0,0),AL4(RTYXVAL)                                   
         DC    AL1(RTXDIS,0,0,0),AL4(RTYXDIS)                                   
         DC    AL1(RTMCH,0,0,0),AL4(RTYMCH)                                     
         DC    AL1(RTRDIS,0,0,0),AL4(RTYRDIS)                                   
RTYTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
RTYVAL   XR    R3,R3                                                            
         ICM   R3,3,GSDSPREC       DISPLACEMENT TO RECORD FIELD                 
         JZ    EXITOK              CLEARED IF FOCUS OF 'SELECT'                 
         A     R3,ATWA                                                          
         USING FHD,R3              R3=(RECORD TYPE INPUT FIELD)                 
*                                                                               
         OC    CSREC,CSREC         HAVE A RECORD?                               
         JZ    *+12                NO                                           
         TM    FHII,FHIIVA         INPUT THIS TIME?                             
         JO    RTYVOK              NO - RECORD IS UNCHANGED                     
*                                                                               
         NI    GCINDS2,FF-(GCINTRS+GCIXITS)                                     
         MVI   FVMINL,1            SET INPUT REQUIRED                           
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         JNE   EXITL               FAILED VALIDATION                            
*                                                                               
         CLI   FVIFLD,C'?'         HELP REQUESTED?                              
         JNE   RTYV02              NO                                           
*                                                                               
         CLI   CSREC,O#RTYP        RECORD HELP DISPLAYED?                       
         JE    EXITOK              YES - IGNORE REQUEST                         
         CLI   CSREC,O#ACT         ACTION HELP DISPLAYED?                       
         JE    EXITOK              YES - IGNORE REQUEST                         
*                                                                               
*        MVC   RTHALF1,=AL1(O#RTYP,A#LST)                                       
*        GOTOX  ('TSTACS',AGROUTS),RTHALF1                                      
*        JE    *+14                ABLE TO ASK FOR HELP                         
*        MVC   FVMSGNO,=AL2(GE$NOSEL)                                           
*        J     EXITL               NO HELP FOR YOU SONNY                        
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#RTYP       RECORD HELP IS IN GEFILREC                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL    MAKE THIS A SELECTABLE LIST                  
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DC    H'0'                NEVER RETURN                                 
         DROP  N                                                                
*                                                                               
RTYV02   GOTOX AGEN,RTPARM,ORTYPE,RTMCH,0                                       
         JNE   EXITL                                                            
         GOTOX AGEN,RTPARM,ORTYPE,RTSET,AIO1                                    
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORTYPE,RTDIS,AIO1                                    
         JL    EXITL                                                            
*                                                                               
RTYVOK   OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         J     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY NAME INTO RECORD FIELD                                 *  2 *         
*                                                                ******         
* NTRY: P3=A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
RTYDIS   XR    R3,R3                                                            
         ICM   R3,3,GSDSPREC       DISPLACEMENT TO RECORD FIELD                 
         JZ    EXITOK              CLEARED IF THE FOCUS OF A SELECT             
         A     R3,ATWA                                                          
         USING FHD,R3              R3=(RECORD TYPE INPUT FIELD)                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         JZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),CSRECNAM    RECORD NAME SAVED IN HERE (RTSET)            
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA                                                      
         J     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TEST VALIDITY OF RECORD                                             *         
*                                                                     *         
* NTRY: P3 BYTE 0 = 0                                                 *         
*             1-3 = A(RECORD)                                         *         
*    OR P3 BYTE 0 = RECORD TYPE                                       *         
*             1-3 = 0                                                 *         
* EXIT: P3        = A(RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
RTYTST   XR    R2,R2                                                            
         ICM   R2,7,RTPARMS3+1     A(RECORD) PASSED?                            
         JNZ   RTYT02              YES                                          
*                                                                               
         XR    R1,R1               GO GET BEST RECORD RECORD                    
         ICM   R1,8,GCOVSYS        SYSTEM                                       
         ICM   R1,4,GCPRGNO        PROGRAM                                      
         ICM   R1,2,RTPARMS3       RECORD NUMBER                                
         GOTOX ('GETFRECD',AGROUTS),(R1)                                        
         JNE   EXITL                                                            
*                                                                               
         L     R2,AIO1             GET A(RECORD) INTO R2                        
         USING FRRRECD,R2                                                       
RTYT02   LA    R3,FRRFIRST(R2)                                                  
         ST    R2,RTPARMS3         SAVE A(RECORD) FOR CALLER                    
         USING FRRELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
RTYT04   CLI   FRREL,0             END OF RECORD?                               
         JNE   *+6                                                              
         DC    H'0'                NO FRREL                                     
*                                                                               
         CLI   FRREL,FRRELQ        RECORD ELEMENT?                              
         JE    RTYT06              YES                                          
         IC    RF,FRRLN                                                         
         AR    R3,RF               ITERATE RECORD                               
         J     RTYT04                                                           
*                                                                               
RTYT06   TM    FRRINDS1,FRR1DDS    TEST DDS ONLY RECORD TYPE                    
         JZ    RTYT08              NO                                           
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         JZ    EXITL                                                            
         J     EXITOK                                                           
*                                                                               
RTYT08   XC    RTHALF1,RTHALF1     TEST RECORD SECURITY                         
         OC    RTHALF1(1),FRRSEC   SET OVERRIDE VALUE ON RECORD?                
         JNZ   *+10                                                             
         MVC   RTHALF1(1),FRRKREC                                               
*                                                                               
         CLI   RTHALF1,O#RTYP      RECORD/ACTION HELP ALWAYS AUTHORISED         
         JE    EXITOK                                                           
         CLI   RTHALF1,O#ACT                                                    
         JE    EXITOK                                                           
         CLI   RTHALF1,O#SUBAC                                                  
         JE    EXITOK                                                           
*                                                                               
         GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
         JNE   EXITL               SORRY - YOU FAILED                           
         J     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET CURRENT RECORD TYPE VALUES                                      *         
*                                                                     *         
* NTRY: P3 BYTE 0 = 0                                                 *         
*             1-3 = A(RECORD)                                         *         
*    OR P3 BYTE 0 = RECORD NUMBER                                     *         
*             1-3 = 0                                                 *         
***********************************************************************         
         SPACE 1                                                                
RTYSET   XR    R2,R2                                                            
         ICM   R2,7,RTPARMS3+1     PASSED A(RECORD)?                            
         JNZ   RTYS02              YES                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTTST,RTPARMS3                                
         JL    EXITL               NOT ALLOWED TO SET THIS RECORD               
*                                                                               
         L     R2,8(R1)            A(RECORD) RETURNED HERE                      
         USING FRRRECD,R2                                                       
RTYS02   LA    R3,FRRFIRST(R2)     A(FIRST ELEMENT)                             
         USING FRRELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
RTYS03   CLI   FRREL,0             END OF RECORD                                
         JNE   *+6                                                              
         DC    H'0'                RECORD RECORD WITH NO FRRELD                 
         CLI   FRREL,FRRELQ        FRREL?                                       
         JE    RTYS04              YES                                          
         IC    RF,FRRLN                                                         
         AR    R3,RF               ITERATE RECORD                               
         J     RTYS03                                                           
*                                                                               
RTYS04   IC    RF,FRRLN            SAVE CURRENT FRREL                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   GSFRREL(0),FRRELD                                                
*                                                                               
         CLC   CSREC,FRRKREC       TEST CHANGE OF RECORD                        
         JE    RTYS08              NO                                           
*                                                                               
         OI    BCINDS1,BCINREC+BCINACT SET NEW RECORD & NEW ACTION R            
         XC    GSSCREEN,GSSCREEN   RESET SCREEN INDICATORS                      
         XC    GSINDSL,GSINDSL     RESET SESSION INDICATORS                     
         XC    CSINDSL1,CSINDSL1   RESET LOCAL SESSION INDICATORS               
         TM    GCINDS2,GCIDONTR    DOING AN NTRSES?                             
         JZ    *+10                                                             
         MVC   GSSMPAGE,PSSAV+(SMPAGE-SSAVD)                                    
         XC    LSSTAT1,LSSTAT1     RESET LIST INDICATORS                        
         XC    LSSTAT2,LSSTAT2                                                  
         XC    LSSTAT3,LSSTAT3                                                  
         XC    LSPFKLST,LSPFKLST   RESET PFK POINTERS                           
         XC    LSPFKNOW,LSPFKNOW                                                
         MVI   GS#FDR,0            RESET COUNT OF FIELD ELEMENTS                
*                                                                               
         LA    RE,GSFDRLST         CLEAR FIELD ELEMENT ARRAY                    
         LA    RF,L'GSFDRLST*GS#FDRMX                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
RTYS08   XC    CSRECNAM,CSRECNAM   CLEAR RECORD NAME                            
         MVC   CSREC,FRRKREC       SET RECORD NUMBER FOR GENWORK                
         MVC   GSREC,FRRKREC       SET RECORD NUMBER FOR FILWORK                
         MVI   CSRECNAM,DD#ESCL    LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   CSRECNAM+1(L'FRRDICT),FRRDICT                                    
         MVI   CSRECNAM+3,RECNAMLQ                                              
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FRRKSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),CSRECNAM TRANSLATE DICTIONARY NAME           
*                                                                               
         MVC   GSOVER,FRRPHASE     SET RECORD OVERLAY NUMBER                    
         MVC   GSSYS,FRRSYS        SET RECORD NATIVE SYSTEM                     
         MVC   GSDIR,FRRDIR        SET RECORD NATIVE DIRECTORY #                
         GOTOX ('SWCHFC',AGROUTS),GSSYS                                         
*                                                                               
         L     RF,AXFILTAB         PARSE FILE TABLES FOR I/S & D/A              
         USING NFITABD,RF                                                       
         XC    GSDIRDSP,GSDIRDSP   RESET DIRECTORY DISPLACEMENT                 
         XC    GSFILDSP,GSFILDSP   RESET FILE DISPLACEMENT                      
*                                                                               
RTYS10   CLI   0(RF),EOT           END OF FILE TABLE?                           
         JNE   *+6                                                              
         DC    H'0'                DIRECTORY REQUESTED NOT KNOWN                
*                                                                               
         CLC   GSSYS,NFIOSE        CORRECT SYSTEM                               
         JNE   *+14                                                             
         CLC   GSDIR,NFINUM        CORRECT FILE NUMBER                          
         JE    *+12                                                             
         LA    RF,NFITABL(RF)                                                   
         J     RTYS10                                                           
*                                                                               
         LR    RE,RF               SAVE DIRECTORY ENTRY                         
DIR      USING NFITABD,RE                                                       
         S     RF,AXFILTAB                                                      
         STH   RF,GSDIRDSP         SAVE DISP TO DIRECTORY ENTRY                 
         L     RF,AXFILTAB                                                      
         TM    DIR.NFIINDS,NFIIIS  MAKE SURE DIRECTORY IS I/S                   
         JO    *+6                 YES                                          
         DC    H'0'                                                             
         TM    DIR.NFIINDS2,NFIIID ASSOCIATED D/A?                              
         JO    RTYS12              YES                                          
         TM    DIR.NFIINDS,NFIIVL  NO D/A - MUST JE V/L I/S                     
         JO    *+6                 IT IS                                        
         DC    H'0'                                                             
         MVC   GSFILDSP,GSDIRDSP   COPY FILE ENTRY FROM DIRECTORY               
         J     RTYSOK              IT IS                                        
*                                                                               
RTYS12   CLI   0(RF),EOT           LOOK FOR ASSOCIATED D/A                      
         JNE   *+6                                                              
         DC    H'0'                D/A LINK REQUESTED NOT KNOWN                 
*                                                                               
         CLC   GSSYS,NFIOSE        CHECK SYSTEM                                 
         JNE   *+14                                                             
         CLC   NFINUM,DIR.NFINUM2  CHECK REQUESTED D/A                          
         JE    *+12                                                             
         LA    RF,NFITABL(RF)                                                   
         J     RTYS12                                                           
*                                                                               
         S     RF,AXFILTAB                                                      
         STH   RF,GSFILDSP         SAVE DISP TO FILE                            
         DROP  RF,DIR                                                           
*                                                                               
RTYSOK   J     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* REFRESH RECORD  (??? USED ???)                                 *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTYREF   J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD TYPE FIELD (ANY FIELD)                              *         
*                                                                     *         
* P3 BYTE 0 = SYSTEM TO USE                                           *         
* P3 BYTE 1 = PROGRAM TO USE                                          *         
* P3 BYTE 2 = N/D                                                     *         
* P3 BYTE 3 = FF IF SELECT RECORDS TO JE ALLOWED                      *         
* FIELD IS VALIDATED INTO FVIFLD/FVADDR ETC                           *         
***********************************************************************         
         SPACE 1                                                                
RTYXVAL  CLI   FVILEN,0            ANYTHING TO VALIDATE?                        
         JE    EXITOK                                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         L     R3,FVADDR                                                        
         USING FHD,R3              R3=A(RECORD NAME INPUT FIELD)                
X        USING FRRRECD,IOKEY                                                    
*                                                                               
RTXV02   GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         L     RF,RTPARMS3         GET SYS/PROG TO USE                          
         XC    X.FRRKEY,X.FRRKEY                                                
         MVI   X.FRRKMIN,FRRKMINQ                                               
         MVI   X.FRRKTYP,FRRKTYPQ                                               
         STCM  RF,8,X.FRRKSYS                                                   
         STCM  RF,4,X.FRRKPRG                                                   
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         J     *+8                                                              
*                                                                               
RTXV04   L     R1,=AL4(XOGENDIR+XOSEQ+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DUPEN) SET DATAMGR ERROR                         
         J     EXITL                                                            
*                                                                               
         CLC   IOKEY(FRRKREC-FRRRECD),IOKEYSAV                                  
         JE    RTXV06              SAME PROGRAM                                 
*                                                                               
         MVC   FVMSGNO,=AL2(GE$INREC)                                           
         MVC   FVXTRA,FVIFLD                                                    
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITL                                                            
*                                                                               
RTXV06   CLI   RTPARMS3+3,FF       SELECT TYPE WANTED?                          
         JNZ   *+12                YES                                          
         TM    X.FRRKIND1,FRR1SEL  SELECT TYPE TO JE IGNORED HERE               
         JO    RTXV04                                                           
*                                                                               
         XC    GCDUB1,GCDUB1                                                    
         MVI   GCDUB1,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   GCDUB1+1(L'FRRKDICT),X.FRRKDICT                                  
         MVI   GCDUB1+3,RECNAMLQ                                                
         MVC   GCDUB2,GCDUB1                                                    
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,RTPARMS3                                                    
         GOTOX VDICTAT,RTPARM,(RF),GCDUB1 TRANSLATE DICTIONARY UPPRCASE         
*                                                                               
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,RTPARMS3                                                    
         GOTOX VDICTAT,RTPARM,(RF),GCDUB2 TRANSLATE DICTIONARY LOWRCASE         
*                                                                               
         XR    RF,RF               TRY TO MATCH UPPERCASE NAME                  
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         JE    RTXV08                                                           
         CLC   FVIFLD(0),GCDUB1                                                 
*                                                                               
         EX    RF,*+8              TRY TO MATCH LOWERCASE NAME                  
         JNE   RTXV04                                                           
         CLC   FVIFLD(0),GCDUB2                                                 
*                                                                               
RTXV08   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTTST,AIO1                                    
         JNE   RTXV04                                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTRDIS,AIO1                                   
*                                                                               
         L     RF,RTPARMS3         RETURN RECORD CODE                           
         ICM   RF,2,X.FRRKREC                                                   
         ST    RF,RTPARMS3                                                      
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD TYPE FIELD                                      *  7 *         
*                                                                ******         
* P3 BYTE 0 = SYSTEM TO USE                                           *         
* P3 BYTE 1 = PROGRAM TO USE                                          *         
* P3 BYTE 2 = RECORD TO DISPLAY                                       *         
***********************************************************************         
         SPACE 1                                                                
RTYXDIS  ICM   RF,15,RTPARMS3      ANYTHING PASSED?                             
         JZ    EXITOK              NO                                           
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,14,RTPARMS3                                                   
         GOTOX ('GETFRECD',AGROUTS),(R1)                                        
         JNE   EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTRDIS,AIO1                                   
         JL    EXITL                                                            
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* MATCH ON RECORD TYPE NAME                                           *         
*                                                                     *         
* NTRY: DATA TO MATCH ON IS VALIDATED IN FVIFLD                       *         
*       P3 <> 0 IF SELECT RECORDS REQUIRED                            *         
* EXIT: AIO1 HOLDS RECORD IF VALID NAME INPUT                         *         
***********************************************************************         
         SPACE 1                                                                
RTYMCH   GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XC    PLRECB,PLRECB                                                    
*                                                                               
X        USING FRRRECD,IOKEY                                                    
RTYM02   XC    X.FRRKEY,X.FRRKEY                                                
         MVI   X.FRRKMIN,FRRKMINQ                                               
         MVI   X.FRRKTYP,FRRKTYPQ                                               
         MVC   X.FRRKSYS,GCOVSYS                                                
         MVC   X.FRRKPRG,GCPRGNO                                                
         XR    RF,RF               INCREMENT RECORD NUMBER                      
         IC    RF,PLRECB                                                        
         LA    RF,1(RF)                                                         
         STC   RF,X.FRRKREC                                                     
*                                                                               
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(FRRKREC-FRRRECD),IOKEYSAV                                  
         BE    RTYM04              NO MORE RECORDS FOR THIS PROGRAM             
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
*                                                                               
*        XC    RTHALF1,RTHALF1     MAKE SURE YOU CAN DO ONE OF THESE            
*        MVC   RTHALF1,=AL1(O#RTYP,A#LST)                                       
*        GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
*        BE    *+14                YES                                          
*        MVC   FVMSGNO,=AL2(GE$NOSEL)                                           
*        B     EXITL               NO HELP FOR YOU SONNY                        
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#RTYP       RECORD HELP IS IN GEFILREC                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL    MAKE THIS A 'SELECT' ACTION                  
         XC    PLRECB,PLRECB                                                    
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DC    H'0'                IF YOU GET HERE YOU ARE IN TROUBLE           
         DROP  N                                                                
*                                                                               
RTYM04   MVC   PLRECB,X.FRRKREC    SAVE NEXT RECORD NUMBER                      
         XR    R1,R1                                                            
         ICM   R1,14,RTPARMS3                                                   
         ICM   R1,8,GCOVSYS                                                     
         ICM   R1,4,GCPRGNO                                                     
         ICM   R1,2,PLRECB                                                      
         GOTOX ('GETFRECD',AGROUTS),(R1)                                        
         JNE   RTYM02                                                           
*                                                                               
RTYM10   CLI   NSREC,O#ACT         CAME FROM ACTION HELP?                       
         BE    RTYM12                                                           
         CLI   NSREC,O#RTYP        CAME FROM RECORD HELP?                       
         BE    RTYM12                                                           
         OC    RTPARMS3,RTPARMS3   SELECT TYPE WANTED?                          
         BNZ   RTYM14              YES                                          
         TM    X.FRRKIND1,FRR1SEL  SELECT TYPE RECORD?                          
         BO    RTYM02              YES - IGNORE IT                              
         B     RTYM14                                                           
*                                                                               
RTYM12   TM    NSSAV+(SXINDS1-SSAVD),SXISEL                                     
         BNZ   RTYM14                                                           
         TM    X.FRRKIND1,FRR1SEL  SELECT TYPE RECORD?                          
         BO    RTYM02              NOT ALLOWED                                  
*                                                                               
RTYM14   XR    RF,RF                                                            
         NI    X.FRRKSTAT,FRRKLVAL TURN OFF ALL BUT MINIMUM INPUT LEN.          
         ICM   RF,1,X.FRRKSTAT                                                  
         BNZ   *+8                                                              
         LA    RF,1                MINIMUM LENGTH DEFAULT IS 1                  
         CLM   RF,1,FVILEN                                                      
         BH    RTYM02              NOT ENOUGH CHARS INPUT FOR THIS ONE          
*                                                                               
         XC    GCDUB1,GCDUB1                                                    
         MVI   GCDUB1,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   GCDUB1+1(L'FRRKDICT),X.FRRKDICT                                  
         MVI   GCDUB1+3,8                                                       
         MVC   GCDUB2,GCDUB1                                                    
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),GCDUB1 TRANSLATE DICTIONARY UPPRCASE         
*                                                                               
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),GCDUB2 TRANSLATE DICTIONARY LOWRCASE         
*                                                                               
         XR    RF,RF               TRY TO MATCH UPPERCASE NAME                  
         IC    RF,FVILEN                                                        
         CLM   RF,1,=AL1(3)        3 CHAR MATCH FOR MEL...                      
         BNH   *+8                                                              
         LA    RF,3                                                             
         CLM   RF,1,X.FRRKSTAT     SET MUST BE VALIDATE MORE THAN 3             
         BH    *+8                                                              
         ICM   RF,1,X.FRRKSTAT                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    RTYM16                                                           
         CLC   FVIFLD(0),GCDUB1                                                 
*                                                                               
         EX    RF,*+8              TRY TO MATCH LOWERCASE NAME                  
         BNE   RTYM02                                                           
         CLC   FVIFLD(0),GCDUB2                                                 
*                                                                               
RTYM16   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTTST,AIO1                                    
         BNE   RTYM02              FAILED SECURITY TEST                         
*                                                                               
         XC    PLRECB,PLRECB                                                    
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD TYPE NAME INTO FVIFLD                           *  9 *         
*                                                                ******         
* NTRY: P3 = A(RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
RTYRDIS  L     R2,RTPARMS3                                                      
         USING FRRRECD,R2                                                       
         LA    R3,FRRFIRST(R2)                                                  
         USING FRRELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
RTRD02   CLI   FRREL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRREL,FRRELQ                                                     
         JE    RTRD04                                                           
         IC    RF,FRRLN                                                         
         LA    R3,0(RF,R3)                                                      
         J     RTRD02                                                           
*                                                                               
RTRD04   MVC   FVIFLD,BCSPACES                                                  
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   FVIFLD+1(L'FRRDICT),FRRDICT                                      
         MVI   FVIFLD+3,8                                                       
         ICM   RF,15,=C'SL  '      SET LOWER CASE                               
         ICM   RF,2,FRRKSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),FVIFLD TRANSLATE DICTIONARY NAME             
         J     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACTION CODE OBJECT                                                  *         
*                                                                     *         
* NTRY: P1        = ACTION OBJECT                                     *         
*       P2        = VERB                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ACT      LR    R7,RF                                                            
         L     R1,RTPARMS2                                                      
         LA    RF,ACTTAB                                                        
         J     ITER                                                             
*                                                                               
ACTTAB   DC    AL1(AVAL,0,0,0),AL4(ACTVAL)                                      
         DC    AL1(ADIS,0,0,0),AL4(ACTDISC)                                     
         DC    AL1(ATST,0,0,0),AL4(ACTTEST)                                     
         DC    AL1(ASET,0,0,0),AL4(ACTSET)                                      
         DC    AL1(APRC,0,0,0),AL4(ACTPRC)                                      
         DC    AL1(AMCH,0,0,0),AL4(ACTMCH)                                      
         DC    AL1(AXDIS,0,0,0),AL4(ACTXDIS)                                    
         DC    AL1(AXVAL,0,0,0),AL4(ACTXVAL)                                    
         DC    AL1(ATRDIS,0,0,0),AL4(ACTRDIS)                                   
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD                                          *  1 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTVAL   XR    R3,R3                                                            
         ICM   R3,3,GSDSPACT       DISPLACEMENT CLEARED IF SELECT FOCUS         
         JZ    EXITOK                                                           
         A     R3,ATWA                                                          
         USING FHD,R3                                                           
*                                                                               
         TM    BCINDS1,BCINREC     NEW RECORD THIS TIME?                        
         JO    ACTV02              YES                                          
         OC    CSACT,CSACT         ALREADY HAVE VALID ACTION?                   
         JZ    ACTV02              NO                                           
         TM    FHII,FHIIVA         INPUT THIS TIME?                             
         JO    ACTVOK              YES                                          
*                                                                               
ACTV02   NI    GCINDS2,FF-(GCINTRS+GCIXITS)                                     
         MVI   FVMINL,1            SET FIELD REQUIRES INPUT                     
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         JNE   EXITL                                                            
*                                                                               
         CLI   FVIFLD,C'?'         REQUESTED HELP FOR ACTIONS?                  
         JNE   ACTV04              NO                                           
*                                                                               
         CLI   CSREC,O#RTYP        RECORD OR ACTION HELP DISPLAYED?             
         JE    EXITOK                                                           
         CLI   CSREC,O#ACT                                                      
         JE    EXITOK                                                           
*                                                                               
*        XC    RTHALF1,RTHALF1     MAKE SURE YOU CAN DO ONE OF THESE            
*        MVC   RTHALF1,=AL1(O#ACT,A#LST)                                        
*        GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
*        JE    *+14                YES                                          
*        MVC   FVMSGNO,=AL2(GE$NOSEL)                                           
*        J     EXITL               NO HELP FOR YOU SONNY                        
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#ACT                                                     
         MVI   N.SACT,A#LST                                                     
         TM    BCINDS1,BCINREC     NEW RECORD & WANT ACTION HELP                
         JZ    *+14                                                             
         OI    N.SNINDS1,SNINEWR   YES - PRESERVE FLAG                          
         XC    CSACT,CSACT                                                      
         OI    N.SNINDS1,SNISEL    MAKE ACTION FIELD SELECT FOCUS               
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
*                                                                               
ACTV04   GOTOX AGEN,RTPARM,OACT,AMCH,0                                          
         JNE   ACTVL               NO MATCH ON ACTION NAME                      
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ASET,AIO1                                       
         JL    ACTVL                                                            
*                                                                               
         TM    BCINDS1,BCINACT     RESET NEST LEVEL IF ACTION TYPED IN          
         JZ    ACTV06                                                           
         CLI   N.SREC,O#ACT                                                     
         JE    ACTV06                                                           
         GOTOX AGEN,RTPARM,OSES,SRESLVL                                         
         DROP  N                                                                
*                                                                               
ACTV06   GOTOX AGEN,RTPARM,OACT,ADIS,AIO1                                       
         JL    ACTVL                                                            
*                                                                               
ACTVOK   OI    FHII,FHIIVA         SET FIELD VALID                              
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
*                                                                               
ACTVL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITL                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME INTO ACTION FIELD                          *  2 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTDISC  XR    R3,R3                                                            
         ICM   R3,3,GSDSPACT       DISPLACEMENT CLEARED IF SELECT FOCUS         
         JZ    EXITOK                                                           
         A     R3,ATWA                                                          
         USING FHD,R3              R3=(ACTION CODE INPUT FIELD)                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RE,FHDAD+1                                                       
         TM    FHAT,FHATXH                                                      
         JZ    *+8                                                              
         LA    RE,FHDAD(RE)                                                     
         SR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   FHDA(0),CSACTNAM    ACTION NAME SAVED IN HERE                    
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA                                                      
         J     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* TEST VALIDITY OF ACTION CODE                                   *  3 *         
*                                                                ******         
* NTRY: P3 BYTE 0 = 0                                                 *         
*             1-3 = A(RECORD)                                         *         
*    OR P3 BYTE 0 = ACTION NUMBER                                     *         
*               3 = RECORD NUMBER (IF 0 USE CSREC)                    *         
* EXIT: P3        = A(RECORD)                                         *         
*       P4        = A(FRAELD)                                         *         
***********************************************************************         
         SPACE 1                                                                
ACTTEST  GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         CLI   RTPARMS3,0          TEST GIVEN A(ACTION RECORD)                  
         JNE   *+12                NO                                           
         L     R2,RTPARMS3                                                      
         J     ACTT08                                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XR    R1,R1                                                            
         ICM   R1,8,GCOVSYS        SYSTEM                                       
         ICM   R1,4,GCPRGNO        PROGRAM                                      
         ICM   R1,2,RTPARMS3+3     OVERRIDE RECORD?                             
         JNZ   *+8                 YES                                          
         ICM   R1,2,CSREC          ELSE USE CSREC                               
         ICM   R1,1,RTPARMS3  ACTION NUMBER                                     
         GOTOX ('GETFACTN',AGROUTS),(R1)                                        
         JNE   ACTTL                                                            
*                                                                               
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
*                                                                               
         L     R2,AIO1                                                          
         USING FRARECD,R2                                                       
ACTT08   LA    R3,FRAFIRST(R2)                                                  
         USING FRAELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
ACTT10   CLI   FRAEL,0             TRY TO FIND ACTION ELEMENT                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRAEL,FRAELQ                                                     
         JE    ACTT12                                                           
         IC    RF,FRALN                                                         
         LA    R3,0(RF,R3)                                                      
         J     ACTT10                                                           
*                                                                               
ACTT12   TM    FRAINDS1,FRA1DDS    TEST DDS ONLY RECORD TYPE                    
         JZ    ACTT14                                                           
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         JZ    ACTTL                                                            
*                                                                               
ACTT14   XC    RTHALF1,RTHALF1     TEST RECORD SECURITY                         
         OC    RTHALF1(1),FRARSEC                                               
         JNZ   *+10                                                             
         MVC   RTHALF1(1),FRAKREC                                               
         OC    RTHALF1+1(1),FRAASEC                                             
         JNZ   *+10                                                             
         MVC   RTHALF1+1(1),FRAKACT                                             
*                                                                               
         CLI   RTHALF1,O#RTYP      RECORD/ACTION HELP ALWAYS AUTHORISED         
         JE    ACTTOK                                                           
         CLI   RTHALF1,O#ACT                                                    
         JE    ACTTOK                                                           
         CLI   RTHALF1,O#SUBAC                                                  
         JE    ACTTOK                                                           
*                                                                               
         GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
         JNE   ACTTL               FAILED RECORD SECURITY TEST                  
*                                                                               
ACTTOK   STM   R2,R3,RTPARMS3                                                   
         J     EXITOK                                                           
*                                                                               
ACTTL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITL                                                            
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* SET CURRENT ACTION CODE VALUES                                 *  4 *         
*                                                                ******         
* NTRY: P3 BYTE 0 = 0                                                 *         
*             1-3 = A(ACTION RECORD)                                  *         
*    OR P3 BYTE 0 = ACTION CODE                                       *         
*             1-3 = 0                                                 *         
***********************************************************************         
         SPACE 1                                                                
ACTSET   XR    R2,R2                                                            
         ICM   R2,7,RTPARMS3+1     TEST GIVEN TABLE ENTRY                       
         JNZ   ACTS02                                                           
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ATST,RTPARMS3                                   
         JL    EXITL                                                            
         L     R2,8(R1)                                                         
         USING FRARECD,R2                                                       
*                                                                               
ACTS02   LA    R3,FRAFIRST(R2)     LOOK FOR ACTION ELEMENT                      
         USING FRAELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
ACTS04   CLI   FRAEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRAEL,FRAELQ                                                     
         JE    ACTS06                                                           
         IC    RF,FRALN                                                         
         LA    R3,0(RF,R3)                                                      
         J     ACTS04                                                           
*                                                                               
ACTS06   MVC   RTBYTE1,GSFRAEL+(FRATYPE-FRAELD) SAVE ACTION TYPE                
         IC    RF,FRALN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   GSFRAEL(0),FRAELD   SAVE ACTION ELEMENT                          
*                                                                               
         TM    BCINDS1,BCINACT     FORCE NEW ACTION?                            
         JO    ACTS08              YES                                          
         CLC   CSACT,FRAKACT       HAS ACTION CHANGED?                          
         JE    ACTS10              ACTION UNCHANGED                             
*                                                                               
ACTS08   OI    BCINDS1,BCINACT     SET NEW ACTION                               
         XC    LSPFKLST,LSPFKLST   RESET PFK POINTERS                           
         XC    LSPFKNOW,LSPFKNOW                                                
*                                                                               
         CLC   RTBYTE1,GSFRAEL+(FRATYPE-FRAELD) ACTION TYPE CHANGED?            
         JE    ACTS10                                                           
*                                                                               
         XC    GSSCREEN,GSSCREEN   NEED TO REBUILD SCREEN                       
         XC    GSLSTTOP,GSLSTTOP   RESET BOX FOR MAINT LIST                     
         XC    GSLSTEND,GSLSTEND   RESET BOX FOR MAINT LIST                     
         TM    GCINDS2,GCIDONTR    DOING AN NTRSES?                             
         JZ    *+10                                                             
         MVC   GSSMPAGE,PSSAV+(SMPAGE-SSAVD)                                    
         XC    GSINDSL,GSINDSL     RESET LOCAL SESSION INDICATORS               
         XC    CSINDSL1,CSINDSL1   RESET LOCAL SESSION INDICATORS               
         XC    LSSTAT1,LSSTAT1     NEED TO REBUILD LIST                         
*                                                                               
ACTS10   XC    CSACTNAM,CSACTNAM   SET ACTION NAME FROM DD REFERENCE            
         MVC   CSACT,FRAKACT       SET THIS ACTION NUMBER                       
*                                                                               
         MVI   CSACTNAM,DD#ESCL    LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   CSACTNAM+1(L'FRADICT),GSFRA.FRADICT                              
         MVI   CSACTNAM+3,8                                                     
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),CSACTNAM TRANSLATE ACTION NAME               
*                                                                               
         L     R0,AOVERWRK         CLEAR OVERWRK                                
         LHI   R1,OVERWRKL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,GSFRR.FRRPHASE   RECORD OVERLAY                               
         OC    FRAOACT,FRAOACT     OVERRIDE OVERLAY?                            
         JZ    *+8                 NO                                           
         LA    R1,FRAOACT                                                       
         STC   R1,CSOVER                                                        
*                                                                               
         GOTOX ('OVRLAY',AGROUTS)  LOAD CORRECT OVERLAY & INIT                  
         JNE   EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OPAGE,PGCOUNT                                        
*                                                                               
ACTSOK   XR    RE,RE                                                            
         ICM   RE,3,GSDSPACT                                                    
         JZ    EXITOK                                                           
         A     RE,ATWA                                                          
         OI    FHII-FHD(RE),FHIIVA SET ACTION FIELD VALIDATED                   
         J     EXITOK                                                           
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* PROCESS ACTION                                                 *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTPRC   XR    RF,RF                                                            
         IC    RF,GSFRA.FRAVERB    GET INTERNAL ACTION VERB                     
         XR    R0,R0                                                            
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         JZ    *+8                                                              
         LA    R0,ACT1ST           SET FIRST TIME FOR ACTION                    
*                                                                               
         TM    GCINDS2,GCINTRS     JUST NTRSES'D?                               
         JZ    *+8                                                              
         LA    R0,ACTNTR           SET JUST NTRSES'D                            
*                                                                               
         TM    GCINDS2,GCIXITS     JUST XITSES'D?                               
         JZ    *+8                                                              
         LA    R0,ACTXIT           SET JUST XITSES'D                            
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',OACTH),((R0),(RF))                        
         JNE   EXIT                ACTION CODE PROCESSED BY OVERLAY             
*                                  OR UNRECOGNISED (MESSAGE SET)                
*                                                                               
         CLI   TWASESNL,1          NESTED?                                      
         JE    EXITOK              NO - EXIT                                    
         CLI   BCPFKEY,0           ENTER KEY PRESSED?                           
         JNE   EXITOK              NO - EXIT                                    
         TM    GCINDS2,GCINTRS     JUST NTRSES'D                                
         JO    EXITOK              YES - EXIT                                   
         TM    GCINDS2,GCIACTCP    ACTION COMPLETE?                             
         JZ    EXITOK              NO - EXIT                                    
         NI    GCINDS2,FF-GCIACTCP RESET FLAG                                   
*                                                                               
         TM    GCINDS1,GCISSCRL    SCREEN SCROLL REQUEST SET BY SPAGE?          
         JZ    *+12                NO                                           
         NI    GCINDS1,FF-GCISSCRL YES - RESET FLAG & EXIT                      
         J     EXITOK                                                           
*                                                                               
         GOTOX AGEN,RTPARM,OSES,SXIT                                            
         DC    H'0'                XITSES TO PREVIOUS LEVEL                     
         SPACE 2                                                                
***********************************************************************         
* MATCH ON ACTION CODE NAME                                      *  6 *         
*                                                                ******         
* ENTRY-DATA TO MATCH ON IS VALIDATED IN FVIFLD                       *         
* XIT - AIO1 HOLDS RECORD IF VALID NAME INPUT                         *         
***********************************************************************         
         SPACE 1                                                                
ACTMCH   GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
X        USING FRARECD,IOKEY                                                    
         XC    X.FRAKEY,X.FRAKEY                                                
         MVI   X.FRAKMIN,FRAKMINQ                                               
         MVI   X.FRAKTYP,FRAKTYPQ                                               
         MVC   X.FRAKSYS,GCOVSYS   SYSTEM                                       
         MVC   X.FRAKPRG,GCPRGNO   PROGRAM                                      
         MVC   X.FRAKREC,CSREC     RECORD                                       
         XC    PLACTB,PLACTB                                                    
*                                                                               
ACTM02   XR    RF,RF               INCREMENT RECORD NUMBER                      
         IC    RF,PLACTB                                                        
         LA    RF,1(RF)                                                         
         STC   RF,X.FRAKACT                                                     
*                                                                               
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(FRAKACT-FRARECD),IOKEYSAV                                  
         JE    ACTM04              NO MORE RECORDS FOR THIS PROGRAM             
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
*                                                                               
*        XC    RTHALF1,RTHALF1     MAKE SURE YOU CAN DO ONE OF THESE            
*        MVC   RTHALF1,=AL1(O#ACT,A#LST)                                        
*        GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
*        JE    *+14                YES                                          
*        MVC   FVMSGNO,=AL2(GE$NOSEL)                                           
*        J     EXITL               NO HELP FOR YOU SONNY                        
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#ACT        RECORD HELP IS IN GEFILREC                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL    MAKE THIS A 'SELECT' ACTION                  
         TM    BCINDS1,BCINREC     NEW RECORD & WANT ACTION HELP                
         JZ    *+14                                                             
         OI    N.SNINDS1,SNINEWR   YES - PRESERVE FLAG                          
         XC    CSACT,CSACT                                                      
         XC    PLACTB,PLACTB                                                    
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DC    H'0'                IF YOU GET HERE YOU ARE IN TROUBLE           
         DROP  N                                                                
*                                                                               
ACTM04   MVC   PLACTB,X.FRAKACT    SAVE NEXT RECORD NUMBER                      
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,8,GCOVSYS                                                     
         ICM   R1,4,GCPRGNO                                                     
         ICM   R1,2,CSREC                                                       
         ICM   R1,1,PLACTB                                                      
         GOTOX ('GETFACTN',AGROUTS),(R1)                                        
         JNE   ACTM02                                                           
*                                                                               
ACTM10   CLI   NSREC,O#ACT         CAME FROM ACTION HELP?                       
         JNE   ACTM12                                                           
         TM    NSSAV+(SXINDS1-SSAVD),SXISEL                                     
         JO    ACTM14                                                           
*                                                                               
ACTM12   TM    X.FRAKIND1,FRA1NOH  SELECT ONLY ACTION?                          
         JO    ACTM02              NOT ALLOWED                                  
*                                                                               
ACTM14   XR    RF,RF                                                            
         NI    X.FRAKSTAT,FRAKLVAL TURN OFF ALL BUT MINIMUM INPUT LEN.          
         ICM   RF,1,X.FRAKSTAT                                                  
         JNZ   *+8                                                              
         LA    RF,1                MINIMUM LENGTH DEFAULT IS 1                  
         CLM   RF,1,FVILEN                                                      
         JH    ACTM02              NOT ENOUGH CHARS INPUT FOR THIS ONE          
*                                                                               
         XC    GCDUB1,GCDUB1                                                    
         MVI   GCDUB1,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   GCDUB1+1(L'FRAKDICT),X.FRAKDICT                                  
         MVI   GCDUB1+3,ACTNAMLQ                                                
         MVC   GCDUB2,GCDUB1                                                    
         ICM   R3,15,=C'SU  '                                                   
         ICM   R3,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(R3),GCDUB1 TRANSLATE UPPERCASE NAME              
         ICM   R3,4,=C'L'                                                       
         GOTOX (RF),(R1),(R3),GCDUB2      TRANSLATE LOWERCASE NAME              
*                                                                               
         XR    RF,RF               TRY TO MATCH UPPERCASE NAME                  
         IC    RF,FVILEN                                                        
         CLM   RF,1,=AL1(ALEN#Q)   LEN IS > THAN REQUIREMENT?                   
         BNH   *+8                 NO                                           
         LA    RF,ALEN#Q           SET DEFAULT INPUT REQUIREMENT                
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         JE    ACTM16                                                           
         CLC   FVIFLD(0),GCDUB1                                                 
*                                                                               
         EX    RF,*+8              TRY TO MATCH LOWERCASE NAME                  
         JNE   ACTM02                                                           
         CLC   FVIFLD(0),GCDUB2                                                 
*                                                                               
ACTM16   GOTOX AGEN,RTPARM,OACT,ATST,AIO1                                       
         JNE   ACTM02              FAILED SECURITY TEST                         
*                                                                               
         XC    PLACTB,PLACTB                                                    
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME INTO A FIELD                               *  7 *         
*                                                                ******         
* P3 BYTE 0 = SYSTEM TO USE                                           *         
* P3 BYTE 1 = PROGRAM TO USE                                          *         
* P3 BYTE 2 = RECORD TO USE                                           *         
* P3 BYTE 3 = ACTION TO DISPLAY                                       *         
***********************************************************************         
         SPACE 1                                                                
ACTXDIS  ICM   R1,15,RTPARMS3      ANYTHING PASSED?                             
         JZ    EXITOK              NO                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         ICM   R1,15,RTPARMS3                                                   
         GOTOX ('GETFACTN',AGROUTS),(R1)                                        
         IPM   R0                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         SPM   R0                                                               
         JNE   EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ATRDIS,AIO1                                     
         JL    EXITL               FAILED RECORD SECURITY TEST                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION NAME FIELD                                     *  8 *         
*                                                                ******         
* P3 BYTE 0 = SYSTEM TO USE                                           *         
* P3 BYTE 1 = PROGRAM TO USE                                          *         
* P3 BYTE 2 = RECORD TO USE                                           *         
* P3 BYTE 3 = N/D                                                     *         
* FIELD IS VALIDATED INTO FVIFLD/FVADDR ETC                           *         
***********************************************************************         
         SPACE 1                                                                
ACTXVAL  CLI   FVILEN,0            ANYTHING TO VALIDATE?                        
         JE    EXITOK              NO                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
X        USING FRARECD,IOKEY                                                    
         XC    X.FRAKEY,X.FRAKEY                                                
         MVI   X.FRAKMIN,FRAKMINQ                                               
         MVI   X.FRAKTYP,FRAKTYPQ                                               
         L     RF,RTPARMS3                                                      
         STCM  RF,8,X.FRAKSYS                                                   
         STCM  RF,4,X.FRAKPRG                                                   
         STCM  RF,2,X.FRAKREC                                                   
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         J     *+8                                                              
*                                                                               
ACXV02   L     R1,=AL4(XOGENDIR+XOSEQ+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DUPEN) SET DATAMGR ERROR                         
         J     ACVXL                                                            
*                                                                               
         CLC   IOKEY(FRAKACT-FRARECD),IOKEYSAV                                  
         JE    ACXV04              NO MORE ACTIONS FOR THIS RECORD              
*                                                                               
         MVC   FVMSGNO,=AL2(GE$INACT)                                           
         MVC   FVXTRA,FVIFLD                                                    
         J     ACVXL                                                            
*                                                                               
ACXV04   XC    GCDUB1,GCDUB1                                                    
         MVI   GCDUB1,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   GCDUB1+1(L'FRAKDICT),X.FRAKDICT                                  
         MVI   GCDUB1+3,8                                                       
         MVC   GCDUB2,GCDUB1                                                    
         ICM   R0,15,=C'SU  '                                                   
         ICM   R0,2,X.FRAKSYS                                                   
         GOTOX VDICTAT,RTPARM,(R0),GCDUB1 TRANSLATE DICTIONARY UPPRCASE         
*                                                                               
         ICM   R0,4,=C'L'                                                       
         GOTOX (RF),(R1),(R0),GCDUB2      TRANSLATE DICTIONARY LOWRCASE         
*                                                                               
         XR    RF,RF               TRY TO MATCH UPPERCASE NAME                  
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         JE    ACXV06                                                           
         CLC   FVIFLD(0),GCDUB1                                                 
*                                                                               
         EX    RF,*+8              TRY TO MATCH LOWERCASE NAME                  
         JNE   ACXV02                                                           
         CLC   FVIFLD(0),GCDUB2                                                 
*                                                                               
ACXV06   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ATST,AIO1                                       
         JNE   ACXV02              FAILED SECURITY TEST                         
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ATRDIS,AIO1                                     
*                                                                               
         L     RF,RTPARMS3         RETURN ACTION CODE                           
         ICM   RF,1,X.FRAKACT                                                   
         ST    RF,RTPARMS3                                                      
*                                                                               
ACVXOK   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
*                                                                               
ACVXL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITL                                                            
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME INTO FVIFLD                                *  9 *         
*                                                                ******         
* NTRY: P3 = A(ACTION RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
ACTRDIS  ICM   R2,15,RTPARMS3      A(RECORD) PASSED?                            
         JZ    EXITOK              NO                                           
         USING FRARECD,R2                                                       
         LA    R3,FRAFIRST(R2)                                                  
         USING FRAELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
ACXR02   CLI   FRAEL,0             SEARCH RECORD FOR ACTION ELEMENT             
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRAEL,FRAELQ                                                     
         JE    ACXR04                                                           
         IC    RF,FRALN                                                         
         LA    R3,0(RF,R3)                                                      
         J     ACXR02                                                           
*                                                                               
ACXR04   MVC   FVIFLD,BCSPACES                                                  
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   FVIFLD+1(L'FRADICT),FRADICT                                      
         MVI   FVIFLD+3,ACTNAMLQ                                                
         ICM   RF,15,=C'SL  '      SET LOWER CASE                               
         ICM   RF,2,FRAKSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),FVIFLD TRANSLATE DICTIONARY NAME             
         J     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* ACTION CODE HANDLING OBJECT                                         *         
*                                                                     *         
* NTRY: P1        = ACTION HANDLING OBJECT                            *         
*       P2 BYTE 0 = SUB-VERB (ACTNTR OR ACTXIT OR 0)                  *         
*          BYTE 3 = VERB                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ACTH     LR    R7,RF                                                            
         LA    R4,ACHTAB                                                        
         USING OBJTABD,R4                                                       
         ICM   RF,15,RTPARMS2                                                   
*                                                                               
ACH02    CLI   OBJVERB,EOT                                                      
         JE    ACH10                                                            
         CLM   RF,1,OBJVERB                                                     
         JE    *+12                MATCHED                                      
         LA    R4,OBJTABL(R4)                                                   
         J     ACH02               BUMP & LOOP                                  
*                                                                               
ACH04    CLM   RF,8,OBJSVB         MATCH ON SUB-VERB                            
         JE    ACH06                                                            
         CLI   OBJSVB,0                                                         
         JNE   *+6                                                              
         LR    R0,R4                                                            
         LA    R4,OBJTABL(R4)                                                   
         CLC   OBJVERB,RTPARMS2+3                                               
         JE    ACH04                                                            
         LTR   R4,R0               NO SUB-VERB MATCH USE 0 IF THERE             
         JZ    ACH10                                                            
*                                                                               
ACH06    TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         JZ    ACH08                                                            
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         JE    ACH08                                                            
         DC    H'0'                                                             
*                                                                               
ACH08    ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R4                                                               
*                                                                               
ACH10    MVC   FVMSGNO,=AL2(GE$ACNR) ACTION NOT KNOWN                           
         J     EXITH                                                            
*                                                                               
ACHTAB   DC    AL1(ACTADD,0,0,ACTTST),AL4(ADDTST)                               
         DC    AL1(ACTADD,0,0,ACT1ST),AL4(ADD1ST)                               
         DC    AL1(ACTADD,0,0,ACTNTR),AL4(ADDNTR)                               
         DC    AL1(ACTADD,0,0,0),AL4(ADD)                                       
         DC    AL1(ACTADD,0,0,ACTXIT),AL4(ADDXIT)                               
*                                                                               
         DC    AL1(ACTDIS,0,0,ACTTST),AL4(DISTST)                               
         DC    AL1(ACTDIS,0,0,ACT1ST),AL4(DIS1ST)                               
         DC    AL1(ACTDIS,0,0,ACTNTR),AL4(DISNTR)                               
         DC    AL1(ACTDIS,0,0,0),AL4(DIS)                                       
         DC    AL1(ACTDIS,0,0,ACTXIT),AL4(DISXIT)                               
*                                                                               
         DC    AL1(ACTCHA,0,0,ACTTST),AL4(CHATST)                               
         DC    AL1(ACTCHA,0,0,ACT1ST),AL4(CHA1ST)                               
         DC    AL1(ACTCHA,0,0,ACTNTR),AL4(CHANTR)                               
         DC    AL1(ACTCHA,0,0,0),AL4(CHA)                                       
         DC    AL1(ACTCHA,0,0,ACTXIT),AL4(CHAXIT)                               
*                                                                               
         DC    AL1(ACTDEL,0,0,ACTTST),AL4(DELTST)                               
         DC    AL1(ACTDEL,0,0,ACT1ST),AL4(DEL1ST)                               
         DC    AL1(ACTDEL,0,0,ACTNTR),AL4(DELNTR)                               
         DC    AL1(ACTDEL,0,0,0),AL4(DEL)                                       
         DC    AL1(ACTDEL,0,0,ACTXIT),AL4(DELXIT)                               
*                                                                               
         DC    AL1(ACTRES,0,0,ACTTST),AL4(RESTST)                               
         DC    AL1(ACTRES,0,0,ACT1ST),AL4(RES1ST)                               
         DC    AL1(ACTRES,0,0,ACTNTR),AL4(RESNTR)                               
         DC    AL1(ACTRES,0,0,0),AL4(RES)                                       
         DC    AL1(ACTRES,0,0,ACTXIT),AL4(RESXIT)                               
*                                                                               
         DC    AL1(ACTBLD,0,0,ACT1ST),AL4(BUILD1ST)                             
         DC    AL1(ACTBLD,0,0,ACTNTR),AL4(BUILDNTR)                             
         DC    AL1(ACTBLD,0,0,0),AL4(BUILD)                                     
         DC    AL1(ACTBLD,0,0,ACTXIT),AL4(BUILDXIT)                             
*                                                                               
         DC    AL1(ACTCPY,0,0,ACTTST),AL4(CPYTST)                               
         DC    AL1(ACTCPY,0,0,ACT1ST),AL4(CPY1ST)                               
         DC    AL1(ACTCPY,0,0,ACTNTR),AL4(CPYNTR)                               
         DC    AL1(ACTCPY,0,0,0),AL4(CPY)                                       
         DC    AL1(ACTCPY,0,0,ACTXIT),AL4(CPYXIT)                               
*                                                                               
         DC    AL1(ACTREN,0,0,ACTTST),AL4(RENTST)                               
         DC    AL1(ACTREN,0,0,ACT1ST),AL4(REN1ST)                               
         DC    AL1(ACTREN,0,0,ACTNTR),AL4(RENNTR)                               
         DC    AL1(ACTREN,0,0,0),AL4(REN)                                       
         DC    AL1(ACTREN,0,0,ACTXIT),AL4(RENXIT)                               
*                                                                               
         DC    AL1(ACTLST,0,0,ACTTST),AL4(LSTTST)                               
         DC    AL1(ACTLST,0,0,0),AL4(LST)                                       
*                                                                               
         DC    AL1(ACTFOFF,0,0,0),AL4(FOFF)                                     
*                                                                               
         DC    AL1(ACTDWN,0,0,ACTTST),AL4(DWNTST)                               
         DC    AL1(ACTDWN,0,0,ACT1ST),AL4(DWN1ST)                               
         DC    AL1(ACTDWN,0,0,ACTNTR),AL4(DWNNTR)                               
         DC    AL1(ACTDWN,0,0,0),AL4(DWN)                                       
         DC    AL1(ACTDWN,0,0,ACTXIT),AL4(DWNXIT)                               
*                                                                               
         DC    AL1(ACTREP,0,0,ACT1ST),AL4(REP1ST)                               
         DC    AL1(ACTREP,0,0,ACTNTR),AL4(REPNTR)                               
         DC    AL1(ACTREP,0,0,0),AL4(REP)                                       
         DC    AL1(ACTREP,0,0,ACTXIT),AL4(REPXIT)                               
*                                                                               
         DC    AL1(ACTMNT,0,0,ACTTST),AL4(MNTTST)                               
         DC    AL1(ACTMNT,0,0,ACT1ST),AL4(MNT1ST)                               
         DC    AL1(ACTMNT,0,0,ACTNTR),AL4(MNTNTR)                               
         DC    AL1(ACTMNT,0,0,0),AL4(MNT)                                       
         DC    AL1(ACTMNT,0,0,ACTXIT),AL4(MNTXIT)                               
*                                                                               
         DC    AL1(ACTRENO,0,0,ACTTST),AL4(RENOTST)                             
         DC    AL1(ACTRENO,0,0,ACT1ST),AL4(RENO1ST)                             
         DC    AL1(ACTRENO,0,0,ACTNTR),AL4(RENONTR)                             
         DC    AL1(ACTRENO,0,0,0),AL4(RENO)                                     
         DC    AL1(ACTRENO,0,0,ACTXIT),AL4(RENOXIT)                             
*                                                                               
ACHTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD A RECORD - TEST ACTION VALIDITY                                 *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ADDTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#ADD)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ADD A RECORD - FIRST TIME AFTER NTRSES                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ADDNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    ADDNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
ADDNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
*                                                                               
         OI    GSINDSL1,GSIXMNT    FLAG MAINT SCREEN LOADED?                    
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      EXIT WITH 'ENTER CHANGES' MESSAGE            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
*??      OI    GCINDS2,GCIACTCP                                                 
         J     EXITREC                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ADD A RECORD - FIRST TIME AFTER XITSES                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ADDXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA                                                  
         JZ    EXITACT                                                          
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ADD A RECORD                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ADD1ST   DS    0H                                                               
ADD      LR    R7,RF                                                            
*                                                                               
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         JZ    *+8                                                              
         OI    GSINDSG1,GSG1MSAV                                                
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    ADD04                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    ADD02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    ADD04               NO                                           
         TM    GSINDSL1,GSIXKEY                                                 
         JO    ADD04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
ADD02    OC    GSSMPAGE,GSSMPAGE   CHECK PAGE REQUESTED                         
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE  LOAD IN MAINT SCREEN                    
         JL    EXITL                                                            
         XC    GSRECSAV,GSRECSAV   CLEAR CURRENT RECORD INFO                    
         TM    GSINDSL1,GSIXKEY    NO 'ENTER KEY' MSG REQUESTED                 
         JO    ADD04               YES                                          
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
ADD04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST NOT ON FILE                             
         JZ    ADD06                                                            
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXITACT                                                          
*                                                                               
ADD06    TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    ADD08               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   ADD10                                                            
*                                                                               
ADD08    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    ADD12               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
ADD10    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
*                                                                               
ADD12    TM    GSFRR.FRRINDS1,FRR1HEIR   RECORD HAS DEFAULTS                    
         JZ    ADD18               NO                                           
*                                                                               
         CLC   GSRECKEY,GCLASKEY   TEST NEW KEY                                 
         JE    ADD14               NO                                           
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,OHEIR,HGET,GSRECKEY                                  
         JH    ADD18               NO DEFAULT RECORD                            
         JL    EXIT                ERROR EXIT                                   
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     ADD16               ALLOW USER TO ENTER OVERRIDES                
*                                                                               
ADD14    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC VALIDATE RECORD                    
         JL    EXITL                                                            
         J     ADD24                                                            
*                                                                               
ADD16    MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXITREC                                                          
*                                                                               
ADD18    TM    GSINDSL1,GSIDISCL      EXTRA DISPLAY FOR WALTER                  
         JZ    ADD20                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         NI    GSINDSL1,X'FF'-GSIDISCL  SHOULDN'T NEED THIS AGAIN               
*                                                                               
ADD20    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC VALIDATE RECORD DETAILS            
         JL    EXITL                                                            
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1UPDT TEST UPDATABLE RECORD                    
         JZ    ADD24                                                            
         CLI   GSFRP.FRPTYPE,FRPTUPDT TEST UPDATE PFKEY                         
         JE    ADD24                                                            
*                                                                               
         TM    GCINDS2,GCIANYCH    ANY CHANGES THIS TIME                        
         JZ    ADD22                                                            
         OI    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT             SAVE RECORD ON TSAR RECORD                   
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL               REDISPLAY RECORD                             
         J     ADDX3                                                            
*                                                                               
ADD22    GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         TM    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         JZ    ADDX1               NO INPUT                                     
         J     ADDX3               INPUT NOT SAVED                              
*                                                                               
ADD24    TM    BOINDS1,BOIUPSEL    TEST SELECTIONS TO JE UPDATED                
         JZ    ADD26                                                            
* ??     GOTOX AUPDSEL                                                          
*                                                                               
ADD26    TM    GCINDS2,GCIPFNTR    TEST PFK ACTION PENDING                      
         JO    ADD28                                                            
         GOTOX AGEN,RTPARM,ORECH,RADD,AIOREC  ADD RECORD                        
         JL    EXITL                                                            
*                                                                               
ADD28    GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC RE-DISPLAY RECORD                  
         JL    EXITL                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
*??      OI    GCINDS2,GCIACTCP                                                 
         J     EXITACT                                                          
*                                                                               
ADDX1    MVC   FVMSGNO,=AL2(FVFEKEYD)                                           
*??      OI    GCINDS2,GCIACTCP    ??                                           
         J     EXITACT                                                          
*                                                                               
ADDX3    MVI   FVOMTYP,GTMWRN      EXIT WITH 'UPDATE ME ' MESSAGE               
         MVC   FVMSGNO,=AL2(GW$RCNUP)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD - TEST ACTION VALIDITY                             *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK      COPY VALID ACTION MASK                       
         NC    RTMSK,=AL4(MK#DIS)                                               
         JZ    EXITL               NOT VALID TO DISPLAY                         
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY - FIRST TIME AFTER NTRSES                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY  LOAD IN KEY SCREEN                       
*                                                                               
         CLI   CSREC,O#ACV         RECORD ACTIVITY?                             
         JE    DNTR04                                                           
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    DNTR02              NOT PASSING PARAMETERS                       
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
DNTR02   GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0   GET RECORD                            
         JL    EXITL                                                            
*                                                                               
DNTR04   CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    FLAG MAINT SCREEN LOADED                     
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL DISPLAY                                                    
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCDIS)                                           
         TM    GSRECSTA,X'80'      TEST RECORD DELETED                          
         JZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$RCDEL)                                           
*                                                                               
         OI    GCINDS2,GCIACTCP    SET ACTION COMPLETED                         
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY - FIRST TIME AFTER XITSES                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JZ    EXITACT             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DIS1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         L     RF,=A(DIS)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DIS      LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY CHANGE OF SCREEN?                         
         JE    DIS04               NO                                           
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    DIS02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    DIS04               NO                                           
         TM    GSINDSL1,GSIXKEY                                                 
         JO    DIS04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
DIS02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    DIS04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
DIS04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JNZ   *+14                YES                                          
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY             ERROR RECORD NOT FOUND                       
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    DIS06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   DIS08                                                            
*                                                                               
DIS06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    DIS10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
DIS08    OC    GSSMPAGE,GSSMPAGE   SET PAGE NUMBER                              
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINTENANCE SCREEN                  
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
DIS10    GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTDIS)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX SCROLL              DEAL WITH SCREEN SCROLL PFKEYS               
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
DISX     MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCDIS)                                           
         TM    GSRECSTA,X'80'      TEST RECORD DELETED                          
         JZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$RCDEL)                                           
         OI    GCINDS2,GCIACTCP                                                 
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHANGE A RECORD - TEST ACTION VALIDITY                              *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CHATST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#CHA)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CHANGE - 1ST TIME AFTER NTRSES                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CHANTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    CHANTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
CHANTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
CHANTR04 CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
*                                                                               
         OI    GSINDSL1,GSIXMNT    FLAG MAINT SCREEN LOADED?                    
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      EXIT WITH 'ENTER CHANGES' MESSAGE            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
*??      OI    GCINDS2,GCIACTCP                                                 
         J     EXITREC                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CHANGE - 1ST TIME AFTER XITSES                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CHAXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JZ    EXITACT             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CHANGE - FIRST TIME FOR ACTION                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CHA1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         NI    GSINDSL,FF-(GSIDIRTY)                                            
         L     RF,=A(CHA)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CHANGE                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CHA      LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY CHANGE OF SCREEN?                         
         JE    CHA04               NO                                           
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    CHA02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    CHA04               NO                                           
         TM    GSINDSL1,GSIXKEY                                                 
         JO    CHA04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
CHA02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    CHA04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
CHA04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    CHA06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   CHA08                                                            
*                                                                               
CHA06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    CHA10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
CHA08    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETED                       
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         J     EXITKEY                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF      SET NORMAL 'RECORD DISPLAYED' MSG            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         J     EXITREC                                                          
*                                                                               
CHA10    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JE    CHA12               KEY SAME - UPDATE RECORD                     
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTCHA)                              
         BNL   CHA14                                                            
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
CHA12    GOTOX AGEN,RTPARM,OIO,IRECGET,A(XOLOCK) GET RECORD FOR UPD             
         JL    EXITL                                                            
         JE    CHA14                CC HIGH - RECORD CHANGED ELSEWHERE          
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTCHA)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         GOTOX ('GETPID',AGROUTS),GCRACCHA+(RACPERS-RACELD)                     
         MVC   FVXTRA(8),BCWORK     SOMEONE ELSE CHANGED IT                     
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCRED)                                           
         J     EXITREC                                                          
*                                                                               
CHA14    GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTCHA)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETABLE                     
         JZ    CHA16                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         J     EXITKEY                                                          
*                                                                               
CHA16    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JE    CHA18               NO                                           
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         J     EXITREC                                                          
*                                                                               
CHA18    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
CHA20    TM    GSFRR.FRRINDS1,FRR1UPDT TEST UPDATABLE RECORD                    
         JZ    CHA24                   NO                                       
         CLI   GSFRP.FRPTYPE,FRPTUPDT  TEST UPDATE PFKEY                        
         JE    CHA28               YES                                          
*                                                                               
         TM    GCINDS2,GCIANYCH    ANY CHANGES THIS TIME                        
         JZ    CHA22               NO                                           
         OI    GSINDSL1,GSIDIRTY                                                
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT             SAVE RECORD ON TSAR RECORD                   
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL               REDISPLAY RECORD                             
         J     CHAX3                                                            
*                                                                               
CHA22    GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         JZ    CHAX1               NO INPUT                                     
         J     CHAX3               INPUT NOT SAVED                              
*                                                                               
CHA24    TM    GCINDS2,GCIANYCH    TEST ANY INPUT                               
         JO    CHA28                                                            
*                                                                               
         GOTOX SCROLL              TEST ANY SCROLL                              
         JE    CHA26                                                            
         JL    EXITL                                                            
*                                                                               
         TM    GCINDS3,GCIRCHG                                                  
         JZ    CHAX1                                                            
         NI    GCINDS3,255-GCIRCHG                                              
         OI    GCINDS2,GCIACTCP                                                 
         J     CHAX1                                                            
*                                                                               
CHA26    GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
CHA28    NI    GCINDS2,FF-GCIPFNTR WRITE BACK UPDATED RECORD                    
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT                                                          
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITACT                                                          
         J     CHAX2                                                            
*                                                                               
CHAX1    MVI   FVOMTYP,GTMINF      EXIT WITH 'ENTER CHANGES' MESSAGE            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
*??      OI    GCINDS2,GCIACTCP                                                 
         J     EXITREC                                                          
*                                                                               
CHAX2    MVI   FVOMTYP,GTMINF      EXIT WITH 'RECORD CHANGED' MESSAGE           
         MVC   FVMSGNO,=AL2(GI$RCCHA)                                           
         J     EXITACT                                                          
*                                                                               
CHAX3    MVI   FVOMTYP,GTMWRN      EXIT WITH 'UPDATE ME' MESSAGE                
         MVC   FVMSGNO,=AL2(GW$RCNUP)                                           
         J     EXITREC                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE A RECORD - TEST ACTION VALIDITY                              *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DELTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#DEL)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DELETE A RECORD FIRST TIME AFTER NTRSES                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DELNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY SET KEY SCREEN                            
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    DELNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
DELNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
DELNTR04 CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    SET MAINTENANCE SCREEN DISPLAYED             
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      ENTER TO DELETE MESSAGE                      
         MVC   FVMSGNO,=AL2(GI$RDETD)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DELETE A RECORD - FIRST TIME AFTER EXIT                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DELXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JZ    EXITACT             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DELETE A RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DEL1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    DELF02                                                           
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    *+14                NOT USING SCREEN CODES                       
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
         OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    DELF02                                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
DELF02   GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    DELF04              NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   DELF06                                                           
*                                                                               
DELF04   TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    DELF08              YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
DELF06   OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
DELF08   GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTDEL)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
DELF10   GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      RECORD DELETED?                              
         JZ    *+14                                                             
*&&US*&& MVC   FVMSGNO,=AL2(FVFXDEL)                                            
*&&UK*&& MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         J     EXITKEY                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF      ENTER TO DELETE MESSAGE                      
         MVC   FVMSGNO,=AL2(GI$RDETD)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DELETE A RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DEL      LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    DEL02                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JO    DEL02                                                            
*                                                                               
         OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    DEL02                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
DEL02    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    DEL04               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   DEL06                                                            
*                                                                               
DEL04    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    DEL08               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
DEL06    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
DEL08    CLC   GSRECKEY,GCLASKEY    TEST CHANGE OF KEY                          
         JE    DEL10                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTDEL)                              
         BNL   DEL12                                                            
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
DEL10    GOTOX AGEN,RTPARM,OIO,IRECGET,A(XOLOCK) GET RECORD FOR UPD             
         JE    DEL12                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTDEL)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC (ERROR SET BY GETREC??)            
         J     EXITREC                                                          
*                                                                               
DEL12    OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTDEL)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         TM    GSRECSTA,X'80'                                                   
         JZ    DEL14                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*&&US*&& MVC   FVMSGNO,=AL2(FVFXDEL)                                            
*&&UK*&& MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         J     EXITKEY                                                          
*                                                                               
DEL14    CLC   GSRECKEY,GCLASKEY    TEST CHANGE OF KEY                          
         JE    DEL16                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     DELX1                                                            
*                                                                               
DEL16    GOTOX SCROLL              TEST SCROLLING                               
         JL    EXITL                                                            
         JH    DEL18                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     DELX1                                                            
*                                                                               
DEL18    GOTOX AGEN,RTPARM,ORECH,RDEL,AIOREC  DELETE THE RECORD                 
         JL    EXITL                                                            
         OI    GCINDS2,GCIACTCP                                                 
         J     DELX2                                                            
*                                                                               
DELX1    MVI   FVOMTYP,GTMINF      ENTER TO DELETE MESSAGE                      
         MVC   FVMSGNO,=AL2(GI$RDETD)                                           
         J     EXITACT                                                          
*                                                                               
DELX2    MVI   FVOMTYP,GTMINF      RECORD DELETED MESSAGE                       
         MVC   FVMSGNO,=AL2(GI$RCDEL)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RESTORE A RECORD - TEST ACTION VALIDITY                             *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RESTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#RES)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RESTORE A RECORD - FIRST TIME AFTER NTRSES                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RESNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    RESNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
RESNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
RESNTR04 CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT                                                 
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      ENTER TO RESTORE MESSAGE                     
         MVC   FVMSGNO,=AL2(GI$RDETR)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RESTORE A RECORD - FIRST TIME AFTER XITSES                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RESXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JZ    EXITACT             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RESTORE A RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RES1ST   DS    0H                                                               
RES      LR    R7,RF                                                            
*                                                                               
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         JZ    *+8                                                              
         OI    GSINDSG1,GSG1MSAV                                                
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    RES04                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    RES02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    RES04               NO                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
RES02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    RES04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
RES04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JNE   EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    RES06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   RES08                                                            
*                                                                               
RES06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    RES10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
RES08    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
RES10    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JE    RES12                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTRES)                              
         BNL   RES14                                                            
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
RES12    GOTOX AGEN,RTPARM,OIO,IRECGET,A(XOLOCK) GET RECORD FOR UPD             
         JE    RES14                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTRES)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         J     EXITREC                                                          
*                                                                               
RES14    OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTRES)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD DELETED                          
         JO    RES16                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
*&&US*&& MVC   FVMSGNO,=AL2(FVFXRES)                                            
*&&UK*&& MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXITKEY                                                          
*                                                                               
RES16    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JE    RES18               NO                                           
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         J     RESX1                                                            
*                                                                               
RES18    GOTOX SCROLL              TEST SCROLLING                               
         JH    RES20                                                            
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         J     RESX1                                                            
*                                                                               
RES20    GOTOX AGEN,RTPARM,ORECH,RRES,AIOREC  RESTORE THE RECORD                
         JL    EXITL                                                            
         OI    GCINDS2,GCIACTCP                                                 
         J     RESX2                                                            
*                                                                               
RESX1    MVI   FVOMTYP,GTMINF      ENTER TO RESTORE MESSAGE                     
         MVC   FVMSGNO,=AL2(GI$RDETR)                                           
         J     EXITACT                                                          
*                                                                               
RESX2    MVI   FVOMTYP,GTMINF      RECORD RESTORED MESSAGE                      
         MVC   FVMSGNO,=AL2(GI$RCRES)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MAINTAIN A RECORD - TEST ACTION VALIDITY                            *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
MNTTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#MNT)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* MAINTAIN - 1ST TIME AFTER NTRSES                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
MNTNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY LOAD KEY SCREEN                           
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    MNTNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
MNTNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
MNTNTR04 CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT    FLAG MAINT SCREEN LOADED?                    
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      EXIT WITH 'ENTER CHANGES' MESSAGE            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         OI    GCINDS2,GCIACTCP                                                 
         J     EXITREC                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* MAINT - 1ST TIME AFTER XITSES                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
MNTXIT   LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA     RECORD ON FILE?                              
         JZ    EXITACT             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* MAINTAIN - FIRST TIME FOR ACTION                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
MNT1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         NI    GSINDSL,FF-(GSIDIRTY)                                            
         L     RF,=A(MNT)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* MAINTAIN A RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
MNT      LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    MNT04                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    MNT02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    MNT04               NO                                           
         TM    GSINDSL1,GSIXKEY                                                 
         JO    MNT04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
MNT02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         TM    GSINDSL1,GSIXKEY    NO KEY ENTER REQUESTED?                      
         JO    MNT04                                                            
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
MNT04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTMNT)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         OC    GSRECDA,GSRECDA     TEST ON FILE                                 
         JNZ   MNT24               YES                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    MNT06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   MNT08                                                            
*                                                                               
MNT06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    MNT10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
MNT08    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
MNT10    TM    GSFRR.FRRINDS1,FRR1HEIR   RECORD HAS DEFAULTS                    
         JZ    MNT14               NO                                           
*                                                                               
         CLC   GSRECKEY,GCLASKEY   TEST NEW KEY                                 
         JE    MNT12               NO                                           
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,OHEIR,HGET,GSRECKEY                                  
         JH    MNT14               NO DEFAULT RECORD                            
         JL    EXIT                ERROR EXIT                                   
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXITREC             ALLOW USER TO ENTER OVERRIDES                
*                                                                               
MNT12    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC VALIDATE RECORD                    
         JL    EXITL                                                            
         J     MNT18                                                            
*                                                                               
MNT14    TM    GSINDSL1,GSIDISCL      EXTRA DISPLAY FOR WALTER                  
         JZ    MNT16                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
*                                                                               
MNT16    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC VALIDATE RECORD DETAILS            
         JL    EXITL                                                            
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1UPDT TEST UPDATABLE RECORD                    
         JZ    MNT20                                                            
         CLI   GSFRP.FRPTYPE,FRPTUPDT  TEST UPDATE PFKEY                        
         JE    MNT20                                                            
*                                                                               
         TM    GCINDS2,GCIANYCH    ANY CHANGES THIS TIME                        
         JZ    MNT18                                                            
*                                                                               
         OI    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT             SAVE RECORD ON TSAR RECORD                   
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL               REDISPLAY RECORD                             
         J     MNTX2                                                            
*                                                                               
MNT18    GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         JZ    MNTX1               NO INPUT                                     
         J     MNTX2               INPUT NOT SAVED                              
*                                                                               
MNT20    TM    GCINDS2,GCIPFNTR    TEST PFK ACTION PENDING                      
         JO    MNT22                                                            
         GOTOX AGEN,RTPARM,ORECH,RADD,AIOREC  ADD RECORD                        
         JL    EXITL                                                            
*                                                                               
MNT22    GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC RE-DISPLAY RECORD                  
         JL    EXITL                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
         J     EXITKEY                                                          
*                                                                               
MNT24    TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    MNT26               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   MNT28                                                            
*                                                                               
MNT26    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    MNT30               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
MNT28    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
MNT30    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JNE   MNT32               YES                                          
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECGET,A(XOLOCK) GET RECORD FOR UPD             
         JL    EXITL                                                            
         JE    MNT32                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC (ERROR SET BY GREC??)              
         JL    EXITL                                                            
         J     EXITREC                                                          
*                                                                               
MNT32    TM    GSRECSTA,X'80'      TEST RECORD IS DELETED                       
         JZ    MNT34                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         J     EXITKEY                                                          
*                                                                               
MNT34    CLC   GSRECKEY,GCLASKEY   TEST CHANGE OF KEY                           
         JE    MNT36                                                            
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         J     EXITREC                                                          
*                                                                               
MNT36    GOTOX AGEN,RTPARM,ORECH,RVAL,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1UPDT TEST UPDATEABLE RECORD                   
         JZ    MNT40                                                            
         CLI   GSFRP.FRPTYPE,FRPTUPDT  TEST UPDATE PFKEY                        
         JE    MNT42                                                            
*                                                                               
         TM    GCINDS2,GCIANYCH    ANY CHANGES THIS TIME                        
         JZ    MNT38                                                            
         OI    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT             SAVE RECORD ON TSAR RECORD                   
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL               REDISPLAY RECORD                             
         J     MNTX2                                                            
*                                                                               
MNT38    GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSINDSL1,GSIDIRTY   TEST ANY INPUT TO JE SAVED                   
         JZ    MNTX3               NO INPUT                                     
         J     MNTX2               INPUT NOT SAVED                              
*                                                                               
MNT40    TM    GCINDS2,GCIANYCH    TEST ANY INPUT                               
         JO    MNT42                                                            
*                                                                               
         GOTOX SCROLL              TEST ANY SCROLL                              
         JH    MNTX3                                                            
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     MNTX3                                                            
*                                                                               
MNT42    NI    GCINDS2,FF-GCIPFNTR WRITE BACK UPDATED RECORD                    
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC                                    
         JL    EXITACT                                                          
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) TURN OFF 'DIRTY' FLAG                     
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITACT                                                          
         J     MNTX4                                                            
*                                                                               
MNTX1    MVC   FVMSGNO,=AL2(FVFEKEYD)                                           
*        OI    GCINDS2,GCIACTCP    ??                                           
         J     EXITACT                                                          
*                                                                               
MNTX2    MVI   FVOMTYP,GTMWRN      EXIT WITH 'UPDATE ME ' MESSAGE               
         MVC   FVMSGNO,=AL2(GW$RCNUP)                                           
         J     EXITACT                                                          
*                                                                               
MNTX3    MVI   FVOMTYP,GTMINF      EXIT WITH 'ENTER CHANGES' MESSAGE            
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
*        OI    GCINDS2,GCIACTCP    ??                                           
         J     EXITREC                                                          
*                                                                               
MNTX4    MVI   FVOMTYP,GTMINF      EXIT WITH 'RECORD CHANGED' MESSAGE           
         MVC   FVMSGNO,=AL2(GI$RCCHA)                                           
         J     EXITACT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACTION BUILD - FIRST TIME AFTER AN NTRSES                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BUILDNTR LR    R7,RF                                                            
         L     RF,=A(BUILD)                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ACTION BUILD - FIRST TIME AFTER AN EXITSES                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BUILDXIT LR    R7,RF                                                            
         OC    GSRECDA,GSRECDA                                                  
         JZ    EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SSCRN BUILD SCREEN                             
         J     EXITOPT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ACTION BUILD                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BUILD1ST DS    0H                                                               
BUILD    LR    R7,RF                                                            
*                                                                               
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         JZ    *+8                                                              
         OI    GSINDSG1,GSG1MSAV                                                
*                                                                               
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECGET,A(XOLOCK) GET RECORD FOR UPD             
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETED                       
         JZ    BLD02                                                            
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         J     EXITOPT                                                          
BLD02    MVC   RTMSK,GSRECMSK      TEST RECORD CHANGEABLE                       
         NC    RTMSK,=AL4(MK#CHA)                                               
         JNZ   BLD04                                                            
         GOTOX AGEN,RTPARM,OSCRN,SSCRN                                          
         MVC   FVMSGNO,=AL2(FVFXCHA)                                            
         J     EXITOPT                                                          
*                                                                               
BLD04    TM    GCINDS2,GCINTRS     FIRST TIME IN?                               
         JZ    BLD06                                                            
         GOTOX AGEN,RTPARM,OSCRN,SSCRN BUILD SCREEN                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         J     EXITOPT                                                          
*                                                                               
BLD06    GOTOX AGEN,RTPARM,('GCBOVER',OBUILD),BLDSCRN,AIOREC                    
         JNE   EXIT                                                             
         NI    GCINDS2,FF-GCIPFNTR IGNORE ANY PFKEYS                            
         GOTOX AGEN,RTPARM,ORECH,RWRT,AIOREC WRITE BACK RECORD                  
         JNE   EXITOPT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SSCRN BUILD SCREEN                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCCHA)                                           
         OI    GCINDS2,GCIACTCP                                                 
         J     EXITOPT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COPY A RECORD - TEST ACTION VALIDITY                                *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CPYTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#CPY)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* COPY A RECORD - FIRST TIME AFTER NTRSES                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CPYNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    CPYNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
CPYNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
CPYNTR04 CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT                                                 
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      RECORD DELETED?                              
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITL                                                            
*                                                                               
         OI    GSINDSL1,GSIRCOPY   FLAG RECORD DISPLAYED TO COPY                
         MVC   GSCPYKEY,GSRECKEY                                                
         MVC   GSCPYDA,GSRECDA                                                  
*                                                                               
         GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW)                                            
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* COPY A RECORD - FIRST TIME AFTER XITSES                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CPYXIT   LR    R7,RF                                                            
         L     RF,=A(CPY)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* COPY A RECORD - FIRST TIME FOR ACTION                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CPY1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         NI    GSINDSL1,FF-GSIRCOPY                                             
         L     RF,=A(CPY)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* COPY A RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CPY      LR    R7,RF                                                            
         TM    GSINDSL1,GSIRCOPY   TEST RECORD DISPLAYED FOR COPY               
         JNZ   CPY12                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    CPY04                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    CPY02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    CPY04               NO                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
CPY02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
CPY04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    CPY06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   CPY08                                                            
*                                                                               
CPY06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    CPY10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
CPY08    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
CPY10    GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTCPY)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETABLE                     
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITKEY                                                          
*                                                                               
         OI    GSINDSL1,GSIRCOPY  SET RECORD DISPLAYED FOR COPY                 
         MVC   GSCPYKEY,GSRECKEY  SAVE KEY & D/A                                
         MVC   GSCPYDA,GSRECDA                                                  
         J     CPYX1                                                            
*                                                                               
CPY12    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         CLC   GSRECKEY,GSCPYKEY   TEST SAME COPY KEY                           
         JNE   CPY14                                                            
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     CPYX1                                                            
*                                                                               
CPY14    OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JZ    CPY16                                                            
         TM    GSRECSTA,X'80'      NO - TEST RECORD IS DELETED                  
         JO    CPY16                                                            
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXITKEY                                                          
*                                                                               
CPY16    GOTOX AGEN,RTPARM,ORECH,RCPY,GSCPYDA,0                                 
         JL    EXITL                                                            
         NI    GSINDSL1,FF-GSIRCOPY                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
         J     EXITKEY                                                          
*                                                                               
CPYX1    GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW) ENTER NEW KEY                              
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RENAME A RECORD - TEST ACTION VALIDITY                              *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#REN)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME A RECORD - FIRST TIME AFTER NTRSES                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENNTR   LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    RENNTR02                                                         
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
RENNTR02 GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT                                                 
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      RECORD DELETED?                              
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITL                                                            
*                                                                               
         OI    GSINDSL1,GSIRCOPY   FLAG RECORD DISPLAYED TO COPY                
         MVC   GSCPYKEY,GSRECKEY                                                
         MVC   GSCPYDA,GSRECDA                                                  
*                                                                               
         GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW)                                            
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME A RECORD - FIRST TIME AFTER XITSES                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENXIT   LR    R7,RF                                                            
         L     RF,=A(REN)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME A RECORD - FIRST TIME FOR ACTION                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REN1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         NI    GSINDSL1,FF-GSIRCOPY                                             
         L     RF,=A(REN)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME A RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REN      LR    R7,RF                                                            
         TM    GSINDSL1,GSIRCOPY   TEST RECORD DISPLAYED FOR RENAME             
         JNZ   REN12                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    REN04                                                            
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    REN02               NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    REN04               NO                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
REN02    OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
REN04    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    REN06               NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   REN08                                                            
*                                                                               
REN06    TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    REN10               YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
REN08    OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
*                                                                               
REN10    GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTREN)                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETABLE                     
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITKEY                                                          
*                                                                               
         OI    GSINDSL1,GSIRCOPY  SET RECORD DISPLAYED FOR COPY                 
         MVC   GSCPYKEY,GSRECKEY   SAVE KEY & D/A                               
         MVC   GSCPYDA,GSRECDA                                                  
         J     RENX1                                                            
*                                                                               
REN12    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         CLC   GSRECKEY,GSCPYKEY   TEST SAME COPY KEY                           
         JNE   REN14                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     RENX1                                                            
*                                                                               
REN14    OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JZ    REN16                                                            
         TM    GSRECSTA,X'80'      NO - TEST RECORD IS DELETED                  
         JO    REN16                                                            
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXITKEY                                                          
*                                                                               
REN16    GOTOX AGEN,RTPARM,ORECH,RREN,GSCPYDA                                   
         NI    GSINDSL1,FF-GSIRCOPY                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
         J     EXITKEY                                                          
*                                                                               
RENX1    GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW) ENTER NEW KEY                              
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST A RECORD - TEST ACTION VALIDITY                                *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
LSTTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#LST)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ACTION LIST                                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
LST      LR    R7,RF                                                            
         GOTOX AGENLST,RTPARM,OLIST,(RTPARMS2,LPRC)                             
         J     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACTION  FOFF                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
FOFF     LR    R7,RF                                                            
         GOTOX AOLY,0                                                           
         J     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD A RECORD - TEST ACTION VALIDITY                            *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DWNTST   LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#DWN)                                               
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DOWNLOAD - 1ST TIME FOR ACTION                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DWN1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         MVI   DLINDS,0            RESET DOWNLOAD INDICATORS                    
         L     RF,=A(DWN)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DOWNLOAD                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DWNNTR   DS    0H                  FIRST TIME AFTER NTRSES                      
DWNXIT   DS    0H                  FIRST TIME AFTER XITSES                      
DWN      LR    R7,RF                                                            
         CLI   ASONOFF,ASON        ONLINE REPORT?                               
         JE    DWN01                                                            
         CLI   TWMODE,TWLOAD       LOAD MODE FROM SPOOF?                        
         JNE   DWN01                                                            
         GOTOX AGEN,RTPARM,OSCRN,SDLOAD LOAD IN REPORT SCREEN                   
         J     DWNOK                                                            
*                                                                               
DWN01    CLI   ASONOFF,ASOFF                                                    
         JE    DWN02                                                            
         GOTOX AGEN,RTPARM,OSCRN,SDLOAD LOAD IN DOWNLOAD SCREEN                 
         JE    DWN02                                                            
         TM    GSINDSL1,GSIXKEY    NO 'ENTER FIELDS' MSG REQUESTED              
         JO    DWN02               YES                                          
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
         J     DWNX                INVITE USER TO ENTER DOWNLOAD DATA           
*                                                                               
DWN02    GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AOLY,RTPARM,ODLOAD,DAPPCOL                                       
         JL    EXITL                                                            
         JNE   *+8                                                              
         OI    DLINDS,DLCOLSEL     OVERLAY SET COLUMNS ITSELF                   
*                                                                               
         TM    DLINDS,DLCOLSEL     SET COLUMNS?                                 
         JO    DWN04                                                            
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#DLOAD      SELECT WHICH DOWNLOAD TO DO                  
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL                                                 
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DROP  N                                                                
         DC    H'0'                                                             
*                                                                               
DWN04    GOTOX AGENRPT,RTPARM,ODLOAD,DPQINIT                                    
         JL    EXITL               INITIALISE REPORT BLOCK                      
*                                                                               
         GOTOX AOLY,RTPARM,ODLOAD,DAPPFLT                                       
         JL    EXITL                                                            
         JNE   *+8                                                              
         OI    DLINDS,DLFLTSET     OVERLAY SET FILTERS ITSELF                   
*                                                                               
         TM    DLINDS,DLFLTSET     FILTERS SET?                                 
         JO    DWN06               YES                                          
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#FLTR       SELECT FILTERS                               
         MVI   N.SACT,A#LST                                                     
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DROP  N                                                                
         DC    H'0'                                                             
*                                                                               
DWN06    CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         JE    DWN10               YES                                          
         CLI   GSFRP.FRPTYPE,FRPTDWN  DOWNLOAD PFKEY?                           
         JNE   DWNX                                                             
         CLI   INWHEN,INWNNOW      NOW REPORT?                                  
         JE    DWN10               YES                                          
*                                                                               
         GOTOX  ('SAVVAL',AGROUTS) SAVE ALL VALUES (TIA GETS CORRUPTED)         
         GOTOX AGENRPT,RTPARM,ODLOAD,DSPOOK                                     
         IPM   R0                                                               
         GOTOX  ('RESVAL',AGROUTS) RESTORE TIA                                  
         SPM   R0                                                               
         JL    DWNERR              SPOOK RETURN HERE                            
         J     DWNOK                                                            
*                                                                               
DWN10    GOTOX  ('SAVVAL',AGROUTS) SAVE ALL VALUES (TIA GETS CORRUPTED)         
         GOTOX AGENRPT,RTPARM,ODLOAD,DLDO                                       
         IPM   R0                                                               
         GOTOX  ('RESVAL',AGROUTS) RESTORE TIA                                  
         SPM   R0                                                               
         JL    DWNERR              SPOOK RETURN HERE                            
         J     DWNOK                                                            
*                                                                               
DWNOK    CLC   FVMSGNO,=AL2(FVFOK)                                              
         JNE   EXITKEY                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(31)                                                 
         J     EXITKEY                                                          
*                                                                               
DWNERR   J     EXITL                                                            
         SPACE 1                                                                
*                                                                               
DWNX     MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$EFAR)                                            
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT - 1ST TIME FOR ACTION                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REP1ST   LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         MVI   RPOINDS,0           RESET REPORT INDICATORS                      
         L     RF,=A(REP)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* REPORT                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REPNTR   DS    0H                  FIRST TIME AFTER NTRSES                      
REPXIT   DS    0H                  FIRST TIME AFTER XITSES                      
REP      LR    R7,RF                                                            
         CLI   ASONOFF,ASON        ONLINE REPORT?                               
         JE    REP02                                                            
         CLI   TWMODE,TWLOAD       LOAD MODE FROM SPOOF?                        
         JNE   REP02                                                            
         GOTOX AGEN,RTPARM,OSCRN,SREPORT LOAD IN REPORT SCREEN                  
         J     REPOK                                                            
*                                                                               
REP02    GOTOX AGEN,RTPARM,OSCRN,SREPORT LOAD IN REPORT SCREEN                  
         JE    REP04               INVITE USER TO ENTER REP DETAILS             
         TM    GSINDSL1,GSIXKEY    NO 'ENTER FIELDS' MSG REQUESTED              
         JO    REP04               YES                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$EFAR)                                            
         J     EXITKEY                                                          
*                                                                               
REP04    GOTOX AGENRPT,RTPARM,OREP,RPQINIT                                      
         JL    REPERR              INITIALISE REPORT BLOCK                      
         GOTOX AGENRPT,RTPARM,OREP,RPSVAL                                       
         JL    REPERR              VALIDATE REPORT SCREEN                       
*                                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         JE    *+12                YES                                          
         CLI   INWHEN,INWNNOW      NOW REPORT?                                  
         JNE   REP06               NO - BUILD SPOOK                             
*                                                                               
         GOTOX  ('SAVVAL',AGROUTS) SAVE ALL VALUES (TIA GETS CORRUPTED)         
         GOTOX AGENRPT,RTPARM,OREP,RDO                                          
         IPM   R0                                                               
         GOTOX  ('RESVAL',AGROUTS) RESTORE TIA                                  
         SPM   R0                                                               
         JL    REPERR              REPORT RETURN HERE                           
         J     REPOK                                                            
*                                                                               
REP06    GOTOX  ('SAVVAL',AGROUTS) SAVE ALL VALUES (TIA GETS CORRUPTED)         
         GOTOX AGENRPT,RTPARM,OREP,RSPOOK                                       
         IPM   R0                                                               
         GOTOX  ('RESVAL',AGROUTS) RESTORE TIA                                  
         SPM   R0                                                               
         JL    REPERR              SPOOK RETURN HERE                            
         J     REPOK                                                            
*                                                                               
REPOK    CLC   FVMSGNO,=AL2(FVFOK)                                              
         JNE   EXITKEY                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(31)                                                 
         J     EXITKEY                                                          
*                                                                               
REPERR   J     EXITL                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RENAME OVER LIVE RECORD - TEST ACTION VALIDITY                      *         
*                                                                     *         
* CC LOW IF ACTION INVALID FOR RECORD                                 *         
* CC EQ  IF ACTION IS VALID FOR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENOTST  LR    R7,RF                                                            
         MVC   RTMSK,GSRECMSK                                                   
         NC    RTMSK,=AL4(MK#RENO)                                              
         JZ    EXITL                                                            
         J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME OVER RECORD - FIRST TIME AFTER NTRSES                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENONTR  LR    R7,RF                                                            
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
*                                                                               
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         JO    RENON02                                                          
         GOTOX AGEN,RTPARM,OKEY,KDIS,GSRECKEY                                   
*                                                                               
RENON02  GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
*                                                                               
         CLI   GSSMPAGE,0          LOAD IN MAINTENANCE SCREEN                   
         JNE   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT                                                 
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      RECORD DELETED?                              
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITL                                                            
*                                                                               
         OI    GSINDSL1,GSIRCOPY   FLAG RECORD DISPLAYED TO COPY                
         MVC   GSCPYKEY,GSRECKEY                                                
         MVC   GSCPYDA,GSRECDA                                                  
*                                                                               
         GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW)                                            
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME OVER RECORD - FIRST TIME AFTER XITSES                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENOXIT  LR    R7,RF                                                            
         L     RF,=A(REN)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME OVER RECORD - FIRST TIME FOR ACTION                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENO1ST  LR    R7,RF                                                            
         OI    GSINDSG1,GSG1MSAV                                                
         NI    GSINDSL1,FF-GSIRCOPY                                             
         L     RF,=A(REN)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* RENAME OVER RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RENO     LR    R7,RF                                                            
         TM    GSINDSL1,GSIRCOPY   TEST RECORD DISPLAYED FOR RENAME             
         JNZ   RENO12                                                           
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKEY TEST CHANGE OF SCREEN                     
         JE    RENO04                                                           
*                                                                               
         NI    GSINDSL1,FF-GSIXMNT TURN OFF MAINT SCREEN LOADED FLAG            
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    RENO02              NOT USING SCREEN CODES                       
*                                                                               
         TM    BCINDS1,BCINACT     FIRST TIME?                                  
         JZ    RENO04              NO                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
RENO02   OC    GSSMPAGE,GSSMPAGE   LOAD IN MAINTENANCE SCREEN                   
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1          LOAD IN MAINTENANCE SCREEN                   
         GOTOX AGEN,RTPARM,OSCRN,SPAGE                                          
         JL    EXITL                                                            
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         J     EXITKEY             INVITE USER TO ENTER KEY                     
*                                                                               
RENO04   GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
         OC    GSRECDA,GSRECDA     TEST RECORD ON FILE                          
         JNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITKEY                                                          
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         JZ    RENO06              NOT USING SCREEN CODES                       
*                                                                               
         MVC   RTBYTE1,GSSMCODE    SAVE CURRENT CODE                            
         GOTOX AOLY,RTPARM,OSCRN,SSET,GSRECKEY SET NEW CODE                     
         CLC   GSSMCODE,RTBYTE1    IF CHANGED WILL NEED DIFF. SCREEN            
         JNE   RENO08                                                           
*                                                                               
RENO06   TM    GSINDSL1,GSIXMNT    MAINT SCREEN ALREADY LOADED?                 
         JO    RENO10              YES - WELL DON`T WASTE MY TIME THEN          
*                                                                               
RENO08   OC    GSSMPAGE,GSSMPAGE   MAINTENANCE SCREEN PAGE SET?                 
         JNZ   *+8                                                              
         MVI   GSSMPAGE,1                                                       
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
         GOTOX AGEN,RTPARM,OSCRN,SPAGE LOAD MAINT SCREEN PAGE                   
         JL    EXITL                                                            
*                                                                               
RENO10   GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,OACTH,('ACTTST',ACTRENO)                             
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACNV)                                            
         J     EXITACT                                                          
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
*                                                                               
         TM    GSRECSTA,X'80'      TEST RECORD IS DELETABLE                     
         JZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         J     EXITKEY                                                          
*                                                                               
         OI    GSINDSL1,GSIRCOPY  SET RECORD DISPLAYED FOR COPY                 
         MVC   GSCPYKEY,GSRECKEY   SAVE KEY & D/A                               
         MVC   GSCPYDA,GSRECDA                                                  
         J     RENOX1                                                           
*                                                                               
RENO12   GOTOX AGEN,RTPARM,OKEY,KVAL,GSRECKEY                                   
         JL    EXITL                                                            
*                                                                               
         CLC   GSRECKEY,GSCPYKEY   TEST SAME COPY KEY                           
         JNE   RENO14                                                           
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RGET,0                                         
         JL    EXITL                                                            
*                                                                               
         GOTOX SCROLL              TEST ANY SCROLL                              
         JL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RDIS,AIOREC                                    
         JL    EXITL                                                            
         J     RENOX1                                                           
*                                                                               
RENO14   OC    GSRECDA,GSRECDA     TEST RECORD IS ON FILE                       
         JZ    RENO16                                                           
*                                                                               
RENO16   GOTOX AGEN,RTPARM,ORECH,RRENO,GSCPYDA                                  
         NI    GSINDSL1,FF-GSIRCOPY                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
         J     EXITKEY                                                          
*                                                                               
RENOX1   GOTOX ('UPROSCR',AGROUTS)   UNPROTECT SCREEN IF REQUIRED               
         MVC   FVMSGNO,=AL2(FVFENEW) ENTER NEW KEY                              
         J     EXITKEY                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  PFKEY OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PFKEY    LR    R7,RF                                                            
         L     R1,RTPARMS2                                                      
         LA    RF,PFKEYT                                                        
         J     ITER                                                             
*                                                                               
PFKEYT   DS    0X                                                               
         DC    AL1(PFLST,0,0,0),AL4(PFKLST)                                     
         DC    AL1(PFVAL,0,0,0),AL4(PFKVAL)                                     
         DC    AL1(PFBLD,0,0,0),AL4(PFKBLD)                                     
         DC    AL1(PFUSER,0,0,0),AL4(PFKUSER)                                   
         DC    AL1(PFREC,0,0,0),AL4(PFKREC)                                     
         DC    AL1(PFACT,0,0,0),AL4(PFKACT)                                     
PFKEYTX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST OF PFK ELEMENTS                                     *  1 *         
*                                                                ******         
* NTRY: P3 BYTE 0 = PFKEY NUMBER TO MATCH ON OR 0                     *         
*             1-3 = A(OUTPUT FOR PFKEY ELEMENTS)                      *         
* EXIT: OUTPUT    = PFK1 ENTRY, PFK2 ENTRY,.....,PFK24 ENTRY          *         
***********************************************************************         
         SPACE 1                                                                
X        USING FRPRECD,IOKEY                                                    
PFKLST   XR    R0,R0                                                            
         ICM   R0,7,RTPARMS3+1     GET A(OUTPUT BUFFER)                         
         LHI   R1,24*FRPLNQ                                                     
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT BUFFER                          
*                                                                               
         XC    RTBYTE1,RTBYTE1                                                  
         MVC   PLNUMB,RTPARMS3     SET REQUESTED NUMBER (OR 0)                  
         MVC   PLKEYB,CSRECACT     SET CURRENT RECORD/ACTION                    
         CLI   PLACTB,0            ANY CURRENT ACTION?                          
         JNE   *+12                                                             
         MVI   PLRECB,FF           SET ALL RECORD/ACTION                        
         MVI   PLACTB,FF                                                        
         XR    R4,R4                                                            
         ICM   R4,7,RTPARMS3+1     R4=A(OUTPUT BUFFER)                          
*                                                                               
         OC    PLNUMB,PLNUMB       READING FOR SPECIFIC PFKEY                   
         JZ    PKLST06             NO                                           
         OC    ASTEST,ASTEST       CONNECTED TO A TEST PHASE?                   
         JZ    PKLST02             NO                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XC    X.FRPKEY,X.FRPKEY   READ PFKEY RECORD FOR NUMBER                 
         MVI   X.FRPKMIN,FRPKMINQ  CONNECTED COUNTRY AND                        
         MVI   X.FRPKTYP,FRPKTYPQ  TEST PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,PLNUMB                                                 
         MVC   X.FRPKCTRY,CUCTRY                                                
         XI    X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         MVC   X.FRPKTEST,ASTEST                                                
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
PKLST02  XC    X.FRPKEY,X.FRPKEY   READ PFKEY RECORD FOR NUMBER                 
         MVI   X.FRPKMIN,FRPKMINQ  CONNECTED COUNTRY AND                        
         MVI   X.FRPKTYP,FRPKTYPQ  LIVE PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,PLNUMB                                                 
         MVC   X.FRPKCTRY,CUCTRY                                                
         XI    X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         XC    X.FRPKTEST,X.FRPKTEST                                            
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         JZ    PKLST04             NO                                           
*                                                                               
         XC    X.FRPKEY,X.FRPKEY   READ PFKEY RECORD FOR NUMBER                 
         MVI   X.FRPKMIN,FRPKMINQ  HOST PROCESSOR AND                           
         MVI   X.FRPKTYP,FRPKTYPQ  TEST PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,PLNUMB                                                 
         MVI   X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         MVC   X.FRPKTEST,ASTEST                                                
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
PKLST04  XC    X.FRPKEY,X.FRPKEY   READ PFKEY RECORD FOR NUMBER                 
         MVI   X.FRPKMIN,FRPKMINQ  HOST PROCESSOR AND                           
         MVI   X.FRPKTYP,FRPKTYPQ  LIVE PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,PLNUMB                                                 
         MVI   X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         XC    X.FRPKTEST,X.FRPKTEST                                            
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
         J     PKLSTX                                                           
*                                                                               
PKLST06  XR    RF,RF                                                            
         IC    RF,RTBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,RTBYTE1                                                       
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XC    X.FRPKEY,X.FRPKEY   READ HIGH FOR NEXT NUMBER                    
         MVI   X.FRPKMIN,FRPKMINQ                                               
         MVI   X.FRPKTYP,FRPKTYPQ                                               
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,RTBYTE1                                                
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(FRPKPFK-FRPRECD),IOKEYSAV                                  
         JNE   PKLSTX              NO MORE PFKEYS FOR THIS ACTION               
*                                                                               
         MVC   RTBYTE1,X.FRPKPFK   SAVE NEXT PFKEY NUMBER                       
         OC    ASTEST,ASTEST       CONNECTED TO A TEST PHASE?                   
         JZ    PKLST08                                                          
*                                                                               
         XC    X.FRPKEY,X.FRPKEY   READ NEXT PFKEY RECORD FOR                   
         MVI   X.FRPKMIN,FRPKMINQ  CONNECTED COUNTRY AND                        
         MVI   X.FRPKTYP,FRPKTYPQ  TEST PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,RTBYTE1                                                
         MVC   X.FRPKCTRY,CUCTRY                                                
         XI    X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         MVC   X.FRPKTEST,ASTEST                                                
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
PKLST08  XC    X.FRPKEY,X.FRPKEY   READ NEXT PFKEY RECORD FOR                   
         MVI   X.FRPKMIN,FRPKMINQ  CONNECTED COUNTRY AND                        
         MVI   X.FRPKTYP,FRPKTYPQ  LIVE PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,RTBYTE1                                                
         MVC   X.FRPKCTRY,CUCTRY                                                
         XI    X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         XC    X.FRPKTEST,X.FRPKTEST                                            
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         JZ    PKLST10             NO                                           
*                                                                               
         XC    X.FRPKEY,X.FRPKEY   READ NEXT PFKEY RECORD FOR                   
         MVI   X.FRPKMIN,FRPKMINQ  HOST PROCESSOR AND                           
         MVI   X.FRPKTYP,FRPKTYPQ  TEST PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,RTBYTE1                                                
         MVI   X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         MVC   X.FRPKTEST,ASTEST                                                
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
*                                                                               
PKLST10  XC    X.FRPKEY,X.FRPKEY   READ NEXT PFKEY RECORD FOR                   
         MVI   X.FRPKMIN,FRPKMINQ  HOST PROCESSOR AND                           
         MVI   X.FRPKTYP,FRPKTYPQ  LIVE PHASE                                   
         MVC   X.FRPKSYS,GCOVSYS                                                
         MVC   X.FRPKPRG,GCPRGNO                                                
         MVC   X.FRPKREC,PLRECB                                                 
         MVC   X.FRPKACT,PLACTB                                                 
         MVC   X.FRPKPFK,RTBYTE1                                                
         MVI   X.FRPKCTRY,FF                                                    
         MVI   X.FRPKSUB,FF                                                     
         XC    X.FRPKTEST,X.FRPKTEST                                            
         L     R1,=AL4(XOGENDIR+XOREAD+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PKLST12                                                          
         J     PKLST06             NO MATCH FOR THIS NUMBER - TRY NEXT          
*                                                                               
PKLST12  L     R1,=AL4(XOGET+XOGENFIL+XIO2)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         JE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         L     R2,AIO2             ITERATE RECORD FOR FRPEL                     
         LA    R2,FRPFIRST(R2)                                                  
         USING FRPELD,R2                                                        
*                                                                               
         XR    RE,RE                                                            
PKLST14  CLI   FRPEL,0             END OF RECORD?                               
         JE    PKLSTNO             NO                                           
         CLI   FRPEL,FRPELQ        FRPEL?                                       
         JE    *+16                YES                                          
         IC    RE,FRPLN            NEXT ELEMENT                                 
         LA    R2,0(RE,R2)                                                      
         J     PKLST14                                                          
*                                                                               
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         JO    *+12                YES                                          
         TM    FRPINDS1,FRP1DDS                                                 
         JNZ   PKLST06                                                          
*                                                                               
         CLI   FRPTYPE,FRPTRAC     RECORD/ACTION PFKEY?                         
         JE    PKLST16             YES                                          
         CLI   FRPTYPE,FRPTRACN    RECORD/ACTION WITH NTRSES?                   
         JE    PKLST16             YES                                          
         CLI   FRPTYPE,FRPTPOSN    POSITIONAL PFKEY ON LIST                     
         JE    PKLST16             YES                                          
         J     PKLST22             NO                                           
*                                                                               
PKLST16  MVC   PLKEYA,FRPRECA      SET RECORD/ACTION REQUIRED                   
         CLI   PLRECA,R#ALL        ALL RECORDS?                                 
         JNE   *+10                                                             
         MVC   PLRECA,PLRECB                                                    
         CLI   PLACTA,A#ALL        ALL ACTIONS?                                 
         JNE   *+10                                                             
         MVC   PLACTA,PLACTB                                                    
*                                                                               
         LA    RE,TWASESRA         RECORD/ACTION ALREADY USED?                  
         XR    RF,RF                                                            
         ICM   RF,1,TWASESNL       NEST LEVEL                                   
         BRCT  RF,*+8                                                           
         J     PKLST20             NO - SAFE TO USE RECORD/ACTION               
*                                                                               
PKLST18  CLC   FRPRECA,0(RE)                                                    
         JE    PKLSTNO             ALREADY USED THIS ONE                        
         LA    RE,L'TWASESRA(RE)                                                
         BRCT  RF,PKLST18                                                       
*                                                                               
PKLST20  XR    R1,R1                                                            
         ICM   R1,8,GCOVSYS        SYSTEM                                       
         ICM   R1,4,GCPRGNO        PROGRAM                                      
         ICM   R1,3,PLKEYA         RECORD/ACTION                                
         GOTOX ('GETFACTN',AGROUTS),(R1)                                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         LA    RF,FRAFIRST(RF)                                                  
         XR    R1,R1                                                            
         USING FRAELD,RF                                                        
PKLST20A CLI   FRAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRAEL,FRAELQ                                                     
         BE    PKLST20B                                                         
         IC    R1,FRALN                                                         
         AR    RF,R1                                                            
         B     PKLST20A                                                         
*                                                                               
PKLST20B XC    RTHALF1,RTHALF1                                                  
         MVC   RTHALF1,PLKEYA                                                   
         OC    FRARSEC,FRARSEC     SECURITY OVERRIDE?                           
         BZ    *+10                                                             
         MVC   RTHALF1(1),FRARSEC                                               
         OC    FRAASEC,FRAASEC                                                  
         BZ    *+10                                                             
         MVC   RTHALF1+1(1),FRAASEC                                             
         DROP  RF                                                               
*                                                                               
         GOTOX ('TSTACS',AGROUTS),RTHALF1                                       
         JE    PKLST36             RECORD/ACTION COMBINATION VALID              
         J     PKLSTNO                                                          
*                                                                               
PKLST22  CLI   FRPTYPE,FRPTQPN     QUIT & PROCESS NEXT?                         
         JE    PKLST32             YES                                          
         CLI   FRPTYPE,FRPTQPL     QUIT TO PREVIOUS LEVEL?                      
         JE    PKLST32             YES                                          
*                                                                               
         CLI   FRPTYPE,FRPTMBAC    SCROLL MAINTENANCE BACKWARDS?                
         JNE   PKLST24                                                          
         CLI   GSSMPAGE,1                                                       
         JH    PKLST24                                                          
         J     PKLSTNO                                                          
*                                                                               
PKLST24  CLI   FRPTYPE,FRPTUP      SCROLL LIST UP?                              
         JNE   PKLST25                                                          
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         TM    LSSCIND1,LSSCINUP   VALID TO SCROLL UP?                          
         JO    PKLSTNO             NO                                           
         CLC   LSPAG#1,LSLST#1     AT TOP OF LIST CURRENTLY?                    
         JE    PKLSTNO             YES                                          
         J     PKLST36                                                          
*                                                                               
PKLST25  CLI   FRPTYPE,FRPTMUP     MAX SCROLL LIST UP?                          
         JNE   PKLST26                                                          
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         TM    LSSCIND1,LSSCINUP   VALID TO SCROLL UP?                          
         JO    PKLSTNO             NO                                           
         CLC   LSPAG#1,LSLST#1     AT TOP OF LIST CURRENTLY?                    
         JE    PKLSTNO             YES                                          
         J     PKLST36                                                          
*                                                                               
PKLST26  CLI   FRPTYPE,FRPTDOWN    SCROLL LIST DOWN?                            
         JNE   PKLST27             NO                                           
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         TM    LSLTIND1,LSLTIEOL   FOUND END OF LIST?                           
         JZ    PKLST36             NO                                           
         CLC   LSPAG#1,LSLST#1     TEST FIRST & LAST ON SAME SCREEN             
         JNE   PKLST36                                                          
         CLC   LSPAG#X,LSLST#X                                                  
         BNL   PKLSTNO                                                          
         J     PKLST36                                                          
*                                                                               
PKLST27  CLI   FRPTYPE,FRPTMDWN    MAX SCROLL LIST DOWN?                        
         JNE   PKLST28             NO                                           
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         TM    LSLTIND1,LSLTIEOL   FOUND END OF LIST?                           
         JZ    PKLSTNO             NO                                           
         CLC   LSPAG#X,LSLST#X     LAST ON SCREEN?                              
         BNL   PKLSTNO             YES - SUPPRESS                               
         J     PKLST36                                                          
*                                                                               
PKLST28  CLI   FRPTYPE,FRPTLEFT    SCROLL LIST LEFT?                            
         JNE   PKLST30                                                          
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         OC    LSVARLHS,LSVARLHS   START IS LEFT OF SCREEN?                     
         JZ    PKLSTNO             YES - CANNOT SCROLL LEFT                     
         J     PKLST36                                                          
*                                                                               
PKLST30  CLI   FRPTYPE,FRPTRGHT    SCROLL LIST RIGHT?                           
         JNE   PKLST36                                                          
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         JZ    PKLSTNO             NO                                           
         CLC   LSVARPAG,LSVARNUM   ALL VARIABLES ON SCREEN?                     
         JE    PKLSTNO             YES                                          
         J     PKLST36                                                          
*                                                                               
PKLST32  CLI   TWASESNL,1          MUST JE NESTED FOR QUIT/NEXT                 
         BNH   PKLSTNO             NOT NESTED                                   
*                                                                               
PKLST34  CLI   FRPTYPE,FRPTQPN     PROCESS NEXT PFKEY?                          
         JNE   PKLST36             NO                                           
         CLI   PSFRA.FRATYPE,FRATLIST  PREVIOUS LEVEL MUST JE A LIST            
         JE    PKLST36                 FOR PROCESS NEXT TO JE VALID             
         TM    PSINDSL1,GSIMLST        PREV LEVEL MUST JE A MAINT LIST          
         JZ    PKLSTNO                 FOR PROCESS NEXT TO JE VALID             
*                                                                               
PKLST36  GOTOX APRG,RTPARM,('GCBOVER',OPFK),PFLST,FRPELD                        
         JL    PKLSTNO             USER REJECTS THIS PFKEY                      
*                                                                               
PKLST38  XR    RE,RE               SAVE PFK IN TABLE ENTRY                      
         IC    RE,FRPPFK#                                                       
         BCTR  RE,0                                                             
         MHI   RE,FRPLNQ                                                        
         LA    RE,0(RE,R4)                                                      
         MVC   0(FRPLNQ,RE),FRPELD                                              
*                                                                               
PKLSTNO  OC    PLNUMB,PLNUMB       READING FOR ONE NUMBER?                      
         JNZ   PKLSTX              YES - EXIT                                   
         J     PKLST06             NEXT PFKEY                                   
*                                                                               
PKLSTX   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY PRESSED                                         *  2 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PFKVAL   XC    GSFRP.FRPELD(FRPLNQ),GSFRP.FRPELD                                
         TM    BCINDS1,BCIANYPF    USER ENTERED PFKEY THIS TIME?                
         JNZ   PFKVL02             YES                                          
         CLI   CSNEXTPF,0          NEXT TIME PFKEY SET?                         
         JE    PFKVLX              NO                                           
*                                                                               
         TM    BCINDS2,BCIACTCP    ACTION COMPLETED?                            
         JZ    PFKVLX              NO - IGNORE PFKEY                            
         MVC   BCPFKEY,CSNEXTPF    USE NEXT TIME PFKEY                          
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
*                                                                               
PFKVL02  GOTOX AGEN,RTPARM,OPFK,PFLST,(BCPFKEY,BPAPFK)                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCPFKEY          GET PFKEY PRESSED                            
         BCTR  RF,0                ZERO BASE IT                                 
         MHI   RF,FRPLNQ           MULTIPLY BY LENGTH OF ELEMENT                
         LA    RF,BPAPFK(RF)       INDEX INTO TABLE OF FRPELS                   
*                                                                               
         CLI   0(RF),FRPELQ        PFKEY ELEMENT?                               
         JNE   PFKVL26             NO                                           
         MVC   GSFRPEL,0(RF)       SAVE PFKEY ELEMENT                           
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTRAC   RECORD ACTION?                           
         JE    PFKVL04                                                          
         CLI   GSFRP.FRPTYPE,FRPTRACN  RECORD ACTION WITH NTRSES?               
         JE    PFKVL04                                                          
         CLI   GSFRP.FRPTYPE,FRPTRACS  REC/ACT WITH NTRSES FOR SELECT?          
         JE    PFKVL04                                                          
         CLI   GSFRP.FRPTYPE,FRPTPOSN  POSITIONAL ON LIST                       
         JE    PFKVL04                                                          
         J     PFKVL06                                                          
*                                                                               
PFKVL04  MVC   RTHALF1,GSFRP.FRPRECA  RTHALF1=PFKEY RECORD/ACTION               
         CLI   GSFRP.FRPREC,R#ALL     RECORD FOR ALL RECORDS?                   
         JNE   *+10                   NO                                        
         MVC   RTHALF1(L'CSREC),CSREC SET CURRENT RECORD                        
         CLI   GSFRP.FRPACT,A#ALL     ACTION FOR ALL RECORDS                    
         JNE   *+10                   NO                                        
         MVC   RTHALF1+L'CSREC(L'CSACT),CSACT                                   
*                                                                               
PFKVL06  CLI   GSFRP.FRPTYPE,FRPTAPPL  TEST PFKEY FOR APPLICATION USE           
         JE    PFKVLX                                                           
         CLI   GSFRP.FRPTYPE,FRPTAPA   TEST PFKEY FOR APPLICATION USE           
         JE    PFKVLX                                                           
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTRAC   RECORD ACTION?                           
         JE    PFKVL08                                                          
         CLI   GSFRP.FRPTYPE,FRPTRACS  REC/ACT WITH NTRSES FOR SELECT?          
         JE    PFKVL10                                                          
         CLI   GSFRP.FRPTYPE,FRPTRACN  RECORD ACTION WITH NTRSES?               
         JE    PFKVL12                                                          
         J     PFKVL16                                                          
*                                                                               
PFKVL08  OI    BCINDS1,BCINREC+BCINACT SET NEW RECORD ACTION                    
         MVC   CSNEXTPF,GSFRP.FRPNEXT  SET NEXT TIME AUTO PFKEY                 
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTSET,(GSFRP.FRPREC,0),0                      
         GOTOX AGEN,RTPARM,ORTYPE,RTDIS                                         
*                                                                               
         GOTOX AGEN,RTPARM,OACT,ASET,(GSFRP.FRPACT,0),0                         
         GOTOX AGEN,RTPARM,OACT,ADIS                                            
         J     PFKVLX                                                           
*                                                                               
N        USING SSAVD,NSSAV                                                      
PFKVL10  OI    N.SNINDS1,SNISEL    NTRSES FOR SELECT TYPE                       
         MVC   N.SRECACT,RTHALF1                                                
         J     PFKVL14                                                          
*                                                                               
PFKVL12  MVC   N.SRECACT,RTHALF1                                                
         CLI   RTHALF1,O#DIS       FIDDLE FOR COLUMN/LIST                       
         JE    *+12                                                             
         OI    N.SNINDS1,SNIUSECR                                               
         OI    N.SXINDS1,SXIREPRC                                               
*                                                                               
         CLC   CSREC,N.SREC        GOING TO A DIFFERENT RECORD TYPE?            
         JE    *+8                 NO                                           
         NI    N.SNINDS1,FF-SNIUSECR CAN'T HAVE SAME RECORD THEN                
*                                                                               
PFKVL14  GOTOX AGEN,RTPARM,OSES,SNTR,RTHALF1,0                                  
         DC    H'0'                YOU'RE FUCKED IF YOU GET HERE                
         DROP  N                                                                
*                                                                               
PFKVL16  CLI   GSFRP.FRPTYPE,FRPTALT  ALTERNATE PFKEY DISPLAY?                  
         JNE   PFKVL18             NO                                           
*                                                                               
         XI    TWAINDS3,TWAIPFKD   SWAP INDICATOR BIT (OLD STYLE)               
         OI    FVCURIND,FVCKEEP    KEEP CURSOR WHERE IT IS                      
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         J     EXITL                                                            
*                                                                               
PFKVL18  CLI   GSFRP.FRPTYPE,FRPTQPL  QUIT PREVIOUS LEVEL?                      
         JE    PFKVL20                                                          
         CLI   GSFRP.FRPTYPE,FRPTQPN  QUIT & PROCESS NEXT?                      
         JE    PFKVL20                                                          
         J     PFKVL22                                                          
*                                                                               
PFKVL20  GOTOX AGEN,RTPARM,OSES,SXIT                                            
         DC    H'0'                AGAIN, SHOULD NEVER GO HERE                  
*                                                                               
PFKVL22  CLI   GSFRP.FRPTYPE,FRPTSCRL  SCROLL PFKEY?                            
         JNE   PFKVL24                                                          
         J     PFKVLX              DON'T DO MUCH IF IT'S A SCROLLER...          
*                                                                               
PFKVL24  CLI   GSFRP.FRPTYPE,FRPTPOSN  POSITIONAL PFKEY                         
         JNE   PFKVLX              NO                                           
         GOTOX AGENLST,RTPARM,OLIST,LPFKPOS,RTHALF1                             
         J     EXIT                                                             
*                                                                               
PFKVL26  XR    R0,R0               SET INVALID PFKEY ERROR MESSAGE              
         IC    R0,BCPFKEY                                                       
         CVD   R0,GCDUB1                                                        
         OI    GCDUB1+7,X'0F'                                                   
         UNPK  FVXTRA(2),GCDUB1                                                 
         CLI   FVXTRA,C'0'                                                      
         JH    *+14                                                             
         MVC   FVXTRA(1),FVXTRA+1                                               
         MVI   FVXTRA+1,C' '                                                    
         MVI   FVOMTYP,GTMERR      EXIT WITH 'PF&T UNDEFINED' MESSAGE           
         MVC   FVMSGNO,=AL2(GE$PFUND)                                           
         OI    FVCURIND,FVCKEEP                                                 
         J     EXITH                                                            
*                                                                               
PFKVLX   J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD PFKEY LINE FOR DISPLAY ON SCREEN                         *  3 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PFKBLD   LA    R2,TWAPOLY          LOCATE PFKEY DISPLAY LINE IN TWA             
         USING FHD,R2                                                           
         LH    RF,GSDSPMAX                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                RF=A(LAST POSSIBLE BYTE IN TWA)              
         XR    RE,RE                                                            
*                                                                               
PFKBD02  TM    FHAT,FHATXH                                                      
         JZ    PFKBD04                                                          
         IC    RE,FHLN                                                          
         LA    R1,FHD(RE)                                                       
         AHI   R1,-(FHDAD)         R1=A(EXTENDED FIELD HEADER)                  
         CLI   0(R1),FD#PFK                                                     
         JE    PFKBD06                                                          
*                                                                               
PFKBD04  ICM   RE,1,FHLN                                                        
         JZ    PFBDX                                                            
         BRXLE R2,RE,PFKBD02                                                    
         J     PFBDX                                                            
*                                                                               
PFKBD06  ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD+FHDAD+1)                                              
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         OI    FHAT,FHATHI                                                      
         OI    FHOI,FHOITR                                                      
         ST    RE,BPLENLIN                                                      
         DROP  R2                                                               
*                                                                               
         LA    R2,L'FVIHDR(R2)     BUMP TO PFKEY FIELD                          
         ST    R2,BCFULL           SAVE A(OUTPUT FIELD)                         
         MVI   BOELEM,C' '         CLEAR WORK AREA TO SPACES                    
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
*                                                                               
         GOTOX AGEN,RTPARM,OPFK,PFLST,(0,BPAPFK)                                
*                                                                               
         LA    R0,MAXPFKS          24 PFKEYS MAX                                
         LA    R4,BPAPFK           A(FORMATTED FRPELS)                          
         USING FRPELD,R4                                                        
         XC    BPAMORE,BPAMORE     SAVE AREA FOR 'MORE' PFKEY                   
*                                                                               
PFKBD08  CLI   0(R4),FRPELQ        ITERATE LIST OF PFKEYS                       
         JNE   PFKBD10                                                          
         CLI   FRPTYPE,FRPTALT     TEST ALTPFS KEY                              
         JE    PFKBD12                                                          
*                                                                               
PFKBD10  LA    R4,FRPLNQ(R4)       BUMP TO NEXT ELEMENT                         
         BRCT  R0,PFKBD08                                                       
         J     PFKBD14             NO 'MORE' PFKEY SET                          
*                                                                               
PFKBD12  LA    R2,BPAMORE                                                       
         XR    R1,R1               OUTPUT PFKEY NUMBER                          
         ICM   R1,1,FRPPFK#                                                     
         CVD   R1,GCDUB1                                                        
         OI    GCDUB1+L'GCDUB1-1,X'0F'                                          
         UNPK  0(2,R2),GCDUB1                                                   
         CLI   0(R2),C'0'          TEST LEADING ZERO                            
         JE    *+12                                                             
         LA    R2,2(R2)                                                         
         J     *+14                                                             
         MVC   0(2,R2),1(R2)       SUPPRESS LEADING ZERO                        
         LA    R2,1(R2)                                                         
         MVC   0(L'BCEQUAL,R2),BCEQUAL  MOVE IN '=' SIGN                        
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),DD#ESCL       LEFT ALIGNED, LENGTH 8 FOR NAME              
         MVC   1(L'FRPDICT,R2),FRPDICT                                          
         MVI   3(R2),8                                                          
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),(R2)   TRANSLATE NAME                        
*                                                                               
PFKBD14  LA    R2,BOELEM           POINT TO WORK AREA                           
         MVI   BCBYTE1,0           INITIALISE FLAG BYTE                         
*                                                                               
         LA    R0,MAXPFKS          24 PFKEYS MAXIMUM                            
         LH    R4,LSPFKLST         PFKEY NUMBER STARTED FROM LAST TIME          
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTALT  WANT NEXT LOT?                            
         JNE   *+14                NO                                           
         LH    R4,LSPFKNOW         PFKEY NUMBER REACHED LAST TIME               
         MVC   LSPFKLST,LSPFKNOW   RESET ONE TO START FROM                      
*                                                                               
         SR    R0,R4               NUMBER OF TIMES TO LOOP                      
         SH    R4,=H'1'            MAKE START ZERO BASED                        
         BNM   *+6                                                              
         XR    R4,R4                                                            
         MHI   R4,FRPLNQ           INDEX INTO LIST OF PFKEYS                    
         LA    R4,BPAPFK(R4)                                                    
         XC    PFXREC,PFXREC       RESET LAST RECORD                            
*                                                                               
PFKBD16  CLI   0(R4),FRPELQ        ITERATE LIST                                 
         JNE   PFKBD48             DONE                                         
         USING FRPELD,R4                                                        
         MVI   PFLINDS,0           RESET INDICATORS                             
         XC    PFLREC,PFLREC       RESET PFKEY TEXT BUFFERS                     
         XC    PFLACT,PFLACT                                                    
         XC    PFLUSER,PFLUSER                                                  
*                                                                               
         OC    CSNEXTPF,CSNEXTPF   TEST AUTO PF KEY SET                         
         JZ    *+14                                                             
         CLC   FRPNEXT,CSNEXTPF    IF SO THEN DON'T DISPLAY IT                  
         JE    PFKBD48                                                          
*                                                                               
         CLI   FRPTYPE,FRPTALT     TEST ALTPFS KEY                              
         JE    PFKBD48             YES - WE HAVE IT ALREADY                     
*                                                                               
PFKBD18  CLI   FRPTYPE,FRPTSRCH    SEARCH?                                      
         JE    PFKBD22                                                          
         CLI   FRPTYPE,FRPTAPA     TEST FOR KNOWN APPLICATION ACTION            
         JE    PFKBD22                                                          
         CLI   FRPTYPE,FRPTAPPL    TEST FOR APPLICATION USE                     
         JE    PFKBD22                                                          
*                                                                               
*??      JNE   PFKBD20                                                          
*??      CLC   PFKSPROG,CSSPROG    MATCH ON OVERLAY SUB-PROGRAM                 
*??      JNE   PFKBD48             THIS IS FOR CBILL IF IT EVER RUNS            
*??      J     PFKBD22             UNDER NEW FILE                               
*                                                                               
PFKBD20  CLI   FRPTYPE,FRPTQPL     TEST QUIT PFKEY                              
         JNE   *+16                                                             
         CLI   TWASESNL,1          TEST NESTED                                  
         JE    PFKBD48             NO - IGNORE IT THEN                          
         J     PFKBD22                                                          
*                                                                               
         CLI   FRPTYPE,FRPTQPN     TEST NEXT PFKEY                              
         JNE   *+16                                                             
         CLI   TWASESNL,1          TEST NESTED                                  
         JE    PFKBD48             NO - IGNORE IT THEN                          
         J     PFKBD22                                                          
*                                                                               
PFKBD22  XC    FVIFLD,FVIFLD       CALL USER FOR RECORD                         
         GOTOX AGEN,RTPARM,('GCBOVER',OPFK),PFREC,FRPPFK#,FVIFLD,      *        
               FRPELD                                                           
         TM    RTPARM+8,X'80'      CONVENTION FOR IGNORE THIS                   
         JZ    *+8                                                              
         OI    PFLINDS,PFLXREC     RECORD NOT WANTED                            
*                                                                               
         CLC   FVIFLD,BCSPACES     DID USER SET RECORD?                         
         BNH   PFKBD24             NO                                           
*                                                                               
         MVC   PFLREC,FVIFLD       SAVE USER RECORD NAME                        
         OI    PFLINDS,PFLIREC     FLAG RECORD SET BY USER                      
         J     PFKBD26                                                          
*                                                                               
PFKBD24  MVC   BPKRECN,FRPREC      STANDARD RECORD HANDLING                     
         CLI   BPKRECN,0                                                        
         JNE   *+10                                                             
         MVC   BPKRECN,CSREC       NO RECORD SET, USE CURRENT RECORD            
*                                                                               
         ICM   RF,8,GCOVSYS        VERB DISPLAYS REQUESTED RECORD               
         ICM   RF,4,GCPRGNO        INTO FVIFLD, SO YOU DON'T HAVE TO            
         ICM   RF,2,BPKRECN                                                     
         GOTOX AGEN,RTPARM,ORTYPE,RTXDIS,(RF)                                   
         JE    *+6                                                              
         DC    H'0'                UNKNOWN RECORD NUMBER                        
*                                                                               
         MVC   PFLREC,FVIFLD       SAVE OFF RECORD NAME                         
*                                                                               
PFKBD26  OC    FRPACT,FRPACT       IS THERE AN ASSOCIATED ACTION                
         JZ    PFKBD30             NO                                           
*                                                                               
         XC    FVIFLD,FVIFLD       CALL USER FOR ACTION                         
         GOTOX AGEN,RTPARM,('GCBOVER',OPFK),PFACT,FRPPFK#,FVIFLD,      *        
               FRPELD                                                           
         TM    RTPARM+8,X'80'      CONVENTION FOR IGNORE THIS                   
         JZ    *+12                                                             
         OI    PFLINDS,PFLXACT     ACTION NOT WANTED                            
         J     PFKBD34                                                          
*                                                                               
         CLC   FVIFLD,BCSPACES     DID USER SET ACTION?                         
         BNH   PFKBD28             NO                                           
*                                                                               
         MVC   PFLACT,FVIFLD       SAVE USER ACTION NAME                        
         OI    PFLINDS,PFLIACT     ACTION SET BY USER                           
         J     PFKBD34                                                          
*                                                                               
PFKBD28  ICM   RF,8,GCOVSYS        STANDARD ACTION HANDLING                     
         ICM   RF,4,GCPRGNO                                                     
         ICM   RF,2,BPKRECN                                                     
         ICM   RF,1,FRPACT                                                      
         GOTOX AGEN,RTPARM,OACT,AXDIS,(RF)                                      
         JE    *+6                                                              
         DC    H'0'                UNKNOWN ACTION NUMBER                        
*                                                                               
         MVC   PFLACT,FVIFLD       SAVE OFF ACTION NAME                         
         J     PFKBD34                                                          
*                                                                               
PFKBD30  XC    FVIFLD,FVIFLD       NOT ACTION, MUST JE USER DEFINED             
         GOTOX AGEN,RTPARM,('GCBOVER',OPFK),PFUSER,FRPPFK#,FVIFLD,     *        
               FRPELD                                                           
         TM    RTPARM+8,X'80'                                                   
         JZ    *+12                SUPPRESS USER DEFINED - IGNORE PFKEY         
         OI    PFLINDS,PFLXUSR                                                  
         J     PFKBD34                                                          
*                                                                               
         CLC   FVIFLD,BCSPACES     DID USER SET USER DEFINED WORD?              
         BNH   PFKBD32             NO                                           
*                                                                               
         MVC   PFLUSER,FVIFLD                                                   
         OI    PFLINDS,PFLIUSR                                                  
         J     PFKBD34                                                          
*                                                                               
PFKBD32  MVI   PFLUSER,DD#ESCL     STANDARD PROCESSING FOR USER WORD            
         MVC   PFLUSER+1(L'FRPDICT),FRPDICT                                     
         MVI   PFLUSER+3,8                                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTO1 VDICTAT,RTPARM,(RF),PFLUSER TRANSLATE USER-DEFINED               
         J     PFKBD34                                                          
         SPACE 1                                                                
***********************************************************************         
* THE RULES FOR IGNORING PFKEYS ARE AS FOLLOWS:                       *         
* 1. USER DEFINED PFKEYS  - 'SUPPRESS USER DEFINED ON'                *         
* 2. RECORD ACTION PFKEYS - 'SUPPRESS RECORD ON' &&                   *         
*                           'SUPPRESS ACTION ON'                      *         
***********************************************************************         
         SPACE 1                                                                
PFKBD34  TM    PFLINDS,PFLXUSR     IGNORE IF SUPPRESS USER-DEFINED              
         JO    PFKBD48                                                          
         TM    PFLINDS,PFLXREC+PFLXACT IGNORE IF SUPPRESS REC & ACT             
         JO    PFKBD48                                                          
*                                                                               
         CLI   BCBYTE1,1           TEST IF PF PREFIX OUTPUT YET                 
         JNE   *+16                                                             
         MVI   0(R2),C','          MOVE IN A ',' THEN                           
         LA    R2,1(R2)                                                         
         J     PFKBD36                                                          
*                                                                               
         MVI   BCBYTE1,1           SET PREFIX OUTPUT FLAG                       
         MVI   0(R2),DD#ESCL2                                                   
         MVC   1(2,R2),=AL2(GE#PFK)                                             
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,=AL1(QSCON)                                                 
         GOTOX VDICTAT,RTPARM,(RF),(R2)                                         
         LA    R2,2(R2)                                                         
*                                                                               
PFKBD36  XR    R1,R1               OUTPUT PFKEY NUMBER                          
         ICM   R1,1,FRPPFK#                                                     
         CVD   R1,GCDUB1                                                        
         OI    GCDUB1+L'GCDUB1-1,X'0F'                                          
         UNPK  0(2,R2),GCDUB1                                                   
         CLI   0(R2),C'0'          TEST LEADING ZERO                            
         JE    *+12                                                             
         LA    R2,2(R2)                                                         
         J     *+14                                                             
         MVC   0(2,R2),1(R2)       SUPPRESS LEADING 0                           
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(L'BCEQUAL,R2),BCEQUAL                                          
         LA    R2,L'BCEQUAL(R2)    MOVE IN EQUALS SIGN                          
*                                                                               
         TM    PFLINDS,PFLIUSR     USER SET - IGNORE RECORD PORTION             
         JZ    PFKBD38                                                          
         MVC   0(L'PFLUSER,R2),PFLUSER                                          
         LA    R2,L'PFLUSER-1(R2)                                               
*                                                                               
         CLI   0(R2),C' '          STRIP OFF TRAILING SPACES                    
         JH    *+8                                                              
         BRCT  R2,*-8                                                           
         LA    R2,1(R2)                                                         
         J     PFKBD46                                                          
*                                                                               
PFKBD38  TM    PFLINDS,PFLXREC     SUPPRESS RECORD FIELD                        
         JO    PFKBD42                                                          
         CLC   PFLREC,PFXREC       SAME AS LAST TIME?                           
         JE    PFKBD40             YES - DON'T BOTHER TO DISPLAY THEN           
*                                                                               
         MVC   PFXREC,PFLREC       SAVE RECORD                                  
         MVC   0(L'PFLREC,R2),PFLREC                                            
         LA    R2,L'PFLREC-1(R2)                                                
*                                                                               
         CLI   0(R2),C' '          STRIP OFF TRAILING SPACES                    
         JH    *+8                                                              
         BRCT  R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
PFKBD40  OC    FRPACT,FRPACT       ACTION SET?                                  
         JZ    *+12                                                             
         TM    PFLINDS,PFLXACT     SUPPRESS ACTION COMPLETELY?                  
         JO    PFKBD46             DON'T PUT IN SLASH THEN                      
*                                                                               
         MVC   0(L'BCSLASH,R2),BCSLASH                                          
         LA    R2,L'BCSLASH(R2)    MOVE IN SLASH AFTER RECORD                   
*                                                                               
PFKBD42  OC    FRPACT,FRPACT       ACTION SET?                                  
         JNZ   PFKBD44                                                          
         MVC   0(L'PFLUSER,R2),PFLUSER                                          
         LA    R2,L'PFLUSER-1(R2)                                               
*                                                                               
         CLI   0(R2),C' '          STRIP OFF TRAILING SPACES                    
         JH    *+8                                                              
         BRCT  R2,*-8                                                           
         LA    R2,1(R2)                                                         
         J     PFKBD46                                                          
*                                                                               
PFKBD44  MVC   0(L'PFLACT,R2),PFLACT  MOVE IN ACTION                            
         LA    R2,L'PFLACT-1(R2)                                                
*                                                                               
         CLI   0(R2),C' '          STRIP OFF TRAILING SPACES                    
         JH    *+8                                                              
         BRCT  R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
PFKBD46  LR    RF,R2               CURRENT DISPLACEMENT INTO AREA               
         LA    RE,BOELEM           START OF BUILD AREA                          
         SR    RF,RE               LENGTH OF DATA TO GO ON LINE                 
         C     RF,BPLENLIN                                                      
         JH    PFKBD50             NO MORE PFKEYS WILL FIT ON LINE              
*                                                                               
PFKBD48  LA    R4,FRPLNQ(R4)       BUMP TO NEXT ELEMENT                         
         BRCT  R0,PFKBD16          ** LOOP **                                   
         OI    PFLINDS,PFLALL      SET DID ALL PFKEYS                           
*                                                                               
PFKBD50  LA    R2,BOELEM           GO TO END OF LINE                            
         A     R2,BPLENLIN                                                      
         CLC   BPAMORE,BCSPACES    WAS THERE A 'MORE' PFKEY?                    
         BNH   *+8                 NO                                           
         AHI   R2,-(L'BPAMORE)     ALLOW SPACE FOR IT                           
         L     R0,BPLENLIN                                                      
*                                                                               
         BCTR  R2,0                LOOK FOR LAST COMMA ON LINE                  
         CLI   0(R2),C','                                                       
         JE    PFKBD52                                                          
         BRCT  R0,*-10             TEST WHOLE LINE ONLY                         
         XC    LSPFKNOW,LSPFKNOW   RESET THESE INDICATORS                       
         XC    LSPFKLST,LSPFKLST                                                
         J     PFKBD56             NO COMMA ON THIS LINE                        
*                                                                               
PFKBD52  CLI   1(R2),C'0'          CHECK PFKEY LINE INTEGRITY                   
         BNL   *+6                                                              
         DC    H'0'                R2 OUGHT TO JE ,N(N)=XXXXXXXX                
*                                                                               
         XR    RF,RF               GET NUMBER OF NUMERIC CHARACTERS-1           
         CLI   2(R2),C'0'          SECOND CHARACTER IS A NUMBER?                
         JL    *+8                 NO                                           
         LA    RF,1                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         PACK  GCDUB1,1(0,R2)      EVALUATE LAST DISPLAYED LINE NUMBER          
         CVB   RF,GCDUB1                                                        
         STH   RF,LSPFKNOW         THIS IS THE LAST ONE PROCESSED               
*                                                                               
         LA    RF,BOELEM           POINT TO END OF LINE                         
         A     RF,BPLENLIN                                                      
         CLC   BPAMORE,BCSPACES    WAS THERE A 'MORE' PFKEY?                    
         BNH   *+8                 NO                                           
         AHI   RF,-(L'BPAMORE)     ALLOW ROOM FOR IT TO FIT                     
         BCTR  RF,0                RF = LAST CHARACTER DISPLAYABLE              
*                                                                               
         CLC   0(L'BPAMORE+1,RF),BCSPACES WILL LINE FIT NORMALLY?               
         JE    PFKBD54             YES - GOOD                                   
*                                                                               
*                              *** LAST PFKEY WILL NOT FIT ON SCREEN            
         MVC   0(40,R2),BCSPACES   R2 POINTS TO LAST COMMA                      
         CLC   BPAMORE,BCSPACES    WAS THERE A 'MORE' PFKEY?                    
         BNH   PFKBD56             NO - DISPLAY FIELD ON SCREEN                 
*                                                                               
         LA    R2,BOELEM           MOVE OUT 'MORE' PFKEY                        
         A     R2,BPLENLIN                                                      
         AHI   R2,-(L'BPAMORE)                                                  
         MVC   0(L'BPAMORE,R2),BPAMORE                                          
         J     PFKBD56             DISPLAY FIELD ON SCREEN                      
*                                                                               
*                              *** NO NEED TO REMOVE LAST PFKEY                 
PFKBD54  TM    PFLINDS,PFLALL      DID WE DO ALL PFKEYS?                        
         JZ    *+14                                                             
         XC    LSPFKNOW,LSPFKNOW   RESET START NUMBER                           
*                                                                               
         CLC   BPAMORE,BCSPACES    WAS THERE A 'MORE' PFKEY?                    
         BNH   PFKBD56             NO - DISPLAY FIELD ON SCREEN                 
*                                                                               
         LA    R2,BOELEM           MOVE OUT 'MORE' PFKEY                        
         A     R2,BPLENLIN                                                      
         AHI   R2,-(L'BPAMORE)                                                  
         MVC   0(L'BPAMORE,R2),BPAMORE                                          
         J     PFKBD56             DISPLAY FIELD ON SCREEN                      
*                                                                               
PFKBD56  L     R1,BCFULL           MOVE BUILT LINE TO TWA FIELD                 
         L     RF,BPLENLIN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM                                                   
*                                                                               
PFBDX    J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CALL USER TO OUTPUT USER DEFINED TEXT FOR PFKEY                *  4 *         
*                                                                ******         
* EXIT: FVIFLD CONTAINS TEXT USER WISHES TO DISPLAY                   *         
* EXIT: 8(R1) - X'80' BIT ON - SUPPRESS ANY DISPLAY FOR THIS TEXT     *         
***********************************************************************         
         SPACE 1                                                                
PFKUSER  J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CALL USER TO OUTPUT RECORD NAME FOR PFKEY                      *  5 *         
*                                                                ******         
* EXIT: FVIFLD CONTAINS TEXT USER WISHES TO DISPLAY                   *         
* EXIT: 8(R1) - X'80' BIT ON - SUPPRESS ANY DISPLAY FOR THIS TEXT     *         
***********************************************************************         
         SPACE 1                                                                
PFKREC   J     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CALL USER TO OUTPUT ACTION NAME FOR PFKEY                      *  6 *         
*                                                                ******         
* EXIT: FVIFLD CONTAINS TEXT USER WISHES TO DISPLAY                   *         
* EXIT: 8(R1) - X'80' BIT ON - SUPPRESS ANY DISPLAY FOR THIS TEXT     *         
***********************************************************************         
         SPACE 1                                                                
PFKACT   J     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PAGE OBJECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PAG      LR    R7,RF                                                            
         L     R1,RTPARMS2                                                      
         LA    RF,PAGTAB                                                        
         J     ITER                                                             
*                                                                               
PAGTAB   DS    0X                                                               
         DC    AL1(PGDIS,0,0,0),AL4(PAGDIS)                                     
         DC    AL1(PGVAL,0,0,0),AL4(PVAL)                                       
         DC    AL1(PGCOUNT,0,0,0),AL4(PAGCNT)                                   
         DC    AL1(PGDISALF,0,0,0),AL4(PDALF)                                   
         DC    AL1(PGVALALF,0,0,0),AL4(PVALF)                                   
PAGTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PAGE DISPLAY                                                   *  1 *         
*                                                                ******         
* SUPPRESSES PAGE FIELD IF NO INPUT RELEVANT                          *         
***********************************************************************         
         SPACE 1                                                                
PAGDIS   XR    R2,R2                                                            
         ICM   R2,3,GSDSPPAG       IS THERE A PAGE FIELD ON SCREEN?             
         JZ    PAGDISX             NO - CAN'T DO MUCH THEN...                   
*                                                                               
         A     R2,ATWA                                                          
         XC    RTFULL,RTFULL       USED TO SAVE A(PAGE TAG FIELD)               
         USING FHD,R2                                                           
*                                                                               
         LA    RF,TWASCR           START OF SCREEN                              
TAG      USING FHD,RF                                                           
         XR    RE,RE                                                            
*                                                                               
PDIS02   CR    RF,R2               REACHED 'PAGE' INPUT FIELD YET?              
         BNL   PDIS04              YES                                          
         LR    R0,RF               SAVE A(THIS FIELD)                           
         IC    RE,TAG.FHLN                                                      
         LA    RF,0(RE,RF)                                                      
         J     PDIS02                                                           
*                                                                               
PDIS04   LR    RF,R0               R0=A(FIELD IMMEDIATELY BEFORE INPUT)         
         TM    TAG.FHAT,FHATPR                                                  
         JZ    PDIS06              NOT PAGE TAG FIELD IF NOT PROTECTED          
*                                                                               
         ST    RF,RTFULL           SAVE A(PAGE TAG FIELD)                       
         NI    TAG.FHAT,FF-FHATLO  SHOW IT                                      
         OI    TAG.FHOI,FHOITR     TRANSMIT IT                                  
         DROP  TAG                                                              
*                                                                               
PDIS06   NI    FHAT,FF-(FHATPR+FHATLO) UNPROT AND DISP INPUT FIELD              
         OI    FHOI,FHOITR         TRANSMIT IT                                  
         MVC   FVIFLD,BCSPACES                                                  
*                                                                               
         GOTOX AGEN,RTPARM,OPAGE,PGDISALF                                       
*                                  ALLOW PROGRAM TO SET PAGE NAME               
         CLC   FVIFLD,BCSPACES     NAME SET BY PROGRAM?                         
         JH    PDIS10              NO                                           
         OC    GSPGNAM,GSPGNAM     DD REF SET FOR AUTOTRANSLATE                 
         JZ    PDIS08              NO                                           
*                                                                               
         MVC   FVIFLD(L'GSPGNAM),GSPGNAM                                        
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTO1 VDICTAT,RTPARM,(RF),FVIFLD                                       
         J     PDIS10                                                           
*                                                                               
PDIS08   LA    R1,GSSMPAGE         EDIT OUT PAGE NUMBER                         
         LA    R3,FVIFLD           TEMP BUILD AREA                              
         BAS   RE,PAGEDT                                                        
         MVC   0(1,R3),BCSLASH                                                  
         LA    R3,1(R3)                                                         
         LA    R1,GS#PAGE                                                       
         BAS   RE,PAGEDT                                                        
*                                                                               
         CLI   GS#PAGE,1          ONE PAGE SET?                                 
         JH    PDIS10             MORE THAN 1 PAGE SET                          
*                                                                               
         TM    GCINDS3,GCIPNORM   LEAVE FIELD ALONE?                            
         JO    PDIS10             YES                                           
*                                                                               
TAG      USING FHD,RF                                                           
         ICM   RF,15,RTFULL       GET BACK A(PAGE TAG FIELD)                    
         JZ    *+16                                                             
         OI    TAG.FHAT,FHATLO    HIDE TAG FIELD                                
         OI    TAG.FHOI,FHOITR    TRANSMIT IT                                   
*                                                                               
         OI    FHAT,FHATPR+FHATLO PROTECT AND HIDE INPUT FIELD                  
         OI    FHOI,FHOITR        TRANSMIT IT                                   
         OI    FHII,FHIIVA        SET VALIDATED                                 
         J     PAGDISX                                                          
         DROP  TAG                                                              
*                                                                               
PDIS10   XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AHI   RF,-(FHDAD+FHDAD+1)                                              
         TM    FHAT,FHATXH                                                      
         JO    *+8                                                              
         LA    RF,FHDAD(RF)                                                     
         EX    RF,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA                                                      
*                                                                               
PAGDISX  J     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
PAGEDT   XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         CVD   RF,RTDUB1                                                        
         OI    RTDUB1+7,X'0F'                                                   
         UNPK  0(3,R3),RTDUB1                                                   
*                                                                               
         CLC   =C'00',0(R3)        TWO LEADING ZEROS?                           
         JNE   PDT02                                                            
         MVC   0(2,R3),2(R3)                                                    
         MVC   1(3,R3),BCSPACES                                                 
         LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
PDT02    CLI   0(R3),C'0'          ONE LEADING ZERO?                            
         JNE   PDT04                                                            
         MVC   0(2,R3),1(R3)                                                    
         MVI   2(R3),C' '                                                       
         LA    R3,2(R3)                                                         
         BR    RE                                                               
*                                                                               
PDT04    LA    R3,3(R3)            NO LEADING ZEROS                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PAGE VALIDATE                                                  *  2 *         
*                                                                ******         
* EXIT: CC = EQUAL IF PAGE CHANGED                                    *         
*          = HIGH IF PAGE UNCHANGED                                   *         
*          = LOW IF ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
PVAL     XR    R2,R2                                                            
         ICM   R2,3,GSDSPPAG       IS THERE A PAGE FIELD ON SCREEN?             
         JZ    PVALH               NO                                           
*                                                                               
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         TM    FHII,FHIIVA         HAS FIELD BEEN INPUT?                        
         JO    PVALH               NO                                           
         TM    FHAT,FHATPR         IS FIELD PROTECTD?                           
         JO    PVALH               YES                                          
         NI    FHAT,FF-(FHATPR+FHATLO)                                          
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         JH    PVALL               ERROR ON FVAL CALL                           
         JL    PVALOK              NO INPUT INTO FIELD                          
*                                                                               
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
         CLI   0(RF),C'/'          PARSE FIELD FOR DIVIDER                      
         JE    PVAL02                                                           
         LA    RF,1(RF)                                                         
         BRCT  R0,*-12                                                          
         J     PVAL04              NO DIVIDER                                   
*                                                                               
PVAL02   MVC   0(8,RF),BCSPACES    CLEAR DIVIDER & ALL AFTER IT                 
         IC    R0,FVILEN           REVALIDATE THIS INPUT                        
         GOTOX ('FLDVAL',AGROUTS),0                                             
*                                                                               
PVAL04   TM    FVIIND,FVINUM       TEST INPUT NUMERICAL                         
         JZ    PVAL06              NO                                           
         ICM   RE,15,BCFULL        RE=NUMBER INPUT                              
         JZ    PVALL                                                            
         XR    RF,RF                                                            
         IC    RF,GS#PAGE          TEST NUMBER IN VALID RANGE                   
         CR    RE,RF                                                            
         JH    PVALL                                                            
         CLM   RE,1,GSSMPAGE       TEST NUMBER CHANGED                          
         JE    PVALH               NO                                           
         STC   RE,GSSMPAGE         SAVE CHANGED NUMBER                          
         J     PVALOK              REDISPLAY SCREEN                             
*                                                                               
PVAL06   GOTOX AGEN,RTPARM,OPAGE,PGVALALF                                       
         JL    PVALL               INVALID ALPHA CHARACTERS                     
         JE    PVALOK              SCROLLING OCCURRED                           
         JH    PVALH               NOTHING HAPPENED                             
*                                                                               
PVALOK   GOTOX AGEN,RTPARM,OPAGE,PGDIS                                          
         J     EXITOK                                                           
*                                                                               
PVALH    GOTOX AGEN,RTPARM,OPAGE,PGDIS                                          
         J     EXITH                                                            
*                                                                               
PVALL    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXITL                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* COUNT NUMBER OF PAGES SET FOR THIS APPLICATION                 *  3 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PAGCNT   MVI   GS#PAGE,1           DEFAULT NUMBER OF SCREENS                    
         CLI   CSACT,A#LST         LIST HAS ONLY 1 SCREEN                       
         JE    EXITOK                                                           
         CLI   CSACT,A#DLOAD       DOWNLOAD HAS ONLY 1 SCREEN                   
         JE    EXITOK                                                           
         CLI   CSACT,A#REPT        REPORT HAS ONLY 1 SCREEN                     
         JE    EXITOK                                                           
*                                                                               
         PUSH  USING                                                            
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON) SWITCH TO CONTROL SYSTEM          
         OC    ASTEST,ASTEST                                                    
         JZ    PCNT02                                                           
*                                                                               
         USING FSRRECD,IOKEY                                                    
         XC    FSRKEY,FSRKEY       READ SCREEN FOR CONNECTED COUNTRY            
         MVI   FSRKMIN,FSRKMINQ    AND TEST PHASE                               
         MVI   FSRKTYP,FSRKTYPQ                                                 
         MVC   FSRKSYS,GCOVSYS                                                  
         MVC   FSRKPRG,GCPRGNO                                                  
         MVC   FSRKREC,CSREC                                                    
         MVC   FSRKCODE,GSSMCODE                                                
         MVI   FSRKPAGE,1                                                       
         MVC   FSRKCTRY,CUCTRY                                                  
         XI    FSRKCTRY,FF                                                      
         MVI   FSRKSUB,FF                                                       
         MVC   FSRKTEST,ASTEST                                                  
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PCNT06                                                           
*                                                                               
PCNT02   XC    FSRKEY,FSRKEY       READ SCREEN FOR CONNECTED COUNTRY            
         MVI   FSRKMIN,FSRKMINQ    AND LIVE PHASE                               
         MVI   FSRKTYP,FSRKTYPQ                                                 
         MVC   FSRKSYS,GCOVSYS                                                  
         MVC   FSRKPRG,GCPRGNO                                                  
         MVC   FSRKREC,CSREC                                                    
         MVC   FSRKCODE,GSSMCODE                                                
         MVI   FSRKPAGE,1                                                       
         MVC   FSRKCTRY,CUCTRY                                                  
         XI    FSRKCTRY,FF                                                      
         MVI   FSRKSUB,FF                                                       
         XC    FSRKTEST,FSRKTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PCNT06                                                           
*                                                                               
         OC    ASTEST,ASTEST                                                    
         JZ    PCNT04                                                           
*                                                                               
         XC    FSRKEY,FSRKEY       READ SCREEN FOR HOST PROCESSOR               
         MVI   FSRKMIN,FSRKMINQ    AND TEST PHASE                               
         MVI   FSRKTYP,FSRKTYPQ                                                 
         MVC   FSRKSYS,GCOVSYS                                                  
         MVC   FSRKPRG,GCPRGNO                                                  
         MVC   FSRKREC,CSREC                                                    
         MVC   FSRKCODE,GSSMCODE                                                
         MVI   FSRKPAGE,1          FIRST DATA PAGE                              
         MVI   FSRKCTRY,FF                                                      
         MVI   FSRKSUB,FF                                                       
         MVC   FSRKTEST,ASTEST                                                  
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         JE    PCNT06                                                           
*                                                                               
PCNT04   XC    FSRKEY,FSRKEY       READ SCREEN FOR HOST PROCESSOR               
         MVI   FSRKMIN,FSRKMINQ    AND LIVE PHASE                               
         MVI   FSRKTYP,FSRKTYPQ                                                 
         MVC   FSRKSYS,GCOVSYS                                                  
         MVC   FSRKPRG,GCPRGNO                                                  
         MVC   FSRKREC,CSREC                                                    
         MVC   FSRKCODE,GSSMCODE                                                
         MVI   FSRKPAGE,1          FIRST DATA PAGE                              
         MVI   FSRKCTRY,FF                                                      
         MVI   FSRKSUB,FF                                                       
         MVI   FSRKCTRY,FF                                                      
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         JNE   PCNTX               NO PAGE 1 SET UP                             
*                                                                               
PCNT06   MVC   RTBYTE1,FSRKCTRY    SAVE COUNTRY CODE FOR COUNT                  
         MVC   RTBYTE2,FSRKSUB     SAVE SUB-COUNTRY CODE FOR COUNT              
         J     PCNT10                                                           
*                                                                               
PCNT08   L     R1,=AL4(XOSEQ+XOGENDIR)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         JNE   PCNTX                                                            
*                                                                               
         CLC   FSRKEY(FSRKPAGE-FSRKEY),IOKEYSAV                                 
         JNE   PCNTX                                                            
*                                                                               
         CLI   FSRKPAGE,FSRKPLST   IGNORE LIST,DOWNLOAD AND REPORT              
         JE    PCNTX               AS PART OF PAGE TOTAL                        
         CLI   FSRKPAGE,FSRKPDWN                                                
         JE    PCNTX                                                            
         CLI   FSRKPAGE,FSRKPREP                                                
         JE    PCNTX                                                            
*                                                                               
PCNT10   CLC   RTBYTE1,FSRKCTRY    PAGE IS FOR THIS COUNTRY?                    
         JNE   PCNT08              NO - IGNORE                                  
         CLC   RTBYTE2,FSRKSUB     PAGE IS FOR THIS SUB-COUNTRY?                
         JNE   PCNT08              NO - IGNORE                                  
*                                                                               
         OC    ASTEST,ASTEST       CONNECTED TO A TEST PHASE?                   
         JZ    PCNT12              NO                                           
         CLC   FSRKTEST,ASTEST     SCREEN FOR THIS TEST PHASE?                  
         JE    PCNT14              YES - INCLUDE                                
         OC    FSRKTEST,FSRKTEST   LIVE SCREEN?                                 
         JZ    PCNT14              YES - INCLUDE                                
         J     PCNT08              NO - IGNORE                                  
*                                                                               
PCNT12   OC    FSRKTEST,FSRKTEST   TEST PHASE SCREEN?                           
         JNZ   PCNT08              YES - IGNORE                                 
         J     PCNT14              KEEP ALL LIVE SCREENS                        
*                                                                               
PCNT14   MVC   GS#PAGE,FSRKPAGE    SAVE THIS HIGH PAGE                          
         J     PCNT08                                                           
*                                                                               
PCNTX    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ALPHANUMERIC INPUT FOR PAGE FIELD                      *  4 *         
*                                                                ******         
* PAGE NUMBER IS IN GSSMPAGE                                          *         
* APPLICATION MUST SET DATA INTO FVIFLD IF DISPLAY TO OCCUR           *         
* APPLICATION CAN SET 4 BYTE DD-REF INTO GSPGNAM FOR AUTO TRANSLATE   *         
***********************************************************************         
         SPACE 1                                                                
PDALF    GOTOX APRG,RTPARM,('GCBOVER',OPAGE),PGDISALF                           
         J     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC INPUT FOR PAGE FIELD                     *  5 *         
*                                                                ******         
* EXTRACTED INPUT IS IN FVIFLD ETC.                                   *         
* APPLICATION MUST SET GSSMPAGE - PAGE MUST EXIST ELSE INVALID ERROR  *         
***********************************************************************         
         SPACE 1                                                                
PVALF    GOTOX APRG,RTPARM,('GCBOVER',OPAGE),PGVALALF                           
         JL    EXITL                                                            
         JH    PVALF02             NOT DEFINED                                  
*                                                                               
         CLC   GSSMPAGE,GS#PAGE    PAGE NUMBER VALID?                           
         JH    EXITL               NO                                           
         J     EXITOK                                                           
*                                                                               
PVALF02  J     EXITOK              VALIDATION FOR NAME HERE                     
         SPACE 2                                                                
***********************************************************************         
* TABLES AND EQUATES                                                  *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
MAXPFKS  EQU   24                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
RTDUB1   DS    D                                                                
RTDUB2   DS    D                                                                
RTFULL   DS    F                                                                
RTBYTE1  DS    X                                                                
RTBYTE2  DS    X                                                                
RTMSK    DS    XL(L'GSRECMSK)                                                   
RTHALF1  DS    H                                                                
RTHALF2  DS    H                                                                
*                                                                               
*                                  ** PFKLST S/R LOCAL W/S **                   
PLNUMB   DS    XL1                 REQUIRED PFKEY NUMBER                        
PLKEYB   DS    0XL2                BEFORE RECORD/ACTION                         
PLRECB   DS    XL1                                                              
PLACTB   DS    XL1                                                              
PLKEYA   DS    0XL2                AFTER RECORD/ACTION                          
PLRECA   DS    XL1                                                              
PLACTA   DS    XL1                                                              
PLMASK   DS    XL2                                                              
PLBYTE   DS    XL1                                                              
*                                  ** PFKBLD S/R LOCAL W/S **                   
PFLINDS  DS    X                   PFKEY INDICATORS                             
PFLXREC  EQU   X'80'               SUPPRESS RECORD                              
PFLXACT  EQU   X'40'               SUPPRESS ACTION                              
PFLXUSR  EQU   X'20'               SUPPRESS USER DEFINED WORD                   
PFLIREC  EQU   X'10'               USER RECORD                                  
PFLIACT  EQU   X'08'               USER ACTION                                  
PFLIUSR  EQU   X'04'               USER USER DEFINED WORD                       
PFLALL   EQU   X'02'               FINISHED LOOP                                
*                                                                               
PFXREC   DS    XL8                 LAST RECORD                                  
PFLREC   DS    XL8                 THIS RECORD                                  
PFLACT   DS    XL8                 THIS ACTION                                  
PFLUSER  DS    XL16                THIS USER-DEFINED WORD                       
*                                                                               
BPAMORE  DS    XL11                XX=MORE                                      
         DS    XL8                 (OVERFLOW FOR BPAMORE)                       
BPAPFK   DS    XL(24*FRPLNQ)       LIST OF A(PFK TABLE ENTRIES)                 
BPLENLIN DS    A                   LENGTH OF PFKEY LINE                         
BPRECACT DS    XL2                                                              
BPKRECN  DS    XL1                                                              
*                                                                               
RTWORKL  EQU   *-RTWORKD                                                        
         EJECT                                                                  
* GEFILWORK                                                                     
         PUSH  PRINT                                                            
         PRINT  OFF                                                             
       ++INCLUDE GEFILWORK                                                      
         PRINT  ON                                                              
         POP   PRINT                                                            
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
FIL02    CSECT                                                                  
         ORG   FIL02+(((*-FIL02)/2048)+1)*2048                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060GEFIL02S  08/29/00'                                      
         END                                                                    
