*          DATA SET TALNK1D    AT LEVEL 001 AS OF 04/27/12                      
*PHASE T7041DA                                                                  
TALNK1D  TITLE 'FIXED CYCLE UPLOAD SERVER'                                      
         PRINT NOGEN                                                            
SVRDEF   LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TAFC,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (20*L'TLDRREC)+1                                                 
UPPTRBLK EQU   (20*L'TLDRREC)+1                                                 
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK                                         
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1D**,RR=RE                                           
         LR    RF,RC                                                            
         LR    RC,R1                                                            
         USING LP_D,RC                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         ST    RF,AERRTAB          SAVE A(ERROR TABLE)                          
         AHI   RF,ERRTAB                                                        
         ST    RF,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RF,SVPTRBLK                                                      
         ST    RF,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         J     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        INITIALIZE UPLOAD                                            *         
***********************************************************************         
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVED STORAGE                          
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,ALP              RESTORE A(LP_D)                              
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         J     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PROCESS AND UPLOAD RECORDS                                   *         
***********************************************************************         
                                                                                
INPUT    BRAS  RE,FCUPLOAD         PROCESS THE INPUT RECORD                     
         J     YES                 EXIT BACK TO DDLINK                          
                                                                                
***********************************************************************         
*        EXITS                                                        *         
***********************************************************************         
                                                                                
YES      LHI   RE,1                                                             
         J     *+8                                                              
NO       LHI   RE,0                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS FIXED CYCLE MAINTENANCE UPLOAD REQUEST               *         
***********************************************************************         
                                                                                
FCUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#FCULD)               
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQFCSTF                                   
         JNE   FCUP20                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         BRAS  RE,VALCOM           VALIDATE INTERNAL COMMERCIAL NUMBER          
         JNE   FCUP20                                                           
         BRAS  RE,VALSEQ           VALIDATE CAST SEQUENCE NUMBER                
         JNE   FCUP20                                                           
         BRAS  RE,VALGRR           VALIDATE GRR COVERED USE                     
                                                                                
         OC    RQFCBAL,RQFCBAL     IF BALANCE IS NOT PROVIDED                   
         JNZ   FCUP10                                                           
         BRAS  RE,INITADD          PREPARE TO ADD NEW FIXED CYCLE               
         J     FCUP20                                                           
FCUP10   BRAS  RE,INITCHA          OTHERWISE, PREPARE TO UPD. EXISTING          
                                                                                
FCUP20   MVI   OUTPUT,FCSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    FCUP30                                                           
         MVI   OUTPUT,FCSTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    FCUP30                                                           
         MVI   OUTPUT,FCSTOK2                                                   
                                                                                
FCUP30   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQFCMOD,RQFCEXE     IF MODE IS EXECUTE                           
         JNE   FCUP40                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    FCUP40                                                           
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE CAST RECORD'S ORIGINAL PASSIVES         
         BRAS  RE,EXECADD          ADD                                          
         BRAS  RE,EXECCHA          OR CHANGE FIXED CYCLE ELEMENT                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS) AND UPDATE PASSIVE POINTERS                  
                                                                                
         USING TALKFCYD,RF                                                      
         LA    RF,TLFCBLK                                                       
         MVC   TLFCSTF,RQFCSTF                                                  
         MVC   TLFCCOM,RQFCCOM                                                  
         MVC   TLFCSEQ,RQFCSEQ                                                  
         MVC   TLFCCYS,RQFCCYS                                                  
         MVC   TLFCCYE,RQFCCYE                                                  
         MVC   TLFCBAL,RQFCBAL                                                  
         MVC   TLFCGRR,RQFCGRR                                                  
         MVC   TLFCACS,RQFCACS                                                  
         MVC   TLFCACE,RQFCACE                                                  
         XC    TLFCPNH,TLFCPNH                                                  
         MVC   TLFCAAM,RQFCAAM                                                  
         MVC   TLFCCMT,RQFCCMT                                                  
         MVC   TLFCSSN,SVSSN                                                    
         MVC   TLFCTIME,SVTIME     ADD FIXED CYCLE TRACKING RECORD              
         GOTOR (#ADDFTRK,AADDFTRK),DMCB,(RF)                                    
         DROP  RF                                                               
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQFCSTF),(L'RQFCSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
FCUP40   GOTO1 VHEXOUT,DMCB,RQFCCOM,OUTPUT,L'RQFCCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VHEXOUT,DMCB,RQFCSEQ,OUTPUT,L'RQFCSEQ,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',9),            +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
         GOTO1 VDATCON,DMCB,(1,RQFCCYS),(8,OUTPUT)                              
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',10),           +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VDATCON,DMCB,(1,RQFCCYE),(8,OUTPUT)                              
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',11),           +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#FCERR,OUTPUT                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
FCSTOK1  EQU   1                   NO ERRORS - FIXED CYCLE ADDED                
FCSTOK2  EQU   2                   NO ERRORS - FIXED CYCLE CHANGED              
FCSTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         OC    RQFCSTF,RQFCSTF     ASSERT THAT STAFF ID IS PROVIDED             
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQFCCOM,RQFCCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   *+6                 NUMBER IS PROVIDED                           
         DC    H'00'                                                            
         OC    RQFCSEQ,RQFCSEQ     ASSERT THAT CAST SEQUENCE NUMBER             
         JNZ   *+6                 IS PROVIDED                                  
         DC    H'00'                                                            
         OC    RQFCCYS,RQFCCYS     ASSERT THAT CYCLE START DATE                 
         JNZ   *+6                 IS PROVIDED                                  
         DC    H'00'                                                            
         OC    RQFCCYE,RQFCCYE     ASSERT THAT CYCLE END DATE                   
         JNZ   *+6                 IS PROVIDED                                  
         DC    H'00'                                                            
         OC    RQFCAMT,RQFCAMT     ASSERT THAT AMOUNT IS PROVIDED               
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQFCUSE,RQFCUSE     ASSERT THAT USE IS PROVIDED                  
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQFCWID,RQFCWID     ASSERT THAT WEB APPLICATION ID               
         JNZ   *+6                 IS PROVIDED                                  
         DC    H'00'                                                            
                                                                                
         OC    RQFCBAL,RQFCBAL     IF BALANCE IS PROVIDED                       
         JZ    AR10                                                             
         CLI   RQFCGRT,0           ASSERT THAT "IS A GUARANTEE?"                
         JE    *+6                 INDICATOR IS NOT PROVIDED                    
         DC    H'00'                                                            
         OC    RQFCAAM,RQFCAAM     ASSERT THAT APPLICATION AMOUNT               
         JNZ   *+6                 IS PROVIDED                                  
         DC    H'00'                                                            
         CLI   RQFCAAB,0           ASSERT THAT "ADD APPLICATION AMOUNT          
         JNE   AR20                TO BALANCE?" IS PROVIDED                     
         DC    H'00'                                                            
                                                                                
AR10     CLI   RQFCGRT,0           IF BALANCE IS NOT PROVIDED                   
         JNE   *+6                 ASSERT THAT "IS A GUARANTEE?"                
         DC    H'00'               INDICATOR IS PROVIDED                        
         OC    RQFCACS,RQFCACS     ASSERT THAT APPLICATION CYCLE                
         JZ    *+6                 START DATE IS NOT PROVIDED                   
         DC    H'00'                                                            
         CLI   RQFCAAB,0           ASSERT THAT "ADD APPLICATION AMOUNT          
         JE    AR20                TO BALANCE?" IS NOT PROVIDED                 
         DC    H'00'                                                            
                                                                                
AR20     OC    RQFCACS,RQFCACS     IF APPLICATION CYCLE START DATE              
         JNZ   AR30                IS NOT PROVIDED                              
         OC    RQFCACE,RQFCACE     ASSERT THAT APPLICATION CYCLE                
         JZ    AR30                END DATE IS NOT PROVIDED                     
         DC    H'00'                                                            
                                                                                
AR30     CLC   RQFCUSE,=C'GRR'     IF PROVIDED USE IS NOT GRR                   
         JE    XIT                                                              
         OC    RQFCGRR,RQFCGRR     ASSERT THAT GRR COVERED USE                  
         JZ    XIT                 IS NOT PROVIDED                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED INTERNAL COMMERCIAL NUMBER                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
VALCOM   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOCCDQ    AND ENSURE IT EXISTS                         
         MVC   TLCOCCOM,RQFCCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERFCCONF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERFCCONF DC    AL1(EFCCONFX-*),AL2(1),AL1(ERRCATY3),AL1(D#FCCOM)                
         DC    C'Commercial record is not on file'                              
EFCCONFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED CAST SEQUENCE NUMBER                         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
VALSEQ   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCAKEY,TLCAKEY     READ FOR CAST KEY/RECORD                     
         MVI   TLCACD,TLCACDQ      AND ENSURE IT EXISTS                         
         MVC   TLCACOM,RQFCCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VS20                                                             
VS10     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VS20     CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   VS30                                                             
         CLC   TLCASEQ,RQFCSEQ                                                  
         JNE   VS10                                                             
         MVC   SVSSN,TLCASSN                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         J     YES                                                              
         DROP  R3                                                               
                                                                                
VS30     GOTOR (#ADDERR,AADDERR),DMCB,ERFCCANF                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALSEQ                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERFCCANF DC    AL1(EFCCANFX-*),AL2(2),AL1(ERRCATY3),AL1(D#FCSEQ)                
         DC    C'Cast record is not on file'                                    
EFCCANFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED GRR COVERED USED                             *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
VALGRR   NTR1  BASE=*,LABEL=*                                                   
         CLC   RQFCUSE,=C'GRR'     IF PROVIDED USE IS GRR                       
         JNE   XIT                                                              
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TACASTA2,TACASPUS   IF PERFORMER IS NOT SET TO                   
         JO    VG10                RECEIVE PAYMENT ONCE PER USE                 
         OC    RQFCGRR,RQFCGRR     PER CYCLE, GRR COVERED USE IS                
         JZ    XIT                 NOT ALLOWED                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERFCGRRN                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VG10     OC    RQFCGRR,RQFCGRR     OTEHRWISE, GRR COVERED USE                   
         JNZ   VG20                IS REQUIRED                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERFCGRRM                                  
         J     XIT                                                              
                                                                                
         USING TGTABLES,RE                                                      
VG20     L     RE,VSYSTAB                                                       
         L     RE,TGAUSES          RE=A(USE TABLE)                              
         DROP  RE                                                               
                                                                                
         USING USETABD,RE                                                       
VG30     CLI   0(RE),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   USECDE,RQFCGRR      FIND COVERED USE IN USE TABLE                
         JE    VG40                                                             
         LH    RF,USELEN                                                        
         AR    RE,RF                                                            
         J     VG30                                                             
VG40     MVC   SVFCGRR,USEEQU      AND SAVE COVERED USE EQUATE                  
         J     XIT                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALGRR                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERFCGRRN DC    AL1(EFCGRRNX-*),AL2(9),AL1(ERRCATY1),AL1(D#FCGCU)                
         DC    C'Only allowed for Once Per Use Per Cycle Performers'            
EFCGRRNX EQU   *                                                                
                                                                                
ERFCGRRM DC    AL1(EFCGRRMX-*),AL2(10),AL1(ERRCATY1),AL1(D#FCGCU)               
         DC    C'GRR Covered Use must be specified'                             
EFCGRRMX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF COMMENT RECORD           *         
*        ON ENTRY ... R4=A(CAST RECORD)                                         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
                                                                                
         USING TACRD,R4                                                         
         MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL            READ ALL APPLIED CREDIT ELEMENTS             
         J     *+8                                                              
IA10     BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLC   RQFCCYS,TACRSTRT    IF CYCLE START DATE FITS WITHIN              
         JL    IA20                AN EXISTING FIXED CYCLE                      
         CLC   RQFCCYS,TACREND                                                  
         JNH   IA30                                                             
IA20     CLC   RQFCCYE,TACRSTRT    OR CYCLE END DATE FITS WITHIN                
         JL    IA10                EXISTING CYCLE FIXED CYCLE                   
         CLC   RQFCCYE,TACREND                                                  
         JH    IA10                                                             
                                                                                
IA30     OC    RQFCGRR,RQFCGRR                                                  
         JZ    IA40                                                             
         CLC   TACRUSE,=C'GRR'                                                  
         JNE   IA10                                                             
         CLC   TACRTYPE,SVFCGRR                                                 
         JNE   IA10                                                             
                                                                                
IA40     LA    R2,ERFCAEXI         ... RETURN MESSAGE BASED ON                  
         CLC   RQFCCYS,TACRSTRT    WHETHER CYCLE START DATE IS                  
         JE    *+8                 SAME OR OVERLAPPING                          
         LA    R2,ERFCOVLP                                                      
         GOTOR (#ADDERR,AADDERR),DMCB,(R2)                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERFCAEXI DC    AL1(EFCAEXIX-*),AL2(3),AL1(ERRCATY1),AL1(D#FCCYS)                
         DC    C'Fixed Cycle already exists'                                    
EFCAEXIX EQU   *                                                                
                                                                                
ERFCOVLP DC    AL1(EFCOVLPX-*),AL2(4),AL1(ERRCATY1),AL1(D#FCCYS)                
         DC    C'Overlapping Fixed Cycle already exists'                        
EFCOVLPX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF FIXED CYCLE                *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTCHA                                                    
                                                                                
         XR    R0,R0                                                            
                                                                                
         USING TACRD,R4                                                         
         MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL            ENSURE FIXED CYCLE ALREADY                   
         J     *+8                 EXISTS ...                                   
IC10     BRAS  RE,NEXTEL                                                        
         JE    IC20                                                             
         LA    R2,ERFCCSNF                                                      
         LTR   R0,R0                                                            
         JZ    *+8                                                              
         LA    R2,ERFCCENF                                                      
         GOTOR (#ADDERR,AADDERR),DMCB,(R2)                                      
         J     XIT                                                              
                                                                                
IC20     CLC   TACRUSE,RQFCUSE     ONLY CONSIDER FIXED CYCLES FOR               
         JNE   IC10                PROVIDED USE                                 
                                                                                
         OC    RQFCGRR,RQFCGRR     IF GRR COVERED USE IS PROVIDED               
         JZ    IC30                                                             
         CLC   TACRTYPE,SVFCGRR    ONLY CONSIDER FIXED CYCLE FOR                
         JNE   IC10                MATCHING TYPE                                
                                                                                
IC30     CLC   RQFCCYS,TACRSTRT    ONLY CONSIDER FIXED CYCLES WITH              
         JNE   IC10                THAT MATCH THE PROVIDED CYCLE                
         LHI   R0,1                START                                        
         CLC   RQFCCYE,TACREND     AND PROVIDED CYCLE END DATE                  
         JNE   IC10                                                             
                                                                                
         CLC   TACRAPPL,RQFCAMT    ENSURE AMOUNT MATCHES MAINFRAME              
         JE    IC40                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERFCAMNM                                  
         J     XIT                                                              
                                                                                
IC40     ST    R4,ATACREL          SAVE A(APPLIED CREDIT ELEMENT)               
         ZICM  RF,RQFCAAM,4                                                     
                                                                                
         CLI   RQFCAAB,C'Y'        IF NOT "ADDING APPLICATION AMOUNT            
         JE    IC50                TO BALANCE?"                                 
         LCR   RF,RF               COMPLEMENT APPLICATION AMOUNT                
         STCM  RF,15,RQFCAAM                                                    
                                                                                
IC50     ZICM  RE,TACRBAL,4        ENSURE UPDATED BALANCE WILL                  
         AR    RE,RF               MATCH THE MAINFRAME                          
         ZICM  RF,RQFCBAL,4                                                     
         CR    RF,RE                                                            
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERFCBANM                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERFCCSNF DC    AL1(EFCCSNFX-*),AL2(5),AL1(ERRCATY1),AL1(D#FCCYS)                
         DC    C'Fixed Cycle not on file'                                       
EFCCSNFX EQU   *                                                                
                                                                                
ERFCCENF DC    AL1(EFCCENFX-*),AL2(6),AL1(ERRCATY1),AL1(D#FCCYE)                
         DC    C'Fixed Cycle not on file'                                       
EFCCENFX EQU   *                                                                
                                                                                
ERFCAMNM DC    AL1(EFCAMNMX-*),AL2(7),AL1(ERRCATY1),AL1(D#FCAMT)                
         DC    C'Amount does not match mainframe'                               
EFCAMNMX EQU   *                                                                
                                                                                
ERFCBANM DC    AL1(EFCBANMX-*),AL2(8),AL1(ERRCATY1),AL1(D#FCBAL)                
         DC    C'Balance does not match mainframe'                              
EFCBANMX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS FIXED CYCLE ELEMENT                             *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         USING TACRD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM          BUILD NEW APPLIED CREDIT                      
         MVI   TACREL,TACRELQ     HISTORY ELEMENT                               
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT,RQFCCYS                                                 
         MVC   TACREND,RQFCCYE                                                  
         MVC   TACRAPPL,RQFCAMT                                                 
         MVC   TACRBAL,RQFCAMT                                                  
         OI    TACRSTAT,TACRSTRK                                                
         MVC   TACRUSE,RQFCUSE                                                  
         MVC   TACRTYPE,SVFCGRR                                                 
                                                                                
         CLI   RQFCGRT,C'Y'        IF EXPLICITY INPUT                           
         JE    EA10                                                             
         CLC   TACRUSE,=C'GRR'     OR USE IS GRR                                
         JNE   EA20                                                             
EA10     OI    TACRSTAT,TACRSGUA   SET "IS A GUARANTEE?" INDICATOR              
                                                                                
EA20     CLC   TACRUSE,=C'ADT'     IF ADT USE                                   
         JE    EA30                                                             
         CLC   TACRUSE,=C'ADC'     OR ADC USE                                   
         JE    EA30                                                             
         CLC   TACRUSE,=C'ADO'     OR ADO USE                                   
         JNE   EA40                                                             
EA30     MVI   TACRTYPE,UADT13W    ASSUME 13W TYPE                              
                                                                                
EA40     GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  R2                                                               
                                                                                
         MVC   RQFCAAM,RQFCAMT     SET APPLICATION AMOUNT                       
         MVC   RQFCBAL,RQFCAMT     AND BALANCE                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES EXISTING FIXED CYCLE ELEMENT                 *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                                                              
                                                                                
         USING TACRD,R4                                                         
         L     R4,ATACREL          UPDATE EXISTING APPLIED CREDIT               
         MVC   TACRBAL,RQFCBAL     ELEMENT                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - FIXED CYCLE MAINTENANCE UPLOAD                        *         
***********************************************************************         
                                                                                
FCHDR    LKMAP H,I#FCULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#FCMOD,UBIN,TA#PMODE,OLEN=L'RQFCMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQFCMOD)                                       
F$STF    LKMAP F,D#FCSTF,CHAR,TA#STAFF,MAXLEN=L'RQFCSTF,               +        
               OUTPUT=(D,B#SAVED,RQFCSTF)                                       
F$COM    LKMAP F,D#FCCOM,HEXD,TA#COMCD,OLEN=L'RQFCCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQFCCOM)                                       
F$SEQ    LKMAP F,D#FCSEQ,HEXD,TA#CSTSQ,OLEN=L'RQFCSEQ,MAXLEN=4,        +        
               OUTPUT=(D,B#SAVED,RQFCSEQ)                                       
F$CYS    LKMAP F,D#FCCYS,PDAT,TA#CYCST,OUTPUT=(D,B#SAVED,RQFCCYS)               
F$CYE    LKMAP F,D#FCCYE,PDAT,TA#CYCED,OUTPUT=(D,B#SAVED,RQFCCYE)               
F$AMT    LKMAP F,D#FCAMT,UBIN,TA#AMT,OLEN=L'RQFCAMT,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQFCAMT)                                       
F$BAL    LKMAP F,D#FCBAL,UBIN,TA#BAL,OLEN=L'RQFCBAL,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQFCBAL)                                       
F$GRT    LKMAP F,D#FCGRT,CHAR,TA#ISGRT,MAXLEN=L'RQFCGRT,               +        
               OUTPUT=(D,B#SAVED,RQFCGRT)                                       
F$USE    LKMAP F,D#FCUSE,CHAR,TA#USECD,MAXLEN=L'RQFCUSE,               +        
               OUTPUT=(D,B#SAVED,RQFCUSE)                                       
F$GRR    LKMAP F,D#FCGCU,CHAR,TA#GRRCU,MAXLEN=L'RQFCGRR,               +        
               OUTPUT=(D,B#SAVED,RQFCGRR)                                       
F$ACS    LKMAP F,D#FCACS,PDAT,TA#APCYS,OUTPUT=(D,B#SAVED,RQFCACS)               
F$ACE    LKMAP F,D#FCACE,PDAT,TA#APCYE,OUTPUT=(D,B#SAVED,RQFCACE)               
F$AAM    LKMAP F,D#FCAAM,UBIN,TA#PYOAP,OLEN=L'RQFCAAM,MAXLEN=9,        +        
               OUTPUT=(D,B#SAVED,RQFCAAM)                                       
F$AAB    LKMAP F,D#FCAAB,CHAR,TA#FTAAB,MAXLEN=L'RQFCAAB,               +        
               OUTPUT=(D,B#SAVED,RQFCAAB)                                       
F$CMT    LKMAP F,D#FCCMT,CHAR,TA#COMNT,MAXLEN=L'RQFCCMT,               +        
               OUTPUT=(D,B#SAVED,RQFCCMT)                                       
F$WID    LKMAP F,D#FCWID,CHAR,TA#WAPID,MAXLEN=L'RQFCWID,               +        
               OUTPUT=(D,B#SAVED,RQFCWID)                                       
F$EOV    LKREQ F,250,(I,B#SAVED,I$EROV),UBIN,LIST=F,                   +        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,251,(I,B#SAVED,I$CLMC),UBIN,LIST=F,                   +        
               OLEN=1,MAXLEN=3,TEXT=TA#CLRMC,COL=*                              
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
SVSSN    DS    CL(L'TLFCSSN)       SAVED SOCIAL SECURITY NUMBER                 
SVFCGRR  DS    CL(L'TACRTYPE)      SAVED GRR COVERED USE TYPE                   
SVTIME   DS    XL3                 SAVED TIME                                   
                                                                                
ATACREL  DS    A                   SAVED A(APPLIED CREDIT ELEMENT)              
                                                                                
TLFCBLK  DS    XL(TLFCLNQ)         TALNKFCY BLOCK                               
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        FIXED CYCLE MAINTENANCE REQUEST MAP FIELDS                   *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQFCMOD  DS    CL1                 MODE                                         
RQFCRTV  EQU   1                   RETRIEVE                                     
RQFCVFY  EQU   2                   VERIFY                                       
RQFCEXE  EQU   3                   EXECUTE                                      
RQFCSTF  DS    CL(L'TLFCSTF)       STAFF CODE                                   
RQFCCOM  DS    XL(L'TLFCCOM)       INTERNAL COMMERCIAL NUMBER                   
RQFCSEQ  DS    XL(L'TLFCSEQ)       CAST SEQUENCE NUMBER                         
RQFCCYS  DS    XL(L'TLFCCYS)       CYCLE START DATE                             
RQFCCYE  DS    XL(L'TLFCCYE)       CYCLE END DATE                               
RQFCAMT  DS    XL4                 AMOUNT                                       
RQFCBAL  DS    XL(L'TLFCBAL)       BALANCE                                      
RQFCGRT  DS    CL1                 IS A GUARANTEE?                              
RQFCUSE  DS    CL3                 USE                                          
RQFCGRR  DS    CL(L'TLFCGRR)       GRR COVERED USE                              
RQFCACS  DS    XL(L'TLFCACS)       APPLICATION CYCLE START DATE                 
RQFCACE  DS    XL(L'TLFCACE)       APPLICATION CYCLE END DATE                   
RQFCAAM  DS    XL(L'TLFCAAM)       APPLICATION AMOUNT                           
RQFCAAB  DS    CL1                 ADD APPLICATION AMOUNT TO BALANCE?           
RQFCCMT  DS    CL(L'TLFCCMT)       COMMENT                                      
RQFCWID  DS    CL18                WEB APPLICATION ID                           
RQRCLNQ  EQU   *-RQFCMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TALNKFCYD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK1D   04/27/12'                                      
         END                                                                    
