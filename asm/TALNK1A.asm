*          DATA SET TALNK1A    AT LEVEL 001 AS OF 08/29/13                      
*PHASE T7041AC                                                                  
TALNK1A  TITLE 'INVOICE REOPEN/RECALL/CANCEL UPLOAD SERVER'                     
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TA1A,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (60*L'TLDRREC)+1                                       f         
UPPTRBLK EQU   (60*L'TLDRREC)+1                                                 
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK                                         
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1A**,RR=RE                                           
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
                                                                                
INPUT    BRAS  RE,INRCUPL          PROCESS THE INPUT RECORD                     
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
*        PROCESS INVOICE REOPEN/RECALL/CANCEL UPLOAD REQUEST          *         
***********************************************************************         
                                                                                
INRCUPL  NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#IRCULD)              
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,0                              
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQIRCSTF                                  
         JNE   INRCUP30                                                         
                                                                                
         GOTOR (#CVTINV,ACVTINV),DMCB,RQIRCINV,SVINV                            
         MVC    SVCINV,SVINV                                                    
         XC     SVCINV,=6X'FF'     SET INVOICE VARIABLES                        
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         USING TLIND,R3                                                         
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      READ FOR INVOICE KEY                         
         MVC   TLINAGY,RQIRCAGY                                                 
         MVC   TLININV,SVCINV                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   INRCUP10                                                         
         TM    IOKEY+TLDRSTAT-TLDRD,TLINSDEL                                    
         JZ    INRCUP20                                                         
INRCUP10 GOTOR (#ADDERR,AADDERR),DMCB,ERIRCINF                                  
         J     INRCUP30                                                         
         DROP  R3                                                               
                                                                                
INRCUP20 MVC   SVINKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         BRAS  RE,VALCOM           VALIDATE COMMON RULES FOR                    
         JNE   INRCUP30            REOPEN/RECALL AND CANCEL                     
                                                                                
         BRAS  RE,VALREO           VALIDATE SPECIFIC REOPEN/RECALL              
         JNE   INRCUP30                                                         
         BRAS  RE,VALCNL           OR CANCEL RULES                              
         JNE   INRCUP30                                                         
                                                                                
INRCUP30 MVI   OUTPUT,INRCSTER     IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    *+8                                                              
         MVI   OUTPUT,INRCSTOK     ELSE RETURN "OK" STATUS                      
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',2),               +        
               ('LD_CHARQ',RQIRCSTF),(L'RQIRCSTF,0)                             
         GOTO1 VDATCON,(R1),(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,(R1),('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',4),               +        
               ('LD_CHARQ',RQIRCAGY),(L'RQIRCAGY,0)                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',5),               +        
               ('LD_CHARQ',RQIRCINV),(L'RQIRCINV,0)                             
                                                                                
         CLI   RQIRCMOD,RQIRCEXE   IF MODE IS EXECUTE                           
         JNE   INRCUP50                                                         
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    INRCUP50                                                         
                                                                                
         MVC   IOKEY,SVINKEY       READ INVOICE FOR UPDATE                      
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
INRCUP40 GOTOR (#SAVPTRS,ASAVPTRS) SAVE ORIGINAL POINTERS                       
                                                                                
         BRAS  RE,EXECREO          EXECUTE INITIAL RECALL/REOPEN STEPS          
         BRAS  RE,EXECCNL          OR INITIAL CANCEL STEPS                      
                                                                                
         GOTOR DELUH,DMCB,0        DELETE USAGE HISTORY RECORDS                 
         BRAS  RE,UNMADV           UNMARK ADVICE RECORDS                        
         BRAS  RE,PROPERFS         PROCESS ALL PERFORMERS ON PAYMENT            
                                                                                
         BAS   RE,GETSINV          IF INVOICE HAS SUBSIDIARY INVOICES           
         JE    INRCUP40            GO PROCESS THEM                              
                                                                                
         BAS   RE,GETCINV          IF INVOICE HAS ATTACHED CANADIAN             
         JE    INRCUP40            INVOICE, GO PROCESS IT                       
                                                                                
INRCUP50 GOTOR (#OUTERR,AOUTERR),DMCB,O#IRCERR,OUTPUT                           
                                                                                
         TM    ERRSTAT,ESECTRD     IF ANY ERRORS HAVE BEEN ENCOUNTERED          
         JZ    YES                 ALWAYS SEND DOWN INVOICE RECORD              
         OI    ERRSTAT,ESREVIW                                                  
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#IRCULD)              
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#INSDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#INSSTF),        +        
               ('LD_CHARQ',RQIRCSTF),(L'RQIRCSTF,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#INSAGY),        +        
               ('LD_CHARQ',RQIRCAGY),(L'RQIRCAGY,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#INSINV),        +        
               ('LD_CHARQ',RQIRCINV),(L'RQIRCINV,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE READS SUBSIARY INVOICE AND SETS VARIABLES            *         
*        ON ENTRY ... R3 = A(I/O KEY)                                           
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
GETSINV  NTR1                                                                   
         CLI   RQIRCACT,RQIRCACA   IF ACTION IS CANCEL                          
         JNE   NO                                                               
                                                                                
         LA    R2,SUBINVS                                                       
GSI10    CLI   0(R2),X'FF'                                                      
         JE    NO                                                               
         OC    0(L'TASIINV,R2),0(R2)                                            
         JNZ   GSI20                                                            
         LA    R2,L'TASIINV(R2)                                                 
         J     GSI10                                                            
                                                                                
         USING TLIND,R3                                                         
GSI20    XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      READ FOR SUBSIDIARY INVOICE                  
         MVC   TLINAGY,RQIRCAGY    KEY/RECORD                                   
         MVC   TLININV,0(R2)                                                    
         XC    TLININV,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    IOKEY+TLDRSTAT-TLDRD,TLINSDEL                                    
         JZ    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         MVC   SVINKEY,IOKEY       RESET VARIABLES                              
         MVC   SVCINV,TLININV                                                   
         MVC   SVINV,0(R2)                                                      
         XC    0(L'TASIINV,R2),0(R2)                                            
         DROP  R3                                                               
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GSI30    BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAIND,R4                                                         
         CLI   0(R4),TAINELQ       SET INVOICE STATUS VARIABLES                 
         JNE   GSI40                                                            
         MVC   SVINSTA2,TAINSTA2                                                
         ST    R4,ATAINEL                                                       
         J     GSI30                                                            
         DROP  R4                                                               
                                                                                
GSI40    CLI   0(R4),TAPDELQ       SET PAYMENT DETAILS VARIABLES                
         JNE   GSI30                                                            
         ST    R4,ATAPDEL                                                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE READS ATTACHED CANADIAN INVOICE AND SETS VARIABLES   *         
*        ON ENTRY ... R3 = A(I/O KEY)                                 *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
GETCINV  NTR1                                                                   
         OC    SVUCINC,SVUCINC     IF INVOICE HAS ATTACHED CANADIAN             
         JZ    NO                  INVOICE ...                                  
                                                                                
         USING TLIND,R3                                                         
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      READ FOR CANADIAN INVOICE                    
         MVC   TLINAGY,RQIRCAGY    KEY/RECORD                                   
         MVC   TLININV,SVUCINC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    IOKEY+TLDRSTAT-TLDRD,TLINSDEL                                    
         JZ    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         MVC   SVINKEY,IOKEY       RESET VARIABLES                              
         MVC   SVCINV,SVUCINC                                                   
         MVC   SVINV,SVUCINC                                                    
         XC    SVINV,=6X'FF'                                                    
         XC    SVUCINC,SVUCINC                                                  
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GCI10    BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAIND,R4                                                         
         CLI   0(R4),TAINELQ       SET INVOICE STATUS VARIABLES                 
         JNE   GCI20                                                            
         MVC   SVINSTA2,TAINSTA2                                                
         ST    R4,ATAINEL                                                       
         J     GCI10                                                            
         DROP  R4                                                               
                                                                                
GCI20    CLI   0(R4),TAPDELQ       SET PAYMENT DETAILS VARIABLES                
         JNE   GCI10                                                            
         ST    R4,ATAPDEL                                                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
INRCSTOK EQU   1                   NO ERRORS - INVOICE REOPENED/CANCEL          
INRCSTER EQU   2                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INRCUPL                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERIRCINF DC    AL1(EIRCINFX-*),AL2(1),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Invoice is not on file'                                        
EIRCINFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE COMMON RULES FOR REOPEN/RECALL AND CANCEL           *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(INVOICE RECORD)                          *         
***********************************************************************         
                                                                                
VALCOM   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVWID,SPACES        INTIALIZE SAVED WEB APPLICATION ID           
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
VCOM10   BRAS  RE,NEXTEL                                                        
         JNE   VCOM120                                                          
                                                                                
         USING TAPDD,R4                                                         
         CLI   0(R4),TAPDELQ       IF PAYMENT DETAILS ELEMENT                   
         JNE   VCOM40                                                           
         MVC   SVPDCOM,TAPDCOM     SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   SVPDUSE,TAPDUSE     USE                                          
         MVC   SVPDCYC,TAPDCYCS    CYCLE                                        
         MVC   SVPDSTA2,TAPDSTA2   STATUS 2                                     
         MVC   SVPDPST1,TAPDPST1   STATUS 1                                     
         ST    R4,ATAPDEL          AND ADDRESS OF ELEMENT                       
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               RF=A(USE TABLE)                              
                                                                                
VCOM20   CLC   TAPDUSE,USECDE      FIND USE EQUATE IN USE TABLE                 
         JE    VCOM30                                                           
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   VCOM20                                                           
         DC    H'00'                                                            
                                                                                
VCOM30   MVC   SVUSEQU,USEEQU      SAVE USE EQUATE                              
         MVC   SVUSSTA2,USESTAT2   AND SECOND USE STATUS                        
         J     VCOM10                                                           
         DROP  R4,RF                                                            
                                                                                
         USING TACOD,R4                                                         
VCOM40   CLI   0(R4),TACOELQ       IF COMMERCIAL DETAILS ELEMENT                
         JNE   VCOM50                                                           
         MVC   SVCOCID,TACOCID     SAVE COMMERCIAL ID                           
         J     VCOM10                                                           
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
VCOM50   CLI   0(R4),TAINELQ       IF INVOICE STATUS ELEMENT                    
         JNE   VCOM90                                                           
                                                                                
         TM    TAINSTAT,TAINSPAY   ENSURE THAT INVOICE IS PAID                  
         JO    VCOM60                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCNPD                                  
         J     NO                                                               
                                                                                
VCOM60   TM    TAINSTAT,TAINSCHK   ENSURE THAT CHECKS HAVE NOT BEEN             
         JZ    VCOM70              WRITTEN                                      
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCKW                                  
         J     NO                                                               
                                                                                
VCOM70   TM    TAINSTAT,TAINSCIN   ENSURE THAT INVOICE IS NOT A                 
         JZ    VCOM80              CANCELLER                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCIN                                  
         J     NO                                                               
                                                                                
VCOM80   MVC   SVINSTA2,TAINSTA2   SAVE INVOICE STATUS 2                        
         MVC   SVINPDTE,TAINPDTE   PAYMENT DATE                                 
         ST    R4,ATAINEL          AND ADDRESS OF ELEMENT                       
         J     VCOM10                                                           
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
VCOM90   CLI   0(R4),TAFNELQ      IF FREE FORM NAME ELEMENT                     
         JNE   VCOM110                                                          
         LA    RF,SVWID                                                         
         CLI   TAFNTYPE,TAFNTWEB  SAVE WEB APPLICATION ID                       
         JE    VCOM100                                                          
         LA    RF,SVADV                                                         
         CLI   TAFNTYPE,TAFNTADV  OR ADVICE NUMBER                              
         JNE   VCOM10                                                           
VCOM100  ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),TAFNNAME                                                 
         J     VCOM10                                                           
         DROP  R4                                                               
                                                                                
         USING TAUCD,R4                                                         
VCOM110  CLI   0(R4),TAUCELQ       IF US/CANADIAN INVOICE ELEMENT               
         JNE   VCOM10                                                           
         MVC   SVUCINC,TAUCINC     SAVE CANADIAN INVOICE NUMBER                 
         J     VCOM10                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
VCOM120  OC    RQIRCWID,RQIRCWID   IF WEB APPLICATION ID IS PROVIDED            
         JZ    VCOM130                                                          
         CLC   RQIRCWID,SVWID      ENSURE THAT IT MATCHES INVOICE STAMP         
         JE    VCOM130                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCWID                                  
         J     NO                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLIND,R3                                                         
VCOM130  OC    SVUCINC,SVUCINC     IF CANADIAN INVOICE DEFINED                  
         JZ    VCOM140                                                          
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      READ FOR CANADIAN INVOICE RECORD             
         MVC   TLINAGY,RQIRCAGY                                                 
         MVC   TLININV,SVUCINC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAINELQ      GET CANADIAN INVOICE STATUS ELEEMNT          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TAINSTAT,TAINSCHK   ENSURE THAT CHECKS HAVE NOT BEEN             
         JZ    VCOM140             WRITTEN                                      
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCCK                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
VCOM140  CLI   RQIRCMOD,RQIRCEXE   IF MODE IS EXECUTE                           
         JNE   VCOM150             ENSURE CHECK LOCKOUT STATUS IS OFF           
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_CHECKS',ERIRCCKL                 
         JE    VCOM150                                                          
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_PRCHKS',ERIRCCKL                 
         JE    VCOM150                                                          
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_P+CHKS',ERIRCCKL                 
                                                                                
***********************************************************************         
                                                                                
         USING TLCMD,R3                                                         
VCOM150  XC    TLCMKEY,TLCMKEY     ENSURE USER ACKNOWLEDGED ANY                 
         MVI   TLCMCD,TLCMCDQ      ATTACHED INVOICE COMMENTS                    
         MVC   TLCMAGY,RQIRCAGY                                                 
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMINV,SVCINV                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         CLC   IOKEY(TLCMLEV-TLCMD),IOKEYSAV                                    
         JNE   YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCACM                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERIRCNPD DC    AL1(EIRCNPDX-*),AL2(2),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Invoice has not been paid'                                     
EIRCNPDX EQU   *                                                                
                                                                                
ERIRCCKW DC    AL1(EIRCCKWX-*),AL2(3),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Checks have already been written for this invoice'             
EIRCCKWX EQU   *                                                                
                                                                                
ERIRCCIN DC    AL1(EIRCCINX-*),AL2(4),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Invoice is a canceller invoice'                                
EIRCCINX EQU   *                                                                
                                                                                
ERIRCCCK DC    AL1(EIRCCCKX-*),AL2(5),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Checks have already been written for Canadian invoice'         
EIRCCCKX EQU   *                                                                
                                                                                
ERIRCACM DC    AL1(EIRCACMX-*),AL2(22),AL1(ERRCATY2),AL1(D#IRCINV)              
         DC    C'Please review attached invoice comment'                        
EIRCACMX EQU   *                                                                
                                                                                
ERIRCWID DC    AL1(EIRCWIDX-*),AL2(23),AL1(ERRCATY3),AL1(D#IRCWID)              
         DC    C'Web Application ID mismatch'                                   
EIRCWIDX EQU   *                                                                
                                                                                
ERIRCCKL DC    AL1(EIRCCKLX-*),AL2(500),AL1(ERRCATY1),AL1(D#IRCMOD)             
         DC    C'Urgent check run in progress'                                  
EIRCCKLX EQU   *                                                                
                                                                                
***********************************************************************         
*        VALIDATE FOR ACTION REOPEN/RECALL                            *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4/AIO3 = A(INVOICE RECORD)                     *         
*                     AIO4 = A(2404A CANADIAN INVOICE)                *         
***********************************************************************         
                                                                                
VALREO   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQIRCACT,RQIRCARE   IF ACTION IS REOPEN/RECALL                   
         JNE   YES                                                              
                                                                                
         USING TAIND,R4                                                         
         L     R4,ATAINEL          ENSURE THAT INVOICE HAS NOT BEEN             
         TM    TAINSTAT,TAINSBIL   BILLED                                       
         JZ    VREO10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCBIL                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
VREO10   TM    SVINSTA2,TAINSHLP   IF PUR INVOICE HAS BEEN PRINTED ...          
         JZ    VREO30                                                           
                                                                                
         CLC   SVPDUSE,=C'GRT'     ENSURE INVOICE WAS NOT FOR A                 
         JNE   VREO20              GUARANTEE PAYMENT                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCGRT                                  
         J     NO                                                               
                                                                                
         USING TAPDD,R4                                                         
VREO20   L     R4,ATAPDEL                                                       
         OC    TAPDGUAR,TAPDGUAR   ENSURE PAYMENT DID NOT APPLY                 
         JZ    VREO30              AGAINT GUARANTEE                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCAPG                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         USING TAUCD,R4                                                         
VREO30   L     R4,AIO3                                                          
         MVI   ELCODE,TAUCELQ      IF PAYMENT FOR 2404A COMMERCIAL              
         BRAS  RE,GETEL            ENSURE THIS IS NOT THE CANADIAN              
         JNE   VREO50              INVOICE                                      
         CLC   TAUCINU,SVINKEY+TLININV-TLIND                                    
         JE    VREO40                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCROC                                  
         J     NO                                                               
                                                                                
VREO40   OC    TAUCINC,TAUCINC     IF CANADIAN INVOICE IS DEFINED               
         JZ    VREO50                                                           
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAINELQ      GET CANADIAN INVOICE STATUS ELEMENT          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TAINSTAT,TAINSBIL   ENSURE THAT INVOICE HAS NOT BEEN             
         JZ    VREO50              BILLED                                       
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCBI                                  
         J     NO                                                               
                                                                                
VREO50   MVC   SCRNINV,SVINV                                                    
         TM    TAINSTA2,TAINSSCR   SET SCREENS INVOICE NUMBER                   
         JZ    VREO60                                                           
         MVC   SCRNINV(3),TAINIDTE                                              
         MVC   SCRNINV+3(3),TAINITIM                                            
         DROP  R4                                                               
                                                                                
VREO60   BAS   RE,CHKGRT                                                        
         JNE   XIT                                                              
                                                                                
         BAS   RE,CHKPCY                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES LARGE SCALE GUARANTEE PAYMENTS ARE ELIGIBLE  *         
*        TO BE REOPENED                                               *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
CHKGRT   NTR1                                                                   
         CLC   SVPDUSE,=C'GRT'     EXIT IF PAY TYPE IS NOT GRT                  
         JNE   YES                                                              
         TM    SVPDPST1,TAPDPCRD   OR IF THIS IS A CREDIT PAYMENT               
         JO    YES                                                              
                                                                                
         USING TLCKD,R3                                                         
         XC    TLCKKEY,TLCKKEY     READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKCD,TLCKCDQ      INVOICE                                      
         MVC   TLCKAGY,RQIRCAGY                                                 
         MVC   TLCKINV,SVINV                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CGRT20                                                           
CGRT10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
CGRT20   CLC   IOKEY(TLCKSORT-TLCKD),IOKEYSAV                                   
         JNE   YES                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CGRT10                                                           
         MVC   SVCAGUA,TACAGUA     SAVE GUARANTEE CODE                          
         MVC   SVCKKEY,IOKEY       AND CHECK KEY                                
         DROP  R4                                                               
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         MVC   TLGUGUA,SVCAGUA                                                  
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT LARGE OVERSCALE              
         JNZ   CGRT110             GUARANTEES                                   
                                                                                
         CLC   TAGUINV,SVINV       IF THIS PAYMENT CREATED THE                  
         JNE   CGRT50              GUARANTEE ...                                
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   READ ALL CAST KEYS ATTACHED TO               
         MVI   TLCAPCD,TLCAGCDQ    THIS GUARANTEE                               
         MVC   TLCAGSSN,SVCKKEY+TLCKSSN-TLCKD                                   
         MVC   TLCAGGUA,SVCAGUA                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CGRT40                                                           
CGRT30   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CGRT40   CLC   IOKEY(TLCAGCOM-TLCAPD),IOKEYSAV                                  
         JNE   CGRT50              IF ATTACHED TO ANY ADDITIONAL                
         CLC   TLCAGCOM,SVPDCOM    COMMERCIALS, MUST REMOVE THOSE               
         JE    CGRT30              ATTACHMENTS FIRST                            
         GOTO1 BLDAC,DMCB,TLCAGCOM,ERIRCACD                                     
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCAAC                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLCKPD,R3                                                        
CGRT50   XC    TLCKPKEY,TLCKPKEY   READ UNPROCESSED CHECK RECORDS               
         MVI   TLCKPCD,TLCKECDQ    FOR THIS PERFORMER                           
         MVC   TLCKESSN,SVCKKEY+TLCKSSN-TLCKD                                   
         MVI   TLCKECUR,C'U'                                                    
         J     CGRT70                                                           
                                                                                
CGRT60   CLI   IOKEYSAV+TLCKECUR-TLCKPD,C'C'                                    
         JE    CGRT110             NEXT READ UNPROCESSED CAN$ CHECK             
         MVC   TLCKPKEY,IOKEYSAV   RECORDS FOR THIS PERFOMER                    
         MVI   TLCKECUR,C'C'                                                    
                                                                                
CGRT70   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CGRT90                                                           
CGRT80   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
CGRT90   CLC   IOKEY(TLCKEEMP-TLCKPCD),IOKEYSAV                                 
         JNE   CGRT60                                                           
         CLI   TLCKEDTE,0                                                       
         JNE   CGRT80                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,SVCAGUA     IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CGRT80              THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,=C'GRT'     SKIP GRT PAYMENTS                            
         JE    CGRT80                                                           
                                                                                
         LA    R1,TAPDCYCS                                                      
         TM    TAPDOPT4,TAPDGRTE   R1=A(PAYMENT'S APPLY DATE)                   
         JZ    CGRT100                                                          
         LA    R1,TAPDCYCE                                                      
         DROP  R4                                                               
                                                                                
         USING TLCKD,R4                                                         
CGRT100  CLC   0(L'TAPDCYCS,R1),SVPDCYC                                         
         JL    CGRT80                                                           
         CLC   0(L'TAPDCYCS,R1),SVPDCYC+3                                       
         JH    CGRT80              IF APPLY DATE WITHIN GRT'S CYCLE             
         L     R4,AIO3             CANNOT REOPEN THIS GUARANTEE                 
         GOTO1 BLDAI,DMCB,TLCKAGY,TLCKINV,ERIRCARD                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCAIR                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
CGRT110  MVC   IOKEY,SVCKKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CGRT10                                                           
                                                                                
***********************************************************************         
*        ROUTINE ENSURES PER CYCLE GUARANTEE PAYMENTS ARE ELIGIBLE    *         
*        TO BE REOPENED                                               *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
CHKPCY   NTR1                                                                   
         TM    SVPDSTA2,TAPDSPRI   EXIT IF PAY TYPE IS NOT PER CYCLE            
         JZ    XIT                                                              
         TM    SVPDPST1,TAPDPCRD   OR IF THIS IS A CREDIT PAYMENT               
         JO    XIT                                                              
                                                                                
         USING TLCKD,R3                                                         
         XC    TLCKKEY,TLCKKEY     READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKCD,TLCKCDQ      INVOICE                                      
         MVC   TLCKAGY,RQIRCAGY                                                 
         MVC   TLCKINV,SVINV                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CPCY20                                                           
CPCY10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
CPCY20   CLC   IOKEY(TLCKSORT-TLCKD),IOKEYSAV                                   
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CPCY10                                                           
         MVC   SVCAGUA,TACAGUA     SAVE GUARANTEE CODE                          
         MVC   SVCKKEY,IOKEY       AND CHECK KEY                                
         DROP  R4                                                               
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         MVC   TLGUGUA,SVCAGUA                                                  
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT PER CYCLE                    
         JZ    CPCY60              GUARANTEES                                   
         DROP  R4                                                               
                                                                                
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY   READ UNPROCESSED CHECK RECORDS               
         MVI   TLCKPCD,TLCKECDQ    FOR THIS PERFORMER                           
         MVC   TLCKESSN,SVCKKEY+TLCKSSN-TLCKD                                   
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'TP '                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CPCY40                                                           
CPCY30   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
CPCY40   CLC   IOKEY(TLCKEDTE+1-TLCKPCD),IOKEYSAV                               
         JNE   CPCY60                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,SVCAGUA     IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CPCY30              THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO3             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   SKIP PER CYCLE PAYMENTS                      
         JO    CPCY30                                                           
                                                                                
         LA    R1,TAPDCYCS                                                      
         TM    TAPDOPT4,TAPDGRTE   R1=A(PAYMENT'S APPLY DATE)                   
         JZ    CPCY50                                                           
         LA    R1,TAPDCYCE                                                      
         DROP  R4                                                               
                                                                                
         USING TLCKD,R4                                                         
CPCY50   CLC   0(L'TAPDCYCS,R1),SVPDCYC                                         
         JL    CGRT80                                                           
         CLC   0(L'TAPDCYCS,R1),SVPDCYC+3                                       
         JH    CGRT80              IF APPLY DATE WITHIN GRT'S CYCLE             
         L     R4,AIO3             CANNOT REOPEN THIS GUARANTEE                 
         GOTO1 BLDAI,DMCB,TLCKAGY,TLCKINV,ERIRCARD                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCAIR                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
CPCY60   MVC   IOKEY,SVCKKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CPCY10                                                           
                                                                                
***********************************************************************         
*        ROUTINE INSERTS AGENCY/INVOICE INTO 14-BYTE BLOCK            *         
***********************************************************************         
                                                                                
BLDAI    NTR1                                                                   
         L     R2,0(R1)            R2=A(AGENCY CODE)                            
         L     R3,4(R1)            R3=A(INVOICE CODE)                           
         L     R4,8(R1)            R4=A(13-BYTE OUTPUT BLOCK)                   
                                                                                
         MVC   0(6,R4),0(R2)                                                    
                                                                                
         LA    R4,1(R4)                                                         
BAI10    CLI   0(R4),C' '                                                       
         JE    BAI20                                                            
         LA    R4,1(R4)                                                         
         J     BAI10                                                            
                                                                                
BAI20    MVI   0(R4),C'/'                                                       
         GOTOR (#CVTINV,ACVTINV),DMCB,0(R3),1(R4)                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE INSERTS AGENCY/COMMERCIAL ID INTO 19 BYTE BLOCK      *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
BLDAC    NTR1                                                                   
         L     RE,0(R1)                                                         
         MVC   FULL1,0(RE)         FULL1 = INTERNAL COMMERCIAL NUMBER           
                                                                                
         L     R2,4(R1)            R2 = A(19-BYTE OUTPUT BLOCK)                 
         MVC   0(19,R2),SPACES                                                  
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,FULL1                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO3                                                          
         MVC   0(6,R2),TLCOAGY                                                  
         DROP  R4                                                               
                                                                                
         LA    R2,1(R2)                                                         
BAC10    CLI   0(R2),C' '                                                       
         JE    BAC20                                                            
         LA    R2,1(R2)                                                         
         J     BAC10                                                            
BAC20    MVI   0(R2),C'/'                                                       
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   1(L'TACOCID,R2),TACOCID                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALREO                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERIRCROC DC    AL1(EIRCROCX-*),AL2(6),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'2404A Canadian invoice - please reopen US Invoice'             
EIRCROCX EQU   *                                                                
                                                                                
ERIRCAIR DC    AL1(EIRCAIRX-*),AL2(7),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Applying payment must be reopened first: '                     
ERIRCARD DC    CL14''                                                           
EIRCAIRX EQU   *                                                                
                                                                                
ERIRCAAC DC    AL1(EIRCAACX-*),AL2(8),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Guarantee attached to Commercial '                             
ERIRCACD DC    CL19''                                                           
EIRCAACX EQU   *                                                                
                                                                                
ERIRCBIL DC    AL1(EIRCBILX-*),AL2(9),AL1(ERRCATY3),AL1(D#IRCINV)               
         DC    C'Invoice has already been billed'                               
EIRCBILX EQU   *                                                                
                                                                                
ERIRCGRT DC    AL1(EIRCGRTX-*),AL2(10),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'PUR Invoice for Guarantee payment already printed'             
EIRCGRTX EQU   *                                                                
                                                                                
ERIRCAPG DC    AL1(EIRCAPGX-*),AL2(11),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Printed PUR invoice applied against Guarantee'                 
EIRCAPGX EQU   *                                                                
                                                                                
ERIRCCBI DC    AL1(EIRCCBIX-*),AL2(12),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Canadian invoice has already been billed'                      
EIRCCBIX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FOR ACTION CANCEL                                   *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
*                     AIO4 = A(2404A CANADIAN INVOICE RECORD)         *         
***********************************************************************         
                                                                                
VALCNL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQIRCACT,RQIRCACA   IF ACTION IS CANCEL                          
         JNE   YES                                                              
                                                                                
         USING TAIND,R4                                                         
         L     R4,ATAINEL          ENSURE THAT INVOICE HAS NOT BEEN             
         TM    TAINSTAT,TAINSCAN   CANCELLED                                    
         JZ    VCNL10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCNL                                  
         J     NO                                                               
                                                                                
VCNL10   TM    TAINSTA2,TAINSHLP   IF NOT A PRINTED PUR INVOICE                 
         JO    VCNL20                                                           
         TM    TAINSTAT,TAINSBIL   ENSURE THAT INVOICE HAS BEEN BILLED          
         JO    VCNL20                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCNBI                                  
         J     NO                                                               
                                                                                
VCNL20   TM    TAINSTAT,TAINSHLD   ENSURE THAT INVOICE IS NOT IN PUR            
         JZ    VCNL30              HOLD STATUS                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCPUH                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
VCNL30   TM    SVPDSTA2,TAPDSSUB   ENSURE INVOICE IS NOT A SUBSIDIARY           
         JZ    VCNL40                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCSUB                                  
         J     NO                                                               
                                                                                
VCNL40   TM    SVPDPST1,TAPDPBNP   ENSURE INVOICE IS NOT A BNP INVOICE          
         JZ    VCNL50                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCBNP                                  
         J     NO                                                               
                                                                                
         USING TAPDD,R4                                                         
VCNL50   TM    SVINSTA2,TAINSHLP   IF PUR INVOICE HAS BEEN PRINTED              
         JZ    VCNL60                                                           
         CLC   SVPDUSE,=C'GRT'     AND NOT FOR A GUARANTEE PAYMENT              
         JNE   VCNL60                                                           
         L     R4,ATAPDEL                                                       
         OC    TAPDGUAR,TAPDGUAR   ENSURE PAYMENT APPLIED AGAINST               
         JNZ   VCNL60              GUARANTEE                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCNAP                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         USING TAUCD,R4                                                         
VCNL60   L     R4,AIO3                                                          
         MVI   ELCODE,TAUCELQ      IF PAYMENT FOR 2404A COMMERCIAL              
         BRAS  RE,GETEL            ENSURE THIS IS NOT THE CANADIAN              
         JNE   YES                 INVOICE                                      
         CLC   TAUCINU,SVINKEY+TLININV-TLIND                                    
         JE    VCNL70                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCOC                                  
         J     NO                                                               
                                                                                
VCNL70   OC    TAUCINC,TAUCINC     IF CANADIAN INVOICE IS DEFINED               
         JZ    YES                                                              
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAINELQ      GET CANADIAN INVOICE STATUS ELEMENT          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TAINSTA2,TAINSHLP   IF NOT A PRINTED PUR INVOICE                 
         JO    VCNL80                                                           
         TM    TAINSTAT,TAINSBIL   ENSURE THAT INVOICE HAS BEEN                 
         JO    VCNL80              BILLED                                       
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCNB                                  
         J     NO                                                               
                                                                                
VCNL80   TM    TAINSTAT,TAINSHLD   ENSURE THAT INVOICE IS NOT IN                
         JZ    VCNL90              PUR HOLD STATUS                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERIRCCPH                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
VCNL90   L     R4,AIO3                                                          
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVBDCSF,TABDCSF     SAVE CONTRACT SERVICE FEE AMOUNT             
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCNL                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERIRCCOC DC    AL1(EIRCCOCX-*),AL2(13),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'2404A Canadian invoice - please cancel US Invoice'             
EIRCCOCX EQU   *                                                                
                                                                                
ERIRCCNL DC    AL1(EIRCCNLX-*),AL2(14),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Invoice has already been cancelled'                            
EIRCCNLX EQU   *                                                                
                                                                                
ERIRCNBI DC    AL1(EIRCNBIX-*),AL2(15),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Invoice has not been billed'                                   
EIRCNBIX EQU   *                                                                
                                                                                
ERIRCPUH DC    AL1(EIRCPUHX-*),AL2(16),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Invoice is in PUR Hold status'                                 
EIRCPUHX EQU   *                                                                
                                                                                
ERIRCSUB DC    AL1(EIRCSUBX-*),AL2(17),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Subsidiary invoice - please cancel Primary Invoice'            
EIRCSUBX EQU   *                                                                
                                                                                
ERIRCBNP DC    AL1(EIRCBNPX-*),AL2(18),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Invoice is a BNP invoice'                                      
EIRCBNPX EQU   *                                                                
                                                                                
ERIRCNAP DC    AL1(EIRCNAPX-*),AL2(19),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Printed PUR invoice did not apply against Guarantee'           
EIRCNAPX EQU   *                                                                
                                                                                
ERIRCCNB DC    AL1(EIRCCNBX-*),AL2(20),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Canadian invoice has not been billed'                          
EIRCCNBX EQU   *                                                                
                                                                                
ERIRCCPH DC    AL1(EIRCCPHX-*),AL2(21),AL1(ERRCATY3),AL1(D#IRCINV)              
         DC    C'Canadian invoice is in PUR Hold status'                        
EIRCCPHX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE REOPENS/RECALLS INVOICE AND ALL ATTACHED RECORDS     *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
*                     AIO4 = A(2404A CANADIAN INVOICE RECORD)         *         
***********************************************************************         
                                                                                
EXECREO  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQIRCACT,RQIRCARE   IF ACTION IS REOPEN/RECALL                   
         JNE   XIT                                                              
         BAS   RE,EREOINV          REOPEN INVOICE                               
         BAS   RE,EREOSCR          DELETE SCREENS                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE REOPENS/RECALLS INVOICE                              *         
*        ON ENTRY ... R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
EREOINV  NTR1                                                                   
         USING TAIND,R1                                                         
         L     R1,ATAINEL          PROCESS INVOICE STATUS ELEMENT               
         XC    TAINPINF,TAINPINF   CLEAR PAYMENT INFO                           
         XC    TAINQINF,TAINQINF         QC INFO                                
         MVI   TAINTERR,0                ERROR NUMBER                           
         NI    TAINSTAT,X'FF'-TAINSREO   STATUS BYTE 1                          
         NI    TAINSTA2,X'FF'-TAINS2RE   STATUS BYTE 2                          
         CLI   TAINLEN,TAINHDTE+L'TAINHDTE-TAIND                                
         JL    ERI10                                                            
         XC    TAINHDTE,TAINHDTE         AND PUR PRINT DATE                     
         DROP  R1                                                               
                                                                                
ERI10    MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
ERI20    BRAS  RE,NEXTEL                                                        
         JNE   ERI30                                                            
         CLI   0(R4),TAINELQ       DO NOT DELETE INVOICE STATUS                 
         JE    ERI20                                                            
         CLI   0(R4),TASIELQ       SUBSIDIARY INVOICE                           
         JE    ERI20                                                            
         CLI   0(R4),TAAIELQ       OR INVOICE ASSIGN ELEMENTS                   
         JE    ERI20                                                            
         MVI   0(R4),X'FF'         SET TO DELETE ALL OTHER ELEMENTS             
         J     ERI20                                                            
                                                                                
ERI30    GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO3),0                     
                                                                                
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',AIO3),(X'44',RQIRCSTF), +        
               SVTIME                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DELETES ATTACHED SCREEN RECORDS                      *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
         USING TLSCD,R3                                                         
EREOSCR  NTR1                                                                   
ERS10    XC    TLSCKEY,TLSCKEY     READ ALL SCREEN KEY/RECORDS                  
         MVI   TLSCCD,TLSCCDQ      FOR INVOICE                                  
         MVC   TLSCAGY,RQIRCAGY                                                 
         MVC   TLSCINV,SCRNINV                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO3'                             
         CLC   IOKEY(TLSCPG-TLSCD),IOKEYSAV                                     
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         BRAS  RE,DELRAK           DELETE RECORDS AND KEYS                      
         J     ERS10                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE CANCELS INVOICE                                      *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
*                     AIO4 = A(2404A CANADIAN INVOICE)                *         
***********************************************************************         
                                                                                
EXECCNL  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQIRCACT,RQIRCACA   IF ACTION IS CANCEL                          
         JNE   XIT                                                              
         BAS   RE,ECNLSSI          SAVE SUBSIDIARY INVOICES                     
         BAS   RE,ECNLCIN          CANCEL INVOICE                               
         BAS   RE,ECNLACI          ADD CANCELLER INVOICE                        
         BAS   RE,ECNLCOM          AND UPDATE COMMERCIAL                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE SAVES INVOICE'S SUBSIDIARY INVOICES                  *         
*        ON ENTRY ... R4 = A(INVOICE RECORD)                          *         
***********************************************************************         
                                                                                
ECNLSSI  NTR1                                                                   
         LA    R2,SUBINVS                                                       
                                                                                
         USING TASID,R3                                                         
         MVI   ELCODE,TASIELQ                                                   
         BRAS  RE,GETEL            BUILD SUBSIDIARY INVOICE TABLE               
         J     *+8                                                              
ECS10    BRAS  RE,NEXTEL                                                        
         JNE   ECS20                                                            
         MVC   0(L'TASIINV,R2),TASIINV                                          
         LA    R4,L'TASIINV(R2)                                                 
         J     ECS10                                                            
         DROP  R3                                                               
                                                                                
ECS20    MVI   0(R2),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE CANCELS INVOICE                                      *         
*        ON ENTRY ... R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
ECNLCIN  NTR1                                                                   
         USING TAIND,R1                                                         
         L     R1,ATAINEL          SET CANCELLED STATUS IN                      
         OI    TAINSTAT,TAINSCAN   INVOICE STATUS ELEMENT                       
         DROP  R1                                                               
                                                                                
         USING TAPDD,R1                                                         
         L     R1,ATAPDEL          SET CANCELLED STATUS IN                      
         OI    TAPDSTAT,TAPDSCNL   PAYMENT DETAILS ELEMENT                      
         DROP  R1                                                               
                                                                                
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),(X'44',RQIRCSTF), +        
               SVTIME                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS CANCELLER INVOICE                               *         
*        ON ENTRY ... R4 = A(INVOICE RECORD)                          *         
*                     AIO3 = A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
ECNLACI  NTR1                                                                   
         L     RF,ASVPTRS          INITIALIZE POINTER BLOCK                     
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLIND,R4                                                         
         NI    TLININV+5,X'7F'     SET CANCEL BIT IN INVOICE NUMBER             
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
ECA10    BRAS  RE,NEXTEL                                                        
         JNE   ECA60                                                            
                                                                                
         USING TASID,R4                                                         
         CLI   0(R4),TASIELQ       PROCESS SUBSIDIARY INVOICE ELEMENTS          
         JNE   ECA20                                                            
         OI    TASIINV+5,X'80'                                                  
         B     ECA10                                                            
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
ECA20    CLI   0(R4),TAPDELQ       PROCESS PAYMENT DETAILS ELEMENT              
         JNE   ECA30                                                            
         OI    TAPDINV+5,X'80'                                                  
         NI    TAPDSTA2,X'FF'-TAPDSCPO                                          
         J     ECA10                                                            
         DROP  R4                                                               
                                                                                
         USING TAUCD,R4                                                         
ECA30    CLI   0(R4),TAUCELQ       PROCESS US/CANADIAN INVOICE ELEMENT          
         JNE   ECA40                                                            
         NI    TAUCINU+5,X'7F'                                                  
         NI    TAUCINC+5,X'7F'                                                  
         J     ECA10                                                            
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
ECA40    CLI   0(R4),TAINELQ       PROCESS INVOIVE STATUS ELEMENT               
         JNE   ECA50                                                            
         MVC   SVINBDTE,TAINBDTE                                                
         NI    TAINSTAT,X'FF'-TAINSCNL                                          
         OI    TAINSTAT,TAINSCIN                                                
         XC    TAINBDTE,TAINBDTE                                                
         MVI   TAINTERR,0                                                       
         NI    TAINSTA2,ALL-TAINS2CL                                            
         CLI   TAINLEN,TAINHDTE+L'TAINHDTE-TAIND                                
         JL    ECA10                                                            
         XC    TAINHDTE,TAINHDTE                                                
         J     ECA10                                                            
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
ECA50    CLI   0(R4),TANUELQ       PROCESS PRIMARY INVOICE FOR                  
         JNE   ECA10               SPLIT BILLING ELEMENT                        
         CLI   TANUTYPE,TANUTSPL                                                
         JNE   ECA10                                                            
         OI    TANUMBER+5,X'80'                                                 
         J     ECA10                                                            
         DROP  R4                                                               
                                                                                
         USING TAOBD,R4                                                         
ECA60    LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAOBEL,TAOBELQ      ADD ORIGINAL BILL DATE ELEMENT               
         MVI   TAOBLEN,TAOBLNQ                                                  
         MVC   TAOBBDTE,SVINBDTE                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,ELEM                          
         DROP  R4                                                               
                                                                                
         BRAS  RE,REVERSE          REVERSE AMOUNT FIELDS                        
                                                                                
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE UPDATES COMMERCIAL RECORD                            *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(AIO3)                                    *         
***********************************************************************         
                                                                                
ECNLCOM  NTR1                                                                   
         OC    SVBDCSF,SVBDCSF     IF CSF WAS PAID ON THIS INVOICE ...          
         JZ    XIT                                                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ COMMERCIAL RECORD                       
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVPDCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      TURN OFF JPC CONTRACT SERVICE                
         BRAS  RE,GETEL            FEE PAID STATUS                              
         JE    *+6                                                              
         DC    H'00'                                                            
         NI    TACOSTA2,X'FF'-TACOSJPC                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES USAGE HISTORY RECORDS                        *         
*        ON ENTRY ... P1 = A(CAST SEQUENCE NUMBER)                    *         
*                     R3 = A(IOKEY)                                   *         
*                     R4 = A(AIO3)                                    *         
***********************************************************************         
                                                                                
         USING TLUHD,R3                                                         
DELUH    NTR1  BASE=*,LABEL=*                                                   
         XC    HALF1,HALF1                                                      
         ZICM  R1,0(R1),4                                                       
         JZ    DUH10                                                            
         MVC   HALF1,0(R1)         R1 = A(CAST SEQUENCE NUMBER)                 
                                                                                
DUH10    XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ      READ FOR USAGE HISTORY RECORD                
         MVC   TLUHCSEQ,HALF1                                                   
         MVC   TLUHCOM,SVPDCOM                                                  
         MVC   TLUHUSE,SVPDUSE                                                  
         MVC   TLUHINV,SVCINV                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO3'                             
         CLC   TLUHKEY,IOKEYSAV                                                 
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         BRAS  RE,DELRAK           DELETE RECORD AND KEYS                       
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES PAYMENT INFORMATION FROM ATTACHED ADVICE     *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(AIO3)                                    *         
***********************************************************************         
                                                                                
         USING TLDVD,R3                                                         
UNMADV   NTR1  BASE=*,LABEL=*                                                   
         OC    SVADV,SVADV         IF INVOICE HAS AN ATATCHED ADVICE            
         JZ    XIT                                                              
                                                                                
         XC    TLDVKEY,TLDVKEY       READ ADVICE RECORD                         
         MVI   TLDVCD,TLDVCDQ                                                   
         MVC   TLDVAGY,RQIRCAGY                                                 
         MVC   TLDVCID,SVCOCID                                                  
         MVC   TLDVADV,SVADV                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLDVSEQ-TLDVD),IOKEYSAV                                    
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         GOTOR (#SAVPTRS,ASAVPTRS)   SAVE INITIAL POIINTERS                     
                                                                                
         USING TADVD,R4                                                         
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL              GET ADVICE DETAILS ELEMENT                 
         JE    *+6                   AND CLEAR ALL PAY INFORMATION              
         DC    H'0'                                                             
         XC    TADVPINF,TADVPINF                                                
         NI    TADVSTAT,X'FF'-TADVSPAY-TADVSCMP                                 
         DROP  R4                                                               
                                                                                
         USING TAACD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAACELQ        GET COMPLETION ACTIVITY ELEMENT            
         BRAS  RE,GETEL              AND DELETE IT                              
         J     UMA20                                                            
UMA10    BRAS  RE,NEXTEL                                                        
UMA20    JNE   UMA30                                                            
         CLI   TAACSCR,X'7A'                                                    
         JNE   UMA10                                                            
         MVI   TAACEL,X'FF'                                                     
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO3),0                     
         DROP  R4                                                               
                                                                                
UMA30    GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES PROCESSES PERFORMER SPECIFIC RECORDS         *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
         USING TLCKD,R3                                                         
PROPERFS NTR1  BASE=*,LABEL=*                                                   
         XC    TLCKKEY,TLCKKEY     READ ALL CHECK RECORDS ATTACHED              
         MVI   TLCKCD,TLCKCDQ      TO THIS INVOICE                              
         MVC   TLCKAGY,RQIRCAGY                                                 
         MVC   TLCKINV,SVINV                                                    
PP10     GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOCHKDIR+IO3'                          
PP20     CLC   IOKEY(TLCKSORT-TLCKD),IOKEYSAV                                   
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOCHKFIL+IO3'                        
         DROP  R3                                                               
                                                                                
         BAS   RE,SVCHKVAR         SAVE CHECK VARIABLES                         
                                                                                
         CLI   RQIRCACT,RQIRCARE   IF ACTION IS REOPEN/RECALL                   
         JNE   PP30                                                             
         BRAS  RE,DELRAK           DELETE RECORD AND KEYS                       
         BAS   RE,DELGUA           DELETE GUARANTEE                             
                                                                                
PP30     CLI   RQIRCACT,RQIRCACA   IF ACTION IS CANCEL                          
         JNE   PP40                                                             
         BAS   RE,ADJOCK           ADJUST ORIGINAL CHECK                        
         BAS   RE,ADDCCK           AND ADD CANCELLER CHECK                      
                                                                                
PP40     BAS   RE,DELDUC           DELETE AUTO DUE COMPANY RECORD               
                                                                                
         GOTOR DELUH,DMCB,SVCKKEY+TLCKSORT+4-TLCKD                              
                                                                                
         BAS   RE,SCANCAST         SCAN CAST FOR ERRORS/NEED TO UPDATE          
         GOTO1 DELFCT,DMCB,AADDTACR DELETE FIXED CYCLE TRACKING RECORD          
         GOTO1 (RF),(R1),AAPPTACR   FOR ADDED AND APPLIED TO PAYMENTS           
         BAS   RE,UPDCAST          UPDATE CAST RECORD                           
                                                                                
         MVC   IOKEY,SVCKKEY                                                    
         CLI   RQIRCACT,RQIRCACA   SET TO READ NEXT CHECK KEY                   
         JNE   PP10                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
         J     PP20                                                             
                                                                                
***********************************************************************         
*        ROUTINE SAVES CHECK VARIABLES                                *         
*        ON ENTRY ... R4 = A(CHECK RECORD)                            *         
***********************************************************************         
                                                                                
SVCHKVAR NTR1                                                                   
         MVC   SVCKKEY,IOKEY       SAVE CHECK KEY                               
                                                                                
         MVI   CASTSTAT,0          INITIALIZE CHECK VARIABLES                   
         MVI   CASTERRS,X'FF'                                                   
         XC    SVTIID,SVTIID                                                    
         XC    AADDTACR,AADDTACR                                                
         XC    AAPPTACR,AAPPTACR                                                
                                                                                
***********************************************************************         
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGACATS          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING CATTABD,RF                                                       
         AR    RF,RE               RF=A(CATEGORY TABLE)                         
                                                                                
         USING TLCKD,R4                                                         
SCV10    CLC   TLCKCAT,CATCDE      FIND CAST CATEGORY IN                        
         JE    SCV20               CATEGORY TABLE                               
         ZIC   RE,CATLEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   SCV10                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
SCV20    MVC   SVCATYPE,CATTYPE    SAVE CATEGORY TYPE                           
         DROP  RF                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVCHKAMT,TAPDPAYC   SAVE PAYMENT AMOUNT                          
         CLI   TAPDW4TY,TAW4TYCA                                                
         JE    SCV30                                                            
         CLI   TAPDW4TY,TAW4TYCO                                                
         JE    SCV30                                                            
         CLI   TAPDW4TY,TAW4TYTR                                                
         JE    SCV30                                                            
         CLI   TAPDW4TY,TAW4TYFO                                                
         JE    SCV30                                                            
         MVC   SVCHKAMT,TAPDPAYI                                                
SCV30    MVC   SVPDACDE,TAPDACDE   APPLIED CODE                                 
         MVC   SVPDAPPL,TAPDAPPL   APPLIED AMOUNT                               
         MVC   SVPDREXP,TAPDREXP   AND REIMBURSED EXPENSES                      
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO3             GET TAX ID ELEMENT                           
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SVTIID,TATIID       SAVE CORPORATION ID                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADJUSTS ORIGINAL CHECK FOR CANCEL                    *         
*        ON ENTRY ... R4 = A(CHECK RECORD)                            *         
***********************************************************************         
                                                                                
ADJOCK   NTR1                                                                   
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE INITIAL POINTERS                        
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
AOCK10   BRAS  RE,NEXTEL                                                        
         JNE   AOCK30                                                           
                                                                                
         USING TAYED,R4                                                         
         CLI   0(R4),TAYEELQ       SET TO DELETE CHECK YTD EARNINGS             
         JNE   AOCK20              ELEMENT                                      
         CLI   TAYETYPE,TAYETCHK                                                
         JNE   AOCK10                                                           
         MVI   0(R4),X'FF'                                                      
         J     AOCK10                                                           
         DROP  R4                                                               
                                                                                
AOCK20   CLI   0(R4),TACYELQ       SET TO DELETE YTD WITHHOLDING                
         JNE   *+12                ELEMENT                                      
         MVI   0(R4),X'FF'                                                      
         J     AOCK10                                                           
                                                                                
         CLI   0(R4),TACWELQ       SET TO DELETE CHECK WITHHOLDING              
         JNE   *+12                ELEMENT                                      
         MVI   0(R4),X'FF'                                                      
         J     AOCK10                                                           
                                                                                
         CLI   0(R4),TACDELQ       SET TO DELETE CHECK DETAILS                  
         JNE   *+12                ELEMENT                                      
         MVI   0(R4),X'FF'                                                      
         J     AOCK10                                                           
                                                                                
         USING TAPDD,R4                                                         
         CLI   0(R4),TAPDELQ       PROCESS PAYMENT DETAILS ELEMENT              
         JNE   AOCK10                                                           
         OI    TAPDSTAT,TAPDSCNL   SET CANCELLED STATUS                         
         J     AOCK10                                                           
         DROP  R4                                                               
                                                                                
AOCK30   GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO3),0                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOCHKFIL+IO3'                        
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS CANCELLER CHECK FOR CANCEL                      *         
*        ON ENTRY ... R4 = A(DELETED CHECK RECORD)                    *         
***********************************************************************         
                                                                                
         USING TLCKD,R4                                                         
ADDCCK   NTR1                                                                   
         L     RF,ASVPTRS          INITIALIZE POINTER BLOCK                     
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         OI    TLCKINV+5,X'80'     TURN ON CANCELLED BIT IN INVOICE             
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
ACCK10   BRAS  RE,NEXTEL                                                        
         JNE   ACCK50                                                           
                                                                                
         USING TAYED,R4                                                         
         CLI   0(R4),TAYEELQ       PROCESS YTD EARNINGS ELEMENT                 
         JNE   ACCK20              FOR BILLING                                  
         CLI   TAYETYPE,TAYETBIL                                                
         JNE   ACCK10                                                           
         ZIC   R1,TAYELEN                                                       
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         XC    TAYEEL+3(0),TAYEEL+3                                             
         J     ACCK10                                                           
         DROP  R4                                                               
                                                                                
         USING TABYD,R4                                                         
ACCK20   CLI   0(R4),TABYELQ       PROCESS BILLING YTD ELEMENT                  
         JNE   ACCK30                                                           
         MVC   BYTE1,TABYLVL                                                    
         ZIC   R1,TABYLEN                                                       
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         XC    TABYEL+2(0),TABYEL+2                                             
         MVC   TABYLVL,BYTE1                                                    
         J     ACCK10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
ACCK30   CLI   0(R4),TAPDELQ       PROCESS PAYMENT DETAILS ELEMENT              
         JNE   ACCK40                                                           
         OI    TAPDINV+5,X'80'                                                  
         NI    TAPDSTAT,X'FF'-TAPDSGUP-TAPDSGOF                                 
         J     ACCK10                                                           
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
ACCK40   CLI   0(R4),TANUELQ       PROCESS PRIMARY INVOICE FOR                  
         JNE   ACCK10              SPLIT BILLING ELEMENT                        
         CLI   TANUTYPE,TANUTSPL                                                
         JNE   ACCK10                                                           
         OI    TANUMBER+5,X'80'                                                 
         J     ACCK10                                                           
         DROP  R4                                                               
                                                                                
ACCK50   BRAS  RE,REVERSE          REVERSE ALL AMOUNT FIELDS                    
                                                                                
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DELETES ATTACHED GUARANTEE RECORDS                   *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(CHECK RECORD)                            *         
***********************************************************************         
                                                                                
DELGUA   NTR1                                                                   
         CLC   SVPDUSE,=C'GRT'     IF INVOICE WAS FOR NON-CREDIT                
         JNE   XIT                 GUARANTEE PAYMENT ...                        
         TM    SVPDPST1,TAPDPCRD                                                
         JO    XIT                                                              
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TLGTD,R3                                                         
         XC    TLGTKEY,TLGTKEY     READ FOR GUARANTEE TRACKING                  
         MVI   TLGTCD,TLGTCDQ                                                   
         MVC   TLGTSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         MVC   TLGTGUA,TACAGUA                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLGTSTRT-TLGTD),IOKEYSAV                                   
         JNE   DGUA10                                                           
         OI    CASTSTAT,GHASTRK    IF FOUND, SET TO NOT DELETE GRT              
         DROP  R3                                                               
                                                                                
         USING TLGUD,R3                                                         
DGUA10   XC    TLGUKEY,TLGUKEY     READ GUARANTEE KEY/RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         MVC   TLGUGUA,TACAGUA                                                  
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3,R4                                                            
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    TAGUINV,TAGUINV     IF GUARANTEE WAS ADDED BY PAY                
         JZ    XIT                                                              
         CLC   TAGUINV,SVINV       VIA THE INVOICE BEING REOPENED               
         JNE   XIT                                                              
         OC    TAGUIAY,TAGUIAY                                                  
         JZ    DGUA20                                                           
         CLC   TAGUIAY,RQIRCAGY                                                 
         JNE   XIT                                                              
DGUA20   TM    TAGUSTAT,TAGUSINS   ENSURE THAT AN INSTALLMENT PAYMENT           
         JZ    DGUA30              HAS NOT BEEN MADE                            
         GOTOR ADDCERR,DMCB,TACEEGRT                                            
         J     XIT                                                              
                                                                                
DGUA30   BRAS  RE,DELRAK           DELETE GUARANTEE RECORD AND KEY              
         OI    CASTSTAT,GDELETD+NEEDUPD     AND SET TO UPDATE CAST              
         J     XIT                                                              
                                                                                
*&&DO                                                                           
DGUA50   L     RE,TAGUAMT          IF THIS IN AN INSTALLMENT PAYMENT            
         S     RE,SVCHKAMT         SUBTRACT PAYMENT AMOUNT FROM                 
         ST    RE,TAGUAMT          GUARANTEE'S AMOUNT AND BALANCE               
         L     RE,TAGUBAL          AND PUT BACK GUARANTEE                       
         S     RE,SVCHKAMT                                                      
         ST    RE,TAGUBAL                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
*&&                                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE DELETES AUTO DUE COMPANY RECORD                      *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
DELDUC   NTR1                                                                   
         TM    SVPDPST1,TAPDPCRD   IF INVOICE WAS FOR A NON-BNP                 
         JZ    XIT                 CREDIT INVOICE WITH A PAYMENT                
         TM    SVPDPST1,TAPDPBNP   OR REIMBURSED EXPENSE AMOUNT ...             
         JO    XIT                                                              
         OC    SVCHKAMT,SVCHKAMT                                                
         JNZ   DDUC10                                                           
         OC    SVPDREXP,SVPDREXP                                                
         JZ    XIT                                                              
                                                                                
         USING TLDUD,R3                                                         
DDUC10   XC    TLDUKEY,TLDUKEY     READ DUE COMPANY KEYS                        
         MVC   TLDUSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         OC    SVTIID,SVTIID                                                    
         JZ    *+10                                                             
         MVC   TLDUSSN,SVTIID                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO3'                             
         J     DDUC30                                                           
DDUC20   GOTOR (#IOEXEC,AIOEXEC),'IOSQUP+IODIR+IO3'                             
DDUC30   CLC   IOKEY(TLDUDUC-TLDUD),IOKEYSAV                                    
         JE    DDUC40                                                           
         DROP  R3                                                               
                                                                                
         GOTOR ADDCERR,DMCB,TACEEDUE                                            
         J     XIT                 IF NOT FOUND, GIVE CAST ERROR                
                                                                                
DDUC40   GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         USING TADUD,R4                                                         
         MVI   ELCODE,TADUELQ      GET DUE COMPANY DETAILS ELEMENT              
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TADUSTAT,TADUSAUT   IF DUE COMPANY WAS AUTO-GENERATE             
         JZ    DDUC20              BY THIS INVOICE NUMBER                       
         CLC   TADUCINV,SVINV                                                   
         JNE   DDUC20                                                           
         BRAS  RE,DELRAK           DELETE DUE COMPANY RECORD AND KEYS           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE SCANS CAST RECORD TO GATHER INFORMATION              *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(AIO3)                                    *         
***********************************************************************         
                                                                                
SCANCAST NTR1                                                                   
         TM    SVCATYPE,EXTRA      IF PERFORMER WAS AN EXTRA                    
         JO    XIT                 DO NOT SCAN CAST RECORD                      
                                                                                
         CLI   CASTERRS,X'FF'      ELSE IF ERRORS HAVE BEEN ENCOUNTERED         
         JNE   SC10                                                             
         CLC   SVPDUSE,=C'DLR'     OR INVOICE WAS FOR DEALER PAYMENT            
         JE    SC10                                                             
         TM    SVUSSTA2,APPREUSE   OR REUSE APPLIES AGAINST INVOICE'S           
         JO    SC10                PAYMENT TYPE                                 
         CLI   SVPDACDE,APPLHLD    OR PAYMENT APPLIED AGAINST A                 
         JE    SC10                HOLDING FEE                                  
         CLC   SVPDUSE,=C'MUS'     OR INVOICE WAS FOR A NON-MUS                 
         JE    XIT                 PAYMENT THAT APPLIED AGAINST A               
         CLI   SVPDACDE,APPLSESS   SESSION                                      
         JNE   XIT                 SCAN THE CAST RECORD                         
                                                                                
         USING TLCAD,R3                                                         
SC10     XC    TLCAKEY,TLCAKEY     READ CAST KEY/RECORD                         
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,SVPDCOM                                                  
         MVC   TLCASORT,SVCKKEY+TLCKSORT-TLCKD                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCASSN-TLCAD),IOKEYSAV                                    
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         CLC   SVPDUSE,=C'DLR'     IF INVOICE WAS FOR DEALER PAYMENT            
         JE    SC20                                                             
         TM    SVUSSTA2,APPREUSE   OR REUSE APPLIES AGAINST INVOICE'S           
         JZ    SC60                PAYMENT TYPE ...                             
                                                                                
         MVC   SVCRCYC,SVPDCYC     INITIALIZE FTRACK CYCLE                      
                                                                                
         USING TACRD,R4                                                         
SC20     MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL            READ FOR FTRACK THAT INVOICE ADDED           
         J     *+8                                                              
SC30     BRAS  RE,NEXTEL                                                        
         JNE   SC40                                                             
         CLC   TACRINV,SVINV                                                    
         JNE   SC30                                                             
                                                                                
         OI    CASTSTAT,NEEDUPD    IF FOUND, SET TO UPDATE CAST                 
         MVC   SVCRCYC,TACRSTRT    SAVE FTRACK CYCLE                            
         ST    R4,AADDTACR         AND A(ADDED APPLIED CREDIT ELEMENT)          
                                                                                
         CLC   SVPDUSE,=C'ADC'     IF INVOICE NOT FOR ADC PAYMENT               
         JE    XIT                                                              
         L     R1,TACRBAL          AND FTRACK HAS ALREADY BEEN                  
         C     R1,TACRAPPL         CREDITED, GIVE CAST ERROR                    
         JNL   SC50                                                             
         GOTOR ADDCERR,DMCB,TACEEBAL                                            
         J     SC50                                                             
         DROP  R4                                                               
                                                                                
SC40     CLC   SVPDUSE,=C'DLR'      IF NOT FOUND AND INVOICE WAS                
         JE    SC50                 NOT FOR A DEALER PAYMENT                    
         GOTOR ADDCERR,DMCB,TACEENFT  FOUND, GIVE CAST ERROR                    
         J     XIT                                                              
                                                                                
SC50     CLC   SVPDUSE,=C'DLR'     IF INVOICE WAS FOR A DEALER PAYMENT          
         JNE   XIT                                                              
         CLI   SVPDACDE,APPLHLD    AND PAYMENT APPLIED AGAINST A                
         JE    SC60                HOLDING FEE                                  
         CLI   SVPDACDE,APPLSESS   OR SESSION                                   
         JNE   XIT                 PROCESS THE APPLICATION AS WELL              
                                                                                
***********************************************************************         
                                                                                
SC60     XC    DUB1,DUB1           IF PAYMENT APPLIED AGAINST SESSION           
         XR    R2,R2               OR HOLDING FEE ...                           
                                                                                
         LA    R1,SVPDCYC                                                       
         OC    0(3,R1),0(R1)                                                    
         JNZ   *+8                                                              
         LA    R1,SVINPDTE         SET PAYMENT APPLY DATE                       
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL            READ FOR FTRACK CYCLE THAT                   
         J     *+8                 INVOICE APPLIED AGAINST                      
SC70     BRAS  RE,NEXTEL                                                        
         JNE   SC100                                                            
         CLC   0(3,R1),TACRSTRT                                                 
         JL    SC70                                                             
         CLC   0(3,R1),TACREND                                                  
         JH    SC70                                                             
                                                                                
         OC    TACRINV,TACRINV     SAVE THE INVOICE NUMBER OF THE               
         JZ    SC80                LATEST FTRACK THAT THE APPLY                 
         CLC   TACRINV,DUB1        DATE FITS WITHIN                             
         JNH   SC70                                                             
                                                                                
SC80     CLC   TACRUSE,=C'GRR'     IF FTRACK WAS CREATED BY A GRR               
         JNE   SC90                                                             
         CLI   TACRTYPE,0          TO COVER A SPECIFIC USE                      
         JE    SC90                                                             
         CLC   TACRTYPE,SVUSEQU    REJECT IF THIS IS NOT THAT USE               
         JNE   SC90                                                             
         OI    CASTSTAT,GRRUSES    TURN ON STATUS FOR TRACKING                  
                                                                                
SC90     LR    R2,R4               SAVE A(FTRACK) AND INOICE NUMBER             
         MVC   DUB1(L'TACRINV),TACRINV                                          
         J     SC70                                                             
                                                                                
SC100    LTR   R4,R2               IF NO MATCHING FTRACKS WERE FOUND            
         JNZ   SC110               GIVE CAST ERROR                              
         GOTOR ADDCERR,DMCB,TACEENFR                                            
         J     XIT                                                              
                                                                                
SC110    MVC   SVCRCYC,TACRSTRT    SAVE FTRACK CYCLE                            
         MVC   SVCRBAL,TACRBAL     INITIAL BALANCE                              
         ST    R4,AAPPTACR         AND A(APPLIED TO APPLIED CREDIT EL)          
                                                                                
         TM    TACRBAL,X'80'       IF FOUND BUT BALANCE IS NEGATIVE             
         JZ    SC120               GIVE CAST ERROR                              
         GOTOR ADDCERR,DMCB,TACEENEG                                            
         J     XIT                                                              
                                                                                
SC120    L     R1,TACRBAL          RESTORE APPLIED AMOUNT TO                    
         S     R1,SVPDAPPL         CURRENT BALANCE                              
                                                                                
         C     R1,TACRAPPL         IF BALANCE NOW EXCEEDS INITIAL               
         JNH   SC130               AMOUNT, GIVE CAST ERROR                      
         GOTOR ADDCERR,DMCB,TACEEFUL                                            
         L     R1,TACRAPPL                                                      
                                                                                
SC130    ST    R1,SVCRBAL          SAVE NEW BALANCE                             
         OI    CASTSTAT,NEEDUPD    AND SET TO UPDATE CAST                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DELETES FIXED CYCLE TRACKING RECORDS                 *         
*        ON ENTRY ... P1 = A(ADDED APPLIED CREDIT ELEMENT OR APPLIED  *         
*                            TO APPLIED CREDIT ELEMENT)               *         
*                     R3 = A(IOKEY)                                   *         
*                     R4 = A(AIO3)                                    *         
***********************************************************************         
                                                                                
DELFCT   NTR1                                                                   
         OC    0(L'AADDTACR,R1),0(R1)    IF CAST'S FIXED CYCLE ELEMENT          
         JZ    XIT                       WILL BE UPDATED ...                    
                                                                                
         USING TLFTD,R3                                                         
         XC    TLFTKEY,TLFTKEY     READ FIXED CYCLE TRACKING KEY/RECORD         
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSSN,SVCKKEY+TLCKSSN-TLCKD                                    
         MVC   TLFTCOM,SVPDCOM                                                  
         MVC   TLFTCAST,SVCKKEY+TLCKSORT+4-TLCKD                                
         MVC   TLFTSTRT(6),SVCRCYC                                              
         XC    TLFTSTRT(6),=6X'FF'                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO3'                             
         J     EDF20                                                            
EDF10    GOTOR (#IOEXEC,AIOEXEC),'IOSQUP+IODIR+IO3'                             
EDF20    CLC   IOKEY(TLFTTRK-TLFTD),IOKEYSAV                                    
         JNE   EDF30                                                            
                                                                                
         CLC   TLFTINV,SVINV       IF FIRST KEY IS NOT FOR THIS INVOICE         
         JE    EDF40                                                            
         TM    CASTSTAT,GRRUSES    AND NOT APPLYING AGAINST                     
         JO    EDF10               USE-SPECIFIC GRR, GIVE CAST ERROR            
         GOTOR ADDCERR,DMCB,TACEESUB                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
EDF30    GOTOR ADDCERR,DMCB,TACEENTR   IF NOT FOUND AT ALL                      
         J     XIT                     GIVE CAST ERROR                          
                                                                                
EDF40    GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         BRAS  RE,DELRAK           IF FOUND, DELETE RECORD AND KEY              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE PROCESSED CAST INFORMATION                           *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
UPDCAST  NTR1                                                                   
         TM    CASTSTAT,NEEDUPD    IF CAST NEEDS TO BE UPDATED                  
         JZ    XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     READ CAST KEY/RECORD                         
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,SVPDCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     UCAST20                                                          
UCAST10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
UCAST20  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   XIT                                                              
         CLC   TLCASEQ,SVCKKEY+4                                                
         JNE   UCAST10                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE INITIAL POINTERS                        
                                                                                
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ      MARK EXISTING REOPEN ACTIVITY                
         BRAS  RE,GETEL            ELEMENT AS DELETED                           
         J     *+8                                                              
UCAST30  BRAS  RE,NEXTEL                                                        
         JNE   UCAST40                                                          
         CLI   TAACSCR,X'44'                                                    
         JNE   UCAST30                                                          
         MVI   0(R4),X'FF'                                                      
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
UCAST40  L     R4,AIO3                                                          
                                                                                
         CLC   SVPDUSE,=C'GRT'     IF INVOICE WAS FOR GUARANTEE                 
         JNE   UCAST50             PAYMENT                                      
         TM    CASTSTAT,GDELETD    AND GUARANTEE HAS BEEN DELETED               
         JZ    UCAST50                                                          
                                                                                
         USING TLCAD,R4                                                         
         OI    TLCASORT,X'02'      TURN OFF KEY'S GUARANTEE STATUS              
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            GET CAST DETAILS ELEMENT                     
         JE    *+6                                                              
         DC    H'0'                                                             
         XC    TACAGUA,TACAGUA     AND CLEAR GUARANTEE CODE                     
         J     UCAST80                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TACRD,R4                                                         
UCAST50  L     R4,AADDTACR                                                      
         LTR   R4,R4               IF FTRACK THAT WAS ADDED MUST BE             
         JZ    UCAST60             DELETED, DELETE IT                           
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,AIO3,(X'44',RQIRCSTF)                   
         MVI   0(R4),X'FF'         AND ADD ACTIVITY ELEMENT                     
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TACRD,R4                                                         
UCAST60  L     R4,AAPPTACR                                                      
         LTR   R4,R4               IF FTRACK THAT WAS APPLIED TO                
         JZ    UCAST70             MUST BE ADJUSTED                             
         MVC   TACRBAL,SVCRBAL     UPDATE FTRACK'S BALANCE                      
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
UCAST70  GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO3),0                     
                                                                                
         CLI   CASTERRS,X'FF'      IF A CAST ERROR WAS ENCOUNTERED              
         JE    UCAST90             OUTPUT DETAILS                               
                                                                                
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#IRCWRN)              
                                                                                
         GOTO1 VHEXOUT,(R1),SVCKKEY+TLCKSORT+4-TLCKD,OUTPUT,2,0                 
         GOTOR VLINKIO,(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),           +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
         GOTO1 VDATCON,(R1),(1,SVCRCYC),(8,OUTPUT)                              
         GOTO1 VLINKIO,(R1),('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VDATCON,(R1),(1,SVCRCYC+3),(8,OUTPUT)                            
         GOTO1 VLINKIO,(R1),('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
         LA    R2,CASTERRS                                                      
         LHI   R0,4                                                             
UCAST80  GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',(R0)),         +        
               ('LD_UBINQ',0(R2)),(1,0)                                         
         AHI   R0,1                                                             
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         JNE   UCAST80                                                          
         DROP  R5                                                               
                                                                                
         SHI   R0,4                                                             
                                                                                
         USING TACED,R4                                                         
         LA    R4,ELEM             ADD CAST ERROR ELEMENT                       
         XC    ELEM,ELEM                                                        
         MVI   TACEEL,TACEELQ                                                   
         STC   R0,TACENUM                                                       
         AHI   R0,TACELNQ                                                       
         STC   R0,TACELEN                                                       
         MVC   TACEERR(L'CASTERRS),CASTERRS                                     
         GOTO1 VDATCON,DMCB,(5,0),(1,TACEDATE)                                  
         TIME  DEC                                                              
         STCM  R0,14,TACETIME                                                   
         MVC   TACECYC,SVCRCYC                                                  
         MVC   TACEINV,SVINV                                                    
         MVC   TACEUSEQ,SVUSEQU                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,ELEM                          
         DROP  R4                                                               
                                                                                
UCAST90  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES RECORD AND KEYS                              *         
*        ON ENTRY ... R3 = A(KEY OF RECORD TO DELETE)                 *         
***********************************************************************         
                                                                                
DELRAK   NTR1  BASE=*,LABEL=*                                                   
         MVC   HALF1,=AL2(IOFIL)   DETERMINE FILE BY RECORD EQUATE              
         CLI   0(R3),TLCKCDQ                                                    
         JNE   DRAK10                                                           
         MVC   HALF1,=AL2(IOCHKFIL)                                             
                                                                                
DRAK10   GOTOR (#SAVPTRS,ASAVPTRS) SAVE INITIAL POINTERS                        
                                                                                
         USING TLRCD,R4                                                         
         L     R4,AIO3             MARK RECORD DELETED                          
         OI    TLRCSTAT,X'80'                                                   
         LH    R1,HALF1                                                         
         AHI   R1,IOPUTREC+IO3                                                  
         GOTOR (#IOEXEC,AIOEXEC)                                                
         DROP  R4                                                               
                                                                                
         GOTOR (#UPDPTRS,AUPDPTRS) MARK KEYS DELETED                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS CAST ERROR TO CAST ERROR TABLE                  *         
*        ON ENTRY ... P1 = CAST ERROR EQUATE                          *         
***********************************************************************         
                                                                                
ADDCERR  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,CASTERRS                                                      
ACE10    CLI   0(RE),X'FF'                                                      
         JE    ACE20                                                            
         LA    RE,1(RE)                                                         
         J     ACE10                                                            
ACE20    MVC   0(1,RE),3(R1)                                                    
         MVI   1(RE),X'FF'                                                      
         OI    CASTSTAT,NEEDUPD                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE REVERSES ALL AMOUNTS IN INVOICE/CHECK RECORD         *         
*        ON ENTRY ... AIO3 = A(INVOICE OR CHECK RECORD)               *         
***********************************************************************         
                                                                                
REVERSE  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3             LOOP THROUGH RECORD                          
                                                                                
       ++INCLUDE TAREVERSE                                                      
                                                                                
REVX     J     XIT                                                              
                                                                                
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
* REQUEST MAP - INVOICE REOPEN/RECALL/CANCEL UPLOAD                   *         
***********************************************************************         
                                                                                
INHDR    LKMAP H,I#IRCULD,NEWREC=Y                                              
F$ACT    LKMAP F,D#IRCACT,UBIN,TA#PACTN,OLEN=L'RQIRCACT,MAXLEN=1,      *        
               OUTPUT=(D,B#SAVED,RQIRCACT)                                      
F$MOD    LKMAP F,D#IRCMOD,UBIN,TA#PMODE,OLEN=L'RQIRCMOD,MAXLEN=1,      *        
               OUTPUT=(D,B#SAVED,RQIRCMOD)                                      
F$STF    LKMAP F,D#IRCSTF,CHAR,TA#STAFF,MAXLEN=L'RQIRCSTF,             +        
               OUTPUT=(D,B#SAVED,RQIRCSTF)                                      
F$AGY    LKMAP F,D#IRCAGY,CHAR,TA#AGYCD,MAXLEN=L'RQIRCAGY,             *        
               OUTPUT=(D,B#SAVED,RQIRCAGY)                                      
F$INV    LKMAP F,D#IRCINV,CHAR,TA#INV,MAXLEN=L'RQIRCINV,               *        
               OUTPUT=(D,B#SAVED,RQIRCINV)                                      
F$WID    LKMAP F,D#IRCWID,CHAR,TA#WAPID,MAXLEN=L'RQIRCWID,             +        
               OUTPUT=(D,B#SAVED,RQIRCWID)                                      
F$EOV    LKREQ F,D#IRCEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,              *        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
                                                                                
SVINKEY  DS    XL(L'IOKEY)         SAVED INVOICE KEY                            
SVCKKEY  DS    XL(L'IOKEY)         SAVED CHECK KEY                              
                                                                                
SVINV    DS    XL(L'TLININV)       SAVED CONVERTED INVOICE NUMBER               
SVCINV   DS    XL(L'TLININV)       SAVED COMPLEMENTED INVOICE NUMBER            
SCRNINV  DS    XL(L'TLININV)       INVOICE NUMBER FOR SCREENS                   
                                                                                
ATAINEL  DS    A                   A(INVOICE STATUS ELEMENT)                    
SVINSTA2 DS    XL(L'TAINSTA2)      SAVED INVOICE STATUS 2                       
SVINPDTE DS    XL(L'TAINPDTE)      SAVED PAY DATE                               
SVINBDTE DS    XL(L'TAINBDTE)      SAVED BILL DATE                              
                                                                                
ATAPDEL  DS    A                   A(PAYMENT DETAILS ELEMENT)                   
SVPDCOM  DS    XL(L'TAPDCOM)       SAVED INTERNAL COMMERCIAL NUMBER             
SVPDUSE  DS    XL(L'TAPDUSE)       SAVED USE CODE                               
SVPDCYC  DS    XL6                 SAVED PAYMENT CYCLE                          
SVPDACDE DS    CL(L'TAPDACDE)      SAVED APPLIED CODE                           
SVPDAPPL DS    F                   SAVED APPLIED AMOUNT                         
SVPDSTA2 DS    XL(L'TAPDSTA2)      SAVED PAYMENT STATUS 2                       
SVPDPST1 DS    XL(L'TAPDPST1)      SAVED PAYMENT OPTIONS                        
SVUSEQU  DS    X                   SAVED USE EQUATE                             
SVUSSTA2 DS    X                   SAVED 2ND USE STATUS                         
                                                                                
SVBDCSF  DS    XL(L'TABDCSF)       SAVED CSF AMOUNT                             
                                                                                
SVCOCID  DS    CL(L'TACOCID)       SAVED COMMERCIAL ID                          
                                                                                
SVUCINC  DS    CL(L'TAUCINC)       SAVED CANADIAN INVOICE                       
                                                                                
SVCATYPE DS    XL1                 SAVED CAST CATEGORY TYPE                     
SVCAGUA  DS    CL(L'TACAGUA)       SAVED CAST GUARANTEE CODE                    
SVCRCYC  DS    XL6                 SAVED FTRACK CYCLE                           
SVCRBAL  DS    F                   SAVED FTRACK BALANCE                         
AADDTACR DS    A                   A(ADDED APPLIED CREDIT ELEMENT)              
AAPPTACR DS    A                   A(APPLIED TO APPLIED CREDIT ELEMENT)         
                                                                                
SVADV    DS    CL(L'TLDVADV)       SAVED ADVICE NUMBER                          
                                                                                
SVWID    DS    CL18                SAVED WEB APPLICATION ID                     
                                                                                
SVCHKAMT DS    F                   SAVED CHECK AMOUNT                           
SVPDREXP DS    F                   SAVED REIMBURSED EXPENSES                    
                                                                                
SVTIID   DS    CL(L'TATID)         SAVED CORPORATION ID                         
                                                                                
CASTSTAT DS    X                   CHECK STATUS                                 
GHASTRK  EQU   X'80'               GUARANTEE HAS TRACKING                       
GDELETD  EQU   X'40'               GUARANTEE DELETED                            
GRRUSES  EQU   X'20'               USE SPECIFIC GRR PAYMENT                     
NEEDUPD  EQU   X'10'               NEEDS TO BE UPDATED                          
                                                                                
CASTERRS DS    XL10                CHECK ERROR TABLE                            
                                                                                
SUBINVS  DS    11CL(L'TASIINV)     SUBSIDIARY INVOICE TABLE                     
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        W4 MAINTENANCE REQUEST MAP FIELDS                            *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQIRCACT DS    CL1                 ACTION                                       
RQIRCARE EQU   1                   REOPEN/RECALL                                
RQIRCACA EQU   2                   CANCEL                                       
RQIRCMOD DS    CL1                 MODE                                         
RQIRCRTV EQU   1                   RETRIEVE                                     
RQIRCVFY EQU   2                   VERIFY                                       
RQIRCEXE EQU   3                   EXECUTE                                      
RQIRCSTF DS    CL8                 STAFF ID                                     
RQIRCAGY DS    CL6                 AGENCY                                       
RQIRCINV DS    CL6                 INVOICE NUMBER                               
RQIRCWID DS    CL18                WEB APPLICATION ID                           
RQRCLNQ  EQU   *-RQIRCACT                                                       
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK1A   08/29/13'                                      
         END                                                                    
