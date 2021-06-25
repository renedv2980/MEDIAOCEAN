*          DATA SET TALNK1B    AT LEVEL 001 AS OF 08/29/13                      
*PHASE T7041BC                                                                  
TALNK1B  TITLE 'GUARANTEE MAINTENANCE UPLOAD SERVER'                            
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TAGU,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
WORKLNQ  EQU   ERRTAB                                                           
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1B**,RR=RE                                           
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
                                                                                
INPUT    BRAS  RE,GUUPLOAD         PROCESS THE INPUT RECORD                     
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
*        PROCESS GUARANTEE MAINTENANCE UPLOAD REQUEST                 *         
***********************************************************************         
                                                                                
GUUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#GUULD)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         ZICM  RF,I$SUAC+1,3                                                    
         MVC   ISUBAC,9(RF)        SET SUBSIDIARY AGENCY/CLIENT COUNT           
         LA    RF,10(RF)                                                        
         STCM  RF,7,ASUBAC         SET A(SUB AGENCY/CLIENT ARRAY)               
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQGUSTF                                   
         JNE   GUUP20                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         BRAS  RE,VALSSN           VALIDATE SOCIAL SECURITY NUMBER              
         JNE   GUUP20                                                           
                                                                                
         BRAS  RE,VALATC           VALIDATE ATTACHED CORPORATION                
         BRAS  RE,VALAGY           VALIDATE PRIMARY AGENCY                      
         BRAS  RE,VALCLI           VALIDATE PRIMARY CLIENT                      
         BRAS  RE,VALCOM           VALIDATE PRIMARY COMMERCIAL                  
         BRAS  RE,VALEXU           VALIDATE EXCLUDED USES                       
         BRAS  RE,VALSAC           VALIDATE SUBSIDIARY AGENCY/CLIENTS           
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      READ FOR GUARANTEE KEY                       
         MVC   TLGUSSN,RQGUSSN                                                  
         MVC   TLGUGUA,RQGUGUA                                                  
         XC    TLGUGUA,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    GUUP10                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         J     GUUP20                                                           
                                                                                
GUUP10   MVI   ACTION,ACTCHA       IF FOUND, SET ACTION TO CHANGE               
         MVC   SVGUKEY,IOKEY       AND SAVE GUARANTEE KEY                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         BRAS  RE,VALGUA           VALIDATE GUARANTEE                           
         JNE   GUUP20                                                           
         BRAS  RE,VALTYP           AND VALIDATE TYPE                            
         JNE   GUUP20                                                           
         BRAS  RE,VALACS           PERFORM ADDITIONAL AGENCY/CLIENT             
         BRAS  RE,VALCO2           AND PRIMARY COMMERCIAL VALIDATION            
         BRAS  RE,VALPER           VALIDATE PERIOD                              
         BRAS  RE,VALAMT           AMOUNT                                       
         BRAS  RE,VALBAL           BALANCE                                      
         BRAS  RE,VALPNH           AND PAY P&H ON USE                           
         BRAS  RE,INITCHA          AND PREPARE TO CHANGE                        
                                                                                
GUUP20   MVI   OUTPUT,GUSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    GUUP30                                                           
         MVI   OUTPUT,GUSTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    GUUP30                                                           
         MVI   OUTPUT,GUSTOK2                                                   
                                                                                
GUUP30   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQGUMOD,RQGUEXE     IF MODE IS EXECUTE                           
         JNE   GUUP40                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    GUUP40                                                           
         BRAS  RE,EXECADD          PREPARE TO ADD                               
         BRAS  RE,EXECCHA          OR CHANGE GUARANTEE RECORD                   
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQGUSTF),(L'RQGUSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
GUUP40   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',RQGUSSN),(L'RQGUSSN,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',5),               +        
               ('LD_CHARQ',RQGUGUA),(L'RQGUGUA,0)                               
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#GUERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    YES                 SEND DOWN GUARANTEE RECORD                   
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#GUULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#GUSDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#GUSSTF),        +        
               ('LD_CHARQ',RQGUSTF),(L'RQGUSTF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#GUSSSN),        +        
               ('LD_CHARQ',RQGUSSN),(L'RQGUSSN,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#GUSGUA),        +        
               ('LD_CHARQ',RQGUGUA),(L'RQGUGUA,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
GUSTOK1  EQU   1                   NO ERRORS - GUARANTEE ADDED                  
GUSTOK2  EQU   2                   NO ERRORS - GUARANTEE CHANGED                
GUSTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED SOCIAL SECURITY NUMBER                       *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLW4D,R3                                                         
VALSSN   NTR1  BASE=*,LABEL=*                                                   
         XC    TLW4KEY,TLW4KEY     READ FOR W4 KEY/RECORD                       
         MVI   TLW4CD,TLW4CDQ      AND ENSURE IT EXISTS                         
         MVC   TLW4SSN,RQGUSSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JE    VSSN10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUW4NF                                  
         J     NO                                                               
                                                                                
VSSN10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALSSN                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUW4NF DC    AL1(EGUW4NFX-*),AL2(1),AL1(ERRCATY3),AL1(D#GUSSN)                
         DC    C'W4 not on file'                                                
EGUW4NFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED ATTACHED CORPORATION FID                     *         
*        ON ENTRY ... AIO5=A(W4 RECORD)                               *         
***********************************************************************         
                                                                                
VALATC   NTR1  BASE=*,LABEL=*                                                   
         OC    RQGUATC,RQGUATC     ONLY VALIDATE IF ATTACHED                    
         JZ    XIT                 CORPORATION FID HAS BEEN PASSED              
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TATIELQ      READ THROUGH ALL TAX ID ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VATC10   BRAS  RE,NEXTEL                                                        
         JNE   VATC20                                                           
         CLI   TATITYPE,TATITYCO                                                
         JNE   VATC10                                                           
         CLC   RQGUATC,TATIID      IF FOUND, SAVE CORPORATION NUMBER            
         JNE   VATC10              ELSE, RETURN ERROR                           
         MVC   SVCORP,TATICRPN                                                  
         JE    XIT                                                              
         DROP  R4                                                               
                                                                                
VATC20   GOTOR (#ADDERR,AADDERR),DMCB,ERGUINCP                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALATC                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUINCP DC    AL1(EGUINCPX-*),AL2(4),AL1(ERRCATY1),AL1(D#GUFID)                
         DC    C'Corporation FID# not attached to Performer'                    
EGUINCPX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AGENCY CODE                                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLAYD,R3                                                         
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY                          
         MVI   TLAYCD,TLAYCDQ      AND ENSURE IT EXISTS                         
         MVC   TLAYAGY,RQGUAGY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUAYNF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGY                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUAYNF DC    AL1(EGUAYNFX-*),AL2(6),AL1(ERRCATY1),AL1(D#GUAGY)                
         DC    C'Agency not on file'                                            
EGUAYNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED CLIENT CODE                                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCLI   NTR1  BASE=*,LABEL=*                                                   
         OC    RQGUCLI,RQGUCLI     ONLY VALIDATE IF PRIMARY CLIENT              
         JZ    XIT                 CODE HAS BEEN PASSED                         
                                                                                
         USING TLCLD,R3                                                         
         XC    TLCLKEY,TLCLKEY     READ FOR CLIENT KEY                          
         MVI   TLCLCD,TLCLCDQ      AND ENSURE IT EXISTS                         
         MVC   TLCLAGY,RQGUAGY                                                  
         MVC   TLCLCLI,RQGUCLI                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUCLNF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCLI                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUCLNF DC    AL1(EGUCLNFX-*),AL2(8),AL1(ERRCATY1),AL1(D#GUCLI)                
         DC    C'Client not on file'                                            
EGUCLNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED PRIMARY INTERNAL COMMERCIAL NUMBER           *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCOM   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUTYP,RQGUTYPP    ONLY VALIDATE IF TYPE IS PER CYCLE           
         JNE   XIT                                                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY/RECORD               
         MVI   TLCOPCD,TLCOCCDQ    AND ENSURE IT EXISTS                         
         MVC   TLCOCCOM,RQGUCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    VCOM10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUCONF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VCOM10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TLCOD,R4                                                         
         CLC   TLCOAGY,RQGUAGY     ENSURE COMMERCIAL EXISTS UNDER               
         JNE   VCOM20              PRIMARY AGENCY/CLIENT                        
         CLC   TLCOCLI,RQGUCLI                                                  
         JE    XIT                                                              
         OC    RQGUCLI,RQGUCLI                                                  
         JZ    XIT                                                              
VCOM20   GOTOR (#ADDERR,AADDERR),DMCB,ERGUCOAC                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUCONF DC    AL1(EGUCONFX-*),AL2(24),AL1(ERRCATY1),AL1(D#GUCOM)               
         DC    C'Commercial not on file'                                        
EGUCONFX EQU   *                                                                
                                                                                
ERGUCOAC DC    AL1(EGUCOACX-*),AL2(25),AL1(ERRCATY1),AL1(D#GUCOM)               
         DC    C'Commercial does not exist under Primary Agency/Client'         
EGUCOACX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE EXCLUDED USES                                       *         
***********************************************************************         
                                                                                
VALEXU   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RQGUEX1                                                       
         LA    R2,EXCLUSES                                                      
         LHI   R3,L'EXCLUSES                                                    
                                                                                
VEXU10   OC    0(L'RQGUEX1,R1),0(R1)                                            
         JZ    XIT                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               FIND USE IN USE TABLE                        
VEXU20   CLC   USECDE,0(R1)                                                     
         JE    VEXU30                                                           
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   VEXU20                                                           
         DC    H'00'                                                            
VEXU30   MVC   0(L'USEEQU,R2),USEEQU                                            
         DROP  RF                                                               
                                                                                
         LA    R1,L'RQGUEX1(R1)    NEXT EXCLUDED USE                            
         LA    R2,L'USEEQU(R2)     NEXT EXCLUDED USE IN LIST                    
         BCT   R3,VEXU10                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED SUBSIDIARY AGENCY/CLIENT CODES               *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALSAC   NTR1  BASE=*,LABEL=*                                                   
         CLI   ISUBAC,0            ONLY VALIDATE IF SUBSIDIARY                  
         JE    XIT                 AGENCY/CLIENTS HAVE BEEN PASSED              
                                                                                
         ZIC   R0,ISUBAC           R0=# OF SUBSIDIARY AGENCY/CLIENTS            
         ZICM  R2,ASUBAC,3         R2=A(SUB AGENCY/CLIENT ARRAY)                
                                                                                
VSAC10   MVC   EGUSANFA,0(R2)      SET CURRENT AGENCY/CLIENT                    
         MVC   EGUSCNFA,0(R2)      IN POTENTIAL ERROR MESSAGES                  
         MVC   EGUSCNFC,6(R2)                                                   
                                                                                
         USING TLAYD,R3                                                         
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY                          
         MVI   TLAYCD,TLAYCDQ      AND ENSURE IT EXISTS                         
         MVC   TLAYAGY,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    VSAC20                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUSANF                                  
         J     VSAC30                                                           
         DROP  R3                                                               
                                                                                
VSAC20   CLC   6(6,R2),SPACES      ONLY VALIDATE IF CLIENT CODE                 
         JE    VSAC30              HAS BEEN PASSED                              
                                                                                
         USING TLCLD,R3                                                         
         XC    TLCLKEY,TLCLKEY     READ FOR CLIENT KEY                          
         MVI   TLCLCD,TLCLCDQ      AND ENSURE IT EXISTS                         
         MVC   TLCLAGY,0(R2)                                                    
         MVC   TLCLCLI,6(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    VSAC30                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUSCNF                                  
         DROP  R3                                                               
                                                                                
VSAC30   LA    R2,12(R2)                                                        
         BCT   R0,VSAC10                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALSAC                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUSANF DC    AL1(EGUSANFX-*),AL2(43),AL1(ERRCATY1),AL1(D#GUSAC)               
         DC    C'Agency not on file: '                                          
EGUSANFA DC    CL6' '                                                           
EGUSANFX EQU   *                                                                
                                                                                
ERGUSCNF DC    AL1(EGUSCNFX-*),AL2(44),AL1(ERRCATY1),AL1(D#GUSAC)               
         DC    C'Client not on file: '                                          
EGUSCNFA DC    CL6' '                                                           
         DC    CL1'/'                                                           
EGUSCNFC DC    CL6' '                                                           
EGUSCNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED GUARANTEE CODE                               *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
*                     AIO5=A(W4 RECORD)                               *         
***********************************************************************         
                                                                                
VALGUA   NTR1  BASE=*,LABEL=*                                                   
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ST    R4,ATAGUEL                                                       
                                                                                
         TM    TAGUSTA2,TAGUSNEW   ENSURE THAT THIS IS A NEW-STYLE              
         JO    VGUA10              GUARANTEE                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUNMOD                                  
         J     NO                                                               
                                                                                
VGUA10   CLI   TAGUCRP,0           IF GUARANTEE IS ATTACHED TO                  
         JE    YES                 CORPORATION                                  
         MVC   BYTE1,TAGUCRP                                                    
         DROP  R4                                                               
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO5             R4=A(W4 RECORD)                              
         MVI   ELCODE,TATIELQ      READ THROUGH ALL TAX ID ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VGUA20   BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
         CLI   TATITYPE,TATITYCO                                                
         JNE   VGUA20                                                           
         CLC   BYTE1,TATIID        IF FOUND, SAVE ORIGINAL CORPORATION          
         JNE   VGUA20              FID#                                         
         MVC   SVOCORP,TATIID                                                   
         JE    XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALGUA                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUNMOD DC    AL1(EGUNMODX-*),AL2(2),AL1(ERRCATY3),AL1(D#GUGUA)                
         DC    C'Old style guarantees can only be modified via PC Talen+        
               t'                                                               
EGUNMODX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED TYPE                                         *         
***********************************************************************         
                                                                                
VALTYP   NTR1  BASE=*,LABEL=*                                                   
         USING TAGUD,R4                                                         
         L     R4,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         CLI   RQGUTYP,RQGUTYPL    IF TYPE IS LARGE OVERSCALE                   
         JNE   VTYP10                                                           
         OC    TAGUCOM,TAGUCOM     ENSURE TYPE IS NOT CHANGING                  
         JZ    YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUTYNC                                  
         J     NO                                                               
                                                                                
VTYP10   CLI   RQGUTYP,RQGUTYPP    IF TYPE IS PER CYCLE                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUCOM,TAGUCOM     ENSURE TYPE IS NOT CHANGING                  
         JNZ   YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUTYNC                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTYP                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUTYNC DC    AL1(EGUTYNCX-*),AL2(10),AL1(ERRCATY3),AL1(D#GUTYP)               
         DC    C'Type cannot be changed'                                        
EGUTYNCX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AGENCY AND CLIENT CODES                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
VALACS   NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VERATCOS       VERIFY ALL ATTACHED COMM'LS COVERED            
         BAS   RE,VERPENDS       VERIFY ALL PENDING PAYMENTS COVERED            
         BAS   RE,VERCHNGS       VERIFY ALL CHANGES                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        VERIFY ALL ATTACHED COMMERCIALS ARE COVERED BY PASSED        *         
*        AGENCY/CLIENT LIMITATIONS                                    *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VERATCOS NTR1                                                                   
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY READ ALL CAST RECORDS THAT                     
         MVI   TLCAPCD,TLCAGCDQ  ARE ATTACHED TO THIS GUARANTEE                 
         MVC   TLCAGSSN,RQGUSSN                                                 
         MVC   TLCAGGUA,RQGUGUA                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     VACS20                                                           
VACS10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
VACS20   CLC   IOKEY(TLCAGCOM-TLCAPCD),IOKEYSAV                                 
         JNE   XIT                                                              
         MVC   SVCAKEY,IOKEY                                                    
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY GET COMMERCIAL RECORD FOR CAST                 
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCAKEY+TLCAGCOM-TLCAPD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   VACS70                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO4                                                          
                                                                                
         CLC   TLCOAGY,RQGUAGY   IF ATTACHED COMMERCIAL'S AGENCY/CLIENT         
         JNE   VACS30            DOES NOT MATCH THE PROVIDED PRIMARY            
         CLC   TLCOCLI,RQGUCLI   AGENCY/CLIENT ...                              
         JE    VACS70                                                           
         OC    RQGUCLI,RQGUCLI                                                  
         JZ    VACS70                                                           
                                                                                
VACS30   CLI   ISUBAC,0          ... CHECK IF IT MATCHES AGAINST ONE            
         JE    VACS60            OF THE PROVIDED SUBSIDIARY AGENCY/             
         ZIC   R0,ISUBAC         CLIENTS ...                                    
         ZICM  R1,ASUBAC,3                                                      
VACS40   CLC   TLCOAGY,0(R1)                                                    
         JNE   VACS50                                                           
         CLC   TLCOCLI,6(R1)                                                    
         JE    VACS70                                                           
         CLC   6(6,R1),SPACES                                                   
         JE    VACS70                                                           
VACS50   LA    R1,12(R1)                                                        
         BCT   R0,VACS40                                                        
                                                                                
VACS60   MVC   EGUPIACA,SPACES   ... IF NOT, RETURN ERROR                       
         MVC   EGUPIACA(L'TLCOAGY),TLCOAGY                                      
         LA    RE,EGUPIACA                                                      
         CLI   0(RE),C' '                                                       
         JE    *+12                                                             
         LA    RE,1(RE)                                                         
         J     *-12                                                             
         MVI   0(RE),C'/'                                                       
         MVC   1(L'TLCOCLI,RE),TLCOCLI                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUPIAC                                  
         DROP  R4                                                               
                                                                                
VACS70   MVC   IOKEY,SVCAKEY     RESTORE CAST READ SEQUENCE                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     VACS10                                                           
                                                                                
***********************************************************************         
*        VERIFY ALL PENDING PAYMENTS ARE COVERED BY PASSED            *         
*        AGENCY/CLIENT LIMITATIONS                                    *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VERPENDS NTR1                                                                   
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKECDQ  PERFORMER FOR THIS AGENCY THAT                 
         MVC   TLCKESSN,RQGUSSN  HAVE NOT BEEN PROCESSED YET                    
         OC    SVOCORP,SVOCORP                                                  
         JZ    *+10                                                             
         MVC   TLCKESSN,SVOCORP                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VPS20                                                            
VPS10    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
VPS20    CLC   IOKEY(TLCKECUR-TLCKPCD),IOKEYSAV                                 
         JNE   XIT                                                              
         CLI   TLCKEDTE,0                                                       
         JNE   VPS10                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,RQGUGUA   ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   VPS10             CODE THAT MATCHES CURRENT PAYMENT              
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4           R4=A(CHECK RECORD)                             
         MVI   ELCODE,TAPDELQ    GET PAYMENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   TLCKEAGY,RQGUAGY  IF CHECK'S AGENCY/CLIENT                       
         JNE   VPS30             DOES NOT MATCH THE PROVIDED PRIMARY            
         CLC   TAPDCLI,RQGUCLI   AGENCY/CLIENT ...                              
         JE    VPS10                                                            
         OC    RQGUCLI,RQGUCLI                                                  
         JZ    VPS10                                                            
                                                                                
VPS30    CLI   ISUBAC,0          ... CHECK IF IT MATCHES AGAINST ONE            
         JE    VPS60             OF THE PROVIDED SUBSIDIARY AGENCY/             
         ZIC   R0,ISUBAC         CLIENTS ...                                    
         ZICM  R1,ASUBAC,3                                                      
VPS40    CLC   TLCKEAGY,0(R1)                                                   
         JNE   VPS50                                                            
         CLC   TAPDCLI,6(R1)                                                    
         JE    VPS10                                                            
         CLC   6(6,R1),SPACES                                                   
         JE    VPS10                                                            
VPS50    LA    R1,12(R1)                                                        
         BCT   R0,VPS40                                                         
         DROP  R3,R4                                                            
                                                                                
VPS60    MVC   SVCKKEY,IOKEY     ... IF NOT, SAVE CHECK KEY                     
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO4           R4=A(CHECK RECORD)                             
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINBCDQ  IF INVOICE HAS NOT ALREADY BEEN                
         MVC   TLINBAGY,TLCKAGY  BILLED, RETURN ERROR                           
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,=6X'FF'                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   VPS70                                                            
         TM    TLINBST2,TAINSBIL                                                
         JO    VPS70                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUPMRI                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VPS70    MVC   IOKEY,SVCKKEY       RESTORE CHECK READ SEQUENCE                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VPS10                                                            
                                                                                
***********************************************************************         
*        FORCE VERIFICATION OF ANY SUBSIDIARY AGENCY/CLINT CHANGES    *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
VERCHNGS NTR1                                                                   
         USING TAVAD,R4                                                         
         MVI   ELCODE,TAVAELQ      READ ALL ORIGINAL SUBSIDIARY                 
         BRAS  RE,GETEL            AGENCY/CLIENT ELEMENTS                       
         J     VCS20                                                            
VCS10    BRAS  RE,NEXTEL                                                        
VCS20    JNE   VCS100                                                           
                                                                                
         CLI   ISUBAC,0            IF ANY ARE FOUND AND NONE HAVE               
         JE    VCS200              BEEN PROVIDED, PROMPT VERIFICATION           
                                                                                
         XC    SVOCLI,SVOCLI       CLEAR ORIGINAL CLIENT                        
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF SUBSIDIARY               
         SHI   RF,TAVALNQ          CLIENTS IN ELEMENT                           
         LTR   RF,RF                                                            
         JNZ   VCS30                                                            
         LHI   RF,TAVALNQ                                                       
VCS30    D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R0,RF               R0=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R1,TAVACLI          R1=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
VCS40    MVC   SVOAGY,TAVAAGY      SAVE ORIGINAL AGENCY/CLIENT                  
         CLI   TAVALEN,TAVALNQ                                                  
         JE    *+10                                                             
         MVC   SVOCLI,0(R1)                                                     
                                                                                
         ZIC   RF,ISUBAC           RF=SUBSIDIARY AGENCY/CLIENT COUNTER          
         ZICM  RE,ASUBAC,3         RE=A(SUBSIDUARY AGY/CLI ARRAY)               
                                                                                
VCS50    CLC   SVOAGY(6),0(RE)     IF EXACT MATCH FOR AGENCY/CLIENT             
         JE    VCS60               IS NOT FOUND, PROMPT VERIFICATION            
         LA    RE,12(RE)                                                        
         BCT   RF,VCS50                                                         
         J     VCS200                                                           
                                                                                
VCS60    LA    R1,L'TAVACLI(R1)                                                 
         BCT   R0,VCS40                                                         
         J     VCS10                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
VCS100   CLI   ISUBAC,0            IF ANY HAVE BEEN PROVIDED                    
         JE    XIT                                                              
                                                                                
         ZIC   R2,ISUBAC           R2=SUBSIDIARY AGENCY/CLIENT COUNTER          
         ZICM  R3,ASUBAC,3         R3=A(SUBSIDUARY AGY/CLI ARRAY)               
                                                                                
         USING TAVAD,R4                                                         
VCS110   L     R4,AIO3                                                          
         MVI   ELCODE,TAVAELQ      BUT NONE WERE THERE ORIGINALLY               
         BRAS  RE,GETEL            PROMPT FOR VERIFICATION                      
         JNE   VCS200                                                           
         J     *+8                                                              
VCS120   BRAS  RE,NEXTEL                                                        
         JNE   VCS200                                                           
                                                                                
         CLC   TAVAAGY,0(R3)                                                    
         JNE   VCS120                                                           
                                                                                
         CLI   TAVALEN,TAVALNQ                                                  
         JNE   VCS140                                                           
         CLC   6(6,R3),SPACES                                                   
         JE    VCS160                                                           
         J     VCS200                                                           
                                                                                
VCS140   XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF SUBSIDIARY               
         SHI   RF,TAVALNQ          CLIENTS IN ELEMENT                           
         D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R0,RF               R0=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R1,TAVACLI          R1=A(CURRENT CLIENT IN ELEMENT)              
         DROP  R4                                                               
                                                                                
VCS150   CLC   0(6,R3),0(R1)       IF EXACT MATCH FOR AGENCY/CLIENT             
         JE    VCS160              IS NOT FOUND, PROMPT VERIFICATION            
         LA    RE,12(RE)                                                        
         BCT   RF,VCS150                                                        
         J     VCS120                                                           
                                                                                
VCS160   LA    R3,12(R3)                                                        
         BCT   R2,VCS110                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
VCS200   GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERGUSAC)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALACS                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUPIAC DC    AL1(EGUPIACX-*),AL2(45),AL1(ERRCATY1),AL1(D#GUSAC)               
         DC    C'Guarantee must remain inclusive of '                           
EGUPIACA DC    CL13' '                                                          
EGUPIACX EQU   *                                                                
                                                                                
ERGUPMRI DC    AL1(EGUPMRIX-*),AL2(46),AL1(ERRCATY1),AL1(D#GUSAC)               
         DC    C'Guarantee must remain inclusive of all pending payment+        
               s'                                                               
EGUPMRIX EQU   *                                                                
                                                                                
ERGUSAC  DC    AL1(EGUSACX-*),AL2(47),AL1(ERRCATY2),AL1(D#GUSAC)                
         DC    C'Review update to Subsidiary Agency/Clients'                    
EGUSACX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED PRIMARY INTERNAL COMMERCIAL NUMBER           *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCO2   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUTYP,RQGUTYPP    ONLY VALIDATE IF TYPE IS PER CYCLE           
         JNE   XIT                                                              
                                                                                
         USING TAGUD,R2                                                         
         L     R2,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         CLC   TAGUCOM,RQGUCOM     IF PRIMARY INTERNAL COMMERCIAL               
         JE    XIT                 NUMBER IS CHANGING                           
         XC    FPCYSTRT,FPCYSTRT   INITIALIZE FIRST AND LAST PER                
         XC    LPCYSTRT,LPCYSTRT   CYCLE PAYMENT START DATES                    
                                                                                
***********************************************************************         
                                                                                
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKPCD,TLCKHCDQ    PERFORMER ON SAVED PRIMARY COMML             
         MVC   TLCKHCOM,TAGUCOM                                                 
         MVC   TLCKHSSN,RQGUSSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VC220                                                            
VC210    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
VC220    CLC   IOKEY(TLCKHCAT-TLCKPCD),IOKEYSAV                                 
         JNE   VC260                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
         DROP  R3                                                               
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JNE   VC210                                                            
         OC    TACDDTE,TACDDTE     ONLY CONSIDER CHECKS THAT HAVE               
         JNZ   VC210               NOT PROCESSED YET                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,RQGUGUA     ONLY CONSIDER CHECKS WITH GUARANTEE          
         JNE   VC210               CODE THAT MATCHES CURRENT PAYMENT            
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               RF=A(USE TABLE)                              
                                                                                
VC230    CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,USECDE      FIND USE EQUATE IN USE TABLE                 
         JE    VC240                                                            
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         J     VC230                                                            
                                                                                
VC240    TM    USESTAT2,APPREUSE   IF CHECK IS FOR A PER CYCLE PAYMENT          
         JZ    VC210                                                            
         MVC   SVCKKEY,IOKEY       SAVE CHECK KEY                               
         DROP  R4,RF                                                            
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
                                                                                
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINBCDQ    CHECK IF INVOICE HAS ALREADY BEEN            
         MVC   TLINBAGY,TLCKAGY    BILLED                                       
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,=6X'FF'                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   VC250                                                            
         TM    TLINBST2,TAINSBIL   IF NOT, LEAVE LATEST PER CYCLE START         
         JZ    VC300               DATE UNSET                                   
         DROP  R3,R4                                                            
                                                                                
VC250    MVC   IOKEY,SVCKKEY       RESTORE CHECK READ SEQUENCE                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VC210                                                            
                                                                                
***********************************************************************         
                                                                                
         USING TAGCD,R4                                                         
VC260    L     R4,AIO3             IF ALL PER CYCLE PAYMENTS HAVE               
         MVI   ELCODE,TAGCELQ      BEEN PROCESSED, SAVE CYCLE START             
         BRAS  RE,GETEL            DATE OF THE LATEST ONE                       
         J     *+8                                                              
VC270    BRAS  RE,NEXTEL                                                        
         JNE   VC280                                                            
         MVC   LPCYSTRT,TAGCSTRT                                                
         J     VC270                                                            
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
VC280    XC    TLCAPKEY,TLCAPKEY   READ CAST RECORD FOR PRIMARY                 
         MVI   TLCAPCD,TLCAGCDQ    COMMERCIAL                                   
         MVC   TLCAGSSN,RQGUSSN                                                 
         MVC   TLCAGGUA,RQGUGUA                                                 
         MVC   TLCAGCOM,TAGUCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLCAGCAT-TLCAPCD),IOKEYSAV                                 
         JNE   VC290                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            IF FIRST FIXED CYCLE AT CAST LEVEL           
         JE    *+6                 SAVE AS FIRST PER CYCLE PAYMENT              
         DC    H'00'               START DATE                                   
         OC    TACAFCYC,TACAFCYC                                                
         JZ    VC290                                                            
         MVC   FPCYSTRT,TACAFCYC                                                
         J     VC300                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOPD,R3                                                        
VC290    XC    TLCOPKEY,TLCOPKEY   IF FFC NOT AT CAST LEVEL                     
         MVI   TLCOPCD,TLCOCCDQ    READ PRIMARY COMMERCIAL RECORD               
         MVC   TLCOCCOM,TAGUCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   VC300                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                 SAVE COMMERCIAL FIRST FIXED CYCLE            
         DC    H'00'               AS FIRST PER CYCLE PAYMENT START             
         MVC   FPCYSTRT,TACOFCYC   DATE                                         
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
                                                                                
VC300    OC    FPCYSTRT,FPCYSTRT   RETURN ERROR IF PAYMENT IS PENDING           
         JNZ   VC310                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUPCPN                                  
         J     XIT                                                              
                                                                                
         USING TLCAPD,R3                                                        
VC310    XC    TLCAPKEY,TLCAPKEY   READ CAST RECORD FOR INTENDED                
         MVI   TLCAPCD,TLCAGCDQ    PRIMARY COMEMRCIAL                           
         MVC   TLCAGSSN,RQGUSSN                                                 
         MVC   TLCAGGUA,RQGUGUA                                                 
         MVC   TLCAGCOM,RQGUCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLCAGCAT-TLCAPCD),IOKEYSAV                                 
         JNE   VC380                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            IF FIRST FIXED CYCLE AT CAST LEVEL           
         JE    *+6                 SAVE AS LATEST FTRACK START DATE             
         DC    H'00'                                                            
         OC    TACALAST,TACALAST                                                
         JNZ   VC380                                                            
         MVC   LFTRSTRT,TACAFCYC                                                
         DROP  R4                                                               
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO4             R4=A(CAST RECORD)                            
         MVI   ELCODE,TACRELQ      READ ALL APPLIED CREDIT ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     VC330                                                            
VC320    BRAS  RE,NEXTEL                                                        
VC330    JNE   VC350                                                            
         CLC   TACRUSE,=C'HLD'     REJECT IF NOT FOR HOLDING FEE                
         JE    VC340                                                            
         CLC   TACRUSE,=C'SHL'     OR SPANISH HOLDING FEE                       
         JE    VC340                                                            
         CLC   TACRUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         JE    VC340                                                            
         CLC   TACRUSE,=C'REN'     OR REINSTATEMENT                             
         JE    VC340                                                            
         CLC   TACRUSE,=C'SRE'     OR SPANISH REINSTATEMENT                     
         JNE   VC320                                                            
VC340    MVC   LFTRSTRT,TACRSTRT   SAVE LATEST FTRACK START DATE                
         J     VC320                                                            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
VC350    L     R4,AIO4             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                 MAY NOT BE LOCKED, RELEASED OR               
         DC    H'00'               SET UP FOR CANADIAN RATES                    
         TM    TACOSTAT,TACOSTLO+TACOSTRL+TACOSCRT                              
         JNZ   VC380                                                            
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TELEVISION                     
         JE    VC360                                                            
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    VC360                                                            
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   VC380                                                            
                                                                                
VC360    OC    LFTRSTRT,LFTRSTRT   IF LATEST FTRACK START DATE                  
         JNZ   VC370               STILL IS NOT SET                             
         MVC   LFTRSTRT,TACOFCYC   SET IT AS COMMERCIAL'S FFC                   
         DROP  R4                                                               
                                                                                
VC370    CLC   LPCYSTRT,LFTRSTRT   IF LATEST PER CYCLE PAYMENT DOES             
         JE    XIT                 NOT MATCH CAST'S LATEST FTRACK               
         OC    LPCYSTRT,LPCYSTRT   AND PER CYCLE PAYMENT HAS BEEN MADE          
         JNZ   VC380               COMM'L IS NOT ELIGIBLE TO BE PRIMARY         
                                                                                
         CLC   LFTRSTRT,FPCYSTRT   IF PER CYCLE PAYMENT HAS NOT BEEN            
         JNH   XIT                 MADE,FFC MUST BE THE SAME OR EARLIER         
VC380    GOTOR (#ADDERR,AADDERR),DMCB,ERGUPCNE                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCO2                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUPCPN DC    AL1(EGUPCPNX-*),AL2(26),AL1(ERRCATY1),AL1(D#GUCOM)               
         DC    C'Cannot change Primary Commercial - Per Cycle payment p+        
               ending'                                                          
EGUPCPNX EQU   *                                                                
                                                                                
ERGUPCNE DC    AL1(EGUPCNEX-*),AL2(27),AL1(ERRCATY1),AL1(D#GUCOM)               
         DC    C'Commercial not eligible to be primary'                         
EGUPCNEX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED PERIOD START AND END DATES                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALPER   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUTYP,RQGUTYPL    ONLY VALIDATE IF TYPE IS LARGE               
         JNE   XIT                 OVERSCALE                                    
                                                                                
         USING TAGUD,R2                                                         
         L     R2,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         CLC   TAGUPD,RQGUPST      IF PERIOD IS CHANGING                        
         JE    XIT                                                              
                                                                                
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKPCD,TLCKECDQ    PERFORMER                                    
         MVC   TLCKESSN,RQGUSSN                                                 
         OC    SVOCORP,SVOCORP                                                  
         JZ    *+10                                                             
         MVC   TLCKESSN,SVOCORP                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VPER20                                                           
VPER10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
VPER20   CLC   IOKEY(TLCKECUR-TLCKPCD),IOKEYSAV                                 
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,RQGUGUA     IGNORE IF PAYMENT IS NOT FOR                 
         JNE   VPER10              THIS GUARANTEE                               
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAPDCYCS,TAPDCYCS   IF PAYMENT HAS CYCLE DATES                   
         JZ    VPER10                                                           
         MVC   SVDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    VPER30                                                           
         MVC   SVDATE,TAPDCYCE                                                  
         DROP  R4                                                               
                                                                                
VPER30   CLC   SVDATE,RQGUPST      PERIOD MUST REMAIN INCLUSIVE                 
         JNL   VPER40              OF ALL APPLY DATES                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUSINA                                  
         J     XIT                                                              
VPER40   CLC   SVDATE,RQGUPEN                                                   
         JNH   VPER10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUEINA                                  
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALPER                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUSINA DC    AL1(EGUSINAX-*),AL2(11),AL1(ERRCATY1),AL1(D#GUPST)               
         DC    C'Period must remain inclusive of all applying payments'         
EGUSINAX EQU   *                                                                
                                                                                
ERGUEINA DC    AL1(EGUEINAX-*),AL2(13),AL1(ERRCATY1),AL1(D#GUPEN)               
         DC    C'Period must remain inclusive of all applying payments'         
EGUEINAX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AMOUNT                                       *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALAMT   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUTYP,RQGUTYPL    ONLY VALIDATE IF TYPE IS LARGE               
         JNE   XIT                 OVERSCALE                                    
                                                                                
         USING TAGUD,R4                                                         
         L     R4,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         CLC   TAGUAMT,RQGUAMT     IF AMOUNT IS CHANGING ...                    
         JE    XIT                                                              
                                                                                
         TM    TAGUSTAT,TAGUSPAY   ... AND GUARANTEE WAS ADDED VIA              
         JZ    VAMT10              GRT/PAY, AMOUNT CANNOT BE CHANGED            
         OC    RQGUAMT,RQGUAMT     TO $0                                        
         JNZ   VAMT10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUABAL                                  
         J     XIT                                                              
                                                                                
         USING TLGTD,R3                                                         
VAMT10   XC    TLGTKEY,TLGTKEY     ... AND GUARANTEE HAS ESTABLISHED            
         MVI   TLGTCD,TLGTCDQ      GTRACKS ...                                  
         MVC   TLGTSSN,RQGUSSN                                                  
         MVC   TLGTGUA,RQGUGUA                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLGTSTRT-TLGTD),IOKEYSAV                                   
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         OC    TAGUAMT,TAGUAMT    ... IF GUARANTEE HAS AN ASCENDING             
         JNZ   VAMT20             BALANCE, BALANCE CANNOT BE CHANGED            
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUDBAL                                  
         J     XIT                                                              
                                                                                
VAMT20   CLC   TAGUAMT,=F'25000'  ... IF GUARANTEE'S AMOUNT IS GREATER          
         JL    XIT                THAN $250, CANNOT BE CHANGED TO LESS          
         CLC   RQGUAMT,=F'25000'  THAN $250                                     
         JNL   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUABAL                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAMT                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUDBAL DC    AL1(EGUDBALX-*),AL2(15),AL1(ERRCATY1),AL1(D#GUAMT)               
         DC    C'Cannot be changed to Descending Balance Guarantee'             
EGUDBALX EQU   *                                                                
                                                                                
ERGUABAL DC    AL1(EGUABALX-*),AL2(16),AL1(ERRCATY1),AL1(D#GUAMT)               
         DC    C'Cannot be changed to Ascending Balance Guarantee'              
EGUABALX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED BALANCE                                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALBAL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUTYP,RQGUTYPL    ONLY VALIDATE IF TYPE IS LARGE               
         JNE   XIT                 OVERSCALE                                    
                                                                                
         USING TAGUD,R4                                                         
         L     R4,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         CLC   TAGUBAL,RQGUBAL     IF BALANCE IS CHANGING ...                   
         JE    XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TLGTD,R3                                                         
         XC    TLGTKEY,TLGTKEY     ... ENSURE GUARANTEE DOES NOT                
         MVI   TLGTCD,TLGTCDQ      HAVE ANY ESTABLISHED GTRACKS                 
         MVC   TLGTSSN,RQGUSSN                                                  
         MVC   TLGTGUA,RQGUGUA                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLGTSTRT-TLGTD),IOKEYSAV                                   
         JNE   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUBANC                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALBAL                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUBANC DC    AL1(EGUBANCX-*),AL2(18),AL1(ERRCATY1),AL1(D#GUBAL)               
         DC    C'GTrack has been established - use GTrack/Add to update+        
               '                                                                
EGUBANCX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED PAY P&H ON USE STATUS                        *         
***********************************************************************         
                                                                                
VALPNH   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQGUPNH,C'Y'        ONLY VALIDATE IF PAY P&H ON USE              
         JNE   XIT                 STATUS IS BEING SET AS YES                   
                                                                                
         USING TAGUD,R4                                                         
         L     R4,ATAGUEL          R4=A(GUARANTEE DETAILS ELEMENT)              
         TM    TAGUSTAT,TAGUSPAY   ENSURE GUARANTEE WAS NO ADDED                
         JZ    XIT                 VIA GRT/PAY                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUPNHU                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALPNH                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUPNHU DC    AL1(EGUPNHUX-*),AL2(22),AL1(ERRCATY1),AL1(D#GUPNH)               
         DC    C'Invalid setting for GRT/PAY created Guarantee'                 
EGUPNHUX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF GUARANTEE RECORD         *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERGUNTFD                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUNTFD DC    AL1(EGUNTFDX-*),AL2(3),AL1(ERRCATY2),AL1(D#GUSSN)                
         DC    C'Guarantee not on file'                                         
EGUNTFDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF GUARANTEE RECORD           *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         CLI   RQGUMOD,RQGUEXE     IF MODE IS EXECUTE                           
         JNE   INITC10             ENSURE CHECK LOCKOUT STATUS IS OFF           
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_CHECKS',ERGUCKL                  
         JE    INITC10                                                          
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_PRCHKS',ERGUCKL                  
         JE    INITC10                                                          
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_P+CHKS',ERGUCKL                  
         JE    INITC10                                                          
                                                                                
INITC10  MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC20  BRAS  RE,NEXTEL                                                        
         JNE   INITC30                                                          
                                                                                
         CLI   0(R4),TAVAELQ       DELETE SUBSIDIARY AGENCY/CLIENT              
         JNE   *+12                ELEMENTS                                     
         MVI   0(R4),X'FF'                                                      
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TAGXELQ       PROCESS EXCLUDED USES                        
         JNE   *+12                                                             
         BRAS  RE,CPYTAGX                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TAGUELQ       PROCESS GUARANTEE DETAILS                    
         JNE   *+12                                                             
         BRAS  RE,CPYTAGU                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TAFNELQ       DELETE FREE FORM NAME ELEMENT                
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC20                                                          
                                                                                
INITC30  BRAS  RE,NEWTAGX          PROCESS NEW EXCLUDED USES                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUCKL  DC    AL1(EGUCKLX-*),AL2(500),AL1(ERRCATY1),AL1(D#GUMOD)               
         DC    C'Urgent check run in progress'                                  
EGUCKLX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES EXCLUDED USE ELEMENT INTO REQUST MAP          *         
*        ON ENTRY ... R4 = A(EXCLUDED USE ELEMENT)                    *         
***********************************************************************         
                                                                                
         USING TAGXD,R4                                                         
CPYTAGX  NTR1  BASE=*,LABEL=*                                                   
         OI    CPYSTAT1,CPYSTAGX   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
                                                                                
         XC    ELEM,ELEM           COPY EXCLUDED USE ELEMENT INTO ELEM          
         ZIC   RF,TAGXLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         LA    R4,ELEM                                                          
         LA    R4,TAGXUSE                                                       
         DROP  R4                                                               
                                                                                
         USING ERRENTD,R1                                                       
         LA    R1,ERGUEXU          INTIALIZE ERROR ENTRY TO FIRST               
         MVI   EENUMB+1,30         ERROR NUMBER AND FIELD                       
         MVI   EEFIELD,D#GUEX1                                                  
         DROP  R1                                                               
                                                                                
         LHI   R0,L'EXCLUSES       R0=# OF EXCLUDED USES                        
         LA    R3,EXCLUSES         R3=A(PASSED EXCLUDED USES)                   
                                                                                
CGX10    GOTOR (#CHKFLD,ACHKFLD),DMCB,(1,0(R3)),0(R4),ERGUEXU,0                 
                                                                                
         USING ERRENTD,R1                                                       
         LA    R1,ERGUEXU          SET ERROR ENTRY TO NEXT ERROR                
         ZICM  RE,EENUMB,2         NUMBER                                       
         AHI   RE,1                                                             
         STCM  RE,3,EENUMB                                                      
                                                                                
         ZIC   RE,EEFIELD          SET ERROR ENTRY TO NEXT ERROR                
         AHI   RE,1                FIELD                                        
         STC   RE,EEFIELD                                                       
         DROP  R1                                                               
                                                                                
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,CGX10                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF EXCLUDED USE ELEMENTS DID NOT EXIST ON RECORD             *         
*        PREVIOUSLY AND EXCLUDED USES ARE BEING ADDED NOW,            *         
*        ROUTINE PROCESS THEM                                         *         
***********************************************************************         
                                                                                
NEWTAGX  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAGX   IF EXCLUDED USES DID NOT EXIST               
         JO    XIT                 ON RECORD PREVIOUSLY                         
         CLI   EXCLUSES,0          AND ARE BEING ADDED NOW                      
         JE    XIT                 RETURN PROMPT FOR VERIFICATION               
                                                                                
         USING ERRENTD,R1                                                       
         LA    R1,ERGUEXU          INTIALIZE ERROR ENTRY TO FIRST               
         MVI   EENUMB+1,30         ERROR NUMBER AND FIELD                       
         MVI   EEFIELD,D#GUEX1                                                  
         DROP  R1                                                               
                                                                                
         LHI   R0,L'EXCLUSES       R0=# OF EXCLUDED USES                        
         LA    R3,EXCLUSES         R3=A(PASSED EXCLUDED USES)                   
                                                                                
NGX10    CLI   0(R3),0                                                          
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERGUEXU)                       
                                                                                
         USING ERRENTD,R1                                                       
         LA    R1,ERGUEXU          SET ERROR ENTRY TO NEXT ERROR                
         ZICM  RE,EENUMB,2         NUMBER                                       
         AHI   RE,1                                                             
         STCM  RE,3,EENUMB                                                      
                                                                                
         ZIC   RE,EEFIELD          SET ERROR ENTRY TO NEXT ERROR                
         AHI   RE,1                FIELD                                        
         STC   RE,EEFIELD                                                       
         DROP  R1                                                               
                                                                                
         LA    R3,1(R3)                                                         
         BCT   R0,NGX10                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAGX AND NEWTAGX                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUEXU  DC    AL1(EGUEXUX-*),AL2(30),AL1(ERRCATY2),AL1(D#GUEX1)                
         DC    C'Review update to Excluded Use'                                 
EGUEXUX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES GUARANTEE DETAILS ELEMENT INTO REQUEST MAP    *         
*        ON ENTRY ... R4 = A(GUARANTEE DETAILS ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
CPYTAGU  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'SVCORP,SVCORP),TAGUCRP,       +        
               ERGUCRP,0                                                        
         GOTOR (RF),(R1),(L'RQGUAGY,RQGUAGY),TAGUAGY,ERGUAGY,0                  
         GOTOR (RF),(R1),(L'RQGUCLI,RQGUCLI),TAGUCLI,ERGUCLI,0                  
         GOTOR (RF),(R1),(L'RQGUPST,RQGUPST),TAGUSTRT,ERGUPST,0                 
         GOTOR (RF),(R1),(L'RQGUPEN,RQGUPEN),TAGUEND,ERGUPEN,0                  
         GOTOR (RF),(R1),(L'RQGUAMT,RQGUAMT),TAGUAMT,ERGUAMT,0                  
         GOTOR (RF),(R1),(L'RQGUBAL,RQGUBAL),TAGUBAL,ERGUBAL,0                  
         GOTOR (RF),(R1),(L'RQGUCOM,RQGUCOM),TAGUCOM,ERGUCOM,0                  
                                                                                
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),('TAGUSOVR',RQGUPOV),          +        
               (0,TAGUSTAT),ERGUPOV                                             
         GOTOR (RF),(R1),('TAGUSIGP',RQGUIPO),(0,TAGUSTA2),ERGUIPO              
         GOTOR (RF),(R1),('TAGUSPNH',RQGUPNH),(0,TAGUSTAT),ERGUPNH              
         GOTOR (RF),(R1),('TAGUSLCK',RQGULCK),(0,TAGUSTAT),ERGULCK              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAW4                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERGUCRP  DC    AL1(EGUCRPX-*),AL2(5),AL1(ERRCATY2),AL1(D#GUFID)                 
         DC    C'Review update to Attached Corporation FID#'                    
EGUCRPX  EQU   *                                                                
                                                                                
ERGUAGY  DC    AL1(EGUAGYX-*),AL2(7),AL1(ERRCATY2),AL1(D#GUAGY)                 
         DC    C'Review update to Primary Agency Code'                          
EGUAGYX  EQU   *                                                                
                                                                                
ERGUCLI  DC    AL1(EGUCLIX-*),AL2(9),AL1(ERRCATY2),AL1(D#GUCLI)                 
         DC    C'Review update to Primary Client Code'                          
EGUCLIX  EQU   *                                                                
                                                                                
ERGUPST  DC    AL1(EGUPSTX-*),AL2(12),AL1(ERRCATY2),AL1(D#GUPST)                
         DC    C'Review update to Period Start Date'                            
EGUPSTX  EQU   *                                                                
                                                                                
ERGUPEN  DC    AL1(EGUPENX-*),AL2(14),AL1(ERRCATY2),AL1(D#GUPEN)                
         DC    C'Review update to Period End Date'                              
EGUPENX  EQU   *                                                                
                                                                                
ERGUAMT  DC    AL1(EGUAMTX-*),AL2(17),AL1(ERRCATY2),AL1(D#GUAMT)                
         DC    C'Review update to Amount'                                       
EGUAMTX  EQU   *                                                                
                                                                                
ERGUBAL  DC    AL1(EGUBALX-*),AL2(19),AL1(ERRCATY2),AL1(D#GUBAL)                
         DC    C'Review update to Balance'                                      
EGUBALX  EQU   *                                                                
                                                                                
ERGUCOM  DC    AL1(EGUCOMX-*),AL2(28),AL1(ERRCATY2),AL1(D#GUCOM)                
         DC    C'Review update to Primary Commercial'                           
EGUCOMX  EQU   *                                                                
                                                                                
ERGUPOV  DC    AL1(EGUPOVX-*),AL2(20),AL1(ERRCATY2),AL1(D#GUPOV)                
         DC    C'Review update to Pay Overage Status'                           
EGUPOVX  EQU   *                                                                
                                                                                
ERGUIPO  DC    AL1(EGUIPOX-*),AL2(21),AL1(ERRCATY2),AL1(D#GUIPO)                
         DC    C'Review update to Ignore Pay Overage on Estimating Stat+        
               us'                                                              
EGUIPOX  EQU   *                                                                
                                                                                
ERGUPNH  DC    AL1(EGUPNHX-*),AL2(23),AL1(ERRCATY2),AL1(D#GUPNH)                
         DC    C'Review update to Pay P&&H on Use Status'                       
EGUPNHX  EQU   *                                                                
                                                                                
ERGULCK  DC    AL1(EGULCKX-*),AL2(29),AL1(ERRCATY2),AL1(D#GULCK)                
         DC    C'Review update to Locked Status'                                
EGULCKX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS GUARANTEE RECORD TO FILE                        *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         OC    RQGUGUA,RQGUGUA     ... IF GUARANTEE CODE IS NOT                 
         JNZ   EA10                PROVIDED, DEFAULT TO 0001                    
         MVC   RQGUGUA,=C'0001'                                                 
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ FOR THIS PERFORMER'S HIGHEST            
         MVI   TLGUCD,TLGUCDQ      GUARANTEE CODE                               
         MVC   TLGUSSN,RQGUSSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLGUGUA-TLGUD),IOKEYSAV                                    
         JNE   EA10                                                             
         MVC   RQGUGUA,TLGUGUA                                                  
         XC    RQGUGUA,=4X'FF'                                                  
         PACK  DUB,RQGUGUA                                                      
         AP    DUB,=P'1'           IF ONE IS FOUND, ADD 1 TO IT                 
         OI    DUB+7,X'0F'                                                      
         UNPK  RQGUGUA,DUB+5(3)                                                 
         DROP  R3                                                               
                                                                                
EA10     XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
                                                                                
         USING TLGUD,R4                                                         
         MVI   TLGUCD,TLGUCDQ      BUILD KEY WITH RECORD CODE                   
         MVC   TLGUSSN,RQGUSSN     SS#/FID#                                     
         MVC   TLGUGUA,RQGUGUA     GUARANTEE CODE                               
         XC    TLGUGUA,=6X'FF'     (COMPLEMENTED)                               
         MVI   TLGULEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAGUD,RF                                                         
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM           INITIALIZE GUARANTEE DETAILS ELEMENT         
         MVI   TAGUEL,TAGUELQ      AND ADD IT TO GUARANTEE RECORD               
         MVI   TAGULEN,TAGULNQ                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM                          
         DROP  RF                                                               
                                                                                
         BRAS  RE,BLDREC           BUILD GUARANTEE RECORD                       
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING GUARANTEE RECORD                    *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                 DELETE ALL MARKED ELEMENTS                   
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         BRAS  RE,BLDREC           BUILD GUARANTEE RECORD                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD GUARANTEE RECORD                                       *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,UPDTAGU          UPDATE GUARANTEE DETAILS ELEMENT             
         BAS   RE,ADDTAGX          ADD EXCLUDED USE ELEMENTS                    
         BAS   RE,ADDTAVA          ADD SUBSIDIARY AGY/CLI ELEMENTS              
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQGUWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQGUSTF,SVTIME             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        UPDATE GUARANTEE RECORD'S GUARANTEE DETAILS ELEMENT          *         
*        ON ENTRY ... R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
UPDTAGU  NTR1                                                                   
         USING TAGUD,R4                                                         
         MVI   ELCODE,TAGUELQ                                                   
         BRAS  RE,GETEL            R4=A(GUARANTEE DETAILS ELEMENT)              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         NI    TAGUSTAT,X'FF'-TAGUSDES-TAGUSPNH-TAGUSOVR-TAGUSLCK               
         NI    TAGUSTA2,X'FF'-TAGUSIGP                                          
                                                                                
         MVC   TAGUAGY,RQGUAGY     PUT PRIMARY AGENCY                           
         MVC   TAGUCLI,RQGUCLI     PRIMARY CLIENT                               
         MVC   TAGUCRP,SVCORP      CORPORATION CODE                             
         OI    TAGUSTA2,TAGUSNEW   AND NEW GUARANTEE STATUS                     
                                                                                
         CLI   RQGULCK,C'Y'                                                     
         JNE   *+8                 PUT "LOCKED?"                                
         OI    TAGUSTAT,TAGUSLCK   INDICATOR INTO ELEMENT                       
                                                                                
         CLI   RQGUTYP,RQGUTYPL    IF LARGE OVERSCALE GUARANTEE                 
         JNE   UGU10                                                            
         MVC   TAGUPD,RQGUPST      PUT PERIOD                                   
         MVC   TAGUAMT,RQGUAMT     AMOUNT                                       
         MVC   TAGUBAL,RQGUBAL     AND BALANCE                                  
                                                                                
         OC    RQGUAMT,RQGUAMT                                                  
         JZ    *+8                 PUT "DESCENDING BALANCE?"                    
         OI    TAGUSTAT,TAGUSDES   INDICATOR INTO ELEMENT                       
                                                                                
         CLI   RQGUPNH,C'Y'                                                     
         JNE   *+8                 PUT "PAY P&H ON USE"                         
         OI    TAGUSTAT,TAGUSPNH   INDICATOR INTO ELEMENT                       
                                                                                
         CLI   RQGUPOV,C'Y'                                                     
         JNE   *+8                 PUT "PAY OVERAGE?"                           
         OI    TAGUSTAT,TAGUSOVR   INIDICATOR INTO ELEMENT                      
                                                                                
         CLI   RQGUIPO,C'Y'                                                     
         JNE   *+8                 PUT "IGNORE PAY OVERAGE"                     
         OI    TAGUSTA2,TAGUSIGP   INDICATOR INTO ELEMENT                       
         J     XIT                                                              
                                                                                
UGU10    MVC   TAGUCOM,RQGUCOM     IF PER CYCLE GUARANTEE                       
         OI    TAGUSTAT,TAGUSDES+TAGUSOVR PUT PROPER STATUS                     
         J     XIT                 AND PRIMARY COMMERCIAL                       
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ADD EXCLUDED USE ELEMENTS TO GUARANTEE RECORD                *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
ADDTAGX  NTR1                                                                   
         OC    EXCLUSES,EXCLUSES   IF EXCLUDED USES ARE PROVIDED                
         JZ    XIT                                                              
                                                                                
         USING TAGXD,R2                                                         
         XC    ELEM,ELEM           INITIALIZE EXCLUDES USES ELEMENT             
         MVI   TAGXEL,TAGXELQ      PUT EXCLUDED USES INTO IT ELEMENT            
         MVC   TAGXUSE(13),EXCLUSES                                             
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2)                          
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD SUBSIDIARY AGENCY/CLIENT ELEMENTS TO GUARANTEE RECORD    *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
ADDTAVA  NTR1                                                                   
         CLI   ISUBAC,0            IF SUBSIDIARY AGENCY/CLIENTS                 
         JE    XIT                 ARE PROVIDED                                 
                                                                                
         ZIC   R0,ISUBAC           R0=PROVIDED SUBSIDUARYCOUNTER                
         ZICM  R3,ASUBAC,3         R3=A(PROVIDED SUBDIDIARY AGY/CLIS)           
                                                                                
         USING TAVAD,R2                                                         
AVA10    XC    ELEM,ELEM           INITIALIZE SUBSIDIARY AGENCY/CLIENT          
         MVI   TAVAEL,TAVAELQ      ELEMENT                                      
         MVI   TAVALEN,TAVALNQ                                                  
         LA    R5,TAVACLI                                                       
                                                                                
AVA20    CLC   TAVAAGY,0(R3)       IF THIS AGENCY IS DIFFERENT THAN             
         JE    AVA40               THE LAST ONE THAT WAS ENCOUNTERED            
         OC    TAVAAGY,TAVAAGY     ADD THE ELEMENT                              
         JZ    AVA30                                                            
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2)                          
         J     AVA10                                                            
                                                                                
AVA30    MVC   TAVAAGY,0(R3)       ADD AGENCY TO ELEMENT                        
                                                                                
AVA40    OC    6(6,R3),6(R3)       IF CLIENT IS PROVIDED                        
         JZ    AVA50               ADD CLIENT TO ELEMENT                        
         MVC   0(L'TAVACLI,R5),6(R3)                                            
         LA    R5,L'TAVACLI(R5)                                                 
         ZIC   RE,TAVALEN          AND BUMP UP ELEMENT LENGTH                   
         AHI   RE,L'TAVACLI                                                     
         STC   RE,TAVALEN                                                       
                                                                                
AVA50    LA    R3,12(R3)           BUMP TO NEXT PROVIDED                        
         BCT   R0,AVA20            AGENCY/CLIENT                                
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2)                          
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
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
* REQUEST MAP - GUARANTEE MAINTENANCE UPLOAD                          *         
***********************************************************************         
                                                                                
GUHDR    LKMAP H,I#GUULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#GUMOD,UBIN,TA#PMODE,OLEN=L'RQGUMOD,MAXLEN=1,        *        
               OUTPUT=(D,B#SAVED,RQGUMOD)                                       
F$STF    LKMAP F,D#GUSTF,CHAR,TA#STAFF,MAXLEN=L'RQGUSTF,               *        
               OUTPUT=(D,B#SAVED,RQGUSTF)                                       
F$SSN    LKMAP F,D#GUSSN,CHAR,TA#SSN,MAXLEN=L'RQGUSSN,                 *        
               OUTPUT=(D,B#SAVED,RQGUSSN)                                       
F$GUA    LKMAP F,D#GUGUA,CHAR,TA#GRTCD,MAXLEN=L'RQGUGUA,               *        
               OUTPUT=(D,B#SAVED,RQGUGUA)                                       
F$FID    LKMAP F,D#GUFID,CHAR,TA#ATC,MAXLEN=L'RQGUATC,                 *        
               OUTPUT=(D,B#SAVED,RQGUATC)                                       
F$AGY    LKMAP F,D#GUAGY,CHAR,TA#AGYCD,MAXLEN=L'RQGUAGY,               *        
               OUTPUT=(D,B#SAVED,RQGUAGY)                                       
F$CLI    LKMAP F,D#GUCLI,CHAR,TA#CLICD,MAXLEN=L'RQGUCLI,               *        
               OUTPUT=(D,B#SAVED,RQGUCLI)                                       
F$TYP    LKMAP F,D#GUTYP,CHAR,TA#GRTTY,MAXLEN=L'RQGUTYP,               *        
               OUTPUT=(D,B#SAVED,RQGUTYP)                                       
F$PST    LKMAP F,D#GUPST,PDAT,TA#PERST,OUTPUT=(D,B#SAVED,RQGUPST)               
F$PEN    LKMAP F,D#GUPEN,PDAT,TA#PEREN,OUTPUT=(D,B#SAVED,RQGUPEN)               
F$AMT    LKMAP F,D#GUAMT,UBIN,TA#AMT,MAXLEN=11,OLEN=L'RQGUAMT,         *        
               OUTPUT=(D,B#SAVED,RQGUAMT)                                       
F$BAL    LKMAP F,D#GUBAL,UBIN,TA#BAL,MAXLEN=11,OLEN=L'RQGUBAL,         *        
               OUTPUT=(D,B#SAVED,RQGUBAL)                                       
F$POV    LKMAP F,D#GUPOV,CHAR,TA#POV,MAXLEN=L'RQGUPOV,                 *        
               OUTPUT=(D,B#SAVED,RQGUPOV)                                       
F$IPO    LKMAP F,D#GUIPO,CHAR,TA#IPO,MAXLEN=L'RQGUIPO,                 *        
               OUTPUT=(D,B#SAVED,RQGUIPO)                                       
F$PNH    LKMAP F,D#GUPNH,CHAR,TA#PYPNH,MAXLEN=L'RQGUPNH,               *        
               OUTPUT=(D,B#SAVED,RQGUPNH)                                       
F$COM    LKMAP F,D#GUCOM,HEXD,TA#PRCOM,OLEN=L'RQGUCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQGUCOM)                                       
F$LCK    LKMAP F,D#GULCK,CHAR,TA#LOCKD,MAXLEN=L'RQGULCK,               *        
               OUTPUT=(D,B#SAVED,RQGULCK)                                       
F$EX1    LKMAP F,D#GUEX1,CHAR,TA#GUEX1,MAXLEN=L'RQGUEX1,               *        
               OUTPUT=(D,B#SAVED,RQGUEX1)                                       
F$EX2    LKMAP F,D#GUEX2,CHAR,TA#GUEX2,MAXLEN=L'RQGUEX2,               *        
               OUTPUT=(D,B#SAVED,RQGUEX2)                                       
F$EX3    LKMAP F,D#GUEX3,CHAR,TA#GUEX3,MAXLEN=L'RQGUEX3,               *        
               OUTPUT=(D,B#SAVED,RQGUEX3)                                       
F$EX4    LKMAP F,D#GUEX4,CHAR,TA#GUEX4,MAXLEN=L'RQGUEX4,               *        
               OUTPUT=(D,B#SAVED,RQGUEX4)                                       
F$EX5    LKMAP F,D#GUEX5,CHAR,TA#GUEX5,MAXLEN=L'RQGUEX5,               *        
               OUTPUT=(D,B#SAVED,RQGUEX5)                                       
F$EX6    LKMAP F,D#GUEX6,CHAR,TA#GUEX6,MAXLEN=L'RQGUEX6,               *        
               OUTPUT=(D,B#SAVED,RQGUEX6)                                       
F$EX7    LKMAP F,D#GUEX7,CHAR,TA#GUEX7,MAXLEN=L'RQGUEX7,               *        
               OUTPUT=(D,B#SAVED,RQGUEX7)                                       
F$EX8    LKMAP F,D#GUEX8,CHAR,TA#GUEX8,MAXLEN=L'RQGUEX8,               *        
               OUTPUT=(D,B#SAVED,RQGUEX8)                                       
F$EX9    LKMAP F,D#GUEX9,CHAR,TA#GUEX9,MAXLEN=L'RQGUEX9,               *        
               OUTPUT=(D,B#SAVED,RQGUEX9)                                       
F$EXA    LKMAP F,D#GUEXA,CHAR,TA#GUEXA,MAXLEN=L'RQGUEXA,               *        
               OUTPUT=(D,B#SAVED,RQGUEXA)                                       
F$EXB    LKMAP F,D#GUEXB,CHAR,TA#GUEXB,MAXLEN=L'RQGUEXB,               *        
               OUTPUT=(D,B#SAVED,RQGUEXB)                                       
F$EXC    LKMAP F,D#GUEXC,CHAR,TA#GUEXC,MAXLEN=L'RQGUEXC,               *        
               OUTPUT=(D,B#SAVED,RQGUEXC)                                       
F$EXD    LKMAP F,D#GUEXD,CHAR,TA#GUEXD,MAXLEN=L'RQGUEXD,               *        
               OUTPUT=(D,B#SAVED,RQGUEXD)                                       
F$SAC    LKREQ F,D#GUSAC,(I,B#SAVED,I$SUAC),CHAR,LIST=F,               *        
               OLEN=12,TEXT=TA#GUSAC,COL=*                                      
F$WID    LKMAP F,D#GUWID,CHAR,TA#WAPID,MAXLEN=L'RQGUWID,               +        
               OUTPUT=(D,B#SAVED,RQGUWID)                                       
F$EOV    LKREQ F,D#GUEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,               *        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,D#GUCMC,(I,B#SAVED,I$CLMC),UBIN,LIST=F,               *        
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
                                                                                
ATAGUEL  DS    A                   A(GUARANTEE DETAILS ELEMENT)                 
                                                                                
SVCORP   DS    CL(L'TATICRPN)      SAVED CORPORATION NUMBER                     
SVOCORP  DS    CL9                 SAVED ORIGINAL CORPORATION FID#              
SVOAGY   DS    CL(L'TAVAAGY)       SAVED ORIGINAL SUBSIDIARY AGENCY             
SVOCLI   DS    CL(L'TAVACLI)       SAVED ORIGINAL SUBSIDIARY CLIENT             
SVDATE   DS    XL3                 SAVED DATE                                   
SVTIME   DS    XL3                 SAVED TIME                                   
SVCLI    DS    CL6                 SAVED CLIENT CODE                            
SVGUKEY  DS    XL(L'IOKEY)         SAVED GUARANTEE KEY                          
SVCAKEY  DS    XL(L'IOKEY)         SAVED CAST KEY                               
                                                                                
CPYSTAT1 DS    X                   COPY ROUTINES' STATUS                        
CPYSTAGX EQU   X'80'               EXCLUDED USES ENCOUNTERED                    
                                                                                
EXCLUSES DS    XL13                PASSED EXCLUDED USE EQUATES                  
                                                                                
SVCKKEY  DS    XL(L'IOKEY)         SAVED CHECK KEY                              
FPCYSTRT DS    XL3                 FIRST PER CYCLE START DATE                   
LPCYSTRT DS    XL3                 LATEST PER CYCLE PAYMENT START DATE          
LFTRSTRT DS    XL3                 LATEST SUBSIDIARY FTRACK START DATE          
                                                                                
ISUBAC   DS    XL1                 INDEX OF SUBSIDIARY AGENCY/CLIENTS           
ASUBAC   DS    AL3                 A(SUBSIDIARY AGENCY/CLIENT ARRAY)            
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        W4 MAINTENANCE REQUEST MAP FIELDS                            *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQGUMOD  DS    CL1                 MODE                                         
RQGURTV  EQU   1                   RETRIEVE                                     
RQGUVFY  EQU   2                   VERIFY                                       
RQGUEXE  EQU   3                   EXECUTE                                      
RQGUSTF  DS    CL8                 STAFF CODE                                   
RQGUSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
RQGUGUA  DS    CL4                 GUARANTEE CODE                               
RQGUATC  DS    CL9                 ATTACHED CORPORATION FID                     
RQGUAGY  DS    CL6                 PRIMARY AGENCY                               
RQGUCLI  DS    CL6                 PRIMARY CLIENT                               
RQGUTYP  DS    CL1                 TYPE                                         
RQGUTYPL EQU   C'L'                TYPE LARGE OVERSCALE                         
RQGUTYPP EQU   C'P'                TYPE PER CYCLE                               
RQGUPST  DS    XL3                 PERIOD START DATE                            
RQGUPEN  DS    XL3                 PERIOD END DATE                              
RQGUAMT  DS    F                   AMOUNT                                       
RQGUBAL  DS    F                   BALANCE                                      
RQGUPOV  DS    CL1                 PAY OVERAGE?                                 
RQGUIPO  DS    CL1                 IGNORE PAY OVERAGE NO ON ESTIMATING?         
RQGUPNH  DS    CL1                 PAY P&H ON USE?                              
RQGUCOM  DS    XL4                 PRIMARY INTERNAL COMMERCIAL NUMBER           
RQGULCK  DS    CL1                 LOCKED?                                      
RQGUEX1  DS    CL3                 EXCLUDED USE 1                               
RQGUEX2  DS    CL3                 EXCLUDED USE 2                               
RQGUEX3  DS    CL3                 EXCLUDED USE 3                               
RQGUEX4  DS    CL3                 EXCLUDED USE 4                               
RQGUEX5  DS    CL3                 EXCLUDED USE 5                               
RQGUEX6  DS    CL3                 EXCLUDED USE 6                               
RQGUEX7  DS    CL3                 EXCLUDED USE 7                               
RQGUEX8  DS    CL3                 EXCLUDED USE 8                               
RQGUEX9  DS    CL3                 EXCLUDED USE 9                               
RQGUEXA  DS    CL3                 EXCLUDED USE 10                              
RQGUEXB  DS    CL3                 EXCLUDED USE 11                              
RQGUEXC  DS    CL3                 EXCLUDED USE 12                              
RQGUEXD  DS    CL3                 EXCLUDED USE 13                              
RQGUWID  DS    CL18                WEB APPLICATION ID                           
RQRCLNQ  EQU   *-RQGUMOD                                                        
                                                                                
I$SUAC   DS    A                   A(SUBSIDIARY AGENCY/CLIENTS)                 
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK1B   08/29/13'                                      
         END                                                                    
