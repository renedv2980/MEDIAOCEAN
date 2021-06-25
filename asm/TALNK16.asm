*          DATA SET TALNK16    AT LEVEL 002 AS OF 05/29/15                      
*PHASE T70416E                                                                  
TALNK16  TITLE 'COMMERCIAL MAINTENANCE UPLOAD SERVER'                           
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TACO,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (320*L'TLDRREC)+1                                                
UPPTRBLK EQU   (320*L'TLDRREC)+1                                                
UCSTTBL  EQU   (300*UCTLNQ)+1                                                   
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK+UCSTTBL                                 
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA16**,RR=RE                                           
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
                                                                                
INPUT    BRAS  RE,COUPLOAD         PROCESS THE INPUT RECORD                     
         J     YES                 EXIT BACK TO DDLINK                          
                                                                                
***********************************************************************         
*        XITS                                                         *         
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
*        PROCESS COMMERCIAL RECORD                                    *         
***********************************************************************         
                                                                                
COUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#COSTA)               
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BRAS  RE,SYNCDB           POSSIBLY SYNC DATABASES                      
                                                                                
         L     RF,AUPPTRS                                                       
         AHI   RF,UPPTRBLK                                                      
         ST    RF,AUCSTTBL         SAVE A(UPDATED CAST TABLE)                   
         MVI   0(RF),X'FF'                                                      
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,EROV501,I$CLMC                   
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   COUP70                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   COUP70                                                           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCOSTF                                   
         JNE   COUP70                                                           
                                                                                
         BRAS  RE,PROWRI           PROCESS WEB APPLICATION RECORD ID            
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL VIA                      
         MVI   TLCOPCD,TLCOICDQ    AGENCY/COMMERCIAL ID                         
         MVC   TLCOIAGY,RQCOAGY                                                 
         MVC   TLCOICID,RQCOCID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCOICOM-TLCOPD),IOKEYSAV                                  
         JNE   COUP20                                                           
                                                                                
         CLC   TLCOICOM,RQCOCOM    IF FOUND, INTERNAL COMMERCIAL                
         JE    COUP10              NUMBER MUST MATCH                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINUS                                  
         J     COUP70                                                           
                                                                                
COUP10   CLC   =C'VC',RQCOWID      IF FOUND AND WEB APPLICATION ID              
         JE    COUP20              IS NOT VITA COMPLETIONS                      
         CLC   =C'RC',RQCOWID                                                   
         JE    COUP20                                                           
         CLI   TLCOIVER,0          COMMERCIAL ID MUST BE                        
         JE    COUP20              FOR VERSION 0 OR 1                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINUS                                  
         J     COUP70                                                           
                                                                                
COUP20   MVI   ACTION,ACTADD       INITIALIZE ACTION TO ADD                     
         OC    RQCOCOM,RQCOCOM                                                  
         JZ    COUP30                                                           
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ    READ FOR COMMERCIAL KEY VIA                  
         MVC   TLCOCCOM,RQCOCOM    INTERNAL COMMERCIAL NUMBER                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   COUP30                                                           
         MVI   ACTION,ACTCHA       IF FOUND, SET ACTION TO CHANGE               
         MVC   SVCOKEY,IOKEY       AND SAVE COMMERCIAL KEY                      
                                                                                
COUP30   BRAS  RE,VALAGY           VALIDATE AGENCY                              
         BRAS  RE,VALCLI           VALIDATE CLIENT                              
         BRAS  RE,VALPRD           VALIDATE PRODUCT                             
         BRAS  RE,VALINM           VALIDATE INTERNET/NEW MEDIA                  
         BRAS  RE,VALPOL           VALIDATE COMMERCIAL POOL                     
         BRAS  RE,VALATT           VALIDATE ATTENTION CODE                      
         BRAS  RE,VALAFM           VALIDATE AFM CONTRACTS/TRACKS                
         GOTOR VALMUS,DMCB,RQCOAGY VALIDATE MUSIC CODES                         
         BRAS  RE,VALALS           VALIDATE VERSION ALIAS                       
                                                                                
         MVI   OTTAB,X'FF'         INITIALIZE ORIGINAL TRACK TABLE              
         MVI   TRKDLIST,X'FF'      AND DELETED TRACK TABLE                      
                                                                                
         CLI   ACTION,ACTADD       IF ACTION IS ADD                             
         JNE   COUP40                                                           
         BRAS  RE,INITADD          PREPARE TO ADD                               
         J     COUP50                                                           
                                                                                
COUP40   BRAS  RE,VALREL           ELSE VALIDATE RELEASED STATUS                
         BRAS  RE,INITCHA          PREPARE TO CHANGE                            
         BRAS  RE,VALWID           AND VALIDATE WEB APPLICATION ID              
         JNE   COUP70                                                           
                                                                                
COUP50   BRAS  RE,VALTYP           VALIDATE TYPE                                
         JNE   COUP70                                                           
         BRAS  RE,VALLFT           VALIDATE LIFT                                
                                                                                
         CLI   TRKALIST,X'FF'      IF TRACKS ARE BEING ADDED                    
         JNE   COUP60                                                           
         CLI   TRKDLIST,X'FF'      OR DELETED                                   
         JE    COUP70                                                           
COUP60   OI    COMSTAT,COTRKCST    SET TO UPDATE CAST                           
                                                                                
COUP70   MVI   OUTPUT,COSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    COUP80                                                           
         MVI   OUTPUT,COSTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    COUP80                                                           
         MVI   OUTPUT,COSTOK2                                                   
                                                                                
COUP80   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   COUP110                                                          
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    COUP110                                                          
         BRAS  RE,EXECADD          ADD                                          
         BRAS  RE,EXECCHA          OR CHANGE COMMERCIAL RECORD                  
         JNE   COUP100                                                          
         TM    COMSTAT,COSEACST+COTYPCST+COMEDCST+COTRKCST                      
         JZ    COUP90              IF CAST NEEDS TO BE UPDATED                  
         GOTOR PROCST,DMCB,AIO3    THEN PROCESS CAST RECORDS                    
COUP90   BRAS  RE,PROALS           THEN PROCESS ALIAS RECORD                    
         BRAS  RE,ADDWTR           THEN ADD WEB TRANSACTION RECORD              
                                                                                
COUP100  GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            *        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
COUP110  OC    RQCOCOM,RQCOCOM                                                  
         JZ    COUP120                                                          
         GOTO1 VHEXOUT,DMCB,RQCOCOM,OUTPUT,L'RQCOCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
COUP120  OC    RQCORID,RQCORID                                                  
         JZ    COUP130                                                          
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',5),               +        
               ('LD_CHARQ',RQCORID),(L'RQCORID,0)                               
                                                                                
COUP130  MVI   OUTPUT,C'N'                                                      
         TM    COMSTAT,CORECUPD                                                 
         JZ    COUP140                                                          
         MVI   OUTPUT,C'Y'                                                      
COUP140  GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',6),               +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#COERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    COUP150             SEND DOWN COMMERCIAL RECORD                  
         CLI   ACTION,ACTADD                                                    
         JE    COUP150                                                          
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#COULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#COSDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSSTF),        +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSCOM),        +        
               ('LD_CHARQ',RQCOCOM),(L'RQCOCOM,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSAVR),        +        
               ('LD_CHARQ',=C'Y'),(1,0)                                         
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
                                                                                
COUP150  BRAS  RE,OUTUCAST         OUTPUT UPDATED CAST RECORDS                  
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR COUPLOAD                                   *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOINUS DC    AL1(ECOINUSX-*),AL2(5),AL1(ERRCATY1),AL1(D#COCID)                
         DC    C'Commercial ID is already in use'                               
ECOINUSX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
EROV501  DC    AL2(501),AL2(EROV501X-EROV501)                                   
         DC    AL2(4,6,14,15,16,18,20,23,24,25,27,28,31,32,33,133,34)           
         DC    AL2(35,36,134,37,38,39,135,72,73,119,139,140,226)                
EROV501X EQU   *                                                                
                                                                                
EROV502  DC    AL2(502),AL2(EROV502X-EROV502)                                   
         DC    AL2(4,6,14,15,16,18,20,23,24,25,27,28,29,30,31,32,33,34)         
         DC    AL2(35,36,37,38,39,42,43,46,47,50,51,54,55,58,59,62,63)          
         DC    AL2(70,72,73,75,83,91,99,119,123,124,133,134,135,139)            
         DC    AL2(140,162,169,176,183,184,185,186,187,188,189,222,226)         
EROV502X EQU   *                                                                
                                                                                
EROV503  DC    AL2(503),AL2(EROV503X-EROV503)                                   
         DC    AL2(4,6,14,15,16,18,20,23,24,25,27,28,31,32,33,133,34)           
         DC    AL2(35,36,134,37,38,39,135,72,73,119,139,140,226)                
EROV503X EQU   *                                                                
                                                                                
EROV504  DC    AL2(504),AL2(EROV504X-EROV504)                                   
         DC    AL2(4,6,14,15,16,18,20,23,24,25,27,28,29,30,31,32,33,34)         
         DC    AL2(35,36,37,38,39,42,43,46,47,50,51,54,55,58,59,62,63)          
         DC    AL2(70,72,73,75,83,91,99,119,123,124,133,134,135,139)            
         DC    AL2(140,162,169,176,183,184,185,186,187,188,189,222,226)         
EROV504X EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
COSTOK1  EQU   1                   NO ERRORS - COMMERCIAL ADDED                 
COSTOK2  EQU   2                   NO ERRORS - COMMERCIAL CHANGED               
COSTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#COMOD                                                    
         CLI   RQCOMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#COSTF                                                    
         OC    RQCOSTF,RQCOSTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         OC    RQCORID,RQCORID     IF REQUEST ID IS NOT PROVIDED                
         JNZ   AR00                                                             
         MVI   BYTE1,D#COCOM                                                    
         OC    RQCOCOM,RQCOCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
AR00     MVI   BYTE1,D#COAGY                                                    
         OC    RQCOAGY,RQCOAGY     ASSERT THAT AGENCY IS PROVIDED               
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#COCID                                                    
         OC    RQCOCID,RQCOCID     ASSERT THAT COMMERCIAL ID IS                 
         JZ    ARMIS               PROVIDED                                     
                                                                                
         MVI   BYTE1,D#COCLI                                                    
         OC    RQCOCLI,RQCOCLI     ASSERT THAT CLIENT IS PROVIDED               
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#COTIT                                                    
         OC    RQCOTIT,RQCOTIT     ASSERT THAT TITLE IS PROVIDED                
         JZ    ARMIS                                                            
                                                                                
         CLI   RQCOTYP,CTYADD      IF COMMERCIAL TYPE IS NOT ADDENDUM           
         JE    AR05                                                             
         OC    RQCOADS,RQCOADS     ASSERT THAT ADDENDUM STATE IS                
         JZ    AR05                NOT PROVIDED                                 
         MVI   BYTE1,D#COADS                                                    
         J     ARNAL                                                            
                                                                                
AR05     MVI   BYTE1,D#COFFC                                                    
         OC    RQCOFFC,RQCOFFC     ASSERT THAT FIRST FIXED CYCLE                
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
         MVI   BYTE1,D#COMED                                                    
         CLI   RQCOMED,0           ASSERT THAT MEDIA IS PROVIDED                
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#COWID                                                    
         OC    RQCOWID,RQCOWID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
         OC    RQCOPRD,RQCOPRD     IF PRODUCT CODE IS PROVIDED                  
         JZ    AR10                                                             
         OC    RQCOPRN,RQCOPRN     ASSERT THAT PRODUCT NAME                     
         JZ    AR10                IS NOT PROVIDED                              
         MVI   BYTE1,D#COPRN                                                    
         J     ARNAL                                                            
                                                                                
AR10     OC    RQCOSAR,RQCOSAR     IF SECOND SEASON FIRST AIR DATE              
         JZ    AR20                IS PROVIDED                                  
         MVI   BYTE1,D#COSAR                                                    
         CLI   RQCOTYP,CTYSEAS2    ASSERT THAT COMMERCIAL TYPE                  
         JNE   ARNAL               IS NEW SEASONAL                              
         MVI   BYTE1,D#COSAR                                                    
         OC    RQCOFAR,RQCOFAR     AND FIRST AIR DATE IS ALSO PROVIDED          
         JZ    ARNAL                                                            
                                                                                
AR20     CLI   RQCOTYP,CTYDEM      IF COMMERCIAL TYPE IS DEMO                   
         JE    AR30                                                             
         CLI   RQCOTYP,CTYSEAS2    OR NEW SEASONAL                              
         JE    AR30                                                             
         CLI   RQCOTYP,CTYIND      OR INDUSTRIAL                                
         JE    AR30                                                             
         CLI   RQCOTYP,CTYSDEM     OR SPANISH DEMO                              
         JE    AR30                                                             
         CLI   RQCOTYP,CTYMUS      OR MUSIC                                     
         JE    AR30                                                             
         CLI   RQCOTYP,CTYPROMO    OR PROMO                                     
         JE    AR30                                                             
         CLI   RQCOTYP,CTYICAT1    OR INDUSTRIAL CATEGORY 1                     
         JNE   AR40                                                             
AR30     OC    RQCOEXP,RQCOEXP     ASSERT THAT EXPIRATION DATE                  
         JZ    AR50                IS NOT PROVIDED                              
         MVI   BYTE1,D#COEXP                                                    
         J     ARNAL                                                            
AR40     OC    RQCOEXP,RQCOEXP     OTHERWISE, ASSERT THAT EXPIRATION            
         JNZ   AR50                DATE IS PROVIDED                             
         MVI   BYTE1,D#COEXP                                                    
         J     ARMIS                                                            
                                                                                
AR50     CLI   RQCOMED,TACOMEDT    IF MEDIA IS TELEVISION                       
         JE    AR60                                                             
         CLI   RQCOMED,TACOMEDI    OR INTERNET                                  
         JE    AR60                                                             
         CLI   RQCOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   AR70                                                             
AR60     MVI   BYTE1,D#COSEC                                                    
         CLI   RQCOSEC,0           ASSERT THAT LENGTH IS PROVIDED               
         JE    ARMIS                                                            
         CLI   RQCOTYP,CTYMUS      AND IF COMMERCIAL TYPE IS NOT                
         JE    AR70                MUSIC                                        
         OC    RQCORDT,RQCORDT     AND RECORDING DATE IS NOT                    
         JNZ   AR70                PROVIDED                                     
         OC    RQCOFDT,RQCOFDT     ASSERT THAT FILM DATE IS PROVIDED            
         JNZ   AR70                                                             
         MVI   BYTE1,D#COFDT                                                    
         J     ARMIS                                                            
                                                                                
AR70     OC    RQCOLID,RQCOLID     IF LIFT ID IS PROVIDED                       
         JZ    AR75                                                             
         CLI   RQCOLLN,0           ASSERT THAT LIFT LENGTH IS                   
         JNE   AR80                PROVIDED                                     
         MVI   BYTE1,D#COLLN                                                    
         J     ARMIS                                                            
                                                                                
AR75     CLI   RQCOLLN,0           IF LIFT ID IS NOT PROVIDED                   
         JE    AR80                ASSERT THAT LIFT LENGTH IS NOT               
         MVI   BYTE1,D#COLLN       PROVIDED                                     
         J     ARNAL                                                            
                                                                                
AR80     CLI   RQCOATY,CCTY04A     IF ACTRA TYPE IS 2404A                       
         JE    AR85                                                             
         CLI   RQCOATY,CCTY2404    OR 2404                                      
         JNE   AR90                                                             
AR85     OC    RQCOAEX,RQCOAEX     ASSERT THAT ACTRA EXPIRATION DATE            
         JNZ   AR100               IS PROVIDED                                  
         MVI   BYTE1,D#COAEX                                                    
         J     ARMIS                                                            
AR90     OC    RQCOAEX,RQCOAEX     OTHERWISE ASSERT THAT ACTRA                  
         JZ    AR100               EXPIRATION DATE IS NOT PROVIDED              
         MVI   BYTE1,D#COAEX                                                    
         J     ARNAL                                                            
                                                                                
AR100    CLI   RQCOMED,TACOMEDR    IF MEDIA IS RADIO                            
         JNE   AR110                                                            
         MVI   BYTE1,D#COFDT                                                    
         OC    RQCOFDT,RQCOFDT     ASSERT THAT FILM DATE IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         CLI   RQCOTYP,CTYMUS      AND IF COMMERCIAL TYPE IS NOT                
         JE    AR110               MUSIC                                        
         OC    RQCORDT,RQCORDT     ASSERT THAT RECORD DATE IS PROVIDED          
         JNZ   AR110                                                            
         MVI   BYTE1,D#CORDT                                                    
         J     ARMIS                                                            
                                                                                
AR110    MVI   BYTE1,D#COFSU                                                    
         OC    RQCOFDT,RQCOFDT     IF FILM DATE IS PROVIDED                     
         JZ    AR120                                                            
         OC    RQCOFSU,RQCOFSU     ASSERT THAT FILM STUDIO IS PROVIDED          
         JZ    ARMIS                                                            
         OC    RQCOFCY,RQCOFCY     AND ASSERT THAT FILM CITY IS                 
         JNZ   AR130               PROVIDED                                     
         MVI   BYTE1,D#COFCY                                                    
         J     ARMIS                                                            
AR120    OC    RQCOFSU,RQCOFSU     OTHERWISE ASSERT THAT FILM STUDIO            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COFCY                                                    
         OC    RQCOFCY,RQCOFCY     AND ASSERT THAT FILM CITY IS NOT             
         JNZ   ARNAL               PROVIDED                                     
         OC    RQCOFST,RQCOFST     AND ASSERT THAT FILM STATE IS NOT            
         JZ    AR130               PROVIDED                                     
         MVI   BYTE1,D#COFST                                                    
         J     ARNAL                                                            
                                                                                
AR130    MVI   BYTE1,D#CORSU                                                    
         OC    RQCORDT,RQCORDT     IF RECORD DATE IS PROVIDED                   
         JZ    AR140                                                            
         OC    RQCORSU,RQCORSU     ASSERT THAT RECORD STUDIO IS                 
         JZ    ARMIS               PROVIDED                                     
         OC    RQCORCY,RQCORCY     AND ASSERT THAT RECORD CITY IS               
         JNZ   AR150               PROVIDED                                     
         MVI   BYTE1,D#CORCY                                                    
         J     ARMIS                                                            
AR140    OC    RQCORSU,RQCORSU     OTHERWISE ASSERT THAT RECORD STUDIO          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#CORCY                                                    
         OC    RQCORCY,RQCORCY     AND ASSERT THAT RECORD CITY IS NOT           
         JNZ   ARNAL               PROVIDED                                     
         OC    RQCORST,RQCORST     AND ASSERT THAT RECORD STATE IS NOT          
         JZ    AR150               PROVIDED                                     
         MVI   BYTE1,D#CORST                                                    
         J     ARNAL                                                            
                                                                                
AR150    CLI   RQCOTYP,CTYMUS      IF COMMERCIAL TYPE IS MUSIC                  
         JNE   AR160                                                            
         MVI   BYTE1,D#COMSU                                                    
         OC    RQCOMDT,RQCOMDT     ASSERT THAT MUSIC DATE IS PROVIDED           
         JZ    ARMIS                                                            
         CLI   RQCOART,0           AND ASSERT THAT AFM RATE IS PROVIDED         
         JNE   AR170                                                            
         MVI   BYTE1,D#COART                                                    
         J     ARMIS                                                            
                                                                                
AR160    MVI   BYTE1,D#COALU       IF COMMERCIAL TYPE IS NOT MUSIC              
         CLI   RQCOALU,0           ASSERT THAT ALLOWABLE USES IS                
         JNE   ARNAL               NOT PROVIDED                                 
         CLI   RQCORMU,0           AND ASSERT THAT REMAINING USES               
         JE    AR170               IS NOT PROVIDED                              
         MVI   BYTE1,D#CORMU                                                    
         J     ARNAL                                                            
                                                                                
AR170    MVI   BYTE1,D#COMSU                                                    
         OC    RQCOMDT,RQCOMDT     IF MUSIC DATE IS PROVIDED                    
         JZ    AR180                                                            
         OC    RQCOMSU,RQCOMSU     ASSERT THAT MUSIC STUDIO IS                  
         JZ    ARMIS               PROVIDED                                     
         OC    RQCOMCY,RQCOMCY     AND ASSERT THAT MUSIC CITY IS                
         JNZ   AR190               PROVIDED                                     
         MVI   BYTE1,D#COMCY                                                    
         J     ARMIS                                                            
AR180    OC    RQCOMSU,RQCOMSU     OTHERWISE ASSERT THAT MUSIC STUDIO           
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COMCY                                                    
         OC    RQCOMCY,RQCOMCY     AND ASSERT THAT MUSIC CITY IS NOT            
         JNZ   ARNAL               PROVIDED                                     
         OC    RQCOMST,RQCOMST     AND ASSERT THAT MUSIC STATE IS NOT           
         JZ    AR190               PROVIDED                                     
         MVI   BYTE1,D#COMST                                                    
         J     ARNAL                                                            
                                                                                
AR190    CLI   RQCOMED,TACOMEDI    IF MEDIA IS NOT INTERNET                     
         JE    AR200                                                            
         CLI   RQCOMED,TACOMEDN    OR NEW MEDIA                                 
         JE    AR200                                                            
         MVI   BYTE1,D#COIN1                                                    
         OC    RQCOIN1,RQCOIN1     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 1 IS NOT PROVIDED                       
         MVI   BYTE1,D#COIN2                                                    
         OC    RQCOIN2,RQCOIN2     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 2 IS NOT PROVIDED                       
         MVI   BYTE1,D#COIN3                                                    
         OC    RQCOIN3,RQCOIN3     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 3 IS NOT PROVIDED                       
         MVI   BYTE1,D#COIN4                                                    
         OC    RQCOIN4,RQCOIN4     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 4 IS NOT PROVIDED                       
         MVI   BYTE1,D#COIN5                                                    
         OC    RQCOIN5,RQCOIN5     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 5 IS NOT PROVIDED                       
         MVI   BYTE1,D#COIN6                                                    
         OC    RQCOIN6,RQCOIN6     ASSERT THAT INTERNET/NEW MEDIA               
         JNZ   ARNAL               CODE 6 IS NOT PROVIDED                       
                                                                                
AR200    OC    RQCOEYR,RQCOEYR     IF EDIT TYPE YEAR IS PROVIDED                
         JZ    AR210                                                            
         OC    RQCOETY,RQCOETY     ASSERT THAT EDIT TYPE IS PROVIDED            
         JNZ   AR220                                                            
         MVI   BYTE1,D#COETY                                                    
         J     ARMIS                                                            
AR210    OC    RQCOETY,RQCOETY     OTHERWISE ASSERT THAT EDIT TYPE              
         JZ    AR220               IS NOT PROVIDED                              
         MVI   BYTE1,D#COETY                                                    
         J     ARNAL                                                            
                                                                                
AR220    CLI   RQCOTYP,CTYMUS      IF COMMERCIAL TYPE IS MUSIC ...              
         JNE   AR290                                                            
                                                                                
         MVI   BYTE1,D#COA1O                                                    
         OC    RQCOA1O,RQCOA1O     ASSERT THAT AFM 1 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA2O                                                    
         OC    RQCOA2O,RQCOA2O     ASSERT THAT AFM 2 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA3O                                                    
         OC    RQCOA3O,RQCOA3O     ASSERT THAT AFM 3 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA4O                                                    
         OC    RQCOA4O,RQCOA4O     ASSERT THAT AFM 4 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA5O                                                    
         OC    RQCOA5O,RQCOA5O     ASSERT THAT AFM 5 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA6O                                                    
         OC    RQCOA6O,RQCOA6O     ASSERT THAT AFM 6 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
         MVI   BYTE1,D#COA7O                                                    
         OC    RQCOA7O,RQCOA7O     ASSERT THAT AFM 7 INTERNAL COMM'L            
         JNZ   ARNAL               NUMBER IS NOT PROVIDED                       
                                                                                
         CLI   RQCOA1T,0           IF AFM 1 LIFT IS NOT PROVIDED                
         JNE   AR230                                                            
         MVI   BYTE1,D#COA1L                                                    
         CLI   RQCOA1L,0           ASSERT THAT AFM 1 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA1S                                                    
         OC    RQCOA1S,RQCOA1S     ASSERT THAT AFM 1 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA1I                                                    
         OC    RQCOA1I,RQCOA1I     ASSERT THAT AFM 1 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA1Y                                                    
         CLI   RQCOA1Y,0           ASSERT THAT AFM 1 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR230    CLI   RQCOA2T,0           IF AFM 2 LIFT IS NOT PROVIDED                
         JNE   AR240                                                            
         MVI   BYTE1,D#COA2L                                                    
         CLI   RQCOA2L,0           ASSERT THAT AFM 2 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA2S                                                    
         OC    RQCOA2S,RQCOA2S     ASSERT THAT AFM 2 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA2I                                                    
         OC    RQCOA2I,RQCOA2I     ASSERT THAT AFM 2 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA2Y                                                    
         CLI   RQCOA2Y,0           ASSERT THAT AFM 2 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR240    CLI   RQCOA3T,0           IF AFM 3 LIFT IS NOT PROVIDED                
         JNE   AR250                                                            
         MVI   BYTE1,D#COA3L                                                    
         CLI   RQCOA3L,0           ASSERT THAT AFM 3 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA3S                                                    
         OC    RQCOA3S,RQCOA3S     ASSERT THAT AFM 3 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA3I                                                    
         OC    RQCOA3I,RQCOA3I     ASSERT THAT AFM 3 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA3Y                                                    
         CLI   RQCOA3Y,0           ASSERT THAT AFM 3 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR250    CLI   RQCOA4T,0           IF AFM 4 LIFT IS NOT PROVIDED                
         JNE   AR260                                                            
         MVI   BYTE1,D#COA4L                                                    
         CLI   RQCOA4L,0           ASSERT THAT AFM 4 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA4S                                                    
         OC    RQCOA4S,RQCOA4S     ASSERT THAT AFM 4 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA4I                                                    
         OC    RQCOA4I,RQCOA4I     ASSERT THAT AFM 4 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA4Y                                                    
         CLI   RQCOA4Y,0           ASSERT THAT AFM 4 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR260    CLI   RQCOA5T,0           IF AFM 5 LIFT IS NOT PROVIDED                
         JNE   AR270                                                            
         MVI   BYTE1,D#COA5L                                                    
         CLI   RQCOA5L,0           ASSERT THAT AFM 5 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA5S                                                    
         OC    RQCOA5S,RQCOA5S     ASSERT THAT AFM 5 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA5I                                                    
         OC    RQCOA5I,RQCOA5I     ASSERT THAT AFM 5 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA5Y                                                    
         CLI   RQCOA5Y,0           ASSERT THAT AFM 5 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR270    CLI   RQCOA6T,0           IF AFM 6 LIFT IS NOT PROVIDED                
         JNE   AR280                                                            
         MVI   BYTE1,D#COA6L                                                    
         CLI   RQCOA6L,0           ASSERT THAT AFM 6 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA6S                                                    
         OC    RQCOA6S,RQCOA6S     ASSERT THAT AFM 6 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA6I                                                    
         OC    RQCOA6I,RQCOA6I     ASSERT THAT AFM 6 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA6Y                                                    
         CLI   RQCOA6Y,0           ASSERT THAT AFM 6 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR280    CLI   RQCOA7T,0           IF AFM 7 LIFT IS NOT PROVIDED                
         JNE   AR440                                                            
         MVI   BYTE1,D#COA7L                                                    
         CLI   RQCOA7L,0           ASSERT THAT AFM 7 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA7S                                                    
         OC    RQCOA7S,RQCOA7S     ASSERT THAT AFM 7 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COA7S                                                    
         OC    RQCOA7I,RQCOA7I     ASSERT THAT AFM 7 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COA7Y                                                    
         CLI   RQCOA7Y,0           ASSERT THAT AFM 7 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
       ++INCLUDE TAARAFM                                                        
                                                                                
AR430    MVI   BYTE,D#COA1L                                                     
         CLI   RQCOA1L,0           ASSERT THAT AFM 1 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA1S                                                     
         OC    RQCOA1S,RQCOA1S     ASSERT THAT AFM 1 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA1I                                                     
         OC    RQCOA1I,RQCOA1I     ASSERT THAT AFM 1 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA1Y                                                     
         CLI   RQCOA1Y,0           ASSERT THAT AFM 1 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA2L                                                     
         CLI   RQCOA2L,0           ASSERT THAT AFM 2 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA2S                                                     
         OC    RQCOA2S,RQCOA2S     ASSERT THAT AFM 2 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA2I                                                     
         OC    RQCOA2I,RQCOA2I     ASSERT THAT AFM 2 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA2Y                                                     
         CLI   RQCOA2Y,0           ASSERT THAT AFM 2 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA3L                                                     
         CLI   RQCOA3L,0           ASSERT THAT AFM 3 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA3S                                                     
         OC    RQCOA3S,RQCOA3S     ASSERT THAT AFM 3 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA3I                                                     
         OC    RQCOA3I,RQCOA3I     ASSERT THAT AFM 3 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA3Y                                                     
         CLI   RQCOA3Y,0           ASSERT THAT AFM 3 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA4L                                                     
         CLI   RQCOA4L,0           ASSERT THAT AFM 4 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA4S                                                     
         OC    RQCOA4S,RQCOA4S     ASSERT THAT AFM 4 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA4I                                                     
         OC    RQCOA4I,RQCOA4I     ASSERT THAT AFM 4 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA4Y                                                     
         CLI   RQCOA4Y,0           ASSERT THAT AFM 4 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA5L                                                     
         CLI   RQCOA5L,0           ASSERT THAT AFM 5 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA5S                                                     
         OC    RQCOA5S,RQCOA5S     ASSERT THAT AFM 5 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA5I                                                     
         OC    RQCOA5I,RQCOA5I     ASSERT THAT AFM 5 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA5Y                                                     
         CLI   RQCOA5Y,0           ASSERT THAT AFM 5 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA6L                                                     
         CLI   RQCOA6L,0           ASSERT THAT AFM 6 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA6S                                                     
         OC    RQCOA6S,RQCOA6S     ASSERT THAT AFM 6 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA6I                                                     
         OC    RQCOA6I,RQCOA6I     ASSERT THAT AFM 6 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA6Y                                                     
         CLI   RQCOA6Y,0           ASSERT THAT AFM 6 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
         MVI   BYTE,D#COA7L                                                     
         CLI   RQCOA7L,0           ASSERT THAT AFM 7 LIFT IS NOT                
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA7S                                                     
         OC    RQCOA7S,RQCOA7S     ASSERT THAT AFM 7 LENGTH IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE,D#COA7I                                                     
         OC    RQCOA7I,RQCOA7I     ASSERT THAT AFM 7 TRACK TITLE                
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE,D#COA7Y                                                     
         CLI   RQCOA7Y,0           ASSERT THAT AFM 7 MUSIC TYPE                 
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
                                                                                
AR440    CLI   RQCOTYP,CTYMUS                                                   
         JE    YES                                                              
         CLI   RQCOTYP,CTYPRNT                                                  
         JE    YES                                                              
         MVI   BYTE1,D#COCON                                                    
         CLI   RQCOMED,TACOMEDP    IF MEDIA IS PRINT                            
         JE    AR450                                                            
         CLI   RQCOMED,TACOMEDE    OR MEDIA IS EVENT                            
         JNE   AR460               ASSERT THAT CONTRACT TYPE IS NOT             
AR450    CLI   RQCOCON,0           PROVIDED                                     
         JNE   ARNAL                                                            
         J     YES                                                              
AR460    CLI   RQCOCON,0           OTHERWISE ASEET THAT CONTRACT TYPE           
         JE    ARMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
ARMIS    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENMIS',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
ARNAL    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENNAL',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
ASRTVAL  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#COMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCOMOD)                        
         JNE   AVINV                                                            
                                                                                
*                                  ASSERT ALL PROVIDED VALUES                   
*                                  (COMMON BETWEEN ASSETS AND                   
         BAS   RE,ACVAV            VERSIONS) ARE VALID                          
                                                                                
         MVI   BYTE1,D#COADS       VALIDATE ADDENDUM STATE                      
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFADST',RQCOADS)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#COLID       VALIDATE LIFT ID                             
         CLC   RQCOLID,RQCOCID                                                  
         JE    AVINV                                                            
                                                                                
         MVI   BYTE1,D#COATY       VALIDATE ACTRA TYPE                          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFACTY',RQCOATY),              +        
               (X'80',RQCOMED)                                                  
         JNE   AVINV                                                            
         CLI   RQCOCDO,C'Y'                                                     
         JE    AV20                                                             
         CLI   RQCOCRT,C'Y'                                                     
         JNE   AV30                                                             
AV20     CLI   RQCOATY,CCTY04A                                                  
         JE    NO                                                               
         CLI   RQCOATY,CCTY04B                                                  
         JE    NO                                                               
         CLI   RQCOATY,CCTY2404                                                 
         JE    NO                                                               
                                                                                
AV30     OC    RQCOIDT,RQCOIDT     INACTIVE DATE MUST BE EQUAL TO OR            
         JZ    AV40                LATER THAN ACTIVE DATE                       
         MVI   BYTE1,D#COIDT                                                    
         CLC   RQCOADT,RQCOIDT                                                  
         JH    NO                                                               
                                                                                
AV40     MVI   BYTE1,D#COSCY       VALIDATE HAS SPLIT CYCLES?                   
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSCY)                        
         JNE   AVINV                                                            
                                                                                
         CLI   RQCOART,0           VALIDATE AFM RATE                            
         JE    AV50                                                             
         CLI   RQCOART,C'1'                                                     
         JE    AV50                                                             
         CLI   RQCOART,C'2'                                                     
         JE    AV50                                                             
         CLI   RQCOART,C'5'                                                     
         JE    AV50                                                             
         MVI   BYTE1,D#COART                                                    
         J     AVINV                                                            
                                                                                
AV50     CLI   RQCOSTY,0           VALIDATE SESSION TYPE                        
         JE    AV60                                                             
         CLI   RQCOSTY,C'D'                                                     
         JE    AV60                                                             
         CLI   RQCOSTY,C'F'                                                     
         JE    AV60                                                             
         CLI   RQCOSTY,C'M'                                                     
         JE    AV60                                                             
         CLI   RQCOSTY,C'T'                                                     
         JE    AV60                                                             
         MVI   BYTE1,D#COSTY                                                    
         J     AVINV                                                            
                                                                                
AV60     MVI   BYTE1,D#COLCK       VALIDATE COMMERCIAL LOCKED?                  
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOLCK)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#COREL       VALIDATE COMMERCIAL RELEASED?                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOREL)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#COWRK       VALIDATE WORK DATE ON CHECKS?                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOWRK)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#COSOP       VALIDATE SOAP RESIDUALS?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSOP)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#COPRT       VALIDATE DISPLAY AS PRINT?                   
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOPRT)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#COVAL       VALIDATE ALIAS                               
         CLI   RQCOALS,1                                                        
         JE    AVINV                                                            
                                                                                
         MVI   BYTE1,D#CONCS       VALIDATE CHARGE CSF?                         
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCONCS)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#COHFN       VALIDATE GEN HF NOTICES?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOHFN)                        
         JNE   AVINV                                                            
                                                                                
         CLI   RQCOCON,0           IF CONTRACT TYPE IS PROVIDED ...             
         JE    AV80                                                             
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGACONTS         RF=DISPLACEMENT OF CONTRACT TYPES            
         DROP  RE                                                               
                                                                                
         USING CONTD,RF                                                         
         AR    RF,RE               FIND EQUATE IN CONTRACT TYPE TABLE           
         MVI   BYTE1,D#COCON                                                    
AV70     CLC   RQCOCON,CONTEQU                                                  
         JE    AV80                                                             
         CLI   CONNEXT,X'FF'                                                    
         JE    AVINV                                                            
         LA    RF,CONNEXT                                                       
         J     AV70                                                             
         DROP  RF                                                               
                                                                                
AV80     GOTOR (#VALFLD,AVALFLD),DMCB,('VFWID',RQCOWID)                         
         JE    YES                                                              
         MVI   BYTE1,D#COWID       VALIDATE WEB APPLICATION ID                  
         J     AVINV                                                            
                                                                                
       ++INCLUDE TACVAV                                                         
                                                                                
***********************************************************************         
*        INVALID FIELD ERROR                                          *         
***********************************************************************         
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ATTEMPTS TO SYNC UP MF/VITA DATABASES ON TEST/FQA    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
SYNCDB   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOCOM,RQCOCOM     IF INTERNAL COMMERCIAL NUMBER                
         JZ    XIT                 IS PROVIDED                                  
         CLC   RQCOCOM,=F'5000000' AND IS LESS THAN 5M ...                      
         JNL   XIT                                                              
                                                                                
         USING TLSYD,R3                                                         
         XC    TLSYKEY,TLSYKEY     READ SYSTEM KEY/RECORD                       
         MVI   TLSYCD,TLSYCDQ                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL            IF WE'RE ON TEST OR FQA                      
         JE    *+6                 AND PROVIDED INTERNAL COMM'L                 
         DC    H'00'               NUMBER IS HIGHER THAN SYSTEM RECORD          
         CLC   =C'DPS2',TASYIDCD   COUNTER, ADJUST SYSTEM RECORD                
         JNE   XIT                 COUNTER                                      
         CLC   RQCOCOM,TASYLCOM                                                 
         JL    XIT                                                              
         MVC   TASYLCOM,RQCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS WEB APPLICATION RECORD ID                            *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
PROWRI   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOWRI,RQCOWRI     IF WEB APPLICATION RECORD ID IS              
         JZ    XIT                 PROVIDED                                     
         OC    RQCOCOM,RQCOCOM     AND INTERNAL COMMERCIAL NUMBER IS            
         JNZ   XIT                 NOT PROVIDED                                 
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   IF WEB APPLICATION RECORD IS FOUND           
         MVI   TLCOPCD,TLCOWCDQ    ON AN EXISTING COMMERCIAL ...                
         MVC   TLCOWWID,RQCOWRI                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEY(TLCOWCOM-TLCOPD),IOKEYSAV                                  
         JNE   XIT                                                              
         MVC   RQCOCOM,TLCOWCOM    ... SAVE INTERNAL COMMERCIAL NUMBER          
         J     XIT                 INTO REQUEST MAP                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AGENCY CODE                                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLAYD,R3                                                         
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY/RECORD                   
         MVI   TLAYCD,TLAYCDQ      AND ENSURE IT EXISTS                         
         MVC   TLAYAGY,RQCOAGY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VA10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOAYNF                                  
         J     XIT                                                              
VA10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            R4=A(AGENCY ELEMENT)                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYSTA3,TAAYSLCK   ENSURE AGENCY IS NOT LOCKED                  
         JZ    VA20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOAYLK                                  
                                                                                
VA20     TM    TAAYSTA6,TAAYST10   IF TYPE 10 JOB VALIDATION ...                
         JO    VA30                                                             
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TABRSTAT,TABRSINT   ... OR AGENCY IS ON INTERFACE ...            
         JZ    XIT                                                              
         DROP  R4                                                               
                                                                                
VA30     OC    RQCOPRD,RQCOPRD     ENSURE PRODUCT IS POPULATED                  
         JNZ   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOPRRQ                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGY                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOAYNF DC    AL1(ECOAYNFX-*),AL2(1),AL1(ERRCATY1),AL1(D#COAGY)                
         DC    C'Agency record is not on file'                                  
ECOAYNFX EQU   *                                                                
                                                                                
ERCOAYLK DC    AL1(ECOAYLKX-*),AL2(2),AL1(ERRCATY1),AL1(D#COAGY)                
         DC    C'Agency record is locked'                                       
ECOAYLKX EQU   *                                                                
                                                                                
ERCOPRRQ DC    AL1(ECOPRRQX-*),AL2(11),AL1(ERRCATY1),AL1(D#COPRD)               
         DC    C'Product is required'                                           
ECOPRRQX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED CLIENT CODE                                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLCLD,R3                                                         
VALCLI   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCLKEY,TLCLKEY     READ FOR CLIENT KEY/RECORD                   
         MVI   TLCLCD,TLCLCDQ      AND ENSURE IT EXISTS                         
         MVC   TLCLAGY,RQCOAGY                                                  
         MVC   TLCLCLI,RQCOCLI                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VC10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCLNF                                  
         J     XIT                                                              
VC10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TABRD,R4                                                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL            R4=A(BILLING RULES ELEMENT)                  
         JNE   VC20                                                             
         CLC   =C'PP ',TABROEOR    ENSURE EMPLOYER IS NOT PRINT                 
         JNE   VC20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCLPR                                  
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
VC20     L     R4,AIO3                                                          
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL            R4=A(CLIENT INFORMATION ELEMENT)             
         JNE   XIT                                                              
         MVC   SVCICLG,TACICLG     SAVE CLIENT GROUP                            
                                                                                
         CLI   ACTION,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
         TM    TACISTAT,TACISLCK ENSURE CLIENT IS NOT LOCKED                    
         JZ    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCLLK                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCLI                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOCLNF DC    AL1(ECOCLNFX-*),AL2(7),AL1(ERRCATY1),AL1(D#COCLI)                
         DC    C'Client record is not on file'                                  
ECOCLNFX EQU   *                                                                
                                                                                
ERCOCLPR DC    AL1(ECOCLPRX-*),AL2(8),AL1(ERRCATY1),AL1(D#COCLI)                
         DC    C'Client is a print employer'                                    
ECOCLPRX EQU   *                                                                
                                                                                
ERCOCLLK DC    AL1(ECOCLLKX-*),AL2(9),AL1(ERRCATY1),AL1(D#COCLI)                
         DC    C'Client record is locked'                                       
ECOCLLKX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED PRODUCT CODE                                 *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALPRD   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOPRD,RQCOPRD     ONLY VALIDATE IF PRODUCT CODE HAS            
         JZ    XIT                 BEEN PASSED                                  
                                                                                
         USING TLPRD,R3                                                         
         XC    TLPRKEY,TLPRKEY     READ FOR PRODUCT KEY                         
         MVI   TLPRCD,TLPRCDQ      AND ENSURE IT EXISTS                         
         MVC   TLPRAGY,RQCOAGY                                                  
         MVC   TLPRCLI,RQCOCLI                                                  
         MVC   TLPRPRD,RQCOPRD                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VP10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOPRNF                                  
         J     XIT                                                              
VP10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAPID,R4                                                         
         CLI   ACTION,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
         MVI   ELCODE,TAPIELQ                                                   
         BRAS  RE,GETEL            R4=A(PRODUCT INFORMATION ELEMENT)            
         JNE   XIT                                                              
         TM    TAPISTAT,TAPISLCK   ENSURE PRODUCT IS NOT LOCKED                 
         JZ    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOPRLK                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALPRD                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOPRNF DC    AL1(ECOPRNFX-*),AL2(12),AL1(ERRCATY1),AL1(D#COPRD)               
         DC    C'Product record is not on file'                                 
ECOPRNFX EQU   *                                                                
                                                                                
ERCOPRLK DC    AL1(ECOPRLKX-*),AL2(131),AL1(ERRCATY1),AL1(D#COPRD)              
         DC    C'Product record is locked'                                      
ECOPRLKX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED INTERNET/NEW MEDIA CODES                     *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALINM   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RQCOIN1          R2=A(FIRST INTERNET/NEW MEDIA FIELD)         
         LHI   R0,6                R0=# OF INTERNET/NEW MEDIA FIELDS            
                                                                                
         USING ERRENTD,R4                                                       
         LA    R4,ERCOI1NF         R4=A(FIRST ERROR ENTRY)                      
                                                                                
         CLI   RQCOMED,TACOMEDI    IF COMMERCIAL MEDIA IS INTERNET              
         JNE   VIN10                                                            
         MVI   MEDIA,INTERNET      SET MEDIA AS INTERNET                        
                                                                                
VIN10    CLI   RQCOMED,TACOMEDN    IF COMMERCIAL MEDIA IS NEW MEDIA             
         JNE   VIN20                                                            
         LA    R4,ERCON1NF         R3=A(FIRST ERROR ENTRY)                      
         MVI   MEDIA,NEWMEDIA      AND SET MEDIA AS NEW MEDIA                   
                                                                                
         USING TLMDD,R3                                                         
VIN20    OC    0(L'RQCOIN1,R2),0(R2)                                            
         JZ    VIN30                                                            
         XC    TLMDKEY,TLMDKEY     IF CURRENT FIELD IS POPULATED                
         MVI   TLMDCD,TLMDCDQ      READ FOR INTERNET/NEW MEDIA KEY              
         MVC   TLMDTYPE,MEDIA      AND ENSURE IT EXISTS                         
         MVC   TLMDCODE,0(R2)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VIN30                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,(R4)                                      
         DROP  R3                                                               
                                                                                
VIN30    LA    R2,L'RQCOIN1(R2)    BUMP TO THE NEXT INTERNET/NEW MEDIA          
         ZIC   RE,EELEN            FIELD AND ERROR MESSAGE                      
         AR    R4,RE               AND GO VALIDATE                              
         BCT   R0,VIN20                                                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALINM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOI1NF DC    AL1(ECOI1NFX-*),AL2(40),AL1(ERRCATY1),AL1(D#COIN1)               
         DC    C'Internet code not on file'                                     
ECOI1NFX EQU   *                                                                
                                                                                
ERCOI2NF DC    AL1(ECOI2NFX-*),AL2(44),AL1(ERRCATY1),AL1(D#COIN2)               
         DC    C'Internet code not on file'                                     
ECOI2NFX EQU   *                                                                
                                                                                
ERCOI3NF DC    AL1(ECOI3NFX-*),AL2(48),AL1(ERRCATY1),AL1(D#COIN3)               
         DC    C'Internet code not on file'                                     
ECOI3NFX EQU   *                                                                
                                                                                
ERCOI4NF DC    AL1(ECOI4NFX-*),AL2(52),AL1(ERRCATY1),AL1(D#COIN4)               
         DC    C'Internet code not on file'                                     
ECOI4NFX EQU   *                                                                
                                                                                
ERCOI5NF DC    AL1(ECOI5NFX-*),AL2(56),AL1(ERRCATY1),AL1(D#COIN5)               
         DC    C'Internet code not on file'                                     
ECOI5NFX EQU   *                                                                
                                                                                
ERCOI6NF DC    AL1(ECOI6NFX-*),AL2(60),AL1(ERRCATY1),AL1(D#COIN6)               
         DC    C'Internet code not on file'                                     
ECOI6NFX EQU   *                                                                
                                                                                
ERCON1NF DC    AL1(ECON1NFX-*),AL2(41),AL1(ERRCATY1),AL1(D#COIN1)               
         DC    C'New Media code not on file'                                    
ECON1NFX EQU   *                                                                
                                                                                
ERCON2NF DC    AL1(ECON2NFX-*),AL2(45),AL1(ERRCATY1),AL1(D#COIN2)               
         DC    C'New Media code not on file'                                    
ECON2NFX EQU   *                                                                
                                                                                
ERCON3NF DC    AL1(ECON3NFX-*),AL2(49),AL1(ERRCATY1),AL1(D#COIN3)               
         DC    C'New Media code not on file'                                    
ECON3NFX EQU   *                                                                
                                                                                
ERCON4NF DC    AL1(ECON4NFX-*),AL2(53),AL1(ERRCATY1),AL1(D#COIN4)               
         DC    C'New Media code not on file'                                    
ECON4NFX EQU   *                                                                
                                                                                
ERCON5NF DC    AL1(ECON5NFX-*),AL2(57),AL1(ERRCATY1),AL1(D#COIN5)               
         DC    C'New Media code not on file'                                    
ECON5NFX EQU   *                                                                
                                                                                
ERCON6NF DC    AL1(ECON6NFX-*),AL2(61),AL1(ERRCATY1),AL1(D#COIN6)               
         DC    C'New Media code not on file'                                    
ECON6NFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED COMMERCIAL POOL CODE                         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALPOL   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOPOL,RQCOPOL     ONLY VALIDATE IF COMMERCIAL POOL             
         JZ    XIT                 CODE HAS BEEN PASSED                         
                                                                                
         USING TLOGD,R3                                                         
         XC    TLOGKEY,TLOGKEY     READ FOR COMMERCIAL POOL KEY                 
         MVI   TLOGCD,TLOGCDQ      AND ENSURE IT EXISTS                         
         MVC   TLOGAGY,RQCOAGY                                                  
         MVC   TLOGCLI,RQCOCLI                                                  
         MVC   TLOGPRD,RQCOPRD                                                  
         MVC   TLOGCOG,RQCOPOL                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINPL                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALPOL                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOINPL DC    AL1(ECOINPLX-*),AL2(68),AL1(ERRCATY1),AL1(D#COPOL)               
         DC    C'Commercial Pool not on file'                                   
ECOINPLX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED COMMERCIAL ATTENTION CODE                    *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALATT   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOATT,RQCOATT     ONLY VALIDATE IF ATTENTION CODE              
         JZ    XIT                 HAS BEEN PASSED                              
                                                                                
         USING TLATD,R3                                                         
         XC    TLATKEY,TLATKEY     READ FOR ATTENTION KEY                       
         MVI   TLATCD,TLATCDQ      AND ENSURE IT EXISTS                         
         MVC   TLATAGY,RQCOAGY                                                  
         MVC   TLATATT,RQCOATT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOATNF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALATT                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOATNF DC    AL1(ECOATNFX-*),AL2(71),AL1(ERRCATY1),AL1(D#COATT)               
         DC    C'Bill to code not on file'                                      
ECOATNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED VERSION ALIAS                                *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALALS   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCOALS,0           ONLY VALIDATE IF VERSION ALIAS               
         JE    XIT                 HAS BEEN PASSED                              
                                                                                
         USING TLCOD,R3                                                         
         XC    TLCOKEY,TLCOKEY     READ FOR INDEXED COMMERCIAL                  
         MVI   TLCOCD,TLCOCDQ      KEY/RECORD                                   
         GOTOR SETINDEX,DMCB,RQCOALS,TLCOVER                                    
         MVC   TLCOAGY,RQCOAGY                                                  
         MVC   TLCOCLI,RQCOCLI                                                  
         MVC   TLCOPRD,RQCOPRD                                                  
         MVC   TLCOCID,RQCOCID                                                  
         MVC   TLCOCOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   VALS40                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO3             ENSURE VERSION CODE EXISTS                   
         MVI   ELCODE,TAVRELQ      ON COMMERCIAL                                
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VALS10   BRAS  RE,NEXTEL                                                        
         JNE   VALS40                                                           
         CLC   TAVRVERS,RQCOALS                                                 
         JNE   VALS10                                                           
                                                                                
         USING TLAKD,R3                                                         
         XC    TLAKKEY,TLAKKEY     ENSURE ALIASED VERSION IS NOT                
         MVI   TLAKCD,TLAKCDQ      ALIASED TO ANOTHER VERSION                   
         MVC   TLAKAGY,RQCOAGY                                                  
         MVC   TLAKADID,TAVRCID                                                 
         MVC   TLAKNCLI,RQCOCLI                                                 
         MVC   TLAKNPRD,RQCOPRD                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VALS30                                                           
VALS20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VALS30   CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   XIT                                                              
         CLC   TLAKCOM,RQCOCOM                                                  
         JNE   VALS20                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOALSD                                  
         J     XIT                                                              
         DROP  R3,R4                                                            
                                                                                
VALS40   GOTOR (#ADDERR,AADDERR),DMCB,ERCOVANF                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE RETURNS COMMERCIAL RECORD INDEX FOR VERSION          *         
*        ON ENTRY ... P1=A(VERSION NUMBER)                            *         
*                     P2=A(FIELD TO STORE INDEX IN)                   *         
***********************************************************************         
                                                                                
SETINDEX NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)                                                         
                                                                                
         LA    RE,VERINDEX                                                      
SI10     CLC   0(1,RF),0(RE)     FIND PASSED IN VERSION NUMBER                  
         JNH   SI20              IN VERINDEX TABLE                              
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         JNE   SI10                                                             
                                                                                
SI20     L     RF,4(R1)          SET COMMERCIAL INDEX EQUATE                    
         MVC   0(1,RF),1(RE)                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE VERSION       *         
*        CODE IS ON                                                   *         
***********************************************************************         
                                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALALS                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOVANF DC    AL1(ECOVANFX-*),AL2(128),AL1(ERRCATY1),AL1(D#COVAL)              
         DC    C'Version Code not on file'                                      
ECOVANFX EQU   *                                                                
                                                                                
ERCOALSD DC    AL1(ECOALSDX-*),AL2(129),AL1(ERRCATY1),AL1(D#COVAL)              
         DC    C'Version has an Alias'                                          
ECOALSDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF COMMERCIAL RECORD        *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   RQCOVER,1                                                        
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCONF                                  
                                                                                
         MVC   TRKALIST,ITTAB                                                   
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   XIT                 AND INTERNAL COMMERCIAL NUMBER               
         OC    RQCOCOM,RQCOCOM     IS NOT PROVIDED                              
         JNZ   XIT                 ENSURE CHECK LOCKOUT STATUS IS OFF           
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_CHECKS',ERCOUCLK                 
         JE    XIT                                                              
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_PRCHKS',ERCOUCLK                 
         JE    XIT                                                              
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_P+CHKS',ERCOUCLK                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOCONF DC    AL1(ECOCONFX-*),AL2(4),AL1(ERRCATY2),AL1(D#COCID)                
         DC    C'Commercial record is not on file'                              
ECOCONFX EQU   *                                                                
                                                                                
ERCOUCLK DC    AL1(ECOUCLKX-*),AL2(196),AL1(ERRCATY3),AL1(D#COCOM)              
         DC    C'Unable to Submit to MF - urgent check run in progress'         
ECOUCLKX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED RELEASED? STATUS                             *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALREL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCOREL,C'Y'        IF RELEASED STATUS IS BEING                  
         JNE   XIT                 TURNED ON                                    
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   READ ALL HOLDING FEE ELIGIBLE                
         MVI   TLCAPCD,TLCAHCDQ    CAST KEYS/RECORDS                            
         MVC   TLCAHCOM,RQCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
VR10     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLI   TLCAPCD,TLCAHCDQ                                                 
         JNE   XIT                                                              
         CLC   TLCAHCOM,RQCOCOM                                                 
         JNE   XIT                                                              
         MVC   SVCAHKEY,TLCAPKEY   SAVE CAST HOLDING FEE KEY                    
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            R4=A(CAST DETAILS ELEMENT)                   
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST DOES NOT HAVE A GUARANTEE            
         JZ    VR10                                                             
         OC    TACALAST,TACALAST   OR HAS A LAST SERVICES DATE                  
         JNZ   VR10                GO READ NEXT CAST KEY                        
         MVC   FULL1,TACAGUA                                                    
         DROP  R4                                                               
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE KEY/RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCAHKEY+TLCAHSSN-TLCAPD                                 
         MVC   TLGUGUA,FULL1                                                    
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   TLGUKEY,IOKEYSAV                                                 
         JNE   VR50                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGUELQ      R4=A(GUARANTEE DETAILS ELEMENT)              
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   RQCOCOM,TAGUCOM     IF THIS IS NOT GUARANTEE'S PRIMARY           
         JNE   VR50                COMMERCIAL, GO READ NEXT CAST KEY            
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   READ ALL CAST KEYS/RECORDS ATTACHED          
         MVI   TLCAPCD,TLCAGCDQ    ATTACHED TO THE GUARANTEE                    
         MVC   TLCAGSSN,SVCAHKEY+TLCAHSSN-TLCAPD                                
         MVC   TLCAGGUA,FULL1                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VR30                                                             
VR20     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VR30     CLI   TLCAPCD,TLCAGCDQ                                                 
         JNE   VR50                                                             
         CLC   TLCAPKEY(TLCAGCOM-TLCAPD),IOKEYSAV                               
         JNE   VR50                                                             
         CLC   TLCAGCOM,RQCOCOM    (SKIP THE PRIMARY COMMERCIAL)                
         JE    VR20                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ      R4=A(CAST DETAILS ELEMENT)                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST   IF LAST SERVICED, GO READ NEXT               
         JNZ   VR20                ATTACHED CAST                                
         DROP  R4                                                               
                                                                                
         MVC   SVACKEY,IOKEY       SAVE ATTACHED-GUARANTEE CAST KEY             
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR ATTACHED COMMERCIAL                 
         MVI   TLCOPCD,TLCOCCDQ    KEY/RECORD                                   
         MVC   TLCOCCOM,SVCOKEY+TLCAGCOM-TLCAPD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   VR40                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL DETAILS ELEMENT)             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTAT,TACOSTRL   IF ATTACHED COMMERCIAL IS NOT                
         JO    VR40                RELEASED, DO NOT ALLOW RELEASE               
         GOTOR (#ADDERR,AADDERR),DMCB,ERCORLPR                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VR40     MVC   IOKEY,SVACKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VR20                                                             
                                                                                
VR50     MVC   IOKEY(L'SVCAHKEY),SVCAHKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    VR10                                                             
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALREL                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCORLPR DC    AL1(ECORLPRX-*),AL2(121),AL1(ERRCATY1),AL1(D#COREL)              
         DC    C'Cannot release Primary Commercial for Per Cycle Guaran+        
                 tee'                                                           
ECORLPRX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF COMMERCIAL RECORD          *         
*        ON ENTRY ... R4=A(I/O AREA)                                  *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEY,SVCOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   INITC10             SAVE ORIGINAL POINTERS                       
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
                                                                                
         USING TLCOD,R4                                                         
INITC10  CLC   RQCOAGY,TLCOAGY     IF AGENCY IS CHANGING                        
         JE    INITC20                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOAGY)                       
         OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
                                                                                
INITC20  CLC   RQCOCLI,TLCOCLI     IF CLIENT IS CHANGING                        
         JE    INITC30                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOCLI)                       
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
                                                                                
INITC30  CLC   RQCOPRD,TLCOPRD     IF PRODUCT IS CHANGING                       
         JE    INITC40                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOPRD)                       
         OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
                                                                                
INITC40  MVC   ORIGAGY,TLCOAGY     SAVE ORIGINAL AGENCY                         
         DROP  R4                                                               
                                                                                
         LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC50  BRAS  RE,NEXTEL                                                        
         JNE   INITC60                                                          
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TAMDELQ       PROCESS EXISTING INTERNET/                   
         JNE   *+12                NEW MEDIA CODES                              
         BRAS  RE,CPYTAMD                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TAVRELQ       PROCESS EXISTING VERSION INFORMATION         
         JNE   *+12                                                             
         BRAS  RE,CPYTAVR                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TACMELQ       PROCESS EXISTING COMMENT                     
         JNE   *+12                                                             
         BRAS  RE,CPYTACM                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TACPELQ       PROCESS EXISTING PUBLISHED MUSIC             
         JNE   *+12                INFORMATION                                  
         BRAS  RE,CPYTACP                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TACSELQ       PROCESS EXISTING STUDIO INFORMATION          
         JNE   *+12                                                             
         BRAS  RE,CPYTACS                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TACOELQ       PROCESS EXISTING COMMERCIAL DETAILS          
         JNE   *+12                                                             
         BRAS  RE,CPYTACO                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TALFELQ       PROCESS EXISTING LIFT DETAILS                
         JNE   *+12                                                             
         BRAS  RE,CPYTALF                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TAMCELQ       PROCESS EXISTING MUSIC CONTRACT              
         JNE   *+12                INFORMATION                                  
         BRAS  RE,CPYTAMC                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TATRELQ       PROCESS EXISTING MUSIC CONTRACT/             
         JNE   *+12                TRACK INFORMATION                            
         BRAS  RE,CPYTATR                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TAFNELQ       PROCESS EXISTING FREE FORM NAMES             
         JNE   *+12                                                             
         BRAS  RE,CPYTAFN                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TAFLELQ       PROCESS EXISTING FILTERS                     
         JNE   *+12                                                             
         BRAS  RE,CPYTAFL                                                       
         J     INITC50                                                          
                                                                                
         CLI   0(R4),TANAELQ       PROCESS EXISTING NAME                        
         JNE   INITC50                                                          
         BRAS  RE,CPYTANA                                                       
         J     INITC50                                                          
                                                                                
INITC60  BRAS  RE,CPYTLAK          PROCESS EXISTING ALIAS                       
                                                                                
         BRAS  RE,NEWTAMD          PROCESS NEW INT/NEWMED CODES                 
         BRAS  RE,NEWTACM          PROCESS NEW COMMENT                          
         BRAS  RE,NEWTACP          PROCESS NEW PUBLISHED MUSIC INFO             
         BRAS  RE,NEWTACS          PROCESS NEW STUDIO INFORMATION               
         BRAS  RE,NEWTALF          PROCESS NEW LIFT DETAILS                     
         BRAS  RE,NEWTAMC          PROCESS NEW MUSIC CONTRACT INFO              
         BRAS  RE,NEWTATR          PROCESS NEW CONTRACT/TRACK INFO              
         BRAS  RE,NEWTAFN          PROCESS NEW FREE FORM NAMES                  
         BRAS  RE,NEWTAFL          PROCESS NEW FILTERS                          
         BRAS  RE,NEWTLAK          PROCESS NEW VERSION ALIAS                    
                                                                                
         GOTOR BLDTLIST,DMCB,TRKALIST,ITTAB,OTTAB                               
         GOTOR BLDTLIST,DMCB,TRKDLIST,OTTAB,ITTAB                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOAGY  DC    AL1(ECOAGYX-*),AL2(3),AL1(ERRCATY2),AL1(D#COAGY)                 
         DC    C'Review Agency - does not match mainframe'                      
ECOAGYX  EQU   *                                                                
                                                                                
ERCOCLI  DC    AL1(ECOCLIX-*),AL2(10),AL1(ERRCATY2),AL1(D#COCLI)                
         DC    C'Review Client - does not match mainframe'                      
ECOCLIX  EQU   *                                                                
                                                                                
ERCOPRD  DC    AL1(ECOPRDX-*),AL2(13),AL1(ERRCATY2),AL1(D#COPRD)                
         DC    C'Review Product - does not match mainframe'                     
ECOPRDX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES INTERNET/NEWMEDIA ELEMENT INTO REQUEST MAP    *         
*        ON ENTRY ... R4 = A(INTERNET/NEW MEDIA ELEMENT)              *         
***********************************************************************         
                                                                                
         USING TAMDD,R4                                                         
CPYTAMD  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ERCOIN1                                                       
         CLI   RQCOMED,TACOMEDI    PROCESS FIRST INTERNET/NEW MEDIA             
         JE    *+8                 CODE                                         
         LA    R3,ERCONM1                                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN1,RQCOIN1),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD1                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               PROCESS SECOND INTERNET/NEW MEDIA            
         CLI   0(R4),TAMDELQ       CODE                                         
         JNE   XIT                                                              
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN2,RQCOIN2),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD2                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               PROCESS THIRD INTERNET/NEW MEDIA             
         CLI   0(R4),TAMDELQ       CODE                                         
         JNE   XIT                                                              
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN3,RQCOIN3),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD3                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               PROCESS FOURTH INTERNET/NEW MEDIA            
         CLI   0(R4),TAMDELQ       CODE                                         
         JNE   XIT                                                              
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN4,RQCOIN4),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD4                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               PROCESS FIFTH INTERNET/NEW MEDIA             
         CLI   0(R4),TAMDELQ       CODE                                         
         JNE   XIT                                                              
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN5,RQCOIN5),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD5                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               PROCESS SIXTH INTERNET/NEW MEDIA             
         CLI   0(R4),TAMDELQ       CODE                                         
         JNE   XIT                                                              
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOIN6,RQCOIN6),TAMDCODE,    +        
               (R3),0                                                           
         OI    CPYSTAT1,CPYTAMD6                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF INTERNET/NEW MEDIA ELEMENTS DID NOT EXIST ON RECORD       *         
*        PREVIOUSLY AND INTERNET/NEW MEDIA CODES ARE BEING ADDED NOW, *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAMD  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ERCOIN1                                                       
         CLI   RQCOMED,TACOMEDI    R3=A(INTERNET OR NEW MEDIA ERROR             
         JE    *+8                 MESSAGE)                                     
         LA    R3,ERCONM1                                                       
                                                                                
         TM    CPYSTAT1,CPYTAMD1   IF INTERNET/NEW MEDIA 1 ELEMENT DID          
         JO    NMD10               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN1,RQCOIN1     AND IS BEING ADDE NOW                        
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
                                                                                
NMD10    ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         TM    CPYSTAT1,CPYTAMD2   IF INTERNET/NEW MEDIA 2 ELEMENT DID          
         JO    NMD20               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN2,RQCOIN2     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
                                                                                
NMD20    ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         TM    CPYSTAT1,CPYTAMD3   IF INTERNET/NEW MEDIA 3 ELEMENT DID          
         JO    NMD30               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN3,RQCOIN3     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
                                                                                
NMD30    ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         TM    CPYSTAT1,CPYTAMD4   IF INTERNET/NEW MEDIA 4 ELEMENT DID          
         JO    NMD40               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN4,RQCOIN4     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
                                                                                
NMD40    ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         TM    CPYSTAT1,CPYTAMD5   IF INTERNET/NEW MEDIA 5 ELEMENT DID          
         JO    NMD50               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN5,RQCOIN5     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
                                                                                
NMD50    ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         TM    CPYSTAT1,CPYTAMD6   IF INTERNET/NEW MEDIA 6 ELEMENT DID          
         JO    XIT                 NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQCOIN6,RQCOIN6     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R3))                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAMD AND NEWTAMD                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOIN1  DC    AL1(ECOIN1X-*),AL2(42),AL1(ERRCATY2),AL1(D#COIN1)                
         DC    C'Review update to Internet Code '                               
ECOIN1X  EQU   *                                                                
                                                                                
ERCOIN2  DC    AL1(ECOIN2X-*),AL2(46),AL1(ERRCATY2),AL1(D#COIN2)                
         DC    C'Review update to Internet Code '                               
ECOIN2X  EQU   *                                                                
                                                                                
ERCOIN3  DC    AL1(ECOIN3X-*),AL2(50),AL1(ERRCATY2),AL1(D#COIN3)                
         DC    C'Review update to Internet Code '                               
ECOIN3X  EQU   *                                                                
                                                                                
ERCOIN4  DC    AL1(ECOIN4X-*),AL2(54),AL1(ERRCATY2),AL1(D#COIN4)                
         DC    C'Review update to Internet Code '                               
ECOIN4X  EQU   *                                                                
                                                                                
ERCOIN5  DC    AL1(ECOIN5X-*),AL2(58),AL1(ERRCATY2),AL1(D#COIN5)                
         DC    C'Review update to Internet Code '                               
ECOIN5X  EQU   *                                                                
                                                                                
ERCOIN6  DC    AL1(ECOIN6X-*),AL2(62),AL1(ERRCATY2),AL1(D#COIN6)                
         DC    C'Review update to Internet Code '                               
ECOIN6X  EQU   *                                                                
                                                                                
ERCONM1  DC    AL1(ECONM1X-*),AL2(43),AL1(ERRCATY2),AL1(D#COIN1)                
         DC    C'Review update to New Media Code'                               
ECONM1X  EQU   *                                                                
                                                                                
ERCONM2  DC    AL1(ECONM2X-*),AL2(47),AL1(ERRCATY2),AL1(D#COIN2)                
         DC    C'Review update to New Media Code'                               
ECONM2X  EQU   *                                                                
                                                                                
ERCONM3  DC    AL1(ECONM3X-*),AL2(51),AL1(ERRCATY2),AL1(D#COIN3)                
         DC    C'Review update to New Media Code'                               
ECONM3X  EQU   *                                                                
                                                                                
ERCONM4  DC    AL1(ECONM4X-*),AL2(55),AL1(ERRCATY2),AL1(D#COIN4)                
         DC    C'Review update to New Media Code'                               
ECONM4X  EQU   *                                                                
                                                                                
ERCONM5  DC    AL1(ECONM5X-*),AL2(59),AL1(ERRCATY2),AL1(D#COIN5)                
         DC    C'Review update to New Media Code'                               
ECONM5X  EQU   *                                                                
                                                                                
ERCONM6  DC    AL1(ECONM6X-*),AL2(63),AL1(ERRCATY2),AL1(D#COIN6)                
         DC    C'Review update to New Media Code'                               
ECONM6X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES COMMERCIAL DETAILS ELEMENT INTO REQUEST MAP   *         
*        ON ENTRY ... R4 = A(COMMERCIAL DETAILS ELEMENT)              *         
***********************************************************************         
                                                                                
         USING TACOD,R4                                                         
CPYTACO  NTR1  BASE=*,LABEL=*                                                   
         TM    TACOSTA2,TACOPCYC   IF COMMERCIAL IS PER CYCLE                   
         JZ    CCO10                                                            
         OC    RQCOFFC,RQCOFFC     AND FIRST FIXED CYCLE IS PROVIDED            
         JZ    CCO10                                                            
         CLC   TACOFCYC,RQCOFFC    FFC CANNOT BE CHANGED                        
         JE    CCO10                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOFFP                                   
         MVC   RQCOFFC,TACOFCYC                                                 
                                                                                
CCO10    GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOCID,RQCOCID),TACOCID,     +        
               ERCOCID,0                                                        
         GOTOR (RF),(R1),(L'RQCOTYP,RQCOTYP),TACOTYPE,ERCOTYP,0                 
         GOTOR (RF),(R1),(L'RQCOADS,RQCOADS),TACOADST,ERCOADS,         +        
               (X'FF',RQCOTYP),CCOADD                                           
         GOTOR (RF),(R1),(L'RQCOFFC,RQCOFFC),TACOFCYC,ERCOFFC,0                 
         GOTOR (RF),(R1),(L'RQCOFAR,RQCOFAR),TACOAIR,ERCOFAR,0                  
                                                                                
         CLI   RQCOTYP,CTYSEAS2    IF COMMERCIAL TYPE IS                        
         JNE   CCO20               NOT NEW SEASONAL                             
         OC    RQCOFAR,RQCOFAR     OR FIRST AIR DATE IS NOT                     
         JNZ   CCO30               POPULATED                                    
CCO20    XC    RQCOSAR,RQCOSAR     AUTO-CLEAR SECOND SEASON FIRST AIR           
         J     CCO40                                                            
CCO30    GOTOR (RF),(R1),(L'RQCOSAR,RQCOSAR),TACOSAIR,ERCOSAR,0                 
                                                                                
CCO40    GOTOR (RF),(R1),(L'RQCOEXP,RQCOEXP),TACOEXP,ERCOEXP,          +        
               (0,RQCOTYP),CCOEXP                                               
         GOTOR (RF),(R1),(L'RQCOMED,RQCOMED),TACOMED,ERCOMED,0                  
         GOTOR (RF),(R1),(L'RQCOSEC,RQCOSEC),TACOSEC,ERCOSEC,0                  
         GOTOR (RF),(R1),(L'RQCOATY,RQCOATY),TACOCTYP,ERCOATY,0                 
         GOTOR (RF),(R1),(L'RQCOAEX,RQCOAEX),TACOAEXP,ERCOAEX,         +        
               (X'FF',RQCOATY),CCO04A                                           
         GOTOR (RF),(R1),(L'TRANEYR,TRANEYR),TACOEDYR,ERCOEYR,0                 
         GOTOR (RF),(R1),(L'RQCOETY,RQCOETY),TACOEDT,ERCOETY,          +        
               (L'TRANEYR,TRANEYR)                                              
         GOTOR (RF),(R1),(L'RQCOADT,RQCOADT),TACOACT,ERCOADT,0                  
         GOTOR (RF),(R1),(L'RQCOIDT,RQCOIDT),TACOINAC,ERCOIDT,0                 
         GOTOR (RF),(R1),(L'RQCOPOL,RQCOPOL),TACOCGRP,ERCOPOL,0                 
         GOTOR (RF),(R1),(L'RQCOSCY,RQCOSCY),TACOSPCY,ERCOSCY,0                 
         GOTOR (RF),(R1),(L'RQCOATT,RQCOATT),TACOATT,ERCOATT,0                  
         GOTOR (RF),(R1),(L'RQCOSTY,RQCOSTY),TACOSES,ERCOSTY,0                  
         GOTOR (RF),(R1),(L'RQCOALU,RQCOALU),TACOAUSE,ERCOALU,         +        
               (X'FF',RQCOTYP),CCOMUS                                           
         GOTOR (RF),(R1),(L'RQCORMU,RQCORMU),TACORUSE,ERCORMU,         +        
               (X'FF',RQCOTYP),CCOMUS                                           
         GOTOR (RF),(R1),(L'RQCOART,RQCOART),TACOAFM,ERCOART,0                  
         GOTOR (RF),(R1),(L'RQCODUB,RQCODUB),TACODUB,ERCODUB,0                  
         GOTOR (RF),(R1),(L'RQCOCON,RQCOCON),TACOCONT,ERCOCON,0                 
                                                                                
         GOTOR (#CHKSTAT,ACHKSTAT),DMCB,('TACOSTLO',RQCOLCK),          +        
               (0,TACOSTAT),ERCOLCK                                             
         GOTOR (RF),(R1),('TACOSTRL',RQCOREL),(0,TACOSTAT),ERCOREL              
         GOTOR (RF),(R1),('TACOSCAN',RQCOCDO),(0,TACOSTAT),ERCOCDO              
         GOTOR (RF),(R1),('TACOSCRT',RQCOCRT),(0,TACOSTAT),ERCOCRT              
         GOTOR (RF),(R1),('TACOSWDT',RQCOWRK),(0,TACOSTAT),ERCOWRK              
         GOTOR (RF),(R1),('TACOSRES',RQCOSOP),(0,TACOSTAT),ERCOSOP              
         GOTOR (RF),(R1),('TACOSPRT',RQCOPRT),(0,TACOSTAT),ERCOPRT              
         GOTOR (RF),(R1),(0,RQCONCS),('TACOSNCS',TACOSTA2),ERCONCS              
         GOTOR (RF),(R1),(0,RQCOHFN),('TACOSNHF',TACOSTA3),ERCOHFN              
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   XIT                                                              
         MVC   ORIGCID,TACOCID     SAVE ORIGINAL COMMERCIAL ID                  
         MVC   ORIGTYP,TACOTYPE    AND ORIGINAL TYPE                            
                                                                                
         CLC   TACOTYPE,RQCOTYP    IF TYPE IS CHANGING                          
         JNE   CCO50                                                            
         CLC   TACOMED,RQCOMED     OR MEDIA IS CHANGING                         
         JNE   CCO50                                                            
         CLC   TACOSEC,RQCOSEC     OR LENGTH IS CHANGING                        
         JNE   CCO50                                                            
         CLC   TACOFCYC,RQCOFFC    OR FIRST FIXED CYCLE IS CHANGING             
         JNE   CCO50                                                            
         CLC   TACOAFM,RQCOART     OR AFM RATE IS CHANGING                      
         JE    CCO60                                                            
CCO50    OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
                                                                                
CCO60    CLC   TACOCID,RQCOCID     IF COMMERCIAL ID IS CHANGING                 
         JNE   CCO70                                                            
         CLC   TACOTYPE,RQCOTYP    OR TYPE IS CHANGING                          
         JNE   CCO70                                                            
         CLC   TACOFCYC,RQCOFFC    OR FIRST FIXED CYCLE IS CHANGING             
         JNE   CCO70                                                            
         CLC   TACOAIR,RQCOFAR     OR FIRST AIR DATE IS CHANGING                
         JNE   CCO70                                                            
         CLC   TACOSAIR,RQCOSAR    OR 2ND SEASON 1ST AIR DATE                   
         JE    CCO80                                                            
CCO70    OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
                                                                                
CCO80    CLC   TACOTYPE,RQCOTYP    IF TYPE IS CHANGING                          
         JE    *+8                                                              
         OI    COMSTAT,COTYPCST    SET STATUS                                   
                                                                                
         CLC   TACOMED,RQCOMED     IF MEDIA IS CHANGING                         
         JE    XIT                                                              
         OI    COMSTAT,COTYPCST    SET STATUS                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTACO                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOCID  DC    AL1(ECOCIDX-*),AL2(6),AL1(ERRCATY2),AL1(D#COCID)                 
         DC    C'Review Commercial ID - does not match mainframe'               
ECOCIDX  EQU   *                                                                
                                                                                
ERCOTYP  DC    AL1(ECOTYPX-*),AL2(16),AL1(ERRCATY2),AL1(D#COTYP)                
         DC    C'Review update to Commercial Type'                              
ECOTYPX  EQU   *                                                                
                                                                                
ERCOADS  DC    AL1(ECOADSX-*),AL2(18),AL1(ERRCATY2),AL1(D#COADS)                
         DC    C'Review update to Addendum State'                               
ECOADSX  EQU   *                                                                
                                                                                
ERCOFFP DC     AL1(ECOFFPX-*),AL2(19),AL1(ERRCATY1),AL1(D#COFFC)                
         DC    C'Cannot change First Fixed Cycle'                               
ECOFFPX  EQU   *                                                                
                                                                                
ERCOFFC  DC    AL1(ECOFFCX-*),AL2(20),AL1(ERRCATY2),AL1(D#COFFC)                
         DC    C'Review update to First Fixed Cycle Date'                       
ECOFFCX  EQU   *                                                                
                                                                                
ERCOFAR  DC    AL1(ECOFARX-*),AL2(21),AL1(ERRCATY2),AL1(D#COFAR)                
         DC    C'Review update to First Air Date'                               
ECOFARX  EQU   *                                                                
                                                                                
ERCOSAR  DC    AL1(ECOTYPX-*),AL2(22),AL1(ERRCATY2),AL1(D#COSAR)                
         DC    C'Review update to Second Season First Air Date'                 
ECOSARX  EQU   *                                                                
                                                                                
ERCOEXP  DC    AL1(ECOEXPX-*),AL2(23),AL1(ERRCATY2),AL1(D#COEXP)                
         DC    C'Review update to Expiration Date'                              
ECOEXPX  EQU   *                                                                
                                                                                
ERCOMED  DC    AL1(ECOMEDX-*),AL2(24),AL1(ERRCATY2),AL1(D#COMED)                
         DC    C'Review update to Media'                                        
ECOMEDX  EQU   *                                                                
                                                                                
ERCOSEC  DC    AL1(ECOSECX-*),AL2(25),AL1(ERRCATY2),AL1(D#COSEC)                
         DC    C'Review update to Length'                                       
ECOSECX  EQU   *                                                                
                                                                                
ERCOATY  DC    AL1(ECOATYX-*),AL2(29),AL1(ERRCATY2),AL1(D#COATY)                
         DC    C'Review update to Actra Type'                                   
ECOATYX  EQU   *                                                                
                                                                                
ERCOAEX  DC    AL1(ECOAEXX-*),AL2(30),AL1(ERRCATY2),AL1(D#COAEX)                
         DC    C'Review update to Actra Expiration Date'                        
ECOAEXX  EQU   *                                                                
                                                                                
ERCOEYR  DC    AL1(ECOEYRX-*),AL2(64),AL1(ERRCATY2),AL1(D#COEYR)                
         DC    C'Review update to Edit Type Year'                               
ECOEYRX  EQU   *                                                                
                                                                                
ERCOETY  DC    AL1(ECOETYX-*),AL2(65),AL1(ERRCATY2),AL1(D#COETY)                
         DC    C'Review update to Edit Type'                                    
ECOETYX  EQU   *                                                                
                                                                                
ERCOADT  DC    AL1(ECOADTX-*),AL2(66),AL1(ERRCATY2),AL1(D#COADT)                
         DC    C'Review update to Active Date'                                  
ECOADTX  EQU   *                                                                
                                                                                
ERCOIDT  DC    AL1(ECOIDTX-*),AL2(67),AL1(ERRCATY2),AL1(D#COIDT)                
         DC    C'Review update to Inactive Date'                                
ECOIDTX  EQU   *                                                                
                                                                                
ERCOPOL  DC    AL1(ECOPOLX-*),AL2(69),AL1(ERRCATY2),AL1(D#COPOL)                
         DC    C'Review update to Commercial Pool'                              
ECOPOLX  EQU   *                                                                
                                                                                
ERCOSCY  DC    AL1(ECOSCYX-*),AL2(70),AL1(ERRCATY2),AL1(D#COSCY)                
         DC    C'Review update to Split Cycle Status'                           
ECOSCYX  EQU   *                                                                
                                                                                
ERCOATT  DC    AL1(ECOATTX-*),AL2(72),AL1(ERRCATY2),AL1(D#COATT)                
         DC    C'Review update to Bill To Code'                                 
ECOATTX  EQU   *                                                                
                                                                                
ERCOSTY  DC    AL1(ECOSTYX-*),AL2(118),AL1(ERRCATY2),AL1(D#COSTY)               
         DC    C'Review update to Session Type'                                 
ECOSTYX  EQU   *                                                                
                                                                                
ERCOALU  DC    AL1(ECOALUX-*),AL2(114),AL1(ERRCATY2),AL1(D#COALU)               
         DC    C'Review update to Allowable Uses'                               
ECOALUX  EQU   *                                                                
                                                                                
ERCORMU  DC    AL1(ECORMUX-*),AL2(115),AL1(ERRCATY2),AL1(D#CORMU)               
         DC    C'Review update to Remaining Uses'                               
ECORMUX  EQU   *                                                                
                                                                                
ERCOART  DC    AL1(ECOARTX-*),AL2(116),AL1(ERRCATY2),AL1(D#COART)               
         DC    C'Review update to AFM Rate'                                     
ECOARTX  EQU   *                                                                
                                                                                
ERCODUB  DC    AL1(ECODUBX-*),AL2(117),AL1(ERRCATY2),AL1(D#CODUB)               
         DC    C'Review update to Dub Date'                                     
ECODUBX  EQU   *                                                                
                                                                                
ERCOCON  DC    AL1(ECOCONX-*),AL2(226),AL1(ERRCATY2),AL1(D#COCON)               
         DC    C'Review update to Contract Type'                                
ECOCONX  EQU   *                                                                
                                                                                
ERCOLCK  DC    AL1(ECOLCKX-*),AL2(120),AL1(ERRCATY2),AL1(D#COLCK)               
         DC    C'Review Locked Status - does not match mainframe'               
ECOLCKX  EQU   *                                                                
                                                                                
ERCOREL  DC    AL1(ECORELX-*),AL2(122),AL1(ERRCATY2),AL1(D#COREL)               
         DC    C'Review Released Status - does not match mainframe'             
ECORELX  EQU   *                                                                
                                                                                
ERCOCDO  DC    AL1(ECOCDOX-*),AL2(123),AL1(ERRCATY2),AL1(D#COCDO)               
         DC    C'Review update to Canadian Dollar Status'                       
ECOCDOX  EQU   *                                                                
                                                                                
ERCOCRT  DC    AL1(ECOCRTX-*),AL2(124),AL1(ERRCATY2),AL1(D#COCRT)               
         DC    C'Review update to Canadian Rates Status'                        
ECOCRTX  EQU   *                                                                
                                                                                
ERCOWRK  DC    AL1(ECOWRKX-*),AL2(125),AL1(ERRCATY2),AL1(D#COWRK)               
         DC    C'Review update to Work Date on Checks Status'                   
ECOWRKX  EQU   *                                                                
                                                                                
ERCOSOP  DC    AL1(ECOSOPX-*),AL2(126),AL1(ERRCATY2),AL1(D#COSOP)               
         DC    C'Review update to Soap Residuals Status'                        
ECOSOPX  EQU   *                                                                
                                                                                
ERCOPRT  DC    AL1(ECOPRTX-*),AL2(127),AL1(ERRCATY2),AL1(D#COPRT)               
         DC    C'Review update to Display as Print Status'                      
ECOPRTX  EQU   *                                                                
                                                                                
ERCONCS  DC    AL1(ECONCSX-*),AL2(139),AL1(ERRCATY2),AL1(D#CONCS)               
         DC    C'Review update to Charge CSF Status'                            
ECONCSX  EQU   *                                                                
                                                                                
ERCOHFN  DC    AL1(ECOHFNX-*),AL2(197),AL1(ERRCATY2),AL1(D#COHFN)               
         DC    C'Review update to Generate Holding Fee Notice Status'           
ECOHFNX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CCOEXP   DC    AL1(CTYDEM,CTYSEAS2,CTYIND,CTYSDEM,CTYMUS,CTYPROMO)              
         DC    AL1(CTYPROMO,CTYICAT1),X'FF'                                     
                                                                                
CCOADD   DC    AL1(CTYADD),X'FF'                                                
                                                                                
CCO04A   DC    AL1(CCTY04A,CCTY2404),X'FF'                                      
                                                                                
CCOMUS   DC    AL1(CTYMUS),X'FF'                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES VERSION ELEMENT INTO REQUEST MAP              *         
*        ON ENTRY ... R4 = A(VERSION ELEMENT)                         *         
***********************************************************************         
                                                                                
         USING TAVRD,R4                                                         
CPYTAVR  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVRVERS,1          IF VERSIONS EXIST ON COMMERCIAL              
         JNE   CVR10                                                            
         MVI   RQCOVER,1           ENSURE THAT VERSION 1 IS IN THE              
         MVI   0(R4),X'FF'         REQUEST MAP                                  
         J     XIT                                                              
                                                                                
CVR10    CLI   TAVRVERS,2                                                       
         JNE   XIT                                                              
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOLID,RQCOLID),TAVRCID,     +        
               ERCOLID                                                          
         GOTOR (RF),(R1),(L'RQCOLLN,RQCOLLN),TAVRSEC,ERCOLLN,          +        
               (L'RQCOLID,RQCOLID)                                              
         OI    CPYSTAT2,CPYSTALF                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE COPIES LIFT DETAILS ELEMENT INTO REQUEST MAP         *         
*        ON ENTRY ... R4 = A(COMMERCIAL LIFT DETAILS ELEMENT)         *         
***********************************************************************         
                                                                                
         USING TALFD,R4                                                         
CPYTALF  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOLID,RQCOLID),TALFLID,     +        
               ERCOLID                                                          
         GOTOR (RF),(R1),(L'RQCOLLN,RQCOLLN),TALFSEC,ERCOLLN,          +        
               (L'RQCOLID,RQCOLID)                                              
         OI    CPYSTAT2,CPYSTALF                                                
         MVI   0(R4),X'FF'                                                      
                                                                                
CLF20    CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   XIT                                                              
         CLC   TALFSEC,RQCOLLN     AND LIFT LENGTH IS CHANGING                  
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF LIFT DETAILS ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY   *         
*        AND LIFT DETAILS ARE BEING ADDED NOW, ROUTINE PROCESSES THEM *         
***********************************************************************         
                                                                                
NEWTALF  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT2,CPYSTALF   IF LIFT DETAILS DID NOT EXIST ON             
         JO    XIT                 RECORD PREVIOUSLY                            
         OC    RQCOLID,RQCOLID     AND ARE BEING ADDED NOW                      
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOLID)                       
         GOTOR (RF),(R1),ERCOLLN                                                
         OI    COMSTAT,COUNVRFY    AND SET TO UNVERIFY COMMERCIAL               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTALF AND NEWTALF                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOLID  DC    AL1(ECOLIDX-*),AL2(27),AL1(ERRCATY2),AL1(D#COLID)                
         DC    C'Review update to Lift ID'                                      
ECOLIDX  EQU   *                                                                
                                                                                
ERCOLLN  DC    AL1(ECOLLNX-*),AL2(28),AL1(ERRCATY2),AL1(D#COLLN)                
         DC    C'Review update to Lift Length'                                  
ECOLLNX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FREE FORM NAME ELEMENT INTO REQUEST MAP       *         
*        ON ENTRY ... R2 = A(ELEM)                                              
*                     R4 = A(FREE FORM NAME ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
CPYTAFN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAFNTYPE,TAFNTOWB   IF TYPE IS ORIGINAL WEB ID                   
         JNE   CFN10                                                            
         MVI   0(R4),X'FF'         SET TO DELETE                                
         J     XIT                                                              
                                                                                
CFN10    CLI   TAFNTYPE,TAFNTWEB   IF TYPE IS WEB APPLICATION ID                
         JNE   CFN20                                                            
         MVC   SVWID,TAFNNAME      SAVE INITIAL WEB APPLICATION ID              
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         J     XIT                                                              
                                                                                
CFN20    CLI   TAFNTYPE,TAFNTWRI   IF TYPE IS WEB RECORD ID                     
         JNE   CFN30                                                            
         MVI   0(R4),X'FF'         SET TO DELETE                                
         J     XIT                                                              
                                                                                
CFN30    CLI   TAFNTYPE,TAFNTPRD   IF TYPE IS PRODUCT NAME                      
         JNE   CFN40                                                            
         MVI   0(R4),X'FF'         SET TO DELETE ELEMENT                        
         OI    CPYSTAT3,CPYTAFNP   AND SET PROCESSED STATUS                     
                                                                                
         OC    RQCOPRD,RQCOPRD     IF PRODUCT CODE IS NOT PROVIDED              
         JNZ   XIT                                                              
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAFNLEN          COPY COMMENT ELEMENT INTO ELEM               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         DROP  R4                                                               
                                                                                
         USING TAFND,R2                                                         
         OC    TAFNNAME(L'RQCOPRN),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOPRN,RQCOPRN),TAFNNAME,    +        
               ERCOPRN,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
         USING TAFND,R4                                                         
CFN40    CLI   TAFNTYPE,TAFNTMUS   IF TYPE IS AFM TITLE                         
         JNE   XIT                                                              
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAFNLEN          COPY COMMENT ELEMENT INTO ELEM               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
         OI    CPYSTAT3,CPYTAFNM   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TAFND,R2                                                         
         OC    TAFNNAME(L'RQCOATI),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOATI,RQCOATI),TAFNNAME,    +        
               ERCOATI,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF FREE FORM NAME ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY *         
*        AND AFM TITLE IS BEING ADDED NOW, ROUTINE PROCESSES IT       *         
***********************************************************************         
                                                                                
NEWTAFN  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYTAFNP   IF PRODUCT NAME DID NOT EXIST                
         JO    NFN10               ON RECORD PREVIOUSLY                         
         OC    RQCOPRN,RQCOPRN     AND IS BEING ADDED NOW                       
         JZ    NFN10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOPRN)                       
                                                                                
NFN10    TM    CPYSTAT3,CPYTAFNM   IF AFM TITLE ELEMENT DID NOT EXIST           
         JO    XIT                 ON RECORD PREVIOUSLY                         
         OC    RQCOATI,RQCOATI     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOATI)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAFN AND NEWTAFN                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOPRN  DC    AL1(ECOPRNX-*),AL2(140),AL1(ERRCATY2),AL1(D#COPRN)               
         DC    C'Review update to Product Name'                                 
ECOPRNX  EQU   *                                                                
                                                                                
ERCOATI  DC    AL1(ECOATIX-*),AL2(17),AL1(ERRCATY2),AL1(D#COATI)                
         DC    C'Review update to Title for AFM'                                
ECOATIX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES COMMERCIAL FILTERS ELEMENT INTO REQUEST MAP   *         
*        ON ENTRY ... R4 = A(FILTERS ELEMENT)                         *         
***********************************************************************         
                                                                                
         USING TAFLD,R4                                                         
CPYTAFL  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOFL1,RQCOFL1),TAFLFLT1,    +        
               ERCOFL1,0                                                        
         GOTOR (RF),(R1),(L'RQCOFL2,RQCOFL2),TAFLFLT2,ERCOFL2,0                 
         GOTOR (RF),(R1),(L'RQCOFL3,RQCOFL3),TAFLFLT3,ERCOFL3,0                 
         GOTOR (RF),(R1),(L'RQCOFL4,RQCOFL4),TAFLFLT4,ERCOFL4,0                 
         OI    CPYSTAT3,CPYSTAFL                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF FILTERS ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY        *         
*        AND FILTERS ARE BEING ADDED NOW, ROUTINE PROCESSES THEM      *         
***********************************************************************         
                                                                                
NEWTAFL  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYSTAFL   IF FILTERS ELEMENT DID NOT EXIST             
         JO    XIT                 ON RECORD PREVIOUSLY ...                     
                                                                                
         CLI   RQCOFL1,0           IF FILTER 1 IS BEING ADDED                   
         JZ    NFL10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOFL1)                       
                                                                                
NFL10    OC    RQCOFL2,RQCOFL2     IF FILTER 2 IS BEING ADDED                   
         JZ    NFL20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOFL2)                       
                                                                                
NFL20    CLI   RQCOFL3,0           IF FILTER 3 IS BEING ADDED                   
         JZ    NFL30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOFL3)                       
                                                                                
NFL30    CLI   RQCOFL4,0           IF FILTER 4 IS BEING ADDED                   
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOFL4)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAFL AND NEWTAFL                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOFL1  DC    AL1(ECOFL1X-*),AL2(119),AL1(ERRCATY2),AL1(D#COFL1)               
         DC    C'Review update to Filter 1'                                     
ECOFL1X  EQU   *                                                                
                                                                                
ERCOFL2  DC    AL1(ECOFL2X-*),AL2(222),AL1(ERRCATY2),AL1(D#COFL2)               
         DC    C'Review update to Filter 2'                                     
ECOFL2X  EQU   *                                                                
                                                                                
ERCOFL3  DC    AL1(ECOFL3X-*),AL2(223),AL1(ERRCATY2),AL1(D#COFL3)               
         DC    C'Review update to Filter 3'                                     
ECOFL3X  EQU   *                                                                
                                                                                
ERCOFL4  DC    AL1(ECOFL4X-*),AL2(224),AL1(ERRCATY2),AL1(D#COFL4)               
         DC    C'Review update to Filter 4'                                     
ECOFL4X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES VERSION ALIAS INTO REQUEST MAP                *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLAKD,R3                                                         
CPYTLAK  NTR1  BASE=*,LABEL=*                                                   
         XC    TLAKKEY,TLAKKEY                                                  
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,RQCOAGY                                                  
         MVC   TLAKADID,RQCOCID                                                 
         MVC   TLAKNCLI,RQCOCLI                                                 
         MVC   TLAKNPRD,RQCOPRD                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CAK20                                                            
CAK10    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CAK20    CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   XIT                                                              
         CLC   TLAKCOM,RQCOCOM                                                  
         JNE   CAK10                                                            
         MVC   SVAKKEY,IOKEY                                                    
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOALS,RQCOALS),TLAKVER,     +        
               ERCOALS,0                                                        
         OI    CPYSTAT3,CPYSTLAK                                                
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        IF VERSION ALIAS RECORD DID NOT EXIST PREVIOUSLY AND         *         
*        VERSION ALIAS IS BEING ADDED NOW, ROUTINE PROCESSES IT       *         
***********************************************************************         
                                                                                
NEWTLAK  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYSTLAK   IF VERSION ALIAS RECORD DID NOT              
         JO    XIT                 EXIST PREVIOUSLY                             
         CLI   RQCOALS,0           AND ARE BEING ADDED                          
         JE    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCOALS)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTLAK AND NEWTLAK                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOALS  DC    AL1(ECOALSX-*),AL2(130),AL1(ERRCATY2),AL1(D#COVAL)               
         DC    C'Review update to Alias Version'                                
ECOALSX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED WEB APPLICATION ID                           *         
***********************************************************************         
                                                                                
VALWID   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'VS',RQCOWID      IF COMING FROM VITA SESSION ...              
         JE    VWID10                                                           
         CLC   =C'TS',RQCOWID                                                   
         JE    VWID10                                                           
         CLC   =C'RS',RQCOWID                                                   
         JNE   YES                                                              
                                                                                
VWID10   OC    SVWID,SVWID         ENSURE CAST WAS LAST UPDATED                 
         JNZ   VWID20              BY VITA SESSIONS                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCOVI                                  
         J     NO                                                               
                                                                                
VWID20   CLC   RQCOWID,SVWID       ENSURE COMMERCIAL WAS LAST UPDATED           
         JE    YES                 FROM THIS SESSION                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOCDVS                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALWID                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOCOVI DC    AL1(ECOCOVIX-*),AL2(136),AL1(ERRCATY3),AL1(D#COWID)              
         DC    C'Commercial editable in mainframe only'                         
ECOCOVIX EQU   *                                                                
                                                                                
ERCOCDVS DC    AL1(ECOCDVSX-*),AL2(137),AL1(ERRCATY3),AL1(D#COWID)              
         DC    C'Commercial was updated from Vita Completion'                   
ECOCDVSX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED COMMERCIAL TYPE                              *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
VALTYP   NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,R4                                                         
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   VTYP10                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
VTYP10   CLI   RQCOTYP,CTYSEAS     IF COMMERCIAL TYPE HAS BEEN                  
         JNE   VTYP30              PASSED AS OLD SEASONAL                       
         CLI   ACTION,ACTADD       ONLY ALLOW IF COMMERCIAL IS                  
         JE    VTYP20              ALREADY OLD SEASONAL                         
         CLI   TACOTYPE,CTYSEAS                                                 
         JE    YES                                                              
VTYP20   GOTOR (#ADDERR,AADDERR),DMCB,ERCOINOS                                  
         J     YES                                                              
                                                                                
VTYP30   CLI   RQCOTYP,CTYIND      IF COMMERCIAL TYPE HAS BEEN                  
         JNE   VTYP50              PASSED AS OLD INDUSTRIAL                     
         CLI   ACTION,ACTADD       ONLY ALLOW IF COMMERCIAL IS                  
         JE    VTYP40              ALREADY OLD SEASONAL                         
         CLI   TACOTYPE,CTYIND                                                  
         JE    YES                                                              
VTYP40   GOTOR (#ADDERR,AADDERR),DMCB,ERCOINOI                                  
         J     YES                                                              
                                                                                
VTYP50   CLI   ACTION,ACTCHA       IF CHANGING COMMERCIAL                       
         JNE   YES                                                              
         BAS   RE,VALTYPH          VALIDATE FOR TYPE NEW SEASONAL               
         BAS   RE,VALTYPM          VALIDATE FOR TYPE MUSIC                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        VALIDATE PASSED COMMERCIAL TYPE OF SEASONAL                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALTYPH  NTR1                                                                   
         CLI   RQCOTYP,CTYSEAS2    IF COMMERCIAL TYPE IS NEW SEASONAL           
         JNE   XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     READ ALL CAST KEYS/RECORDS                   
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     VTYPH20                                                          
VTYPH10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
VTYPH20  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            R4=A(CAST DETAILS ELEMENT)                   
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAFCYC,TACAFCYC   ENSURE FIRST FIXED CYCLE DATE                
         JNZ   VTYPH30                                                          
         OC    TACAEXP,TACAEXP     AND EXPIRATION DATE ARE NOT SET              
         JZ    VTYPH10                                                          
VTYPH30  GOTOR (#ADDERR,AADDERR),DMCB,ERCOINNS                                  
         JE    XIT                                                              
         OI    COMSTAT,COSEACST    IF BYPASSED, SET TO UPDATE CAST              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        VALIDATE PASSED COMMERCIAL TYPE OF MUSIC                     *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(COMMERCIAL DETAILS ELEMENT)                *         
*                     AIO3=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
         USING TACOD,R4                                                         
VALTYPM  NTR1                                                                   
         CLC   TACOTYPE,RQCOTYP    IF COMMERCIAL TYPE IS BEING CHANGED          
         JE    YES                                                              
         CLI   TACOTYPE,CTYMUS     AND WAS MUSIC                                
         JE    VTYPM10                                                          
         CLI   RQCOTYP,CTYMUS      OR IS BECOMING MUSIC                         
         JNE   YES                                                              
         DROP  R4                                                               
                                                                                
VTYPM10  L     R4,AIO3             ENSURE THAT AFM CONTRACT DOES NOT            
         MVI   ELCODE,TAMCELQ      HAVE ANY TRACKS                              
         BRAS  RE,GETEL                                                         
         JE    VTYPM20                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINTR                                  
         J     NO                                                               
                                                                                
VTYPM20  L     R4,AIO3             ENSURE THAT COMMERCIAL DOES NOT              
         MVI   ELCODE,TATRELQ      HAVE ANY TRACKS                              
         BRAS  RE,GETEL                                                         
         JE    VTYPM30                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINTR                                  
         J     NO                                                               
                                                                                
         USING TLCAD,R3                                                         
VTYPM30  XC    TLCAKEY,TLCAKEY     ENSURE THAT COMMERCIAL DOES NOT              
         MVI   TLCACD,TLCACDQ      HAVE ANY ATTACHED CAST                       
         MVC   TLCACOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCOINAP                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTYP                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOINOS DC    AL1(ECOINOSX-*),AL2(132),AL1(ERRCATY1),AL1(D#COTYP)              
         DC    C'Please use New Seasonal Commercial Type'                       
ECOINOSX EQU   *                                                                
                                                                                
ERCOINOI DC    AL1(ECOINOIX-*),AL2(138),AL1(ERRCATY1),AL1(D#COTYP)              
         DC    C'Please use Industrial Category 1 or 2 Type'                    
ECOINOIX EQU   *                                                                
                                                                                
ERCOINNS DC    AL1(ECOINNSX-*),AL2(15),AL1(ERRCATY2),AL1(D#COTYP)               
         DC    C'Cast settings invalid for Seasonal Commercial'                 
ECOINNSX EQU   *                                                                
                                                                                
ERCOINTR DC    AL1(ECOINTRX-*),AL2(197),AL1(ERRCATY3),AL1(D#COTYP)              
         DC    C'Cannot change - Commercial has attached Tracks'                
ECOINTRX EQU   *                                                                
                                                                                
ERCOINAP DC    AL1(ECOINAPX-*),AL2(198),AL1(ERRCATY3),AL1(D#COTYP)              
         DC    C'Cannot change - Commercial has attached performers'            
ECOINAPX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED LIFT ID                                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALLFT   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOLID,RQCOLID     ONLY VALIDATE IF LIFT ID HAS                 
         JZ    XIT                 BEEN PASSED                                  
         CLI   RQCOVER,0           AND COMMERCIAL HAS VERSIONS                  
         JE    XIT                                                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,RQCOAGY                                                 
         MVC   TLCOICID,RQCOLID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   IOKEY(TLCOICOM-TLCOPD),IOKEYSAV                                  
         JNE   XIT                                                              
         CLC   TLCOICOM,RQCOCOM                                                 
         JNE   VLFT10                                                           
         CLI   TLCOIVER,2                                                       
         JE    XIT                                                              
VLFT10   GOTOR (#ADDERR,AADDERR),DMCB,ERCOV2IU                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALLFT                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCOV2IU DC    AL1(ECOV2IUX-*),AL2(26),AL1(ERRCATY1),AL1(D#COLID)               
         DC    C'Version 2 ID already in use'                                   
ECOV2IUX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS COMMERCIAL RECORD TO FILE                       *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         CLI   RQCOSCY,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOSCY,C'N'        SET DEFAULT "SPLIT CYCLES?"                  
                                                                                
         CLI   RQCOLCK,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOLCK,C'N'        SET DEFAULT "COMMERCIAL LOCKED?"             
                                                                                
         CLI   RQCOREL,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOREL,C'N'        SET DEFAULT "COMMERCIAL RELEASED?"           
                                                                                
         CLI   RQCOCDO,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOCDO,C'N'        SET DEFAULT "CANADIAN DOLLARS?"              
                                                                                
         CLI   RQCOCRT,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOCRT,C'N'        SET DEFAULT "CANADIAN RATES?"                
                                                                                
         CLI   RQCOWRK,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOWRK,C'N'        SET DEFAULT "WORKDATE ON CHECKS?"            
                                                                                
         CLI   RQCOSOP,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOSOP,C'N'        SET DEFAULT "SOAP RESIDUALS?"                
                                                                                
         CLI   RQCOPRT,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOPRT,C'N'        SET DEFAULT "DISPLAY AS PRINT?"              
                                                                                
         CLI   RQCOHFN,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCOHFN,C'Y'        SET DEFAULT "GEN HLD FEE NOTICES?"           
                                                                                
         USING TLSYD,R3                                                         
         OC    RQCOCOM,RQCOCOM     IF INTERNAL COMMERCIAL NUMBER                
         JNZ   EXECA10             IS NOT PROVIDED                              
         XC    TLSYKEY,TLSYKEY     READ SYSTEM KEY/RECORD                       
         MVI   TLSYCD,TLSYCDQ                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZICM  R1,TASYLCOM,4                                                    
         AHI   R1,1                                                             
         STCM  R1,15,TASYLCOM      INCREMENT LAST COMMERCIAL NUMBER             
         STCM  R1,15,RQCOCOM       AND SAVE IT TO REQUEST                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         DROP  R4                                                               
                                                                                
         L     R4,AIO3                                                          
                                                                                
EXECA10  XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
         L     RF,ASVPTRS          AND POINTER BLOCK                            
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLCOD,R4                                                         
         MVI   TLCOCD,TLCOCDQ      BUILD KEY WITH RECORD CODE                   
         MVC   TLCOAGY,RQCOAGY     AGENCY                                       
         MVC   TLCOCLI,RQCOCLI     CLIENT                                       
         MVC   TLCOPRD,RQCOPRD     PRODUCT                                      
         MVC   TLCOCID,RQCOCID     COMMERCIAL ID                                
         MVC   TLCOCOM,RQCOCOM     INTERNAL COMMERCIAL NUMBER                   
         MVI   TLCOLEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TACOD,RF                                                         
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM           INITIALIZE COMMERCIAL DETAILS ELEM           
         MVI   TACOEL,TACOELQ      AND ADD IT TO COMMERCIAL RECORD              
         MVI   TACOLEN,TACOLNQ3                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  RF                                                               
                                                                                
         USING TANUD,RF                                                         
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM           ADD NEXT CAST SEQUENCE NUMBER                
         MVI   TANUEL,TANUELQ      ELEMENT                                      
         MVI   TANULEN,TANULNQ1                                                 
         MVI   TANUTYPE,TANUTSEQ                                                
         MVC   TANUNXTC,=X'0001'                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  RF                                                               
                                                                                
         BRAS  RE,BLDREC           BUILD RECORD AND ADD TO FILE                 
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS) AND UPDATE PASSIVE POINTERS                  
                                                                                
         BRAS  RE,PROVR2           PROCESS VERSION 2 RECORD                     
         OI    COMSTAT,CORECUPD                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING COMMERCIAL RECORD                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   YES                 DELETE ALL MARKED ELEMENTS                   
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         USING TAOCD,R2                                                         
         CLC   ORIGAGY,RQCOAGY     IF AGENCY                                    
         JNE   EXECC10                                                          
         CLC   ORIGCID,RQCOCID     OR COMMERCIAL ID HAS CHANGED ...             
         JE    EXECC20                                                          
EXECC10  LA    R2,ELEM                                                          
         XC    ELEM,ELEM           ADD OLD AGENCY/CID ELEMENT                   
         MVI   TAOCEL,TAOCELQ                                                   
         MVI   TAOCLEN,TAOCLNQ                                                  
         GOTO1 VDATCON,DMCB,(5,0),(1,TAOCDTE)                                   
         XC    TAOCDTE,=6X'FF'     PUT DATE                                     
         MVC   TAOCAGY,ORIGAGY     OLD AGENCY                                   
         MVC   TAOCCID,ORIGCID     AND OLD COMMERCIAL ID INTO ELEMENT           
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         USING TLCAPD,R3                                                        
EXECC20  TM    COMSTAT,COREISSU    IF A FIELD THAT TRIGGERS HOLDING             
         JZ    EXECC50             FEE REISSUE HAS BEEN CHANGED                 
         XC    TLCAPKEY,TLCAPKEY   READ ALL HOLDING FEE KEYS FOR                
         MVI   TLCAPCD,TLCAHCDQ    COMMERCIAL'S CAST                            
         MVC   TLCAHCOM,RQCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
EXECC30  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLCAHSRT-TLCAPD),IOKEYSAV                                  
         JNE   EXECC40                                                          
         OC    TLCAHNXT,TLCAHNXT   IF ALL OF THE LATEST HOLDING FEES            
         JZ    EXECC30             HAVE BEEN PAID                               
         CLC   TLCAHDTE,TLCAHNXT   NO NEED TO REISSUE                           
         JNH   EXECC50                                                          
EXECC40  NI    COMSTAT,X'FF'-COREISSU                                           
         DROP  R3                                                               
                                                                                
EXECC50  BRAS  RE,BLDREC           BUILD RECORD                                 
                                                                                
         CLC   TLCOAGY,RQCOAGY     IF KEY FIELDS HAVE NOT CHANGED               
         JNE   EXECC70             PUT RECORD TO FILE                           
         CLC   TLCOCLI,RQCOCLI     UPDATE PASSIVE POINTERS                      
         JNE   EXECC70                                                          
         CLC   TLCOPRD,RQCOPRD                                                  
         JNE   EXECC70                                                          
         CLC   TLCOCID,RQCOCID                                                  
         JNE   EXECC70                                                          
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         JNE   NO                                                               
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         BRAS  RE,PROVR2           AND PROCESS VERSION 2 RECORD                 
         CLC   RQCOTYP,ORIGTYP                                                  
         JE    EXECC210                                                         
         J     EXECC100                                                         
                                                                                
EXECC70  XC    SVCOKEY,SVCOKEY     IF KEY FIELDS HAVE CHANGED ...               
         MVC   SVCOKEY(L'TLCOKEY),TLCOKEY                                       
                                                                                
         OI    TLCOSTAT,X'81'      DELETE OLD RECORD AND POINTERS               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
                                                                                
         MVC   TLCOAGY,RQCOAGY     UPDATE RECORD KEY WITH AGENCY                
         MVC   TLCOCLI,RQCOCLI     CLIENT CODE                                  
         MVC   TLCOPRD,RQCOPRD     PRODUCT CODE                                 
         MVC   TLCOCID,RQCOCID     AND COMMERCIAL ID                            
         XC    TLCOSTAT,TLCOSTAT                                                
         DROP  R4                                                               
                                                                                
         L     RF,ASVPTRS          ADD RECORD WITH NEW KEY TO FILE              
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS) AND UPDATE PASSIVE POINTERS                  
                                                                                
         BRAS  RE,PROVR2           PROCESS VERSION 2 RECORD                     
                                                                                
         USING TLCOD,R3                                                         
         LHI   R0,TLCOV120        READ ALL INDEXED "VERSION"                    
EXECC80  MVC   TLCOKEY,SVCOKEY    COMMERCIAL KEYS/RECORDS                       
         STC   R0,TLCOVER                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   EXECC90                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
         OI    TLCOSTAT,X'81'     DELETE RECORD AND PASSIVE KEYS                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
                                                                                
         MVC   TLCOAGY,RQCOAGY     UPDATE RECORD KEY WITH AGENCY                
         MVC   TLCOCLI,RQCOCLI     CLIENT CODE                                  
         MVC   TLCOPRD,RQCOPRD     PRODUCT CODE                                 
         MVC   TLCOCID,RQCOCID     AND COMMERCIAL ID                            
         XC    TLCOSTAT,TLCOSTAT   ADD RECORD WITH NEW KEY TO FILE              
         L     RF,ASVPTRS                                                       
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS) AND ADD PASSIVE POINTERS                     
         DROP  R4                                                               
                                                                                
EXECC90  AHI   R0,1                                                             
         CHI   R0,TLCOV250                                                      
         JNH   EXECC80                                                          
                                                                                
         USING TLVRD,R3                                                         
EXECC100 XC    TLVRKEY,TLVRKEY     READ ALL ATTACHED VERSION KEY/               
         MVI   TLVRCD,TLVRCDQ      RECORDS                                      
         MVC   TLVRCOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     EXECC120                                                         
EXECC110 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
EXECC120 CLC   IOKEY(TLVRVER-TLVRD),IOKEYSAV                                    
         JNE   EXECC150                                                         
         CLI   TLVRVER,2           SKIP VERSION 2                               
         JNE   EXECC130                                                         
         TM    COMSTAT,COVR2PRO    IF IT HAS ALREADY BEEN PROCESSED             
         JO    EXECC110                                                         
EXECC130 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         MVC   SVVRKEY,IOKEY                                                    
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE ORIGINAL VERSION KEYS                   
                                                                                
         L     R4,AIO3                                                          
         BRAS  RE,UPDVAC           UPDATE VERSION'S AGY/CLI/PRD                 
                                                                                
         USING TACOD,R4                                                         
         CLC   RQCOTYP,ORIGTYP     IF TYPE HAS CHANGED                          
         JE    EXECC140            UPDATE VERSION'S TYPE                        
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TACOTYPE,RQCOTYP                                                 
         DROP  R4                                                               
                                                                                
EXECC140 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         MVC   IOKEY,SVVRKEY       AND UPDATE VERSION RECORD/KEYS               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     EXECC110                                                         
                                                                                
         USING TLCOD,R2                                                         
EXECC150 CLC   ORIGAGY,RQCOAGY     IF AGENCY                                    
         JNE   EXECC160                                                         
         LA    R2,SVCOKEY                                                       
         CLC   TLCOCLI,RQCOCLI     OR CLIENT HAVE CHANGED                       
         JE    EXECC210                                                         
         DROP  R2                                                               
                                                                                
         USING TLCAD,R3                                                         
EXECC160 XC    TLCAKEY,TLCAKEY     READ ALL CAST FOR COMMERCIAL                 
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     EXECC180                                                         
EXECC170 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
EXECC180 CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   EXECC210                                                         
         MVC   SVCAKEY,IOKEY                                                    
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JNE   EXECC170                                                         
         OC    TACAGUA,TACAGUA     IF PERFORMER IS ON GUARANTEE                 
         JZ    EXECC170                                                         
         MVC   SVCAKEY,IOKEY       SAVE CAST KEY                                
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCAKEY+TLCASSN-TLCAD                                    
         MVC   TLGUGUA,TACAGUA                                                  
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   EXECC200                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3,R4                                                            
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JNE   EXECC200                                                         
                                                                                
         OC    TAGUAGY,TAGUAGY     IF PRIMARY AGENCY/CLIENT IS NOT              
         JZ    EXECC200            DEFINED, DO NOT UPDATE AGY/CLI               
                                                                                
         CLC   TAGUAGY,RQCOAGY     IF NEW AGENCY IS ALREADY SET AS              
         JNE   EXECC190            PRIMARY                                      
         OC    TAGUCLI,TAGUCLI     WITH NO CLIENT LIMITS                        
         JZ    EXECC200                                                         
         CLC   TAGUCLI,RQCOCLI     OR NEW CLIENT IS ALREADY SET AS              
         JE    EXECC200            PRIMARY, DO NOT UPDATE AGY/CLI               
                                                                                
EXECC190 BAS   RE,UPDGAC           UPDATE GUARANTEE'S AGENCY/CLIENT             
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         DROP  R4                                                               
                                                                                
EXECC200 MVC   IOKEY,SVCAKEY       RESTORE CAST READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     EXECC170                                                         
                                                                                
EXECC210 OI    COMSTAT,CORECUPD                                                 
                                                                                
         CLC   ORIGAGY,RQCOAGY     IF AGENCY                                    
         JNE   EXECC220                                                         
         CLC   ORIGCID,RQCOCID     OR COMMERCIAL ID HAS CHANGED ...             
         JE    YES                                                              
                                                                                
         USING TLCMD,R3                                                         
EXECC220 XC    TLCMKEY,TLCMKEY     READ ALL COMMENTS FOR COMMERCIAL             
         MVI   TLCMCD,TLCMCDQ                                                   
         MVC   TLCMAGY,ORIGAGY                                                  
         MVI   TLCMTYP,TLCMTCOM                                                 
         MVC   TLCMCID,ORIGCID                                                  
         MVC   TLCMICOM,RQCOCOM                                                 
EXECC230 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCMVER-TLCMD),IOKEYSAV                                    
         JNE   YES                                                              
         MVC   SVCMKEY,IOKEY                                                    
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         USING TLCMD,R4                                                         
         L     R4,AIO3             DELETE ORIGINAL COMMENT RECORD               
         OI    TLCMSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         DROP  R4                                                               
                                                                                
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'80'      DELETE ORIGINAL COMMENT KEY                  
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR'                                
         DROP  R3                                                               
                                                                                
         USING TLCMD,R4                                                         
         MVC   TLCMKEY,SVCMKEY     ADD NEW COMMENT RECORD                       
         MVC   TLCMAGY,RQCOAGY                                                  
         MVC   TLCMCID,RQCOCID                                                  
         NI    TLCMSTAT,X'FF'-X'80'                                             
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         DROP  R4                                                               
                                                                                
         MVC   IOKEY,SVCMKEY                                                    
         J     EXECC230                                                         
                                                                                
***********************************************************************         
*        ROUTINE UPDATES GUARANTEE RECORD'S VALID AGENCY/CLIENTS      *         
*        WITH NEW AGENCY/CLIENT                                       *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
UPDGAC   NTR1                                                                   
         USING GACTABD,R3                                                       
         LA    R3,ADDGAC                                                        
         MVC   GACAGY,RQCOAGY      PREPARE TO ADD NEW AGENCY/CLIENT             
         MVC   GACCLI,RQCOCLI      TO GUARANTEE                                 
         MVI   GACLNQ(R3),X'FF'                                                 
                                                                                
         CLC   TAGUCOM,RQCOCOM     IF THIS IS GUARANTEE'S PRIMARY               
         JNE   UGAC10              COMMERCIAL                                   
         MVC   GACAGY,TAGUAGY      STRIP OLD PRIMARY AGY/CLI OF PRIMARY         
         MVC   GACCLI,TAGUCLI      STATUS                                       
         MVC   TAGUAGY,RQCOAGY     ADD NEW AGENCY/CLIENT AS PRIMARY             
         MVC   TAGUCLI,RQCOCLI                                                  
                                                                                
UGAC10   L     R3,AUPPTRS          R3=A(GUARANTEE AGENCY/CLIENT TABLE)          
         BAS   RE,CLRGTBL          CLEAR TABLE                                  
         BAS   RE,BLDGTBL          AND BUILD IT                                 
                                                                                
UGAC20   CLI   0(R3),X'FF'         SEARCH THROUGH GUARANTEE'S ORIGINAL          
         JE    UGAC50              AGENCY/CLIENT BLOCK                          
                                                                                
         CLC   GACAGY,RQCOAGY      IF NEW AGENCY IS FOUND                       
         JNE   UGAC40                                                           
         OC    GACCLI,GACCLI       WITHOUT A CLIENT RESTRICTION                 
         JNZ   UGAC30                                                           
         MVI   ADDGAC,X'FF'        NO NEED TO ADD AGENCY/CLIENT                 
         J     UGAC50                                                           
                                                                                
UGAC30   CLC   GACCLI,RQCOCLI      IF NEW AGENCY AND CLIENT ARE FOUND           
         JNE   UGAC40                                                           
         CLC   GACAGY,TAGUAGY      AND AGENCY                                   
         JNE   XIT                                                              
         CLC   GACCLI,TAGUCLI      AND CLIENT MATCH THE PRIMARY COMM'L          
         JNE   UGAC40              CLEAR THEM FROM ORIGINAL BLOCK               
         XC    GACTABD(GACLNQ),GACTABD                                          
         J     UGAC50                                                           
                                                                                
UGAC40   LA    R3,GACLNQ(R3)       BUMP TO NEXT ORIGINAL AGENCY/CLIENT          
         J     UGAC20                                                           
         DROP  R4                                                               
                                                                                
UGAC50   GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),('TAVAELQ',AIO3),0                 
                                                                                
         BAS   RE,BLDGACEL         BUILD AGENCY/CLIENT LIMITS                   
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO CLEAR GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(GUARANTEE'S AGENCY/CLIENT TABLE)           *         
***********************************************************************         
                                                                                
CLRGTBL  NTR1                                                                   
         LR    RE,R3               RE=(GUARANTEE AGENCY/CLIENT TABLE)           
         LR    RF,R3                                                            
         AHI   RF,GACTLNQ          RF=(END OF GRT AGENCY/CLIENT TABLE)          
CGTBL10  XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         CR    RE,RF                                                            
         JL    CGTBL10                                                          
         MVI   0(R3),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT TABLE AREA)                            
***********************************************************************         
                                                                                
         USING GACTABD,R3                                                       
BLDGTBL  NTR1                                                                   
         USING TAVAD,R4                                                         
         L     R4,AIO3             READ ALL SUBSIDIARY AGENCY/CLIENT            
         MVI   ELCODE,TAVAELQ      ELEMENTS                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BGTBL10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   BGTBL20                                                          
         LHI   RF,TAVALNQ                                                       
BGTBL20  D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
BGTBL30  CLI   TAVALEN,TAVALNQ                                                  
         JE    BGTBL40                                                          
                                                                                
         MVC   GACCLI-GACTABD(L'GACCLI,R3),0(R5)                                
                                                                                
BGTBL40  MVC   0(GACCLI-GACTABD,R3),TAVAAGY                                     
         LA    R3,GACLNQ(R3)                                                    
         MVI   0(R3),X'FF'         ADD CURRENT CLIENT TO TABLE                  
                                                                                
         LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,BGTBL30          BUMP TO NEXT CLIENT IN ELEMENT               
         J     BGTBL10                                                          
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT LIMIT ELEMENTS    *         
***********************************************************************         
                                                                                
BLDGACEL NTR1                                                                   
         USING GACTABD,R2                                                       
         L     R2,AUPPTRS        R2=A(INITIAL TABLE)                            
         LA    R3,ADDGAC         R3=A(NEW AGENCY/CLIENT TABLE)                  
                                                                                
         USING TAVAD,R4                                                         
BGACE10  LA    R4,ELEM           R4=A(TAVA ELEMENT TO BUILD)                    
         XC    ELEM,ELEM                                                        
         MVI   TAVAEL,TAVAELQ    INITIALIZE AGENCY/CLIENT LIMIT ELEMENT         
         MVI   TAVALEN,TAVALNQ                                                  
                                                                                
         LA    R5,TAVACLI        R5=A(WHERE TO SAVE NXT CLI IN ELEMENT)         
                                                                                
BGACE20  OC    GACTABD(GACLNQ),GACTABD                                          
         JNZ   BGACE30                                                          
         LA    R2,GACLNQ(R2)     IF AT DELETED ENTRY FROM INITIAL TABLE         
         J     BGACE20           BUMP TO NEXT INITIAL ENTRY                     
                                                                                
BGACE30  CLI   0(R2),X'FF'       IF AT THE END OF BOTH TABLES                   
         JNE   BGACE40           GO ADD THE FINAL AGY/CLI ELEMENT               
         CLI   0(R3),X'FF'                                                      
         JE    BGACE110                                                         
                                                                                
BGACE40  CLI   0(R2),X'FF'       IF AT END OF INITIAL TABLE                     
         JE    BGACE60           GO ADD FROM ADDITIONS TABLE                    
                                                                                
         CLI   0(R3),X'FF'       IF AT END OF ADDITIONS TABLE                   
         JE    BGACE50           GO ADD ENTRY FROM INITAL TABLE                 
                                                                                
         CLC   0(GACLNQ,R2),0(R3) COMPARE ENTRIES FROM INITIAL AND              
         JH    BGACE60           ADDITIONS - ADD THE ALPHA LOWER                
                                                                                
BGACE50  LR    R1,R2             SET TO ADD FROM INITIAL TABLE                  
         LA    R2,GACLNQ(R2)     AND BUMP TO NEXT INITIAL ENTRY                 
         J     BGACE70                                                          
                                                                                
BGACE60  LR    R1,R3             SET TO ADD FROM ADDITIONS TABLE                
         LA    R3,GACLNQ(R3)     AND BUMP TO NEXT ADDITIONS ENTRY               
                                                                                
BGACE70  OC    TAVAAGY,TAVAAGY   IF ELEMENT BUILDING IN PROGRESS                
         JZ    BGACE80                                                          
         CLC   TAVAAGY,GACAGY-GACTABD(R1)                                       
         JE    BGACE80           BUT CURRENT AGENCY DOES NOT MATCH              
         BAS   RE,ADDTAVA        THE ELEMENT, GO ADD THE ELEMENT                
         XC    ELEM,ELEM         AND INITIALIZE THE NEW ELEMENT                 
         MVI   TAVAEL,TAVAELQ                                                   
         MVI   TAVALEN,TAVALNQ                                                  
         LA    R5,TAVACLI                                                       
                                                                                
BGACE80  MVC   TAVAAGY,GACAGY-GACTABD(R1)                                       
                                                                                
         OC    GACCLI-GACTABD(L'GACCLI,R1),GACCLI-GACTABD(R1)                   
         JZ    BGACE20                                                          
         MVC   0(L'TAVACLI,R5),GACCLI-GACTABD(R1)                               
         ZIC   RE,TAVALEN                                                       
         AHI   RE,L'TAVACLI     IF NEW CLIENT IS BEING ADDED                    
         STC   RE,TAVALEN       ADD CLIENT AND BUMP UP ELEMENT LENGTH           
                                                                                
         CLI   TAVALEN,255      IF ELEMENT IS NOW AT MAXIMUM LENGTH             
         JL    BGACE90                                                          
         BAS   RE,ADDTAVA       ADD ELEMENT TO RECORD                           
         J     BGACE10          AND REINITIALIZE THE ELEMENT                    
                                                                                
BGACE90  LA    R5,L'GACCLI(R5)  BUMP TO SPOT IN ELEMENT FOR THE                 
         J     BGACE20          NEXT CLIENT                                     
         DROP  R2                                                               
                                                                                
BGACE110 OC    TAVAAGY,TAVAAGY  WHEN END OF BOTH TABLES IS REACHED              
         JZ    XIT                                                              
         BAS   RE,ADDTAVA       ADD THE FINAL ELEMENT                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO ADD LIMITED AGENCY/CLIENT ELEMENT TO GUARANTEE    *         
***********************************************************************         
                                                                                
ADDTAVA  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R4),0                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESS VERSION 2 RECORD                             *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(MASTER COMMERCIAL RECORD)                  *         
***********************************************************************         
                                                                                
PROVR2   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCOLID,RQCOLID     IF LIFT ID HAS BEEN PASSED                   
         JZ    PV2100                                                           
         CLI   RQCOVER,0           AND COMMERCIAL HAS VERSIONS                  
         JE    XIT                                                              
                                                                                
         L     RF,ASVPTRS          INITIALIZE SAVED POINTER BLOCK               
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLVRD,R3                                                         
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   PV210                                                            
         XC    TLVRKEY,TLVRKEY     AND VERSION ALREADY EXISTS                   
         MVI   TLVRCD,TLVRCDQ      GET IT AND SAVE POINTERS                     
         MVC   TLVRCOM,RQCOCOM                                                  
         MVI   TLVRVER,2                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   PV210                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
         J     PV220                                                            
         DROP  R3                                                               
                                                                                
         USING TLVRD,R4                                                         
PV210    XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ      BUILD VERSION KEY WITH RECORD CODE           
         MVC   TLVRCOM,RQCOCOM     INTERNAL COMMERCIAL NUMBER                   
         MVI   TLVRVER,2           AND VERSION NUMBER                           
         DROP  R4                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),('TATRELQ',(R4)),0                 
                                                                                
PV220    MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
PV230    BRAS  RE,NEXTEL                                                        
         JNE   PV260                                                            
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     PV230                                                            
                                                                                
         CLI   0(R4),TAVRELQ       DELETE VERSION ELEMENTS                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     PV230                                                            
                                                                                
         USING TACOD,R4                                                         
         CLI   0(R4),TACOELQ       PROCESS COMMERCIAL DETAILS ELEMENT           
         JNE   PV240                                                            
         MVC   TACOCID,RQCOLID                                                  
         MVC   TACOSEC,RQCOLLN                                                  
         CLC   RQCOTYP,ORIGTYP                                                  
         JE    PV230                                                            
         MVC   TACOTYPE,RQCOTYP                                                 
         J     PV230                                                            
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
PV240    CLI   0(R4),TAFNELQ      DELETE WEB APPLICATION ID ELEMENT             
         JNE   PV250                                                            
         CLI   TAFNTYPE,TAFNTWEB                                                
         JNE   PV230                                                            
         MVI   0(R4),X'FF'                                                      
         J     PV230                                                            
         DROP  R4                                                               
                                                                                
PV250    CLI   0(R4),TAOCELQ       DELETE OLD AGENCY/CID ELEMENTS               
         JNE   PV230                                                            
         MVI   0(R4),X'FF'                                                      
         J     PV230                                                            
                                                                                
PV260    L     R4,AIO3                                                          
         BRAS  RE,UPDVAC           UPDATE VERSION'S AGY/CLI/PRD                 
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQCOWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(R4),RQCOSTF                            
                                                                                
         L     RF,ASVPTRS                                                       
         CLI   0(RF),0                                                          
         JNE   PV270                                                            
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     PV280                                                            
                                                                                
PV270    GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
                                                                                
PV280    GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     PVX                                                              
                                                                                
***********************************************************************         
                                                                                
         USING TLVRD,R3                                                         
PV2100   XC    TLVRKEY,TLVRKEY     IF VERSION ID IS NOT PROVIDED                
         MVI   TLVRCD,TLVRCDQ      DELETE VERSION 2 RECORD                      
         MVC   TLVRCOM,RQCOCOM                                                  
         MVI   TLVRVER,2                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   PVX                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
         DROP  R3                                                               
                                                                                
         USING TLVRD,R4                                                         
         OI    TLVRSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         DROP  R4                                                               
                                                                                
PVX      OI    COMSTAT,COVR2PRO                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES VERSION RECORD'S AGENCY/CLIENT/PRODUCT       *         
*        ON ENTRY ... R4=A(VERSION RECORD)                            *         
***********************************************************************         
                                                                                
UPDVAC   NTR1  BASE=*,LABEL=*                                                   
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
UVAC10   BRAS  RE,NEXTEL                                                        
         JNE   UVAC40                                                           
         CLI   TAFNTYPE,TAFNTAGY   DELETE ORIGINAL AGENCY                       
         JE    UVAC30                                                           
         CLI   TAFNTYPE,TAFNTCLI   CLIENT                                       
         JE    UVAC30                                                           
         CLI   TAFNTYPE,TAFNTPRD   AND PRODUCT ELEMENTS                         
         JNE   UVAC10                                                           
UVAC30   MVI   0(R4),X'FF'                                                      
         J     UVAC10                                                           
         DROP  R4                                                               
                                                                                
UVAC40   L     R4,AIO3                                                          
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         USING TAFND,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'RQCOAGY                                        
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'RQCOAGY),RQCOAGY                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
         XC    ELEM,ELEM           ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'RQCOCLI                                        
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'RQCOCLI),RQCOCLI                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
         OC    RQCOPRD,RQCOPRD                                                  
         JZ    XIT                                                              
         XC    ELEM,ELEM           ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'RQCOPRD                                        
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'RQCOPRD),RQCOPRD                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS OR PUTS COMMERCIAL'S CAST RECORD                *         
*        ON ENTRY ... AIO5 = A(CAST RECORD)                           *         
***********************************************************************         
                                                                                
UPDCST   NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO5             IF RECORD IS NOT SET FOR DELETE              
         TM    TLCASTAT,TLCASDCG                                                
         JO    UCST40                                                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         TM    COMSTAT,COSEACST    IF CHANGE TO SEASONAL COMMERCIAL             
         JZ    UCST20              NECESSITATES CAST UPDATES                    
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            R4=A(CAST DETAILS ELEMENT)                   
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAFCYC,TACAFCYC   AND FIRST FIXED CYCLE DATE                   
         JNZ   UCST10                                                           
         OC    TACAEXP,TACAEXP     OR EXPIRATION DATE IS SET                    
         JZ    UCST20                                                           
UCST10   XC    TACAFCYC,TACAFCYC   CLEAR FIRST FIXED CYCLE                      
         XC    TACAEXP,TACAEXP     AND EXPIRATION DATE                          
         OI    CASTSTAT,CAUPDATE                                                
         DROP  R4                                                               
                                                                                
UCST20   TM    COMSTAT,COTYPCST+COMEDCST                                        
         JZ    UCST30              IF TYPE OR MEDIA HAS CHANGED ...             
                                                                                
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAOAELQ',AIO5),RQCOTYP,RQCOMED          
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAOPELQ',AIO5),RQCOTYP,RQCOMED          
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAO2ELQ',AIO5),RQCOTYP,RQCOMED          
                                                                                
         USING TLRCD,R4                                                         
         L     R4,AIO5                                                          
         ZICM  R2,TLRCLEN,2                                                     
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         ZICM  RE,TLRCLEN,2                                                     
         CR    R2,RE                     IF ANY ADJUSTMENTS WERE                
         JE    UCST30                    NECESSARY, SET "UPDATE NEEDED"         
         OI    CASTSTAT,CAUPDATE                                                
         DROP  R4                                                               
                                                                                
UCST30   TM    CASTSTAT,CAUPDATE         IF "UPDATE NEEDED"                     
         JZ    XIT                                                              
         L     RE,ASVPTRS                                                       
         CLI   0(RE),0                                                          
         JNE   UCST40                                                           
         GOTOR (#ADDREC,AADDREC),'IO5'   ADD OR UPDATE THE CAST                 
         OI    CASTSTAT,CAADDED          RECORD AND ITS PASSIVES                
         J     UCST50                                                           
UCST40   GOTOR (#PUTREC,APUTREC),'IO5'                                          
         JNE   XIT                                                              
UCST50   GOTOR (#UPDPTRS,AUPDPTRS)                                              
         BRAS  RE,ADDUCAST               ADD CAST SEQUENCE NUMBER TO            
         J     XIT                       UPDATED CAST LIST                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESSES ALIAS RECORD                               *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
PROALS   NTR1  BASE=*,LABEL=*                                                   
         CLC   RQCOALS,SVAKKEY+TLAKVER-TLAKD                                    
         JE    XIT                 IF ALIAS VERSION IS CHANGING                 
                                                                                
         USING TLAKD,R4                                                         
         CLI   SVAKKEY+TLAKVER-TLAKD,0                                          
         JE    PALS10                                                           
         MVC   IOKEY(L'SVAKKEY),SVAKKEY                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         OI    TLAKSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         DROP  R4                                                               
                                                                                
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR'                                
         DROP  R3                                                               
                                                                                
         CLI   RQCOALS,0                                                        
         JE    XIT                                                              
                                                                                
         USING TLAKD,R4                                                         
PALS10   XC    0(255,R4),0(R4)     INITIALIZE NEW ALIAS RECORD                  
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,RQCOAGY                                                  
         MVC   TLAKADID,RQCOCID                                                 
         MVC   TLAKNCLI,RQCOCLI                                                 
         MVC   TLAKNPRD,RQCOPRD                                                 
         MVC   TLAKMED,RQCOMED                                                  
         MVC   TLAKVER,RQCOALS                                                  
         MVC   TLAKCOM,RQCOCOM                                                  
         MVI   TLAKLEN+1,41                                                     
         DROP  R4                                                               
                                                                                
         USING TACMD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ+L'RQCOCID                                        
         MVI   TACMTYPE,TACMTYPX                                                
         MVC   TACMCOMM(L'RQCOCID),RQCOCID                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(R4),RQCOSTF                            
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD COMMERCIAL RECORD                                      *         
*        ON ENTRY ... R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,UPDTACO          UPDATE COMMERCIAL DETAILS ELEMENT            
         BAS   RE,ADDTAMD          ADD INTERNET/NEW MEDIA ELEMENTS              
         BRAS  RE,ADDTACM          ADD COMMENT ELEMENT                          
         BRAS  RE,ADDTACP          ADD COMMERCIAL MUSIC ELEMENT                 
         BRAS  RE,ADDTACS          ADD COMMERCIAL STUDIO ELEMENT                
         BAS   RE,ADDTALF          ADD LIFT DETAILS ELEMENT                     
         BAS   RE,ADDTAMC          ADD MUSIC CONTRACT ELEMENTS                  
         BRAS  RE,ADDTATR          ADD MUSIC CONTRACT/TRACK ELEMENTS            
         BAS   RE,ADDTAFN          ADD FREE FORM NAME ELEMENT                   
         BAS   RE,ADDTAFL          ADD FILTERS ELEMENT                          
         BRAS  RE,ADDTANA          ADD NAME ELEMENT                             
         BAS   RE,ADDTAVR          ADD VERSION ELEMENT                          
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQCOWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQCOSTF,SVTIME             
                                                                                
         GOTOR SVNXTSEQ,DMCB,(R4)                                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        UPDATE COMMERCIAL DETAILS ELEMENT                            *         
*        ON ENTRY ... R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
UPDTACO  NTR1                                                                   
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            R4=A(COMMERCIAL DETAILS ELEMENT)             
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   TACOCONT,RQCOCON    PUT CONTRACT TYPE                            
         MVC   TACOCID,RQCOCID     COMMERCIAL ID                                
         MVC   TACOTYPE,RQCOTYP    TYPE                                         
         MVC   TACOADST,RQCOADS    ADDENDUM STATE                               
         MVC   TACOFCYC,RQCOFFC    FIRST FIXED CYCLE DATE                       
         MVC   TACOAIR,RQCOFAR     FIRST AIR DATE                               
         MVC   TACOSAIR,RQCOSAR    SECOND SEASON FIRST AIR DATE                 
         MVC   TACOEXP,RQCOEXP     EXPIRATION DATE                              
         MVC   TACOMED,RQCOMED     MEDIA                                        
         MVC   TACOSEC,RQCOSEC     LENGTH                                       
         MVC   TACOCTYP,RQCOATY    ACTRA TYPE                                   
         MVC   TACOAEXP,RQCOAEX    ACTRA EXPIRATION DATE                        
         MVC   TACOEDYR,TRANEYR    EDIT TYPE YEAR                               
         MVC   TACOEDT,RQCOETY     EDIT TYPE                                    
         MVC   TACOACT,RQCOADT     ACTIVE DATE                                  
         MVC   TACOINAC,RQCOIDT    INACTIVE DATE                                
         MVC   TACOCGRP,RQCOPOL    COMMERCIAL POOL                              
         MVC   TACOSPCY,RQCOSCY    SPLIT CYCLE INDICATOR                        
         MVC   TACOCLG,SVCICLG     CLIENT GROUP                                 
         MVC   TACOATT,RQCOATT     ATTENTION CODE                               
         MVC   TACOSES,RQCOSTY     SESSION TYPE                                 
         MVC   TACOAUSE,RQCOALU    ALLOWABLE USES                               
         MVC   TACORUSE,RQCORMU    REMAINING USES                               
         MVC   TACOAFM,RQCOART     AFM RATE                                     
         MVC   TACODUB,RQCODUB     DUB DATE                                     
                                                                                
         MVC   SVSTAT,TACOSTAT     SAVE ORIGINAL STATUS                         
                                                                                
         NI    TACOSTAT,X'FF'-TACOSTLO                                          
         CLI   RQCOLCK,C'Y'                                                     
         JNE   *+8                 PUT "LOCKED?"                                
         OI    TACOSTAT,TACOSTLO   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSTRL                                          
         CLI   RQCOREL,C'Y'                                                     
         JNE   *+8                 PUT "RELEASED?"                              
         OI    TACOSTAT,TACOSTRL   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSCAN                                          
         CLI   RQCOCDO,C'Y'                                                     
         JNE   *+8                 PUT "CANADIAN DOLLARS?"                      
         OI    TACOSTAT,TACOSCAN   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSCRT                                          
         CLI   RQCOCRT,C'Y'                                                     
         JNE   *+8                 PUT "CANADIAN RATES?"                        
         OI    TACOSTAT,TACOSCRT   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSWDT                                          
         CLI   RQCOWRK,C'Y'                                                     
         JNE   *+8                 PUT "WORK DATE ON CHECKS?"                   
         OI    TACOSTAT,TACOSWDT   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSRES                                          
         CLI   RQCOSOP,C'Y'                                                     
         JNE   *+8                 PUT "SOAP RESIDUALS?"                        
         OI    TACOSTAT,TACOSRES   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTAT,X'FF'-TACOSPRT                                          
         CLI   RQCOPRT,C'Y'                                                     
         JNE   *+8                 PUT "DISPLAY AS PRINT?"                      
         OI    TACOSTAT,TACOSPRT   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTA2,X'FF'-TACOSNCS                                          
         CLI   RQCONCS,C'N'                                                     
         JNE   *+8                 PUT "CHARGE CSF?"                            
         OI    TACOSTA2,TACOSNCS   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACOSTA3,X'FF'-TACOSNHF                                          
         CLI   RQCOHFN,C'N'                                                     
         JNE   *+8                 PUT "GENERATE HOLDING FEE NOTICES?"          
         OI    TACOSTA3,TACOSNHF   INDICATOR INTO ELEMENT                       
                                                                                
         OC    TACOVDTE,TACOVDTE   IF COMMERCIAL IS VERIFIED                    
         JZ    UCO40                                                            
         TM    TACOSTAT,TACOSTRL   AND NOT RELEASED                             
         JO    UCO40                                                            
         TM    COMSTAT,COUNVRFY    BUT A FIELD CHANGED THAT SHOULD              
         JO    UCO30               UNVERIFY THE COMMERCIAL                      
         CLC   TACOSTAT,SVSTAT     OR STATUS CHANGED                            
         JE    UCO40                                                            
UCO30    XC    TACOVDTE,TACOVDTE   UNVERIFY THE COMMERCIAL                      
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         OI    TACOUVST,TACOUVCO                                                
                                                                                
UCO40    TM    COMSTAT,COREISSU    IF CHANGE NECESSITATES HF REISSUE            
         JZ    XIT                                                              
         OI    TACOSTA2,TACOCHHF   SET REISSUE STATUS                           
         GOTOR SNDMQHFR,DMCB,(SYSTEM,RQCOCOM),(SVSYSTA2,VHEXOUT),VMQIO          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ADD INTERNET/NEW MEDIA ELEMENTS                              *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
ADDTAMD  NTR1                                                                   
         LA    R6,RQCOIN1          R4=A(FIRST INTERNET/NEWMEDIA FIELD)          
         LHI   R0,6                R0=INTERNET/NEWMEDIA FIELD COUNTER           
                                                                                
         USING TAMDD,R2                                                         
AMD10    OC    0(L'RQCOIN1,R6),0(R6)                                            
         JZ    AMD20               IF INTERNET/NEWMEDIA CODE IS PRESENT         
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMDEL,TAMDELQ                                                   
         MVI   TAMDLEN,TAMDLNQ                                                  
         MVC   TAMDTYPE,MEDIA      PUT TYPE                                     
         MVC   TAMDCODE,0(R6)      AND CODE INTO ELEMENT                        
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
AMD20    LA    R6,L'RQCOIN1(R6)                                                 
         BCT   R0,AMD10                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ADD LIFT DETAILS ELEMENT                                     *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TALFD,R2                                                         
ADDTALF  NTR1                                                                   
         CLI   RQCOVER,1           IF COMMERCIAL DOES NOT HAVE VERSIONS         
         JE    XIT                                                              
         OC    RQCOLID,RQCOLID     AND LIFT INFO IS PRESENT                     
         JZ    XIT                                                              
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TALFEL,TALFELQ                                                   
         MVI   TALFLEN,TALFLNQ                                                  
         MVC   TALFLID,RQCOLID     PUT LIFT ID                                  
         MVC   TALFSEC,RQCOLLN     AND LIFT LENGTH INTO ELEMENT                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD MUSIC CONTRACT ELEMENTS                                  *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TAMCD,R2                                                         
ADDTAMC  NTR1                                                                   
         CLI   RQCOTYP,CTYMUS      IF COMMERCIAL TYPE IS MUSIC                  
         JNE   XIT                                                              
                                                                                
         CLI   RQCOA1T,0                                                        
         JE    AMC10               IF TRACK 1 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,1           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA1T     TRACK                                        
         MVC   TAMCLFT,RQCOA1L     LIFT                                         
         MVC   TAMCLLEN,RQCOA1S    LENGTH                                       
         MVC   TAMCTYP,RQCOA1Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA1I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC10    CLI   RQCOA2T,0                                                        
         JE    AMC20               IF TRACK 2 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,2           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA2T     TRACK                                        
         MVC   TAMCLFT,RQCOA2L     LIFT                                         
         MVC   TAMCLLEN,RQCOA2S    LENGTH                                       
         MVC   TAMCTYP,RQCOA2Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA2I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC20    CLI   RQCOA3T,0                                                        
         JE    AMC30               IF TRACK 3 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,3           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA3T     TRACK                                        
         MVC   TAMCLFT,RQCOA3L     LIFT                                         
         MVC   TAMCLLEN,RQCOA3S    LENGTH                                       
         MVC   TAMCTYP,RQCOA3Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA3I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC30    CLI   RQCOA4T,0                                                        
         JE    AMC40               IF TRACK 4 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,4           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA4T     TRACK                                        
         MVC   TAMCLFT,RQCOA4L     LIFT                                         
         MVC   TAMCLLEN,RQCOA4S    LENGTH                                       
         MVC   TAMCTYP,RQCOA4Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA4I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC40    CLI   RQCOA5T,0                                                        
         JE    AMC50               IF TRACK 5 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,5           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA5T     TRACK                                        
         MVC   TAMCLFT,RQCOA5L     LIFT                                         
         MVC   TAMCLLEN,RQCOA5S    LENGTH                                       
         MVC   TAMCTYP,RQCOA5Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA5I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC50    CLI   RQCOA6T,0                                                        
         JE    AMC60               IF TRACK 6 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,6           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA6T     TRACK                                        
         MVC   TAMCLFT,RQCOA6L     LIFT                                         
         MVC   TAMCLLEN,RQCOA6S    LENGTH                                       
         MVC   TAMCTYP,RQCOA6Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA6I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AMC60    CLI   RQCOA7T,0                                                        
         JE    XIT                 IF TRACK 7 IS PRESENT                        
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ+L'TAMCTRKT                                       
         MVI   TAMCSEQ,7           PUT SEQUENCE NUMBER                          
         MVC   TAMCTRK,RQCOA7T     TRACK                                        
         MVC   TAMCLFT,RQCOA7L     LIFT                                         
         MVC   TAMCLLEN,RQCOA7S    LENGTH                                       
         MVC   TAMCTYP,RQCOA7Y     TYPE                                         
         MVC   TAMCTRKT,RQCOA7I    AND TITLE INTO ELEMENT                       
         OC    TAMCTRKT,SPACES                                                  
         OI    TAMCSTAT,TAMCSNEW                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD FREE FORM NAME ELEMENT                                   *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TAFND,R2                                                         
ADDTAFN  NTR1                                                                   
         OC    RQCOPRN,RQCOPRN     IF PRODUCT NAME IS PRESENT                   
         JZ    AFN10                                                            
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ      PUT TYPE                                     
         MVI   TAFNTYPE,TAFNTPRD   AND PRODUCT NAME INTO ELEMENT                
         MVC   TAFNNAME(L'RQCOPRN),RQCOPRN                                      
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AFN10    OC    RQCOATI,RQCOATI     IF TITLE FOR AFM IS PRESENT                  
         JZ    AFN20                                                            
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTMUS   PUT TYPE                                     
         MVC   TAFNNAME,RQCOATI    AND AFM TITLE INTO ELEMENT                   
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AFN20    OC    RQCOWRI,RQCOWRI     IF WEB APPLICATION RECORD ID                 
         JZ    XIT                 IS PRESENT                                   
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ      PUT TYPE                                     
         MVI   TAFNTYPE,TAFNTWRI   AND WEB APPLICATION RECORD ID                
         MVC   TAFNNAME(L'RQCOWRI),RQCOWRI                                      
         GOTOR (#SETELEN,ASETELEN) INTO ELEMENT AND ADD IT                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD FILTERS ELEMENT                                          *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TAFLD,R2                                                         
ADDTAFL  NTR1                                                                   
         OC    RQCOFLTS(RQCOFLNQ),RQCOFLTS                                      
         JZ    XIT                                                              
         XC    ELEM,ELEM           IF FILTERS ARE PRESENT                       
         MVI   TAFLEL,TAFLELQ      INITIALIZE FILTERS ELEMENT                   
         MVI   TAFLLEN,TAFLLNQ     PUT FILTERS INTO ELEMENT                     
         MVC   TAFLFLT1(RQCOFLNQ),RQCOFLTS                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD VERSION ELEMENT                                          *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R3=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TAVRD,R2                                                         
ADDTAVR  NTR1                                                                   
         CLI   RQCOVER,1           IF VERSION NUMBER IS PRESENT                 
         JNE   XIT                                                              
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVI   TAVRVERS,1          PUT VERSION NUMBER                           
         MVC   TAVRCID,RQCOCID     VERSION ID                                   
         MVC   TAVRSEC,RQCOSEC     AND VERSION LENGTH INTO ELEMENT              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
         OC    RQCOLID,RQCOLID     IF LIFT ID IS PRESENT                        
         JZ    XIT                                                              
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVI   TAVRVERS,2          PUT VERSION NUMBER                           
         MVC   TAVRCID,RQCOLID     VERSION ID                                   
         MVC   TAVRSEC,RQCOLLN     AND VERSION LENGTH INTO ELEMENT              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS WEB TRANSACTION RECORD                          *         
*        ON ENTRY ... R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
         USING TLWTD,R4                                                         
ADDWTR   NTR1  BASE=*,LABEL=*                                                   
         TM    COMSTAT,CORECUPD    IF ADDING WEB TRASACTION RECORD              
         JZ    XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE WEB TRANSACTION RECORD            
         MVI   TLWTCD,TLWTCDQ                                                   
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   SET WEB APPLICATION                          
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTACCO   INDICATE IF CHANGING                         
         CLI   ACTION,ACTCHA                                                    
         JE    AWTR20                                                           
         MVI   TLWTACTN,TLWTAACO   OR ADDING COMMERCIAL                         
                                                                                
AWTR20   MVC   TLWTWBID,RQCOWID    SET WEB APPLICATION ID                       
         MVC   TLWTUNIQ,SPACES     AND UNIQUE IDENTIFIER                        
         GOTO1 VHEXOUT,DMCB,RQCOCOM,TLWTUCOM,L'RQCOCOM,0                        
                                                                                
         MVI   TLWTLEN+1,41        SET RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           BUILD WEB TRANSACTION ELEMENT                
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWT5LNQ                                                 
         MVC   TAWTSTAF,RQCOSTF                                                 
         MVC   TAWTCOAY,RQCOAGY                                                 
         MVC   TAWTCOID,RQCOCID                                                 
         MVC   TAWTCOTI,RQCOTIT                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PERFORMS SPECIAL HANDLING FOR FILM DATE CHANGE       *         
***********************************************************************         
                                                                                
FDTCHG   NTR1  BASE=*,LABEL=*                                                   
         OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TALNKCOM                                                       
         EJECT                                                                  
       ++INCLUDE TALNKMUS                                                       
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - COMMERCIAL MAINTENANCE UPLOAD                         *         
***********************************************************************         
                                                                                
COHDR    LKMAP H,I#COULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#COMOD,UBIN,TA#PMODE,OLEN=L'RQCOMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCOMOD)                                       
F$STF    LKMAP F,D#COSTF,CHAR,TA#STAFF,MAXLEN=L'RQCOSTF,               +        
               OUTPUT=(D,B#SAVED,RQCOSTF)                                       
F$COM    LKMAP F,D#COCOM,HEXD,TA#COMCD,OLEN=L'RQCOCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOCOM)                                       
F$AGY    LKMAP F,D#COAGY,CHAR,TA#AGYCD,MAXLEN=L'RQCOAGY,               +        
               OUTPUT=(D,B#SAVED,RQCOAGY)                                       
F$CID    LKMAP F,D#COCID,CHAR,TA#CIDCD,MAXLEN=L'RQCOCID,               +        
               OUTPUT=(D,B#SAVED,RQCOCID)                                       
F$CLI    LKMAP F,D#COCLI,CHAR,TA#CLICD,MAXLEN=L'RQCOCLI,               +        
               OUTPUT=(D,B#SAVED,RQCOCLI)                                       
F$PRD    LKMAP F,D#COPRD,CHAR,TA#PRDCD,MAXLEN=L'RQCOPRD,               +        
               OUTPUT=(D,B#SAVED,RQCOPRD)                                       
F$TIT    LKMAP F,D#COTIT,CHAR,TA#COTIT,MAXLEN=L'RQCOTIT,               +        
               OUTPUT=(D,B#SAVED,RQCOTIT)                                       
F$TYP    LKMAP F,D#COTYP,CHAR,TA#COTYP,MAXLEN=L'RQCOTYP,               +        
               OUTPUT=(D,B#SAVED,RQCOTYP)                                       
F$ATI    LKMAP F,D#COATI,CHAR,TA#COATI,MAXLEN=L'RQCOATI,               +        
               OUTPUT=(D,B#SAVED,RQCOATI)                                       
F$ADS    LKMAP F,D#COADS,CHAR,TA#COADS,MAXLEN=L'RQCOADS,               +        
               OUTPUT=(D,B#SAVED,RQCOADS)                                       
F$FFC    LKMAP F,D#COFFC,PDAT,TA#FFC,OUTPUT=(D,B#SAVED,RQCOFFC)                 
F$FAR    LKMAP F,D#COFAR,PDAT,TA#FAD,OUTPUT=(D,B#SAVED,RQCOFAR)                 
F$SAR    LKMAP F,D#COSAR,PDAT,TA#SAD,OUTPUT=(D,B#SAVED,RQCOSAR)                 
F$EXP    LKMAP F,D#COEXP,PDAT,TA#EXP,OUTPUT=(D,B#SAVED,RQCOEXP)                 
F$MED    LKMAP F,D#COMED,CHAR,TA#MEDCD,MAXLEN=L'RQCOMED,               +        
               OUTPUT=(D,B#SAVED,RQCOMED)                                       
F$SEC    LKMAP F,D#COSEC,UBIN,TA#COSEC,OLEN=L'RQCOSEC,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOSEC)                                       
F$LID    LKMAP F,D#COLID,CHAR,TA#LFTID,MAXLEN=L'RQCOLID,               +        
               OUTPUT=(D,B#SAVED,RQCOLID)                                       
F$LLN    LKMAP F,D#COLLN,UBIN,TA#LFTLN,OLEN=L'RQCOLLN,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOLLN)                                       
F$ATY    LKMAP F,D#COATY,UBIN,TA#ACTYP,OLEN=L'RQCOATY,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOATY)                                       
F$AEX    LKMAP F,D#COAEX,PDAT,TA#ACTEX,OUTPUT=(D,B#SAVED,RQCOAEX)               
F$FDT    LKMAP F,D#COFDT,PDAT,TA#FLMDT,OUTPUT=(D,B#SAVED,RQCOFDT)               
F$FSU    LKMAP F,D#COFSU,CHAR,TA#FLMST,MAXLEN=L'RQCOFSU,               +        
               OUTPUT=(D,B#SAVED,RQCOFSU)                                       
F$FCY    LKMAP F,D#COFCY,CHAR,TA#FLMCY,MAXLEN=L'RQCOFCY,               +        
               OUTPUT=(D,B#SAVED,RQCOFCY)                                       
F$FST    LKMAP F,D#COFST,CHAR,TA#FLSTA,MAXLEN=L'RQCOFST,               +        
               OUTPUT=(D,B#SAVED,RQCOFST)                                       
F$RDT    LKMAP F,D#CORDT,PDAT,TA#RECDT,OUTPUT=(D,B#SAVED,RQCORDT)               
F$RSU    LKMAP F,D#CORSU,CHAR,TA#RECST,MAXLEN=L'RQCORSU,               +        
               OUTPUT=(D,B#SAVED,RQCORSU)                                       
F$RCY    LKMAP F,D#CORCY,CHAR,TA#RECCY,MAXLEN=L'RQCORCY,               +        
               OUTPUT=(D,B#SAVED,RQCORCY)                                       
F$RST    LKMAP F,D#CORST,CHAR,TA#RESTA,MAXLEN=L'RQCORST,               +        
               OUTPUT=(D,B#SAVED,RQCORST)                                       
F$MDT    LKMAP F,D#COMDT,PDAT,TA#MUSDT,OUTPUT=(D,B#SAVED,RQCOMDT)               
F$MSU    LKMAP F,D#COMSU,CHAR,TA#MUSST,MAXLEN=L'RQCOMSU,               +        
               OUTPUT=(D,B#SAVED,RQCOMSU)                                       
F$MCY    LKMAP F,D#COMCY,CHAR,TA#MUSCY,MAXLEN=L'RQCOMCY,               +        
               OUTPUT=(D,B#SAVED,RQCOMCY)                                       
F$MST    LKMAP F,D#COMST,CHAR,TA#MUSTA,MAXLEN=L'RQCOMST,               +        
               OUTPUT=(D,B#SAVED,RQCOMST)                                       
F$IN1    LKMAP F,D#COIN1,CHAR,TA#INNM1,MAXLEN=L'RQCOIN1,               +        
               OUTPUT=(D,B#SAVED,RQCOIN1)                                       
F$IN2    LKMAP F,D#COIN2,CHAR,TA#INNM2,MAXLEN=L'RQCOIN2,               +        
               OUTPUT=(D,B#SAVED,RQCOIN2)                                       
F$IN3    LKMAP F,D#COIN3,CHAR,TA#INNM3,MAXLEN=L'RQCOIN3,               +        
               OUTPUT=(D,B#SAVED,RQCOIN3)                                       
F$IN4    LKMAP F,D#COIN4,CHAR,TA#INNM4,MAXLEN=L'RQCOIN4,               +        
               OUTPUT=(D,B#SAVED,RQCOIN4)                                       
F$IN5    LKMAP F,D#COIN5,CHAR,TA#INNM5,MAXLEN=L'RQCOIN5,               +        
               OUTPUT=(D,B#SAVED,RQCOIN5)                                       
F$IN6    LKMAP F,D#COIN6,CHAR,TA#INNM6,MAXLEN=L'RQCOIN6,               +        
               OUTPUT=(D,B#SAVED,RQCOIN6)                                       
F$EYR    LKMAP F,D#COEYR,CHAR,TA#EDTYR,MAXLEN=L'RQCOEYR,               +        
               OUTPUT=(D,B#SAVED,RQCOEYR)                                       
F$ETY    LKMAP F,D#COETY,CHAR,TA#EDTYP,MAXLEN=L'RQCOETY,               +        
               OUTPUT=(D,B#SAVED,RQCOETY)                                       
F$ADT    LKMAP F,D#COADT,PDAT,TA#ACTDT,OUTPUT=(D,B#SAVED,RQCOADT)               
F$IDT    LKMAP F,D#COIDT,PDAT,TA#INADT,OUTPUT=(D,B#SAVED,RQCOIDT)               
F$POL    LKMAP F,D#COPOL,CHAR,TA#COPOL,MAXLEN=L'RQCOPOL,               +        
               OUTPUT=(D,B#SAVED,RQCOPOL)                                       
F$SCY    LKMAP F,D#COSCY,CHAR,TA#COSCY,MAXLEN=L'RQCOSCY,               +        
               OUTPUT=(D,B#SAVED,RQCOSCY)                                       
F$ATT    LKMAP F,D#COATT,CHAR,TA#ATTBT,MAXLEN=L'RQCOATT,               +        
               OUTPUT=(D,B#SAVED,RQCOATT)                                       
F$CMT    LKMAP F,D#COCMT,CHAR,TA#COMNT,MAXLEN=L'RQCOCMT,               +        
               OUTPUT=(D,B#SAVED,RQCOCMT)                                       
F$A1T    LKMAP F,D#COA1T,CHAR,TA#AFM1T,MAXLEN=L'RQCOA1T,               +        
               OUTPUT=(D,B#SAVED,RQCOA1T)                                       
F$A1L    LKMAP F,D#COA1L,CHAR,TA#AFM1L,MAXLEN=L'RQCOA1L,               +        
               OUTPUT=(D,B#SAVED,RQCOA1L)                                       
F$A1S    LKMAP F,D#COA1S,UBIN,TA#AFM1S,OLEN=L'RQCOA1S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA1S)                                       
F$A1I    LKMAP F,D#COA1I,CHAR,TA#AFM1I,MAXLEN=L'RQCOA1I,               +        
               OUTPUT=(D,B#SAVED,RQCOA1I)                                       
F$A1Y    LKMAP F,D#COA1Y,CHAR,TA#AFM1Y,MAXLEN=L'RQCOA1Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA1Y)                                       
F$A2T    LKMAP F,D#COA2T,CHAR,TA#AFM2T,MAXLEN=L'RQCOA2T,               +        
               OUTPUT=(D,B#SAVED,RQCOA2T)                                       
F$A2L    LKMAP F,D#COA2L,CHAR,TA#AFM2L,MAXLEN=L'RQCOA2L,               +        
               OUTPUT=(D,B#SAVED,RQCOA2L)                                       
F$A2S    LKMAP F,D#COA2S,UBIN,TA#AFM2S,OLEN=L'RQCOA2S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA2S)                                       
F$A2I    LKMAP F,D#COA2I,CHAR,TA#AFM2I,MAXLEN=L'RQCOA2I,               +        
               OUTPUT=(D,B#SAVED,RQCOA2I)                                       
F$A2Y    LKMAP F,D#COA2Y,CHAR,TA#AFM2Y,MAXLEN=L'RQCOA2Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA2Y)                                       
F$A3T    LKMAP F,D#COA3T,CHAR,TA#AFM3T,MAXLEN=L'RQCOA3T,               +        
               OUTPUT=(D,B#SAVED,RQCOA3T)                                       
F$A3L    LKMAP F,D#COA3L,CHAR,TA#AFM3L,MAXLEN=L'RQCOA3L,               +        
               OUTPUT=(D,B#SAVED,RQCOA3L)                                       
F$A3S    LKMAP F,D#COA3S,UBIN,TA#AFM3S,OLEN=L'RQCOA3S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA3S)                                       
F$A3I    LKMAP F,D#COA3I,CHAR,TA#AFM3I,MAXLEN=L'RQCOA3I,               +        
               OUTPUT=(D,B#SAVED,RQCOA3I)                                       
F$A3Y    LKMAP F,D#COA3Y,CHAR,TA#AFM3Y,MAXLEN=L'RQCOA3Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA3Y)                                       
F$A4T    LKMAP F,D#COA4T,CHAR,TA#AFM4T,MAXLEN=L'RQCOA4T,               +        
               OUTPUT=(D,B#SAVED,RQCOA4T)                                       
F$A4L    LKMAP F,D#COA4L,CHAR,TA#AFM4L,MAXLEN=L'RQCOA4L,               +        
               OUTPUT=(D,B#SAVED,RQCOA4L)                                       
F$A4S    LKMAP F,D#COA4S,UBIN,TA#AFM4S,OLEN=L'RQCOA4S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA4S)                                       
F$A4I    LKMAP F,D#COA4I,CHAR,TA#AFM4I,MAXLEN=L'RQCOA4I,               +        
               OUTPUT=(D,B#SAVED,RQCOA4I)                                       
F$A4Y    LKMAP F,D#COA4Y,CHAR,TA#AFM4Y,MAXLEN=L'RQCOA4Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA4Y)                                       
F$MU1    LKMAP F,D#COMU1,CHAR,TA#MUSC1,MAXLEN=L'RQCOMU1,               +        
               OUTPUT=(D,B#SAVED,RQCOMU1)                                       
F$MU2    LKMAP F,D#COMU2,CHAR,TA#MUSC2,MAXLEN=L'RQCOMU2,               +        
               OUTPUT=(D,B#SAVED,RQCOMU2)                                       
F$MU3    LKMAP F,D#COMU3,CHAR,TA#MUSC3,MAXLEN=L'RQCOMU3,               +        
               OUTPUT=(D,B#SAVED,RQCOMU3)                                       
F$MU4    LKMAP F,D#COMU4,CHAR,TA#MUSC4,MAXLEN=L'RQCOMU4,               +        
               OUTPUT=(D,B#SAVED,RQCOMU4)                                       
F$ALU    LKMAP F,D#COALU,UBIN,TA#COALU,OLEN=L'RQCOALU,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCOALU)                                       
F$RMU    LKMAP F,D#CORMU,UBIN,TA#CORMU,OLEN=L'RQCORMU,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCORMU)                                       
F$ART    LKMAP F,D#COART,CHAR,TA#AFMRT,MAXLEN=L'RQCOART,               +        
               OUTPUT=(D,B#SAVED,RQCOART)                                       
F$DUB    LKMAP F,D#CODUB,PDAT,TA#DUBDT,OUTPUT=(D,B#SAVED,RQCODUB)               
F$STY    LKMAP F,D#COSTY,CHAR,TA#SESTY,MAXLEN=L'RQCOSTY,               +        
               OUTPUT=(D,B#SAVED,RQCOSTY)                                       
F$FL1    LKMAP F,D#COFL1,CHAR,TA#FILT1,MAXLEN=L'RQCOFL1,               +        
               OUTPUT=(D,B#SAVED,RQCOFL1)                                       
F$LCK    LKMAP F,D#COLCK,CHAR,TA#LOCKD,MAXLEN=L'RQCOLCK,               +        
               OUTPUT=(D,B#SAVED,RQCOLCK)                                       
F$REL    LKMAP F,D#COREL,CHAR,TA#RELSD,MAXLEN=L'RQCOREL,               +        
               OUTPUT=(D,B#SAVED,RQCOREL)                                       
F$CDO    LKMAP F,D#COCDO,CHAR,TA#CANDO,MAXLEN=L'RQCOCDO,               +        
               OUTPUT=(D,B#SAVED,RQCOCDO)                                       
F$CRT    LKMAP F,D#COCRT,CHAR,TA#CANRT,MAXLEN=L'RQCOCRT,               +        
               OUTPUT=(D,B#SAVED,RQCOCRT)                                       
F$WRK    LKMAP F,D#COWRK,CHAR,TA#WRKOC,MAXLEN=L'RQCOWRK,               +        
               OUTPUT=(D,B#SAVED,RQCOWRK)                                       
F$SOP    LKMAP F,D#COSOP,CHAR,TA#SOAPR,MAXLEN=L'RQCOSOP,               +        
               OUTPUT=(D,B#SAVED,RQCOSOP)                                       
F$PRT    LKMAP F,D#COPRT,CHAR,TA#DAPRT,MAXLEN=L'RQCOPRT,               +        
               OUTPUT=(D,B#SAVED,RQCOPRT)                                       
F$VAL    LKMAP F,D#COVAL,UBIN,TA#VRAL,OLEN=L'RQCOALS,MAXLEN=3,         +        
               OUTPUT=(D,B#SAVED,RQCOALS)                                       
F$NCS    LKMAP F,D#CONCS,CHAR,TA#CONCS,MAXLEN=L'RQCONCS,               +        
               OUTPUT=(D,B#SAVED,RQCONCS)                                       
F$PRN    LKMAP F,D#COPRN,CHAR,TA#PRDNM,MAXLEN=L'RQCOPRN,               +        
               OUTPUT=(D,B#SAVED,RQCOPRN)                                       
F$A1O    LKMAP F,D#COA1O,HEXD,TA#AFM1O,OLEN=L'RQCOA1O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA1O)                                       
F$A2O    LKMAP F,D#COA2O,HEXD,TA#AFM2O,OLEN=L'RQCOA2O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA2O)                                       
F$A3O    LKMAP F,D#COA3O,HEXD,TA#AFM3O,OLEN=L'RQCOA3O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA3O)                                       
F$A4O    LKMAP F,D#COA4O,HEXD,TA#AFM4O,OLEN=L'RQCOA4O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA4O)                                       
F$A5O    LKMAP F,D#COA5O,HEXD,TA#AFM5O,OLEN=L'RQCOA5O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA5O)                                       
F$A5T    LKMAP F,D#COA5T,CHAR,TA#AFM5T,MAXLEN=L'RQCOA5T,               +        
               OUTPUT=(D,B#SAVED,RQCOA5T)                                       
F$A5L    LKMAP F,D#COA5L,CHAR,TA#AFM5L,MAXLEN=L'RQCOA5L,               +        
               OUTPUT=(D,B#SAVED,RQCOA5L)                                       
F$A5S    LKMAP F,D#COA5S,UBIN,TA#AFM5S,OLEN=L'RQCOA5S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA5S)                                       
F$A5I    LKMAP F,D#COA5I,CHAR,TA#AFM5I,MAXLEN=L'RQCOA5I,               +        
               OUTPUT=(D,B#SAVED,RQCOA5I)                                       
F$A5Y    LKMAP F,D#COA5Y,CHAR,TA#AFM5Y,MAXLEN=L'RQCOA5Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA5Y)                                       
F$A6O    LKMAP F,D#COA6O,HEXD,TA#AFM6O,OLEN=L'RQCOA6O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA6O)                                       
F$A6T    LKMAP F,D#COA6T,CHAR,TA#AFM6T,MAXLEN=L'RQCOA6T,               +        
               OUTPUT=(D,B#SAVED,RQCOA6T)                                       
F$A6L    LKMAP F,D#COA6L,CHAR,TA#AFM6L,MAXLEN=L'RQCOA6L,               +        
               OUTPUT=(D,B#SAVED,RQCOA6L)                                       
F$A6S    LKMAP F,D#COA6S,UBIN,TA#AFM6S,OLEN=L'RQCOA6S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA6S)                                       
F$A6I    LKMAP F,D#COA6I,CHAR,TA#AFM6I,MAXLEN=L'RQCOA6I,               +        
               OUTPUT=(D,B#SAVED,RQCOA6I)                                       
F$A6Y    LKMAP F,D#COA6Y,CHAR,TA#AFM6Y,MAXLEN=L'RQCOA6Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA6Y)                                       
F$A7O    LKMAP F,D#COA7O,HEXD,TA#AFM7O,OLEN=L'RQCOA7O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA7O)                                       
F$A7T    LKMAP F,D#COA7T,CHAR,TA#AFM7T,MAXLEN=L'RQCOA7T,               +        
               OUTPUT=(D,B#SAVED,RQCOA7T)                                       
F$A7L    LKMAP F,D#COA7L,CHAR,TA#AFM7L,MAXLEN=L'RQCOA7L,               +        
               OUTPUT=(D,B#SAVED,RQCOA7L)                                       
F$A7S    LKMAP F,D#COA7S,UBIN,TA#AFM7S,OLEN=L'RQCOA7S,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOA7S)                                       
F$A7I    LKMAP F,D#COA7I,CHAR,TA#AFM7I,MAXLEN=L'RQCOA7I,               +        
               OUTPUT=(D,B#SAVED,RQCOA7I)                                       
F$A7Y    LKMAP F,D#COA7Y,CHAR,TA#AFM7Y,MAXLEN=L'RQCOA7Y,               +        
               OUTPUT=(D,B#SAVED,RQCOA7Y)                                       
F$HFN    LKMAP F,D#COHFN,CHAR,TA#HFN,MAXLEN=L'RQCOHFN,                 +        
               OUTPUT=(D,B#SAVED,RQCOHFN)                                       
F$FL2    LKMAP F,D#COFL2,CHAR,TA#FILT2,MAXLEN=L'RQCOFL2,               +        
               OUTPUT=(D,B#SAVED,RQCOFL2)                                       
F$FL3    LKMAP F,D#COFL3,CHAR,TA#FILT3,MAXLEN=L'RQCOFL3,               +        
               OUTPUT=(D,B#SAVED,RQCOFL3)                                       
F$FL4    LKMAP F,D#COFL4,CHAR,TA#FILT4,MAXLEN=L'RQCOFL4,               +        
               OUTPUT=(D,B#SAVED,RQCOFL4)                                       
F$FL4    LKMAP F,D#COCON,CHAR,TA#CON,MAXLEN=L'RQCOCON,                 +        
               OUTPUT=(D,B#SAVED,RQCOCON)                                       
F$WID    LKMAP F,D#COWID,CHAR,TA#WAPID,MAXLEN=L'RQCOWID,               +        
               OUTPUT=(D,B#SAVED,RQCOWID)                                       
F$EOV    LKREQ F,250,(I,B#SAVED,I$EROV),UBIN,LIST=F,                   +        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,251,(I,B#SAVED,I$CLMC),UBIN,LIST=F,                   +        
               OLEN=1,MAXLEN=3,TEXT=TA#CLRMC,COL=*                              
F$RID    LKMAP F,D#CORID,CHAR,TA#REQID,MAXLEN=L'RQCORID,               +        
               OUTPUT=(D,B#SAVED,RQCORID)                                       
F$WRI    LKMAP F,D#COWRI,CHAR,TA#WARID,MAXLEN=L'RQCOWRI,               +        
               OUTPUT=(D,B#SAVED,RQCOWRI)                                       
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
AUCSTTBL DS    A                   A(UPDATED CAST SEQ NUMBER TABLE)             
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
SVSTAT   DS    XL(L'TACOSTAT)      SAVED STATUS                                 
SVCOKEY  DS    XL(L'IOKEY)         SAVED COMMERCIAL KEY                         
SVCAKEY  DS    XL(L'IOKEY)         SAVED CAST KEY                               
SVCAHKEY DS    XL(L'IOKEY)         SAVED CAST HOLDING FEE KEY                   
SVACKEY  DS    XL(L'IOKEY)         SAVED CAST GUARANTEE KEY                     
SVCMKEY  DS    XL(L'IOKEY)         SAVED COMMENT KEY                            
SVAKKEY  DS    XL(L'TLAKKEY)       SAVED ALIAS KEY                              
SVVRKEY  DS    XL(L'TLVRKEY)       SAVED VERSION KEY                            
SVCOSTA  DS    XL(L'TACOSTA2)      SAVED COMMERCIAL STATUS 2                    
SVWID    DS    CL(L'RQCOWID)       SAVED WEB APPLICATION ID                     
SVCICLG  DS    CL(L'TACICLG)       SAVED CLIENT GROUP                           
         DS    0H                                                               
SVNUNXTC DS    XL(L'TANUNXTC)      HIGHEST CAST SEQUENCE NUMBER                 
                                                                                
ORIGAGY  DS    XL(L'TLCOAGY)       ORIGINAL AGENCY                              
ORIGCID  DS    XL(L'TACOCID)       ORIGINAL COMMERCIAL ID                       
ORIGTYP  DS    XL(L'TACOTYPE)      ORIGINAL COMMERCIAL TYPE                     
                                                                                
COMSTAT  DS    X                   COMMERCIAL STATUS                            
COUNVRFY EQU   X'80'               CHANGE UNVERIFIES THE COMMERCIAL             
COREISSU EQU   X'40'               CHANGE NECESSITATES HF REISSUE               
COVR2PRO EQU   X'20'               VERSION 2 RECORD HAS BEEN PROCESSED          
COSEACST EQU   X'10'               CHANGE TO SEASONAL - UPDATE CAST             
COTYPCST EQU   X'08'               CHANGE TO TYPE - UPDATE CAST                 
COMEDCST EQU   X'04'               CHANGE TO MEDIA - UPDATE CAST                
COTRKCST EQU   X'02'               CHANGE TO TRACKS - UPDATE CAST               
CORECUPD EQU   X'01'               RECORD WAS UPDATED ON FILE                   
                                                                                
CASTSTAT DS    X                   CAST STATUS                                  
CAUPDATE EQU   X'80'               CAST NEEDS TO BE UPDATED                     
CAADDED  EQU   X'40'               CAST ADDED                                   
                                                                                
MEDIA    DS    X                   MEDIA EQUATE                                 
TRANEYR  DS    X                   TRANSLATED EDIT TYPE YEAR                    
                                                                                
ADDGAC   DS    XL(GACLNQ+1)        NEW AGENCY/CLIENT LIMITATION                 
                                                                                
CPYSTAT1 DS    X                   COPY ROUTINES' STATUS                        
CPYTAMD1 EQU   X'80'               INTERNET/NEW MEDIA 1 ENCOUNTERED             
CPYTAMD2 EQU   X'40'               INTERNET/NEW MEDIA 2 ENCOUNTERED             
CPYTAMD3 EQU   X'20'               INTERNET/NEW MEDIA 3 ENCOUNTERED             
CPYTAMD4 EQU   X'10'               INTERNET/NEW MEDIA 4 ENCOUNTERED             
CPYTAMD5 EQU   X'08'               INTERNET/NEW MEDIA 5 ENCOUNTERED             
CPYTAMD6 EQU   X'04'               INTERNET/NEW MEDIA 6 ENCOUNTERED             
CPYTACMG EQU   X'02'               COMMENT ENCOUNTERED                          
CPYTACP1 EQU   X'01'               MUSIC CODE 1 ENCOUNTERED                     
                                                                                
CPYSTAT2 DS    X                   COPY ROUTINES' STATUS                        
CPYTACP2 EQU   X'80'               MUSIC CODE 2 ENCOUNTERED                     
CPYTACP3 EQU   X'40'               MUSIC CODE 3 ENCOUNTERED                     
CPYTACP4 EQU   X'20'               MUSIC CODE 4 ENCOUNTERED                     
CPYTACSF EQU   X'10'               FILM STUDIO ENCOUNTERED                      
CPYTACSM EQU   X'08'               MUSIC STUDIO ENCOUNTERED                     
CPYTACSR EQU   X'04'               RECORD STUDIO ENCOUNTERED                    
CPYSTALF EQU   X'02'               LIFT DETAILS ENCOUNTERED                     
CPYTAMC1 EQU   X'01'               MUSIC CONTRACT 1 ENCOUNTERED                 
                                                                                
CPYSTAT3 DS    X                   COPY ROUTINES' STATUS                        
CPYTAMC2 EQU   X'80'               MUSIC CONTRACT 2 ENCOUNTERED                 
CPYTAMC3 EQU   X'40'               MUSIC CONTRACT 3 ENCOUNTERED                 
CPYTAMC4 EQU   X'20'               MUSIC CONTRACT 4 ENCOUNTERED                 
CPYTAFNP EQU   X'10'               PRODUCT NAME ENCOUNTERED                     
CPYTAFNM EQU   X'08'               AFM TITLE ENCOUNTERED                        
CPYSTAFL EQU   X'04'               FILTERS ENCOUNTERED                          
CPYSTLAK EQU   X'02'               VERSION ALIAS ENCOUNTERED                    
CPYTAMC5 EQU   X'01'               MUSIC CONTRACT 5 ENCOUNTERED                 
                                                                                
CPYSTAT4 DS    X                   COPY ROUTINES' STATUS                        
CPYTAMC6 EQU   X'80'               MUSIC CONTRACT 6 ENCOUNTERED                 
CPYTAMC7 EQU   X'40'               MUSIC CONTRACT 7 ENCOUNTERED                 
CPYTATR1 EQU   X'20'               MUSIC CONTRACT/TRACK 1 ENCOUNTERED           
CPYTATR2 EQU   X'10'               MUSIC CONTRACT/TRACK 2 ENCOUNTERED           
CPYTATR3 EQU   X'08'               MUSIC CONTRACT/TRACK 3 ENCOUNTERED           
CPYTATR4 EQU   X'04'               MUSIC CONTRACT/TRACK 4 ENCOUNTERED           
CPYTATR5 EQU   X'02'               MUSIC CONTRACT/TRACK 5 ENCOUNTERED           
CPYTATR6 EQU   X'01'               MUSIC CONTRACT/TRACK 6 ENCOUNTERED           
                                                                                
CPYSTAT5 DS    X                   COPY ROUTINES' STATUS                        
CPYTATR7 EQU   X'80'               MUSIC CONTRACT 7 ENCOUNTERED                 
                                                                                
ITTAB    DS    XL(TLLNQ*7+1)       INPUT TRACK TABLE                            
OTTAB    DS    XL(TLLNQ*7+1)       ORIGINAL TRACK TABLE                         
TRKALIST DS    XL(TLLNQ*7+1)       ADDED TRACKS LIST                            
TRKDLIST DS    XL(TLLNQ*7+1)       DELETED TRACKS LIST                          
CTRKALST DS    XL(TLLNQ*7+1)       CAST SPECIFIC ADDED TRACKS LIST              
                                                                                
BLOCK    DS    CL(MQMLNQ)          SNDMQHFR BLOCK                               
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        COMMERCIAL MAINTENANCE REQUEST MAP FIELDS                    *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQCOMOD  DS    CL1                 MODE                                         
RQCORTV  EQU   1                   RETRIEVE                                     
RQCOVFY  EQU   2                   VERIFY                                       
RQCOEXE  EQU   3                   EXECUTE                                      
RQCOSTF  DS    CL8                 STAFF CODE                                   
RQCOCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCOVER  DS    XL1                 VERSION NUMBER                               
RQCOAGY  DS    CL6                 AGENCY CODE                                  
RQCOCID  DS    CL12                COMMERCIAL ID                                
RQCOCLI  DS    CL6                 CLIENT CODE                                  
RQCOPRD  DS    CL6                 PRODUCT CODE                                 
RQCOTIT  DS    CL36                TITLE                                        
RQCOTYP  DS    CL1                 COMMERCIAL TYPE                              
RQCOATI  DS    CL36                TITLE FOR AFM                                
RQCOADS  DS    CL2                 ADDENDUM STATE                               
RQCOFFC  DS    XL3                 FIRST FIXED CYCLE DATE                       
RQCOFAR  DS    XL3                 FIRST AIR DATE                               
RQCOSAR  DS    XL3                 SECOND SEASON FIRST AIR DATE                 
RQCOEXP  DS    XL3                 EXPIRATION DATE                              
RQCOMED  DS    CL1                 MEDIA CODE                                   
RQCOSEC  DS    XL1                 LENGTH (SECONDS)                             
RQCOLID  DS    CL12                LIFT ID                                      
RQCOLLN  DS    XL1                 LIFT LENGTH                                  
RQCOATY  DS    XL1                 ACTRA TYPE                                   
RQCOAEX  DS    XL3                 ACTRA EXPIRATION DATE                        
RQCOFDT  DS    XL3                 FILM DATE                                    
RQCOFSU  DS    CL12                FILM STUDIO                                  
RQCOFCY  DS    CL12                FILM CITY                                    
RQCOFST  DS    CL2                 FILM STATE                                   
RQCORDT  DS    XL3                 RECORDING DATE                               
RQCORSU  DS    CL12                RECORDING STUDIO                             
RQCORCY  DS    CL12                RECORDING CITY                               
RQCORST  DS    CL2                 RECORDING STATE                              
RQCOMDT  DS    XL3                 MUSIC DATE                                   
RQCOMSU  DS    CL12                MUSIC STUDIO                                 
RQCOMCY  DS    CL12                MUSIC CITY                                   
RQCOMST  DS    CL2                 MUSIC STATE                                  
RQCOIN1  DS    CL4                 INTERNET/NEW MEDIA CODE 1                    
RQCOIN2  DS    CL4                 INTERNET/NEW MEDIA CODE 2                    
RQCOIN3  DS    CL4                 INTERNET/NEW MEDIA CODE 3                    
RQCOIN4  DS    CL4                 INTERNET/NEW MEDIA CODE 4                    
RQCOIN5  DS    CL4                 INTERNET/NEW MEDIA CODE 5                    
RQCOIN6  DS    CL4                 INTERNET/NEW MEDIA CODE 6                    
RQCOEYR  DS    CL4                 EDIT TYPE YEAR                               
RQCOETY  DS    CL6                 EDIT TYPE                                    
RQCOADT  DS    XL3                 ACTIVE DATE                                  
RQCOIDT  DS    XL3                 INACTIVE DATE                                
RQCOPOL  DS    CL6                 COMMERCIAL POOL CODE                         
RQCOSCY  DS    CL1                 HAS SPLIT CYCLES?                            
RQCOATT  DS    CL2                 ATTENTION CODE (BILL TO)                     
RQCOCMT  DS    CL60                COMMENT                                      
RQCOA1O  DS    XL4                 AFM 1 INTERNAL COMMERCIAL NUMBER             
RQCOA1T  DS    CL1                 AFM 1 TRACK                                  
RQCOA1L  DS    CL1                 AFM 1 LIFT                                   
RQCOA1S  DS    XL2                 AFM 1 LENGTH (SECONDS)                       
RQCOA1I  DS    CL36                AFM 1 TRACK TITLE                            
RQCOA1Y  DS    CL1                 AFM 1 MUSIC TYPE                             
RQCOA2O  DS    XL4                 AFM 2 INTERNAL COMMERCIAL NUMBER             
RQCOA2T  DS    CL1                 AFM 2 TRACK                                  
RQCOA2L  DS    CL1                 AFM 2 LIFT                                   
RQCOA2S  DS    XL2                 AFM 2 LENGTH (SECONDS)                       
RQCOA2I  DS    CL36                AFM 2 TRACK TITLE                            
RQCOA2Y  DS    CL1                 AFM 2 MUSIC TYPE                             
RQCOA3O  DS    XL4                 AFM 3 INTERNAL COMMERCIAL NUMBER             
RQCOA3T  DS    CL1                 AFM 3 TRACK                                  
RQCOA3L  DS    CL1                 AFM 3 LIFT                                   
RQCOA3S  DS    XL2                 AFM 3 LENGTH (SECONDS)                       
RQCOA3I  DS    CL36                AFM 3 TRACK TITLE                            
RQCOA3Y  DS    CL1                 AFM 3 MUSIC TYPE                             
RQCOA4O  DS    XL4                 AFM 4 INTERNAL COMMERCIAL NUMBER             
RQCOA4T  DS    CL1                 AFM 4 TRACK                                  
RQCOA4L  DS    CL1                 AFM 4 LIFT                                   
RQCOA4S  DS    XL2                 AFM 4 LENGTH (SECONDS)                       
RQCOA4I  DS    CL36                AFM 4 TRACK TITLE                            
RQCOA4Y  DS    CL1                 AFM 4 MUSIC TYPE                             
RQCOA5O  DS    XL4                 AFM 5 INTERNAL COMMERCIAL NUMBER             
         DS    CL12                                                             
RQCOA5T  DS    CL1                 AFM 5 TRACK                                  
RQCOA5L  DS    CL1                 AFM 5 LIFT                                   
RQCOA5S  DS    XL2                 AFM 5 LENGTH (SECONDS)                       
RQCOA5I  DS    CL36                AFM 5 TRACK TITLE                            
RQCOA5Y  DS    CL1                 AFM 5 MUSIC TYPE                             
RQCOA6O  DS    XL4                 AFM 6 INTERNAL COMMERCIAL NUMBER             
         DS    CL12                                                             
RQCOA6T  DS    CL1                 AFM 6 TRACK                                  
RQCOA6L  DS    CL1                 AFM 6 LIFT                                   
RQCOA6S  DS    XL2                 AFM 6 LENGTH (SECONDS)                       
RQCOA6I  DS    CL36                AFM 6 TRACK TITLE                            
RQCOA6Y  DS    CL1                 AFM 6 MUSIC TYPE                             
RQCOA7O  DS    XL4                 AFM 7 INTERNAL COMMERCIAL NUMBER             
         DS    CL12                                                             
RQCOA7T  DS    CL1                 AFM 7 TRACK                                  
RQCOA7L  DS    CL1                 AFM 7 LIFT                                   
RQCOA7S  DS    XL2                 AFM 7 LENGTH (SECONDS)                       
RQCOA7I  DS    CL36                AFM 7 TRACK TITLE                            
RQCOA7Y  DS    CL1                 AFM 7 MUSIC TYPE                             
RQCOMU1  DS    CL8                 MUSIC CODE 1                                 
RQCOMU2  DS    CL8                 MUSIC CODE 2                                 
RQCOMU3  DS    CL8                 MUSIC CODE 3                                 
RQCOMU4  DS    CL8                 MUSIC CODE 4                                 
RQCOALU  DS    XL1                 ALLOWABLE USES                               
RQCORMU  DS    XL1                 REMAINING USES                               
RQCOART  DS    CL1                 AFM RATE                                     
RQCODUB  DS    XL3                 DUB DATE                                     
RQCOSTY  DS    CL1                 SESSION TYPE                                 
RQCOFLTS DS    0C                  FILTERS                                      
RQCOFL1  DS    CL1                 FILTER 1                                     
RQCOFL2  DS    CL3                 FILTER 2                                     
RQCOFL3  DS    CL1                 FILTER 3                                     
RQCOFL4  DS    CL1                 FILTER 4                                     
RQCOFLNQ EQU   *-RQCOFLTS                                                       
RQCOLCK  DS    CL1                 COMMERCIAL LOCKED?                           
RQCOREL  DS    CL1                 COMMERCIAL RELEASED?                         
RQCOCDO  DS    CL1                 CANADIAN DOLLARS?                            
RQCOCRT  DS    CL1                 CANADIAN RATES?                              
RQCOWRK  DS    CL1                 WORKDATE ON CHECKS?                          
RQCOSOP  DS    CL1                 SOAP RESIDUALS?                              
RQCOPRT  DS    CL1                 DISPLAY AS PRINT?                            
RQCOALS  DS    XL1                 VERSION ALIAS                                
RQCONCS  DS    CL1                 CHARGE CSF?                                  
RQCOPRN  DS    CL16                PRODUCT NAME                                 
RQCOHFN  DS    CL1                 GENERATE HOLDING FEE NOTICES?                
RQCOCON  DS    CL1                 CONTRACT TYPE                                
RQCOWID  DS    CL18                WEB APPLICATION ID                           
RQCORID  DS    CL6                 REQUEST ID                                   
RQCOWRI  DS    CL12                WEB APPLICATION RECORD ID                    
RQRCLNQ  EQU   *-RQCOMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR GUARANTEE'S AGENCY/CLIENT TABLE                   *          
**********************************************************************          
                                                                                
GACTABD DSECT                                                                   
GACAGY   DS    CL6                                                              
GACCLI   DS    CL6                                                              
GACLNQ   EQU   *-GACTABD                                                        
GACTLNQ  EQU   (GACLNQ*100)+1                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TALNK16   05/29/15'                                      
         END                                                                    
