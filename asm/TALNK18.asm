*          DATA SET TALNK18    AT LEVEL 002 AS OF 05/29/15                      
*PHASE T70418E                                                                  
TALNK18  TITLE 'VERSION MAINTENANCE UPLOAD SERVER'                              
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TAVR,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (320*L'TLDRREC)+1                                                
UPPTRBLK EQU   (320*L'TLDRREC)+1                                                
VIDTBL   EQU   (249*L'TAVRCID)+1                                                
UCSTTBL  EQU   (300*UCTLNQ)+1                                                   
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK+VIDTBL+UCSTTBL                          
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA18**,RR=RE                                           
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
                                                                                
INPUT    CLC   LP_QMAPN,=AL2(I#VRULD)                                           
         JNE   INPUT10                                                          
         BRAS  RE,VRUP             PROCESS THE MAINTENANCE INPUT RECORD         
         J     YES                 EXIT BACK TO DDLINK                          
                                                                                
INPUT10  CLC   LP_QMAPN,=AL2(I#VRDULD)                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,VRDUP            PROCESS THE DELETE INPUT RECORD              
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
*        PROCESS VERSION RECORD                                       *         
***********************************************************************         
                                                                                
VRUP     NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#COSTA)               
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA 3)                             
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AUPPTRS                                                       
         AHI   RF,UPPTRBLK                                                      
         AHI   RF,VIDTBL                                                        
         ST    RF,AUCSTTBL         SAVE A(UPDATED CAST TABLE)                   
         MVI   0(RF),X'FF'                                                      
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,EROV502,I$CLMC                   
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   VRUP30                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   VRUP30                                                           
                                                                                
***********************************************************************         
                                                                                
         CLI   RQCOVER,1           IF VERSION NUMBER IS NOT 1 ...               
         JE    VRUP100                                                          
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCOSTF                                   
         JNE   VRUP30                                                           
                                                                                
         BRAS  RE,VALAGY           VALIDATE AGENCY                              
         JNE   VRUP30                                                           
                                                                                
         BRAS  RE,VALCOM           SAVE PRIMARY COMMERCIAL IN AIO1              
         JNE   VRUP30              SAVE INDEXED COMMERCIAL IN AIO2              
                                                                                
         BRAS  RE,VALCID           VALIDATE VERSION ID                          
                                                                                
         BRAS  RE,VALAFM           VALIDATE AFM CONTRACTS/TRACKS                
         GOTOR VALMUS,DMCB,SVAGY   VALIDATE MUSIC CODES                         
         BRAS  RE,VALALS           VALIDATE VERSION ALIAS                       
                                                                                
         MVI   OTTAB,X'FF'         INITIALIZE ORIGINAL TRACK TABLE              
         MVI   TRKALIST,X'FF'      ADDED TRACK TABLE                            
         MVI   TRKDLIST,X'FF'      DELETED TRACK TABLE                          
                                                                                
         USING TLVRD,R3                                                         
         XC    TLVRKEY,TLVRKEY     READ FOR VERSION KEY/RECORD                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,RQCOCOM                                                  
         MVC   TLVRVER,RQCOVER                                                  
         MVC   SVVRKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VRUP10                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         J     VRUP20                                                           
VRUP10   BRAS  RE,INITCHA          IF FOUND, PREPARE TO CHANGE                  
                                                                                
VRUP20   BRAS  RE,VALTYP           VALIDATE TYPE                                
                                                                                
VRUP30   MVI   OUTPUT,VRSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    VRUP40                                                           
         MVI   OUTPUT,VRSTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    VRUP40                                                           
         MVI   OUTPUT,VRSTOK2                                                   
                                                                                
VRUP40   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   VRUP70                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    VRUP70                                                           
         BRAS  RE,EXECADD          ADD OR                                       
         BRAS  RE,EXECCHA          CHANGE VERSION RECORD ...                    
         JNE   VRUP70                                                           
         GOTOR (#UPDPTRS,AUPDPTRS) AND UPDATE PASSIVE POINTERS                  
                                                                                
         BRAS  RE,ADDCOM           ... THEN ADD OR                              
         BRAS  RE,CHGCOM           CHANGE INDEXED COMMERCIAL RECORD             
         BRAS  RE,UVFYCOM          AND UNVERIFY MASTER COMMERCIAL ...           
                                                                                
         CLI   TRKALIST,X'FF'      IF TRACKS ARE BEING ADDED                    
         JNE   VRUP50                                                           
         CLI   TRKDLIST,X'FF'      OR DELETED                                   
         JE    VRUP60                                                           
VRUP50   GOTOR PROCST,DMCB,AIO1    THEN PROCESS CAST RECORDS                    
                                                                                
VRUP60   BRAS  RE,PROALS           PROCESS ALIAS RECORD                         
         BRAS  RE,ADDWTR           AND ADD WEB TRANSACTION RECORD               
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
VRUP70   GOTO1 VHEXOUT,DMCB,RQCOCOM,OUTPUT,L'RQCOCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',5),            +        
               ('LD_UBINQ',RQCOVER),(L'RQCOVER,0)                               
                                                                                
         MVI   OUTPUT,C'N'                                                      
         TM    VERSTAT,VRRECUPD                                                 
         JZ    VRUP80                                                           
         MVI   OUTPUT,C'Y'                                                      
VRUP80   GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',6),               +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#COERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    VRUP90              SEND DOWN VERSION RECORD                     
         CLI   ACTION,ACTADD                                                    
         JE    VRUP90                                                           
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#VRULD)               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRUN',I#COSDLD)               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',D#COSSTF),     +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',D#COSCOM),     +        
               ('LD_CHARQ',RQCOCOM),(L'RQCOCOM,0)                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',D#COSVER),     +        
               ('LD_CHARQ',RQCOVER),(L'RQCOVER,0)                               
         GOTOR VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTERU',0),0,0                  
                                                                                
VRUP90   BRAS  RE,OUTUCAST         OUTPUT UPDATED CAST RECORDS                  
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
VRUP100  GOTOR (#VALSTF,AVALSTF),DMCB,RQCOSTF                                   
         JNE   VRUP110                                                          
                                                                                
         BRAS  RE,VALCOM           SAVE PRIMARY COMMERCIAL IN AIO1              
         JNE   VRUP110                                                          
                                                                                
         BRAS  RE,VALALS           VALIDATE VERSION ALIAS                       
         JNE   VRUP110                                                          
                                                                                
         BRAS  RE,SVV1INFO         SAVE COMMERCIAL ID                           
                                                                                
         BRAS  RE,CPYTLAK          PROCESS EXISTING ALIAS                       
                                                                                
VRUP110  MVI   OUTPUT,VRSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    VRUP120                                                          
         MVI   OUTPUT,VRSTOK2      ELSE RETURN "OK" STATUS                      
                                                                                
VRUP120  GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   VRUP130                                                          
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    VRUP130                                                          
         BRAS  RE,PROALS           PROCESS ALIAS RECORD                         
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
VRUP130  GOTO1 VHEXOUT,DMCB,RQCOCOM,OUTPUT,L'RQCOCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',5),            +        
               ('LD_UBINQ',RQCOVER),(L'RQCOVER,0)                               
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#COERR,OUTPUT                            
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRUP                                       *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
EROV502  DC    AL2(502),AL2(EROV502X-EROV502)                                   
         DC    AL2(4,6,14,16,25,31,32,33,34,35,36,37,38,39,75,83,91)            
         DC    AL2(99,130,133,134,135,162,169,176,183,184,185,186,187)          
         DC    AL2(188,189,228)                                                 
EROV502X EQU   *                                                                
                                                                                
EROV504  DC    AL2(504),AL2(EROV504X-EROV504)                                   
         DC    AL2(4,6,14,16,25,31,32,33,34,35,36,37,38,39,75,83,91)            
         DC    AL2(99,130,133,134,135,162,169,176,183,184,185,186,187)          
         DC    AL2(188,189,228)                                                 
EROV504X EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
VRSTOK1  EQU   1                   NO ERRORS - COMMERCIAL ADDED                 
VRSTOK2  EQU   2                   NO ERRORS - COMMERCIAL CHANGED               
VRSTER   EQU   3                   ERRORS                                       
                                                                                
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
                                                                                
         MVI   BYTE1,D#COCOM                                                    
         OC    RQCOCOM,RQCOCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
         MVI   BYTE1,D#COVER                                                    
         CLI   RQCOVER,0           ASSERT THAT VERSION NUMBER IS                
         JE    ARMIS               PROVIDED                                     
                                                                                
         MVI   BYTE1,D#COWID                                                    
         OC    RQCOWID,RQCOWID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
***********************************************************************         
                                                                                
         CLI   RQCOVER,1           IF VERSION NUMBER IS 1                       
         JNE   AR00                                                             
         MVI   BYTE1,D#COCID                                                    
         OC    RQCOCID,RQCOCID     ASSERT THAT VERSION ID IS                    
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#COTIT                                                    
         OC    RQCOTIT,RQCOTIT     ASSERT THAT VERSION TITLE IS                 
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#COTYP                                                    
         OC    RQCOTYP,RQCOTYP     ASSERT THAT TYPE IS NOT PROVIDED             
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COATI                                                    
         OC    RQCOATI,RQCOATI     ASSERT THAT TITLE FOR AFM IS                 
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#COFFC                                                    
         OC    RQCOFFC,RQCOFFC     ASSERT THAT FIRST FIXED CYCLE DATE           
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COFAR                                                    
         OC    RQCOFAR,RQCOFAR     ASSERT THAT FIRST AIR DATE                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COMED                                                    
         CLI   RQCOMED,0           ASSERT THAT MASTER COMMERCIAL MEDIA          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COSEC                                                    
         CLI   RQCOSEC,0           ASSERT THAT LENGTH IS NOT PROVIDED           
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COFDT                                                    
         OC    RQCOFDT,RQCOFDT     ASSERT THAT FILM DATE IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COFSU                                                    
         OC    RQCOFSU,RQCOFSU     ASSERT THAT FILM STUDIO IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COFCY                                                    
         OC    RQCOFCY,RQCOFCY     ASSERT THAT FILM CITY IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COFST                                                    
         OC    RQCOFST,RQCOFST     ASSERT THAT FILM STATE IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#CORDT                                                    
         OC    RQCORDT,RQCORDT     ASSERT THAT RECORD DATE IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#CORSU                                                    
         OC    RQCORSU,RQCORSU     ASSERT THAT RECORD STUDIO IS NOT             
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#CORCY                                                    
         OC    RQCORCY,RQCORCY     ASSERT THAT RECORD CITY IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#CORST                                                    
         OC    RQCORST,RQCORST     ASSERT THAT RECORD STATE IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COMDT                                                    
         OC    RQCOMDT,RQCOMDT     ASSERT THAT MUSIC DATE IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COMSU                                                    
         OC    RQCORMU,RQCOMSU     ASSERT THAT MUSIC STUDIO IS NOT              
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COMCY                                                    
         OC    RQCOMCY,RQCOMCY     ASSERT THAT MUSIC CITY IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COMST                                                    
         OC    RQCOMST,RQCOMST     ASSERT THAT MUSIC STATE IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COEYR                                                    
         OC    RQCOEYR,RQCOEYR     ASSERT THAT EDIT TYPE YEAR IS                
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#COADT                                                    
         OC    RQCOADT,RQCOADT     ASSERT THAT ACTIVE DATE IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COIDT                                                    
         OC    RQCOIDT,RQCOIDT     ASSERT THAT INACTIVE DATE IS NOT             
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#COCMT                                                    
         OC    RQCOCMT,RQCOCMT     ASSERT THAT COMMENT IS NOT                   
         JNZ   ARNAL               PROVIDED                                     
                                                                                
         MVI   BYTE1,D#COA1O                                                    
         OC    RQCOA1O,RQCOA1O     ASSERT THAT AFM 1 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA1T                                                    
         CLI   RQCOA1T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA1L                                                    
         CLI   RQCOA1L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA1S                                                    
         OC    RQCOA1S,RQCOA1S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA1I                                                    
         OC    RQCOA1I,RQCOA1I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA1Y                                                    
         CLI   RQCOA1Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA2O                                                    
         OC    RQCOA2O,RQCOA2O     ASSERT THAT AFM 2 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA2T                                                    
         CLI   RQCOA2T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA2L                                                    
         CLI   RQCOA2L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA2S                                                    
         OC    RQCOA2S,RQCOA2S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA2I                                                    
         OC    RQCOA2I,RQCOA2I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA2Y                                                    
         CLI   RQCOA2Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA3O                                                    
         OC    RQCOA3O,RQCOA3O     ASSERT THAT AFM 3 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA3T                                                    
         CLI   RQCOA3T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA3L                                                    
         CLI   RQCOA3L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA3S                                                    
         OC    RQCOA3S,RQCOA3S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA3I                                                    
         OC    RQCOA3I,RQCOA3I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA3Y                                                    
         CLI   RQCOA3Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA4O                                                    
         OC    RQCOA4O,RQCOA4O     ASSERT THAT AFM 4 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA4T                                                    
         CLI   RQCOA4T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA4L                                                    
         CLI   RQCOA4L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA4S                                                    
         OC    RQCOA4S,RQCOA4S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA4I                                                    
         OC    RQCOA4I,RQCOA4I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA4Y                                                    
         CLI   RQCOA4Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA5O                                                    
         OC    RQCOA5O,RQCOA5O     ASSERT THAT AFM 5 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA5T                                                    
         CLI   RQCOA5T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA5L                                                    
         CLI   RQCOA5L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA5S                                                    
         OC    RQCOA5S,RQCOA5S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA5I                                                    
         OC    RQCOA5I,RQCOA5I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA5Y                                                    
         CLI   RQCOA5Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA6O                                                    
         OC    RQCOA6O,RQCOA6O     ASSERT THAT AFM 6 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA6T                                                    
         CLI   RQCOA6T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA6L                                                    
         CLI   RQCOA6L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA6S                                                    
         OC    RQCOA6S,RQCOA6S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA6I                                                    
         OC    RQCOA6I,RQCOA6I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA6Y                                                    
         CLI   RQCOA6Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COA7O                                                    
         OC    RQCOA7O,RQCOA7O     ASSERT THAT AFM 7 IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA7T                                                    
         CLI   RQCOA7T,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA7L                                                    
         CLI   RQCOA7L,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COA7S                                                    
         OC    RQCOA7S,RQCOA7S                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA7I                                                    
         OC    RQCOA7I,RQCOA7I                                                  
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COA7Y                                                    
         CLI   RQCOA7Y,0                                                        
         JNE   ARNAL                                                            
                                                                                
         MVI   BYTE1,D#COMU1       ASSERT THAT MUSIC CODE 1 IS NOT              
         OC    RQCOMU1,RQCOMU1     PROVIDED                                     
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COMU2       ASSERT THAT MUSIC CODE 2 IS NOT              
         OC    RQCOMU2,RQCOMU2     PROVIDED                                     
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COMU3       ASSERT THAT MUSIC CODE 3 IS NOT              
         OC    RQCOMU3,RQCOMU3     PROVIDED                                     
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COMU4       ASSERT THAT MUSIC CODE 4 IS NOT              
         OC    RQCOMU4,RQCOMU4     PROVIDED                                     
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COALU       ASSERT THAT ALLOWABLE USES ARE               
         CLI   RQCOALU,0           NOT PROVIDED                                 
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CORMU       ASSERT THAT REMAINING USES ARE               
         CLI   RQCORMU,0           NOT PROVIDED                                 
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CODUB       ASSERT THAT DUB DATE IS NOT                  
         OC    RQCODUB,RQCODUB     PROVIDED                                     
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#COCDO       ASSERT THAT MASTER COMMERCIAL                
         CLI   RQCOCDO,0           CANADIAN DOLLARS IS NOT PROVIDED             
         JNE   ARNAL                                                            
         MVI   BYTE1,D#COCRT       ASSERT THAT MASTER COMMERCIAL                
         CLI   RQCOCRT,0           CANADIAN RATES IS NOT PROVIDED               
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CO26K       ASSERT THAT 26K IS NOT PROVIDED              
         CLI   RQCO26K,0                                                        
         JNE   ARNAL                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
AR00     MVI   BYTE1,D#COCID                                                    
         OC    RQCOCID,RQCOCID     ASSERT THAT VERSION ID IS                    
         JZ    ARMIS               PROVIDED                                     
                                                                                
         MVI   BYTE1,D#COTIT                                                    
         OC    RQCOTIT,RQCOTIT     ASSERT THAT TITLE IS PROVIDED                
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#COFFC                                                    
         OC    RQCOFFC,RQCOFFC     ASSERT THAT MASTER COMMERCIAL FIRST          
         JZ    ARMIS               FIXED CYCLE DATE IS PROVIDED                 
                                                                                
         MVI   BYTE1,D#COMED                                                    
         CLI   RQCOMED,0           ASSERT THAT MASTER COMMERCIAL MEDIA          
         JE    ARMIS               IS PROVIDED                                  
                                                                                
         MVI   BYTE1,D#COCDO                                                    
         CLI   RQCOCDO,0           ASSERT THAT MASTER COMMERCIAL                
         JE    ARMIS               CANADIAN DOLLARS? IS PROVIDED                
                                                                                
         MVI   BYTE1,D#COCRT                                                    
         CLI   RQCOCRT,0           ASSERT THAT MASTER COMMERCIAL                
         JE    ARMIS               CANADIAN RATES? IS PROVIDED                  
                                                                                
         CLI   RQCOMED,TACOMEDT    IF MEDIA IS TELEVISION                       
         JE    AR10                                                             
         CLI   RQCOMED,TACOMEDI    OR INTERNET                                  
         JE    AR10                                                             
         CLI   RQCOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   AR20                                                             
AR10     MVI   BYTE1,D#COSEC                                                    
         CLI   RQCOSEC,0           ASSERT THAT LENGTH IS PROVIDED               
         JE    ARMIS                                                            
         CLC   =C'VC',RQCOWID      IF WEB APPLICATION IS NOT VITA               
         JE    AR20                COMPLETIONS                                  
         CLC   =C'TS',RQCOWID                                                   
         JE    AR20                                                             
         CLC   =C'TC',RQCOWID                                                   
         JE    AR20                                                             
         CLC   =C'RC',RQCOWID                                                   
         JE    AR20                                                             
         OC    RQCORDT,RQCORDT     AND RECORDING DATE IS NOT                    
         JNZ   AR20                PROVIDED                                     
         OC    RQCOFDT,RQCOFDT     ASSERT THAT FILM DATE IS PROVIDED            
         JNZ   AR20                                                             
         MVI   BYTE1,D#COFDT                                                    
         J     ARMIS                                                            
                                                                                
AR20     CLI   RQCOMED,TACOMEDR    IF MEDIA IS RADIO                            
         JNE   AR30                                                             
         MVI   BYTE1,D#COFDT                                                    
         OC    RQCOFDT,RQCOFDT     ASSERT THAT FILM DATE IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         CLC   =C'RC',RQCOWID                                                   
         JE    AR30                                                             
         OC    RQCORDT,RQCORDT     ASSERT THAT RECORD DATE IS PROVIDED          
         JNZ   AR30                                                             
         MVI   BYTE1,D#CORDT                                                    
         J     ARMIS                                                            
                                                                                
AR30     MVI   BYTE1,D#COFSU                                                    
         OC    RQCOFDT,RQCOFDT     IF FILM DATE IS PROVIDED                     
         JZ    AR40                                                             
         OC    RQCOFSU,RQCOFSU     ASSERT THAT FILM STUDIO IS PROVIDED          
         JZ    ARMIS                                                            
         OC    RQCOFCY,RQCOFCY     AND ASSERT THAT FILM CITY IS                 
         JNZ   AR50                PROVIDED                                     
         MVI   BYTE1,D#COFCY                                                    
         J     ARMIS                                                            
AR40     OC    RQCOFSU,RQCOFSU     OTHERWISE ASSERT THAT FILM STUDIO            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#COFCY                                                    
         OC    RQCOFCY,RQCOFCY     AND ASSERT THAT FILM CITY IS NOT             
         JNZ   ARNAL               PROVIDED                                     
         OC    RQCOFST,RQCOFST     AND ASSERT THAT FILM STATE IS NOT            
         JZ    AR50                PROVIDED                                     
         MVI   BYTE1,D#COFST                                                    
         J     ARNAL                                                            
                                                                                
AR50     MVI   BYTE1,D#CORSU                                                    
         OC    RQCORDT,RQCORDT     IF RECORD DATE IS PROVIDED                   
         JZ    AR60                                                             
         OC    RQCORSU,RQCORSU     ASSERT THAT RECORD STUDIO IS                 
         JZ    ARMIS               PROVIDED                                     
         OC    RQCORCY,RQCORCY     AND ASSERT THAT RECORD CITY IS               
         JNZ   AR70                PROVIDED                                     
         MVI   BYTE1,D#CORCY                                                    
         J     ARMIS                                                            
AR60     OC    RQCORSU,RQCORSU     OTHERWISE ASSERT THAT RECORD STUDIO          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#CORCY                                                    
         OC    RQCORCY,RQCORCY     AND ASSERT THAT RECORD CITY IS NOT           
         JNZ   ARNAL               PROVIDED                                     
         OC    RQCORST,RQCORST     AND ASSERT THAT RECORD STATE IS NOT          
         JZ    AR70                PROVIDED                                     
         MVI   BYTE1,D#CORST                                                    
         J     ARNAL                                                            
                                                                                
AR70     OC    RQCOEYR,RQCOEYR     IF EDIT TYPE YEAR IS PROVIDED                
         JZ    AR80                                                             
         OC    RQCOETY,RQCOETY     ASSERT THAT EDIT TYPE IS PROVIDED            
         JNZ   AR290                                                            
         MVI   BYTE1,D#COETY                                                    
         J     ARMIS                                                            
AR80     OC    RQCOETY,RQCOETY     OTHERWISE ASSERT THAT EDIT TYPE              
         JZ    AR290               IS NOT PROVIDED                              
         MVI   BYTE1,D#COETY                                                    
         J     ARNAL                                                            
                                                                                
       ++INCLUDE TAARAFM                                                        
                                                                                
AR430    J     YES                                                              
                                                                                
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
                                                                                
         MVI   BYTE1,D#COVER       VALIDATE VERSION NUMBER                      
         CLI   RQCOVER,1                                                        
         JL    AVINV                                                            
         CLI   RQCOVER,250                                                      
         JH    AVINV                                                            
                                                                                
         MVI   BYTE1,D#COVAL       VALIDATE ALIAS                               
         CLC   RQCOALS,RQCOVER                                                  
         JE    AVINV                                                            
                                                                                
         CLC   =C'VC',RQCOWID      VALIDATE WEB APPLICATION ID                  
         JE    AV10                                                             
         CLC   =C'TS',RQCOWID                                                   
         JE    AV10                                                             
         CLC   =C'TC',RQCOWID                                                   
         JE    AV10                                                             
         CLC   =C'RC',RQCOWID                                                   
         JE    AV10                                                             
         MVI   BYTE1,D#COWID                                                    
         J     AVINV                                                            
                                                                                
AV10     CLI   RQCOVER,1                                                        
         JE    YES                                                              
*                                  ASSERT ALL PROVIDED VALUES                   
*                                  (COMMON BETWEEN ASSETS AND                   
         BAS   RE,ACVAV            VERSIONS) ARE VALID                          
                                                                                
         MVI   BYTE1,D#CO26K       VALIDATE 26K?                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCO26K)                        
         JNE   AVINV                                                            
         J     YES                                                              
                                                                                
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
*        PROCESS VERSION DELETE REQUEST                               *         
***********************************************************************         
                                                                                
VRDUP    NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#COSTA)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AUPPTRS                                                       
         AHI   RF,UPPTRBLK                                                      
         ST    RF,AVIDTBL          SAVE A(VERSION ID TABLE)                     
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BAS   RE,DASTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   VRDUP10                                                          
         BAS   RE,DASTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   VRDUP10                                                          
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCOSTF                                   
         JNE   VRDUP10                                                          
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA 3)                             
                                                                                
         MVI   ACTION,ACTDEL       SET ACTION TO DELETE                         
                                                                                
         BRAS  RE,VALCOM           SAVE PRIMARY COMMERCIAL IN AIO1              
         JNE   VRDUP10             SAVE INDEXED COMMERCIAL IN AIO2              
                                                                                
*                                  ENSURE VERSION DOES NOT HAVE ANY             
         BRAS  RE,VALCST           ATTACHED CAST                                
*                                  ENSURE VERSION IS NOT ALIASED BY             
         BRAS  RE,VALALSD          ANOTHER VERSION                              
                                                                                
         USING TLVRD,R3                                                         
         XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ      READ FOR VERSION KEY                         
         MVC   TLVRCOM,RQCOCOM                                                  
         MVC   TLVRVER,RQCOVER                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VRDUP10                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERVDVRNF                                  
         DROP  R3                                                               
                                                                                
VRDUP10  MVI   OUTPUT,VRDEER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    VRDUP20                                                          
         MVI   OUTPUT,VRDEOK1      ELSE RETURN "OK" STATUS                      
                                                                                
VRDUP20  GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   VRDUP40                                                          
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    VRDUP40                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE ORIGINAL PASSIVE POINTERS               
                                                                                
         BAS   RE,BLDOTTAB         BUILD ORIGINAL TRACK TABLE                   
         MVI   ITTAB,X'FF'         INPUT TRACK TABLE                            
         MVI   TRKALIST,X'FF'      ADDED AND DELETED TRACK TABLES               
         GOTOR BLDTLIST,DMCB,TRKDLIST,OTTAB,ITTAB                               
                                                                                
         BRAS  RE,EXECDEL          DELETE VERSION RECORD                        
         BRAS  RE,CHGCOM           CHANGE INDEXED COMMERCIAL RECORD             
         BRAS  RE,UVFYCOM          UNVERIFY MASTER COMMERCIAL                   
         BRAS  RE,DELALS           AND DELETE ANY ALIAS RECORDS                 
                                                                                
         CLI   TRKDLIST,X'FF'      IF DELETED VERSION HAD ANY TRACKS            
         JE    VRDUP30                                                          
         GOTOR PROCST,DMCB,AIO1    THEN PROCESS CAST RECORDS                    
                                                                                
VRDUP30  TIME  DEC                                                              
         STCM  R0,14,SVTIME        SAVE TIME                                    
         BRAS  RE,ADDWTR           AND ADD WEB TRANSACTION RECORD               
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCOSTF),(L'RQCOSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
VRDUP40  GOTO1 VHEXOUT,DMCB,RQCOCOM,OUTPUT,L'RQCOCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',5),            +        
               ('LD_UBINQ',RQCOVER),(L'RQCOVER,0)                               
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#COERR,OUTPUT                            
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS ORIGINAL TRACK TABLE                          *         
*        ON ENTRY ... R4 = A(VERSION RECORD)                          *         
***********************************************************************         
                                                                                
BLDOTTAB NTR1                                                                   
         MVI   OTTAB,X'FF'                                                      
                                                                                
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BOTT10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         GOTOR ADDTTAB,DMCB,OTTAB                                               
         J     BOTT10                                                           
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
DASTREQ  NTR1                                                                   
         MVI   BYTE1,D#COMOD                                                    
         CLI   RQCOMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    DRMIS                                                            
                                                                                
         MVI   BYTE1,D#COSTF                                                    
         OC    RQCOSTF,RQCOSTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    DRMIS                                                            
                                                                                
         MVI   BYTE1,D#COCOM                                                    
         OC    RQCOCOM,RQCOCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    DRMIS               NUMBER IS PROVIDED                           
                                                                                
         MVI   BYTE1,D#COVER                                                    
         OC    RQCOVER,RQCOVER     ASSERT THAT VERSION NUMBER                   
         JZ    DRMIS               IS PROVIDED                                  
                                                                                
         MVI   BYTE1,D#COWID                                                    
         OC    RQCOWID,RQCOWID     ASSERT THAT WEB APPLICATION ID               
         JZ    DRMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
DRMIS    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENMIS',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
DASTVAL  NTR1                                                                   
         MVI   BYTE1,D#COMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCOMOD)                        
         JNE   DVINV                                                            
         J     YES                                                              
                                                                                
DVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVDVRNF DC    AL1(EVDVRNFX-*),AL2(4),AL1(ERRCATY3),AL1(D#COVER)                
         DC    C'Version not on file'                                           
EVDVRNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
VRDEOK1  EQU   1                   NO ERRORS - VERSION DELETED                  
VRDEER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AGENCY CODE FROM MASTER COMMERCIAL                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLAYD,R3                                                         
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY/RECORD                   
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SVAGY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   YES                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            R4=A(AGENCY ELEMENT)                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYSTA3,TAAYSLCK   ENSURE AGENCY IS NOT LOCKED                  
         JZ    YES                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRAYLK                                  
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGY                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRAYLK DC    AL1(EVRAYLKX-*),AL2(2),AL1(ERRCATY1),AL1(D#COAGY)                
         DC    C'Agency record is locked'                                       
EVRAYLKX EQU   *                                                                
                                                                                
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
         XC    TLCOPKEY,TLCOPKEY   READ FOR MASTER COMMERCIAL KEY/REC           
         MVI   TLCOPCD,TLCOCCDQ    VIA INTERNAL COMMERCIAL NUMBER               
         MVC   TLCOCCOM,RQCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEY(L'TLCOPKEY),IOKEYSAV                                       
         JE    VCOM10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRMCNF                                  
         J     NO                                                               
VCOM10   GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO1'                           
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
         MVC   SVAGY,TLCOAGY       SAVE AGENCY                                  
         MVC   SVCLI,TLCOCLI       CLIENT                                       
         MVC   SVPRD,TLCOPRD       PRODUCT                                      
         DROP  R4                                                               
                                                                                
         CLI   RQCOVER,1                                                        
         JE    VCOM30                                                           
         CLI   ACTION,ACTDEL                                                    
         JE    VCOM30                                                           
                                                                                
         MVI   ELCODE,TALFELQ      ENSURE MASTER COMMERCIAL DOES                
         BRAS  RE,GETEL            NOT HAVE A LIFT                              
         JNE   VCOM20                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRLIFT                                  
         J     NO                                                               
                                                                                
VCOM20   L     R4,AIO1                                                          
         MVI   ELCODE,TAVRELQ      ENSURE MASTER COMMERCIAL HAS                 
         BRAS  RE,GETEL            A VERSION 1                                  
         JE    VCOM30                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRV1NF                                  
         J     NO                                                               
                                                                                
         USING TACOD,R4                                                         
VCOM30   L     R4,AIO1                                                          
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL DETAILS ELEMENT)             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   SVMCID,TACOCID      SAVE MASTER COMMERCIAL ID                    
                                                                                
         CLI   RQCOVER,1                                                        
         JE    XIT                                                              
         CLI   ACTION,ACTDEL                                                    
         JE    VCOM80                                                           
                                                                                
         CLI   TACOTYPE,CTYMUS     ENSURE MASTER COMMERCIAL IS NOT              
         JNE   VCOM40              FOR AN AFM CONTRACT                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRCTYM                                  
         J     NO                                                               
                                                                                
VCOM40   CLC   RQCOFFC,TACOFCYC    ENSURE APPLICATION IS AWARE                  
         JE    VCOM50              OF MASTER COMMERCIAL FFC                     
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRFFCD                                  
         J     NO                                                               
                                                                                
VCOM50   CLC   RQCOMED,TACOMED     ENSURE APPLICATION IS AWARE                  
         JE    VCOM60              OF MASTER COMMERCIAL MEDIA                   
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRMEDD                                  
         J     NO                                                               
                                                                                
VCOM60   MVI   BYTE1,C'N'                                                       
         TM    TACOSTAT,TACOSCAN   ENSURE APPLICATION IS AWARE                  
         JZ    *+8                 OF MASTER COMMERCIAL CANADIAN                
         MVI   BYTE1,C'Y'          DOLLAR SETTING                               
         CLC   RQCOCDO,BYTE1                                                    
         JE    VCOM70                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRCDOD                                  
         J     NO                                                               
                                                                                
VCOM70   MVI   BYTE1,C'N'                                                       
         TM    TACOSTAT,TACOSCRT   ENSURE APPLICATION IS AWARE                  
         JZ    *+8                 OF MASTER COMMERCIAL CANADIAN                
         MVI   BYTE1,C'Y'          RATE SETTING                                 
         CLC   RQCOCRT,BYTE1                                                    
         JE    VCOM80                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRCRTD                                  
         J     NO                                                               
                                                                                
VCOM80   MVC   SVCOTYPE,TACOTYPE   SAVE COMMERCIAL TYPE                         
                                                                                
         OC    TACOVDTE,TACOVDTE   IF CAST IS VERIFIED                          
         JZ    VCOM90                                                           
         OI    COMSTAT,COVRFIED    SET STATUS                                   
         DROP  R4                                                               
                                                                                
         USING TLCOD,R3                                                         
VCOM90   XC    TLCOKEY,TLCOKEY     READ FOR INDEXED COMMERCIAL                  
         MVI   TLCOCD,TLCOCDQ      KEY/RECORD                                   
         GOTOR SETINDEX,DMCB,RQCOVER,TLCOVER                                    
         MVC   TLCOAGY,SVAGY                                                    
         MVC   TLCOCLI,SVCLI                                                    
         MVC   TLCOPRD,SVPRD                                                    
         MVC   TLCOCID,SVMCID                                                   
         MVC   TLCOCOM,RQCOCOM                                                  
         MVC   SVCOKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   YES                                                              
         OI    COMSTAT,COEXISTS    IF IT ALREADY EXISTS, SET STATUS             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO2'                           
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRMCNF DC    AL1(EVRMCNFX-*),AL2(214),AL1(ERRCATY3),AL1(D#COCOM)              
         DC    C'Master Commercial not on file'                                 
EVRMCNFX EQU   *                                                                
                                                                                
ERVRLIFT DC    AL1(EVRLIFTX-*),AL2(215),AL1(ERRCATY3),AL1(D#COCOM)              
         DC    C'Master Commercial has a Lift'                                  
EVRLIFTX EQU   *                                                                
                                                                                
ERVRV1NF DC    AL1(EVRV1NFX-*),AL2(216),AL1(ERRCATY3),AL1(D#COCOM)              
         DC    C'Version 1 must be setup first'                                 
EVRV1NFX EQU   *                                                                
                                                                                
ERVRCTYM DC    AL1(EVRCRTDX-*),AL2(199),AL1(ERRCATY3),AL1(D#COCOM)              
         DC    C'Versions cannot be setup for Music Contract'                   
EVRCTYMX EQU   *                                                                
                                                                                
ERVRFFCD DC    AL1(EVRFFCDX-*),AL2(218),AL1(ERRCATY3),AL1(D#COFFC)              
         DC    C'Master Commercial First Fixed Cycle Discrepancy'               
EVRFFCDX EQU   *                                                                
                                                                                
ERVRMEDD DC    AL1(EVRMEDDX-*),AL2(219),AL1(ERRCATY3),AL1(D#COMED)              
         DC    C'Master Commercial Media Discrepancy'                           
EVRMEDDX EQU   *                                                                
                                                                                
ERVRCDOD DC    AL1(EVRCDODX-*),AL2(220),AL1(ERRCATY3),AL1(D#COCDO)              
         DC    C'Master Commercial Canadian Dollars Discrepancy'                
EVRCDODX EQU   *                                                                
                                                                                
ERVRCRTD DC    AL1(EVRCRTDX-*),AL2(221),AL1(ERRCATY3),AL1(D#COCRT)              
         DC    C'Master Commercial Canadian Rates Discrepancy'                  
EVRCRTDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ENSURE VERSION DOES NOT HAVE ANY ATTACHED CAST               *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
VALCST   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCAKEY,TLCAKEY     READ FOR CAST KEY/RECORDS                    
         MVI   TLCACD,TLCACDQ      VIA INTERNAL COMMERCIAL NUMBER               
         MVC   TLCACOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VCST20                                                           
VCST10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VCST20   CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   XIT                                                              
         TM    TLCASRT,TLCASRMQ                                                 
         JO    VCST10                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO3),('TAFNTVER',0)         
         JNE   VCST10                                                           
         L     R4,AELEM            SEARCH FOR VERSION ELEMENT                   
         CLI   TAFNNAME,251        AND ENSURE CAST IS ON THE VERSION            
         JE    VCST10                                                           
         ZIC   R0,TAFNLEN                                                       
         SHI   R0,3                                                             
         LA    RE,TAFNNAME                                                      
VCST30   CLC   RQCOVER,0(RE)                                                    
         JE    VCST40                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,VCST30                                                        
         J     VCST10                                                           
         DROP  R4                                                               
                                                                                
VCST40   GOTOR (#ADDERR,AADDERR),DMCB,ERVRACST                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCST                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRACST DC    AL1(EVRACSTX-*),AL2(226),AL1(ERRCATY1),AL1(D#COVER)              
         DC    C'Version has attached cast'                                     
EVRACSTX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED VERSION ID                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
VALCID   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOICDQ    VIA VERSION ID                               
         MVC   TLCOIAGY,SVAGY                                                   
         MVC   TLCOICID,RQCOCID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCOICOM-TLCOPD),IOKEYSAV                                  
         JNE   XIT                                                              
         CLC   TLCOICOM,RQCOCOM    ENSURE VERSION IS NOT ALREADY                
         JNE   VCID10              ATTACHED TO A DIFFERENT COMMERCIAL           
         CLC   =C'VC',RQCOWID      OR, IF WEB APPLICATION ID IS NOT             
         JE    XIT                 VITA COMPLETIONS, A DIFFERENT                
         CLC   =C'TS',RQCOWID      VERSION                                      
         JE    XIT                                                              
         CLC   =C'TC',RQCOWID                                                   
         JE    XIT                                                              
         CLC   =C'RC',RQCOWID                                                   
         JE    XIT                                                              
         CLC   TLCOIVER,RQCOVER                                                 
         JE    XIT                                                              
VCID10   GOTOR (#ADDERR,AADDERR),DMCB,ERVRINUS                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCID                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRINUS DC    AL1(EVRINUSX-*),AL2(5),AL1(ERRCATY1),AL1(D#COCID)                
         DC    C'Version ID is already in use'                                  
EVRINUSX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED VERSION ALIAS                                *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALALS   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCOALS,0           ONLY VALIDATE IF VERSION ALIAS               
         JE    YES                 HAS BEEN PASSED                              
                                                                                
         USING TLCOD,R3                                                         
         XC    TLCOKEY,TLCOKEY     READ FOR INDEXED COMMERCIAL                  
         MVI   TLCOCD,TLCOCDQ      KEY/RECORD                                   
         GOTOR SETINDEX,DMCB,RQCOALS,TLCOVER                                    
         MVC   TLCOAGY,SVAGY                                                    
         MVC   TLCOCLI,SVCLI                                                    
         MVC   TLCOPRD,SVPRD                                                    
         MVC   TLCOCID,SVMCID                                                   
         MVC   TLCOCOM,RQCOCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   VALS40                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAVRD,R4                                                         
         MVI   ELCODE,TAVRELQ      ENSURE VERSION CODE EXISTS                   
         BRAS  RE,GETEL            ON COMMERCIAL                                
         J     *+8                                                              
VALS10   BRAS  RE,NEXTEL                                                        
         JNE   VALS40                                                           
         CLC   TAVRVERS,RQCOALS                                                 
         JNE   VALS10                                                           
                                                                                
         USING TLAKD,R3                                                         
         XC    TLAKKEY,TLAKKEY     ENSURE ALIASED VERSION IS NOT                
         MVI   TLAKCD,TLAKCDQ      ALIASED TO ANOTHER VERSION                   
         MVC   TLAKAGY,SVAGY                                                    
         MVC   TLAKADID,TAVRCID                                                 
         MVC   TLAKNCLI,SVCLI                                                   
         MVC   TLAKNPRD,SVPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VALS30                                                           
VALS20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VALS30   CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   YES                                                              
         CLC   TLAKCOM,RQCOCOM                                                  
         JNE   VALS20                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRALSD                                  
         J     NO                                                               
         DROP  R3,R4                                                            
                                                                                
VALS40   GOTOR (#ADDERR,AADDERR),DMCB,ERVRVANF                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALALS                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRVANF DC    AL1(EVRVANFX-*),AL2(128),AL1(ERRCATY1),AL1(D#COVAL)              
         DC    C'Version Code not on file'                                      
EVRVANFX EQU   *                                                                
                                                                                
ERVRALSD DC    AL1(EVRALSDX-*),AL2(129),AL1(ERRCATY1),AL1(D#COVAL)              
         DC    C'Version has an Alias'                                          
EVRALSDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ENSURE VERSION IS NOT ALIASED BY ANOTHER VERSION             *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     AIO1=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
VALALSD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AVIDTBL          BUILD VERSION ID TABLE                       
         BAS   RE,BLDVIDTB                                                      
                                                                                
VAD10    CLI   0(R2),X'FF'                                                      
         JE    XIT                                                              
                                                                                
         USING TLAKD,R3                                                         
         XC    TLAKKEY,TLAKKEY     ENSURE VERSION TO DELETE IS NOT              
         MVI   TLAKCD,TLAKCDQ      ALIASED BY ANOTHER VERSION                   
         MVC   TLAKAGY,SVAGY                                                    
         MVC   TLAKADID,0(R2)                                                   
         MVC   TLAKNCLI,SVCLI                                                   
         MVC   TLAKNPRD,SVPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   VAD20                                                            
         CLC   TLAKVER,RQCOVER                                                  
         JNE   VAD20                                                            
         CLC   TLAKCOM,RQCOCOM                                                  
         JNE   VAD20                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRAANV                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VAD20    LA    R2,L'TAVRCID(R2)                                                 
         J     VAD10                                                            
                                                                                
***********************************************************************         
*        BUILD VERSION ID TABLE                                       *         
*        ON ENTRY ... R2=A(VERSION ID TABLE)                          *         
*                     R3=A(KEY)                                       *         
*                     AIO1=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
BLDVIDTB NTR1                                                                   
         L     R4,AIO1                                                          
         MVC   IOKEYSAV(L'TLCOKEY),0(R4)                                        
         XR    R0,R0                                                            
                                                                                
         USING TAVRD,R4                                                         
BVT10    MVI   ELCODE,TAVRELQ      SAVE ALL VERSION IDS INTO TABLE              
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BVT20    BRAS  RE,NEXTEL                                                        
         JNE   BVT30                                                            
         CLC   TAVRVERS,RQCOVER                                                 
         JE    BVT20                                                            
         MVC   0(L'TAVRCID,R2),TAVRCID                                          
         LA    R2,L'TAVRCID(R2)                                                 
         J     BVT20                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R3                                                         
BVT30    AHI   R0,1                                                             
         CHI   R0,TLCOV250                                                      
         JH    BVT40                                                            
         MVC   TLCOKEY,IOKEYSAV    READ FOR VERSION KEY/RECORDS                 
         STC   R0,TLCOVER                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   BVT30                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         L     R4,AIO3                                                          
         J     BVT10                                                            
                                                                                
BVT40    MVI   0(R2),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALALSD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRAANV DC    AL1(EVRAANVX-*),AL2(227),AL1(ERRCATY1),AL1(D#COVER)              
         DC    C'Version is aliased by another Version'                         
EVRAANVX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED VERSION TYPE                                 *         
***********************************************************************         
                                                                                
VALTYP   NTR1  BASE=*,LABEL=*                                                   
         CLC   SVCOTYPE,RQCOTYP    ENSURE VERSION TYPE IS COMPATIBLE            
         JE    XIT                 WITH MASTER COMMERCIAL TYPE                  
         CLI   SVCOTYPE,0                                                       
         JE    VTYP10                                                           
         CLI   SVCOTYPE,CTYSPAN                                                 
         JNE   VTYP20                                                           
VTYP10   CLI   RQCOTYP,0                                                        
         JE    XIT                                                              
         CLI   RQCOTYP,CTYSPAN                                                  
         JE    XIT                                                              
                                                                                
VTYP20   GOTOR (#ADDERR,AADDERR),DMCB,ERVRINTY                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTYP                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRINTY DC    AL1(EVRINTYX-*),AL2(217),AL1(ERRCATY1),AL1(D#COTYP)              
         DC    C'Type incompatible with Master Commercial Type'                 
EVRINTYX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF VERSION                  *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERVRVRNF                                  
                                                                                
         MVC   TRKALIST,ITTAB                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRVRNF DC    AL1(EVRVRNFX-*),AL2(4),AL1(ERRCATY2),AL1(D#COCID)                
         DC    C'Version not on file'                                           
EVRVRNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF VERSION                    *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTCHA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         CLI   RQCOMOD,RQCOEXE     IF MODE IS EXECUTE                           
         JNE   INITC10                                                          
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE ORIGINAL POINTERS                       
                                                                                
INITC10  LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC20  BRAS  RE,NEXTEL                                                        
         JNE   INITC30                                                          
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TACMELQ       PROCESS EXISTING COMMENT                     
         JNE   *+12                                                             
         BRAS  RE,CPYTACM                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TACPELQ       PROCESS EXISTING PUBLISHED MUSIC             
         JNE   *+12                INFORMATION                                  
         BRAS  RE,CPYTACP                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TACSELQ       PROCESS EXISTING STUDIO INFORMATION          
         JNE   *+12                                                             
         BRAS  RE,CPYTACS                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TACOELQ       PROCESS EXISTING COMMERCIAL DETAILS          
         JNE   *+12                                                             
         BRAS  RE,CPYTACO                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TATRELQ       PROCESS EXISTING MUSIC CONTRACT/             
         JNE   *+12                TRACK INFORMATION                            
         BRAS  RE,CPYTATR                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TAFNELQ       PROCESS EXISTING FREE FORM NAMES             
         JNE   *+12                                                             
         BRAS  RE,CPYTAFN                                                       
         J     INITC20                                                          
                                                                                
         CLI   0(R4),TANAELQ       PROCESS EXISTING NAME                        
         JNE   INITC20                                                          
         BRAS  RE,CPYTANA                                                       
         J     INITC20                                                          
                                                                                
INITC30  BRAS  RE,CPYTLAK          PROCESS EXISTING ALIAS                       
                                                                                
         BRAS  RE,NEWTACM          PROCESS NEW COMMENT                          
         BRAS  RE,NEWTACP          PROCESS NEW PUBLISHED MUSIC INFO             
         BRAS  RE,NEWTACS          PROCESS NEW STUDIO INFORMATION               
         BRAS  RE,NEWTAFN          PROCESS NEW FREE FORM NAMES                  
         BRAS  RE,NEWTLAK          PROCESS NEW VERSION ALIAS                    
                                                                                
         GOTOR BLDTLIST,DMCB,TRKALIST,ITTAB,OTTAB                               
         GOTOR BLDTLIST,DMCB,TRKDLIST,OTTAB,ITTAB                               
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
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
         MVC   SVVRCID,TACOCID     SAVE ORIGINAL VERSION ID                     
                                                                                
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOCID,RQCOCID),TACOCID,     +        
               ERVRCID,0                                                        
         GOTOR (RF),(R1),(L'RQCOTYP,RQCOTYP),TACOTYPE,ERVRTYP,0                 
         GOTOR (RF),(R1),(L'RQCOSEC,RQCOSEC),TACOSEC,ERVRSEC,0                  
         GOTOR (RF),(R1),(L'RQCOFAR,RQCOFAR),TACOAIR,ERVRFAR,0                  
         GOTOR (RF),(R1),(L'TRANEYR,TRANEYR),TACOEDYR,ERVREYR,0                 
         GOTOR (RF),(R1),(L'RQCOETY,RQCOETY),TACOEDT,ERVRETY,          +        
               (L'TRANEYR,TRANEYR)                                              
         GOTOR (RF),(R1),(L'RQCOADT,RQCOADT),TACOACT,ERVRADT,0                  
         GOTOR (RF),(R1),(L'RQCOIDT,RQCOIDT),TACOINAC,ERVRIDT,0                 
         GOTOR (RF),(R1),(L'RQCOALU,RQCOALU),TACOAUSE,ERVRALU,         +        
               (X'FF',RQCOTYP),CCOMUS                                           
         GOTOR (RF),(R1),(L'RQCORMU,RQCORMU),TACORUSE,ERVRRMU,         +        
               (X'FF',RQCOTYP),CCOMUS                                           
         GOTOR (RF),(R1),(L'RQCODUB,RQCODUB),TACODUB,ERVRDUB,0                  
                                                                                
         GOTOR (#CHKSTAT,ACHKSTAT),DMCB,('TACOS26K',RQCO26K),          +        
               (0,TACOSTA3),ERVR26K                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTACO                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRCID  DC    AL1(EVRCIDX-*),AL2(6),AL1(ERRCATY2),AL1(D#COCID)                 
         DC    C'Review update to Version ID'                                   
EVRCIDX  EQU   *                                                                
                                                                                
ERVRTYP  DC    AL1(EVRTYPX-*),AL2(16),AL1(ERRCATY2),AL1(D#COTYP)                
         DC    C'Review update to Commercial Type'                              
EVRTYPX  EQU   *                                                                
                                                                                
ERVRSEC  DC    AL1(EVRSECX-*),AL2(25),AL1(ERRCATY2),AL1(D#COSEC)                
         DC    C'Review update to Length'                                       
EVRSECX  EQU   *                                                                
                                                                                
ERVRFAR  DC    AL1(EVRFARX-*),AL2(21),AL1(ERRCATY2),AL1(D#COFAR)                
         DC    C'Review update to First Air Date'                               
EVRFARX  EQU   *                                                                
                                                                                
ERVREYR  DC    AL1(EVREYRX-*),AL2(64),AL1(ERRCATY2),AL1(D#COEYR)                
         DC    C'Review update to Edit Type Year'                               
EVREYRX  EQU   *                                                                
                                                                                
ERVRETY  DC    AL1(EVRETYX-*),AL2(65),AL1(ERRCATY2),AL1(D#COETY)                
         DC    C'Review update to Edit Type'                                    
EVRETYX  EQU   *                                                                
                                                                                
ERVRADT  DC    AL1(EVRADTX-*),AL2(66),AL1(ERRCATY2),AL1(D#COADT)                
         DC    C'Review update to Active Date'                                  
EVRADTX  EQU   *                                                                
                                                                                
ERVRIDT  DC    AL1(EVRIDTX-*),AL2(67),AL1(ERRCATY2),AL1(D#COIDT)                
         DC    C'Review update to Inactive Date'                                
EVRIDTX  EQU   *                                                                
                                                                                
ERVRALU  DC    AL1(EVRALUX-*),AL2(114),AL1(ERRCATY2),AL1(D#COALU)               
         DC    C'Review update to Allowable Uses'                               
EVRALUX  EQU   *                                                                
                                                                                
ERVRRMU  DC    AL1(EVRRMUX-*),AL2(115),AL1(ERRCATY2),AL1(D#CORMU)               
         DC    C'Review update to Remaining Uses'                               
EVRRMUX  EQU   *                                                                
                                                                                
ERVRDUB  DC    AL1(EVRDUBX-*),AL2(117),AL1(ERRCATY2),AL1(D#CODUB)               
         DC    C'Review update to Dub Date'                                     
EVRDUBX  EQU   *                                                                
                                                                                
ERVR26K  DC    AL1(EVR26KX-*),AL2(228),AL1(ERRCATY2),AL1(D#CO26K)               
         DC    C'Review update to 26K Status'                                   
EVR26KX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CCOEXP   DC    AL1(CTYDEM),AL1(CTYIND),AL1(CTYSEAS2),AL1(CTYMUS)                
         DC    AL1(CTYPROMO),X'FF'                                              
                                                                                
CCOMUS   DC    AL1(CTYMUS),X'FF'                                                
                                                                                
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
         JE    CFN10                                                            
         CLI   TAFNTYPE,TAFNTWEB   OR WEB ID                                    
         JNE   CFN20                                                            
CFN10    MVI   0(R4),X'FF'         SET TO DELETE ELEMENT                        
         J     XIT                                                              
                                                                                
CFN20    CLI   TAFNTYPE,TAFNTMUS   IF TYPE IS AFM TITLE                         
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
               ERVRATI,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF FREE FORM NAME ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY *         
*        AND AFM TITLE IS BEING ADDED NOW, ROUTINE PROCESSES IT       *         
***********************************************************************         
                                                                                
NEWTAFN  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYTAFNM   IF AFM TITLE ELEMENT DID NOT EXIST           
         JO    XIT                 ON RECORD PREVIOUSLY                         
         OC    RQCOATI,RQCOATI     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERVRATI)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAFN AND NEWTAFN                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRATI  DC    AL1(EVRATIX-*),AL2(17),AL1(ERRCATY2),AL1(D#COATI)                
         DC    C'Review update to Title for AFM'                                
EVRATIX  EQU   *                                                                
                                                                                
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
         MVC   TLAKAGY,SVAGY                                                    
         MVC   TLAKADID,SVVRCID                                                 
         MVC   TLAKNCLI,SVCLI                                                   
         MVC   TLAKNPRD,SVPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CAK20                                                            
CAK10    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CAK20    CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   XIT                                                              
         CLC   TLAKCOM,RQCOCOM                                                  
         JNE   CAK10                                                            
         MVC   SVAKKEY,IOKEY                                                    
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCOALS,RQCOALS),TLAKVER,     +        
               ERVRALS,0                                                        
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
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERVRALS)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTLAK AND NEWTLAK                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERVRALS  DC    AL1(EVRALSX-*),AL2(130),AL1(ERRCATY2),AL1(D#COVAL)               
         DC    C'Review update to Alias Version'                                
EVRALSX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS VERSION RECORD TO FILE                          *         
*        ON ENTRY ... R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ADDING A VERSION RECORD ...               
         JNE   XIT                                                              
                                                                                
         CLI   RQCO26K,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCO26K,C'N'        SET DEFAULT "26K?"                           
                                                                                
         BAS   RE,EXECADVC         PRE-PROCESS FOR VITA COMPLETIONS             
                                                                                
         L     R4,AIO3             R4=A(I/O AREA 3)                             
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
         L     RF,ASVPTRS          AND POINTER BLOCK                            
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLVRD,R4                                                         
         MVC   0(L'TLVRKEY,R4),SVVRKEY                                          
         MVI   TLVRLEN+1,41                                                     
         DROP  R4                                                               
                                                                                
         USING TACOD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           INITIALIZE COMMERCIAL DETAILS ELEM           
         MVI   TACOEL,TACOELQ      AND ADD IT TO VERSION RECORD                 
         MVI   TACOLEN,TACOLNQ3                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
         XC    ELEM,ELEM           ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'SVAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'SVAGY),SVAGY                                          
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
         XC    ELEM,ELEM           ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'SVCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'SVCLI),SVCLI                                          
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
         OC    SVPRD,SVPRD                                                      
         JZ    EXECA10                                                          
         XC    ELEM,ELEM           ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'SVPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'SVPRD),SVPRD                                          
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
EXECA10  BRAS  RE,BLDREC           BUILD RECORD AND ADD TO FILE                 
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         OI    VERSTAT,VRRECUPD                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DOES SOME PREPROCESSING FOR ADDING A VERSION         *         
*        FROM VITA COMPLETIONS                                                  
*        ON ENTRY ... AIO1=A(PRIMARY COMMERCIAL RECORD)               *         
***********************************************************************         
                                                                                
EXECADVC NTR1                                                                   
         CLC   =C'VC',RQCOWID      IF WEB APPLICATION ID IS                     
         JE    EAVC00              VITA COMPLETIONS                             
         CLC   =C'TS',RQCOWID                                                   
         JE    EAVC00                                                           
         CLC   =C'TC',RQCOWID                                                   
         JE    EAVC00                                                           
         CLC   =C'RC',RQCOWID                                                   
         JNE   XIT                                                              
                                                                                
         USING TACOD,R4                                                         
EAVC00   L     R4,AIO1             GET PRIMARY COMMERCIAL'S                     
         MVI   ELCODE,TACOELQ      COMMERCIAL DETAILS ELEMENT                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'               COPY PRIMARY COMMERCIAL VALUES               
         MVC   RQCOFAR,TACOAIR     FOR FIRST AIR DATE                           
         MVC   RQCODUB,TACODUB     DUB DATE                                     
         MVC   RQCOETY,TACOEDT     EDIT TYPE                                    
         MVC   RQCOADT,TACOACT     ACTIVE DATE                                  
         MVC   RQCOIDT,TACOINAC    INACTIVE DATE                                
         MVC   TRANEYR,TACOEDYR    AND EDIT YEAR                                
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO1),('TAFNTMUS',0)         
         JNE   EAVC10                                                           
         MVC   RQCOATI,SPACES      COPY PRIMARY COMMERCIAL VALUE                
         L     R4,AELEM            FOR TITLE FOR AFM                            
         ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RQCOATI(0),TAFNNAME                                              
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
EAVC10   GOTOR (#GETELEM,AGETELEM),DMCB,('TACSELQ',AIO1),('TACSTYPF',0)         
         JNE   EAVC20              COPY PRIMARY COMMERCIAL VALUES               
         L     R4,AELEM            FOR FILM INFO                                
         MVC   RQCOFDT(RQCOFLNQ),TACSDATE                                       
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
EAVC20   GOTOR (#GETELEM,AGETELEM),DMCB,('TACSELQ',AIO1),('TACSTYPR',0)         
         JNE   EAVC30              COPY PRIMARY COMMERCIAL VALUES               
         L     R4,AELEM            FOR RECORD INFO                              
         MVC   RQCORDT(RQCORLNQ),TACSDATE                                       
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
EAVC30   GOTOR (#GETELEM,AGETELEM),DMCB,('TACSELQ',AIO1),('TACSTYPM',0)         
         JNE   XIT                 COPY PRIMARY COMMERCIAL VALUES               
         L     R4,AELEM            FOR MUSIC INFO                               
         MVC   RQCOMDT(RQCOMLNQ),TACSDATE                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING VERSION RECORD                      *         
*        ON ENTRY ... R4=A(VERSION RECORD)                            *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF CHANGING A VERSION RECORD ...             
         JNE   YES                                                              
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         BRAS  RE,BLDREC           BUILD RECORD AND PUT TO FILE                 
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         JE    EXECC10                                                          
         CLC   RQCOALS,SVAKKEY+TLAKVER-TLAKD                                    
         JE    NO                                                               
EXECC10  OI    VERSTAT,VRRECUPD                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES EXISTING VERSION RECORD                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLVRD,R4                                                         
EXECDEL  NTR1  BASE=*,LABEL=*                                                   
         OI    TLVRSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS) DELETE OLD RECORD AND POINTERS               
         OI    VERSTAT,VRRECUPD                                                 
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   RQCOCID,TACOCID     SAVE VERSION ID                              
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TANAELQ      SAVE COMMERCIAL TITLE                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   RQCOTIT(0),TANANAME                                              
         OC    RQCOTIT,SPACES                                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD VERSION RECORD                                         *         
*        ON ENTRY ... R4=A(VERSION RECORD)                            *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,UPDTACO          UPDATE COMMERCIAL DETAILS ELEMENT            
         BRAS  RE,ADDTACM          ADD COMMENT ELEMENT                          
         BRAS  RE,ADDTACP          ADD COMMERCIAL MUSIC ELEMENT                 
         BRAS  RE,ADDTACS          ADD COMMERCIAL STUDIO ELEMENT                
         BRAS  RE,ADDTATR          ADD MUSIC CONTRACT/TRACK ELEMENTS            
         BAS   RE,ADDTAFN          ADD FREE FORM NAME ELEMENT                   
         BRAS  RE,ADDTANA          ADD NAME ELEMENT                             
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQCOWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQCOSTF,SVTIME             
                                                                                
         GOTOR SVNXTSEQ,DMCB,AIO1                                               
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
                                                                                
         MVC   TACOCID,RQCOCID     PUT VERSION ID                               
         MVC   TACOTYPE,RQCOTYP    TYPE                                         
         MVC   TACOAIR,RQCOFAR     FIRST AIR DATE                               
         MVC   TACOSEC,RQCOSEC     LENGTH                                       
         MVC   TACOEDYR,TRANEYR    EDIT TYPE YEAR                               
         MVC   TACOEDT,RQCOETY     EDIT TYPE                                    
         MVC   TACOACT,RQCOADT     ACTIVE DATE                                  
         MVC   TACOINAC,RQCOIDT    INACTIVE DATE                                
         MVC   TACOAUSE,RQCOALU    ALLOWABLE USES                               
         MVC   TACORUSE,RQCORMU    REMAINING USES                               
         MVC   TACODUB,RQCODUB     DUB DATE                                     
                                                                                
         NI    TACOSTA3,X'FF'-TACOS26K                                          
         CLI   RQCO26K,C'Y'                                                     
         JNE   *+8                 PUT "26K?"                                   
         OI    TACOSTA3,TACOS26K   INDICATOR INTO ELEMENT                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ADD FREE FORM NAME ELEMENT                                   *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TAFND,R2                                                         
ADDTAFN  NTR1                                                                   
         OC    RQCOATI,RQCOATI     IF AFM TITLE IS PRESENT                      
         JZ    XIT                                                              
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTMUS   PUT TYPE                                     
         MVC   TAFNNAME,RQCOATI    AND AFM TITLE INTO ELEMENT                   
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS INDEXED COMMERCIAL RECORD TO FILE               *         
*        ON ENTRY ... R4=(I/O AREA 3)                                 *         
***********************************************************************         
                                                                                
ADDCOM   NTR1  BASE=*,LABEL=*                                                   
         TM    COMSTAT,COEXISTS    IF ADDING AN INDEXED COMMERCIAL              
         JO    XIT                 RECORD                                       
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
         L     RF,ASVPTRS          AND POINTER BLOCK                            
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLCOD,R4                                                         
         MVC   0(L'TLCOKEY,R4),SVCOKEY                                          
         MVI   TLCOLEN+1,41                                                     
                                                                                
         USING TAVRD,RF                                                         
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM           ADD VERSION ELEMENT TO INDEXED               
         MVI   TAVREL,TAVRELQ      COMMERCIAL RECORD                            
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,RQCOVER                                                 
         MVC   TAVRCID,RQCOCID                                                  
         MVC   TAVRSEC,RQCOSEC                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  RF                                                               
                                                                                
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING INDEXED COMMERCIAL RECORD           *         
***********************************************************************         
                                                                                
CHGCOM   NTR1  BASE=*,LABEL=*                                                   
         TM    COMSTAT,COEXISTS    IF CHANGING AN INDEXED COMMERCIAL            
         JZ    XIT                 RECORD                                       
                                                                                
         MVC   IOADDR,AIO2                                                      
         GOTOR (#SAVPTRS,ASAVPTRS) SAVE ORIGINAL POINTERS                       
                                                                                
         USING TAVRD,R4                                                         
         CLI   ACTION,ACTCHA       IF CHANGING EXISTING VERSION                 
         JE    CCOM10                                                           
         CLI   ACTION,ACTDEL       OR DELETING EXISTING VERSION                 
         JNE   CCOM30                                                           
CCOM10   L     R4,AIO2                                                          
         MVI   ELCODE,TAVRELQ      READ THROUGH ALL VERSION ELEMENTS            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CCOM20   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAVRVERS,RQCOVER    AND DELETE ORIGINAL VERSION ELEMENT          
         JNE   CCOM20                                                           
         MVI   0(R4),X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO2),0                     
         DROP  R4                                                               
                                                                                
         CLI   ACTION,ACTDEL                                                    
         JE    CCOM40                                                           
                                                                                
         USING TAVRD,RF                                                         
CCOM30   LA    RF,ELEM                                                          
         XC    ELEM,ELEM           ADD VERSION ELEMENT TO COMMERCIAL            
         MVI   TAVREL,TAVRELQ      RECORD                                       
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,RQCOVER                                                 
         MVC   TAVRCID,RQCOCID                                                  
         MVC   TAVRSEC,RQCOSEC                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO2,ELEM,0                        
         DROP  RF                                                               
                                                                                
         USING TLCOD,R4                                                         
CCOM40   CLI   ACTION,ACTCHA       IF ADDING OR DELETING VERSION                
         JE    CCOM50                                                           
         L     R4,AIO2                                                          
         CLI   TLCOVER,TLCOV026    TO/FROM MASTER COMMERCIAL                    
         JNE   CCOM50                                                           
         TM    COMSTAT,COVRFIED    AND CAST IS VERIFIED ...                     
         JZ    CCOM50                                                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACOVDTE,TACOVDTE   UNVERIFY THE COMMERCIAL                      
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         OI    TACOUVST,TACOUVCO                                                
         NI    COMSTAT,X'FF'-COVRFIED                                           
         DROP  R4                                                               
                                                                                
CCOM50   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO2'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UNVERIFIES MASTER COMMERCIAL IF VERSION IS           *         
*        BEING ADDED                                                  *         
***********************************************************************         
                                                                                
UVFYCOM  NTR1  BASE=*,LABEL=*                                                   
         TM    COMSTAT,COVRFIED    IF CAST IS VERIFIED                          
         JZ    XIT                                                              
         CLI   ACTION,ACTCHA       AND ADDING OR DELETING VERSION               
         JE    XIT                                                              
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1             R4=A(MASTER COMMERCIAL)                      
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACOVDTE,TACOVDTE   UNVERIFY THE COMMERCIAL                      
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         OI    TACOUVST,TACOUVCO                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO1'                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES "TO" ALIAS RECORDS FOR THIS VERSION          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
DELALS   NTR1  BASE=*,LABEL=*                                                   
         USING TLAKD,R3                                                         
         XC    TLAKKEY,TLAKKEY     ENSURE VERSION TO DELETE IS NOT              
         MVI   TLAKCD,TLAKCDQ      ALIASED BY ANOTHER VERSION                   
         MVC   TLAKAGY,SVAGY                                                    
         MVC   TLAKADID,RQCOCID                                                 
         MVC   TLAKNCLI,SVCLI                                                   
         MVC   TLAKNPRD,SVPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO3'                             
         CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR'                                
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         USING TLAKD,R4                                                         
         OI    TLAKSTAT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS OR PUTS COMMERCIAL'S CAST RECORD                *         
*        ON ENTRY ... AIO5 = A(CAST RECORD)                           *         
***********************************************************************         
                                                                                
UPDCST   NTR1  BASE=*,LABEL=*                                                   
         TM    CASTSTAT,CAUPDATE         IF "UPDATE NEEDED"                     
         JZ    XIT                                                              
         L     RE,ASVPTRS                                                       
         CLI   0(RE),0                                                          
         JNE   UCST10                                                           
         GOTOR (#ADDREC,AADDREC),'IO5'   ADD OR UPDATE THE CAST                 
         OI    CASTSTAT,CAADDED          RECORD AND ITS PASSIVES                
         J     UCST20                                                           
UCST10   GOTOR (#PUTREC,APUTREC),'IO5'                                          
         JNE   XIT                                                              
UCST20   GOTOR (#UPDPTRS,AUPDPTRS)                                              
         BRAS  RE,ADDUCAST               ADD CAST SEQUENCE NUMBER TO            
         J     XIT                       UPDATED CAST LIST                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESS ALIAS RECORD                                 *         
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
         MVC   TLAKAGY,SVAGY                                                    
         MVC   TLAKADID,RQCOCID                                                 
         MVC   TLAKNCLI,SVCLI                                                   
         MVC   TLAKNPRD,SVPRD                                                   
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
*        ROUTINE ADDS WEB TRANSACTION RECORD                          *         
*        ON ENTRY ... R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
         USING TLWTD,R4                                                         
ADDWTR   NTR1  BASE=*,LABEL=*                                                   
         TM    VERSTAT,VRRECUPD    IF ADDING WEB TRANSACTION RECORD             
         JZ    XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE WEB TRANSACTION RECORD            
         MVI   TLWTCD,TLWTCDQ                                                   
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   SET WEB APPLICATION                          
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTACVR   INDICATE IF CHANGING                         
         CLI   ACTION,ACTCHA                                                    
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTAAVR   OR ADDING VERSION                            
         CLI   ACTION,ACTADD                                                    
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTADVR   OR DELETING VERSION                          
                                                                                
AWTR10   MVC   TLWTWBID,RQCOWID    SET WEB APPLICATION ID                       
         MVC   TLWTUVCO,RQCOCOM    AND UNIQUE IDENTIFIER                        
         MVC   TLWTUVER,RQCOVER                                                 
                                                                                
         MVI   TLWTLEN+1,41        SET RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           BUILD WEB TRANSACTION ELEMENT                
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWT8LNQ                                                 
         MVC   TAWTSTAF,RQCOSTF                                                 
         MVC   TAWTCOAY,SVAGY                                                   
         MVC   TAWTCOID,SVMCID                                                  
         MVC   TAWTVRID,RQCOCID                                                 
         MVC   TAWTVRTI,RQCOTIT                                                 
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
*        ROUTINE RETURNS COMMERCIAL RECORD INDEX FOR RQCOVER          *         
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
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PERFORMS SPECIAL HANDLING FOR FILM DATE CHANGE       *         
***********************************************************************         
                                                                                
FDTCHG   NTR1  BASE=*,LABEL=*                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SAVES NEEDED VERSION 1 INFO FOR ALIAS PROCESSING     *         
*        ON ENTRY ... AIO1 = A(COMMERCIAL RECORD)                     *         
***********************************************************************         
                                                                                
SVV1INFO NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVVRCID,TACOCID     SAVE COMMERCIAL ID                           
         MVC   RQCOCID,TACOCID                                                  
         MVC   RQCOMED,TACOMED     AND MEDIA                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TALNKCOM                                                       
         EJECT                                                                  
       ++INCLUDE TALNKMUS                                                       
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - VERSION MAINTENANCE UPLOAD                            *         
***********************************************************************         
                                                                                
VRHDR    LKMAP H,I#VRULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#COMOD,UBIN,TA#PMODE,OLEN=L'RQCOMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCOMOD)                                       
F$STF    LKMAP F,D#COSTF,CHAR,TA#STAFF,MAXLEN=L'RQCOSTF,               +        
               OUTPUT=(D,B#SAVED,RQCOSTF)                                       
F$COM    LKMAP F,D#COCOM,HEXD,TA#COMCD,OLEN=L'RQCOCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOCOM)                                       
F$VER    LKMAP F,D#COVER,UBIN,TA#VER,OLEN=L'RQCOVER,MAXLEN=3,          +        
               OUTPUT=(D,B#SAVED,RQCOVER)                                       
F$CID    LKMAP F,D#COCID,CHAR,TA#CIDCD,MAXLEN=L'RQCOCID,               +        
               OUTPUT=(D,B#SAVED,RQCOCID)                                       
F$TIT    LKMAP F,D#COTIT,CHAR,TA#COTIT,MAXLEN=L'RQCOTIT,               +        
               OUTPUT=(D,B#SAVED,RQCOTIT)                                       
F$TYP    LKMAP F,D#COTYP,CHAR,TA#COTYP,MAXLEN=L'RQCOTYP,               +        
               OUTPUT=(D,B#SAVED,RQCOTYP)                                       
F$ATI    LKMAP F,D#COATI,CHAR,TA#COATI,MAXLEN=L'RQCOATI,               +        
               OUTPUT=(D,B#SAVED,RQCOATI)                                       
F$FFC    LKMAP F,D#COFFC,PDAT,TA#FFC,OUTPUT=(D,B#SAVED,RQCOFFC)                 
F$FAR    LKMAP F,D#COFAR,PDAT,TA#FAD,OUTPUT=(D,B#SAVED,RQCOFAR)                 
F$MED    LKMAP F,D#COMED,CHAR,TA#MEDCD,MAXLEN=L'RQCOMED,               +        
               OUTPUT=(D,B#SAVED,RQCOMED)                                       
F$SEC    LKMAP F,D#COSEC,UBIN,TA#COSEC,OLEN=L'RQCOSEC,MAXLEN=3,        +        
               OUTPUT=(D,B#SAVED,RQCOSEC)                                       
F$FDT    LKMAP F,D#COFDT,PDAT,TA#FLMDT,OUTPUT=(D,B#SAVED,RQCOFDT)               
F$FSU    LKMAP F,D#COFSU,CHAR,TA#FLMST,MAXLEN=L'RQCOFSU,               +        
               OUTPUT=(D,B#SAVED,RQCOFSU)                                       
F$FCY    LKMAP F,D#COFCY,CHAR,TA#FLMCY,MAXLEN=L'RQCOFCY,               +        
               OUTPUT=(D,B#SAVED,RQCOFCY)                                       
F$FST    LKMAP F,D#COFST,CHAR,TA#FLSTA,MAXLEN=L'RQCOFST,               +        
               OUTPUT=(D,B#SAVED,RQCOFST)                                       
F$RDT    LKMAP F,D#CORDT,PDAT,TA#RECDT,OUTPUT=(D,B#SAVED,RQCORDT)               
F$RSU    LKMAP F,D#CORSU,CHAR,TA#RECSU,MAXLEN=L'RQCORSU,               +        
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
F$EYR    LKMAP F,D#COEYR,CHAR,TA#EDTYR,MAXLEN=L'RQCOEYR,               +        
               OUTPUT=(D,B#SAVED,RQCOEYR)                                       
F$ETY    LKMAP F,D#COETY,CHAR,TA#EDTYP,MAXLEN=L'RQCOETY,               +        
               OUTPUT=(D,B#SAVED,RQCOETY)                                       
F$ADT    LKMAP F,D#COADT,PDAT,TA#ACTDT,OUTPUT=(D,B#SAVED,RQCOADT)               
F$IDT    LKMAP F,D#COIDT,PDAT,TA#INADT,OUTPUT=(D,B#SAVED,RQCOIDT)               
F$CMT    LKMAP F,D#COCMT,CHAR,TA#COMNT,MAXLEN=L'RQCOCMT,               +        
               OUTPUT=(D,B#SAVED,RQCOCMT)                                       
F$A1T    LKMAP F,D#COA1T,CHAR,TA#AFM1T,MAXLEN=L'RQCOA1T,               +        
               OUTPUT=(D,B#SAVED,RQCOA1T)                                       
F$A2T    LKMAP F,D#COA2T,CHAR,TA#AFM2T,MAXLEN=L'RQCOA2T,               +        
               OUTPUT=(D,B#SAVED,RQCOA2T)                                       
F$A3T    LKMAP F,D#COA3T,CHAR,TA#AFM3T,MAXLEN=L'RQCOA3T,               +        
               OUTPUT=(D,B#SAVED,RQCOA3T)                                       
F$A4T    LKMAP F,D#COA4T,CHAR,TA#AFM4T,MAXLEN=L'RQCOA4T,               +        
               OUTPUT=(D,B#SAVED,RQCOA4T)                                       
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
F$DUB    LKMAP F,D#CODUB,PDAT,TA#DUBDT,OUTPUT=(D,B#SAVED,RQCODUB)               
F$CDO    LKMAP F,D#COCDO,CHAR,TA#CANDO,MAXLEN=L'RQCOCDO,               +        
               OUTPUT=(D,B#SAVED,RQCOCDO)                                       
F$CRT    LKMAP F,D#COCRT,CHAR,TA#CANRT,MAXLEN=L'RQCOCRT,               +        
               OUTPUT=(D,B#SAVED,RQCOCRT)                                       
F$VAL    LKMAP F,D#COVAL,UBIN,TA#VRAL,OLEN=L'RQCOALS,MAXLEN=3,         +        
               OUTPUT=(D,B#SAVED,RQCOALS)                                       
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
F$A6O    LKMAP F,D#COA6O,HEXD,TA#AFM5O,OLEN=L'RQCOA6O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA6O)                                       
F$A6T    LKMAP F,D#COA6T,CHAR,TA#AFM6T,MAXLEN=L'RQCOA6T,               +        
               OUTPUT=(D,B#SAVED,RQCOA6T)                                       
F$A7O    LKMAP F,D#COA7O,HEXD,TA#AFM7O,OLEN=L'RQCOA7O,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCOA7O)                                       
F$A7T    LKMAP F,D#COA7T,CHAR,TA#AFM7T,MAXLEN=L'RQCOA7T,               +        
               OUTPUT=(D,B#SAVED,RQCOA7T)                                       
F$26K    LKMAP F,D#CO26K,CHAR,TA#26K,MAXLEN=L'RQCO26K,                 +        
               OUTPUT=(D,B#SAVED,RQCO26K)                                       
F$WID    LKMAP F,D#W4WID,CHAR,TA#WAPID,MAXLEN=L'RQCOWID,               +        
               OUTPUT=(D,B#SAVED,RQCOWID)                                       
F$EOV    LKREQ F,250,(I,B#SAVED,I$EROV),UBIN,LIST=F,                   +        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,251,(I,B#SAVED,I$CLMC),UBIN,LIST=F,                   +        
               OLEN=1,MAXLEN=3,TEXT=TA#CLRMC,COL=*                              
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP - VERSION DELETE UPLOAD                                 *         
***********************************************************************         
                                                                                
VRDHDR   LKMAP H,I#VRDULD,NEWREC=Y                                              
F$MOD    LKMAP F,D#CODMOD,UBIN,TA#PMODE,OLEN=L'RQCOMOD,MAXLEN=1,       +        
               OUTPUT=(D,B#SAVED,RQCOMOD)                                       
F$STF    LKMAP F,D#CODSTF,CHAR,TA#STAFF,MAXLEN=L'RQCOSTF,              +        
               OUTPUT=(D,B#SAVED,RQCOSTF)                                       
F$COM    LKMAP F,D#CODCOM,HEXD,TA#COMCD,OLEN=L'RQCOCOM,MAXLEN=8,       +        
               OUTPUT=(D,B#SAVED,RQCOCOM)                                       
F$VER    LKMAP F,D#CODVER,UBIN,TA#VER,OLEN=L'RQCOVER,MAXLEN=3,         +        
               OUTPUT=(D,B#SAVED,RQCOVER)                                       
F$WID    LKMAP F,D#CODWID,CHAR,TA#WAPID,MAXLEN=L'RQCOWID,              +        
               OUTPUT=(D,B#SAVED,RQCOWID)                                       
F$EOV    LKREQ F,D#CODEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,              +        
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
                                                                                
AVIDTBL  DS    A                   A(VERSION ID TABLE)                          
AUCSTTBL DS    A                   A(UPDATED CAST TABLE)                        
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
SVSTAT   DS    XL(L'TACOSTAT)      SAVED STATUS                                 
SVCOKEY  DS    XL(L'TLCOKEY)       SAVED COMMERCIAL KEY                         
SVVRKEY  DS    XL(L'TLVRKEY)       SAVED VERSION KEY                            
SVVRCID  DS    XL(L'TAVRCID)       SAVED VERSION ID                             
SVAKKEY  DS    XL(L'TLAKKEY)       SAVED ALIAS KEY                              
SVAGY    DS    CL(L'TLCOAGY)       SAVE AGENCY                                  
SVCLI    DS    CL(L'TLCOCLI)       CLIENT                                       
SVPRD    DS    CL(L'TLCOPRD)       PRODUCT                                      
SVMCID   DS    CL(L'TACOCID)       SAVED MASTER COMMERCIAL ID                   
SVCOTYPE DS    CL(L'TACOTYPE)      SAVED COMMERCIAL TYPE                        
SVCAKEY  DS    XL(L'IOKEY)         SAVED CAST KEY                               
         DS    0H                                                               
SVNUNXTC DS    XL(L'TANUNXTC)      HIGHEST CAST SEQUENCE NUMBER                 
                                                                                
COMSTAT  DS    X                   COMMERCIAL STATUS                            
COEXISTS EQU   X'80'               COMMERCIAL RECORD EXISTS                     
COVRFIED EQU   X'40'               COMMERCIAL IS UNVERIFIED                     
                                                                                
CASTSTAT DS    X                   CAST STATUS                                  
CAUPDATE EQU   X'80'               CAST NEEDS TO BE UPDATED                     
CAADDED  EQU   X'40'               CAST ADDED                                   
                                                                                
VERSTAT  DS    X                   VERSION STATUS                               
VRRECUPD EQU   X'01'               RECORD WAS UPDATED ON FILE                   
                                                                                
MEDIA    DS    X                   MEDIA EQUATE                                 
TRANEYR  DS    X                   TRANSLATED EDIT TYPE YEAR                    
                                                                                
CPYSTAT1 DS    X                   COPY ROUTINES' STATUS                        
CPYTACMG EQU   X'02'               COMMENT ENCOUNTERED                          
CPYTACP1 EQU   X'01'               MUSIC CODE 1 ENCOUNTERED                     
                                                                                
CPYSTAT2 DS    X                   COPY ROUTINES' STATUS                        
CPYTACP2 EQU   X'80'               MUSIC CODE 2 ENCOUNTERED                     
CPYTACP3 EQU   X'40'               MUSIC CODE 3 ENCOUNTERED                     
CPYTACP4 EQU   X'20'               MUSIC CODE 4 ENCOUNTERED                     
CPYTACSF EQU   X'10'               FILM STUDIO ENCOUNTERED                      
CPYTACSM EQU   X'08'               MUSIC STUDIO ENCOUNTERED                     
CPYTACSR EQU   X'04'               RECORD STUDIO ENCOUNTERED                    
CPYTAMC1 EQU   X'01'               MUSIC CONTRACT 1 ENCOUNTERED                 
                                                                                
CPYSTAT3 DS    X                   COPY ROUTINES' STATUS                        
CPYTAMC2 EQU   X'80'               MUSIC CONTRACT 2 ENCOUNTERED                 
CPYTAMC3 EQU   X'40'               MUSIC CONTRACT 3 ENCOUNTERED                 
CPYTAMC4 EQU   X'20'               MUSIC CONTRACT 4 ENCOUNTERED                 
CPYTAFNM EQU   X'08'               AFM TITLE ENCOUNTERED                        
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
RQCOCID  DS    CL12                VERSION ID                                   
RQCOTIT  DS    CL36                TITLE                                        
RQCOTYP  DS    CL1                 COMMERCIAL TYPE                              
RQCOATI  DS    CL36                TITLE FOR AFM                                
RQCOFFC  DS    XL3                 MASTER COMM'L FIRST FIXED CYCLE DATE         
RQCOFAR  DS    XL3                 FIRST AIR DATE                               
RQCOMED  DS    CL1                 MASTER COMMERCIAL MEDIA CODE                 
RQCOSEC  DS    XL1                 LENGTH (SECONDS)                             
RQCOFDT  DS    XL3                 FILM DATE                                    
RQCOFSU  DS    CL12                FILM STUDIO                                  
RQCOFCY  DS    CL12                FILM CITY                                    
RQCOFST  DS    CL2                 FILM STATE                                   
RQCOFLNQ EQU   *-RQCOFDT                                                        
RQCORDT  DS    XL3                 RECORDING DATE                               
RQCORSU  DS    CL12                RECORDING STUDIO                             
RQCORCY  DS    CL12                RECORDING CITY                               
RQCORST  DS    CL2                 RECORDING STATE                              
RQCORLNQ EQU   *-RQCORDT                                                        
RQCOMDT  DS    XL3                 MUSIC DATE                                   
RQCOMSU  DS    CL12                MUSIC STUDIO                                 
RQCOMCY  DS    CL12                MUSIC CITY                                   
RQCOMST  DS    CL2                 MUSIC STATE                                  
RQCOMLNQ EQU   *-RQCOMDT                                                        
RQCOEYR  DS    CL4                 EDIT TYPE YEAR                               
RQCOETY  DS    CL6                 EDIT TYPE                                    
RQCOADT  DS    XL3                 ACTIVE DATE                                  
RQCOIDT  DS    XL3                 INACTIVE DATE                                
RQCOCMT  DS    CL60                COMMENT                                      
RQCOA1O  DS    XL4                 AFM 1 INTERNAL COMMERCIAL NUMBER             
RQCOA1T  DS    CL1                 AFM 1 TRACK                                  
RQCOA1L  DS    CL1                                                              
RQCOA1S  DS    XL2                                                              
RQCOA1I  DS    CL36                                                             
RQCOA1Y  DS    CL1                                                              
RQCOA2O  DS    XL4                 AFM 2 INTERNAL COMMERCIAL NUMBER             
RQCOA2T  DS    CL1                 AFM 2 TRACK                                  
RQCOA2L  DS    CL1                                                              
RQCOA2S  DS    XL2                                                              
RQCOA2I  DS    CL36                                                             
RQCOA2Y  DS    CL1                                                              
RQCOA3O  DS    XL4                 AFM 3 INTERNAL COMMERCIAL NUMBER             
RQCOA3T  DS    CL1                 AFM 3 TRACK                                  
RQCOA3L  DS    CL1                                                              
RQCOA3S  DS    XL2                                                              
RQCOA3I  DS    CL36                                                             
RQCOA3Y  DS    CL1                                                              
RQCOA4O  DS    XL4                 AFM 4 INTERNAL COMMERCIAL NUMBER             
RQCOA4T  DS    CL1                 AFM 4 TRACK                                  
RQCOA4L  DS    CL1                                                              
RQCOA4S  DS    XL2                                                              
RQCOA4I  DS    CL36                                                             
RQCOA4Y  DS    CL1                                                              
RQCOA5O  DS    XL4                 AFM 5 INTERNAL COMMERCIAL NUMBER             
RQCOA5T  DS    CL1                 AFM 5 TRACK                                  
RQCOA5L  DS    CL1                                                              
RQCOA5S  DS    XL2                                                              
RQCOA5I  DS    CL36                                                             
RQCOA5Y  DS    CL1                                                              
RQCOA6O  DS    XL4                 AFM 6 INTERNAL COMMERCIAL NUMBER             
RQCOA6T  DS    CL1                 AFM 6 TRACK                                  
RQCOA6L  DS    CL1                                                              
RQCOA6S  DS    XL2                                                              
RQCOA6I  DS    CL36                                                             
RQCOA6Y  DS    CL1                                                              
RQCOA7O  DS    XL4                 AFM 7 INTERNAL COMMERCIAL NUMBER             
RQCOA7T  DS    CL1                 AFM 7 TRACK                                  
RQCOA7L  DS    CL1                                                              
RQCOA7S  DS    XL2                                                              
RQCOA7I  DS    CL36                                                             
RQCOA7Y  DS    CL1                                                              
RQCOMU1  DS    CL8                 MUSIC CODE 1                                 
RQCOMU2  DS    CL8                 MUSIC CODE 2                                 
RQCOMU3  DS    CL8                 MUSIC CODE 3                                 
RQCOMU4  DS    CL8                 MUSIC CODE 4                                 
RQCOALU  DS    XL1                 ALLOWABLE USES                               
RQCORMU  DS    XL1                 REMAINING USES                               
RQCODUB  DS    XL3                 DUB DATE                                     
RQCOCDO  DS    CL1                 MASTER COMMERCIAL CANADIAN DOLLARS?          
RQCOCRT  DS    CL1                 MASTER COMMERCIAL CANADIAN RATES?            
RQCOALS  DS    XL1                 VERSION ALIAS                                
RQCO26K  DS    CL1                 26K?                                         
RQCOWID  DS    CL18                WEB APPLICATION ID                           
RQRCLNQ  EQU   *-RQCOMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TALNK18   05/29/15'                                      
         END                                                                    
