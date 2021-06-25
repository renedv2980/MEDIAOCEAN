*          DATA SET TALNK17    AT LEVEL 004 AS OF 05/29/15                      
*PHASE T70417E                                                                  
TALNK17  TITLE 'CAST MAINTENANCE UPLOAD SERVER'                                 
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TACA,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (20*L'TLDRREC)+1                                                 
UPPTRBLK EQU   (20*L'TLDRREC)+1                                                 
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK                                         
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA17**,RR=RE                                           
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
                                                                                
INPUT    CLC   LP_QMAPN,=AL2(I#CAULD)                                           
         JNE   INPUT10                                                          
         BRAS  RE,CAUP             PROCESS THE MAINTENANCE INPUT RECORD         
         J     YES                 EXIT BACK TO DDLINK                          
                                                                                
INPUT10  CLC   LP_QMAPN,=AL2(I#CADULD)                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,CADUP            PROCESS THE DELETE INPUT RECORD              
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
*        PROCESS CAST MAINTENANCE REQUEST                             *         
***********************************************************************         
                                                                                
CAUP     NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#CASTA)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RQCASRT,RQCASRT                                                  
                                                                                
         ZICM  RF,I$VERS+1,3                                                    
         MVC   I$VERS,9(RF)        SET VERSION COUNT                            
         ZICM  RE,I$VERS,1                                                      
         JZ    CAUP10                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   VERSIONS(0),10(RF)  AND SAVE VERSION ARRAY                       
                                                                                
CAUP10   CLI   RQCADBL,0           IF DOUBLES FIELD IS BLANK                    
         JNE   *+8                                                              
         MVI   RQCADBL,C' '        SET AS A SPACE                               
                                                                                
         CLI   RQCARLL,C'A'        IF RELEASE LETTER IS A                       
         JL    CAUP15                                                           
         CLI   RQCARLL,C'B'        OR B                                         
         JH    CAUP15                                                           
         OC    RQCAEFF,RQCAEFF     AND EFFECTIVE DATE IS NOT PROVIDED           
         JNZ   CAUP15              DEFAULT IT TO TODAY'S DATE                   
         GOTO1 VDATCON,DMCB,(5,0),(1,RQCAEFF)                                   
                                                                                
CAUP15   GOTOR (#INITERR,AINITERR),DMCB,I$EROV,EROV501,I$CLMC                   
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCASTF                                   
         JNE   CAUP60                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA 3)                             
                                                                                
         BRAS  RE,PROWRI           PROCESS WEB APPLICATION RECORD ID            
                                                                                
         BRAS  RE,VALCOM           VALIDATE INTERNAL COMMERCIAL NUMBER          
         JNE   CAUP60              (SAVE COMMERCIAL RECORD IN AIO1)             
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   CAUP60                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   CAUP60                                                           
                                                                                
         BRAS  RE,VALAGY           VALIDATE AGENCY                              
                                                                                
         BRAS  RE,VALCLI           VALIDATE CLIENT                              
                                                                                
         BRAS  RE,VALSSN           VALIDATE SOCIAL SECURITY NUMBER              
         JNE   CAUP60              (SAVE W4 RECORD IN AIO2)                     
                                                                                
         BRAS  RE,VALATC           VALIDATE ATTACHED CORPORATION                
         BRAS  RE,VALLFI           VALIDATE LIFTED FROM COMMERCIAL              
         BRAS  RE,VALLCL           VALIDATE LOCAL                               
         BRAS  RE,VALAGT           VALIDATE AGENT CODE                          
         GOTOR VALTRK,DMCB,RQCATR1,ERCAINT1 VALIDATE TRACK 1                    
         GOTOR VALTRK,DMCB,RQCATR2,ERCAINT2 VALIDATE TRACK 2                    
         GOTOR VALTRK,DMCB,RQCATR3,ERCAINT3 VALIDATE TRACK 3                    
         GOTOR VALTRK,DMCB,RQCATR4,ERCAINT4 VALIDATE TRACK 4                    
                                                                                
         GOTOR TRNOVUS,DMCB,RQCAUA1 BASED ON COMMERCIAL TYPE                    
         GOTOR TRNOVUS,DMCB,RQCAUP1 TRANSLATE OVERSCALE USES                    
         GOTOR TRNOVUS,DMCB,RQCA2U1                                             
                                                                                
         BRAS  RE,CLCEXP           CALCULATE EXPIRATION DATE                    
                                                                                
         USING TLCAPD,R3                                                        
         OC    RQCASEQ,RQCASEQ     IF CAST SEQUENCE NUMBER IS PROVIDED          
         JZ    CAUP40                                                           
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCACCDQ    READ FOR CAST KEY                            
         MVC   TLCACSSN,RQCASSN                                                 
         MVC   TLCACCOM,RQCACOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CAUP30                                                           
CAUP20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CAUP30   CLC   IOKEY(TLCACCAT-TLCAPD),IOKEYSAV                                  
         JNE   CAUP40                                                           
         CLC   TLCACSEQ,RQCASEQ                                                 
         JNE   CAUP20                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITCHA          IF FOUND, PREPARE TO CHANGE                  
         BRAS  RE,VALWID           AND VALIDATE WEB APPLICATION ID              
         JE    CAUP50                                                           
         J     CAUP60                                                           
                                                                                
CAUP40   BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         JNE   CAUP60                                                           
                                                                                
CAUP50   BRAS  RE,VALATCLK         VALIDATE ATTACHED CORP LOCKED STATUS         
         BRAS  RE,VALGUA           VALIDATE GUARANTEE CODE                      
         BRAS  RE,VALTRN           VALIDATE TRANSFERRED FIELDS                  
                                                                                
CAUP60   MVI   OUTPUT,CASTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    CAUP70                                                           
         MVI   OUTPUT,CASTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    CAUP70                                                           
         MVI   OUTPUT,CASTOK2                                                   
                                                                                
CAUP70   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCAMOD,RQCAEXE     IF MODE IS EXECUTE                           
         JNE   CAUP80                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    CAUP80                                                           
         BRAS  RE,BLDSRTK          BUILD SORT KEY                               
         BRAS  RE,EXECADD          THEN ADD                                     
         BRAS  RE,EXECCHA          OR CHANGE CAST RECORD                        
         JNE   CAUP80                                                           
         GOTOR (#UPDPTRS,AUPDPTRS) AND UPDATE PASSIVE POINTERS                  
                                                                                
         BRAS  RE,UPDCOM           UPDATE COMMERCIAL                            
         BRAS  RE,ADDRLL           ADD RELEASE LETTER REQUEST                   
         BRAS  RE,ADDWTR           AND ADD WEB TRANSACTION RECORD               
         BRAS  RE,UPDTRN           UPDATE/ADD TRANSFERRED FROM RECORDS          
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCASTF),(L'RQCASTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
CAUP80   GOTO1 VHEXOUT,DMCB,RQCACOM,OUTPUT,L'RQCACOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         OC    RQCASEQ,RQCASEQ                                                  
         JZ    CAUP90                                                           
         GOTO1 VHEXOUT,DMCB,RQCASEQ,OUTPUT,L'RQCASEQ,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',9),            +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
CAUP90   OC    RQCARID,RQCARID                                                  
         JZ    CAUP100                                                          
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',10),              +        
               ('LD_CHARQ',RQCARID),(L'RQCARID,0)                               
CAUP100  MVI   OUTPUT,C'N'                                                      
         TM    CASTSTAT,CARECUPD                                                
         JZ    CAUP110                                                          
         MVI   OUTPUT,C'Y'                                                      
CAUP110  GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',11),              +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#CAERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    YES                 SEND DOWN CAST RECORD                        
         CLI   ACTION,ACTADD                                                    
         JE    YES                                                              
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#CAULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#CASDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASSTF),        +        
               ('LD_CHARQ',RQCASTF),(L'RQCASTF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASCOM),        +        
               ('LD_CHARQ',RQCACOM),(L'RQCACOM,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASSEQ),        +        
               ('LD_CHARQ',RQCASEQ),(L'RQCASEQ,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASSSN),        +        
               ('LD_CHARQ',RQCASSN),(L'RQCASSN,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CAUP                                       *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAINT1 DC    AL1(ECAINT1X-*),AL2(62),AL1(ERRCATY1),AL1(D#CATR1)               
         DC    C'Invalid Track for Commercial'                                  
ECAINT1X EQU   *                                                                
                                                                                
ERCAINT2 DC    AL1(ECAINT2X-*),AL2(64),AL1(ERRCATY1),AL1(D#CATR2)               
         DC    C'Invalid Track for Commercial'                                  
ECAINT2X EQU   *                                                                
                                                                                
ERCAINT3 DC    AL1(ECAINT3X-*),AL2(66),AL1(ERRCATY1),AL1(D#CATR3)               
         DC    C'Invalid Track for Commercial'                                  
ECAINT3X EQU   *                                                                
                                                                                
ERCAINT4 DC    AL1(ECAINT4X-*),AL2(68),AL1(ERRCATY1),AL1(D#CATR4)               
         DC    C'Invalid Track for Commercial'                                  
ECAINT4X EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
EROV501  DC    AL2(501),AL2(EROV501X-EROV501)                                   
         DC    AL2(3,8,12,13,15,17,19,20,21,27,29,34,35,36,37,38,39)            
         DC    AL2(40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56)          
         DC    AL2(57,58,59,60,61,70,71,72,73,82,94,95,96,97,98,99,100)         
         DC    AL2(101,102,103,127)                                             
EROV501X EQU   *                                                                
                                                                                
EROV502  DC    AL2(502),AL2(EROV502X-EROV502)                                   
         DC    AL2(3,8,12,13,15,17,19,20,21,23,24,25,27,29,34,35,36,37)         
         DC    AL2(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54)          
         DC    AL2(55,56,57,58,59,60,61,70,71,82,88,94,95,96,97,98,99)          
         DC    AL2(100,101,102,103,127)                                         
EROV502X EQU   *                                                                
                                                                                
EROV503  DC    AL2(503),AL2(EROV503X-EROV503)                                   
         DC    AL2(3,8,12,13,15,17,19,20,21,27,29,34,35,36,37,38,39)            
         DC    AL2(40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56)          
         DC    AL2(57,58,59,60,61,70,71,72,73,82,94,95,96,97,98,99,100)         
         DC    AL2(101,102,103,127)                                             
EROV503X EQU   *                                                                
                                                                                
EROV504  DC    AL2(504),AL2(EROV504X-EROV504)                                   
         DC    AL2(3,8,12,13,15,17,19,20,21,23,24,25,27,29,34,35,36,37)         
         DC    AL2(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54)          
         DC    AL2(55,56,57,58,59,60,61,70,71,82,88,94,95,96,97,98,99)          
         DC    AL2(100,101,102,103,127)                                         
EROV504X EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
CASTOK1  EQU   1                   NO ERRORS - CAST ADDED                       
CASTOK2  EQU   2                   NO ERRORS - CAST CHANGED                     
CASTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#CAMOD                                                    
         CLI   RQCAMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CASTF                                                    
         OC    RQCASTF,RQCASTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CACOM                                                    
         OC    RQCACOM,RQCACOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
         CLI   SVCOTYPE,CTYMUS     IF COMMERCIAL TYPE IS MUSIC                  
         JNE   AR50                                                             
         MVI   BYTE1,D#CATR1       ASSERT THAT TRACK 1 IS PROVIDED              
         CLI   RQCATR1,0                                                        
         JE    ARMIS                                                            
                                                                                
         CLI   RQCATR1,C'*'        IF ON ALL TRACKS                             
         JNE   AR00                                                             
         CLI   RQCATR2,0           ASSERT THAT TRACK 2                          
         JNE   ATR2NAL                                                          
         CLI   RQCATR3,0           AND TRACK 3                                  
         JNE   ATR3NAL                                                          
         CLI   RQCATR4,0           AND TRACK 4                                  
         JNE   ATR4NAL                                                          
         CLI   RQCATR5,0           AND TRACK 5                                  
         JNE   ATR5NAL                                                          
         CLI   RQCATR6,0           AND TRACK 6                                  
         JNE   ATR6NAL                                                          
         CLI   RQCATR7,0           AND TRACK 7 ARE NOT PROVIDED                 
         JNE   ATR7NAL                                                          
                                                                                
AR00     CLI   RQCATR2,0           IF TRACK 2 IS NOT PROVIDED                   
         JNE   AR10                                                             
         CLI   RQCATR3,0           ASSERT THAT TRACK 3                          
         JNE   ATR3NAL                                                          
         CLI   RQCATR4,0           AND TRACK 4                                  
         JNE   ATR4NAL                                                          
         CLI   RQCATR5,0           AND TRACK 5                                  
         JNE   ATR5NAL                                                          
         CLI   RQCATR6,0           AND TRACK 6                                  
         JNE   ATR6NAL                                                          
         CLI   RQCATR7,0           AND TRACK 7 ARE NOT PROVIDED                 
         JNE   ATR7NAL                                                          
                                                                                
AR10     CLI   RQCATR3,0           IF TRACK 3 IS NOT PROVIDED                   
         JNE   AR20                                                             
         CLI   RQCATR4,0           ASSERT THAT TRACK 4                          
         JNE   ATR4NAL                                                          
         CLI   RQCATR5,0           AND TRACK 5                                  
         JNE   ATR5NAL                                                          
         CLI   RQCATR6,0           AND TRACK 6                                  
         JNE   ATR6NAL                                                          
         CLI   RQCATR7,0           AND TRACK 7 ARE NOT PROVIDED                 
         JNE   ATR7NAL                                                          
                                                                                
AR20     CLI   RQCATR4,0           IF TRACK 4 IS NOT PROVIDED                   
         JNE   AR30                                                             
         CLI   RQCATR5,0           ASSERT THAT TRACK 5                          
         JNE   ATR5NAL                                                          
         CLI   RQCATR6,0           AND TRACK 6                                  
         JNE   ATR6NAL                                                          
         CLI   RQCATR7,0           AND TRACK 7 ARE NOT PROVIDED                 
         JNE   ATR7NAL                                                          
                                                                                
AR30     CLI   RQCATR5,0           IF TRACK 5 IS NOT PROVIDED                   
         JNE   AR40                                                             
         CLI   RQCATR6,0           ASSERT THAT TRACK 6                          
         JNE   ATR6NAL                                                          
         CLI   RQCATR7,0           AND TRACK 7 ARE NOT PROVIDED                 
         JNE   ATR7NAL                                                          
                                                                                
AR40     CLI   RQCATR6,0           IF TRACK 6 IS NOT PROVIDED                   
         JNE   AR60                                                             
         CLI   RQCATR7,0           ASSERT THAT TRACK 7 IS NOT PROVIDED          
         JNE   ATR7NAL                                                          
         J     AR60                                                             
                                                                                
AR50     MVI   BYTE1,D#CATR1                                                    
         CLI   RQCATR1,0           IF COMMERCIAL TYPE IS NOT MUSIC              
         JNE   ARNAL               ASSERT THAT TRACKS ARE NOT PROVIDED          
         MVI   BYTE1,D#CATR2                                                    
         CLI   RQCATR2,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CATR3                                                    
         CLI   RQCATR3,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CATR4                                                    
         CLI   RQCATR4,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CATR5                                                    
         CLI   RQCATR5,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CATR6                                                    
         CLI   RQCATR6,0                                                        
         JNE   ARNAL                                                            
         MVI   BYTE1,D#CATR7                                                    
         CLI   RQCATR7,0                                                        
         JNE   ARNAL                                                            
                                                                                
AR60     OC    RQCARID,RQCARID     IF REQUEST ID IS NOT PROVIDED                
         JNZ   AR70                                                             
         MVI   BYTE1,D#CASEQ       ASSERT THAT CAST SEQUENCE NUMBER             
         OC    RQCASEQ,RQCASEQ     IS PROVIDED                                  
         JZ    ARMIS                                                            
                                                                                
AR70     MVI   BYTE1,D#CASSN                                                    
         OC    RQCASSN,RQCASSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JZ    ARMIS               NUMBER IS PROVIDED                           
         MVI   BYTE1,D#CACAT                                                    
         OC    RQCACAT,RQCACAT     ASSERT THAT CATEGORY IS PROVIDED             
         JZ    ARMIS                                                            
         MVI   BYTE1,D#CAONO                                                    
         OC    RQCAONO,RQCAONO     ASSERT THAT ON/OFF CAMERA IS                 
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#CATAX                                                    
         OC    RQCATAX,RQCATAX     ASSERT THAT TAX UNIT CODE IS                 
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#CAUNI                                                    
         OC    RQCAUNI,RQCAUNI     ASSERT THAT UNION IS PROVIDED                
         JZ    ARMIS                                                            
         MVI   BYTE1,D#CALCL                                                    
         OC    RQCALCL,RQCALCL     ASSERT THAT LOCAL IS PROVIDED                
         JZ    ARMIS                                                            
         MVI   BYTE1,D#CACYR                                                    
         OC    RQCACYR,RQCACYR     ASSERT THAT CONTRACT YEAR IS                 
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#CAWID                                                    
         OC    RQCAWID,RQCAWID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
         CLI   SVCOTYPE,CTYSEAS2   IF COMMERCIAL TYPE IS SEASONAL               
         JNE   AR80                                                             
         MVI   BYTE1,D#CAFFC                                                    
         OC    RQCAFFC,RQCAFFC     FIRST FIXED CYCLE AND                        
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#CAEXP                                                    
         OC    RQCAEXP,RQCAEXP     EXPIRATION DATE ARE NOT ALLOWED              
         JNZ   ARNAL                                                            
                                                                                
AR80     OC    RQCALSV,RQCALSV     IF LAST SERVICES DATE IS NOT                 
         JNZ   AR90                PROVIDED                                     
         CLI   RQCARLL,0           ASSERT THAT RELEASE LETTER CODE              
         JE    AR90                IS NOT PROVIDED                              
         MVI   BYTE1,D#CARLL                                                    
         J     ARNAL                                                            
                                                                                
AR90     CLI   RQCARLL,0           IF RELEASE LETTER CODE IS PROVIDED           
         JE    AR100                                                            
         OC    RQCAEFF,RQCAEFF     ASSERT THAT EFFECTIVE DATE IS                
         JNZ   AR100               PROVIDED                                     
         MVI   BYTE1,D#CAEFF                                                    
         J     ARMIS                                                            
                                                                                
AR100    OC    RQCAUP1,RQCAUP1     IF OVERSCALE PERCENTAGE USE 1                
         JNZ   AR110               IS NOT PROVIDED                              
         OC    RQCAOP1,RQCAOP1     ASSERT THAT OVERSCALE PERCENTAGE 1           
         JNZ   AOP1NAL             IS NOT PROVIDED                              
         OC    RQCAUP2,RQCAUP2     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP2NAL             USE 2 IS NOT PROVIDED                        
         OC    RQCAUP3,RQCAUP3     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP3NAL             USE 3 IS NOT PROVIDED                        
         OC    RQCAUP4,RQCAUP4     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP4NAL             USE 4 IS NOT PROVIDED                        
         OC    RQCAUP5,RQCAUP5     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP5NAL             USE 5 IS NOT PROVIDED                        
         OC    RQCAUP6,RQCAUP6     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP6NAL             USE 6 IS NOT PROVIDED                        
                                                                                
AR110    OC    RQCAUP2,RQCAUP2     IF OVERSCALE PERCENTAGE USE 2                
         JNZ   AR120               IS NOT PROVIDED                              
         OC    RQCAOP2,RQCAOP2     ASSERT THAT OVERSCALE PERCENTAGE 2           
         JNZ   AOP2NAL             IS NOT PROVIDED                              
         OC    RQCAUP3,RQCAUP3     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP3NAL             USE 3 IS NOT PROVIDED                        
         OC    RQCAUP4,RQCAUP4     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP4NAL             USE 4 IS NOT PROVIDED                        
         OC    RQCAUP5,RQCAUP5     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP5NAL             USE 5 IS NOT PROVIDED                        
         OC    RQCAUP6,RQCAUP6     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP6NAL             USE 6 IS NOT PROVIDED                        
                                                                                
AR120    OC    RQCAUP3,RQCAUP3     IF OVERSCALE PERCENTAGE USE 3                
         JNZ   AR130               IS NOT PROVIDED                              
         OC    RQCAOP3,RQCAOP3     ASSERT THAT OVERSCALE PERCENTAGE 3           
         JNZ   AOP3NAL             IS NOT PROVIDED                              
         OC    RQCAUP4,RQCAUP4     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP4NAL             USE 4 IS NOT PROVIDED                        
         OC    RQCAUP5,RQCAUP5     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP5NAL             USE 5 IS NOT PROVIDED                        
         OC    RQCAUP6,RQCAUP6     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP6NAL             USE 6 IS NOT PROVIDED                        
                                                                                
AR130    OC    RQCAUP4,RQCAUP4     IF OVERSCALE PERCENTAGE USE 4                
         JNZ   AR140               IS NOT PROVIDED                              
         OC    RQCAOP4,RQCAOP4     ASSERT THAT OVERSCALE PERCENTAGE 4           
         JNZ   AOP4NAL             IS NOT PROVIDED                              
         OC    RQCAUP5,RQCAUP5     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP5NAL             USE 5 IS NOT PROVIDED                        
         OC    RQCAUP6,RQCAUP6     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP6NAL             USE 6 IS NOT PROVIDED                        
                                                                                
AR140    OC    RQCAUP5,RQCAUP5     IF OVERSCALE PERCENTAGE USE 5                
         JNZ   AR150               IS NOT PROVIDED                              
         OC    RQCAOP5,RQCAOP5     ASSERT THAT OVERSCALE PERCENTAGE 5           
         JNZ   AOP5NAL             IS NOT PROVIDED                              
         OC    RQCAUP6,RQCAUP6     ASSERT THAT OVERSCALE PERCENTAGE             
         JNZ   AUP6NAL             USE 6 IS NOT PROVIDED                        
                                                                                
AR150    OC    RQCAUP6,RQCAUP6     IF OVERSCALE PERCENTAGE USE 6                
         JNZ   AR160               IS NOT PROVIDED                              
         OC    RQCAOP6,RQCAOP6     ASSERT THAT OVERSCALE PERCENTAGE 6           
         JNZ   AOP6NAL             IS NOT PROVIDED                              
                                                                                
AR160    OC    RQCAUA1,RQCAUA1     IF OVERSCALE AMOUNT USE 1                    
         JNZ   AR170               IS NOT PROVIDED                              
         OC    RQCAOA1,RQCAOA1     ASSERT THAT OVERSCALE AMOUNT 1               
         JNZ   AOA1NAL             IS NOT PROVIDED                              
         OC    RQCAUA2,RQCAUA2     ASSERT THAT OVERSCALE AMOUNT USE 2           
         JNZ   AUA2NAL             IS NOT PROVIDED                              
         OC    RQCAUA3,RQCAUA3     ASSERT THAT OVERSCALE AMOUNT USE 3           
         JNZ   AUA3NAL             IS NOT PROVIDED                              
         OC    RQCAUA4,RQCAUA4     ASSERT THAT OVERSCALE AMOUNT USE 4           
         JNZ   AUA4NAL             IS NOT PROVIDED                              
         OC    RQCAUA5,RQCAUA5     ASSERT THAT OVERSCALE AMOUNT USE 5           
         JNZ   AUA5NAL             IS NOT PROVIDED                              
         OC    RQCAUA6,RQCAUA6     ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNZ   AUA6NAL             IS NOT PROVIDED                              
                                                                                
AR170    OC    RQCAUA2,RQCAUA2     IF OVERSCALE AMOUNT USE 2                    
         JNZ   AR180               IS NOT PROVIDED                              
         OC    RQCAOA2,RQCAOA2     ASSERT THAT OVERSCALE AMOUNT 2               
         JNZ   AOA2NAL             IS NOT PROVIDED                              
         OC    RQCAUA3,RQCAUA3     ASSERT THAT OVERSCALE AMOUNT USE 3           
         JNZ   AUA3NAL             IS NOT PROVIDED                              
         OC    RQCAUA4,RQCAUA4     ASSERT THAT OVERSCALE AMOUNT USE 4           
         JNZ   AUA4NAL             IS NOT PROVIDED                              
         OC    RQCAUA5,RQCAUA5     ASSERT THAT OVERSCALE AMOUNT USE 5           
         JNZ   AUA5NAL             IS NOT PROVIDED                              
         OC    RQCAUA6,RQCAUA6     ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNZ   AUA6NAL             IS NOT PROVIDED                              
                                                                                
AR180    OC    RQCAUA3,RQCAUA3     IF OVERSCALE AMOUNT USE 3                    
         JNZ   AR190               IS NOT PROVIDED                              
         OC    RQCAOA3,RQCAOA3     ASSERT THAT OVERSCALE AMOUNT 3               
         JNZ   AOA3NAL             IS NOT PROVIDED                              
         OC    RQCAUA4,RQCAUA4     ASSERT THAT OVERSCALE AMOUNT USE 4           
         JNZ   AUA4NAL             IS NOT PROVIDED                              
         OC    RQCAUA5,RQCAUA5     ASSERT THAT OVERSCALE AMOUNT USE 5           
         JNZ   AUA5NAL             IS NOT PROVIDED                              
         OC    RQCAUA6,RQCAUA6     ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNZ   AUA6NAL             IS NOT PROVIDED                              
                                                                                
AR190    OC    RQCAUA4,RQCAUA4     IF OVERSCALE AMOUNT USE 4                    
         JNZ   AR200               IS NOT PROVIDED                              
         OC    RQCAOA4,RQCAOA4     ASSERT THAT OVERSCALE AMOUNT 4               
         JNZ   AOA4NAL             IS NOT PROVIDED                              
         OC    RQCAUA5,RQCAUA5     ASSERT THAT OVERSCALE AMOUNT USE 5           
         JNZ   AUA5NAL             IS NOT PROVIDED                              
         OC    RQCAUA6,RQCAUA6     ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNZ   AUA6NAL             IS NOT PROVIDED                              
                                                                                
AR200    OC    RQCAUA5,RQCAUA5     IF OVERSCALE AMOUNT USE 5                    
         JNZ   AR210               IS NOT PROVIDED                              
         OC    RQCAOA5,RQCAOA5     ASSERT THAT OVERSCALE AMOUNT 5               
         JNZ   AOA5NAL             IS NOT PROVIDED                              
         OC    RQCAUA6,RQCAUA6     ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNZ   AUA6NAL             IS NOT PROVIDED                              
                                                                                
AR210    OC    RQCAUA6,RQCAUA6     IF OVERSCALE AMOUNT USE 6                    
         JNZ   AR220               IS NOT PROVIDED                              
         OC    RQCAOA6,RQCAOA6     ASSERT THAT OVERSCALE AMOUNT 6               
         JNZ   AOA6NAL             IS NOT PROVIDED                              
                                                                                
AR220    CLI   RQCAMST,0           IF "ON MASTER COMMERCIAL?" IS                
         JE    AR230               PROVIDED                                     
         MVI   BYTE1,D#CAVER                                                    
         OC    VERSIONS,VERSIONS   ASSERT THAT VERSIONS ARE NOT                 
         JNZ   ARNAL               PROVIDED                                     
         CLI   RQCALFT,0           ASSERT THAT "ON LIFT VERSION?" IS            
         JNE   AR240               PROVIDED                                     
         MVI   BYTE1,D#CALFT                                                    
         J     ARMIS                                                            
AR230    CLI   RQCALFT,0           OTHERWISE ASSERT THAT "ON LIFT               
         JE    AR240               VERSION?" IS NOT PROVIDED                    
         MVI   BYTE1,D#CALFT                                                    
         J     ARNAL                                                            
                                                                                
AR240    OC    RQCA2U1,RQCA2U1     IF 2ND OVERSCALE PERCENTAGE USE 1            
         JZ    AR250               IS PROVIDED                                  
         OC    RQCA2OP,RQCA2OP     ASSERT THAT 2ND OVERSCALE PERCENTAGE         
         JZ    AR260               IS NOT PROVIDED                              
         MVI   BYTE1,D#CA2OP                                                    
         J     ARNAL                                                            
AR250    OC    RQCA2P1,RQCA2P1     ELSE ASSERT THAT 2ND OVERSCALE               
         JNZ   A2P1NAL             PCT 1 IS NOT PROVIDED                        
         OC    RQCA2U2,RQCA2U2     ASSERT THAT 2ND OVERSCALE PCT 2              
         JNZ   A2P2NAL             IS NOT PROVIDED                              
         OC    RQCA2U3,RQCA2U3     ASSERT THAT 2ND OVERSCALE PCT 3              
         JNZ   A2P3NAL             IS NOT PROVIDED                              
         OC    RQCA2U4,RQCA2U4     ASSERT THAT 2ND OVERSCALE PCT 4              
         JNZ   A2P4NAL             IS NOT PROVIDED                              
         OC    RQCA2U5,RQCA2U5     ASSERT THAT 2ND OVERSCALE PCT 5              
         JNZ   A2P5NAL             IS NOT PROVIDED                              
                                                                                
AR260    OC    RQCA2U2,RQCA2U2     IF 2ND OVERSCALE PERCENTAGE USE 2            
         JNZ   AR270               IS NOT PROVIDED                              
         OC    RQCA2P2,RQCA2P2     ASSERT THAT 2ND OVERSCALE PCT 2              
         JNZ   A2P2NAL             IS NOT PROVIDED                              
         OC    RQCA2U3,RQCA2U3     ASSERT THAT 2ND OVERSCALE PCT 3              
         JNZ   A2P3NAL             IS NOT PROVIDED                              
         OC    RQCA2U4,RQCA2U4     ASSERT THAT 2ND OVERSCALE PCT 4              
         JNZ   A2P4NAL             IS NOT PROVIDED                              
         OC    RQCA2U5,RQCA2U5     ASSERT THAT 2ND OVERSCALE PCT 5              
         JNZ   A2P5NAL             IS NOT PROVIDED                              
                                                                                
AR270    OC    RQCA2U3,RQCA2U3     IF 2ND OVERSCALE PERCENTAGE USE 3            
         JNZ   AR280               IS NOT PROVIDED                              
         OC    RQCA2P3,RQCA2P3     ASSERT THAT 2ND OVERSCALE PCT 3              
         JNZ   A2P3NAL             IS NOT PROVIDED                              
         OC    RQCA2U4,RQCA2U4     ASSERT THAT 2ND OVERSCALE PCT 4              
         JNZ   A2P4NAL             IS NOT PROVIDED                              
         OC    RQCA2U5,RQCA2U5     ASSERT THAT 2ND OVERSCALE PCT 5              
         JNZ   A2P5NAL             IS NOT PROVIDED                              
                                                                                
AR280    OC    RQCA2U4,RQCA2U4     IF 2ND OVERSCALE PERCENTAGE USE 4            
         JNZ   AR290               IS NOT PROVIDED                              
         OC    RQCA2P4,RQCA2P4     ASSERT THAT 2ND OVERSCALE PCT 4              
         JNZ   A2P4NAL             IS NOT PROVIDED                              
         OC    RQCA2U5,RQCA2U5     ASSERT THAT 2ND OVERSCALE PCT 5              
         JNZ   A2P5NAL             IS NOT PROVIDED                              
                                                                                
AR290    OC    RQCA2U5,RQCA2U5     IF 2ND OVERSCALE PERCENTAGE USE 5            
         JNZ   AR300               IS NOT PROVIDED                              
         OC    RQCA2U5,RQCA2U5     ASSERT THAT 2ND OVERSCALE PCT 5              
         JNZ   A2P5NAL             IS NOT PROVIDED                              
                                                                                
AR300    OC    RQCATCO,RQCATCO     IF TRANSFERRED FROM INTERNAL COMM'L          
         JZ    AR310               NUMBER IS PROVIDED                           
         OC    RQCATSQ,RQCATSQ     ASSERT THAT TRANSFERRED FROM CAST            
         JNZ   YES                 SEQUENCE NUMBER IS PROVIDED                  
         MVI   BYTE1,D#CATSQ                                                    
         J     ARMIS                                                            
                                                                                
AR310    OC    RQCATSQ,RQCATSQ     IF TRANSFERRED FROM INTERNAL COMM'L          
         JZ    YES                 NUMBER IS NOT PROVIDED, ASSERT TRAN          
         MVI   BYTE1,D#CATSQ       FROM CAST SEQ NUM IS NOT PROVIDED            
         J     ARNAL                                                            
                                                                                
AOP1NAL  MVI   BYTE1,D#CAOP1       OVERSCALE PERCENTAGE 1 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AOP2NAL  MVI   BYTE1,D#CAOP2       OVERSCALE PERCENTAGE 2 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AUP2NAL  MVI   BYTE1,D#CAUP2       OVERSCALE PERCENTAGE USE 2 IS                
         J     ARNAL               NOT ALLOWED                                  
                                                                                
AOP3NAL  MVI   BYTE1,D#CAOP3       OVERSCALE PERCENTAGE 3 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AUP3NAL  MVI   BYTE1,D#CAUP3       OVERSCALE PERCENTAGE USE 3 IS                
         J     ARNAL               NOT ALLOWED                                  
                                                                                
AOP4NAL  MVI   BYTE1,D#CAOP4       OVERSCALE PERCENTAGE 4 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AUP4NAL  MVI   BYTE1,D#CAUP4       OVERSCALE PERCENTAGE USE 4 IS                
         J     ARNAL               NOT ALLOWED                                  
                                                                                
AOP5NAL  MVI   BYTE1,D#CAOP5       OVERSCALE PERCENTAGE 5 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AUP5NAL  MVI   BYTE1,D#CAUP5       OVERSCALE PERCENTAGE USE 5 IS                
         J     ARNAL               NOT ALLOWED                                  
                                                                                
AOP6NAL  MVI   BYTE1,D#CAOP6       OVERSCALE PERCENTAGE 6 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AUP6NAL  MVI   BYTE1,D#CAUP6       OVERSCALE PERCENTAGE USE 6 IS                
         J     ARNAL               NOT ALLOWED                                  
                                                                                
AOA1NAL  MVI   BYTE1,D#CAOA1       OVERSCALE AMOUNT 1 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AOA2NAL  MVI   BYTE1,D#CAOA2       OVERSCALE AMOUNT 2 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AUA2NAL  MVI   BYTE1,D#CAUA2       OVERSCALE AMOUNT USE 2 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AOA3NAL  MVI   BYTE1,D#CAOA3       OVERSCALE AMOUNT 3 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AUA3NAL  MVI   BYTE1,D#CAUA3       OVERSCALE AMOUNT USE 3 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AOA4NAL  MVI   BYTE1,D#CAOA4       OVERSCALE AMOUNT 4 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AUA4NAL  MVI   BYTE1,D#CAUA4       OVERSCALE AMOUNT USE 4 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AOA5NAL  MVI   BYTE1,D#CAOA5       OVERSCALE AMOUNT 5 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AUA5NAL  MVI   BYTE1,D#CAUA5       OVERSCALE AMOUNT USE 5 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
AOA6NAL  MVI   BYTE1,D#CAOA6       OVERSCALE AMOUNT 6 IS NOT ALLOWED            
         J     ARNAL                                                            
                                                                                
AUA6NAL  MVI   BYTE1,D#CAUA6       OVERSCALE AMOUNT USE 6 IS NOT                
         J     ARNAL               ALLOWED                                      
                                                                                
A2P1NAL  MVI   BYTE1,D#CA2P1       SECOND OVERSCALE PERCENTAGE 1 IS             
         J     ARNAL               NOT ALLOWED                                  
                                                                                
A2U1NAL  MVI   BYTE1,D#CA2U1       SECOND OVERSCALE PERCENTAGE USE 1            
         J     ARNAL               IS NOT ALLOWED                               
                                                                                
A2P2NAL  MVI   BYTE1,D#CA2P2       SECOND OVERSCALE PERCENTAGE 2 IS             
         J     ARNAL               NOT ALLOWED                                  
                                                                                
A2U2NAL  MVI   BYTE1,D#CA2U2       SECOND OVERSCALE PERCENTAGE USE 2            
         J     ARNAL               IS NOT ALLOWED                               
                                                                                
A2P3NAL  MVI   BYTE1,D#CA2P3       SECOND OVERSCALE PERCENTAGE 3 IS             
         J     ARNAL               NOT ALLOWED                                  
                                                                                
A2U3NAL  MVI   BYTE1,D#CA2U3       SECOND OVERSCALE PERCENTAGE USE 3            
         J     ARNAL               IS NOT ALLOWED                               
                                                                                
A2P4NAL  MVI   BYTE1,D#CA2P4       SECOND OVERSCALE PERCENTAGE 4 IS             
         J     ARNAL               NOT ALLOWED                                  
                                                                                
A2U4NAL  MVI   BYTE1,D#CA2U4       SECOND OVERSCALE PERCENTAGE USE 4            
         J     ARNAL               IS NOT ALLOWED                               
                                                                                
A2P5NAL  MVI   BYTE1,D#CA2P5       SECOND OVERSCALE PERCENTAGE 5 IS             
         J     ARNAL               NOT ALLOWED                                  
                                                                                
A2U5NAL  MVI   BYTE1,D#CA2U5       SECOND OVERSCALE PERCENTAGE USE 5            
         J     ARNAL               IS NOT ALLOWED                               
                                                                                
ATR2NAL  MVI   BYTE1,D#CATR2       TRACK 2 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
ATR3NAL  MVI   BYTE1,D#CATR3       TRACK 3 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
ATR4NAL  MVI   BYTE1,D#CATR4       TRACK 4 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
ATR5NAL  MVI   BYTE1,D#CATR5       TRACK 5 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
ATR6NAL  MVI   BYTE1,D#CATR6       TRACK 6 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
ATR7NAL  MVI   BYTE1,D#CATR7       TRACK 7 IS NOT ALLOWED                       
         J     ARNAL                                                            
                                                                                
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
         MVI   BYTE1,D#CAMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCAMOD)                        
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVCATCAM         VALIDATE CATEGORY/CAMERA                     
         JNE   NO                                                               
         MVC   ACATENT,AENTRY                                                   
                                                                                
         USING TALUNITD,RE                                                      
         MVI   BYTE1,D#CATAX       VALIDATE TAX UNIT                            
         CLC   RQCATAX,=C'OT '                                                  
         JE    AV10                                                             
         CLC   RQCATAX,=C'LAX'                                                  
         JE    AVINV                                                            
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSOP',RQCATAX)                         
         JE    AV10                                                             
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCITY',RQCATAX)                        
         JNE   AVINV                                                            
         L     RE,AENTRY                                                        
         TM    TALUSTAT,TASUINGF                                                
         JO    AVINV                                                            
         DROP  RE                                                               
                                                                                
AV10     BAS   RE,AVCATUNI         VALIDATE CATEGORY/UNION                      
         JNE   XIT                                                              
         MVC   AUNIENT,AENTRY                                                   
                                                                                
         BAS   RE,AVLCL                                                         
         JNE   XIT                                                              
                                                                                
         BAS   RE,AVUNIYR          VALIDATE UNION/YEAR                          
         JNE   XIT                                                              
         MVC   AYEARENT,AENTRY                                                  
                                                                                
         BAS   RE,AVRLL            VALIDATE RELEASE LETTER CODE                 
         JNE   XIT                                                              
                                                                                
         BAS   RE,AVOP             VALIDATE OVERSCALE PERCENTAGES               
         JNE   XIT                                                              
                                                                                
         USING CATTABD,RE                                                       
         CLI   RQCADBL,C' '        IF DOUBLES ARE PROVIDED                      
         JE    AV20                                                             
         MVI   BYTE1,D#CADBL       ASSERT THAT CATEGORY ALLOWS THEM             
         L     RE,ACATENT                                                       
         TM    CATSTAT,OKDOUBLE                                                 
         JZ    AVINV                                                            
         DROP  RE                                                               
                                                                                
AV20     BAS   RE,AVOA             VALIDATE OVERSCALE AMOUNTS                   
         JNE   XIT                                                              
         BAS   RE,AV2OP            VALIDATE SECOND OVERSCALE PCTS               
         JNE   XIT                                                              
         BRAS  RE,AVTRK            VALIDATE TRACKS                              
         JNE   XIT                                                              
                                                                                
         MVI   BYTE1,D#CAMST       VALIDATE ON MASTER COMMERCIAL?               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAMST)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CALFT       VALIDATE ON LIFT VERSION?                    
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCALFT)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CACPA       VALIDATE CHECKS PAYABLE TO AGENT?            
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCACPA)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAPCR       VALIDATE PAY CAN RATES?                      
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAPCR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAPUR       VALIDATE PAY US RATES?                       
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAPUR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAFGN       VALIDATE FOREIGN USE?                        
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAFGN)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAINA       VALIDATE INTERACTIVE USE?                    
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAINA)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAINR       VALIDATE INDUSTRIAL USE?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAINR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAACP       VALIDATE APPLY CBL TO PER CYCLE?             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAACP)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAAPP       VALIDATE APPLY PAX TO PER CYCLE?             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAAPP)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAPDT       VALIDATE PRINT DAILY FTRACKS?                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAPDT)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CACLB       VALIDATE CELEBRITY?                          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCACLB)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAGRR       VALIDATE GRR COVERS ALL USE?                 
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAGRR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAEUR       VALIDATE PAY IN EUROS?                       
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAEUR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAAFT       VALIDATE APPLY USE TO FTRACK?                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAAFT)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAEQY       VALIDATE EQUITY?                             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAEQY)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#CAETM       VALIDATE ENFOCE TYPE/MEDIA VAL?              
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCAETM)                        
         JNE   AVINV                                                            
                                                                                
         BRAS  RE,AVVER            VALIDATE VERSIONS                            
         JNE   AVERINV             DUPLICATED                                   
                                                                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFWID',RQCAWID)                         
         JE    YES                                                              
         MVI   BYTE1,D#CAWID       VALIDATE WEB APPLICATION ID                  
         J     AVINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS CATEGORY AND CAMERA VALUES ARE VALID         *         
*        AND COHESIVE                                                 *         
***********************************************************************         
                                                                                
AVCATCAM NTR1                                                                   
         MVI   BYTE1,D#CACAT       VALIDATE CATEGORY                            
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFCAT',RQCACAT)                         
         JNE   AVINV                                                            
                                                                                
         USING CATTABD,R2                                                       
         L     R2,AENTRY           R2=A(CATEGORY TABLE ENTRY)                   
                                                                                
         MVI   BYTE1,D#CAONO                                                    
         CLC   RQCAONO,=C'ON '     IF ON/OFF CAMERA IS ON                       
         JNE   ACC10                                                            
         TM    CATSTAT,OKON        ASSERT THAT ON CAMERA IS VALID               
         JZ    AVINV               FOR CATEGORY                                 
         CLI   SVCOMED,TACOMEDR    AND ASSERT THAT MEDIA IS NOT                 
         JE    AVINV               RADIO                                        
         J     YES                                                              
                                                                                
ACC10    CLC   RQCAONO,=C'OFF'     IF ON/OFF CAMERA IS OFF                      
         JNE   ACC20                                                            
         TM    CATSTAT,OKOFF       ASSERT THAT OFF CAMERA IS VALID              
         JZ    AVINV               FOR CATEGORY                                 
         J     YES                                                              
         DROP  R2                                                               
                                                                                
ACC20    MVI   BYTE1,D#CAONO       ASSERT THAT ON/OFF CAMERA IS                 
         J     AVINV               EITHER ON OR OFF                             
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS CATEGORY AND UNION VALUES ARE VALID          *         
*        AND COHESIVE                                                 *         
***********************************************************************         
                                                                                
AVCATUNI NTR1                                                                   
         MVI   BYTE1,D#CAUNI                                                    
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFUNI',RQCAUNI)                         
         JNE   AVINV                                                            
                                                                                
         USING CATTABD,R2                                                       
         L     R2,ACATENT                                                       
                                                                                
         USING UNITABD,R3                                                       
         L     R3,AENTRY                                                        
         ZIC   RF,UNIEQU           ASSERT THAT UNION IS VALID                   
         EX    RF,*+8              FOR CATEGORY                                 
         B     *+8                                                              
         TM    CATUNI,0                                                         
         JZ    AVINV                                                            
                                                                                
         CLI   SVCOTYPE,CTYMUS     IF COMMERCIAL TYPE IS MUSIC                  
         JNE   AVCU10                                                           
         TM    CATUNI,AFM          CATEGORY MUST BE VALID FOR MUSICIAN          
         JZ    AVINV               NEW CATEGORY MUST BE MUSICIAN                
         J     YES                                                              
                                                                                
AVCU10   CLI   CATEQU,CTZZ         IF COMMERCIAL TYPE IS NOT MUSIC              
         JE    YES                 CATEGORY MUST BE ZZ                          
         CLI   CATEQU,CTZZZ        OR ZZZ                                       
         JE    YES                                                              
         TM    CATUNI,AFM          OR A NON-MUSICIAN                            
         JO    AVINV                                                            
         J     YES                                                              
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS LOCAL VALUES ARE VALID                       *         
***********************************************************************         
                                                                                
AVLCL    NTR1                                                                   
         MVI   BYTE1,D#CALCL                                                    
         CLC   RQCATAX,=C'LA '     IF TAX UNIT IS LOUISIANA                     
         JNE   AL10                                                             
         CLC   RQCALCL,=C'LA '     LOCAL CANNOT BE LOUISIANA                    
         JE    AVINV                                                            
         CLC   RQCALCL,=C'47 '     OR 47                                        
         JE    AVINV                                                            
                                                                                
AL10     CLC   RQCAUNI,=C'NON'     IF UNION IS NON                              
         JNE   YES                                                              
         CLC   RQCALCL,=C'NON'     AND LOCAL IS NON                             
         JNE   YES                 TAX UNIT CANNOT BE CANADIAN PROV             
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFPROV',RQCATAX)                        
         JE    AVINV                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS UNION AND CONTRACT YEAR VALUES ARE VALID     *         
*        AND COHESIVE                                                 *         
***********************************************************************         
                                                                                
AVUNIYR  NTR1                                                                   
         MVI   BYTE1,D#CACYR       VALIDATE CONTRACT YEAR                       
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFYEAR',RQCACYR)                        
         JNE   AVINV                                                            
                                                                                
         USING YRTABD,R2                                                        
         L     R2,AENTRY           R2=A(CONTRACT YEAR ENTRY)                    
                                                                                
         USING UNITABD,R3                                                       
         L     R3,AUNIENT                                                       
         LA    R3,UNIYR            R3=A(VALID YEARS FOR UNION)                  
         DROP  R3                                                               
                                                                                
AVUY10   CLC   YREQU,0(R3)                                                      
         JE    AVUY20                                                           
         CLI   1(R3),0                                                          
         JE    AVINV                                                            
         LA    R3,1(R3)                                                         
         J     AVUY10                                                           
                                                                                
AVUY20   MVI   BYTE1,D#CACAT                                                    
         CLI   YREQU,CN13          IF CONTRACT YEAR IS 2013 OR GREATER          
         JL    AVUY30                                                           
         CLC   RQCACAT,=C'GD6'     GD6 AND GD9 ARE NO LONGER VALID              
         JE    AVINV                                                            
         CLC   RQCACAT,=C'GD9'                                                  
         JE    AVINV                                                            
         J     YES                                                              
         DROP  R2                                                               
                                                                                
AVUY30   CLC   RQCACAT,=C'GD+'     IF YEAR IS < 2013 GD+ IS NOT VALID           
         JE    AVINV                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS RELEASE LETTER CODE IS VALID                 *         
***********************************************************************         
                                                                                
AVRLL    NTR1                                                                   
         CLI   RQCARLL,0           IF RELEASE LETTER CODE IS PROVIDED           
         JE    YES                                                              
         MVI   BYTE1,D#CARLL                                                    
         CLI   RQCARLL,C'A'        VALID VALUES ARE PRINCIPALS ARE              
         JL    AVINV               A,B,C AND D                                  
         CLI   RQCARLL,C'D'                                                     
         JH    AVINV                                                            
                                                                                
         USING CATTABD,R3                                                       
         CLI   RQCARLL,C'B'        VALID VALUES FOR EXTRAS ARE                  
         JNE   YES                 A,C AND D                                    
         L     R3,ACATENT                                                       
         TM    CATTYPE,EXTRA                                                    
         JO    AVINV                                                            
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS OVERSCALE PERCENTAGE USE AND PERCENTAGES     *         
*        ARE VALID                                                    *         
***********************************************************************         
                                                                                
AVOP     NTR1                                                                   
         L     R2,=F'99999'                                                     
                                                                                
         OC    RQCAUP1,RQCAUP1     VALIDATE OVERSCALE PERCENTAGE USE 1          
         JZ    YES                                                              
         GOTO1 AVOPNDUP,DMCB,RQCAUP1                                            
         JNE   NO                                                               
         GOTOR AVOPVAL,DMCB,RQCAUP1                                             
         JNE   AUP1INV                                                          
         ICM   RE,15,RQCAOP1       VALIDATE OVERSCALE PERCENTAGE 1              
         CR    RE,R2                                                            
         JH    AOP1INV                                                          
                                                                                
         OC    RQCAUP2,RQCAUP2     VALIDATE OVERSCALE PERCENTAGE USE 2          
         JZ    YES                                                              
         GOTO1 AVOPNDUP,DMCB,RQCAUP2                                            
         JNE   NO                                                               
         GOTOR AVOPVAL,DMCB,RQCAUP2                                             
         JNE   AUP2INV                                                          
         ICM   RE,15,RQCAOP2       VALIDATE OVERSCALE PERCENTAGE 2              
         CR    RE,R2                                                            
         JH    AOP2INV                                                          
                                                                                
         OC    RQCAUP3,RQCAUP3     VALIDATE OVERSCALE PERCENTAGE USE 3          
         JZ    YES                                                              
         GOTO1 AVOPNDUP,DMCB,RQCAUP3                                            
         JNE   NO                                                               
         GOTOR AVOPVAL,DMCB,RQCAUP3                                             
         JNE   AUP3INV                                                          
         ICM   RE,15,RQCAOP3       VALIDATE OVERSCALE PERCENTAGE 3              
         CR    RE,R2                                                            
         JH    AOP3INV                                                          
                                                                                
         OC    RQCAUP4,RQCAUP4     VALIDATE OVERSCALE PERCENTAGE USE 4          
         JZ    YES                                                              
         GOTO1 AVOPNDUP,DMCB,RQCAUP4                                            
         JNE   NO                                                               
         GOTOR AVOPVAL,DMCB,RQCAUP4                                             
         JNE   AUP4INV                                                          
         ICM   RE,15,RQCAOP4       VALIDATE OVERSCALE PERCENTAGE 4              
         CR    RE,R2                                                            
         JH    AOP4INV                                                          
                                                                                
         OC    RQCAUP5,RQCAUP5     VALIDATE OVERSCALE PERCENTAGE USE 5          
         JZ    YES                                                              
         GOTO1 AVOPNDUP,DMCB,RQCAUP5                                            
         JNE   NO                                                               
         GOTOR AVOPVAL,DMCB,RQCAUP5                                             
         JNE   AUP5INV                                                          
         ICM   RE,15,RQCAOP5       VALIDATE OVERSCALE PERCENTAGE 5              
         CR    RE,R2                                                            
         JH    AOP5INV                                                          
                                                                                
         OC    RQCAUP6,RQCAUP6     VALIDATE OVERSCALE PERCENTAGE USE 6          
         JZ    YES                                                              
         GOTOR AVOPVAL,DMCB,RQCAUP6                                             
         JNE   AUP6INV                                                          
         ICM   RE,15,RQCAOP6       VALIDATE OVERSCALE PERCENTAGE 6              
         CR    RE,R2                                                            
         JH    AOP6INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED OVERSCALE PERCENTAGE USE       *         
*        IS NOT DUPLICATED                                            *         
*        ON ENTRY ... P1 = A(OVERSCALE PERCENTAGE USE TO CHECK)       *         
***********************************************************************         
                                                                                
AVOPNDUP NTR1                                                                   
         L     R2,0(R1)            R2=A(OVERSCALE PCT USE TO CHECK)             
         XR    R0,R0                                                            
                                                                                
         CLC   RQCAUP1,0(R2)                                                    
         JNE   *+8                                                              
         AHI   R0,1                                                             
                                                                                
         CLC   RQCAUP2,0(R2)       ASSERT THAT OVERSCALE AMOUNT                 
         JNE   *+8                 USE 2 IS NOT DUPLICATED                      
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUP2INV                                                          
                                                                                
         CLC   RQCAUP3,0(R2)       ASSERT THAT OVERSCALE AMOUNT                 
         JNE   *+8                 USE 3 IS NOT DUPLICATED                      
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUP3INV                                                          
                                                                                
         CLC   RQCAUP4,0(R2)       ASSERT THAT OVERSCALE AMOUNT                 
         JNE   *+8                 USE 4 IS NOT DUPLICATED                      
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUP4INV                                                          
                                                                                
         CLC   RQCAUP5,0(R2)       ASSERT THAT OVERSCALE AMOUNT                 
         JNE   *+8                 USE 5 IS NOT DUPLICATED                      
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUP5INV                                                          
                                                                                
         CLC   RQCAUP6,0(R2)       ASSERT THAT OVERSCALE AMOUNT                 
         JNE   *+8                 USE 6 IS NOT DUPLICATED                      
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUP6INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS OVERSCALE AMOUNT USE AND AMOUNTS ARE VALID   *         
***********************************************************************         
                                                                                
AVOA     NTR1                                                                   
         L     R2,=F'999999999'                                                 
                                                                                
         OC    RQCAUA1,RQCAUA1     VALIDATE OVERSCALE AMOUNT USE 1              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA1                                             
         JNE   AUA1INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA1                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA1                                             
         JNE   AUA1INV                                                          
         ICM   RE,15,RQCAOA1       VALIDATE OVERSCALE AMOUNT 1                  
         CR    RE,R2                                                            
         JH    AOA1INV                                                          
                                                                                
         OC    RQCAUA2,RQCAUA2     VALIDATE OVERSCALE AMOUNT USE 2              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA2                                             
         JNE   AUA2INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA2                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA2                                             
         JNE   AUA2INV                                                          
         ICM   RE,15,RQCAOA2       VALIDATE OVERSCALE AMOUNT 2                  
         CR    RE,R2                                                            
         JH    AOA2INV                                                          
                                                                                
         OC    RQCAUA3,RQCAUA3     VALIDATE OVERSCALE AMOUNT USE 3              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA3                                             
         JNE   AUA3INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA3                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA3                                             
         JNE   AUA3INV                                                          
         ICM   RE,15,RQCAOA3       VALIDATE OVERSCALE AMOUNT 3                  
         CR    RE,R2                                                            
         JH    AOA3INV                                                          
                                                                                
         OC    RQCAUA4,RQCAUA4     VALIDATE OVERSCALE AMOUNT USE 4              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA4                                             
         JNE   AUA4INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA4                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA4                                             
         JNE   AUA4INV                                                          
         ICM   RE,15,RQCAOA4       VALIDATE OVERSCALE AMOUNT 4                  
         CR    RE,R2                                                            
         JH    AOA4INV                                                          
                                                                                
         OC    RQCAUA5,RQCAUA5     VALIDATE OVERSCALE AMOUNT USE 5              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA5                                             
         JNE   AUA5INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA5                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA5                                             
         JNE   AUA5INV                                                          
         ICM   RE,15,RQCAOA5       VALIDATE OVERSCALE AMOUNT 5                  
         CR    RE,R2                                                            
         JH    AOA5INV                                                          
                                                                                
         OC    RQCAUA6,RQCAUA6     VALIDATE OVERSCALE AMOUNT USE 6              
         JZ    YES                                                              
         GOTOR VALOUSE,DMCB,RQCAUA6                                             
         JNE   AUA6INV                                                          
         GOTO1 AVOANDUP,DMCB,RQCAUA6                                            
         JNE   XIT                                                              
         GOTO1 AVOANOP,DMCB,RQCAUA6                                             
         JNE   AUA6INV                                                          
         ICM   RE,15,RQCAOA6       VALIDATE OVERSCALE AMOUNT 6                  
         CR    RE,R2                                                            
         JH    AOA6INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED OVERSCALE AMOUNT USE IS        *         
*        NOT DUPLICATED                                               *         
*        ON ENTRY ... P1 = A(OVERSCALE AMOUNT USE TO CHECK)           *         
***********************************************************************         
                                                                                
AVOANDUP NTR1                                                                   
         L     R2,0(R1)            R2=A(OVERSCALE AMT USE TO CHECK)             
         XR    R0,R0                                                            
                                                                                
         CLC   RQCAUA1,0(R2)                                                    
         JNE   *+8                                                              
         AHI   R0,1                                                             
                                                                                
         CLC   RQCAUA2,0(R2)       ASSERT THAT OVERSCALE AMOUNT USE 2           
         JNE   *+8                 IS NOT DUPLICATED                            
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUA2INV                                                          
                                                                                
         CLC   RQCAUA3,0(R2)       ASSERT THAT OVERSCALE AMOUNT USE 3           
         JNE   *+8                 IS NOT DUPLICATED                            
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUA3INV                                                          
                                                                                
         CLC   RQCAUA4,0(R2)       ASSERT THAT OVERSCALE AMOUNT USE 4           
         JNE   *+8                 IS NOT DUPLICATED                            
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUA4INV                                                          
                                                                                
         CLC   RQCAUA5,0(R2)       ASSERT THAT OVERSCALE AMOUNT USE 5           
         JNE   *+8                 IS NOT DUPLICATED                            
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUA5INV                                                          
                                                                                
         CLC   RQCAUA6,0(R2)       ASSERT THAT OVERSCALE AMOUNT USE 6           
         JNE   *+8                 IS NOT DUPLICATED                            
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AUA6INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED OVERSCALE AMOUNT USE IS        *         
*        NOT PROVIDED AS AN OVERSCALE PERCENTAGE                      *         
*        ON ENTRY ... P1 = A(OVERSCALE AMOUNT USE TO CHECK)           *         
***********************************************************************         
                                                                                
AVOANOP  NTR1                                                                   
         L     R2,0(R1)            R2=A(OVERSCALE AMT USE TO CHECK)             
         XR    R0,R0                                                            
                                                                                
         CLC   RQCAUP1,0(R2)                                                    
         JE    NO                                                               
         CLC   RQCAUP2,0(R2)                                                    
         JE    NO                                                               
         CLC   RQCAUP3,0(R2)                                                    
         JE    NO                                                               
         CLC   RQCAUP4,0(R2)                                                    
         JE    NO                                                               
         CLC   RQCAUP5,0(R2)                                                    
         JE    NO                                                               
         CLC   RQCAUP6,0(R2)                                                    
         JNE   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS SECOND OVERSCALE PERCENTAGE USE AND          *         
*        PERCENTAGES ARE VALID                                        *         
***********************************************************************         
                                                                                
AV2OP    NTR1                                                                   
         L     R2,=F'99999'                                                     
                                                                                
         OC    RQCA2U1,RQCA2U1     VALIDATE 2ND OVERSCALE PCT USE 1             
         JZ    YES                                                              
         GOTO1 AV2NDUP,DMCB,RQCA2U1                                             
         JNE   XIT                                                              
         GOTOR AV2OVAL,DMCB,RQCA2U1                                             
         JNE   A2U1INV                                                          
         ICM   RE,15,RQCA2P1       VALIDATE 2ND OVERSCALE PCT 1                 
         CR    RE,R2                                                            
         JH    A2P1INV                                                          
                                                                                
         OC    RQCA2U2,RQCA2U2     VALIDATE 2ND OVERSCALE PCT USE 2             
         JZ    YES                                                              
         GOTO1 AV2NDUP,DMCB,RQCA2U2                                             
         JNE   XIT                                                              
         GOTOR AV2OVAL,DMCB,RQCA2U2                                             
         JNE   A2U2INV                                                          
         ICM   RE,15,RQCA2P2       VALIDATE 2ND OVERSCALE PCT 2                 
         CR    RE,R2                                                            
         JH    A2P2INV                                                          
                                                                                
         OC    RQCA2U3,RQCA2U3     VALIDATE 2ND OVERSCALE PCT USE 3             
         JZ    YES                                                              
         GOTO1 AV2NDUP,DMCB,RQCA2U3                                             
         JNE   XIT                                                              
         GOTOR AV2OVAL,DMCB,RQCA2U3                                             
         JNE   A2U3INV                                                          
         ICM   RE,15,RQCA2P3       VALIDATE 2ND OVERSCALE PCT 3                 
         CR    RE,R2                                                            
         JH    A2P3INV                                                          
                                                                                
         OC    RQCA2U4,RQCA2U4     VALIDATE 2ND OVERSCALE PCT USE 4             
         JZ    YES                                                              
         GOTO1 AV2NDUP,DMCB,RQCA2U4                                             
         JNE   XIT                                                              
         GOTOR AV2OVAL,DMCB,RQCA2U4                                             
         JNE   A2U4INV                                                          
         ICM   RE,15,RQCA2P4       VALIDATE 2ND OVERSCALE PCT 4                 
         CR    RE,R2                                                            
         JH    A2P4INV                                                          
                                                                                
         OC    RQCA2U5,RQCA2U5     VALIDATE 2ND OVERSCALE PCT USE 5             
         JZ    YES                                                              
         GOTOR AV2OVAL,DMCB,RQCA2U5                                             
         JNE   A2U5INV                                                          
         ICM   RE,15,RQCA2P5       VALIDATE 2ND OVERSCALE PCT 5                 
         CR    RE,R2                                                            
         JH    A2P5INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED SECOND OVERSCALE USE IS        *         
*        NOT DUPLICATED                                               *         
*        ON ENTRY ... P1 = A(2ND OVERSCALE AMOUNT USE TO CHECK)       *         
***********************************************************************         
                                                                                
AV2NDUP  NTR1                                                                   
         L     R2,0(R1)            R2=A(2ND OVERSCALE PCT USE TO CHECK)         
         XR    R0,R0                                                            
                                                                                
         CLC   RQCA2U1,0(R2)                                                    
         JNE   *+8                                                              
         AHI   R0,1                                                             
                                                                                
         CLC   RQCA2U2,0(R2)       ASSERT THAT SECOND OVERSCALE                 
         JNE   *+8                 PERCENTAGE USE 2 IS NOT DUPLICATED           
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    A2U2INV                                                          
                                                                                
         CLC   RQCA2U3,0(R2)       ASSERT THAT SECOND OVERSCALE                 
         JNE   *+8                 PERCENTAGE USE 3 IS NOT DUPLICATED           
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    A2U3INV                                                          
                                                                                
         CLC   RQCA2U4,0(R2)       ASSERT THAT SECOND OVERSCALE                 
         JNE   *+8                 PERCENTAGE USE 4 IS NOT DUPLICATED           
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    A2U4INV                                                          
                                                                                
         CLC   RQCA2U5,0(R2)       ASSERT THAT SECOND OVERSCALE                 
         JNE   *+8                 PERCENTAGE USE 5 IS NOT DUPLICATED           
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    A2U5INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        INVALID FIELD ERROR                                          *         
***********************************************************************         
                                                                                
AUP1INV  MVI   BYTE1,D#CAUP1       INVALID OVERSCALE PERCENTAGE USE 1           
         J     AVINV                                                            
                                                                                
AOP1INV  MVI   BYTE1,D#CAOP1       INVALID OVERSCALE PERCENTAGE 1               
         J     AVINV                                                            
                                                                                
AUP2INV  MVI   BYTE1,D#CAUP2       INVALID OVERSCALE PERCENTAGE USE 2           
         J     AVINV                                                            
                                                                                
AOP2INV  MVI   BYTE1,D#CAOP2       INVALID OVERSCALE PERCENTAGE 2               
         J     AVINV                                                            
                                                                                
AUP3INV  MVI   BYTE1,D#CAUP3       INVALID OVERSCALE PERCENTAGE USE 3           
         J     AVINV                                                            
                                                                                
AOP3INV  MVI   BYTE1,D#CAOP3       INVALID OVERSCALE PERCENTAGE 3               
         J     AVINV                                                            
                                                                                
AUP4INV  MVI   BYTE1,D#CAUP4       INVALID OVERSCALE PERCENTAGE USE 4           
         J     AVINV                                                            
                                                                                
AOP4INV  MVI   BYTE1,D#CAOP4       INVALID OVERSCALE PERCENTAGE 4               
         J     AVINV                                                            
                                                                                
AUP5INV  MVI   BYTE1,D#CAUP5       INVALID OVERSCALE PERCENTAGE USE 5           
         J     AVINV                                                            
                                                                                
AOP5INV  MVI   BYTE1,D#CAOP5       INVALID OVERSCALE PERCENTAGE 5               
         J     AVINV                                                            
                                                                                
AUP6INV  MVI   BYTE1,D#CAUP6       INVALID OVERSCALE PERCENTAGE USE 6           
         J     AVINV                                                            
                                                                                
AOP6INV  MVI   BYTE1,D#CAOP6       INVALID OVERSCALE PERCENTAGE 6               
         J     AVINV                                                            
                                                                                
AUA1INV  MVI   BYTE1,D#CAUA1       INVALID OVERSCALE AMOUNT USE 1               
         J     AVINV                                                            
                                                                                
AOA1INV  MVI   BYTE1,D#CAOA1       INVALID OVERSCALE AMOUNT 1                   
         J     AVINV                                                            
                                                                                
AUA2INV  MVI   BYTE1,D#CAUA2       INVALID OVERSCALE AMOUNT USE 2               
         J     AVINV                                                            
                                                                                
AOA2INV  MVI   BYTE1,D#CAOA2       INVALID OVERSCALE AMOUNT 2                   
         J     AVINV                                                            
                                                                                
AUA3INV  MVI   BYTE1,D#CAUA3       INVALID OVERSCALE AMOUNT USE 3               
         J     AVINV                                                            
                                                                                
AOA3INV  MVI   BYTE1,D#CAOA3       INVALID OVERSCALE AMOUNT 3                   
         J     AVINV                                                            
                                                                                
AUA4INV  MVI   BYTE1,D#CAUA4       INVALID OVERSCALE AMOUNT USE 4               
         J     AVINV                                                            
                                                                                
AOA4INV  MVI   BYTE1,D#CAOA4       INVALID OVERSCALE AMOUNT 4                   
         J     AVINV                                                            
                                                                                
AUA5INV  MVI   BYTE1,D#CAUA5       INVALID OVERSCALE AMOUNT USE 5               
         J     AVINV                                                            
                                                                                
AOA5INV  MVI   BYTE1,D#CAOA5       INVALID OVERSCALE AMOUNT 5                   
         J     AVINV                                                            
                                                                                
AUA6INV  MVI   BYTE1,D#CAUA6       INVALID OVERSCALE AMOUNT USE 6               
         J     AVINV                                                            
                                                                                
AOA6INV  MVI   BYTE1,D#CAOA6       INVALID OVERSCALE AMOUNT 6                   
         J     AVINV                                                            
                                                                                
A2U1INV  MVI   BYTE1,D#CA2U1       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               USE 1                                        
                                                                                
A2P1INV  MVI   BYTE1,D#CA2P1       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               1                                            
                                                                                
A2U2INV  MVI   BYTE1,D#CA2U2       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               USE 2                                        
                                                                                
A2P2INV  MVI   BYTE1,D#CA2P2       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               2                                            
                                                                                
A2U3INV  MVI   BYTE1,D#CA2U3       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               USE 3                                        
                                                                                
A2P3INV  MVI   BYTE1,D#CA2P3       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               3                                            
                                                                                
A2U4INV  MVI   BYTE1,D#CA2U4       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               USE 4                                        
                                                                                
A2P4INV  MVI   BYTE1,D#CA2P4       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               4                                            
                                                                                
A2U5INV  MVI   BYTE1,D#CA2U5       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               USE 5                                        
                                                                                
A2P5INV  MVI   BYTE1,D#CA2P5       INVALID SECOND OVERSCALE PERCENTAGE          
         J     AVINV               5                                            
                                                                                
AVERINV  MVI   BYTE1,D#CAVER       INVALID VERSIONS                             
         J     AVINV                                                            
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED OVERSCALE PERCENTAGE USE       *         
*        IS VALID                                                     *         
*        ON ENTRY ... P1 = A(OVERSCALE PERCENTAGE USE TO CHECK)       *         
***********************************************************************         
                                                                                
AVOPVAL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(OVERSCALE PCT USE TO CHECK)             
                                                                                
         CLC   =C'ALL',0(R2)       IF OVERSCALE PERCENTAGE IS "ALL"             
         JNE   AVOPV10                                                          
         OI    CASTSTAT,CAOPALL    TURN ON "ALL" STATUS                         
         J     YES                                                              
                                                                                
AVOPV10  CLC   =C'ARE',0(R2)       IF OVERSCALE PERCENTAGE IS "ARE"             
         JNE   AVOPV20                                                          
         OI    CASTSTAT,CAOPARE    TURN ON "ARE" STATUS                         
         J     YES                                                              
                                                                                
AVOPV20  GOTOR VALOUSE,DMCB,0(R2)                                               
         JE    YES                 OTHERWISE, ASSERT THAT USE IS VALID          
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED SECOND OVERSCALE USE IS        *         
*        IS ALSO AN OVERSCALE USE                                     *         
*        ON ENTRY ... P1 = A(2ND OVERSCALE AMOUNT USE TO CHECK)       *         
***********************************************************************         
                                                                                
AV2OVAL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(OVERSCALE PCT USE TO CHECK)             
                                                                                
         CLC   RQCAUP1,0(R2)       IF USE PROVIDED AS OVERSCALE PCT             
         JE    YES                 USE 1                                        
         CLC   RQCAUP2,0(R2)       OR OVERSCALE USE 2                           
         JE    YES                                                              
         CLC   RQCAUP3,0(R2)       OR OVERSCALE USE 3                           
         JE    YES                                                              
         CLC   RQCAUP4,0(R2)       OR OVERSCALE USE 4                           
         JE    YES                                                              
         CLC   RQCAUP5,0(R2)       OR OVERSCALE USE 5                           
         JE    YES                                                              
         CLC   RQCAUP6,0(R2)       OR OVERSCALE USE 6                           
         JE    YES                 THEN IT IS VALID                             
                                                                                
         TM    CASTSTAT,CAOPALL    IF "ALL" WAS ENTERED AS AN OVERSCALE         
         JZ    AV2OV10             PERCENTAGE THEN ANY USE IS VALID             
         GOTOR VALOUSE,DMCB,0(R2)                                               
         J     XIT                                                              
                                                                                
         USING USETABD,RE                                                       
AV2OV10  TM    CASTSTAT,CAOPARE    IF "ARE" WAS ENTERED AN AN OVERSCALE         
         JZ    NO                  PERCENTAGE                                   
         GOTOR VALOUSE,DMCB,0(R2)                                               
         JNE   NO                                                               
         L     RE,AENTRY           THEN ANY REUSE USE IS VALID                  
         TM    USESTAT,SESSION                                                  
         JZ    YES                                                              
         J     NO                                                               
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS VERSIONS ARE VALID                           *         
***********************************************************************         
                                                                                
AVVER    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,VERSIONS         R2=A(VERSIONS)                               
AVV10    CLI   0(R2),0                                                          
         JE    YES                                                              
         CLC   0(1,R2),1(R2)       ASSERT THAT VERSION IS NOT                   
         JE    NO                  DUPLICATED                                   
                                                                                
         LA    R3,ORIGVERS                                                      
AVV20    CLC   0(1,R2),0(R3)       ASSERT THAT VERSION IS VALID                 
         JE    AVV30               FOR COMMERCIAL                               
         CLI   1(R3),0                                                          
         JE    NO                                                               
         LA    R3,1(R3)                                                         
         J     AVV20                                                            
                                                                                
AVV30    LA    R2,1(R2)                                                         
         JE    AVV10                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS TRACKS ARE VALID                             *         
***********************************************************************         
                                                                                
AVTRK    NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCATR1,0           ASSERT THAT TRACK 1 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR1                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR2,0           ASSERT THAT TRACK 2 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR2                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR3,0           ASSERT THAT TRACK 3 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR3                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR4,0           ASSERT THAT TRACK 4 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR4                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR5,0           ASSERT THAT TRACK 5 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR5                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR6,0           ASSERT THAT TRACK 6 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR6                                             
         JNE   XIT                                                              
                                                                                
         CLI   RQCATR7,0           ASSERT THAT TRACK 7 IS NOT                   
         JE    YES                 DUPLICATED                                   
         GOTO1 AVTNDUP,DMCB,RQCATR6                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED TRACK IS NOT DUPLICATED        *         
*        ON ENTRY ... P1 = A(TRACK TO CHECK)                          *         
***********************************************************************         
                                                                                
AVTNDUP  NTR1                                                                   
         L     R2,0(R1)            R2=A(TRACK TO CHECK)                         
         XR    R0,R0                                                            
                                                                                
         CLC   RQCATR1,0(R2)                                                    
         JNE   *+8                                                              
         AHI   R0,1                                                             
                                                                                
         CLC   RQCATR2,0(R2)       ASSERT THAT TRACK 2 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR2INV                                                          
                                                                                
         CLC   RQCATR3,0(R2)       ASSERT THAT TRACK 3 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR3INV                                                          
                                                                                
         CLC   RQCATR4,0(R2)       ASSERT THAT TRACK 4 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR4INV                                                          
                                                                                
         CLC   RQCATR5,0(R2)       ASSERT THAT TRACK 5 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR5INV                                                          
                                                                                
         CLC   RQCATR6,0(R2)       ASSERT THAT TRACK 6 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR6INV                                                          
                                                                                
         CLC   RQCATR7,0(R2)       ASSERT THAT TRACK 7 IS NOT                   
         JNE   *+8                 DUPLICATED                                   
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    ATR7INV                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        INVALID FIELD ERROR                                          *         
***********************************************************************         
                                                                                
ATR2INV  MVI   BYTE1,D#CATR2       INVALID TRACK 2                              
         J     AVINV                                                            
                                                                                
ATR3INV  MVI   BYTE1,D#CATR3       INVALID TRACK 3                              
         J     AVINV                                                            
                                                                                
ATR4INV  MVI   BYTE1,D#CATR4       INVALID TRACK 4                              
         J     AVINV                                                            
                                                                                
ATR5INV  MVI   BYTE1,D#CATR5       INVALID TRACK 5                              
         J     AVINV                                                            
                                                                                
ATR6INV  MVI   BYTE1,D#CATR6       INVALID TRACK 6                              
         J     AVINV                                                            
                                                                                
ATR7INV  MVI   BYTE1,D#CATR7       INVALID TRACK 7                              
         J     AVINV                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS CAST DELETE REQUEST                                  *         
***********************************************************************         
                                                                                
CADUP    NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#CASTA)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RQCASRT,RQCASRT                                                  
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BAS   RE,DASTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   CADUP40                                                          
         BAS   RE,DASTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   CADUP40                                                          
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCASTF                                   
         JNE   CADUP40                                                          
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA 3)                             
                                                                                
         BRAS  RE,VALCOM           VALIDATE INTERNAL COMMERCIAL NUMBER          
         JNE   CADUP40             (SAVE COMMERCIAL RECORD IN AIO1)             
                                                                                
         BRAS  RE,VALSSN           VALIDATE SOCIAL SECURITY NUMBER              
         JNE   CADUP40                                                          
                                                                                
         BRAS  RE,VALHST           ENSURE CAST DOES NOT HAVE ANY                
         JNE   CADUP40             PAYMENT HISTORY                              
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCACCDQ    READ FOR CAST KEY                            
         MVC   TLCACSSN,RQCASSN                                                 
         MVC   TLCACCOM,RQCACOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CADUP20                                                          
CADUP10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CADUP20  CLC   IOKEY(TLCACCAT-TLCAPD),IOKEYSAV                                  
         JNE   CADUP30                                                          
         CLC   TLCACSEQ,RQCASEQ                                                 
         JNE   CADUP10                                                          
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         BRAS  RE,SETWID           SET WEB APPLICATION ID                       
         BRAS  RE,VALWID           AND VALIDATE WEB APPLICATION ID              
         J     CADUP40                                                          
                                                                                
CADUP30  GOTOR (#ADDERR,AADDERR),DMCB,ERCDCANF                                  
                                                                                
CADUP40  MVI   OUTPUT,CADEER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    CADUP50                                                          
         MVI   OUTPUT,CADEOK1      ELSE RETURN "OK" STATUS                      
                                                                                
CADUP50  GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCAMOD,RQCAEXE     IF MODE IS EXECUTE                           
         JNE   CADUP60                                                          
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    CADUP60             SAVE ORIGINAL PASSIVE POINTERS               
         GOTOR (#SAVPTRS,ASAVPTRS) AND DETERMINE IF ORIGINALLY                  
         BRAS  RE,STOELHLD         ELIGIBLE FOR HOLDING FEES                    
         OI    COMSTAT,COUNVRFY+COREISSU                                        
         BRAS  RE,UPDCOM           UPDATE COMMMERCIAL                           
         BRAS  RE,EXECDEL          THEN DELETE CAST RECORD                      
                                                                                
         TIME  DEC                                                              
         STCM  R0,14,SVTIME        SAVE TIME                                    
         BRAS  RE,ADDWTR           AND ADD WEB TRANSACTION RECORD               
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCASTF),(L'RQCASTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
CADUP60  GOTO1 VHEXOUT,DMCB,RQCACOM,OUTPUT,L'RQCACOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 VHEXOUT,DMCB,RQCASEQ,OUTPUT,L'RQCASEQ,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',9),            +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#CAERR,OUTPUT                            
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
DASTREQ  NTR1                                                                   
         MVI   BYTE1,D#CAMOD                                                    
         CLI   RQCAMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    DRMIS                                                            
                                                                                
         MVI   BYTE1,D#CASTF                                                    
         OC    RQCASTF,RQCASTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    DRMIS                                                            
                                                                                
         MVI   BYTE1,D#CACOM                                                    
         OC    RQCACOM,RQCACOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    DRMIS               NUMBER IS PROVIDED                           
                                                                                
         MVI   BYTE1,D#CASEQ                                                    
         OC    RQCASEQ,RQCASEQ     ASSERT THAT CAST SEQUENCE NUMBER             
         JZ    DRMIS               IS PROVIDED                                  
                                                                                
         MVI   BYTE1,D#CASSN                                                    
         OC    RQCASSN,RQCASSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JZ    DRMIS               IS PROVIDED                                  
                                                                                
         MVI   BYTE1,D#CAWID                                                    
         OC    RQCAWID,RQCAWID     ASSERT THAT WEB APPLICATION ID               
         JZ    DRMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
DRMIS    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENMIS',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
DASTVAL  NTR1                                                                   
         MVI   BYTE1,D#CAMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCAMOD)                        
         JNE   DVINV                                                            
         J     YES                                                              
                                                                                
DVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CADUP                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCDCANF DC    AL1(ECDCANFX-*),AL2(3),AL1(ERRCATY3),AL1(D#CASEQ)                
         DC    C'Cast record is not on file'                                    
ECDCANFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CADEOK1  EQU   1                   NO ERRORS - CAST DELETED                     
CADEER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS WEB APPLICATION RECORD ID                            *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
PROWRI   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAWRI,RQCAWRI     IF WEB APPLICATION RECORD ID IS              
         JZ    XIT                 PROVIDED                                     
         OC    RQCASEQ,RQCASEQ     AND CAST SEQUENCE NUMBER IS NOT              
         JNZ   XIT                 PROVIDED                                     
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     READ ALL CAST KEYS/RECORDS                   
         MVI   TLCACD,TLCACDQ      FOR THIS INTERNAL COMMERCIAL NUMBER          
         MVC   TLCACOM,RQCACOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     PW20                                                             
PW10     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
PW20     CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   XIT                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO1),('TAFNTWRI',0)         
         JNE   PW10                                                             
         L     R4,AELEM                                                         
         CLC   RQCAWRI,TAFNNAME                                                 
         JNE   PW10                                                             
         MVC   RQCASEQ,TLCASEQ     ... SAVE CAST SEQUENCE NUMBER                
         J     XIT                 INTO REQUEST MAP                             
         DROP  R3,R4                                                            
                                                                                
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
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY/RECORD               
         MVI   TLCOPCD,TLCOCCDQ    AND ENSURE IT EXISTS                         
         MVC   TLCOCCOM,RQCACOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VC10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACONF                                  
         J     NO                                                               
VC10     GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO1'                           
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
                                                                                
         BAS   RE,VALCOWID         VALIDATE COMMERCIAL WEB ID                   
         JNE   NO                                                               
                                                                                
         MVC   SVAGY,TLCOAGY       SAVE COMMERCIAL AGENCY                       
         MVC   SVCLI,TLCOCLI       AND CLIENT                                   
         DROP  R4                                                               
                                                                                
         MVI   LFTSTAT,C'Y'                                                     
         MVI   ELCODE,TALFELQ      SET COMMERCIAL LIFT STATUS                   
         BRAS  RE,GETEL                                                         
         JE    VC20                                                             
         MVI   LFTSTAT,C'N'                                                     
                                                                                
VC20     L     R4,AIO1                                                          
         MVI   VERSTAT,C'Y'                                                     
         MVI   ELCODE,TAVRELQ      SET COMMERCIAL VERSION STATUS                
         BRAS  RE,GETEL                                                         
         JE    VC30                                                             
         MVI   VERSTAT,C'N'                                                     
                                                                                
         CLI   I$VERS,0            IF COMMERCIAL DOES NOT HAVE VERSIONS         
         JE    VC30                CANNOT ADD THEM TO CAST                      
         GOTOR (#ADDERR,AADDERR),DMCB,ERCANOVR                                  
                                                                                
         USING TANAD,R4                                                         
VC30     L     R4,AIO1                                                          
         MVI   ELCODE,TANAELQ      SAVE COMMERCIAL TITLE                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   SVTITLE(0),TANANAME                                              
         OC    SVTITLE,SPACES                                                   
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            R4=A(COMMERCIAL DETAILS ELEMENT)             
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   SVCID,TACOCID       SAVE COMMERCIAL ID                           
         MVC   SVCOFCYC,TACOFCYC   FIRST FIXED CYCLE DATE                       
         MVC   SVCOAIR,TACOAIR     FIRST AIR DATE                               
         MVC   SVCOEXP,TACOEXP     EXPIRATION DATE                              
         MVC   SVCOVDTE,TACOVDTE   VERIFICATION DATE                            
         MVC   SVCOMED,TACOMED     COMMERCIAL MEDIA                             
         MVC   SVCOTYPE,TACOTYPE   COMMERCIAL TYPE                              
         MVC   SVCOSTA2,TACOSTA2   COMMERCIAL STATUS 2                          
         MVC   SVCOSTAT,TACOSTAT   AND COMMERCIAL STATUS                        
                                                                                
         CLI   ACTION,ACTDEL                                                    
         JE    YES                                                              
                                                                                
         CLI   TACOMED,TACOMEDR    IF MEDIA IS RADIO                            
         JNE   VC50                                                             
         CLC   RQCAONO,=C'ON '     ON/OFF CAMERA CANNOT BE ON                   
         JNE   VC40                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAINCR                                  
VC40     CLC   RQCAUNI,=C'SAG'     AND UNION CANNOT BE SAG                      
         JNE   VC50                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAINUR                                  
                                                                                
VC50     CLI   TACOTYPE,CTYIND     IF TYPE IS NOT INDUSTRIAL                    
         JE    VC60                                                             
         CLC   RQCACYR,=CL3'01'    AND CONTRACT YEAR IS 01                      
         JNE   VC60                                                             
         CLC   RQCAUNI,=C'SAG'     UNION CANNOT BE SAG                          
         JE    *+14                                                             
         CLC   RQCAUNI,=C'AFT'     OR AFTRA                                     
         JNE   VC60                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAINCY                                  
                                                                                
VC60     TM    TACOSTAT,TACOSCRT   IF SET FOR CANADIAN RATES                    
         JZ    VC70                CANNOT SET "PAY CANADIAN RATES               
         CLI   RQCAPCR,C'Y'        ON US COMMERCIAL?" TO Y                      
         JNE   VC80                TO Y                                         
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACOCR                                  
         J     VC80                                                             
*                                  IF SET FOR US RATES, CANNOT SET              
VC70     CLI   RQCAPUR,C'Y'        "PAY US RATES ON CANADIAN                    
         JNE   VC80                COMMERCIAL" TO Y                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACOUR                                  
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
VC80     OC    RQCASEQ,RQCASEQ     IF CAST SEQUENCE NUMBER IS NOT               
         JNZ   VC90                PROVIDED                                     
         GOTOR (#GETELEM,AGETELEM),DMCB,('TANUELQ',AIO1),('TANUTSEQ',0)         
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AELEM                                                         
         MVC   SVNUNXTC,TANUNXTC   SAVE NEXT CAST SEQUENCE NUMBER               
         OI    COMSTAT,COCSTSEQ    AND SET TO UPDATE COMMERCIAL                 
         DROP  R4                                                               
                                                                                
VC90     CLI   VERSTAT,C'Y'        IF COMMERCIAL HAS VERSIONS                   
         JNE   YES                                                              
         MVI   ORIGVERS,1                                                       
         LA    R2,ORIGVERS+1       PREPARE TO SAVE VERSIONS                     
                                                                                
         USING TLVRD,R3                                                         
         XC    TLVRKEY,TLVRKEY     READ ALL VERSION KEYS FOR                    
         MVI   TLVRCD,TLVRCDQ      COMMERCIAL AND SAVE ALL VERSIONS             
         MVC   TLVRCOM,RQCACOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     VC110                                                            
VC100    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
VC110    CLC   IOKEY(TLVRVER-TLVRD),IOKEYSAV                                    
         JNE   YES                                                              
         MVC   0(1,R2),TLVRVER                                                  
         LA    R2,1(R2)                                                         
         J     VC100                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        VALIDATE COMMERCIAL WEB APPLICATION ID                       *         
*        ON ENTRY ... R4 = A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
VALCOWID NTR1                                                                   
         CLC   =C'VS',RQCAWID      IF COMING FROM VITA SESSION ...              
         JE    VCWID10                                                          
         CLC   =C'TS',RQCAWID                                                   
         JE    VCWID10                                                          
         CLC   =C'RS',RQCAWID                                                   
         JNE   YES                                                              
                                                                                
         USING TAFND,R4                                                         
VCWID10  GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',(R4)),('TAFNTWEB',0)         
         JNE   VCWID20                                                          
         L     R4,AELEM                                                         
         MVC   SVCOWID,TAFNNAME                                                 
         DROP  R4                                                               
                                                                                
VCWID20  OC    SVCOWID,SVCOWID     ENSURE COMEMRCIAL WAS LAST UPDATED           
         JZ    VCWID30             IN VITA                                      
         CLC   =C'VS',SVCOWID                                                   
         JE    VCWID40                                                          
         CLC   =C'TS',SVCOWID                                                   
         JE    VCWID40                                                          
         CLC   =C'RS',SVCOWID                                                   
         JE    VCWID40                                                          
         CLC   =C'VC',SVCOWID                                                   
         JE    YES                                                              
         CLC   =C'TC',SVCOWID                                                   
         JE    YES                                                              
         CLC   =C'RC',SVCOWID                                                   
         JE    YES                                                              
VCWID30  GOTOR (#ADDERR,AADDERR),DMCB,ERCACOVI                                  
         J     NO                                                               
                                                                                
VCWID40  CLC   RQCAWID,SVCOWID     ENSURE COMMERCIAL WAS LAST UPDATED           
         JE    YES                 FROM THIS SESSION                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACDVS                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCACONF DC    AL1(ECACONFX-*),AL2(1),AL1(ERRCATY3),AL1(D#CACOM)                
         DC    C'Commercial record is not on file'                              
ECACONFX EQU   *                                                                
                                                                                
ERCANOVR DC    AL1(ECANOVRX-*),AL2(87),AL1(ERRCATY1),AL1(D#CAVER)               
         DC    C'Commercial does not have Versions'                             
ECANOVRX EQU   *                                                                
                                                                                
ERCAINCR DC    AL1(ECAINCRX-*),AL2(11),AL1(ERRCATY1),AL1(D#CAONO)               
         DC    C'Invalid Camera Setting for Radio Commercial'                   
ECAINCRX EQU   *                                                                
                                                                                
ERCAINUR DC    AL1(ECAINURX-*),AL2(14),AL1(ERRCATY1),AL1(D#CAUNI)               
         DC    C'Invalid Union for Radio Commercial'                            
ECAINURX EQU   *                                                                
                                                                                
ERCAINCY DC    AL1(ECAINCYX-*),AL2(18),AL1(ERRCATY1),AL1(D#CACYR)               
         DC    C'Invalid Contract Year for non-Industrial Commercial'           
ECAINCYX EQU   *                                                                
                                                                                
ERCACOCR DC    AL1(ECACOCRX-*),AL2(75),AL1(ERRCATY1),AL1(D#CAPCR)               
         DC    C'Commercial is set for Canadian Rates'                          
ECACOCRX EQU   *                                                                
                                                                                
ERCACOUR DC    AL1(ECACOURX-*),AL2(77),AL1(ERRCATY1),AL1(D#CAPUR)               
         DC    C'Commercial is set for US Rates'                                
ECACOURX EQU   *                                                                
                                                                                
ERCACOVI DC    AL1(ECACOVIX-*),AL2(89),AL1(ERRCATY3),AL1(D#CAWID)               
         DC    C'Commercial was updated outside of Vita'                        
ECACOVIX EQU   *                                                                
                                                                                
ERCACDVS DC    AL1(ECACDVSX-*),AL2(90),AL1(ERRCATY3),AL1(D#CAWID)               
         DC    C'Commercial was updated from a different Vita Session'          
ECACDVSX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SAVED AGENCY CODE                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLAYD,R3                                                         
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY/RECORD                   
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SVAGY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         DROP  R3                                                               
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAAYELQ      R4=A(AGENCY DETAILS ELEMENT)                 
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVAYTPOF,TAAYTPOF   SAVE OFFICE                                  
         MVC   SVAYSTA6,TAAYSTA6   AND 6TH STATUS BYTE                          
                                                                                
         TM    TAAYSTA3,TAAYSLCK   CANNOT BE LOCKED                             
         JZ    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAAYLK                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGY                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAAYLK DC    AL1(ECAAYLKX-*),AL2(2),AL1(ERRCATY1),AL1(D#CACOM)                
         DC    C'Agency record is locked'                                       
ECAAYLKX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SAVED CLIENT CODE                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCLI   NTR1  BASE=*,LABEL=*                                                   
         USING TLCLD,R3                                                         
         XC    TLCLKEY,TLCLKEY     READ FOR CLIENT KEY/RECORD                   
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,SVAGY                                                    
         MVC   TLCLCLI,SVCLI                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         DROP  R3                                                               
                                                                                
         USING TACID,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACIELQ      R4=A(CLIENT INFORMATION ELEMENT)             
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SVCISTA2,TACISTA2   SAVE 2ND STATUS BYTE                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
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
         MVC   TLW4SSN,RQCASSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    VS10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAW4NF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
VS10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            R4=A(W4 DETAILS ELEMENT)                     
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   ACTION,ACTDEL                                                    
         JE    VS30                                                             
                                                                                
         TM    TAW4STA2,TAW4SLCK   CANNOT BE LOCKED                             
         JZ    VS20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAW4LK                                  
                                                                                
VS20     CLI   TAW4TYPE,TAW4TYTR   CANNOT BE TYPE TRUSTEE                       
         JNE   VS30                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAW4TR                                  
                                                                                
VS30     MVC   SVW4STA3,TAW4STA3                                                
                                                                                
         MVC   SVW4NAME,TAW4CRPN                                                
         CLI   TAW4TYPE,TAW4TYCO                                                
         JE    YES                                                              
         CLI   TAW4TYPE,TAW4TYES                                                
         JE    YES                                                              
         MVC   SVW4NAME(L'TAW4NAM1),TAW4NAM1                                    
         MVC   SVW4NAME+L'TAW4NAM1(L'TAW4NAM2),TAW4NAM2                         
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALSSN                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAW4NF DC    AL1(ECAW4NFX-*),AL2(4),AL1(ERRCATY3),AL1(D#CASSN)                
         DC    C'W4 record is not on file'                                      
ECAW4NFX EQU   *                                                                
                                                                                
ERCAW4LK DC    AL1(ECAW4LKX-*),AL2(6),AL1(ERRCATY1),AL1(D#CASSN)                
         DC    C'W4 record is locked'                                           
ECAW4LKX EQU   *                                                                
                                                                                
ERCAW4TR DC    AL1(ECAW4TRX-*),AL2(7),AL1(ERRCATY1),AL1(D#CASSN)                
         DC    C'W4 record is a trustee'                                        
ECAW4TRX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PAYMENT HISTORY                                     *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCKPD,R3                                                        
VALHST   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCKPKEY,TLCKPKEY   READ FOR CHECK PAYMENT HISTORY               
         MVI   TLCKPCD,TLCKHCDQ                                                 
         MVC   TLCKHCOM,RQCACOM                                                 
         MVC   TLCKHSSN,RQCASSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     VH20                                                             
VH10     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
VH20     CLC   IOKEY(TLCKHCAT-TLCKPD),IOKEYSAV                                  
         JNE   YES                                                              
         CLC   TLCKHSEQ,RQCASEQ                                                 
         JNE   VH10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCDCAPD                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALHST                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCDCAPD DC    AL1(ECDCAPDX-*),AL2(95),AL1(ERRCATY3),AL1(D#CASEQ)               
         DC    C'Performer has been paid and cannot be removed'                 
ECDCAPDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED LIFTED FROM INTERNAL COMMERCIAL NUMBER       *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALLFI   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCALFI,RQCALFI     ONLY VALIDATE IF LIFTED FROM                 
         JZ    XIT                 COMMERCIAL NUMBER HAS BEEN PASSED            
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOCCDQ    AND ENSURE IT EXISTS                         
         MVC   TLCOCCOM,RQCALFI                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCALCNF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALLFI                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCALCNF DC    AL1(ECALCNFX-*),AL2(9),AL1(ERRCATY1),AL1(D#CALFI)                
         DC    C'Lifted from commercial record is not on file'                  
ECALCNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED LOCAL CODE                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALLCL   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCALCL,RQCALCL     ONLY VALIDATE IF LOCAL HAS BEEN              
         JZ    XIT                 PASSED                                       
                                                                                
         USING TLLOD,R3                                                         
         XC    TLLOKEY,TLLOKEY     READ FOR LOCAL KEY                           
         MVI   TLLOCD,TLLOCDQ      AND ENSURE IT EXISTS                         
         MVC   TLLOUN,RQCAUNI                                                   
         MVC   TLLOLCL,RQCALCL                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCALONF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALLCL                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCALONF DC    AL1(ECALONFX-*),AL2(16),AL1(ERRCATY1),AL1(D#CALCL)               
         DC    C'Local record is not on file'                                   
ECALONFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AGENT CODE                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALAGT   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAAGT,RQCAAGT     ONLY VALIDATE IF AGENT CODE                  
         JZ    XIT                 HAS BEEN PASSED                              
                                                                                
         USING TLAND,R3                                                         
         XC    TLANKEY,TLANKEY     READ FOR AGENT KEY                           
         MVI   TLANCD,TLANCDQ      AND ENSURE IT EXISTS                         
         MVC   TLANAGT,RQCAAGT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         JE    VAGT10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAANFD                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VAGT10   GOTOR (#TRNSAGT,ATRNSAGT),DMCB,(X'80',RQCAAGT),TRNSAGT                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGT                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAANFD DC    AL1(ECAANFDX-*),AL2(28),AL1(ERRCATY1),AL1(D#CAAGT)               
         DC    C'Agent record is not on file'                                   
ECAANFDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED TRACK #1                                     *         
*        ON ENTRY ... P1 = A(TRACK FIELD)                             *         
*                     P2 = A(ERROR ENTRY)                             *         
*                     AIO1=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
VALTRK   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(TRACK FIELD)                            
         CLI   0(R2),0             ONLY VALIDATE IF TRACK                       
         JE    XIT                 HAS BEEN PASSED                              
                                                                                
         L     R3,4(R1)            R3=A(ERROR ENTRY)                            
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAMCELQ      READ ALL MUSIC CONTRACT DETAILS              
         BRAS  RE,GETEL            ELEMENTS                                     
         J     *+8                                                              
VTRK10   BRAS  RE,NEXTEL                                                        
         JNE   VTRK20                                                           
         CLC   TAMCTRK,0(R2)       RETURN ERROR IF TRACK IS NOT                 
         JNE   VTRK10              FOUND                                        
         JE    XIT                                                              
         DROP  R4                                                               
                                                                                
VTRK20   GOTOR (#ADDERR,AADDERR),DMCB,0(R3)                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TRANSLATE OVERSCALE USE CODES BASED ON COMMERCIAL MEDIA/TYPE *         
*        ON ENTRY ... R1 = A(FIRST OF SIX OVERSCALE FIELDS)           *         
***********************************************************************         
                                                                                
TRNOVUS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         LHI   R0,6                                                             
                                                                                
TOU10    OC    0(L'RQCAUP1,R2),0(R2)                                            
         JZ    XIT                                                              
                                                                                
         CLC   0(L'RQCAUP1,R2),=C'ALL'                                          
         JNE   TOU20                                                            
         MVC   0(L'RQCAUP1,R2),SPACES                                           
         J     TOU60                                                            
                                                                                
         USING TOUTABD,R3                                                       
TOU20    LA    R3,TOUTAB                                                        
TOU30    CLC   TOUTOUSE,0(R2)                                                   
         JNE   TOU40                                                            
         CLC   TOUTTYPE,SVCOTYPE                                                
         JNE   TOU40                                                            
         CLI   TOUTMED,0                                                        
         JE    TOU50                                                            
         CLC   TOUTMED,SVCOMED                                                  
         JE    TOU50                                                            
TOU40    LA    R3,TOUTLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         JNE   TOU30                                                            
         J     TOU60                                                            
                                                                                
TOU50    MVC   0(L'RQCAUP1,R2),TOUTTUSE                                         
         DROP  R3                                                               
                                                                                
TOU60    LA    R2,L'RQCAUP1+L'RQCAOP1(R2)                                       
         BCT   R0,TOU10                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
TOUTAB   DC    CL3'BSS',CL3'SCS',AL1(CTYSPAN),AL1(TACOMEDC)                     
         DC    CL3'BSS',CL3'DEM',AL1(CTYDEM),AL1(0)                             
         DC    CL3'BSS',CL3'SNA',AL1(CTYSDEM),AL1(0)                            
         DC    CL3'BSS',CL3'SSS',AL1(CTYSPAN),AL1(0)                            
         DC    CL3'BSS',CL3'PUB',AL1(CTYPUB),AL1(0)                             
         DC    CL3'BSS',CL3'FGS',AL1(CTYFGN),AL1(0)                             
         DC    CL3'BSS',CL3'ADT',AL1(CTYADD),AL1(0)                             
         DC    CL3'BSS',CL3'PRM',AL1(CTYPROMO),AL1(0)                           
         DC    CL3'BSS',CL3'DEM',AL1(CTYANIM),AL1(0)                            
         DC    CL3'WSP',CL3'SWS',AL1(CTYSPAN),AL1(0)                            
         DC    CL3'WSP',CL3'ADW',AL1(CTYADD),AL1(0)                             
         DC    CL3'CBL',CL3'SCB',AL1(CTYSPAN),AL1(0)                            
         DC    CL3'LFT',CL3'SLF',AL1(CTYSPAN),AL1(0)                            
         DC    CL3'LFT',CL3'ALF',AL1(CTYADD),AL1(0)                             
         DC    CL3'HLD',CL3'SHL',AL1(CTYSPAN),AL1(0)                            
         DC    CL3'HLD',CL3'ADH',AL1(CTYADD),AL1(0)                             
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CALCULATE EXPIRATION DATE                                              
***********************************************************************         
                                                                                
CLCEXP   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAEXP,RQCAEXP     IF CAST EXPIRATION DATE IS NOT               
         JNZ   XIT                 PROVIDED                                     
         OC    RQCAFSV,RQCAFSV     AND FIRST SERVICES DATE IS PROVIDED          
         JZ    XIT                                                              
         OC    SVCOFCYC,SVCOFCYC   AND COMMERCIAL HAS A FIRST FIXED             
         JZ    XIT                 CYCLE DATE                                   
         CLI   SVCOTYPE,CTYSEAS2   AND COMMERCIAL TYPE IS NOT                   
         JE    XIT                 NEW SEASONAL                                 
         CLI   SVCOTYPE,CTYICAT1   AND COMMERCIAL TYPE IS NOT                   
         JE    XIT                 INDUSTRIAL CATEGORY 1 ...                    
                                                                                
         GOTO1 VDATCON,DMCB,(1,SVCOFCYC),(0,WORK)                               
         GOTO1 VADDAY,DMCB,WORK,WORK2,-89                                       
         GOTO1 VDATCON,DMCB,(0,WORK2),(1,FCYCM89)                               
                                                                                
         GOTO1 VADDAY,DMCB,WORK,WORK2,45                                        
         GOTO1 VDATCON,DMCB,(0,WORK2),(1,FCYCP45)                               
                                                                                
         GOTO1 VADDAY,DMCB,WORK,WORK2,90                                        
         GOTO1 VDATCON,DMCB,(0,WORK2),(1,FCYCP90)                               
                                                                                
         USING PERVALD,R2                                                       
         LA    R2,WORK2                                                         
                                                                                
         CLC   RQCAONO,=C'ON '     IF PERFORMER IS ON CAMERA                    
         JNE   CE10                                                             
         CLC   RQCAFSV,SVCOFCYC    AND FIRST SERVICES DATE IS                   
         JL    CE30                => COMMERCIAL FIRST FIXED CYCLE              
         CLC   RQCAFSV,FCYCP90     AND <= 90 DAYS BEYOND COMMERCIAL             
         JH    CE30                FIRST FIXED CYCLE, EXPIRATION DATE           
         MVC   RQCAEXP,SVCOEXP     IS COMMERCIAL'S EXPIRATION DATE              
         J     XIT                                                              
                                                                                
CE10     CLC   RQCAFSV,FCYCM89     IF PERFORMER IS OFF CAMERA AND               
         JL    CE30                FIRST SERVICES DATE IS > 89 DAYS             
         CLC   RQCAFSV,FCYCP90     BEFORE OR > 90 DAYS AFTER                    
         JH    CE30                COMMERCIAL FIRST FIXED CYCLE ...             
                                                                                
         CLC   RQCAFSV,FCYCP45     ... IF <= 45 DAYS AFTER COMMERCIAL           
         JH    CE20                FIRST FIXED CYCLE, EXPIRATION DATE           
         MVC   RQCAEXP,SVCOEXP     IS COMMERCIAL'S EXPIRATION DATE              
         J     XIT                                                              
                                                                                
CE20     GOTO1 VDATCON,DMCB,(1,SVCOEXP),(5,WORK)                                
         MVC   WORK+8(5),=C'-(3M)' ... IF > 45 DAYS AFTER COMMERCIAL            
         GOTO1 VPERVAL,DMCB,(13,WORK),(R2)   FIRST FIXED CYCLE,                 
         MVC   RQCAEXP,PVALPEND        EXPIRATION DATE IS COMMERCIAL'S          
         J     XIT                     EXPIRATION DATE + 3 MONTHS               
                                                                                
***********************************************************************         
                                                                                
*                                  IF EXPIRATION DATE IS BASED ON               
CE30     MVC   WORK+9(5),=C'(21M)' CAST FIRST SERVICES, DEFAULT 21M             
                                                                                
         CLI   SVCOMED,TACOMEDC    IF COMM'L MEDIA IS CABLE                     
         JE    CE40                                                             
         CLI   SVCOTYPE,CTYPUB     OR IF COMM'L TYPE B                          
         JNE   CE50                                                             
CE40     MVC   WORK+9(5),=C'(12M)' DEFAULT OF 12 MONTHS                         
                                                                                
CE50     TM    SVCOSTAT,TACOSCRT   IF CANADIAN RATES                            
         JZ    CE70                                                             
         MVC   WORK+9(5),=C'(65W)' DEFAULT OF 65 WEEKS                          
                                                                                
         CLI   SVCOMED,TACOMEDT    IF CANADIAN TELEVISION                       
         JE    CE60                                                             
         CLI   SVCOMED,TACOMEDI    OR INTERNET                                  
         JE    CE60                                                             
         CLI   SVCOMED,TACOMEDN    OR NEW MEDIA                                 
         BNE   CE80                                                             
CE60     MVC   WORK+9(5),=C'(24M)' DEFAULT TO 24 MONTHS                         
         OC    SVCOAIR,SVCOAIR     IF FIRST AIR DATE WITHIN 6 MONTHS            
         JZ    CE80                OF 1ST SERVICES DATE                         
         GOTO1 VDATCON,DMCB,(1,SVCOFCYC),(0,TEMP)                               
         GOTO1 (RF),(R1),(1,SVCOAIR),(0,TEMP+6)                                 
         GOTO1 VPERVERT,DMCB,TEMP,TEMP+6                                        
         CLC   =H'7',DMCB+14                                                    
         JL    CE80                                                             
         JH    *+14                                                             
         CLC   SVCOAIR+2(1),2(R3)                                               
         JH    CE80                                                             
         MVC   WORK+9(5),=C'(18M)' THEN ONLY 18 MONTHS                          
         J     CE80                                                             
                                                                                
CE70     CLI   SVCOTYPE,CTYICAT2   IF INDUSTRIALS CATEGORY 2                    
         JNE   CE80                                                             
         MVC   WORK+9(5),=C'(60M)' DEFAULT OF 5 YEARS                           
                                                                                
CE80     GOTO1 VDATCON,DMCB,(1,RQCAFSV),(8,WORK)                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 VPERVAL,DMCB,(14,WORK),('PVIN1DYL',(R2))                         
         MVC   RQCAEXP,PVALPEND                                                 
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF CAST RECORD              *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
INITADD  NTR1  BASE=*,LABEL=*                                                   
         OI    CASTSTAT,CANEWCRP                                                
         MVI   ACTION,ACTADD                                                    
         MVC   IOADDR,AIO3                                                      
                                                                                
         TM    SVW4STA3,TAW4SREG   IF W4 IS FOR REGRESSION TESTING              
         JZ    INITA10                                                          
         TM    SVAYSTA6,TAAYSREG   ENSURE AGENCY                                
         JO    INITA30                                                          
         TM    SVCISTA2,TACISREG   OR CLIENT IS FOR REGRESSION TESTING          
         JO    INITA30                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAW4RG                                  
         J     NO                                                               
                                                                                
INITA10  TM    SVAYSTA6,TAAYSREG   IF W4 IS NOT FOR REGRESSION TESTING          
         JO    INITA20             ENSURE AGENCY                                
         TM    SVCISTA2,TACISREG   AND CLIENT ARE NOT FOR REGRESSION            
         JZ    INITA30             TESTING                                      
INITA20  GOTOR (#ADDERR,AADDERR),DMCB,ERCAW4NR                                  
         J     NO                                                               
                                                                                
INITA30  OC    RQCASEQ,RQCASEQ     IF CAST SEQUENCE NUMBER IS PROVIDED          
         JZ    INITA60                                                          
         XC    TLCAKEY,TLCAKEY     ENSURE CAST SEQUENCE NUMBER IS NOT           
         MVI   TLCACD,TLCACDQ      ALREADY ATTACHED TO DIFFERENT SS#            
         MVC   TLCACOM,RQCACOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         J     INITA50                                                          
INITA40  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR'                                   
INITA50  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   INITA60                                                          
         CLC   TLCASEQ,RQCASEQ                                                  
         JNE   INITA40                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCANCSS                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
INITA60  GOTOR (#ADDERR,AADDERR),DMCB,ERCACANF                                  
                                                                                
         CLI   VERSTAT,C'Y'        IF COMMERCIAL HAS VERSIONS                   
         JNE   YES                                                              
         LA    RE,VERSIONS                                                      
         CLI   RQCAMST,C'Y'        AND "ON MASTER COMMERCIAL?"                  
         JNE   INITA70             IS SET, SET VERSION 1                        
         MVI   0(RE),1                                                          
         LA    RE,1(RE)                                                         
INITA70  CLI   RQCALFT,C'Y'        AND "ON LIFT VERSION?" IS SET,               
         JNE   YES                 SET VERSION 2                                
         MVI   0(RE),2                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCANCSS DC    AL1(ECANCSSX-*),AL2(5),AL1(ERRCATY3),AL1(D#CASEQ)                
         DC    C'Cannot change Social Security Number'                          
ECANCSSX EQU   *                                                                
                                                                                
ERCAW4RG DC    AL1(ECAW4RGX-*),AL2(129),AL1(ERRCATY3),AL1(D#CASSN)              
         DC    C'W4 record is for regression testing'                           
ECAW4RGX EQU   *                                                                
                                                                                
ERCAW4NR DC    AL1(ECAW4NRX-*),AL2(130),AL1(ERRCATY3),AL1(D#CASSN)              
         DC    C'W4 record is not for regression testing'                       
ECAW4NRX EQU   *                                                                
                                                                                
ERCACANF DC    AL1(ECACANFX-*),AL2(3),AL1(ERRCATY2),AL1(D#CASEQ)                
         DC    C'Cast record is not on file'                                    
ECACANFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF CAST RECORD                *         
*        ON ENTRY ... R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTCHA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         CLI   RQCAMOD,RQCAEXE     IF MODE IS EXECUTE                           
         JNE   INITC10             SAVE ORIGINAL POINTERS                       
         GOTOR (#SAVPTRS,ASAVPTRS) AND DETERMINE IF ORIGINALLY                  
         BRAS  RE,STOELHLD         ELIGIBLE FOR HOLDING FEES                    
                                                                                
         USING TLCAD,R4                                                         
INITC10  CLC   RQCACAT,TLCACAT     IF CATEGORY IS CHANGING                      
         JE    INITC20             SET TO UNVERIFY AND REISSUE HF               
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACAT                                   
         OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
INITC20  GOTOR (#CHKFLD,ACHKFLD),DMCB,(2,RQCAVSQ+2),TLCASRVS,ERCAVSQ,0          
         DROP  R4                                                               
                                                                                
         BRAS  RE,INITVERS         IF USING MASTER/LIFT, INIT VERSIONS          
                                                                                
         LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC30  BRAS  RE,NEXTEL                                                        
         JNE   INITC40                                                          
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TACMELQ       PROCESS COMMENT ELEMENT                      
         JNE   *+12                                                             
         BRAS  RE,CPYTACM                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TARLELQ       PROCESS RELEASE STATUS ELEMENT               
         JNE   *+12                                                             
         BRAS  RE,CPYTARL                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TACAELQ       PROCESS CAST DETAILS ELEMENT                 
         JNE   *+12                                                             
         BRAS  RE,CPYTACA                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TAO2ELQ       PROCESS 2ND OVERSCALE PERCENTAGE             
         JNE   *+12                ELEMENT                                      
         BRAS  RE,CPYTAO2                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TAOAELQ       PROCESS OVERSCALE AMOUNT ELEMENT             
         JNE   *+12                                                             
         BRAS  RE,CPYTAOA                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TAOPELQ       PROCESS OVERSCALE PERCENTAGE ELEMENT         
         JNE   *+12                                                             
         BRAS  RE,CPYTAOP                                                       
         J     INITC30                                                          
                                                                                
         CLI   0(R4),TAFNELQ       PROCESS FREE FORM NAME ELEMENT               
         JNE   INITC30                                                          
         BRAS  RE,CPYTAFN                                                       
         J     INITC30                                                          
                                                                                
INITC40  BRAS  RE,NEWTACM          PROCESS NEW COMMENTS                         
         BRAS  RE,NEWTARL          PROCESS NEW RELEASE STATUS                   
         BRAS  RE,NEWTAOA          PROCESS NEW OVERSCALE AMOUNT                 
         BRAS  RE,NEWTAOP          PROCESS NEW OVERSCALE PERCENT                
         BRAS  RE,NEWTAFN          PROCESS NEW FREE FORM NAME                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCACAT  DC    AL1(ECACATX-*),AL2(8),AL1(ERRCATY2),AL1(D#CACAT)                 
         DC    C'Review update to Category'                                     
ECACATX  EQU   *                                                                
                                                                                
ERCAVSQ  DC    AL1(ECAVSQX-*),AL2(127),AL1(ERRCATY2),AL1(D#CAVSQ)               
         DC    C'Review update to Vita Cast Sequence Number'                    
ECAVSQX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES COMMENT ELEMENT INTO REQUEST MAP              *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(VARIABLE LENGTH COMMENT ELEMENT)         *         
***********************************************************************         
                                                                                
         USING TACMD,R4                                                         
CPYTACM  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         ZIC   RF,TACMLEN          COPY COMMENT ELEMENT INTO ELEM               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
***********************************************************************         
                                                                                
         CLI   TACMTYPE,TACMTYPG   PROCESS GENERAL COMMENT                      
         JNE   CCM10                                                            
         OI    CPYSTAT1,CPYTACMG   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TACMD,R2                                                         
         OC    TACMCOMM(L'RQCACMT),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCACMT,RQCACMT),TACMCOMM,    +        
               ERCACMT,0                                                        
                                                                                
         CLC   RQCACMT,TACMCOMM    IF GENERAL COMMENT HAS CHANGED               
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TACMD,R4                                                         
CCM10    CLI   TACMTYPE,TACMTYPD   PROCESS ROLE DESCRIPTION                     
         JNE   XIT                                                              
         OI    CPYSTAT1,CPYTACMD   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENET                   
         DROP  R4                                                               
                                                                                
         USING TACMD,R2                                                         
         OC    TACMCOMM(L'RQCARDE),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCARDE,RQCARDE),TACMCOMM,    +        
               ERCARDE,0                                                        
                                                                                
         CLC   RQCARDE,TACMCOMM    IF ROLE DESCRIPTION HAS CHANGED              
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF COMMENT ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY        *         
*        AND COMMENT IS BEING ADDED NOW, ROUTINE PROCESSES IT         *         
***********************************************************************         
                                                                                
NEWTACM  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYTACMG   IF GENERAL COMMENT ELEMENT DID NOT           
         JO    NCM10               EXIST ON RECORD PREVIOUSLY                   
         OC    RQCACMT,RQCACMT     AND IS BEING ADDED NOW                       
         JZ    NCM10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCACMT)                       
         OI    COMSTAT,COUNVRFY    AND SET TO UNVERIFY COMMERCIAL               
                                                                                
NCM10    TM    CPYSTAT1,CPYTACMD   IF ROLE DESCRIPTION ELEMENT DID NOT          
         JO    XIT                 EXIST ON RECORD PREVIOUSLY                   
         OC    RQCARDE,RQCARDE     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCARDE)                       
         OI    COMSTAT,COUNVRFY    AND SET TO UNVERIFY COMMERCIAL               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTACM AND NEWTACM                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCACMT  DC    AL1(ECACMTX-*),AL2(70),AL1(ERRCATY2),AL1(D#CACMT)                
         DC    C'Review update to Comment'                                      
ECACMTX  EQU   *                                                                
                                                                                
ERCARDE  DC    AL1(ECARDEX-*),AL2(71),AL1(ERRCATY2),AL1(D#CARDE)                
         DC    C'Review update to Role Description'                             
ECARDEX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES RELEASE STATUS ELEMENT INTO REQUEST MAP       *         
*        ON ENTRY ... R4 = A(RELEASE STATUS ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TARLD,R4                                                         
CPYTARL  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCARLL,RQCARLL),TARLSTAT,    +        
               ERCARLL,(L'RQCALSV,RQCALSV)                                      
         GOTOR (RF),(R1),(L'RQCAEFF,RQCAEFF),TARLEFDT,ERCAEFF,         +        
               (X'FF',RQCARLL),CRLCD                                            
         OI    CPYSTAT1,CPYSTARL                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF RELEASE STATUS ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY *         
*        AND RELEASE STATUS IS BEING ADDED NOW, ROUTINE PROCESSES IT  *         
***********************************************************************         
                                                                                
NEWTARL  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTARL   IF RELEASE STATUS ELEMENT DID NOT            
         JO    XIT                 EXIST ON RECORD PREVIOUSLY                   
         OC    RQCARLL,RQCARLL     AND IS BEING ADDED NOW                       
         JZ    NRL10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCARLL)                       
NRL10    OC    RQCAEFF,RQCAEFF                                                  
         JZ    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCAEFF)                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTARL AND NEWTARL                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCARLL  DC    AL1(ECARLLX-*),AL2(24),AL1(ERRCATY2),AL1(D#CARLL)                
         DC    C'Review update to Release Letter Code'                          
ECARLLX  EQU   *                                                                
                                                                                
ERCAEFF  DC    AL1(ECAEFFX-*),AL2(25),AL1(ERRCATY2),AL1(D#CAEFF)                
         DC    C'Review update to Effective Date'                               
ECAEFFX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
***CRLCD    DC    C'C',C'D',X'FF'                                               
CRLCD    DC    C'A',C'B',C'C',C'D',X'FF'                                        
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES CAST DETAILS ELEMENT INTO REQUEST MAP         *         
*        ON ENTRY ... R4 = A(CAST DETAILS ELEMENT)                    *         
***********************************************************************         
                                                                                
         USING TACAD,R4                                                         
CPYTACA  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCALFI,RQCALFI),TACALFTF,    +        
               ERCALFI,0                                                        
         GOTOR (RF),(R1),(L'RQCAONO,RQCAONO),TACAONOF,ERCAONO,0                 
         GOTOR (RF),(R1),(L'RQCATAX,RQCATAX),TACAUNIT,ERCATAX,0                 
         GOTOR (RF),(R1),(L'RQCAUNI,RQCAUNI),TACAUN,ERCAUNI,0                   
         GOTOR (RF),(R1),(L'RQCALCL,RQCALCL),TACALOCL,ERCALCL,0                 
         GOTOR (RF),(R1),(L'RQCACYR,RQCACYR),TACAYEAR,ERCACYR,0                 
         GOTOR (RF),(R1),(L'RQCAFFC,RQCAFFC),TACAFCYC,ERCAFFC,         +        
               (0,SVCOTYPE),CCAH                                                
         GOTOR (RF),(R1),(L'RQCAFSV,RQCAFSV),TACAFRST,ERCAFSV,0                 
         GOTOR (RF),(R1),(L'RQCALSV,RQCALSV),TACALAST,ERCALSV,0                 
         GOTOR (RF),(R1),(L'SVCORP,SVCORP),TACACORP,ERCAATC,0                   
         GOTOR (RF),(R1),(L'TRNSAGT,TRNSAGT),TACANCDE,ERCAAGT,0                 
         GOTOR (RF),(R1),(L'RQCAGUA,RQCAGUA),TACAGUA,ERCAGUA,0                  
         GOTOR (RF),(R1),(L'RQCAEXP,RQCAEXP),TACAEXP,ERCAEXP,          +        
               (0,SVCOTYPE),CCAH                                                
         GOTOR (RF),(R1),(L'RQCADBL,RQCADBL),TACADBL,ERCADBL,0                  
                                                                                
         OC    RQCA2U1,RQCA2U1                                                  
         JNZ   CCA10                                                            
         GOTOR (RF),(R1),(L'RQCA2OP,RQCA2OP),TACAOV2,ERCA2OP,0                  
                                                                                
CCA10    XC    RQCATCO,RQCATCO                                                  
         XC    RQCATSQ,RQCATSQ                                                  
         CLI   TACALEN,TACALNQ                                                  
         JL    CCA20                                                            
         GOTOR (RF),(R1),(L'RQCARER,RQCARER),TACARERC,ERCARER,0                 
         MVC   RQCATCO,TACATCOM                                                 
         MVC   RQCATSQ,TACATSEQ                                                 
                                                                                
CCA20    GOTOR (#CHKSTAT,ACHKSTAT),DMCB,(0,RQCAMST),                   +        
               ('TACASTLO',TACASTAT),ERCAMST                                    
         GOTOR (RF),(R1),('TACASTLF',RQCALFT),(0,TACASTAT),ERCALFT              
         GOTOR (RF),(R1),('TACASTAG',RQCACPA),(0,TACASTAT),ERCACPA              
         GOTOR (RF),(R1),('TACASTCR',RQCAPCR),(0,TACASTA2),ERCAPCR              
         GOTOR (RF),(R1),('TACASTDP',RQCAPUR),(0,TACASTA2),ERCAPUR              
         GOTOR (RF),(R1),(0,RQCAFGN),('TACASFGR',TACASTA2),ERCAFGN              
         GOTOR (RF),(R1),(0,RQCAINA),('TACASINA',TACASTA2),ERCAINA              
         GOTOR (RF),(R1),(0,RQCAINR),('TACASINR',TACASTA2),ERCAINR              
         GOTOR (RF),(R1),(0,RQCAACP),('TACASXAC',TACASTAT),ERCAACP              
         GOTOR (RF),(R1),(0,RQCAAPP),('TACASXAP',TACASTAT),ERCAAPP              
         GOTOR (RF),(R1),(0,RQCAPDT),('TACASTNF',TACASTAT),ERCAPDT              
         GOTOR (RF),(R1),('TACASCLB',RQCACLB),(0,TACASTA2),ERCACLB              
         GOTOR (RF),(R1),(0,RQCAGRR),('TACASPUS',TACASTA2),ERCAGRR              
         GOTOR (RF),(R1),('TACASEUR',RQCAEUR),(0,TACASTA2),ERCAEUR              
         GOTOR (RF),(R1),(0,RQCAAFT),('TACASXFT',TACASTA3),ERCAAFT              
         GOTOR (RF),(R1),('TACASEQY',RQCAEQY),(0,TACASTA3),ERCAEQY              
                                                                                
         CLC   TACACORP,SVCORP     IF CORPORATION CODE IS CHANGING              
         JE    CCA30                                                            
         OI    CASTSTAT,CANEWCRP   TURN ON STATUS                               
                                                                                
CCA30    CLI   RQCAMOD,RQCAEXE     IF MODE IS EXECUTE                           
         JNE   XIT                                                              
         CLC   TACAONOF,RQCAONO    AND ON/OFF CAMERA IS CHANGING                
         JNE   CCA40                                                            
         CLC   TACAYEAR,RQCACYR    OR CONTRACT YEAR IS CHANGING                 
         JNE   CCA40                                                            
         CLC   TACAFCYC,RQCAFFC    OR FIRST FIXED CYCLE IS CHANGING             
         JNE   CCA40                                                            
         CLC   TACAFRST,RQCAFSV    OR FIRST SERVICES DATE IS CHANGING           
         JNE   CCA40                                                            
         CLC   TACALAST,RQCALSV    OR LAST SERVICES DATE IS CHANGING            
         JNE   CCA40                                                            
         CLC   TACAGUA,RQCAGUA     OR GUARANTEE CODE IS CHANGING                
         JNE   CCA40                                                            
         CLC   TACAEXP,RQCAEXP     OR EXPIRATION DATE IS CHANGING               
         JNE   CCA40                                                            
         CLC   TACAOV2,RQCA2OP     OR OVERSCALE SECOND PCT IS CHANGING          
         JE    XIT                                                              
CCA40    OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTACA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCALFI  DC    AL1(ECALFIX-*),AL2(10),AL1(ERRCATY2),AL1(D#CALFI)                
         DC    C'Review update to Lifted from Commercial'                       
ECALFIX  EQU   *                                                                
                                                                                
ERCAONO  DC    AL1(ECAONOX-*),AL2(12),AL1(ERRCATY2),AL1(D#CAONO)                
         DC    C'Review update to On/Off Camera'                                
ECAONOX  EQU   *                                                                
                                                                                
ERCATAX  DC    AL1(ECATAXX-*),AL2(13),AL1(ERRCATY2),AL1(D#CATAX)                
         DC    C'Review update to Tax Unit Code'                                
ECATAXX  EQU   *                                                                
                                                                                
ERCAUNI  DC    AL1(ECAUNIX-*),AL2(15),AL1(ERRCATY2),AL1(D#CAUNI)                
         DC    C'Review update to Union'                                        
ECAUNIX  EQU   *                                                                
                                                                                
ERCALCL  DC    AL1(ECALCLX-*),AL2(17),AL1(ERRCATY2),AL1(D#CALCL)                
         DC    C'Review update to Local'                                        
ECALCLX  EQU   *                                                                
                                                                                
ERCACYR  DC    AL1(ECACYRX-*),AL2(19),AL1(ERRCATY2),AL1(D#CACYR)                
         DC    C'Review update to Contract Year'                                
ECACYRX  EQU   *                                                                
                                                                                
ERCAFFC  DC    AL1(ECAFFCX-*),AL2(20),AL1(ERRCATY2),AL1(D#CAFFC)                
         DC    C'Review update to First Fixed Cycle'                            
ECAFFCX  EQU   *                                                                
                                                                                
ERCAFSV  DC    AL1(ECAFSVX-*),AL2(21),AL1(ERRCATY2),AL1(D#CAFSV)                
         DC    C'Review update to First Services Date'                          
ECAFSVX  EQU   *                                                                
                                                                                
ERCALSV  DC    AL1(ECALSVX-*),AL2(23),AL1(ERRCATY2),AL1(D#CALSV)                
         DC    C'Review update to Last Services Date'                           
ECALSVX  EQU   *                                                                
                                                                                
ERCAATC  DC    AL1(ECAATCX-*),AL2(27),AL1(ERRCATY2),AL1(D#CAATC)                
         DC    C'Review update to Corporation FID#'                             
ECAATCX  EQU   *                                                                
                                                                                
ERCAAGT  DC    AL1(ECAAGTX-*),AL2(29),AL1(ERRCATY2),AL1(D#CAAGT)                
         DC    C'Review update to Agent Code'                                   
ECAAGTX  EQU   *                                                                
                                                                                
ERCAGUA  DC    AL1(ECAGUAX-*),AL2(34),AL1(ERRCATY2),AL1(D#CAGUA)                
         DC    C'Review update to Guarantee Code'                               
ECAGUAX  EQU   *                                                                
                                                                                
ERCAEXP  DC    AL1(ECAEXPX-*),AL2(35),AL1(ERRCATY2),AL1(D#CAEXP)                
         DC    C'Review update to Expiration Date'                              
ECAEXPX  EQU   *                                                                
                                                                                
ERCA2OP  DC    AL1(ECA2OPX-*),AL2(48),AL1(ERRCATY2),AL1(D#CA2OP)                
         DC    C'Review update to Overscale Second Percentage'                  
ECA2OPX  EQU   *                                                                
                                                                                
ERCADBL  DC    AL1(ECADBLX-*),AL2(49),AL1(ERRCATY2),AL1(D#CADBL)                
         DC    C'Review update to Doubles'                                      
ECADBLX  EQU   *                                                                
                                                                                
ERCARER  DC    AL1(ECARERX-*),AL2(104),AL1(ERRCATY2),AL1(D#CARER)               
         DC    C'Review update to Rerecord Date'                                
ECARERX  EQU   *                                                                
                                                                                
ERCAMST  DC    AL1(ECAMSTX-*),AL2(72),AL1(ERRCATY2),AL1(D#CAMST)                
         DC    C'Review update to Master Commercial Status'                     
ECAMSTX  EQU   *                                                                
                                                                                
ERCALFT  DC    AL1(ECALFTX-*),AL2(73),AL1(ERRCATY2),AL1(D#CALFT)                
         DC    C'Review update to Lift Version Status'                          
ECALFTX  EQU   *                                                                
                                                                                
ERCACPA  DC    AL1(ECACPAX-*),AL2(74),AL1(ERRCATY2),AL1(D#CACPA)                
         DC    C'Review update to Checks Payable to Agent Status'               
ECACPAX  EQU   *                                                                
                                                                                
ERCAPCR  DC    AL1(ECAPCRX-*),AL2(76),AL1(ERRCATY2),AL1(D#CAPCR)                
         DC    C'Review update to Pay Canadian Rates Status'                    
ECAPCRX  EQU   *                                                                
                                                                                
ERCAPUR  DC    AL1(ECAPURX-*),AL2(78),AL1(ERRCATY2),AL1(D#CAPUR)                
         DC    C'Review update to Pay US Rates Status'                          
ECAPURX  EQU   *                                                                
                                                                                
ERCAFGN  DC    AL1(ECAFGNX-*),AL2(79),AL1(ERRCATY2),AL1(D#CAFGN)                
         DC    C'Review update to Foreign Use Status'                           
ECAFGNX  EQU   *                                                                
                                                                                
ERCAINA  DC    AL1(ECAINAX-*),AL2(80),AL1(ERRCATY2),AL1(D#CAINA)                
         DC    C'Review update to Interactive Use Status'                       
ECAINAX  EQU   *                                                                
                                                                                
ERCAINR  DC    AL1(ECAINRX-*),AL2(81),AL1(ERRCATY2),AL1(D#CAINR)                
         DC    C'Review update to Industrial Use Status'                        
ECAINRX  EQU   *                                                                
                                                                                
ERCAACP  DC    AL1(ECAACPX-*),AL2(82),AL1(ERRCATY2),AL1(D#CAACP)                
         DC    C'Review update to Apply Cable to Per Cycle Amount Statu+        
               s'                                                               
ECAACPX  EQU   *                                                                
                                                                                
ERCAAPP  DC    AL1(ECAAPPX-*),AL2(83),AL1(ERRCATY2),AL1(D#CAAPP)                
         DC    C'Review update to Apply PAX to Per Cycle Amount Status'         
ECAAPPX  EQU   *                                                                
                                                                                
ERCAPDT  DC    AL1(ECAPDTX-*),AL2(84),AL1(ERRCATY2),AL1(D#CAPDT)                
         DC    C'Review update to Print Daily Fixed Cycle Tracking Stat+        
               us'                                                              
ECAPDTX  EQU   *                                                                
                                                                                
ERCACLB  DC    AL1(ECACLBX-*),AL2(85),AL1(ERRCATY2),AL1(D#CACLB)                
         DC    C'Review update to Celebrity Status'                             
ECACLBX  EQU   *                                                                
                                                                                
ERCAGRR  DC    AL1(ECAGRRX-*),AL2(86),AL1(ERRCATY2),AL1(D#CAGRR)                
         DC    C'Review update to GRR Covers All Use Status'                    
ECAGRRX  EQU   *                                                                
                                                                                
ERCAEUR  DC    AL1(ECAEURX-*),AL2(93),AL1(ERRCATY2),AL1(D#CAEUR)                
         DC    C'Review update to Pay in Euros Status'                          
ECAEURX  EQU   *                                                                
                                                                                
ERCAAFT  DC    AL1(ECAAFTX-*),AL2(127),AL1(ERRCATY2),AL1(D#CAAFT)               
         DC    C'Review update to Apply to FTracks Status'                      
ECAAFTX  EQU   *                                                                
                                                                                
ERCAEQY  DC    AL1(ECAEQYX-*),AL2(131),AL1(ERRCATY2),AL1(D#CAEQY)               
         DC    C'Review update to Equity Status'                                
ECAEQYX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CCAH     DC    AL1(CTYSEAS2),X'FF'                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES 2ND OVERSCALE PERCENT ELEM INTO REQUEST MAP   *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(2ND OVERSCALE PERCENT ELEMENT)           *         
***********************************************************************         
                                                                                
         USING TAO2D,R4                                                         
CPYTAO2  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           COPY EXISTING 2ND OVERSCALE                  
         ZIC   RF,TAO2LEN          PERCENTAGE ELEMENT INTO ELEM                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),TAO2D                                                    
                                                                                
         OI    CPYSTAT1,CPYSTAO2   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         OC    RQCA2OP,RQCA2OP                                                  
         JNZ   XIT                                                              
                                                                                
         USING TAO2D,R2                                                         
         ZIC   R0,TAO2NUM                                                       
         LA    R2,TAO2SBEL                                                      
         DROP  R2                                                               
                                                                                
         USING TAO2SBEL,R2                                                      
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCA2U1,RQCA2U1),TAO2USE,     +        
               ERCA2U1,0                                                        
         GOTOR (RF),(R1),(L'RQCA2P1,RQCA2P1),TAO2PCT,ERCA2P1,0                  
         CLC   TAO2USE,RQCA2U1     IF 2ND OVERSCALE PERCENTAGE USE #1           
         JNE   CO210                                                            
         CLC   TAO2PCT,RQCA2P1     OR 2ND OVERSCALE PCT #1 IS CHANGING          
         JE    CO220               SET TO UNVERIFY AND REISSUE HF               
CO210    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
CO220    LA    R2,TAO2XSUB                                                      
         GOTOR (RF),(R1),(L'RQCA2U2,RQCA2U2),TAO2USE,ERCA2U2,0                  
         GOTOR (RF),(R1),(L'RQCA2P2,RQCA2P2),TAO2PCT,ERCA2P2,0                  
         CLC   TAO2USE,RQCA2U2     IF 2ND OVERSCALE PERCENTAGE USE #2           
         JNE   CO230                                                            
         CLC   TAO2PCT,RQCA2P2     OR 2ND OVERSCALE PCT #2 IS CHANGING          
         JE    CO240               SET TO UNVERIFY AND REISSUE HF               
CO230    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
CO240    LA    R2,TAO2XSUB                                                      
         GOTOR (RF),(R1),(L'RQCA2U3,RQCA2U3),TAO2USE,ERCA2U3,0                  
         GOTOR (RF),(R1),(L'RQCA2P3,RQCA2P3),TAO2PCT,ERCA2P3,0                  
         CLC   TAO2USE,RQCA2U3     IF 2ND OVERSCALE PERCENTAGE USE #3           
         JNE   CO250                                                            
         CLC   TAO2PCT,RQCA2P3     OR 2ND OVERSCALE PCT #3 IS CHANGING          
         JE    CO260               SET TO UNVERIFY AND REISSUE HF               
CO250    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
CO260    LA    R2,TAO2XSUB                                                      
         GOTOR (RF),(R1),(L'RQCA2U4,RQCA2U4),TAO2USE,ERCA2U4,0                  
         GOTOR (RF),(R1),(L'RQCA2P4,RQCA2P4),TAO2PCT,ERCA2P4,0                  
         CLC   TAO2USE,RQCA2U4     IF 2ND OVERSCALE PERCENTAGE USE #4           
         JNE   CO270                                                            
         CLC   TAO2PCT,RQCA2P4     OR 2ND OVERSCALE PCT #4 IS CHANGING          
         JE    CO280               SET TO UNVERIFY AND REISSUE HF               
CO270    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
CO280    LA    R2,TAO2XSUB                                                      
         GOTOR (RF),(R1),(L'RQCA2U5,RQCA2U5),TAO2USE,ERCA2U5,0                  
         GOTOR (RF),(R1),(L'RQCA2P5,RQCA2P5),TAO2PCT,ERCA2P5,0                  
         CLC   TAO2USE,RQCA2U5     IF OVERSCALE PERCENTAGE USE #5               
         JNE   CO290                                                            
         CLC   TAO2PCT,RQCA2P5     OR OVERSCALE PCT #5 IS CHANGING              
         JE    XIT                 SET TO UNVERIFY AND REISSUE HF               
CO290    OI    COMSTAT,COUNVRFY+COREISSU                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF 2ND OVERSCALE PERCENTAGE ELEMENT DID NOT EXIST ON RECORD  *         
*        PREVIOUSLY AND 2ND OVERSCALE PERCENTAGES ARE BEING ADDED NOW,*         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAO2  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAO2   IF 2ND OVERSCALE PERCENT ELEMENT             
         JO    XIT                 WAS NOT PROCESSED ...                        
                                                                                
         OC    RQCA2U1,RQCA2U1     IF 2ND OVERSCALE PCT IS BEING ADDED          
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCA2U1)                       
         GOTOR (RF),(R1),ERCA2P1                                                
         OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
         OC    RQCA2U2,RQCA2U2     IF 2ND OVERSCALE PCT 2 IS BEING              
         JZ    XIT                 ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (RF),(R1),ERCA2U2                                                
         GOTOR (RF),(R1),ERCA2P2                                                
                                                                                
         OC    RQCA2U3,RQCA2U3     IF 2ND OVERSCALE PCT 3 IS BEING              
         JZ    XIT                 ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (RF),(R1),ERCA2U3                                                
         GOTOR (RF),(R1),ERCA2P3                                                
                                                                                
         OC    RQCA2U4,RQCA2U4     IF 2ND OVERSCALE PCT 4 IS BEING              
         JZ    XIT                 ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (RF),(R1),ERCA2U4                                                
         GOTOR (RF),(R1),ERCA2P4                                                
                                                                                
         OC    RQCA2U5,RQCA2U5     IF 2ND OVERSCALE PCT 5 IS BEING              
         JZ    XIT                 ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (RF),(R1),ERCA2U5                                                
         GOTOR (RF),(R1),ERCA2P5                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAO2 AND NEWTAO2                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCA2U1  DC    AL1(ECAUP1X-*),AL2(94),AL1(ERRCATY2),AL1(D#CA2U1)                
         DC    C'Review update to 2nd Overscale Percentage Use'                 
ECA2U1X  EQU   *                                                                
                                                                                
ERCA2P1  DC    AL1(ECAOP1X-*),AL2(95),AL1(ERRCATY2),AL1(D#CA2P1)                
         DC    C'Review update to 2nd Overscale Percentage'                     
ECA2P1X  EQU   *                                                                
                                                                                
ERCA2U2  DC    AL1(ECAUP2X-*),AL2(96),AL1(ERRCATY2),AL1(D#CA2U2)                
         DC    C'Review update to 2nd Overscale Percentage Use'                 
ECA2U2X  EQU   *                                                                
                                                                                
ERCA2P2  DC    AL1(ECAOP2X-*),AL2(97),AL1(ERRCATY2),AL1(D#CA2P2)                
         DC    C'Review update to 2nd Overscale Percentage'                     
ECA2P2X  EQU   *                                                                
                                                                                
ERCA2U3  DC    AL1(ECAUP3X-*),AL2(98),AL1(ERRCATY2),AL1(D#CA2U3)                
         DC    C'Review update to 2nd Overscale Percentage Use'                 
ECA2U3X  EQU   *                                                                
                                                                                
ERCA2P3  DC    AL1(ECAOP3X-*),AL2(99),AL1(ERRCATY2),AL1(D#CA2P3)                
         DC    C'Review update to 2nd Overscale Percentage'                     
ECA2P3X  EQU   *                                                                
                                                                                
ERCA2U4  DC    AL1(ECAUP4X-*),AL2(100),AL1(ERRCATY2),AL1(D#CA2U4)               
         DC    C'Review update to 2nd Overscale Percentage Use'                 
ECA2U4X  EQU   *                                                                
                                                                                
ERCA2P4  DC    AL1(ECAOP4X-*),AL2(101),AL1(ERRCATY2),AL1(D#CA2P4)               
         DC    C'Review update to 2nd Overscale Percentage'                     
ECA2P4X  EQU   *                                                                
                                                                                
ERCA2U5  DC    AL1(ECAUP5X-*),AL2(102),AL1(ERRCATY2),AL1(D#CA2U5)               
         DC    C'Review update to 2nd Overscale Percentage Use'                 
ECA2U5X  EQU   *                                                                
                                                                                
ERCA2P5  DC    AL1(ECAOP5X-*),AL2(103),AL1(ERRCATY2),AL1(D#CA2P5)               
         DC    C'Review update to 2nd Overscale Percentage'                     
ECA2P5X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES OVERSCALE AMOUNT ELEMENT INTO REQUEST MAP     *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(OVERSCALE AMOUNT ELEMENT)                *         
***********************************************************************         
                                                                                
         USING TAOAD,R4                                                         
CPYTAOA  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           COPY EXISTING OVERSCALE AMOUNT               
         ZIC   RF,TAOALEN          ELEMENT INTO ELEM                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),TAOAD                                                    
                                                                                
         OI    CPYSTAT1,CPYSTAOA   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TAOAD,R2                                                         
         ZIC   R0,TAOANUM                                                       
         LA    R2,TAOASBEL                                                      
         DROP  R2                                                               
                                                                                
         USING TAOASBEL,R2                                                      
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCAUA1,RQCAUA1),TAOAUSE,     +        
               ERCAUA1,0                                                        
         GOTOR (RF),(R1),(L'RQCAOA1,RQCAOA1),TAOAAMT,ERCAOA1,0                  
         CLC   TAOAUSE,RQCAUA1     IF OVERSCALE AMOUNT USE #1                   
         JNE   COA10                                                            
         CLC   TAOAAMT,RQCAOA1     OR OVERSCALE AMOUNT #1 IS CHANGING           
         JE    COA20               SET TO UNVERIFY AND REISSUE HF               
COA10    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COA20    LA    R2,TAOAXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUA2,RQCAUA2),TAOAUSE,ERCAUA2,0                  
         GOTOR (RF),(R1),(L'RQCAOA2,RQCAOA2),TAOAAMT,ERCAOA2,0                  
         CLC   TAOAUSE,RQCAUA2     IF OVERSCALE AMOUNT USE #2                   
         JNE   COA30                                                            
         CLC   TAOAAMT,RQCAOA2     OR OVERSCALE AMOUNT #2 IS CHANGING           
         JE    COA40               SET TO UNVERIFY AND REISSUE HF               
COA30    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COA40    LA    R2,TAOAXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUA3,RQCAUA3),TAOAUSE,ERCAUA3,0                  
         GOTOR (RF),(R1),(L'RQCAOA3,RQCAOA3),TAOAAMT,ERCAOA3,0                  
         CLC   TAOAUSE,RQCAUA3     IF OVERSCALE AMOUNT USE #3                   
         JNE   COA50                                                            
         CLC   TAOAAMT,RQCAOA3     OR OVERSCALE AMOUNT #3 IS CHANGING           
         JE    COA60               SET TO UNVERIFY AND REISSUE HF               
COA50    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COA60    LA    R2,TAOAXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUA4,RQCAUA4),TAOAUSE,ERCAUA4,0                  
         GOTOR (RF),(R1),(L'RQCAOA4,RQCAOA4),TAOAAMT,ERCAOA4,0                  
         CLC   TAOAUSE,RQCAUA4     IF OVERSCALE AMOUNT USE #4                   
         JNE   COA70                                                            
         CLC   TAOAAMT,RQCAOA4     OR OVERSCALE AMOUNT #4 IS CHANGING           
         JE    COA80               SET TO UNVERIFY AND REISSUE HF               
COA70    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COA80    LA    R2,TAOAXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUA5,RQCAUA5),TAOAUSE,ERCAUA5,0                  
         GOTOR (RF),(R1),(L'RQCAOA5,RQCAOA5),TAOAAMT,ERCAOA5,0                  
         CLC   TAOAUSE,RQCAUA5     IF OVERSCALE AMOUNT USE #5                   
         JNE   COA90                                                            
         CLC   TAOAAMT,RQCAOA5     OR OVERSCALE AMOUNT #5 IS CHANGING           
         JE    COA100              SET TO UNVERIFY AND REISSUE HF               
COA90    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COA100   LA    R2,TAOAXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUA6,RQCAUA6),TAOAUSE,ERCAUA6,0                  
         GOTOR (RF),(R1),(L'RQCAOA6,RQCAOA6),TAOAAMT,ERCAOA6,0                  
         CLC   TAOAUSE,RQCAUA6     IF OVERSCALE AMOUNT USE #6                   
         JNE   COA110                                                           
         CLC   TAOAAMT,RQCAOA6     OR OVERSCALE AMOUNT #6 IS CHANGING           
         JE    XIT                 SET TO UNVERIFY AND REISSUE HF               
COA110   OI    COMSTAT,COUNVRFY+COREISSU                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF OVERSCALE AMOUNT ELEMENT DID NOT EXIST ON RECORD          *         
*        PREVIOUSLY AND OVERSCALE AMOUNTS ARE BEING ADDED NOW,        *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAOA  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAOA   IF OVERSCALE AMOUNTS ELEMENT WAS             
         JO    XIT                 NOT PROCESSED ...                            
                                                                                
         OC    RQCAUA1,RQCAUA1     IF OVERSCALE AMOUNT 1 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCAUA1)                       
         GOTOR (RF),(R1),ERCAOA1                                                
         OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
         OC    RQCAUA2,RQCAUA2     IF OVERSCALE AMOUNT 2 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUA2                                                
         GOTOR (RF),(R1),ERCAOA2                                                
                                                                                
         OC    RQCAUA3,RQCAUA3     IF OVERSCALE AMOUNT 3 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUA3                                                
         GOTOR (RF),(R1),ERCAOA3                                                
                                                                                
         OC    RQCAUA4,RQCAUA4     IF OVERSCALE AMOUNT 4 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUA4                                                
         GOTOR (RF),(R1),ERCAOA4                                                
                                                                                
         OC    RQCAUA5,RQCAUA5     IF OVERSCALE AMOUNT 5 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUA5                                                
         GOTOR (RF),(R1),ERCAOA5                                                
                                                                                
         OC    RQCAUA6,RQCAUA6     IF OVERSCALE AMOUNT 6 IS BEING ADDED         
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUA6                                                
         GOTOR (RF),(R1),ERCAOA6                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAOA AND NEWTAOA                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAUA1  DC    AL1(ECAUA1X-*),AL2(50),AL1(ERRCATY2),AL1(D#CAUA1)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA1X  EQU   *                                                                
                                                                                
ERCAOA1  DC    AL1(ECAOA1X-*),AL2(51),AL1(ERRCATY2),AL1(D#CAOA1)                
         DC    C'Review update to Overscale Amount'                             
ECAOA1X  EQU   *                                                                
                                                                                
ERCAUA2  DC    AL1(ECAUA2X-*),AL2(52),AL1(ERRCATY2),AL1(D#CAUA2)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA2X  EQU   *                                                                
                                                                                
ERCAOA2  DC    AL1(ECAOA2X-*),AL2(53),AL1(ERRCATY2),AL1(D#CAOA2)                
         DC    C'Review update to Overscale Amount'                             
ECAOA2X  EQU   *                                                                
                                                                                
ERCAUA3  DC    AL1(ECAUA3X-*),AL2(54),AL1(ERRCATY2),AL1(D#CAUA3)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA3X  EQU   *                                                                
                                                                                
ERCAOA3  DC    AL1(ECAOA3X-*),AL2(55),AL1(ERRCATY2),AL1(D#CAOA3)                
         DC    C'Review update to Overscale Amount'                             
ECAOA3X  EQU   *                                                                
                                                                                
ERCAUA4  DC    AL1(ECAUA4X-*),AL2(56),AL1(ERRCATY2),AL1(D#CAUA4)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA4X  EQU   *                                                                
                                                                                
ERCAOA4  DC    AL1(ECAOA4X-*),AL2(57),AL1(ERRCATY2),AL1(D#CAOA4)                
         DC    C'Review update to Overscale Amount'                             
ECAOA4X  EQU   *                                                                
                                                                                
ERCAUA5  DC    AL1(ECAUA5X-*),AL2(58),AL1(ERRCATY2),AL1(D#CAUA5)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA5X  EQU   *                                                                
                                                                                
ERCAOA5  DC    AL1(ECAOA5X-*),AL2(59),AL1(ERRCATY2),AL1(D#CAOA5)                
         DC    C'Review update to Overscale Amount'                             
ECAOA5X  EQU   *                                                                
                                                                                
ERCAUA6  DC    AL1(ECAUA6X-*),AL2(60),AL1(ERRCATY2),AL1(D#CAUA6)                
         DC    C'Review update to Overscale Amount Use'                         
ECAUA6X  EQU   *                                                                
                                                                                
ERCAOA6  DC    AL1(ECAOA6X-*),AL2(61),AL1(ERRCATY2),AL1(D#CAOA6)                
         DC    C'Review update to Overscale Amount'                             
ECAOA6X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES OVERSCALE PERCENT ELEMENT INTO REQUEST MAP    *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(OVERSCALE PERCENT ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAOPD,R4                                                         
CPYTAOP  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           COPY EXISTING OVERSCALE PERCENTAGE           
         ZIC   RF,TAOPLEN          ELEMENT INTO ELEM                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),TAOPD                                                    
                                                                                
         OI    CPYSTAT1,CPYSTAOP   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TAOPD,R2                                                         
         ZIC   R0,TAOPNUM                                                       
         LA    R2,TAOPSBEL                                                      
         DROP  R2                                                               
                                                                                
         USING TAOPSBEL,R2                                                      
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCAUP1,RQCAUP1),TAOPUSE,     +        
               ERCAUP1,0                                                        
         GOTOR (RF),(R1),(L'RQCAOP1,RQCAOP1),TAOPPCT,ERCAOP1,0                  
         CLC   TAOPUSE,RQCAUP1     IF OVERSCALE PERCENTAGE USE #1               
         JNE   COP10                                                            
         CLC   TAOPPCT,RQCAOP1     OR OVERSCALE PCT #1 IS CHANGING              
         JE    COP20               SET TO UNVERIFY AND REISSUE HF               
COP10    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COP20    LA    R2,TAOPXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUP2,RQCAUP2),TAOPUSE,ERCAUP2,0                  
         GOTOR (RF),(R1),(L'RQCAOP2,RQCAOP2),TAOPPCT,ERCAOP2,0                  
         CLC   TAOPUSE,RQCAUP2     IF OVERSCALE PERCENTAGE USE #2               
         JNE   COP30                                                            
         CLC   TAOPPCT,RQCAOP2     OR OVERSCALE PCT #2 IS CHANGING              
         JE    COP40               SET TO UNVERIFY AND REISSUE HF               
COP30    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COP40    LA    R2,TAOPXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUP3,RQCAUP3),TAOPUSE,ERCAUP3,0                  
         GOTOR (RF),(R1),(L'RQCAOP3,RQCAOP3),TAOPPCT,ERCAOP3,0                  
         CLC   TAOPUSE,RQCAUP3     IF OVERSCALE PERCENTAGE USE #3               
         JNE   COP50                                                            
         CLC   TAOPPCT,RQCAOP3     OR OVERSCALE PCT #3 IS CHANGING              
         JE    COP60               SET TO UNVERIFY AND REISSUE HF               
COP50    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COP60    LA    R2,TAOPXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUP4,RQCAUP4),TAOPUSE,ERCAUP4,0                  
         GOTOR (RF),(R1),(L'RQCAOP4,RQCAOP4),TAOPPCT,ERCAOP4,0                  
         CLC   TAOPUSE,RQCAUP4     IF OVERSCALE PERCENTAGE USE #4               
         JNE   COP70                                                            
         CLC   TAOPPCT,RQCAOP4     OR OVERSCALE PCT #4 IS CHANGING              
         JE    COP80               SET TO UNVERIFY AND REISSUE HF               
COP70    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COP80    LA    R2,TAOPXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUP5,RQCAUP5),TAOPUSE,ERCAUP5,0                  
         GOTOR (RF),(R1),(L'RQCAOP5,RQCAOP5),TAOPPCT,ERCAOP5,0                  
         CLC   TAOPUSE,RQCAUP5     IF OVERSCALE PERCENTAGE USE #5               
         JNE   COP90                                                            
         CLC   TAOPPCT,RQCAOP5     OR OVERSCALE PCT #5 IS CHANGING              
         JE    COP100              SET TO UNVERIFY AND REISSUE HF               
COP90    OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
COP100   LA    R2,TAOPXSUB                                                      
         GOTOR (RF),(R1),(L'RQCAUP6,RQCAUP6),TAOPUSE,ERCAUP6,0                  
         GOTOR (RF),(R1),(L'RQCAOP6,RQCAOP6),TAOPPCT,ERCAOP6,0                  
         CLC   TAOPUSE,RQCAUP6     IF OVERSCALE PERCENTAGE USE #6               
         JNE   COP110                                                           
         CLC   TAOPPCT,RQCAOP6     OR OVERSCALE PCT #6 IS CHANGING              
         JE    XIT                 SET TO UNVERIFY AND REISSUE HF               
COP110   OI    COMSTAT,COUNVRFY+COREISSU                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF OVERSCALE PERCENTAGE ELEMENT DID NOT EXIST ON RECORD      *         
*        PREVIOUSLY AND OVERSCALE PERCENTAGES ARE BEING ADDED NOW,    *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAOP  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAOP   IF OVERSCALE PERCENTAGE ELEMENT WAS          
         JO    XIT                 NOT PROCESSED ...                            
                                                                                
         OC    RQCAUP1,RQCAUP1     IF OVERSCALE PCT 1 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCAUP1)                       
         GOTOR (RF),(R1),ERCAOP1                                                
         OI    COMSTAT,COUNVRFY+COREISSU                                        
                                                                                
         OC    RQCAUP2,RQCAUP2     IF OVERSCALE PCT 2 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUP2                                                
         GOTOR (RF),(R1),ERCAOP2                                                
                                                                                
         OC    RQCAUP3,RQCAUP3     IF OVERSCALE PCT 3 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUP3                                                
         GOTOR (RF),(R1),ERCAOP3                                                
                                                                                
         OC    RQCAUP4,RQCAUP4     IF OVERSCALE PCT 4 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUP4                                                
         GOTOR (RF),(R1),ERCAOP4                                                
                                                                                
         OC    RQCAUP5,RQCAUP5     IF OVERSCALE PCT 5 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUP5                                                
         GOTOR (RF),(R1),ERCAOP5                                                
                                                                                
         OC    RQCAUP6,RQCAUP6     IF OVERSCALE PCT 6 IS BEING ADDED            
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERCAUP6                                                
         GOTOR (RF),(R1),ERCAOP6                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAOP AND NEWTAOP                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAUP1  DC    AL1(ECAUP1X-*),AL2(36),AL1(ERRCATY2),AL1(D#CAUP1)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP1X  EQU   *                                                                
                                                                                
ERCAOP1  DC    AL1(ECAOP1X-*),AL2(37),AL1(ERRCATY2),AL1(D#CAOP1)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP1X  EQU   *                                                                
                                                                                
ERCAUP2  DC    AL1(ECAUP2X-*),AL2(38),AL1(ERRCATY2),AL1(D#CAUP2)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP2X  EQU   *                                                                
                                                                                
ERCAOP2  DC    AL1(ECAOP2X-*),AL2(39),AL1(ERRCATY2),AL1(D#CAOP2)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP2X  EQU   *                                                                
                                                                                
ERCAUP3  DC    AL1(ECAUP3X-*),AL2(40),AL1(ERRCATY2),AL1(D#CAUP3)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP3X  EQU   *                                                                
                                                                                
ERCAOP3  DC    AL1(ECAOP3X-*),AL2(41),AL1(ERRCATY2),AL1(D#CAOP3)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP3X  EQU   *                                                                
                                                                                
ERCAUP4  DC    AL1(ECAUP4X-*),AL2(42),AL1(ERRCATY2),AL1(D#CAUP4)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP4X  EQU   *                                                                
                                                                                
ERCAOP4  DC    AL1(ECAOP4X-*),AL2(43),AL1(ERRCATY2),AL1(D#CAOP4)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP4X  EQU   *                                                                
                                                                                
ERCAUP5  DC    AL1(ECAUP5X-*),AL2(44),AL1(ERRCATY2),AL1(D#CAUP5)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP5X  EQU   *                                                                
                                                                                
ERCAOP5  DC    AL1(ECAOP5X-*),AL2(45),AL1(ERRCATY2),AL1(D#CAOP5)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP5X  EQU   *                                                                
                                                                                
ERCAUP6  DC    AL1(ECAUP6X-*),AL2(46),AL1(ERRCATY2),AL1(D#CAUP6)                
         DC    C'Review update to Overscale Percentage Use'                     
ECAUP6X  EQU   *                                                                
                                                                                
ERCAOP6  DC    AL1(ECAOP6X-*),AL2(47),AL1(ERRCATY2),AL1(D#CAOP6)                
         DC    C'Review update to Overscale Percentage'                         
ECAOP6X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF USING "ON MASTER COMMERCIAL?" OR "ON LIFT VERSION?"       *         
*        FIELDS, INITIALIZE VERSION FIELD                             *         
*        ON ENTRY ... R4 = A(CAST RECORD)                             *         
***********************************************************************         
                                                                                
INITVERS NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSTAT,C'Y'        IF COMMERCIAL HAS VERSIONS                   
         JNE   XIT                                                              
         OC    RQCAMST(2),RQCAMST  AND USING "ON MASTER COMMERCIAL?" OR         
         JZ    XIT                 "ON LIFT VERSION?" FIELDS                    
                                                                                
         USING TACAD,R2                                                         
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LR    R2,R4               R2=A(CAST DETAILS ELEMENT)                   
                                                                                
         XC    ORIGVERS,ORIGVERS   CLEAR ORIGINAL VERSIONS                      
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO3),('TAFNTVER',0)         
         JNE   IV20                                                             
         L     R4,AELEM                                                         
         CLI   TAFNNAME,251        IF ORIGINALLY ATTACHED TO                    
         JNE   IV10                ALL VERSIONS                                 
         OI    TACASTAT,TACASTLF   TURN ON ORIGINAL "ON LIFT?" STATUS           
         CLI   RQCAMST,C'Y'                                                     
         JNE   IV20                                                             
         CLI   RQCALFT,C'Y'                                                     
         JNE   IV20                IF REMAINING ON ALL VERSIONS                 
         MVI   VERSIONS,251        BUILD VERSIONS AS ALL                        
         J     XIT                 AND EXIT                                     
                                                                                
IV10     ZIC   RF,TAFNLEN          COPY ORIGINAL VERSIONS                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ORIGVERS(0),TAFNNAME                                             
         DROP  R4                                                               
                                                                                
         CLC   ORIGVERS,=X'12'     IF CAST ORIGINALLY ON VERSIONS               
         JNE   *+8                 1 AND 2, TURN ON ORIGINAL "ON LIFT"          
         OI    TACASTAT,TACASTLF   STATUS                                       
                                                                                
         CLI   ORIGVERS,2          IF CAST ORIGINALLY ON VERSION 2              
         JNE   *+8                 BUT NOT 2, TURN ON ORIGINAL "ONLY            
         OI    TACASTAT,TACASTLF+TACASTLO            ON LIFT" STATUS            
                                                                                
IV20     LA    R2,VERSIONS                                                      
         LA    R3,ORIGVERS                                                      
                                                                                
         CLI   RQCAMST,C'Y'                                                     
         JNE   IV30                                                             
         MVI   0(R2),1                                                          
         LA    R2,1(R2)                                                         
                                                                                
IV30     CLI   RQCALFT,C'Y'                                                     
         JNE   IV40                                                             
         MVI   0(R2),2                                                          
         LA    R2,1(R2)                                                         
                                                                                
IV40     CLI   0(R3),0                                                          
         JE    XIT                                                              
         CLI   0(R3),2                                                          
         JNH   IV50                                                             
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,1(R2)                                                         
IV50     LA    R3,1(R3)                                                         
         J     IV40                                                             
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FREE FORM NAME ELEMENT INTO REQUEST MAP       *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(FREE FORM NAME ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
CPYTAFN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAFNTYPE,TAFNTOWB   IF TYPE IS ORIGINAL WEB ID                   
         JNE   CFN10                                                            
         MVI   0(R4),X'FF'         SET TO DELETE                                
         J     XIT                                                              
                                                                                
CFN10    CLI   TAFNTYPE,TAFNTWEB   IF TYPE IS WEB ID                            
         JNE   CFN20                                                            
         MVC   SVWID,TAFNNAME      SAVE INITIAL WEB APPLICATION ID              
         MVI   0(R4),X'FF'         AND SET TO DELETE                            
         J     XIT                                                              
                                                                                
CFN20    CLI   TAFNTYPE,TAFNTWRI   IF TYPE IS WEB RECORD ID                     
         JNE   CFN30                                                            
         MVI   0(R4),X'FF'         SET TO DELETE                                
         J     XIT                                                              
                                                                                
CFN30    CLI   TAFNTYPE,TAFNTTRK   IF TYPE IS TRACKS                            
         JNE   CFN40                                                            
         OI    CPYSTAT1,CPYTAFNK   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAFNLEN          COPY FREE FORM NAME ELEMENT INTO             
         BCTR  RF,0                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         DROP  R4                                                               
                                                                                
         USING TAFND,R2                                                         
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCATR1,RQCATR1),TAFNNAME,    +        
               ERCATR1,0                                                        
         GOTOR (RF),(R1),(L'RQCATR2,RQCATR2),TAFNNAME+1,ERCATR2,       +        
               (0,RQCATR1),CFNALL                                               
         GOTOR (RF),(R1),(L'RQCATR3,RQCATR3),TAFNNAME+2,ERCATR3,       +        
               (0,RQCATR1),CFNALL                                               
         GOTOR (RF),(R1),(L'RQCATR4,RQCATR4),TAFNNAME+3,ERCATR4,       +        
               (0,RQCATR1),CFNALL                                               
                                                                                
         CLC   TAFNNAME(4),RQCATR1 IF ANY TRACKS HAVE CHANGED                   
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
         USING TAFND,R4                                                         
CFN40    CLI   TAFNTYPE,TAFNTVER   IF TYPE IS VERSIONS                          
         JNE   XIT                                                              
         OI    CPYSTAT1,CPYTAFNV   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
                                                                                
         CLI   VERSTAT,C'Y'                                                     
         JNE   XIT                                                              
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAFNLEN          COPY FREE FORM NAME ELEMENT INTO             
         SHI   RF,4                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),TAFNNAME                                                 
         DROP  R4                                                               
                                                                                
         OC    RQCAMST(2),RQCAMST                                               
         JNZ   CFN50                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'VERSIONS,VERSIONS),ELEM,      *        
               ERCAVER,0                                                        
                                                                                
CFN50    CLC   VERSIONS,ELEM       IF ANY VERSIONS HAVE CHANGED                 
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        IF FREE FORM NAME ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY *         
*        AND TRACKS ARE BEING ADDED NOW, ROUTINE PROCESSES THEM       *         
***********************************************************************         
                                                                                
NEWTAFN  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYTAFNK   IF TRACKS ELEMENT DID NOT EXIST              
         JO    NFN10               ON RECORD PREVIOUSLY ...                     
                                                                                
         OC    RQCATR1,RQCATR1     IF TRACK #1 IS BEING ADDED NOW               
         JZ    NFN10               RETURN PROMPTS FOR VERIFICATION              
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCATR1)                       
         OI    COMSTAT,COUNVRFY    AND SET TO UNVERIFY COMMERCIAL               
                                                                                
         OC    RQCATR2,RQCATR2     IF TRACK #2 IS BEING ADDED NOW               
         JZ    NFN10               RETURN PROMPTS FOR VERIFICATION              
         GOTOR (RF),(R1),ERCATR2                                                
                                                                                
         OC    RQCATR3,RQCATR3     IF TRACK #3 IS BEING ADDED NOW               
         JZ    NFN10               RETURN PROMPTS FOR VERIFICATION              
         GOTOR (RF),(R1),DMCB,ERCATR3                                           
                                                                                
         OC    RQCATR4,RQCATR4     IF TRACK #4 IS BEING ADDED NOW               
         JZ    NFN10               RETURN PROMPTS FOR VERIFICATION              
         GOTOR (RF),(R1),DMCB,ERCATR4                                           
                                                                                
NFN10    TM    CPYSTAT1,CPYTAFNV   IF VERSIONS ELEMENT DID NOT EXIST            
         JO    XIT                 ON RECORD PREVIOUSLY                         
         CLI   VERSIONS,0          AND ARE BEING ADDED NOW                      
         JE    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ERCAVER)                       
         OI    COMSTAT,COUNVRFY    AND SET TO UNVERIFY COMMERCIAL               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAFN AND NEWTAFN                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCATR1  DC    AL1(ECATR1X-*),AL2(63),AL1(ERRCATY2),AL1(D#CATR1)                
         DC    C'Review update to Track #1'                                     
ECATR1X  EQU   *                                                                
                                                                                
ERCATR2  DC    AL1(ECATR2X-*),AL2(65),AL1(ERRCATY2),AL1(D#CATR2)                
         DC    C'Review update to Track #2'                                     
ECATR2X  EQU   *                                                                
                                                                                
ERCATR3  DC    AL1(ECATR3X-*),AL2(67),AL1(ERRCATY2),AL1(D#CATR3)                
         DC    C'Review update to Track #3'                                     
ECATR3X  EQU   *                                                                
                                                                                
ERCATR4  DC    AL1(ECATR4X-*),AL2(69),AL1(ERRCATY2),AL1(D#CATR4)                
         DC    C'Review update to Track #4'                                     
ECATR4X  EQU   *                                                                
                                                                                
ERCAVER  DC    AL1(ECAVERX-*),AL2(88),AL1(ERRCATY2),AL1(D#CAVER)                
         DC    C'Review update to Versions'                                     
ECAVERX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CFNALL   DC    C'*',X'FF'                                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*       SET PASSED WEB APPLICATION ID                                 *         
*       ON ENTRY ... R4=A(CAST RECORD)                                *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
SETWID   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAFNELQ      READ ALL FREE FORM NUMBER ELEMENTS           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SWID10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLI   TAFNTYPE,TAFNTWEB   IF TYPE IS WEB ID                            
         JNE   SWID10                                                           
         MVC   SVWID,TAFNNAME      SAVE INITIAL WEB APPLICATION ID              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED WEB APPLICATION ID                           *         
***********************************************************************         
                                                                                
VALWID   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'VS',RQCAWID      IF COMING FROM VITA SESSION ...              
         JE    VWID10                                                           
         CLC   =C'TS',RQCAWID                                                   
         JE    VWID10                                                           
         CLC   =C'RS',RQCAWID                                                   
         JNE   YES                                                              
                                                                                
VWID10   OC    SVWID,SVWID         ENSURE CAST WAS LAST UPDATED                 
         JNZ   VWID20              BY VITA SESSIONS                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAOVIT                                  
         J     NO                                                               
                                                                                
VWID20   CLC   RQCAWID,SVWID       ENSURE CAST WAS LAST UPDATED                 
         JE    YES                 FROM THIS SESSION                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAOVIS                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALWID                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAOVIT DC    AL1(ECAOVITX-*),AL2(91),AL1(ERRCATY3),AL1(D#CAWID)               
         DC    C'Cast editable in mainframe only'                               
ECAOVITX EQU   *                                                                
                                                                                
ERCAOVIS DC    AL1(ECAOVISX-*),AL2(92),AL1(ERRCATY3),AL1(D#CAWID)               
         DC    C'Performer was updated from Vita Completion'                    
ECAOVISX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ATTACHED CORPORATION                                *         
*        ON ENTRY ... AIO2=A(W4 RECORD)                               *         
***********************************************************************         
                                                                                
VALATC   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAATC,RQCAATC     IF ATTACHED CORPORATION HAS                  
         JZ    XIT                 BEEN PASSED                                  
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TATIELQ      READ THROUGH ALL TAX ID ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
VA10     BRAS  RE,NEXTEL                                                        
         JNE   VA20                                                             
         CLI   TATITYPE,TATITYCO                                                
         JNE   VA10                                                             
         CLC   RQCAATC,TATIID      IF FOUND, SAVE CORPORATION NUMBER            
         JNE   VA10                ELSE, RETURN ERROR                           
         MVC   SVCORP,TATICRPN                                                  
         JE    XIT                                                              
VA20     GOTOR (#ADDERR,AADDERR),DMCB,ERCAINCP                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALATC                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAINCP DC    AL1(ECAINCPX-*),AL2(26),AL1(ERRCATY1),AL1(D#CAATC)               
         DC    C'Corporation is not attached to performer'                      
ECAINCPX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ATTACHED CORPORATION LOCKED STATUS                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALATCLK NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAATC,RQCAATC     IF ATTACHED CORPORATION HAS                  
         JZ    XIT                 BEEN PASSED                                  
         TM    CASTSTAT,CANEWCRP   AND IS BEING CHANGED                         
         JZ    XIT                                                              
         TM    SVSYSTAT,TASYSECL   AND ENFORCING LOCKING RULES AT               
         JZ    XIT                 CORP LEVEL                                   
                                                                                
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY     GET CORPORATION'S W4                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,RQCAATC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAW4STA2,TAW4SLCK                                                
         JZ    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCACPLK                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALATCLK                                   *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCACPLK DC    AL1(ECACPLKX-*),AL2(105),AL1(ERRCATY1),AL1(D#CAATC)              
         DC    C'Corporation is locked'                                         
ECACPLKX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE GUARANTEE CODE                                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALGUA   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCAGUA,RQCAGUA     ONLY VALIDATE IF GUARANTEE CODE              
         JZ    XIT                 HAS BEEN PASSED                              
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE KEY/RECORD                    
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,RQCASSN                                                  
         MVC   TLGUGUA,RQCAGUA                                                  
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    VG10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAGUNF                                  
         J     XIT                                                              
VG10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAGUELQ      R4=A(GUARANTEE DETAILS ELEMENT)              
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LR    R2,R4                                                            
                                                                                
         TM    TAGUSTAT,TAGUSLCK   ENSURE GUARANTEE IS NOT LOCKED               
         JZ    VG20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAGULK                                  
                                                                                
VG20     CLC   TAGUCRP,SVCORP      ENSURE CORPORATION CODE MATCHES              
         JE    VG30                CAST CORPORATION CODE                        
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAGUIC                                  
                                                                                
VG30     OC    TAGUAGY,TAGUAGY     ENSURE GUARANTEE'S AGENCY/CLIENT             
         JZ    VG70                IS VALID FOR GUARANTEE                       
         CLC   TAGUAGY,SVAGY                                                    
         JNE   VG60                                                             
         OC    TAGUCLI,TAGUCLI                                                  
         JZ    VG70                                                             
         CLC   TAGUCLI,SVCLI                                                    
         JE    VG70                                                             
                                                                                
         USING TAVAD,R2                                                         
VG40     CLI   0(R2),TAVAELQ                                                    
         JNE   VG60                                                             
                                                                                
         CLC   TAVAAGY,SVAGY                                                    
         JNE   VG60                                                             
         CLI   TAVALEN,TAVALNQ                                                  
         JE    VG70                                                             
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
VG50     CLC   SVCLI,0(RF)                                                      
         JE    VG70                                                             
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   VG50                                                             
                                                                                
VG60     ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),0                                                          
         JNE   VG40                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAGUAC                                  
         DROP  R2                                                               
                                                                                
VG70     OC    RQCALSV,RQCALSV     IF LAST SERVICES DATE HAS BEEN               
         JZ    XIT                 PASSED                                       
         CLC   RQCACOM,TAGUCOM     AND THIS IS GUARANTEE'S PRIMARY              
         JNE   XIT                 COMMERCIAL ...                               
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   READ ALL CAST KEYS/RECORDS ATTACHED          
         MVI   TLCAPCD,TLCAGCDQ    ATTACHED TO THE GUARANTEE                    
         MVC   TLCAGSSN,RQCASSN                                                 
         MVC   TLCAGGUA,RQCAGUA                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     VG90                                                             
VG80     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
VG90     CLI   TLCAPCD,TLCAGCDQ                                                 
         JNE   XIT                                                              
         CLC   TLCAPKEY(TLCAGCOM-TLCAPD),IOKEYSAV                               
         JNE   XIT                                                              
         CLC   TLCAGCOM,RQCACOM    (SKIP THE PRIMARY COMMERCIAL)                
         JE    VG80                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ      R4=A(CAST DETAILS ELEMENT)                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST   IF LAST SERVICED, GO READ NEXT               
         JNZ   VG80                ATTACHED CAST                                
         DROP  R4                                                               
                                                                                
         MVC   SVCAHKEY(L'TLCAPKEY),IOKEY SAVE ATTACHED-GRT CAST KEY            
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR ATTACHED COMMERCIAL                 
         MVI   TLCOPCD,TLCOCCDQ    KEY/RECORD                                   
         MVC   TLCOCCOM,SVCAHKEY+TLCAGCOM-TLCAPD                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JNE   VG100                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL DETAILS ELEMENT)             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTAT,TACOSTRL   IF ATTACHED COMMERCIAL IS NOT                
         JO    VG100               RELEASED, DO NOT ALLOW RELEASE               
         GOTOR (#ADDERR,AADDERR),DMCB,ERCAGULS                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VG100    MVC   IOKEY,SVCAHKEY      RESTORE CAST READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     VG80                                                             
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALGUA                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCAGUNF DC    AL1(ECAGUNFX-*),AL2(30),AL1(ERRCATY1),AL1(D#CAGUA)               
         DC    C'Guarantee record is not on file'                               
ECAGUNFX EQU   *                                                                
                                                                                
ERCAGULK DC    AL1(ECAGULKX-*),AL2(31),AL1(ERRCATY1),AL1(D#CAGUA)               
         DC    C'Guarantee record is locked'                                    
ECAGULKX EQU   *                                                                
                                                                                
ERCAGUIC DC    AL1(ECAGUICX-*),AL2(32),AL1(ERRCATY1),AL1(D#CAGUA)               
         DC    C'Invalid guarantee for the attached corporation'                
ECAGUICX EQU   *                                                                
                                                                                
ERCAGUAC DC    AL1(ECAGUACX-*),AL2(33),AL1(ERRCATY1),AL1(D#CAGUA)               
         DC    C'Invalid Guarantee for the Commercial''s Agency/Client'         
ECAGUACX EQU   *                                                                
                                                                                
ERCAGULS DC    AL1(ECAGULSX-*),AL2(22),AL1(ERRCATY1),AL1(D#CALSV)               
         DC    C'Cannot release Primary Commercial for Per Cycle Guaran+        
                 tee'                                                           
ECAGULSX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TRANSFERRED FIXED CYCLE FIELDS                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALTRN   NTR1  BASE=*,LABEL=*                                                   
         OC    RQCATCO,RQCATCO     ONLY VALIDATE IF TRANSFERRED INFO            
         JZ    XIT                 HAS BEEN PASSED                              
         CLI   ACTION,ACTADD       AND ACTION IS ADD                            
         JNE   XIT                                                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   ENSURE TRANSFERRED FROM COMMERCIAL           
         MVI   TLCOPCD,TLCOCCDQ    IS ON FILE                                   
         MVC   TLCOCCOM,RQCATCO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    VT10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATONF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VT10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO4                                                          
         MVC   TRNFRAGY,TLCOAGY                                                 
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      R4=A(COMMERCIAL DETAILS ELEMENT)             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,FULL2)                                     
                                                                                
         OC    TACOEXP,TACOEXP     ENSURE COMMERCIAL'S EXPIRATION DATE          
         JZ    VT20                HAS NOT PASSED                               
         CLC   TACOEXP,FULL2                                                    
         JH    VT20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATOEX                                  
                                                                                
VT20     TM    TACOSTAT,TACOSTRL   ENSURE COMMERCIAL IS NOT RELEASED            
         JZ    VT30                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATORL                                  
                                                                                
VT30     MVC   TRNFRCID,TACOCID                                                 
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCACCDQ    ENSURE TRANSFERRED FROM CAST                 
         MVC   TLCACSSN,RQCASSN    IS ON FILE                                   
         MVC   TLCACCOM,RQCATCO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     VT50                                                             
VT40     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
VT50     CLC   IOKEY(TLCACCAT-TLCAPD),IOKEYSAV                                  
         JE    VT60                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATSNF                                  
         J     XIT                                                              
VT60     CLC   TLCACSEQ,RQCATSQ                                                 
         JNE   VT40                                                             
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ      R4=A(CAST DETAILS ELEMENT)                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TACAEXP,TACAEXP     ENSURE CAST'S EXPIRATION DATE                
         JZ    VT70                HAS NOT PASSED                               
         CLC   TACAEXP,FULL2                                                    
         JH    VT70                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATAEX                                  
                                                                                
VT70     OC    TACALAST,TACALAST   ENSURE CAST IS NOT LAST SERVICED             
         JZ    VT80                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATALS                                  
                                                                                
         USING TLCAD,R4                                                         
VT80     L     R4,AIO4                                                          
         TM    TLCASRT,TLCASREQ    IF TRASNFERRED FROM CAST IS A                
         JO    XIT                 PRINCIPAL ...                                
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,TACRELQ      ... ENSURE CAST HAS AT LEAST                 
         BRAS  RE,GETEL            1 FTRACK ...                                 
         JE    VT90                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATNCR                                  
         J     XIT                                                              
                                                                                
         USING TACRD,R4                                                         
VT90     LA    R2,TRNTACR1                                                      
         LA    R6,TRNTACR8                                                      
                                                                                
VT100    CLC   TACRAPPL,TACRBAL    ... AND ALL FTRACKS ARE UNCREDITED           
         JE    VT110                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCATCCR                                  
         J     XIT                                                              
                                                                                
VT110    MVC   0(TACRLNQ,R2),TACREL                                             
                                                                                
         BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         LA    R2,TACRLNQ(R2)                                                   
         CR    R2,R6                                                            
         JNH   VT100                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTRN                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCATONF DC    AL1(ECATONFX-*),AL2(120),AL1(ERRCATY1),AL1(D#CATCO)              
         DC    C'Transferred from commercial record is not on file'             
ECATONFX EQU   *                                                                
                                                                                
ERCATOEX DC    AL1(ECATOEXX-*),AL2(121),AL1(ERRCATY1),AL1(D#CATCO)              
         DC    C'Commercial expiration date has passed'                         
ECATOEXX EQU   *                                                                
                                                                                
ERCATORL DC    AL1(ECATORLX-*),AL2(122),AL1(ERRCATY1),AL1(D#CATCO)              
         DC    C'Commercial is released'                                        
ECATORLX EQU   *                                                                
                                                                                
ERCATSNF DC    AL1(ECATSNFX-*),AL2(123),AL1(ERRCATY1),AL1(D#CATSQ)              
         DC    C'Transferred performer''s cast record is not on file'           
ECATSNFX EQU   *                                                                
                                                                                
ERCATAEX DC    AL1(ECATAEXX-*),AL2(124),AL1(ERRCATY1),AL1(D#CATSQ)              
         DC    C'Cast record expiration date has passed'                        
ECATAEXX EQU   *                                                                
                                                                                
ERCATALS DC    AL1(ECATALSX-*),AL2(125),AL1(ERRCATY1),AL1(D#CATSQ)              
         DC    C'Cast is last serviced'                                         
ECATALSX EQU   *                                                                
                                                                                
ERCATNCR DC    AL1(ECATNCRX-*),AL2(126),AL1(ERRCATY1),AL1(D#CATSQ)              
         DC    C'Cast does not have an FTrack'                                  
ECATNCRX EQU   *                                                                
                                                                                
ERCATCCR DC    AL1(ECATCCRX-*),AL2(128),AL1(ERRCATY1),AL1(D#CATSQ)              
         DC    C'FTrack has been applied to'                                    
ECATCCRX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DETERMINE IF CAST WAS ORIGINALLY ELIGIBLE FOR HOLDING FEES   *         
***********************************************************************         
                                                                                
STOELHLD NTR1  BASE=*,LABEL=*                                                   
         L     RE,ASVPTRS                                                       
SOEH10   CLI   0(RE),0                                                          
         JE    XIT                                                              
         CLI   0(RE),TLCAHCDQ                                                   
         JE    SOEH20                                                           
         LA    RE,L'TLDRREC(RE)                                                 
         J     SOEH10                                                           
                                                                                
SOEH20   OI    CASTSTAT,CAOELHLD                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD CAST RECORD SORT KEY                                   *         
***********************************************************************         
                                                                                
BLDSRTK  NTR1  BASE=*,LABEL=*                                                   
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGACATS          RF=DISPLACEMENT OF CATEGORY TABLE            
         DROP  RE                                                               
                                                                                
         USING CATTABD,RF                                                       
         AR    RF,RE               RF=A(CATEGORY TABLE)                         
                                                                                
BSK10    CLC   RQCACAT,CATCDE      FIND CAST CATEGORY IN                        
         JE    BSK20               CATEGORY TABLE                               
         ZIC   RE,CATLEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   BSK10                                                            
         DC    H'00'                                                            
                                                                                
BSK20    CLC   RQCAUNI,=C'AFM'     IF MUSICIAN                                  
         JNE   *+8                 SET MUSICIAN AND OFF CAMERA BITS             
         OI    RQCASRT,TLCASRMQ+TLCASRFQ                                        
                                                                                
         TM    CATTYPE,EXTRA       IF EXTRA                                     
         JZ    *+8                                                              
         OI    RQCASRT,TLCASREQ    SET EXTRA BIT                                
                                                                                
         CLC   RQCAONO,=C'OFF'     IF OFF CAMERA                                
         JNE   *+8                                                              
         OI    RQCASRT,TLCASRFQ    SET OFF CAMERA BIT                           
                                                                                
         OC    RQCAGUA,RQCAGUA     IF NOT ON GUARANTEE                          
         JNZ   *+8                                                              
         OI    RQCASRT,TLCASRGQ    THEN NOT ON GUARANTEE BIT                    
                                                                                
         MVC   RQCASRT+1(L'CATSORT),CATSORT                                     
                                                                                
         MVC   RQCASRT+2(2),RQCAVSQ+2                                           
         J     XIT                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADD CAST RECORD TO FILE                              *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         CLI   RQCAMST,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAMST,C'Y'        SET DEFAULT "ON MASTER COMMERCIAL?"          
                                                                                
         CLI   RQCALFT,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCALFT,C'N'        SET DEFAULT "ON LIFT VERSION?"               
                                                                                
         CLI   RQCACPA,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCACPA,C'N'        SET DEFAULT "CHKS PAYABLE TO AGENT?"         
                                                                                
         CLI   RQCAACP,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAACP,C'Y'        SET DEFAULT "APPLY CBL TO PER CYC?"          
                                                                                
         CLI   RQCAAPP,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAAPP,C'Y'        SET DEFAULT "APPLY PAX TO PER CYC?"          
                                                                                
         CLI   RQCAPDT,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAPDT,C'Y'        SET DEFAULT "DAILY FTRACK REPORT?"           
                                                                                
         CLI   RQCAPCR,0           IF NOT PROVIDED                              
         JNE   *+8                 SET DEFAULT "PAY CANDIAN RATES               
         MVI   RQCAPCR,C'N'        ON US COMMERCIAL?"                           
                                                                                
         CLI   RQCAPUR,0           IF NOT PROVIDED                              
         JNE   *+8                 SET DEFAULT "PAY US RATES ON                 
         MVI   RQCAPUR,C'N'        CANADIAN COMMERCIAL?"                        
                                                                                
         CLI   RQCACLB,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCACLB,C'N'        SET DEFAULT "CELEBRITY?"                     
                                                                                
         CLI   RQCAFGN,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAFGN,C'Y'        SET DEFAULT "FOREIGN USE?"                   
                                                                                
         CLI   RQCAINA,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAINA,C'Y'        SET DEFAULT "INTERACTIVE USE?"               
                                                                                
         CLI   RQCAINR,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAINR,C'Y'        SET DEFAULT "INDUSTRIAL USE?"                
                                                                                
         CLI   RQCAGRR,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAGRR,C'Y'        SET DEFAULT "GRR COVERS ALL USES?"           
                                                                                
         CLI   RQCAEUR,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAEUR,C'N'        SET DEFAULT "PAY EUROS?"                     
                                                                                
         CLI   RQCAAFT,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAAFT,C'Y'        SET DEFAULT "APPLY USE TO FTRACKS?"          
                                                                                
         CLI   RQCAEQY,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQCAEQY,C'N'        SET DEFAULT "EQUTIY?"                        
                                                                                
         OC    RQCASEQ,RQCASEQ     IF NOT PROVIDED                              
         JNZ   *+10                                                             
         MVC   RQCASEQ,SVNUNXTC    SET CAST SEQUENCE NUMBER                     
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
         L     RF,ASVPTRS          AND POINTER BLOCK                            
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLCAD,R4                                                         
         MVI   TLCACD,TLCACDQ      BUILD KEY WITH RECORD CODE                   
         MVC   TLCACOM,RQCACOM     INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCASORT,RQCASRT    SORT KEY                                     
         MVC   TLCASSN,RQCASSN     SOCIAL SECURITY NUMBER                       
         MVC   TLCACAT,RQCACAT     CATEGORY                                     
         MVI   TLCALEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           INITIALIZE CAST DETAILS ELEMENT              
         MVI   TACAEL,TACAELQ      AND ADD IT TO CAST RECORD                    
         MVI   TACALEN,TACALNQ                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  R2                                                               
                                                                                
         USING TAHFD,R2                                                         
         XC    ELEM,ELEM           INITIALIZE HOLDING FEE NOTIFICATION          
         MVI   TAHFEL,TAHFELQ      ELEMENT AND ADD IT TO CAST RECORD            
         MVI   TAHFLEN,TAHFLNQ                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  R2                                                               
                                                                                
         OC    TRNTACR1,TRNTACR1   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR1,0                    
                                                                                
         OC    TRNTACR2,TRNTACR2   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR2,0                    
                                                                                
         OC    TRNTACR3,TRNTACR3   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR3,0                    
                                                                                
         OC    TRNTACR4,TRNTACR4   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR4,0                    
                                                                                
         OC    TRNTACR5,TRNTACR5   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR5,0                    
                                                                                
         OC    TRNTACR6,TRNTACR6   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR6,0                    
                                                                                
         OC    TRNTACR7,TRNTACR7   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR7,0                    
                                                                                
         OC    TRNTACR8,TRNTACR8   IF FIXED CYCLE IS BEING TRANSFERRED          
         JZ    EXECA10             COPY ELEMENT TO THIS CAST RECORD             
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),TRNTACR8,0                    
                                                                                
EXECA10  BRAS  RE,BLDREC           BUILD RECORD AND ADD TO FILE                 
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         OI    CASTSTAT,CARECUPD                                                
                                                                                
         OI    COMSTAT,COREISSU    SET TO REISSUE HOLDING FEE                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING CAST RECORD                         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
         USING TLCAD,R4                                                         
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   YES                 DELETE ALL MARKED ELEMENTS                   
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         BRAS  RE,BLDREC           BUILD RECORD                                 
                                                                                
         CLC   TLCASORT,RQCASRT    IF KEY FIELD HAVE NOT CHANGED                
         JNE   EXECC10             PUT RECORD TO FILE                           
         CLC   TLCACAT,RQCACAT                                                  
         JNE   EXECC10                                                          
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         JE    EXECC20                                                          
         J     NO                                                               
                                                                                
EXECC10  OI    TLCASTAT,TLCASDCG   IF KEY FIELDS HAVE CHANGED                   
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS) DELETE OLD RECORD AND POINTERS               
                                                                                
         USING TLCAD,R4                                                         
         MVC   TLCASORT,RQCASRT    UPDATE RECORD KEY WITH SORT KEY              
         MVC   TLCACAT,RQCACAT     AND CATEGORY                                 
         XC    TLCASTAT,TLCASTAT                                                
         DROP  R4                                                               
                                                                                
         L     RF,ASVPTRS          ADD RECORD WITH NEW KEY TO FILE              
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
EXECC20  OI    CASTSTAT,CARECUPD                                                
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD CAST RECORD                                            *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEM)                                   
         BAS   RE,UPDTARL          UPDATE RELEASE STATUS ELEMENT                
         BAS   RE,UPDTACA          UPDATE CAST DETAILS ELEMENT                  
         BAS   RE,ADDTAOP          ADD OVERSCALE PERCENTAGE ELEMENT             
         BAS   RE,ADDTAO2          ADD 2ND OVERSCALE PERCENTAGE ELEMENT         
         BAS   RE,ADDTAOA          ADD OVERSCALE AMOUNT ELEMENT                 
         BAS   RE,ADDTAFN          ADD FREE FORM NAME ELEMENT                   
         BAS   RE,ADDTACM          ADD COMMENT ELEMENTS                         
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQCAWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQCASTF,SVTIME             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        UPDATE CAST DETAILS ELEMENT                                  *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
UPDTACA  NTR1                                                                   
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            R4=A(CAST DETAILS ELEMENT)                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   ELEM(TACALNQ),TACAD SAVE ORIGINAL ELEMENT                        
                                                                                
         MVC   TACAONOF,RQCAONO    PUT ON/OFF CAMERA                            
         MVC   TACAUN,RQCAUNI      UNION                                        
         MVC   TACALOCL,RQCALCL    LOCAL                                        
         MVC   TACAYEAR,RQCACYR    CONTRACT YEAR                                
         MVC   TACAFRST,RQCAFSV    FIRST SERVICES DATE                          
         MVC   TACALAST,RQCALSV    LAST SERVICES DATE                           
         MVC   TACAEXP,RQCAEXP     EXPIRATION DATE                              
         MVC   TACANCDE,TRNSAGT    AGENT                                        
         MVC   TACACORP,SVCORP     CORPORATION NUMBER                           
         MVC   TACAUNIT,RQCATAX    TAX UNIT                                     
         MVC   TACAOV2,RQCA2OP     SECOND OVERSCALE PERCENTAGE                  
         MVC   TACADBL,RQCADBL     DOUBLES                                      
         MVC   TACAFCYC,RQCAFFC    FIRST FIXED CYCLE                            
         MVC   TACAGUA,RQCAGUA     GUARANTEE CODE                               
         MVC   TACALFTF,RQCALFI    LIFTED FROM INTERNAL COMML NUMBER            
         MVC   TACARERC,RQCARER    RERECORD DATE                                
         MVC   TACATCOM,RQCATCO    TRANSFERRED FROM INT COMM'L NUMBER           
         MVC   TACATSEQ,RQCATSQ    TRANSFERRED FROM CAST SEQ NUMBER             
                                                                                
         NI    TACASTAT,X'FF'-TACASTLF                                          
         CLI   LFTSTAT,C'Y'        IF COMMERCIAL HAS LIFT                       
         JNE   UCA10                                                            
         CLI   RQCALFT,C'Y'                                                     
         JNE   UCA10               PUT "ON LIFT VERSION?"                       
         OI    TACASTAT,TACASTLF   INDICATOR INTO ELEMENT                       
                                                                                
UCA10    NI    TACASTAT,X'FF'-TACASTLO                                          
         CLI   LFTSTAT,C'Y'        IF COMMERCIAL HAS LIFT                       
         JNE   UCA20                                                            
         CLI   RQCAMST,C'Y'                                                     
         JE    UCA20               PUT "ON MASTER COMMERCIAL?"                  
         OI    TACASTAT,TACASTLO   INDICATOR INTO ELEMENT                       
                                                                                
UCA20    NI    TACASTAT,X'FF'-TACASTAG                                          
         CLI   RQCACPA,C'Y'                                                     
         JNE   *+8                 PUT "CHECKS PAYABLE TO AGENT?"               
         OI    TACASTAT,TACASTAG   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTAT,X'FF'-TACASXAC                                          
         CLI   RQCAACP,C'Y'                                                     
         JE    *+8                 PUT "APPLY CBL TO PER CYCLE?"                
         OI    TACASTAT,TACASXAC   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTAT,X'FF'-TACASXAP                                          
         CLI   RQCAAPP,C'Y'                                                     
         JE    *+8                 PUT "APPLY PAX TO PER CYCLE?"                
         OI    TACASTAT,TACASXAP   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTAT,X'FF'-TACASTNF                                          
         CLI   RQCAPDT,C'Y'                                                     
         JE    *+8                 PUT "DAILY FTRACK REPORT?"                   
         OI    TACASTAT,TACASTNF   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASTCR                                          
         CLI   RQCAPCR,C'Y'                                                     
         JNE   *+8                 PUT "PAY CANDIAN RATES ON US                 
         OI    TACASTA2,TACASTCR   COMMERCIAL?" INDICATOR INTO ELEMENT          
                                                                                
         NI    TACASTA2,X'FF'-TACASTDP                                          
         CLI   RQCAPUR,C'Y'                                                     
         JNE   *+8                 PUT "PAY US RATES ON CANADIAN                
         OI    TACASTA2,TACASTDP   COMMERCIAL?" INDICATOR INTO ELEMENT          
                                                                                
         NI    TACASTA2,X'FF'-TACASCLB                                          
         CLI   RQCACLB,C'Y'                                                     
         JNE   *+8                 PUT "CELEBRITY?"                             
         OI    TACASTA2,TACASCLB   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASFGR                                          
         CLI   RQCAFGN,C'Y'                                                     
         JE    *+8                 PUT "FOREIGN USE?"                           
         OI    TACASTA2,TACASFGR   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASINA                                          
         CLI   RQCAINA,C'Y'                                                     
         JE    *+8                 PUT "INTERACTIVE USE?"                       
         OI    TACASTA2,TACASINA   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASINR                                          
         CLI   RQCAINR,C'Y'                                                     
         JE    *+8                 PUT "INDUSTRIAL USE?"                        
         OI    TACASTA2,TACASINR   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASPUS                                          
         CLI   RQCAGRR,C'Y'                                                     
         JE    *+8                 PUT "GRR COVERS ALL USE?"                    
         OI    TACASTA2,TACASPUS   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA2,X'FF'-TACASEUR                                          
         CLI   RQCAEUR,C'Y'                                                     
         JNE   *+8                 PUT "PAY IN EUROS?"                          
         OI    TACASTA2,TACASEUR   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA3,X'FF'-TACASXFT                                          
         CLI   RQCAAFT,C'Y'                                                     
         JE    *+8                 PUT "APPLY USE TO FTRACKS?"                  
         OI    TACASTA3,TACASXFT   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TACASTA3,X'FF'-TACASEQY                                          
         CLI   RQCAEQY,C'N'                                                     
         JE    *+8                 PUT "EQUITY?"                                
         OI    TACASTA3,TACASEQY   INDICATOR INTO ELEMENT                       
                                                                                
         OC    RQCAMST(2),RQCAMST                                               
         JZ    *+8                                                              
         NI    ELEM+TACASTAT-TACAD,X'FF'-TACASTLF-TACASTLO                      
                                                                                
         CLC   ELEM(TACALNQ),0(R4) IF ANY FIELDS HAVE CHANGED                   
         JE    XIT                                                              
         OI    COMSTAT,COUNVRFY    SET TO UNVERIFY COMMERCIAL                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        UPDATE CAST RELEASE STATUS ELEMENT                           *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
UPDTARL  NTR1                                                                   
         MVI   ELCODE,TARLELQ      ATTEMPT TO GET EXISTING                      
         BRAS  RE,GETEL            RELEASE STATUS ELEMENT                       
         JE    URL10                                                            
                                                                                
         USING TARLD,R2                                                         
         CLI   RQCARLL,0           IF ELEMENT IS NOT FOUND                      
         JE    XIT                 AND RELEASE LETTER IS PRESENT                
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   TARLEL,TARLELQ      PREPARE TO ADD ELEMENT NOW                   
         MVI   TARLLEN,TARLLNQ                                                  
         MVC   TARLSTAT,RQCARLL    PUT RELEASE LETTER CODE                      
         MVC   TARLEFDT,RQCAEFF    AND EFFECTIVE DATE INTO ELEMENT              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R2),0                        
         OI    CASTSTAT,CAGENRLL   SET TO GENERATE RELEASE LETTER               
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
         USING TARLD,R4                                                         
URL10    CLC   TARLSTAT,RQCARLL    IF ELEMENT IS FOUND                          
         JE    XIT                 AND RELEASE LETTER IS CHANGING               
         MVC   TARLSTAP,TARLSTAT   UPDATE OLD RELEASE LETTER CODE               
         MVC   TARLSTAT,RQCARLL    PUT NEW RELEASE LETTER CODE                  
         MVC   TARLEFDT,RQCAEFF    AND EFFECTIVE DATE                           
                                                                                
         OC    TARLDATE,TARLDATE   IF RELEASE DATE IS SET                       
         JZ    URL20                                                            
         MVC   TARLDATP,TARLDATE   MOVE IT TO PREVIOUS RELEASE DATE             
URL20    XC    TARLDATE,TARLDATE   AND CLEAR NEW RELEASE DATE                   
         CLI   RQCARLL,0           IF RELEASE LETTER IS POPULATED               
         JE    XIT                                                              
         OI    CASTSTAT,CAGENRLL   SET TO GENERATE RELEASE LETTER               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ADD OVERSCALE PERCENTAGE ELEMENT                             *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
ADDTAOP  NTR1                                                                   
         OC    RQCAUP1,RQCAUP1     IF OVERSCALE PERCENTAGE IS PRESENT           
         JZ    XIT                                                              
                                                                                
         LA    R6,RQCAUP1          R4=A(FIRST OVERSCALE PCT FIELD)              
         LHI   R0,6                R0=OVERSCALE PCT FIELD COUNTER               
                                                                                
         USING TAOPD,R2                                                         
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAOPEL,TAOPELQ                                                   
         MVI   TAOPLEN,TAOPLNQ                                                  
         LHI   RE,TAOPLNQ                                                       
                                                                                
         XR    R3,R3               INITIALIZE COUNTER                           
         LA    RF,TAOPSBEL         RF=A(FIRST SUB-ELEMENT)                      
                                                                                
AOP10    OC    0(L'RQCAUP1,R6),0(R6)                                            
         JZ    AOP20               IF OVERSCALE PERCENTAGE IS PRESENT           
         MVC   0(L'TAOPSBEL,RF),0(R6)      MOVE IN USE AND PERCENTAGE           
         AHI   R3,1                                    ADD TO COUNTER           
         AHI   RE,L'TAOPSBEL                AND ADD TO ELEMENT LENGTH           
         LA    RF,L'TAOPSBEL(RF)       BUMP TO NEXT OVERSCALE ENTRIES           
         LA    R6,L'RQCAUP1+L'RQCAOP1(R6)                                       
         BCT   R0,AOP10                                                         
                                                                                
AOP20    STC   RE,TAOPLEN                                                       
         STC   R3,TAOPNUM                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAOPELQ',(R4)),SVCOTYPE,SVCOMED         
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD SECOND OVERSCALE PERCENTAGE ELEMENT                      *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
ADDTAO2  NTR1                                                                   
         OC    RQCA2U1,RQCA2U1     IF OVERSCALE 2ND PERCENTAGE IS               
         JZ    XIT                 PRESENT                                      
                                                                                
         LA    R6,RQCA2U1          R4=A(FIRST OVERSCALE PCT FIELD)              
         LHI   R0,5                R0=OVERSCALE PCT FIELD COUNTER               
                                                                                
         USING TAO2D,R2                                                         
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAO2EL,TAO2ELQ                                                   
         MVI   TAO2LEN,TAO2LNQ                                                  
         LHI   RE,TAO2LNQ                                                       
                                                                                
         XR    R3,R3               INITIALIZE COUNTER                           
         LA    RF,TAO2SBEL         RF=A(FIRST SUB-ELEMENT)                      
                                                                                
AO210    OC    0(L'RQCA2U1,R6),0(R6)                                            
         JZ    AO220                  IF OVERSCALE 2ND PCT IS PRESENT           
         MVC   0(L'TAO2SBEL,RF),0(R6)      MOVE IN USE AND PERCENTAGE           
         AHI   R3,1                                    ADD TO COUNTER           
         AHI   RE,L'TAO2SBEL                AND ADD TO ELEMENT LENGTH           
         LA    RF,L'TAO2SBEL(RF)       BUMP TO NEXT OVERSCALE ENTRIES           
         LA    R6,L'RQCA2U1+L'RQCA2P1(R6)                                       
         BCT   R0,AO210                                                         
                                                                                
AO220    STC   RE,TAO2LEN                                                       
         STC   R3,TAO2NUM                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAO2ELQ',(R4)),SVCOTYPE,SVCOMED         
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD OVERSCALE AMOUNT ELEMENT                                 *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
ADDTAOA  NTR1                                                                   
         OC    RQCAUA1,RQCAUA1     IF OVERSCALE AMOUNT IS PRESENT               
         JZ    XIT                                                              
                                                                                
         LA    R6,RQCAUA1          R4=A(FIRST OVERSCALE AMOUNT FIELD)           
         LHI   R0,6                R0=OVERSCALE AMOUNT FIELD COUNTER)           
                                                                                
         USING TAOAD,R2                                                         
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
         LHI   RE,TAOALNQ                                                       
                                                                                
         XR    R3,R3               INITIALIZE COUNTER                           
         LA    RF,TAOASBEL         RF=A(FIRST SUB-ELEMENT)                      
                                                                                
AOA10    OC    0(L'RQCAUA1,R6),0(R6)                                            
         JZ    AOA20               IF OVERSCALE AMOUNT IS PRESENT               
         MVC   0(L'TAOASBEL,RF),0(R6)      MOVE IN USE AND AMOUNT               
         AHI   R3,1                                ADD TO COUNTER               
         AHI   RE,L'TAOASBEL            AND ADD TO ELEMENT LENGTH               
         LA    RF,L'TAOPSBEL(RF)   BUMP TO NEXT OVERSCALE ENTRIES               
         LA    R6,L'RQCAUA1+L'RQCAOA1(R6)                                       
         BCT   R0,AOA10                                                         
                                                                                
AOA20    STC   RE,TAOALEN                                                       
         STC   R3,TAOANUM                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
         GOTOR (#UPDOAP,AUPDOAP),DMCB,('TAOAELQ',(R4)),SVCOTYPE,SVCOMED         
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD FREE FORM NAME ELEMENT                                   *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
ADDTAFN  NTR1                                                                   
         USING TAFND,R2                                                         
         OC    RQCATR1,RQCATR1     IF TRACK #1 IS PRESENT                       
         JZ    AFN10                                                            
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTTRK   PUT TYPE                                     
         MVC   TAFNNAME(4),RQCATR1 AND TRACKS INTO ELEMENT                      
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AFN10    CLI   VERSTAT,C'Y'        IF COMMERCIAL HAS VERSIONS                   
         JNE   AFN20                                                            
         OC    VERSIONS,VERSIONS   AND VERSIONS ARE PRESENT                     
         JZ    AFN20                                                            
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ      PUT TYPE                                     
         MVI   TAFNTYPE,TAFNTVER   AND VERSIONS INTO ELEMENT                    
         MVC   TAFNNAME(L'VERSIONS),VERSIONS                                    
         GOTOR (#SETELEN,ASETELEN) AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AFN20    OC    RQCAWRI,RQCAWRI     IF WEB APPLICATION RECORD ID                 
         JZ    XIT                 IS PRESENT                                   
         XC    ELEM,ELEM           INITIALIZE ELEMENT                           
         MVI   TAFNEL,TAFNELQ      PUT TYPE                                     
         MVI   TAFNTYPE,TAFNTWRI   AND WEB APPLICATION RECORD ID                
         MVC   TAFNNAME(L'RQCAWRI),RQCAWRI                                      
         GOTOR (#SETELEN,ASETELEN) INTO ELEMENT AND ADD IT                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD VARIABLE LENGTH COMMENT ELEMENT                          *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R3=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
ADDTACM  NTR1                                                                   
         USING TACMD,R2                                                         
         OC    RQCACMT,RQCACMT     IF COMMENT IS PRESENT                        
         JZ    ACM10                                                            
         XC    ELEM,ELEM           INITIALIZE COMMENT                           
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ                                                  
         MVI   TACMTYPE,TACMTYPG   PUT COMMENT INTO ELEMENT                     
         MVC   TACMCOMM(L'RQCACMT),RQCACMT                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
ACM10    OC    RQCARDE,RQCARDE     IF ROLE DESCRIPTION IS PRESENT               
         JZ    XIT                                                              
         XC    ELEM,ELEM           INITIALIZE COMMENT                           
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ                                                  
         MVI   TACMTYPE,TACMTYPD   PUT ROLE DESCRIPTION INTO ELEMENT            
         MVC   TACMCOMM(L'RQCARDE),RQCARDE                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        UPDATE COMMERCIAL RECORD                                     *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     AIO1=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
UPDCOM   NTR1  BASE=*,LABEL=*                                                   
         USING TLCOD,R4                                                         
         L     R4,AIO1             R4=A(COMMERCIAL RECORD)                      
                                                                                
         TM    SVCOSTA2,TACOCHHF   IF COMMERCIAL IS ALREADY SET FOR             
         JZ    UCOM10              HOLDING FEE REISSUE, SKIP AHEAD              
         NI    COMSTAT,X'FF'-COREISSU                                           
         J     UCOM50                                                           
                                                                                
UCOM10   TM    COMSTAT,COREISSU    IF A FIELD THAT TRIGGERS A REISSUE           
         JZ    UCOM50              HAS BEEN CHANGED ...                         
         NI    COMSTAT,X'FF'-COREISSU                                           
                                                                                
         USING TLCAPD,R3                                                        
         TM    CASTSTAT,CAOELHLD   ... AND CAST WAS ORIGINALLY ELIGIBLE         
         JO    UCOM30              FOR A HOLDING FEE                            
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCAHCDQ    OR IS ELIGIBLE NOW ...                       
         MVC   TLCAHCOM,TLCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
UCOM20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR'                                   
         CLC   IOKEY(TLCAHSRT-TLCAPD),IOKEYSAV                                  
         JNE   UCOM50                                                           
         CLC   RQCASEQ,TLCAHSRT+4                                               
         JNE   UCOM20                                                           
         DROP  R3                                                               
                                                                                
         USING TLCAPD,R3                                                        
UCOM30   XC    TLCAPKEY,TLCAPKEY   READ ALL HOLDING FEE KEYS FOR                
         MVI   TLCAPCD,TLCAHCDQ    COMMERCIAL'S CAST                            
         MVC   TLCAHCOM,TLCOCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
UCOM40   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR'                                   
         CLC   IOKEY(TLCAHSRT-TLCAPD),IOKEYSAV                                  
         JNE   UCOM50                                                           
         OC    TLCAHNXT,TLCAHNXT   IF ANY OF THE LATEST HOLDING FEES            
         JZ    UCOM40              HAVE NOT BEEN PAID                           
         CLC   TLCAHDTE,TLCAHNXT   WE NEED TO REISSUE                           
         JH    UCOM40                                                           
         OI    COMSTAT,COREISSU                                                 
         DROP  R3,R4                                                            
                                                                                
UCOM50   OC    SVCOVDTE,SVCOVDTE   IF COMMERCIAL WAS NOT ORIGINALLY             
         JNZ   UCOM60              VERIFIED, NO NEED TO UNVERIFY                
         NI    COMSTAT,X'FF'-COUNVRFY                                           
                                                                                
UCOM60   TM    COMSTAT,COUNVRFY+COREISSU+COCSTSEQ                               
         JZ    XIT                                                              
                                                                                
         OC    SVNUNXTC,SVNUNXTC   IF CAST HAS BEEN ADDED ...                   
         JZ    UCOM70                                                           
                                                                                
         USING TANUD,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TANUELQ',AIO1),('TANUTSEQ',0)         
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AELEM                                                         
         ZICM  RF,SVNUNXTC,2                                                    
         AHI   RF,1                                                             
         STCM  RF,3,TANUNXTC       UPDATE NEXT CAST SEQUENCE NUMBER             
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1                                                          
UCOM70   MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            R4=A(COMMERCIAL DETAILS ELEMENT)             
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    COMSTAT,COUNVRFY    IF COMMERCIAL MUST BE UNVERIFIED             
         JZ    UCOM90                                                           
         TM    CASTSTAT,CAGENRLL                                                
         JO    UCOM90                                                           
         XC    TACOVDTE,TACOVDTE   CLEAR DATE OF VERIFICATION                   
         XC    TACOVTIM,TACOVTIM         TIME OF VERIFICATION                   
         XC    TACOVSTU,TACOVSTU   USER ID                                      
         XC    TACOVST,TACOVST     STAFF ID                                     
         MVI   BYTE1,TACOUVNM      AND SET UNVERIFIED STATUS                    
         CLC   RQCAUNI,=C'AFM'                                                  
         JNE   UCOM80                                                           
         MVI   BYTE1,TACOUVMU                                                   
UCOM80   OC    TACOUVST,BYTE1                                                   
                                                                                
UCOM90   TM    COMSTAT,COREISSU    IF HOLDING FEE MUST BE REISSUED              
         JZ    UCOM100                                                          
         OI    TACOSTA2,TACOCHHF   SET STATUS                                   
         GOTOR SNDMQHFR,DMCB,(SYSTEM,RQCACOM),(SVSYSTA2,VHEXOUT),VMQIO          
         DROP  R4                                                               
                                                                                
UCOM100  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO1'                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES EXISTING CAST RECORD                         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLCAD,R4                                                         
EXECDEL  NTR1  BASE=*,LABEL=*                                                   
         OI    TLCASTAT,TLCASDCG+TLCASDEL                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS) DELETE OLD RECORD AND POINTERS               
                                                                                
         OI    COMSTAT,COUNVRFY+COREISSU                                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD RELEASE LETTER REQUEST                                   *         
*        ON ENTRY ... R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
ADDRLL   NTR1  BASE=*,LABEL=*                                                   
         TM    CASTSTAT,CAGENRLL   IF RELEASE LETTER NEEDS TO BE                
         JZ    XIT                 GENERATED ...                                
                                                                                
         USING REQD,R3                                                          
         LA    R3,ELEM2            BUILD REQUEST                                
         XC    REQHDR,REQHDR                                                    
         OI    REQHDR+15,X'01'                                                  
         MVC   REQREQ,RLEQCARD                                                  
         MVC   REQREQ+2(L'LP_AGY),LP_AGY                                        
         MVC   REQREQ+4(1),SVAYTPOF                                             
         MVC   REQREQ+5(6),SVAGY                                                
         GOTO1 VHEXOUT,DMCB,RQCACOM,REQREQ+38,L'RQCACOM,0                       
         MVC   REQREQ+46(1),RQCARLL                                             
         MVC   REQREQ+52(6),=6C'0'                                              
         OC    RQCAEFF,RQCAEFF                                                  
         BZ    ARLL10                                                           
         GOTO1 VDATCON,DMCB,(1,RQCAEFF),(20,DUB)                                
         MVC   REQREQ+52(6),DUB+2                                               
                                                                                
         USING TANUD,R4                                                         
ARLL10   GOTOR (#GETELEM,AGETELEM),DMCB,('TANUELQ',AIO2),('TANUPIDN',0)         
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AELEM                                                         
         MVC   REQREQ+63(6),TANUMBER                                            
         MVC   REQREQ+69(3),SPACES                                              
         DROP  R4                                                               
                                                                                
         GOTO1 VHEXOUT,DMCB,RQCASEQ,REQREQ+72,L'RQCASEQ,0                       
         MVC   REQREQ+76(3),RQCACAT                                             
         DROP  R3                                                               
                                                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'DMADD'),=C'TALREQ',(R3),(R3)                 
         CLI   8(R1),0                                                          
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
RLEQCARD DC    CL80'RLXXOAGENCY 0203REL 0301R 0503DDS 0909INTCOMNOL 100X        
               6YYMMDD 1116123456789SEQ CAT*'                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
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
         TM    CASTSTAT,CARECUPD   IF ADDING WEB TRANSACTION RECORD             
         JZ    XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE WEB TRANSACTION RECORD            
         MVI   TLWTCD,TLWTCDQ                                                   
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   SET WEB APPLICATION                          
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTACCA   INDICATE IF CHANGING                         
         CLI   ACTION,ACTCHA                                                    
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTAACA   OR ADDING CAST                               
         CLI   ACTION,ACTADD                                                    
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTADCA   OR DELETING CAST                             
                                                                                
AWTR10   MVC   TLWTWBID,RQCAWID    SET WEB APPLICATION ID                       
         MVC   TLWTUCCO,RQCACOM    AND UNIQUE IDENTIFIER                        
         MVC   TLWTUCSQ,RQCASEQ                                                 
                                                                                
         MVI   TLWTLEN+1,41        SET RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           BUILD WEB TRANSACTION ELEMENT                
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWT6LNQ                                                 
         MVC   TAWTSTAF,RQCASTF                                                 
         MVC   TAWTCOAY,SVAGY                                                   
         MVC   TAWTCOID,SVCID                                                   
         MVC   TAWTCOTI,SVTITLE                                                 
         MVC   TAWTCASS,RQCASSN                                                 
         MVC   TAWTCANM,SVW4NAME                                                
         MVC   TAWTCACA,RQCACAT                                                 
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
*        ADD/UPDATE TRANSFERRED FROM RECORDS                          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
UPDTRN   NTR1  BASE=*,LABEL=*                                                   
         OC    TRNTACR1,TRNTACR1   IF CAST FIXED CYCLE IS BEING                 
         JZ    XIT                 TRANSFERRED ...                              
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCACCDQ    READ TRANSFERRED FROM CAST RECORD            
         MVC   TLCACSSN,RQCASSN                                                 
         MVC   TLCACCOM,RQCATCO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     UT20                                                             
UT10     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
UT20     CLC   IOKEY(TLCACCAT-TLCAPD),IOKEYSAV                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TLCACSEQ,RQCATSQ                                                 
         JNE   UT10                                                             
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         NI    CASTSTAT,X'FF'-CAOELHLD                                          
                                                                                
         GOTOR (#SAVPTRS,ASAVPTRS) DETERMINE IF CAST WAS ALREADY                
         BRAS  RE,STOELHLD         ELIGIBLE FOR HOLDING FEES                    
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      SET LAST SERVICED DATE AS                    
         BRAS  RE,GETEL            TRANSFERRED TO CAST'S                        
         JE    *+6                 FIRST SERVICE DATE                           
         DC    H'00'                                                            
         MVC   TACALAST,RQCAFSV                                                 
         DROP  R4                                                               
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACRELQ      ZERO OUT TRANSFERRED FROM CAST'S             
         BRAS  RE,GETEL            FTRACK BALANCE                               
         J     *+8                                                              
UT30     BRAS  RE,NEXTEL                                                        
         JNE   UT40                                                             
         XC    TACRBAL,TACRBAL                                                  
         J     UT30                                                             
         DROP  R4                                                               
                                                                                
UT40     GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
                                                                                
         USING TACRD,R6                                                         
         LA    R6,TRNTACR1                                                      
                                                                                
         USING TALKFCYD,R2                                                      
UT50     LA    R2,TLFCBLK                                                       
                                                                                
         XR    R0,R0                                                            
                                                                                
         USING TLFTD,R3                                                         
         XC    TLFTKEY,TLFTKEY                                                  
         MVI   TLFTCD,TLFTCDQ      READ FIXED CYCLE TRACKING RECORD             
         MVC   TLFTSSN,RQCASSN     FOR FROM CAST                                
         MVC   TLFTCOM,RQCATCO                                                  
         MVC   TLFTCAST,RQCATSQ                                                 
         MVC   TLFTSTRT,TACRSTRT                                                
         XC    TLFTSTRT,=3X'FF'                                                 
         MVC   TLFTEND,TACREND                                                  
         XC    TLFTEND,=3X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLFTTRK-TLFTD),IOKEYSAV                                    
         JNE   UT55                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TAGTD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGTELQ      GET FIXED CYCLE TRACKING ELEMENT             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R0,TAGTPNH                                                       
         DROP  R4                                                               
                                                                                
UT55     XC    TLFCBLK,TLFCBLK                                                  
         MVC   TLFCSTF,RQCASTF     ADD FIXED CYCLE TRACKING RECORD              
         MVC   TLFCCOM,RQCACOM     FOR FROM CAST                                
         MVC   TLFCSEQ,RQCASEQ                                                  
         MVC   TLFCCYS,TACRSTRT                                                 
         MVC   TLFCCYE,TACREND                                                  
         MVC   TLFCBAL,TACRAPPL                                                 
         MVC   TLFCACS,TACRSTRT                                                 
         MVC   TLFCACE,TACREND                                                  
         STCM  R0,15,TLFCPNH                                                    
         MVC   TLFCAAM,TACRAPPL                                                 
                                                                                
         MVC   TLFCCMT,SPACES                                                   
         MVC   TLFCCMT(L'TRFR),TRFR                                             
         MVC   TLFCCMT+L'TRFR(L'TRNFRAGY),TRNFRAGY                              
         LA    RF,TLFCCMT+L'TRFR+L'TRNFRAGY+1                                   
UT60     CLI   0(RF),C' '                                                       
         JH    UT70                                                             
         SHI   RF,1                                                             
         J     UT60                                                             
UT70     MVI   1(RF),C'/'                                                       
         OC    TACRINV,TACRINV                                                  
         JZ    UT90                                                             
UT80     GOTOR (#CVTINV,ACVTINV),DMCB,TACRINV,2(RF)                             
         J     UT100                                                            
UT90     MVC   2(L'TRNFRCID,RF),TRNFRCID                                        
                                                                                
UT100    MVC   TLFCSSN,RQCASSN                                                  
         MVC   TLFCTIME,SVTIME                                                  
         GOTOR (#ADDFTRK,AADDFTRK),DMCB,(R2)                                    
                                                                                
         MVC   TLFCCOM,RQCATCO                                                  
         MVC   TLFCSEQ,RQCATSQ                                                  
         XC    TLFCBAL,TLFCBAL                                                  
         ICM   RE,15,TLFCAAM       ADD FIXED CYCLE TRACKING RECORD              
         LNR   RE,RE               FOR TO CAST                                  
         STCM  RE,15,TLFCAAM                                                    
         XC    TLFCPNH,TLFCPNH                                                  
                                                                                
         MVC   TLFCCMT,SPACES                                                   
         MVC   TLFCCMT(L'TRTO),TRTO                                             
         MVC   TLFCCMT+L'TRTO(L'SVAGY),SVAGY                                    
         LA    RF,TLFCCMT+L'TRTO+L'SVAGY+1                                      
UT110    CLI   0(RF),C' '                                                       
         JH    UT120                                                            
         SHI   RF,1                                                             
         J     UT110                                                            
UT120    MVI   1(RF),C'/'                                                       
         MVC   2(L'SVCID,RF),SVCID                                              
                                                                                
         GOTOR (#ADDFTRK,AADDFTRK),DMCB,(R2)                                    
         DROP  R2                                                               
                                                                                
         LA    RE,TRNTACR8                                                      
         CR    R4,RE                                                            
         JE    UT130                                                            
         LA    R6,L'TRNTACR1(R6)                                                
         OC    0(L'TRNTACR1,R6),0(R6)                                           
         JNZ   UT50                                                             
         DROP  R6                                                               
                                                                                
         USING TLCOPD,R3                                                        
UT130    XC    TLCOPKEY,TLCOPKEY   READ TRANSFERRED FROM COMMERCIAL             
         MVI   TLCOPCD,TLCOCCDQ    INTO AIO1                                    
         MVC   TLCOCCOM,RQCATCO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO1'                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVCOSTA2,TACOSTA2   SAVE COMMERCIAL STATUS 2                     
         MVC   SVCOVDTE,TACOVDTE   AND VERIFICATION DATE                        
         DROP  R4                                                               
                                                                                
         MVI   COMSTAT,COUNVRFY+COREISSU                                        
         XC    SVNUNXTC,SVNUNXTC                                                
         BRAS  RE,UPDCOM                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
TRTO     DC    C'XFER TO '                                                      
TRFR     DC    C'XFER FROM '                                                    
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE OVERSCALE USE                            *         
*        ON ENTRY ... P1=A(USE FIELD TO VALIDATE)                     *         
***********************************************************************         
                                                                                
VALOUSE  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE4,X'80'                                                      
         CLI   RQCAETM,C'Y'                                                     
         JE    *+8                                                              
         MVI   BYTE4,0                                                          
         ZICM  R2,1(R1),3                                                       
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFUSE',0(R2)),                 +        
               (BYTE4,SVCOTYPE),SVCOMED                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - CAST MAINTENANCE UPLOAD                               *         
***********************************************************************         
                                                                                
CAHDR    LKMAP H,I#CAULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#CAMOD,UBIN,TA#PMODE,OLEN=L'RQCAMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCAMOD)                                       
F$STF    LKMAP F,D#CASTF,CHAR,TA#STAFF,MAXLEN=L'RQCASTF,               +        
               OUTPUT=(D,B#SAVED,RQCASTF)                                       
F$COM    LKMAP F,D#CACOM,HEXD,TA#COMCD,OLEN=L'RQCACOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCACOM)                                       
F$SEQ    LKMAP F,D#CASEQ,HEXD,TA#CSTSQ,OLEN=L'RQCASEQ,MAXLEN=4,        +        
               OUTPUT=(D,B#SAVED,RQCASEQ)                                       
F$SSN    LKMAP F,D#CASSN,CHAR,TA#SSN,MAXLEN=L'RQCASSN,                 +        
               OUTPUT=(D,B#SAVED,RQCASSN)                                       
F$CAT    LKMAP F,D#CACAT,CHAR,TA#CAT,MAXLEN=L'RQCACAT,                 +        
               OUTPUT=(D,B#SAVED,RQCACAT)                                       
F$LFI    LKMAP F,D#CALFI,HEXD,TA#CALFI,OLEN=L'RQCALFI,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCALFI)                                       
F$ONO    LKMAP F,D#CAONO,CHAR,TA#ONO,MAXLEN=L'RQCAONO,                 +        
               OUTPUT=(D,B#SAVED,RQCAONO)                                       
F$TAX    LKMAP F,D#CATAX,CHAR,TA#TAX,MAXLEN=L'RQCATAX,                 +        
               OUTPUT=(D,B#SAVED,RQCATAX)                                       
F$UNI    LKMAP F,D#CAUNI,CHAR,TA#UNI,MAXLEN=L'RQCAUNI,                 +        
               OUTPUT=(D,B#SAVED,RQCAUNI)                                       
F$LCL    LKMAP F,D#CALCL,CHAR,TA#LCL,MAXLEN=L'RQCALCL,                 +        
               OUTPUT=(D,B#SAVED,RQCALCL)                                       
F$CYR    LKMAP F,D#CACYR,CHAR,TA#CYR,MAXLEN=L'RQCACYR,                 +        
               OUTPUT=(D,B#SAVED,RQCACYR)                                       
F$FFC    LKMAP F,D#CAFFC,PDAT,TA#FFC,OUTPUT=(D,B#SAVED,RQCAFFC)                 
F$FSV    LKMAP F,D#CAFSV,PDAT,TA#FSV,OUTPUT=(D,B#SAVED,RQCAFSV)                 
F$LSV    LKMAP F,D#CALSV,PDAT,TA#LSV,OUTPUT=(D,B#SAVED,RQCALSV)                 
F$RLL    LKMAP F,D#CARLL,CHAR,TA#RLL,MAXLEN=L'RQCARLL,                 +        
               OUTPUT=(D,B#SAVED,RQCARLL)                                       
F$EFF    LKMAP F,D#CAEFF,PDAT,TA#EFF,OUTPUT=(D,B#SAVED,RQCAEFF)                 
F$ATC    LKMAP F,D#CAATC,CHAR,TA#ATC,MAXLEN=L'RQCAATC,                 +        
               OUTPUT=(D,B#SAVED,RQCAATC)                                       
F$AGT    LKMAP F,D#CAAGT,CHAR,TA#ANCD,MAXLEN=L'RQCAAGT,                +        
               OUTPUT=(D,B#SAVED,RQCAAGT)                                       
F$GUA    LKMAP F,D#CAGUA,CHAR,TA#GUA,MAXLEN=L'RQCAGUA,                 +        
               OUTPUT=(D,B#SAVED,RQCAGUA)                                       
F$EXP    LKMAP F,D#CAEXP,PDAT,TA#EXP,OUTPUT=(D,B#SAVED,RQCAEXP)                 
F$UP1    LKMAP F,D#CAUP1,CHAR,TA#UP1,MAXLEN=L'RQCAUP1,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP1)                                       
F$OP1    LKMAP F,D#CAOP1,UBIN,TA#OP1,OLEN=L'RQCAOP1,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP1)                                       
F$UP2    LKMAP F,D#CAUP2,CHAR,TA#UP2,MAXLEN=L'RQCAUP2,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP2)                                       
F$OP2    LKMAP F,D#CAOP2,UBIN,TA#OP2,OLEN=L'RQCAOP2,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP2)                                       
F$UP3    LKMAP F,D#CAUP3,CHAR,TA#UP3,MAXLEN=L'RQCAUP3,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP3)                                       
F$OP3    LKMAP F,D#CAOP3,UBIN,TA#OP3,OLEN=L'RQCAOP3,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP3)                                       
F$UP4    LKMAP F,D#CAUP4,CHAR,TA#UP4,MAXLEN=L'RQCAUP4,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP4)                                       
F$OP4    LKMAP F,D#CAOP4,UBIN,TA#OP4,OLEN=L'RQCAOP4,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP4)                                       
F$UP5    LKMAP F,D#CAUP5,CHAR,TA#UP5,MAXLEN=L'RQCAUP5,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP5)                                       
F$OP5    LKMAP F,D#CAOP5,UBIN,TA#OP5,OLEN=L'RQCAOP5,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP5)                                       
F$UP6    LKMAP F,D#CAUP6,CHAR,TA#UP6,MAXLEN=L'RQCAUP6,                 +        
               OUTPUT=(D,B#SAVED,RQCAUP6)                                       
F$OP6    LKMAP F,D#CAOP6,UBIN,TA#OP6,OLEN=L'RQCAOP6,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOP6)                                       
F$2OP    LKMAP F,D#CA2OP,UBIN,TA#2OP,OLEN=L'RQCA2OP,MAXLEN=8,          +        
               OUTPUT=(D,B#SAVED,RQCA2OP)                                       
F$DBL    LKMAP F,D#CADBL,CHAR,TA#DBL,MAXLEN=L'RQCADBL,                 +        
               OUTPUT=(D,B#SAVED,RQCADBL)                                       
F$UA1    LKMAP F,D#CAUA1,CHAR,TA#UA1,MAXLEN=L'RQCAUA1,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA1)                                       
F$OA1    LKMAP F,D#CAOA1,UBIN,TA#OA1,OLEN=L'RQCAOA1,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA1)                                       
F$UA2    LKMAP F,D#CAUA2,CHAR,TA#UA2,MAXLEN=L'RQCAUA2,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA2)                                       
F$OA2    LKMAP F,D#CAOA2,UBIN,TA#OA2,OLEN=L'RQCAOA2,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA2)                                       
F$UA3    LKMAP F,D#CAUA3,CHAR,TA#UA3,MAXLEN=L'RQCAUA3,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA3)                                       
F$OA3    LKMAP F,D#CAOA3,UBIN,TA#OA3,OLEN=L'RQCAOA3,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA3)                                       
F$UA4    LKMAP F,D#CAUA4,CHAR,TA#UA4,MAXLEN=L'RQCAUA4,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA4)                                       
F$OA4    LKMAP F,D#CAOA4,UBIN,TA#OA4,OLEN=L'RQCAOA4,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA4)                                       
F$UA5    LKMAP F,D#CAUA5,CHAR,TA#UA5,MAXLEN=L'RQCAUA5,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA5)                                       
F$OA5    LKMAP F,D#CAOA5,UBIN,TA#OA5,OLEN=L'RQCAOA5,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA5)                                       
F$UA6    LKMAP F,D#CAUA6,CHAR,TA#UA6,MAXLEN=L'RQCAUA6,                 +        
               OUTPUT=(D,B#SAVED,RQCAUA6)                                       
F$OA6    LKMAP F,D#CAOA6,UBIN,TA#OA6,OLEN=L'RQCAOA6,MAXLEN=9,          +        
               OUTPUT=(D,B#SAVED,RQCAOA6)                                       
F$TR1    LKMAP F,D#CATR1,CHAR,TA#AFM1T,MAXLEN=L'RQCATR1,               +        
               OUTPUT=(D,B#SAVED,RQCATR1)                                       
F$TR2    LKMAP F,D#CATR2,CHAR,TA#AFM2T,MAXLEN=L'RQCATR2,               +        
               OUTPUT=(D,B#SAVED,RQCATR2)                                       
F$TR3    LKMAP F,D#CATR3,CHAR,TA#AFM3T,MAXLEN=L'RQCATR3,               +        
               OUTPUT=(D,B#SAVED,RQCATR3)                                       
F$TR4    LKMAP F,D#CATR4,CHAR,TA#AFM4T,MAXLEN=L'RQCATR4,               +        
               OUTPUT=(D,B#SAVED,RQCATR4)                                       
F$CMT    LKMAP F,D#CACMT,CHAR,TA#COMNT,MAXLEN=L'RQCACMT,               +        
               OUTPUT=(D,B#SAVED,RQCACMT)                                       
F$RDE    LKMAP F,D#CARDE,CHAR,TA#RODES,MAXLEN=L'RQCARDE,               +        
               OUTPUT=(D,B#SAVED,RQCARDE)                                       
F$MST    LKMAP F,D#CAMST,CHAR,TA#ONMST,MAXLEN=L'RQCAMST,               +        
               OUTPUT=(D,B#SAVED,RQCAMST)                                       
F$LFT    LKMAP F,D#CALFT,CHAR,TA#ONLFT,MAXLEN=L'RQCALFT,               +        
               OUTPUT=(D,B#SAVED,RQCALFT)                                       
F$CPA    LKMAP F,D#CACPA,CHAR,TA#CHKPA,MAXLEN=L'RQCACPA,               +        
               OUTPUT=(D,B#SAVED,RQCACPA)                                       
F$PCR    LKMAP F,D#CAPCR,CHAR,TA#PAYCR,MAXLEN=L'RQCAPCR,               +        
               OUTPUT=(D,B#SAVED,RQCAPCR)                                       
F$PUR    LKMAP F,D#CAPUR,CHAR,TA#NPAYC,MAXLEN=L'RQCAPUR,               +        
               OUTPUT=(D,B#SAVED,RQCAPUR)                                       
F$FGN    LKMAP F,D#CAFGN,CHAR,TA#FGN,MAXLEN=L'RQCAFGN,                 +        
               OUTPUT=(D,B#SAVED,RQCAFGN)                                       
F$INA    LKMAP F,D#CAINA,CHAR,TA#INA,MAXLEN=L'RQCAINA,                 +        
               OUTPUT=(D,B#SAVED,RQCAINA)                                       
F$INR    LKMAP F,D#CAINR,CHAR,TA#INR,MAXLEN=L'RQCAINR,                 +        
               OUTPUT=(D,B#SAVED,RQCAINR)                                       
F$ACP    LKMAP F,D#CAACP,CHAR,TA#ACBLP,MAXLEN=L'RQCAACP,               +        
               OUTPUT=(D,B#SAVED,RQCAACP)                                       
F$APP    LKMAP F,D#CAAPP,CHAR,TA#APAXP,MAXLEN=L'RQCAAPP,               +        
               OUTPUT=(D,B#SAVED,RQCAAPP)                                       
F$PDT    LKMAP F,D#CAPDT,CHAR,TA#PDFTR,MAXLEN=L'RQCAPDT,               +        
               OUTPUT=(D,B#SAVED,RQCAPDT)                                       
F$CLB    LKMAP F,D#CACLB,CHAR,TA#PDCLB,MAXLEN=L'RQCACLB,               +        
               OUTPUT=(D,B#SAVED,RQCACLB)                                       
F$GRR    LKMAP F,D#CAGRR,CHAR,TA#GRRAU,MAXLEN=L'RQCAGRR,               +        
               OUTPUT=(D,B#SAVED,RQCAGRR)                                       
F$VER    LKREQ F,D#CAVER,(I,B#SAVED,I$VERS),UBIN,LIST=F,               +        
               OLEN=1,MAXLEN=3,TEXT=TA#VER,COL=*                                
F$EUR    LKMAP F,D#CAEUR,CHAR,TA#EURO,MAXLEN=L'RQCAEUR,                +        
               OUTPUT=(D,B#SAVED,RQCAEUR)                                       
F$2UP1   LKMAP F,D#CA2U1,CHAR,TA#2UP1,MAXLEN=L'RQCA2U1,                +        
               OUTPUT=(D,B#SAVED,RQCA2U1)                                       
F$2OP1   LKMAP F,D#CA2P1,UBIN,TA#2OP1,OLEN=L'RQCA2P1,MAXLEN=9,         +        
               OUTPUT=(D,B#SAVED,RQCA2P1)                                       
F$2UP2   LKMAP F,D#CA2U2,CHAR,TA#2UP2,MAXLEN=L'RQCA2U2,                +        
               OUTPUT=(D,B#SAVED,RQCA2U2)                                       
F$2OP2   LKMAP F,D#CA2P2,UBIN,TA#2OP2,OLEN=L'RQCA2P2,MAXLEN=9,         +        
               OUTPUT=(D,B#SAVED,RQCA2P2)                                       
F$2UP3   LKMAP F,D#CA2U3,CHAR,TA#2UP3,MAXLEN=L'RQCA2U3,                +        
               OUTPUT=(D,B#SAVED,RQCA2U3)                                       
F$2OP3   LKMAP F,D#CA2P3,UBIN,TA#2OP3,OLEN=L'RQCA2P3,MAXLEN=9,         +        
               OUTPUT=(D,B#SAVED,RQCA2P3)                                       
F$2UP4   LKMAP F,D#CA2U4,CHAR,TA#2UP4,MAXLEN=L'RQCA2U4,                +        
               OUTPUT=(D,B#SAVED,RQCA2U4)                                       
F$2OP4   LKMAP F,D#CA2P4,UBIN,TA#2OP4,OLEN=L'RQCA2P4,MAXLEN=9,         +        
               OUTPUT=(D,B#SAVED,RQCA2P4)                                       
F$2UP5   LKMAP F,D#CA2U5,CHAR,TA#2UP5,MAXLEN=L'RQCA2U5,                +        
               OUTPUT=(D,B#SAVED,RQCA2U5)                                       
F$2OP5   LKMAP F,D#CA2P5,UBIN,TA#2OP5,OLEN=L'RQCA2P5,MAXLEN=9,         +        
               OUTPUT=(D,B#SAVED,RQCA2P5)                                       
F$RER    LKMAP F,D#CARER,PDAT,TA#RERC,OUTPUT=(D,B#SAVED,RQCARER)                
F$AFT    LKMAP F,D#CAAFT,CHAR,TA#AFT,MAXLEN=L'RQCAAFT,                 +        
               OUTPUT=(D,B#SAVED,RQCAAFT)                                       
F$TCO    LKMAP F,D#CATCO,HEXD,TA#TCO,OLEN=L'RQCATCO,MAXLEN=8,          +        
               OUTPUT=(D,B#SAVED,RQCATCO)                                       
F$TSQ    LKMAP F,D#CATSQ,HEXD,TA#TSQ,OLEN=L'RQCATSQ,MAXLEN=4,          +        
               OUTPUT=(D,B#SAVED,RQCATSQ)                                       
F$VSQ    LKMAP F,D#CAVSQ,HEXD,TA#VSQ,OLEN=L'RQCAVSQ,MAXLEN=8,          +        
               OUTPUT=(D,B#SAVED,RQCAVSQ)                                       
F$TFC    LKMAP F,D#CATFC,CHAR,TA#TFC,MAXLEN=L'RQCATFC,                 +        
               OUTPUT=(D,B#SAVED,RQCATFC)                                       
F$TTC    LKMAP F,D#CATTC,CHAR,TA#TTC,MAXLEN=L'RQCATTC,                 +        
               OUTPUT=(D,B#SAVED,RQCATTC)                                       
F$EQY    LKMAP F,D#CAEQY,CHAR,TA#EQY,MAXLEN=L'RQCAEQY,                 +        
               OUTPUT=(D,B#SAVED,RQCAEQY)                                       
F$ETM    LKMAP F,D#CAETM,CHAR,TA#ETM,MAXLEN=L'RQCAETM,                 +        
               OUTPUT=(D,B#SAVED,RQCAETM)                                       
F$WID    LKMAP F,D#CAWID,CHAR,TA#WAPID,MAXLEN=L'RQCAWID,               +        
               OUTPUT=(D,B#SAVED,RQCAWID)                                       
F$EOV    LKREQ F,D#CAEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,               +        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,D#CACMC,(I,B#SAVED,I$CLMC),UBIN,LIST=F,               +        
               OLEN=1,MAXLEN=3,TEXT=TA#CLRMC,COL=*                              
F$RID    LKMAP F,D#CARID,CHAR,TA#REQID,MAXLEN=L'RQCARID,               +        
               OUTPUT=(D,B#SAVED,RQCARID)                                       
F$WRI    LKMAP F,D#CAWRI,CHAR,TA#WARID,MAXLEN=L'RQCAWRI,               +        
               OUTPUT=(D,B#SAVED,RQCAWRI)                                       
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP - CAST DELETE UPLOAD                                    *         
***********************************************************************         
                                                                                
CADHDR   LKMAP H,I#CADULD,NEWREC=Y                                              
F$MOD    LKMAP F,D#CADMOD,UBIN,TA#PMODE,OLEN=L'RQCAMOD,MAXLEN=1,       +        
               OUTPUT=(D,B#SAVED,RQCAMOD)                                       
F$STF    LKMAP F,D#CADSTF,CHAR,TA#STAFF,MAXLEN=L'RQCASTF,              +        
               OUTPUT=(D,B#SAVED,RQCASTF)                                       
F$COM    LKMAP F,D#CADCOM,HEXD,TA#COMCD,OLEN=L'RQCACOM,MAXLEN=8,       +        
               OUTPUT=(D,B#SAVED,RQCACOM)                                       
F$SEQ    LKMAP F,D#CADSEQ,HEXD,TA#CSTSQ,OLEN=L'RQCASEQ,MAXLEN=4,       +        
               OUTPUT=(D,B#SAVED,RQCASEQ)                                       
F$SSN    LKMAP F,D#CADSSN,CHAR,TA#SSN,MAXLEN=L'RQCASSN,                +        
               OUTPUT=(D,B#SAVED,RQCASSN)                                       
F$WID    LKMAP F,D#CADWID,CHAR,TA#WAPID,MAXLEN=L'RQCAWID,              +        
               OUTPUT=(D,B#SAVED,RQCAWID)                                       
F$EOV    LKREQ F,D#CADEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,              +        
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
                                                                                
SVW4STA3 DS    XL(L'TAW4STA3)      SAVED W4 3RD STATUS BYTE                     
SVW4NAME DS    CL(L'TAW4CRPN)      SAVED NAME                                   
SVAGY    DS    CL(L'TLCOAGY)       SAVED AGENCY                                 
SVAYTPOF DS    CL(L'TAAYTPOF)      SAVED OFFICE                                 
SVAYSTA6 DS    XL(L'TAAYSTA6)      SAVED AGENY 6TH STATUS BYTE                  
SVCLI    DS    CL(L'TLCOCLI)       SAVED CLIENT                                 
SVCISTA2 DS    CL(L'TACISTA2)      SAVED CLIENT 2ND STATUS BYTE                 
SVCORP   DS    CL(L'TATICRPN)      SAVED CORPORATION NUMBER                     
SVCID    DS    CL(L'TACOCID)       SAVED COMMERCIAL ID                          
SVTITLE  DS    CL(L'TANANAME)      SAVED COMMERCIAL TITLE                       
SVTIME   DS    XL3                 SAVED TIME                                   
SVCAHKEY DS    XL(L'IOKEY)         SAVED CAST HOLDING FEE KEY                   
SVCOFCYC DS    XL(L'TACOFCYC)      SAVED FIRST FIXED CYCLE DATE                 
SVCOAIR  DS    XL(L'TACOAIR)       SAVED FIRST AIR DATE                         
SVCOEXP  DS    XL(L'TACOEXP)       SAVED EXPIRATION DATE                        
SVCOVDTE DS    XL(L'TACOVDTE)      SAVED VERIFICATION DATE                      
SVCOMED  DS    XL(L'TACOMED)       SAVED COMMERCIAL MEDIA                       
SVCOTYPE DS    XL(L'TACOTYPE)      SAVED COMMERCIAL TYPE                        
SVCOSTA2 DS    XL(L'TACOSTA2)      SAVED COMMERCIAL STATUS 2                    
SVCOSTAT DS    XL(L'TACOSTAT)      SAVED COMMERCIAL STATUS                      
SVNUNXTC DS    XL(L'TANUNXTC)      SAVED NEXT CAST SEQUENCE NUMBER              
SVCOWID  DS    CL(L'RQCAWID)       SAVED WEB APPLICATION ID (COML)              
SVWID    DS    CL(L'RQCAWID)       SAVED WEB APPLICATION ID (CAST)              
                                                                                
COMSTAT  DS    X                   COMMERCIAL STATUS                            
COUNVRFY EQU   X'80'               CHANGE UNVERIFIES THE COMMERCIAL             
COREISSU EQU   X'40'               CHANGE NECESSITATES HF REISSUE               
COCSTSEQ EQU   X'20'               CHANGE NECESSITATES CAST ADDITION            
                                                                                
LFTSTAT  DS    C                   COMMERCIAL LIFT STATUS                       
VERSTAT  DS    C                   COMMERCIAL VERSION STATUS                    
                                                                                
CASTSTAT DS    X                   CAST STATUS                                  
CAGENRLL EQU   X'80'               REQUEST A RELEASE LETTER                     
CAOELHLD EQU   X'40'               ORIGINALLY ELIGIBLE FOR HOLDING FEES         
CARECUPD EQU   X'20'               RECORD WAS UPDATED ON FILE                   
CAOPALL  EQU   X'10'               CAST HAS ALL OVERSCALE PERCENT               
CAOPARE  EQU   X'08'               CAST HAS ARE OVERSCALE PERCENT               
CANEWCRP EQU   X'04'               CORPORATION CODE IS BEING CHANGED            
                                                                                
CPYSTAT1 DS    X                   COPY ROUTINES' STATUS                        
CPYTACMG EQU   X'80'               GENERAL COMMENT ENCOUNTERED                  
CPYTACMD EQU   X'40'               ROLE DESCRIPTION ENCOUNTERED                 
CPYSTARL EQU   X'20'               RELEASE STATUS ENCOUNTERED                   
CPYSTAOA EQU   X'10'               OVERSCALE AMOUNT ENCOUNTERED                 
CPYSTAOP EQU   X'08'               OVERSCALE PERCENT ENCOUNTERED                
CPYTAFNK EQU   X'04'               CAST TRACKS ENCOUNTERED                      
CPYTAFNV EQU   X'02'               CAST VERSIONS ENCOUNTERED                    
CPYSTAO2 EQU   X'01'               OVERSCALE 2ND PERCENT ENCOUNTERED            
                                                                                
TRNSAGT  DS    XL(L'TACANCDE)      TRANSLATED AGENT CODE                        
                                                                                
ORIGVERS DS    XL255               ORIGINAL VERSIONS                            
VERSIONS DS    XL255               VERSIONS                                     
                                                                                
FCYCM89  DS    XL3                 COMMERCIAL FFC - 89 DAYS                     
FCYCP45  DS    XL3                 COMMERCIAL FFC + 45 DAYS                     
FCYCP90  DS    XL3                 COMMERCIAL FFC + 90 DAYS                     
                                                                                
TRNFRAGY DS    CL(L'TLCOAGY)       TRANSFERRED FROM AGENCY                      
TRNFRCID DS    CL(L'TACOCID)       TRANSFERRED FROM COMMERCIAL ID               
TLFCBLK  DS    XL(TLFCLNQ)         TALNKFCY BLOCK                               
TRNTACR1 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR2 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR3 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR4 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR5 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR6 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR7 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
TRNTACR8 DS    XL(TACRLNQ)         TRANSFERRED FROM APPLIED CREDIT ELEM         
                                                                                
ACATENT  DS    A                   A(CATEGORY ENTRY IN TASYSCATS)               
AUNIENT  DS    A                   A(UNION ENTRY IN TASYSUNIS)                  
AYEARENT DS    A                   A(YEAR ENTRY IN TASYSYEARS)                  
                                                                                
BLOCK    DS    CL(MQMLNQ)          SNDMQHFR BLOCK                               
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        CAST MAINTENANCE REQUEST MAP FIELDS                          *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQCAMOD  DS    CL1                 MODE                                         
RQCARTV  EQU   1                   RETRIEVE                                     
RQCAVFY  EQU   2                   VERIFY                                       
RQCAEXE  EQU   3                   EXECUTE                                      
RQCASTF  DS    CL8                 STAFF CODE                                   
RQCACOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCASRT  DS    XL4                 SORT KEY                                     
RQCASEQ  DS    XL2                 SEQUENCE NUMBER                              
RQCASSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
RQCACAT  DS    CL3                 CATEGORY                                     
RQCALFI  DS    XL4                 LIFTED FROM INTERNAL COMML NUMBER            
RQCAONO  DS    CL3                 ON/OFF CAMERA                                
RQCATAX  DS    CL3                 TAX UNIT CODE                                
RQCAUNI  DS    CL3                 UNION                                        
RQCALCL  DS    CL3                 LOCAL                                        
RQCACYR  DS    CL3                 CONTRACT YEAR                                
RQCAFFC  DS    XL3                 FIRST FIXED CYCLE                            
RQCAFSV  DS    XL3                 FIRST SERVICES DATE                          
RQCALSV  DS    XL3                 LAST SERVICES DATE                           
RQCARLL  DS    CL1                 RELEASE LETTER CODE                          
RQCAEFF  DS    XL3                 EFFECTIVE DATE                               
RQCAATC  DS    CL9                 ATTACHED CORPORATION FID#                    
RQCAAGT  DS    CL4                 AGENT CODE                                   
RQCAGUA  DS    CL4                 GUARANTEE CODE                               
RQCAEXP  DS    XL3                 EXPIRATION DATE                              
RQCAUP1  DS    CL3                 OVERSCALE PERCENTAGE USE #1                  
RQCAOP1  DS    XL4                 OVERSCALE PERCENTAGE #1                      
RQCAUP2  DS    CL3                 OVERSCALE PERCENTAGE USE #2                  
RQCAOP2  DS    XL4                 OVERSCALE PERCENTAGE #2                      
RQCAUP3  DS    CL3                 OVERSCALE PERCENTAGE USE #3                  
RQCAOP3  DS    XL4                 OVERSCALE PERCENTAGE #3                      
RQCAUP4  DS    CL3                 OVERSCALE PERCENTAGE USE #4                  
RQCAOP4  DS    XL4                 OVERSCALE PERCENTAGE #4                      
RQCAUP5  DS    CL3                 OVERSCALE PERCENTAGE USE #5                  
RQCAOP5  DS    XL4                 OVERSCALE PERCENTAGE #5                      
RQCAUP6  DS    CL3                 OVERSCALE PERCENTAGE USE #6                  
RQCAOP6  DS    XL4                 OVERSCALE PERCENTAGE #6                      
RQCA2OP  DS    XL4                 SECOND OVERSCALE PERCENTAGE                  
RQCADBL  DS    CL1                 DOUBLES                                      
RQCAUA1  DS    CL3                 OVERSCALE AMOUNT USE #1                      
RQCAOA1  DS    XL4                 OVERSCALE AMOUNT #1                          
RQCAUA2  DS    CL3                 OVERSCALE AMOUNT USE #2                      
RQCAOA2  DS    XL4                 OVERSCALE AMOUNT #2                          
RQCAUA3  DS    CL3                 OVERSCALE AMOUNT USE #3                      
RQCAOA3  DS    XL4                 OVERSCALE AMOUNT #3                          
RQCAUA4  DS    CL3                 OVERSCALE AMOUNT USE #4                      
RQCAOA4  DS    XL4                 OVERSCALE AMOUNT #4                          
RQCAUA5  DS    CL3                 OVERSCALE AMOUNT USE #5                      
RQCAOA5  DS    XL4                 OVERSCALE AMOUNT #5                          
RQCAUA6  DS    CL3                 OVERSCALE AMOUNT USE #6                      
RQCAOA6  DS    XL4                 OVERSCALE AMOUNT #6                          
RQCATR1  DS    CL1                 TRACK #1                                     
RQCATR2  DS    CL1                 TRACK #2                                     
RQCATR3  DS    CL1                 TRACK #3                                     
RQCATR4  DS    CL1                 TRACK #4                                     
RQCATR5  DS    CL1                 TRACK #5                                     
RQCATR6  DS    CL1                 TRACK #6                                     
RQCATR7  DS    CL1                 TRACK #7                                     
RQCACMT  DS    CL57                COMMENT                                      
RQCARDE  DS    CL57                ROLE DESCRIPTION                             
RQCAMST  DS    CL1                 ON MASTER COMMERCIAL?                        
RQCALFT  DS    CL1                 ON LIFT VERSION?                             
RQCACPA  DS    CL1                 CHECKS PAYABLE TO AGENT?                     
RQCAPCR  DS    CL1                 PAY CANADIAN RATES ON US COMMERCIAL?         
RQCAPUR  DS    CL1                 PAY US RATES ON CANADIAN COMMERCIAL?         
RQCAFGN  DS    CL1                 FOREIGN USE?                                 
RQCAINA  DS    CL1                 INTERACTIVE USE?                             
RQCAINR  DS    CL1                 INDUSTRIAL USE?                              
RQCAACP  DS    CL1                 APPLY CABLE TO PER CYCLE AMOUNT?             
RQCAAPP  DS    CL1                 APPLY PAX TO PER CYCLE AMOUNT?               
RQCAPDT  DS    CL1                 PRINT DAILY FIXED CYCLE TRACKING?            
RQCACLB  DS    CL1                 CELEBRITY?                                   
RQCAGRR  DS    CL1                 GRR COVERS ALL USE?                          
RQCAEUR  DS    CL1                 PAY IN EUROS?                                
RQCA2U1  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #1              
RQCA2P1  DS    XL4                 2ND OVERSCALE PERCENTAGE #1                  
RQCA2U2  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #2              
RQCA2P2  DS    XL4                 2ND OVERSCALE PERCENTAGE #2                  
RQCA2U3  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #3              
RQCA2P3  DS    XL4                 2ND OVERSCALE PERCENTAGE #3                  
RQCA2U4  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #4              
RQCA2P4  DS    XL4                 2ND OVERSCALE PERCENTAGE #4                  
RQCA2U5  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #5              
RQCA2P5  DS    XL4                 2ND OVERSCALE PERCENTAGE #5                  
RQCARER  DS    XL3                 RERECORD DATE                                
RQCAAFT  DS    CL1                 APPLY USE TO FTRACK?                         
RQCATCO  DS    XL4                 TRANSFERRED FROM INT COMM'L NUMBER           
RQCATSQ  DS    XL2                 TRANSFERRED FROM CAST SEQ NUMBER             
RQCAVSQ  DS    XL4                 VITA SEQUENCE NUMBER                         
RQCATFC  DS    CL40                TRANSFERRED FROM COMMENT                     
RQCATTC  DS    CL40                TRANSFERRED TO COMMENT                       
RQCAEQY  DS    CL1                 EQUITY?                                      
RQCAETM  DS    CL1                 ENFORCE COM'L TYPE/MEDIA VALIDATION?         
RQCAWID  DS    CL18                WEB APPLICATION ID                           
RQCARID  DS    CL6                 REQUEST ID                                   
RQCAWRI  DS    CL12                WEB APPLICATION RECORD ID                    
RQRCLNQ  EQU   *-RQCAMOD                                                        
                                                                                
I$VERS   DS    A                   A(VERSIONS)                                  
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
*        DSECT TO COVER REQUEST RECORD                                          
                                                                                
REQD     DSECT                                                                  
REQHDR   DS    CL26                HEADER                                       
REQREQ   DS    CL80                REQUEST CARD                                 
                                                                                
*        DSECT TO COVER USE TRANSLATION TABLE                                   
                                                                                
TOUTABD  DSECT                                                                  
TOUTOUSE DS    CL3                 ORIGINAL USE                                 
TOUTTUSE DS    CL3                 TRANSLATED USE                               
TOUTTYPE DS    CL1                 COMMERCIAL TYPE                              
TOUTMED  DS    CL1                 COMMERCIAL MEDIA                             
TOUTLNQ  EQU   *-TOUTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TALNK17   05/29/15'                                      
         END                                                                    
