*          DATA SET TALNK1C    AT LEVEL 001 AS OF 08/28/13                      
*PHASE T7041CA                                                                  
TALNK1C  TITLE 'COMMENT MAINTENANCE UPLOAD SERVER'                              
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TACM,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
WORKLNQ  EQU   ERRTAB                                                           
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1C**,RR=RE                                           
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
                                                                                
INPUT    BRAS  RE,CMUPLOAD         PROCESS THE INPUT RECORD                     
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
                                                                                
CMUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#CMULD)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   CMUP20                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   CMUP20                                                           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCMSTF                                   
         JNE   CMUP20                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         BRAS  RE,VALCONF          VALIDATE CONTRACT FIELDS                     
         JNE   CMUP20                                                           
         BRAS  RE,VALCOMF          VALIDATE COMMERCIAL FIELDS                   
         JNE   CMUP20                                                           
         BRAS  RE,VALGUAF          VALIDATE GUARANTEE FIELDS                    
         JNE   CMUP20                                                           
         BRAS  RE,VALINVF          VALIDATE INVOICE FIELDS                      
         JNE   CMUP20                                                           
         BRAS  RE,VALADVF          VALIDATE ADVICE FIELDS                       
         JNE   CMUP20                                                           
                                                                                
         USING TLCMD,R3                                                         
         MVI   TLCMCD,TLCMCDQ      READ FOR COMMENT KEY                         
         MVC   TLCMAGY,RQCMAGY                                                  
         MVC   TLCMTYP,RQCMTYP                                                  
         CLI   RQCMLEV,RQCMLEVT                                                 
         JNE   *+8                                                              
         MVI   TLCMLEV,X'80'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    CMUP10                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         J     CMUP20                                                           
                                                                                
CMUP10   BRAS  RE,INITCHA          AND PREPARE TO CHANGE                        
                                                                                
CMUP20   MVI   OUTPUT,CMSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    CMUP30                                                           
         MVI   OUTPUT,CMSTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    CMUP30                                                           
         MVI   OUTPUT,CMSTOK2                                                   
                                                                                
CMUP30   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCMMOD,RQCMEXE     IF MODE IS EXECUTE                           
         JNE   CMUP40                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    CMUP40                                                           
         BRAS  RE,EXECADD          ADD                                          
         BRAS  RE,EXECCHA          OR CHANGE COMMENT RECORD                     
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCMSTF),(L'RQCMSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
CMUP40   BRAS  RE,OUTCMTKY         OUTPUT COMMENT KEY FIELDS                    
                                                                                
         MVI   OUTPUT,C'N'                                                      
         TM    CMTSTAT,CMRECUPD                                                 
         JZ    CMUP50                                                           
         MVI   OUTPUT,C'Y'                                                      
CMUP50   GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',16),              +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#CMERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    YES                 SEND DOWN COMMENT RECORD                     
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#CMULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#CMSDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSSTF),        +        
               ('LD_CHARQ',RQCMSTF),(L'RQCMSTF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSTYP),        +        
               ('LD_CHARQ',RQCMTYP),(L'RQCMTYP,0)                               
                                                                                
         CLI   RQCMTYP,TLCMTGUA                                                 
         JNE   CMUP60                                                           
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSSSN),        +        
               ('LD_CHARQ',RQCMSSN),(L'RQCMSSN,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSGUA),        +        
               ('LD_CHARQ',RQCMGUA),(L'RQCMGUA,0)                               
         J     CMUP100                                                          
                                                                                
CMUP60   GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSLEV),        +        
               ('LD_CHARQ',RQCMLEV),(L'RQCMLEV,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSAGY),        +        
               ('LD_CHARQ',RQCMAGY),(L'RQCMAGY,0)                               
                                                                                
         CLI   RQCMTYP,TLCMTCON                                                 
         JNE   CMUP70                                                           
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSCON),        +        
               ('LD_CHARQ',RQCMCON),(L'RQCMCON,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSTST),        +        
               ('LD_CHARQ',RQCMTST),(L'RQCMTST,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSTEN),        +        
               ('LD_CHARQ',RQCMTEN),(L'RQCMTEN,0)                               
         J     CMUP100                                                          
                                                                                
CMUP70   CLI   RQCMTYP,TLCMTINV                                                 
         JNE   CMUP80                                                           
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSINV),        +        
               ('LD_CHARQ',RQCMINV),(L'RQCMINV,0)                               
         J     CMUP100                                                          
                                                                                
CMUP80   GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSCID),        +        
               ('LD_CHARQ',SVCID),(L'SVCID,0)                                   
                                                                                
         CLI   RQCMTYP,TLCMTCOM                                                 
         JNE   CMUP90                                                           
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSVER),        +        
               ('LD_CHARQ',RQCMVER),(L'RQCMVER,0)                               
         J     CMUP100                                                          
                                                                                
CMUP90   GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CMSADV),        +        
               ('LD_CHARQ',RQCMADV),(L'RQCMADV,0)                               
                                                                                
CMUP100  GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CMSTOK1  EQU   1                   NO ERRORS - COMMENT ADDED                    
CMSTOK2  EQU   2                   NO ERRORS - COMMENT CHANGED                  
CMSTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#CMMOD                                                    
         CLI   RQCMMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CMSTF                                                    
         OC    RQCMSTF,RQCMSTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CMLEV                                                    
         CLI   RQCMTYP,TLCMTGUA    IF COMMENT TYPE IS GUARANTEE                 
         JNE   AR10                                                             
         CLI   RQCMLEV,0           ASSERT THAT COMMENT LEVEL IS                 
         JNE   ARMIS               NOT PROVIDED                                 
         J     AR20                                                             
AR10     CLI   RQCMLEV,0           OTHERWISE, ASSERT IT IS PROVIDED             
         JE    ARMIS                                                            
                                                                                
AR20     MVI   BYTE1,D#CMTYP                                                    
         CLI   RQCMTYP,0           ASSERT THAT COMMENT TYPE IS                  
         JE    ARMIS               PROVIDED                                     
                                                                                
         CLI   RQCMTYP,TLCMTCON    IF COMMENT TYPE IS CONTRACT                  
         JNE   AR30                                                             
         OC    RQCMSSN,RQCMSSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JNZ   ASSNNAL             IS NOT PROVIDED                              
         OC    RQCMGUA,RQCMGUA     ASSERT THAT GUARANTEE CODE IS NOT            
         JNZ   AGUANAL             PROVIDED                                     
         OC    RQCMCOM,RQCMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   ACOMNAL             NUMBER IS NOT PROVIDED                       
         OC    RQCMVER,RQCMVER     ASSERT THAT VERSION IS NOT PROVIDED          
         JNZ   AVERNAL                                                          
         OC    RQCMAGY,RQCMAGY     ASSERT THAT AGENCY CODE IS                   
         JZ    AAGYMIS             PROVIDED                                     
         OC    RQCMCON,RQCMCON     ASSERT THAT CONTRACT CODE IS                 
         JZ    ACONMIS             PROVIDED                                     
         OC    RQCMTST,RQCMTST     ASSERT THAT TERM START DATE IS               
         JZ    ATSTMIS             PROVIDED                                     
         OC    RQCMTEN,RQCMTEN     ASSERT THAT TERM END DATE DATE IS            
         JZ    ATENMIS             PROVIDED                                     
         OC    RQCMINV,RQCMINV     ASSERT THAT INVOICE IS NOT PROVIDED          
         JNZ   AINVNAL                                                          
         OC    RQCMADV,RQCMADV     ASSERT THAT ADVICE IS NOT PROVIDED           
         JNZ   AADVNAL                                                          
         J     AR70                                                             
                                                                                
AR30     CLI   RQCMTYP,TLCMTCOM    IF COMMENT TYPE IS COMMERCIAL                
         JNE   AR40                                                             
         OC    RQCMSSN,RQCMSSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JNZ   ASSNNAL             IS NOT PROVIDED                              
         OC    RQCMGUA,RQCMGUA     ASSERT THAT GUARANTEE CODE IS NOT            
         JNZ   AGUANAL             PROVIDED                                     
         OC    RQCMCOM,RQCMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ACOMMIS             NUMBER IS PROVIDED                           
         OC    RQCMAGY,RQCMAGY     ASSERT THAT AGENCY CODE IS NOT               
         JNZ   AAGYNAL             PROVIDED                                     
         OC    RQCMCON,RQCMCON     ASSERT THAT CONTRACT CODE IS NOT             
         JNZ   ACONNAL             PROVIDED                                     
         OC    RQCMTST,RQCMTST     ASSERT THAT TERM START DATE IS NOT           
         JNZ   ATSTNAL             PROVIDED                                     
         OC    RQCMTEN,RQCMTEN     ASSERT THAT TERM END DATE DATE IS            
         JNZ   ATENNAL             NOT PROVIDED                                 
         OC    RQCMINV,RQCMINV     ASSERT THAT INVOICE IS NOT PROVIDED          
         JNZ   AINVNAL                                                          
         OC    RQCMADV,RQCMADV     ASSERT THAT ADVICE IS NOT PROVIDED           
         JNZ   AADVNAL                                                          
         J     AR70                                                             
                                                                                
AR40     CLI   RQCMTYP,TLCMTGUA    IF COMMENT TYPE IS GUARANTEE                 
         JNE   AR50                                                             
         OC    RQCMSSN,RQCMSSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JZ    ASSNMIS             IS PROVIDED                                  
         OC    RQCMGUA,RQCMGUA     ASSERT THAT GUARANTEE CODE IS                
         JZ    AGUAMIS             PROVIDED                                     
         OC    RQCMCOM,RQCMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   ACOMNAL             NUMBER IS NOT PROVIDED                       
         OC    RQCMVER,RQCMVER     ASSERT THAT VERSION IS NOT PROVIDED          
         JNZ   AVERNAL                                                          
         OC    RQCMAGY,RQCMAGY     ASSERT THAT AGENCY CODE IS NOT               
         JNZ   AAGYNAL             PROVIDED                                     
         OC    RQCMCON,RQCMCON     ASSERT THAT CONTRACT CODE IS NOT             
         JNZ   ACONNAL             PROVIDED                                     
         OC    RQCMTST,RQCMTST     ASSERT THAT TERM START DATE IS NOT           
         JNZ   ATSTNAL             PROVIDED                                     
         OC    RQCMTEN,RQCMTEN     ASSERT THAT TERM END DATE DATE IS            
         JNZ   ATENNAL             NOT PROVIDED                                 
         OC    RQCMINV,RQCMINV     ASSERT THAT INVOICE IS NOT PROVIDED          
         JNZ   AINVNAL                                                          
         OC    RQCMADV,RQCMADV     ASSERT THAT ADVICE IS NOT PROVIDED           
         JNZ   AADVNAL                                                          
         J     AR70                                                             
                                                                                
AR50     CLI   RQCMTYP,TLCMTINV    IF COMMENT TYPE IS INVOICE                   
         JNE   AR60                                                             
         OC    RQCMSSN,RQCMSSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JNZ   ASSNNAL             IS NOT PROVIDED                              
         OC    RQCMGUA,RQCMGUA     ASSERT THAT GUARANTEE CODE IS NOT            
         JNZ   AGUANAL             PROVIDED                                     
         OC    RQCMCOM,RQCMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   ACOMNAL             NUMBER IS PROVIDED                           
         OC    RQCMVER,RQCMVER     ASSERT THAT VERSION IS NOT PROVIDED          
         JNZ   AVERNAL                                                          
         OC    RQCMAGY,RQCMAGY     ASSERT THAT AGENCY CODE IS                   
         JZ    AAGYMIS             PROVIDED                                     
         OC    RQCMCON,RQCMCON     ASSERT THAT CONTRACT CODE IS NOT             
         JNZ   ACONNAL             PROVIDED                                     
         OC    RQCMTST,RQCMTST     ASSERT THAT TERM START DATE IS NOT           
         JNZ   ATSTNAL             PROVIDED                                     
         OC    RQCMTEN,RQCMTEN     ASSERT THAT TERM END DATE DATE IS            
         JNZ   ATENNAL             NOT PROVIDED                                 
         OC    RQCMINV,RQCMINV     ASSERT THAT INVOICE IS PROVIDED              
         JZ    AINVMIS                                                          
         OC    RQCMADV,RQCMADV     ASSERT THAT ADVICE IS NOT PROVIDED           
         JNZ   AADVNAL                                                          
         J     AR70                                                             
                                                                                
AR60     CLI   RQCMTYP,TLCMTADV    IF COMMENT TYPE IS ADVICE                    
         JNE   AR70                                                             
         OC    RQCMSSN,RQCMSSN     ASSERT THAT SOCIAL SECURITY NUMBER           
         JNZ   ASSNNAL             IS NOT PROVIDED                              
         OC    RQCMGUA,RQCMGUA     ASSERT THAT GUARANTEE CODE IS NOT            
         JNZ   AGUANAL             PROVIDED                                     
         OC    RQCMCOM,RQCMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ACOMMIS             NUMBER IS PROVIDED                           
         OC    RQCMVER,RQCMVER     ASSERT THAT VERSION IS NOT PROVIDED          
         JNZ   AVERNAL                                                          
         OC    RQCMAGY,RQCMAGY     ASSERT THAT AGENCY CODE IS                   
         JNZ   AAGYNAL             PROVIDED                                     
         OC    RQCMCON,RQCMCON     ASSERT THAT CONTRACT CODE IS NOT             
         JNZ   ACONNAL             PROVIDED                                     
         OC    RQCMTST,RQCMTST     ASSERT THAT TERM START DATE IS NOT           
         JNZ   ATSTNAL             PROVIDED                                     
         OC    RQCMTEN,RQCMTEN     ASSERT THAT TERM END DATE DATE IS            
         JNZ   ATENNAL             NOT PROVIDED                                 
         OC    RQCMINV,RQCMINV     ASSERT THAT INVOICE IS PROVIDED              
         JNZ   AINVNAL                                                          
         OC    RQCMADV,RQCMADV     ASSERT THAT ADVICE IS NOT PROVIDED           
         JZ    AADVMIS                                                          
                                                                                
AR70     MVI   BYTE1,D#CMWID                                                    
         OC    RQCMWID,RQCMWID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
ASSNMIS  MVI   BYTE1,D#CMSSN                                                    
         J     ARMIS                                                            
ASSNNAL  MVI   BYTE1,D#CMSSN                                                    
         J     ARNAL                                                            
                                                                                
AGUAMIS  MVI   BYTE1,D#CMGUA                                                    
         J     ARMIS                                                            
AGUANAL  MVI   BYTE1,D#CMGUA                                                    
         J     ARNAL                                                            
                                                                                
ACOMMIS  MVI   BYTE1,D#CMCOM                                                    
         J     ARMIS                                                            
ACOMNAL  MVI   BYTE1,D#CMCOM                                                    
         J     ARNAL                                                            
                                                                                
AVERMIS  MVI   BYTE1,D#CMVER                                                    
         J     ARMIS                                                            
AVERNAL  MVI   BYTE1,D#CMVER                                                    
         J     ARNAL                                                            
                                                                                
AAGYMIS  MVI   BYTE1,D#CMAGY                                                    
         J     ARMIS                                                            
AAGYNAL  MVI   BYTE1,D#CMAGY                                                    
         J     ARNAL                                                            
                                                                                
ACONMIS  MVI   BYTE1,D#CMCON                                                    
         J     ARMIS                                                            
ACONNAL  MVI   BYTE1,D#CMCON                                                    
         J     ARNAL                                                            
                                                                                
ATSTMIS  MVI   BYTE1,D#CMTST                                                    
         J     ARMIS                                                            
ATSTNAL  MVI   BYTE1,D#CMTST                                                    
         J     ARNAL                                                            
                                                                                
ATENMIS  MVI   BYTE1,D#CMTEN                                                    
         J     ARMIS                                                            
ATENNAL  MVI   BYTE1,D#CMTEN                                                    
         J     ARNAL                                                            
                                                                                
AINVMIS  MVI   BYTE1,D#CMINV                                                    
         J     ARMIS                                                            
AINVNAL  MVI   BYTE1,D#CMINV                                                    
         J     ARNAL                                                            
                                                                                
AADVMIS  MVI   BYTE1,D#CMADV                                                    
         J     ARMIS                                                            
AADVNAL  MVI   BYTE1,D#CMADV                                                    
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
         MVI   BYTE1,D#CMMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCMMOD)                        
         JNE   AVINV                                                            
                                                                                
         CLI   RQCMLEV,0           VALIDATE COMMENT LEVEL                       
         JE    AV10                                                             
         CLI   RQCMLEV,RQCMLEVC                                                 
         JE    AV10                                                             
         CLI   RQCMLEV,RQCMLEVT                                                 
         JE    AV10                                                             
         MVI   BYTE1,D#CMLEV                                                    
         J     AVINV                                                            
                                                                                
AV10     CLI   RQCMTYP,TLCMTCON    VALIDATE TYPE                                
         JE    AV20                                                             
         CLI   RQCMTYP,TLCMTCOM                                                 
         JE    AV20                                                             
         CLI   RQCMTYP,TLCMTGUA                                                 
         JE    AV20                                                             
         CLI   RQCMTYP,TLCMTINV                                                 
         JE    AV20                                                             
         CLI   RQCMTYP,TLCMTADV                                                 
         JE    AV20                                                             
         MVI   BYTE1,D#CMTYP                                                    
         J     AVINV                                                            
                                                                                
AV20     GOTOR (#VALFLD,AVALFLD),DMCB,('VFWID',RQCMWID)                         
         JE    YES                                                              
         MVI   BYTE1,D#CMWID                                                    
         J     AVINV                                                            
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CONTRACT FIELDS AND INITIALIZE COMMENT KEY          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCONF  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCMTYP,TLCMTCON    IF COMMENT TYPE IS CONTRACT                  
         JNE   YES                                                              
                                                                                
         BRAS  RE,VALAGY           VALIDATE PASSED AGENCY CODE                  
         JNE   NO                                                               
                                                                                
         LA    R2,ERCMCNNF                                                      
                                                                                
         USING TLCND,R3                                                         
         XC    TLCNKEY,TLCNKEY     READ FOR CONTRACT KEY                        
         MVI   TLCNCD,TLCNCDQ      AND ENSURE IT EXISTS                         
         MVC   TLCNAGY,RQCMCON                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VCONF20                                                          
VCONF10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VCONF20  CLC   IOKEY(TLCNTRMS-TLCND),IOKEYSAV                                   
         JE    VCONF30                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,(R2)                                      
         J     NO                                                               
                                                                                
         LA    R2,ERCMTSNF                                                      
VCONF30  CLC   TLCNTRMS,RQCMTST                                                 
         JNE   VCONF10                                                          
                                                                                
         LA    R2,ERCMTENF                                                      
         CLC   TLCNTRME,RQCMTEN                                                 
         JNE   VCONF10                                                          
         DROP  R3                                                               
                                                                                
         USING TLCMD,R3                                                         
         XC    TLCMKEY,TLCMKEY     SET CONTRACT INFORMATION INTO                
         MVC   TLCMTRMS,RQCMTST    COMMENT KEY                                  
         MVC   TLCMTRME,RQCMTEN                                                 
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCONF AND INITIALIZE COMMENT KEY         *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMCNNF DC    AL1(ECMCNNFX-*),AL2(6),AL1(ERRCATY1),AL1(D#CMCON)                
         DC    C'Contract ID is not on file'                                    
ECMCNNFX EQU   *                                                                
                                                                                
ERCMTSNF DC    AL1(ECMTSNFX-*),AL2(7),AL1(ERRCATY1),AL1(D#CMTST)                
         DC    C'Term Start Date invalid for this Contract ID'                  
ECMTSNFX EQU   *                                                                
                                                                                
ERCMTENF DC    AL1(ECMTENFX-*),AL2(8),AL1(ERRCATY1),AL1(D#CMTEN)                
         DC    C'Term End Date invalid for this Contract ID'                    
ECMTENFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE COMMERCIAL FIELDS AND INITIALIZE COMMENT KEY        *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALCOMF  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCMTYP,TLCMTCOM    IF COMMENT TYPE IS COMMERCIAL                
         JNE   YES                                                              
                                                                                
         BRAS  RE,VALCOM           VALIDATE PASSED INTERNAL COMMERCIAL          
         JNE   NO                  NUMBER                                       
                                                                                
         USING TLVRD,R3                                                         
         CLI   RQCMVER,1                                                        
         JNH   VCOMF10                                                          
         XC    TLVRKEY,TLVRKEY     READ FOR VERSION KEY                         
         MVI   TLVRCD,TLVRCDQ      AND ENSURE IT EXISTS                         
         MVC   TLVRCOM,RQCMCOM                                                  
         MVC   TLVRVER,RQCMVER                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VCOMF10                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMVRNF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLCMD,R3                                                         
VCOMF10  XC    TLCMKEY,TLCMKEY     SET COMMERCIAL/VERSION INFORMATION           
         MVC   TLCMCID,SVCID       INTO COMMENT KEY                             
         MVC   TLCMICOM,RQCMCOM                                                 
         MVC   TLCMVER,RQCMVER                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOMF                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMVRNF DC    AL1(ECMVRNFX-*),AL2(4),AL1(ERRCATY1),AL1(D#CMVER)                
         DC    C'Version is not on file'                                        
ECMVRNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE GUARANTEE FIELDS AND INITIALIZE COMMENT KEY         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALGUAF  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCMTYP,TLCMTGUA    IF COMMENT TYPE IS GUARANTEE                 
         JNE   YES                                                              
                                                                                
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY     READ FOR W4 KEY                              
         MVI   TLW4CD,TLW4CDQ      AND ENSURE IT EXISTS                         
         MVC   TLW4SSN,RQCMSSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VGUAF10                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMW4NF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLGUD,R3                                                         
VGUAF10  XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      READ FOR GUARANTEE KEY                       
         MVC   TLGUSSN,RQCMSSN     AND ENSURE IT EXISTS                         
         MVC   TLGUGUA,RQCMGUA                                                  
         XC    TLGUGUA,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VGUAF20                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMGUNF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLCMD,R3                                                         
VGUAF20  XC    TLCMKEY,TLCMKEY     SET GUARANTEE INFORMATION                    
         MVC   TLCMSSN,RQCMSSN     INTO COMMENT KEY                             
         MVC   TLCMGUA,RQCMGUA                                                  
         XC    TLCMGUA,=4X'FF'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALGUAF                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMW4NF DC    AL1(ECMW4NFX-*),AL2(1),AL1(ERRCATY1),AL1(D#CMSSN)                
         DC    C'W4 not on file'                                                
ECMW4NFX EQU   *                                                                
                                                                                
ERCMGUNF DC    AL1(ECMGUNFX-*),AL2(2),AL1(ERRCATY1),AL1(D#CMGUA)                
         DC    C'Guarantee code is not on file'                                 
ECMGUNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE INVOICE FIELDS AND INITIALIZE COMMENT KEY           *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALINVF  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCMTYP,TLCMTINV    IF COMMENT TYPE IS INVOICE                   
         JNE   YES                                                              
                                                                                
         BRAS  RE,VALAGY           VALIDATE PASSED AGENCY CODE                  
         JNE   NO                                                               
                                                                                
         GOTOR (#CVTINV,ACVTINV),DMCB,RQCMINV,SVCINV                            
         XC     SVCINV,=6X'FF'     SET INVOICE VARIABLES                        
                                                                                
         USING TLIND,R3                                                         
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      READ FOR INVOICE KEY                         
         MVC   TLINAGY,RQCMAGY                                                  
         MVC   TLININV,SVCINV                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   VINVF10                                                          
         TM    IOKEY+TLDRSTAT-TLDRD,TLINSDEL                                    
         JZ    VINVF20                                                          
VINVF10  GOTOR (#ADDERR,AADDERR),DMCB,ERCMINNF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLCMD,R3                                                         
VINVF20  XC    TLCMKEY,TLCMKEY     SET INVOICE INFORMATION                      
         MVC   TLCMINV,SVCINV      INTO COMMENT KEY                             
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALINVF                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMINNF DC    AL1(ECMINNFX-*),AL2(9),AL1(ERRCATY1),AL1(D#CMINV)                
         DC    C'Invoice is not on file'                                        
ECMINNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ADVICE FIELDS AND INITIALIZE COMMENT KEY            *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALADVF  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCMTYP,TLCMTADV    IF COMMENT TYPE IS ADVICE                    
         JNE   YES                                                              
                                                                                
         BRAS  RE,VALCOM           VALIDATE PASSED INTERNAL COMMERCIAL          
         JNE   NO                  NUMBER                                       
                                                                                
         USING TLDVD,R3                                                         
         XC    TLDVKEY,TLDVKEY     READ FOR ADVICE KEY                          
         MVI   TLDVCD,TLDVCDQ      AND ENSURE IT EXISTS                         
         MVC   TLDVAGY,RQCMAGY                                                  
         MVC   TLDVCID,SVCID                                                    
         MVC   TLDVADV,RQCMADV                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VADVF10                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMDVNF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
         USING TLCMD,R3                                                         
VADVF10  XC    TLCMKEY,TLCMKEY     SET ADVICE INFORMATION                       
         MVC   TLCMVCID,SVCID      INTO COMMENT KEY                             
         MVC   TLCMVADV,RQCMADV                                                 
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALADVF                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMDVNF DC    AL1(ECMDVNFX-*),AL2(10),AL1(ERRCATY1),AL1(D#CMADV)               
         DC    C'Advice is not on file'                                         
ECMDVNFX EQU   *                                                                
                                                                                
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
         MVI   TLAYCD,TLCMCDQ      AND ENSURE IT EXISTS                         
         MVC   TLAYAGY,RQCMAGY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMAYNF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAGY                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMAYNF DC    AL1(ECMAYNFX-*),AL2(5),AL1(ERRCATY1),AL1(D#CMAGY)                
         DC    C'Agency not on file'                                            
ECMAYNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED INTERNAL COMMERCIAL NUMBER                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
VALCOM   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOCCDQ    AND ENSURE IT EXISTS                         
         MVC   TLCOCCOM,RQCMCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VCOM10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMCONF                                  
         J     NO                                                               
         DROP  R3                                                               
                                                                                
VCOM10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCOD,R4                                                         
         MVC   RQCMAGY,TLCOAGY     SAVE COMMERCIAL AGENCY                       
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVCID,TACOCID       SAVE COMMERCIAL ID                           
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCOM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMCONF DC    AL1(ECMCONFX-*),AL2(3),AL1(ERRCATY1),AL1(D#CMCOM)                
         DC    C'Commercial is not on file'                                     
ECMCONFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF COMMENT RECORD           *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERCMNTFD                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMNTFD DC    AL1(ECMNTFDX-*),AL2(11),AL1(ERRCATY2),AL1(D#CMSSN)               
         DC    C'Comment not on file'                                           
ECMNTFDX EQU   *                                                                
                                                                                
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
         MVI   ACTION,ACTCHA       IF FOUND, SET ACTION TO CHANGE               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC10  BRAS  RE,NEXTEL                                                        
         JNE   INITC20                                                          
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC10                                                          
                                                                                
         CLI   0(R4),TAXCELQ       PROCESS EXTENDED COMMENT ELEMENTS            
         JNE   *+12                                                             
         BRAS  RE,CPYTAXC                                                       
         J     INITC10                                                          
                                                                                
         CLI   0(R4),TAFNELQ       DELETE FREE FORM NAME ELEMENTS               
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC10                                                          
                                                                                
INITC20  BRAS  RE,NEWTAXC          PROCESS NEW EXTENDED COMMENTS                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES EXTENDED COMMENT ELEMENT INTO REQUEST MAP     *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(GUARANTEE DETAILS ELEMENT                *         
***********************************************************************         
                                                                                
         USING TAXCD,R4                                                         
CPYTAXC  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R3,TAXCSEQ          SAVE SEQUENCE NUMBER                         
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAXCLEN          COPY EXISTING COMMENT ELEMENT                
         BCTR  RF,0                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         BRAS  RE,BLDERENT         BUILD ERROR ENTRY IN ELEM2                   
                                                                                
         BCTR  R3,0                                                             
                                                                                
         LA    RF,CMCPYSTA                                                      
         AR    RF,R3                                                            
         MVI   0(RF),X'FF'         SET CURRENT COMMENT PROCESSED                
                                                                                
         LA    RF,RQCMC1                                                        
         MHI   R3,L'RQCMC1                                                      
         AR    RF,R3               RF=A(CURRENT REQUEST COMMENT FIELD)          
                                                                                
         USING TAXCD,R2                                                         
         OC    TAXCCMNT(L'RQCMC1),SPACES                                        
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQCMC1,(RF)),TAXCCMNT,        +        
               ELEM2,0                                                          
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
                                                                                
***********************************************************************         
*        IF EXTENDED COMMENT ELEMENT DID NOT EXIST ON RECORD          *         
*        PREVIOUSLY AND EXTENDED COMMENT IS BEING ADDED NOW,          *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAXC  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CMCPYSTA         PREPARE TO LOOP THROUGH ALL                  
         LHI   R3,1                REQUEST COMMENT FIELDS                       
                                                                                
NXC10    CLI   0(R2),X'FF'         IF COMMENT DID NOT ALREADY EXIST             
         JE    NXC20               ON RECORD                                    
         LA    RF,RQCMC1                                                        
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,L'RQCMC1                                                      
         AR    RF,RE                                                            
         OC    0(L'RQCMC1,RF),0(RF)                                             
         JZ    NXC20               AND IS BEING ADDED NOW                       
         BRAS  RE,BLDERENT         BUILD ERROR ENTRY IN ELEM2                   
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',ELEM2)                         
                                                                                
NXC20    CHI   R3,16                                                            
         JE    XIT                                                              
         LA    R2,1(R2)                                                         
         AHI   R3,1                                                             
         J     NXC10                                                            
                                                                                
***********************************************************************         
*        ROUTINE BUILDS ERROR ENTRY BASED ON SEQUENCE NUMBER          *         
*        IN R3                                                        *         
*        ON ENTRY ... R3=A(COMMENT SEQUENCE NUMBER)                   *         
***********************************************************************         
                                                                                
BLDERENT NTR1  BASE=*,LABEL=*                                                   
         USING ERRENTD,R1                                                       
         LA    R1,ELEM2            COPY GENERIC ERROR MESSAGE                   
         MVC   0(255,R1),ERCMCMT   INTO ELEM2                                   
                                                                                
         EDIT  (R3),(L'ECMCMTN,ELEM2+ECMCMTN-ERCMCMT),ALIGN=LEFT                
                                                                                
         BCTR  R3,0                                                             
                                                                                
         ZICM  RF,EENUMB,2                                                      
         AR    RF,R3                                                            
         STCM  RF,3,EENUMB         ADJUST ERROR NUMBER                          
                                                                                
         ZIC   RF,EEFIELD                                                       
         AR    RF,R3                                                            
         STC   RF,EEFIELD          ADJUST ERROR FIELD                           
         J     XIT                                                              
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAXC AND NEWTAXC                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCMCMT  DC    AL1(ECMCMTX-*),AL2(12),AL1(ERRCATY2),AL1(D#CMC1)                 
         DC    C'Review update to Comment #'                                    
ECMCMTN  DC    C'  '                                                            
ECMCMTX  EQU   *                                                                
                                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS COMMENT RECORD TO FIEL                          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(COMMENT RECORD)                            *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
                                                                                
         USING TLCMD,R4                                                         
         MVC   TLCMKEY,IOKEYSAV    BUILD KEY WITH SAVED KEY                     
         MVI   TLCMLEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         BRAS  RE,BLDREC           BUILD COMMENT RECORD                         
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         OI    CMTSTAT,CMRECUPD                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING COMMENT RECORD                      *         
*        ON ENTRY ... R4=A(COMMENT RECORD)                            *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                 DELETE ALL MARKED ELEMENTS                   
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         BRAS  RE,BLDREC           BUILD COMMENT RECORD                         
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         JNE   XIT                                                              
         OI    CMTSTAT,CMRECUPD                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD COMMENT RECORD                                         *         
*        ON ENTRY ... R4=A(COMMEN RECORD)                             *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,ADDTAXC          ADD EXCLUDED USE ELEMENTS                    
******** GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQCMWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQCMSTF,SVTIME             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ADD EXTENDED COMMENT ELEMENTS                                *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R3=A(COMMENT RECORD)                            *         
***********************************************************************         
                                                                                
ADDTAXC  NTR1                                                                   
         LA    R3,RQCMC1                                                        
         LHI   R0,1                                                             
                                                                                
AXC10    OC    0(L'RQCMC1,R3),0(R3)                                             
         JZ    AXC20                                                            
                                                                                
         USING TAXCD,R2                                                         
         XC    ELEM,ELEM           INITIALIZE EXTENDED COMMENT ELEMENT          
         MVI   TAXCEL,TAXCELQ                                                   
         MVI   TAXCLEN,TAXCLNQ                                                  
         STC   R0,TAXCSEQ          PUT COMMENT INTO ELEMENT                     
         MVC   TAXCCMNT(L'RQCMC1),0(R3)                                         
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
AXC20    CHI   R0,16                                                            
         JE    XIT                                                              
         LA    R3,L'RQCMC1(R3)                                                  
         AHI   R0,1                                                             
         J     AXC10                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS COMMENT KEY FIELDS                           *         
***********************************************************************         
                                                                                
OUTCMTKY NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',RQCMLEV),(L'RQCMLEV,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',5),               +        
               ('LD_CHARQ',RQCMTYP),(L'RQCMTYP,0)                               
                                                                                
         OC    RQCMSSN,RQCMSSN                                                  
         JZ    OCK00                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',6),               +        
               ('LD_CHARQ',RQCMSSN),(L'RQCMSSN,0)                               
                                                                                
OCK00    OC    RQCMGUA,RQCMGUA                                                  
         JZ    OCK10                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',7),               +        
               ('LD_CHARQ',RQCMGUA),(L'RQCMGUA,0)                               
                                                                                
OCK10    OC    RQCMCOM,RQCMCOM                                                  
         JZ    OCK20                                                            
         GOTO1 VHEXOUT,DMCB,RQCMCOM,OUTPUT,L'RQCMCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',8),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
OCK20    OC    RQCMVER,RQCMVER                                                  
         JZ    OCK30                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',9),               +        
               ('LD_UBINQ',RQCMVER),(L'RQCMVER,0)                               
                                                                                
OCK30    OC    RQCMAGY,RQCMAGY                                                  
         JZ    OCK40                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',10),              +        
               ('LD_CHARQ',RQCMAGY),(L'RQCMAGY,0)                               
                                                                                
OCK40    OC    RQCMCON,RQCMCON                                                  
         JZ    OCK50                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',11),              +        
               ('LD_CHARQ',RQCMCON),(L'RQCMCON,0)                               
OCK50    OC    RQCMTST,RQCMTST                                                  
         JZ    OCK60                                                            
         GOTO1 VDATCON,DMCB,(1,RQCMTST),(8,OUTPUT)                              
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',12),           +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
OCK60    OC    RQCMTEN,RQCMTEN                                                  
         JZ    OCK70                                                            
         GOTO1 VDATCON,DMCB,(1,RQCMTEN),(8,OUTPUT)                              
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',13),           +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
OCK70    OC    RQCMADV,RQCMADV                                                  
         JZ    OCK80                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',14),              +        
               ('LD_CHARQ',RQCMADV),(L'RQCMADV,0)                               
                                                                                
OCK80    OC    RQCMINV,RQCMINV                                                  
         JZ    XIT                                                              
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',15),              +        
               ('LD_CHARQ',RQCMINV),(L'RQCMINV,0)                               
         J     XIT                                                              
                                                                                
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
* REQUEST MAP - COMMENT MAINTENANCE UPLOAD                            *         
***********************************************************************         
                                                                                
CMHDR    LKMAP H,I#CMULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#CMMOD,UBIN,TA#PMODE,OLEN=L'RQCMMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCMMOD)                                       
F$STF    LKMAP F,D#CMSTF,CHAR,TA#STAFF,MAXLEN=L'RQCMSTF,               +        
               OUTPUT=(D,B#SAVED,RQCMSTF)                                       
F$LEV    LKMAP F,D#CMLEV,CHAR,TA#CMLEV,OLEN=L'RQCMLEV,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCMLEV)                                       
F$TYP    LKMAP F,D#CMTYP,CHAR,TA#CMTYP,OLEN=L'RQCMTYP,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCMTYP)                                       
F$SSN    LKMAP F,D#CMSSN,CHAR,TA#SSN,MAXLEN=L'RQCMSSN,                 +        
               OUTPUT=(D,B#SAVED,RQCMSSN)                                       
F$GUA    LKMAP F,D#CMGUA,CHAR,TA#GRTCD,MAXLEN=L'RQCMGUA,               +        
               OUTPUT=(D,B#SAVED,RQCMGUA)                                       
F$COM    LKMAP F,D#CMCOM,HEXD,TA#COMCD,OLEN=L'RQCMCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCMCOM)                                       
F$VER    LKMAP F,D#CMVER,UBIN,TA#VER,OLEN=L'RQCMVER,MAXLEN=3,          +        
               OUTPUT=(D,B#SAVED,RQCMVER)                                       
F$AGY    LKMAP F,D#CMAGY,CHAR,TA#AGYCD,MAXLEN=L'RQCMAGY,               +        
               OUTPUT=(D,B#SAVED,RQCMAGY)                                       
F$CON    LKMAP F,D#CMCON,CHAR,TA#CNTID,MAXLEN=L'RQCMCON,               +        
               OUTPUT=(D,B#SAVED,RQCMCON)                                       
F$PST    LKMAP F,D#CMTST,PDAT,TA#CNTST,OUTPUT=(D,B#SAVED,RQCMTST)               
F$PEN    LKMAP F,D#CMTEN,PDAT,TA#CNTEN,OUTPUT=(D,B#SAVED,RQCMTEN)               
F$INV    LKMAP F,D#CMINV,CHAR,TA#INV,MAXLEN=L'RQCMINV,                 +        
               OUTPUT=(D,B#SAVED,RQCMINV)                                       
F$ADV    LKMAP F,D#CMADV,CHAR,TA#DVNUM,MAXLEN=L'RQCMADV,               +        
               OUTPUT=(D,B#SAVED,RQCMADV)                                       
F$CM1    LKMAP F,D#CMC1,CHAR,TA#CMC1,MAXLEN=L'RQCMC1,                  +        
               OUTPUT=(D,B#SAVED,RQCMC1)                                        
F$CM2    LKMAP F,D#CMC2,CHAR,TA#CMC2,MAXLEN=L'RQCMC2,                  +        
               OUTPUT=(D,B#SAVED,RQCMC2)                                        
F$CM3    LKMAP F,D#CMC3,CHAR,TA#CMC3,MAXLEN=L'RQCMC3,                  +        
               OUTPUT=(D,B#SAVED,RQCMC3)                                        
F$CM4    LKMAP F,D#CMC4,CHAR,TA#CMC4,MAXLEN=L'RQCMC4,                  +        
               OUTPUT=(D,B#SAVED,RQCMC4)                                        
F$CM5    LKMAP F,D#CMC5,CHAR,TA#CMC5,MAXLEN=L'RQCMC5,                  +        
               OUTPUT=(D,B#SAVED,RQCMC5)                                        
F$CM6    LKMAP F,D#CMC6,CHAR,TA#CMC6,MAXLEN=L'RQCMC6,                  +        
               OUTPUT=(D,B#SAVED,RQCMC6)                                        
F$CM7    LKMAP F,D#CMC7,CHAR,TA#CMC7,MAXLEN=L'RQCMC7,                  +        
               OUTPUT=(D,B#SAVED,RQCMC7)                                        
F$CM8    LKMAP F,D#CMC8,CHAR,TA#CMC8,MAXLEN=L'RQCMC8,                  +        
               OUTPUT=(D,B#SAVED,RQCMC8)                                        
F$CM9    LKMAP F,D#CMC9,CHAR,TA#CMC9,MAXLEN=L'RQCMC9,                  +        
               OUTPUT=(D,B#SAVED,RQCMC9)                                        
F$CM10   LKMAP F,D#CMC10,CHAR,TA#CMC10,MAXLEN=L'RQCMC10,               +        
               OUTPUT=(D,B#SAVED,RQCMC10)                                       
F$CM11   LKMAP F,D#CMC11,CHAR,TA#CMC11,MAXLEN=L'RQCMC11,               +        
               OUTPUT=(D,B#SAVED,RQCMC11)                                       
F$CM12   LKMAP F,D#CMC12,CHAR,TA#CMC12,MAXLEN=L'RQCMC12,               +        
               OUTPUT=(D,B#SAVED,RQCMC12)                                       
F$CM13   LKMAP F,D#CMC13,CHAR,TA#CMC13,MAXLEN=L'RQCMC13,               +        
               OUTPUT=(D,B#SAVED,RQCMC13)                                       
F$CM14   LKMAP F,D#CMC14,CHAR,TA#CMC14,MAXLEN=L'RQCMC14,               +        
               OUTPUT=(D,B#SAVED,RQCMC14)                                       
F$CM15   LKMAP F,D#CMC15,CHAR,TA#CMC15,MAXLEN=L'RQCMC15,               +        
               OUTPUT=(D,B#SAVED,RQCMC15)                                       
F$CM16   LKMAP F,D#CMC16,CHAR,TA#CMC16,MAXLEN=L'RQCMC16,               +        
               OUTPUT=(D,B#SAVED,RQCMC16)                                       
F$WID    LKMAP F,D#CMWID,CHAR,TA#WAPID,MAXLEN=L'RQCMWID,               +        
               OUTPUT=(D,B#SAVED,RQCMWID)                                       
F$EOV    LKREQ F,D#CMEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,               *        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,D#CMCMC,(I,B#SAVED,I$CLMC),UBIN,LIST=F,               *        
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
                                                                                
SVCID    DS    CL(L'TACOCID)       SAVED COMMERCIAL ID                          
SVCINV   DS    XL(L'TLININV)       SAVED COMPLEMENTED INVOICE NUMBER            
SVTIME   DS    XL3                 SAVED TIME                                   
                                                                                
CMTSTAT  DS    X                   COMMENT STATUS                               
CMRECUPD EQU   X'80'               RECORD WAS UPDATED ON FILE                   
                                                                                
CMCPYSTA DS    XL16                COMMENT ENCOUNTERED STATUS                   
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        COMMENT MAINTENANCE REQUEST MAP FIELDS                       *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQCMMOD  DS    CL1                 MODE                                         
RQCMRTV  EQU   1                   RETRIEVE                                     
RQCMVFY  EQU   2                   VERIFY                                       
RQCMEXE  EQU   3                   EXECUTE                                      
RQCMSTF  DS    CL8                 STAFF CODE                                   
RQCMLEV  DS    CL1                 LEVEL                                        
RQCMLEVC EQU   C'C'                CLIENT                                       
RQCMLEVT EQU   C'T'                TPC                                          
RQCMTYP  DS    CL1                 TYPE                                         
RQCMSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
RQCMGUA  DS    CL4                 GUARANTEE CODE                               
RQCMCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCMVER  DS    XL1                 VERSION NUMBER                               
RQCMAGY  DS    CL6                 AGENCY CODE                                  
RQCMCON  DS    CL12                CONTRACT ID                                  
RQCMTST  DS    XL3                 TERM START DATE                              
RQCMTEN  DS    XL3                 TERM END DATE                                
RQCMINV  DS    CL6                 INVOICE NUMBER                               
RQCMADV  DS    CL6                 ADVICE NUMBER                                
RQCMC1   DS    CL78                COMMENT #1                                   
RQCMC2   DS    CL78                COMMENT #2                                   
RQCMC3   DS    CL78                COMMENT #3                                   
RQCMC4   DS    CL78                COMMENT #4                                   
RQCMC5   DS    CL78                COMMENT #5                                   
RQCMC6   DS    CL78                COMMENT #6                                   
RQCMC7   DS    CL78                COMMENT #7                                   
RQCMC8   DS    CL78                COMMENT #8                                   
RQCMC9   DS    CL78                COMMENT #9                                   
RQCMC10  DS    CL78                COMMENT #10                                  
RQCMC11  DS    CL78                COMMENT #11                                  
RQCMC12  DS    CL78                COMMENT #12                                  
RQCMC13  DS    CL78                COMMENT #13                                  
RQCMC14  DS    CL78                COMMENT #14                                  
RQCMC15  DS    CL78                COMMENT #15                                  
RQCMC16  DS    CL78                COMMENT #16                                  
RQCMWID  DS    CL18                WEB APPLICATION ID                           
RQRCLNQ  EQU   *-RQCMMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK1C   08/28/13'                                      
         END                                                                    
