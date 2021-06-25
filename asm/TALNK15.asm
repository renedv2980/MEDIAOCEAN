*          DATA SET TALNK15    AT LEVEL 004 AS OF 07/29/15                      
*PHASE T70415A                                                                  
TALNK15  TITLE 'W4 MAINTENANCE UPLOAD SERVER'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TAW4,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (20*L'TLDRREC)+1                                                 
UPPTRBLK EQU   (20*L'TLDRREC)+1                                                 
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK                                         
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA15**,RR=RE                                           
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
*        INITIAILZE UPLOAD                                            *         
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
                                                                                
INPUT    BRAS  RE,W4UPLOAD         PROCESS THE INPUT RECORD                     
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
*        PROCESS W4 MAINTENANCE UPLOAD REQUEST                        *         
***********************************************************************         
                                                                                
W4UPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#W4STA)               
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   W4UP20                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   W4UP20                                                           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQW4STF                                   
         JNE   W4UP20                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         BRAS  RE,VALSSN           VALIDATE SOCIAL SECURITY NUMBER              
         BRAS  RE,VALCRP           VALIDATE CORPORATIONS                        
         BRAS  RE,VALAFM           VALIDATE AFM LOCAL                           
         BRAS  RE,VALTRST          VALIDATE TRUSTEE                             
         BRAS  RE,VALFILT          VALIDATE FILTERS                             
                                                                                
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ      READ FOR W4 KEY                              
         MVC   TLW4SSN,RQW4SSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    W4UP10                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         J     W4UP20                                                           
W4UP10   BRAS  RE,INITCHA          IF FOUND, PREPARE TO CHANGE                  
         BRAS  RE,VALTYPE          VALIDATE TYPE                                
         BRAS  RE,VALTYLK          AND VALIDATE TYPE/LOCKED STATUS              
                                                                                
W4UP20   MVI   OUTPUT,W4STER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    W4UP30                                                           
         MVI   OUTPUT,W4STOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    W4UP30                                                           
         MVI   OUTPUT,W4STOK2                                                   
                                                                                
W4UP30   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQW4MOD,RQW4EXE     IF MODE IS EXECUTE                           
         JNE   W4UP40                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    W4UP40                                                           
         BRAS  RE,EXECADD          ADD OR                                       
         BRAS  RE,EXECCHA          CHANGE W4 RECORD                             
         JNE   W4UP40                                                           
         GOTOR (#UPDPTRS,AUPDPTRS) UPDATE PASSIVE POINTERS                      
         BRAS  RE,ADDWTR           ADD WEB TRANSACTION RECORD                   
         BRAS  RE,UPDETNAM         AND POSSIBLY UPDATE EMP/TS NAMES             
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQW4STF),(L'RQW4STF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
W4UP40   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',RQW4SSN),(L'RQW4SSN,0)                               
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#W4ERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESCECTR                                                  
         JO    YES                                                              
         CLI   ACTION,ACTADD                                                    
         JE    YES                                                              
         OI    ERRSTAT,ESREVIW                                                  
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#W4ULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#W4SDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#W4SSTF),        +        
               ('LD_CHARQ',RQW4STF),(L'RQW4STF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#W4SSSN),        +        
               ('LD_CHARQ',RQW4SSN),(L'RQW4SSN,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
W4STOK1  EQU   1                   NO ERRORS - W4 ADDED                         
W4STOK2  EQU   2                   NO ERRORS - W4 CHANGED                       
W4STER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#W4MOD                                                    
         CLI   RQW4MOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         CLI   RQW4MOD,RQW4EXE     IF MODE IS NOT EXECUTE                       
         JE    AR00                                                             
         CLI   RQW4WID,C'V'        AND COMING FROM VITA TV                      
         JE    YES                 NO FURTHER ASSERTIONS NEEDED                 
         CLI   RQW4WID,C'T'        AND COMING FROM VITA 4.0                     
         JE    YES                 NO FURTHER ASSERTIONS NEEDED                 
                                                                                
AR00     MVI   BYTE1,D#W4STF                                                    
         OC    RQW4STF,RQW4STF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#W4SSN                                                    
         OC    RQW4SSN,RQW4SSN     ASSERT THAT SS#/FID IS PROVIDED              
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#W4TYP                                                    
         CLI   RQW4TYP,0           ASSERT THAT TYPE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#W4AD1                                                    
         OC    RQW4AD1,RQW4AD1     ASSERT THAT ADDRESS LINE #1 IS               
         JZ    ARMIS               PROVIDED                                     
                                                                                
         MVI   BYTE1,D#W4WID                                                    
         OC    RQW4WID,RQW4WID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
         CLI   RQW4TYP,TAW4TYCA    IF TYPE IS CANADIAN                          
         JNE   AR05                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS PROVIDED            
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS PROVIDED           
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4CRY                                                    
         OC    RQW4CRY,RQW4CRY     ASSERT THAT COUNTRY IS PROVIDED              
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4SXC                                                    
         CLI   RQW4SXC,0           ASSERT THAT SEX CODE IS PROVIDED             
         JE    ARMIS                                                            
         MVI   BYTE1,D#W4IDT                                                    
         OC    RQW4IDT,RQW4IDT     ASSERT THAT INDEMINIFICATION DATE            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CAR                                                    
         OC    RQW4CAR,RQW4CAR     ASSERT THAT CITY TAX AREA                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CMS                                                    
         CLI   RQW4CMS,0           ASSERT THAT CITY MARRIED/SINGLE              
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4RST                                                    
         OC    RQW4RST,RQW4RST     ASSERT THAT RECIPROCAL STATE                 
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         CLC   RQW4CRY,=C'CA'      IF COUNTRY IS CANADA                         
         JNE   AR05                                                             
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
                                                                                
AR05     CLI   RQW4TYP,TAW4TYCO    IF TYPE IS CORPORATION                       
         JNE   AR10                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4MNM                                                    
         OC    RQW4MNM,RQW4MNM     ASSERT THAT MIDDLE NAME IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4SUF                                                    
         OC    RQW4SUF,RQW4SUF     ASSERT THAT SUFFIX IS NOT PROVIDED           
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#W4AKF                                                    
         OC    RQW4AKF,RQW4AKF     ASSERT THAT AKA FIRST NAME IS                
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#W4CRY                                                    
         OC    RQW4CRY,RQW4CRY     ASSERT THAT COUNTRY IS PROVIDED              
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4CP1       ASSERT THAT ATTACHED CORPORATIONS            
         OC    RQW4CP1(RQW4RLNQ),RQW4CP1                                        
         JNZ   ARNAL               ARE NOT PROVIDED                             
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CAR                                                    
         OC    RQW4CAR,RQW4CAR     ASSERT THAT CITY TAX AREA                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CMS                                                    
         CLI   RQW4CMS,0           ASSERT THAT CITY MARRIED/SINGLE              
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FTX                                                    
         CLI   RQW4FTX,0           ASSERT THAT TAKE TAXES FOR                   
         JNE   ARNAL               FOREIGNERS IS NOT PROVIDED                   
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FNC                                                    
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CANADIAN FEDERAL NET             
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FET                                                    
         CLI   RQW4FET,0           ASSERT THAT CANADIAN FEDERAL EXEMPT          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PNC                                                    
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CANADIAN PROVINCIAL NET          
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           ASSERT THAT CANADIAN PROVINCIAL              
         JNE   ARNAL               EXEMPT IS NOT PROVIDED                       
         MVI   BYTE1,D#W4PPZ                                                    
         OC    RQW4PPZ,RQW4PPZ     ASSERT THAT CAN PRV PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM HC           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR10     CLI   RQW4TYP,TAW4TYES    IF TYPE IS ESTATE                            
         JNE   AR20                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4MNM                                                    
         OC    RQW4MNM,RQW4MNM     ASSERT THAT MIDDLE NAME IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4SUF                                                    
         OC    RQW4SUF,RQW4SUF     ASSERT THAT SUFFIX IS NOT PROVIDED           
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#W4CRY                                                    
         OC    RQW4CRY,RQW4CRY     ASSERT THAT COUNTRY IS PROVIDED              
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4SXC                                                    
         CLI   RQW4SXC,0           ASSERT THAT SEX CODE IS PROVIDED             
         JE    ARMIS                                                            
         MVI   BYTE1,D#W4IDT                                                    
         OC    RQW4IDT,RQW4IDT     ASSERT THAT INDEMINIFICATION DATE            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JE    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JZ    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JE    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4GST                                                    
         OC    RQW4GST,RQW4GST     ASSERT THAT GST NUMBER IS                    
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4FTX                                                    
         CLI   RQW4FTX,0           ASSERT THAT TAKE TAXES FOR                   
         JNE   ARNAL               FOREIGNERS IS NOT PROVIDED                   
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FNC                                                    
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CANADIAN FEDERAL NET             
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FET                                                    
         CLI   RQW4FET,0           ASSERT THAT CANADIAN FEDERAL EXEMPT          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PNC                                                    
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CANADIAN PROVINCIAL NET          
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           ASSERT THAT CANADIAN PROVINCIAL              
         JNE   ARNAL               EXEMPT IS NOT PROVIDED                       
         MVI   BYTE1,D#W4PPZ                                                    
         OC    RQW4PPZ,RQW4PPZ     ASSERT THAT CAN PRV PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM HC           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR20     CLI   RQW4TYP,TAW4TYFO    IF TYPE IS FOREIGNER                         
         JNE   AR30                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS PROVIDED            
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS PROVIDED           
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4CRY                                                    
         OC    RQW4CRY,RQW4CRY     ASSERT THAT COUNTRY IS PROVIDED              
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4SXC                                                    
         CLI   RQW4SXC,0           ASSERT THAT SEX CODE IS PROVIDED             
         JE    ARMIS                                                            
         MVI   BYTE1,D#W4IDT                                                    
         OC    RQW4IDT,RQW4IDT     ASSERT THAT INDEMINIFICATION DATE            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CAR                                                    
         OC    RQW4CAR,RQW4CAR     ASSERT THAT CITY TAX AREA                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CMS                                                    
         CLI   RQW4CMS,0           ASSERT THAT CITY MARRIED/SINGLE              
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4RST                                                    
         OC    RQW4RST,RQW4RST     ASSERT THAT RECIPROCAL STATE                 
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4GST                                                    
         OC    RQW4GST,RQW4GST     ASSERT THAT GST NUMBER IS                    
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FNC                                                    
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CANADIAN FEDERAL NET             
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FET                                                    
         CLI   RQW4FET,0           ASSERT THAT CANADIAN FEDERAL EXEMPT          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PNC                                                    
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CANADIAN PROVINCIAL NET          
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           ASSERT THAT CANADIAN PROVINCIAL              
         JNE   ARNAL               EXEMPT IS NOT PROVIDED                       
         MVI   BYTE1,D#W4PPZ                                                    
         OC    RQW4PPZ,RQW4PPZ     ASSERT THAT CAN PRV PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM HC           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR30     CLI   RQW4TYP,TAW4TYIN    IF TYPE IS INDIVIDUAL                        
         JNE   AR40                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS PROVIDED            
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS PROVIDED           
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4CRY                                                    
         OC    RQW4CRY,RQW4CRY     ASSERT THAT COUNTRY IS PROVIDED              
         JZ    ARMIS                                                            
         MVI   BYTE1,D#W4SXC                                                    
         CLI   RQW4SXC,0           ASSERT THAT SEX CODE IS PROVIDED             
         JE    ARMIS                                                            
         MVI   BYTE1,D#W4IDT                                                    
         OC    RQW4IDT,RQW4IDT     ASSERT THAT INDEMINIFICATION DATE            
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JE    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JZ    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JE    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4GST                                                    
         OC    RQW4GST,RQW4GST     ASSERT THAT GST NUMBER IS                    
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4FTX                                                    
         CLI   RQW4FTX,0           ASSERT THAT TAKE TAXES FOR                   
         JNE   ARNAL               FOREIGNERS IS NOT PROVIDED                   
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FNC                                                    
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CANADIAN FEDERAL NET             
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FET                                                    
         CLI   RQW4FET,0           ASSERT THAT CANADIAN FEDERAL EXEMPT          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PNC                                                    
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CANADIAN PROVINCIAL NET          
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           ASSERT THAT CANADIAN PROVINCIAL              
         JNE   ARNAL               EXEMPT IS NOT PROVIDED                       
         MVI   BYTE1,D#W4PPZ                                                    
         OC    RQW4PPZ,RQW4PPZ     ASSERT THAT CAN PRV PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM HC           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR40     CLI   RQW4TYP,TAW4TYTR    IF TYPE IS TRUSTEE                           
         JNE   AR50                                                             
         MVI   BYTE1,D#W4LNM                                                    
         OC    RQW4LNM,RQW4LNM     ASSERT THAT LAST NAME IS NOT                 
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4FNM                                                    
         OC    RQW4FNM,RQW4FNM     ASSERT THAT FIRST NAME IS NOT                
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4MNM                                                    
         OC    RQW4MNM,RQW4MNM     ASSERT THAT MIDDLE NAME IS NOT               
         JNZ   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4SUF                                                    
         OC    RQW4SUF,RQW4SUF     ASSERT THAT SUFFIX IS NOT PROVIDED           
         JNZ   ARNAL                                                            
         MVI   BYTE1,D#W4AKF                                                    
         OC    RQW4AKF,RQW4AKF     ASSERT THAT AKA FIRST NAME IS                
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4CPN                                                    
         OC    RQW4CPN,RQW4CPN     ASSERT THAT CORPORATION NAME IS              
         JZ    ARMIS               PROVIDED                                     
         MVI   BYTE1,D#W4ETH                                                    
         CLI   RQW4ETH,0           ASSERT THAT ETHNICITY IS NOT                 
         JNE   ARNAL               PROVIDED                                     
         MVI   BYTE1,D#W4SXC                                                    
         CLI   RQW4SXC,0           ASSERT THAT SEX CODE IS NOT PROVIDED         
         JNE   ARNAL                                                            
         MVI   BYTE1,D#W4CP1       ASSERT THAT ATTACHED CORPORATIONS            
         OC    RQW4CP1(RQW4RLNQ),RQW4CP1                                        
         JNZ   ARNAL               ARE NOT PROVIDED                             
         MVI   BYTE1,D#W4FMS                                                    
         CLI   RQW4FMS,0           ASSERT THAT FEDERAL MARRIED/SINGLE           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SAR                                                    
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE TAX AREA                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           ASSERT THAT STATE MARRIED/SINGLE             
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CAR                                                    
         OC    RQW4CAR,RQW4CAR     ASSERT THAT CITY TAX AREA                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CMS                                                    
         CLI   RQW4CMS,0           ASSERT THAT CITY MARRIED/SINGLE              
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4GST                                                    
         OC    RQW4GST,RQW4GST     ASSERT THAT GST NUMBER IS                    
         JNZ   ARNAL               NOT PROVIDED                                 
         MVI   BYTE1,D#W4FTX                                                    
         CLI   RQW4FTX,0           ASSERT THAT TAKE TAXES FOR                   
         JNE   ARNAL               FOREIGNERS IS NOT PROVIDED                   
         MVI   BYTE1,D#W4NHA                                                    
         CLI   RQW4NHA,0           ASSERT THAT ELIGIBLE FOR NEW HIRE            
         JNE   ARNAL               ACT IS NOT PROVIDED                          
         MVI   BYTE1,D#W4NHP                                                    
         CLI   RQW4NHP,0           ASSERT THAT NEW HIRE ACT ELIGIBILITY         
         JNE   ARNAL               PENDING IS NOT PROVIDED                      
         MVI   BYTE1,D#W4NHD                                                    
         OC    RQW4NHD,RQW4NHD     ASSERT THAT NEW HIRE DATE                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4TCP                                                    
         OC    RQW4TCP,RQW4TCP     ASSERT THAT TAXABLE CAN PROVINCE             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FNC                                                    
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CANADIAN FEDERAL NET             
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FET                                                    
         CLI   RQW4FET,0           ASSERT THAT CANADIAN FEDERAL EXEMPT          
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PNC                                                    
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CANADIAN PROVINCIAL NET          
         JNZ   ARNAL               CLAIM AMOUNT IS NOT PROVIDED                 
         MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO CC1           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           ASSERT THAT CANADIAN PROVINCIAL              
         JNE   ARNAL               EXEMPT IS NOT PROVIDED                       
         MVI   BYTE1,D#W4PPZ                                                    
         OC    RQW4PPZ,RQW4PPZ     ASSERT THAT CAN PRV PRESCRIBED ZONE          
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM HC           
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR50     OC    RQW4AKF,RQW4AKF     IF AKA FIRST NAME IS NOT PROVIDED            
         JNZ   AR60                                                             
         OC    RQW4AKL,RQW4AKL     ASSERT THAT AKA LAST NAME IS                 
         JZ    AR60                NOT PROVIDED                                 
         MVI   BYTE1,D#W4AKL                                                    
         J     ARNAL                                                            
                                                                                
AR60     OC    RQW4CRY,RQW4CRY     IF COUNTRY IS NOT PROVIDED                   
         JNZ   AR70                                                             
         MVI   BYTE1,D#W4STA                                                    
         OC    RQW4STA,RQW4STA     ASSERT THAT STATE IS NOT PROVIDED            
         JNZ   ARNAL                                                            
         OC    RQW4ZIP,RQW4ZIP     ASSERT THAT ZIP IS NOT PROVIDED              
         JZ    AR100                                                            
         MVI   BYTE1,D#W4ZIP                                                    
         J     ARNAL                                                            
                                                                                
AR70     MVI   BYTE1,D#W4CTY                                                    
         OC    RQW4CTY,RQW4CTY     IF COUNTRY IS PROVIDED                       
         JZ    ARMIS               ASSERT THAT CITY IS PROVIDED                 
         MVI   BYTE1,D#W4ZIP                                                    
         OC    RQW4ZIP,RQW4ZIP     ASSERT THAT ZIP IS PROVIDED                  
         JZ    ARMIS                                                            
                                                                                
         CLC   RQW4CRY,=C'US'      IF COUNTRY IS US                             
         JNE   AR80                                                             
         OC    RQW4STA,RQW4STA     ASSERT THAT STATE IS PROVIDED                
         JNZ   AR100                                                            
         MVI   BYTE1,D#W4STA                                                    
         J     ARMIS                                                            
                                                                                
AR80     MVI   BYTE1,D#W4AD3       IF COUNTRY IS NOT US                         
         OC    RQW4AD3,RQW4AD3     ASSERT THAT ADDRESS LINE 3 IS                
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
         CLC   RQW4CRY,=C'CA'      IF COUNTRY IS CANADA                         
         JNE   AR90                                                             
         OC    RQW4STA,RQW4STA     ASSERT THAT PROVINCE IS PROVIDED             
         JNZ   AR100                                                            
         MVI   BYTE1,D#W4STA                                                    
         J     ARMIS                                                            
                                                                                
AR90     OC    RQW4STA,RQW4STA     IF COUNTRY IS NOT US OR CANADA               
         JZ    AR100               ASSERT THAT PROVINCE IS NOT                  
         MVI   BYTE1,D#W4STA       PROVIDED                                     
         J     ARNAL                                                            
                                                                                
AR100    CLI   RQW4FMS,0           IF FEDERAL MARRIED/SINGLE IS                 
         JNE   AR110               NOT PROVIDED                                 
         MVI   BYTE1,D#W4FEX                                                    
         CLI   RQW4FEX,0           ASSERT THAT FEDERAL EXEMPTIONS               
         JNE   ARNAL               ARE NOT PROVIDED                             
         CLI   RQW4FFX,0           AND ASSERT THAT FEDERAL FIXED                
         JE    AR110               PERCENTAGE IS NOT PROVIDED                   
         MVI   BYTE1,D#W4FFX                                                    
         J     ARNAL                                                            
                                                                                
AR110    MVI   BYTE1,D#W4SMS                                                    
         CLI   RQW4SMS,0           IF STATE MARRIED/SINGLE IS                   
         JE    AR120               PROVIDED                                     
         OC    RQW4SAR,RQW4SAR     ASSERT THAT STATE AREA IS PROVIDED           
         JNZ   AR130                                                            
         J     ARNAL                                                            
AR120    OC    RQW4SAR,RQW4SAR     ELSE ASSERT THAT STATE AREA                  
         JNZ   ARMIS               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4SEX                                                    
         CLI   RQW4SEX,0           AND ASSERT THAT STATE EXEMPTIONS             
         JNE   ARNAL               ARE NOT PROVIDED                             
         OC    RQW4SFX,RQW4SFX     AND ASSERT THAT STATE FIXED                  
         JZ    AR130               PERCENTAGE IS NOT PROVIDED                   
         MVI   BYTE1,D#W4SFX                                                    
         J     ARNAL                                                            
                                                                                
AR130    MVI   BYTE1,D#W4CMS                                                    
         CLI   RQW4CMS,0           IF CITY MARRIED/SINGLE IS                    
         JE    AR140               PROVIDED                                     
         OC    RQW4CAR,RQW4CAR     ASSERT THAT CITY AREA IS PROVIDED            
         JNZ   AR150                                                            
         J     ARNAL                                                            
AR140    OC    RQW4CAR,RQW4CAR     ELSE ASSERT THAT CITY AREA                   
         JNZ   ARMIS               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CEX                                                    
         CLI   RQW4CEX,0           AND ASSERT THAT CITY EXEMPTIONS              
         JNE   ARNAL               ARE NOT PROVIDED                             
         OC    RQW4CFX,RQW4CFX     AND ASSERT THAT CITY FIXED                   
         JZ    AR150               PERCENTAGE IS NOT PROVIDED                   
         MVI   BYTE1,D#W4CFX                                                    
         J     ARNAL                                                            
                                                                                
AR150    OC    RQW4PA1,RQW4PA1     IF PAYEE ADDRESS IS PROVIDED                 
         JZ    AR160                                                            
         MVI   BYTE1,D#W4PCY                                                    
         OC    RQW4PCY,RQW4PCY     ASSERT THAT PAYEE CITY                       
         JZ    ARMIS               IS PROVIDED                                  
         MVI   BYTE1,D#W4PZP                                                    
         OC    RQW4PZP,RQW4PZP     ASSERT THAT PAYEE ZIP CODE                   
         JZ    ARMIS               IS PROVIDED                                  
         OC    RQW4PCT,RQW4PCT                                                  
         JNZ   AR170               ASSERT THAT PAYEE COUNTRY                    
         MVI   BYTE1,D#W4PCT       IS PROVIDED                                  
         J     ARMIS                                                            
AR160    MVI   BYTE1,D#W4PA2                                                    
         OC    RQW4PA2,RQW4PA2     ELSE ASSERT THAT PAYEE ADDRESS 2             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PA3                                                    
         OC    RQW4PA3,RQW4PA3     AND ASSERT THAT PAYEE ADDRESS 3              
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PCY                                                    
         OC    RQW4PCY,RQW4PCY     AND ASSERT THAT PAYEE CITY                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PZP                                                    
         OC    RQW4PZP,RQW4PZP     AND ASSERT THAT PAYEE ZIP CODE               
         JNZ   ARNAL               IS NOT PROVIDED                              
         OC    RQW4PCT,RQW4PCT     AND ASSERT THAT PAYEE COUNTRY                
         JZ    AR170               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PCT                                                    
         J     ARNAL                                                            
                                                                                
AR170    CLC   RQW4PCT,=C'US'      IF PAYEE COUNTRY IS US                       
         JNE   AR180                                                            
         OC    RQW4PST,RQW4PST     ASSERT THAT PAYEE STATE                      
         JNZ   AR210               IS PROVIDED                                  
         MVI   BYTE1,D#W4PST                                                    
         J     ARMIS                                                            
AR180    OC    RQW4PA3,RQW4PA3     ELSE ASSERT THAT PAYEE ADDRESS 3             
         JZ    AR190               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PA3                                                    
         J     ARNAL                                                            
                                                                                
AR190    CLC   RQW4PCT,=C'CA'      IF PAYEE COUNTRY IS CANADA                   
         JNE   AR200                                                            
         OC    RQW4PST,RQW4PST     ASSERT THAT PAYEE PROVINCE                   
         JNZ   AR210               IS PROVIDED                                  
         MVI   BYTE1,D#W4PST                                                    
         J     ARMIS                                                            
AR200    OC    RQW4PST,RQW4PST     ELSE ASSERT THAT PAYEE STATE                 
         JZ    AR210               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4PST                                                    
         J     ARNAL                                                            
                                                                                
AR210    OC    RQW4CP1(54),RQW4CP1 IF ATTACHED CORPORATIONS ARE                 
         JZ    AR220               PROVIDED                                     
         OC    RQW4CRP,RQW4CRP     ASSERT THAT ATTACHED CORPORATION             
         JZ    AR220               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4CRP                                                    
         J     ARNAL                                                            
                                                                                
AR220    MVI   BYTE1,D#W4FNC                                                    
         CLI   RQW4FD1,C'N'        IF CAN FED DEFAULT TO CLAIM CODE             
         JNE   AR230               1 IS N                                       
         CLI   RQW4FET,C'N'        AND CAN FED EXEMPT IS N                      
         JNE   AR230                                                            
         OC    RQW4FNC,RQW4FNC     ASSERT THAT CAN FED NET CLAIM                
         JNZ   AR240               AMOUNT IS PROVIDED                           
         J     ARMIS                                                            
AR230    OC    RQW4FNC,RQW4FNC     ELSE, ASSET THAT CAN FED NET CLAIM           
         JNZ   ARNAL               AMOUNT IS NOT PROVIDED                       
                                                                                
AR240    MVI   BYTE1,D#W4FD1                                                    
         CLI   RQW4FET,0           IF CAN FED EXEMPT IS PROVIDED                
         JE    AR250                                                            
         CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO               
         JE    ARMIS               CLAIM CODE 1 IS PROVIDED                     
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           AND ASSERT THAT CAN PRV EXEMPT               
         JZ    ARMIS               IS PROVIDED                                  
         J     AR260                                                            
*                                  IF CAN FED EXEMPT IS NOT PROVIDED            
AR250    CLI   RQW4FD1,0           ASSERT THAT CAN FED DEFAULT TO               
         JNE   ARNAL               CLAIM CODE 1 IS NOT PROVIDED                 
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED               
         JNZ   ARNAL               ZONE IS NOT PROVIDED                         
         MVI   BYTE1,D#W4PET                                                    
         CLI   RQW4PET,0           AND ASSERT THAT CAN PRV EXEMPT               
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR260    CLI   RQW4FET,C'Y'        IF CAN FED EXEMPT IS Y                       
         JNE   AR270                                                            
         MVI   BYTE1,D#W4FPZ                                                    
         OC    RQW4FPZ,RQW4FPZ     ASSERT THAT CAN FED PRESCRIBED               
         JNZ   ARNAL               ZONE IS NOT PROVIDED                         
         MVI   BYTE1,D#W4FEC                                                    
         CLI   RQW4FEC,0           ASSERT THAT CAN FED EXEMPT FROM CPP          
         JNE   ARNAL               IS NOT PROVIDED                              
                                                                                
AR270    MVI   BYTE1,D#W4PNC                                                    
         CLI   RQW4PD1,C'N'        IF CAN PRV DEFAULT TO CLAIM CODE             
         JNE   AR280               1 IS N                                       
         CLI   RQW4PET,C'N'        AND CAN PRV EXEMPT IS N                      
         JNE   AR280                                                            
         OC    RQW4PNC,RQW4PNC     ASSERT THAT CAN PRV NET CLAIM                
         JNZ   AR290               AMOUNT IS PROVIDED                           
         J     ARMIS                                                            
AR280    OC    RQW4PNC,RQW4PNC     ELSE, ASSET THAT CAN PRV NET CLAIM           
         JNZ   ARNAL               AMOUNT IS NOT PROVIDED                       
                                                                                
AR290    MVI   BYTE1,D#W4PD1                                                    
         CLI   RQW4PET,0           IF CAN PRV EXEMPT IS PROVIDED                
         JE    AR300                                                            
         CLI   RQW4PD1,0           ASSERT THAT CAN PRV DEFAULT TO               
         JE    ARMIS               CLAIM CODE 1 IS PROVIDED                     
         CLC   RQW4STA,=C'QC'                                                   
         JE    AR310                                                            
         J     YES                                                              
                                                                                
AR300    CLI   RQW4PD1,0           IF CAN PRV EXEMPT IS NOT PROVIDED            
         JNE   ARNAL               ASSERT THAT CAN PRV DEFAULT TO               
         MVI   BYTE1,D#W4PPZ       CLAIM CODE 1 IS NOT PROVIDED                 
         OC    RQW4PPZ,RQW4PPZ     AND ASSERT THAT CAN PRV PRESCRIBED           
         JNZ   ARNAL               ZONE IS NOT PROVIDED                         
                                                                                
AR310    MVI   BYTE1,D#W4PHD                                                    
         OC    RQW4PHD,RQW4PHD     ASSERT THAT CAN PRV HOUSING                  
         JNZ   ARNAL               DEDUCTION IS NOT PROVIDED                    
         MVI   BYTE1,D#W4PSP                                                    
         OC    RQW4PSP,RQW4PSP     ASSERT THAT CAN PRV SUPPORT                  
         JNZ   ARNAL               PAYMENT IS NOT PROVIDED                      
         MVI   BYTE1,D#W4PHC                                                    
         CLI   RQW4PHC,0           ASSERT THAT CAN PRV EXEMPT FROM              
         JNE   ARNAL               HEALTH CONT IS NOT PROVIDED                  
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
         MVI   BYTE1,D#W4MOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQW4MOD)                        
         JNE   AVINV                                                            
                                                                                
         CLI   RQW4MOD,RQW4EXE     IF MODE IS NOT EXECUTE                       
         JE    AV00                                                             
         CLI   RQW4WID,C'V'        AND COMING FROM VITA TV                      
         JE    YES                 NO FURTHER ASSERTIONS NEEDED                 
         CLI   RQW4WID,C'T'        AND COMING FROM VITA 4.0                     
         JE    YES                 NO FURTHER ASSERTIONS NEEDED                 
                                                                                
AV00     MVI   BYTE1,D#W4SSN       VALIDATE SOCIAL SECURITY NUMBER              
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',RQW4SSN),(L'RQW4SSN,0)           
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVLNM            VALIDATE LAST NAME                           
         JNE   AVINV                                                            
         BAS   RE,AVFNM            VALIDATE FIRST NAME                          
         JNE   AVINV                                                            
         BAS   RE,AVMNM            VALIDATE MIDDLE NAME                         
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4TYP       VALIDATE W4 TYPE                             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFW4TY',RQW4TYP)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4CRY       VALIDATE COUNTRY                             
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCTRY',RQW4CRY)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4STA       VALIDATE STATE                               
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSOPC',RQW4STA)                        
         JNE   AVINV                                                            
         MVC   ASTAENT,AENTRY                                                   
                                                                                
         MVI   BYTE1,D#W4ZIP       VALIDATE ZIP CODE                            
         GOTO1 AVZIP,DMCB,RQW4CRY,RQW4ZIP                                       
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVSUF            VALIDATE SUFFIX                              
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVSEX            VALIDATE SEX CODE                            
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVETH            VALIDATE ETHNICITY CODE                      
         JNE   AVINV                                                            
                                                                                
         BAS   RE,AVMNU            VALIDATE MEMBERSHIP NUMBER                   
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4YTD       VALIDATE YTD ON CHECKS?                      
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4YTD)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4CKF       VALIDATE SORT CHECKS FIRST?                  
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4CKF)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4ILF       VALIDATE IRS LOCK ON FEDERAL WITHS?          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4ILF)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4LCK       VALIDATE W4 LOCKED?                          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4LCK)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4FIW       VALIDATE FICA WITHHOLDING?                   
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4FIW)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4DCW       VALIDATE DUE COMPANY WITHHOLDING?            
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4DCW)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PEN       VALIDATE PENSION?                            
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4PEN)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4DIR       VALIDATE DIRECT DEPOSIT?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4DIR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4WIR       VALIDATE WIRE TRANSFER FOR CHECKS?           
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4WIR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4SPL       VALIDATE SPECIAL LETTER ON FILE?             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4SPL)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4FMS                                                    
         GOTO1 AVMS,DMCB,RQW4FMS   VALIDATE FEDERAL MARITAL STATUS              
         JNE   AVINV               AND FIXED PERCENTAGE                         
         MVI   BYTE1,D#W4FFX                                                    
         GOTO1 AV9999,DMCB,RQW4FFX                                              
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4SAR       VALIDATE STATE TAX - AREA                    
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSTATE',RQW4SAR)                       
         JE    AV10                                                             
         CLC   =C'OT ',RQW4SAR                                                  
         JNE   AVINV                                                            
         CLI   RQW4TYP,TAW4TYES                                                 
         JNE   AVINV                                                            
AV10     MVI   BYTE1,D#W4SMS                                                    
         GOTO1 AVMS,DMCB,RQW4SMS   VALIDATE STATE MARITAL STATUS                
         JNE   AVINV               AND FIXED PERCENTAGE                         
         MVI   BYTE1,D#W4SFX                                                    
         GOTO1 AV9999,DMCB,RQW4SFX                                              
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4CAR       VALIDATE CITY TAX - AREA                     
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCITY',RQW4CAR)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4CMS                                                    
         GOTO1 AVMS,DMCB,RQW4CMS   VALIDATE CITY MARITAL STATUS                 
         JNE   AVINV               AND FIXED PERCENTAGE                         
         MVI   BYTE1,D#W4CFX                                                    
         GOTO1 AV9999,DMCB,RQW4CFX                                              
         JNE   AVINV                                                            
                                                                                
         OC    RQW4RST,RQW4RST     VALIDATE RECIPROCAL STATE                    
         JZ    AV30                                                             
         MVI   BYTE1,D#W4RST                                                    
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSTATE',RQW4RST)                       
         JNE   AVINV                                                            
         CLC   RQW4RST,RQW4SAR                                                  
         JE    AVINV                                                            
                                                                                
AV30     MVI   BYTE1,D#W4DED       VALIDATE DEDUCTION PERCENTAGE                
         GOTO1 AV9999,DMCB,RQW4DED                                              
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4MPR       VALIDATE MPR FUND PERCENTAGE                 
         GOTO1 AV9999,DMCB,RQW4MPR                                              
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4PCH       VALIDATE PERMANENT CHARITY PCT               
         GOTO1 AV9999,DMCB,RQW4PCH                                              
         JNE   AVINV                                                            
                                                                                
         OC    RQW4PEX,RQW4PEX     PAYEE EXPIRATION DATE MUST BE                
         JZ    AV40                EQUAL TO OR LATER THAN PAYEE                 
         MVI   BYTE1,D#W4PEX       ACTIVE DATE                                  
         CLC   RQW4PAC,RQW4PEX                                                  
         JH    AVINV                                                            
                                                                                
AV40     MVI   BYTE1,D#W4FTX       VALIDATE TAX FOREIGNER?                      
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4FTX)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4PCT       VALIDATE PAYEE COUNTRY                       
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCTRY',RQW4PCT)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4PST       VALIDATE PAYEE STATE                         
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSOPC',RQW4PST)                        
         JNE   AVINV                                                            
         MVC   ASTAENT,AENTRY                                                   
                                                                                
         MVI   BYTE1,D#W4PZP       VALIDATE PAYEE ZIP                           
         GOTO1 AVZIP,DMCB,RQW4PCT,RQW4PZP                                       
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4NHA       VALIDATE ELIGIBLE FOR NEW HIRE ACT?          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4NHA)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4NHP       VALIDATE NEW HIRE ACT ELIG PENDING?          
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4NHP)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4EFT       VALIDATE EFT?                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4EFT)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4REG       VALIDATE REGRESSION TESTING?                 
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4REG)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4TCP       VALIDATE TAXABLE CANADIAN PROVINCE           
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFPROV',RQW4TCP)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4FNC       VALIDATE CAN FED NET CLAIM AMOUNT            
         GOTO1 AV9X7,DMCB,RQW4FNC                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4FD1       VALIDATE CAN FED DEFAULT TO CC1?             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4FD1)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4FET       VALIDATE CAN FED EXEMPT?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4FET)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4FPZ       VALIDATE CAN FED PRESCRIBED ZONE             
         GOTO1 AV9X7,DMCB,RQW4FPZ                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4FEC       VALIDATE FED EXEMPT FROM CPP?                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4FEC)                        
         JNE   AVINV                                                            
                                                                                
         MVI   BYTE1,D#W4PNC       VALIDATE PRV FED NET CLAIM AMOUNT            
         GOTO1 AV9X7,DMCB,RQW4PNC                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PD1       VALIDATE PRV FED DEFAULT TO CC1?             
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4PD1)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PET       VALIDATE PRV FED EXEMPT?                     
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4PET)                        
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PPZ       VALIDATE PRV FED PRESCRIBED ZONE             
         GOTO1 AV9X7,DMCB,RQW4PPZ                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PHD       VALIDATE PRV PRV HOUSING DEDUCTION           
         GOTO1 AV9X7,DMCB,RQW4PHD                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PSP       VALIDATE PRV PRV SUPPORT PAYMENT             
         GOTO1 AV9X7,DMCB,RQW4PSP                                               
         JNE   AVINV                                                            
         MVI   BYTE1,D#W4PHC       VALIDATE PRV EXEMPT HEALT CONT?              
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4PHC)                        
         JNE   AVINV                                                            
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFWID',RQW4WID)                         
         JE    YES                                                              
         MVI   BYTE1,D#W4WID       VALIDATE WEB APPLICATION ID                  
         J     AVINV                                                            
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED LAST NAME IS VALID                  *         
***********************************************************************         
                                                                                
AVLNM    NTR1                                                                   
         OC    RQW4LNM,RQW4LNM     IF LAST NAME IS PROVIDED                     
         JZ    YES                                                              
                                                                                
         MVI   BYTE1,D#W4LNM       CAN ONLY CONTAIN CERTAIN CHARACTERS          
         GOTO1 AVCHAR,DMCB,(L'RQW4LNM,RQW4LNM)                                  
         JNE   NO                                                               
                                                                                
         GOTO1 AV1WORD,DMCB,(L'RQW4LNM,RQW4LNM)                                 
         JE    YES                                                              
         LA    RF,VPREFIX                                                       
AVLN10   CLI   0(RF),X'FF'         IF LAST NAME CONTAINS MORE THAN 1            
         JE    NO                  WORD, VALIDATE THAT THE 1ST WORD             
         ZIC   R1,0(RF)            IS A VALID PREFIX                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   1(0,RF),RQW4LNM                                                  
         JE    AVLN20                                                           
         AHI   RF,L'VPREFIX                                                     
         J     AVLN10                                                           
                                                                                
AVLN20   ZIC   R1,0(RF)            BUMP R2 TO THE BEGINNING OF THE              
         AHI   R1,1                2ND WORD                                     
         LA    R2,RQW4LNM                                                       
         AR    R2,R1                                                            
                                                                                
         LHI   RE,L'RQW4LNM        SAVE REMAINING NUMBER OF CHARACTERS          
         SR    RE,R1               IN BYTE2 AND VERIFY THERE IS ONLY            
         STC   RE,BYTE2            ONE WORD WORD                                
         GOTO1 AV1WORD,DMCB,(BYTE2,0(R2))                                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED FIRST NAME IS VALID                 *         
***********************************************************************         
                                                                                
AVFNM    NTR1                                                                   
         OC    RQW4FNM,RQW4FNM     IF FIRST NAME IS PROVIDED                    
         JZ    YES                                                              
                                                                                
         MVI   BYTE1,D#W4FNM       CAN ONLY CONTAIN CERTAIN CHARACTERS          
         GOTO1 AVCHAR,DMCB,(L'RQW4FNM,RQW4FNM)                                  
         JNE   NO                                                               
                                                                                
         GOTO1 AV1WORD,DMCB,(L'RQW4FNM,RQW4FNM)                                 
         JE    YES                 AND CAN ONLY BE 1 WORD LONG                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED FIRST NAME IS VALID                 *         
***********************************************************************         
                                                                                
AVMNM    NTR1                                                                   
         OC    RQW4MNM,RQW4MNM     IF MIDDLE NAME IS PROVIDED                   
         JZ    YES                                                              
                                                                                
         MVI   BYTE1,D#W4MNM       CAN ONLY CONTAIN CERTAIN CHARACTERS          
         GOTO1 AVCHAR,DMCB,(L'RQW4MNM,RQW4MNM)                                  
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED FIELD CONTAINS ONLY THE ALLOWABLE   *         
*        CHARACTERS FOR A NAME                                        *         
*        ON ENTRY ... P1 BYTE 0 = L'FIELD TO CHECK                    *         
*                     P1        = A(FIELD TO CHECK)                   *         
***********************************************************************         
                                                                                
AVCHAR   NTR1                                                                   
         ZICM  RF,1(R1),3                                                       
         ZIC   R2,0(R1)                                                         
                                                                                
AVC10    LA    RE,VNAMCHR                                                       
AVC20    CLI   0(RE),X'FF'                                                      
         JE    NO                                                               
         CLC   0(1,RF),0(RE)                                                    
         JE    AVC30                                                            
         AHI   RE,1                                                             
         J     AVC20                                                            
                                                                                
AVC30    AHI   RF,1                                                             
         BCT   R2,AVC10                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED FIELD CONTAINS ONLY ONE WORD   *         
*        ON ENTRY ... P1 BYTE 0 = L'FIELD TO CHECK                    *         
*                     P1        = A(FIELD TO CHECK)                   *         
***********************************************************************         
                                                                                
AV1WORD  NTR1                                                                   
         ZICM  RF,1(R1),3                                                       
         CLI   0(RF),C' '                                                       
         JE    NO                                                               
                                                                                
         ZIC   R2,0(R1)                                                         
                                                                                
AV1W10   CLI   0(RF),C' '                                                       
         JE    AV1W20                                                           
         AHI   RF,1                                                             
         BCT   R2,AV1W10                                                        
         J     YES                                                              
                                                                                
AV1W20   BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         J     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED ZIP CODE IS VALID                   *         
*        ON ENTRY ... P1 = A(COUNTRY FIELD)                           *         
*                     P2 = A(ZIP CODE FIELD)                          *         
*                     ASTAENT = A(STATE ENTRY IN TASYSUNITS           *         
*                               OR A(PROVINCE ENTRY IN TASYSCTRY)     *         
***********************************************************************         
                                                                                
AVZIP    NTR1                                                                   
         ZICM  RE,1(R1),3          RE=A(COUNTRY FIELD)                          
         ZICM  R2,5(R1),3          R2=A(ZIP CODE FIELD)                         
                                                                                
         CLC   =C'US',0(RE)        IF COUNTRY IS US                             
         JNE   AVZ10                                                            
         BAS   RE,AVUSZIP          VALIDATE ZIP CODE FOR STATE                  
         J     XIT                                                              
                                                                                
AVZ10    CLC   =C'CA',0(RE)        IF COUNTRY IS CANADA                         
         JNE   YES                                                              
         BAS   RE,AVCAZIP          VALIDATE ZIP CODE FOR CANADA                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED ZIP CODE IS VALID FOR US            *         
*        ON ENTRY ... R2      = A(ZIP CODE FIELD)                     *         
*                     ASTAENT = A(STATE ENTRY IN TASYSUNITS)          *         
***********************************************************************         
                                                                                
AVUSZIP  NTR1                                                                   
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',0(R2)),(5,0)                     
         JNE   AVINV               FIRST 5 CHARACTERS MUST BE NUMERIC           
                                                                                
         CLI   5(R2),C' '          IF THE 6TH CHARACTER IS A SPACE              
         JNE   AVUZ10              ASSERT THAT ALL REMINING CHARACTERS          
         CLC   5(5,R2),SPACES      ARE SPACES AS WELL                           
         JE    AVUZ20                                                           
         J     NO                                                               
                                                                                
AVUZ10   CLI   5(R2),C'-'          OTHERWISE 6TH CHARACTER MUST BE -            
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',6(R2)),(4,0)                     
         JNE   AVINV               AND CHARACTERS 7-10 MUST BE NUMERIC          
                                                                                
         USING TALUNITD,R1                                                      
AVUZ20   L     R1,ASTAENT          ENSURE FIRST 3 CHARACTERS ARE                
         CLC   0(3,R2),TALUZIPF    VALID FOR STATE                              
         JL    NO                                                               
         CLC   0(3,R2),TALUZIPT                                                 
         JNH   AVUZ30                                                           
                                                                                
         CLC   TALUCODE,=C'GA '    IF GEORGIA                                   
         JNE   NO                                                               
         CLC   0(3,R2),=C'398'     398 - 399 ALSO ALLOWED                       
         JL    NO                                                               
         CLC   0(3,R2),=C'399'                                                  
         JH    NO                                                               
                                                                                
AVUZ30   CLC   TALUCODE,=C'PR '    IF PUERTO RICO                               
         JNE   AVUZ40                                                           
         CLC   0(3,R2),=C'008'     CAN'T START WITH 008                         
         JNE   YES                                                              
         J     NO                                                               
                                                                                
AVUZ40   CLC   TALUCODE,=C'VI '    IF VIRGIN ISLANDS                            
         JNE   YES                                                              
         CLC   0(5,R2),=C'00851'   ONLY 00801-00851 IS VALID                    
         JNH   YES                                                              
         J     NO                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED ZIP CODE IS VALID FOR CANADA        *         
*        ON ENTRY ... R2      = A(ZIP CODE FIELD)                     *         
*                     ASTAENT = A(PROVINCE ENTRY IN TASYSCTRY)        *         
***********************************************************************         
                                                                                
AVCAZIP  NTR1                                                                   
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',1(R2)),(1,0)                     
         JNE   NO                                                               
         GOTO1 VALCAALP,DMCB,2(R2)                                              
         JNE   NO                                                               
         CLI   3(R2),C' '                                                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',4(R2)),(1,0)                     
         JNE   NO                                                               
         GOTO1 VALCAALP,DMCB,5(R2)                                              
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFNUM',6(R2)),(1,0)                     
         JNE   NO                                                               
         CLC   7(3,R2),SPACES                                                   
         JNE   NO                                                               
                                                                                
         USING CTRYSUBD,R1                                                      
         L     R1,ASTAENT          ENSURE FIRST CHARACTER IS VALID              
         LA    R1,CTRYSZIP         FOR PROVINCE                                 
         LHI   R0,L'CTRYSZIP                                                    
AVCZ10   CLC   0(1,R2),0(R1)                                                    
         JE    YES                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,AVCZ10                                                        
         J     NO                                                               
         DROP  R1                                                               
                                                                                
       ++INCLUDE TAVCAALP                                                       
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED SUFFIX IS VALID                     *         
***********************************************************************         
                                                                                
AVSUF    NTR1                                                                   
         OC    RQW4SUF,RQW4SUF     IF SUFFIX IS PROVIDED                        
         JZ    YES                                                              
                                                                                
         MVI   BYTE1,D#W4SUF                                                    
                                                                                
         LA    RE,VSUFFIX                                                       
AVS10    CLC   RQW4SUF,0(RE)       ASSERT IT IS A VALID SUFFIX                  
         JE    YES                                                              
         CLI   L'VSUFFIX(RE),X'FF'                                              
         JE    NO                                                               
         LA    RE,L'VSUFFIX(RE)                                                 
         J     AVS10                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED SEX CODE IS VALID                   *         
***********************************************************************         
                                                                                
AVSEX    NTR1                                                                   
         CLI   RQW4SXC,0           IF SEX CODE IS PROVIDED                      
         JE    YES                                                              
         CLI   RQW4SXC,C'F'        ASSERT IT IS A VALID VALUE                   
         JE    YES                                                              
         CLI   RQW4SXC,C'M'                                                     
         JE    YES                                                              
         MVI   BYTE1,D#W4SXC                                                    
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED ETHNICITY CODE IS VALID             *         
***********************************************************************         
                                                                                
AVETH    NTR1                                                                   
         OC    RQW4ETH,RQW4ETH     IF ETHNICITY IS PROVIDED                     
         JE    YES                                                              
                                                                                
         MVI   BYTE1,D#W4ETH                                                    
                                                                                
         LA    R2,VETHCTY                                                       
AVE10    CLC   RQW4ETH,0(R2)       ASSERT THAT IT IS A VALID VALUE              
         JE    YES                                                              
         CLI   L'VETHCTY(R2),X'FF'                                              
         JE    NO                                                               
         LA    R2,L'VETHCTY(R2)                                                 
         J     AVE10                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED MEMBERSHIP NUMBER IS VALID          *         
***********************************************************************         
                                                                                
AVMNU    NTR1                                                                   
         OC    RQW4MNU,RQW4MNU     IF MEMBERSHIP NUMBER IS PROVIDED             
         JE    YES                                                              
         MVI   BYTE1,D#W4MNU                                                    
         CLI   RQW4MNU,C'A'        ASSERT THAT FIRST CHARACTER                  
         JL    NO                  IS A LETTER                                  
         CLI   RQW4MNU,C'Z'                                                     
         JNH   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED MARITAL STATUS IS VALID             *         
*        ON ENTRY ... P1 = A(MARITAL STATUS FIELD)                    *         
***********************************************************************         
                                                                                
AVMS     NTR1                                                                   
         ZICM  R2,1(R1),3          R2=A(MARITAL STATUS FIELD)                   
                                                                                
         CLI   0(R2),0             IF MARITAL STATUS IS PROVIDED                
         JE    YES                 ASSERT THAT IT IS A VALID VALUE              
         CLI   0(R2),C'M'                                                       
         JE    YES                                                              
         CLI   0(R2),C'S'                                                       
         JE    YES                                                              
         CLI   0(R2),C'H'                                                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED FIELD IS BETWEEN 0 AND 9999         *         
*        ON ENTRY ... P1 = A(FIELD TO VALIDATE)                       *         
***********************************************************************         
                                                                                
AV9999   NTR1                                                                   
         ZICM  R2,1(R1),3                                                       
         L     R2,0(R2)            R2=A(FIELD TO VALIDATE)                      
                                                                                
         LTR   R2,R2               ASSERT THAT FIELD IS NOT NEGATIVE            
         JM    NO                                                               
         LHI   RE,9999             OR GREATER THAN 9999                         
         CR    R2,RE                                                            
         JNH   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS PROVIDED FIELD IS BETWEEN 0 AND 9999999      *         
*        ON ENTRY ... P1 = A(FIELD TO VALIDATE)                       *         
***********************************************************************         
                                                                                
AV9X7    NTR1                                                                   
         ZICM  R2,1(R1),3                                                       
         L     R2,0(R2)            R2=A(FIELD TO VALIDATE)                      
                                                                                
         LTR   R2,R2               ASSERT THAT FIELD IS NOT NEGATIVE            
         JM    NO                                                               
         L     RE,=F'9999999'      OR GREATER THAN 9999999                      
         CR    R2,RE                                                            
         JNH   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
       ++INCLUDE TAPREFIX          VALID LAST NAME PREFIXES                     
                                                                                
       ++INCLUDE TANAMCHR          VALID FIRST/LAST NAME CHARACTERS             
                                                                                
       ++INCLUDE TASUFFIX          VALID SUFFIXES                               
                                                                                
       ++INCLUDE TAETHCTY          VALID ETHNICITIES                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SOCIAL SECURITY NUMBER                              *         
***********************************************************************         
                                                                                
VALSSN   NTR1  BASE=*,LABEL=*                                                   
         OC    RQW4PID,RQW4PID                                                  
         JNZ   XIT                                                              
         GOTOR SSN2PID,DMCB,RQW4SSN,RQW4PID                                     
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALSSN                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TASS2PID                                                       
***********************************************************************         
*        VALIDATE PASSED CORPORATION IDS                              *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALCRP   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RQW4CP1          R2=A(FIRST CORPORATION ID FIELD)             
         LHI   R0,7                R0=# OF CORPORATION ID FIELDS                
                                                                                
         LA    R4,ERW4C1NF         R4=A(FIRST CORP NOT ON FILE ERROR)           
         LA    R6,ERW4NCP1         R6=A(FIRST NOT A CORPORATION ERROR)          
                                                                                
         USING TLW4D,R3                                                         
VC10     OC    0(L'RQW4CP1,R2),0(R2)                                            
         JZ    VC60                                                             
         XC    TLW4KEY,TLW4KEY     IF CURRENT FIELD IS POPULATED                
         MVI   TLW4CD,TLW4CDQ      READ FOR W4 KEY/RECORD                       
         MVC   TLW4FID,0(R2)       AND ENSURE IT EXISTS                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VC20                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,(R4)                                      
         J     VC60                                                             
VC20     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            R4=A(W4 DETAILS ELEMENT)                     
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TAW4TYPE,TAW4TYCO   ENSURE W4 IS A CORPORATION                   
         JE    VC30                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,(R6)                                      
         J     VC60                                                             
         DROP  R4                                                               
                                                                                
         USING TAFLD,R4                                                         
VC30     L     R4,AIO3             IF CORPORATION HAS AN L IN                   
         MVI   ELCODE,TAFLELQ      FILTER FIELD ...                             
         BRAS  RE,GETEL                                                         
         JNE   VC60                                                             
         CLI   TAFLFLT1,TA99TYL                                                 
         JNE   VC60                                                             
         DROP  R4                                                               
                                                                                
         USING TLW4PD,R3                                                        
         XC    TLW4PKEY,TLW4PKEY   ... ENSURE THAT THIS W4 IS ITS               
         MVI   TLW4PCD,TLW4CCDQ    ONLY ASSOCIATION                             
         MVC   TLW4CCRP,0(R2)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VC50                                                             
VC40     GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
VC50     CLC   IOKEY(TLW4CSSN-TLW4PCD),IOKEYSAV                                 
         JNE   VC60                                                             
         CLC   TLW4CSSN,RQW4SSN                                                 
         JE    VC40                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4LHAW                                  
         DROP  R3                                                               
                                                                                
VC60     LA    R2,L'RQW4CP1(R2)    BUMP TO THE NEXT CORPORATION FIELD           
                                                                                
         USING ERRENTD,R4                                                       
         ZIC   RE,EELEN            BUMP TO NEXT "CORPORATION NOT                
         AR    R4,RE               ON FILE" ERROR                               
         DROP  R4                                                               
                                                                                
         USING ERRENTD,R6                                                       
         ZIC   RE,EELEN            BUMP TO NEXT "NOT A CORPORATION"             
         AR    R6,RE               ERROR                                        
         DROP  R6                                                               
                                                                                
         BCT   R0,VC10             GO PROCESS NEXT CORPORATION ID               
         J     XIT                 FIELD                                        
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALCRP                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4C1NF DC    AL1(EW4C1NFX-*),AL2(22),AL1(ERRCATY1),AL1(D#W4CP1)               
         DC    C'Corporation not on file'                                       
EW4C1NFX EQU   *                                                                
                                                                                
ERW4NCP1 DC    AL1(EW4NCP1X-*),AL2(23),AL1(ERRCATY1),AL1(D#W4CP1)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP1X EQU   *                                                                
                                                                                
ERW4C2NF DC    AL1(EW4C2NFX-*),AL2(25),AL1(ERRCATY1),AL1(D#W4CP2)               
         DC    C'Corporation not on file'                                       
EW4C2NFX EQU   *                                                                
                                                                                
ERW4NCP2 DC    AL1(EW4NCP2X-*),AL2(26),AL1(ERRCATY1),AL1(D#W4CP2)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP2X EQU   *                                                                
                                                                                
ERW4C3NF DC    AL1(EW4C3NFX-*),AL2(28),AL1(ERRCATY1),AL1(D#W4CP3)               
         DC    C'Corporation not on file'                                       
EW4C3NFX EQU   *                                                                
                                                                                
ERW4NCP3 DC    AL1(EW4NCP3X-*),AL2(29),AL1(ERRCATY1),AL1(D#W4CP3)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP3X EQU   *                                                                
                                                                                
ERW4C4NF DC    AL1(EW4C4NFX-*),AL2(31),AL1(ERRCATY1),AL1(D#W4CP4)               
         DC    C'Corporation not on file'                                       
EW4C4NFX EQU   *                                                                
                                                                                
ERW4NCP4 DC    AL1(EW4NCP4X-*),AL2(32),AL1(ERRCATY1),AL1(D#W4CP4)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP4X EQU   *                                                                
                                                                                
ERW4C5NF DC    AL1(EW4C5NFX-*),AL2(34),AL1(ERRCATY1),AL1(D#W4CP5)               
         DC    C'Corporation not on file'                                       
EW4C5NFX EQU   *                                                                
                                                                                
ERW4NCP5 DC    AL1(EW4NCP5X-*),AL2(35),AL1(ERRCATY1),AL1(D#W4CP5)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP5X EQU   *                                                                
                                                                                
ERW4C6NF DC    AL1(EW4C6NFX-*),AL2(37),AL1(ERRCATY1),AL1(D#W4CP6)               
         DC    C'Corporation not on file'                                       
EW4C6NFX EQU   *                                                                
                                                                                
ERW4NCP6 DC    AL1(EW4NCP6X-*),AL2(38),AL1(ERRCATY1),AL1(D#W4CP6)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP6X EQU   *                                                                
                                                                                
ERW4C7NF DC    AL1(EW4C7NFX-*),AL2(93),AL1(ERRCATY1),AL1(D#W4CRP)               
         DC    C'Corporation not on file'                                       
EW4C7NFX EQU   *                                                                
                                                                                
ERW4NCP7 DC    AL1(EW4NCP7X-*),AL2(94),AL1(ERRCATY1),AL1(D#W4CRP)               
         DC    C'W4 Type is not Corporation'                                    
EW4NCP7X EQU   *                                                                
                                                                                
ERW4LHAW DC    AL1(EW4LHAWX-*),AL2(99),AL1(ERRCATY1),AL1(D#W4CRP)               
         DC    C'LLC already has an associated W4'                              
EW4LHAWX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED AFM LOCAL                                    *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALAFM   NTR1  BASE=*,LABEL=*                                                   
         OC    RQW4AFM,RQW4AFM     ONLY VALIDATE IF AFM LOCAL HAS               
         JZ    XIT                 BEEN PASSED                                  
                                                                                
         USING TLLOD,R3                                                         
         XC    TLLOKEY,TLLOKEY     READ FOR LOCAL KEY                           
         MVI   TLLOCD,TLLOCDQ      AND ENSURE IT EXISTS                         
         MVC   TLLOUN,=C'AFM'                                                   
         MVC   TLLOLCL,RQW4AFM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4LONF                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALAFM                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4LONF DC    AL1(EW4LONFX-*),AL2(41),AL1(ERRCATY1),AL1(D#W4AFM)               
         DC    C'AFM Local not on file'                                         
EW4LONFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED TRUSTEE ID                                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALTRST  NTR1  BASE=*,LABEL=*                                                   
         OC    RQW4TRS,RQW4TRS     ONLY VALIDATE IF TRUSTEE ID HAS              
         JZ    XIT                 BEEN PASSED                                  
                                                                                
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY     READ FOR W4 KEY/RECORD                       
         MVI   TLW4CD,TLW4CDQ      AND ENSURE IT EXISTS                         
         MVC   TLW4FID,RQW4TRS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VT10                                                             
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4TRNF                                  
         J     XIT                                                              
VT10     GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            R4=A(W4 DETAILS ELEMENT)                     
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TAW4TYPE,TAW4TYTR   ENSURE W4 IS A TRUSTEE                       
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4NOTR                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTRST                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4TRNF DC    AL1(EW4TRNFX-*),AL2(70),AL1(ERRCATY1),AL1(D#W4TRS)               
         DC    C'Trustee not on file'                                           
EW4TRNFX EQU   *                                                                
                                                                                
ERW4NOTR DC    AL1(EW4NOTRX-*),AL2(71),AL1(ERRCATY1),AL1(D#W4TRS)               
         DC    C'W4 Type is not Trustee'                                        
EW4NOTRX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED FILTERS                                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
VALFILT  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQW4FIL,TA99TYL     ONLY VALIDATE IF FIRST CHARACTER             
         JNE   XIT                 IS L (LLC)                                   
         CLI   RQW4TYP,TAW4TYCO    AND W4 TYPE IS CORPORATION ...               
         JNE   XIT                                                              
                                                                                
         USING TLW4PD,R3                                                        
         XC    TLW4PKEY,TLW4PKEY   ENSURE THAT THE CORPORATION                  
         MVI   TLW4PCD,TLW4CCDQ    DOES NOT HAVE MORE THAN ONE                  
         MVC   TLW4CCRP,RQW4SSN    ASSOCIATION                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLW4CSSN-TLW4PCD),IOKEYSAV                                 
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLW4CSSN-TLW4PCD),IOKEYSAV                                 
         JNE   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FLTL                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALFILT                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4FLTL DC    AL1(EW4FLTLX-*),AL2(100),AL1(ERRCATY1),AL1(D#W4FIL)              
         DC    C'LLC can only have one associated W4'                           
EW4FLTLX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF W4 RECORD                *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4NTFD                                  
                                                                                
         OC    RQW4CRP,RQW4CRP                                                  
         JZ    IA10                                                             
         MVC   RQW4CP1,RQW4CRP                                                  
                                                                                
IA10     OI    PROSTAT,PSNPCHG                                                  
         OC    RQW4CP1(RQW4RLNQ),RQW4CP1                                        
         JNZ   IA20                                                             
         OC    RQW4PNM(RQW4PLNQ),RQW4PNM                                        
         JZ    IA30                                                             
IA20     OI    PROSTAT,PSPYCHG                                                  
                                                                                
IA30     CLI   RQW4FET,0                                                        
         JE    IA40                                                             
         OI    PROSTAT,PSTDCHG                                                  
                                                                                
IA40     CLI   RQW4SSN,C'9'        SS# CANNOT START WITH A 9                    
         JNE   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4ISSN                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4NTFD DC    AL1(EW4NTFDX-*),AL2(2),AL1(ERRCATY2),AL1(D#W4SSN)                
         DC    C'W4 not on file'                                                
EW4NTFDX EQU   *                                                                
                                                                                
ERW4ISSN DC    AL1(EW4ISSNX-*),AL2(1),AL1(ERRCATY1),AL1(D#W4SSN)                
         DC    C'Invalid Social Security Number'                                
EW4ISSNX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF W4 RECORD                  *         
*        ON ENTRY ... R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTCHA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TGTABLES,RE                                                      
         CLI   RQW4MOD,RQW4EXE     IF MODE IS EXECUTE                           
         JNE   IC10                ENSURE CHECK LOCKOUT STATUS IS OFF           
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_CHECKS',ERW4CKL                  
         JE    IC10                                                             
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_PRCHKS',ERW4CKL                  
         JE    IC10                                                             
         GOTOR (#TSTCLCK,ATSTCLCK),DMCB,=C'TAL_P+CHKS',ERW4CKL                  
         JE    IC10                                                             
         GOTOR (#SAVPTRS,ASAVPTRS) AND SAVE ORIGINAL POINTERS                   
                                                                                
IC10     LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
IC20     BRAS  RE,NEXTEL                                                        
         JNE   IC30                                                             
                                                                                
         CLI   0(R4),TAWXELQ       PROCESS EXISTING EXTRA DETAILS               
         JNE   *+12                                                             
         BRAS  RE,CPYTAWX                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TACMELQ       PROCESS EXISTING COMMENT                     
         JNE   *+12                                                             
         BRAS  RE,CPYTACM                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAPEELQ       PROCESS EXISTING PAYEE INFORMATION           
         JNE   *+12                                                             
         BRAS  RE,CPYTAPE                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAOWELQ       PROCESS EXISTING OTHER WITHHOLDINGS          
         JNE   *+12                                                             
         BRAS  RE,CPYTAOW                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAWHELQ       PROCESS EXISTING WITHHOLDING DETAILS         
         JNE   *+12                                                             
         BRAS  RE,CPYTAWH                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAAKELQ       PROCESS EXISTING AKA NAME                    
         JNE   *+12                                                             
         BRAS  RE,CPYTAAK                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAW4ELQ       PROCESS EXISTING W4 DETAILS                  
         JNE   *+12                                                             
         BRAS  RE,CPYTAW4                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TATIELQ       PROCESS EXISTING CORPORATION TAX ID          
         JNE   *+12                                                             
         BRAS  RE,CPYTATI                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAFLELQ       PROCESS EXISTING FILTERS                     
         JNE   *+12                                                             
         BRAS  RE,CPYTAFL                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAFNELQ       PROCESS EXISTING FREE FORM NAMES             
         JNE   *+12                                                             
         BRAS  RE,CPYTAFN                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TANUELQ       PROCESS EXISTING NUMBERS                     
         JNE   *+12                                                             
         BRAS  RE,CPYTANU                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAA2ELQ       PROCESS EXISTING ADDRESS                     
         JNE   *+12                                                             
         BRAS  RE,CPYTAA2                                                       
         J     IC20                                                             
                                                                                
         CLI   0(R4),TAD1ELQ       PROCESS EXISTING TD1                         
         JNE   IC20                                                             
         BRAS  RE,CPYTAD1                                                       
         J     IC20                                                             
                                                                                
IC30     BRAS  RE,NEWTAWX          PROCESS NEW EXTRA DETAILS                    
         BRAS  RE,NEWTACM          PROCESS NEW COMMENT                          
         BRAS  RE,NEWTAPE          PROCESS NEW PAYEE INFORMATION                
         BRAS  RE,NEWTAOW          PROCESS NEW OTHER WITHHOLDINGS               
         BRAS  RE,NEWTAWH          PROCESS NEW WITHHOLDING DETAILS              
         BRAS  RE,NEWTAAK          PROCESS NEW AKA NAME                         
         BRAS  RE,NEWTATI          PROCESS NEW TAX ID INFORMATION               
         BRAS  RE,NEWTAFL          PROCESS NEW FILTERS                          
         BRAS  RE,NEWTANU          PROCESS NEW NUMBERS                          
         BRAS  RE,NEWTAD1          PROCESS NEW TD1                              
                                                                                
***********************************************************************         
                                                                                
         OC    RQW4CRP,RQW4CRP     IF CORPORATION ID IS PRESENT ...             
         JZ    IC70                                                             
                                                                                
         LA    R2,RQW4CP1          R2=A(FIRST CORPORATION ID FIELD)             
         LHI   R0,6                R0=# OF CORPORATION ID FIELDS                
         XR    R1,R1               R1=A(FIRST BLANK CORP ID FIELD)              
                                                                                
IC40     CLC   RQW4CRP,0(R2)       EXIT IF CORPORATION IS ALREADY               
         JE    IC70                ATTACHED TO THE W4                           
                                                                                
         LTR   R1,R1               IF WE HAVE NOT ALREADY NOTED                 
         JNZ   IC50                FIRST BLANK CORP ID FIELD                    
         OC    0(L'RQW4CP1,R2),0(R2)                                            
         JNZ   IC50                AND THIS FIELD IS BLANK                      
         LR    R1,R2               NOTE IT                                      
                                                                                
IC50     LA    R2,L'RQW4CP1(R2)                                                 
         BCT   R0,IC40                                                          
                                                                                
         LTR   R1,R1               INSERT CORPORATION ID INTO                   
         JZ    IC60                FIRST BLANK FIELD                            
         MVC   0(L'RQW4CRP,R1),RQW4CRP                                          
         OI    PROSTAT,PSPYCHG                                                  
         LA    RE,RQW4CP1                                                       
         CR    R1,RE                                                            
         JNE   IC70                                                             
         OI    PROSTAT,PSNPCHG                                                  
         J     IC120                                                            
                                                                                
IC60     GOTOR (#ADDERR,AADDERR),DMCB,ERW4CRP                                   
                                                                                
***********************************************************************         
                                                                                
IC70     CLI   BYPSERRS,X'FF'      IF ANY ERRORS WERE BYPASSED ...              
         JE    IC120                                                            
                                                                                
         LA    R2,BYPSERRS         SCAN THROUGH ALL ERRORS                      
IC80     BRAS  RE,CORPCHG          IF BYPASSED ERROR IS CORPORATION             
         JE    IC90                                                             
         BRAS  RE,TAPECHG          OR PAYEE RELATED                             
         JE    IC90                                                             
         BRAS  RE,TAD1CHG          OR TD1 RELATED                               
         JE    IC90                                                             
         OI    PROSTAT,PSNPCHG     SET TO UPDATE REGULAR ELEMENT                
         J     IC110                                                            
IC90     OI    PROSTAT,PSPYCHG     OR PAYEE ELEMENT                             
         J     IC110                                                            
IC100    OI    PROSTAT,PSTDCHG     OR TD1 ELEMENT                               
IC110    CLI   L'EENUMB(R2),X'FF'                                               
         JE    DAC10                                                            
         LA    R2,L'EENUMB(R2)                                                  
         J     IC80                                                             
                                                                                
***********************************************************************         
                                                                                
IC120    BAS   RE,DELTAAC          DELETE ACTIVITY ELEMENTS                     
         BAS   RE,STNMCHG          SET NAME CHANGE STATUS                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DELETES ACTIVITY ELEMENTS WHEN APPROPRIATE           *         
*        ON ENTRY ... R4 = A(ACTIVITY ELEMENT)                        *         
***********************************************************************         
                                                                                
DELTAAC  NTR1                                                                   
         USING TAACD,R4                                                         
DAC10    L     R4,AIO3                                                          
         MVI   ELCODE,TAACELQ      READ THROUGH ALL ACTIVITY                    
         BRAS  RE,GETEL            ELEMENTS                                     
         J     *+8                                                              
DAC20    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         CLI   TAACSCR,X'24'       IF ELEMENT IS FOR PAYEE SCREEN               
         JNE   DAC30                                                            
         TM    PROSTAT,PSPYCHG     AND PAYEE SCREEN FIELD HAS BEEN              
         JZ    DAC20               UPDATED                                      
         MVI   0(R4),X'FF'         MARK ELEMENT FOR DELETION                    
         J     DAC20                                                            
                                                                                
DAC30    CLI   TAACSCR,X'76'       IF ELEMENT IS FOR TD1 SCREEN                 
         JNE   DAC40                                                            
         TM    PROSTAT,PSTDCHG     AND TD1 SCREEN FIELD HAS BEEN                
         JZ    DAC20               UPDATED                                      
         MVI   0(R4),X'FF'         MARK ELEMENT FOR DELETION                    
         J     DAC20                                                            
                                                                                
DAC40    TM    PROSTAT,PSNPCHG     IF ELEMENT IS FOR NON-PAYEE SCREEN           
         JZ    DAC20               AND NON-PAYEE SCREEN FIELD HAS BEEN          
         MVI   0(R4),X'FF'         UPDATED, MARK ELEMENT FOR DELETION           
         J     DAC20                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITCHA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4CKL  DC    AL1(EW4CKLX-*),AL2(500),AL1(ERRCATY1),AL1(D#W4MOD)               
         DC    C'Urgent check run in progress'                                  
EW4CKLX  EQU   *                                                                
                                                                                
ERW4CRP  DC    AL1(EW4CRPX-*),AL2(95),AL1(ERRCATY1),AL1(D#W4CRP)                
         DC    C'Maximum number of Corporations already reached'                
EW4CRPX  EQU   *                                                                
                                                                                
***********************************************************************         
*        ROUTINE SETS NAME CHANGE STATUS                              *         
*        ON ENTRY ... R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
STNMCHG  NTR1                                                                   
         USING TAW4D,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAW4ELQ      READ W4 DETAILS ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LA    RE,RQW4LNM                                                       
         OC    RQW4LNM,RQW4LNM                                                  
         JNZ   *+8                                                              
         LA    RE,RQW4CPN                                                       
         CLC   TAW4CRPN,0(RE)      IF NAME IS CHANGING                          
         JE    XIT                                                              
         OI    PROSTAT,PSNMCHG     SET NAME CHANGED STATUS                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED W4 TYPE                                      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
VALTYPE  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQW4TYP,TAW4TYCO    IF W4 TYPE IS NOT CORPORATION                
         JE    XIT                                                              
                                                                                
         USING TLANPD,R3                                                        
         XC    TLANPKEY,TLANPKEY   ENSURE SSN IS NOT ASSOCIATED TO              
         MVI   TLANPCD,TLANSCDQ    ANY AGENTS                                   
         MVC   TLANSSSN,RQW4SSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLANSAGT-TLANPD),IOKEYSAV                                  
         JNE   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4ANS                                   
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTYPE                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4ANS  DC    AL1(EW4ANSX-*),AL2(115),AL1(ERRCATY1),AL1(D#W4TYP)               
         DC    C'W4 associated to Agent - type must be corporation'             
EW4ANSX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PASSED W4 TYPE/LOCKED STATUS                        *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(I/O AREA 3)                                *         
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
VALTYLK  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAW4TYPE,RQW4TYP    IF W4 TYPE IS BEING CHANGED                  
         JNE   VTYP10                                                           
         TM    TAW4STA2,TAW4SLCK   OR W4 IS GOING FROM UNLOCKED                 
         JO    XIT                                                              
         CLI   RQW4LCK,C'Y'        TO LOCKED                                    
         JNE   XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TLCKPD,R3                                                        
VTYP10   XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKPCD,TLCKECDQ    PERFORMER                                    
         MVC   TLCKESSN,RQW4SSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     VTYP30                                                           
VTYP20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
VTYP30   CLC   IOKEY(TLCKECUR-TLCKPD),IOKEYSAV                                  
         JNE   VTYP50                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
         DROP  R3                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VTYP20                                                           
         TM    TAPDSTAT,TAPDSCNL   IF CHECK IS CANCELLED/CANCELLER              
         JO    VTYP20                                                           
         TM    TAPDPST1,TAPDPBNP   OR A BNP                                     
         JO    VTYP20                                                           
         OC    TAPDPAYI,TAPDPAYI   OR A ZERO DOLLAR CHECK                       
         JNZ   VTYP40              NO NEED TO CHECK THE CHECK DATE              
         OC    TAPDPAYC,TAPDPAYC                                                
         JNZ   VTYP40                                                           
         OC    TAPDREXP,TAPDREXP                                                
         JZ    VTYP20                                                           
         DROP  R4                                                               
                                                                                
         USING TACDD,R4                                                         
VTYP40   L     R4,AIO4             OTHERWISE ...                                
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VTYP20                                                           
         OC    TACDDTE,TACDDTE     IF CHECK DOES NOT HAVE A DATE                
         JNZ   VTYP20              RETURN ERROR                                 
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4TCN                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
VTYP50   L     R4,AIO3                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAW4TYPE,RQW4TYP    IF W4 TYPE IS BEING CHANGED                  
         JE    XIT                                                              
         CLI   RQW4TYP,TAW4TYTR    TO TRUSTEE                                   
         JNE   XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   ENSURE PERFORMER HAS NO                      
         MVI   TLCAPCD,TLCACCDQ    CAST RECORDS                                 
         MVC   TLCACSSN,RQW4SSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         CLC   IOKEY(TLCACCOM-TLCAPD),IOKEYSAV                                  
         JNE   XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4TCT                                   
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALTYLK                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4TCN  DC    AL1(EW4TCNX-*),AL2(101),AL1(ERRCATY1),AL1(D#W4TYP)               
         DC    C'W4 cannot be saved until all open payments are '               
         DC    C'processed'                                                     
EW4TCNX  EQU   *                                                                
                                                                                
ERW4TCT  DC    AL1(EW4TCTX-*),AL2(114),AL1(ERRCATY1),AL1(D#W4TYP)               
         DC    C'Cannot change to trustee. Performer is on cast.'               
EW4TCTX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES EXTRA DETAILS ELEMENT INTO REQUEST MAP        *         
*        ON ENTRY ... R4 = A(EXTRA DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAWXD,R4                                                         
CPYTAWX  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4DOB,RQW4DOB),TAWXDOB,     +        
               ERW4DOB,0                                                        
         GOTOR (RF),(R1),(L'RQW4DED,RQW4DED),TAWXPCT,ERW4DED,0                  
         GOTOR (RF),(R1),(L'RQW4TRS,RQW4TRS),TAWXTSSN,ERW4TRS,0                 
         GOTOR (RF),(R1),(L'RQW4ERN,RQW4ERN),TAWXMERN,ERW4ERN,0                 
         OI    CPYSTAT1,CPYSTAWX                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF EXTRA DETAILS ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY  *         
*        AND EXTRA DETAILS ARE BEING ADDED NOW, ROUTINE PROCESSES THEM*         
***********************************************************************         
                                                                                
NEWTAWX  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAWX   IF EXTRA DETAILS ELEMENT DID NOT             
         JO    XIT                 EXIST ON RECORD PREVIOUSLY ...               
                                                                                
         OC    RQW4DOB,RQW4DOB     IF DATE OF BIRTH IS BEING ADDED              
         JZ    NWX10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4DOB                                   
                                                                                
NWX10    OC    RQW4DED,RQW4DED     IF DEDUCTION PCT IS BEING ADDED              
         JZ    NWX20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4DED                                   
                                                                                
NWX20    OC    RQW4TRS,RQW4TRS     IF TRUSTEE IS BEING ADDED                    
         JZ    NWX30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4TRS                                   
                                                                                
NWX30    OC    RQW4ERN,RQW4ERN     IF EARNINGS IS BEING ADDED                   
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4ERN                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAWX AND NEWTAWX                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4DOB  DC    AL1(EW4DOBX-*),AL2(68),AL1(ERRCATY2),AL1(D#W4DOB)                
         DC    C'Review update to Date of Birth'                                
EW4DOBX  EQU   *                                                                
                                                                                
ERW4DED  DC    AL1(EW4DEDX-*),AL2(69),AL1(ERRCATY2),AL1(D#W4DED)                
         DC    C'Review update to Deduction %'                                  
EW4DEDX  EQU   *                                                                
                                                                                
ERW4TRS  DC    AL1(EW4TRSX-*),AL2(72),AL1(ERRCATY2),AL1(D#W4TRS)                
         DC    C'Review update to Trustee ID'                                   
EW4TRSX  EQU   *                                                                
                                                                                
ERW4ERN  DC    AL1(EW4ERNX-*),AL2(66),AL1(ERRCATY2),AL1(D#W4ERN)                
         DC    C'Review update to Earnings'                                     
EW4ERNX  EQU   *                                                                
                                                                                
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
         ZIC   RF,TACMLEN          COPY EXISTING COMMENT ELEMENT                
         BCTR  RF,0                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
***********************************************************************         
                                                                                
         CLI   TACMTYPE,TACMTYPG   PROCESS GENERAL COMMENT                      
         JNE   CCM10                                                            
         OI    CPYSTAT1,CPYTACMG   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE IT                         
         DROP  R4                                                               
                                                                                
         USING TACMD,R2                                                         
         OC    TACMCOMM(L'RQW4CMT),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4CMT,RQW4CMT),TACMCOMM,    +        
               ERW4CMT,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TACMD,R4                                                         
CCM10    CLI   TACMTYPE,TACMTYPI   PROCESS EMAIL ADDRESS                        
         JNE   XIT                                                              
         OI    CPYSTAT3,CPYTACMI   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENET                   
         DROP  R4                                                               
                                                                                
         USING TACMD,R2                                                         
         OC    TACMCOMM(L'RQW4EML),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4EML,RQW4EML),TACMCOMM,    +        
               ERW4EML,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF COMMENT ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY        *         
*        AND COMMENT IS BEING ADDED NOW, ROUTINE PROCESSES IT         *         
***********************************************************************         
                                                                                
NEWTACM  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYTACMG   IF GENERAL COMMENT ELEMENT DID NOT           
         JO    NCM10               EXIST ON RECORD PREVIOUSLY                   
         OC    RQW4CMT,RQW4CMT     AND IS BEING ADDED NOW                       
         JZ    NCM10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CMT                                   
                                                                                
NCM10    TM    CPYSTAT3,CPYTACMI   IF EMAIL ADDRESS ELEMENT DID NOT             
         JO    XIT                 EXIST ON RECORD PREVIOUSLY                   
         OC    RQW4EML,RQW4EML     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4EML                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTACM AND NEWTACM                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4CMT  DC    AL1(EW4CMTX-*),AL2(76),AL1(ERRCATY2),AL1(D#W4CMT)                
         DC    C'Review update to Comment'                                      
EW4CMTX  EQU   *                                                                
                                                                                
ERW4EML  DC    AL1(EW4EMLX-*),AL2(83),AL1(ERRCATY2),AL1(D#W4EML)                
         DC    C'Review update to Email Address'                                
EW4EMLX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES PAYEE ELEMENT INTO REQUEST MAP                *         
*        ON ENTRY ... R2 = A(ELEM)                                    *         
*                     R4 = A(PAYEE ELEMENT)                           *         
***********************************************************************         
                                                                                
         USING TAPED,R4                                                         
CPYTAPE  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         ZIC   RF,TAPELEN          COPY EXISTING COMMENT ELEMENT                
         BCTR  RF,0                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
         OI    CPYSTAT1,CPYSTAPE                                                
         MVI   0(R4),X'FF'                                                      
         DROP  R4                                                               
                                                                                
         USING TAPED,R2                                                         
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4PNM,RQW4PNM),TAPENAME,    +        
               ERW4PNM,0                                                        
         GOTOR (RF),(R1),(L'RQW4PA1,RQW4PA1),TAPEADD1,ERW4PA1,0                 
         GOTOR (RF),(R1),(L'RQW4PAC,RQW4PAC),TAPEACT,ERW4PAC,0                  
         GOTOR (RF),(R1),(L'RQW4PEX,RQW4PEX),TAPEEXP,ERW4PEX,0                  
                                                                                
         OC    RQW4PA1,RQW4PA1     IF PAYEE ADDRESS IS PRESENT                  
         JZ    XIT                 CHECK MORE FIELDS                            
         GOTOR (RF),(R1),(L'RQW4PA2,RQW4PA2),TAPEADD2,ERW4PA2,0                 
         GOTOR (RF),(R1),(L'RQW4PCY,RQW4PCY),TAPECITY,ERW4PCY,0                 
         GOTOR (RF),(R1),(L'RQW4PZP,RQW4PZP),TAPEZIP,ERW4PZP,0                  
         GOTOR (RF),(R1),(L'RQW4PCT,RQW4PCT),TAPECTRY,ERW4PCT,0                 
                                                                                
         CLC   RQW4PCT,=C'US'      IF PAYEE COUNTRY IS US                       
         JNE   CP10                HANDLE ADDRESS LINE 3 AND STATE              
         GOTOR (RF),(R1),(L'RQW4PA3,RQW4PA3),TAPEADD3,ERW4PA3,0                 
         GOTOR (RF),(R1),(L'RQW4PST,RQW4PST),TAPEST,ERW4PST,0                   
         J     XIT                                                              
                                                                                
CP10     CLC   RQW4PCT,=C'CA'      IF COUNTRY IS CANADA                         
         JNE   CP20                CLEAR ADDRESS LINE 3                         
         XC    RQW4PA3,RQW4PA3     AND HANDLE PROVINCE                          
         GOTOR (RF),(R1),(L'RQW4PST,RQW4PST),TAPEST,ERW4PST,0                   
         J     XIT                                                              
                                                                                
CP20     XC    RQW4PA3,RQW4PA3     ELSE CLEAR ADDRESS LINE 3                    
         XC    RQW4PST,RQW4PST     AND STATE                                    
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF PAYEE ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY AND      *         
*        PAYEE INFORMATION IS BEING ADDED NOW, ROUTINE PROCESSES IT   *         
***********************************************************************         
                                                                                
NEWTAPE  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYSTAPE   IF PAYEE ELEMENT DID NOT EXIST ON            
         JO    XIT                 RECORD PREVIOUSLY ...                        
                                                                                
         OC    RQW4PNM,RQW4PNM     IF PAYEE NAME IS BEING ADDED                 
         JZ    NPE10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PNM                                   
                                                                                
NPE10    OC    RQW4PA1,RQW4PA1     IF PAYEE ADDRESS 1 IS BEING ADDED            
         JZ    NPE20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PA1                                   
                                                                                
NPE20    OC    RQW4PA2,RQW4PA2     IF PAYEE ADDRESS 2 IS BEING ADDED            
         JZ    NPE30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PA2                                   
                                                                                
NPE30    OC    RQW4PA3,RQW4PA3     IF PAYEE ADDRESS 3 IS BEING ADDED            
         JZ    NPE40               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PA3                                   
                                                                                
NPE40    OC    RQW4PCY,RQW4PCY     IF PAYEE CITY IS BEING ADDED                 
         JZ    NPE50               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PCY                                   
                                                                                
NPE50    OC    RQW4PST,RQW4PST     IF PAYEE STATE IS BEING ADDED                
         JZ    NPE60               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PST                                   
                                                                                
NPE60    OC    RQW4PZP,RQW4PZP     IF PAYEE ZIP CODE IS BEING ADDED             
         JZ    NPE70               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PZP                                   
                                                                                
NPE70    OC    RQW4PCT,RQW4PCT     IF PAYEE COUNTRY IS BEING ADDED              
         JZ    NPE80               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PCT                                   
                                                                                
NPE80    OC    RQW4PAC,RQW4PAC     IF PAYEE ACTIVE DATE IS BEING ADDED          
         JZ    NPE90               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PAC                                   
                                                                                
NPE90    OC    RQW4PEX,RQW4PEX     IF PAYEE EXPIRATION DATE IS BEING            
         JZ    XIT                 ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PEX                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF PASSED ERROR ENTRY IS REVIEW UPDATE    *         
*        TO ONE OF THE PAYEE FIELDS                                   *         
*        ON ENTRY ... 0(R2) = ERROR ENTRY                             *         
***********************************************************************         
                                                                                
TAPECHG  NTR1  BASE=*,LABEL=*                                                   
         CLC   ERW4PNM+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PA1+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PA2+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PA3+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PCY+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PST+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PZP+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PCT+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PAC+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PEX+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAPE AND NEWTAPE AND TAPECHG            *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4PNM  DC    AL1(EW4PNMX-*),AL2(77),AL1(ERRCATY2),AL1(D#W4PNM)                
         DC    C'Review update to Payee Name'                                   
EW4PNMX  EQU   *                                                                
                                                                                
ERW4PA1  DC    AL1(EW4PA1X-*),AL2(78),AL1(ERRCATY2),AL1(D#W4PA1)                
         DC    C'Review update to Payee Address Line 1'                         
EW4PA1X  EQU   *                                                                
                                                                                
ERW4PA2  DC    AL1(EW4PA2X-*),AL2(79),AL1(ERRCATY2),AL1(D#W4PA2)                
         DC    C'Review update to Payee Address Line 2'                         
EW4PA2X  EQU   *                                                                
                                                                                
ERW4PA3  DC    AL1(EW4PA3X-*),AL2(80),AL1(ERRCATY2),AL1(D#W4PA3)                
         DC    C'Review update to Payee Address Line 3'                         
EW4PA3X  EQU   *                                                                
                                                                                
ERW4PCY  DC    AL1(EW4PCYX-*),AL2(85),AL1(ERRCATY2),AL1(D#W4PCY)                
         DC    C'Review update to Payee City'                                   
EW4PCYX  EQU   *                                                                
                                                                                
ERW4PST  DC    AL1(EW4PSTX-*),AL2(86),AL1(ERRCATY2),AL1(D#W4PST)                
         DC    C'Review update to Payee State'                                  
EW4PSTX  EQU   *                                                                
                                                                                
ERW4PZP  DC    AL1(EW4PZPX-*),AL2(87),AL1(ERRCATY2),AL1(D#W4PZP)                
         DC    C'Review update to Payee Zip Code'                               
EW4PZPX  EQU   *                                                                
                                                                                
ERW4PCT  DC    AL1(EW4PCTX-*),AL2(88),AL1(ERRCATY2),AL1(D#W4PCT)                
         DC    C'Review update to Payee Country'                                
EW4PCTX  EQU   *                                                                
                                                                                
ERW4PAC  DC    AL1(EW4PACX-*),AL2(89),AL1(ERRCATY2),AL1(D#W4PAC)                
         DC    C'Review update to Payee Active Date'                            
EW4PACX  EQU   *                                                                
                                                                                
ERW4PEX  DC    AL1(EW4PEXX-*),AL2(81),AL1(ERRCATY2),AL1(D#W4PEX)                
         DC    C'Review update to Payee Expiration Date'                        
EW4PEXX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES OTHER WITHHOLDING ELEMENTS INTO REQUEST MAP   *         
*        ON ENTRY ... R4 = A(OTHER WITHHOLDING ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAOWD,R4                                                         
CPYTAOW  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAOWTYPE,TAOWTMPF   PROCESS MOTION PICTURE FUND                  
         JNE   COW10                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4MPR,RQW4MPR),TAOWFLAT,    +        
               ERW4MPR,0                                                        
         OI    CPYSTAT1,CPYTAOWM                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
                                                                                
COW10    CLI   TAOWTYPE,TAOWTCHA    PROCESS PERMANENT CHARITY                   
         JNE   XIT                                                              
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4PCH,RQW4PCH),TAOWFLAT,    +        
               ERW4PCH,0                                                        
         OI    CPYSTAT1,CPYTAOWC                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF OTHER WITHHOLDING ELEMENTS DID NOT EXIST ON RECORD        *         
*        PREVIOUSLY AND OTHER WITHHOLDINGS ARE BEING ADDED NOW,       *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAOW  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYTAOWM   IF MOTION PICTURE FUND ELEMENT DID           
         JO    NOW10               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4MPR,RQW4MPR     AND IS BEING ADDED NOW                       
         JZ    NOW10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4MPR                                   
                                                                                
NOW10    TM    CPYSTAT1,CPYTAOWC   IF PERMANENT CHARITY ELEMENT DID             
         JO    XIT                 NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4PCH,RQW4PCH     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PCH                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAOW AND NEWTAOW                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4MPR  DC    AL1(EW4MPRX-*),AL2(73),AL1(ERRCATY2),AL1(D#W4MPR)                
         DC    C'Review update to MPR Fund %'                                   
EW4MPRX  EQU   *                                                                
                                                                                
ERW4PCH  DC    AL1(EW4PCHX-*),AL2(74),AL1(ERRCATY2),AL1(D#W4PCH)                
         DC    C'Review update to Permanent Charity %'                          
EW4PCHX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES EMPLOYEE WITHHOLDING DETAILS ELEMENT INTO     *         
*        REQUEST MAP                                                  *         
*        ON ENTRY ... R4 = A(EMPLOYEE WITHHOLDING DETAILS ELEMENT)    *         
***********************************************************************         
                                                                                
         USING TAWHD,R4                                                         
CPYTAWH  NTR1  BASE=*,LABEL=*                                                   
         CLC   TAWHUNIT(2),=C'FD'  PROCESS FEDERAL WITHHOLDING                  
         JNE   CWH10                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4FMS,RQW4FMS),TAWHSTAT,    +        
               ERW4FMS,(X'FF',RQW4TYP),CWHEI                                    
         GOTOR (RF),(R1),(L'RQW4FEX,RQW4FEX),TAWHEXS,ERW4FEX,          +        
               (L'RQW4FMS,RQW4FMS)                                              
         GOTOR (RF),(R1),(L'RQW4FFX,RQW4FFX),TAWHFLAT,ERW4FFX,         +        
               (L'RQW4FMS,RQW4FMS)                                              
         OI    CPYSTAT1,CPYTAWHF                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
                                                                                
CWH10    CLI   TAWHUNIT+2,C' '     PROCESS STATE WITHHOLDING                    
         JNE   CWH20                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4SAR,RQW4SAR),TAWHUNIT,    +        
               ERW4SAR,(X'FF',RQW4TYP),CWHEI                                    
         GOTOR (RF),(R1),(L'RQW4SMS,RQW4SMS),TAWHSTAT,ERW4SMS,         +        
               (L'RQW4SAR,RQW4SAR)                                              
         GOTOR (RF),(R1),(L'RQW4SEX,RQW4SEX),TAWHEXS,ERW4SEX,          +        
               (L'RQW4SAR,RQW4SAR)                                              
         GOTOR (RF),(R1),(L'RQW4SFX,RQW4SFX),TAWHFLAT,ERW4SFX,         +        
               (L'RQW4SAR,RQW4SAR)                                              
         OI    CPYSTAT1,CPYTAWHS                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
                                                                                
CWH20    GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4CAR,RQW4CAR),TAWHUNIT,    +        
               ERW4CAR,(X'FF',RQW4TYP),CWHEI                                    
         GOTOR (RF),(R1),(L'RQW4CMS,RQW4CMS),TAWHSTAT,ERW4CMS,         +        
               (L'RQW4CAR,RQW4CAR)                                              
         GOTOR (RF),(R1),(L'RQW4CEX,RQW4CEX),TAWHEXS,ERW4CEX,          +        
               (L'RQW4CAR,RQW4CAR)                                              
         GOTOR (RF),(R1),(L'RQW4CFX,RQW4CFX),TAWHFLAT,ERW4CFX,         +        
               (L'RQW4CAR,RQW4CAR)                                              
         OI    CPYSTAT1,CPYTAWHC                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF WITHHOLDING DETAILS ELEMENTS DID NOT EXIST ON RECORD      *         
*        PREVIOUSLY AND WITHHOLDING DETAILS ARE BEING ADDED NOW,      *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTAWH  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT1,CPYTAWHF   IF FEDERAL WITHHOLDING ELEMENT DID           
         JO    NWH20               NOT EXIST ON RECORD PREVIOUSLY ...           
                                                                                
         CLI   RQW4FMS,0           IF MARRIED/SINGLE IS BEING ADDED             
         JE    NWH10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FMS                                   
         GOTOR (RF),(R1),ERW4FEX                                                
                                                                                
NWH10    OC    RQW4FFX,RQW4FFX     IF FIXED PERCENTAGE IS BEING ADDED           
         JZ    NWH20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERW4FFX                                                
                                                                                
***********************************************************************         
                                                                                
NWH20    TM    CPYSTAT1,CPYTAWHS   IF STATE WITHHOLDING ELEMENT DID             
         JO    NWH30               NOT EXIST ON RECORD PREVIOUSLY ...           
                                                                                
         OC    RQW4SAR,RQW4SAR     IF AREA IS BEING ADDED                       
         JZ    NWH30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4SAR                                   
         GOTOR (RF),(R1),ERW4SMS                                                
         GOTOR (RF),(R1),ERW4SEX                                                
                                                                                
         OC    RQW4SFX,RQW4SFX     IF FIXED PERCENTAGE IS BEING ADDED           
         JZ    NWH30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (RF),(R1),ERW4SFX                                                
                                                                                
***********************************************************************         
                                                                                
NWH30    TM    CPYSTAT1,CPYTAWHC   IF CITY WITHHOLDING ELEMENT DID              
         JO    XIT                 NOT EXIST ON RECORD PREVIOUSLY ...           
                                                                                
         OC    RQW4CAR,RQW4CAR     IF AREA IS BEING ADDED                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CAR                                   
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CMS                                   
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CEX                                   
                                                                                
         OC    RQW4CFX,RQW4CFX     IF FIXED PERCENTAGE IS BEING ADDED           
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CFX                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAWH AND NEWTAWH                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4FMS  DC    AL1(EW4FMSX-*),AL2(55),AL1(ERRCATY2),AL1(D#W4FMS)                
         DC    C'Review update to Federal Tax M/S'                              
EW4FMSX  EQU   *                                                                
                                                                                
ERW4FEX  DC    AL1(EW4FEXX-*),AL2(56),AL1(ERRCATY2),AL1(D#W4FEX)                
         DC    C'Review update to Federal Tax Exemptions'                       
EW4FEXX  EQU   *                                                                
                                                                                
ERW4FFX  DC    AL1(EW4FFXX-*),AL2(57),AL1(ERRCATY2),AL1(D#W4FFX)                
         DC    C'Review update to Federal Tax Fixed %'                          
EW4FFXX  EQU   *                                                                
                                                                                
ERW4SAR  DC    AL1(EW4SARX-*),AL2(58),AL1(ERRCATY2),AL1(D#W4SAR)                
         DC    C'Review update to State Tax Area'                               
EW4SARX  EQU   *                                                                
                                                                                
ERW4SMS  DC    AL1(EW4SMSX-*),AL2(59),AL1(ERRCATY2),AL1(D#W4SMS)                
         DC    C'Review update to State Tax M/S'                                
EW4SMSX  EQU   *                                                                
                                                                                
ERW4SEX  DC    AL1(EW4SEXX-*),AL2(60),AL1(ERRCATY2),AL1(D#W4SEX)                
         DC    C'Review update to State Tax Exemptions'                         
EW4SEXX  EQU   *                                                                
                                                                                
ERW4SFX  DC    AL1(EW4SFXX-*),AL2(61),AL1(ERRCATY2),AL1(D#W4SFX)                
         DC    C'Review update to State Tax Fixed %'                            
EW4SFXX  EQU   *                                                                
                                                                                
ERW4CAR  DC    AL1(EW4CARX-*),AL2(62),AL1(ERRCATY2),AL1(D#W4CAR)                
         DC    C'Review update to City Tax Area'                                
EW4CARX  EQU   *                                                                
                                                                                
ERW4CMS  DC    AL1(EW4CMSX-*),AL2(63),AL1(ERRCATY2),AL1(D#W4CMS)                
         DC    C'Review update to City Tax M/S'                                 
EW4CMSX  EQU   *                                                                
                                                                                
ERW4CEX  DC    AL1(EW4CEXX-*),AL2(64),AL1(ERRCATY2),AL1(D#W4CEX)                
         DC    C'Review update to City Tax Exemptions'                          
EW4CEXX  EQU   *                                                                
                                                                                
ERW4CFX  DC    AL1(EW4CFXX-*),AL2(65),AL1(ERRCATY2),AL1(D#W4CFX)                
         DC    C'Review update to City Tax Fixed %'                             
EW4CFXX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CWHEI    DC    AL1(TAW4TYES,TAW4TYIN),X'FF'                                     
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES EMPLOYEE AKA NAME ELEMENT INTO REQUEST MAP    *         
*        ON ENTRY ... R4 = A(EMPLOYEE AKA NAME ELEMENT)               *         
***********************************************************************         
                                                                                
         USING TAAKD,R4                                                         
CPYTAAK  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4AKF,RQW4AKF),TAAKNAM1,    +        
               ERW4AKF,(0,RQW4TYP),CAKCT                                        
         GOTOR (RF),(R1),(L'RQW4AKL,RQW4AKL),TAAKNAM2,ERW4AKL,         +        
               (L'RQW4AKF,RQW4AKF)                                              
         OI    CPYSTAT2,CPYSTAAK                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF EMPLOYEE AKA NAME ELEMENT DID NOT EXIST ON RECORD         *         
*        PREVIOUSLY AND EMPLOYEE AKA NAME IS BEING ADDED NOW,         *         
*        ROUTINE PROCESSES IT                                         *         
***********************************************************************         
                                                                                
NEWTAAK  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT2,CPYSTAAK   IF EMPLOYEE AKA NAME ELEMENT DID             
         JO    XIT                 NOT EXIST ON RECORD PREVIOUSLY ...           
                                                                                
         OC    RQW4AKF,RQW4AKF     IF FIRST NAME IS BEING ADDED                 
         JZ    NAK10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4AKF                                   
                                                                                
NAK10    OC    RQW4AKL,RQW4AKL     IF LAST NAME IS BEING ADDED                  
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4AKL                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAAK AND NEWTAAK                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4AKF  DC    AL1(EW4AKFX-*),AL2(15),AL1(ERRCATY2),AL1(D#W4AKF)                
         DC    C'Review update to AKA First Name'                               
EW4AKFX  EQU   *                                                                
                                                                                
ERW4AKL  DC    AL1(EW4AKLX-*),AL2(16),AL1(ERRCATY2),AL1(D#W4AKL)                
         DC    C'Review update to AKA Last Name'                                
EW4AKLX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CAKCT    DC    AL1(TAW4TYCO,TAW4TYTR),X'FF'                                     
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES W4 DETAILS ELEMENT INTO REQUEST MAP           *         
*        ON ENTRY ... R4 = A(EMPLOYEE W4 DETAILS ELEMENT)             *         
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
CPYTAW4  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4TYP,RQW4TYP),TAW4TYPE,    +        
               ERW4TYP,0                                                        
         GOTOR (RF),(R1),(L'RQW4LNM,RQW4LNM),TAW4NAM2,ERW4LNM,         +        
               (0,RQW4TYP),CW4CET                                               
         GOTOR (RF),(R1),(L'RQW4FNM,RQW4FNM),TAW4NAM1,ERW4FNM,         +        
               (0,RQW4TYP),CW4CET                                               
         GOTOR (RF),(R1),(L'RQW4MNM,RQW4MNM),TAW4MIDN,ERW4MNM,         +        
               (0,RQW4TYP),CW4CET                                               
         GOTOR (RF),(R1),(L'RQW4SUF,RQW4SUF),TAW4SUFF,ERW4SUF,         +        
               (0,RQW4TYP),CW4CET                                               
         GOTOR (RF),(R1),(L'RQW4CPN,RQW4CPN),TAW4CRPN,ERW4CPN,         +        
               (0,RQW4TYP),CW4AFI                                               
         GOTOR (RF),(R1),(L'RQW4IDT,RQW4IDT),TAW4INDT,ERW4IDT,         +        
               (X'FF',RQW4TYP),CW4CT                                            
         GOTOR (RF),(R1),(L'RQW4AFM,RQW4AFM),TAW4LOCL,ERW4AFM,0                 
         GOTOR (RF),(R1),(L'RQW4RST,RQW4RST),TAW4RECP,ERW4RST,         +        
               (0,RQW4TYP),CW4AF                                                
         GOTOR (RF),(R1),(L'RQW4SXC,RQW4SXC),TAW4SEX,ERW4SXC,          +        
               (0,RQW4TYP),CW4T                                                 
         GOTOR (RF),(R1),(L'RQW4ETH,RQW4ETH),TAW4RACE,ERW4ETH,         +        
               (0,RQW4TYP),CW4T                                                 
         GOTOR (RF),(R1),(L'RQW4TCP,RQW4TCP),TAW4CP,ERW4TCP,           +        
               (0,RQW4TCP),CW4A                                                 
                                                                                
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),(0,RQW4YTD),                   +        
               ('TAW4STNY',TAW4STAT),ERW4YTD                                    
         GOTOR (RF),(R1),('TAW4SCKF',RQW4CKF),(0,TAW4STAT),ERW4CKF              
         GOTOR (RF),(R1),('TAW4SIRS',RQW4ILF),(0,TAW4STA2),ERW4ILF              
         GOTOR (RF),(R1),('TAW4SLCK',RQW4LCK),(0,TAW4STA2),ERW4LCK              
         GOTOR (RF),(R1),(0,RQW4FIW),('TAW4SNFI',TAW4STA2),ERW4FIW              
         GOTOR (RF),(R1),(0,RQW4DCW),('TAW4SNDU',TAW4STA2),ERW4DCW              
         GOTOR (RF),(R1),(0,RQW4PEN),('TAW4SNPE',TAW4STA2),ERW4PEN              
         GOTOR (RF),(R1),('TAW4SDD',RQW4DIR),(0,TAW4STA2),ERW4DIR               
         GOTOR (RF),(R1),('TAW4SWIR',RQW4WIR),(0,TAW4STA2),ERW4WIR              
         GOTOR (RF),(R1),('TAW4SSPL',RQW4SPL),(0,TAW4STA3),ERW4SPL              
         GOTOR (RF),(R1),('TAW4SEFT',RQW4EFT),(0,TAW4STA3),ERW4EFT              
                                                                                
         TM    TAW4STA3,TAW4SREG                                                
         JZ    *+8                                                              
         MVI   RQW4REG,C'Y'                                                     
         GOTOR (RF),(R1),('TAW4SREG',RQW4REG),(0,TAW4STA3),ERW4REG              
                                                                                
         CLI   RQW4TYP,TAW4TYFO                                                 
         JE    CW400                                                            
         CLI   RQW4TYP,TAW4TYCA                                                 
         JNE   CW410                                                            
CW400    GOTOR (RF),(R1),(0,RQW4FTX),('TAW4SNTX',TAW4STA3),ERW4FTX              
         J     XIT                                                              
                                                                                
CW410    CLI   RQW4TYP,TAW4TYIN                                                 
         JNE   XIT                                                              
         GOTOR (RF),(R1),('TAW4SNHA',RQW4NHA),(0,TAW4STA3),ERW4NHA              
         GOTOR (RF),(R1),('TAW4SNHP',RQW4NHP),(0,TAW4STA3),ERW4NHP              
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4NHD,RQW4NHD),TAW4NHAD,    +        
               ERW4NHD,0                                                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF PASSED ERROR ENTRY IS REVIEW UPDATE    *         
*        TO ONE OF THE CORPORATION FIELDS                             *         
*        ON ENTRY ... 0(R2) = ERROR ENTRY                             *         
***********************************************************************         
                                                                                
CORPCHG  NTR1  BASE=*,LABEL=*                                                   
         CLC   ERW4CP1+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4CP2+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4CP3+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4CP4+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4CP5+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4CP6+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAW4 AND CORPCHG                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4LNM  DC    AL1(EW4LNMX-*),AL2(3),AL1(ERRCATY2),AL1(D#W4LNM)                 
         DC    C'Review update to Last Name'                                    
EW4LNMX  EQU   *                                                                
                                                                                
ERW4FNM  DC    AL1(EW4FNMX-*),AL2(4),AL1(ERRCATY2),AL1(D#W4FNM)                 
         DC    C'Review update to First Name'                                   
EW4FNMX  EQU   *                                                                
                                                                                
ERW4MNM  DC    AL1(EW4MNMX-*),AL2(5),AL1(ERRCATY2),AL1(D#W4MNM)                 
         DC    C'Review update to Middle Name'                                  
EW4MNMX  EQU   *                                                                
                                                                                
ERW4TYP  DC    AL1(EW4TYPX-*),AL2(6),AL1(ERRCATY2),AL1(D#W4TYP)                 
         DC    C'Review update to Type'                                         
EW4TYPX  EQU   *                                                                
                                                                                
ERW4SUF  DC    AL1(EW4SUFX-*),AL2(13),AL1(ERRCATY2),AL1(D#W4SUF)                
         DC    C'Review update to Suffix'                                       
EW4SUFX  EQU   *                                                                
                                                                                
ERW4CPN  DC    AL1(EW4CPNX-*),AL2(17),AL1(ERRCATY2),AL1(D#W4CPN)                
         DC    C'Review update to Corporation Name'                             
EW4CPNX  EQU   *                                                                
                                                                                
ERW4SXC  DC    AL1(EW4SXCX-*),AL2(18),AL1(ERRCATY2),AL1(D#W4SXC)                
         DC    C'Review update to Sex Code'                                     
EW4SXCX  EQU   *                                                                
                                                                                
ERW4ETH  DC    AL1(EW4ETHX-*),AL2(19),AL1(ERRCATY2),AL1(D#W4ETH)                
         DC    C'Review update to Ethnicity'                                    
EW4ETHX  EQU   *                                                                
                                                                                
ERW4IDT  DC    AL1(EW4IDTX-*),AL2(40),AL1(ERRCATY2),AL1(D#W4IDT)                
         DC    C'Review update to Indemnification Date'                         
EW4IDTX  EQU   *                                                                
                                                                                
ERW4AFM  DC    AL1(EW4AFMX-*),AL2(42),AL1(ERRCATY2),AL1(D#W4AFM)                
         DC    C'Review update to AFM Local'                                    
EW4AFMX  EQU   *                                                                
                                                                                
ERW4YTD  DC    AL1(EW4YTDX-*),AL2(44),AL1(ERRCATY2),AL1(D#W4YTD)                
         DC    C'Review update to YTD on Checks'                                
EW4YTDX  EQU   *                                                                
                                                                                
ERW4CKF  DC    AL1(EW4CKFX-*),AL2(45),AL1(ERRCATY2),AL1(D#W4CKF)                
         DC    C'Review update to Sort Checks First'                            
EW4CKFX  EQU   *                                                                
                                                                                
ERW4ILF  DC    AL1(EW4ILFX-*),AL2(46),AL1(ERRCATY2),AL1(D#W4ILF)                
         DC    C'Review update to IRS Lock'                                     
EW4ILFX  EQU   *                                                                
                                                                                
ERW4LCK  DC    AL1(EW4LCKX-*),AL2(47),AL1(ERRCATY2),AL1(D#W4LCK)                
         DC    C'Review update to Locked Status'                                
EW4LCKX  EQU   *                                                                
                                                                                
ERW4FIW  DC    AL1(EW4FIWX-*),AL2(48),AL1(ERRCATY2),AL1(D#W4FIW)                
         DC    C'Review update to FICA Withholding'                             
EW4FIWX  EQU   *                                                                
                                                                                
ERW4DCW  DC    AL1(EW4DCWX-*),AL2(49),AL1(ERRCATY2),AL1(D#W4DCW)                
         DC    C'Review update to Due Company Withholding'                      
EW4DCWX  EQU   *                                                                
                                                                                
ERW4PEN  DC    AL1(EW4PENX-*),AL2(50),AL1(ERRCATY2),AL1(D#W4PEN)                
         DC    C'Review update to Pension'                                      
EW4PENX  EQU   *                                                                
                                                                                
ERW4FAD  DC    AL1(EW4FADX-*),AL2(51),AL1(ERRCATY2),AL1(D#W4FAD)                
         DC    C'Review update to Foreign Address'                              
EW4FADX  EQU   *                                                                
                                                                                
ERW4DIR  DC    AL1(EW4DIRX-*),AL2(52),AL1(ERRCATY2),AL1(D#W4DIR)                
         DC    C'Review update to Direct Deposit'                               
EW4DIRX  EQU   *                                                                
                                                                                
ERW4WIR  DC    AL1(EW4WIRX-*),AL2(53),AL1(ERRCATY2),AL1(D#W4WIR)                
         DC    C'Review update to Wire Transfer Checks'                         
EW4WIRX  EQU   *                                                                
                                                                                
ERW4SPL  DC    AL1(EW4SPLX-*),AL2(54),AL1(ERRCATY2),AL1(D#W4SPL)                
         DC    C'Review update to Special Letter on File'                       
EW4SPLX  EQU   *                                                                
                                                                                
ERW4RST  DC    AL1(EW4RSTX-*),AL2(67),AL1(ERRCATY2),AL1(D#W4RST)                
         DC    C'Review update to Reciprocal State'                             
EW4RSTX  EQU   *                                                                
                                                                                
ERW4FTX  DC    AL1(EW4FTXX-*),AL2(84),AL1(ERRCATY2),AL1(D#W4FTX)                
         DC    C'Review update to Take Taxes for Foreigners'                    
EW4FTXX  EQU   *                                                                
                                                                                
ERW4NHA  DC    AL1(EW4NHAX-*),AL2(90),AL1(ERRCATY2),AL1(D#W4NHA)                
         DC    C'Review update to Eligible for New Hire Act'                    
EW4NHAX  EQU   *                                                                
                                                                                
ERW4NHP  DC    AL1(EW4NHPX-*),AL2(91),AL1(ERRCATY2),AL1(D#W4NHP)                
         DC    C'Review update to New Hire Act Eligibility Pending'             
EW4NHPX  EQU   *                                                                
                                                                                
ERW4NHD  DC    AL1(EW4NHDX-*),AL2(92),AL1(ERRCATY2),AL1(D#W4NHD)                
         DC    C'Review update to New Hire Act Date'                            
EW4NHDX  EQU   *                                                                
                                                                                
ERW4EFT  DC    AL1(EW4EFTX-*),AL2(96),AL1(ERRCATY2),AL1(D#W4EFT)                
         DC    C'Review update to EFT'                                          
EW4EFTX  EQU   *                                                                
                                                                                
ERW4REG  DC    AL1(EW4REGX-*),AL2(97),AL1(ERRCATY2),AL1(D#W4REG)                
         DC    C'Review update to Regression Testing'                           
EW4REGX  EQU   *                                                                
                                                                                
ERW4TCP  DC    AL1(EW4TCPX-*),AL2(98),AL1(ERRCATY2),AL1(D#W4TCP)                
         DC    C'Review update to Canadian Taxable Province'                    
EW4TCPX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CW4A     DC    AL1(TAW4TYCA),X'FF'                                              
CW4AF    DC    AL1(TAW4TYCA,TAW4TYFO),X'FF'                                     
CW4AFI   DC    AL1(TAW4TYCA,TAW4TYFO,TAW4TYIN),X'FF'                            
CW4CET   DC    AL1(TAW4TYCO,TAW4TYES,TAW4TYTR),X'FF'                            
CW4CT    DC    AL1(TAW4TYCO,TAW4TYTR),X'FF'                                     
CW4T     DC    AL1(TAW4TYTR),X'FF'                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES CORPORATION TAX ID ELEEMNT INTO REQUEST MAP   *         
*        ON ENTRY ... R4 = A(TAX ID ELEMENT)                          *         
***********************************************************************         
                                                                                
         USING TATID,R4                                                         
CPYTATI  NTR1  BASE=*,LABEL=*                                                   
         CLI   TATITYPE,TATITYCO   ONLY HANDLE CORPORATION TAX ID               
         JNE   XIT                 ELEMENTS ...                                 
                                                                                
         CLI   TATICRPN,C'1'       IF CORPORATION ID 1                          
         JNE   CTI10                                                            
         LA    R2,RQW4CP1          R2=A(CORPORATION ID 1 FIELD)                 
         LA    R3,ERW4CP1          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI1   AND SET PROCESSED STATUS                     
         J     CTI60                                                            
                                                                                
CTI10    CLI   TATICRPN,C'2'       IF CORPORATION ID 2                          
         JNE   CTI20                                                            
         LA    R2,RQW4CP2          R2=A(CORPORATION ID 2 FIELD)                 
         LA    R3,ERW4CP2          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI2   AND SET PROCESSED STATUS                     
         J     CTI60                                                            
                                                                                
CTI20    CLI   TATICRPN,C'3'       IF CORPORATION ID 3                          
         JNE   CTI30                                                            
         LA    R2,RQW4CP3          R2=A(CORPORATION ID 3 FIELD)                 
         LA    R3,ERW4CP3          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI3   AND SET PROCESSED STATUS                     
         J     CTI60                                                            
                                                                                
CTI30    CLI   TATICRPN,C'4'       IF CORPORATION ID 4                          
         JNE   CTI40                                                            
         LA    R2,RQW4CP4          R2=A(CORPORATION ID 4 FIELD)                 
         LA    R3,ERW4CP4          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI4   AND SET PROCESSED STATUS                     
         J     CTI60                                                            
                                                                                
CTI40    CLI   TATICRPN,C'5'       IF CORPORATION ID 5                          
         JNE   CTI50                                                            
         LA    R2,RQW4CP5          R2=A(CORPORATION ID 5 FIELD)                 
         LA    R3,ERW4CP5          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI5   AND SET PROCESSED STATUS                     
         J     CTI60                                                            
                                                                                
CTI50    CLI   TATICRPN,C'6'       IF CORPORATION ID 6                          
         JNE   XIT                                                              
         LA    R2,RQW4CP6          R2=A(CORPORATION ID 6 FIELD)                 
         LA    R3,ERW4CP6          R3=A(ERROR ENTRY)                            
         OI    CPYSTAT2,CPYTATI6   AND SET PROCESSED STATUS                     
                                                                                
CTI60    GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4CP1,(R2)),TATIID,(R3),    +        
               (0,RQW4TYP),CTICT                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF CORPORATION TAX ID ELEMENT DID NOT EXIST ON RECORD        *         
*        PREVIOUSLY AND CORPORATION TAX IDS ARE BEING ADDED NOW,      *         
*        ROUTINE PROCESSES THEM                                       *         
***********************************************************************         
                                                                                
NEWTATI  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT2,CPYTATI1   IF CORPORATION TAX ID 1 ELEMENT DID          
         JO    NTI10               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP1,RQW4CP1     AND CORPORATION 1 IS BEING ADDED             
         JZ    NTI10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP1                                   
                                                                                
NTI10    TM    CPYSTAT2,CPYTATI2   IF CORPORATION TAX ID 2 ELEMENT DID          
         JO    NTI20               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP2,RQW4CP2     AND CORPORATION 2 IS BEING ADDED             
         JZ    NTI20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP2                                   
                                                                                
NTI20    TM    CPYSTAT2,CPYTATI3   IF CORPORATION TAX ID 3 ELEMENT DID          
         JO    NTI30               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP3,RQW4CP3     AND CORPORATION 3 IS BEING ADDED             
         JZ    NTI30               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP3                                   
                                                                                
NTI30    TM    CPYSTAT2,CPYTATI4   IF CORPORATION TAX ID 4 ELEMENT DID          
         JO    NTI40               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP4,RQW4CP4     AND CORPORATION 4 IS BEING ADDED             
         JZ    NTI40               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP4                                   
                                                                                
NTI40    TM    CPYSTAT2,CPYTATI5   IF CORPORATION TAX ID 5 ELEMENT DID          
         JO    NTI50               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP5,RQW4CP5     AND CORPORATION 5 IS BEING ADDED             
         JZ    NTI50               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP5                                   
                                                                                
NTI50    TM    CPYSTAT2,CPYTATI6   IF CORPORATION TAX ID 6 ELEMENT DID          
         JO    XIT                 NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4CP6,RQW4CP6     AND CORPORATION 6 IS BEING ADDED             
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4CP6                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTATI AND NEWTATI                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4CP1  DC    AL1(EW4CP1X-*),AL2(24),AL1(ERRCATY2),AL1(D#W4CP1)                
         DC    C'Review update to Corporation ID #1'                            
EW4CP1X  EQU   *                                                                
                                                                                
ERW4CP2  DC    AL1(EW4CP2X-*),AL2(27),AL1(ERRCATY2),AL1(D#W4CP2)                
         DC    C'Review update to Corporation ID #2'                            
EW4CP2X  EQU   *                                                                
                                                                                
ERW4CP3  DC    AL1(EW4CP3X-*),AL2(30),AL1(ERRCATY2),AL1(D#W4CP3)                
         DC    C'Review update to Corporation ID #3'                            
EW4CP3X  EQU   *                                                                
                                                                                
ERW4CP4  DC    AL1(EW4CP4X-*),AL2(33),AL1(ERRCATY2),AL1(D#W4CP4)                
         DC    C'Review update to Corporation ID #4'                            
EW4CP4X  EQU   *                                                                
                                                                                
ERW4CP5  DC    AL1(EW4CP5X-*),AL2(36),AL1(ERRCATY2),AL1(D#W4CP5)                
         DC    C'Review update to Corporation ID #5'                            
EW4CP5X  EQU   *                                                                
                                                                                
ERW4CP6  DC    AL1(EW4CP6X-*),AL2(39),AL1(ERRCATY2),AL1(D#W4CP6)                
         DC    C'Review update to Corporation ID #6'                            
EW4CP6X  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CTICT    DC    AL1(TAW4TYCO,TAW4TYTR),X'FF'                                     
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FILTERS ELEMENT INTO REQUEST MAP              *         
*        ON ENTRY ... R4 = A(FILTERS ELEMENT)                         *         
***********************************************************************         
                                                                                
         USING TAFLD,R4                                                         
CPYTAFL  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4FIL,RQW4FIL),TAFLFLT1,    +        
               ERW4FIL,0                                                        
         OI    CPYSTAT2,CPYSTAFL                                                
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF FILTERS ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY        *         
*        AND FILTERS ARE BEING ADDED NOW, ROUTINE PROCESSES THEM      *         
***********************************************************************         
                                                                                
NEWTAFL  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT2,CPYSTAFL   IF FILTERS ELEMENT DID NOT EXIST             
         JO    XIT                 ON RECORD PREVIOUSLY                         
         OC    RQW4FIL,RQW4FIL     AND ARE BEING ADDED                          
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FIL                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAFL AND NEWTAFL                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4FIL  DC    AL1(EW4FILX-*),AL2(20),AL1(ERRCATY2),AL1(D#W4FIL)                
         DC    C'Review update to Filters'                                      
EW4FILX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESSES FREE FORM NAME ELEMENTS                    *         
*        ON ENTRY ... R4 = A(FREE FORM NAME ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
CPYTAFN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAFNTYPE,TAFNTWEB                                                
         JNE   XIT                                                              
         MVI   0(R4),X'FF'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FREE FORM NUMBER ELEMENTS INTO REQUEST MAP    *         
*        ON ENTRY ... R4 = A(FREE FORM NUMBER ELEMENT)                *         
***********************************************************************         
                                                                                
         USING TANUD,R4                                                         
CPYTANU  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         ZIC   RF,TANULEN          COPY EXISTING FREE FORM NUMBER               
         BCTR  RF,0                ELEMENT INTO ELEM                            
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
***********************************************************************         
                                                                                
         CLI   TANUTYPE,TANUTPHN  PROCESS PHONE NUMBER                          
         JNE   CNU10                                                            
         OI    CPYSTAT3,CPYTANUP  SET PROCESSED STATUS                          
         MVI   0(R4),X'FF'        AND SET TO DELETE ELEMENT                     
         DROP  R4                                                               
                                                                                
         USING TANUD,R2                                                         
         OC    TANUMBER(L'RQW4PHO),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4PHO,RQW4PHO),TANUMBER,    +        
               ERW4PHO,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TANUD,R4                                                         
CNU10    CLI   TANUTYPE,TANUTMEM   PROCESS ACTRA MEMBERSHIP NUMBER              
         JNE   CNU20                                                            
         OI    CPYSTAT3,CPYTANUM   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TANUD,R2                                                         
         OC    TANUMBER(L'RQW4MNU),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4MNU,RQW4MNU),TANUMBER,    +        
               ERW4MNU,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TANUD,R4                                                         
CNU20    CLI   TANUTYPE,TANUTGST   PROCESS GST NUMBER                           
         JNE   XIT                                                              
         OI    CPYSTAT3,CPYTANUG   SET PROCESSED STATUS                         
         MVI   0(R4),X'FF'         AND SET TO DELETE ELEMENT                    
         DROP  R4                                                               
                                                                                
         USING TANUD,R2                                                         
         OC    TANUMBER(L'RQW4GST),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4GST,RQW4GST),TANUMBER,    +        
               ERW4GST,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        IF FREE FORM NUMBER ELEMENTS DID NOT EXIST ON RECORD         *         
*        PREVIOUSLY AND NUMBERS ARE BEING ADDED NOW, ROUTINE          *         
*        PROCESSES THEM                                               *         
***********************************************************************         
                                                                                
NEWTANU  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYTANUP   IF PHONE NUMBER ELEMENT DID                  
         JO    NNU10               NOT EXIST ON RECORD PREVIOUSLY               
         OC    RQW4PHO,RQW4PHO     AND IS BEING ADDED NOW                       
         JZ    NNU10               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PHO                                   
                                                                                
NNU10    TM    CPYSTAT3,CPYTANUM   IF ACTRA MEMBERSHIP NUMBER ELEMENT           
         JO    NNU20               DID NOT EXIST ON RECORD PREVIOUSLY           
         OC    RQW4MNU,RQW4MNU     AND IS BEING ADDED NOW                       
         JZ    NNU20               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4MNU                                   
                                                                                
NNU20    TM    CPYSTAT3,CPYTANUG   IF GST NUMBER ELEMENT DID NOT EXIST          
         JO    XIT                 ON RECORD PREVIOUSLY                         
         OC    RQW4GST,RQW4GST     AND IS BEING ADDED NOW                       
         JZ    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4GST                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTANU AND NEWTANU                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4PHO  DC    AL1(EW4PHOX-*),AL2(14),AL1(ERRCATY2),AL1(D#W4PHO)                
         DC    C'Review update to Phone Number'                                 
EW4PHOX  EQU   *                                                                
                                                                                
ERW4MNU  DC    AL1(EW4MNUX-*),AL2(43),AL1(ERRCATY2),AL1(D#W4MNU)                
         DC    C'Review update to ACTRA Membership Number'                      
EW4MNUX  EQU   *                                                                
                                                                                
ERW4GST  DC    AL1(EW4GSTX-*),AL2(75),AL1(ERRCATY2),AL1(D#W4GST)                
         DC    C'Review update to GST Number'                                   
EW4GSTX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES ADDRESS ELEMENT INTO REQUEST MAP              *         
*        ON ENTRY ... R4 = A(ADDRESS ELEMENT)                         *         
***********************************************************************         
                                                                                
         USING TAA2D,R4                                                         
CPYTAA2  NTR1  BASE=*,LABEL=*                                                   
         MVC   ELEM,SPACES                                                      
         ZIC   RE,TAA2LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),TAA2D                                                    
         MVI   0(R4),X'FF'                                                      
         DROP  R4                                                               
                                                                                
         USING TAA2D,R4                                                         
         LA    R4,ELEM                                                          
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4AD1,RQW4AD1),TAA2ADD1,    +        
               ERW4AD1,0                                                        
         GOTOR (RF),(R1),(L'RQW4AD2,RQW4AD2),TAA2ADD2,ERW4AD2,0                 
         GOTOR (RF),(R1),(L'RQW4CTY,RQW4CTY),TAA2CITY,ERW4CTY,0                 
         GOTOR (RF),(R1),(L'RQW4ZIP,RQW4ZIP),TAA2ZIP,ERW4ZIP,0                  
         GOTOR (RF),(R1),(L'RQW4CRY,RQW4CRY),TAA2CTRY,ERW4CRY,0                 
                                                                                
         CLC   RQW4CRY,=C'US'      IF COUNTRY IS US                             
         JNE   CA210               HANDLE ADDRESS LINE 3 AND STATE              
         GOTOR (RF),(R1),(L'RQW4AD3,RQW4AD3),TAA2ADD3,ERW4AD3,0                 
         GOTOR (RF),(R1),(L'RQW4STA,RQW4STA),TAA2ST,ERW4STA,0                   
         J     XIT                                                              
                                                                                
CA210    CLC   RQW4CRY,=C'CA'      IF COUNTRY IS CANADA                         
         JNE   CA220               CLEAR ADDRESS LINE 3                         
         XC    RQW4AD3,RQW4AD3     AND HANDLE PROVINCE                          
         GOTOR (RF),(R1),(L'RQW4STA,RQW4STA),TAA2ST,ERW4STA,0                   
         J     XIT                                                              
                                                                                
CA220    XC    RQW4AD3,RQW4AD3     ELSE CLEAR ADDRESS LINE 3                    
         XC    RQW4STA,RQW4STA     AND STATE                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAA2                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4AD1  DC    AL1(EW4AD1X-*),AL2(7),AL1(ERRCATY2),AL1(D#W4AD1)                 
         DC    C'Review update to Address Line 1'                               
EW4AD1X  EQU   *                                                                
                                                                                
ERW4AD2  DC    AL1(EW4AD2X-*),AL2(8),AL1(ERRCATY2),AL1(D#W4AD2)                 
         DC    C'Review update to Address Line 2'                               
EW4AD2X  EQU   *                                                                
                                                                                
ERW4AD3  DC    AL1(EW4AD3X-*),AL2(9),AL1(ERRCATY2),AL1(D#W4AD3)                 
         DC    C'Review update to Address Line 3'                               
EW4AD3X  EQU   *                                                                
                                                                                
ERW4CTY  DC    AL1(EW4CTYX-*),AL2(10),AL1(ERRCATY2),AL1(D#W4CTY)                
         DC    C'Review update to City'                                         
EW4CTYX  EQU   *                                                                
                                                                                
ERW4STA  DC    AL1(EW4STAX-*),AL2(11),AL1(ERRCATY2),AL1(D#W4STA)                
         DC    C'Review update to State'                                        
EW4STAX  EQU   *                                                                
                                                                                
ERW4ZIP  DC    AL1(EW4ZIPX-*),AL2(12),AL1(ERRCATY2),AL1(D#W4ZIP)                
         DC    C'Review update to Zip Code'                                     
EW4ZIPX  EQU   *                                                                
                                                                                
ERW4CRY  DC    AL1(EW4CRYX-*),AL2(82),AL1(ERRCATY2),AL1(D#W4CRY)                
         DC    C'Review update to Country'                                      
EW4CRYX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES TD1 ELEMENT INTO REQUEST MAP                  *         
*        ON ENTRY ... R4 = A(TD1 ELEMENT)                             *         
***********************************************************************         
                                                                                
         USING TAD1D,R4                                                         
CPYTAD1  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQW4TYP,TAW4TYCA                                                 
         JNE   XIT                                                              
                                                                                
         TM    TAD1STAT,TAD1SCAN                                                
         JZ    CD110                                                            
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4FNC,RQW4FNC),TAD1NCL1,    +        
               ERW4FNC,0                                                        
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),('TAD1SCC1',RQW4FD1),          +        
               (0,TAD1STAT),ERW4FD1                                             
         GOTOR (RF),(R1),('TAD1SEXM',RQW4FET),(0,TAD1STAT),ERW4FET              
         GOTOR (#CHKFLD,ACHKFLD),(R1),(L'RQW4FPZ,RQW4FPZ),TAD1PZON,    +        
               ERW4FPZ,0                                                        
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),('TAD1SECP',RQW4FEC),          +        
               (0,TAD1STAT),ERW4FEC                                             
         OI    CPYSTAT3,CPYSTAD1                                                
         J     XIT                                                              
                                                                                
CD110    GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQW4PNC,RQW4PNC),TAD1NCL1,    +        
               ERW4PNC,0                                                        
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),('TAD1SCC1',RQW4PD1),          +        
               (0,TAD1STAT),ERW4PD1                                             
         GOTOR (RF),(R1),('TAD1SEXM',RQW4PET),(0,TAD1STAT),ERW4PET              
         GOTOR (#CHKFLD,ACHKFLD),(R1),(L'RQW4PPZ,RQW4PPZ),TAD1PZON,    +        
               ERW4PPZ,0                                                        
         GOTOR (RF),(R1),(L'RQW4PHD,RQW4PHD),TAD1HOUS,ERW4PHD,0                 
         GOTOR (RF),(R1),(L'RQW4PSP,RQW4PSP),TAD1SPRT,ERW4PSP,0                 
         GOTOR (#CHKSTAT,ACHKSTAT),(R1),('TAD1SEHC',RQW4PHC),          +        
               (0,TAD1STAT),ERW4PHC                                             
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF TD1 ELEMENT DID NOT EXIST ON RECORD PREVIOUSLY            *         
*        AND TD1 IS BEING ADDED NOW, ROUTINE PROCESSES THEM           *         
***********************************************************************         
                                                                                
NEWTAD1  NTR1  BASE=*,LABEL=*                                                   
         TM    CPYSTAT3,CPYSTAD1   IF TD1 ELEMENT DID NOT EXIST ON              
         JO    XIT                 RECORD PREVIOUSLY ...                        
                                                                                
         OC    RQW4FNC,RQW4FNC     IF CAN FED NET CLAIM IS BEING ADDED          
         JZ    ND110               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FNC                                   
                                                                                
ND110    CLI   RQW4FD1,0           IF CAN FED DEFAULT TO CC1 IS BEING           
         JE    ND110A              ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FD1                                   
                                                                                
ND110A   CLI   RQW4FET,0           IF CAN FED EXEMPT IS BEING ADDED             
         JE    ND110B              RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FET                                   
                                                                                
ND110B   CLI   RQW4FET,0           IF CAN FED PRE ZONE IS BEING ADDED           
         JE    ND120               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FPZ                                   
                                                                                
ND120    CLI   RQW4FEC,0           IF CAN FED EX CPP IS BEING ADDED             
         JE    ND130               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4FEC                                   
                                                                                
ND130    OC    RQW4PNC,RQW4PNC     IF CAN PRV NET CLAIM IS BEING ADDED          
         JZ    ND140               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PNC                                   
                                                                                
ND140    CLI   RQW4PD1,0           IF CAN PRV DEFAULT TO CC1 IS BEING           
         JE    ND140A              ADDED RETURN PROMPT FOR VERIFICATION         
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PD1                                   
                                                                                
ND140A   CLI   RQW4PET,0           IF CAN PRV EXEMPT IS BEING ADDED             
         JE    ND140B              RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PET                                   
                                                                                
ND140B   OC    RQW4PPZ,RQW4PPZ     IF CAN PRV PRE ZONE IS BEING ADDED           
         JZ    ND150               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PPZ                                   
                                                                                
ND150    OC    RQW4PHD,RQW4PHD     IF CAN PRV HOUS DED IS BEING ADDED           
         JZ    ND160               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PHD                                   
                                                                                
ND160    OC    RQW4PSP,RQW4PSP     IF CAN PRV SUP PAY IS BEING ADDED            
         JZ    ND170               RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PSP                                   
                                                                                
ND170    CLI   RQW4PHC,0           IF CAN PRV EX HL CON IS BEING ADDED          
         JE    XIT                 RETURN PROMPT FOR VERIFICATION               
         GOTOR (#ADDERR,AADDERR),DMCB,ERW4PHC                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF PASSED ERROR ENTRY IS REVIEW UPDATE    *         
*        TO ONE OF THE TD1 FIELDS                                     *         
*        ON ENTRY ... 0(R2) = ERROR ENTRY                             *         
***********************************************************************         
                                                                                
TAD1CHG  NTR1  BASE=*,LABEL=*                                                   
         CLC   ERW4FNC+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4FD1+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4FET+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4FPZ+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4FEC+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PNC+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PD1+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PET+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PPZ+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PHD+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PSP+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         CLC   ERW4PHC+EENUMB-ERRENTD(L'EENUMB),0(R2)                           
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTAD1 AND NEWTAD1                        *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERW4FNC  DC    AL1(EW4FNCX-*),AL2(102),AL1(ERRCATY2),AL1(D#W4FNC)               
         DC    C'Review update to Federal Net Claim'                            
EW4FNCX  EQU   *                                                                
                                                                                
ERW4FD1  DC    AL1(EW4FD1X-*),AL2(103),AL1(ERRCATY2),AL1(D#W4FD1)               
         DC    C'Review update to Federal Default to Claim Code 1'              
EW4FD1X  EQU   *                                                                
                                                                                
ERW4FET  DC    AL1(EW4FETX-*),AL2(104),AL1(ERRCATY2),AL1(D#W4FET)               
         DC    C'Review update to Federal Exempt'                               
EW4FETX  EQU   *                                                                
                                                                                
ERW4FPZ  DC    AL1(EW4FPZX-*),AL2(105),AL1(ERRCATY2),AL1(D#W4FPZ)               
         DC    C'Review update to Federal Prescribed Zone'                      
EW4FPZX  EQU   *                                                                
                                                                                
ERW4FEC  DC    AL1(EW4FECX-*),AL2(113),AL1(ERRCATY2),AL1(D#W4FEC)               
         DC    C'Review update to Exempt from CPP'                              
EW4FECX  EQU   *                                                                
                                                                                
ERW4PNC  DC    AL1(EW4PNCX-*),AL2(106),AL1(ERRCATY2),AL1(D#W4PNC)               
         DC    C'Review update to Provincial Net Claim'                         
EW4PNCX  EQU   *                                                                
                                                                                
ERW4PD1  DC    AL1(EW4PD1X-*),AL2(107),AL1(ERRCATY2),AL1(D#W4PD1)               
         DC    C'Review update to Provincial Default to Claim Code 1'           
EW4PD1X  EQU   *                                                                
                                                                                
ERW4PET  DC    AL1(EW4PETX-*),AL2(108),AL1(ERRCATY2),AL1(D#W4PET)               
         DC    C'Review update to Provincial Exempt'                            
EW4PETX  EQU   *                                                                
                                                                                
ERW4PPZ  DC    AL1(EW4PPZX-*),AL2(109),AL1(ERRCATY2),AL1(D#W4PPZ)               
         DC    C'Review update to Provincial Prescribed Zone'                   
EW4PPZX  EQU   *                                                                
                                                                                
ERW4PHD  DC    AL1(EW4PHDX-*),AL2(110),AL1(ERRCATY2),AL1(D#W4PHD)               
         DC    C'Review update to Provincial Housing Deduction'                 
EW4PHDX  EQU   *                                                                
                                                                                
ERW4PSP  DC    AL1(EW4PSPX-*),AL2(111),AL1(ERRCATY2),AL1(D#W4PSP)               
         DC    C'Review update to Provincial Support Payment'                   
EW4PSPX  EQU   *                                                                
                                                                                
ERW4PHC  DC    AL1(EW4PHCX-*),AL2(112),AL1(ERRCATY2),AL1(D#W4PHC)               
         DC    C'Review update to Exempt from Health Contributions'             
EW4PHCX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS W4 RECORD TO FILE                               *         
*        ON ENTRY ... R4=(I/O AREA 3)                                 *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         CLI   RQW4YTD,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQW4YTD,C'Y'        SET DEFAULT "PRINT YTD ON CHECKS?"           
                                                                                
         CLI   RQW4FIW,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQW4FIW,C'Y'        SET DEFAULT "FICA WITHHOLDING?"              
                                                                                
         CLI   RQW4DCW,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQW4DCW,C'Y'        SET DEFAULT "DUE COMP WITHHOLDING?"          
                                                                                
         CLI   RQW4PEN,0           IF NOT PROVIDED                              
         JNE   *+8                                                              
         MVI   RQW4PEN,C'Y'        SET DEFAULT "PENSION?"                       
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
         L     RF,ASVPTRS          AND POINTER BLOCK                            
         XC    0(L'TLDRREC+1,RF),0(RF)                                          
                                                                                
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ      BUILD KEY WITH RECORD CODE                   
         MVC   TLW4SSN,RQW4SSN     SS#/FID#                                     
         MVI   TLW4LEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAW4D,RF                                                         
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM           INITIALIZE W4 DETAILS ELEMENT                
         MVI   TAW4EL,TAW4ELQ      AND ADD IT TO W4 RECORD                      
         MVI   TAW4LEN,TAW4LN2Q                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  RF                                                               
                                                                                
         BRAS  RE,BLDREC           BUILD RECORD AND ADD TO FILE                 
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING W4 RECORD                           *         
*        ON ENTRY ... R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   YES                                                              
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
         BRAS  RE,BLDREC           BUILD RECORD AND PUT TO FILE                 
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD W4 RECORD                                              *         
*        ON ENTRY ... R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,UPDTAW4          UPDATE W4 DETAILS ELEMENT                    
         BAS   RE,ADDTAA2          ADD ADDRESS ELEMENT                          
         BAS   RE,ADDTANU          ADD FREE FORM NUMBER ELEMENTS                
         BAS   RE,ADDTAAK          ADD AKA NAME ELEMENT                         
         BAS   RE,ADDTAFL          ADD FILTERS ELEMENT                          
         BAS   RE,ADDTATI          ADD TAX ID ELEMENT                           
         BAS   RE,ADDTAWH          ADD EMPLOYEE WITHHOLDINGS ELEMENT            
         BAS   RE,UPDTAWX          UPDATE W4 EXTRA DETAILS ELEMENT              
         BAS   RE,ADDTAOW          ADD OTHER WITHHOLDING ELEMENT                
         BAS   RE,ADDTACM          ADD COMMENT/EMAIL ELEMENTS                   
         BAS   RE,ADDTAPE          ADD PAYEE ELEMENT                            
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQW4WID                              
                                                                                
         TM    PROSTAT,PSNPCHG                                                  
         JZ    BR10                                                             
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQW4STF,SVTIME             
                                                                                
BR10     TM    PROSTAT,PSPYCHG                                                  
         JZ    XIT                                                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),(X'24',RQW4STF),  +        
               SVTIME                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        UPDATE W4 RECORD'S EMPLOYEE DETAILS ELEMENT                  *         
*        ON ENTRY ... R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
UPDTAW4  NTR1                                                                   
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            R4=A(EMPLOYEE W4 DETAILS ELEMENT)            
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   TAW4TYPE,RQW4TYP    PUT TYPE                                     
         MVC   TAW4NAM2,RQW4LNM    LAST NAME                                    
         MVC   TAW4NAM1,RQW4FNM    FIRST NAME                                   
         MVC   TAW4MIDN,RQW4MNM    MIDDLE NAME                                  
         MVC   TAW4SUFF,RQW4SUF    SUFFIX                                       
         MVC   TAW4INDT,RQW4IDT    INDEMNIFICATION DATE                         
         MVC   TAW4LOCL,RQW4AFM    AFM LOCAL                                    
         MVC   TAW4RECP,RQW4RST    RECIPROCAL STATE                             
         MVC   TAW4SEX,RQW4SXC     SEX CODE                                     
         MVC   TAW4RACE,RQW4ETH    ETHNICITY INTO ELEMENT                       
         MVC   TAW4NHAD,RQW4NHD    NEW HIRE DATE                                
         MVC   TAW4CP,RQW4TCP      AND TAXABLE CANADIAN PROVINCE                
                                                                                
         OC    RQW4CPN,RQW4CPN     IF CORPORATION NAME IS PRESENT               
         JZ    *+10                                                             
         MVC   TAW4CRPN,RQW4CPN    PUT IT INTO ELEMENT                          
                                                                                
         NI    TAW4STAT,X'FF'-TAW4STNY                                          
         CLI   RQW4YTD,C'Y'                                                     
         JE    *+8                 PUT "PRINT YTD ON CHECKS?"                   
         OI    TAW4STAT,TAW4STNY   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STAT,X'FF'-TAW4SCKF                                          
         CLI   RQW4CKF,C'Y'                                                     
         JNE   *+8                 PUT "SORT CHECKS FIRST?"                     
         OI    TAW4STAT,TAW4SCKF   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SIRS                                          
         CLI   RQW4ILF,C'Y'                                                     
         JNE   *+8                 PUT "IRS LOCK ON FEDERAL                     
         OI    TAW4STA2,TAW4SIRS   WITHHOLDING?" INDICATOR INTO ELEMENT         
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SLCK                                          
         CLI   RQW4LCK,C'Y'                                                     
         JNE   *+8                 PUT "W4 LOCKED?"                             
         OI    TAW4STA2,TAW4SLCK   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SNFI                                          
         CLI   RQW4FIW,C'Y'                                                     
         JE    *+8                 PUT "FICA WITHHOLDING?"                      
         OI    TAW4STA2,TAW4SNFI   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SNDU                                          
         CLI   RQW4DCW,C'Y'                                                     
         JE    *+8                 PUT "DUE COMPANY WITHHOLDING?"               
         OI    TAW4STA2,TAW4SNDU   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SNPE                                          
         CLI   RQW4PEN,C'Y'                                                     
         JE    *+8                 PUT "PENSION?"                               
         OI    TAW4STA2,TAW4SNPE   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SFGN                                          
         CLC   RQW4CRY,=C'US'                                                   
         JE    *+8                 PUT "FOREIGN ADDRESS?"                       
         OI    TAW4STA2,TAW4SFGN   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SDD                                           
         CLI   RQW4DIR,C'Y'                                                     
         JNE   *+8                 PUT "DIRECT DEPOSIT?"                        
         OI    TAW4STA2,TAW4SDD    INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SWIR                                          
         CLI   RQW4WIR,C'Y'                                                     
         JNE   *+8                 PUT "WIRE TRANSFER CHECKS?"                  
         OI    TAW4STA2,TAW4SWIR   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SSPL                                          
         CLI   RQW4SPL,C'Y'                                                     
         JNE   *+8                 PUT "SPECIAL LETTER ON FILE?"                
         OI    TAW4STA3,TAW4SSPL   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SNTX                                          
         CLI   RQW4FTX,C'N'                                                     
         JNE   *+8                 PUT "TAKE TAXES FOR FOREIGNERS?"             
         OI    TAW4STA3,TAW4SNTX   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SNHA                                          
         CLI   RQW4NHA,C'Y'                                                     
         JNE   *+8                 PUT "ELIGIBLE FOR NEW HIRE ACT?"             
         OI    TAW4STA3,TAW4SNHA   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SNHP                                          
         CLI   RQW4NHP,C'Y'                                                     
         JNE   *+8                 PUT "NEW HIRE ACT ELIGIBILITY                
         OI    TAW4STA3,TAW4SNHP   PENDING?" INDICATOR INTO ELEMENT             
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SEFT                                          
         CLI   RQW4EFT,C'Y'                                                     
         JNE   *+8                 PUT "EFT?"                                   
         OI    TAW4STA3,TAW4SEFT   INDICATOR INTO ELEMENT                       
                                                                                
         NI    TAW4STA3,X'FF'-TAW4SREG                                          
         CLI   RQW4REG,C'Y'                                                     
         JNE   XIT                 PUT "REGRESSION TESTING?"                    
         OI    TAW4STA3,TAW4SREG   INDICATOR INTO ELEMENT                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ADD W4 RECORD'S ADDRESS ELEMENT                              *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAA2D,R2                                                         
ADDTAA2  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVI   TAA2EL,TAA2ELQ                                                   
         MVI   TAA2LEN,TAA2LNQ                                                  
         MVI   TAA2EL,TAA2ELQ                                                   
         MVI   TAA2LEN,TAA2LNQ                                                  
         MVC   TAA2ADD1,RQW4AD1    PUT ADDRESS LINE 1                           
         MVC   TAA2ADD2,RQW4AD2    ADDRESS LINE 2                               
         MVC   TAA2ADD3,RQW4AD3    ADDRESS LINE 3                               
         MVC   TAA2CITY,RQW4CTY    CITY                                         
         MVC   TAA2ST,RQW4STA      STATE                                        
         MVC   TAA2ZIP,RQW4ZIP     ZIP CODE                                     
         MVC   TAA2CTRY,RQW4CRY    AND COUNTRY INTO ELEMENT                     
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD FREE FORM NUMBER ELEMENTS TO W4 RECORD                   *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TANUD,R2                                                         
ADDTANU  NTR1                                                                   
         OC    RQW4PHO,RQW4PHO     IF CORPORATION NAME IS PRESENT               
         JZ    ANU10                                                            
         XC    ELEM,ELEM           INITIALIZE NUMBER ELEMENT                    
         MVI   TANUEL,TANUELQ      PUT PHONE NUMBER INTO ELEMENT                
         MVI   TANULEN,TANULNQ     AND ADD IT                                   
         MVI   TANUTYPE,TANUTPHN                                                
         MVC   TANUMBER(L'RQW4PHO),RQW4PHO                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
ANU10    CLI   ACTION,ACTADD       IF ACTION IS ADD                             
         JNE   ANU20                                                            
         XC    ELEM,ELEM           INITIALIZE NUMBER ELEMENT                    
         MVI   TANUEL,TANUELQ      PUT PID INTO ELEMENT                         
         MVI   TANULEN,TANULNQ     AND ADD IT                                   
         MVI   TANUTYPE,TANUPIDN                                                
         MVC   TANUMBER(L'RQW4PID),RQW4PID                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
ANU20    OC    RQW4MNU,RQW4MNU     IF ACTRA MEMBERSHIP NUMBER IS                
         JZ    ANU30               PRESENT                                      
         XC    ELEM,ELEM           INITIALIZE NUMBER ELEMENT                    
         MVI   TANUEL,TANUELQ      PUT MEMBERSHIP NUMBER INTO ELEMENT           
         MVI   TANULEN,TANULNQ     AND ADD IT                                   
         MVI   TANUTYPE,TANUTMEM                                                
         MVC   TANUMBER(L'RQW4MNU),RQW4MNU                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
ANU30    OC    RQW4GST,RQW4GST     IF GST NUMBER IS PRESENT                     
         JZ    XIT                 PRESENT                                      
         XC    ELEM,ELEM           INITIALIZE NUMBER ELEMENT                    
         MVI   TANUEL,TANUELQ      PUT GST NUMBER INTO ELEMENT                  
         MVI   TANULEN,TANULNQ     AND ADD IT                                   
         MVI   TANUTYPE,TANUTGST                                                
         MVC   TANUMBER(L'RQW4GST),RQW4GST                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD EMPLOYEE AKA NAME ELEMENT TO W4 RECORD                   *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAAKD,R2                                                         
ADDTAAK  NTR1                                                                   
         OC    RQW4AKF(RQW4ALNQ),RQW4AKF                                        
         JZ    XIT                 IF AKA NAMES ARE PRESENT                     
         XC    ELEM,ELEM                                                        
         MVI   TAAKEL,TAAKELQ                                                   
         MVI   TAAKLEN,TAAKLNQ     INITIALIZE AKA NAME ELEMENT                  
         MVC   TAAKNAM2,RQW4AKL    PUT AKA LAST NAME                            
         MVC   TAAKNAM1,RQW4AKF    AND AKA FIRST NAME INTO ELEMENT              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD FILTERS ELEMENT TO W4 RECORD                             *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAFLD,R2                                                         
ADDTAFL  NTR1                                                                   
         OC    RQW4FIL,RQW4FIL     IF FILTERS ARE PRESENT                       
         JZ    XIT                                                              
         XC    ELEM,ELEM                                                        
         MVI   TAFLEL,TAFLELQ      INITIALIZE FILTERS ELEMENT                   
         MVI   TAFLLEN,TAFLLNQ     PUT FILTERS INTO ELEMENT                     
         MVC   TAFLFLT1(L'RQW4FIL),RQW4FIL                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD TAX ID ELEMENTS TO W4 RECORD                             *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TATID,R2                                                         
ADDTATI  NTR1                                                                   
         LA    R6,RQW4CP1           R6=A(FIRST CORPORATION ID)                  
         LHI   R3,1                 R3=CORPORATION NUMBER                       
                                                                                
ATI10    OC    0(L'RQW4CP1,R6),0(R6) IF CORPORATION ID IS PRESENT               
         JZ    ATI20                                                            
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   TATIEL,TATIELQ       INITIALIZE TAX ID ELEMENT                   
         MVI   TATILEN,TATILNQ      PUT CORPORATION ID                          
         MVI   TATITYPE,TATITYCO    AND CORPORATION NUMBER INTO ELEMENT         
         EDIT  (R3),TATICRPN,ALIGN=LEFT                                         
         MVC   TATIID(L'RQW4CP1),0(R6)  AND ADD IT                              
         OC    TATIID,SPACES                                                    
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
ATI20    LA    R6,L'RQW4CP1(R6)     BUMP TO NEXT CORPORATION ID                 
         AHI   R3,1                                                             
         CHI   R3,6                                                             
         JNH   ATI10                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ADD EMPLOYEE WITHHOLDINGS DETAILS ELEMENTS TO W4 RECORD      *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAWHD,R2                                                         
ADDTAWH  NTR1                                                                   
         OC    RQW4FMS(RQW4FLNQ),RQW4FMS IF FEDERAL TAX INFORMATION             
         JZ    AWH10                     IS PRESENT ...                         
         XC    ELEM,ELEM                                                        
         MVI   TAWHEL,TAWHELQ                                                   
         MVI   TAWHLEN,TAWHLNQ           INITIALIZE TAX ID ELEMENT              
         MVC   TAWHEMP,=C'TP '           PUT EMPLOYER                           
         MVC   TAWHUNIT,=C'FD '          UNIT                                   
         MVC   TAWHSTAT,RQW4FMS          MARRIED/SINGLE STATUS                  
         MVC   TAWHEXS,RQW4FEX           EXEMPTIONS                             
         MVC   TAWHFLAT,RQW4FFX          AND FIXED % INTO ELEMENT               
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AWH10    OC    RQW4SAR(RQW4SLNQ),RQW4SAR IF STATE TAX INFORMATION               
         JZ    AWH20                     IS PRESENT ...                         
         XC    ELEM,ELEM                                                        
         MVI   TAWHEL,TAWHELQ                                                   
         MVI   TAWHLEN,TAWHLNQ           INITIALIZE TAX ID ELEMENT              
         MVC   TAWHEMP,=C'TP '           PUT EMPLOYER                           
         MVC   TAWHUNIT,RQW4SAR          UNIT                                   
         MVC   TAWHSTAT,RQW4SMS          MARRIED/SINGLE STATUS                  
         MVC   TAWHEXS,RQW4SEX           EXEMPTIONS                             
         MVC   TAWHFLAT,RQW4SFX          AND FIXED % INTO ELEMENT               
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AWH20    OC    RQW4CAR(RQW4CLNQ),RQW4CAR IF CITY TAX INFORMATION                
         JZ    XIT                       IS PRESENT ...                         
         XC    ELEM,ELEM                                                        
         MVI   TAWHEL,TAWHELQ                                                   
         MVI   TAWHLEN,TAWHLNQ           INITIALIZE TAX ID ELEMENT              
         MVC   TAWHEMP,=C'TP '           PUT EMPLOYER                           
         MVC   TAWHUNIT,RQW4CAR          UNIT                                   
         MVC   TAWHSTAT,RQW4CMS          MARRIED/SINGLE STATUS                  
         MVC   TAWHEXS,RQW4CEX           EXEMPTIONS                             
         MVC   TAWHFLAT,RQW4CFX          AND FIXED % INTO ELEMENT               
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        UPDATE W4 RECORD'S EXTRA DETAILS ELEMENT                     *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
UPDTAWX  NTR1                                                                   
         MVI   ELCODE,TAWXELQ      ATTEMPT TO GET W4 EXTRA DETAILS              
         BRAS  RE,GETEL            ELEMENT                                      
         JNE   UWX10                                                            
                                                                                
         USING TAWXD,R4                                                         
         MVC   TAWXDOB,RQW4DOB     IF FOUND, PUT DATE OF BIRTH                  
         MVC   TAWXPCT,RQW4DED     DEDUCTION PERCENTAGE                         
         MVC   TAWXTSSN,RQW4TRS    TRUSTEE SS#                                  
         MVC   TAWXMERN,RQW4ERN    AND EARNINGS INTO EXISTING ELEMENT           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAWXD,R2                                                         
UWX10    OC    RQW4ERN(RQW4XLNQ),RQW4ERN                                        
         JZ    XIT                 ELSE,IF EXTRA DETAILS ARE PRESENT            
         L     R4,AIO3             PREPARE TO ADD ELEMENT NOW                   
         XC    ELEM,ELEM                                                        
         MVI   TAWXEL,TAWXELQ                                                   
         MVI   TAWXLEN,TAWXLNQ                                                  
         MVC   TAWXDOB,RQW4DOB     PUT DATE OF BIRTH                            
         MVC   TAWXPCT,RQW4DED     DEDUCTION PERCENTAGE                         
         MVC   TAWXTSSN,RQW4TRS    TRUSTEE SS#                                  
         MVC   TAWXMERN,RQW4ERN    AND EARNINGS INTO ELEMENT                    
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD OTHER WITHHOLDING ELEMENT TO W4 RECORD                   *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAOWD,R2                                                         
ADDTAOW  NTR1                                                                   
         OC    RQW4MPR,RQW4MPR     IF MPR FUND % IS PRESENT                     
         JZ    AOW10                                                            
         XC    ELEM,ELEM                                                        
         MVI   TAOWEL,TAOWELQ      INITIALIZE OTHER WITHHOLDINGS                
         MVI   TAOWLEN,TAOWLNQ     ELEMENT                                      
         MVI   TAOWTYPE,TAOWTMPF   PUT MPR FUND INTO ELEMENT                    
         MVC   TAOWFLAT,RQW4MPR    ADD ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
AOW10    OC    RQW4PCH,RQW4PCH     IF PERMANENT CHARITY % IS PRESENT            
         JZ    XIT                                                              
         XC    ELEM,ELEM                                                        
         MVI   TAOWEL,TAOWELQ      INITIALIZE OTHER WITHHOLDINGS                
         MVI   TAOWLEN,TAOWLNQ     ELEMENT                                      
         MVI   TAOWTYPE,TAOWTCHA   PUT PERMANENT CHARITY INTO ELEMENT           
         MVC   TAOWFLAT,RQW4PCH    AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD COMMENT ELEMENT TO W4 RECORD                             *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R3=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TACMD,R2                                                         
ADDTACM  NTR1                                                                   
         OC    RQW4CMT,RQW4CMT     IF COMMENT IS PRESENT                        
         JZ    ACM10                                                            
         XC    ELEM,ELEM                                                        
         MVI   TACMEL,TACMELQ      INITIALIZE COMMENT ELEMENT                   
         MVI   TACMLEN,TACMLNQ     PUT COMMENT INTO ELEMENT                     
         MVI   TACMTYPE,TACMTYPG   AND ADD IT                                   
         MVC   TACMCOMM(L'RQW4CMT),RQW4CMT                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
                                                                                
ACM10    OC    RQW4EML,RQW4EML     IF EMAIL ADDRESS IS PRESENT                  
         JZ    XIT                                                              
         XC    ELEM,ELEM                                                        
         MVI   TACMEL,TACMELQ      INITIALIZE EMAIL ELEMENT                     
         MVI   TACMLEN,TACMLNQ     PUT EMAIL INTO ELEMENT                       
         MVI   TACMTYPE,TACMTYPI   AND ADD IT                                   
         MVC   TACMCOMM(L'RQW4EML),RQW4EML                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ADD PAYEE ELEMENT TO W4 RECORD                               *         
*        ON ENTRY ... R2=A(ELEMENT)                                   *         
*                     R4=A(W4 RECORD)                                 *         
***********************************************************************         
                                                                                
         USING TAPED,R2                                                         
ADDTAPE  NTR1                                                                   
         OC    RQW4PNM(RQW4PLNQ),RQW4PNM                                        
         JZ    XIT                                                              
         XC    ELEM,ELEM                                                        
         MVI   TAPEEL,TAPEELQ      INITIALIZE PAYEE ELEMENT                     
         MVI   TAPELEN,TAPELNQ                                                  
         MVC   TAPENAME,RQW4PNM    PUT PAYEE NAME                               
         MVC   TAPEADD1,RQW4PA1    PAYEE ADDRESS LINE 1                         
         MVC   TAPEADD2,RQW4PA2    PAYEE ADDRESS LINE 2                         
         MVC   TAPEADD3,RQW4PA3    PAYEE ADDRESS LINE 3                         
         MVC   TAPECITY,RQW4PCY    PAYEE CITY                                   
         MVC   TAPEST,RQW4PST      PAYEE STATE                                  
         MVC   TAPEZIP,RQW4PZP     PAYEE ZIP CODE                               
         MVC   TAPECTRY,RQW4PCT    PAYEE COUNTRY                                
         MVC   TAPEACT,RQW4PAC     PAYEE ACTIVE DATE                            
         MVC   TAPEEXP,RQW4PEX     AND PAYEE EXP. DATE INTO ELEMENT             
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
         XC    0(255,R4),0(R4)     INITIALIZE WEB TRANSACTION RECORD            
         MVI   TLWTCD,TLWTCDQ                                                   
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   SET WEB APPLICATION                          
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTACW4   INDICATE IF CHANGING                         
         CLI   ACTION,ACTCHA                                                    
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTAAW4   OR ADDING W4                                 
                                                                                
AWTR10   MVC   TLWTWBID,RQW4WID    SET WEB APPLICATION ID                       
         MVC   TLWTUNIQ,SPACES     AND UNIQUE IDENTIFIER                        
         MVC   TLWTUPID,RQW4PID                                                 
                                                                                
         MVI   TLWTLEN+1,41        SET RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           BUILD WEB TRANSACTION ELEMENT                
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWTLNQ                                                  
         MVC   TAWTSTAF,RQW4STF                                                 
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
*        ROUTINE UPDATES NAME IN P+ EMPLOYEE AND TIMESHEET RECORDS    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
UPDETNAM NTR1  BASE=*,LABEL=*                                                   
         TM    PROSTAT,PSNMCHG     IF NAME CHANGED                              
         JZ    XIT                                                              
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   READ ALL CAST RECORDS FOR THIS               
         MVI   TLCAPCD,TLCACCDQ    W4                                           
         MVC   TLCACSSN,RQW4SSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     UETN20                                                           
UETN10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
UETN20   CLC   IOKEY(TLCACCOM-TLCAPCD),IOKEYSAV                                 
         JNE   XIT                                                              
         BAS   RE,UPDNAM           AND UPDATE THE NAME                          
         J     UETN10                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE UPDATES NAME IN P+ EMPLOYEE AND TIMESHEET RECORDS    *         
*        ON ENTRY ... R3 = A(IOKEY OF EMPLOYEE/TIMESHEET RECORD)      *         
***********************************************************************         
                                                                                
UPDNAM   NTR1                                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         USING TANMD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TANMELQ      IF RECORD HAS NAME ELEMENT                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
         MVC   SVNMKEY,IOKEY                                                    
                                                                                
         LA    RE,RQW4LNM                                                       
         OC    RQW4LNM,RQW4LNM     UPDATE IT                                    
         JNZ   *+8                                                              
         LA    RE,RQW4CPN                                                       
         MVC   TANMCRPN,0(RE)                                                   
         DROP  R4                                                               
                                                                                
         GOTOR (#PUTREC,APUTREC),'IO3'                                          
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
                                                                                
         MVC   IOKEY,SVNMKEY       AND RESTORE READ SEQUENCE                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
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
* REQUEST MAP - W4 MAINTENANCE UPLOAD                                 *         
***********************************************************************         
                                                                                
W4HDR    LKMAP H,I#W4ULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#W4MOD,UBIN,TA#PMODE,OLEN=L'RQW4MOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQW4MOD)                                       
F$STF    LKMAP F,D#W4STF,CHAR,TA#STAFF,MAXLEN=L'RQW4STF,               +        
               OUTPUT=(D,B#SAVED,RQW4STF)                                       
F$SSN    LKMAP F,D#W4SSN,CHAR,TA#SSN,MAXLEN=L'RQW4SSN,                 +        
               OUTPUT=(D,B#SAVED,RQW4SSN)                                       
F$PID    LKMAP F,D#W4PID,CHAR,TA#PIDNM,MAXLEN=L'RQW4PID,               +        
               OUTPUT=(D,B#SAVED,RQW4PID)                                       
F$LNM    LKMAP F,D#W4LNM,CHAR,TA#LNAME,MAXLEN=L'RQW4LNM,               +        
               OUTPUT=(D,B#SAVED,RQW4LNM)                                       
F$FNM    LKMAP F,D#W4FNM,CHAR,TA#FNAME,MAXLEN=L'RQW4FNM,               +        
               OUTPUT=(D,B#SAVED,RQW4FNM)                                       
F$MNM    LKMAP F,D#W4MNM,CHAR,TA#MNAME,MAXLEN=L'RQW4MNM,               +        
               OUTPUT=(D,B#SAVED,RQW4MNM)                                       
F$TYP    LKMAP F,D#W4TYP,CHAR,TA#W4TYP,MAXLEN=L'RQW4TYP,               +        
               OUTPUT=(D,B#SAVED,RQW4TYP)                                       
F$AD1    LKMAP F,D#W4AD1,CHAR,TA#ADDR1,MAXLEN=L'RQW4AD1,               +        
               OUTPUT=(D,B#SAVED,RQW4AD1)                                       
F$AD2    LKMAP F,D#W4AD2,CHAR,TA#ADDR2,MAXLEN=L'RQW4AD2,               +        
               OUTPUT=(D,B#SAVED,RQW4AD2)                                       
F$AD3    LKMAP F,D#W4AD3,CHAR,TA#ADDR3,MAXLEN=L'RQW4AD3,               +        
               OUTPUT=(D,B#SAVED,RQW4AD3)                                       
F$CTY    LKMAP F,D#W4CTY,CHAR,TA#CITY,MAXLEN=L'RQW4CTY,                +        
               OUTPUT=(D,B#SAVED,RQW4CTY)                                       
F$STA    LKMAP F,D#W4STA,CHAR,TA#STATE,MAXLEN=L'RQW4STA,               +        
               OUTPUT=(D,B#SAVED,RQW4STA)                                       
F$ZIP    LKMAP F,D#W4ZIP,CHAR,TA#ZIP,MAXLEN=L'RQW4ZIP,                 +        
               OUTPUT=(D,B#SAVED,RQW4ZIP)                                       
F$SUF    LKMAP F,D#W4SUF,CHAR,TA#SUFFX,MAXLEN=L'RQW4SUF,               +        
               OUTPUT=(D,B#SAVED,RQW4SUF)                                       
F$PHO    LKMAP F,D#W4PHO,CHAR,TA#PHONE,MAXLEN=L'RQW4PHO,               +        
               OUTPUT=(D,B#SAVED,RQW4PHO)                                       
F$AKF    LKMAP F,D#W4AKF,CHAR,TA#AKAFN,MAXLEN=L'RQW4AKF,               +        
               OUTPUT=(D,B#SAVED,RQW4AKF)                                       
F$AKL    LKMAP F,D#W4AKL,CHAR,TA#AKALN,MAXLEN=L'RQW4AKL,               +        
               OUTPUT=(D,B#SAVED,RQW4AKL)                                       
F$CPN    LKMAP F,D#W4CPN,CHAR,TA#CRPNM,MAXLEN=L'RQW4CPN,               +        
               OUTPUT=(D,B#SAVED,RQW4CPN)                                       
F$SEX    LKMAP F,D#W4SXC,CHAR,TA#SEXCD,MAXLEN=L'RQW4SXC,               +        
               OUTPUT=(D,B#SAVED,RQW4SXC)                                       
F$ETH    LKMAP F,D#W4ETH,CHAR,TA#ETHNC,MAXLEN=L'RQW4ETH,               +        
               OUTPUT=(D,B#SAVED,RQW4ETH)                                       
F$FLT    LKMAP F,D#W4FIL,CHAR,TA#FILTR,MAXLEN=L'RQW4FIL,               +        
               OUTPUT=(D,B#SAVED,RQW4FIL)                                       
F$CP1    LKMAP F,D#W4CP1,CHAR,TA#CRPID,MAXLEN=L'RQW4CP1,               +        
               OUTPUT=(D,B#SAVED,RQW4CP1)                                       
F$CP2    LKMAP F,D#W4CP2,CHAR,TA#CRPI2,MAXLEN=L'RQW4CP2,               +        
               OUTPUT=(D,B#SAVED,RQW4CP2)                                       
F$CP3    LKMAP F,D#W4CP3,CHAR,TA#CRPI3,MAXLEN=L'RQW4CP3,               +        
               OUTPUT=(D,B#SAVED,RQW4CP3)                                       
F$CP4    LKMAP F,D#W4CP4,CHAR,TA#CRPI4,MAXLEN=L'RQW4CP4,               +        
               OUTPUT=(D,B#SAVED,RQW4CP4)                                       
F$CP5    LKMAP F,D#W4CP5,CHAR,TA#CRPI5,MAXLEN=L'RQW4CP5,               +        
               OUTPUT=(D,B#SAVED,RQW4CP5)                                       
F$CP6    LKMAP F,D#W4CP6,CHAR,TA#CRPI6,MAXLEN=L'RQW4CP6,               +        
               OUTPUT=(D,B#SAVED,RQW4CP6)                                       
F$IDT    LKMAP F,D#W4IDT,PDAT,TA#INDEM,OUTPUT=(D,B#SAVED,RQW4IDT)               
F$ALC    LKMAP F,D#W4AFM,CHAR,TA#AFMLC,MAXLEN=L'RQW4AFM,               +        
               OUTPUT=(D,B#SAVED,RQW4AFM)                                       
F$MNU    LKMAP F,D#W4MNU,CHAR,TA#MEMNM,MAXLEN=L'RQW4MNU,               +        
               OUTPUT=(D,B#SAVED,RQW4MNU)                                       
F$YTD    LKMAP F,D#W4YTD,CHAR,TA#YTDCK,MAXLEN=L'RQW4YTD,               +        
               OUTPUT=(D,B#SAVED,RQW4YTD)                                       
F$CKF    LKMAP F,D#W4CKF,CHAR,TA#STCK1,MAXLEN=L'RQW4CKF,               +        
               OUTPUT=(D,B#SAVED,RQW4CKF)                                       
F$ILF    LKMAP F,D#W4ILF,CHAR,TA#ILCKF,MAXLEN=L'RQW4ILF,               +        
               OUTPUT=(D,B#SAVED,RQW4ILF)                                       
F$LCK    LKMAP F,D#W4LCK,CHAR,TA#LOCKD,MAXLEN=L'RQW4LCK,               +        
               OUTPUT=(D,B#SAVED,RQW4LCK)                                       
F$FIW    LKMAP F,D#W4FIW,CHAR,TA#FICAW,MAXLEN=L'RQW4FIW,               +        
               OUTPUT=(D,B#SAVED,RQW4FIW)                                       
F$DCW    LKMAP F,D#W4DCW,CHAR,TA#DUECW,MAXLEN=L'RQW4DCW,               +        
               OUTPUT=(D,B#SAVED,RQW4DCW)                                       
F$PEN    LKMAP F,D#W4PEN,CHAR,TA#PENSN,MAXLEN=L'RQW4PEN,               +        
               OUTPUT=(D,B#SAVED,RQW4PEN)                                       
F$DIR    LKMAP F,D#W4DIR,CHAR,TA#DIRCT,MAXLEN=L'RQW4DIR,               +        
               OUTPUT=(D,B#SAVED,RQW4DIR)                                       
F$WIR    LKMAP F,D#W4WIR,CHAR,TA#WIRET,MAXLEN=L'RQW4WIR,               +        
               OUTPUT=(D,B#SAVED,RQW4WIR)                                       
F$SPL    LKMAP F,D#W4SPL,CHAR,TA#SPLET,MAXLEN=L'RQW4SPL,               +        
               OUTPUT=(D,B#SAVED,RQW4SPL)                                       
F$FMS    LKMAP F,D#W4FMS,CHAR,TA#FTMS,MAXLEN=L'RQW4FMS,                +        
               OUTPUT=(D,B#SAVED,RQW4FMS)                                       
F$FEX    LKMAP F,D#W4FEX,UBIN,TA#FEXM,MAXLEN=3,OLEN=L'RQW4FEX,         +        
               OUTPUT=(D,B#SAVED,RQW4FEX)                                       
F$FFX    LKMAP F,D#W4FFX,UBIN,TA#FFIX,MAXLEN=4,OLEN=L'RQW4FFX,         +        
               OUTPUT=(D,B#SAVED,RQW4FFX)                                       
F$SAR    LKMAP F,D#W4SAR,CHAR,TA#SAREA,MAXLEN=L'RQW4SAR,               +        
               OUTPUT=(D,B#SAVED,RQW4SAR)                                       
F$SMS    LKMAP F,D#W4SMS,CHAR,TA#STMS,MAXLEN=L'RQW4SMS,                +        
               OUTPUT=(D,B#SAVED,RQW4SMS)                                       
F$SEX    LKMAP F,D#W4SEX,UBIN,TA#STEXM,MAXLEN=3,OLEN=L'RQW4SEX,        +        
               OUTPUT=(D,B#SAVED,RQW4SEX)                                       
F$SFX    LKMAP F,D#W4SFX,UBIN,TA#SFIX,MAXLEN=4,OLEN=L'RQW4SFX,         +        
               OUTPUT=(D,B#SAVED,RQW4SFX)                                       
F$CAR    LKMAP F,D#W4CAR,CHAR,TA#CAREA,MAXLEN=L'RQW4CAR,               +        
               OUTPUT=(D,B#SAVED,RQW4CAR)                                       
F$CMS    LKMAP F,D#W4CMS,CHAR,TA#CTMS,MAXLEN=L'RQW4CMS,                +        
               OUTPUT=(D,B#SAVED,RQW4CMS)                                       
F$CEX    LKMAP F,D#W4CEX,UBIN,TA#CTEXM,MAXLEN=3,OLEN=L'RQW4CEX,        +        
               OUTPUT=(D,B#SAVED,RQW4CEX)                                       
F$CFX    LKMAP F,D#W4CFX,UBIN,TA#CFIX,MAXLEN=4,OLEN=L'RQW4CFX,         +        
               OUTPUT=(D,B#SAVED,RQW4CFX)                                       
F$ERN    LKMAP F,D#W4ERN,UBIN,TA#EARNS,MAXLEN=10,OLEN=L'RQW4ERN,       +        
               OUTPUT=(D,B#SAVED,RQW4ERN)                                       
F$RST    LKMAP F,D#W4RST,CHAR,TA#RECST,MAXLEN=L'RQW4RST,               +        
               OUTPUT=(D,B#SAVED,RQW4RST)                                       
F$DOB    LKMAP F,D#W4DOB,PDAT,TA#DOBRT,OUTPUT=(D,B#SAVED,RQW4DOB)               
F$DED    LKMAP F,D#W4DED,UBIN,TA#DEDPC,MAXLEN=4,OLEN=L'RQW4DED,        +        
               OUTPUT=(D,B#SAVED,RQW4DED)                                       
F$TRS    LKMAP F,D#W4TRS,CHAR,TA#TRPID,MAXLEN=L'RQW4TRS,               +        
               OUTPUT=(D,B#SAVED,RQW4TRS)                                       
F$MPR    LKMAP F,D#W4MPR,UBIN,TA#MPRFN,MAXLEN=4,OLEN=L'RQW4MPR,        +        
               OUTPUT=(D,B#SAVED,RQW4MPR)                                       
F$PCH    LKMAP F,D#W4PCH,UBIN,TA#PCHAR,MAXLEN=4,OLEN=L'RQW4PCH,        +        
               OUTPUT=(D,B#SAVED,RQW4PCH)                                       
F$GST    LKMAP F,D#W4GST,CHAR,TA#GSTNM,MAXLEN=L'RQW4GST,               +        
               OUTPUT=(D,B#SAVED,RQW4GST)                                       
F$CMT    LKMAP F,D#W4CMT,CHAR,TA#COMNT,MAXLEN=L'RQW4CMT,               +        
               OUTPUT=(D,B#SAVED,RQW4CMT)                                       
F$PNM    LKMAP F,D#W4PNM,CHAR,TA#PAYNM,MAXLEN=L'RQW4PNM,               +        
               OUTPUT=(D,B#SAVED,RQW4PNM)                                       
F$PA1    LKMAP F,D#W4PA1,CHAR,TA#PAYA1,MAXLEN=L'RQW4PA1,               +        
               OUTPUT=(D,B#SAVED,RQW4PA1)                                       
F$PA2    LKMAP F,D#W4PA2,CHAR,TA#PAYA2,MAXLEN=L'RQW4PA2,               +        
               OUTPUT=(D,B#SAVED,RQW4PA2)                                       
F$PA3    LKMAP F,D#W4PA3,CHAR,TA#PAYA3,MAXLEN=L'RQW4PA3,               +        
               OUTPUT=(D,B#SAVED,RQW4PA3)                                       
F$PEX    LKMAP F,D#W4PEX,PDAT,TA#PAYEX,OUTPUT=(D,B#SAVED,RQW4PEX)               
F$CRY    LKMAP F,D#W4CRY,CHAR,TA#CTRY,MAXLEN=L'RQW4CRY,                +        
               OUTPUT=(D,B#SAVED,RQW4CRY)                                       
F$EML    LKMAP F,D#W4EML,CHAR,TA#EMAIL,MAXLEN=L'RQW4EML,               +        
               OUTPUT=(D,B#SAVED,RQW4EML)                                       
F$FTX    LKMAP F,D#W4FTX,CHAR,TA#TTXFO,MAXLEN=L'RQW4FTX,               +        
               OUTPUT=(D,B#SAVED,RQW4FTX)                                       
F$PCY    LKMAP F,D#W4PCY,CHAR,TA#PAYCY,MAXLEN=L'RQW4PCY,               +        
               OUTPUT=(D,B#SAVED,RQW4PCY)                                       
F$PST    LKMAP F,D#W4PST,CHAR,TA#PAYST,MAXLEN=L'RQW4PST,               +        
               OUTPUT=(D,B#SAVED,RQW4PST)                                       
F$PZP    LKMAP F,D#W4PZP,CHAR,TA#PAYZP,MAXLEN=L'RQW4PZP,               +        
               OUTPUT=(D,B#SAVED,RQW4PZP)                                       
F$PCT    LKMAP F,D#W4PCT,CHAR,TA#PAYCT,MAXLEN=L'RQW4PCT,               +        
               OUTPUT=(D,B#SAVED,RQW4PCT)                                       
F$PAC    LKMAP F,D#W4PAC,PDAT,TA#PAYAC,OUTPUT=(D,B#SAVED,RQW4PAC)               
F$NHA    LKMAP F,D#W4NHA,CHAR,TA#NHA,MAXLEN=L'RQW4NHA,                 +        
               OUTPUT=(D,B#SAVED,RQW4NHA)                                       
F$NHP    LKMAP F,D#W4NHP,CHAR,TA#NHP,MAXLEN=L'RQW4NHP,                 +        
               OUTPUT=(D,B#SAVED,RQW4NHP)                                       
F$NHD    LKMAP F,D#W4NHD,PDAT,TA#NHD,OUTPUT=(D,B#SAVED,RQW4NHD)                 
F$CRP    LKMAP F,D#W4CRP,CHAR,TA#CRP,MAXLEN=L'RQW4CRP,                 +        
               OUTPUT=(D,B#SAVED,RQW4CRP)                                       
F$EFT    LKMAP F,D#W4EFT,CHAR,TA#EFT,MAXLEN=L'RQW4EFT,                 +        
               OUTPUT=(D,B#SAVED,RQW4EFT)                                       
F$REG    LKMAP F,D#W4REG,CHAR,TA#RGTST,MAXLEN=L'RQW4REG,               +        
               OUTPUT=(D,B#SAVED,RQW4REG)                                       
F$TCP    LKMAP F,D#W4TCP,CHAR,TA#CANPR,MAXLEN=L'RQW4TCP,               +        
               OUTPUT=(D,B#SAVED,RQW4TCP)                                       
F$FNC    LKMAP F,D#W4FNC,UBIN,TA#FNC,MAXLEN=4,OLEN=L'RQW4FNC,          +        
               OUTPUT=(D,B#SAVED,RQW4FNC)                                       
F$FD1    LKMAP F,D#W4FD1,CHAR,TA#FD1,MAXLEN=L'RQW4FD1,                 +        
               OUTPUT=(D,B#SAVED,RQW4FD1)                                       
F$FET    LKMAP F,D#W4FET,CHAR,TA#FET,MAXLEN=L'RQW4FET,                 +        
               OUTPUT=(D,B#SAVED,RQW4FET)                                       
F$FPZ    LKMAP F,D#W4FPZ,UBIN,TA#FPZ,MAXLEN=4,OLEN=L'RQW4FPZ,          +        
               OUTPUT=(D,B#SAVED,RQW4FPZ)                                       
F$PNC    LKMAP F,D#W4PNC,UBIN,TA#PNC,MAXLEN=4,OLEN=L'RQW4PNC,          +        
               OUTPUT=(D,B#SAVED,RQW4PNC)                                       
F$PD1    LKMAP F,D#W4PD1,CHAR,TA#PD1,MAXLEN=L'RQW4PD1,                 +        
               OUTPUT=(D,B#SAVED,RQW4PD1)                                       
F$PET    LKMAP F,D#W4PET,CHAR,TA#PET,MAXLEN=L'RQW4PET,                 +        
               OUTPUT=(D,B#SAVED,RQW4PET)                                       
F$PPZ    LKMAP F,D#W4PPZ,UBIN,TA#PPZ,MAXLEN=4,OLEN=L'RQW4PPZ,          +        
               OUTPUT=(D,B#SAVED,RQW4PPZ)                                       
F$PHD    LKMAP F,D#W4PHD,UBIN,TA#PHD,MAXLEN=4,OLEN=L'RQW4PHD,          +        
               OUTPUT=(D,B#SAVED,RQW4PHD)                                       
F$PSP    LKMAP F,D#W4PSP,UBIN,TA#PSP,MAXLEN=4,OLEN=L'RQW4PSP,          +        
               OUTPUT=(D,B#SAVED,RQW4PSP)                                       
F$PHC    LKMAP F,D#W4PHC,CHAR,TA#PHC,MAXLEN=L'RQW4PHC,                 +        
               OUTPUT=(D,B#SAVED,RQW4PHC)                                       
F$FEC    LKMAP F,D#W4FEC,CHAR,TA#FEC,MAXLEN=L'RQW4FEC,                 +        
               OUTPUT=(D,B#SAVED,RQW4FEC)                                       
F$WID    LKMAP F,D#W4WID,CHAR,TA#WAPID,MAXLEN=L'RQW4WID,               +        
               OUTPUT=(D,B#SAVED,RQW4WID)                                       
F$EOV    LKREQ F,D#W4EOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,               +        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,D#W4CMC,(I,B#SAVED,I$CLMC),UBIN,LIST=F,               +        
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
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
SVIOKEY  DS    XL(L'IOKEY)         SAVED KEY                                    
SVNMKEY  DS    XL(L'IOKEY)         SAVED NAME KEY                               
                                                                                
PROSTAT  DS    X                                                                
PSNPCHG  EQU   X'80'               NON-PAYEE SCREEN FIELD CHANGED               
PSPYCHG  EQU   X'40'               PAYEE SCREEN FIELD CHANGED                   
PSTDCHG  EQU   X'20'               TD1 SCREEN FIELD CHANGED                     
PSNMCHG  EQU   X'10'               NAME CHANGED                                 
                                                                                
CPYSTAT1 DS    X                   COPY ROUTINES' STATUS                        
CPYSTAWX EQU   X'80'               EXTRA DETAILS ENCOUNTERED                    
CPYTACMG EQU   X'40'               GENERAL COMMENT ENCOUNTERED                  
CPYSTAPE EQU   X'20'               PAYEE ENCOUNTERED                            
CPYTAOWM EQU   X'10'               MOTION PICTURE FUND ENCOUNTERED              
CPYTAOWC EQU   X'08'               PERMANENT CHARITY ENCOUNTERED                
CPYTAWHF EQU   X'04'               FEDERAL WITHHOLDING ENCOUNTERED              
CPYTAWHS EQU   X'02'               STATE WITHHOLDING ENCOUNTERED                
CPYTAWHC EQU   X'01'               CITY WIHTTHOLDING ENCOUNTERED                
                                                                                
CPYSTAT2 DS    X                   COPY ROUTINES' STATUS                        
CPYSTAAK EQU   X'80'               AKA NAME ENCOUNTERED                         
CPYTATI1 EQU   X'40'               TAX ID 1 ENCOUNTERED                         
CPYTATI2 EQU   X'20'               TAX ID 2 ENCOUNTERED                         
CPYTATI3 EQU   X'10'               TAX ID 3 ENCOUNTERED                         
CPYTATI4 EQU   X'08'               TAX ID 4 ENCOUNTERED                         
CPYTATI5 EQU   X'04'               TAX ID 5 ENCOUNTERED                         
CPYTATI6 EQU   X'02'               TAX ID 6 ENCOUNTERED                         
CPYSTAFL EQU   X'01'               FILTERS ENCOUNTERED                          
                                                                                
CPYSTAT3 DS    X                   COPY ROUTINES' STATUS                        
CPYTANUP EQU   X'80'               PHONE NUMBER ENCOUNTERED                     
CPYTANUM EQU   X'40'               ACTRA MEMBERSHIP ENCOUNTERED                 
CPYTANUG EQU   X'20'               GST NUMBER ENCOUNTERED                       
CPYTACMI EQU   X'10'               EMAIL ADDRESS ENCOUNTERED                    
CPYSTAD1 EQU   X'08'               TD1 ENCOUNTERED                              
                                                                                
FULL     DS    F                                                                
HALF     DS    F                                                                
*                                  A(STATE ENTRY IN TASYSUNITS                  
ASTAENT  DS    A                   OR A(PROVINCE ENTRY IN TASYSCTRY)            
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        W4 MAINTENANCE REQUEST MAP FIELDS                            *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQW4MOD  DS    CL1                 MODE                                         
RQW4RTV  EQU   1                   RETRIEVE                                     
RQW4VFY  EQU   2                   VERIFY                                       
RQW4EXE  EQU   3                   EXECUTE                                      
RQW4STF  DS    CL8                 STAFF CODE                                   
RQW4SSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
RQW4PID  DS    CL6                 PID                                          
RQW4LNM  DS    CL16                LAST NAME                                    
RQW4FNM  DS    CL16                FIRST NAME                                   
RQW4MNM  DS    CL16                MIDDLE NAME                                  
RQW4NLNQ EQU   *-RQW4LNM                                                        
RQW4TYP  DS    CL1                 TYPE                                         
RQW4AD1  DS    CL30                ADDRESS LINE 1                               
RQW4AD2  DS    CL30                ADDRESS LINE 2                               
RQW4AD3  DS    CL30                ADDRESS LINE 3                               
RQW4CTY  DS    CL25                CITY                                         
RQW4STA  DS    CL2                 STATE                                        
RQW4ZIP  DS    CL10                ZIP CODE                                     
RQW4SUF  DS    CL4                 SUFFIX                                       
RQW4PHO  DS    CL12                PHONE NUMBER                                 
RQW4AKF  DS    CL16                AKA FIRST NAME                               
RQW4AKL  DS    CL16                AKA LAST NAME                                
RQW4ALNQ EQU   *-RQW4AKF                                                        
RQW4CPN  DS    CL32                CORPORATION NAME                             
RQW4SXC  DS    CL1                 SEX CODE                                     
RQW4ETH  DS    CL2                 ETHNICITY                                    
RQW4FIL  DS    CL4                 FILTERS                                      
RQW4CP1  DS    CL9                 CORPORATION ID #1                            
RQW4CP2  DS    CL9                 CORPORATION ID #2                            
RQW4CP3  DS    CL9                 CORPORATION ID #3                            
RQW4CP4  DS    CL9                 CORPORATION ID #4                            
RQW4CP5  DS    CL9                 CORPORATION ID #5                            
RQW4CP6  DS    CL9                 CORPORATION ID #6                            
RQW4CRP  DS    CL9                 CORPORATION ID                               
RQW4RLNQ EQU   *-RQW4CP1                                                        
RQW4IDT  DS    XL3                 INDEMINIFICATION DATE                        
RQW4AFM  DS    CL3                 AFM LOCAL                                    
RQW4MNU  DS    CL9                 MEMBERSHIP NUMBER                            
RQW4YTD  DS    CL1                 YTD ON CHECKS?                               
RQW4CKF  DS    CL1                 SORT CHECKS FIRST?                           
RQW4ILF  DS    CL1                 IRS LOCK ON FEDERAL WITHHOLDING?             
RQW4LCK  DS    CL1                 W4 LOCKED?                                   
RQW4FIW  DS    CL1                 FICA WITHHOLDING?                            
RQW4DCW  DS    CL1                 DUE COMPANY WITHHOLDING?                     
RQW4PEN  DS    CL1                 PENSION?                                     
RQW4DIR  DS    CL1                 DIRECT DEPOSIT?                              
RQW4WIR  DS    CL1                 WIRE TRANSFER FOR CHECKS?                    
RQW4SPL  DS    CL1                 SPECIAL LETTER ON FILE?                      
RQW4FMS  DS    CL1                 FEDERAL TAX - MARRIED/SINGLE                 
RQW4FEX  DS    XL1                 FEDERAL TAX - EXEMPTIONS                     
RQW4FFX  DS    XL4                 FEDERAL TAX - FIXED PERCENTAGE               
RQW4FLNQ EQU   *-RQW4FMS                                                        
RQW4SAR  DS    CL3                 STATE TAX - AREA                             
RQW4SMS  DS    CL1                 STATE TAX - MARRIED/SINGLE                   
RQW4SEX  DS    XL1                 STATE TAX - EXEMPTIONS                       
RQW4SFX  DS    XL4                 STATE TAX - FIXED PERCENTAGE                 
RQW4SLNQ EQU   *-RQW4SAR                                                        
RQW4CAR  DS    CL3                 CITY TAX - AREA                              
RQW4CMS  DS    CL1                 CITY TAX - MARRIED/SINGLE                    
RQW4CEX  DS    XL1                 CITY TAX - EXEMPTIONS                        
RQW4CFX  DS    XL4                 CITY TAX - FIXED PERCENTAGE                  
RQW4CLNQ EQU   *-RQW4CAR                                                        
RQW4ERN  DS    XL4                 MINOR LIFETIME EARNINGS                      
RQW4DOB  DS    XL3                 DATE OF BIRTH                                
RQW4DED  DS    XL4                 DEDUCTION PERCENTAGE                         
RQW4TRS  DS    CL9                 TRUSTEE SS#                                  
RQW4XLNQ EQU   *-RQW4ERN                                                        
RQW4RST  DS    CL2                 RECIPROCAL STATE                             
RQW4MPR  DS    XL4                 MPR FUND PERCENTAGE                          
RQW4PCH  DS    XL4                 PERMANENT CHARITY PERCENTAGE                 
RQW4GST  DS    CL10                GST NUMBER                                   
RQW4CMT  DS    CL70                COMMENT                                      
RQW4PNM  DS    CL36                PAYEE NAME                                   
RQW4PA1  DS    CL30                PAYEE ADDRESS 1                              
RQW4PA2  DS    CL30                PAYEE ADDRESS 2                              
RQW4PA3  DS    CL30                PAYEE ADDRESS 3                              
RQW4PCY  DS    CL25                CITY                                         
RQW4PST  DS    CL2                 STATE                                        
RQW4PZP  DS    CL10                ZIP CODE                                     
RQW4PCT  DS    CL2                 COUNTRY                                      
RQW4PAC  DS    XL3                 PAYEE ACTIVE DATE                            
RQW4PEX  DS    XL3                 PAYEE EXPIRATION DATE                        
RQW4PLNQ EQU   *-RQW4PNM                                                        
RQW4CRY  DS    CL2                 COUNTRY                                      
RQW4EML  DS    CL35                EMAIL ADDRESS                                
RQW4FTX  DS    CL1                 TAKE TAXES FOR FOREIGNER?                    
RQW4WID  DS    CL18                WEB APPLICATION ID                           
RQW4NHA  DS    CL1                 ELIGIBLE FOR NEW HIRE ACT?                   
RQW4NHP  DS    CL1                 NEW HIRE ACT ELIGIBILITY PENDING?            
RQW4NHD  DS    XL3                 NEW HIRE ACT DATE                            
RQW4EFT  DS    CL1                 EFT?                                         
RQW4REG  DS    CL1                 REGRESSION TESTING?                          
RQW4TCP  DS    CL2                 TAXABLE CANADIAN PROVINCE                    
RQW4FNC  DS    XL4                 CAN FEDERAL NET CLAIM                        
RQW4FD1  DS    CL1                 CAN FEDERAL DEFAULT TO CC1?                  
RQW4FET  DS    CL1                 CAN FEDERAL EXEMPT?                          
RQW4FPZ  DS    XL4                 CAN FEDERAL PRESCRIBED ZONE                  
RQW4FEC  DS    CL1                 CAN FEDERAL EXEMPT FROM CPP?                 
RQW4PNC  DS    XL4                 CAN PROVINCIAL NET CLAIM                     
RQW4PD1  DS    CL1                 CAN PROVINCIAL DEFAULT TO CC1?               
RQW4PET  DS    CL1                 CAN PROVINCIAL EXEMPT?                       
RQW4PPZ  DS    XL4                 CAN PROVINCIAL PRESCRIBED ZONE               
RQW4PHD  DS    XL4                 CAN PROVINCIAL HOUSING DEDUCTION             
RQW4PSP  DS    XL4                 CAN PROVINCIAL SUPPORT PAYMENT               
RQW4PHC  DS    CL1                 CAN PROVINCIAL EXEMPT HEALT CONT?            
RQRCLNQ  EQU   *-RQW4MOD                                                        
                                                                                
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
**PAN#1  DC    CL21'004TALNK15   07/29/15'                                      
         END                                                                    
