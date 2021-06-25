*          DATA SET SPLNK02    AT LEVEL 027 AS OF 11/19/19                      
*PHASE T21E02B                                                                  
*                                                                               
*=======================T O M B S T O N E=============================*         
* WHO  DDMMMYR LVL DESCRIPTION                                                  
* ---- ------- --- -----------                                                  
* HWON 11NOV19 *26 SPEC-29367|19.3|SUPPORT 2-DEC TV IMPRESSIONS                 
* HWON **19.2* *26 SPEC-31225|CHG GETDEM TO SUPPORT MARKET BOOKTYPE             
*              *26 SPEC-31225|FIX GETDEM TO SUPPORT 2-CHAR BOOKTYPES            
*=====================================================================*         
*                                                                               
SPLNK02  TITLE '- SPOT SYSTEM SERVER SUPPORT ROUTINES 2'                        
SPLNK02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SL02**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    RE,ROU2RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ROUTINE NOT DEFINED                          
         AR    RF,RB               RF=A(ROUTINE)                                
         L     R8,ALP                                                           
         USING LP_D,R8             R8=A(LP_D)                                   
         L     RA,LP_ATWA                                                       
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(GETAGY-SPLNK02),AL2(GAWORKL)      #GETAGY (00)               
         DC    AL2(VALMED-SPLNK02),AL2(0)            #VALMED (01)               
         DC    AL2(EDTMED-SPLNK02),AL2(0)            #EDTMED (02)               
         DC    AL2(VALCLT-SPLNK02),AL2(0)            #VALCLT (03)               
         DC    AL2(GETCLT-SPLNK02),AL2(GCWORKL)      #GETCLT (04)               
         DC    AL2(LIMCLT-SPLNK02),AL2(LCWORKL)      #LIMCLT (05)               
         DC    AL2(EDTCLT-SPLNK02),AL2(0)            #EDTCLT (06)               
         DC    AL2(FLTPOL-SPLNK02),AL2(0)            #FLTPOL (07)               
         DC    AL2(VALDCD-SPLNK02),AL2(0)            #VALDCD (08)               
         DC    AL2(EDTDCD-SPLNK02),AL2(0)            #EDTDCD (09)               
         DC    AL2(VALPRD-SPLNK02),AL2(0)            #VALPRD (10)               
         DC    AL2(EDTPRD-SPLNK02),AL2(0)            #EDTPRD (11)               
         DC    AL2(VALORD-SPLNK02),AL2(VOWORKL)      #VALORD (12)               
         DC    AL2(EDTORD-SPLNK02),AL2(EOWORKL)      #EDTORD (13)               
         DC    AL2(VALSTA-SPLNK02),AL2(VSWORKL)      #VALSTA (14)               
         DC    AL2(VALDTM-SPLNK02),AL2(VDWORKL)      #VALDTM (15)               
         DC    AL2(VALDLB-SPLNK02),AL2(VBWORKL)      #VALDLB (16)               
         DC    AL2(VALUPG-SPLNK02),AL2(VUWORKL)      #VALUPG (17)               
         DC    AL2(GETDEM-SPLNK02),AL2(GDWORKL)      #GETDEM (18)               
         DC    AL2(EDTREP-SPLNK02),AL2(0)            #EDTREP (19)               
         DC    AL2(VALSLN-SPLNK02),AL2(SLWORKL)      #VALSLN (20)               
         DC    AL2(0),AL2(0)                         SPARE   (21)               
         DC    AL2(TRNSBT-SPLNK02),AL2(0)            #TRNSBT (22)               
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* GET AGENCY RECORD - R1=A(AGENCY ALPHA ID)                           *         
***********************************************************************         
                                                                                
GETAGY   J     *+12                                                             
         DC    C'*GETAGY*'                                                      
         LR    RB,RF                                                            
         USING GETAGY,RB                                                        
         USING GAWORKD,RC                                                       
         LA    R3,IOKEY                                                         
         USING AGYRECD,R3                                                       
         USING AGYKEY,IOKEY                                                     
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,0(R1)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#AGYREC'                        
         JNE   EXIT                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#AGYREC'                       
         JNE   EXIT                                                             
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   GETAGY10                                                         
         CLI   SVS002DP,0          TEST FIRST TIME                              
         JE    GETAGY10                                                         
         CLI   SVS00A2DI,0         TEST FIRST TIME                              
         JNE   EXITY                                                            
*                                                                               
GETAGY10 XC    GAPROFK,GAPROFK                                                  
         MVC   GAPROFK+0(4),=C'S000'                                            
         MVC   GAPROFK+4(L'LP_AGY),LP_AGY                                       
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(X'90',GAPROFK),GAPROFV,VDATAMGR                       
         MVC   SVS002DP,GAPROFV+9                                               
         CLI   SVS002DP,C'Y'                                                    
         BE    *+8                                                              
         MVI   SVS002DP,C'N'                                                    
*                                                                               
         MVC   GAPROFK+0(4),=C's00A'                                            
         GOTOR (RF),DMCB,(X'90',GAPROFK),GAPROFV,VDATAMGR                       
         MVC   SVS00A2DI,GAPROFV+6                                              
         CLI   SVS00A2DI,C'Y'                                                   
         BE    *+8                                                              
         MVI   SVS00A2DI,C'N'                                                   
*                                                                               
         J     EXITY                                                            
         DROP  R3,RB,RC                                                         
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
GAWORKD  DSECT                     ** GETAGY LOCAL W/S **                       
GAPROFK  DS    XL16                PROFILE KEY                                  
GAPROFV  DS    XL16                PROFILE VALUES                               
GAWORKL  EQU   *-GAWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MEDIA CODE                                                 *         
***********************************************************************         
                                                                                
VALMED   J     *+12                                                             
         DC    C'*VALMED*'                                                      
         LR    RB,RF                                                            
         USING VALMED,RB                                                        
         LM    R2,R4,0(R1)                                                      
         MVC   QMEDA,0(R2)                                                      
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         LA    R5,AGYEL                                                         
         USING AGYMEDEL,R5                                                      
         SR    R0,R0                                                            
VALMED02 CLI   AGYMEDEL,0          TEST END OF RECORD                           
         JE    EXITN                                                            
         CLI   AGYMEDEL,AGYMEDEQ   TEST MEDIA ELEMENT                           
         BNE   *+14                                                             
         CLC   AGYMEDCD,0(R2)      MATCH ON MEDIA CODE                          
         BE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R5,R0                                                            
         B     VALMED02                                                         
         MVC   0(L'AGYMEDBT,R4),AGYMEDBT                                        
         MVC   QMEDX,AGYMEDBT      SET AGENCY/MEDIA NUMBER                      
         J     EXITY                                                            
                                                                                
         DROP  R3,R5,RB                                                         
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT MEDIA CODE                                                     *         
***********************************************************************         
                                                                                
EDTMED   J     *+12                                                             
         DC    C'*EDTMED*'                                                      
         LR    RB,RF                                                            
         USING EDTMED,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         LA    R5,AGYEL                                                         
         USING AGYMEDEL,R5                                                      
         SR    R0,R0                                                            
EDTMED02 CLI   AGYMEDEL,0          TEST END OF RECORD                           
         JE    EXITN                                                            
         CLI   AGYMEDEL,AGYMEDEQ   TEST MEDIA ELEMENT                           
         BNE   *+14                                                             
         CLC   AGYMEDBT,0(R2)      MATCH ON AGENCY/MEDIA NUMBER                 
         BE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R5,R0                                                            
         B     EDTMED02                                                         
         MVC   0(L'AGYMEDCD,R4),AGYMEDCD                                        
         J     EXITY                                                            
         DROP  R3,R5,RB                                                         
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT CODE                                                *         
***********************************************************************         
                                                                                
VALCLT   J     *+12                                                             
         DC    C'*VALCLT*'                                                      
         LR    RB,RF                                                            
         USING VALCLT,RB                                                        
         LM    R2,R4,0(R1)                                                      
         MVC   QCLTA,0(R2)                                                      
         CHI   R3,2                                                             
         JL    EXITN                                                            
         BH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
         GOTOR VCLPACK,DMCB,QCLTA,(R4)                                          
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         MVC   QCLTX,0(R4)                                                      
         GOTOR (#GETCLT,AGETCLT)                                                
         J     EXIT                                                             
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* GET CLIENT RECORD QMEDX=AGENCY/MEDIA, QCLTX=CLIENT                  *         
***********************************************************************         
                                                                                
GETCLT   J     *+12                                                             
         DC    C'*GETCLT*'                                                      
         LR    RB,RF                                                            
         USING GETCLT,RB                                                        
         USING GCWORKD,RC                                                       
         MVC   GCIOSAVE,IOVALS                                                  
         LA    R3,IOKEY                                                         
         USING CLTRECD,R3                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,QMEDX                                                     
         MVC   CKEYCLT,QCLTX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CLTREC'                        
         BNE   GETCLTX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#CLTREC'                       
         BNE   GETCLTX                                                          
         GOTOR (#LIMCLT,ALIMCLT)   APPLY CLIENT LIMIT ACCESS                    
                                                                                
GETCLTX  MVC   IOVALS(IOVALL),GCIOSAVE                                          
         J     EXIT                                                             
         DROP  R3,RB,RC                                                         
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
GCWORKD  DSECT                     ** GETCLT S/R LOCAL W/S **                   
GCIOSAVE DS    XL(IOVALL)          SAVED I/O VALUES                             
GCWORKL  EQU   *-GCWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* APPLY CLIENT LIMIT ACCESS                                           *         
***********************************************************************         
                                                                                
LIMCLT   J     *+12                                                             
         DC    C'*LIMCLT*'                                                      
         LR    RB,RF                                                            
         USING LIMCLT,RB                                                        
         USING LCWORKD,RC                                                       
         USING OFFICED,LCOFFBLK    OFFICER CONTROL BLOCK                        
         L     R3,ACLTREC                                                       
         USING CLTRECD,R3          R3=A(CLIENT RECORD)                          
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),QCLTA                             
         MVI   OFCSYS,SPTLETQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCAUTH,LP_ACCS                                                  
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLTA                                                     
         MVC   OFCSAGMD,CKEYAM                                                  
         MVC   OFCLMT,LP_ACCS                                                   
         MVC   OFCSECD,LP_ASECD                                                 
         MVC   OFCACCSC(L'CACCESS),CACCESS                                      
         MVI   OFCACCSM,FF                                                      
         MVI   OFCINDS,OFCI2CSC                                                 
         MVC   OFCCLT2,CKEYCLT                                                  
         GOTOR VOFFICER,DMCB,(C'N',OFFICED),ACOMFACS                            
         J     EXIT                NOTE: CONDITION CODE SET BY OFFICER          
         DROP  R3,RB                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
LCWORKD  DSECT                     ** LIMCLT S/R LOCAL W/S **                   
LCOFFBLK DS    XL(OFCLENQ)         OFFICER BLOCK                                
LCWORKL  EQU   *-LCWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT CLIENT CODE                                                    *         
***********************************************************************         
                                                                                
EDTCLT   J     *+12                                                             
         DC    C'*EDTCLT*'                                                      
         LR    RB,RF                                                            
         USING EDTCLT,RB                                                        
         LM    R2,R4,0(R1)                                                      
                                                                                
         OC    0(L'CKEYCLT,R2),0(R2)                                            
         BNZ   *+14                                                             
         XC    4(4,R1),4(R1)       CLEAR THE LENGTH IF NO CLIENT                
         J     EXITY                                                            
         LHI   R0,3                                                             
         STCM  R0,15,4(R1)                                                      
                                                                                
         L     R5,ACLTREC                                                       
         USING CLTHDR,R5                                                        
         CLC   CKEYAM,QMEDX        TEST CLIENT RECORD AROUND                    
         BNE   *+14                                                             
         CLC   CKEYCLT,0(R2)                                                    
         BE    EDTCLT02                                                         
         MVC   QCLTX,0(R2)                                                      
         GOTOR (#GETCLT,AGETCLT)   NO - READ IT                                 
                                                                                
EDTCLT02 GOTOR VCLUNPK,DMCB,(CPROF+6,(R2)),(R4)                                 
         J     EXIT                                                             
         DROP  RB,R5                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY FILTERS TO POOL ESTIMATE RECORD                               *         
***********************************************************************         
                                                                                
FLTPOL   J     *+12                                                             
         DC    C'*FLTPOL*'                                                      
         LR    RB,RF                                                            
         USING FLTPOL,RB                                                        
         LR    R2,R1                                                            
         USING ESTHDR,R2                                                        
                                                                                
         CLC   ESTART,QENDDATE     TEST ESTIMATE WITHIN REQUEST RANGE           
         JH    EXITN                                                            
         CLC   EEND,QSTRDATE                                                    
         JL    EXITN                                                            
                                                                                
         OC    QPOLFLT,QPOLFLT     TEST ESTIMATE FILTER PRESENT                 
         JZ    EXITY                                                            
         LA    R1,QPOLFLT          R1=A(ESTIMATE REQUEST FILTERS)               
         LA    RF,EPROF            RF=A(ESTIMATE FILTERS)                       
         LHI   R0,3                R0=N'ESTIMATE FILTERS                        
FLTPOL02 CLI   0(R1),C'*'          TEST NO FILTER HERE (WILD CARD)              
         BE    FLTPOL06                                                         
         CLI   0(R1),C'-'          TEST NEGATIVE FILTER                         
         BNE   FLTPOL04                                                         
         AHI   R1,1                                                             
         CLC   0(1,R1),0(RF)       NEGATIVE FILTER                              
         JE    EXITN               DROP IF THE SAME                             
         B     FLTPOL06                                                         
FLTPOL04 CLC   0(1,R1),0(RF)       POSITIVE FILTER                              
         JNE   EXITN               DROP IF DIFFERENT                            
FLTPOL06 AHI   R1,1                BUMP TO NEXT REQUEST FILTER                  
         AHI   RF,1                BUMP TO NEXT ESTIMATE FILTER                 
         BCT   R0,FLTPOL02         DO FOR NUMBER OF FILTERS                     
         J     EXITY                                                            
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ORDER NUMBER                                               *         
***********************************************************************         
                                                                                
VALORD   J     *+12                                                             
         DC    C'*VALORD*'                                                      
         LR    RB,RF                                                            
         USING VALORD,RB                                                        
         USING VOWORKD,RC                                                       
         LM    R2,R4,0(R1)                                                      
         CHI   R3,8                TEST INPUT LENGTH CORRECT                    
         JNE   EXITN                                                            
*                                                                               
         LA    RE,7(R2)                                                         
VORD000  CR    RE,R2                                                            
         BL    VORD005                                                          
         CLI   0(RE),C'0'                                                       
         JL    EXITN                                                            
         BCT   RE,VORD000                                                       
*                                                                               
VORD005  CLI   1(R2),C'3'          NEW STYLE ORDER NUMBER?                      
         JNH   VORD010                                                          
         PACK  VODUB,0(8,R2)       YES                                          
         SP    VODUB,=P'04000000'                                               
         CVB   R1,VODUB                                                         
         STCM  R1,15,0(R4)                                                      
         OI    0(R4),X'80'                                                      
         J     VORD020                                                          
*                                                                               
*ORD010  GOTOR VDATCON,VODMCB,(5,0),(15,VOWORK)                                 
* MAKE IT LOOK LIKE AUG20,2012 WHEN WE LOADED THE NEW NUMBERING SCHEME          
VORD010  DS    0H                                                               
         MVC   VOWORK(4),=X'0112233F'   JULIAN DATE FOR AUG20/12                
         SRP   VOWORK,64-3,0                                                    
         GOTOR VHEXIN,VODMCB,(R2),VODUB,8                                       
         MVC   VOFULL,VODUB                                                     
         OI    VOFULL+L'VOFULL-1,X'0F'                                          
         SRP   VOFULL,64-6,0                                                    
         MVC   VOFULL+2(1),VOWORK+2                                             
         CP    VOFULL+3(1),VOWORK+3(1)                                          
         BNH   *+10                                                             
         SP    VOFULL,=P'10'                                                    
         SP    VOFULL,=P'90'                                                    
         SRP   VOFULL,4,0                                                       
         OC    VOFULL+1(2),VODUB                                                
         SRP   VOFULL,64-1,0                                                    
         ZAP   VODUB,VOFULL                                                     
         CVB   R0,VODUB                                                         
         STCM  R0,3,0(R4)                                                       
         PACK  VODUB,4(4,R2)                                                    
         CVB   R0,VODUB                                                         
         STCM  R0,3,2(R4)                                                       
*                                                                               
VORD020  XC    0(4,R4),EFFS                                                     
         J     EXITY                                                            
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
VOWORKD  DSECT                     ** VALORD S/R LOCAL W/S **                   
VODUB    DS    D                                                                
VODMCB   DS    6F                                                               
VOWORK   DS    F                                                                
VOFULL   DS    F                                                                
VOWORKL  EQU   *-VOWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT ORDER NUMBER                                                   *         
***********************************************************************         
                                                                                
EDTORD   J     *+12                                                             
         DC    C'*EDTORD*'                                                      
         LR    RB,RF                                                            
         USING EDTORD,RB                                                        
         USING EOWORKD,RC                                                       
         LM    R2,R4,0(R1)                                                      
         MVC   EOWORK(4),0(R2)                                                  
         XC    EOWORK(4),EFFS                                                   
         TM    EOWORK,X'80'         NEW STYLE ORDER NUMBER?                     
         BZ    EORD010                                                          
         NI    EOWORK,X'FF'-X'80'                                               
         ICM   R0,15,EOWORK                                                     
         CVD   R0,EODUB                                                         
         AP    EODUB,=P'04000000'                                               
         OI    EODUB+7,X'0F'                                                    
         UNPK  0(8,R4),EODUB                                                    
         B     EORD020                                                          
*                                                                               
EORD010  SR    R0,R0                                                            
         ICM   R0,3,EOWORK                                                      
         CVD   R0,EODUB                                                         
         OI    EODUB+L'EODUB-1,X'0F'                                            
         UNPK  EOWORK+8(4),EODUB                                                
         ICM   R0,3,EOWORK+2                                                    
         CVD   R0,EODUB                                                         
         OI    EODUB+L'EODUB-1,X'0F'                                            
         UNPK  EOWORK+12(4),EODUB                                               
         MVC   0(8,R4),EOWORK+8                                                 
*                                                                               
EORD020  LHI   R0,8                                                             
         STCM  R0,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
EOWORKD  DSECT                     ** EDTORD S/R LOCAL W/S **                   
EODUB    DS    D                                                                
EOWORK   DS    CL16                                                             
EOWORKL  EQU   *-EOWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION CALL LETTERS                                       *         
*                                                                               
* ON ENTRY:    PARAM 1            A(STATION CALL LETTERS)                       
*              PARAM 2            L(CALL LETTERS INPUT)                         
*              PARAM 3            A(OUTPUT AREA)                                
*                                                                               
* IF LP_VPARM:                    OUPUT WILL BE:                                
*              == 0               STAPSTA - 3 BYTE BINARY STATION               
*              $VALSSCL           STATION CALL LETTERS IF VALID                 
*              $VALSPAK           CONTENTS OF STAPACKD                          
*              $VALMKAS           CONTENTS OF STAPACKD HAS CORRECT MKT          
*              $VALSBTY           CONTENTS OF STAPACKD + BOOK TYPE              
*              $VALFLG1           CONTENTS OF STAPACKD + SFLAG1                 
*                                                                               
***********************************************************************         
                                                                                
         USING VSWORKD,RC                                                       
         USING STABLKD,VSSTABLK    STAVAL BLOCK                                 
         USING STAPACKD,VSSTAPKD   STAPACK BLOCK                                
         USING STARECD,VSSTAREC    STATION RECORD                               
VALSTA   J     *+12                                                             
         DC    C'*VALSTA*'                                                      
         LR    RB,RF                                                            
         USING VALSTA,RB                                                        
         LM    R2,R4,0(R1)                                                      
         MVC   VSWORK,SPACES                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VSWORK(0),0(R2)     EXTRACT INPUT INTO WORK                      
*                                                                               
         LA    R0,VSWORK                                                        
         STCM  R0,15,STBADDR                                                    
         OI    STBADDR,X'80'                                                    
         MVC   STBMED,QMEDA                                                     
         CLI   STBMED,C' '                                                      
         BH    *+8                                                              
         MVI   STBMED,C'T'                                                      
         L     RF,AAGYREC                                                       
         MVC   STBCTRY,AGYPCNDA-AGYHDR(RF)                                      
         MVC   STBACOM,ACOMFACS                                                 
         GOTOR VSTAVAL,DMCB,STABLKD                                             
         CLI   STBERR,0            TEST VALID STATION                           
         JNE   EXITN                                                            
*                                                                               
         MVC   VSMARKET,=C'0000'   PRESET NO MARKET SPECIFIED                   
         CLI   LP_VPARM,$VALFLG1   WANT STATION FLAG1?                          
         BE    VALSTA05            YES, WE'LL NEED THE STATION RECORD           
         CLI   LP_VPARM,$VALSBTY   WANT STATION BOOKTYPE?                       
         BE    VALSTA05            YES, WE'LL NEED THE STATION RECORD           
         CLI   LP_VPARM,$VALMKAS   RETURN MARKET IN STAPACK TOO?                
         BNE   VALSTA10                                                         
*                                                                               
VALSTA05 MVI   STAKEY,C'0'    PRE-FILL KEY WITH EBCDIC ZEROES                   
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,STBMED                                                   
         MVC   STAKCALL,STBSTA                                                  
         CLI   STAKCALL+L'STAKCALL-1,C' '                                       
         BH    *+10                                                             
         MVC   STAKCALL+L'STAKCALL-1(L'STBMED),STBMED                           
         MVC   STAKAGY,LP_AGY                                                   
         OC    QCLTA,QCLTA                                                      
         BZ    *+10                                                             
         MVC   STAKCLT,QCLTA                                                    
         MVC   VSIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                      
         MVC   IOKEY,STAKEY                                                     
         LA    R0,VSSTAREC                                                      
         STCM  R0,15,IOADDR                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL'                                
         MVC   IOVALS(IOVALL),VSIOSAVE                                          
         JNE   EXITN               EXITN IF STATION RECORD NOT FOUND            
         MVC   VSMARKET,SMKT       EXTRACT STATION VALUES                       
*         MVC   VSBKTYPE,SBKTYPE                                                
*         MVC   VSFLAG1,SFLAG1                                                  
***         DROP  VSSK                                                          
*&                                                                              
*VSSK     USING STARECD,VSSTAREC    YES - LOOK UP MARKET NUMBER                 
*VALSTA05 MVI   VSSK.STAKEY,C'0'    PRE-FILL KEY WITH EBCDIC ZEROES             
*         MVC   VSSK.STAKEY+1(L'STAKEY-1),VSSK.STAKEY                           
*         MVI   VSSK.STAKTYPE,STAKTYPQ                                          
*         MVC   VSSK.STAKMED,STBMED                                             
*         MVC   VSSK.STAKCALL,STBSTA                                            
*         CLI   VSSK.STAKCALL+L'STAKCALL-1,C' '                                 
*         BH    *+10                                                            
*         MVC   VSSK.STAKCALL+L'STAKCALL-1(L'STBMED),STBMED                     
*         MVC   VSSK.STAKAGY,LP_AGY                                             
*         OC    QCLTA,QCLTA                                                     
*         BZ    *+10                                                            
*         MVC   VSSK.STAKCLT,QCLTA                                              
*         MVC   VSIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                     
*         MVC   IOKEY,VSSK.STAKEY                                               
*         LA    R0,VSSTAREC                                                     
*         STCM  R0,15,IOADDR                                                    
*         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL'                               
*         MVC   IOVALS(IOVALL),VSIOSAVE                                         
*         JNE   EXITN               EXITN IF STATION RECORD NOT FOUND           
*         MVC   VSMARKET,VSSK.SMKT  EXTRACT STATION VALUES                      
*         MVC   VSBKTYPE,VSSK.SBKTYPE                                           
*         MVC   VSFLAG1,VSSK.SFLAG1                                             
*         DROP  VSSK                                                            
*                                                                               
VALSTA10 MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         MVC   STAPCTRY,STBCTRY                                                 
         MVC   STAPMED,STBMED                                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,VSMARKET                                                
         MVC   STAPQSTA(STASTAL),STBSTA                                         
         GOTOR VSTAPACK,STAPACKD                                                
*                                                                               
         CLI   LP_VPARM,$VALSSCL   TEST CALL LETTERS REQUIRED                   
         BNE   *+14                                                             
         MVC   0(L'STAPQSTA,R4),STAPQSTA                                        
         B     VALSTAX                                                          
*                                                                               
         MVC   0(L'STAPSTA,R4),STAPSTA  3 BYTE BINARY STATION                   
*                                                                               
         CLI   LP_VPARM,0          TEST SPECIAL CALL                            
         BE    *+10                                                             
         MVC   0(STAPACKL,R4),STAPACKD                                          
*                                                                               
         CLI   LP_VPARM,$VALFLG1   TEST RETURN SFLAG1 AS WELL                   
         BNE   *+10                                                             
         MVC   STAPACKL(L'SFLAG1,R4),SFLAG1                                     
*                                                                               
         CLI   LP_VPARM,$VALSBTY   TEST RETURN BOOK TYPE TOO                    
         BNE   VALSTAX                                                          
         MVC   STAPACKL(L'SBKTYPE,R4),SBKTYPE                                   
                                                                                
VALSTAX  CLI   STAPERR,0           SET CONDITION CODE AND EXIT                  
         J     EXIT                                                             
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
VSWORKD  DSECT                     ** VALSTA LOCAL W/S **                       
VSWORK   DS    CL20                EXTRACTED INPUT VALUE                        
VSMARKET DS    CL(L'SMKT)          MARKET NUMBER                                
*VSBKTYPE DS    CL(L'SBKTYPE)       BOOK TYPE                                   
VSFLAG1  DS    XL(L'SFLAG1)        FLAG1 FROM STATION RECORD                    
*                                                                               
VSIOSAVE DS    XL(IOVALL)          SAVED I/O VALUES                             
VSSTABLK DS    XL(STBLNQ)          STAVAL BLOCK                                 
VSSTAPKD DS    XL(STAPACKL)        STAPACK BLOCK                                
VSSTAREC DS    XL1000              STATION FILE I/O AREA                        
VSWORKL  EQU   *-VSWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAY/TIME EXPRESSION                                        *         
***********************************************************************         
                                                                                
         USING VDWORKD,RC                                                       
VALDTM   J     *+12                                                             
         DC    C'*VALDTM*'                                                      
         LR    RB,RF                                                            
         USING VALDTM,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    VDFLDH,VDFLDH                                                    
         MVC   VDFLD,SPACES                                                     
         STC   R3,VDFLDH+5                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VDFLD(0),0(R2)                                                   
* PROTECTION CODE TO FIX BAD DAY/TIME STRING PASSED BY PC                       
         ZIC   RE,VDFLDH+5                                                      
         SHI   RE,1                                                             
VALDTM1A LA    RF,VDFLD(RE)                                                     
         CHI   RE,0                                                             
         BE    VALDTM1D                                                         
         CLI   0(RF),C'/'                                                       
         BE    VALDTM1B                                                         
         SHI   RE,1                                                             
         B     VALDTM1A                                                         
VALDTM1B SHI   RE,1                                                             
VALDTM1C LA    RF,VDFLD(RE)                                                     
         CHI   RE,0                                                             
         BE    VALDTM1D                                                         
         CLI   0(RF),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C','                                                       
         SHI   RE,1                                                             
         B     VALDTM1C                                                         
                                                                                
**       GOTOR VSCANNER,DMCB,(12,VDFLDH),VDSCAN,C',= /'                         
VALDTM1D GOTOR VSCANNER,DMCB,VDFLDH,VDSCAN,C',=/ '                              
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          INPUT MUST BE VALID                          
         JZ    EXITN                                                            
         USING SCANBLKD,VDSCAN                                                  
         CLI   SC1STLEN,0          THERE MUST BE A DAY EXPRESSION               
         JE    EXITN                                                            
         CLC   =C'ALL',SC1STFLD    ALL DAYPARTS                                 
         JNE   *+14                                                             
         MVC   0(5,R4),=X'FFFFFFFFFF'                                           
         J     EXITY                                                            
         CLC   =C'E-ALL',SC1STFLD    EASTERN TIMEZONE DAYPARTS                  
         JNE   *+14                                                             
         MVC   0(5,R4),=X'FFFFFFFFFE'                                           
         J     EXITY                                                            
         CLC   =C'C-ALL',SC1STFLD    CENTRAL TIMEZONE DAYPARTS                  
         JNE   *+14                                                             
         MVC   0(5,R4),=X'FFFFFFFFFC'                                           
         J     EXITY                                                            
                                                                                
         SR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'VAR' TEST FOR VARIABLE                            
         BNE   *+12                                                             
         MVI   0(R4),X'90'                                                      
         B     VALDTM04                                                         
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'AVG' TEST FOR AVERAGE                             
         BNE   VALDTM02                                                         
         LA    RF,SC1STFLD+1(RF)                                                
         CLI   0(RF),C'2'          AVERAGE MUST BE 2 THROUGH 6                  
         JL    EXITN                                                            
         CLI   0(RF),C'6'                                                       
         JH    EXITN                                                            
         MVC   0(1,R4),0(RF)                                                    
         NI    0(R4),X'FF'-X'60'                                                
         B     VALDTM04                                                         
                                                                                
VALDTM02 GOTOR VDAYVAL,DMCB,(SC1STLEN,SC1STFLD),0(R4),=X'17'                    
                                                                                
*VALDTM04 CLI   SC2NDLEN,0          THERE MUST BE A TIME EXPRESSION             
VALDTM04 CLI   SC1STLEN+L'SCLINE,0  THERE MUST BE A TIME EXPRESSION             
         JE    EXITN                                                            
*        GOTOR VTIMVAL,DMCB,(SC2NDLEN,SC2NDFLD),1(R4)                           
         CLC   =C'6A-6A',SC1STFLD+L'SCLINE                                      
         BE    *+10                                                             
         CLC   =C'6AM-6AM',SC1STFLD+L'SCLINE                                    
         BNE   *+14                                                             
         MVC   1(4,R4),=X'02580258'                                             
         J     EXITY                                                            
         CLC   =C'12M-6A',SC1STFLD+L'SCLINE                                     
         BE    *+10                                                             
         CLC   =C'12A-6A',SC1STFLD+L'SCLINE                                     
         BNE   *+14                                                             
         MVC   1(4,R4),=X'09600258'                                             
         J     EXITY                                                            
         GOTOR VTIMVAL,DMCB,(SC1STLEN+L'SCLINE,SC1STFLD+L'SCLINE),1(R4)         
         CLI   DMCB,X'FF'                                                       
         JE    EXITN              INVALID TIME EXPRESSION                       
         J     EXITY                                                            
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
VDWORKD  DSECT                     ** VALDTM LOCAL W/S **                       
VDFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VDFLD    DS    CL80                DUMMY FIELD                                  
VDSCAN   DS    2XL(SCBLKLQ+2)      SCANNER OUTPUT                               
VDWORKL  EQU   *-VDWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEMO LOOKUP BOOKS                                          *         
***********************************************************************         
                                                                                
         USING VBWORKD,RC                                                       
VALDLB   J     *+12                                                             
         DC    C'*VALDLB*'                                                      
         LR    RB,RF                                                            
         USING VALDLB,RB                                                        
         MVC   VBCALOPT,0(R1)      SET CALLING OPTIONS                          
         LM    R2,R4,0(R1)                                                      
         LA    R2,0(R2)                                                         
         XC    0(6,R4),0(R4)       CLEAR OUTPUT AREA                            
                                                                                
         CHI   R3,3                TEST 3 CHARACTERS INPUT                      
         BNE   *+14                                                             
         CLC   0(3,R2),=C'LATEST'                                               
         BE    VALDLB12                                                         
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    VALDLB12                                                         
         CLC   0(3,R2),=C'LATEST'                                               
         BNE   VALDLB01                                                         
         LA    RF,1                 MASK OF 1                                   
         CHI   R3,4                                                             
         BE    *+16                                                             
         CHI   R3,5                                                             
         BH    EXITN                                                            
         LA    RF,3                 MASK OF 3                                   
         SR    RE,RE                                                            
         EX    RF,*+8                                                           
         B     *+8                                                              
         ICM   RE,0,3(R2)           RE=NUMBER OF LATEST BOOKS (EBCDIC)          
         B     VALDLB02                                                         
*                                                                               
* ALSO CHECK X(BOOK) OR XX(BOOK) STRING                                         
VALDLB01 CLC   =C'BOOK',1(R2)                                                   
         BE    *+14                                                             
         CLC   =C'BOOK',2(R2)                                                   
         BNE   VALDLB03                                                         
         LA    RF,1                 MASK OF 1                                   
         CHI   R3,5                                                             
         BE    *+16                                                             
         CHI   R3,6                                                             
         BH    EXITN                                                            
         LA    RF,3                 MASK OF 3                                   
         SR    RE,RE                                                            
         EX    RF,*+8                                                           
         B     *+8                                                              
         ICM   RE,0,0(R2)           RE=NUMBER OF LATEST BOOKS (EBCDC)           
         B     VALDLB02                                                         
*                                                                               
*RE SHOULD HAVE THE NUMBER OF LATEST BOOKS                                      
VALDLB02 XC    DUB,DUB                                                          
         STCM  RE,15,DUB+8                                                      
         PACK  DUB,DUB+8(4)                                                     
         CVB   RE,DUB                                                           
         CHI   RE,1                                                             
         BL    EXITN                                                            
         CHI   RE,12                                                            
         BH    EXITN                                                            
         MVI   4(R4),FF                                                         
         STC   RE,5(R4)                                                         
         B     VALDLB12                                                         
*                                                                               
                                                                                
VALDLB03 LR    R0,R3                                                            
         LR    RF,R2                                                            
         SR    R1,R1                                                            
                                                                                
VALDLB04 CLI   0(RF),C'('                                                       
         BNE   VALDLB06                                                         
         AHI   RF,1                                                             
         CLI   1(RF),C')'                                                       
         BNE   *+14                                                             
         MVC   0(1,R4),0(RF)      1 CHAR BOOKTYPE                               
         B     VALDLB05                                                         
*                                 ELSE ASSUME 2 CHAR BOOKTYPE                   
         LR    R5,RF                                                            
         LR    R6,R1                                                            
         GOTOR (#TRNSBT,ATRNSBT),DMCB,0(RF),2,0(R4)                             
         LR    RF,R5                                                            
         LR    R1,R6                                                            
         CLI   0(R4),X'FF' MUST BE VALID BOOKTYPE IN TABLE                      
         BE    EXITN                                                            
VALDLB05 DS    0H                                                               
*                                                                               
         AHI   RF,1                                                             
                                                                                
VALDLB06 CLI   0(RF),C'-'                                                       
         BNE   VALDLB08                                                         
         AHI   RF,1                                                             
         MVC   1(1,R4),0(RF)                                                    
         CLI   1(RF),C'W'                                                       
         BE    VALDLB08                                                         
         CLI   0(RF),C'1'                                                       
         JL    EXITN                                                            
         CLI   0(RF),C'4'                                                       
         JH    EXITN                                                            
                                                                                
VALDLB08 OC    0(2,R4),0(R4)       TEST REACHED SUFFIX DATA YET                 
         BNZ   *+8                                                              
         AHI   R1,1                NO - BUMP BOOK INPUT CHARACTER COUNT         
         AHI   RF,1                                                             
         BCT   R0,VALDLB04                                                      
                                                                                
         LTR   R1,R1               TEST ANY BOOK INPUT                          
         JZ    EXITN                                                            
         MVI   VBFLDH,L'VBFLDH+L'VBFLD                                          
         STC   R1,VBFLDH+5                                                      
         MVC   VBFLD,SPACES                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VBFLD(0),0(R2)                                                   
         TM    VBCALOPT,$VALDWKY   TEST WEEKLY BOOK TYPE                        
         JNZ   VALDLB10                                                         
                                                                                
         GOTOR VBOOKVAL,DMCB,(C'N',VBFLDH),(1,3(R4)),(C'B',VSCANNER),  *        
               VBWORK                                                           
         CLI   4(R1),1                                                          
         JNE   EXITN                                                            
         B     VALDLB12                                                         
                                                                                
VALDLB10 GOTOR VDATVAL,DMCB,(0,VBFLD),VBEDATE                                   
         OC    0(4,R1),0(R1)                                                    
         JZ    EXITN                                                            
         LHI   R0,0                                                             
         TM    VBCALOPT,$VALDDMY   TEST MONDAY DEFAULT                          
         BZ    *+8                                                              
         LHI   R0,1                                                             
         GOTOR VNSIWEEK,DMCB,VBEDATE,((R0),VGETDAY),VADDAY,VDATCON              
         MVI   3(R4),0                                                          
         MVC   4(1,R4),4(R1)       SET YEAR                                     
         MVC   5(1,R4),0(R1)       SET WEEK                                     
                                                                                
VALDLB12 MVI   2(R4),C'Y'                                                       
         J     EXITY                                                            
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
VBWORKD  DSECT                     ** VALDLB LOCAL W/S **                       
VBCALOPT DS    X                   CALLING OPTIONS ($VALDXXX EQUATES)           
VBEDATE  DS    CL6                 EBCDIC DATE                                  
VBFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VBFLD    DS    CL80                DUMMY INPUT FIELD                            
VBWORK   DS    CL8                 FOR BOOK TYPES                               
VBWORKL  EQU   *-VBWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A DEMO CODE                                     *         
***********************************************************************         
                                                                                
VALDCD   J     *+12                                                             
         DC    C'*VALDCD*'                                                      
         LR    RB,RF                                                            
         USING VALDCD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         CHI   R3,1                LENGTH LESS THAN OR EQ 1                     
         JNH   EXITN                ERROR                                       
         CHI   R3,4                LENGTH GRTR THAN 4                           
         JH    EXITN                ERROR                                       
         SHI   R3,2                SETUP FOR EX-MVZ                             
         MVC   DUB(3),=C'000'                                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),1(R2)                                                     
         CLC   DUB(3),=C'000'      MAKE SURE NUMERIC                            
         JNE   EXITN                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R2)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0               NOT 0                                        
         JZ    EXITN                                                            
         CHI   R0,255              NOT GRTR THAN 255                            
         JH    EXITN                                                            
         MVC   1(1,R4),0(R2)                                                    
         CLI   1(R4),C'U'          TEST USER DEMO REQUESTED                     
         BNE   *+8                                                              
         MVI   1(R4),33            X'21'-USER-DEFINED                           
         STC   R0,2(R4)                                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT DEMO CODE                                                      *         
***********************************************************************         
                                                                                
EDTDCD   J     *+12                                                             
         DC    C'*EDTDCD*'                                                      
         LR    RB,RF                                                            
         USING EDTDCD,RB                                                        
         L     R2,0(R1)                                                         
         L     R4,8(R1)                                                         
         SR    R3,R3                                                            
         OC    0(3,R2),0(R2)                                                    
         BZ    EDTDCDX                                                          
*                                                                               
         LLC   RF,0(R2)                                                         
         ICM   RF,B'0010',2(R2)    1&3 BYTE ZERO, NON-TRADITIONAL?              
         LTR   RF,RF                                                            
         JZ    EDTDCDX             IGNORE NON-TRADITIONAL                       
                                                                                
         CLI   1(R2),63            X'3F' - IGNORE WEIGHTED DEMOS                
         BNE   EDTDCD05                                                         
         MVI   0(R4),C'W'          YES, SET RATING TYPE TO C'U'                 
         B     EDTDCD15                                                         
                                                                                
EDTDCD05 CLI   1(R2),33            X'21' - USER DEFINED?                        
         BNE   EDTDCD10                                                         
         MVI   0(R4),C'U'          YES, SET RATING TYPE TO C'U'                 
         B     EDTDCD15                                                         
                                                                                
EDTDCD10 MVC   0(1,R4),1(R2)                                                    
EDTDCD15 SR    RF,RF                                                            
         ICM   RF,1,2(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(3,R4),DUB                                                      
         LHI   R3,4                                                             
EDTDCD30 CLI   1(R4),C'0'                                                       
         BNE   EDTDCDX                                                          
         MVC   1(2,R4),2(R4)                                                    
         MVI   3(R4),C' '                                                       
         BCTR  R3,0                                                             
         B     EDTDCD30                                                         
EDTDCDX  ST    R3,4(R1)                                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PRODUCT CODE                                             *         
*                                                                     *         
* RETURNS PRODUCT NUMBER UNLESS LP_VPARM IS SET TO C'C' TO GET CODE   *         
***********************************************************************         
                                                                                
VALPRD   J     *+12                                                             
         DC    C'*VALPRD*'                                                      
         LR    RB,RF                                                            
         USING VALPRD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         MVC   WORK(L'CPLPMNEM),SPACES                                          
         SHI   R3,1                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         L     R3,ACLTREC                                                       
         AHI   R3,CPLDATA-CLTHDR                                                
         USING CPLDATA,R3                                                       
         LHI   R0,CPLDMAXN                                                      
                                                                                
VALPRD02 CLC   CPLPMNEM,WORK                                                    
         BE    VALPRD04                                                         
         AHI   R3,CPLDATAL                                                      
         BCT   R0,VALPRD02                                                      
         J     EXITN                                                            
                                                                                
VALPRD04 MVC   0(L'CPLPNUMB,R4),CPLPNUMB                                        
         CLI   LP_VPARM,C'C'                                                    
         JNE   EXITY                                                            
         MVC   0(L'CPLPMNEM,R4),WORK                                            
         J     EXITY                                                            
         DROP  R3,RB                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT PRODUCT CODE                                                   *         
***********************************************************************         
                                                                                
EDTPRD   J     *+12                                                             
         DC    C'*EDTPRD*'                                                      
         LR    RB,RF                                                            
         USING EDTPRD,RB                                                        
         LM    R2,R4,0(R1)                                                      
         OC    0(L'CPLPNUMB,R2),0(R2)                                           
         BNZ   *+14                                                             
         XC    4(4,R1),4(R1)       CLEAR THE LENGTH IF NO PRODUCT               
         J     EXITY                                                            
                                                                                
         L     R3,ACLTREC                                                       
         AHI   R3,CPLDATA-CLTHDR                                                
         USING CPLDATA,R3                                                       
         LHI   R0,CPLDMAXN                                                      
                                                                                
EDTPRD02 CLC   CPLPNUMB,0(R2)                                                   
         BE    EDTPRD04                                                         
         AHI   R3,CPLDATAL                                                      
         BCT   R0,EDTPRD02                                                      
         MVC   0(3,R4),=C'???'                                                  
         B     EDTPRD06                                                         
                                                                                
EDTPRD04 MVC   0(L'CPLPMNEM,R4),CPLPMNEM                                        
                                                                                
EDTPRD06 LHI   R3,L'CPLPMNEM                                                    
         STCM  R3,15,4(R1)                                                      
         J     EXITY                                                            
         DROP  R3,RB                                                            
                                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE UPGRADE EXPRESSION                                         *         
***********************************************************************         
                                                                                
         USING VUWORKD,RC                                                       
VALUPG   J     *+12                                                             
         DC    C'*VALUPG*'                                                      
         LR    RB,RF                                                            
         USING VALUPG,RB                                                        
         LM    R2,R4,0(R1)                                                      
         USING VUFORM,R4           R4=A(OUTPUT FORMULA)                         
         MVI   VUFLDH,L'VUFLDH+L'VUFLD                                          
         STC   R3,VUFLDH+5                                                      
         MVI   VUFLD,C' '                                                       
         MVC   VUFLD+1(L'VUFLD-1),VUFLD                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VUFLD(0),0(R2)                                                   
                                                                                
         GOTOR VSCANNER,DMCB,('VUSCANL2',VUFLDH),VUSCAN,C',=,='                 
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          R0=NUMBER OF SCAN FIELDS                     
         JZ    EXITN                                                            
         LA    R2,VUSCAN                                                        
         USING SCANBLKD,R2         R2=A(SCANNER OUTPUT LINE)                    
                                                                                
VALUPG02 CLC   =C'UPT',SC1STFLD    GO TO ROUTINE BASED ON KEYWORD               
         BE    VALUPG04                                                         
         CLC   =C'UPP',SC1STFLD                                                 
         BE    VALUPG04                                                         
         CLC   =C'UPX',SC1STFLD                                                 
         BE    VALUPG04                                                         
         CLC   =C'PUT',SC1STFLD                                                 
         BE    VALUPG06                                                         
         CLC   =C'RTG',SC1STFLD                                                 
         BE    VALUPG06                                                         
         CLC   =C'RP ',SC1STFLD                                                 
         BE    VALUPG06                                                         
         CLC   =C'BK ',SC1STFLD                                                 
         BE    VALUPG10                                                         
         CLC   =C'DT ',SC1STFLD                                                 
         BE    VALUPG12                                                         
         J     EXITN                                                            
                                                                                
VALUPG04 SR    RF,RF               VALIDATE UPGRADE EXPRESSION                  
         ICM   RF,1,SC2NDLEN                                                    
         JZ    EXITN                                                            
         STC   RF,VUFLDH+5                                                      
         MVI   VUFLD,C' '                                                       
         MVC   VUFLD+1(L'VUFLD-1),VUFLD                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VUFLD(0),SC2NDFLD                                                
         GOTOR VUPVAL,DMCB,VUFLDH,VUWORK,(C'/',ACOMFACS)                        
****     CLI   VUWORK,0            TEST VALID UPGRADE EXPRESSION                
         CLI   0(R1),0                                                          
         JE    EXITN                                                            
         MVC   VUFBKTY(L'VUFBKTY+L'VUFUPGD),VUWORK+3                            
         B     VALUPG14                                                         
                                                                                
VALUPG06 DS    0H                                                               
         CLI   SC2NDLEN,1                                                       
         JNE   EXITN                                                            
         CLI   SC2NDFLD,C'1'                                                    
         BE    VALUPG14                                                         
         CLI   SC2NDFLD,C'2'                                                    
         BNE   EXITN                                                            
         CLC   =C'PUT',SC1STFLD                                                 
         JNE   VALUPG08                                                         
         MVI   VUFPUTF,VUFPUTFY    SET 2 YEAR PUT                               
         B     VALUPG14                                                         
                                                                                
VALUPG08 MVI   VUFRTGF,VUFRTGFY    SET 2 YEAR RATING                            
         B     VALUPG14                                                         
                                                                                
VALUPG10 SR    RF,RF               VALIDATE BOOK                                
         ICM   RF,1,SC2NDLEN                                                    
         JZ    EXITN                                                            
         GOTOR (#VALDLB,AVALDLB),DMCB,SC2NDFLD,(RF),VUWORK                      
         JNE   EXITN                                                            
         MVC   VUFUPBK,VUWORK+4                                                 
         MVC   VUFBKTY,VUWORK+0                                                 
*  CONVERT BOOKTYPE                                                             
         B     VALUPG14                                                         
                                                                                
VALUPG12 SR    RF,RF               VALIDATE DAY/TIME                            
         ICM   RF,1,SC2NDLEN                                                    
         JZ    EXITN                                                            
         GOTOR (#VALDTM,AVALDTM),DMCB,SC2NDFLD,(RF),VUFUPDT                     
         JNE   EXITN                                                            
                                                                                
VALUPG14 AHI   R2,L'VUSCAN         BUMP TO NEXT SCANNER BLOCK ENTRY             
         BCT   R0,VALUPG02         DO FOR NUMBER OF ENTRIES                     
         J     EXITY                                                            
         DROP  R2,R4,RB,RC                                                      
                                                                                
         LTORG                                                                  
         DS    0H                                                               
                                                                                
VUWORKD  DSECT                     ** VALUPG LOCAL W/S **                       
VUFLDH   DS    XL8                 DUMMY FIELD HEADER                           
VUFLD    DS    CL240               DUMMY INPUT FIELD                            
VUWORK   DS    XL64                WORK AREA                                    
**VUSCANL2 EQU   30                  LENGTH OF SECOND HALF                      
VUSCANL2 EQU   50                  LENGTH OF SECOND HALF                        
VUSCANL  EQU   SC2NDFLD-SCANBLKD+VUSCANL2                                       
VUSCAN   DS    10XL(VUSCANL)       SCANNER OUTPUT BLOCK                         
VUWORKL  EQU   *-VUWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET DEMO VALUES                                                     *         
***********************************************************************         
                                                                                
         USING GDWORKD,RC                                                       
         USING DEMLKD,GDDEMLKB                                                  
         USING SPDEMUPD,GDDEMUPB                                                
         USING STAPACKD,GDSTAOUT                                                
GETDEM   J     *+12                                                             
         DC    C'*GETDEM*'                                                      
         LR    RB,RF                                                            
         USING GETDEM,RB                                                        
                                                                                
         MVC   GDIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                      
                                                                                
         LR    R2,R1                                                            
         USING GDEMBLKD,R2         R2=A(GETDEM INTERFACE BLOCK)                 
         USING VUOUTD,GDUPG                                                     
                                                                                
         CLI   GDMEDA,0            TEST MEDIA CODE PROVIDED                     
         BE    GTDEM010                                                         
         CLI   GDMEDX,0            TEST AGENCY/MEDIA PROVIDED                   
         BNE   GTDEM020                                                         
         GOTOR (#VALMED,AVALMED),DMCB,GDPARM,GDMEDA,,GDMEDX                     
         BNE   GETDEMN                                                          
         B     GTDEM020                                                         
                                                                                
GTDEM010 CLI   GDMEDX,0            TEST AGENCY/MEDIA PROVIDED                   
         BE    GETDEMN                                                          
         GOTOR (#EDTMED,AEDTMED),GDPARM,GDMEDX,,GDMEDA                          
         BNE   GETDEMN                                                          
                                                                                
GTDEM020 L     R3,ACLTREC          TEST A(CLIENT RECORD) PASSED                 
         USING CLTRECD,R3                                                       
         CLC   CKEYAM,GDMEDX       TEST CORRECT AGENCY/MEDIA                    
         BE    *+10                                                             
         XC    CKEY,CKEY                                                        
                                                                                
         OC    GDCLTX,GDCLTX       TEST CLIENT CODE PROVIDED                    
         BNZ   GTDEM030                                                         
         OC    GDCLTA,GDCLTA       TEST CLIENT ALPHA PROVIDED                   
         BZ    GETDEMN                                                          
         GOTOR (#VALCLT,AVALCLT),GDPARM,GDCLTA,,GDCLTX                          
         BNE   GETDEMN                                                          
                                                                                
GTDEM030 MVC   QCLTX,GDCLTX                                                     
         CLC   GDCLTX,CKEYCLT      TEST HAVE CLIENT RECORD                      
         BE    GTDEM040                                                         
         MVC   QMEDX,GDMEDX        NO - READ IT NOW                             
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   GETDEMN                                                          
                                                                                
GTDEM040 OC    GDCLTA,GDCLTA       TEST CLIENT CODE RESOLVED                    
         BNZ   GTDEM050                                                         
         GOTOR (#EDTCLT,AEDTCLT),GDPARM,GDCLTX,,GDCLTA                          
                                                                                
GTDEM050 MVI   GDCLTSRC,C'N'       SET RATING SERVICE                           
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   GDCLTSRC,C'A'                                                    
*                                                                               
         CLI   CPOLONLY,C'Y'       IF TRUE POL CLIENT                           
         BNE   *+8                                                              
         MVI   GDPRDX,X'FF'        FORCE READ POL ESTIMATE                      
         DROP  R3                                                               
                                                                                
         CLI   GDESTX,0            TEST ESTIMATE PASSED                         
         BE    GTDEM130                                                         
         ICM   R3,7,GDAESTR        TEST ESTIMATE RECORD PASSED                  
         BZ    GTDEM130                                                         
         USING ESTRECD,R3                                                       
         CLC   EKEYAM,GDMEDX       TEST CORRECT AGENCY/MEDIA                    
         BNE   GTDEM060                                                         
         CLC   EKEYCLT,GDCLTX      TEST CORRECT CLIENT                          
         BNE   GTDEM060                                                         
         CLC   EKEYPRD,GDPOLPRD    TEST CORRECT PRODUCT                         
         BNE   GTDEM060                                                         
         CLC   EKEYEST,GDESTX      TEST CORRECT ESTIMATE                        
         BE    GTDEM090                                                         
                                                                                
GTDEM060 XC    EKEY,EKEY           FIRST READ BRD ESTIMATE RECORD               
         MVC   EKEYAM,GDMEDX                                                    
         MVC   EKEYCLT,GDCLTX                                                   
         MVC   EKEYEST,GDESTX                                                   
*                                                                               
         CLI   GDPRDX,0            IF NO BRAND                                  
         BE    GTDEM080             READ POL ESTIMATE                           
         CLI   GDPRDX,X'FF'        OR POL BRAND (OR TPOL CLT)                   
         BE    GTDEM080             READ POL ESTIMATE                           
         GOTOR (#EDTPRD,AEDTPRD),DMCB,GDPRDX,FULL,EKEYPRD                       
*                                                                               
GTDEM070 MVC   IOKEY(L'EKEY),EKEY                                               
         ST    R3,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         BNE   GETDEMN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL'                                  
         BNE   GETDEMN                                                          
*                                                                               
         CLC   EKEYPRD,GDPOLPRD    DID WE JUST READ FOR POL EST?                
         BE    GTDEM090            - YES                                        
         MVC   GDESTBTY,EBKTYPE    COPY BRD EST BOOKTYPE &                      
GTDEM080 MVC   EKEYPRD,GDPOLPRD    READ POL ESTIMATE RECORD                     
         B     GTDEM070                                                         
*                                                                               
GTDEM090 CLI   GDESTBTY,0          DID BRAND EST HAVE A BT?                     
         BNE   *+10                YES                                          
         MVC   GDESTBTY,EBKTYPE    NO, USE POL EST BOOKTYPE,                    
*                                                                               
         MVC   EBKTYPE,GDESTBTY    COPY OVER POL EST BT W/BRD EST BT BC         
*                                  -CODE ABOVE DOESN'T RE-READ EST REC          
         GOTOR VDATCON,GDPARM,(0,EEND),(3,GDWORK)                               
         MVC   GDESTLBK,GDWORK     SET LATEST BOOK LIMIT                        
         XC    GDESTDEM,GDESTDEM   CLEAR ESTIMATE DEMO LIST                     
                                                                                
         LA    RF,EDEMLIST         BUILD DEMO LIST FROM ESTIMATE                
         USING EDEMLIST,RF                                                      
         LA    R1,GDESTDEM                                                      
         LHI   R0,EDEMLSTN                                                      
GTDEM100 OC    EDEMLIST,EDEMLIST   TEST END OF LIST                             
         BZ    GTDEM120                                                         
         CLI   EDEMLTYP,33         IGNORE USER DEMOS                            
         BE    GTDEM110                                                         
         CLI   EDEMLTYP,63         AND WEIGHTED DEMOS                           
         BE    GTDEM110                                                         
*                                                                               
* TEMPORARILY IGNORE NON-TRAD DEMOS                                             
*                                                                               
*        CLI   EDEMLNUM,0          AND NON-TRAD DEMOS                           
*        BE    GTDEM110                                                         
*                                                                               
         MVC   0(L'EDEMLIST,R1),EDEMLIST                                        
         AHI   R1,L'EDEMLIST                                                    
GTDEM110 AHI   RF,L'EDEMLIST                                                    
         BCT   R0,GTDEM100                                                      
GTDEM120 MVI   0(R1),FF            SET END OF DEMO LIST                         
         DROP  RF                                                               
                                                                                
GTDEM130 CLI   GDLSTA,0            TEST STATION ALPHA GIVEN                     
         BNE   GTDEM200                                                         
                                                                                
         OC    GDSTABTY,GDSTABTY   TEST STATION FIELDS RESOLVED                 
         BZ    GTDEM140                                                         
         OC    GDSTAMKT,GDSTAMKT   TEST MARKET RESOLVED                         
         BZ    GTDEM140                                                         
         OC    GDSTA,GDSTA         TEST CALL LETTERS PROVIDED                   
         BNZ   GTDEM210                                                         
                                                                                
GTDEM140 OC    GDSTA,GDSTA         TEST CALL LETTERS PROVIDED                   
         BZ    GTDEM150                                                         
         LA    R0,GDSTA                                                         
         STCM  R0,7,GDASTA                                                      
         MVI   GDLSTA,L'GDSTA                                                   
         B     GTDEM200                                                         
                                                                                
GTDEM150 OC    GDSTAX,GDSTAX       TEST STATION CODE SET                        
         BZ    GETDEMN                                                          
                                                                                
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,GDSTAX                                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPMED,C'R'        TEST RADIO                                   
         BE    GTDEM180                                                         
         MVI   STAPQSTA+L'STAPQSTA-1,C' '                                       
         CLC   STAPQNET,SPACES     TEST CABLE CHANNEL SET                       
         BE    *+8                                                              
         MVI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         B     GTDEM190                                                         
                                                                                
GTDEM180 IC    R0,STAPQSTA+L'STAPQSTA-1                                         
         LA    RE,STAPQSTA+L'STAPQSTA-2                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         STC   R0,2(RE)                                                         
         MVI   3(RE),C' '                                                       
                                                                                
GTDEM190 MVI   GDLSTA,L'STAPQSTA+L'STAPQNET                                     
         LA    R0,STAPQSTA                                                      
         STCM  R0,7,GDASTA                                                      
                                                                                
GTDEM200 ICM   RF,7,GDASTA         RF=A(STATION INPUT)                          
         BZ    GETDEMN                                                          
         SR    R0,R0                                                            
         ICM   R0,1,GDLSTA         R0=L'STATION INPUT                           
         BZ    GETDEMN                                                          
         MVI   GDLSTA,0                                                         
         MVC   GDSVPARM,LP_VPARM                                                
         MVI   LP_VPARM,$VALSBTY                                                
         GOTOR (#VALSTA,AVALSTA),GDPARM,(RF),(R0),GDSTAOUT                      
         MVC   LP_VPARM,GDSVPARM                                                
         BNE   GETDEMN                                                          
         MVC   GDSTAX,STAPSTA      SET STATION VALUES                           
         MVC   GDSTABTY,GDSTAOUT+STAPACKL -STA BOOKTYPE                         
         MVC   GDSTAMKT,STAPMKT           -STA MARKET                           
         MVC   GDSTA,STAPQSTA             -CHAR STATION                         
                                                                                
GTDEM210 CLC   =C'DKUI',GDSTA      ALL TEST STATIONS                            
         JE    *+10                                                             
         CLC   =C'DROT',GDSTA                                                   
         JE    *+10                                                             
         CLC   =C'DABC',GDSTA                                                   
         JNE   GTDEM220                                                         
         MVC   GDSTA(4),=C'WABC'   GET CHANGED TO WABC/MARKET 1521              
         MVC   GDSTAMKT,=AL2(1521) FOR DEMO LOOKUPS                             
*                                                                               
GTDEM220 CLI   GDSTABTY,0          STATION HAVE BOOKTYPE?                       
         JNE   GTDEM230             YES, NO NEED TO GET MKT BOOKTYPE            
         LA    R1,IOKEY                                                         
         USING MKTREC,R1           READ MARKET RECORD                           
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         MVC   MKTKMKT,STAPQMKT                                                 
         MVC   MKTKAGY,LP_AGY                                                   
         MVI   MKTKFILL,C'0'       PRE-FILL KEY WITH EBCDIC ZEROES              
         MVC   MKTKFILL+1(L'MKTKFILL-1),MKTKFILL                                
         LA    R1,GDIO             READ MKT REC INTO GDIO                       
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL'                                
         JNE   GTDEM230                                                         
         L     R1,IOADDR                                                        
         MVC   GDSTABTY,MKTBKTYP   SET MARKET BKTYPE AS STATION'S               
         DROP  R1                                                               
*                                                                               
GTDEM230 OC    GDADEM,GDADEM       TEST DEMO LIST PROVIDED                      
         BZ    GTDEM240                                                         
         CLI   GDADEM,0            TEST WMP ENTRY                               
         BE    GTDEM250                                                         
         ICM   RF,7,GDADEM+1       POINT TO WMP ENTRY                           
         BZ    GETDEMN                                                          
         CLI   LW_TYPE-LW_D(RF),LW_TLSTQ                                        
         BNE   GETDEMN             MUST BE A LIST                               
         SR    R1,R1               CONVERT WMP ENTRY TO A LIST                  
         ICM   R1,3,LW_LN-LW_D(RF)                                              
         SHI   R1,LW_LN2Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),LW_DATA2-LW_D(RF)                                        
         LA    R1,1(RF,R1)                                                      
         MVI   0(R1),FF            SET LIST DELIMITER                           
         XC    1(LW_LN2Q-1,R1),1(R1)                                            
         MVI   GDADEM,0                                                         
         B     GTDEM250                                                         
                                                                                
GTDEM240 OC    GDESTDEM,GDESTDEM   TEST ESTIMATE DEMOS SUPPLIED                 
         BZ    GETDEMN                                                          
         LA    R0,GDESTDEM         ELSE POINT TO ESTIMATE DEMO LIST             
         ST    R0,GDADEM                                                        
                                                                                
GTDEM250 OC    GDDAYTIM,GDDAYTIM   AND DAY/TIME EXPRESSION                      
         BZ    GETDEMN                                                          
                                                                                
         OC    GDAOUT,GDAOUT       TEST OUTPUT ADDRESS SET                      
         BNZ   *+12                                                             
         LA    R0,GDDEMV                                                        
         ST    R0,GDAOUT           NO - POINT TO GDDEMV                         
                                                                                
         OC    GDUPGV(GDUPGL),GDUPGV                                            
         BNZ   GTDEM320                                                         
*                                                                               
* PROCESS REGULARY DEMO LOOK-UP, CALL GETDEMF                                   
*                                                                               
         LA    R0,GDIO             REGULAR DEMO LOOK-UPS                        
         ST    R0,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKALST,GDADEM                                                  
         MVC   SPLKAVAL,GDAOUT                                                  
*                                                                               
         LA    R0,GDDEMXTB                                                      
         ST    R0,SPLKXTND                                                      
**                                                                              
GTDEM260 CLI   GD002DPR,C'Y'       GETDEM BLOCK WANTS 2-DEC RTG?                
         BE    GTDEM265            YES                                          
         OC    ATWA,ATWA                                                        
         BZ    GTDEM270                                                         
         L     RF,ATWA                                                          
         CLI   SVS002DP-TWAD(RF),C'Y'                                           
         BNE   GTDEM270                                                         
GTDEM265 OI    SPLKOPT,SPLKOP2D                                                 
**                                                                              
GTDEM270 CLI   GD002DPI,C'Y'       GETDEM BLOCK WANTS 2-DEC IMP?                
         BE    GTDEM275            YES                                          
         OC    ATWA,ATWA                                                        
         BZ    GTDEM280                                                         
         L     RF,ATWA                                                          
         CLI   SVS00A2DI-TWAD(RF),C'Y'                                          
         BNE   GTDEM280                                                         
GTDEM275 OI    GDDEMXTB+(SPXTFLG2-SPLKXTD),SPXTOP2I                             
**                                                                              
GTDEM280 MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKMED,GDMEDA                                                   
         MVC   SPLKSRC,GDCLTSRC                                                 
         MVC   SPLKAGY,LP_AGY                                                   
         MVC   SPLKCLI,GDCLTA                                                   
         MVC   SPLKLBK,GDESTLBK                                                 
*****                                                                           
         OC    GDOFRLBK,GDOFRLBK   IF OFFER HAS A LIMIT BOOK                    
         BZ    GTDEM290                                                         
         CLC   GDOFRLBK,GDESTLBK   USE IT IF IT IS EARLIER THAN EST'S           
         BNL   GTDEM290                                                         
         MVC   SPLKLBK,GDOFRLBK                                                 
*****                                                                           
GTDEM290 MVC   SPLKSTA,GDSTA                                                    
         MVC   SPLKUMK,GDSTAMKT                                                 
         MVC   SPLKDBK,GDBOOK                                                   
         MVC   SPLKDAY(L'GDDAYTIM),GDDAYTIM                                     
*                                                                               
         XR    R0,R0               NEED 1W PROFILE FOR RATINGS BASED            
         ICM   R0,7,GDA1W                       OR IMPRESSIONS BASED            
         ST    R0,SPLKA1W                                                       
*                                                                               
         MVC   SPLKUID,LP_USRID                                                 
*                                                                               
         MVC   SPLKBTYP,GDESTBTY   SET EST BOOKTYPE FIRST (IF ANY)              
         CLI   SPLKBTYP,0          IF EST BOOKTYPE PRESENT                      
         BNE   *+10                THEN IT WINS                                 
         MVC   SPLKBTYP,GDSTABTY   ELSE SET STA/MKT BOOKTYPE                    
*                                                                               
***NOP   CLI   SPLKBTYP,C'A'       *REMOVED TO SUPPORT 2-CHAR BOOKTYPE*         
***NOP   BNL   *+8                                      -HWON 5/1/2019          
***NOP   MVI   SPLKBTYP,0                                                       
*                                                                               
         GOTOR VGETDEM,GDPARM,DEMLKD                                            
         MVC   GDAPROG,SPLKPRG     EXTRACT ACTUAL PROGRAM NAME                  
         MVC   GDABOOK,SPLKABK     EXTRACT ACTUAL BOOK                          
                                                                                
         L     R1,GDADEM           REMOVE SVI VALUES                            
         L     RE,GDAOUT                                                        
         LA    RF,8(RE)                                                         
         LA    RE,4(RE)                                                         
         SR    R0,R0                                                            
GTDEM300 CLI   0(R1),FF                                                         
         BE    GTDEM310                                                         
         MVC   0(4,RE),0(RF)                                                    
         AHI   RE,4                                                             
         AHI   RF,8                                                             
         AHI   R1,L'EDEMLIST                                                    
         AHI   R0,1                                                             
         B     GTDEM300                                                         
                                                                                
GTDEM310 LTR   R1,R0               CLEANSE REMAINDER OF VALUES                  
         BZ    GETDEMY                                                          
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     GETDEMY                                                          
         XC    0(0,RE),0(RE)                                                    
*                                                                               
* PROCESS UPGRADE AND CALL DEMUP                                                
*                                                                               
GTDEM320 SR    R0,R0               DEMO UPGRADES                                
         ICM   R0,1,GDLUPG                                                      
         BZ    GTDEM330                                                         
         MVI   GDLUPG,0                                                         
         XC    GDUPG,GDUPG                                                      
         ICM   RF,7,GDAUPG         VALIDATE UPGRADE EXPRESSION                  
         GOTOR (#VALUPG,AVALUPG),GDPARM,(RF),(R0),GDUPG                         
         BNE   GETDEMN                                                          
                                                                                
GTDEM330 OC    GDUPG,GDUPG                                                      
         BZ    GETDEMN                                                          
         LA    R0,GDIO                                                          
         ST    R0,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,LP_AGY                                                   
         MVC   SPUPMED,GDMEDA                                                   
         MVC   SPUPCLI,GDCLTA                                                   
         MVC   SPUPSTA,GDSTA                                                    
         MVC   SPUPDAY(L'GDDAYTIM),GDDAYTIM                                     
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,GDCLTSRC                                                 
         MVI   SPUPTPTT,C'T'                                                    
         MVC   SPUP2YRP,VUFPUTF                                                 
         MVC   SPUP2YRR,VUFRTGF                                                 
*                                                                               
         MVC   SPUPBTYP,GDESTBTY   SET EST BOOKTYPE FIRST (IF ANY)              
         CLI   SPUPBTYP,0          IF EST BOOKTYPE PRESENT                      
         BNE   *+10                THEN IT WINS                                 
         MVC   SPUPBTYP,GDSTABTY   ELSE SET STA/MKT BOOKTYPE                    
*                                                                               
         CLI   VUFBKTY,0           TEST BOOK TYPE OVERRIDE                      
         BE    *+10                                                             
         MVC   SPUPBTYP,VUFBKTY                                                 
*                                                                               
***NOP   CLI   SPUPBTYP,C'A'       *REMOVED TO SUPPORT 2-CHAR BOOKTYPE*         
***NOP   BNL   *+8                                      -HWON 5/1/2019          
***NOP   MVI   SPUPBTYP,0                                                       
         MVC   SPUPFBK,GDBOOK                                                   
         MVC   SPUPUDAY(L'VUFUPDT),VUFUPDT                                      
         MVC   SPUPTYPE(L'VUFUPGD),VUFUPGD                                      
         L     RF,ATWA                                                          
         MVI   SPUPOPTS,SPOPEXT                                                 
         CLI   SVS002DP-TWAD(RF),C'Y'                                           
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOP2DEC                                                
         MVC   SPUPMKT,GDSTAMKT                                                 
         MVC   SPUPUID,LP_USRID                                                 
         GOTOR VDEMUP,GDPARM,SPDEMUPD,GDADEM,GDAOUT                             
         MVC   GDAPROG,SPUPPRG     EXTRACT PROGRAM NAME                         
         MVC   GDABOOK,SPUPFBK     EXTRACT ACTUAL BOOK                          
                                                                                
GETDEMY  MVC   IOVALS(IOVALL),GDIOSAVE                                          
         J     EXITY                                                            
                                                                                
GETDEMN  MVC   IOVALS(IOVALL),GDIOSAVE                                          
         J     EXITN                                                            
         DROP  R2,R3,RB,RC                                                      
                                                                                
         LTORG                                                                  
GDPOLPRD DC    C'POL'              FOR ESTIMATE KEY                             
         DS    0H                                                               
                                                                                
GDWORKD  DSECT                     ** GETDEM LOCAL W/S **                       
GDPARM   DS    6F                                                               
GDWORK   DS    XL64                                                             
GDSVPARM DS    XL(L'LP_VPARM)                                                   
GDIOSAVE DS    XL(IOVALL)                                                       
GDSTAOUT DS    XL(STAPACKL+L'SBKTYPE)                                           
GDDEMLKB DS    XL(L'SPDEMLK)                                                    
GDDEMXTB DS    XL(L'SPXTAREA)                                                   
GDDEMUPB DS    XL(SPDEMUP2)                                                     
GDIO     DS    XL2000                                                           
GDWORKL  EQU   *-GDWORKD                                                        
SPLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT REP CODE                                                       *         
***********************************************************************         
                                                                                
EDTREP   J     *+12                                                             
         DC    C'*EDTREP*'                                                      
         LR    RB,RF                                                            
         USING EDTREP,RB                                                        
         LM    R2,R4,0(R1)                                                      
         OC    0(2,R2),0(R2)                                                    
         BNZ   *+14                                                             
         XC    4(4,R1),4(R1)                                                    
         JZ    EXITY                                                            
         LHI   R3,3                                                             
         STCM  R3,15,4(R1)                                                      
         GOTOR VRCPACK,DMCB,(C'U',(R2)),(R4)                                    
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*** CODE IS DEFUNCT, SHOULD BE REMOVED *** HWON 12/4/2013                       
*&&DO                                                                           
***********************************************************************         
* VALIDATE MAKEGOOD CODE                                              *         
***********************************************************************         
                                                                                
VALMGC   J     *+12                                                             
         DC    C'*VALMGC*'                                                      
         LR    RB,RF                                                            
         USING VALMGC,RB                                                        
         LM    R2,R4,0(R1)                                                      
         CHI   R3,2                                                             
         JNE   EXITN                                                            
                                                                                
         BASR  R5,0                                                             
         AHI   R5,MGCTAB-*                                                      
         LHI   R6,MGCTABL                                                       
                                                                                
VALMGC02 CLC   0(1,R2),0(R5)                                                    
         BE    VALMGC04                                                         
         AHI   R5,1                                                             
         BCT   R6,VALMGC02                                                      
         J     EXITN                                                            
                                                                                
VALMGC04 LHI   R0,MGCTABL                                                       
         SR    R0,R6                                                            
         MHI   R0,10                                                            
         MVC   0(1,R4),1(R2)                                                    
         NI    0(R4),X'0F'                                                      
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         AHI   R1,1                                                             
         AR    R0,R1                                                            
         STC   R0,0(R4)                                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT MAKEGOOD CODE                                                  *         
***********************************************************************         
                                                                                
EDTMGC   J     *+12                                                             
         DC    C'*EDTMGC*'                                                      
         LR    RB,RF                                                            
         USING EDTMGC,RB                                                        
         LM    R2,R4,0(R1)                                                      
         LR    R5,R1                                                            
                                                                                
         MVC   0(1,R4),0(R2)       EXTRACT INPUT VALUE                          
         NI    0(R4),FF-X'80'      TURN OFF HOB                                 
         SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         JNZ   *+14                                                             
         XC    4(4,R5),4(R5)       CLEAR OUTPUT LENGTH IF NO MAKEGOOD           
         J     EXITY                                                            
                                                                                
         BCTR  R0,0                                                             
         SRDA  R0,32                                                            
         LHI   RE,10                                                            
         DR    R0,RE                                                            
         BASR  RE,0                                                             
         AHI   RE,MGCTAB-*                                                      
         IC    RF,0(RE,R1)                                                      
         STC   RF,0(R4)                                                         
         STC   R0,1(R4)                                                         
         OI    1(R4),X'F0'                                                      
         LHI   R0,2                                                             
         STCM  R0,15,4(R5)                                                      
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*** CODE IS DEFUNCT, SHOULD BE REMOVED *** HWON 12/4/2013                       
*&&                                                                             
                                                                                
***********************************************************************         
* VALIDATE SPOT LENGTH (MEDIA MUST BE VALIDATED FIRST)                *         
***********************************************************************         
                                                                                
VALSLN   J     *+12                                                             
         DC    C'*VALSLN*'                                                      
         LR    RB,RF                                                            
         USING VALSLN,RB                                                        
         USING SLWORKD,RC                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         CHI   R3,0                HAVE INPUT?                                  
         JNH   VALSLN_X                                                         
                                                                                
         MVI   SLBINLEN,0          INIT BINARY SPOT LENGTH                      
         MVC   0(L'SLBINLEN,R4),SLBINLEN                                        
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MVC   SLWORK(6),=6X'F0'                                                
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVZ   SLWORK(0),0(R2)                                                  
         CLC   SLWORK(6),=6X'F0'                                                
         JNE   VALSLN_E                                                         
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  SLDUB1,0(0,R2)                                                   
         CVB   R1,SLDUB1                                                        
         LTR   R1,R1                                                            
         JZ    VALSLN_E                                                         
         CHI   R1,255                                                           
         JH    VALSLN_E                                                         
         STC   R1,SLBINLEN         VALIDATED BINARY LENGTH                      
         MVC   0(L'SLBINLEN,R4),SLBINLEN                                        
                                                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               POINT TO END OF TABLE                        
         AHI   R1,6                POINT TO FIRST ENTRY                         
                                                                                
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMEDA,C'T'                                                       
         JE    VALSLN44                                                         
         CLI   QMEDA,C'N'                                                       
         JE    VALSLN44                                                         
         CLI   QMEDA,C'C'                                                       
         JE    VALSLN44                                                         
                                                                                
         LA    R0,C'R'                                                          
         CLI   QMEDA,C'R'                                                       
         JE    VALSLN44                                                         
         CLI   QMEDA,C'X'                                                       
         JE    VALSLN44                                                         
         DC    H'0'                                                             
                                                                                
VALSLN44 CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         JE    VALSLN46                                                         
         CLC   LP_AGY,0(R1)        ELSE MATCH AGY                               
         JNE   *+12                                                             
VALSLN46 CLM   R0,1,2(R1)          AND MEDIA                                    
         JE    VALSLN50                                                         
*                                                                               
         BXLE  R1,RE,VALSLN44      NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VALSLN50 AHI   R1,4                POINT BEYOND HEADER                          
         LLC   RE,SLBINLEN         GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         JE    VALSLN_E            ZERO IS INVALID                              
         MVC   SLVALBLN,1(RE)      VALIDATED BINARY SPOT LENGTH IN TAB          
                                                                                
         J     VALSLN_X                                                         
                                                                                
VALSLN_X J     EXITY                                                            
VALSLN_E J     EXITN                                                            
         DROP  RB,RC                                                            
                                                                                
         LTORG                                                                  
SLWORKD  DSECT                     ** SPOT LENGTH LOCAL W/S **                  
SLDUB1   DS    D                                                                
SLWORK   DS    XL64                                                             
SLBINLEN DS    X                   BINARY INPUT SPOT LENGTH                     
SLVALBLN DS    X                   VALIDATED BINARY SPOT LENGTH                 
SLWORKL  EQU   *-SLWORKD                                                        
SPLNK02  CSECT                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* TRANSLATE BOOKTYPE TO DEMO INTERNAL 1 CHAR BOOKTYPE                 *         
***********************************************************************         
                                                                                
TRNSBT   J     *+12                                                             
         DC    C'*TRNSBT*'                                                      
         LR    RB,RF                                                            
         USING TRNSBT,RB                                                        
         LM    R2,R5,0(R1)                                                      
         CHI   R3,2                                                             
         JH    EXITN                                                            
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   R6,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            LENGTH OF EACH ENTRY                         
         CHI   R5,12               TRANSLATE 1 TO 2 CHAR BOOKTYPE?              
         JE    TRNSBT40                                                         
         USING SPBKTYPD,R6                                                      
                                                                                
TRNSBT10 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT20                                                         
         MVI   0(R4),X'FF'         IF INVALID BOOKTYPE THEN RETURN              
         J     TRNSBTX             PASS BACK INVALID INDICATOR                  
                                                                                
TRNSBT20 OC    0(2,R2),=X'4040'    PAD INPUT WITH SPACES                        
         CLC   SPBKTYPA,0(R2)      IS BOOKTYPE IN TABLE?                        
         JE    TRNSBT30                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT10                                                         
TRNSBT30 MVC   0(L'SPBKTYPN,R4),SPBKTYPN  MOVE INTERNAL DEMO BKTYP              
         J     TRNSBTX                                                          
                                                                                
                                                                                
*************************************************************                   
* TRANSLATE FROM 1 CHARACTER INTERNAL TO 2 CHARCTER FORMAT  *                   
*************************************************************                   
TRNSBT40 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT50                                                         
         MVI   0(R4),X'FF'         IF INVALID BOOKTYPE THEN RETURN              
         J     TRNSBTX             INVALID INDICATOR                            
                                                                                
TRNSBT50 CLC   SPBKTYPN,0(R2)      IS 1 CHARACTER BOOKTYPE IN TABLE?            
         JE    TRNSBT60                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT40                                                         
TRNSBT60 MVC   0(L'SPBKTYPA,R4),SPBKTYPA  MOVE 2 CHARCTER BOOKTYPE              
*                                                                               
TRNSBTX  J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
MGCTAB   DC    C'ABCDEFGHJK'                                                    
MGCTABL  EQU   *-MGCTAB                                                         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
DEMLKD   DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
       ++INCLUDE SPDEMLKXTD                                                     
       ++INCLUDE SPDEMUPD                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
STASTAL  EQU   L'STAPQSTA+L'STAPQNET                                            
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPLNK02   11/19/19'                                      
         END                                                                    
