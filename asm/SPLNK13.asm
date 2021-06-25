*          DATA SET SPLNK13    AT LEVEL 003 AS OF 09/12/17                      
*PHASE T21E13A                                                                  
SPLNK13  TITLE '- SPOT STEWARD - BRAND ALLOCATION UPLOAD'                       
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=SPTSYSQ,LINKIO=Y,     *        
               WORKERKEY=SPAU,RLEN=512,BLOCKS=(B#SAVED,SAVED)                   
         EJECT                                                                  
CODE     NMOD1 0,**SL13**,RR=RE                                                 
         USING LP_D,R1             R1=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
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
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
         GOTOR UPDATE              FLUSH ANY PENDING I/O                        
         J     EXITY                                                            
                                                                                
INIT     XC    LBUYKEY,LBUYKEY     CLEAR LAST BUY KEY                           
         MVI   FLAGS,0             .......... CONTROL FLAGS                     
         MVI   ALCTAB,ALCTEOTQ     .......... ALLOCATION TABLE                  
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS AN UPLOAD RECORD                                            *         
***********************************************************************         
                                                                                
INPUT    BASR  RF,0                                                             
         AHI   RF,RECTAB-*                                                      
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE             
         BE    *+12                                                             
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         MVC   RECTYPE,RECTTYPE    SET RECORD TYPE                              
         DROP  R1                                                               
                                                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#UPLRSP)               
                                                                                
         GOTOR VALSPT              VALIDATE & POST THE SPOT                     
         JNE   EXITY                                                            
                                                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLRSP),0,0           
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SPOT AND BUILD ALLOCATION TABLE ENTRY           *         
***********************************************************************         
                                                                                
VALSPT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    SPDVALS(SPDVALL),SPDVALS                                         
         NI    FLAGS,FF-FSPTERR    SET NO ERRORS FOR SPOT                       
         MVC   XTRATEXT,SPACES     INITIALIZE EXTRA MESSAGE TEXT                
         OC    CLTA,CLTA           CLEAR SAVED VALUES IF CLIENT GIVEN           
         BZ    *+10                                                             
         XC    SVVALS(SVVALL),SVVALS                                            
                                                                                
NEW      USING BUYKEY,CBUYKEY      BUILD NEW BUY KEY FROM INPUT DATA            
         XC    NEW.BUYKEY,NEW.BUYKEY                                            
                                                                                
         CLI   MEDA,0              TEST MEDIA CODE PRESENT                      
         BE    VALSPT02                                                         
         CLI   MEDA,CBLMEDQ        TEST CABLE MEDIA                             
         BNE   *+8                                                              
         MVI   MEDA,NETMEDQ        YES - IT'S REALLY CANADIAN NETWORK           
         GOTOR (#VALMED,AVALMED),DMCB,MEDA,0,SVAGYM                             
         JNE   VALERR01                                                         
         MVC   SVMEDA,MEDA                                                      
VALSPT02 MVC   NEW.BUYKAM,SVAGYM                                                
         CLI   NEW.BUYKAM,0                                                     
         JE    VALERR02                                                         
                                                                                
         OC    CLTA,CLTA           TEST CLIENT GIVEN                            
         BZ    VALSPT04                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,CLTA,L'CLTA,SVCLTX                        
         JNE   VALERR03                                                         
         OI    FLAGS,FCLTGET       SET CLIENT RECORD GOT                        
VALSPT04 MVC   NEW.BUYKCLT,SVCLTX                                               
         OC    NEW.BUYKCLT,NEW.BUYKCLT                                          
         JZ    VALERR04                                                         
                                                                                
         MVI   NEW.BUYKPRD,POLBRDQ SET POOL BRAND                               
                                                                                
         CLI   SVMEDA,NETMEDQ      TEST CANADIAN NETWORK MEDIA                  
         BE    VALSPT06            YES - NETWORK BUYS HAVE NO MARKET            
         OC    MRKT,MRKT           TEST MARKET GIVEN                            
         BZ    *+10                                                             
         MVC   SVMRKT,MRKT                                                      
         MVC   NEW.BUYKMKTN,SVMRKT                                              
         OC    NEW.BUYKMKTN,NEW.BUYKMKTN                                        
         JZ    VALERR05                                                         
                                                                                
VALSPT06 OC    STAC,STAC           TEST STATION GIVEN                           
         BZ    VALSPT08                                                         
         USING STAPACKD,WORK                                                    
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,TWAAGY                                                   
         MVC   STAPMED,SVMEDA                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         SR    R0,R0                                                            
         ICM   R0,3,NEW.BUYKMKTN                                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  STAPQMKT,DUB                                                     
         MVC   STAPQSTA,STAC                                                    
         CLI   SVMEDA,NETMEDQ      TEST CANADIAN NETWORK MEDIA                  
         BNE   *+8                                                              
         MVI   STAPQSTA+L'STAPQSTA-1,NETMEDQ                                    
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JNE   VALERR06                                                         
         MVC   SVSTAC,STAPSTA                                                   
VALSPT08 MVC   NEW.BUYKSTAC,SVSTAC                                              
         OC    NEW.BUYKSTAC,NEW.BUYKSTAC                                        
         JZ    VALERR07                                                         
                                                                                
         CLI   ESTN,0              TEST ESTIMATE NUMBER GIVEN                   
         BE    *+10                                                             
         MVC   SVESTN,ESTN                                                      
         MVC   NEW.BUYKEST,SVESTN                                               
         CLI   NEW.BUYKEST,0                                                    
         JE    VALERR08                                                         
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,LINE           USE LINE NUMBER IF GIVEN                     
         BNZ   *+8                                                              
         IC    R0,SVLINE           ELSE USE PREVIOUS LINE NUMBER                
         STC   R0,SVLINE                                                        
         MVC   NEW.BUYKBUY+1(1),SVLINE                                          
                                                                                
         MVI   NEW.BUYKBUY+2,1     SET TO READ ACTIVE BUY POINTER               
                                                                                
         OC    SPTD,SPTD           TEST SPOT DATE GIVEN                         
         BZ    *+10                                                             
         MVC   SVSPTD,SPTD                                                      
         MVC   SPTD,SVSPTD                                                      
         OC    SPTD,SPTD                                                        
         JZ    VALERR09                                                         
                                                                                
         TM    FLAGS,FCLTGET       TEST CLIENT RECORD GOT                       
         BNZ   VALSPT10                                                         
         OI    FLAGS,FCLTGET       SET CLIENT RECORD GOT                        
         MVC   QMEDX,SVAGYM                                                     
         MVC   QCLTX,SVCLTX                                                     
         GOTOR (#GETCLT,AGETCLT)   NO - READ THE CLIENT RECORD                  
         BE    VALSPT10                                                         
         DC    H'0'                CAN'T READ CLIENT RECORD                     
                                                                                
VALSPT10 LA    R2,BRDCS            CONVERT BRAND CODES INTO NUMBERS             
         LHI   R0,BRDCN                                                         
         LA    R3,BRDNS                                                         
VALSPT12 XC    0(L'RPPRD,R3),0(R3)                                              
         OC    0(L'BRDCS,R2),0(R2) TEST THIS BRAND SET                          
         BZ    VALSPT14                                                         
         GOTOR (#VALPRD,AVALPRD),DMCB,(R2),0,(R3)                               
         BE    VALSPT14                                                         
         MVC   XTRATEXT(L'PKEYPRD),0(R2)                                        
         GOTOR PUTERR,SE#INVP2     SEND INVALID BRAND MESSAGE                   
VALSPT14 AHI   R2,L'BRDCS          BUMP TO NEXT BRAND CODE                      
         AHI   R3,L'RPPRD          BUMP TO NEXT BRAND NUMBER                    
         BCT   R0,VALSPT12         DO FOR NUMBER OF BRANDS                      
                                                                                
         OC    LBUYKEY,LBUYKEY     TEST HAVE BUY KEY SAVED                      
         BZ    VALSPT16                                                         
         CLC   NEW.BUYKEY,LBUYKEY  YES - TEST CHANGE OF BUY KEY                 
         BE    VALSPT18            NO - UPDATE ALLOCATION TABLE                 
                                                                                
         GOTOR UPDATE              UPDATE SAVED BUY WITH ALLOCATIONS            
                                                                                
VALSPT16 MVC   LMEDA,SVMEDA        SET MEDIA FOR LAST BUY RECORD                
         MVI   SVSPTR,0            RESET SPOT REFERENCE NUMBER                  
         LA    R1,ALCTAB           RESET ALLOCATION POINTER/TABLE               
         ST    R1,ANXTALC                                                       
         MVI   0(R1),ALCTEOTQ                                                   
                                                                                
         MVC   IOKEY(L'BUYKEY),NEW.BUYKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOSPTDIR+IO5'                          
         JNE   VALERR10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO5'                        
         JNE   VALERR11                                                         
         MVC   LBUYKEY,NEW.BUYKEY  SAVE BUY KEY                                 
                                                                                
VALSPT18 SR    RF,RF                                                            
         ICM   RF,1,SPTR           USE SPOT REFERENCE GIVEN                     
         BNZ   *+12                                                             
         IC    RF,SVSPTR           BUMP SAVED REFERENCE IF NOT                  
         AHI   RF,1                                                             
         STC   RF,SVSPTR           SET SPOT REFERENCE NUMBER                    
         MVC   SPTR,SVSPTR                                                      
                                                                                
         L     R2,AIO5             LOCATE SPOT ELEMENT ON BUY RECORD            
         USING BUYREC,R2                                                        
         MVC   SPOTTSEC,BDSEC      SET TOTAL SPOT LENGTH                        
                                                                                
         LA    R2,BDELEM                                                        
         USING REGELEM,R2          R2=A(SPOT ELEMENT)                           
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    RF,SPTR             RF=SPOT REFERENCE NUMBER                     
                                                                                
VALSPT20 IC    R0,RLEN             BUMP TO NEXT ELEMENT ON RECORD               
         AR    R2,R0                                                            
         CLI   RCODE,0             TEST END OF RECORD                           
         JE    VALERR12                                                         
         CLI   RCODE,RCPOLOQ       TEST FOR SPOT ELEMENTS                       
         BL    VALSPT20                                                         
         CLI   RCODE,RCPOLFQ                                                    
         BH    VALSPT20                                                         
         CLC   RDATE,SPTD          MATCH ON SPOT DATE                           
         BNE   VALSPT20                                                         
         TM    RSTATUS,RSMINUSQ+RSHIATSQ                                        
         BNZ   VALSPT20                                                         
                                                                                
         CLI   RLEN,RLPOL1LQ       IS THIS A SPOD ELEMENT                       
         BNE   VALSPT22                                                         
         CLI   RPTIME,0                                                         
         BE    VALSPT22                                                         
         CLC   RPTIME,SPOTTSEC                                                  
         BE    VALSPT22                                                         
                                                                                
         CLI   RPPRD,0             YES IT IS                                    
         BNE   *+6                                                              
         DC    H'0'                SPOD MUST HAVE A BRAND ALLOCATION            
         CLC   SPDTLEN,SPOTTSEC    TEST PREVIOUS SPOD EXHAUSTED                 
         BNE   *+10                                                             
         XC    SPDVALS(SPDVALL),SPDVALS                                         
         SR    R0,R0                                                            
         IC    R0,RPTIME                                                        
         SR    RE,RE                                                            
         IC    RE,SPDTLEN          UPDATE TOTAL SPOD LENGTH                     
         AR    RE,R0                                                            
         CLM   RE,1,SPOTTSEC                                                    
         BNH   *+6                                                              
         DC    H'0'                SPOD TOTAL EXCEEDS BUY SECONDS               
         STC   RE,SPDTLEN                                                       
         IC    RE,SPDCOUNT         UPDATE SPOD COUNT                            
         AHI   RE,1                                                             
         CHI   RE,BRDN                                                          
         BNH   *+6                                                              
         DC    H'0'                TOO MANY SPODS                               
         STC   RE,SPDCOUNT                                                      
         CHI   RE,1                                                             
         BE    VALSPT24                                                         
         B     VALSPT20                                                         
                                                                                
VALSPT22 XC    SPDVALS(SPDVALL),SPDVALS                                         
                                                                                
VALSPT24 BCT   RF,VALSPT20         LOOP UNTIL CORRECT ELEMENT LOCATED           
                                                                                
         CLI   SPDCOUNT,0          TEST OLD WAS A SPOD                          
         BNE   VALSPT26                                                         
         TM    RSTATUS,RSMINSDQ    TEST MISSED SPOT                             
         JNZ   VALERR13                                                         
         TM    RSTATUS,RSNOALLQ    TEST ALLOCATION NOT ALLOWED                  
         JNZ   VALERR14                                                         
         OC    RPAY,RPAY           TEST NOT PAID                                
         JNZ   VALERR18                                                         
         LA    R1,REGELEM                                                       
         IC    R0,RLEN                                                          
         AR    R1,R0                                                            
         CLI   0(R1),ACCODEQ       TEST NOT INVOICE MATCHED                     
         JE    VALERR19                                                         
         B     VALSPT32                                                         
                                                                                
VALSPT26 LA    R1,REGELEM          HANDLE SPODS                                 
SPOD     USING REGELEM,R1                                                       
         SR    RF,RF               RF=SPOD TOTAL SECONDS LENGTH                 
VALSPT28 TM    SPOD.RSTATUS,RSMINSDQ                                            
         JNZ   VALERR13                                                         
         TM    SPOD.RSTATUS,RSNOALLQ                                            
         JNZ   VALERR14                                                         
         OC    SPOD.RPAY,SPOD.RPAY                                              
         JNZ   VALERR18                                                         
         IC    R0,SPOD.RPTIME                                                   
         AR    RF,R0               RF=TOTAL SPOD LENGTH SO FAR                  
         IC    R0,SPOD.RLEN                                                     
         AR    R1,R0                                                            
         CLI   0(R1),ACCODEQ       TEST NOT INVOICE MATCHED                     
         JE    VALERR19                                                         
         CLM   RF,1,SPOTTSEC                                                    
         BE    VALSPT32                                                         
         BNH   VALSPT30                                                         
         DC    H'0'                INVALID SPOD                                 
                                                                                
VALSPT30 CLI   SPOD.RCODE,0        TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SPOD.RCODE,RCPOLOQ  LOOK FOR NEXT SPOT ELEMENT                   
         BL    *+12                                                             
         CLI   SPOD.RCODE,RCPOLFQ                                               
         BNH   VALSPT28                                                         
         IC    R0,SPOD.RLEN                                                     
         AR    R1,R0                                                            
         B     VALSPT30                                                         
         DROP  SPOD                                                             
                                                                                
VALSPT32 GOTOR SETLEN              SET UNSENT SPOT LENGTHS                      
         JNE   VALERR17            INVALID SPOT LENGTH ON ERROR                 
                                                                                
         GOTOR CHKOLD              CHECK OLD ALLOCATIONS VALID                  
         JL    VALERR15            OLD BRAND MISMATCH                           
         JH    VALERR16            OLD LENGTH MISMATCH                          
                                                                                
         TM    FLAGS,FSPTERR       TEST ANY SPOT ERRORS ENCOUNTERED             
         JNZ   EXITN                                                            
                                                                                
         L     R1,ANXTALC          ADD ENTRY TO SPOT ALLOCATION TABLE           
         USING ALCTABD,R1                                                       
         MVC   ALCTSPTD,SPTD       SET SPOT DATE                                
         MVC   ALCTSPTR,SPTR       SET SPOT REFERENCE NUMBER                    
         MVC   ALCTTYPE,RECTYPE    SET SPOT RECORD TYPE                         
         MVC   ALCTBRDS,B1NN       SET BRAND ALLOCATIONS                        
         MVC   ALCTLENS,B1NL       SET SPOT LENGTHS                             
         AHI   R1,ALCTABL                                                       
         ST    R1,ANXTALC          SET POINTER TO NEXT TABLE ENTRY              
         MVI   ALCTABD,ALCTEOTQ    SET NEW END OF TABLE                         
         J     EXITY                                                            
         DROP  NEW,R1,R2,RB                                                     
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPOT VALIDATION ERROR HANDLING                                      *         
***********************************************************************         
                                                                                
VALERR01 MVC   XTRATEXT(L'MEDA),MEDA                                            
         GOTOR PUTERR,SE#INVMD     INVALID MEDIA                                
         J     EXITN                                                            
                                                                                
VALERR02 GOTOR PUTERR,SE#MSGMD     MISSING MEDIA                                
         J     EXITN                                                            
                                                                                
VALERR03 MVC   XTRATEXT(L'CLTA),CLTA                                            
         GOTOR PUTERR,SE#INCLT     INVALID CLIENT                               
         J     EXITN                                                            
                                                                                
VALERR04 GOTOR PUTERR,SE#MSGCL     MISSING CLIENT                               
         J     EXITN                                                            
                                                                                
VALERR05 GOTOR PUTERR,SE#MSGMK     MISSING MARKET                               
         J     EXITN                                                            
                                                                                
VALERR06 MVC   XTRATEXT(L'STAC),STAC                                            
         GOTOR PUTERR,SE#INVST     INVALID STATION/NETWORK                      
         J     EXITN                                                            
                                                                                
VALERR07 GOTOR PUTERR,SE#MSGSN     MISSING STATION/NETWORK                      
         J     EXITN                                                            
                                                                                
VALERR08 GOTOR PUTERR,SE#MSGES     MISSING ESTIMATE                             
         J     EXITN                                                            
                                                                                
VALERR09 GOTOR PUTERR,SE#MSGSD     MISSING SPOT DATE                            
         J     EXITN                                                            
                                                                                
VALERR10 GOTOR PUTERR,SE#KEYNF     (BUY) KEY NOT FOUND                          
         J     EXITN                                                            
                                                                                
VALERR11 GOTOR PUTERR,SE#RECNF     (BUY) RECORD NOT FOUND                       
         J     EXITN                                                            
                                                                                
VALERR12 GOTOR PUTERR,SE#SELNF     SPOT NOT FOUND                               
         J     EXITN                                                            
                                                                                
VALERR13 GOTOR PUTERR,SE#SPMIS     SPOT HAS BEEN MISSED                         
         J     EXITN                                                            
                                                                                
VALERR14 GOTOR PUTERR,SE#NOALC     SPOT ALLOCATION NOT ALLOWED                  
         J     EXITN                                                            
                                                                                
VALERR15 GOTOR PUTERR,SE#OALCM     OLD ALLOCATION MISMATCH - BRAND              
         J     EXITN                                                            
                                                                                
VALERR16 GOTOR PUTERR,SE#OALCL     OLD ALLOCATION MISMATCH - LENGTH             
         J     EXITN                                                            
                                                                                
VALERR17 GOTOR PUTERR,SE#INVSL     INVALID SPOT LENGTH                          
         J     EXITN                                                            
                                                                                
VALERR18 GOTOR PUTERR,SE#SISPD     SPOT IS PAID                                 
         J     EXITN                                                            
                                                                                
VALERR19 GOTOR PUTERR,SE#SISMD     SPOT IS MATCHED                              
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE BUY RECORD(S) WITH SPOT ALLOCATIONS               *         
***********************************************************************         
                                                                                
UPDATE   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR UPDBUY,AIO5         UPDATE BUY RECORD & WRITE IT BACK            
         BNE   UPDATEX                                                          
                                                                                
         CLI   LMEDA,NETMEDQ       TEST CANADIAN NETWORK MEDIA                  
         BNE   UPDATEX                                                          
                                                                                
         L     R2,AIO5             YES - UPDATE EXPLODED BUYS TOO               
         AHI   R2,BDELEM-BUYREC                                                 
         USING NTWKELEM,R2         R2=A(NETWORK ELEMENT)                        
         SR    R0,R0                                                            
                                                                                
UPDATE02 IC    R0,NTWKLEN          LOCATE FIRST/NEXT NETWORK ELEMENT            
         AR    R2,R0                                                            
         CLI   NTWKCODE,0          TEST END OF RECORD                           
         BE    UPDATEX                                                          
         CLI   NTWKCODE,NTWKCODQ   TEST NETWORK POINTER ELEMENT                 
         BNE   UPDATE02                                                         
                                                                                
SELMED   USING BUYKEY,IOKEY                                                     
         MVC   SELMED.BUYKEY,LBUYKEY                                            
         MVC   SELMED.BUYKMSTA,NTWKMKST                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOSPTDIR+IO6'                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO6'                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR UPDBUY,AIO6         UPDATE BUY RECORD & WRITE IT BACK            
                                                                                
         B     UPDATE02            GET NEXT NETWORK ELEMENT                     
                                                                                
UPDATEX  J     EXITY                                                            
         DROP  R2,RB,SELMED                                                     
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE BUY RECORD WITH BRAND ALLOCATIONS                 *         
***********************************************************************         
                                                                                
UPDBUY   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   IOADDR,0(R1)        SET I/O AREA ADDRESS                         
         LA    R3,ALCTAB                                                        
         USING ALCTABD,R3                                                       
         XC    IOBRDLST,IOBRDLST   CLEAR BRANDS ALLOCATED LIST                  
         NI    FLAGS,FF-FBUYUPD    SET BUY RECORD NOT UPDATED                   
         SR    R0,R0                                                            
         XC    SPDVALS(SPDVALL),SPDVALS                                         
                                                                                
UPDBUY02 CLI   ALCTABD,ALCTEOTQ    TEST END OF ALLOCATION TABLE                 
         BE    UPDBUY30                                                         
         L     R2,IOADDR           SET IOADDR FOR I/O ROUTINE                   
         AHI   R2,BDELEM-BUYREC                                                 
         USING REGELEM,R2          R2=A(SPOT ELEMENT)                           
         SR    RF,RF                                                            
         ICM   RF,1,ALCTSPTR                                                    
                                                                                
UPDBUY04 IC    R0,RLEN                                                          
         AR    R2,R0                                                            
         CLI   RCODE,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RCODE,RCPOLOQ       TEST FOR SPOT ELEMENTS                       
         BL    UPDBUY04                                                         
         CLI   RCODE,RCPOLFQ                                                    
         BH    UPDBUY04                                                         
         CLC   RDATE,ALCTSPTD      MATCH ON SPOT DATE                           
         BNE   UPDBUY04                                                         
         TM    RSTATUS,RSMINUSQ+RSHIATSQ                                        
         BNZ   UPDBUY04                                                         
                                                                                
         CLI   RLEN,RLPOL1LQ       IS THIS A SPOD ELEMENT                       
         BNE   UPDBUY06                                                         
         CLI   RPTIME,0                                                         
         BE    UPDBUY06                                                         
         CLC   RPTIME,SPOTTSEC                                                  
         BE    UPDBUY06                                                         
                                                                                
         CLI   RPPRD,0             YES IT IS                                    
         BNE   *+6                                                              
         DC    H'0'                SPOD MUST HAVE A BRAND ALLOCATION            
         CLC   SPDTLEN,SPOTTSEC    TEST PREVIOUS SPOD EXHAUSTED                 
         BNE   *+10                                                             
         XC    SPDVALS(SPDVALL),SPDVALS                                         
         SR    R0,R0                                                            
         IC    R0,RPTIME                                                        
         SR    RE,RE                                                            
         IC    RE,SPDTLEN          UPDATE TOTAL SPOD LENGTH                     
         AR    RE,R0                                                            
         CLM   RE,1,SPOTTSEC                                                    
         BNH   *+6                                                              
         DC    H'0'                SPOD TOTAL EXCEEDS BUY SECONDS               
         STC   RE,SPDTLEN                                                       
         IC    RE,SPDCOUNT         UPDATE SPOD COUNT                            
         AHI   RE,1                                                             
         CHI   RE,6                                                             
         BNH   *+6                                                              
         DC    H'0'                TOO MANY SPODS                               
         STC   RE,SPDCOUNT                                                      
         CHI   RE,1                                                             
         BE    UPDBUY08                                                         
         B     UPDBUY04                                                         
                                                                                
UPDBUY06 XC    SPDVALS(SPDVALL),SPDVALS                                         
                                                                                
UPDBUY08 BCT   RF,UPDBUY04         LOOP UNTIL CORRECT ELEMENT LOCATED           
                                                                                
         LA    R1,ALCTBRDS         ADD BRANDS TO BRAND LIST                     
         SR    RF,RF               AND COUNT NUMBER OF ALLOCATIONS              
         LHI   R0,BRDN                                                          
UPDBUY10 CLI   0(R1),0             TEST END OF BRAND LIST                       
         BE    UPDBUY12                                                         
         GOTOR ADDBRD                                                           
         AHI   R1,L'RPPRD                                                       
         AHI   RF,1                                                             
         BCT   R0,UPDBUY10                                                      
                                                                                
UPDBUY12 STC   RF,ALC#BRDS         SAVE # OF ALLOCATION BRANDS                  
                                                                                
         CLI   SPDCOUNT,0          TEST OLD ALLOCATION WAS REGULAR              
         BNE   *+12                                                             
         CLI   ALCTTYPE,RECTTREG   AND NEW ALLOCATION IS TOO                    
         BE    UPDBUY24                                                         
                                                                                
                                                                                
         MVC   SVSTATUS,RSTATUS    SAVE STATUS & RATE OVERRIDE                  
         MVC   SVCOST,RPCOST                                                    
         LA    R1,REGELEM                                                       
         CLI   SPDCOUNT,0          TEST OLD WAS SPOD                            
         BE    UPDBUY18                                                         
                                                                                
SPOD     USING REGELEM,R1                                                       
         SR    RF,RF               RF=TOTAL SECONDS LENGTH                      
UPDBUY14 IC    R0,SPOD.RPTIME                                                   
         AR    RF,R0                                                            
         CLM   RF,1,SPOTTSEC                                                    
         BE    UPDBUY18                                                         
         BNH   UPDBUY16                                                         
         DC    H'0'                                                             
UPDBUY16 IC    R0,SPOD.RLEN                                                     
         AR    R1,R0                                                            
         CLI   SPOD.RCODE,0        TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SPOD.RCODE,RCPOLOQ                                               
         BL    UPDBUY16                                                         
         CLI   SPOD.RCODE,RCPOLFQ                                               
         BH    UPDBUY16                                                         
         B     UPDBUY14                                                         
                                                                                
UPDBUY18 IC    R0,SPOD.RLEN                                                     
         AR    R1,R0                                                            
         SR    R1,R2               R1=OLD LENGTH                                
         CHI   R1,255              ENSURE NOT GREATER THAN 1 BYTE LONG          
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,RLEN             SET AS OLD ELEMENT LENGTH                    
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
         DROP  SPOD                                                             
                                                                                
         CLI   ALCTTYPE,RECTTREG   TEST NEW SPOT IS REGULAR                     
         BE    UPDBUY24                                                         
         IC    R0,ALC#BRDS         R0=N'SPOD BRANDS                             
         MHI   R0,RLPOL1LQ         R0=NEW SPACE REQUIRED FOR SPOD               
         GOTOR UPDREC,DMCB,((R0),REGELEM)                                       
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
         SR    R1,R1                                                            
         IC    R1,RLEN                                                          
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    REGELEM(0),REGELEM                                               
                                                                                
         LA    R1,ALCTBRDS         R1=A(BRANDS)                                 
         LA    RF,ALCTLENS         RF=A(LENGTHS)                                
         IC    R0,ALC#BRDS         R0=N'ENTRIES                                 
         ST    R2,FULL2            SAVE A(FIRST SPOT ELEMENT FOR SPOD)          
         XC    FULL1,FULL1         FULL1=TOTAL RATE OVERRIDE                    
UPDBUY20 MVI   RCODE,RCPOLOQ       BUILD SPOD ELEMENTS                          
         MVI   RLEN,RLPOL1LQ                                                    
         MVC   RDATE,ALCTSPTD                                                   
         MVC   RSTATUS,SVSTATUS                                                 
         MVC   RPALLOC+(RPCOST-RPALLOC)(L'RPCOST),SVCOST                        
         MVC   RPALLOC+(RPPRD-RPALLOC)(L'RPPRD),0(R1)                           
         MVC   RPALLOC+(RPTIME-RPALLOC)(L'RPTIME),0(RF)                         
UPDBUY22 AHI   R1,L'RPPRD          BUMP TO NEXT BRAND                           
         AHI   RF,L'RPTIME         BUMP TO NEXT LENGTH                          
         AHI   R2,RLPOL1LQ         BUMP TO NEXT ELEMENT                         
         BCT   R0,UPDBUY20         DO FOR NUMBER OF BRANDS                      
         B     UPDBUY28                                                         
                                                                                
UPDBUY24 LHI   R0,RLREGLNQ         HANDLE REGULAR SPOTS (& PIGGYBACKS)          
         TM    RSTATUS,RSRATOVQ    RATE REQUIRES LONGER ELEMENT                 
         BNZ   *+12                                                             
         CLI   ALCTBRD1,0          ELSE SHORT ELEMENT IF NO BRANDS              
         BE    UPDBUY26                                                         
         LHI   R0,RLPOL1LQ                                                      
         CLI   ALCTBRD2,0          TEST NEW PIGGYBACK BRAND SET                 
         BE    UPDBUY26                                                         
         LHI   R0,RLPOL2LQ                                                      
UPDBUY26 GOTOR UPDREC,DMCB,((R0),REGELEM)                                       
         CLI   RLEN,RLREGLNQ       TEST UNALLOCATED                             
         BE    UPDBUY28                                                         
         CLC   RPALLOC+(RPPRD-RPALLOC)(L'RPPRD),ALCTBRD1                        
         MVC   RPALLOC+(RPPRD-RPALLOC)(L'RPPRD),ALCTBRD1                        
         BE    *+8                                                              
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
         CLC   RPALLOC+(RPTIME-RPALLOC)(L'RPTIME),ALCTLEN1                      
         MVC   RPALLOC+(RPTIME-RPALLOC)(L'RPTIME),ALCTLEN1                      
         BE    *+8                                                              
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
         CLI   RLEN,RLPOL1LQ       TEST PIGGYBACK ELEMENT                       
         BE    UPDBUY28                                                         
         CLC   RPALLOC2+(RPPRD-RPALLOC)(L'RPPRD),ALCTBRD2                       
         MVC   RPALLOC2+(RPPRD-RPALLOC)(L'RPPRD),ALCTBRD2                       
         BE    *+8                                                              
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
         CLC   RPALLOC2+(RPTIME-RPALLOC)(L'RPTIME),ALCTLEN2                     
         MVC   RPALLOC2+(RPTIME-RPALLOC)(L'RPTIME),ALCTLEN2                     
         BE    *+8                                                              
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
                                                                                
UPDBUY28 AHI   R3,ALCTABL          BUMP TO NEXT ALLOCATION TABLE ENTRY          
         B     UPDBUY02                                                         
                                                                                
UPDBUY30 TM    FLAGS,FBUYUPD       TEST BUY RECORD WAS UPDATED                  
         JZ    EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3,RB                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD BRAND TO BRAND LIST FOR BUY                                     *         
***********************************************************************         
                                                                                
ADDBRD   STM   RE,R1,12(RD)                                                     
         LA    RF,IOBRDLST                                                      
         LHI   R0,L'IOBRDLST                                                    
ADDBRD02 OC    0(L'RPPRD,RF),0(RF)                                              
         JZ    ADDBRD04                                                         
         CLC   0(L'RPPRD,RF),0(R1)                                              
         JE    ADDBRD06                                                         
         AHI   RF,L'RPPRD                                                       
         BRCT  R0,ADDBRD02                                                      
         DC    H'0'                TOO MANY BRANDS                              
ADDBRD04 MVC   0(L'RPPRD,RF),0(R1)                                              
ADDBRD06 LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE ELEMENT IN RECORD                                            *         
***********************************************************************         
                                                                                
UPDREC   STM   RE,RC,12(RD)                                                     
                                                                                
         SR    R2,R2                                                            
         ICM   R2,1,0(R1)          R2=NEW ELEMENT LENGTH                        
         SR    R3,R3                                                            
         ICM   R3,7,1(R1)          R3=A(ELEMENT)                                
         CLM   R2,1,1(R3)          TEST CHANGE TO ELEMENT LENGTH                
         JE    UPDRECX             NO - EXIT                                    
         OI    FLAGS,FBUYUPD       SET BUY RECORD UPDATED                       
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,1(R3)          RE=OLD ELEMENT LENGTH                        
         STCM  R2,1,1(R3)          SET NEW ELEMENT LENGTH                       
         L     R1,IOADDR                                                        
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(R1)                                          
         LR    R0,R3                                                            
         AR    R0,RE               R0=A(CURRENT END OF ELEMENT)                 
         SR    R0,R1               R0=DISP. TO END OF CURRENT ELEMENT           
         LR    R4,RF                                                            
         SR    R4,RE               SUBTRACT OLD ELEMENT LENGTH                  
         AR    R4,R2               ADD NEW ELEMENT LENGTH                       
         STCM  R4,3,BUYRLEN-BUYREC(R1)                                          
         CHI   R4,MAXRECLN         TEST RECORD TOO LONG                         
         JNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    RF,R0               RF=LENGTH TO BE MOVED/SAVED                  
         CR    R2,RE               TEST EXTEND/SHORTEN ELEMENT                  
         JH    UPDREC02                                                         
                                                                                
         LR    R0,R3               SHORTEN - MOVE BACKWARDS                     
         AR    R0,R2                                                            
         AR    RE,R3                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECX                                                          
                                                                                
UPDREC02 STM   RE,RF,FULL1         EXTEND - SAVE/INSERT/RESTORE                 
         L     R0,AIO7                                                          
         AR    RE,R3                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE RECORD DATA AFTER INSERTION             
         LR    RF,R3                                                            
         A     RF,FULL1                                                         
         LR    R1,R2                                                            
         S     R1,FULL1            R1=INSERTION LENGTH                          
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,RF),0(RF)       CLEAR INSERTION AREA                         
         LA    RE,0(RF,R1)                                                      
         L     RF,FULL2                                                         
         L     R0,AIO7                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               RESTORE SAVED RECORD DATA                    
                                                                                
UPDRECX  LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CALL LINKIO TO BUILD ERROR                                          *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLRSP),     *        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
         MVC   XTRATEXT,SPACES     RESET EXTRA MESSAGE TEXT TO SPACES           
         OI    FLAGS,FSPTERR       SET SPOT IN ERROR                            
PUTERRX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SET UNSENT SPOT LENGTHS                                             *         
*                                                                     *         
* EXIT - CC=NOT EQUAL IF INVALID TOTAL SECONDS LENGTH                 *         
*        CC=EQUAL IF SECONDS LENGTH OKAY                              *         
***********************************************************************         
                                                                                
SETLEN   STM   RE,RC,12(RD)                                                     
                                                                                
         CLI   SPDCOUNT,0          TEST OLD SPOT WAS A SPOD                     
         JNE   SETLEN06                                                         
                                                                                
         CLI   B1ON,0              TEST OLD BRAND1 ALLOCATION SENT              
         JE    SETLEN14                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,1,B1OL           TEST OLD BRAND1 SECONDS SENT                 
         JZ    SETLEN02            NO                                           
         ICM   RF,1,B2OL           TEST OLD BRAND2 SECONDS SENT                 
         JNZ   SETLEN04            YES - WE WERE SENT BOTH                      
         IC    RF,SPOTTSEC         NO - BRAND2 SECS=BUY-BRAND1                  
         SR    RF,RE                                                            
         STC   RF,B2OL                                                          
         J     SETLEN04                                                         
                                                                                
SETLEN02 IC    RE,SPOTTSEC         RE=BUY SECS                                  
         CLI   B2ON,0              TEST OLD BRAND2 ALLOCATION SENT              
         JE    *+10                                                             
         SRL   RE,1                YES - ASSUME 50/50 SPLIT                     
         LR    RF,RE                                                            
         STC   RE,B1OL                                                          
         STC   RF,B2OL                                                          
                                                                                
SETLEN04 AR    RE,RF               ENSURE TOTAL SECONDS LENGTH IS VALID         
         CLM   RE,1,SPOTTSEC                                                    
         JNE   SETLENN                                                          
         J     SETLEN14                                                         
                                                                                
SETLEN06 LA    RE,B1ON             OLD SPOT WAS A SPOD                          
         LA    RF,B1OL                                                          
         LHI   R0,BRDN                                                          
         SR    R1,R1                                                            
         SR    R2,R2               R2=TOTAL SECONDS SENT                        
SETLEN08 CLI   0(RE),0             TEST END OF BRAND LIST                       
         JE    SETLEN12                                                         
         ICM   R1,1,0(RF)          GET SECONDS FOR THIS BRAND                   
         JZ    SETLEN10            NOT GIVEN - MUST BE LAST                     
         AR    R2,R1               ADD TO TOTAL SECONDS                         
         AHI   RE,L'B1ON           BUMP BRAND POINTER                           
         AHI   RF,L'B1OL           BUMP SECONDS POINTER                         
         BRCT  R0,SETLEN08         DO FOR NUMBER OF BRANDS                      
         J     SETLENN                                                          
                                                                                
SETLEN10 IC    R1,SPOTTSEC         BRANDN SECS=BUY-TOTAL SENT                   
         SR    R1,R2                                                            
         JNP   SETLENN                                                          
         STC   R1,0(RF)                                                         
         J     SETLEN14                                                         
                                                                                
SETLEN12 CLM   R2,1,SPOTTSEC       ENSURE TOTAL LENGTH IS VALID                 
         JNE   SETLENN                                                          
                                                                                
SETLEN14 CLI   RECTYPE,RECTTREG    TEST NEW SPOT IS REGULAR                     
         JNE   SETLEN20                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,1,B1NL           TEST NEW BRAND1 SECONDS SENT                 
         JNZ   *+12                YES - MUST BE                                
         CLI   B1NN,0              TEST NEW BRAND1 ALLOCATION SENT              
         JE    SETLENY             NO - MUST BE UNALLOCATION                    
         LTR   RE,RE                                                            
         JZ    SETLEN16                                                         
         ICM   RF,1,B2NL           TEST NEW BRAND2 SECONDS SENT                 
         JNZ   SETLEN18            YES - WE WERE SENT BOTH                      
         IC    RF,SPOTTSEC         NO - BRAND2 SECS=BUY-BRAND1                  
         SR    RF,RE                                                            
         STC   RF,B2NL                                                          
         J     SETLEN18                                                         
                                                                                
SETLEN16 IC    RE,SPOTTSEC         RE=BUY SECS                                  
         CLI   B2NN,0              TEST NEW BRAND2 ALLOCATION SENT              
         JE    *+10                                                             
         SRL   RE,1                YES - ASSUME 50/50 SPLIT                     
         LR    RF,RE                                                            
         STC   RE,B1NL                                                          
         STC   RF,B2NL                                                          
                                                                                
SETLEN18 AR    RE,RF               ENSURE TOTAL SECONDS LENGTH IS VALID         
         CLM   RE,1,SPOTTSEC                                                    
         JNE   SETLENN                                                          
         J     SETLENY                                                          
                                                                                
SETLEN20 CLI   RECTYPE,RECTTSPD    TEST SPOD ALLOCATION                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RE,B1NN             NEW SPOT IS A SPOD                           
         LA    RF,B1NL                                                          
         LHI   R0,BRDN                                                          
         SR    R1,R1                                                            
         SR    R2,R2               R2=TOTAL SECONDS SENT                        
SETLEN22 CLI   0(RE),0             TEST END OF BRAND LIST                       
         JE    SETLEN26                                                         
         ICM   R1,1,0(RF)          GET SECONDS FOR THIS BRAND                   
         JZ    SETLEN24            NOT GIVEN - MUST BE LAST                     
         AR    R2,R1               ADD TO TOTAL SECONDS                         
         AHI   RE,L'B1NN           BUMP BRAND POINTER                           
         AHI   RF,L'B1NL           BUMP SECONDS POINTER                         
         BRCT  R0,SETLEN22         DO FOR NUMBER OF BRANDS                      
         J     SETLENN                                                          
                                                                                
SETLEN24 IC    R1,SPOTTSEC         BRANDN SECS=BUY-TOTAL SENT                   
         SR    R1,R2                                                            
         JNP   SETLENN                                                          
         STC   R1,0(RF)                                                         
         J     SETLENY                                                          
                                                                                
SETLEN26 CLM   R2,1,SPOTTSEC       ENSURE TOTAL LENGTH IS VALID                 
         JE    SETLENY                                                          
         J     SETLENN                                                          
                                                                                
SETLENY  LM    RE,RC,12(RD)                                                     
         CR    RE,RE               SET CC=EQUAL IF OKAY                         
         BR    RE                                                               
                                                                                
SETLENN  LM    RE,RC,12(RD)                                                     
         LTR   RE,RE               SET CC=NOT EQUAL ON ERROR                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK OLD ALLOCATION VALUES                                         *         
*                                                                     *         
* NTRY - R2=A(SPOT ELEMENT)                                           *         
* EXIT - CC=LOW IF BRAND ALLOCATION MISMATCH                          *         
*        CC=HIGH IF SECONDS LENGTH MISMATCH                           *         
*        CC=EQUAL IF NO MISMATCHES                                    *         
***********************************************************************         
                                                                                
         USING REGELEM,R2                                                       
CHKOLD   STM   RE,RC,12(RD)                                                     
                                                                                
         CLI   SPDCOUNT,0          TEST OLD SPOT WAS A SPOD                     
         JNE   CHKOLD08                                                         
                                                                                
         CLI   RLEN,RLPOL1LQ       TEST SPOT PREVIOUSLY ALLOCATED               
         JNL   CHKOLD02                                                         
         CLI   B1ON,0              NO - OLD VALUES MUST BE ZERO                 
         JNE   CHKOLDL                                                          
         CLI   B2ON,0                                                           
         JNE   CHKOLDL                                                          
         J     CHKOLD06                                                         
                                                                                
CHKOLD02 CLC   B1ON,RPALLOC+(RPPRD-RPALLOC)                                     
         JNE   CHKOLDL                                                          
         CLC   B1OL,RPALLOC+(RPTIME-RPALLOC)                                    
         JNE   CHKOLDH                                                          
         CLI   RLEN,RLPOL2LQ       TEST SPOT HAS PIGGYBACK BRAND                
         JL    CHKOLD04                                                         
         CLC   B2ON,RPALLOC2+(RPPRD-RPALLOC)                                    
         JNE   CHKOLDL                                                          
         J     CHKOLD06                                                         
                                                                                
CHKOLD04 CLI   B2ON,0              SPOT HAS NO PIGGYBACK                        
         JNE   CHKOLDL                                                          
                                                                                
CHKOLD06 CLI   B1NL,0              TEST PIGGYBACK ALLOCATION                    
         JE    CHKOLDY                                                          
         CLI   B1NN,0              TEST NEW BRAND1 GIVEN                        
         JNE   *+10                                                             
         MVC   B1NN,B1ON           NO - USE OLD BRAND1 VALUE                    
         CLC   B1NL,SPOTTSEC       TEST ALL ALLOCATED TO BRAND1                 
         JE    CHKOLDY                                                          
         CLI   B2NN,0              TEST NEW BRAND2 GIVEN                        
         JNE   CHKOLDY                                                          
         MVC   B2NN,B2ON           NO - USE OLD BRAND2 VALUE                    
         J     CHKOLDY                                                          
                                                                                
CHKOLD08 LA    R1,B1ON             HANDLE OLD SPOD ALLOCATIONS                  
         LA    RE,B1OL                                                          
         LHI   R0,BRDN                                                          
         SR    RF,RF                                                            
         SR    R3,R3                                                            
CHKOLD10 CLI   0(R1),0             TEST END OF ALLOCATIONS                      
         JE    CHKOLD14                                                         
         ICM   RF,1,0(RE)          GET BRAND LENGTH                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF               UPDATE TOTAL LENGTH                          
                                                                                
         CLI   RLEN,RLPOL1LQ                                                    
         JNE   CHKOLDL                                                          
         CLC   0(L'RPPRD,R1),RPALLOC+(RPPRD-RPALLOC)                            
         JNE   CHKOLDL                                                          
         CLC   0(L'RPTIME,RE),RPALLOC+(RPTIME-RPALLOC)                          
         JNE   CHKOLDH                                                          
                                                                                
         AHI   R1,L'B1ON           BUMP TO NEXT BRAND                           
         AHI   RE,L'B1OL           BUMP TO NEXT LENGTH                          
CHKOLD12 IC    RF,RLEN             BUMP TO NEXT SPOT ELEMENT                    
         AR    R2,RF                                                            
         CLI   RCODE,0             TEST END OF RECORD                           
         JE    CHKOLDL                                                          
         CLI   RCODE,RCPOLOQ       TEST SPOT ELEMENT                            
         JL    CHKOLD12                                                         
         CLI   RCODE,RCPOLFQ                                                    
         JH    CHKOLD12                                                         
         TM    RSTATUS,RSMINUSQ+RSHIATSQ                                        
         JNZ   CHKOLD12                                                         
         BRCT  R0,CHKOLD10                                                      
                                                                                
CHKOLD14 CLM   R3,1,SPOTTSEC       TEST ALL SECONDS ACCOUNTED FOR               
         JNE   CHKOLDH                                                          
                                                                                
CHKOLDY  LHI   R0,1                SET CC=EQUAL IF NO MISMATCHES                
         J     CHKOLDX                                                          
                                                                                
CHKOLDL  LHI   R0,0                SET CC=LOW IF ALLOCATION MISMATCH            
         J     CHKOLDX                                                          
                                                                                
CHKOLDH  LHI   R0,2                SET CC=HIGH IF LENGTH MISMATCH               
                                                                                
CHKOLDX  CHI   R0,1                SET CONDITION CODE                           
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(I#SPTULD),AL1(RECTTREG)                                      
         DC    AL2(I#SPDULD),AL1(RECTTSPD)                                      
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
RECTABD  DSECT                     ** RECORD TABLE **                           
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTTREG EQU   1                   REGULAR SPOT                                 
RECTTSPD EQU   2                   SPOD SPOT                                    
RECTABL  EQU   *-RECTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
NETMEDQ  EQU   C'N'                CANADIAN NETWORK MEDIA LETTER                
CBLMEDQ  EQU   C'C'                CABLE MEDIA LETTER                           
POLBRDQ  EQU   X'FF'               POOL BRAND (PRODUCT) NUMBER                  
MAXRECLN EQU   3972                SPTFIL MAXIMUM RECORD LENGTH                 
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
         XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR SPOT/SPOD ALLOCATION UPLOADS                        *         
***********************************************************************         
                                                                                
REQSPT   LKMAP H,I#SPTULD,POINTTO=REQSPD                                        
                                                                                
REQSPD   LKMAP H,I#SPDULD,NEWREC=Y                                              
                                                                                
MedCd    LKMAP F,D#MED,CHAR,SP#MED,OUTPUT=(D,B#SAVED,MEDA)                      
CltCd    LKMAP F,D#CLT,CHAR,SP#CLI,OUTPUT=(D,B#SAVED,CLTA)                      
MktNo    LKMAP F,D#MKTNUM,LBIN,SP#MKT,OUTPUT=(D,B#SAVED,MRKT)                   
StNet    LKMAP F,D#STANET,CHAR,SP#STNET,OUTPUT=(D,B#SAVED,STAC)                 
EstNo    LKMAP F,D#ESTNUM,LBIN,SP#EST,OUTPUT=(D,B#SAVED,ESTN)                   
Line#    LKMAP F,D#BUYLIN,LBIN,SP#BYLIN,OUTPUT=(D,B#SAVED,LINE)                 
SDate    LKMAP F,D#SPTDAT,CDAT,SP#SDATE,OUTPUT=(D,B#SAVED,SPTD)                 
SRef#    LKMAP F,D#SPTREF,LBIN,SP#SPREF,OUTPUT=(D,B#SAVED,SPTR)                 
B1Old    LKMAP F,D#BR1COD,CHAR,SP#B1OLD,OUTPUT=(D,B#SAVED,B1OC)                 
B1New    LKMAP F,D#BR1COD+1,CHAR,SP#B1NEW,OUTPUT=(D,B#SAVED,B1NC)               
B1SLO    LKMAP F,D#BR1LEN,LBIN,SP#B1SLO,OUTPUT=(D,B#SAVED,B1OL)                 
B1SLN    LKMAP F,D#BR1LEN+1,LBIN,SP#B1SLN,OUTPUT=(D,B#SAVED,B1NL)               
B2Old    LKMAP F,D#BR2COD,CHAR,SP#B2OLD,OUTPUT=(D,B#SAVED,B2OC)                 
B2New    LKMAP F,D#BR2COD+1,CHAR,SP#B2NEW,OUTPUT=(D,B#SAVED,B2NC)               
B2SLO    LKMAP F,D#BR2LEN,LBIN,SP#B2SLO,OUTPUT=(D,B#SAVED,B2OL)                 
B2SLN    LKMAP F,D#BR2LEN+1,LBIN,SP#B2SLN,OUTPUT=(D,B#SAVED,B2NL)               
B3Old    LKMAP F,D#BR3COD,CHAR,SP#B3OLD,OUTPUT=(D,B#SAVED,B3OC)                 
B3New    LKMAP F,D#BR3COD+1,CHAR,SP#B3NEW,OUTPUT=(D,B#SAVED,B3NC)               
B3SLO    LKMAP F,D#BR3LEN,LBIN,SP#B3SLO,OUTPUT=(D,B#SAVED,B3OL)                 
B3SLN    LKMAP F,D#BR3LEN+1,LBIN,SP#B3SLN,OUTPUT=(D,B#SAVED,B3NL)               
B4Old    LKMAP F,D#BR4COD,CHAR,SP#B4OLD,OUTPUT=(D,B#SAVED,B4OC)                 
B4New    LKMAP F,D#BR4COD+1,CHAR,SP#B4NEW,OUTPUT=(D,B#SAVED,B4NC)               
B4SLO    LKMAP F,D#BR4LEN,LBIN,SP#B4SLO,OUTPUT=(D,B#SAVED,B4OL)                 
B4SLN    LKMAP F,D#BR4LEN+1,LBIN,SP#B4SLN,OUTPUT=(D,B#SAVED,B4NL)               
B5Old    LKMAP F,D#BR5COD,CHAR,SP#B5OLD,OUTPUT=(D,B#SAVED,B5OC)                 
B5New    LKMAP F,D#BR5COD+1,CHAR,SP#B5NEW,OUTPUT=(D,B#SAVED,B5NC)               
B5SLO    LKMAP F,D#BR5LEN,LBIN,SP#B4SLO,OUTPUT=(D,B#SAVED,B5OL)                 
B5SLN    LKMAP F,D#BR5LEN+1,LBIN,SP#B5SLN,OUTPUT=(D,B#SAVED,B5NL)               
B6Old    LKMAP F,D#BR6COD,CHAR,SP#B6OLD,OUTPUT=(D,B#SAVED,B6OC)                 
B6New    LKMAP F,D#BR6COD+1,CHAR,SP#B6NEW,OUTPUT=(D,B#SAVED,B6NC)               
                                                                                
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
ANXTALC  DS    A                   A(NEXT ALLOCATION TABLE ENTRY)               
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      UPLOAD RECORD TYPE                           
                                                                                
MEDA     DS    CL(L'QMEDA)         MEDIA CODE                                   
CLTA     DS    CL(L'QCLTA)         CLIENT CODE                                  
MRKT     DS    XL(L'BUYKMKTN)      MARKET NUMBER                                
STAC     DS    CL(L'STAPQSTA)      STATION CODE                                 
ESTN     DS    XL(L'BUYKEST)       ESTIMATE NUMBER                              
LINE     DS    X                   BUY LINE NUMBER                              
SPTD     DS    XL(L'RDATE)         SPOT DATE                                    
SPTR     DS    X                   SPOT REFERENCE NUMBER                        
                                                                                
BRDCS    DS    0CL(L'PKEYPRD)      ** BRAND (PRODUCTS) CODES **                 
B1OC     DS    CL(L'PKEYPRD)       BRAND1 OLD CODE                              
B2OC     DS    CL(L'PKEYPRD)       BRAND2 OLD CODE                              
B3OC     DS    CL(L'PKEYPRD)       BRAND3 OLD CODE                              
B4OC     DS    CL(L'PKEYPRD)       BRAND4 OLD CODE                              
B5OC     DS    CL(L'PKEYPRD)       BRAND5 OLD CODE                              
B6OC     DS    CL(L'PKEYPRD)       BRAND6 OLD CODE                              
BRDN     EQU   (*-BRDCS)/L'BRDCS                                                
B1NC     DS    CL(L'PKEYPRD)       BRAND1 NEW CODE                              
B2NC     DS    CL(L'PKEYPRD)       BRAND2 NEW CODE                              
B3NC     DS    CL(L'PKEYPRD)       BRAND3 NEW CODE                              
B4NC     DS    CL(L'PKEYPRD)       BRAND4 NEW CODE                              
B5NC     DS    CL(L'PKEYPRD)       BRAND5 NEW CODE                              
B6NC     DS    CL(L'PKEYPRD)       BRAND6 NEW CODE                              
BRDCN    EQU   (*-BRDCS)/L'BRDCS                                                
                                                                                
BRDNS    DS    0XL(L'RPPRD)        ** BRAND (PRODUCT) NUMBERS **                
B1ON     DS    XL(L'RPPRD)         BRAND1 OLD NUMBER                            
B2ON     DS    XL(L'RPPRD)         BRAND2 OLD NUMBER                            
B3ON     DS    XL(L'RPPRD)         BRAND3 OLD NUMBER                            
B4ON     DS    XL(L'RPPRD)         BRAND4 OLD NUMBER                            
B5ON     DS    XL(L'RPPRD)         BRAND5 OLD NUMBER                            
B6ON     DS    XL(L'RPPRD)         BRAND6 OLD NUMBER                            
B1NN     DS    XL(L'RPPRD)         BRAND1 NEW NUMBER                            
B2NN     DS    XL(L'RPPRD)         BRAND2 NEW NUMBER                            
B3NN     DS    XL(L'RPPRD)         BRAND3 NEW NUMBER                            
B4NN     DS    XL(L'RPPRD)         BRAND4 NEW NUMBER                            
B5NN     DS    XL(L'RPPRD)         BRAND5 NEW NUMBER                            
B6NN     DS    XL(L'RPPRD)         BRAND6 NEW NUMBER                            
BRDNN    EQU   (*-BRDNS)/L'BRDNS                                                
                                                                                
B1OL     DS    XL(L'RPTIME)        BRAND1 OLD SPOT LENGTH                       
B2OL     DS    XL(L'RPTIME)        BRAND2 OLD SPOT LENGTH                       
B3OL     DS    XL(L'RPTIME)        BRAND3 OLD SPOT LENGTH                       
B4OL     DS    XL(L'RPTIME)        BRAND4 OLD SPOT LENGTH                       
B5OL     DS    XL(L'RPTIME)        BRAND5 OLD SPOT LENGTH                       
B6OL     DS    XL(L'RPTIME)        BRAND6 OLD SPOT LENGTH                       
B1NL     DS    XL(L'RPTIME)        BRAND1 NEW SPOT LENGTH                       
B2NL     DS    XL(L'RPTIME)        BRAND2 NEW SPOT LENGTH                       
B3NL     DS    XL(L'RPTIME)        BRAND3 NEW SPOT LENGTH                       
B4NL     DS    XL(L'RPTIME)        BRAND4 NEW SPOT LENGTH                       
B5NL     DS    XL(L'RPTIME)        BRAND5 NEW SPOT LENGTH                       
B6NL     DS    XL(L'RPTIME)        BRAND6 NEW SPOT LENGTH                       
                                                                                
FLAGS    DS    X                   ** FLAG BYTE **                              
FCLTGET  EQU   X'80'               CLIENT RECORD GOT FLAG                       
FSPTERR  EQU   X'40'               SPOT RECORD IN ERROR FLAG                    
FBUYUPD  EQU   X'20'               BUY RECORD UPDATED FLAG                      
                                                                                
CBUYKEY  DS    XL(L'IOKEY)         CURRENT BUY RECORD KEY                       
LBUYKEY  DS    XL(L'IOKEY)         LAST BUY RECORD KEY                          
LMEDA    DS    CL(L'MEDA)          LAST MEDIA CODE                              
SPOTTSEC DS    XL(L'BDSEC)         TOTAL SPOT LENGTH                            
XTRATEXT DS    CL8                 EXTRA MESSAGE TEXT                           
                                                                                
SPDVALS  DS    0X                  ** SPOD VALUES **                            
SPDTLEN  DS    X                   TOTAL SPOD LENGTH                            
SPDCOUNT DS    X                   SPOD COUNT                                   
SPDVALL  EQU   *-SPDVALS                                                        
                                                                                
SVSTATUS DS    XL(L'RSTATUS)       SAVED SPOT STATUS                            
SVCOST   DS    XL(L'RPCOST)        SAVED SPOT COST                              
                                                                                
ALC#BRDS DS    AL1                 # BRANDS IN ALLOCATION LINE                  
ALCTAB   DS    (ALCTMAXN)XL(ALCTABL),X                                          
                                                                                
RCPOLFQ  EQU   X'0D'                                                            
                                                                                
ALCTABD  DSECT                     ** BUY ALLOCATION TABLE **                   
ALCTEOTQ EQU   0                   END OF TABLE INDICATOR                       
ALCTSPTD DS    XL(L'RDATE)         SPOT DATE                                    
ALCTSPTR DS    XL(L'SPTR)          SPOT REFERENCE NUMBER                        
ALCTTYPE DS    XL(L'RECTYPE)       RECORD TYPE (REGULAR/SPOD)                   
ALCTBRDS DS    XL(BRDN*L'RPPRD)    BRAND ALLOCATIONS                            
         ORG   ALCTBRDS                                                         
ALCTBRD1 DS    XL(L'RPPRD)         BRAND1                                       
ALCTBRD2 DS    XL(L'RPPRD)         BRAND2                                       
         ORG                                                                    
ALCTLENS DS    XL(BRDN*L'RPTIME)   SECONDS LENGTHS                              
         ORG   ALCTLENS                                                         
ALCTLEN1 DS    XL(L'RPTIME)        BRAND1 SECONDS LENGTH                        
ALCTLEN2 DS    XL(L'RPTIME)        BRAND2 SECONDS LENGTH                        
         ORG                                                                    
ALCTABL  EQU   *-ALCTABD                                                        
ALCTMAXN EQU   255                 MAXIMUM N'ALLOCATION TABLE ENTRIES           
                                                                                
* INCLUDED DSECTS FOLLOW                                                        
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVMEDA   DS    CL(L'MEDA)          SAVED MEDIA CODE                             
SVAGYM   DS    XL(L'BUYKAM)        SAVED AGENCY/MEDIA CODE                      
SVCLTX   DS    XL(L'BUYKCLT)       SAVED CLIENT CODE                            
SVMRKT   DS    XL(L'BUYKMKTN)      SAVED MARKET NUMBER                          
SVSTAC   DS    XL(L'BUYKSTAC)      SAVED STATION/NETWORK CODE                   
SVESTN   DS    XL(L'BUYKEST)       SAVED ESTIMATE NUMBER                        
SVLINE   DS    XL(L'LINE)          SAVED BUY LINE NUMBER                        
SVSPTD   DS    XL(L'SPTD)          SAVED SPOT DATE                              
SVSPTR   DS    XL(L'SPTR)          SAVED SPOT REFERENCE NUMBER                  
SVVALL   EQU   *-SVVALS                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPLNK13   09/12/17'                                      
         END                                                                    
