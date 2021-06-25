*          DATA SET SPLNK25    AT LEVEL 018 AS OF 03/24/20                      
*PHASE T21E25A                                                                  
*                                                                               
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* HWON  12SEP17 017 SPEC-13049-RELINK FOR LARGER IOAREAS                        
* HWON  20AUG15 016 FIX CANADIAN NETWORK&COMBINED BRANDS NOT ADDED              
* KWAN  17Apr15 015 Enable flag2 in client UDEF field                           
* KWAN  13NOV14 014 Blank client/product name fix                               
* KWAN  08AUG11 006 Canadian taxes + Selective TV (process N and C)             
* HWON  29JUL05 001 INITIAL DEVELOPMENT 1.0                                     
*                                                                               
*=====================================================================*         
*                                                                               
SPLNK25  TITLE 'SPOT/LINK CFM UPLOAD'                                           
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=500,REQUEST=*,WORKERKEY=SPCU,    *        
               LINKIO=Y,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),          *        
               SYSTEM=SPTSYSQ                                                   
         EJECT                                                                  
ENTRY    NMOD1 0,**SL25**,RR=RE,CLEAR=Y                                         
         LR    R6,R1               R1=A(LP_D)                                   
         USING LP_D,R6                                                          
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     RF,LP_ARUNP                                                      
         MVC   BYTE1,RUNPMODE-RUNPARMD(RF)                                      
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         L     RA,AGLOBALS                                                      
         A     RA,SRVRRELO                                                      
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   BYTE1,RRUNSTRQ      TEST 'FIRST' MODE                            
         BE    FIRST                                                            
         CLI   BYTE1,RINIREQQ      TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   BYTE1,RRUNREQQ      TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   BYTE1,RRUNENDQ      TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
*                                                                               
         J     EXITY                                                            
                                                                                
AGLOBALS DC    A(GLOBALS)                                                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RUN FIRST                                                                     
***********************************************************************         
*                                                                               
FIRST    DS    0H                                                               
*                                                                               
         LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE              
         GOTO1 VDATCON,DMCB,(5,0),(0,TDYDTCHR) TODAYS DATE IN CHAR              
*                                                                               
         MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
*                                                                               
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* INIT - NOTE FIRST REC PROCESSED *BEFORE* INIT CALL!                           
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         MVI   DATADISP+1,24                                                    
*                                                                               
         MVC   WVALUES(WVALUESL),LVALUES                                        
         LHI   R0,ADCONSN                                                       
         CHI   R0,0                                                             
         BE    INIT10                                                           
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
*                                                                               
INIT10   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         LA    RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         OI    LIOBINDS,LIOBINRM   INHIBIT AUTO RETURN MAP ELEMENT              
*                                                                               
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         DROP  RF                                                               
*                                                                               
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                  EXTRACT A(RECUP) FROM COMFACS                
         MVC   ARECUP,CRECUP-COMFACSD(RF)                                       
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS AN UPLOAD RECORD                                                      
***********************************************************************         
*                                                                               
INPUT    LA    RE,RECTAB                                                        
         USING RECTABD,RE                                                       
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   RECTMAP#,LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE             
         BE    INPUT20                                                          
         AHI   RE,RECTABL                                                       
         BCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  CLI   RECTFLAG,X'FF'        CLEAR ALL ERRORS?                          
         BNE   INPUT30               YES                                        
         MVI   MISCFLG3,0                                                       
*                                                                               
INPUT30  CLI   MISCFLG3,0                                                       
         BNE   EXITY                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,RECTPROG                                                    
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR                                                       
*                                                                               
         GOTOR RECADDR                                                          
         B     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
*                                                                               
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTPROG DS    AL3                 A(ROUTINE)                                   
RECTFLAG DS    X                                                                
RECTABL  EQU   *-RECTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE CFM CLIENT UPLOAD                                                
***********************************************************************         
CLTUPLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,I$CAM                                                      
         JZ    ERR00258            MISSING MEDIA                                
*                                                                               
         USING LW_D,RE                                                          
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
         GOTOR (#VALMED,AVALMED),DMCB,LW_DATA1,(R3),SVBAGM                      
         JNE   ERR01023            INVALID MEDIA                                
*                                                                               
         ICM   RE,15,I$CCLT                                                     
         JZ    ERR00966            MISSING CLIENT                               
*                                                                               
         MVC   QCLTA,LW_DATA1                                                   
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
*                                                                               
         CHI   R3,2                                                             
         JL    ERR00014            INVALID CLIENT                               
         BH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
*                                                                               
         GOTOR VCLPACK,DMCB,QCLTA,SVBCLT                                        
         CLI   0(R1),FF                                                         
         JE    ERR00014            INVALID CLIENT                               
*                                                                               
CLTU030  LA    R4,IOKEY                                                         
         USING CLTRECD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,SVBAGM                                                    
         MVC   CKEYCLT,SVBCLT                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CLTREC'                        
         BNE   CLTU040                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+#CLTREC'                    
         JNE   *+2                                                              
         L     RE,ACLTREC          PREPARE IOAREA FOR THE ADD                   
         MVC   SVOFFICE,COFFICE-CLTRECD(RE)    FOR OFFICE PASSIVES              
*                                                                               
         CLI   QCRECACT,C'C'                                                    
         JNE   ERR00049            RECORD ALREADY EXISTS                        
         B     CLTU050                                                          
*                                                                               
* LACK OF THIS INTRUCTION CAUSED CANADTV TO EXIT PREMATURELY                    
*                                                                               
CLTU040  MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
*                                                                               
         CLI   QCRECACT,C'A'                                                    
         JNE   ERR00971            RECORD NOT FOUND                             
*                                                                               
         L     RE,ACLTREC          PREPARE IOAREA FOR THE ADD                   
         LA    RF,CLTHDRSL                                                      
         XCEFL                                                                  
*                                                                               
         L     R2,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,R2),SVBAGM                                  
         MVC   CKEYCLT-CKEY(L'CKEYCLT,R2),SVBCLT                                
         MVC   CLEN-CKEY(L'CLEN,R2),=AL2(CLTHDRSL)                              
*                                                                               
CLTU050  L     R2,ACLTREC          COPY CLIST TO NEW RECORD                     
         AHI   R2,CLIST-CLTRECD                                                 
         LHI   R3,880                                                           
         LA    RE,QCLIST                                                        
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
         L     R2,ACLTREC          COPY CLIST2 TO NEW RECORD                    
         AHI   R2,CLIST2-CLTRECD                                                
         LHI   R3,140                                                           
         LA    RE,QCLIST2                                                       
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
* Client record adjustment can be made here                                     
*                                                                               
         TM    QCPU1FLG1,CFLGBFRQ  Print at front of bill?                      
         JZ    *+8                                                              
         OI    QCPU1FLG1,CFLGSPQ   Set to show on bill too                      
         TM    QCPU1FLG1,CFLGBHLQ  Print in headlines of bill?                  
         JZ    *+8                                                              
         OI    QCPU1FLG1,CFLGSPQ   Set to show on bill too                      
*                                                                               
         TM    QCPU2FLG1,CFLGBFRQ  Print at front of bill?                      
         JZ    *+8                                                              
         OI    QCPU2FLG1,CFLGSPQ   Set to show on bill too                      
         TM    QCPU2FLG1,CFLGBHLQ  Print in headlines of bill?                  
         JZ    *+8                                                              
         OI    QCPU2FLG1,CFLGSPQ   Set to show on bill too                      
*                                                                               
         TM    QCEU1FLG1,CFLGBFRQ  Print at front of bill?                      
         JZ    *+8                                                              
         OI    QCEU1FLG1,CFLGSPQ   Set to show on bill too                      
         TM    QCEU1FLG1,CFLGBHLQ  Print in headlines of bill?                  
         JZ    *+8                                                              
         OI    QCEU1FLG1,CFLGSPQ   Set to show on bill too                      
*                                                                               
         TM    QCEU2FLG1,CFLGBFRQ  Print at front of bill?                      
         JZ    *+8                                                              
         OI    QCEU2FLG1,CFLGSPQ   Set to show on bill too                      
         TM    QCEU2FLG1,CFLGBHLQ  Print in headlines of bill?                  
         JZ    *+8                                                              
         OI    QCEU2FLG1,CFLGSPQ   Set to show on bill too                      
*                                                                               
         CLC   QCNAME,SPACES       If no name, default to client code           
         JH    *+10                                                             
         MVC   QCNAME(L'QCLTA),QCLTA                                            
*                                                                               
         CLC   LP_VRSN,=AL1(01,00,05,00)                                        
         JL    CLTU052                                                          
*                                                                               
         LA    R1,QPSTCODE                                                      
         BRAS  RE,SETPSTC          Set PST Codes                                
         MVC   QCPST,QPSTCODE                                                   
*                                                                               
         LA    R1,QCMPST_C                                                      
         BRAS  RE,SETMPSC          Set Main PST - returned in HALF1             
         MVC   QCMPST,HALF1                                                     
*                                                                               
* Any changes to client record from here will not be saved                      
*                                                                               
CLTU052  L     R2,ACLTREC          NOW COPY CLTREC TO NEW REC                   
         AHI   R2,CNAME-CLTRECD                                                 
         LHI   R3,IO2LQ-(CNAME-CLTRECD)                                         
         LA    RE,QCNAME                                                        
         LR    RF,R3                                                            
         MVCL  R2,RE                                                            
*                                                                               
         USING OFFICED,OFCBLK      OFFICER CONTROL BLOCK                        
         L     R4,ACLTREC                                                       
         USING CLTRECD,R4          R2=A(CLIENT RECORD)                          
         MVC   COFFICE,SVCOFFIC    NOW WHAT WE GOT FROM ORGANIZER               
*                                                                               
         L     R2,ACLTREC                                                       
         MVC   SVIOKEY,0(R2)       SAVE THE KEY FROM REC, DON'T ASSUME          
*                                    IOKEY HAS CORRECT KEY                      
*                                                                               
         CLI   QCRECACT,C'C'       ACTION CHANGE?                               
         BNE   CLTU060                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+#CLTREC'                    
         JNE   *+2                                                              
*                                                                               
         BRAS  RE,CANADTV                                                       
         BNE   CLTU080                                                          
*                                                                               
         LA    R4,IOKEY                                                         
         USING CLTRECD,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   CKEY,SVIOKEY                                                     
         NI    CKEYAM,X'F0'        ADD MEDIA N CLIENT                           
         OI    CKEYAM,X'03'          media x'03' is  N                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),CKEYAM                                  
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO2LQ                                                         
         LHI   RF,IO2LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   CKEY,SVIOKEY                                                     
         NI    CKEYAM,X'F0'        ADD MEDIA C CLIENT                           
         OI    CKEYAM,X'08'          media x'03' is  C                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),CKEYAM                                  
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO2LQ                                                         
         LHI   RF,IO2LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         MVC   IOKEY,SVIOKEY       RESTORE IOKEY                                
*                                                                               
         B     CLTU080                                                          
*                                                                               
CLTU060  CLI   QCRECACT,C'A'       ACTION ADD?                                  
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+#CLTREC'                    
         JNE   *+2                                                              
*                                                                               
         MVC   SVIOKEY+14(4),IODA  DISK ADDR OF THE ADDED RECORD                
*                                                                               
         BRAS  RE,CANADTV                                                       
         BNE   CLTU080                                                          
*                                                                               
         MVC   IOKEY,SVIOKEY                                                    
         LA    R4,IOKEY                                                         
         USING CLTRECD,R4                                                       
         NI    CKEYAM,X'F0'        ADD MEDIA N CLIENT                           
         OI    CKEYAM,X'03'          media x'03' is  N                          
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),CKEYAM                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+#CLTREC'                    
         JNE   *+2                                                              
*                                                                               
         MVC   IOKEY,SVIOKEY                                                    
         NI    CKEYAM,X'F0'        ADD MEDIA C CLIENT                           
         OI    CKEYAM,X'08'          media x'03' is  C                          
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),CKEYAM                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+#CLTREC'                    
         JNE   *+2                                                              
*                                                                               
CLTU080  BRAS  RE,OFCPTR                                                        
*                                                                               
CLTUX    J     EXITY                                                            
CLTUXN   J     EXITN                                                            
*                                                                               
***********************************                                             
* CHECKS IF CANADIAN AGENCY AND SELECTIVE MEDIA                                 
***********************************                                             
CANADTV  NTR1                                                                   
         USING AGYRECD,RE                                                       
         L     RE,AIO1             CANADIAN AGENCY??                            
         CLI   AGYKTYPE,AGYKTYPQ   HAVE AGY RECORD?                             
         JNE   *+2                 NO, BETTER OFF DIEING                        
         CLI   AGYPCNDA,C'C'       YES, COUNTRY SET TO A C'C'                   
         JNE   EXITN                                                            
         L     R4,ACLTREC                                                       
         USING CLTRECD,R4                                                       
         CLI   0(R4),0             MAKE SURE WE HAVE CLIENT KEY                 
         JNE   *+2                 - NO, SOMEONE CLOBBERED IOKEY                
         OC    CKEY+4(9),CKEY+4                                                 
         JNZ   *+2                 - NO, SOMEONE CLOBBERED IOKEY                
         CLC   CKEYCLT,SVBCLT      AND CLIENT KEY MATCHES                       
         JNE   *+2                 - NO, SOMEONE CLOBBERED IOKEY                
         MVC   BYTE4,CKEYAM                                                     
         NI    BYTE4,X'0F'                                                      
         CLI   BYTE4,1             X'01' means Selective TV                     
         JNE   EXITN               if not, then not CANADTV                     
         J     EXITY                                                            
                                                                                
         DROP  R4,RE                                                            
*                                                                               
SETPSTC  SR    R0,R0                                                            
         AHI   R0,L'CPST                                                        
SETPSTC3 CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         JCT   R0,SETPSTC3                                                      
         BR    RE                                                               
*                                                                               
SETMPSC  XC    HALF1,HALF1                                                      
         CLC   0(L'QCMPST_C,R1),SPACES                                          
         JNH   SETMPSCX                                                         
         LA    RF,PSTVTAB                                                       
SETMPSC3 CLI   0(RF),0             End of table?                                
         JE    SETMPSCX                                                         
         CLC   0(L'QCMPSTPC,RF),0(R1)                                           
         JE    *+12                                                             
         LA    RF,3(RF)                                                         
         J     SETMPSC3                                                         
         MVC   HALF1+0(1),2(RF)                                                 
         MVC   HALF1+1(1),L'QCMPSTPC(R1)                                        
SETMPSCX BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE CFM PRODUCT UPLOAD                                               
***********************************************************************         
PRDUPLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,I$PAM                                                      
         JZ    ERR00258              MEDIA MISSING                              
*                                                                               
         USING LW_D,RE                                                          
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
         GOTOR (#VALMED,AVALMED),DMCB,LW_DATA1,(R3),SVBAGM                      
         JNE   ERR01023              INVALID MEDIA                              
*                                                                               
         ICM   RE,15,I$PCLT                                                     
         JZ    ERR00966              MISSING CLIENT                             
*                                                                               
         MVC   QCLTA,LW_DATA1                                                   
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
*                                                                               
         CHI   R3,2                                                             
         JL    ERR00014              INVALID CLIENT                             
         BH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
*                                                                               
         GOTOR VCLPACK,DMCB,QCLTA,SVBCLT                                        
         CLI   0(R1),FF                                                         
         JE    ERR00014              INVALID CLIENT                             
*                                                                               
         MVC   BYTE,INPPCLSS                                                    
         CLI   INPPCLSS+1,C' '                                                  
         BNH   PRDU055                                                          
*                                                                               
         NI    INPPCLSS,X'0F'        HOLD AS 2 NIBBLES 1-9                      
         NI    INPPCLSS+1,X'0F'                                                 
         PACK  BYTE,INPPCLSS(1)      1ST CLASS                                  
         OC    BYTE,INPPCLSS+1       2ND CLASS                                  
PRDU055  MVC   QPCLASS,BYTE                                                     
*                                                                               
         LA    R4,IOKEY                                                         
         USING CLTRECD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,SVBAGM                                                    
         MVC   CKEYCLT,SVBCLT                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CLTREC'                        
         JNE   ERR00014              INVALID CLIENT                             
         DROP  R4                                                               
*                                                                               
         CLI   QPRECACT,C'A'                                                    
         BNE   PRDU060                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+#CLTREC'                    
         BE    PRDU070                                                          
         DC    H'0'                                                             
*                                                                               
PRDU060  CLI   QPRECACT,C'C'                                                    
         JNE   ERR00428              REQ OPTION (&T) MISSING                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#CLTREC'                       
         BE    PRDU070                                                          
         DC    H'0'                                                             
*                                                                               
PRDU070  ICM   RE,15,I$PPRD                                                     
         JZ    ERR01025              INVALID PRODUCT                            
*                                                                               
         MVC   SVQPRD,LW_DATA1                                                  
         CLI   SVQPRD,C'A'           FIRST CHARACTER MUST BE ALPHA              
         JL    ERR00537                                                         
         CLI   SVQPRD,C'Z'                                                      
         JH    ERR00537                                                         
*                                                                               
PRDU100  SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)         R3 = # OF CHARS IN PROD                    
         CHI   R3,2                                                             
         JL    ERR00538              PROD CODE MUST BE 2-3 CHARS                
*                                                                               
PRDU110  LA    R1,SVQPRD+1           R1 = A(OF LAST NON SPACE CHAR)             
         BH    PRDU120               WE HAVE 3 CHAR PRODUCT                     
         MVI   SVQPRD+2,C' '         2 CHAR PROD, SPACE PAD                     
         B     PRDU130                                                          
*                                                                               
PRDU120  L     R4,AIO2                                                          
*&&DO                                                                           
** This has been commented out so that we can allow Organizer                   
**   to add the Trade product without MF involvement                            
         TM    COPT2-CLTRECD(R4),COP2DIY                                        
         BZ    *+12                                                             
*&&                                                                             
         CLI   1(R1),C'#'          CHECK 3RD CHAR IS # OR ALPHANUMERIC          
         BE    PRDU130                                                          
         CLI   1(R1),C'A'                                                       
         JL    ERR01025                                                         
         CLI   1(R1),C'Z'                                                       
         BNH   PRDU130                                                          
         CLI   1(R1),C'0'                                                       
         JL    ERR01025                                                         
         CLI   1(R1),C'9'                                                       
         JH    ERR01025            INVALID OTHERWISE                            
*                                                                               
PRDU130  CLI   0(R1),C'A'          CHECK LAST CHARS ARE ALPHA-NUMERIC           
         JL    ERR01025                                                         
         CLI   0(R1),C'Z'                                                       
         BNH   PRDU160                                                          
         CLI   0(R1),C'0'                                                       
         JL    ERR01025                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR01025            INVALID OTHERWISE                            
*                                                                               
PRDU160  CLC   SVQPRD,=C'ZZZ'        DON'T ALLOW ADD OF PRD CODE ZZZ            
         JE    ERR00535                                                         
PRDU165  CLC   SVQPRD,=C'UNA'        DON'T ALLOW ADD OF PRD CODE UNA            
         JE    ERR01303                                                         
PRDU170  CLC   SVQPRD,=C'ALL'        DON'T ALLOW ADD OF PRD CODE ALL            
         JE    ERR00540                                                         
PRDU175  CLC   SVQPRD,=C'NO '        DON'T ALLOW ADD OF PRD CODE NO             
         JE    ERR00541                                                         
*                                                                               
PRDU180  L     R4,AIO1             AGENCY USING TRADE?                          
         TM    AGYFLAG1-AGYRECD(R4),AGYTRDQ    X'02'                            
         BNZ   PRDU185                                                          
         L     R4,AIO2             CLIENT USING TRADE?                          
         TM    COPT2-CLTRECD(R4),COP2TRAD                                       
         BZ    PRDU190                                                          
PRDU185  L     R4,AIO2             VALTRD NEEDS THE CLIENT RECORD               
         BRAS  RE,VALTRD           CASH PRD EXISTS- <127 NON TRD PRDS           
         BNE   PRDUXN                                                           
*                                                                               
PRDU190  LA    R2,IOKEY                                                         
         USING PRDRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PKEYAM,SVBAGM                                                    
         MVC   PKEYCLT,SVBCLT                                                   
         MVC   PKEYPRD,SVQPRD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         JNE   *+2                                                              
         L     R2,AIO7                                                          
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         BE    PRDU200             FOUND IT!                                    
*                                                                               
         CLI   QPRECACT,C'A'       DIDN'T FIND IT, ACTION MUST BE ADD           
         JNE   ERR00971            RECORD NOT FOUND                             
*                                                                               
* LACK OF THIS INTRUCTION CAUSED CANADTV TO EXIT PREMATURELY AS THERE           
* WAS NO GUARANTEE WE STILL HAD A TV PRODUCT RECORD                             
         MVC   IOKEY,IOKEYSAV           RESTORE KEY                             
*                                                                               
         MVC   0(L'PKEY,R2),IOKEYSAV    SET THE PRODUCT KEY IN REC              
**       MVC   PLEN,=AL2(PRDHDRL)       SET RECORD LENGTH                       
         MVC   PLEN,=AL2(PRDHDRNL)      ON ADD, USE NEW RECORD LENGTH           
         B     PRDU210                                                          
*                                                                               
PRDU200  CLI   QPRECACT,C'C'       FOUND IT, ACTION MUST BE CHANGE              
         JNE   ERR00049                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO7'                        
         JNE   *+2                                                              
*                                                                               
PRDU210  MVC   SVOFFICE,POFFICE                                                 
         MVC   PCODE,QPCODE                                                     
*                                                                               
         OC    APBILBAS,APBILBAS        ANY PBILLBAS SENT?                      
         BZ    PRDU220                                                          
         L     RE,APBILBAS                                                      
         USING LW_D,RE                                                          
         MVC   QPBILLBAS,LW_DATA1       SAVE OFF THE VALUE PC SENT              
         B     PRDU260                                                          
         DROP  RE                                                               
****************************************                                        
* NEW Style: program to an interface, not an implementation                     
*   JAVA should not care about the bits that need to be set.  All it            
*     should care about is the information that MF needs to compose             
*     the settings in PBILLBAS.  That would be: BILL BASIS, COMM ONLY,          
*     COMM BASIS, & COMM PCT                                                    
****************************************                                        
PRDU220  MVI   QPBILLBAS,0                                                      
         CLI   QBILLBAS,0               ANY BILL BASIS?                         
         BNE   PRDU225                  YES, WE HAVE SOMETHING                  
         CLI   QCOMMBAS,0               ANY COMM BASIS?                         
         BE    PRDU260                  NEITHER COMM NOR BILL BASIS             
         B     PRDU230                  NO, BILL BASIS, BUT COMM BASIS          
*                                                                               
PRDU225  CLI   QBILLBAS,C'N'            BILL BASIS IS NET?                      
         BNE   PRDU230                  NO, HAS TO BE GROSS                     
         OI    QPBILLBAS,X'10'          YES                                     
         B     PRDU240                                                          
*                                                                               
PRDU230  OC    QPBILLCOM,QPBILLCOM      ANY COMM PCT?                           
         BNZ   PRDU240                  YES, THEN GROSS IS OKAY                 
         OI    QPBILLBAS,X'80'          NO, GROSS IS SPECIAL THEN               
*                                                                               
PRDU240  CLI   QCOMMBIL,C'Y'            COMM ONLY BILLING?                      
         BNE   PRDU250                                                          
         OI    QPBILLBAS,X'40'                                                  
*                                                                               
PRDU250  CLI   QCOMMBAS,C'N'            COMM BASIS IS NET?                      
         BNE   PRDU260                                                          
         OI    QPBILLBAS,X'01'                                                  
*                                                                               
PRDU260  CLC   LP_VRSN,=AL1(01,00,05,00)                                        
         JL    PRDU261                                                          
*                                                                               
         LA    R1,QPRDPSTC                                                      
         BRAS  RE,SETPSTC          Set PST Codes                                
         MVC   QPPST,QPRDPSTC                                                   
*                                                                               
         LA    R1,QPMPST_C                                                      
         BRAS  RE,SETMPSC          Set Main PST - returned in HALF1             
         MVC   QPMPST,HALF1                                                     
*                                                                               
PRDU261  CLI   QPRECACT,C'A'       CLEAR PRODUCT GROUPS IF ADD                  
         BNE   PRDU262                                                          
         XC    QPGRP1(3*L'QPGRP1),QPGRP1  PGRP1, 2 & 3 ARE TOGETHER             
         XC    QPGRP4(2*L'QPGRP4),QPGRP4  PGRP4 & 5 ARE TOGETHER                
         XC    QPGRP6,PGRP6               PGRP6 IS SOLO BUT BIGGER              
*                                                                               
PRDU262  CLC   QPNAME,SPACES       If no name, default to prd code              
         JH    *+10                                                             
         MVC   QPNAME(L'SVQPRD),SVQPRD                                          
*                                                                               
* Any changes to product record from here will not be saved                     
*                                                                               
         LA    R2,PACCT                   MOVE CONTENTS OF QPACCT TO            
         LHI   R3,IO7LQ-(PACCT-PRDRECD)     THE PRODUCT RECORD                  
         LA    RE,QPACCT                    FOR ALL THOSE MANY BYTES            
         LR    RF,R3                                                            
         MVCL  R2,RE                                                            
         DROP  R2                                                               
*                                                                               
         BRAS  RE,PRDELM                                                        
         BNE   PRDUXN                                                           
*                                                                               
         L     R2,AIO7                                                          
         MVC   SVIOKEY,0(R2)       SAVE KEY FROM REC, DON'T ASSUME              
*                                    IOKEY HAS CORRECT KEY                      
********                                                                        
* ON CHANGE                                                                     
********                                                                        
         CLI   QPRECACT,C'C'       CHANGE ACTION?                               
         BNE   PRDU265             NO, WE MUST BE ADD THEN                      
*                                                                               
         USING PRDRECD,R2                                                       
         CLC   PLEN,=AL2(PRDHDRL)  RECORD AT THE OLD LENGTH?                    
         JNE   PRDU263M                                                         
         OC    PSAPCODE,PSAPCODE   ANY DATA HERE TO CHANGE TO NEW LEN?          
         JZ    PRDU263M                                                         
         MVC   PLEN,=AL2(PRDHDRNL)  USE NEW RECORD LENGTH                       
* SPOT HEADER RECORDS CANNOT CHANGE LENGTHS                                     
* ONLY WAY IS TO ADD A NEW RECORD AND THEN MODIFY THE D/A OF THE KEY            
* HAVE TO IGNORE DUPLICATE ON ADD THOUGH                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+IO7'  ADD OF PRD            
         TM    IOERR,IOEDUP       DUPLICATE KEY ON ADD?                         
         JNZ   *+12               WE KNEW THIS WOULD HAPPEN                     
         CLI   IOERR,0                                                          
         JNE   *+2                                                              
         MVC   SVIOKEY,IOKEY                                                    
         MVC   SVIOKEY+14(L'IODA),IODA                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+IO7'                          
         CLC   IOKEY(L'PKEY),SVIOKEY                                            
         JNE   *+2                                                              
         MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO7'                           
         JNE   *+2                                                              
         J     PRDU264                                                          
         DROP  R2                                                               
*                                                                               
PRDU263M GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO7'                        
*                                                                               
PRDU264  BRAS  RE,CANADTV                                                       
         BNE   PRDU270             Go process passive pointers                  
*                                                                               
         XC    IOKEY,IOKEY         Set key for Network prod record              
         MVC   IOKEY(L'PKEY),SVIOKEY                                            
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'03'       Change to Network                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,AIO7                                                          
         MVC   PKEYAM-PKEY(L'PKEYAM,RE),IOKEY+1                                 
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO7LQ                                                         
         LHI   RF,IO7LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         XC    IOKEY,IOKEY         Set key for Combined prod record             
         MVC   IOKEY(L'PKEY),SVIOKEY                                            
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'08'       Change to Combined                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,AIO7                                                          
         MVC   PKEYAM-PKEY(L'PKEYAM,RE),IOKEY+1                                 
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO7LQ                                                         
         LHI   RF,IO7LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         MVC   IOKEY,SVIOKEY       RESTORE KEY                                  
*                                                                               
         B     PRDU270             Go process passive pointers                  
********                                                                        
* ON ADD                                                                        
********                                                                        
PRDU265  CLI   QPRECACT,C'A'       ADD ACTION?                                  
         JNE   *+2                                                              
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+#CLTREC' FOR CLIST          
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+IO7'     Add of prd         
         JNE   *+2                                                              
*                                                                               
         BRAS  RE,CANADTV                                                       
         BNE   PRDUX                                                            
*                                                                               
         LA    RE,IOKEY            Update the client network record             
         USING CLTRECD,RE                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   CKEYAM,SVBAGM                                                    
         MVC   CKEYCLT,SVBCLT                                                   
         NI    CKEYAM,X'F0'                                                     
         OI    CKEYAM,X'03'        Change to Network                            
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),IOKEY+1                                 
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO2LQ                                                         
         LHI   RF,IO2LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         MVC   IOKEY,SVIOKEY                                                    
         NI    IOKEY+1,X'F0'       Add media N Product                          
         OI    IOKEY+1,X'03'         media x'03' is N                           
         L     RE,AIO7                                                          
         MVC   PKEYAM-PKEY(L'PKEYAM,RE),IOKEY+1                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+IO7'     Add of prd         
         JNE   *+2                                                              
*                                                                               
         LA    RE,IOKEY                                                         
         USING CLTRECD,RE                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   CKEYAM,SVBAGM                                                    
         MVC   CKEYCLT,SVBCLT                                                   
         NI    CKEYAM,X'F0'                                                     
         OI    CKEYAM,X'08'        Change to Combined                           
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
         L     RE,ACLTREC                                                       
         MVC   CKEYAM-CKEY(L'CKEYAM,RE),IOKEY+1                                 
         L     R0,AIO4             Copy updated record to AIO4                  
         LHI   R1,IO2LQ                                                         
         LHI   RF,IO2LQ                                                         
         MVCL  R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO4'                        
         JNE   *+2                                                              
*                                                                               
         MVC   IOKEY,SVIOKEY                                                    
         NI    IOKEY+1,X'F0'       Add media C Product                          
         OI    IOKEY+1,X'08'         media x'08' is C                           
         L     RE,AIO7                                                          
         MVC   PKEYAM-PKEY(L'PKEYAM,RE),IOKEY+1                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+IO7'     Add of prd         
         JNE   *+2                                                              
*                                                                               
PRDU270  BRAS  RE,POFPTR           HANDLE PRODUCT OFFICE PASSIVES PTR           
*                                                                               
PRDUX    J     EXITY                                                            
PRDUXN   J     EXITN                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TRADE PRODUCT                                                        
*        - CASH PRD MUST EXIST AND MAX OF 127 NON TRADE PRODUCTS                
*                                                                               
*   DIY TRADE IS NO LONGER BEING USED SO ORGANIZER IS ONLY HANDLING             
*    STANDARD TRADE. IT WILL AUTOMATICALLY ADD __# AFTER __C.                   
*                                                                               
*  ON ENTRY : R4 = A(CLIENT RECORD)                                             
***********************************************************************         
         USING CLTRECD,R4                                                       
VALTRD   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVQPRD+2,C'#'       ORGANIZER SENDING TRADE PRODUCT?             
         BNE   VTRDXY                                                           
*                                                                               
         MVC   WORK(3),SVQPRD                                                   
         MVI   WORK+2,C'C'         LOOK FOR CASH PRD ENDING IN C                
*                                                                               
         XR    R1,R1               R1 COUNTS HOW MANY CASH PRODUCTS             
         LA    RE,CLIST                                                         
VTRD10   CLI   0(RE),0                                                          
         JE    ERR00730                                                         
*                                                                               
         TM    2(RE),C'#'          SKIP TRADE PRDS AND POL                      
         JE    VTRD15                                                           
         CLC   =C'POL',0(RE)                                                    
         JE    VTRD15                                                           
         LA    R1,1(R1)            +1 TO # OF CASH PRODUCTS                     
*                                                                               
VTRD15   CLC   0(3,RE),WORK        MATCH                                        
         JE    VTRD20                                                           
*                                                                               
         LA    RE,4(RE)                                                         
         J     VTRD10                                                           
*                                                                               
VTRD20   MVC   SVPRDNUM,3(RE)      SAVE HEX PRD CODE FOR CASH                   
         OI    SVPRDNUM,X'80'      TURN ON TRADE BIT FOR TRADE                  
*                                                                               
VTRD30   LA    RE,4(RE)                                                         
         CLI   0(RE),0             CONTINUE LOOKING FOR BINARY PRDS             
         JE    VTRD40                                                           
*                                                                               
         TM    2(RE),C'#'          SKIP TRADE PRDS AND POL                      
         JE    VTRD30                                                           
         CLC   =C'POL',0(RE)                                                    
         JE    VTRD30                                                           
         LA    R1,1(R1)            +1 TO # OF CASH PRODUCTS                     
         J     VTRD30                                                           
*                                                                               
VTRD40   CHI   R1,127              IF WE HAVE MORE THAN 127 CASH                
         JH    ERR00485            TOO MANY PRODUCTS                            
*                                                                               
VTRDXY   J     EXITY                                                            
*                                                                               
VTRDXN   J     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PW PCT                                                               
***********************************************************************         
VALPWPCT DS    0H                                                               
         LM    R2,R4,LP_AINP                                                    
         XC    0(L'CPWPCT,R4),0(R4)                                             
         CHI   R3,0                ANY INPUT?                                   
         JE    VPWPX               NONE, EXIT.  NULLS = NO PW %                 
*                                                                               
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         PACK  DUB,0(0,R2)                                                      
         EX    R3,0(RE)                                                         
         CVB   R1,DUB                                                           
         ST    R1,0(R4)                                                         
*                                                                               
         OC    0(L'CPWPCT,R4),0(R4)       CHECK FOR 0.0                         
         JNZ   VPWPX                                                            
         OI    0(R4),X'80'         IT IS, ZERO IS STORED SPECIALLY              
*                                                                               
VPWPX    J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COST2 FACTOR                                                         
***********************************************************************         
VALCOS2F DS    0H                                                               
         LM    R2,R4,LP_AINP                                                    
         XC    0(4,R4),0(R4)                                                    
         CHI   R3,0                ANY INPUT?                                   
         JE    VC2FX               NONE, EXIT.  NULLS = NO COS2                 
*                                                                               
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         PACK  DUB,0(0,R2)                                                      
         EX    R3,0(RE)                                                         
         CVB   R1,DUB                                                           
         ST    R1,0(R4)                                                         
*                                                                               
         OC    0(4,R4),0(R4)       CHECK FOR 0.0                                
         JNZ   VC2FX                                                            
         OI    0(R4),X'80'         IT IS, ZERO IS STORED SPECIALLY              
*                                                                               
VC2FX    J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIMIT ACCESS - INPUT IS COMMA SEPARATED                              
***********************************************************************         
VALACCS  DS    0H                                                               
         LM    R2,R4,LP_AINP                                                    
         XC    0(L'CACCESS,R4),0(R4)                                            
         CHI   R3,0                ANY INPUT?                                   
         JE    VACCSXY             NONE.  NULLS = NO LIMIT ACCESS               
*                                                                               
         XC    ELEM1,ELEM1         SIMULATE A FIELD HEADER                      
         L     RE,LP_ATWA                                                       
         LA    RE,64(RE)                                                        
         MVC   ELEM1(8),0(RE)      COPY THE FIELD HEADER                        
         STC   R3,ELEM1+5          SET INPUT LENGTH IN OUR FAKE FLDHDR          
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM1+8(0),0(R2)                                                 
         EX    R3,0(RE)                                                         
*                                                                               
         XC    ELEM2,ELEM2         SCANNER BLOCK                                
         GOTO1 VSCANNER,DMCB,ELEM1,ELEM2,0                                      
         LLC   R3,DMCB+4           NUMBER OF ENTRIES                            
         LTR   R3,R3               ERROR?                                       
         JZ    ERR00002            INVALID INPUT FIELD                          
*                                                                               
VACCS10  LA    R2,ELEM2            R2 = A(SCANNER BLOCK)                        
VACCS15  CLI   0(R2),1             OFFICE HAS TO BE 1 OR 2 CHAR                 
         JE    VACCS20                                                          
         CLI   0(R2),2                                                          
         JNE   ERR00002                                                         
*                                                                               
VACCS20  CLI   12(R2),C' '         OFFICE CAN'T BE BLANK BUT IF USER            
         JE    ERR00002             INPUTS =A, THEN IT WOULD BE BLANK           
*                                                                               
         USING OFFICED,OFCBLK      OFFICER CONTROL BLOCK                        
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,SPTLETQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCAUTH,LP_ACCS                                                  
         MVC   OFCLMT,LP_ACCS                                                   
         MVC   OFCOFC2,12(R2)                                                   
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         JNZ   ERR00544            YES                                          
*                                                                               
VACCS30  MVC   0(1,R4),OFCOFC                                                   
         LA    R4,1(R4)                                                         
         LA    R2,32(R2)                                                        
         JCT   R3,VACCS15                                                       
*                                                                               
VACCSXY  J     EXITY                                                            
VACCSXN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PRODUCT TO CLIST                                                          
***********************************************************************         
PRDELM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO2                                                          
         USING CLTRECD,R4                                                       
         L     R2,AIO7                                                          
         USING PRDRECD,R2                                                       
         LA    R1,CLIST              POINT R1 AT CLIENT'S PRODUCT LIST          
*                                                                               
***********************************************************************         
*        POL PRODUCT SECTION                                                    
***********************************************************************         
*                                                                               
         CLC   SVQPRD,=C'POL'        IF POL PRODUCT ...                         
         BNE   PRDE040               CHANGE 2ND BYTE OF PCODE TO X'FF'          
         MVI   PCODE+1,X'FF'                                                    
PRDE010  CLI   0(R1),0               END OF CLIST?                              
         BE    PRDE020                                                          
         CLI   3(R1),X'FF'           POL ALREADY IN CLIST                       
         BE    PRDEX                                                            
         LA    R1,4(R1)              BUMP TO NEXT PRODUCT                       
         B     PRDE010                                                          
*                                                                               
PRDE020  LA    R3,CLIST+876                                                     
         CR    R1,R3                 CAN'T ADD IF CLIST IS FULL                 
         JNL   ERR00485              TOO MANY PRODUCTS                          
         MVC   0(3,R1),=C'POL'                                                  
         MVI   3(R1),X'FF'                                                      
         B     PRDEX                                                            
*                                                                               
***********************************************************************         
*        HOW MANY PRODUCTS IN THE CLIENT'S LIST?                                
***********************************************************************         
*                                                                               
PRDE040  XC    ELEM(220),ELEM        220 = MAX NUMBER OF PRODUCTS               
         LA    R0,220                IN CLIST ... RE = NUMBER OF                
         SR    RE,RE                 PRODUCTS IN CLIST                          
*                                                                               
PRDE050  CLI   0(R1),0               END OF CLIST?                              
         BE    PRDE075                                                          
         CLI   3(R1),X'FF'                                                      
         BNE   PRDE060                                                          
         LA    RE,1(RE)              POL PRODUCT ... INCREMENT PRO-             
         MVC   0(3,R1),=3X'FF'       DUCT COUNTER AND MODIFY TO BE              
         B     PRDE070               LAST ENTRY OF BINSRCH TABLE                
*                                                                               
PRDE060  ZIC   R5,3(R1)              NON-POL PRODUCT ... SET USED               
         LA    R5,ELEM-1(R5)         FLAG AND INCREMENT PRODUCT                 
         MVI   0(R5),X'FF'           COUNTER                                    
         LA    RE,1(RE)                                                         
PRDE070  LA    R1,4(R1)              BUMP TO NEXT ENTRY OF CLIST                
         B     PRDE050                                                          
PRDE075  STC   RE,PCOUNT             PCOUNT = # OF PRDS IN CLIST                
*                                                                               
***********************************************************************         
*        CHECK TAL TO SEE IF VALID PRODUCT CODE FOR CLIENT                      
***********************************************************************         
*                                                                               
         LA    R1,ELEM               FIND TAL OF PRODUCT IN TALTAB              
         LA    RE,TALTAB                                                        
         CLC   QCLTA,=C'PTA'                                                    
         BNE   *+8                                                              
         LA    RE,TALPTA             SPECIAL FOR JO HO                          
*                                                                               
PRDE080  CLI   0(RE),X'FF'           END OF TALTAB?                             
         JE    *+2                                                              
         CLC   PTAL,0(RE)            TAL FOUND?  (Default is 0)                 
         BE    *+12                                                             
         LA    RE,3(RE)              BUMP TO NEXT TAL                           
         B     PRDE080                                                          
*                                                                               
         ZIC   RF,1(RE)            IS PRODUCT CODE >= MINIMUM FOR               
         BCTR  RF,0                THIS TAL?                                    
         AR    R1,RF               START AT LOW PRD NUM FOR THIS TAL            
PRDE090  CLI   0(R1),0             FIND FIRST FREE ENTRY                        
         BE    PRDE100                                                          
         LA    R1,1(R1)                                                         
         B     PRDE090                                                          
*                                                                               
PRDE100  LA    R0,ELEM-1             IF PRODUCT CODE = MINIMUM TAL,             
         SR    R1,R0                 SAVE INTO RECORD                           
         STCM  R1,3,PCODE                                                       
*                                                                               
         L     R4,AIO2                                                          
         TM    COPT2,COP2TRAD      Trade client?                                
         BZ    PRDE110                                                          
         TM    COPT1,COP1GMI          & GMI Client?                             
         BZ    PRDE105                                                          
         CHI   R1,X'80'            CASH PRODUCT ALREADY >= X'80'?               
         JNL   ERR00485            Too many prds, SFM complains as well         
*                                                                               
PRDE105  CLI   SVQPRD+2,C'#'       FOR TRADE PRODUCT                            
         BNE   PRDE110                                                          
         XC    PCODE,PCODE                                                      
         MVC   PCODE+1(1),SVPRDNUM   PRD NUM = CASH PRD# +X'80'                 
         CLI   PCODE+1,0                                                        
         JE    *+2                                                              
*                                                                               
PRDE110  TM    COPT1,COP1GMI       CLIENT - GENERAL MILLS (CHILDSPOT)           
         BNO   PRDE120                                                          
         TM    COPT2-CLTRECD(R4),COP2TRAD  CLIENT IS A TRADE CLIENT?            
         BZ    PRDE115                     NO                                   
         CLI   SVQPRD+2,C'#'       Are we on the trade product?                 
         BE    PRDE120             then we're okay                              
*                                                                               
PRDE115  ZIC   RF,2(RE)            Cash products we still need to check         
         ZIC   RE,PCODE+1            if prd cODE <= max for this tal            
         CR    RE,RF                                                            
         JH    ERR00488            NTP NOT VALID FOR THIS PRD                   
***********************************************************************         
*        IF TRAFFIC CLIENT MASTER RECORD EXISTS FOR THIS CLIENT ...             
*        TEST THAT PRODUCT IS IN TRAFFIC CLIENT MASTER RECORD                   
***********************************************************************         
*                                                                               
PRDE120  L     R2,AIO7               READ TRAFFIC MASTER CLIENT REC             
         MVC   WORK(3),SVQPRD                                                   
         MVC   WORK+3(1),PCODE+1                                                
         OC    CMCLTCOD,CMCLTCOD     ANY TRAFFIC MASTER CLT?                    
         BZ    PRDE160                                                          
         CLI   CMCLTPRD,0            IF TRAFFIC PRD NO NEED TO CHECK            
         BNE   PRDE160                                                          
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+1(1),PKEY+1     SET UP KEY TO READ TRAFFIC MASTER          
         MVC   IOKEY+2(2),CMCLTCOD   CLIENT RECORD INTO AIO3                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR01017              INVALID TRAFFIC CODE                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   *+2                                                              
*                                                                               
         L     R2,AIO3               PRODUCT MUST BE IN TRAFFIC MASTER          
         LA    R2,CLIST-CLTHDR(R2)   CLIENT'S PRODUCT LIST                      
PRDE130  OC    0(4,R2),0(R2)                                                    
         JZ    ERR00491                                                         
         CLC   SVQPRD,0(R2)          PRODUCT FOUND?                             
         BE    *+12                                                             
         LA    R2,4(R2)              BUMP TO NEXT PRODUCT                       
         B     PRDE130                                                          
*                                                                               
         CLC   PCODE+1(1),3(R2)      SEQUENCE NUMBERS MUST MATCH                
         JNE   ERR00491                                                         
***********************************************************************         
*        ADD PRODUCT TO CLIENT'S PRODUCT LIST                                   
***********************************************************************         
*                                                                               
PRDE160  ZIC   R3,PCOUNT                                                        
         GOTOR VBINSRCH,DMCB,(X'01',WORK),CLIST,(R3),4,(0,3),218                
         OC    DMCB(4),DMCB                                                     
         JZ    ERR00485              TOO MANY PRDS IN CLIST, RTRN ERR           
*                                                                               
PRDE170  CLI   DMCB,1                                                           
         BE    PRDE180                                                          
         L     R1,DMCB               PRODUCT ALREADY IN LIST                    
         MVC   PCODE+1(1),3(R1)      ADDRESS OF PRODUCT IN R1                   
         CLI   PCODE+1,0                                                        
         JE    *+2                                                              
*                                    PRODUCT NOT ALREADY IN LIST                
PRDE180  LA    R1,CLIST              ADDRESS OF CLIST IN R1                     
*                                                                               
PRDE190  CLI   0(R1),0               END OF CLIST?                              
         BE    PRDEX                                                            
         CLC   0(3,R1),=3X'FF'       POL PRODUCT?                               
         BE    PRDE200                                                          
*                                                                               
         CLI   3(R1),0             ** MAKE SURE NO PRD CODES ARE 0 **           
         JE    *+2                                                              
*                                                                               
         LA    R1,4(R1)              BUMP TO NEXT PRODUCT                       
         B     PRDE190                                                          
*                                                                               
PRDE200  MVC   0(3,R1),=C'POL'       RESET POL PRODUCT FROM 3X'FF'              
PRDEX    J     EXITY                                                            
PRDEXN   J     EXITN                                                            
         DROP  R2,R4                                                            
*                                                                               
***********************************************************************         
* ADD OR DELETE OF PASSIVE OFFICE POINTERS                                      
***********************************************************************         
OFCPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
*                                                                               
OFCPTR0  CLC   SVOFFICE,COFFICE    TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
********                                                                        
* DELETE OLD OFFICE (IF ANY)                                                    
********                                                                        
         CLI   SVOFFICE,C' '                                                    
         BNH   OFCPTR10                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0D80'                                                
         MVC   IOKEY+2(1),SVIOKEY+1  A/M                                        
         MVC   IOKEY+9(1),SVOFFICE                                              
         MVC   IOKEY+11(2),SVIOKEY+2 CLT                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+#CLTREC'                      
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   OFCPTR2                                                          
         OI    IOKEY+13,X'80'    MARK RECORD                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+#CLTREC'                       
         JNE   *+2                                                              
*                                                                               
OFCPTR2  L     RE,AIO1                                                          
         CLI   AGYPCNDA-AGYHDR(RE),C'C'   CANADIAN AGENCY                       
         BNE   OFCPTR10                                                         
         CLI   QMEDA,C'T'         TV ONLY                                       
         BNE   OFCPTR10                                                         
********                                                                        
* IF IT'S A CANADIAN CLIENT, NEED TO DELETE OFC PASSIVE FOR MEDIA N             
********                                                                        
         NI    IOKEY+2,X'F0'                                                    
         OI    IOKEY+2,X'03'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+#CLTREC'                      
         JNE   *+2                                                              
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   OFCPTR4                                                          
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+#CLTREC'                       
         JNE   *+2                                                              
********                                                                        
* AND FOR MEDIA C (X'08')                                                       
********                                                                        
OFCPTR4  NI    IOKEY+2,X'F0'                                                    
         OI    IOKEY+2,X'08'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+#CLTREC'                      
         JNE   *+2                                                              
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   OFCPTR10                                                         
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+#CLTREC'                       
         JNE   *+2                                                              
********                                                                        
* NOW ADD NEW (OR UNDELETE)                                                     
********                                                                        
OFCPTR10 CLI   COFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX                                                          
         MVC   IOKEY(20),SVIOKEY                                                
*                                                                               
         BRAS  RE,ADDOFC                                                        
*                                                                               
         L     RE,AIO1                                                          
         CLI   AGYPCNDA-AGYHDR(RE),C'C'   CANADIAN AGENCY                       
         BNE   OFCPTRX                                                          
         CLI   QMEDA,C'T'         TV ONLY                                       
         BNE   OFCPTRX                                                          
********                                                                        
* DO MEDIA N                                                                    
********                                                                        
         MVC   IOKEY(13),SVIOKEY                                                
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'03'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   *+2                                                              
         BRAS  RE,ADDOFC                                                        
********                                                                        
* DO MEDIA C                                                                    
********                                                                        
         MVC   IOKEY(13),SVIOKEY                                                
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'08'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   *+2                                                              
         BRAS  RE,ADDOFC                                                        
*                                                                               
OFCPTRX  J     EXITY                                                            
***********************************************************************         
ADDOFC   NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(20),IOKEY      SAVE CLTHDR KEY AND DISK ADDRESS             
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0D80'                                                
         MVC   IOKEY+2(1),WORK+1   A/M                                          
         MVC   IOKEY+9(1),COFFICE                                               
         MVC   IOKEY+11(2),WORK+2  CLT                                          
         MVC   IOKEY+14(4),WORK+14 SET DISK ADDRESS                             
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+#CLTREC'                     
         BNE   ADDOFC06                                                         
ADDOFC03 CLC   IOKEY(13),IOKEYSAV                                               
         BNE   ADDOFC10                                                         
         NI    IOKEY+13,X'7F'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+#CLTREC'                       
         BE    ADDOFCX                                                          
         DC    H'0'                                                             
*                                                                               
ADDOFC06 CLI   IOERR,2            PASSIVE KEY ALREADY DELETED?                  
         BE    ADDOFC03        THAT IS OKAY, WE'RE RESTORING IT                 
*                                                                               
ADDOFC10 MVC   IOKEY,IOKEYSAV     RESTORE KEY                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
         JNE   *+2                                                              
*                                                                               
ADDOFCX  J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* ADD OR DELETE OF PASSIVE OFFICE POINTERS FOR PRODUCT RECORD                   
***********************************************************************         
POFPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIO7                                                          
         USING PRDRECD,R2                                                       
*                                                                               
POFPTR0  CLC   SVOFFICE,POFFICE    TEST OFFICE CHANGED                          
         BE    POFPTRX             NO                                           
********                                                                        
* DELETE OLD OFFICE (IF ANY)                                                    
********                                                                        
         CLI   SVOFFICE,C' '                                                    
         BNH   POFPTR10                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0DF0'                                                
         MVC   IOKEY+2(1),SVBAGM        A/M                                     
         MVC   IOKEY+3(2),SVBCLT        CLT                                     
         MVC   IOKEY+5(1),SVOFFICE                                              
         MVC   IOKEY+6(3),PKEYPRD       PRODUCT CODE                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   POFPTR2                                                          
         OI    IOKEY+13,X'80'      MARK RECORD FOR DELETION                     
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO3'                           
         JNE   *+2                                                              
*                                                                               
POFPTR2  L     RE,AIO1                                                          
         CLI   AGYPCNDA-AGYHDR(RE),C'C'   CANADIAN AGENCY                       
         BNE   POFPTR10                                                         
         CLI   QMEDA,C'T'         TV ONLY                                       
         BNE   POFPTR10                                                         
********                                                                        
* IF CANADIAN CLIENT, DELETE PASSIVE POINTERS FOR MEDIA N(X'03')                
         NI    IOKEY+2,X'F0'                                                    
         OI    IOKEY+2,X'03'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   POFPTR4                                                          
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO3'                           
         JNE   *+2                                                              
********                                                                        
* AND FOR MEDIA C (X'08')                                                       
POFPTR4  NI    IOKEY+2,X'F0'                                                    
         OI    IOKEY+2,X'08'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   POFPTR10                                                         
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO3'                           
         JNE   *+2                                                              
********                                                                        
* NOW ADD NEW (OR UNDELETE) PASSIVE KEY                                         
********                                                                        
POFPTR10 CLI   POFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   POFPTRX             IF NO NEW OFFICE, NO NEED TO ADD             
         MVC   IOKEY(13),PKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO7'                            
         JNE   *+2                                                              
         MVC   DISKADDR,IODA                                                    
*                                                                               
         BRAS  RE,ADDPOFC                                                       
*                                                                               
         L     RE,AIO1                                                          
         CLI   AGYPCNDA-AGYHDR(RE),C'C'   CANADIAN AGENCY                       
         BNE   POFPTRX                                                          
         CLI   QMEDA,C'T'         TV ONLY                                       
         BNE   POFPTRX                                                          
********                                                                        
* DO MEDIA N                                                                    
********                                                                        
         MVC   IOKEY(13),PKEY                                                   
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'03'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   *+2                                                              
         BRAS  RE,ADDPOFC                                                       
********                                                                        
* DO MEDIA C                                                                    
********                                                                        
         MVC   IOKEY(13),PKEY                                                   
         NI    IOKEY+1,X'F0'                                                    
         OI    IOKEY+1,X'08'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   *+2                                                              
         BRAS  RE,ADDPOFC                                                       
*                                                                               
POFPTRX  J     EXITY                                                            
*                                                                               
ADDPOFC  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0DF0'                                                
         MVC   IOKEY+2(1),IOKEYSAV+1 A/M                                        
         MVC   IOKEY+3(2),IOKEYSAV+2 CLT                                        
         MVC   IOKEY+5(1),POFFICE    OFFICE CODE                                
         MVC   IOKEY+6(3),IOKEYSAV+4 PRODUCT CODE                               
         MVC   IOKEY+14(4),DISKADDR  SET DISK ADDRESS                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IO3'                         
         BNE   ADDPOF06                                                         
ADDPOF03 CLC   IOKEY(13),IOKEYSAV                                               
         BNE   ADDPOF10                                                         
         NI    IOKEY+13,X'7F'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR'                               
         BE    ADDPOFX                                                          
         DC    H'0'                                                             
*                                                                               
ADDPOF06 CLI   IOERR,02        PASSIVE ALREADY DELETED?                         
         BE    ADDPOF03        THAT IS OKAY, WE'RE RESTORING IT                 
*                                                                               
ADDPOF10 MVC   IOKEY,IOKEYSAV  RESTORE KEY                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
         JNE   *+2                                                              
*                                                                               
ADDPOFX  J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
*        TAL TABLE                                                    *         
***********************************************************************         
TALTAB   DC   X'0',X'01',X'3F'                                                  
         DC   X'1',X'40',X'5F'                                                  
         DC   X'2',X'60',X'7F'                                                  
         DC   X'3',X'80',X'9F'                                                  
         DC   X'FF'                                                             
*                                                                               
TALPTA   DC   X'0',X'01',X'5F'                                                  
         DC   X'1',X'60',X'9F'                                                  
         DC   X'2',X'A0',X'BF'                                                  
         DC   X'FF'                                                             
*                                                                               
ERR00258 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGMD)  MEDIA MISSING                 
         J     EXITN                                                            
ERR01023 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVMD)  INVALID MEDIA                 
         J     EXITN                                                            
ERR00966 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGCL)  MISSING CLIENT                
         J     EXITN                                                            
ERR00014 GOTOR PUTERR,DMCB,('SE#SY002',SE#INCLT)  INVALID CLIENT                
         J     EXITN                                                            
ERR00971 GOTOR PUTERR,DMCB,('SE#SY002',SE#RECNF)  RECORD NOT FOUND              
         J     EXITN                                                            
ERR00049 GOTOR PUTERR,DMCB,('SE#SY002',SE#RCXST)  REC ALREADY EXISTS            
         J     EXITN                                                            
ERR00428 GOTOR PUTERR,DMCB,('SE#SY002',SE#ROPTM)  REQ OPTION &T MISSING         
         J     EXITN                                                            
ERR00485 GOTOR PUTERR,DMCB,('SE#SY002',485)  TOO MANY PRODUCTS                  
         J     EXITN                                                            
ERR01025 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVPR)  INVALID PRODUCT CODE          
         J     EXITN                                                            
ERR00537 GOTOR PUTERR,DMCB,('SE#SY002',537)  1ST CHAR MUST BE ALPHA             
         J     EXITN                                                            
ERR00538 GOTOR PUTERR,DMCB,('SE#SY002',538)  PRD CD MUST BE 2-3 CHARS           
         J     EXITN                                                            
ERR00535 GOTOR PUTERR,DMCB,('SE#SY002',535)  DON'T ALLOW PRD CD OF ZZZ          
         J     EXITN                                                            
ERR01303 GOTOR PUTERR,DMCB,('SE#SY002',1303) DON'T ALLOW PRD CD OF UNA          
         J     EXITN                                                            
ERR00540 GOTOR PUTERR,DMCB,('SE#SY002',540)  DON'T ALLOW PRD CD OF ALL          
         J     EXITN                                                            
ERR00541 GOTOR PUTERR,DMCB,('SE#SY002',541)  DON'T ALLOW PRD CD OF NO           
         J     EXITN                                                            
ERR00491 GOTOR PUTERR,DMCB,('SE#SY002',491)  MUST MATCH TRF MASTER PRD          
         J     EXITN                                                            
ERR01017 GOTOR PUTERR,DMCB,('SE#SY002',1017) INVALID TRAFFIC CODE               
         J     EXITN                                                            
ERR00002 GOTOR PUTERR,DMCB,('SE#SY002',SE#ININP)  INVALID INPUT FIELD           
         J     EXITN                                                            
ERR00544 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVOF)  INVALID OFFICE                
         J     EXITN                                                            
ERR00488 GOTOR PUTERR,DMCB,('SE#SY002',488)  NTP NOT VALID FOR THIS PRD         
         J     EXITN                                                            
ERR00730 GOTOR PUTERR,DMCB,('SE#SY002',730)  NO CASH PRODUCT FOUND              
         J     EXITN                                                            
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                                     
***********************************************************************         
*                                                                               
PUTERR   NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(2),2(R1)                                                    
*                                                                               
         L     RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         MVC   LIOBMSYS,0(R1)      GET ERROR MESSAGE FROM SYSTEM                
         DROP  RF                                                               
*                                                                               
         XC    TEMP2,TEMP2                                                      
         CLC   LP_QMAPN,=AL2(I#SDCLTU)                                          
         JNE   *+10                                                             
         MVC   TEMP2(L'QCTOKEN),QCTOKEN                                         
         CLC   LP_QMAPN,=AL2(I#SDPRDU)                                          
         JNE   *+10                                                             
         MVC   TEMP2(L'QPTOKEN),QPTOKEN                                         
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',1)                     
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              *        
               ('LD_CHARQ',TEMP2),(L'QCTOKEN,0)                                 
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),    *        
               WORK,0                                                           
*                                                                               
         OI    MISCFLG1,MF1ERROR                                                
*                                                                               
PUTERRX  J     EXITY                                                            
         EJECT                                                                  
*                                                                               
GETEL    AH    R4,DATADISP                                                      
         J     NEXTEL2                                                          
NEXTEL   CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         AR    R4,R0                                                            
NEXTEL2  CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R4)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R4)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
*                                                                               
PSTVTAB  DC    C'BC',X'01'                                                      
         DC    C'AL',X'02'                                                      
         DC    C'SA',X'03'                                                      
         DC    C'MA',X'04'                                                      
         DC    C'ON',X'05'                                                      
         DC    C'PQ',X'06'                                                      
         DC    C'NB',X'07'                                                      
         DC    C'NS',X'08'                                                      
         DC    C'PE',X'09'                                                      
         DC    C'NF',X'0A'                                                      
         DC    X'00'               End of table                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
*                                                                               
LVALUES  DS    0F                                                               
         EJECT                                                                  
*                                                                               
RECTAB   DS    0XL(RECTABL)                ** RECORD TABLE **                   
         DC    AL2(I#SDCLTU),AL3(CLTUPLD),X'FF'      X'04A0'                    
         DC    AL2(I#SDPRDU),AL3(PRDUPLD),X'FF'      X'04A1'                    
*                                                                               
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
         EJECT                                                                  
TCOFFICE DC    C'Client Office'                                                 
TCPROF1  DC    C'Client CPROF 1'                                                
TCPROF2  DC    C'Client CPROF 2'                                                
TCPROF3  DC    C'Client CPROF 3'                                                
TCPROF4  DC    C'Client CPROF 4'                                                
TCPROF5  DC    C'Client CPROF 5'                                                
TCPROF6  DC    C'Client CPROF 6'                                                
TCPROF7  DC    C'Client CPROF 7'                                                
TCPROF8  DC    C'Client CPROF 8'                                                
TCPROF9  DC    C'Client CPROF 9'                                                
TCPROF10 DC    C'Client CPROF 10'                                               
TCPROF11 DC    C'Client CPROF 11'                                               
TCPROF12 DC    C'Client CPROF 12'                                               
TCPROF13 DC    C'Client CPROF 13'                                               
TCPROF14 DC    C'Client CPROF 14'                                               
TCPROF15 DC    C'Client CPROF 15'                                               
TCXTRA1  DC    C'Client EXTRA 1'                                                
TCXTRA2  DC    C'Client EXTRA 2'                                                
TCXTRA3  DC    C'Client EXTRA 3'                                                
TCXTRA4  DC    C'Client EXTRA 4'                                                
TCXTRA5  DC    C'Client EXTRA 5'                                                
TCXTRA6  DC    C'Client EXTRA 6'                                                
TCXTRA7  DC    C'Client EXTRA 7'                                                
TCXTRA8  DC    C'Client EXTRA 8'                                                
TCXTRA9  DC    C'Client EXTRA 9'                                                
TCXTRA10 DC    C'Client EXTRA 10'                                               
TCXTRA11 DC    C'Client EXTRA 11'                                               
TCXTRA12 DC    C'Client EXTRA 12'                                               
TCXTRA13 DC    C'Client EXTRA 13'                                               
TCXTRA14 DC    C'Client EXTRA 14'                                               
TCXTRA15 DC    C'Client EXTRA 15'                                               
TCGRP1   DC    C'ClientGroup Assign 1'                                          
TCGRP2   DC    C'ClientGroup Assign 2'                                          
TCGRP3   DC    C'ClientGroup Assign 3'                                          
TCGRP4   DC    C'ClientGroup Assign 4'                                          
TCGRP5   DC    C'ClientGroup Assign 5'                                          
TCGRP6   DC    C'ClientGroup Assign 6'                                          
TCGRP7   DC    C'ClientGroup Assign 7'                                          
TCGRP8   DC    C'ClientGroup Assign 8'                                          
TCGRP9   DC    C'ClientGroup Assign 9'                                          
TCGRP10  DC    C'ClientGroup Assign 10'                                         
TNCLTINT DC    C'New Client Interface Code'                                     
TACCOFF  DC    C'2 Char Acc Office Code'                                        
TCMCTCD  DC    C'Master Traff Clt Code'                                         
TCMCTUNQ DC    C'Master Trffc Clt Seqnum'                                       
TCMCTPRD DC    C'Master Trffc Clt Prd Code'                                     
TCACCAGY DC    C'ACC Agency Override'                                           
TCPOLONL DC    C'POL BUYING ONLY'                                               
TCCLTINT DC    C'Client Interface'                                              
TCTITLE  DC    C'ID Title'                                                      
TCPU1    DC    C'Product user Fld Desc 1'                                       
TCPU2    DC    C'Product user Fld Desc 2'                                       
TCPUTYP  DC    C'Product user type'                                             
TCPULEN  DC    C'Product user length'                                           
TUFFLAG1 DC    C'UF1 = Required'                                                
TUFFLAG2 DC    C'UF2 = Show on A2'                                              
TUFFLAG3 DC    C'UF3 = Integ Bills'                                             
TUFFLAG4 DC    C'UF4 = Show on bills'                                           
TUFFLAG5 DC    C'UF5 = Transfer on MX'                                          
TUFFLAG6 DC    C'UF6 = Spec Charge Bills'                                       
TUFFLAG7 DC    C'UF7 = Prt at FRONT of Bil'                                     
TUFFLAG8 DC    C'UF8 = Prt in Hdlns of Bil'                                     
TCEU1    DC    C'Est user field Desc 1'                                         
TCEU2    DC    C'Est user field Desc 2'                                         
TCEUTYP  DC    C'Estimate user type'                                            
TCEULEN  DC    C'Estimate user length'                                          
TMEDNAM  DC    C'Media Name Override'                                           
TNETID   DC    C'Network ID'                                                    
TCO1FG1  DC    C'Second Cost Required'                                          
TCO1FG2  DC    C'Informercial'                                                  
TCO1FG3  DC    C'Dont test double-booking'                                      
TCO1FG4  DC    C'Req MGREQ if ID=MKTGRP'                                        
TCO1FG5  DC    C'Client use new makegoods'                                      
TCO1FG6  DC    C'Cntrct Analysis (CTA) Clt'                                     
TCO1FG7  DC    C'Upld Clt - Keep Del. buys'                                     
TCO1FG8  DC    C'GMI in clt option field'                                       
TCO2FG1  DC    C'Clt Xcld from DB/J1 Extrt'                                     
TCO2FG2  DC    C'No PW billing'                                                 
TCO2FG3  DC    C'Clt Xcld from A7 report'                                       
TCO2FG4  DC    C'Trade Client (Y/N)'                                            
TCO2FG5  DC    C'Clt is frzn - no new spot'                                     
TCO2FG6  DC    C'Cross Estimate Reporting'                                      
TCO2FG7  DC    C'Use buy pgmmg Prof (BP)'                                       
TCO2FG8  DC    C'DIY Trade Client'                                              
TCO3FG1  DC    C'Cos2 Optional (Trade=0)'                                       
TCO3FG2  DC    C'Product Level Security'                                        
TCO3FG3  DC    C'CC2Conv *not* set by SPCW'                                     
TCO3FG4  DC    C'COS2 aplies to INTEG also'                                     
TCO3FG5  DC    C'Allow BRD est w/o POL'                                         
TCO3FG6  DC    C'Time'                                                          
TCO3FG7  DC    C'Time and Integration'                                          
TCO3FG8  DC    C'SPODS Allowed  !'                                              
TCO4FG4  DC    C'Bill PCT Split'                                                
TCO4FG5  DC    C'Timebank exclusion'                                            
TCO4FG6  DC    C'UCOM Bill control'                                             
TCO4FG7  DC    C'TRADE=T - COST2 (LCI)'                                         
TCO4FG8  DC    C'Run ECOST/Billing'                                             
TCPST    DC    C'PST Codes'                                                     
TCMPS    DC    C'Main PST Codes'                                                
TCDAILY  DC    C'Estimates will be daily'                                       
TCPWPCT  DC    C'Profit within percentage'                                      
TCZENCLT DC    C'Zenith client code'                                            
TCPRVCLT DC    C'Prev Clt (for Blockbustr)'                                     
TCNXTCLT DC    C'Next Clt (for Blockbustr)'                                     
TCEDIBVR DC    C'EDI version for Billing'                                       
TCEDIBAD DC    C'EDI address for Billing'                                       
TCCLTTYP DC    C'Test Clt type (MM/SprDsk)'                                     
TCCPPRS  DC    C'CPPRS extract option'                                          
TCIN1OVP DC    C'Overflow prds on this clt'                                     
TCCOST2  DC    C'Clt Cost2 factor (6 dec)'                                      
TCACCESS DC    C'Limit Access'                                                  
TCLOCKYR DC    C'Clt freeze year'                                               
TCLOCKMN DC    C'Clt freeze month'                                              
TCLOCKMP DC    C'Clt freeze month prior'                                        
TCLOCKMS DC    C'Clt freeze month after'                                        
TCRFPGRP DC    C'T/A report Rep Group ID'                                       
TCC2CONV DC    C'Date of Cost 2 conversion'                                     
TCTRFOFC DC    C'Traffic office'                                                
TCLTSLMT DC    C'Subline lim. for FrtRnnr'                                      
TCPRPRD  DC    C'Corporate product'                                             
TPACCT   DC    C'Account number'                                                
TPADDR1  DC    C'Bill Address Line 1'                                           
TPADDR2  DC    C'Bill Address Line 2'                                           
TPADDR3  DC    C'Bill Address Line 3'                                           
TPADDR4  DC    C'Bill Address Line 4'                                           
TPDIV    DC    C'Division Code'                                                 
TPBILLDT DC    C'Effective Y/M of service'                                      
TPBILLBS DC    C'Bill BASE/COMM Base'                                           
TPBLLCOM DC    C'Signed Commission'                                             
TPAGYFEE DC    C'Other Agency fee'                                              
TPPROF   DC    C'Profile'                                                       
TPGRP1   DC    C'PrdGrp Assign 1'                                               
TPGRP2   DC    C'PrdGrp Assign 2'                                               
TPGRP3   DC    C'PrdGrp Assign 3'                                               
TPCLASS  DC    C'Product Class'                                                 
TPGRP4   DC    C'PrdGrp Assing 4'                                               
TPGRP5   DC    C'PrdGrp Assign 5'                                               
TPLOCK   DC    C'PRD locked'                                                    
TPLKDAT  DC    C'PRD lock Actv Date'                                            
TPGSTCD  DC    C'Goods and Service tax'                                         
TPCPPRS  DC    C'CPPRS Extract Option'                                          
TPUSER1  DC    C'User Field 1'                                                  
TPUSER2  DC    C'User Field 2'                                                  
TPPST    DC    C'PST Codes'                                                     
TPTALAGY DC    C'Talent Agency'                                                 
TPTAL    DC    C'Talent Factor Groups'                                          
TPRATE   DC    C'Rate Type'                                                     
TPO1NBL  DC    C'Dp not Bill this product'                                      
TPO1RFC  DC    C'RFC = Y'                                                       
TPO1THTR DC    C'Theatrical'                                                    
TPOFFICE DC    C'Media Office'                                                  
TPTRFOF  DC    C'Traffic Office'                                                
TPACCOFC DC    C'ACC Office'                                                    
TPACCAGY DC    C'ACC Agency Override'                                           
TPGRP6   DC    C'PrdGrp Assign 6'                                               
TSPARE   DC    C'SPARE'                                                         
*                                                                               
FLMBUF   BUFFD TYPE=D,KEYLEN=0,COMLEN=0,BUFFERS=10                              
*                                                                               
B#CLTREC EQU   3                   IO2 - CLIENT RECORD                          
B#PRDREC EQU   9                   IO7 - PRODUCT RECORD                         
*                                                                               
*                                                                               
***********************************************************************         
* KEY - OM ORDER AND MAKEGOOD                                                   
***********************************************************************         
*                                                                               
CLTHEAD  LKREQ H,I#SDCLTU,0,NEWREC=Y         04A0                               
MEDIA    LKREQ F,1,(I,B#SAVED,I$CAM),CHAR,                             *        
               MAXLEN=1,OLEN=1,TEXT=SP#MED,COL=*                                
CLIENT   LKREQ F,2,(I,B#SAVED,I$CCLT),VSTR,                            *        
               MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
CNAME    LKREQ F,3,(D,B#SAVED,QCNAME),CHAR,TEXT=SP#CLINA,COL=*                  
COFFICE  LKREQ F,4,(D,B#SAVED,SVCOFFIC),(R,VALACCS),                   +        
               TEXT=(*,TCOFFICE),COL=*                                          
                                                                                
CPROF1   LKREQ F,20,(D,B#SAVED,QCPROF1),CHAR,TEXT=(*,TCPROF1),COL=*             
CPROF2   LKREQ F,21,(D,B#SAVED,QCPROF2),CHAR,TEXT=(*,TCPROF2),COL=*             
CPROF3   LKREQ F,22,(D,B#SAVED,QCPROF3),CHAR,TEXT=(*,TCPROF3),COL=*             
CPROF4   LKREQ F,23,(D,B#SAVED,QCPROF4),CHAR,TEXT=(*,TCPROF4),COL=*             
CPROF5   LKREQ F,24,(D,B#SAVED,QCPROF5),CHAR,TEXT=(*,TCPROF5),COL=*             
CPROF6   LKREQ F,25,(D,B#SAVED,QCPROF6),CHAR,TEXT=(*,TCPROF6),COL=*             
CPROF7   LKREQ F,26,(D,B#SAVED,QCPROF7),CHAR,TEXT=(*,TCPROF7),COL=*             
CPROF8   LKREQ F,27,(D,B#SAVED,QCPROF8),CHAR,TEXT=(*,TCPROF8),COL=*             
CPROF9   LKREQ F,28,(D,B#SAVED,QCPROF9),CHAR,TEXT=(*,TCPROF9),COL=*             
CPROF10  LKREQ F,29,(D,B#SAVED,QCPROF10),CHAR,TEXT=(*,TCPROF10),COL=*           
CPROF11  LKREQ F,30,(D,B#SAVED,QCPROF11),CHAR,TEXT=(*,TCPROF11),COL=*           
CPROF12  LKREQ F,31,(D,B#SAVED,QCPROF12),CHAR,TEXT=(*,TCPROF12),COL=*           
CPROF13  LKREQ F,32,(D,B#SAVED,QCPROF13),CHAR,TEXT=(*,TCPROF13),COL=*           
CPROF14  LKREQ F,33,(D,B#SAVED,QCPROF14),CHAR,TEXT=(*,TCPROF14),COL=*           
CPROF15  LKREQ F,34,(D,B#SAVED,QCPROF15),CHAR,TEXT=(*,TCPROF15),COL=*           
                                                                                
CCLTIFC  LKREQ F,40,(D,B#SAVED,QCCLTIFC),CHAR,TEXT=(*,TNCLTINT),COL=*           
CACCOFF  LKREQ F,41,(D,B#SAVED,QCACCOFC),CHAR,TEXT=(*,TACCOFF),COL=*            
CGRP1    LKREQ F,42,(D,B#SAVED,QCGRP1),CHAR,TEXT=(*,TCGRP1),COL=*               
CGRP2    LKREQ F,43,(D,B#SAVED,QCGRP2),CHAR,TEXT=(*,TCGRP2),COL=*               
CGRP3    LKREQ F,44,(D,B#SAVED,QCGRP3),CHAR,TEXT=(*,TCGRP2),COL=*               
CGRP4    LKREQ F,45,(D,B#SAVED,QCGRP4),CHAR,TEXT=(*,TCGRP2),COL=*               
CGRP5    LKREQ F,46,(D,B#SAVED,QCGRP5),CHAR,TEXT=(*,TCGRP2),COL=*               
CMCLTCOD LKREQ F,47,(D,B#SAVED,QCMCLTCOD),CHAR,TEXT=(*,TCMCTCD),COL=*           
CMCLTUNQ LKREQ F,48,(D,B#SAVED,QCMCLTUNQ),CHAR,TEXT=(*,TCMCTUNQ),COL=*          
CMCLTPRD LKREQ F,49,(D,B#SAVED,QCMCLTPRD),CHAR,TEXT=(*,TCMCTPRD),COL=*          
CACCAGY  LKREQ F,50,(D,B#SAVED,QCACCAGY),CHAR,TEXT=(*,TCACCAGY),COL=*           
CPOLONLY LKREQ F,51,(D,B#SAVED,QCPOLONLY),CHAR,TEXT=(*,TCPOLONL),COL=*          
CCLTINTR LKREQ F,52,(D,B#SAVED,QCCLTINTR),CHAR,TEXT=(*,TCCLTINT),COL=*          
*                                                                               
CEXTRA1  LKREQ F,60,(D,B#SAVED,QCEXTRA1),CHAR,TEXT=(*,TCXTRA1),COL=*            
CEXTRA2  LKREQ F,61,(D,B#SAVED,QCEXTRA2),CHAR,TEXT=(*,TCXTRA2),COL=*            
CEXTRA3  LKREQ F,62,(D,B#SAVED,QCEXTRA3),CHAR,TEXT=(*,TCXTRA3),COL=*            
CEXTRA4  LKREQ F,63,(D,B#SAVED,QCEXTRA4),CHAR,TEXT=(*,TCXTRA4),COL=*            
CEXTRA5  LKREQ F,64,(D,B#SAVED,QCEXTRA5),CHAR,TEXT=(*,TCXTRA5),COL=*            
CEXTRA6  LKREQ F,65,(D,B#SAVED,QCEXTRA6),CHAR,TEXT=(*,TCXTRA6),COL=*            
CEXTRA7  LKREQ F,66,(D,B#SAVED,QCEXTRA7),CHAR,TEXT=(*,TCXTRA7),COL=*            
CEXTRA8  LKREQ F,67,(D,B#SAVED,QCEXTRA8),CHAR,TEXT=(*,TCXTRA8),COL=*            
CEXTRA9  LKREQ F,68,(D,B#SAVED,QCEXTRA9),CHAR,TEXT=(*,TCXTRA9),COL=*            
CEXTRA10 LKREQ F,69,(D,B#SAVED,QCEXTRA10),CHAR,TEXT=(*,TCXTRA10),COL=*          
CEXTRA11 LKREQ F,70,(D,B#SAVED,QCEXTRA11),CHAR,TEXT=(*,TCXTRA11),COL=*          
CEXTRA12 LKREQ F,71,(D,B#SAVED,QCEXTRA12),CHAR,TEXT=(*,TCXTRA12),COL=*          
CEXTRA13 LKREQ F,72,(D,B#SAVED,QCEXTRA13),CHAR,TEXT=(*,TCXTRA13),COL=*          
CEXTRA14 LKREQ F,73,(D,B#SAVED,QCEXTRA14),CHAR,TEXT=(*,TCXTRA14),COL=*          
CEXTRA15 LKREQ F,74,(D,B#SAVED,QCEXTRA15),CHAR,TEXT=(*,TCXTRA15),COL=*          
                                                                                
CTITLE   LKREQ F,80,(D,B#SAVED,QCTITLE),CHAR,TEXT=(*,TCTITLE),COL=*             
                                                                                
CPU1     LKREQ F,90,(D,B#SAVED,QCPU1),CHAR,TEXT=(*,TCPU1),COL=*                 
CPU1TYPE LKREQ F,91,(D,B#SAVED,QCPU1TYPE),CHAR,TEXT=(*,TCPUTYP),COL=*           
CPU1LEN  LKREQ F,92,(D,B#SAVED,QCPU1LEN),LBIN,TEXT=(*,TCPULEN),COL=*            
CPU1F180 LKREQ F,93,(D,B#SAVED,QCPU1FLG1),MB80,TEXT=(*,TUFFLAG1),COL=*          
CPU1F140 LKREQ F,94,(D,B#SAVED,QCPU1FLG1),MB40,TEXT=(*,TUFFLAG2),COL=*          
CPU1F120 LKREQ F,95,(D,B#SAVED,QCPU1FLG1),MB20,TEXT=(*,TUFFLAG3),COL=*          
CPU1F110 LKREQ F,96,(D,B#SAVED,QCPU1FLG1),MB10,TEXT=(*,TUFFLAG4),COL=*          
CPU1F108 LKREQ F,97,(D,B#SAVED,QCPU1FLG1),MB08,TEXT=(*,TUFFLAG5),COL=*          
CPU1F104 LKREQ F,98,(D,B#SAVED,QCPU1FLG1),MB04,TEXT=(*,TUFFLAG6),COL=*          
CPU1F102 LKREQ F,99,(D,B#SAVED,QCPU1FLG1),MB02,TEXT=(*,TUFFLAG7),COL=*          
CPU1F101 LKREQ F,100,(D,B#SAVED,QCPU1FLG1),MB01,TEXT=(*,TUFFLAG8),COL=*         
CPU1F280 LKREQ F,101,(D,B#SAVED,QCPU1FLG2),MB80,TEXT=(*,TUFFLAG1),COL=*         
CPU1F240 LKREQ F,102,(D,B#SAVED,QCPU1FLG2),MB40,TEXT=(*,TUFFLAG2),COL=*         
CPU1F220 LKREQ F,103,(D,B#SAVED,QCPU1FLG2),MB20,TEXT=(*,TUFFLAG3),COL=*         
CPU1F210 LKREQ F,104,(D,B#SAVED,QCPU1FLG2),MB10,TEXT=(*,TUFFLAG4),COL=*         
CPU1F208 LKREQ F,105,(D,B#SAVED,QCPU1FLG2),MB08,TEXT=(*,TUFFLAG5),COL=*         
CPU1F204 LKREQ F,106,(D,B#SAVED,QCPU1FLG2),MB04,TEXT=(*,TUFFLAG6),COL=*         
CPU1F202 LKREQ F,107,(D,B#SAVED,QCPU1FLG2),MB02,TEXT=(*,TUFFLAG7),COL=*         
CPU1F201 LKREQ F,108,(D,B#SAVED,QCPU1FLG2),MB01,TEXT=(*,TUFFLAG8),COL=*         
                                                                                
CPU2     LKREQ F,110,(D,B#SAVED,QCPU2),CHAR,,TEXT=(*,TCPU2),COL=*               
CPU2TYPE LKREQ F,111,(D,B#SAVED,QCPU2TYPE),CHAR,TEXT=(*,TCPUTYP),COL=*          
CPU2LEN  LKREQ F,112,(D,B#SAVED,QCPU2LEN),LBIN,TEXT=(*,TCPULEN),COL=*           
CPU2F180 LKREQ F,113,(D,B#SAVED,QCPU2FLG1),MB80,TEXT=(*,TUFFLAG1),COL=*         
CPU2F140 LKREQ F,114,(D,B#SAVED,QCPU2FLG1),MB40,TEXT=(*,TUFFLAG2),COL=*         
CPU2F120 LKREQ F,115,(D,B#SAVED,QCPU2FLG1),MB20,TEXT=(*,TUFFLAG3),COL=*         
CPU2F110 LKREQ F,116,(D,B#SAVED,QCPU2FLG1),MB10,TEXT=(*,TUFFLAG4),COL=*         
CPU2F108 LKREQ F,117,(D,B#SAVED,QCPU2FLG1),MB08,TEXT=(*,TUFFLAG5),COL=*         
CPU2F104 LKREQ F,118,(D,B#SAVED,QCPU2FLG1),MB04,TEXT=(*,TUFFLAG6),COL=*         
CPU2F102 LKREQ F,119,(D,B#SAVED,QCPU2FLG1),MB02,TEXT=(*,TUFFLAG7),COL=*         
CPU2F101 LKREQ F,120,(D,B#SAVED,QCPU2FLG1),MB01,TEXT=(*,TUFFLAG8),COL=*         
CPU2F280 LKREQ F,121,(D,B#SAVED,QCPU2FLG2),MB80,TEXT=(*,TUFFLAG1),COL=*         
CPU2F240 LKREQ F,122,(D,B#SAVED,QCPU2FLG2),MB40,TEXT=(*,TUFFLAG2),COL=*         
CPU2F220 LKREQ F,123,(D,B#SAVED,QCPU2FLG2),MB20,TEXT=(*,TUFFLAG3),COL=*         
CPU2F210 LKREQ F,124,(D,B#SAVED,QCPU2FLG2),MB10,TEXT=(*,TUFFLAG4),COL=*         
CPU2F208 LKREQ F,125,(D,B#SAVED,QCPU2FLG2),MB08,TEXT=(*,TUFFLAG5),COL=*         
CPU2F204 LKREQ F,126,(D,B#SAVED,QCPU2FLG2),MB04,TEXT=(*,TUFFLAG6),COL=*         
CPU2F202 LKREQ F,127,(D,B#SAVED,QCPU2FLG2),MB02,TEXT=(*,TUFFLAG7),COL=*         
CPU2F201 LKREQ F,128,(D,B#SAVED,QCPU2FLG2),MB01,TEXT=(*,TUFFLAG8),COL=*         
                                                                                
CEU1     LKREQ F,130,(D,B#SAVED,QCEU1),CHAR,,TEXT=(*,TCEU1),COL=*               
CEU1TYPE LKREQ F,131,(D,B#SAVED,QCEU1TYPE),CHAR,TEXT=(*,TCEUTYP),COL=*          
CEU1LEN  LKREQ F,132,(D,B#SAVED,QCEU1LEN),LBIN,TEXT=(*,TCEULEN),COL=*           
CEU2F180 LKREQ F,133,(D,B#SAVED,QCEU1FLG1),MB80,TEXT=(*,TUFFLAG1),COL=*         
CEU1F140 LKREQ F,134,(D,B#SAVED,QCEU1FLG1),MB40,TEXT=(*,TUFFLAG2),COL=*         
CEU1F120 LKREQ F,135,(D,B#SAVED,QCEU1FLG1),MB20,TEXT=(*,TUFFLAG3),COL=*         
CEU1F110 LKREQ F,136,(D,B#SAVED,QCEU1FLG1),MB10,TEXT=(*,TUFFLAG4),COL=*         
CEU1F108 LKREQ F,137,(D,B#SAVED,QCEU1FLG1),MB08,TEXT=(*,TUFFLAG5),COL=*         
CEU1F104 LKREQ F,138,(D,B#SAVED,QCEU1FLG1),MB04,TEXT=(*,TUFFLAG6),COL=*         
CEU1F102 LKREQ F,139,(D,B#SAVED,QCEU1FLG1),MB02,TEXT=(*,TUFFLAG7),COL=*         
CEU1F101 LKREQ F,140,(D,B#SAVED,QCEU1FLG1),MB01,TEXT=(*,TUFFLAG8),COL=*         
CEU1F280 LKREQ F,141,(D,B#SAVED,QCEU1FLG2),MB80,TEXT=(*,TUFFLAG1),COL=*         
CEU1F240 LKREQ F,142,(D,B#SAVED,QCEU1FLG2),MB40,TEXT=(*,TUFFLAG2),COL=*         
CEU1F220 LKREQ F,143,(D,B#SAVED,QCEU1FLG2),MB20,TEXT=(*,TUFFLAG3),COL=*         
CEU1F210 LKREQ F,144,(D,B#SAVED,QCEU1FLG2),MB10,TEXT=(*,TUFFLAG4),COL=*         
CEU1F208 LKREQ F,145,(D,B#SAVED,QCEU1FLG2),MB08,TEXT=(*,TUFFLAG5),COL=*         
CEU1F204 LKREQ F,146,(D,B#SAVED,QCEU1FLG2),MB04,TEXT=(*,TUFFLAG6),COL=*         
CEU1F202 LKREQ F,147,(D,B#SAVED,QCEU1FLG2),MB02,TEXT=(*,TUFFLAG7),COL=*         
CEU1F201 LKREQ F,148,(D,B#SAVED,QCEU1FLG2),MB01,TEXT=(*,TUFFLAG8),COL=*         
                                                                                
CEU2     LKREQ F,150,(D,B#SAVED,QCEU2),CHAR,,TEXT=(*,TCEU2),COL=*               
CEU2TYPE LKREQ F,151,(D,B#SAVED,QCEU2TYPE),CHAR,TEXT=(*,TCEUTYP),COL=*          
CEU2LEN  LKREQ F,152,(D,B#SAVED,QCEU2LEN),LBIN,TEXT=(*,TCEULEN),COL=*           
CEU2F180 LKREQ F,153,(D,B#SAVED,QCEU2FLG1),MB80,TEXT=(*,TUFFLAG1),COL=*         
CEU2F140 LKREQ F,154,(D,B#SAVED,QCEU2FLG1),MB40,TEXT=(*,TUFFLAG2),COL=*         
CEU2F120 LKREQ F,155,(D,B#SAVED,QCEU2FLG1),MB20,TEXT=(*,TUFFLAG3),COL=*         
CEU2F110 LKREQ F,156,(D,B#SAVED,QCEU2FLG1),MB10,TEXT=(*,TUFFLAG4),COL=*         
CEU2F108 LKREQ F,157,(D,B#SAVED,QCEU2FLG1),MB08,TEXT=(*,TUFFLAG5),COL=*         
CEU2F104 LKREQ F,158,(D,B#SAVED,QCEU2FLG1),MB04,TEXT=(*,TUFFLAG6),COL=*         
CEU2F102 LKREQ F,159,(D,B#SAVED,QCEU2FLG1),MB02,TEXT=(*,TUFFLAG7),COL=*         
CEU2F101 LKREQ F,160,(D,B#SAVED,QCEU2FLG1),MB01,TEXT=(*,TUFFLAG8),COL=*         
CEU2F280 LKREQ F,161,(D,B#SAVED,QCEU2FLG2),MB80,TEXT=(*,TUFFLAG1),COL=*         
CEU2F240 LKREQ F,162,(D,B#SAVED,QCEU2FLG2),MB40,TEXT=(*,TUFFLAG2),COL=*         
CEU2F220 LKREQ F,163,(D,B#SAVED,QCEU2FLG2),MB20,TEXT=(*,TUFFLAG3),COL=*         
CEU2F210 LKREQ F,164,(D,B#SAVED,QCEU2FLG2),MB10,TEXT=(*,TUFFLAG4),COL=*         
CEU2F208 LKREQ F,165,(D,B#SAVED,QCEU2FLG2),MB08,TEXT=(*,TUFFLAG5),COL=*         
CEU2F204 LKREQ F,166,(D,B#SAVED,QCEU2FLG2),MB04,TEXT=(*,TUFFLAG6),COL=*         
CEU2F202 LKREQ F,167,(D,B#SAVED,QCEU2FLG2),MB02,TEXT=(*,TUFFLAG7),COL=*         
CEU2F201 LKREQ F,168,(D,B#SAVED,QCEU2FLG2),MB01,TEXT=(*,TUFFLAG8),COL=*         
                                                                                
                                                                                
CMEDNAME LKREQ F,170,(D,B#SAVED,QCMEDNAME),CHAR,TEXT=(*,TMEDNAM),COL=*          
CNETID   LKREQ F,171,(D,B#SAVED,QCNETID),CHAR,TEXT=(*,TNETID),COL=*             
                                                                                
COPT1X80 LKREQ F,180,(D,B#SAVED,QCOPT1),MB80,TEXT=(*,TCO1FG1),COL=*             
COPT1X40 LKREQ F,181,(D,B#SAVED,QCOPT1),MB40,TEXT=(*,TCO1FG2),COL=*             
COPT1X20 LKREQ F,182,(D,B#SAVED,QCOPT1),MB20,TEXT=(*,TCO1FG3),COL=*             
COPT1X10 LKREQ F,183,(D,B#SAVED,QCOPT1),MB10,TEXT=(*,TCO1FG4),COL=*             
COPT1X08 LKREQ F,184,(D,B#SAVED,QCOPT1),MB08,TEXT=(*,TCO1FG5),COL=*             
COPT1X04 LKREQ F,185,(D,B#SAVED,QCOPT1),MB04,TEXT=(*,TCO1FG6),COL=*             
COPT1X02 LKREQ F,186,(D,B#SAVED,QCOPT1),MB02,TEXT=(*,TCO1FG7),COL=*             
COPT1X01 LKREQ F,187,(D,B#SAVED,QCOPT1),MB01,TEXT=(*,TCO1FG8),COL=*             
                                                                                
COPT2X80 LKREQ F,190,(D,B#SAVED,QCOPT2),MB80,TEXT=(*,TCO2FG1),COL=*             
COPT2X40 LKREQ F,191,(D,B#SAVED,QCOPT2),MB40,TEXT=(*,TCO2FG2),COL=*             
COPT2X20 LKREQ F,192,(D,B#SAVED,QCOPT2),MB20,TEXT=(*,TCO2FG3),COL=*             
COPT2X10 LKREQ F,193,(D,B#SAVED,QCOPT2),MB10,TEXT=(*,TCO2FG4),COL=*             
COPT2X08 LKREQ F,194,(D,B#SAVED,QCOPT2),MB08,TEXT=(*,TCO2FG5),COL=*             
COPT2X04 LKREQ F,195,(D,B#SAVED,QCOPT2),MB04,TEXT=(*,TCO2FG6),COL=*             
COPT2X02 LKREQ F,196,(D,B#SAVED,QCOPT2),MB02,TEXT=(*,TCO2FG7),COL=*             
COPT2X01 LKREQ F,197,(D,B#SAVED,QCOPT2),MB01,TEXT=(*,TCO2FG8),COL=*             
                                                                                
COPT3X80 LKREQ F,200,(D,B#SAVED,QCOPT3),MB80,TEXT=(*,TCO3FG1),COL=*             
COPT3X40 LKREQ F,201,(D,B#SAVED,QCOPT3),MB40,TEXT=(*,TCO3FG2),COL=*             
COPT3X20 LKREQ F,202,(D,B#SAVED,QCOPT3),MB20,TEXT=(*,TCO3FG3),COL=*             
COPT3X10 LKREQ F,203,(D,B#SAVED,QCOPT3),MB10,TEXT=(*,TCO3FG4),COL=*             
COPT3X08 LKREQ F,204,(D,B#SAVED,QCOPT3),MB08,TEXT=(*,TCO3FG5),COL=*             
COPT3X04 LKREQ F,205,(D,B#SAVED,QCOPT3),MB04,TEXT=(*,TCO3FG6),COL=*             
COPT3X02 LKREQ F,206,(D,B#SAVED,QCOPT3),MB02,TEXT=(*,TCO3FG7),COL=*             
COPT3X01 LKREQ F,207,(D,B#SAVED,QCOPT3),MB01,TEXT=(*,TCO3FG8),COL=*             
                                                                                
COPT4X80 LKREQ F,210,(D,B#SAVED,QCOPT4),MB80,TEXT=(*,TSPARE),COL=*              
COPT4X40 LKREQ F,211,(D,B#SAVED,QCOPT4),MB40,TEXT=(*,TSPARE),COL=*              
COPT4X20 LKREQ F,212,(D,B#SAVED,QCOPT4),MB20,TEXT=(*,TSPARE),COL=*              
COPT4X10 LKREQ F,213,(D,B#SAVED,QCOPT4),MB10,TEXT=(*,TCO4FG4),COL=*             
COPT4X08 LKREQ F,214,(D,B#SAVED,QCOPT4),MB08,TEXT=(*,TCO4FG5),COL=*             
COPT4X04 LKREQ F,215,(D,B#SAVED,QCOPT4),MB04,TEXT=(*,TCO4FG6),COL=*             
COPT4X02 LKREQ F,216,(D,B#SAVED,QCOPT4),MB02,TEXT=(*,TCO4FG7),COL=*             
COPT4X01 LKREQ F,217,(D,B#SAVED,QCOPT4),MB01,TEXT=(*,TCO4FG8),COL=*             
                                                                                
CPST     LKREQ F,220,(D,B#SAVED,QCPST),CHAR,TEXT=(*,TCPST),COL=*                
CDAILY   LKREQ F,221,(D,B#SAVED,QCDAILY),CHAR,TEXT=(*,TCDAILY),COL=*            
CPWPCT   LKREQ F,222,(D,B#SAVED,QCPWPCT),(R,VALPWPCT),                 +        
               TEXT=(*,TCPWPCT),COL=*                                           
CZENCLT  LKREQ F,223,(D,B#SAVED,QCZENCLT),CHAR,TEXT=(*,TCZENCLT),COL=*          
CPREVCLT LKREQ F,224,(D,B#SAVED,QCPREVCLT),CHAR,TEXT=(*,TCPRVCLT),COL=*         
CNEXTCLT LKREQ F,225,(D,B#SAVED,QCNEXTCLT),CHAR,TEXT=(*,TCNXTCLT),COL=*         
CEDIBVRS LKREQ F,226,(D,B#SAVED,QCEDIBVRS),CHAR,TEXT=(*,TCEDIBVR),COL=*         
CEDIBADR LKREQ F,227,(D,B#SAVED,QCEDIBADR),CHAR,TEXT=(*,TCEDIBAD),COL=*         
CCLTTYPE LKREQ F,228,(D,B#SAVED,QCCLTTYPE),CHAR,TEXT=(*,TCCLTTYP),COL=*         
CCPPRS   LKREQ F,229,(D,B#SAVED,QCCPPRS),CHAR,TEXT=(*,TCCPPRS),COL=*            
                                                                                
CIND1X80 LKREQ F,230,(D,B#SAVED,QCINDS1),MB80,TEXT=(*,TCIN1OVP),COL=*           
                                                                                
CCOST2   LKREQ F,240,(D,B#SAVED,QCCOST2),(R,VALCOS2F),                 +        
               TEXT=(*,TCCOST2),COL=*                                           
CACCESS  LKREQ F,241,(D,B#SAVED,QCACCESS),(R,VALACCS),                 +        
               TEXT=(*,TCACCESS),COL=*                                          
CLOCKYR  LKREQ F,242,(D,B#SAVED,QCLOCKYR),LBIN,TEXT=(*,TCLOCKYR),COL=*          
                                                                                
CLOCKMN  LKREQ F,252,(D,B#SAVED,QCLOCKMON),LBIN,TEXT=(*,TCLOCKMN),COL=*         
CLOCKX80 LKREQ F,250,(D,B#SAVED,QCLOCKMON),MB80,TEXT=(*,TCLOCKMP),COL=*         
CLOCKX40 LKREQ F,251,(D,B#SAVED,QCLOCKMON),MB40,TEXT=(*,TCLOCKMS),COL=*         
                                                                                
CRFPGRP  LKREQ F,260,(D,B#SAVED,QCRFPGRP),CHAR,TEXT=(*,TCRFPGRP),COL=*          
CC2CONV  LKREQ F,261,(D,B#SAVED,QCC2CONV),CDAT,TEXT=(*,TCC2CONV),COL=*          
CTRAFOFC LKREQ F,262,(D,B#SAVED,QCTRAFOFC),LBIN,TEXT=(*,TCTRFOFC),COL=*         
CLTSLLMT LKREQ F,263,(D,B#SAVED,QCLTSLLMT),LBIN,TEXT=(*,TCLTSLMT),COL=*         
CGRP6    LKREQ F,264,(D,B#SAVED,QCGRP6),CHAR,TEXT=(*,TCGRP6),COL=*              
CGRP7    LKREQ F,265,(D,B#SAVED,QCGRP7),CHAR,TEXT=(*,TCGRP7),COL=*              
CGRP8    LKREQ F,266,(D,B#SAVED,QCGRP8),CHAR,TEXT=(*,TCGRP8),COL=*              
CGRP9    LKREQ F,267,(D,B#SAVED,QCGRP9),CHAR,TEXT=(*,TCGRP9),COL=*              
CGRP10   LKREQ F,268,(D,B#SAVED,QCGRP10),CHAR,TEXT=(*,TCGRP10),COL=*            
CPRPRD   LKREQ F,269,(D,B#SAVED,QCPRPRD),CHAR,TEXT=(*,TCPRPRD),COL=*            
                                                                                
CPSTBC   LKREQ F,270,(D,B#SAVED,QCPST0BC),CHAR,TEXT=(*,TCPST),COL=*             
CPSTAL   LKREQ F,271,(D,B#SAVED,QCPST1AL),CHAR,TEXT=(*,TCPST),COL=*             
CPSTSA   LKREQ F,272,(D,B#SAVED,QCPST2SA),CHAR,TEXT=(*,TCPST),COL=*             
CPSTMA   LKREQ F,273,(D,B#SAVED,QCPST3MA),CHAR,TEXT=(*,TCPST),COL=*             
CPSTON   LKREQ F,274,(D,B#SAVED,QCPST4ON),CHAR,TEXT=(*,TCPST),COL=*             
CPSTPQ   LKREQ F,275,(D,B#SAVED,QCPST5PQ),CHAR,TEXT=(*,TCPST),COL=*             
CPSTNB   LKREQ F,276,(D,B#SAVED,QCPST6NB),CHAR,TEXT=(*,TCPST),COL=*             
CPSTNS   LKREQ F,277,(D,B#SAVED,QCPST7NS),CHAR,TEXT=(*,TCPST),COL=*             
CPSTPE   LKREQ F,278,(D,B#SAVED,QCPST8PE),CHAR,TEXT=(*,TCPST),COL=*             
CPSTNF   LKREQ F,279,(D,B#SAVED,QCPST9NF),CHAR,TEXT=(*,TCPST),COL=*             
                                                                                
CMPSP    LKREQ F,280,(D,B#SAVED,QCMPSTPC),CHAR,TEXT=(*,TCMPS),COL=*             
CMPSC    LKREQ F,281,(D,B#SAVED,QCMPSTCD),CHAR,TEXT=(*,TCMPS),COL=*             
                                                                                
REQTKN   LKREQ F,300,(D,B#SAVED,QCTOKEN),CHAR,TEXT=SP#TOKEN,COL=*               
RECACT   LKREQ F,350,(D,B#SAVED,QCRECACT),CHAR,TEXT=SP#ACTN,COL=*               
         LKREQ E                                                                
         EJECT                                                                  
*                                                                               
PRDHEAD  LKREQ H,I#SDPRDU,0,NEWREC=Y         04A1                               
MEDIA    LKREQ F,1,(I,B#SAVED,I$PAM),CHAR,                             *        
               MAXLEN=1,OLEN=1,TEXT=SP#MED,COL=*                                
CLIENT   LKREQ F,2,(I,B#SAVED,I$PCLT),VSTR,                            *        
               MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
PRODUCT  LKREQ F,3,(I,B#SAVED,I$PPRD),VSTR,                            *        
               OLEN=L'QPKEYPRD,TEXT=SP#PRO,COL=*                                
PACCT    LKREQ F,4,(D,B#SAVED,QPACCT),CHAR,TEXT=(*,TPACCT),COL=*                
PNAME    LKREQ F,5,(D,B#SAVED,QPNAME),CHAR,TEXT=SP#PRONA,COL=*                  
PCODE    LKREQ F,6,(D,B#SAVED,QPCODE),HEXD,TEXT=SP#PRO,COL=*                    
PADDR1   LKREQ F,7,(D,B#SAVED,QPADDR1),CHAR,TEXT=(*,TPADDR1),COL=*              
PADDR2   LKREQ F,8,(D,B#SAVED,QPADDR2),CHAR,TEXT=(*,TPADDR2),COL=*              
PADDR3   LKREQ F,9,(D,B#SAVED,QPADDR3),CHAR,TEXT=(*,TPADDR3),COL=*              
PADDR4   LKREQ F,10,(D,B#SAVED,QPADDR4),CHAR,TEXT=(*,TPADDR4),COL=*             
PDIV     LKREQ F,11,(D,B#SAVED,QPDIV),CHAR,TEXT=(*,TPDIV),COL=*                 
PBILLDT  LKREQ F,12,(D,B#SAVED,QPBILLDT),BDAT,TEXT=(*,TPBILLDT),COL=*           
*PBILLBAS LKREQ F,13,(D,B#SAVED,QPBILLBAS),LBIN,TEXT=(*,TPBILLBS),COL=*         
PBILLBAS LKREQ F,13,(I,B#SAVED,APBILBAS),LBIN,TEXT=(*,TPBILLBS),OLEN=1          
PBILLCOM LKREQ F,14,(D,B#SAVED,QPBILLCOM),CBIN,TEXT=(*,TPBLLCOM),COL=*          
PAGYFEE  LKREQ F,15,(D,B#SAVED,QPAGYFEE),SPAK,TEXT=(*,TPAGYFEE),COL=*           
PPROF    LKREQ F,16,(D,B#SAVED,QPPROF),CHAR,TEXT=(*,TPPROF),COL=*               
PGRP1    LKREQ F,17,(D,B#SAVED,QPGRP1),HEXD,TEXT=(*,TPGRP1),COL=*               
PGRP2    LKREQ F,18,(D,B#SAVED,QPGRP2),HEXD,TEXT=(*,TPGRP2),COL=*               
PGRP3    LKREQ F,19,(D,B#SAVED,QPGRP3),HEXD,TEXT=(*,TPGRP3),COL=*               
PCLASS   LKREQ F,20,(D,B#SAVED,INPPCLSS),CHAR,TEXT=(*,TPCLASS),COL=*            
PGRP4    LKREQ F,21,(D,B#SAVED,QPGRP4),HEXD,TEXT=(*,TPGRP4),COL=*               
PGRP5    LKREQ F,22,(D,B#SAVED,QPGRP5),HEXD,TEXT=(*,TPGRP5),COL=*               
PLOCK    LKREQ F,23,(D,B#SAVED,QPLOCK),CHAR,TEXT=(*,TPLOCK),COL=*               
PLKDAT   LKREQ F,24,(D,B#SAVED,QPLKDAT),CDAT,TEXT=(*,TPLKDAT),COL=*             
PGSTCODE LKREQ F,25,(D,B#SAVED,QPGSTCODE),CHAR,TEXT=(*,TPGSTCD),COL=*           
PCPPRS   LKREQ F,26,(D,B#SAVED,QPCPPRS),CHAR,TEXT=(*,TPCPPRS),COL=*             
PUSER1   LKREQ F,27,(D,B#SAVED,QPUSER1),CHAR,TEXT=(*,TPUSER1),COL=*             
PUSER2   LKREQ F,28,(D,B#SAVED,QPUSER2),CHAR,TEXT=(*,TPUSER2),COL=*             
PPST     LKREQ F,29,(D,B#SAVED,QPPST),CHAR,TEXT=(*,TPPST),COL=*                 
PTALAGY  LKREQ F,30,(D,B#SAVED,QPTALAGY),CHAR,TEXT=(*,TPTALAGY),COL=*           
PTAL     LKREQ F,31,(D,B#SAVED,QPTAL),LBIN,TEXT=(*,TPTAL),COL=*                 
PRATE    LKREQ F,32,(D,B#SAVED,QPRATE),CHAR,TEXT=(*,TPRATE),COL=*               
PO1NBL   LKREQ F,40,(D,B#SAVED,QPOPT1),MB80,TEXT=(*,TPO1NBL),COL=*              
PO1RFC   LKREQ F,41,(D,B#SAVED,QPOPT1),MB40,TEXT=(*,TPO1RFC),COL=*              
PO1THTR  LKREQ F,42,(D,B#SAVED,QPOPT1),MB20,TEXT=(*,TPO1THTR),COL=*             
PO1THNT  LKREQ F,43,(D,B#SAVED,QPOPT1),MB10,TEXT=(*,TPO1THTR),COL=*             
POFFICE  LKREQ F,50,(D,B#SAVED,QPOFFICE),CHAR,TEXT=(*,TPOFFICE),COL=*           
PTRAFOFC LKREQ F,51,(D,B#SAVED,QPTRAFOFC),CHAR,TEXT=(*,TPTRFOF),COL=*           
PACCOFC  LKREQ F,52,(D,B#SAVED,QPACCOFC),CHAR,TEXT=(*,TPACCOFC),COL=*           
PACCAGY  LKREQ F,53,(D,B#SAVED,QPACCAGY),CHAR,TEXT=(*,TPACCAGY),COL=*           
PGRP6    LKREQ F,54,(D,B#SAVED,QPGRP6),HEXD,TEXT=(*,TPGRP6),COL=*               
                                                                                
PBILLBAS LKREQ F,80,(D,B#SAVED,QBILLBAS),CHAR,TEXT=(*,TPBILLBS),COL=*           
PCOMMBIL LKREQ F,81,(D,B#SAVED,QCOMMBIL),CHAR,TEXT=(*,TPBILLBS),COL=*           
PCOMMBAS LKREQ F,82,(D,B#SAVED,QCOMMBAS),CHAR,TEXT=(*,TPBILLBS),COL=*           
                                                                                
PPSTBC   LKREQ F,270,(D,B#SAVED,QPPST0BC),CHAR,TEXT=(*,TCPST),COL=*             
PPSTAL   LKREQ F,271,(D,B#SAVED,QPPST1AL),CHAR,TEXT=(*,TCPST),COL=*             
PPSTSA   LKREQ F,272,(D,B#SAVED,QPPST2SA),CHAR,TEXT=(*,TCPST),COL=*             
PPSTMA   LKREQ F,273,(D,B#SAVED,QPPST3MA),CHAR,TEXT=(*,TCPST),COL=*             
PPSTON   LKREQ F,274,(D,B#SAVED,QPPST4ON),CHAR,TEXT=(*,TCPST),COL=*             
PPSTPQ   LKREQ F,275,(D,B#SAVED,QPPST5PQ),CHAR,TEXT=(*,TCPST),COL=*             
PPSTNB   LKREQ F,276,(D,B#SAVED,QPPST6NB),CHAR,TEXT=(*,TCPST),COL=*             
PPSTNS   LKREQ F,277,(D,B#SAVED,QPPST7NS),CHAR,TEXT=(*,TCPST),COL=*             
PPSTPE   LKREQ F,278,(D,B#SAVED,QPPST8PE),CHAR,TEXT=(*,TCPST),COL=*             
PPSTNF   LKREQ F,279,(D,B#SAVED,QPPST9NF),CHAR,TEXT=(*,TCPST),COL=*             
                                                                                
PMPSP    LKREQ F,280,(D,B#SAVED,QPMPSTPC),CHAR,TEXT=(*,TCMPS),COL=*             
PMPSC    LKREQ F,281,(D,B#SAVED,QPMPSTCD),CHAR,TEXT=(*,TCMPS),COL=*             
                                                                                
REQTKN   LKREQ F,300,(D,B#SAVED,QPTOKEN),CHAR,TEXT=SP#TOKEN,COL=*               
RECACT   LKREQ F,350,(D,B#SAVED,QPRECACT),CHAR,TEXT=SP#ACTN,COL=*               
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
*                                                                               
SAVED    DSECT                                                                  
WVALUES  DS    0X                                                               
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
*                                                                               
WVALUESL EQU   *-WVALUES                                                        
*                                                                               
AIOLIST  DS    XL20                10H'0'                                       
*                                                                               
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
*                                                                               
ALIOB    DS    A                   A(ALIOB)                                     
ALINKIO  DS    A                   A(LINKIO)                                    
ARECUP   DS    A                   A(ARECUP)                                    
ABUFFRIN DS    A                   A(BUFFERIN)                                  
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
*                                                                               
DATADISP DS    H                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL64                                                             
*                                                                               
PROF00A  DS    CL16                                                             
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
JDTTODAY DS    PL4                 TODAY'S DATE (0CYYDDDF)                      
*                                                                               
SVBAGM   DS    X                                                                
SVBCLT   DS    CL2                                                              
SVQPRD   DS    CL3                                                              
SVOFFICE DS    C                                                                
PCOUNT   DS    X                                                                
SVPRDNUM DS    XL1                                                              
SVCOFFIC DS    CL2                                                              
OFCBLK   DS    XL(OFCLENQ)                                                      
INPPCLSS DS    CL2                 INPUT PRODUCT CLASS                          
*                                                                               
SVIOKEY  DS    XL(L'IOKEY)                                                      
*                                                                               
QPSTCODE DS    0CL(L'CPST)                                                      
QCPST0BC DS    CL1                                                              
QCPST1AL DS    CL1                                                              
QCPST2SA DS    CL1                                                              
QCPST3MA DS    CL1                                                              
QCPST4ON DS    CL1                                                              
QCPST5PQ DS    CL1                                                              
QCPST6NB DS    CL1                                                              
QCPST7NS DS    CL1                                                              
QCPST8PE DS    CL1                                                              
QCPST9NF DS    CL1                                                              
*                                                                               
QCMPST_C DS    0CL(L'QCMPSTPC+L'QCMPSTCD)                                       
QCMPSTPC DS    CL2                 Main PST province                            
QCMPSTCD DS    CL1                 Main PST code                                
*                                                                               
QPRDPSTC DS    0CL(L'PPST)                                                      
QPPST0BC DS    CL1                                                              
QPPST1AL DS    CL1                                                              
QPPST2SA DS    CL1                                                              
QPPST3MA DS    CL1                                                              
QPPST4ON DS    CL1                                                              
QPPST5PQ DS    CL1                                                              
QPPST6NB DS    CL1                                                              
QPPST7NS DS    CL1                                                              
QPPST8PE DS    CL1                                                              
QPPST9NF DS    CL1                                                              
*                                                                               
QPMPST_C DS    0CL(L'QPMPSTPC+L'QPMPSTCD)                                       
QPMPSTPC DS    CL2                 Main PST province                            
QPMPSTCD DS    CL1                 Main PST code                                
*                                                                               
*PREFIX=Q                                                                       
       ++INCLUDE SPGENCLT                                                       
*PREFIX=                                                                        
         ORG   QCPROF                                                           
QCPROF1  DS    C                                                                
QCPROF2  DS    C                                                                
QCPROF3  DS    C                                                                
QCPROF4  DS    C                                                                
QCPROF5  DS    C                                                                
QCPROF6  DS    C                                                                
QCPROF7  DS    C                                                                
QCPROF8  DS    C                                                                
QCPROF9  DS    C                                                                
QCPROF10 DS    C                                                                
QCPROF11 DS    C                                                                
QCPROF12 DS    C                                                                
QCPROF13 DS    C                                                                
QCPROF14 DS    C                                                                
QCPROF15 DS    C                                                                
         ORG   QCEXTRA                                                          
QCEXTRA1 DS    C                                                                
QCEXTRA2 DS    C                                                                
QCEXTRA3 DS    C                                                                
QCEXTRA4 DS    C                                                                
QCEXTRA5 DS    C                                                                
QCEXTRA6 DS    C                                                                
QCEXTRA7 DS    C                                                                
QCEXTRA8 DS    C                                                                
QCEXTRA9 DS    C                                                                
QCEXTRA10 DS   C                                                                
QCEXTRA11 DS   C                                                                
QCEXTRA12 DS   C                                                                
QCEXTRA13 DS   C                                                                
QCEXTRA14 DS   C                                                                
QCEXTRA15 DS   C                                                                
         ORG                                                                    
*PREFIX=Q                                                                       
       ++INCLUDE SPGENPRD                                                       
*PREFIX=                                                                        
*                                                                               
QCTOKEN  DS    CL8                 TOKEN                                        
QCRECACT DS    C                   RECORD ACTION                                
QPTOKEN  DS    CL8                 TOKEN                                        
QPRECACT DS    C                   RECORD ACTION                                
*                                                                               
QBILLBAS DS    C                   BILL BASIS, X'00', C'G', OR C'N'             
QCOMMBIL DS    C                   COMMISSION ONLY BILLING   Y/N                
QCOMMBAS DS    C                   COMM BASIS, X'00', C'G', OR C'N'             
*                                                                               
         DS    0F                                                               
DISKADDR DS    0AL4                                                             
TDYDTCHR DS    CL6                 TODAYS DATE (CHAR)                           
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS 1                        
MF1ERROR EQU   X'80'                                                            
*                                                                               
MISCFLG2 DS    X                   MISCELLANEOUS FLAGS 2                        
*                                                                               
MISCFLG3 DS    X                   ERROR FLAG                                   
*                                                                               
*  FIELD INDEX ADDRESSES                                                        
*                                                                               
I$CAM    DS    A                   A(CLIENT AGENCY MEDIA)                       
I$CCLT   DS    A                   A(CLIENT CLIENT CODE)                        
*                                                                               
I$PAM    DS    A                   A(PRODUCT AGENCY MEDIA)                      
I$PCLT   DS    A                   A(PRODUCT CLIENT CODE)                       
I$PPRD   DS    A                   A(PRODUCT CLIENT CODE)                       
APBILBAS DS    A                   A(PRODUCT BILLBASIS)                         
                                                                                
SAVEL    EQU   *-SAVED                                                          
*                                                                               
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
         PRINT ON                                                               
*                                                                               
WORKD    DSECT                     ** REDEFINE OVERWORK **                      
         ORG   OVERWORK                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPMSGEQUS                                                      
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
*                                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE DMPRTQK                                                        
       ++INCLUDE FATCB                                                          
       ++INCLUDE DDOFFICED                                                      
*                                                                               
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVVALL   EQU   *-SVVALS                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPLNK25   03/24/20'                                      
         END                                                                    
