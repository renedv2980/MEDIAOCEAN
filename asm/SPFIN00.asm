*          DATA SET SPFIN00    AT LEVEL 075 AS OF 03/25/10                      
*PHASE T20900A                                                                  
         SPACE 2                                                                
**************************************************************                  
*                                                            *                  
* 25MAR10 AKAT RESET ADDFLG ON ESTIMATE CHANGE               *                  
* 24SEP09 AKAT DON'T FILTER OUT BILLS THAT HAVE GROSS=0      *                  
* 14NOV06 EJOR 6K BUY RECS (AND NO AIOAREA3!)                *                  
* 20JUL06 MHER FIX FOR ESTHDR BUCKETS AT NET                 *                  
* 14AUG02 DEIS USE PL6 VALUES FROM SPBVAL                    *                  
* 18JAN02 MHER CONVERT TO PACKED ESTHDR                      *                  
* 11JAN00 EJOR CALL SPOTBUY IN SPLIT MODE                    *                  
* 01DEC99 MZEI ADD CODE TO TEST IF BEING TRANSFERED FROM     *                  
*         SUPERDESK AND GLOBBER CALL TO GET MARKET           *                  
**************************************************************                  
*INCLUDE SPBVAL                                                                 
         TITLE 'SPFIN00 - NEW FINANCIAL INFORMATION - CONTROLLER'               
T20900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,T20900**,RA,R9,R4,RR=RE,CLEAR=YES                    
         LR    R7,RC                                                            
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING WORKD,R7,R8         R7&R8=A(GLOBAL W/S)                          
         MVC   ATWA,4(R1)          SAVE ADDRESS OF TWA                          
         MVC   ASYS,8(R1)          SAVE ADDRESS OF SYSFACS                      
*                                                                               
         L     R5,ATWA                                                          
         USING TWAD,R5             R5=A(TWA)                                    
*                                                                               
         LH    R6,=Y(SAVAREA-TWAD)                                              
         LA    R6,TWAD(R6)                                                      
         USING SAVAREA,R6                                                       
*                                                                               
         ST    R1,ACPARMA          SET BASE ADDRESSES                           
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R4,ACBASE4                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(1,0),0,0         LOAD TABLE PHASE (01)              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          A(LOAD POINT)                                
         LA    R0,(ACSELTAB-ACRECTAB)/4+1 N'TABLES                              
         SR    RE,RE                                                            
*                                                                               
FIN2     L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,FIN2                                                          
*                                                                               
         L     R1,0(RE,RF)         TABLE BASE FOR OPTION VALIDATION             
         AR    R1,RF               TABLES                                       
         ST    R1,ACTBASE                                                       
         OI    ACOPTIND,ACOPTITB                                                
*                                                                               
         LA    R1,CONADDRS         SET HOOK ADDRESSES                           
         SR    RE,RE                                                            
         LA    R0,CONADDRN                                                      
FIN4     L     RF,CONADDRS(RE)                                                  
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO                                                        
         ST    RF,ACPHSLST(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,FIN4                                                          
*                                  SET I/O VARIABLES                            
         LA    R0,WORKD                                                         
         AH    R0,=Y(IODA1-WORKD)                                               
         ST    R0,ACIOADD         ADDRESS OF FIRST IO AREA                      
         LH    R0,=Y(IODA2-IODA1)                                               
         STH   R0,ACIOLEN         LENGTH OF EACH IO AREA                        
         MVI   ACIONUM,2          NUMBER OF IO AREAS                            
         MVI   ACIOIND,ACIOIDA+ACIOIWK                                          
*                                                                               
*                                 SET TWA VARIABLES                             
         MVC   ACSYSPGM,=X'0209'                                                
         OI    ACINDS,ACINOHLP     DON'T WANT GENERALS HELP                     
         MVI   ACTWAREC,1          RELATIVE FIELD FOR RECORD                    
         MVI   ACTWAACT,2                             ACTION                    
         MVI   ACTWAKEY,3                             KEY                       
         MVI   ACTWAOPT,11                            OPTION                    
*                                                                               
         LA    R1,TWAD                                                          
         LA    R0,FINTABH                                                       
         SR    R0,R1                                                            
         STCM  R0,3,ACENDTWA      END OF TWA                                    
*                                                                               
**NOP    LH    R1,=Y(IOAREA1-WORKD)   SET IOAREAS EACH TIME                     
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,IOAREA1                                                       
         ST    R1,AIOAREA1                                                      
**NOP    LH    R1,=Y(IOAREA2-WORKD)                                             
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,IOAREA2                                                       
         ST    R1,AIOAREA2                                                      
**NOP    LH    R1,=Y(CHUNK-WORKD)                                               
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,CHUNK                                                         
         ST    R1,ACHUNK          SET A(CHUNK) EACH TIME                        
**NOP    LH    R1,=Y(SPTTB-WORKD)                                               
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,SPTTB                                                         
         ST    R1,ASPTTB          SET A(SPTTB) EACH TIME                        
**NOP    LH    R1,=Y(TSARBLK-WORKD)                                             
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,TSARBLK                                                       
         ST    R1,ATSARBLK        SET A(TSARBLK) EACH TIME                      
**NOP    LH    R1,=Y(APLOCAL-WORKD)                                             
**NOP    LA    R1,WORKD(R1)                                                     
         LAY   R1,APLOCAL                                                       
         ST    R1,APALOCAL        SET A(LOCAL STORAGE) EACH TIME                
*                                                                               
         L     R1,ACPARMA                                                       
         L     R1,0(R1)            R1=A(TIOB)                                   
         MVC   ACCURD,TIOBCURD-TIOBD(R1)  SET CURSOR DISPLACEMENT               
         MVC   ACCURS,TIOBCURS-TIOBD(R1)  SET CURSOR ABS SCREEN ADDRESS         
*                                                                               
         MVI   ACFSTIND,ACHKBEF    CONTROLLER HOOK                              
         MVI   ACKEYIND,ACHKAFT    HOOK AFTER KEY FIELD PRE-VALIDATION          
         MVI   ACRECIND,ACHKBEF    HOOK BEFORE RECORD VALIDATION                
         OI    ACRECIND,ACHKAFT    HOOK AFTER RECORD VALIDATION                 
         MVI   ACACTIND,ACHKAFT    HOOK AFTER ACTION VALIDATION                 
         MVI   ACLFMIND,ACHKAFT    HOOK AFTER MAINTENANCE ACTION                
         MVI   ACLSTIND,ACHKBEF    HOOK AT TERMINATION                          
*                                                                               
         OI    APINDS2,APIMDIS2    DISPLAY LIST/SELECT RECORD AFTER LFM         
*                                                                               
         XC    SVXFRCTL,SVXFRCTL                                                
         L     RF,ACPARMA          TEST XFER CONTROL FROM ANOTHER PGM           
         L     RF,16(RF)                                                        
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,=C'GETD',APWORK,24,GLVXCTL                           
         CLI   8(R1),0             IF TRANSFER RECORD FOUND                     
         BNE   FIN20                                                            
         MVC   SVXFRSYS,APWORK     SAVE SYSTEM AND PROGRAM                      
         MVC   SVXFRPGM,APWORK+3                                                
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                    DELETE IT                  
*                                                                               
         GOTO1 (RF),APPARM,=C'GETF',FINMEDH,,GLVSPMD THEN SET SCR FLDS          
         GOTO1 (RF),(R1),,FINCLTH,,GLVSPCLT                                     
         GOTO1 (RF),(R1),,FINPRDH,,GLVSPPRD                                     
         GOTO1 (RF),(R1),,FINESTH,,GLVSPEST                                     
         GOTO1 (RF),(R1),,FINMOSH,,GLVSPSTA                                     
*                                                                               
         CLC   SVXFRCTL,=C'SPOSDE' IF SUPERDESK GET MARKET                      
         BNE   FIN10                                                            
*                                                                               
         GOTO1 (RF),(R1),,FINMOSH,,GLVSPMKT        MARKET                       
         GOTO1 (RF),(R1),,FINSDTH,,GLVSPPER        START DATE                   
         GOTO1 (RF),(R1),,FINEDTH,,GLVBUY1         END DATE                     
         GOTO1 (RF),(R1),,FINOPTH,,GLVSPOPT        OPTIONS (WKLY)               
*                                                                               
* CALL BACK SUPERDESK                                                           
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'FIS'    FIS PROGRAM                                  
         MVC   GLVXTOSY,SVXFRSYS   TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,SVXFRPGM   SUPERDESK PROGRAM                            
         OI    GLVXFLG1,GLV1RETN+GLV1RETG                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 (RF),APPARM,=C'PUTD',(R3),24,GLVXCTL                             
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FIN20                                                            
*                                                                               
FIN10    LA    R1,FINSDTH                                                       
         MVC   8(2,R1),=C'ES'                                                   
         NI    4(R1),X'DF'                                                      
         MVI   5(R1),2                                                          
*                                                                               
FIN20    L     RF,ACPARMA                                                       
         L     RF,16(RF)                                                        
         MVC   VGETPROF,CGETPROF-COMFACSD(RF)                                   
         L     RF,CGENERAL-COMFACSD(RF)                                         
         LA    R1,WORKD                                                         
         BASR  RE,RF              CALL GENERAL CONTROLLER                       
         B     EXIT                                                             
*                                                                               
YES      CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NO       LTR   RB,RB                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*==================*                                                            
* GENERAL HOOK     *                                                            
*==================*                                                            
*                                                                               
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         LLC   RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     FIRST                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     PROCLFM                                                          
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         B     LAST                                                             
*                                                                               
HOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*===================================*                                           
* FIRST - RESTORES SBLOCK, ESTTB,   *                                           
*         CLTREC, ESTREC, BLK AND   *                                           
*         TSAR IF NEEDED            *                                           
*===================================*                                           
*                                                                               
FIRST    DS    0H                 GET ESTTB,CLTREC,ESTREC,BLK& SBLOCK           
         XC    STAWORK,STAWORK                                                  
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,CUAALF                                                   
         GOTO1 VSTAPACK,(R1)                                                    
         MVC   STAVRSN,STAPVRSN    SAVE STAPACK VERSION                         
         DROP  R1                                                               
*                                                                               
         TM    TIND2,TRES2        NEED TO RESTORE?                              
         BNO   FIRST10                                                          
         LA    R0,STARTSAV        DETAILS FROM TWA0+6K                          
         ICM   R1,15,=AL4(ENDSAV-STARTSAV)                                      
         L     RE,ATWA                                                          
         ICM   R2,15,=AL4(TWAMXLEN)                                             
         AR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                 RE-SET SOME ADDRESSES                         
         LH    R1,=Y(CLTREC-WORKD)                                              
         LA    R1,WORKD(R1)                                                     
         ST    R1,SBACLTRC                                                      
         LH    R1,=Y(ESTREC-WORKD)                                              
         LA    R1,WORKD(R1)                                                     
         ST    R1,SBAESTRC                                                      
         OC    ESTTB,ESTTB                                                      
         BZ    FIRST5                                                           
         LA    R1,ESTTB                                                         
         ST    R1,SBASVETB                                                      
FIRST5   MVC   SBCOMFAC,ACOM                                                    
         MVC   SBAIO1(8),AIOAREA1                                               
         MVC   SBASPTTB,ASPTTB                                                  
         MVC   SBACHUNK,ACHUNK                                                  
         NI    TIND2,255-TRES2    TURN OF RESTORE FLAG                          
*                                                                               
FIRST10  TM    TIND,TINIT         HAS TSAR BEEN INITIALIZED?                    
         BNO   FIRSTX                                                           
         TM    TIND,TRES          DOES TSAR NEED TO BE RESTORED?                
         BO    FIRSTX             NO - IT WAS RESTORED                          
         L     R1,ATSARBLK        A(TSAR BLOCK)                                 
         USING TSARD,R1                                                         
         MVC   TSABUF,ATIA        A(6K CORE BUFFER)                             
         MVC   TSACOM,ACOM        A(COMFACS)                                    
         MVI   TSNBUF,1           # OF CORE BUFERS                              
         LA    RE,L'TKEY                                                        
         STC   RE,TSKEYL          MAXIMUM KEY LENGTH                            
         LA    RE,TRECL                                                         
         STCM  RE,3,TSRECL        MAXIMUM RECORD LENGTH                         
         MVC   TSPAGL,TSAVE       LOW PAGE NUMBER                               
         MVC   TSPAGN,TSAVE+1     NUMBER OF PAGES ALLOCATED                     
         MVC   TSINDS,TSAVE+2     INDICATORS                                    
         OI    TSINDS,TSIREUSE    RE-USE PREVIOUS ALLOCATION                    
*                                                                               
         MVI   TSACTN,TSARES      RESTORE                                       
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'               UNSUCCESSFUL RESTORE                          
         MVC   TSAVE(1),TSPAGL    SAVE LOW TSAR PAGE NUMBER                     
         MVC   TSAVE+1(1),TSPAGN  SAVE NUMBER OF PAGES ALLOCATED                
         MVC   TSAVE+2(1),TSINDS  SAVE TEMPSTR/TEMPEST INDICATOR                
         NI    TSAVE+2,TSIALLOC+TSIXTTWA                                        
         OI    TIND,TRES          TURN ON RESTORE BIT                           
         DROP  R1                                                               
*                                                                               
FIRSTX   B     HOOKX                                                            
         EJECT                                                                  
*=================================*                                             
* PROCESS FILE MAINTENANCE HOOK   *                                             
*=================================*                                             
*                                                                               
PROCLFM  DS    0H                                                               
         LA    R1,FINMEDH         ALTER CURSOR POSITION                         
         ST    R1,FVADDR          TO MEDIA WHEN EXITING                         
         MVC   FVMSGNO,=AL2(FVLFM) ACTION COMPLETE                              
         TM    INOIND,INOIWK                                                    
         BNO   EXIT                                                             
         CLI   APMODE,APMLRP      END OF DISPLAY                                
         BE    EXIT                                                             
         MVC   FVMSGNO,=AL2(FVWKLY)  WEEKLY DISP-HIT ENTER FOR NEXT             
         MVI   FVOMTYP,C'I'                                                     
         B     EXIT                                                             
         SPACE 2                                                                
*=================================*                                             
* LAST - TERMINATION HOOK         *                                             
*        SAVES TSAR, SBLOCK, BLK  *                                             
*        ESTTB, CLTREC AND ESTREC *                                             
*=================================*                                             
*                                                                               
LAST     DS    0H                                                               
         BAS   RE,FINISH          FINISH UP SAVING INFO                         
         MVI   TSTART,C'N'        MAKE SURE ITS OFF                             
LASTX    B     HOOKX                                                            
         EJECT                                                                  
*=========================================================*                     
* COMMON ROUTINES AVAILABLE TO CONTROLLER AND OVERLAYS    *                     
* NTRY - R1=A(PARAMETER LIST)                             *                     
*        RF=ROUTINE NUMBER (HIGH ORDER BYTE)              *                     
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR *                     
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK            *                     
*=========================================================*                     
*                                                                               
ROUTS    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R4,ACBASE4                                                       
         L     R5,ATWA                                                          
         L     RE,4(RD)            MOVE REGISTER SAVE AREA                      
         LR    RC,RD               TO MAKE ROOM FOR WORK AREA                   
         AH    RD,=Y(RWRKX-RWRKD)                                               
         LA    RD,7(RD)                                                         
         SRL   RD,3                                                             
         SLL   RD,3                                                             
         ST    RE,4(RD)            BACKWARD POINTER                             
         ST    RD,8(RE)            FORWARD POINTER                              
*                                                                               
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   RIOSAVE,IOAREA      SAVE I/O AREA                                
         MVI   RSPACES,C' '                                                     
         MVC   RSPACES+1(L'RSPACES-1),RSPACES                                   
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTSBR(RF)                                                      
*                                                                               
ROUTSX   CLC   RIOSAVE,IOAREA      TEST ANY I/O EXECUTED                        
         BE    *+14                                                             
         OI    APINDS,APILRERD     YES - SET APPLICATION FLAG                   
         MVC   IOAREA(L'RIOSAVE),RIOSAVE                                        
*                                                                               
         L     RF,ACPARMA         GET CURRENT IO COUNT                          
         L     RF,16(RF)                                                        
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,0      GET COUNT FROM GETFACT                        
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MHI   R3,9                                                             
         D     R2,=F'10'           90 PERCENT OF MAX IOS IN R3                  
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    ROUTSXX             NO - STILL WITHIN 90 PERCENT                 
         BAS   RE,FINISH                                                        
         XC    FINMSG,FINMSG                                                    
         MVC   FINMSG(34),=CL34'TOO MUCH DATA FOR ON-LINE ANALYSIS'             
         OI    FINMSGH+6,X'80'                                                  
         L     RD,ACWORKA                                                       
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                 EXIT TO USER                                  
         DROP  R1                                                               
ROUTSXX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
FINISH   NTR1                                                                   
         L     RE,ATWA            DETAILS FROM TWA0                             
         ICM   R2,15,=AL4(TWAMXLEN)                                             
         AR    RE,R2                                                            
         LA    R0,STARTSAV                                                      
         ICM   R1,15,=AL4(ENDSAV-STARTSAV)                                      
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         OI    TIND2,TRES2        TURN ON RESTORE BIT                           
*                                                                               
         TM    TIND,TINIT         TEST TSAR INITIALIZED                         
         BZ    FINISHX                                                          
         TM    TIND,TRES         YES - TEST TSAR WAS RESTORED                   
         BZ    FINISHX                                                          
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV      YES - SAVE TSAR INFORMATION                   
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TIND,255-TRES       TURN OFF RESTORE FLAG                        
         DROP  R1                                                               
FINISHX  XIT1                                                                   
         EJECT                                                                  
*===============================================*                               
* TABLE OF BRANCH ADDRESSES TO ROUTS ROUTINES   *                               
*===============================================*                               
*                                                                               
ROUTSBR  B     VALKEYD                                                          
         B     VALMED                                                           
         B     GETMED                                                           
         B     VALCLT                                                           
         B     GETCLT                                                           
         B     VALPRD                                                           
         B     GETPRD                                                           
         B     VALEST                                                           
         B     GETEST                                                           
         B     VALSDT                                                           
         B     VALEDT                                                           
         B     VALMOS                                                           
         B     GETMOS                                                           
         B     INTSPBK                                                          
         B     CLRBLK                                                           
         B     SPHOOK                                                           
         B     TSARCM                                                           
         B     TSARTAD                                                          
         B     TSARTCM                                                          
         DC    AL4(0)                                                           
         EJECT                                                                  
*===========================*                                                   
* ROUTINE TO VALIDATE KEY   *                                                   
*===========================*                                                   
*                                                                               
VALKEYD  LA    R3,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         USING APRECD,R3                                                        
*                                                                               
         GOTO1 AVALMED,FINMEDH    VALIDATE MEDIA                                
         BNE   VALKDX                                                           
         MVC   APMED,BAGYMD       MOVE MEDIA INTO KEY                           
         MVC   FINMEDN,MEDNM           NAME ONTO SCREEN                         
         OI    FINMEDNH+6,X'80'                                                 
*                                                                               
         GOTO1 AVALCLT,FINCLTH    VALIDATE CLIENT                               
         BNE   VALKDX                                                           
         MVC   APCLT,BCLT         MOVE CLIENT INTO KEY                          
         MVC   FINCLTN,CLTNM            NAME  ONTO SCREEN                       
         OI    FINCLTNH+6,X'80'                                                 
*                                                                               
         GOTO1 AVALPRD,FINPRDH    VALIDATE PRODUCT                              
         BNE   VALKDX                                                           
         MVC   APPRD,QPRD         MOVE PRODUCT INTO KEY                         
         MVC   APPRDB1,BPRD       MOVE PRODUCT INTO KEY                         
         MVC   FINPRDN,PRDNM             NAME  ONTO SCREEN                      
         OI    FINPRDNH+6,X'80'                AND XMIT                         
         MVC   APPRDB2,BPRD2      SAVE PIGGY BACK PRODUCT IF ANY                
         MVC   APPRD2,QPRD2                                                     
*                                                                               
         GOTO1 AVALEST,FINESTH    VALIDATE ESTIMATE                             
         BNE   VALKDX                                                           
         MVC   APEST,QEST         MOVE ESTIMATE INTO KEY                        
         MVC   APESTB1,BEST       MOVE ESTIMATE INTO KEY                        
         MVC   APESTB2,BEST2      SAVE END RANGE OF EST IF ANY                  
         MVC   FINESTN,RSPACES    CLEAR NAME                                    
         CLI   BEST,0             IF LISTING                                    
         BE    VALKD20                                                          
         CLI   BEST2,0            IF MORE THAN ONE ESTIMATE                     
         BNE   VALKD20            SKIP DISPLAYING NAME                          
         MVC   FINESTN,ESTNM                   ONTO SCREEN                      
VALKD20  OI    FINESTNH+6,X'80'                AND XMIT                         
*                                                                               
*                                 DATES                                         
         MVI   APESTDT,C'N'       NOT ESTIMATE DATES                            
         GOTO1 AVALSDT,FINSDTH    VALIDATE START DATE                           
         BNE   VALKDX                                                           
         GOTO1 VDATCON,APPARM,(0,QSTDT),(6,FINSDTN)                             
         OI    FINSDTNH+6,X'80'   TRANSMIT START DATE                           
         CLC   =C'ES',FINSDT      ESTIMATE DATES?                               
         BNE   VALKD25             NO - GO VALIDATE END DATE                    
         MVI   APESTDT,C'Y'        YES - USING ESTIMATE DATES                   
         MVC   FINEDT,RSPACES            CLEAR OUT END DATE AND                 
         OI    FINEDTH+6,X'80'           THEN GO AND XMIT DATE NAME             
         B     VALKD30                                                          
*                                                                               
VALKD25  GOTO1 AVALEDT,FINEDTH                                                  
         BNE   VALKDX                                                           
         BAS   RE,YEAR            CHECK DATES WITHIN 12 MONTHS                  
         BNE   VALKDX                                                           
VALKD30  GOTO1 VDATCON,APPARM,(0,QNDDT),(6,FINEDTN)                             
         OI    FINEDTNH+6,X'80'   TRANSMIT END DATE                             
         MVC   APSTDT,BSTDT       MOVE PACKED DATES INTO KEY                    
         MVC   APENDT,BNDDT                                                     
         BAS   RE,BLDLST          BUILD YEAR MONTH LIST                         
*                                                                               
         GOTO1 AVALMOS,FINMOSH    VALIDATE MARKET/STATION                       
         BNE   VALKDX                                                           
         MVC   FINMOSN,RSPACES                                                  
         CLI   MOSFLG,C'A'        MKT/STA = ALL                                 
         BNE   VALKD40                                                          
         CLI   BPRD2,0                                                          
         BNE   ERRPIG             ERROR- PIGGY-PACKPAIR NOT ALLOWED             
         OC    INOREP,INOREP                                                    
         BNZ   ERRREP             ERROR - SPECIAL REP NOT ALLOWED               
*                                                                               
VALKD40  CLI   MOSFLG,C'S'                                                      
         BNE   VALKD50                                                          
         CLI   MOSFLG+1,C'L'                                                    
         BE    VALKD50                                                          
         MVC   FINMOSN(L'QMKT),QMKT                                             
         B     *+10                                                             
VALKD50  MVC   FINMOSN(L'MKTNM),MKTNM      MARKET ONTO SCREEN                   
*                                                                               
         OI    FINMOSNH+6,X'80'                                                 
         MVC   APMOSFLG,MOSFLG                                                  
         MVC   APMKT,BMKT                                                       
         MVC   APSTA,QSTA                                                       
         MVC   APSTANET,QSTACNET                                                
         CLI   QMED,C'C'          CAN'T HAVE MKT/STA=ALL FOR MEDIA C            
         BNE   *+14                                                             
         CLC   MOSFLG,=C'A '                                                    
         BE    ERRMEDC                                                          
*                                                                               
         TM    INOIND2,INOIBIL    IF BILLING DATA REQUESTED                     
         BNO   VALKD80                                                          
         CLI   BPRD2,0                                                          
         BNE   ERRNOBIL           INVALID W/PIGGY-BACK PAIR                     
         CLI   QMED,C'C'                                                        
         BE    ERRMEDC2           ALSO INVALID W/MEDIA = C                      
         CLI   INOSPL,C'N'                                                      
         BE    ERRNOBL2           ALSO INVALID WITH SPILL=NO                    
*                                                                               
VALKD80  OC    INOAFF,INOAFF       IF AFF                                       
         BZ    VALKDX                                                           
         CLC   =C'S ',APMOSFLG     GOOD WITH STATION DISPLAY ONLY               
         BNE   ERRNOAFF                                                         
*                                                                               
VALKDX   B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* VALMED - VALIDATES MEDIA CODE                            *                    
*            NTRY - R1=A(FIELD HEADER OF MEDIA FIELD)      *                    
*            EXIT - CC=EQUAL MEDIA VALUES EXTRACTED        *                    
*                   CC=NOT EQUAL ON ERROR WITH FVMSGNO SET *                    
*==========================================================*                    
*                                                                               
VALMED   MVI   FVMINL,1            SET REQUIRED FIELD -MINIMUM LENGTH           
         MVI   FVMAXL,L'QMED       MAXIMUM LENGTH                               
         GOTO1 AFVAL                                                            
         BNE   VALMEDX                                                          
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED                         
         BNO   VALMED1                                                          
         CLC   QMED,FVIFLD         TEST CHANGE OF MEDIA                         
         BE    VALMEDX                                                          
VALMED1  LA    R2,IOKEY            YES - BUILD KEY OF AGENCY RECORD             
         USING AGYHDRD,R2                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,CUAALF                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSPTFIL+IORD   GET AGENCY RECORD                            
         BNE   ERRMED                                                           
*                                                                               
         XC    BAGYMD(BVALSX-BAGYMD),BAGYMD                                     
         XC    QMED(QVALSX-QMED),QMED                                           
         MVC   AGYPRF7,AGYPROF+7   CANADIAN FLAG                                
*                                                                               
         LH    R1,=Y(SBAGYREC-WORKD)  PASS RECORD TO SPOTIO                     
         LA    R1,WORKD(R1)                                                     
         MVC   0(L'SBAGYREC,R1),0(R2)                                           
*                                                                               
         LA    R2,AGYEL-AGYHDR(R2)                                              
         SR    R0,R0                                                            
VALMED2  CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    ERRMED                                                           
         CLI   0(R2),2                                                          
         BNE   VALMED4                                                          
         CLC   2(1,R2),FVIFLD      MATCH MEDIA CODE TO ELEMENT                  
         BNE   VALMED4                                                          
         MVC   BAGYMD,3(R2)        EXTRACT MEDIA VALUES                         
         MVC   QMED,FVIFLD         SET MEDIA                                    
         MVC   MEDNM,4(R2)         AND MEDIA NAME                               
         OI    FVIIND,FVIVAL                                                    
         B     VALMED10                                                         
*                                                                               
VALMED4  IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     VALMED2                                                          
         SPACE 1                                                                
*=====================================================*                         
* INITIALIZE SECRET                                   *                         
*=====================================================*                         
         SPACE 1                                                                
VALMED10 OC    TWASAGN,TWASAGN          TEST ON NEW SECURITY                    
         BNZ   *+14                                                             
         OC    TWAACCS,TWAACCS          OR HAVE LIMIT ACCESS                    
         BZ    VALMEDX                                                          
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           GET A(COMFACS)                               
         L     RF,CSECRET-COMFACSD(RF)                                          
         LHI   RE,SECBLK-WORKD                                                  
         LA    RE,WORKD(RE)        GET A(TWA)                                   
         ST    RE,RPARM                                                         
         MVI   RPARM,SECPINIT                                                   
         GOTO1 (RF),RPARM,,0                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALMEDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*==========================================================*                    
* GETMED - GETS MEDIA RECORD                               *                    
*           NTRY - R1=A(BINARY AGENCY/MEDIA VALUE)         *                    
*           EXIT - CC=EQUAL WITH MEDIA VALUES EXTRACTED    *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
GETMED   L     RE,0(R1)                                                         
         MVC   RDUB(L'BAGYMD),0(RE)                                             
         CLC   BAGYMD,RDUB         TEST CHANGE OF MEDIA                         
         BE    GETMEDX                                                          
         LA    R2,IOKEY            BUILD KEY OF AGENCY RECORD                   
         USING AGYHDRD,R2                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,CUAALF                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,FILRD           GET AGENCY RECORD                            
         BNE   ERRMED                                                           
*                                                                               
         LH    R1,=Y(SBAGYREC-WORKD) PASS AGENCY RECORD TO SPOTIO               
         LA    R1,WORKD(R1)                                                     
         MVC   0(L'SBAGYREC,R1),0(R2)                                           
*                                                                               
         LA    R2,AGYEL-AGYHDR(R2)                                              
         SR    R0,R0                                                            
*                                                                               
GETMED2  CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    ERRMED                                                           
         CLI   0(R2),2                                                          
         BNE   GETMED4                                                          
         CLC   3(1,R2),RDUB        MATCH AGENCY/MEDIA CODE TO ELEMENT           
         BNE   GETMED4                                                          
         XC    BAGYMD(BVALSX-BAGYMD),BAGYMD                                     
         MVC   BAGYMD,3(R2)        EXTRACT MEDIA VALUES                         
         XC    QMED(QVALSX-QMED),QMED                                           
         MVC   QMED,2(R2)          SET MEDIA                                    
         MVC   MEDNM,4(R2)                                                      
         B     GETMEDX                                                          
*                                                                               
GETMED4  IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     GETMED2                                                          
*                                                                               
GETMEDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*==========================================================*                    
* VALCLT  VALIDATES A CLIENT CODE                          *                    
*          NTRY - R1=A(FIELD HEADER OF CLIENT FIELD)       *                    
*          EXIT - CC=EQUAL WITH CLIENT VALUES EXTRACTED    *                    
*                 CC=NOT EQUAL ON ERROR WITH FVMSGNO SET   *                    
*==========================================================*                    
*                                                                               
VALCLT   MVI   FVMINL,2            REQUIRED FIELD                               
         MVI   FVMAXL,L'QCLT                                                    
         GOTO1 AFVAL                                                            
         BNE   VALCLTX                                                          
         CLC   QCLT,FVIFLD         TEST CHANGE OF CLIENT                        
         BE    VALCLTX                                                          
         CLC   =C'ALL',FVIFLD                                                   
         BE    ERRCLT             INVALID                                       
*                                                                               
         GOTO1 VCLPACK,RPARM,FVIFLD,RWORK                                       
         CLI   0(R1),0                                                          
         BNE   ERRCLT                                                           
*                                                                               
         LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,RWORK                                                    
         LH    R2,=Y(CLTREC-WORKD)  IO AREA WHICH WE PASS                       
         LA    R2,WORKD(R2)                                                     
         ST    R2,IOADDR          TO SPOTIO                                     
         GOTO1 AIO,FILRD                                                        
         BNE   ERRCLT                                                           
         MVC   CLTNM,CNAME        GET CLIENT NAME                               
*                                                                               
VALCLT4  XC    BCLT(BVALSX-BCLT),BCLT   SET CLIENT VALUES                       
         MVC   BCLT,RWORK                                                       
         XC    QCLT(QVALSX-QCLT),QCLT                                           
         MVC   QCLT,FVIFLD                                                      
         MVC   QOFFICE,COFFICE                                                  
         MVC   QCLTACCS,CACCESS                                                 
         MVI   QMKTACCS,X'FF'      SUPPRESS MKT LIMIT FOR NOW                   
*                                                                               
         OC    CUACCS(2),CUACCS    TEST ANY SEC LIMIT IN TWA                    
         BZ    VALCLT5                                                          
         CLI   CUACCS,C'+'         TEST MKT LOCKOUT                             
         BE    VALCLT5                                                          
         BRAS  RE,CALLOFCR                                                      
         BNZ   ERRSECLK                                                         
*                                 SET THE 00 PROFILE                            
VALCLT5  XC    RWORK,RWORK                                                      
         MVC   RWORK(4),=C'S000'  SPOT CONTROL PROFILE                          
         MVC   RWORK+4(2),CUAALF                                                
         MVC   RWORK+6(1),QMED                                                  
         MVC   RWORK+7(3),QCLT                                                  
         MVI   RWORK+10,C'*'                                                    
         MVC   RWORK+11(1),COFFICE                                              
         GOTO1 VGETPROF,RPARM,RWORK,SVPROF,VDMGR                                
*                                                                               
         TM    INOIND2,INOIB3     B3 CALENDER OPTION REQUESTED?                 
         BNO   VALCLT6                                                          
         XC    RWORK,RWORK                                                      
         MVC   RWORK(4),=C'S0B3'  SPOT CONTROL PROFILE                          
         MVC   RWORK+4(2),CUAALF                                                
         MVC   RWORK+6(1),QMED                                                  
         MVC   RWORK+7(3),QCLT                                                  
         MVI   RWORK+10,C'*'                                                    
         MVC   RWORK+11(1),COFFICE                                              
         GOTO1 VGETPROF,RPARM,RWORK,SVPROFB3,VDMGR                              
*                                 SAVE CLIST AT BOTTOM OF TWA                   
VALCLT6  LA    RE,CLIST                                                         
         DROP  R2                                                               
         LA    R0,PRDLIST                                                       
         LA    R1,880                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
VALCLTX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* GETCLT - GETS CLIENT RECORD                              *                    
*          NTRY - R1=A(BINARY CLIENT VALUE)                *                    
*          EXIT - CC=EQUAL WITH CLIENT VALUES EXTRACTED    *                    
*                 CC=NOT EQUAL ON ERROR WITH FVMSGNO SET   *                    
*==========================================================*                    
*                                                                               
GETCLT   L     RE,0(R1)                                                         
         MVC   RDUB(L'BCLT),0(RE)                                               
         CLC   BCLT,RDUB           TEST CHANGE OF CLIENT                        
         BE    GETCLTX                                                          
         LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,RDUB                                                     
         LH    R2,=Y(CLTREC-WORKD)  IO AREA WHICH WE PASS                       
         LA    R2,WORKD(R2)                                                     
         ST    R2,IOADDR            TO SPOTIO                                   
         GOTO1 AIO,FILRD                                                        
         BNE   ERRCLT                                                           
         L     R2,IOADDR                                                        
         MVC   CLTNM,CNAME                                                      
         MVC   QOFFICE,COFFICE                                                  
         MVC   QCLTACCS,CACCESS                                                 
*                                                                               
         OC    CUACCS(2),CUACCS    TEST ANY SEC LIMIT IN TWA                    
         BZ    GETCLT5                                                          
         CLI   CUACCS,C'+'         TEST MKT LOCKOUT                             
         BE    GETCLT5                                                          
         BRAS  RE,CALLOFCR                                                      
         BNZ   ERRSECLK                                                         
*                                                                               
GETCLT5  XC    BCLT(BVALSX-BCLT),BCLT                                           
         MVC   BCLT,RDUB                                                        
         XC    QCLT(QVALSX-QCLT),QCLT                                           
         GOTO1 VCLUNPK,RPARM,(CPROF+6,RDUB),QCLT                                
*                                 SET THE 00 PROFILE                            
         XC    RWORK,RWORK                                                      
         MVC   RWORK(4),=C'S000'  SPOT CONTROL PROFILE                          
         MVC   RWORK+4(2),CUAALF                                                
         MVC   RWORK+6(1),QMED                                                  
         MVC   RWORK+7(3),QCLT                                                  
         MVI   RWORK+10,C'*'                                                    
         MVC   RWORK+11(1),COFFICE                                              
         GOTO1 VGETPROF,RPARM,RWORK,SVPROF,VDMGR                                
*                                                                               
         TM    INOIND2,INOIB3     B3 CALENDER OPTION REQUESTED?                 
         BNO   GETCLT6                                                          
         XC    RWORK,RWORK                                                      
         MVC   RWORK(4),=C'S0B3'  SPOT CONTROL PROFILE                          
         MVC   RWORK+4(2),CUAALF                                                
         MVC   RWORK+6(1),QMED                                                  
         MVC   RWORK+7(3),QCLT                                                  
         MVI   RWORK+10,C'*'                                                    
         MVC   RWORK+11(1),COFFICE                                              
         GOTO1 VGETPROF,RPARM,RWORK,SVPROFB3,VDMGR                              
*                                 SAVE CLIST AT BOTTOM OF TWA                   
GETCLT6  LA    RE,CLIST                                                         
         DROP  R2                                                               
         LA    R0,PRDLIST                                                       
         LA    R1,880                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
GETCLTX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* VALPRD     NTRY - R1=A(FIELD HEADER OF PRODUCT FIELD)    *                    
*            EXIT - CC=EQUAL AND EXTRACTS PRODUCT VALUES   *                    
*                   CC=NOT EQUAL ON ERROR WITH FVMSGNO SET *                    
*==========================================================*                    
VALPRD   MVI   FVMINL,1            REQUIRED FIELD                               
         MVI   FVMAXL,7                                                         
         GOTO1 AFVAL                                                            
         BNE   VALPRDX                                                          
         XC    RWORK4,RWORK4                                                    
         GOTO1 VSCANNER,RPARM,FVIHDR,(2,RWORK4),C',=-='                         
         CLI   RPARM+4,0          # OF INPUTS                                   
         BE    ERRPRD             OOPS - REQUIRED FIELD                         
         LA    R2,RWORK4                                                        
         CLC   QPRD,12(R2)        TEST CHANGE OF PRODUCT                        
         BNE   VALPRD2                                                          
         LA    R1,32(R2)                                                        
         CLC   QPRD2,12(R1)       OR PIGGY-BACK PRODUCT                         
         BE    VALPRDX            NO CHANGE - DON'T RE-VALIDATE IT              
*                                                                               
VALPRD2  MVI   RFLAG,1            ONE PRODUCT                                   
         XC    BPRD(BVALSX-BPRD),BPRD                                           
         XC    QPRD(QVALSX-QPRD),QPRD                                           
         CLI   RPARM+4,2                                                        
         BL    VALPRD5                                                          
         MVI   RFLAG,2            NOT A SINGLE PRODUCT                          
         B     VALPRD10                                                         
*                                                                               
VALPRD5  MVC   PRDNM,RSPACES                                                    
         CLI   FVIHDR+5,1         IF ONE CHAR ONLY                              
         BNE   VALPRD8                                                          
         CLI   FVIFLD,C'L'        MUST BE L FOR LIST                            
         BE    VALPRDX                                                          
         B     ERRPRD             OTHERWISE INVALID                             
*                                                                               
VALPRD8  MVC   QPRD,FVIFLD                                                      
         CLC   =C'UNA',FVIFLD                                                   
         BE    VALPRDX                 QPRD=UNA                                 
         CLC   =C'ALL',FVIFLD                                                   
         BE    VALPRDX                 QPRD=ALL                                 
*                                                                               
VALPRD10 MVI   RBYTE,1            FIRST PRODUCT                                 
VALPRD12 CLI   1(R2),0            SHOULDN'T BE A SECOND HALF?                   
         BNE   ERRPRD                                                           
         MVC   TEMPPRD,12(R2)       FOR CHKPRD                                  
         BAS   RE,CHKPRD                                                        
         BNE   ERRPRD                                                           
         CLI   RBYTE,1                                                          
         BNE   VALPRDX                                                          
         CLI   RFLAG,1                                                          
         BE    VALPRDX                                                          
         MVI   RBYTE,2                                                          
         LA    R2,32(R2)                                                        
         B     VALPRD12                                                         
VALPRDX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* GETPRD - GETS A PRODUCT RECORD                           *                    
*            NTRY - 0(R1) =A(BINARY PROD NUMBER)           *                    
*                   4(R1) =A(2ND BINARY PROD NUMBER)       *                    
*            EXIT - CC=EQUAL AND EXTRACTS PRODUCT VALUES   *                    
*                   CC=NOT EQUAL ON ERROR WITH FVMSGNO SET *                    
*==========================================================*                    
*                                                                               
GETPRD   L     RE,0(R1)                                                         
         MVC   RDUB(L'BPRD),0(RE)                                               
         L     RE,4(R1)                                                         
         MVC   RDUB2(L'BPRD2),0(RE)                                             
         CLC   BPRD,RDUB           TEST CHANGE OF PRODUCT                       
         BNE   GETPRD1                                                          
         CLC   BPRD2,RDUB2                                                      
         BE    GETPRDX                                                          
*                                                                               
GETPRD1  MVI   RBYTE,1             FIRST PRODUCT                                
GETPRD2  LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   ERRPRD                                                           
GETPRD3  LA    R2,RIO                                                           
         LA    R1,CLIST                                                         
*                                                                               
GETPRD4  OC    0(4,R1),0(R1)                                                    
         BZ    ERRPRD                                                           
         CLC   RDUB(1),3(R1)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETPRD4                                                          
         MVC   RDUB+1(3),0(R1)                                                  
         MVC   TEMPPRD,RDUB+1                                                   
*                                                                               
         BAS   RE,CHKPRD                                                        
         BNE   ERRPRD                                                           
         CLI   RBYTE,1                                                          
         BNE   GETPRDX                                                          
         MVI   RBYTE,2            SECOND PRODUCT                                
         CLI   RDUB2,0           SET UP FOR GETTING PRODUCT TWO                 
         BE    GETPRDX            IF THERE IS ONE                               
         MVC   RDUB(1),RDUB2                                                    
         B     GETPRD2                                                          
*                                                                               
GETPRDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*==========================================================*                    
* VALEST - VALIDATES AN ESTIMATE CODE                      *                    
*           NTRY - R1=A(FIELD HEADER OF ESTIMATE FIELD)    *                    
*           EXIT - CC=EQUAL AND SETS ESTIMATE VALUES       *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
VALEST   MVI   FVMINL,1           FIELD IS REQUIRED                             
         MVI   FVMAXL,8           MAXIMUM LENGTH                                
         GOTO1 AFVAL                                                            
         BNE   VALESTX                                                          
         MVI   ESTOWSDY,0         OWK DEFAULTS TO MONDAY                        
         XC    RWORK4,RWORK4                                                    
         GOTO1 VSCANNER,RPARM,FVIHDR,(2,RWORK4),C',=,='                         
*                                                                               
         LA    R3,RWORK4                                                        
         CLC   QEST,12(R3)        TEST FOR CHANGE OF ESTIMATE                   
         BNE   VALEST8                                                          
         LA    R1,32(R3)                                                        
         TM    2(R2),X'80'        NUMERIC?                                      
         BO    VALEST6                                                          
         CLC   QSEPFLT,22(R1)     NO - COMPARE FILTERS                          
         BNE   VALEST8                                                          
         B     VALESTX                                                          
VALEST6  CLC   QEST2,12(R1)        YES - CHECK SECOND ESTIMATE                  
         BE    VALESTX                                                          
VALEST8  XC    BEST(BVALSX-BEST),BEST                                           
         XC    QEST(QVALSX-QEST),QEST                                           
         MVI   QSEPEST,C'Y'                                                     
         MVC   QSEPFLT,RSPACES                                                  
         CLI   0(R3),4            CAN'T BE GREATER THAN 3 PLACES                
         BH    ERREST                                                           
         CLI   1(R3),0            SHOULDN'T BE A SECOND HALF                    
         BNE   ERREST                                                           
         TM    2(R3),X'80'        NUMERIC?                                      
         BO    VALEST30             YES                                         
         TM    TWAAUTH,X'80'      IF ONLY SINGLE EST REQUIRED                   
         BO    ERREST             ERROR                                         
         CLC   =C'ALL',12(R3)     ALL ESTIMATES?                                
         BNE   VALEST10                                                         
         CLI   RPARM+4,2          MUST NOT BE A SECOND HALF                     
         BNL   ERREST                                                           
         MVC   QEST,=C'ALL'                                                     
         MVC   ESTNM,RSPACES      NO ESTIMATE NAME                              
         OC    QPRD,QPRD          LISTING PRODUCTS?                             
         BNZ   VALESTX                                                          
         MVI   TEMPEST,0                                                        
         BAS   RE,CHKPOL          ONLY NEED TO KNOW IF LISTING PRODUCTS         
         B     VALESTX                                                          
VALEST10 CLC   =C'NO',12(R3)                                                    
         BNE   VALEST12                                                         
         CLI   RPARM+4,2          MUST BE A SECOND HALF                         
         BL    ERREST                                                           
         MVI   QSEPEST,C'N'                                                     
         MVC   QEST(2),=C'NO'     NO ESTIMATES?                                 
         MVC   ESTNM,RSPACES      NO ESTIMATE NAME                              
         OC    QPRD,QPRD          LISTING PRODUCTS?                             
         BNZ   VALEST14                                                         
         MVI   TEMPEST,0                                                        
         BAS   RE,CHKPOL          ONLY NEED TO KNOW IF LISTING PRODUCTS         
         B     VALEST14                                                         
*                                                                               
VALEST12 CLI   0(R3),1                                                          
         BNE   ERREST             NOT VALID LIST                                
         CLC   =C'L',12(R3)                                                     
         BNE   ERREST                                                           
         MVC   QEST,=C'LST'                                                     
         CLI   RPARM+4,2          LIST - 2ND BLOCK OPTIONAL                     
         BL    VALESTX                                                          
*                                                                               
VALEST14 LA    R3,32(R3)          POINT TO NEXT BLOCK OF DATA                   
         LA    RE,3                                                             
         LA    RF,QSEPFLT                                                       
         LLC   R0,0(R3)           # OF FILTERS                                  
         LA    R1,12(R3)          PT TO FILTERS REQUESTED                       
*                                                                               
VALEST15 CLI   0(R1),C'-'         NEGATIVE FILTER?                              
         BNE   VALEST20                                                         
         BCT   R0,*+8                                                           
         B     ERREST                                                           
         LA    R1,1(R1)                                                         
         NI    0(R1),X'FF'-X'40'  TURN NEGATIVE TO LOWER CASE                   
*                                                                               
VALEST20 MVC   0(1,RF),0(R1)      SAVE FILTER                                   
         LA    R1,1(R1)                                                         
         BCT   R0,*+8                                                           
         B     VALESTX                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,VALEST15                                                      
         B     ERREST             NO MORE THAN 3 ALLOWED                        
*                                                                               
VALEST30 MVC   ESTNM,RSPACES      ESTIMATE IS NUMERIC                           
         MVI   RBYTE,1            VALIDATING FIRST ESTIMATE                     
VALEST35 OC    4(4,R3),4(R3)                                                    
         BZ    ERREST             EST NUMBER ZERO INNVALID                      
         CLC   4(4,R3),=F'255'                                                  
         BH    ERREST             AND GREATER THAN 255 INVALID                  
         MVC   TEMPEST,7(R3)                                                    
         MVI   QSEPEST,C'N'                                                     
*                                                                               
         TM    TWAAUTH,X'80'      IF ONLY SINGLE EST REQUIRED                   
         BZ    *+12                                                             
         BAS   RE,CHKREQ          REQ=Y MUST BE ON POL EST                      
         BNE   ERREST                                                           
         OC    QPRD,QPRD          LISTING PRODUCTS                              
         BNZ   VALEST40                                                         
         BAS   RE,CHKPOL          ONLY NEED TO KNOW IF LISTING PRODUCTS         
VALEST40 BAS   RE,CHKEST                                                        
         BNE   ERREST                                                           
         CLI   RBYTE,2            FINISHED VALIDATING 2ND EST?                  
         BE    VALESTX            YES - WE ARE DONE                             
         CLI   RPARM+4,2          NO  - IS THERE A 2ND  ESTIMATE?               
         BNE   VALESTX                                                          
         TM    TWAAUTH,X'80'      IF ONLY SINGLE EST REQUIRED                   
         BO    ERREST             ERROR                                         
         LA    R3,32(R3)          POINT TO NEXT BLOCK                           
         MVI   RBYTE,2            VALIDATING SECOND ESTIMATE                    
         MVI   ESTOWSDY,0         OWK IS MON WHEN MORE THAN 1 EST               
         B     VALEST35                                                         
*                                                                               
VALESTX  B     ROUTSX                                                           
         SPACE 2                                                                
* CHKREQ - MAKE SURE POL ESTIMATE HAS REQ=Y                                     
*                                                                               
CHKREQ   NTR1                                                                   
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,TEMPEST                                                  
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,FILRD                                                        
         BNE   NO                                                               
         TM    EFLAG1,EF1REQ                                                    
         BZ    NO                                                               
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*==========================================================*                    
* GETEST - GETS AN ESTIMATE RECORD                         *                    
*           NTRY - 0(R1)= A(BINARY ESTIMATE NUMBER)        *                    
*                  4(R1)= A(BINARY ESTIMATE NUMBER # TWO)  *                    
*           EXIT - CC=EQUAL AND SETS ESTIMATE VALUES       *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
GETEST   L     RE,0(R1)                                                         
         MVC   RDUB(L'BEST),0(RE)                                               
         L     RE,4(R1)                                                         
         MVC   RDUB2(L'BEST2),0(RE)                                             
         CLC   BEST,RDUB           TEST FOR CHANGE OF ESTIMATE                  
         BNE   GETEST2                                                          
         CLC   BEST2,RDUB2                                                      
         BE    GETESTX                                                          
*                                                                               
GETEST2  MVI   RBYTE,1            FIRST ESTIMATE                                
         MVC   TEMPEST,RDUB                                                     
GETEST4  BAS   RE,CHKEST                                                        
         BNE   ERREST                                                           
         CLI   RBYTE,1                                                          
         BNE   GETESTX                                                          
         OC    RDUB2(L'BEST),RDUB2  IS THERE A SECOND ESTIMATE?                 
         BZ    GETESTX                                                          
         MVI   RBYTE,2              YES - GET IT                                
         MVC   TEMPEST,RDUB2                                                    
         B     GETEST4                                                          
*                                                                               
GETESTX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* VALSDT - VALIDATES A START DATE                          *                    
*           NTRY - R1=A(FIELD HEADER OF ESTIMATE FIELD)    *                    
*           EXIT - CC=EQUAL SETS DATE VALUES               *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
VALSDT   MVI   FVMINL,1            REQUIRED FIELD                               
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BNE   ERRDATE                                                          
         CLC   =C'ES',FVIFLD      USE ESTIMATE DATES                            
         BE    VALSDT5                                                          
         LA    RE,FVIFLD                                                        
         ST    RE,RPARM                                                         
         MVI   RPARM,2            M/Y FORMAT                                    
         TM    INOIND,INOIWK      WEEKLY FORMAT?                                
         BNO   VALSDT2                                                          
         MVI   RPARM,0            M/D/Y FORMAT                                  
VALSDT2  GOTO1 VDATVAL,RPARM,,APWORK                                            
         XR    R0,R0                                                            
         OC    RPARM(4),RPARM                                                   
         BZ    ERRDATE           INVALID                                        
         LLC   R0,FVIHDR+5        IF REQUESTED DATE LENGTH                      
         C     R0,RPARM           GREATER THAN RETURNED LENGTH                  
         BH    ERRDATE            INVALID DATE                                  
         CLC   QSTDT,APWORK      TEST CHANGE OF START DATES                     
         BE    VALSDTX                                                          
*                                                                               
         MVC   QSTDT,APWORK                                                     
         MVC   APDUB(6),QSTDT                                                   
         TM    INOIND,INOIWK                                                    
         BNO   VALSDT3                                                          
         BAS   RE,GTBRDST         GET BROADCAST WEEK START                      
         MVC   QSTDT(6),APDUB                                                   
VALSDT3  CLC   QSTDT+4(2),=C'00'                                                
         BNE   *+10                                                             
         MVC   QSTDT+4(2),=C'01'                                                
         GOTO1 VDATCON,RPARM,(0,APWORK),(3,APWORK+6)                            
         MVC   BSTDT,APWORK+6                                                   
         B     VALSDTX                                                          
*                                                                               
VALSDT5  CLC   QPRD,=C'ALL'       MUST BE ONLY ONE PRODUCT                      
         BE    ERRDATE                                                          
         OC    QPRD,QPRD                                                        
         BZ    ERRDATE                                                          
         CLC   =C'ALL',QEST       AND MUST BE ONLY ONE ESTIMATE                 
         BE    ERRDATE                                                          
         CLC   =C'NO',QEST                                                      
         BE    ERRDATE                                                          
         CLC   =C'LST',QEST                                                     
         BE    ERRDATE                                                          
         OC    QEST2,QEST2        RANGE OF EST NO GOOD                          
         BNZ   ERRDATE                                                          
*        MVC   QSTDT,ESTST        USE ESTIMATE DATES                            
***                                                                             
* WANT TO SEE BROADCAST WEEK START, NOT EST DATE START!                         
***                                                                             
         GOTO1 VGTBROAD,RPARM,(1,ESTST),APWORK,VGETDAY,VADDAY                   
         CLI   0(R1),X'FF'                                                      
         BE    ERRDATE                                                          
         MVC   QSTDT,APWORK+6     START DATE                                    
         GOTO1 VDATCON,RPARM,(0,QSTDT),(3,BSTDT)                                
         MVC   QNDDT,ESTND        END DATE                                      
         GOTO1 VDATCON,RPARM,(0,QNDDT),(3,BNDDT)                                
VALSDTX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* VALEDT - VALIDATES AN END DATE                           *                    
*            NTRY - R1=A(FIELD HEADER OF END DATE          *                    
*            EXIT - CC=EQUAL SETS END DATES                *                    
*                   CC=NOT EQUAL ON ERROR WITH FVMSGNO SET *                    
*==========================================================*                    
*                                                                               
VALEDT   MVI   FVMINL,0            NOT REQUIRED FIELD                           
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BH    VALEDTX                                                          
         BE    VALEDT1                                                          
*                                 NO INPUT - MOVE IN                            
         MVC   APWORK(L'QSTDT),QSTDT  START TO GET THAT WEEK/MONTH              
         TM    INOIND,INOIWK                                                    
         BNO   VALEDT4                                                          
         GOTO1 VADDAY,APPARM,APWORK,APWORK,F'6'                                 
         B     VALEDT4                                                          
*                                                                               
VALEDT1  LA    RE,FVIFLD                                                        
         ST    RE,RPARM                                                         
         MVI   RPARM,2            M/Y FORMAT                                    
         TM    INOIND,INOIWK      WEEKLY FORMAT?                                
         BNO   VALEDT2                                                          
         MVI   RPARM,0            M/D/Y FORMAT                                  
VALEDT2  GOTO1 VDATVAL,RPARM,,APWORK                                            
         OC    RPARM(4),RPARM                                                   
         BZ    ERRDATE            INVALID                                       
         LLC   R0,FVIHDR+5        IF REQUESTED DATE LENGTH                      
         C     R0,RPARM           GREATER THAN RETURNED LENGTH                  
         BH    ERRDATE            INVALID DATE                                  
*                                                                               
VALEDT4  CLC   QNDDT,APWORK       TEST CHANGE OF END DATE                       
         BE    VALEDTX                                                          
         MVC   QNDDT,APWORK                                                     
         MVC   APDUB(6),QNDDT                                                   
         TM    INOIND,INOIWK                                                    
         BNO   VALEDT6                                                          
         BAS   RE,GTBRDND         GET BROADCAST WEEK END                        
         MVC   QNDDT(6),APDUB                                                   
VALEDT6  CLC   QNDDT+4(2),=C'00'                                                
         BNE   *+10                                                             
         MVC   QNDDT+4(2),=C'15'                                                
         GOTO1 VDATCON,RPARM,(0,APWORK),(3,APWORK+6)                            
         MVC   BNDDT,APWORK+6                                                   
*                                                                               
VALEDTX  B     ROUTSX                                                           
         EJECT                                                                  
*=====================================*                                         
* GTBRDST - GIVEN DAY IN APDUB RETURNS*                                         
*           START OF ITS BROADCAST    *                                         
*           WEEK (ONLY USED FOR WKLY) *                                         
*=====================================*                                         
*                                                                               
GTBRDST  NTR1                                                                   
         GOTO1 VGETDAY,APPARM,APDUB,APFULL                                      
         LLC   R1,APPARM                                                        
         LLC   R0,ESTOWSDY                                                      
         CHI   R0,2                                                             
         BNL   GTBRD5                                                           
         BCTR  R1,0                                                             
         LNR   R2,R1                                                            
         B     GTBRD10                                                          
*                                                                               
GTBRD5   SR    R0,R1                                                            
         LNR   R2,R0                                                            
*                                                                               
GTBRD10  GOTO1 VADDAY,APPARM,APDUB,APDUB,(R2)                                   
         B     EXIT                                                             
         SPACE 2                                                                
GTBRDND  NTR1                                                                   
         GOTO1 VGETDAY,APPARM,APDUB,APFULL                                      
         LLC   R1,APPARM                                                        
         CHI   R1,7               SUNDAY                                        
         BE    GTBRDNX                                                          
         LA    R2,7                                                             
         SR    R2,R1                                                            
         GOTO1 VADDAY,APPARM,APDUB,APDUB,(R2)                                   
*                                                                               
GTBRDNX  B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* VALMOS - VALIDATES A MARKET/STATION                      *                    
*           NTRY - R1=A(FIELD HEADER OF MKT/STA FIELD)     *                    
*           EXIT - CC=EQUAL APPROPRIATE VALUES SET         *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
VALMOS   MVI   FVMINL,2           FIELD REQUIRED ( MIN LENGTH=2 )               
         MVI   FVMAXL,9           MAX LENGTH OF FIELD                           
         GOTO1 AFVAL                                                            
         BNE   VALMOSX                                                          
         XC    RWORK4,RWORK4                                                    
         XC    RPARM(12),RPARM                                                  
         GOTO1 VSCANNER,RPARM,FVIHDR,(2,RWORK4)                                 
         LA    R2,RWORK4                                                        
         CLI   RPARM+4,0                                                        
         BE    VALMOSX             INVALID                                      
*                                                                               
         XC    QMKT(QVALSX-QMKT),QMKT                                           
         XC    BMKT(BVALSX-BMKT),BMKT                                           
         LH    RE,=Y(SBMKTREC-WORKD)                                            
         LA    RE,WORKD(RE)                                                     
         XC    0(L'SBMKTREC,RE),0(RE)                                           
         CLI   RPARM+4,2                                                        
         BE    VALMOS20                                                         
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BNO   VALMOS5                                                          
         OC    INOMKT,INOMKT                                                    
         BNZ   ERRMKT                                                           
         CLC   =C'ALL',QPRD       (ONE HALF AND NUMERIC) MARKET                 
         BNE   VALMOS3                                                          
         TM    TWAMODE,TWAMLSM                                                  
         BNO   ERRPRD             ALL PRODUCTS ON LIST NOT ALLOWED              
         CLI   LISTYPE,C'P'                                                     
         BNE   ERRPRD             ALL PRODUCTS FOR PRODUCT LIST                 
VALMOS3  MVC   TEMPMKT(2),6(R2)                                                 
         BAS   RE,CHKMKT          VALIDATE MARKET #                             
         BNE   VALMOSX                                                          
         MVC   MOSFLG,=C'M '      SET FLAG                                      
         MVC   QSTA(3),=C'ALL'                                                  
         B     VALMOSX                                                          
*                                                                               
VALMOS5  CLC   =C'ALL',12(R2)     (ONE HALF & ALPHA)                            
         BNE   VALMOS8                                                          
         OC    QPRD,QPRD          MUST HAVE SINGLE MKT/STA                      
         BZ    ERRPRD             FOR PRODUCT=LIST                              
         CLI   CUACCS,C'+'        IF MARKET LIMIT ACCESS                        
         BE    ERRSECLK           'ALL' NOT ALLOWED                             
         CLI   CUACCS+2,C'+'                                                    
         BE    ERRSECLK                                                         
         TM    INOIND,INOIWK      AND FOR WEEKLY DATA                           
         BO    ERRNOTA                                                          
         MVC   QMKT(3),=C'ALL'                                                  
         MVC   QSTA(3),=C'ALL'                                                  
         MVC   MOSFLG,=C'A '      SET FLAG                                      
         B     VALMOSX                                                          
*                                                                               
VALMOS8  CLC   =C'LM',12(R2)      LIST MARKET REQUEST                           
         BNE   VALMOS12                                                         
         CLC   =C'ALL',QPRD       CAN'T HAVE ALL PRODUCTS                       
         BE    ERRPRD                                                           
         CLI   CUACCS,C'+'        IF MARKET LIMIT ACCESS                        
         BE    ERRSECLK           'LM' NOT ALLOWED                              
         CLI   CUACCS+2,C'+'                                                    
         BE    ERRSECLK                                                         
         MVC   MOSFLG,=C'ML'      SET FLAG                                      
         MVC   MKTNM,RSPACES                                                    
         MVC   QMKT,=X'F0F0F0F0'  START WITH MKT#0 (INCLUDE NETWORK)            
         B     VALMOSX                                                          
*                                                                               
VALMOS12 MVI   RBYTE,1            INDICATE FIRST HALF                           
         CLC   =C'ALL',QPRD                                                     
         BNE   VALMOS15                                                         
         TM    TWAMODE,TWAMLSM    PRODUCT CAN EQUAL ALL                         
         BNO   ERRPRD             FOR A SINGLE MKT - BUT ONLY                   
         CLI   LISTYPE,C'P'       FOR PRODUCT LIST                              
         BNE   ERRPRD                                                           
VALMOS15 BAS   RE,SETNSTA         SET STATION IN NEWSTA AND VALIDATE            
         B     VALMOSX                                                          
*                                                                               
VALMOS20 DS    0H                 SECOND HALF - MUST BE A LIST                  
         MVC   MOSFLG,=C'SL'      SET FLAG                                      
         CLC   =C'LM',12(R2)                                                    
         BNE   VALMOS25                                                         
         MVC   MOSFLG,=C'ML'      SET FLAG                                      
         CLI   CUACCS,C'+'        IF MARKET LIMIT ACCESS                        
         BE    ERRSECLK           'LM,XXXX' NOT ALLOWED                         
         CLI   CUACCS+2,C'+'                                                    
         BE    ERRSECLK                                                         
VALMOS25 CLC   =C'ALL',QPRD       CAN'T HAVE ALL PRODUCTS                       
         BE    ERRPRD                                                           
         LA    R2,32(R2)                                                        
         TM    2(R2),X'80'        MUST BE NUMERIC                               
         BNO   ERRSTA                                                           
         MVC   TEMPMKT(2),6(R2)                                                 
         BAS   RE,CHKMKT          MARKET LIST                                   
*                                                                               
VALMOSX  B     ROUTSX                                                           
         EJECT                                                                  
*              ROUTINE TO SET NEWSTA (EIGHT CHARACTERS)                         
*                                 R2=A(SCANNER ENTRY BLOCK)                     
SETNSTA  NTR1                                                                   
         MVC   NEWSTA,12(R2)                                                    
         CLI   NEWSTA,C'0'        IF NOT CABLE STATION                          
         BNL   SETNSTA8                                                         
         CLI   15(R2),C'-'        AND 3 CHAR STA                                
         BNE   SETNSTA2                                                         
         MVC   NEWSTA,RSPACES                                                   
         MVC   NEWSTA(3),12(R2)                                                 
         MVC   NEWSTA+4(1),16(R2) MOVE IN MEDIA                                 
         B     SETNSTA8                                                         
*                                                                               
SETNSTA2 CLI   15(R2),C' '        NO MEDIA?                                     
         BNE   SETNSTA4                                                         
         MVC   NEWSTA,RSPACES                                                   
         MVC   NEWSTA(3),12(R2)                                                 
         MVC   NEWSTA+4(1),QMED                                                 
         B     SETNSTA8                                                         
*                                                                               
SETNSTA4 CLI   16(R2),C'-'        4 CHAR STATION                                
         BNE   SETNSTA6                                                         
         MVC   NEWSTA,RSPACES                                                   
         MVC   NEWSTA(4),12(R2)                                                 
         MVC   NEWSTA+4(1),17(R2) MOVE IN MEDIA                                 
         B     SETNSTA8                                                         
*                                                                               
SETNSTA6 CLI   16(R2),C' '                                                      
         BNE   SETNSTA8                                                         
         MVC   NEWSTA,RSPACES                                                   
         MVC   NEWSTA(4),12(R2)                                                 
         MVC   NEWSTA+4(1),QMED                                                 
*                                                                               
SETNSTA8 CLI   AGYPRF7,C'C'       IF CANADIAN                                   
         BNE   SETNSTA9                                                         
         CLI   QMED,C'C'          COMBINED OR NETWORK                           
         BE    *+12                                                             
         CLI   QMED,C'N'                                                        
         BNE   SETNSTA9                                                         
         CLI   0(R2),5                                                          
         BNH   SETNSTA9           DON'T ALLOW >5 CHRS UNLESS...                 
         LA    RF,16(R2)          SPECIALTY CABLE (STAT/XX OR STA/XX)           
         CLI   0(R2),7                                                          
         BH    ERRSTA             CAN'T BE >7 CHRS                              
         BE    *+6                                                              
         BCTR  RF,0                                                             
         CLI   0(RF),C'/'                                                       
         BNE   ERRSTA             NOT CABLE                                     
         MVC   NEWSTA,RSPACES                                                   
         MVC   NEWSTA(7),12(R2)   STAT/XX                                       
         CLI   0(R2),7                                                          
         BE    *+8                                                              
         MVI   NEWSTA+6,C' '      STA/XX                                        
*                                                                               
SETNSTA9 BAS   RE,CHKSTA          VALIDATE STATION                              
         BNE   *+10                                                             
         MVC   MOSFLG,=C'S '      SET FLAG                                      
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* GETMOS - GETS A MARKET/STATION RECORD                    *                    
*           NTRY - 0(R1)= A(BINARY MARKET NUMBER)          *                    
*                  4(R1)= A(STATION CALL LETTERS)          *                    
*                  8(R1)= APMOSFLG                         *                    
*           EXIT - CC=EQUAL AND SETS ESTIMATE VALUES       *                    
*                  CC=NOT EQUAL ON ERROR WITH FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
GETMOS   L     RE,0(R1)                                                         
         MVC   RDUB(L'BMKT),0(RE)                                               
         L     RE,4(R1)                                                         
         MVC   RDUB2,RSPACES       SPACE PAD                                    
         MVC   RDUB2(L'QSTA),0(RE)                                              
         L     RE,8(R1)                                                         
         MVC   RHALF,0(RE)                                                      
         CLC   BMKT,RDUB           TEST FOR CHANGE OF ESTIMATE                  
         BNE   GETMOS2                                                          
         CLC   QSTA,RDUB2                                                       
         BE    GETMOSX                                                          
*                                                                               
GETMOS2  CLI   RHALF+1,C'L'       LIST?                                         
         BNE   GETMOS4                                                          
         MVC   NEWSTA,RSPACES      YES -PAD IT WITH SPACES                      
         MVC   NEWSTA,RDUB2        ALWAYS SHOWING STATION                       
         CLI   NEWSTA,C'0'         IF CABLE STATION                             
         BL    *+8                                                              
         MVI   NEWSTA+4,C' '       DON'T PASS ANY BAND                          
         CLI   AGYPRF7,C'C'        IF CANADIAN                                  
         BNE   *+8                                                              
         BRAS  RE,SETCNCBL         SET ANY CABLE MARKET SUFFIX                  
         BAS   RE,CHKSTA                                                        
         B     GETMOSX                                                          
*                                                                               
GETMOS4  CLI   RHALF,C'M'          JUST ONE MARKET                              
         BNE   GETMOS6                                                          
         MVC   TEMPMKT(2),RDUB     YES                                          
         BAS   RE,CHKMKT                                                        
         B     GETMOSX                                                          
*                                                                               
GETMOS6  MVC   NEWSTA,RSPACES      PAD IT WITH SPACES                           
         MVC   NEWSTA,RDUB2        MUST BE JUST ONE STATION                     
         CLI   NEWSTA,C'0'         IF CABLE STATION                             
         BL    *+8                                                              
         MVI   NEWSTA+4,C' '       DON'T PASS ANY BAND                          
         CLI   AGYPRF7,C'C'        IF CANADIAN                                  
         BNE   *+8                                                              
         BRAS  RE,SETCNCBL         SET ANY CABLE MARKET SUFFIX                  
         BAS   RE,CHKSTA                                                        
*                                                                               
GETMOSX  B     ROUTSX                                                           
         EJECT                                                                  
*================================*                                              
* INTSPBK- INITIALIZES SPOTBLOCK *                                              
*          LISTYPE MUST BE SET   *                                              
*================================*                                              
*                                                                               
INTSPBK  DS    0H                                                               
         XC    ESTTB,ESTTB        CLEAR ESTIMATE TABLE                          
         LA    R0,SBLOCK          CLR BLK SKIPPING SPAGYREC & SPMKTREC          
         LA    R1,SBAGYREC-SBLOCK                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,SBBKOPT                                                       
         LA    R1,SBLOCKX-SBBKOPT                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                 SET UP CALL TO SPOTIO                         
         MVC   SBCOMFAC,ACOM      A(COMFACS)                                    
         GOTO1 VDATCON,APPARM,(5,APWORK),SBQTODAY                               
         MVC   SBAIO1(8),AIOAREA1                                               
         MVC   SBLSPTTB,=F'7000'  LENGTH OF SPOT TABLE                          
         MVC   SBASPTTB,ASPTTB    A(SPOT TABLE)                                 
         MVC   SBACHUNK,ACHUNK    A(CHUNK)                                      
         LH    R1,=Y(CLTREC-WORKD)  IO AREA WHICH WE PASS                       
         LA    R1,WORKD(R1)                                                     
         ST    R1,SBACLTRC        SPOTIO                                        
         LH    R1,=Y(ESTREC-WORKD)  IO AREA WHICH WE PASS                       
         LA    R1,WORKD(R1)                                                     
         ST    R1,SBAESTRC        SPOTIO                                        
*                                 ---FILTERS                                    
         MVC   SBQAGY,CUAALF      AGENCY                                        
         MVC   SBBAGYMD,BAGYMD    BINARY AGY/MED                                
         MVC   SBQMED,QMED        MEDIA                                         
         MVC   SBMED,QMED                                                       
         MVC   SBMEDNM,MEDNM                                                    
         MVC   SBQCLT,QCLT        CLIENT                                        
         MVC   SBBCLT,BCLT        BINARY CLIENT                                 
         OI    SBQSKIP,SBQSKGL    SKIP READING GOALS -PERIOD!!!!                
         OI    SBQSKIP,SBQSKMKT   SKIP READING MARKET RECORDS !!                
*                                 TO HAVE LESS I/O'S                            
         OI    SBQSKIP,SBQSKMED   ACCOUNT ONLY -READ LESS RECS                  
         MVC   SBSPPROF,SVPROF                                                  
*                                                                               
         MVI   SBQSPILL,C'N'      NO SPILL- PLEASE                              
         MVC   SBQMKT,QMKT        MARKET                                        
         MVC   SBQSTA,QSTA        STATION                                       
         MVC   SBQCNET,QSTACNET   NETWORK FOR US CABLE STATIONS                 
         MVC   SBQNBITS,NTWKBITS  NETWORK BITS                                  
         CLI   CANMKT0,C'Y'       IF CANADIAN NETWORK OR COMBINED               
         BNE   INITSP3                                                          
         CLI   SBQNBITS,1         TEST CONVERTED CANADIAN CABALE                
         BE    INITSP3                                                          
         MVC   SBQNET,QSTA        SET NETWORK FOR BILL RECORDS                  
         XC    SBQSTA,SBQSTA                                                    
*                                                                               
INITSP3  OI    SBQRDOPT,SBQROSTA  SKIP 0E01 RECS WITH STABKCUR=X'01'            
         CLI   LISTYPE,C'M'                                                     
         BNE   *+8                                                              
         OI    SBQRDOPT,SBQROMAO  START LISTING AT THIS MARKET                  
         MVC   SBQESFLT,QSEPFLT   SEPERATE EST                                  
         MVC   SBQSEPES,QSEPEST   ESTIMATE FILTERS                              
         MVI   SBQEST,1           ESTIMATE START AND END                        
         MVI   SBQESTND,255                                                     
         CLC   =C'ALL',QEST                                                     
         BE    INITSP5                                                          
         CLC   =C'NO',QEST                                                      
         BE    INITSP5                                                          
         CLC   =C'LST',QEST                                                     
         BE    INITSP5                                                          
         MVC   SBQEST,BEST                                                      
         MVC   SBQESTND,BEST      INDICATES SINGLE ESTIMATE                     
         CLI   BEST2,0                                                          
         BE    INITSP5                                                          
         MVC   SBQESTND,BEST2                                                   
INITSP5  MVC   SBQSTART,QSTDT     DATE VALUES                                   
         MVC   SBQEND,QNDDT                                                     
         MVC   SBBQST,BSTDT       BINARY DATE VALUES                            
         MVC   SBBQEND,BNDDT                                                    
*                                                                               
         MVC   SBPRDNM,PRDNM                                                    
         CLI   LISTYPE,C'P'       LISTYPE=0 FOR DISPLAY                         
         BNE   INITSP8                                                          
         CLI   POLEST,C'Y'        READING BY POL POINTERS?                      
         BNE   INITSP15             NO - PROD VALUES FILLED BY 03 PHASE         
         MVC   SBQPRD,=C'POL'       YES                                         
         MVC   SBPRD,SBQPRD           READ FOR POL                              
         MVI   SBQBPRD,X'FF'                                                    
         OI    SBQPIND,SBQPOLSP       SEPERATE POL PRODUCTS                     
         MVI   SBEUNALL,C'Y'          SHOW UNALLOCATED SPOTS                    
         MVI   SBEBYPRD,C'Y'                                                    
         MVI   SBEPRD,0                                                         
         B     INITSP15                                                         
*                                                                               
INITSP8  CLC   =C'UNA',QPRD                                                     
         BNE   INITSP10                                                         
         MVC   SBQPRD,=C'POL'                                                   
         MVC   SBPRD,SBQPRD                                                     
         MVI   SBQBPRD,X'FF'                                                    
         MVI   SBEUNALL,C'Y'      UNALLOCATED SPOTS                             
         MVI   SBEBYPRD,C'N'                                                    
         MVI   SBEPRD,0                                                         
         B     INITSP15                                                         
*                                                                               
INITSP10 MVC   SBQPRD,QPRD        SET PRD VALUES - IN DISPLAY MODE              
         MVC   SBPRD,QPRD         AND OTHER PRODUCT                             
         MVC   SBQBPRD,BPRD       RELATED VALUES                                
         CLC   =C'POL',SBQPRD     IF POL REQUEST                                
         BNE   INITSP11                                                         
         CLC   =C'ALL',QMKT       AND READING HEADER RECORDS                    
         BNE   *+8                                                              
         MVI   SBQBPRD,0          FUDGE BINARY PRODUCT                          
INITSP11 MVI   SBEBYPRD,C'N'      DON'T SPLIT                                   
*                                                                               
         MVI   SBEUNALL,C'Y'      UNALLOCATED SPOTS                             
         CLC   =C'UNA',SBQPRD                                                   
         BE    INITSP12                                                         
         CLC   =C'ALL',SBQPRD                                                   
         BE    INITSP12                                                         
         MVI   SBEUNALL,C'N'      NO                                            
*                                                                               
INITSP12 MVI   SBEPRD,0                                                         
         CLC   =C'ALL',QPRD                                                     
         BE    INITSP15                                                         
         CLC   =C'POL',QPRD                                                     
         BE    INITSP15                                                         
         MVC   SBEPRD,SBQBPRD                                                   
*                                                                               
INITSP15 MVI   SBEBEF,C'N'                                                      
         TM    INOIND,INOIPRE     PREVIOUS DATA?                                
         BNO   *+8                                                              
         MVI   SBEBEF,C'Y'                                                      
         MVI   SBEAFTER,C'N'                                                    
         TM    INOIND,INOIPOS     POST DATA?                                    
         BNO   *+8                                                              
         MVI   SBEAFTER,C'Y'                                                    
*                                                                               
         MVI   SBESPLIT,C'Y'      DEFAULT IS TO SPLIT PIGGYS                    
         CLI   BPRD2,0            ON DISPLAY SCREEN - DON'T SPLIT               
         BE    *+8                ONLY IF THEY REQUEST PIGGY BACK               
         MVI   SBESPLIT,C'N'      PRODUCT                                       
         CLI   INOSPL,C'N'        ON LIST SCREEN -DON'T SPLIT ONLY              
         BNE   *+8                IF THEY REQUEST THAT                          
         MVI   SBESPLIT,C'N'                                                    
*                                                                               
         TM    INOIND,INOICAN     CANADIAN DOLLARS                              
         BNO   *+12                                                             
         MVI   SBQCURR,C'C'                                                     
         B     INITSP35                                                         
         TM    INOIND,INOIUSA     USA DOLLARS                                   
         BNO   *+8                                                              
         MVI   SBQCURR,C'U'                                                     
*                                                                               
INITSP35 OI    SBQPER,SBQPMN      DATA BY MONTHS (DEFAULT)                      
         TM    INOIND,INOIWK      WEEKLY DATA?                                  
         BNO   *+8                                                              
         OI    SBQPER,SBQPWK      YES                                           
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
*                                                                               
         MVI   SBESPILL,0         ORIGINAL MARKET ONLY                          
         MVI   SBEDEMTY,0         NO DEMOS                                      
         MVI   SBEDEM,C'N'                                                      
         MVI   SBEPAID,C'Y'       EXTRACT PAID                                  
         NI    SBQSKIP,FF-SBQSKBUY      MAKE SURE READ BUYS                     
         CLC   =C'UNA',QPRD             SKIP READING BILLING                    
         BE    INITSP40                 IF ASKING FOR UNALLOCATED               
         CLI   BPRD2,0                   OR IF ASKING FOR PIGGYS                
         BNE   INITSP40                                                         
         CLI   INOSPL,C'N'               OR IF ASKING FOR SPL=NO                
         BE    INITSP40                                                         
         TM    INOIND2,INOIBIL           OR IF NOT ASKING FOR BILL              
         BNO   INITSP40                                                         
         TM    INOIND,INOIWK             OR IF DATA BY WEEKS                    
         BNO   *+8                                                              
INITSP40 OI    SBQSKIP,SBQSKBIL                                                 
*                                                                               
         TM    INOIND2,INOICOS2   TEST COST2 REQUEST                            
         BZ    *+8                                                              
         OI    SBEFLAG,SBE2COS     WHY DO THE 2 MOVE ???                        
*                                                                               
         CLC   =C'ALL',QMKT                                                     
         BNE   INITSPX                                                          
         OI    SBQSKIP,SBQSKBIL         SKIP READING '0E01'                     
         OI    SBQSKIP,SBQSKBUY         SKIP READING BUYS                       
         CLC   =C'UNA',QPRD             SKIP READING BILL                       
         BE    INITSP50                 IF UNALLOCATED REQUEST                  
         CLI   BPRD2,0                  OR PIGGY REQUEST                        
         BNE   INITSP50                                                         
         TM    INOIND,INOIWK            OR WEEKLY REQUEST                       
         BO    INITSP50                                                         
         TM    INOIND2,INOIBIL          OR IF NOT ASKING FOR BILL               
         BNO   INITSP50                                                         
         CLI   INOSPL,C'N'              OR SPL=NO                               
         BE    INITSP50                                                         
         OI    SBQREAD,SBQRDBH                                                  
INITSP50 DS    0H                                                               
INITSPX  B     ROUTSX                                                           
         EJECT                                                                  
*=============================*                                                 
* CLRBLK - CLEARS BLK POINTED *                                                 
*          TO BY R1 TO PACKED *                                                 
*          ZEROS              *                                                 
*=============================*                                                 
*                                                                               
LCLRBLK  NTR1                      LOCAL ENTRY PT                               
         MVI   RFLAG,1                                                          
         B     CLRBLK2                                                          
*                                                                               
CLRBLK   MVI   RFLAG,0            COMMON ENTRY POINT                            
CLRBLK2  XC    0(L'BLKLBL,R1),0(R1)      CLEAR LABEL                            
         LA    R1,L'BLKLBL(R1)    PT TO ACCUMULATORS                            
         LA    RE,ROWNUMS         # OF ROWS                                     
CLRBLK5  LLC   RF,NUMDTS          # OF DATES PLUS                               
         AHI   RF,3               TOTAL,PREV,POST                               
CLRBLK10 ZAP   0(L'AMOUNTS,R1),=P'0'                                            
         LA    R1,L'AMOUNTS(R1)                                                 
         BCT   RF,CLRBLK10                                                      
         BCT   RE,CLRBLK5                                                       
         CLI   RFLAG,1                                                          
         BNE   ROUTSX                                                           
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
*===================================*                                           
* SPHOOK - SPOT I/O HOOK BUILDS BLK *                                           
*===================================*                                           
*                                                                               
SPHOOK   MVI   CANFLAG,C'N'       NOT CANADIAN STATION/NTWRK                    
         CLI   AGYPRF7,C'C'                                                     
         BNE   SPHOOK10                                                         
         CLI   QMED,C'C'                                                        
         BE    SPHOOK5                                                          
         CLI   QMED,C'N'                                                        
         BNE   SPHOOK10                                                         
SPHOOK5  MVI   CANFLAG,C'Y'       YES CANADIAN STATION/NTWK                     
*                                                                               
SPHOOK10 CLI   SBMODE,SBPROCES    ESTIMATE FIRST                                
         BE    EST                                                              
         CLI   SBMODE,SBPROCBH    BILL HEADER RECORD                            
         BE    BH                                                               
         CLI   SBMODE,SBPROCBL    STATION BILL RECORD                           
         BE    BILL                                                             
         CLI   SBMODE,SBPROCSP    BUY RECORD                                    
         BE    BUY                                                              
*                                                                               
SPHOOKX  B     ROUTSX                                                           
         EJECT                                                                  
*===========================*                                                   
* PROCESS ESTIMATE RECORDS  *                                                   
*===========================*                                                   
*                                                                               
EST      MVI   ADDFLG,C'N'        NO LAST STATION BUILD UNTIL SET TO Y          
         CLC   =C'ALL',QMKT       DON'T READ IF MKT<> ALL                       
         BNE   ESTX                                                             
*                                                                               
EST1     L     R1,SBAIO1                                                        
         USING ESTHDR,R1                                                        
         CLC   =C'POL',QPRD       POOL REQUEST?                                 
         BNE   EST3                                                             
         CLC   =C'POL',EKEYPRD    WE DON'T WANT UNALLOCATED DOLLARS             
         BE    ESTX               WHICH ARE IN POL EST POINTER                  
EST3     TM    TWAMODE,TWAMLSM    LIST MODE                                     
         BNO   EST15                                                            
         CLI   LISTYPE,C'E'       AND ESTIMATE LIST                             
         BNE   EST15                                                            
         MVI   FIRSTB,C'Y'                                                      
         CLI   TBEST,0          FIRST TIME?                                     
         BNE   EST5                                                             
         MVC   TBEST,EKEYEST                                                    
         MVI   ADDFLG,C'Y'                                                      
         B     EST15                                                            
*                                                                               
EST5     CLC   TBEST,EKEYEST                                                    
         BE    EST15                                                            
         BAS   RE,TSARCUM         ADD CURRENT ESTIMATE                          
         MVC   TBEST,EKEYEST      MOVE IN NEW ESTIMATE                          
         MVI   ADDFLG,C'Y'                                                      
*                                                                               
EST15    MVC   DATES(L'ESTART),ESTART     ESTIMATE START DAY                    
         MVC   DATES+6(L'EEND),EEND                                             
         BAS   RE,BLDELST                                                       
*                                 COMPARE EMLIST TO DTLIST                      
EST20    LA    R3,DTLIST                                                        
         LA    R2,EMLIST                                                        
*                                                                               
EST30    XC    APPARM+12(4),APPARM+12                                           
         CLC   0(2,R2),0(R3)                                                    
         BL    EST40              DATE LOWER THAN FIRST DATE IN LIST            
         BE    EST60                                                            
         LA    R3,2(R3)           BUMP TO NEXT DATE IN LIST                     
         CLI   0(R3),0            LAST DATE?                                    
         BNE   EST30                                                            
*                                                                               
         TM    INOIND,INOIPOS     YES - POST DATA REQUESTED?                    
         BNO   EST70                      NO                                    
         MVI   APPARM+12,2                YES - GO ADD IT                       
         B     EST60                                                            
*                                                                               
EST40    TM    INOIND,INOIPRE     PREVIOUS DATA REQUESTED?                      
         BNO   EST70                NO - GET NEXT DATE IN EST LIST              
         MVI   APPARM+12,1          YES - GO ADD IT                             
         B     EST60                                                            
*                                 PROCESS ESTIMATE INFORMATION                  
EST60    GOTO1 VDATCON,APPARM,(2,0(R2)),(3,APDUB)                               
         SR    RF,RF                                                            
         IC    RF,APDUB+1         GET MONTH #                                   
         BCTR  RF,0                                                             
         MHI   RF,6               MULTIPY BY 6 TO SET TBL LOCATOR               
         ST    RF,APFULL                                                        
*                                 POST ORDERED YTD                              
         L     R1,SBAIO1                                                        
         USING ESTHDR,R1                                                        
         L     RF,APFULL                                                        
         LA    RE,EORD(RF)                                                      
         TM    INOIND,INOINET     NET DOLLARS?                                  
         BNO   *+8                                                              
         LA    RE,EORDNET(RF)                                                   
         ZAP   AMT,0(6,RE)         AMOUNT TO POST                               
         CP    AMT,=P'0'                                                        
         BE    EST64                                                            
         GOTO1 ADDIT,APPARM,AMT,1,(R3)                                          
*                                 POST PAID YTD                                 
EST64    L     R1,SBAIO1                                                        
         USING ESTHDR,R1                                                        
         L     RF,APFULL                                                        
         LA    RE,EPAID(RF)                                                     
         TM    INOIND,INOINET     NET DOLLARS DOWN?                             
         BNO   *+8                                                              
         LA    RE,EPDNET(RF)                                                    
         ZAP   AMT,0(6,RE)         AMOUNT TO POST                               
         CP    AMT,=P'0'                                                        
         BE    EST70                                                            
         GOTO1 ADDIT,APPARM,AMT,2,(R3)                                          
*                                                                               
EST70    LA    R2,2(R2)           GET NEXT DATE IN EST LIST                     
         CLI   0(R2),0                                                          
         BNE   EST30                                                            
*                                 END OF TABLE                                  
ESTX     B     ROUTSX                                                           
         EJECT                                                                  
*===============================*                                               
* BLDELST - BUILDS ESTIMATE LIST*                                               
*           CALLED FROM EST RTN *                                               
*===============================*                                               
*                                                                               
BLDELST  NTR1                                                                   
         MVC   TEMPPROF,SVPROF                                                  
         GOTO1 VGTBROAD,RPARM,(1,DATES+6),APWORK,VGETDAY,VADDAY                 
         MVC   DATES+6(6),APWORK+6   MAKE SURE GET BROADCAST END                
         TM    INOIND,INOIBRD                                                   
         BNO   BLDELST1                                                         
         MVI   APBYTE,0              IGNORE PROFILE                             
         MVC   TEMPPROF+6(3),=X'010101'                                         
         B     BLDELST3                                                         
*                                                                               
BLDELST1 TM    INOIND2,INOIB3     USE B3 CALANDER                               
         BNO   BLDELST2                                                         
         MVC   APBYTE,SVPROFB3+2                                                
         NI    APBYTE,X'0F'                                                     
         MVC   TEMPPROF+6(3),SVPROFB3+6                                         
         B     BLDELST3                                                         
*                                                                               
BLDELST2 MVC   APBYTE,SVPROF+2                                                  
         NI    APBYTE,X'0F'                                                     
BLDELST3 XC    APWORK,APWORK                                                    
         MVC   APWORK+0(4),VGTBROAD                                             
         MVC   APWORK+4(4),VADDAY                                               
         MVC   APWORK+8(4),VGETDAY                                              
         MVC   APWORK+12(4),VDATCON                                             
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R2,APELEM                                                        
         LA    RE,DATES                                                         
         ST    RE,APPARM                                                        
         MVI   APPARM,12                                                        
         GOTO1 VMOBILE,APPARM,,(APBYTE,(R2)),APWORK,TEMPPROF                    
         CLI   0(R2),X'FF'                                                      
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     *-12                                                             
         MVI   0(R2),0            MARK END OF LIST WITH ZERO                    
*                                                                               
         LA    R0,12                                                            
         LA    R2,APELEM                                                        
         XC    EMLIST,EMLIST                                                    
         LA    R3,EMLIST                                                        
         GOTO1 VDATCON,APPARM,DATES,(2,APDUB)                                   
         CLC   APDUB(2),0(R2)                                                   
         BNH   *+8                                                              
         LA    R2,2(R2)           ESTART SHOULD BE LE TO LIST START             
*                                                                               
BLDELST6 CLI   0(R2),0                                                          
         BE    BLDELSTX                                                         
         MVC   0(2,R3),0(R2)                                                    
         TM    INOIND,INOIBRD     IGNORE PROFILE OPTION                         
         BO    *+12                                                             
         CLI   SVPROF+2,0                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),2(R2)                                                    
         LA    R3,2(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,BLDELST6                                                      
*                                                                               
BLDELSTX B     EXIT                                                             
         EJECT                                                                  
*=============================*                                                 
* PROCESS BILL HEADER RECORDS *                                                 
*=============================*                                                 
*                                                                               
BH       L     R2,SBAIO1                                                        
         USING BILLRECD,R2                                                      
         CLI   BRETAIL,X'41'      SKIP RETAIL CORP SUMMARY BILLS                
         BE    BHX                                                              
         TM    TWAMODE,TWAMLSM    LIST MODE                                     
         BNO   BH10                                                             
         CLI   LISTYPE,C'E'       AND ESTIMATE LIST                             
         BNE   BH10                                                             
         CLI   FIRSTB,C'Y'                                                      
         BNE   BH3                                                              
         MVI   FIRSTB,C'N'                                                      
         CLI   ADDFLG,C'Y'                                                      
         BNE   BH3                                                              
         BAS   RE,TSARCUM                                                       
         MVI   TBEST,0                                                          
         B     BH3                                                              
*                                                                               
BH3      CLI   TBEST,0           FIRST TIME?                                    
         BNE   BH5                                                              
         MVC   TBEST,BKEYEST                                                    
         MVI   ADDFLG,C'Y'                                                      
         B     BH10                                                             
*                                                                               
BH5      CLC   TBEST,BKEYEST                                                    
         BE    BH10                                                             
         BAS   RE,TSARCUM         ADD/CUM CURRENT ESTIMATE                      
         MVC   TBEST,BKEYEST      MOVE IN NEW ESTIMATE                          
         MVI   ADDFLG,C'Y'                                                      
*                                                                               
BH10     GOTO1 =V(SPBVAL),APPARM,(C'B',(R2)),AMTBLOCK,0,RR=ACRELO               
         LA    RE,AMTBLOCK                                                      
         USING AMTBLKD,RE                                                       
         ZAP   GROSAMT,SPBVGRSP                                                 
         ZAP   NETAMT,SPBVNETP                                                  
         DROP  RE                                                               
*                                                                               
***      CP    GROSAMT,=P'0'                                                    
***      BE    BHX                                                              
         ZAP   AMT,GROSAMT                                                      
         TM    INOIND,INOINET     NET DOLLARS DOWN?                             
         BNO   BH11                                                             
         ZAP   AMT,NETAMT                                                       
*                                                                               
BH11     LA    R3,DTLIST          DATE LIST                                     
         XC    APPARM+12(4),APPARM+12                                           
*                                                                               
         TM    INOIND,INOIBRD     IGNORE PROFILE                                
         BO    *+12                                                             
         CLI   SVPROF+2,0                                                       
         BNE   BH15                                                             
*                                                                               
BH12     GOTO1 VDATCON,APPARM,(2,0(R3)),(0,APWORK)                              
         GOTO1 VGTBROAD,APPARM,(1,APWORK),APWORK+6,VGETDAY,VADDAY               
         GOTO1 VDATCON,APPARM,(0,APWORK+12),(3,APFULL)                          
         CLC   BKEYYSRV(2),APFULL       CHECK MATCH ON YM                       
         BL    BH20                                                             
         BE    BH30                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),0            END OF DATE LIST?                             
         BNE   BH12                                                             
         B     BH18               PAST END OF LIST                              
*                                 DATE HIGHER THAN ANY DATE IN LIST             
BH15     MVC   APFULL(2),BKEYYSRV                                               
         MVI   APFULL+3,16                                                      
         GOTO1 VDATCON,APPARM,(3,APFULL),(2,APHALF)                             
         CLC   APHALF,0(R3)                                                     
         BL    BH20               LOWER THAN FIRST DATE IN LIST                 
BH17     CLC   APHALF,2(R3)                                                     
         BL    BH30                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   BH17                                                             
*                                 GREATER THAN LAST DATE IN LIST                
BH18     TM    INOIND,INOIPOS                                                   
         BNO   BHX                                                              
         MVI   APPARM+12,2        PROCESS POST DATA                             
         B     BH30                                                             
*                                                                               
BH20     TM    INOIND,INOIPRE     DATE LOWER THAN FIRST DATE IN LIST            
         BNO   BHX                PROCESS IT?                                   
         MVI   APPARM+12,1        YEAP                                          
         B     BH30                                                             
*                                                                               
BH30     GOTO1 ADDIT,APPARM,AMT,3,(R3)                                          
BHX      B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*===========================================*                                   
* CALNET - ROUTINE TO NET DOWN A COST (85%) *                                   
*         -NTRY - AMT = GROSS COST          *                                   
*         -XIT  - AMT = NET COST            *                                   
* NOTE - CALCULATIONS IN PACKED DECIMAL     *                                   
*        DUE TO THE LARGE NUMBERS POSSIBLE  *                                   
*===========================================*                                   
*                                                                               
CALNET   NTR1                                                                   
         ZAP   APDUB,AMT                                                        
         ZAP   TOT,APDUB                                                        
         MP    TOT,=PL8'170'                                                    
         SRP   TOT,64-2,5                                                       
         DP    TOT,=PL8'2'                                                      
         ZAP   APDUB(8),TOT(8)                                                  
         ZAP   AMT,APDUB                                                        
CALNETX  XIT1                                                                   
         EJECT                                                                  
*=====================================*                                         
* PROCESS STATION BILL RECORDS (0E01) *                                         
*=====================================*                                         
*                                                                               
BILL     OC    INOREP,INOREP                                                    
         BNZ   BILLX                                                            
         CLI   BILLFST,C'Y'                                                     
         BNE   BILL3                                                            
         CLI   ADDFLG,C'Y'        ADD LAST STATION BUILD                        
         BNE   BILL2              BY BUY IF THERE                               
         BAS   RE,TSARCUM                                                       
         CLI   LISTYPE,C'M'                                                     
         BNE   *+8                                                              
         BAS   RE,TSARTCUM        ADD/CUM  TOTAL RECORD TO TSAR                 
         MVI   ADDFLG,C'N'                                                      
BILL2    XC    TEMPSTA,TEMPSTA    RESET VALUES                                  
         MVI   TBPRD1,0                                                         
         MVI   TBEST,0                                                          
         MVI   BILLFST,C'N'                                                     
*                                                                               
BILL3    L     R3,SBAIO1                                                        
         USING STABUCKD,R3                                                      
         TM    TWAMODE,TWAMLSM    LIST/SELECT MODE?                             
         BNO   BILL30                                                           
         CLI   LISTYPE,C'P'       PRODUCT LIST?                                 
         BNE   BILL5                                                            
         CLI   TBPRD1,0                                                         
         BNE   BILL4                                                            
         MVC   TBPRD1,5(R3)       SET BINARY                                    
         GOTO1 FDPRD,APPARM,TBPRD1,TEMPRD1                                      
         XC    TEMPRD2,TEMPRD2                                                  
         MVI   ADDFLG,C'Y'                                                      
         B     BILL30                                                           
*                                                                               
BILL4    CLC   5(1,R3),TBPRD1                                                   
         BE    BILL30                                                           
         BAS   RE,TSARCUM                                                       
         MVI   ADDFLG,C'Y'                                                      
         MVC   TBPRD1,5(R3)       RESET IT TO NEW PRODUCT                       
         GOTO1 FDPRD,APPARM,TBPRD1,TEMPRD1                                      
         B     BILL30                                                           
*                                                                               
BILL5    CLI   LISTYPE,C'E'                                                     
         BNE   BILL8                                                            
         CLI   TBEST,0          FIRST TIME?                                     
         BNE   BILL7                                                            
         MVC   TBEST,STABKEST                                                   
         MVI   ADDFLG,C'Y'                                                      
         B     BILL30                                                           
BILL7    CLC   TBEST,STABKEST                                                   
         BE    BILL30                                                           
         BAS   RE,TSARCUM         ADD CURRENT ESTIMATE                          
         MVC   TBEST,STABKEST     MOVE IN NEW ESTIMATE                          
         MVI   ADDFLG,C'Y'                                                      
         B     BILL30                                                           
*                                                                               
BILL8    XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,CUAALF                                                   
         MVC   STAPCTRY,AGYPRF7                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOM                                                    
         MVC   STAPMKST,STABKMKT                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   STAPQSTA,C'0'       IF NOT CABLE STATION                         
         BNL   BILL8X                                                           
         CLI   STAPQSTA+4,C' '     SET BAND IF NOT ALREADY DEFINED              
         BH    *+10                                                             
         MVC   STAPQSTA+4(1),QMED                                               
         CLI   STAPQSTA+4,C'/'     CANADIAN CABLE IS REALLY NETWORK             
         BNE   *+8                                                              
         MVI   STAPQSTA+4,C'N'                                                  
         B     BILL9                                                            
*                                                                               
BILL8X   CLI   STAPQSTA+4,C' '     FOR CABLE                                    
         BH    BILL9                                                            
         MVI   STAPQSTA+4,C'/'     SET SLASH IF NOTHING DEFINED                 
*                                                                               
BILL9    MVC   APWORK,STAPQMKT     SIMULATE MSUNPK OUTPUT                       
         MVC   APWORK+4(5),STAPQSTA                                             
         CLI   CANFLAG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   APWORK+4+5(2),STAPQNET   CABLE SUFFIX TOO FOR CANADA             
         DROP  R1                                                               
*                                                                               
         OC    TEMPSTA,TEMPSTA    CHECK STATION                                 
         BNZ   BILL15                                                           
         MVC   TEMPMKT,APWORK     FIRST TIME ONLY                               
         MVC   TEMPSTA,APWORK+4                                                 
         MVI   ADDFLG,C'Y'                                                      
         CLI   CANFLAG,C'Y'                                                     
         BNE   BILL30                                                           
         SR    RF,RF              CANADIAN STATION - MUST                       
         ICM   RF,7,STABKMKT+2    SHIFT OUT LAST 5 BITS                         
         SRL   RF,8                                                             
         STCM  RF,7,CANSTA                                                      
         B     BILL30                                                           
*                                                                               
BILL15   CLI   CANFLAG,C'Y'                                                     
         BNE   BILL18                                                           
         SR    RF,RF                                                            
         ICM   RF,7,STABKMKT+2                                                  
         SRL   RF,8                                                             
         STCM  RF,7,NEWCSTA                                                     
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,CANSTA                                                      
*                                                                               
         CR    RE,RF                                                            
         BNE   BILL19                                                           
         CLI   APWORK+4+5,C' '                                                  
         BNH   BILL30                                                           
         CLC   TEMPMKT,APWORK     CABLE NEEDS TO CHECK MKT TOO                  
         BE    BILL30                                                           
         B     BILL19                                                           
*                                                                               
BILL18   CLC   TEMPSTA,APWORK+4   CHANGE IN STATION?                            
         BE    BILL30             NOPE                                          
BILL19   BAS   RE,TSARCUM         ADD/CUM TSAR RECORD                           
         MVI   ADDFLG,C'Y'                                                      
         CLI   LISTYPE,C'M'                                                     
         BNE   BILL20                                                           
         CLC   TEMPMKT,APWORK     CHANGE IN MARKET                              
         BE    BILL20                                                           
         BAS   RE,TSARTCUM        ADD/CUM  TOTAL RECORD TO TSAR                 
         BAS   RE,CHKCNT          CHECKS IF SCREEN FULL                         
         BNE   BILLX              IF SO - EXIT                                  
*                                                                               
BILL20   MVC   TEMPMKT,APWORK     MOVE IN NEW MARKET AND                        
         MVC   TEMPSTA,APWORK+4   STATION                                       
         CLI   CANFLAG,C'Y'                                                     
         BNE   BILL30                                                           
         MVC   CANSTA,NEWCSTA     SET NEW CANADIAN STATION                      
         EJECT                                                                  
*                                 PROCESS RECORD                                
BILL30   LA    R3,24(R3)                                                        
         USING STABELEM,R3                                                      
BILL35   CLI   0(R3),0                                                          
         BE    BILLX                                                            
         CLI   0(R3),X'0E'         SHOW ONLY 0E ELEMENTS                        
         BNE   BILL80                                                           
         GOTO1 =V(SPBVAL),APPARM,(C'E',(R3)),AMTBLOCK,0,RR=ACRELO               
         LA    RE,AMTBLOCK                                                      
         USING AMTBLKD,RE                                                       
         ZAP   GROSAMT,SPBVGRSP                                                 
         ZAP   NETAMT,SPBVNETP                                                  
         DROP  RE                                                               
*                                                                               
         LA    R2,DTLIST          DATE LIST                                     
         XC    APPARM+12(4),APPARM+12                                           
BILL45   TM    INOIND,INOIBRD     IGNORE PROFILE                                
         BO    *+12                                                             
         CLI   SVPROF+2,0                                                       
         BNE   BILL56                                                           
         GOTO1 VDATCON,APPARM,(2,0(R2)),(0,APWORK)                              
         GOTO1 VGTBROAD,APPARM,(1,APWORK),APWORK+6,VGETDAY,VADDAY               
         GOTO1 VDATCON,APPARM,(0,APWORK+12),(3,APFULL)                          
         CLC   STABPER,APFULL                                                   
         BL    BILL60             LOWER THAN FIRST DATE IN LIST                 
         BE    BILL70                                                           
         LA    R2,2(R2)           INCREMENT DATE LIST                           
         CLI   0(R2),0            END OF DATE LIST?                             
         BNE   BILL45               NO - CHECK NEXT DATE                        
         B     BILL58             GREATER THAN LAST DATE IN LIST                
*                                                                               
BILL56   MVC   APFULL(2),STABPER                                                
         MVI   APFULL+3,16                                                      
         GOTO1 VDATCON,APPARM,(3,APFULL),(2,APHALF)                             
         CLC   APHALF,0(R2)                                                     
         BL    BILL60             LOWER THAN FIRST DATE IN LIST                 
BILL57   CLC   APHALF,2(R2)                                                     
         BL    BILL70                                                           
         LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   BILL57                                                           
         B     BILL58             GREATER THAN LAST DATE IN LIST                
         EJECT                                                                  
BILL58   TM    INOIND,INOIPOS                                                   
         BNO   BILL80                                                           
         MVI   APPARM+12,2        PROCESS POST DATA                             
         B     BILL70                                                           
*                                                                               
BILL60   TM    INOIND,INOIPRE                                                   
         BNO   BILL80                                                           
         MVI   APPARM+12,1        PROCESS PREV DATA                             
         B     BILL70                                                           
*                                                                               
BILL70   TM    INOIND,INOINET     NET DOLLARS WANTED?                           
         BNO   BILL75                                                           
         CP    NETAMT,=P'0'                                                     
         BE    BILL80                                                           
         GOTO1 ADDIT,APPARM,NETAMT,3,(R2)                                       
         B     BILL80                                                           
*                                                                               
BILL75   CP    GROSAMT,=P'0'      GROSS DOLLARS                                 
         BE    BILL80                                                           
         GOTO1 ADDIT,APPARM,GROSAMT,3,(R2)                                      
*                                                                               
BILL80   LLC   R0,1(R3)           GET NEXT ELEMENT                              
         AR    R3,R0                                                            
         B     BILL35                                                           
*                                                                               
BILLX    OI    SBIOFLAG,SBNOIO    NEVER ANY IO SPOTIO                           
         B     ROUTSX                                                           
         EJECT                                                                  
*=====================*                                                         
* PROCESS BUY RECORDS *                                                         
*=====================*                                                         
*                                                                               
BUY      L     R2,SBAIO1          R2=A(BUY RECORD)                              
         USING BUYREC,R2                                                        
*                                                                               
         OC    INOREP,INOREP      IF SPECIAL REP REQUESTED                      
         BZ    *+14                                                             
         CLC   BDREP,INOREP       MATCH ON IT                                   
         BNE   BUYX                                                             
*                                                                               
         TM    BDSTAT2,X'10'       TEST DIY TRADE BUY                           
         BZ    BUY2                                                             
         CLI   QPRD+2,C'#'         TEST TRADE PRODUCT REQUEST                   
         BE    BUY2                                                             
         LA    RE,SBLOCK                                                        
         OI    SBEFLAG3-SBLOCK(RE),SBE3TRD  SET TO MERGE CASH/TRADE             
*                                                                               
         TM    INOIND2,INOITRD              UNLESS REQUESTED OTHERWISE          
         BZ    BUY2                                                             
         NI    SBEFLAG3-SBLOCK(RE),X'FF'-SBE3TRD UNSET MERGE                    
         OI    SBEFLAG3-SBLOCK(RE),SBE3NTRD   DO NOT MERGE CASH/TRADE           
*                                                                               
BUY2     TM    TWAMODE,TWAMLSM    LIST/SELECT MODE?                             
         BNO   BUY6                                                             
         BAS   RE,CHKBRK                                                        
         BNE   BUYX                                                             
         B     BUY6X                                                            
*                                                                               
BUY6     CLI   BUYKPRD,X'FF'      DISPLAYMODE                                   
         BE    BUY6X                                                            
         CLI   BPRD2,0            IF NON POL EST & NOT SINGLE PRD               
         BE    BUY6X                                                            
         OC    BDTIME,BDTIME      MAKE SURE BUY IS PIGGY BACK                   
         BZ    BUYX               NOT A PIGGYBACK BUY - REJECT                  
         LA    R1,BDELEM                                                        
BUY6C    CLI   0(R1),0                                                          
         BE    BUYX               NO A PIGGYBACK                                
         CLI   0(R1),X'04'        PIGGY BACK ELEMENT?                           
         BE    BUY6F                                                            
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BUY6C                                                            
*                                                                               
BUY6F    L     RE,SBAIO1          AND PRODUCTS MATCH                            
         CLC   3(1,RE),BPRD                                                     
         BNE   BUY6K                                                            
         CLC   2(1,R1),BPRD2                                                    
         BNE   BUYX                                                             
         B     BUY6X                                                            
*                                                                               
BUY6K    CLC   3(1,RE),BPRD2                                                    
         BNE   BUYX                                                             
         CLC   2(1,R1),BPRD                                                     
         BNE   BUYX                                                             
*                                                                               
BUY6X    MVC   SBBMKT,BUYMSTA                                                   
         LA    RE,DTSCP           DATE LIST                                     
         ST    RE,SBADATE                                                       
         LLC   RE,NUMDTS                                                        
         STCM  RE,15,SBNDATES                                                   
*                                                                               
         LA    R1,SPBYHOOK                                                      
         ST    R1,SBSPHOOK        SPOTBUY HOOK                                  
*                                                                               
         LA    R1,SBLOCK          SET SPLIT BUYS FLAG                           
         AHI   R1,SBEFLAG2-SBLOCK                                               
         OI    0(R1),SBESPLBY     SPLIT BUYS                                    
*                                                                               
BUY6Y    GOTO1 VSPOTBUY,APPARM,(X'A0',SBLOCK)   *** CALL SPOTBUY ****           
         BM    BUYERR             SPTTAB FULL                                   
         DROP  R2                                                               
         L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3                                                       
BUY7     OC    SCNEXT,SCNEXT                                                    
         BZ    BUYX                                                             
*                                                                               
         CLC   =C'UNA',QPRD       IF UNALLOCATED REQUEST                        
         BNE   BUY8               PROCESS ONLY UNALLOCATED CHUNKS               
         CLI   SCPRD1,X'FE'                                                     
         BNE   BUY50                                                            
*                                                                               
BUY8     TM    TWAMODE,TWAMLSM                                                  
         BNO   BUY12                                                            
         CLI   LISTYPE,C'P'       ON LIST REQUESTS                              
         BNE   BUY12                                                            
         CLI   SCPRD1,X'FF'       DON'T SHOW POL                                
         BE    BUY50              LINE                                          
*                                                                               
         CLI   TBPRD1,0           FIRST TIME AROUND?                            
         BNE   BUY9               NO                                            
         MVC   TBPRD1,SCPRD1      YES -SET 1ST BINARY PRODUCT                   
         GOTO1 FDPRD,APPARM,TBPRD1,TEMPRD1                                      
         XC    TEMPRD2,TEMPRD2                                                  
         MVC   TBPRD2,SCPRD2      SET 2ND BINARY PRODUCT                        
         CLI   TBPRD2,0           IF THERE IS ONE -                             
         BE    BUY8X                  ALSO SET ITS                              
         GOTO1 FDPRD,APPARM,TBPRD2,TEMPRD2                                      
BUY8X    MVI   ADDFLG,C'Y'                                                      
         B     BUY12                                                            
*                                                                               
BUY9     CLC   SCPRD1,TBPRD1                                                    
         BNE   BUY10                                                            
         CLC   SCPRD2,TBPRD2                                                    
         BE    BUY12                                                            
BUY10    BAS   RE,TSARCUM                                                       
         MVI   ADDFLG,C'Y'                                                      
         MVC   TBPRD1,SCPRD1      SET 1ST PRDD BINARY VALUE                     
         GOTO1 FDPRD,APPARM,TBPRD1,TEMPRD1                                      
         MVC   TBPRD2,SCPRD2      SET 2ND BINARY PRODUCT                        
         XC    TEMPRD2,TEMPRD2                                                  
         CLI   TBPRD2,0           IF THERE IS ONE -                             
         BE    BUY12                  ALSO SET ITS                              
         GOTO1 FDPRD,APPARM,TBPRD2,TEMPRD2                                      
*                                                                               
BUY12    OC    SCDATE,SCDATE      INDICATES PREVIOUS DATA                       
         BNZ   BUY15                                                            
         TM    INOIND,INOIPRE     DATE LOWER THAN FIRST DATE IN LIST            
         BNO   BUY50                                                            
         MVI   APPARM+12,1        PROCESS IT                                    
         B     BUY30                                                            
*                                                                               
BUY15    CLC   SCDATE,=X'FFFF'    INDICATES POST DATA                           
         BNE   BUY20                                                            
         TM    INOIND,INOIPOS     POST DATA REQUESTED?                          
         BNO   BUY50                                                            
         MVI   APPARM+12,2        PROCESS IT                                    
         B     BUY30                                                            
*                                                                               
BUY20    MVC   APFULL(2),SCDATE   IF WEEKLY                                     
         TM    INOIND,INOIWK      COMPARE WITH SCDATE (ALWAYS MONDAY)           
         BO    BUY22                                                            
         TM    INOIND,INOIBRD     IGNORE PROFILE?                               
         BO    *+12                                                             
         CLI   SVPROF+2,0                                                       
         BNE   BUY22                                                            
         GOTO1 VDATCON,APPARM,(2,SCDATE),(0,APWORK)                             
         GOTO1 VGTBROAD,APPARM,(1,APWORK),APELEM,VGETDAY,VADDAY                 
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,APPARM,(0,APELEM+6),(2,APFULL)                           
BUY22    LA    R2,DTLIST            POINT TO COMPRESSED LIST                    
         XC    APPARM+12(4),APPARM+12                                           
BUY25    CLC   APFULL(2),0(R2)       RIGHT DATE?                                
         BE    BUY30                                                            
         LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   BUY25                                                            
         B     BUY50                                                            
*                                   DATE HIGHER THAN ANY DATE IN LIST           
BUY30    TM    INOIND,INOINET     TAKE NET INSTEAD OF GROSS?                    
         BNO   BUY33                                                            
         ICM   R0,15,SCNET                                                      
         BZ    BUY35                                                            
         CVD   R0,AMT                                                           
         GOTO1 ADDIT,APPARM,AMT,1,(R2)                                          
         B     BUY35                                                            
*                                                                               
BUY33    ICM   R0,15,SCGROSS    AMOUNT                                          
         BZ    BUY35                                                            
         CVD   R0,AMT                                                           
         GOTO1 ADDIT,APPARM,AMT,1,(R2)                                          
*                                                                               
BUY35    TM    INOIND,INOINET                                                   
         BNO   BUY40                                                            
         ICM   R0,15,SCPAYN                                                     
         BZ    BUY50                                                            
         CVD   R0,AMT                                                           
         GOTO1 ADDIT,APPARM,AMT,2,(R2)                                          
         B     BUY50                                                            
*                                                                               
BUY40    ICM   R0,15,SCPAY                                                      
         BZ    BUY50                                                            
         CVD   R0,AMT                                                           
         GOTO1 ADDIT,APPARM,AMT,2,(R2)                                          
*                                                                               
BUY50    L     R3,SCNEXT                                                        
         B     BUY7                                                             
*                                                                               
BUYX     LA    R1,SBLOCK                                                        
         AHI   R1,SBACONT-SBLOCK  ANY BUY CONTINUATION?                         
         OC    0(4,R1),0(R1)                                                    
         BNZ   BUY6Y                NO - GO GET THE REST                        
*                                                                               
         OI    SBIOFLAG,SBNOIO    ALWAYS TELL SPOTIO                            
         B     ROUTSX                                                           
         SPACE 2                                                                
BUYERR   XC    FINMSG,FINMSG                                                    
         MVC   FINMSG(34),=CL34'TOO MUCH DATA FOR ON-LINE ANALYSIS'             
         OI    FINMSGH+6,X'80'                                                  
         MVI   TSTART,C'Y'        START AT BEGINNING OF SCREEN                  
         MVI   TIND,0                                                           
         MVI   TIND2,0                                                          
         NI    FVIIND,FVIVAL                                                    
         L     RD,ACWORKA                                                       
         L     R4,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
*===================================*                                           
* CHKBRK - CALLED BY BUY ROUTINE    *                                           
*          ADDS TSAR RECORDS ON     *                                           
*          LOGICAL BREAKS           *                                           
*===================================*                                           
*                                                                               
CHKBRK   NTR1                                                                   
         L     R2,SBAIO1          A(BUY RECORD)                                 
         USING BUYREC,R2                                                        
         MVI   BILLFST,C'Y'       FOR STATION BILL RECORD                       
         CLI   LISTYPE,C'P'       FOR PRODUCT LISTYPE,C'P'                      
         BNE   CHKBRK2                                                          
         CLI   POLEST,C'Y'        NON - POOL BUY LISTING                        
         BE    YES                                                              
         CLC   SBQBPRD,3(R2)      PRODUCT KEY MUST MATCH CURRENT PROD           
         BNE   NO                 TO AVOID DUPLICATE PIGGY DATA                 
         B     YES                                                              
*                                                                               
CHKBRK2  CLI   LISTYPE,C'E'                                                     
         BNE   CHKBRK8                                                          
         CLI   TBEST,0          FIRST TIME?                                     
         BNE   CHKBRK4                                                          
         MVC   TBEST,BUYKEST                                                    
         MVI   ADDFLG,C'Y'                                                      
         B     YES                                                              
CHKBRK4  CLC   TBEST,BUYKEST                                                    
         BE    YES                                                              
         BAS   RE,TSARCUM         ADD CURRENT ESTIMATE                          
         MVC   TBEST,BUYKEST      MOVE IN NEW ESTIMATE                          
         MVI   ADDFLG,C'Y'                                                      
         B     YES                                                              
*                                                                               
CHKBRK8  XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,CUAALF                                                   
         MVC   STAPCTRY,AGYPRF7                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOM                                                    
         MVC   STAPMKST,BUYMSTA                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   STAPQSTA,C'0'       IF NOT CABLE STATION                         
         BNL   CHKBRK8X                                                         
         CLI   STAPQSTA+4,C' '     SET BAND IF NOT DEFINED                      
         BH    *+10                                                             
         MVC   STAPQSTA+4(1),QMED                                               
         CLI   STAPQSTA+4,C'/'     CANADIAN CABLE IS REALLY NETWORK             
         BNE   *+8                                                              
         MVI   STAPQSTA+4,C'N'                                                  
         B     CHKBRK9                                                          
*                                                                               
CHKBRK8X CLI   STAPQSTA+4,C' '     FOR CABLE                                    
         BH    CHKBRK9                                                          
         MVI   STAPQSTA+4,C'/'     SET SLASH IF NOT DEFINED                     
*                                                                               
CHKBRK9  MVC   APWORK(4),STAPQMKT  SIMULATE MSUNPK OUTPUT                       
         MVC   APWORK+4(5),STAPQSTA                                             
         CLI   CANFLAG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   APWORK+4+5(2),STAPQNET   CABLE SUFFIX TOO FOR CANADA             
         DROP  R1                                                               
*                                                                               
         OC    TEMPSTA,TEMPSTA    CHECK STATION                                 
         BNZ   CHKBRK15                                                         
         MVC   TEMPMKT,APWORK     FIRST TIME ONLY                               
         MVC   TEMPSTA,APWORK+4                                                 
         MVI   ADDFLG,C'Y'                                                      
         CLI   CANFLAG,C'Y'       ARE WE CANADIAN NTRK?                         
         BNE   YES                                                              
         ICM   RF,7,BUYMSTA+2     TO GET TRUE CANADIAN STATION                  
         SRL   RF,5               WE MUST SHIFT OUT LAST 5 BITS                 
         CLI   STAVRSN,C'N'        TEST NEW STAPACK                             
         BNE   *+8                                                              
         SRL   RF,3                SHIFT 3 MORE                                 
         STCM  RF,7,CANSTA                                                      
         B     YES                                                              
*                                                                               
CHKBRK15 CLI   CANFLAG,C'Y'                                                     
         BNE   CHKBRK16                                                         
         SR    RF,RF              YES - MUST SHIFT OUT                          
         ICM   RF,7,BUYMSTA+2     LAST 5 BYTES BEFORE                           
         SRL   RF,5               WE CAN CHECK FOR                              
         CLI   STAVRSN,C'N'        TEST NEW STAPACK                             
         BNE   *+8                                                              
         SRL   RF,3                IF YES, SHIFT 3 MORE BYTES                   
         STCM  RF,7,NEWCSTA       CHANGE IN STATION                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,CANSTA                                                      
*                                                                               
         CR    RE,RF                                                            
         BNE   CHKBRK17                                                         
         CLI   APWORK+4+5,C' '                                                  
         BNH   YES                                                              
         CLC   TEMPMKT,APWORK     CABLE NEEDS TO CHECK MKT TOO                  
         BE    YES                                                              
         B     CHKBRK17                                                         
*                                                                               
CHKBRK16 CLC   TEMPSTA,APWORK+4   CHANGE IN STATION?                            
         BE    YES                NOPE                                          
CHKBRK17 BAS   RE,TSARADD                                                       
         CLI   LISTYPE,C'M'                                                     
         BNE   CHKBRK20                                                         
         CLC   TEMPMKT,APWORK     CHANGE IN MARKET                              
         BE    CHKBRK20                                                         
         BAS   RE,TSARTADD        ADD TOTAL RECORD TO TSAR                      
         BAS   RE,CHKCNT          CHECK COUNT AT MARKET BREAK                   
         BE    CHKBRK20                                                         
         PACK  APDUB,TEMPMKT      SAVE LAST MARKET PROCESSED                    
         CVB   R0,APDUB                                                         
         STCM  R0,3,ENDMKT                                                      
         MVC   NXTMKT,APWORK      AND NEXT MARKET TO BE PROCESSED               
         B     NO                 THAT'S IT FOR NOW                             
*                                                                               
CHKBRK20 MVC   TEMPMKT,APWORK     MOVE IN NEW MARKET AND                        
         MVC   TEMPSTA,APWORK+4   STATION                                       
         MVI   ADDFLG,C'Y'        MARK THIS NEEDS TO BE ADDED                   
         CLI   CANFLAG,C'Y'                                                     
         BNE   YES                                                              
         MVC   CANSTA,NEWCSTA     SET NEW CANADIAN STATION                      
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*==============================================================*                
* CHKCNT - CHECKS IF TSAR RECORDS ADDED CAN FILL UP ONE SCREEN *                
*==============================================================*                
*                                                                               
CHKCNT   NTR1                                                                   
         LLC   R1,RECCNT                                                        
         LA    RE,NLINES                                                        
         CR    R1,RE                                                            
         BNH   YES                                                              
         MVI   SBMODE,SBSTOP                                                    
         L     R1,SBAIO1          SAVE NEW BUY KEY                              
         USING BUYRECD,R1         UP TO AND INCLUDING STATION                   
         LR    RE,R7                                                            
         AH    RE,=Y(SBSVKEY-WORKD)                                             
         MVC   0(BUYKEST-BUYKEY,RE),0(R1)                                       
         CLI   SBQBPRD,0                                                        
         BE    *+10                                                             
         MVC   3(1,RE),SBQBPRD                                                  
         MVI   RECCNT,0                                                         
         B     NO                                                               
         DROP  R1                                                               
         EJECT                                                                  
*===============================================*                               
* SPBYHOOK - SETS SBYORN TO N TO REJECT A SPOT  *                               
*===============================================*                               
*                                                                               
SPBYHOOK NTR1                                                                   
         MVI   SBYORN,C'Y'        DEFAULT TO KEEP SPOT                          
         TM    TWAMODE,TWAMLSM                                                  
         BNO   *+12               ONLY CHECK PRODUCTS FOR DISPLAY MODE          
         CLI   LISTYPE,C'P'                                                     
         BE    SPBYX                                                            
         CLI   SBESPLIT,C'Y'                                                    
         BE    SPBYX              WHERE REQUEST NOT TO SPLIT PIGGYBACKS         
         L     R1,SBAIO1          SAVE NEW BUY KEY                              
         USING BUYRECD,R1         UP TO AND INCLUDING STATION                   
         CLI   BUYKPRD,X'FF'                                                    
         BNE   SPBYX              AND POL ESTIMATES                             
         L     R1,SBASPOT                                                       
         USING REGELEM,R1                                                       
         CLI   BPRD2,0            SECOND PRODUCT?                               
         BNE   SPBY10                                                           
         CLI   RLEN,18                                                          
         BE    SPBYNO             NO - GET BUYS WITH NO 2ND PROD                
         CLC   =C'UNA',QPRD       UNA BUY ELE'S                                 
         BNE   SPBY5                                                            
         CLI   RLEN,10            DON'T HAVE PRODUCT                            
         BNE   SPBYNO                                                           
         B     SPBYX                                                            
SPBY5    CLC   BPRD,RPPRD         NO - FIRST PRODUCTS                           
         BNE   SPBYNO             MUST MATCH                                    
         B     SPBYX              HAVE A MATCH                                  
*                                 SECOND PRODUCT EXISTS (2 POSS)                
*    1ST POSS (REQ. PRD1 = PRD1 AND REQ PRD2 = PRD2)                            
*                                                                               
SPBY10   CLC   BPRD,RPPRD         REQUESTED PRD1 MUST = PRD1                    
         BNE   SPBY15                                                           
         CLC   BPRD2,RPPRD+4      AND REQUESTED PRD2 MUST = PRD2                
         BNE   SPBYNO                                                           
         B     SPBYX              TO HAVE MATCH                                 
*                                                                               
*    2ST POSS (REQ. PRD1 = PRD2 AND REQ PRD2 = PRD1)                            
*                                                                               
SPBY15   CLC   BPRD,RPPRD+4       REQUESTED PRD1 MUST = PRD2                    
         BNE   SPBYNO                                                           
         CLC   BPRD2,RPPRD        AND REQUESTED PRD2 MUST = PRD1                
         BNE   SPBYNO                                                           
         B     SPBYX                                                            
*                                                                               
SPBYNO   MVI   SBYORN,C'N'        THEN REJECT                                   
SPBYX    B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT                                                                  
*======================================*                                        
* ADDIT - WITH PARAMETER LIST AS FOLLOW*                                        
*                                      *                                        
*  0(R1) = AMOUNT IN PL8 FORMAT        *                                        
*  4(R1) = ROW # ( TYPE OF DATA )      *                                        
*  8(R1) = CURRENT DISP. INTO DATE LIST*                                        
* 12(R1) = 1 FOR PREV, 2 FOR POST      *                                        
*======================================*                                        
*                                                                               
ADDIT    NTR1                                                                   
         LA    R2,BLK                                                           
         USING BLKD,R2                                                          
         ICM   RE,15,4(R1)        ROW NUMBER                                    
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    ADDIT3                                                           
         LLC   RF,NUMDTS                                                        
         MHI   RF,6                                                             
         AHI   RF,18               TOTAL,PREV,POST                              
         STH   RF,APHALF          APHALF = LENGTH OF 1 ROW                      
         MH    RE,APHALF          (# OF ROWS)X(L'ROW) TO                        
         AR    R2,RE              POINT TO CORRECT ROW                          
*                                                                               
ADDIT3   L     RE,0(R1)           ADDRESS OF AMOUNT                             
         ZAP   APDUB,0(8,RE)                                                    
         AP    BLKTOT,APDUB       ADD TO TOTAL                                  
*                                                                               
         CLI   12(R1),0           IF ZERO - DON'T WANT PREV OR POST             
         BE    ADDIT10                                                          
         CLI   12(R1),1           PREVIOUS DATA?                                
         BNE   ADDIT5                                                           
         AP    BLKBEF,APDUB        YES                                          
         B     ADDITX                                                           
*                                                                               
ADDIT5   CLI   12(R1),2           POST DATA?                                    
         BE    *+6                                                              
         DC    H'0'                PROBLEM ---                                  
         AP    BLKAFT,APDUB        YES                                          
         B     ADDITX                                                           
*                                                                               
ADDIT10  AH    R2,=Y(BLKCUM-BLKD) 1ST ROW - GET TO MONTHS                       
*                                                                               
         L     R3,8(R1)           POINTS TO CURRENT DATE                        
         LA    R1,DTLIST                                                        
         SR    R3,R1              GET DISPLACEMENT INTO LIST                    
         MHI   R3,3               MULTIPLY BY 3                                 
         AR    R2,R3              GET RIGHT ENTRY IN BLK                        
         AP    0(L'AMOUNTS,R2),APDUB                                            
*                                                                               
ADDITX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*===========================================================*                   
* TSARADD - ADDS TSAR STATION TOTAL (ONLY FOR MKT LIST)     *                   
*===========================================================*                   
*                                                                               
TSARADD  NTR1                                                                   
         LA    R1,BLK                                                           
         BAS   RE,CHKZER          CHECK IF BLOCK IS ZEROS                       
         BNE   TSARAX             YES -DON'T ADD                                
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         XC    BLKLBL,BLKLBL                                                    
         LA    R3,BLKLBL                                                        
         USING TRECD,R3                                                         
         MVC   TSTA,TEMPSTA       MOVE IN KEY                                   
         CLI   LISTYPE,C'S'                                                     
         BE    TSARA8                                                           
         MVC   TMKT,TEMPMKT                                                     
         MVC   TMSTA,TEMPSTA      MOVE IN KEY                                   
TSARA8   ST    R3,TSAREC          A(RECORD)                                     
         MVI   TSACTN,TSAADD      ADD THE REOCRD                                
         GOTO1 VTSAR                                                            
         BE    TSARA12                                                          
         TM    TSERRS,TSEEOF                                                    
         BO    TSARFULL           TSAR BUFFER FULL                              
         DC    H'0'                                                             
TSARA12  MVI   ADDFLG,C'N'        MARK ADDED                                    
         BAS   RE,TOTCUM          ADD TO TOTAL                                  
         CLI   LISTYPE,C'M'                                                     
         BNE   TSARA15                                                          
         LLC   R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECCNT                                                        
TSARA15  LA    R1,BLKLBL          CLEAR BLK                                     
         BAS   RE,LCLRBLK                                                       
TSARAX   B     EXIT                                                             
         DROP  R3                                                               
TSARFULL XC    FINMSG,FINMSG                                                    
         MVC   FINMSG(34),=CL34'TOO MANY MARKETS - USE A START MKT'             
         OI    FINMSGH+6,X'80'                                                  
         XC    FINACT,FINACT       SO WE CAN START ALL OVER AGAIN               
         MVC   FINACT(7),=C'DISPLAY'                                            
         NI    FINACTH+4,X'FF'-X'20'                                            
         OI    FINACTH+6,X'80'                                                  
         XC    FINMOS,FINMOS                                                    
         OI    FINMOSH+6,X'80'                                                  
         MVI   TSTART,C'Y'        START AT BEGINNING OF SCREEN                  
         MVI   TIND,0                                                           
         MVI   TIND2,0                                                          
         NI    FVIIND,FVIVAL                                                    
         L     RD,ACWORKA                                                       
         L     R4,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
*===================================*                                           
* TSARCUM - CUMS/ADDS A TSAR RECORD *                                           
*===================================*                                           
*                                                                               
TSARCUM  NTR1                                                                   
         MVI   RFLAG2,1           LOCAL ENTRY PT                                
         B     TSARC2                                                           
*                                                                               
TSARCM   MVI   RFLAG2,0           GENERAL ENTRY PT                              
TSARC2   LA    R1,BLK                                                           
         BAS   RE,CHKZER          CHECK IF BLOCK IS ZEROS                       
         BNE   TSARCX             YES -DON'T ADD                                
TSARC5   L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    R3,TEMPBLK         READ RECORD INTO A TEMPORARY BLOCK            
         USING TRECD,R3                                                         
         XC    TKEY,TKEY                                                        
         CLI   LISTYPE,C'P'                                                     
         BNE   TSARC10                                                          
         MVI   TPRDIND,C'C'                                                     
         MVC   TPRD1,=C'UNA'                                                    
         CLC   =C'UNA',TEMPRD1                                                  
         BE    TSARC20                                                          
         MVI   TPRDIND,C'A'                                                     
         MVC   TPRD1,TEMPRD1                                                    
         MVC   TPRD2,TEMPRD2                                                    
         BAS   RE,SWPRD           SWITCH PRODUCTS?                              
         BNE   TSARC20            NO                                            
         MVC   TPRD1,TEMPRD2                                                    
         MVC   TPRD2,TEMPRD1                                                    
         B     TSARC20                                                          
*                                                                               
TSARC10  CLI   LISTYPE,C'E'                                                     
         BNE   TSARC15                                                          
         MVI   TESTIND,C'A'                                                     
         MVC   TEST1,TBEST                                                      
         B     TSARC20                                                          
*                                                                               
TSARC15  MVC   TSTA,TEMPSTA    MOVE IN KEY                                      
         CLI   LISTYPE,C'S'                                                     
         BE    TSARC20                                                          
         MVC   TMKT,TEMPMKT                                                     
         MVC   TMSTA,TEMPSTA    MOVE IN KEY                                     
*                                                                               
TSARC20  ST    R3,TSAREC          A(RECORD)                                     
         MVC   SVLBL,0(R3)        SAVE LABEL                                    
         MVI   TSACTN,TSARDH      READ THE RECORD                               
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF                                                    
         BO    TSARC22            RECORD NOT FOUND                              
         TM    TSERRS,TSEEOF                                                    
         BO    TSARC22            END OF FILE                                   
         B     TSARC30            FOUND RECORD - CUM RESULTS                    
*                                                                               
TSARC22  LA    R3,BLKLBL          DOESN'T EXIST - ADD IT                        
         MVC   BLKLBL,SVLBL       RESTORE LABEL                                 
         ST    R3,TSAREC          STORE ADDRESS OF RECORD TO ADD                
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR                                                            
         BE    TSARC45                                                          
         TM    TSERRS,TSEEOF                                                    
         BO    TSARFULL           TSAR BUFFER FULL                              
         DC    H'0'                                                             
*                                                                               
TSARC30  L     R2,TSAREC          ADDRESS OF RECORD                             
         LA    R2,L'BLKLBL(R2)    POINT TO DATA                                 
         LA    R1,BLK             NEW DATA TO ADD                               
         LA    RE,ROWNUMS         # OF ROWS                                     
TSARC35  LLC   RF,NUMDTS          # OF DATES PLUS                               
         AHI   RF,3               TOTAL,PREV,POST                               
TSARC40  AP    0(L'AMOUNTS,R2),0(L'AMOUNTS,R1)                                  
         LA    R1,L'AMOUNTS(R1)                                                 
         LA    R2,L'AMOUNTS(R2)                                                 
         BCT   RF,TSARC40                                                       
         BCT   RE,TSARC35                                                       
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT      WRITE THE RECORD                              
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSARC45  BAS   RE,TOTCUM          ADDS TO BLOCK TOTAL                           
         LA    R1,BLKLBL          CLEARS BLK                                    
         BAS   RE,LCLRBLK                                                       
*                                                                               
TSARCX   CLI   RFLAG2,1                                                         
         BNE   ROUTSX                                                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*===============================*                                               
* TSARTADD - ADDS A TOTAL RECORD*                                               
*===============================*                                               
*                                                                               
TSARTADD NTR1                                                                   
         MVI   RFLAG2,1           LOCAL ENTRY PT                                
         B     TSARTA2                                                          
*                                                                               
TSARTAD  MVI   RFLAG2,0           GENERAL ENTRY PT                              
TSARTA2  LA    R1,TOTBLK                                                        
         BAS   RE,CHKZER          CHECK IF BLOCK IS ZEROS                       
         BNE   TSARTAX             YES -DON'T ADD                               
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    RE,TOTLBL                                                        
         USING TRECD,RE                                                         
         XC    TKEY,TKEY                                                        
         CLI   LISTYPE,C'M'                                                     
         BNE   TSARTA5                                                          
         MVC   TMKT,TEMPMKT                                                     
         MVC   TMSTA,=5C'9'                                                     
         B     TSARTA20                                                         
TSARTA5  CLI   LISTYPE,C'P'                                                     
         BNE   TSARTA10                                                         
         MVI   TPRDIND,C'F'                                                     
         MVC   TPRD1,=C'ZZZ'                                                    
         B     TSARTA20                                                         
TSARTA10 CLI   LISTYPE,C'E'                                                     
         BNE   TSARTA15                                                         
         MVI   TESTIND,C'F'                                                     
         MVI   TEST1,X'FF'                                                      
         B     TSARTA20                                                         
TSARTA15 MVC   TSTA(L'BLKLBL),=9C'9'         ASSUME IT'S STATION                
*                                                                               
TSARTA20 ST    RE,TSAREC                                                        
         MVI   TSACTN,TSAADD       ADD IT                                       
         GOTO1 VTSAR                                                            
         BE    TSARTA25                                                         
         TM    TSERRS,TSEEOF                                                    
         BO    TSARFULL           TSAR BUFFER FULL                              
         DC    H'0'                                                             
TSARTA25 LA    R1,TOTLBL                                                        
         BAS   RE,LCLRBLK                                                       
         CLI   LISTYPE,C'M'                                                     
         BNE   TSARTAX                                                          
         LLC   R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECCNT                                                        
TSARTAX  CLI   RFLAG2,1                                                         
         BNE   ROUTSX                                                           
         B     EXIT                                                             
         EJECT                                                                  
*==================================================*                            
* TSARTCUM - ADDS/CUMS TOTAL REC ONLY FOR MKT LIST *                            
*==================================================*                            
TSARTCUM NTR1                                                                   
         MVI   RFLAG2,1           LOCAL ENTRY PT                                
         B     TSARTC2                                                          
*                                                                               
TSARTCM  MVI   RFLAG2,0           GENERAL ENTRY PT                              
TSARTC2  LA    R1,TOTBLK                                                        
         BAS   RE,CHKZER          CHECK IF BLOCK IS ZEROS                       
         BNE   TSARTCX            YES -DON'T ADD                                
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    RE,TEMPBLK         READ RECORD INTO A TEMPORARY BLOCK            
         USING TRECD,RE                                                         
         MVC   TMKT,TEMPMKT                                                     
         MVC   TMSTA,=5C'9'                                                     
         ST    RE,TSAREC                                                        
         MVI   TSACTN,TSARDH      READ THE RECORD                               
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF                                                    
         BNO   TSARTC30           RECORD EXISTS -CUM IT                         
*                                                                               
         LA    RE,TOTLBL          DOESN'T EXIST - ADD IT                        
         ST    RE,TSAREC          STORE ADDRESS OF RECORD TO ADD                
         MVC   TMKT,TEMPMKT                                                     
         MVC   TMSTA,=5C'9'                                                     
         MVI   TSACTN,TSAADD             DOESN'T EXIST - ADD IT                 
         GOTO1 VTSAR                                                            
         BE    TSARTC50                                                         
         TM    TSERRS,TSEEOF                                                    
         BO    TSARFULL                                                         
         DC    H'0'                                                             
TSARTC30 L     R2,TSAREC          ADDRESS OF RECORD                             
         LA    R2,L'BLKLBL(R2)    POINT TO DATA                                 
         LA    R1,TOTBLK          NEW DATA TO ADD                               
         LA    RE,ROWNUMS         # OF ROWS                                     
TSARTC35 LLC   RF,NUMDTS          # OF DATES PLUS                               
         AHI   RF,3               TOTAL,PREV,POST                               
TSARTC40 AP    0(L'AMOUNTS,R2),0(L'AMOUNTS,R1)                                  
         LA    R1,L'AMOUNTS(R1)                                                 
         LA    R2,L'AMOUNTS(R2)                                                 
         BCT   RF,TSARTC40                                                      
         BCT   RE,TSARTC35                                                      
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT      WRITE THE RECORD                              
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
TSARTC50 LA    R1,TOTLBL                                                        
         BAS   RE,LCLRBLK                                                       
TSARTCX  CLI   RFLAG2,1                                                         
         BNE   ROUTSX                                                           
         B     EXIT                                                             
         EJECT                                                                  
*===================================================*                           
* CHKZERO- CHECKS IF BLK(POINTED TO BY R1) IS ZEROS *                           
*===================================================*                           
*                                                                               
CHKZER   NTR1                                                                   
         LA    RE,ROWNUMS         # OF ROWS                                     
         TM    INOIND2,INOIBIL    CAN'T HAVE BILL OPTION                        
         BO    *+8                                                              
         LA    RE,ROWNUMS-1                                                     
CHKZER2  LLC   RF,NUMDTS          # OF DATES PLUS                               
         AHI   RF,3               TOTAL,PREV,POST                               
CHKZER3  CP    0(L'AMOUNTS,R1),=P'0'                                            
         BNE   YES                YES - ADD IT                                  
         LA    R1,L'AMOUNTS(R1)                                                 
         BCT   RF,CHKZER3                                                       
         BCT   RE,CHKZER2                                                       
         B     NO                  BLOCK IS ZEROS - DON'T ADD IT                
         EJECT                                                                  
*===================================================*                           
* ADDTOT - FOR MKT LIST - ADDS A TOTAL LINE FOR ALL *                           
*===================================================*                           
*                                                                               
ADDTOT   NTR1                                                                   
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    RE,TEMPBLK                                                       
         ST    RE,TSAREC                                                        
         USING TRECD,RE                                                         
         MVC   TMKT,=4X'FF'       CHECK FOR ALL TOTAL LINE                      
         MVC   TMSTA,=5C'9'                                                     
         MVI   TSACTN,TSARDH      READ HIGH INTO TEMPORARY BLOCK                
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF      IF NOT FOUND                                  
         BO    ADDTC50            ADD IT                                        
*                                                                               
         L     R2,TSAREC          ELSE, R2=A(ALL TOTAL RECORD)                  
         LA    R2,L'BLKLBL(R2)    BUMP R2 TO DATA PORTION OF RECORD             
         LA    R1,BLK             NEW DATA TO ADD                               
         LA    RE,ROWNUMS         # OF ROWS                                     
ADDTC35  LLC   RF,NUMDTS          # OF DATES PLUS                               
         AHI   RF,3               TOTAL,PREV,POST                               
ADDTC40  AP    0(L'AMOUNTS,R2),0(L'AMOUNTS,R1)                                  
         LA    R1,L'AMOUNTS(R1)                                                 
         LA    R2,L'AMOUNTS(R2)                                                 
         BCT   RF,ADDTC40                                                       
         BCT   RE,ADDTC35                                                       
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT      WRITE THE RECORD                              
         GOTO1 VTSAR                                                            
         BE    ADDTCX                                                           
         DC    H'0'                                                             
*                                                                               
ADDTC50  LA    RE,BLKLBL          DOESN'T EXIST SO ADD ALL TOTAL REC            
         ST    RE,TSAREC                                                        
         MVC   TMKT,=4X'FF'        SET ALL MARKETS                              
         MVC   TMSTA,=5C'9'        AND ALL STATIONS                             
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
ADDTCX   B     EXIT                                                             
         EJECT                                                                  
*==========================*                                                    
* TOTCUM - ADDS NEW DATA TO*                                                    
*          TOTAL           *                                                    
*==========================*                                                    
*                                                                               
TOTCUM   NTR1                                                                   
         LA    R2,TOTBLK          ADDRESS OF RECORD                             
         LA    R1,BLK             NEW DATA TO ADD                               
         LA    RE,ROWNUMS         # OF ROWS                                     
TOTC3    LLC   RF,NUMDTS          # OF DATES                                    
         AHI   RF,3               PLUS TOTAL,PREV,POST                          
TOTC4    AP    0(L'AMOUNTS,R2),0(L'AMOUNTS,R1)                                  
         LA    R1,L'AMOUNTS(R1)                                                 
         LA    R2,L'AMOUNTS(R2)                                                 
         BCT   RF,TOTC4                                                         
         BCT   RE,TOTC3                                                         
         CLI   LISTYPE,C'M'                                                     
         BNE   EXIT                                                             
         BAS   RE,ADDTOT                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*==============================*                                                
* SWPRD - CHECKS IF PRODUCTS   *                                                
*         ARE IN ALPHA ORDER   *                                                
*==============================*                                                
*                                                                               
SWPRD    NTR1                                                                   
         LA    RE,3                                                             
         LA    R1,TEMPRD1                                                       
         LA    R2,TEMPRD2                                                       
         OC    TEMPRD2,TEMPRD2    IF NO SECOND PRODUCT                          
         BZ    NO                 NO NEED TO SWITCH                             
SWPRD5   CLC   0(1,R1),0(R2)                                                    
         BL    NO                                                               
         BH    YES                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   RE,SWPRD5                                                        
         B     YES                 SWITCH                                       
         EJECT                                                                  
*================================*                                              
* FINDPRD - FINDS 3 CHAR PRODUCT *                                              
* GIVEN PRODUCT CODE             *                                              
*        0(R1) - BINARY CODE     *                                              
*        4(R1) - AREA FOR PRODUCT*                                              
*================================*                                              
*                                                                               
FDPRD    NTR1                                                                   
         L     RE,0(R1)           BINARY PRODUCT                                
         L     RF,4(R1)           ADDRESS FOR RETURN 3 CHAR PRODUCT             
         CLI   0(RE),X'FE'                                                      
         BNE   FDP2                                                             
         MVC   0(3,RF),=C'UNA'                                                  
         B     FDPX                                                             
*                                                                               
FDP2     LA    R1,PRDLIST                                                       
FDP5     OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   3(1,R1),0(RE)                                                    
         BE    FDP10                                                            
         LA    R1,4(R1)                                                         
         B     FDP5                                                             
*                                                                               
FDP10    MVC   0(3,RF),0(R1)                                                    
FDPX     B     EXIT                                                             
         EJECT                                                                  
*===================================*                                           
* CHKPOL - CHECKS IF POL ESTIMATE   *                                           
*     OPENED SETS POLEST=Y IF IT IS *                                           
*===================================*                                           
*                                                                               
CHKPOL   NTR1                                                                   
         MVI   POLEST,C'N'         NO POL ESTIMATE                              
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,TEMPEST                                                  
         LA    R3,7                KEY COMPARE INCLUDING EST #                  
         CLI   TEMPEST,0                                                        
         BNE   CHKPOL5                                                          
         MVI   EKEYEST,1           READ HIGH FOR ANY ESTIMATES                  
         LA    R3,6                COMPARE EXCLUDING ESTIMATE #                 
CHKPOL5  GOTO1 AIO,DIRHI                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   CHKPOLX            MEDIA,CLIENT,PRD MUST MATCH                   
         CLI   TEMPEST,0                                                        
         BNE   CHKPOLY            IF EST GIVEN - OKAY                           
         CLI   EKEYEST,0          IF NO EST GIVEN - ANY ESTIMATE OKAY           
         BE    CHKPOLX                                                          
*                                                                               
CHKPOLY  MVI   POLEST,C'Y'         YES - THERE IS A POL ESTIMATE                
CHKPOLX  MVC   FVMSGNO,=X'FFFF'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*===================================*                                           
* CHKPRD - VALIDATES A PRODUCT      *                                           
*      NTRY - TEMPPRD = PRODUCT     *                                           
*             RBYTE = QPRD OR QPRD2 *                                           
*===================================*                                           
*                                                                               
CHKPRD   NTR1                                                                   
         LA    R2,IOKEY            BUILD KEY OF PRODUCT RECORD                  
         USING PRDHDRD,R2                                                       
         XC    PKEY,PKEY                                                        
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,TEMPPRD                                                  
         LA    R2,RIO                                                           
         ST    R2,IOADDR           I/O AREA                                     
         GOTO1 AIO,FILRD           GET PRODUCT RECORD                           
         BNE   NO                                                               
         CLI   RBYTE,1                                                          
         BNE   CHKPRD10                                                         
         MVC   PRDNM,PNAME         SET PRODUCT VALUES                           
         MVC   BPRD,PCODE+1                                                     
         MVC   QPRD,TEMPPRD                                                     
         B     YES                                                              
*                                                                               
CHKPRD10 MVC   QPRD2,TEMPPRD      SET PRODUCT VALUES FOR PROD2                  
         MVC   BPRD2,PCODE+1                                                    
         B     YES                                                              
         EJECT                                                                  
*===================================*                                           
* CHKEST - VALIDATES AN ESTIMATE #  *                                           
*     NTRY -  TEMPEST = EST #       *                                           
*             RYBTE = QEST1 OR QEST2*                                           
*===================================*                                           
CHKEST   NTR1                                                                   
         MVC   APFULL(L'QPRD),QPRD     TEMPORARY                                
         CLC   =C'ALL',QPRD       CAN'T READ ESTIMATE RECORD                    
         BE    CHKEST2                                                          
         CLC   =C'UNA',QPRD       CAN'T READ ESTIMATE RECORD                    
         BNE   CHKEST1                                                          
         MVC   APFULL(L'QPRD),=C'POL'  USE POL TO CHECK                         
         B     CHKEST5                                                          
CHKEST1  OC    QPRD,QPRD          CAN'T READ ( LISTING PRODUCTS )               
         BNZ   CHKEST5                                                          
CHKEST2  CLI   RBYTE,1                                                          
         BNE   CHKEST10                                                         
         B     CHKEST8                                                          
CHKEST5  LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,APFULL                                                   
         MVC   EKEYEST,TEMPEST                                                  
         LH    R2,=Y(ESTREC-WORKD)  IO AREA WHICH WE PASS                       
         LA    R2,WORKD(R2)                                                     
         ST    R2,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,DIRHI                                                        
         CLC   IOKEY(L'EKEY),IOKEYSAV                                           
         BNE   NO                                                               
         GOTO1 AIO,FILGET                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RBYTE,1                                                          
         BNE   CHKEST10                                                         
         MVC   ESTNM,EDESC        SET EST DETAILS                               
         MVC   ESTST,ESTART                                                     
         MVC   ESTND,EEND                                                       
CHKEST8  MVC   BEST,TEMPEST                                                     
         LLC   R1,TEMPEST                                                       
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  QEST,RDUB                                                        
         MVC   ESTOWSDY,EOWSDAY                                                 
         CLI   ESTOWSDY,1                                                       
         BNE   *+8                                                              
         MVI   ESTOWSDY,0                                                       
         B     YES                                                              
*                                                                               
CHKEST10 MVC   BEST2,TEMPEST                                                    
         LLC   R1,TEMPEST                                                       
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  QEST2,RDUB                                                       
         B     YES                                                              
         EJECT                                                                  
*===================================*                                           
* ROUTINE TO VALIDATE A MARKET CODE *                                           
* (CALLED FROM VALMOS) RBYTE SET    *                                           
* TO WHICH HALF FIELD OF SCANNER BLK*                                           
*===================================*                                           
*                                                                               
CHKMKT   NTR1                                                                   
*                                                                               
         BAS   RE,RDMKT            READ MARKET RECORD                           
*                                                                               
         CLI   MOSFLG+1,C'L'       ARE WE LIST?                                 
         BE    YES                                                              
*              *** THIS IS WRONG - SHOULD BE MOVING RIO TO SBKMTREC             
*              *** BUT NO HARM DONE - SPOTIO WILL READ IT                       
**NO-OP**LH    R2,=Y(SBMKTREC-WORKD)                                            
**NO-OP**LA    R2,WORKD(R2)       NO- READ MKT RECORD                           
**NO-OP**ST    R2,IOADDR          INTO SPOTIOS AREA                             
         B     YES                                                              
         EJECT                                                                  
*===================================*                                           
* ROUTINE TO VALIDATE A STATION CALL*                                           
* (CALLED FROM VALMOS) RBYTE SET TO *                                           
* WHICH HALF OF FIELD OF SCANNER BLK*                                           
*===================================*                                           
*                                                                               
CHKSTA   NTR1                                                                   
         LA    R3,RIO                                                           
         USING SSTABLKD,R3                                                      
         XC    0(SSTBLNQ,R3),0(R3)                                              
         LA    R1,NEWSTA                                                        
         ST    R1,SSTBADDR                                                      
         MVI   SSTBADDR,X'80'      INDICATE STRING INPUT                        
         MVC   SSTBMED,QMED                                                     
         MVI   SSTBCTRY,C'U'       US                                           
         CLI   AGYPRF7,C'C'        IF CANDIAN                                   
         BNE   *+8                                                              
         MVI   SSTBCTRY,C'C'       SET IT                                       
         MVC   SSTBACOM,ACOM                                                    
         GOTO1 VSTAVAL,RPARM,SSTABLKD                                           
         CLI   SSTBERR,0                                                        
         BNE   ERRSTA                                                           
         MVC   RDUB,SSTBSTA                                                     
         MVC   QSTACNET,SSTBNET   SET CABLE FOR NETWORK STATIONS                
*                                                                               
         MVI   CANMKT0,C'N'       DEFAULT (DOLLARS ARE IN PROPER MKTS)          
         CLI   AGYPRF7,C'C'       IF CANADIAN                                   
         BNE   CHKSTA20                                                         
         CLI   QMED,C'N'          AND NTWK                                      
         BE    *+12                                                             
         CLI   QMED,C'C'          OR COMBINED?                                  
         BNE   CHKSTA20           NO - GO READ STATION MASTER RECORD            
*                                                                               
         LA    R1,RDUB            PT TO POSSIBLE NETWORK                        
         BAS   RE,RDNETD          CHECK FOR NET DEF                             
         BE    CHKSTA10                                                         
         CLI   QSTACNET,C' '      NONE                                          
         BH    ERRSTA             CANNOT HAVE CABLE SUFFIX /XX SET              
         B     CHKSTA20           MUST BE EXPLODED STA                          
*                                 YES, BUT COULD BE EXPLODED CABLE STA          
CHKSTA10 CLI   NTWKBITS,1         TEST STD NETDEF                               
         BNE   CHKSTA12           YES                                           
         CLI   QSTACNET,C' '      TEST LOCAL CABLE WANTED                       
         BH    CHKSTA15           YES - EXPLODED CABLE STA                      
         B     CHKSTA13                                                         
*                                                                               
CHKSTA12 CLI   QSTACNET,C' '                                                    
         BH    ERRSTA             STD NETDEF - CANNOT HAVE /XX                  
*                                                                               
CHKSTA13 XC    BSTA,BSTA          DOLLARS ARE IN MKT =0000                      
         MVC   QSTA,RDUB                                                        
         XC    QMKT,QMKT                                                        
         XC    BMKT,BMKT                                                        
         MVI   CANMKT0,C'Y'       READ DOLLARS FROM MKT=0000                    
         B     YES                SET GOOD CC CODE                              
*                                                                               
CHKSTA15 LA    RF,RIO             EXPLODED CANADIAN CABLE                       
         LA    R1,NDEFEL-NDEFRECD(RF)  SET MKT FROM REC READ BY RDNETD          
CHKSTA16 CLI   0(R1),1                                                          
         BNE   *+14                                                             
         CLC   QSTACNET(2),NDEFMSUF-NDEFEL01(R1)   MATCH MARKET                 
         BE    CHKSTA17                                                         
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   CHKSTA16                                                         
         B     ERRSTA              SUFFIX NOT DEFINED IN CBLDEF                 
*                                                                               
CHKSTA17 LH    RF,NDEFMNUM-NDEFEL01(R1)                                         
         CVD   RF,RDUB2            RESET MARKET CODE IN RWORK                   
         OI    RDUB2+7,X'0F'                                                    
         UNPK  RWORK(L'QMKT),RDUB2                                              
         B     CHKSTA22                                                         
*                                                                               
CHKSTA20 LA    R2,IOKEY            READ THE STATION MASTER RECORD               
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,RDUB                                                    
         CLI   STAKCALL+4,C' '                                                  
         BNE   *+10               DON'T OVERRIDE LAST BYTE                      
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKAGY,CUAALF                                                   
         MVC   STAKCLT,QCLT        TRY FOR  CLIENT SPECIFIC                     
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   ERRSTA              STATION RECORD NOT FOUND                     
*                                                                               
         LA    R2,RIO                                                           
         MVC   RFULL,SCANNTWK      SAVE NETWORK                                 
         MVC   RWORK(L'QMKT),SMKT  EXTRACT MARKET                               
         OC    INOMKT,INOMKT                IF OVERRIDE MARKET,                 
         BZ    *+10                                                             
         MVC   RWORK(L'INOMKT),INOMKT       USE OVERRIDE MARKET                 
*                                                                               
CHKSTA22 XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,CUAALF                                                   
         MVC   STAPCTRY,AGYPRF7                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOM                                                    
         MVC   STAPQMKT,RWORK                                                   
         MVC   STAPQSTA(8),RDUB                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BNE   ERRSTA                                                           
*                                                                               
         MVC   QMKT,STAPQMKT                                                    
         MVC   BMKT,STAPMKT                                                     
         MVC   QSTA,STAPQSTA                                                    
         CLI   QSTA,C'0'           IF CABLE STATION                             
         BL    *+8                                                              
         MVI   QSTA+4,C'/'         SET SLASH FOR BAND                           
         MVC   BSTA,STAPSTA                                                     
         DROP  R1                                                               
*                                                                               
         OC    INOMKT,INOMKT      IF NO OVERRIDE MARKET                         
         BNZ   CHKSTA25                                                         
         CLI   CUACCS,C'+'        AND MARKET LIMIT ACCESS                       
         BE    CHKSTA25                                                         
         CLI   CUACCS+2,C'+'                                                    
         BNE   CHKSTA30                                                         
CHKSTA25 MVC   TEMPMKT(2),BMKT    CHECK STATION IN MARKET                       
         BAS   RE,RDMKT           ALLOWED ACCESS                                
         MVC   MKTNM,RSPACES                                                    
*                                                                               
CHKSTA30 CLI   NTWKBITS,1                                                       
         BNE   CHKSTA32                                                         
         MVI   NTWKBITS,0                                                       
         B     YES                                                              
*                                                                               
CHKSTA32 LA    R1,INOAFF          REQUESTED NETWORK                             
         OC    INOAFF,INOAFF      OPTION TO OVERIDE NETWORK                     
         BZ    YES                YES - USE THAT ONE                            
         BAS   RE,RDNETD          SET NETWORK BITS                              
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              READ MARKET RECORD                                               
         SPACE                                                                  
RDMKT    NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,TEMPMKT                                                     
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK(L'QMKT),RDUB    GET MARKET FOR NEW BIN MARKET              
         LA    R2,IOKEY              YES - BUILD NEW MARKET KEY                 
         USING MKTRECD,R2                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,RWORK                                                    
         MVC   MKTKAGY,CUAALF                                                   
         LA    R2,RIO              SET IOADDR FOR LIST                          
         ST    R2,IOADDR                                                        
*                                                                               
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   ERRMKT                                                           
*                                                                               
         MVC   QMKTACCS,MKTLTACC   SAVE LIMIT ACCESS CODES                      
*                                                                               
         MVC   MKTNM,MKTNAME      SET MARKET VALUES                             
         MVC   QMKT,RWORK                                                       
         MVC   BMKT,TEMPMKT                                                     
*                                                                               
         OC    QMKTACCS,QMKTACCS                                                
         BZ    EXIT                                                             
         ICM   R0,7,QCLTACCS       SAVE CURRENT CLIENT LIMIT ACCESS             
         MVI   QCLTACCS,X'FF'      SUPPRESS CLIENT SECURITY                     
         BRAS  RE,CALLOFCR                                                      
         BNZ   ERRSECLK                                                         
         STCM  R0,7,QCLTACCS       RESTORE VALUES                               
         B     EXIT                                                             
         EJECT                                                                  
*=======================================*                                       
* RDNETD - READS FOR A NET DEF RECORD   *                                       
*         ON INPUT -- R1 PTS TO NTWK    *                                       
*         ON OUTPUT - NTWKBITS SET      *                                       
*=======================================*                                       
*                                                                               
RDNETD   NTR1                                                                   
         MVI   NTWKBITS,0                                                       
         CLI   AGYPRF7,C'C'       IF CANADIAN                                   
         BNE   EXIT                                                             
         CLI   QMED,C'N'          AND IF NETWORK OR COMBINED                    
         BE    *+12                                                             
         CLI   QMED,C'C'                                                        
         BNE   EXIT                                                             
*                                                                               
         XC    IOKEY,IOKEY        YES - BUILD NET DEF KEY                       
         MVC   IOKEY(2),=X'0D11'                                                
         MVC   IOKEY+2(2),CUAALF                                                
         MVC   IOKEY+4(4),0(R1)   NETWORK                                       
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(8),IOKEYSAV  NETDEF RECORD EXISTS?                         
         BNE   NO                 NO  - MUST BE EXPLODED STA                    
*                                                                               
         CLI   QSTACNET,C' '       TEST FOR CABLE NETWORK                       
         BNH   RDNETD1                                                          
* TRY FOR CLIENT EXCEPTION REC                                                  
         MVC   RWORK(20),IOKEY     SAVE DEFAULT KEY                             
         MVC   RWORK+20(L'IODA),IODA    AND ADDR                                
         MVC   IOKEY+8(2),BCLT                                                  
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    RDNETD1                                                          
         MVC   IOKEY,RWORK         RESTORE KEY                                  
         MVC   IODA,RWORK+20       AND ADDR                                     
*                                                                               
RDNETD1  GOTO1 AIO,IOSPTFIL+IOGET                                               
         USING NDEFRECD,R2                                                      
         LA    R1,NDEFEL                                                        
RDNETD5  CLI   0(R1),2                                                          
         BNE   *+14                                                             
         MVC   NTWKBITS,2(R1)                                                   
         B     YES                                                              
*                                                                               
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   RDNETD5                                                          
         B     YES                NO NETWORK BITS -                             
         EJECT                                                                  
*===================================*                                           
* ROUTINE TO CHECK IF DATES ARE WITH*                                           
* IN 12 MONTHS (CALLED FROM VALKEY) *                                           
*===================================*                                           
*                                                                               
YEAR     NTR1                                                                   
         CLC   QSTDT,QNDDT        START > END                                   
         BH    YEAR9              INVALID                                       
         CLC   QSTDT(2),QNDDT     SAME YEARS - ALWAYS OK                        
         BE    YES                                                              
         GOTO1 VDATCON,APPARM,(0,QSTDT),(3,RDUB)                                
         GOTO1 (RF),(R1),(0,QNDDT),(3,RDUB+3)                                   
         SR    R0,R0                                                            
         IC    R0,RDUB                                                          
         SR    R1,R1                                                            
         IC    R1,RDUB+3                                                        
         SR    R1,R0                                                            
         CHI   R1,1                                                             
         BNE   YEAR10             CAN'T EXCEED ONE YEAR                         
         CLC   RDUB+1(1),RDUB+4   CHECK MONTH                                   
         BNH   YEAR10                                                           
         B     YES                                                              
*                                                                               
YEAR9    MVC   FVMSGNO,=AL2(FVSTENDT)  START>END                                
         B     NO                                                               
YEAR10   MVC   FVMSGNO,=AL2(FVIDAT)    INVALID DATE                             
         B     NO                                                               
         EJECT                                                                  
*===================================*                                           
* ROUTINE TO BUILD LIST FROM REQUEST*                                           
* DATES (CALLED FROM VALKEY)        *                                           
*===================================*                                           
*                                                                               
BLDLST   NTR1                                                                   
         MVC   TEMPPROF,SVPROF                                                  
         MVC   RWORK4(6),QSTDT                                                  
         MVC   RWORK4+6(6),QNDDT                                                
         TM    INOIND,INOIWK                                                    
         BNO   BLDLST1                                                          
         MVI   DATEFORM,X'04'     FORCE FOR WEEKLY                              
         TM    INOIND,INOIBRD                                                   
         BNO   BLDLST4                                                          
         MVC   TEMPPROF+6(3),=X'010101'                                         
         B     BLDLST4                                                          
*                                                                               
BLDLST1  MVC   RDUB,QSTDT                                                       
         CLC   RDUB+4(2),=C'00'                                                 
         BNE   *+10                                                             
         MVC   RDUB+4(2),=C'01'                                                 
         GOTO1 VGTBROAD,RPARM,(1,RDUB),RWORK,VGETDAY,VADDAY                     
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   RWORK4(6),RWORK                                                  
*                                                                               
         MVC   RDUB,QNDDT                                                       
         CLC   RDUB+4(2),=C'00'                                                 
         BNE   *+10                                                             
         MVC   RDUB+4(2),=C'15'                                                 
         GOTO1 VGTBROAD,RPARM,(1,RDUB),RWORK,VGETDAY,VADDAY                     
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   RWORK4+6(6),RWORK+6                                              
         TM    INOIND,INOIBRD                                                   
         BNO   BLDLST2                                                          
         MVI   DATEFORM,0            IGNORE PROFILE                             
         MVC   TEMPPROF+6(3),=X'010101'                                         
         B     BLDLST4                                                          
BLDLST2  TM    INOIND2,INOIB3     USE B3 CALANDER                               
         BNO   BLDLST3                                                          
         MVC   DATEFORM+2,SVPROFB3+2                                            
         NI    DATEFORM,X'0F'                                                   
         MVC   TEMPPROF+6(3),SVPROFB3+6                                         
         B     BLDLST4                                                          
BLDLST3  MVC   DATEFORM,SVPROF+2                                                
         NI    DATEFORM,X'0F'                                                   
*                                                                               
BLDLST4  XC    APWORK,APWORK                                                    
         MVC   APWORK+0(4),VGTBROAD                                             
         MVC   APWORK+4(4),VADDAY                                               
         MVC   APWORK+8(4),VGETDAY                                              
         MVC   APWORK+12(4),VDATCON                                             
*                                                                               
         XC    RPARM(24),RPARM                                                  
         XC    DTSCP,DTSCP                                                      
         LA    R2,DTSCP                                                         
         LA    RE,RWORK4                                                        
         ST    RE,RPARM                                                         
         MVI   RPARM,13           MAX TWELVE MONTHS                             
         TM    INOIND,INOIWK                                                    
         BNO   BLDLST4X                                                         
         MVI   RPARM,53           MAX 53 WEEKS                                  
         TM    INOIND,INOIBRD                                                   
         BO    BLDLST4X           DON'T RESET IT                                
         CLI   ESTOWSDY,0         OUT- OF WEEK ROTATOR?                         
         BE    BLDLST4X           NO                                            
         MVC   TEMPPROF+8(1),ESTOWSDY                                           
BLDLST4X GOTO1 VMOBILE,RPARM,,(DATEFORM,(R2)),APWORK,TEMPPROF                   
         CLI   0(R2),X'FF'                                                      
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     *-12                                                             
         MVI   0(R2),0            MARK END OF LIST WITH ZERO                    
*                                                                               
         LA    R0,13              MAX 13 MONTHS                                 
         TM    INOIND,INOIWK                                                    
         BNO   *+8                                                              
         LA    R0,53              MAX 53 WEEKS                                  
         XC    DTLIST,DTLIST                                                    
         LA    R3,DTLIST                                                        
         LA    R2,DTSCP                                                         
BLDLST5  CLI   0(R2),0                                                          
         BE    BLDLST8            GET YM FROM END DATE                          
         MVC   0(2,R3),0(R2)      TAKE COMPRESSED START FOR                     
         TM    INOIND,INOIWK      WEEKLY DATA                                   
         BO    BLDLST6                                                          
         TM    INOIND,INOIBRD     IGNORE PROFILE                                
         BO    *+12                                                             
         CLI   SVPROF+2,0                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),2(R2)      TAKE END COMPRESSED                           
BLDLST6  LA    R3,2(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,BLDLST5                                                       
*                                                                               
BLDLST8  LA    R1,13                                                            
         TM    INOIND,INOIWK      WEEKLY DATA                                   
         BNO   *+8                                                              
         LA    R1,53                                                            
         SR    R1,R0                                                            
         STC   R1,NUMDTS          # OF DATES                                    
BLDLSTX  B     EXIT                                                             
         EJECT                                                                  
*==================================*                                            
* ENTRY POINT FOR OPTION ROUTINES  *                                            
*==================================*                                            
*                                                                               
OBASE    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R4,ACBASE4                                                       
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+OBA'                                                 
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
         SPACE 2                                                                
OBRANCH  B     VALREP                                                           
         B     VALNET                                                           
         B     VALMKT                                                           
         B     VALTRD                                                           
OPTMAX   EQU   *-OBRANCH                                                        
         EJECT                                                                  
*=========================================*                                     
* VALIDATE REP (SPECIAL REP CODE )        *                                     
* FORMAT:  REP=XXXX  WHERE X=REP CODE     *                                     
*=========================================*                                     
*                                                                               
VALREP   L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         MVC   ACPARM+4(4),=X'D9000ABC'                                         
         GOTO1 (RF),ACPARM,0             GET A(RCPACK)                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         OC    FVIFLD,=C'   '                                                   
         GOTO1 (RF),ACPARM,(C'P',FVIFLD),SCWORK                                 
         BZ    VALREP5                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREPX                                                          
*                                                                               
VALREP5  OC    SCWORK(2),SCWORK                                                 
         BNZ   VALREPX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALREPX  B     EXIT                                                             
         SPACE 2                                                                
*=========================================*                                     
* VALIDATE NTWK - OVERRIDE                *                                     
* FORMAT:  NET=XXXX  WHERE X=NTWK AFFILIATE                                     
*=========================================*                                     
*                                                                               
VALNET   MVC   SCWORK(L'INOAFF),FVIFLD                                          
         B     EXIT                                                             
         SPACE 2                                                                
*=========================================*                                     
* VALIDATE TRD -DON'T COMBINE CASH & TRADE*                                     
* FORMAT:  TRD=N                          *                                     
*=========================================*                                     
*                                                                               
VALTRD   CLI   FVIFLD,C'Y'         IF TRD=Y, THIS IS THE DEFAULT                
         BE    VALTRDX                                                          
         CLI   FVIFLD,C'N'         IF TRD=N                                     
         BNE   *+12                                                             
         MVI   SCWORK,INOITRD      SET TO NOT COMBINE CASH AND TRADE            
         B     VALTRDX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV) ELSE ERROR                                 
VALTRDX  B     EXIT                                                             
         EJECT                                                                  
*=========================================*                                     
* VALIDATE MARKET - OVERRIDE              *                                     
* FORMAT:  MKT=NNNN                       *                                     
*=========================================*                                     
*                                                                               
VALMKT   CLC   =C'ALL',FINMOS      CANNOT COMBINE WITH "ALL" MARKETS,           
         BE    ERRMKT                                                           
         CLC   =C'LM',FINMOS       LIST MARKETS, AND                            
         BE    ERRMKT                                                           
         CLC   =C'LS',FINMOS       LIST STATIONS                                
         BE    ERRMKT                                                           
         TM    FVIIND,FVINUM       MARKET IS VALID NUMERIC?                     
         BNO   ERRMKT                                                           
         OC    SCWORK(L'INOMKT),=C'0000'                                        
         LA    RE,4                                                             
         LLC   R1,FVILEN                                                        
         SR    RE,R1               4 - LENGTH OF INPUT                          
         LA    R0,SCWORK                                                        
         AR    RE,R0               ADDRESS TO MOVE MARKET                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD                                                   
         B     EXIT                                                             
         EJECT                                                                  
*              ERROR EXISTS                                                     
         SPACE                                                                  
ERRMED   MVC   FVMSGNO,=AL2(FVIMED)    SET ERROR NUMBER                         
         B     ROUTSX                                                           
         SPACE                                                                  
ERRCLT   MVC   FVMSGNO,=AL2(FVICLI)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRSECLK MVC   FVMSGNO,=AL2(FVSECLOK)                                           
         B     ROUTSX                                                           
         SPACE                                                                  
ERRPRD   XC    QPRD,QPRD                                                        
         LA    R1,FINPRDH      SET CURSOR TO PRODUCT FIELD                      
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVIPRD)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERREST   MVC   FVMSGNO,=AL2(FVIEST)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRDATE  MVC   FVMSGNO,=AL2(FVIDAT)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRMKT   OC    INOMKT,INOMKT   IF OVERRIDE MARKET,                              
         BZ    *+12                                                             
         LA    R1,FINOPTH      SET CURSOR TO OPTION FIELD                       
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVIMKT)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRSTA   MVC   FVMSGNO,=AL2(FVISTA)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRNOTA  MVC   FVMSGNO,=AL2(FVNOTALL)                                           
         B     ROUTSX                                                           
         SPACE                                                                  
ERRPIG   MVC   FVMSGNO,=AL2(FVNOPIG) PIGGY-BACK PAIR NOT ALLOWED                
         B     ROUTSX                                                           
         SPACE                                                                  
ERRREP   MVC   FVMSGNO,=AL2(FVNOSREP) SPECIAL REP NOT ALLOWED                   
         B     ROUTSX                                                           
         SPACE                                                                  
ERRMEDC  MVC   FVMSGNO,=AL2(FVMEDC)                                             
         B     ROUTSX                                                           
         SPACE                                                                  
ERRMEDC2 LA    R1,FINMEDH                                                       
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVMEDC2)                                            
         B     ROUTSX                                                           
         SPACE                                                                  
ERRNOBIL LA    R1,FINPRDH                                                       
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVNOBIL)                                            
         B     ROUTSX                                                           
         SPACE                                                                  
ERRNOBL2 LA    R1,FINOPTH                                                       
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVNOBIL2)                                           
         B     ROUTSX                                                           
         SPACE                                                                  
ERRNOAFF MVC   FVMSGNO,=AL2(FVNOAFF)                                            
         B     ROUTSX                                                           
         EJECT                                                                  
*              LITERAL POOL                                                     
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
         EJECT                                                                  
*===================*                                                           
* OTHER TABLES      *                                                           
*===================*                                                           
*                                                                               
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSPDEMUP)                                                    
         DC    AL1(QGETDEM2)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QUNDAY)                                                      
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QXSORT)                                                      
         DC    AL1(QEDITOR)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QSPACNVL)                                                    
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QRANSID)                                                     
         DC    AL1(QMSPACK)                                                     
         DC    AL1(QMSUNPK)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSPOTBUY)                                                    
         DC    AL1(QSPOTIO)                                                     
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QSTAPACK)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*=============================================================                  
* CALL NEW OFFICER                                                              
*=============================================================                  
                                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RWORK                                                         
         XC    RWORK,RWORK                                                      
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,CUACCS      NOTE MOVES 2 BYTES                           
         MVC   OFCAGY,CUAALF                                                    
         MVC   OFCOFC,QOFFICE                                                   
         MVC   OFCCLT,QCLT         UNPACKED CLIENT                              
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT,CUACCS       NOTE MOVES 4 BYTES                           
         MVC   OFCACCSC(3),QCLTACCS                                             
         MVC   OFCACCSM(3),QMKTACCS                                             
         LHI   RE,SECBLK-WORKD                                                  
         LA    RE,WORKD(RE)                                                     
         ST    RE,OFCSECD                                                       
         DROP  R1                                                               
         GOTO1 VOFFICER,RPARM,(C'N',RWORK),ACOM                                 
         CLI   0(R1),0                                                          
         JNE   NO                  EXIT WITH CC NEQ                             
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* SETCNCBL - APPENDS CANADIAN CABLE SUFFIX TO NEWSTA  *                         
*            CALLED FROM GETMOS                       *                         
*         ON INPUT -- RDUB=MKT, NEWSTA=STATION        *                         
*         ON OUTPUT - NEWSTA SET FOR CABLE STATION    *                         
*=====================================================*                         
*                                                                               
SETCNCBL NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY        BUILD NET DEF KEY                             
         MVC   IOKEY(2),=X'0D11'                                                
         MVC   IOKEY+2(2),CUAALF                                                
         MVC   IOKEY+4(4),NEWSTA  NETWORK                                       
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(8),IOKEYSAV  NETDEF RECORD EXISTS?                         
         BNE   SETCNCX            NO!                                           
* TRY FOR CLIENT EXCEPTION REC                                                  
         MVC   RWORK(20),IOKEY     SAVE DEFAULT KEY                             
         MVC   RWORK+20(L'IODA),IODA    AND ADDR                                
         MVC   IOKEY+8(2),BCLT                                                  
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    SETCNC10                                                         
         MVC   IOKEY,RWORK         RESTORE KEY                                  
         MVC   IODA,RWORK+20       AND ADDR                                     
*                                                                               
SETCNC10 GOTO1 AIO,IOSPTFIL+IOGET                                               
         USING NDEFRECD,R2                                                      
         LA    R1,NDEFEL                                                        
         SR    R0,R0                                                            
SETCNC15 CLI   0(R1),2                                                          
         BE    SETCNC20                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   SETCNC15                                                         
         B     SETCNCX            NO NETWORK BITS                               
*                                                                               
SETCNC20 CLI   2(R1),1            TEST FOR CABLE NETDEF RECORD                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NDEFEL                                                        
         SR    R0,R0                                                            
SETCNC30 CLI   0(R1),1                                                          
         BNE   *+14                                                             
         CLC   RDUB(2),NDEFMNUM-NDEFEL01(R1)   MATCH MARKET NUMBER              
         BE    SETCNC40                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   SETCNC30                                                         
         B     SETCNCX             SUFFIX NOT DEFINED IN CBLMKT                 
SETCNC40 MVI   NEWSTA+4,C'/'                                                    
         MVC   NEWSTA+5(2),NDEFMSUF-NDEFEL01(R1)                                
SETCNCX  XIT1                                                                   
         LTORG                                                                  
* SPFINWRK                                                                      
       ++INCLUDE SPFINWRK                                                       
TWAD     DSECT                                                                  
         ORG   FINTABH                                                          
       ++INCLUDE SPFINFED                                                       
         ORG                                                                    
*                                                                               
         ORG   FINTABH                                                          
       ++INCLUDE SPFINFDD                                                       
         ORG                                                                    
         EJECT                                                                  
*========================*                                                      
* ROUTS S/R LOCAL W/S    *                                                      
*========================*                                                      
*                                                                               
RWRKD    DSECT                                                                  
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RBYTE    DS    XL1                                                              
RFLAG    DS    XL1                                                              
RFLAG2   DS    XL1                                                              
RHALF    DS    H                                                                
RDUB     DS    D                                                                
RDUB2    DS    D                                                                
RFULL    DS    F                                                                
DATEFORM DS    CL1                                                              
TEMPPRD  DS    CL3                                                              
TEMPEST  DS    CL1                                                              
NEWSTA   DS    CL9                NEW STATION                                   
BILLFST  DS    CL1                                                              
SVLBL    DS    CL9                                                              
         DS    0D                                                               
TOT      DS    PL16                                                             
AMT      DS    PL8                                                              
GROSAMT  DS    PL8                                                              
NETAMT   DS    PL8                                                              
DATES    DS    CL12                                                             
RSPACES  DS    CL24                                                             
EMLIST   DS    CL27               ESTIMATE DATE LIST                            
TEMPPROF DS    CL(L'SVPROF)       TEMPORARY  PROFILE                            
TEMPADD  DS    XL4                                                              
AMTBLOCK DS    CL(SPBVALDL)                                                     
RPARM    DS    8F                                                               
RWORK    DS    XL64                                                             
RWORK4   DS    XL256                                                            
         DS    0D                                                               
TEMPBLK  DS    CL9                TEMPORARY BLK DON'T SEPERATE                  
         DS    45PL6                                                            
*                                                                               
RIO      DS    2000C                                                            
RWRKX    EQU   *                                                                
         EJECT                                                                  
* FAGETTXTD                                                                     
* FAFACTS                                                                       
* FATIOB                                                                        
* FASYSLSTD                                                                     
* DDTWABLDD                                                                     
* DEDBLOCK                                                                      
* DDOFFICED                                                                     
* CTGENFILE                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDTWABLDD                                                      
AMTBLKD  DSECT                                                                  
       ++INCLUDE SPBVALD                                                        
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE                                                                  
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075SPFIN00   03/25/10'                                      
         END                                                                    
