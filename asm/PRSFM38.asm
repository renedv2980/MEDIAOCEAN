*          DATA SET PRSFM38    AT LEVEL 032 AS OF 07/17/18                      
*PHASE T41C38A               * * * * * * * * NOTE "C" PHASE                     
*        TITLE 'T41C38  EXCHANGE RECORD'                                        
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD'                                        
**** CHANGE LOG                                                                 
*                                                                               
* SMUR   04/18  SPEC-17729 NEW MEDIA (D)IGITAL AUDIO                            
*                                                                               
* BPLA   06/15  NEW MEDIA CODES B, V, W                                         
*                                                                               
* BOBY   04/08  BIG BANG                                                        
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T41C38 - EXCHANGE RECORD MAINT/LIST/REPORT            *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN T41CE3 (MAINTENANCE)                           *         
*               SCREEN T41CE4 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED EXCHANGE RECORD                               *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD'                                        
T41C38   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C38,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
*        SWITCH TO CONTROL SYSTEM                                               
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH                       
*                                                                               
         OI    GENSTAT3,MULTFILS   DEALING WITH MULTIPLE FILES                  
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
*                                                                               
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - ANAL'                                 
***********************************************************************         
*                                                                     *         
*        ANALYZE CALLING MODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ANAL     DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     ANALX                                                            
*****                                                                           
*****    CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
*****    BNE   *+12                                                             
*****    NOP   DR                                                               
*****    B     ANALX                                                            
*****                                                                           
*****    CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
*****    BNE   *+12                                                             
*****    NOP   DR                                                               
*****    B     ANALX                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     ANALX                                                            
*                                                                               
ANALX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
NOCHG    EQU   79                  FIELD CANNOT BE CHANGED                      
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VK'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      SKIP IF LISTING OR                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       REPORTING                                    
         BE    VKL                                                              
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKCUR'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE CURRENCY CODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKCUR    DS    0H                                                               
*                                                                               
         LA    R2,SCRCURH          POINT TO CURRENCY INPUT                      
*                                                                               
         GOTOR VGETFLD             READ IN CURRENCY CODE                        
*                                                                               
         BRAS  RE,VALCUR           VALIDATE CURRENCY CODE                       
         BNE   VKCURNV             ERROR                                        
*                                                                               
         OI    6(R2),X'80'         FORCE RE-DISPLAY                             
*                                                                               
VKCURX   DS    0H                                                               
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKCLT'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKCLT    DS    0H                                                               
*                                                                               
         MVC   SCRCLTN,SPACES      INIT                                         
         MVC   QCLT,SPACES         INIT                                         
         MVC   CLTNM,SPACES        INIT                                         
*                                                                               
         MVI   ERROR,0             INIT ERROR CODE                              
*                                                                               
         LA    R2,SCRCLTH          VALIDATE CLIENT                              
*                                                                               
         CLI   5(R2),0             OKAY IF NOT ENTERED                          
         BE    VKCLTX                                                           
*                                                                               
         BRAS  RE,VALCLT           VALIDATE CLIENT                              
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
         MVC   SCRCLTN(L'CLTNM),CLTNM  DISPLAY CLIENT NAME                      
*                                                                               
         OI    SCRCLTH+6,X'80'     FORCE RE-DISPLAY                             
         OI    SCRCLTNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKPER'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE PERIOD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKPER    DS    0H                                                               
*                                                                               
         LA    R2,SCRPERH          POINT TO PERIOD FIELD                        
*                                                                               
         CLI   5(R2),0             REQUIRED FIELD                               
         BE    VKMISSER                                                         
*                                                                               
         BRAS  RE,VVALPER          VALIDATE PERIOD                              
*                                                                               
VKPERX   DS    0H                                                               
*                                                                               
         B     VKBLD                                                            
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKL'                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY FIELDS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKLCUR'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE CURRENCY CODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLCUR   DS    0H                                                               
*                                                                               
         LA    R2,SCLCURH          POINT TO CURRENCY INPUT                      
*                                                                               
         GOTOR VGETFLD             READ IN CURRENCY CODE                        
*                                                                               
         BRAS  RE,VALCUR           VALIDATE CURRENCY CODE                       
         BNE   VKCURNV             ERROR                                        
*                                                                               
         OI    6(R2),X'80'         FORCE RE-DISPLAY                             
*                                                                               
VKLCURX  DS    0H                                                               
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKLCLT'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLCLT   DS    0H                                                               
*                                                                               
         MVC   SCLCLTN,SPACES      INIT                                         
         MVC   QCLT,SPACES         INIT                                         
         MVC   CLTNM,SPACES        INIT                                         
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
         LA    R2,SCLCLTH          VALIDATE CLIENT                              
*                                                                               
         CLI   5(R2),0             OKAY IF NOT ENTERED                          
         BE    VKLCLTX                                                          
*                                                                               
         BRAS  RE,VALCLT           VALIDATE CLIENT                              
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
         MVC   SCLCLTN(L'CLTNM),CLTNM  DISPLAY CLIENT NAME                      
*                                                                               
         OI    SCLCLTH+6,X'80'     FORCE RE-DISPLAY                             
         OI    SCLCLTNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKLPER'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE PERIOD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLPER   DS    0H                                                               
*                                                                               
         LA    R2,SCLPERH          POINT TO PERIOD FIELD                        
*                                                                               
         BRAS  RE,VVALPER          VALIDATE PERIOD                              
*                                                                               
VKLPERX  DS    0H                                                               
*                                                                               
         B     VKBLD                                                            
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VKBLD'                                
***********************************************************************         
*                                                                     *         
*        BUILD STARTING KEY                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKBLD    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY                                                           
         USING GEXCD,R4            ESTABLISH EXCHANGE RECORD KEY                
*                                                                               
         MVI   GEKREC,GEKRECQ      SET RECORD TYPE                              
         MVC   GEKAGY,AGENCY       SET AGENCY                                   
         MVI   GEKSYS,4            SET TO PRINT SYSTEM                          
         MVC   GEKCURF,SCRCUR      SET FROM CURRENCY                            
         MVC   GEKCURT,=C'USD'     SET TO   CURRENCY TO USD                     
         MVI   GEKCTYP,GEKBOOQ     SET TO BOOKING RATES                         
         MVI   GEKMED,X'FF'        MEDIA    UNUSED                              
         MVC   GEKCLI,QCLT         SET CLIENT                                   
*                                                                               
         CLC   QCLT,=CL3' '        IF NONE                                      
         BH    *+10                                                             
         MVC   GEKCLI,=3X'FF'         SET TO DEFAULT                            
*                                                                               
         MVI   GEKPRO,X'FF'        PRODUCT  UNUSED                              
         MVI   GEKCAM,X'FF'        CAMPAIGN UNUSED                              
*                                                                               
         MVC   GEKPEND,BCEND       END   OF PERIOD                              
         MVC   GEKPSTA,BCSTART     START OF PERIOD                              
*                                                                               
*        MAKE SURE NO OVERLAP OF PERIODS                                        
*                                                                               
VKADD    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VKADDX                                                           
*                                                                               
         MVC   SAVKEY,GEKEY        SAVE KEY OF RECORD                           
*                                                                               
         XC    GEKPEND,GEKPEND     INIT PERIOD FIELDS                           
         XC    GEKPSTA,GEKPSTA                                                  
*                                                                               
         GOTOR HIGH                READ FOR SIMILAR RECORDS                     
*                                                                               
VKADDLP  DS    0H                                                               
*                                                                               
         CLC   GEKEY(GEKPEND-GEKEY),SAVKEY DONE IF MAJOR KEY MISMATCH           
         BNE   VKADDDN                                                          
*                                                                               
         CLC   =X'FFFF',GEKPEND                IF UFN                           
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPSTA-GEKEY+SAVKEY       OK IF STARTS EARLIER          
         BNH   VKADDCN                                                          
*                                                                               
         CLC   GEKPEND,GEKPSTA-GEKEY+SAVKEY  CHECK PERIOD OVERLAP               
         BL    VKADDCN                                                          
*                                                                               
         CLC   GEKPSTA,GEKPEND-GEKEY+SAVKEY   ELSE                              
         BH    VKADDCN                                                          
*                                                                               
         B     VKADDE1             ERROR - PERIOD OVERLAP                       
*                                                                               
VKADDCN  DS    0H                                                               
*                                                                               
         GOTOR SEQ                 FIND NEXT RECORD                             
*                                                                               
         B     VKADDLP                                                          
*                                                                               
VKADDDN  DS    0H                                                               
*                                                                               
         MVC   KEY,SAVKEY          RESTORE KEY                                  
*                                                                               
VKADDX   DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKADDE1  LHI   RF,PWEPEROV         PERIODS OVERLAP                              
         LA    R2,SCRPERH          CURSOR HERE                                  
         B     VKERR                                                            
*                                                                               
VKMISSER LA    RF,PWEPERRQ         PERIOD REQUIRED                              
         B     VKERR                                                            
*                                                                               
VKLKOUT  LA    RF,PWESECLK         SECURITY LOCKOUT                             
         B     VKERR                                                            
*                                                                               
VKCURNV  LA    RF,PWECURNV         INVALID CURRENCY                             
         B     VKERR                                                            
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VALCUR'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE CURRENCY                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
VALCUR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             INIT CURRENCY KEY                            
         LA    R4,KEY              ESTABLISH CURRENCY KEY                       
         USING GCURD,R4                                                         
*                                                                               
         MVI   GCKREC,GCKRECQ      RECORD TYPE CURRENCY                         
         MVC   GCKCURR,FLD         ENTERED CURRENCY CODE                        
*                                                                               
         GOTO1 READ                READ CURRENCY RECORD                         
         BNE   VALCURN                                                          
*                                                                               
         MVC   AIO,AIO2            USE IOAREA2                                  
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1            USE AIO1 FOR I/O                             
*                                                                               
         CR    RB,RB                                                            
         B     VALCURX                                                          
*                                                                               
VALCURN  DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
         B     VALCURX                                                          
*                                                                               
VALCURX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VALCUR'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
VALCLT   NTR1  BASE=*,LABEL=*                                                   
*********                                                                       
*********OC    VOFFICER,VOFFICER   SKIP IF WE HAVE A(OFFICER)                   
*********BNZ   VALCLT10                                                         
*********                                                                       
*********XC    DMCB(12),DMCB                                                    
*********MVC   DMCB+4(4),=X'D9000A38'                                           
*********GOTO1 CALLOV,DMCB         GET OFFICER ADDRESS                          
*********CLI   4(R1),255                                                        
*********BNE   *+6                                                              
*********DC    H'0'                                                             
*********                                                                       
*********MVC   VOFFICER,DMCB       SAVE ADDRESS                                 
*********                                                                       
VALCLT10 DS    0H                                                               
*                                                                               
*        SWITCH TO PRINT SYSTEM                                                 
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),CONNSYS                                                  
*                                                                               
         GOTO1 SWITCH,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SYSDIR,=CL8'PRTDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PRTFILE' SET FILE                                    
         MVC   DATADISP,=H'33'                                                  
         MVC   LKEY,=H'25'                                                      
         MVC   LSTATUS,=H'2'                                                    
*                                                                               
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
         MVC   QCLT,8(R2)          SAVE CLIENT CODE                             
         OC    QCLT,SPACES         FORCE TO BE BLANK FILLED                     
*                                                                               
         MVC   AIO,AIO2            USE IOAREA2                                  
*                                                                               
         LA    R3,CLTMEDLS         POINT TO LIST OF MEDIA                       
*                                                                               
VALCLTLP DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         ERROR IF END OF MEDIAS TABLE                 
         BE    VALCLTE1                                                         
*                                                                               
         XC    KEY,KEY             ESTABLISH CLIENT KEY                         
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKAGY,AGENCY     SET AGENCY                                   
         MVC   PCLTKMED,0(R3)      SET MEDIA                                    
         MVI   PCLTKRCD,X'02'      SET RECORD ID                                
         MVC   PCLTKCLT,QCLT       SET CLIENT CODE                              
*                                                                               
         MVI   RDUPDATE,C'N'       READ IS NOT FOR UPDATE                       
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VALCLTCN            NEXT MEDIA IF RECORD NOT FOUND               
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLI   OFFLINE,C'Y'        RUNNING OFFLINE REPORT ?                     
         BE    VALCLTFD            YES - SKIP LIMIT ACCESS                      
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
*                                                                               
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    VALCLTFD                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PCLTOFF          SV PCLTOFF FOR LIMIT ACCESS TEST             
*                                                                               
         CLI   TRFAGSW,C'Y'        TRAFFIC ID SIGN-ON ?                         
         BNE   VALCLT20              NO                                         
*                                                                               
*        SEE IF TRAFFIC OFFICE EXISTS                                           
*                                                                               
         L     R6,AIO              POINT TO CLIENT REC                          
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BRAS  RE,GETEL                                                         
         BNE   VALCLT20              NO TRAFFIC OFFICE FOUND                    
*                                                                               
*        REPLACE PCLTOFF SAVED IN R0 WITH CLIENT TRAFFIC OFFICE CODE            
*                                                                               
         IC    R0,2(R6)                                                         
*                                                                               
VALCLT20 DS    0H                  LIMIT ACCESS TESTING                         
*                                                                               
*        USE OFFICER TO MAKE SURE USER HAS ACCESS TO CLIENT                     
*                                                                               
         XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
*                                                                               
         L     RF,ATWA                                                          
*                                                                               
         MVI   OFCSYS,C'P'         SET SYSTEM                                   
         MVC   OFCAUTH,6(RF)       SET AUTHORIZATION BITS                       
         MVC   OFCAGY,AGENCY       SET AGENCY                                   
         STC   R0,OFCOFC           CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT     SET CLIENT CODE                              
         OC    OFCCLT,=3C' '       MAKE SURE CODE IS UPPERCASE                  
         MVC   OFCPMED,PCLTKMED    SET MEDIA                                    
         MVC   OFCLMT(4),6(RF)     SET AUTHORIZATION BITS                       
         MVC   OFCSECD,ASECBLK     A("SECRET BLOCK")                            
*                                                                               
         DROP  R1                                                               
*                                                                               
*        CALL OFFICER                                                           
*                                                                               
         GOTOR VOFFICER,DMCB,(C'N',WORK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BE    VALCLTFD            CLIENT VALIDATED                             
*                                                                               
VALCLTCN DS    0H                                                               
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT MEDIA                           
         B     VALCLTLP                                                         
*                                                                               
VALCLTFD DS    0H                                                               
*                                                                               
         MVC   CLTNM,PCLTNAME      SAVE CLIENT NAME                             
         MVC   SVCPROF,PCLTPROF    SAVE CLIENT PROFILE                          
         MVC   SVCLSTAT,PCLTSTAT   SAVE CLT STATUS BYTE                         
         MVC   QCLT,PCLTKCLT       SET MEDIA                                    
*                                                                               
* NOTE USE OF SVCPROF+30 TO STORE PCLTOFF                                       
*                                                                               
         MVC   SVCPROF+30(1),PCLTOFF                                            
*                                                                               
* READ FOR F0 PROFILE AND SAVE IN F0PROF                                        
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   WORK+00(12),SPACES                                               
         MVC   WORK+00(04),=C'P0F0'                                             
         MVC   WORK+04(02),AGENCY                                               
         MVC   WORK+06(01),PCLTKMED                                             
         MVC   WORK+07(03),PCLTKCLT                                             
*                                                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,F0PROF,DATAMGR                                 
*                                                                               
         DROP  R4                  DONE USING PCLTRECD                          
*                                                                               
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        USER DEFINITION FLDS ELEM CODE               
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVP1USER(SVULNQ),2(R6)                                           
*                                                                               
VALCLTX  DS    0H                                                               
*                                                                               
         MVI   USEIONUM,0          RESET                                        
*                                                                               
*        SWITCH BACK TO CONTROL SYSTEM                                          
*                                                                               
         MVC   SYSDIR,=CL8'GENDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'GENFILE' SET FILE                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH                       
*                                                                               
         MVC   AIO,AIO1            RESET AIO                                    
*                                                                               
         XIT1                                                                   
*                                                                               
*        ERROR HANDLING                                                         
*                                                                               
VALCLTE1 DS    0H                                                               
*                                                                               
         LHI   RF,INVCLI           INVALID CLIENT ERROR                         
*                                                                               
         B     VALCLTER                                                         
*                                                                               
VALCLTER DS    0H                                                               
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         MVI   USEIONUM,0          RESET                                        
*                                                                               
*        SWITCH BACK TO CONTROL SYSTEM                                          
*                                                                               
         MVC   SYSDIR,=CL8'GENDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'GENFILE' SET FILE                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH                       
*                                                                               
         MVC   AIO,AIO1            RESET AIO                                    
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
CLTMEDLS DS    0X                  LIST OF MEDIAS                               
         DC    C'BDILMNOSTVW'                                                   
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM38 - EXCHANGE RECORDS - VVALPER'                           
***********************************************************************         
*                                                                     *         
*        VALIDATE PERIOD                                              *         
*                                                                     *         
*NTRY    R2==> PERIOD   FIELD ON SCREEN                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         MVC   BCSTART,=X'0000'    DEFAULT TO ALL OF TIME                       
         MVC   BCEND,=X'FFFF'         COMPRESSED DATES                          
*                                                                               
         CLI   5(R2),0             SKIP IF PERIOD FIELD IS EMPTY                
         BE    VVALPERX                                                         
*                                                                               
         MVC   BCEND,=X'0000'      DEFAULT TO NO END TIME                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)            GET INPUT LENGTH                             
         SHI   R1,4                BACK UP 4 POSITIONS                          
         BM    VPERINVE            MUST HAVE SOMETHING                          
*                                                                               
         LA    R1,8(R1,R2)         POINT TO END OF ENTRY                        
*                                                                               
         CLC   =C'-UFN',0(R1)      IF UNTIL FURTHER NOTICE                      
         BNE   VVALPER1                                                         
*                                                                               
         MVC   BCEND,=X'FFFF'         SET TO THE END OF TIME                    
         MVC   0(4,R1),SPACES         ERASE FOR PERVAL                          
*                                                                               
VVALPER1 DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),('PVINTOD+PVINSGLS',(R4)) VAL          
*                                                                               
         CLI   4(R1),PVRCONE       OKAY IF SINGLE DATE RETURNED                 
         BE    *+12                                                             
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VPERINVE                                                         
*                                                                               
         MVC   QSTART,PVALESTA    SAVE START AND END DATES                      
         MVC   QEND,PVALEEND          YYMMDD                                    
         MVC   BSTART,PVALBSTA    SAVE BINARY FORMAT                            
         MVC   BEND,PVALBEND       YMD                                          
         MVC   BCSTART,PVALCSTA    COMPRESSED FORMAT                            
*                                                                               
         CLI   4(R1),PVRCONE       IF SINGLE DATE                               
         BNE   VVALPER2                                                         
*                                                                               
         CLC   BCEND,=X'FFFF'      SKIP IF UFN                                  
         BE    *+10                                                             
         MVC   BCEND,PVALCSTA         ELSE USE SINGLE DATE AS LAST              
*                                                                               
         B     VVALPER3                                                         
*                                                                               
VVALPER2 DS    0H                                                               
*                                                                               
         MVC   BCEND,PVALCEND      SET END OF RANGE DATE                        
*                                                                               
VVALPER3 DS    0H                                                               
*                                                                               
         CLC   BCEND,BCSTART       START MUST BE BEFORE END                     
         BL    VPERDTSE                                                         
*                                                                               
*        RE-DISPLAY PERIOD                                                      
*                                                                               
         GOTOR DATCON,DMCB,(2,BCSTART),(17,8(R2)) START DATE                    
*                                                                               
         MVI   16(R2),C'-'                                                      
*                                                                               
         CLC   BCEND,=X'FFFF'      IF UFN                                       
         BNE   *+14                                                             
         MVC   17(3,R2),=C'UFN'                                                 
         B     VVALPERX                                                         
*                                                                               
         GOTOR DATCON,DMCB,(2,BCEND),(17,17(R2))  END DATE                      
*                                                                               
         OI    6(R2),OI1T          FORCE RE-DISPLAY                             
*                                                                               
VVALPERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPERINVE LHI   RF,PWEOPDNV         INVALID DATE                                 
         B     VPERERR                                                          
*                                                                               
VPERDTSE LHI   RF,PWEOPDTS         END DATE BEFORE START DATE                   
*                                                                               
VPERERR  DS    0H                                                               
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - ACTVDIS'                              
***********************************************************************         
*                                                                     *         
*        DISPLAY ACTIVITY                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
ACTVDIS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R2,SCRACT1H         POINT TO FIRST ACTIVITY LINE                 
*                                                                               
         LA    R0,((SCRACTL-SCRACT1)/(SCRACT2-SCRACT1))+1 # OF LINES            
*                                                                               
*        DISPLAY DATA IN ACTIVITY ELEMENT                                       
*                                                                               
         MVI   ELCODE,PATVELQ      SET FOR ACTIVITY ELEMENTS                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
         BNZ   ACTDDONE            NONE                                         
*                                                                               
ACTDLOOP DS    0H                                                               
*                                                                               
         LA    R5,8(R2)            POINT TO DISPLAY AREA                        
         USING DSPACTVD,R5         ESTABLISH DISPLAY LINE                       
*                                                                               
         USING PATVELD,R6          ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         GOTOR TRNUSR,DMCB,(C'N',PATVUSR),(L'DSPUSR,DSPUSR)  DSP USERID         
*                                                                               
         GOTOR TRNPID,DMCB,(C'N',PATVPID),(L'DSPPID,DSPPID)  TRANS PID          
*                                                                               
         GOTOR DATCON,DMCB,(3,PATVDTE),(17,DSPDTE) DISPLAY DATE                 
*                                                                               
         LA    RF,ACTTBL           POINT TO ACTION TABLE                        
         LA    R1,DSPDATA-1        POINT TO DATA DISPLAY                        
*                                                                               
ACTDACLP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    ACTDACDN                                                         
*                                                                               
         MVC   BYTE,0(RF)          COPY INDICATOR IN TABLE                      
*                                                                               
         NC    BYTE,PATVCH1        CHECK IF ACTION PRESENT                      
         BZ    ACTDACCN                                                         
*                                                                               
         MVI   0(R1),C','          SET SEPARATOR                                
         MVC   1(3,R1),1(RF)       SET TRANSLATION                              
*                                                                               
         LA    R1,4(R1)            BUMP TO NEXT DISPLAY AREA                    
*                                                                               
ACTDACCN DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            NEXT ITEM IN TABLE                           
         B     ACTDACLP                                                         
*                                                                               
ACTDACDN DS    0H                                                               
*                                                                               
         LA    RF,DATTBL           POINT TO DATA TABLE                          
*                                                                               
ACTDDALP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'         DONE AT END OF TABLE                         
         BE    ACTDDADN                                                         
*                                                                               
         MVC   BYTE,0(RF)          COPY INDICATOR IN TABLE                      
*                                                                               
         NC    BYTE,PATVCH2        CHECK IF DATA PRESENT                        
         BZ    ACTDDACN                                                         
*                                                                               
         MVI   0(R1),C','          SET SEPARATOR                                
         MVC   1(4,R1),1(RF)       SET TRANSLATION                              
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT DISPLAY AREA                    
*                                                                               
ACTDDACN DS    0H                                                               
*                                                                               
         LA    RF,5(RF)            NEXT ITEM IN TABLE                           
         B     ACTDDALP                                                         
*                                                                               
ACTDDADN DS    0H                                                               
*                                                                               
         MVI   DSPDATA-1,C' '      REMOVE ANY COMMA                             
*                                                                               
ACTDCONT DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT ELEMENT                            
         BNZ   ACTDDONE                                                         
*                                                                               
         BCT   R0,ACTDLOOP                                                      
*                                                                               
ACTDDONE DS    0H                                                               
*                                                                               
ACTVDISX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ACTTBL   DS    0X                  TABLE OF ACTIONS                             
         DC    X'80',C'ADD'        ADD                                          
         DC    X'40',C'DEL'        DELETE                                       
         DC    X'20',C'RES'        RESTORE                                      
         DC    X'10',C'CHA'        CHANGE                                       
         DC    X'FF'               EOT                                          
*                                                                               
DATTBL   DS    0X                  TABLE OF DATA CHANGED                        
         DC    X'80',C'RATE'       RATE                                         
         DC    X'FF'               EOT                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM38 - EXCHNAGE RECORD MAINT - BUMP'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUMP TO NEXT FIELD ON SCREEN                      *         
*                                                                     *         
*              BUMP -  NEXT FIELD                                     *         
*              BUMPU - NEXT UNPROTECTED FIELD                         *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - NOT END OF SCREEN                                *         
*              EQ  - END OF SCREEN                                    *         
*                                                                     *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT FIELD                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          GET LENGTH OF TWA FIELD                      
         AR    R2,RF               POINT TO NEXT FIELD                          
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
*        THIS VERSION BUMPS TO NEXT UNPROTECTED FIELD                           
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         JNZ   BUMPU                  GO TO NEXT FIELD                          
*                                                                               
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM38 - EXCHNAGE RECORD MAINT - TRNUSR'                       
***********************************************************************         
*                                                                     *         
*        TRANSLATE USR TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1+0     C'N' - NOT DISPLAYING TO SCREEN FIELD               *         
*        P1       A(USR)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNUSR   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   SAVKEY,KEY          SAVE CURRENT KEY                             
         MVC   SAVAIO,AIO          SAVE CURRENT AIO                             
*                                                                               
         L     R5,0(R1)            POINT TO USR                                 
*                                                                               
         MVC   BYTE,4(R1)          SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         CLI   0(R1),C'N'          IF R2 POINTS TO SCREEN FIELD                 
         BE    *+8                                                              
         LA    R3,8(R2)               USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO USR FOUND                         
         BZ    TUSRNOTF                                                         
*                                                                               
*        READ ID RECORD ON CTFILE                                               
*                                                                               
         LA    R4,KEY                                                           
         USING CTIREC,R4           ESTABLISH KEY AS ID RECORD                   
         XC    CTIKEY,CTIKEY       INIT KEY                                     
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    SET RECORD TYPE                              
*                                                                               
         MVC   CTIKNUM,0(R5)       SET USR                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CTIKEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TUSRNOTF                                                         
*                                                                               
*        FIND ID                                                                
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    R6,CTIDATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TUSRCTLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             CHECK FOR END OF RECORD                      
         BE    TUSRCTDN                                                         
*                                                                               
         CLI   0(R6),CTDSCELQ      FIND DESCRIPTION ELEMENT                     
         BE    TUSRCTFD                                                         
*                                                                               
TUSRCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     TUSRCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TUSRCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TUSRNOTF                                                         
*                                                                               
TUSRCTFD DS    0H                                                               
*                                                                               
         USING CTDSCD,R6          ESTABLISH DESCRIPTION ELEMENTT                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CTDSCLEN         GET ELEMENT LENGTH                           
*                                                                               
         SHI   RF,CTDSC-CTDSCD     SUBTRACT HEADER LENGTH                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTDSC       GET USERID                                   
*                                                                               
         B     TUSRDSP                                                          
*                                                                               
TUSRNOTF DS    0H                  PRINT 'UNKNOWN' IF NO USR                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
*                                                                               
TUSRDSP  DS    0H                                                               
*                                                                               
*        OUTPUT USERID                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BYTE           GET OUTPUT LENGTH                            
         BZ    TRNUSRX             NO OUTPUT AREA                               
*                                                                               
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK           INIT OUT PUT AREA                         
*                                                                               
         MVC   KEY,SAVKEY          RESTORE CURRENT KEY                          
         MVC   AIO,SAVAIO          RESTORE CURRENT AIO                          
*                                                                               
         GOTOR HIGH                RESTORE FILE POINTERS                        
*                                                                               
TRNUSRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1+0     C'N' - NOT DISPLAYING TO SCREEN FIELD               *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   SAVKEY,KEY          SAVE CURRENT KEY                             
         MVC   SAVAIO,AIO          SAVE CURRENT AIO                             
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         CLI   0(R1),C'N'          IF R2 POINTS TO SCREEN FIELD                 
         BE    *+8                                                              
         LA    R3,8(R2)               USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
***      CLI   0(R1),C'N'          IF SCREEN FIELD                              
***      BZ    *+8                                                              
***      BRAS  RE,CLRFLD           INIT OUTPUT                                  
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SAVSECAG    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SAVSECAG    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
         MVC   KEY,SAVKEY          RESTORE CURRENT KEY                          
         MVC   AIO,SAVAIO          RESTORE CURRENT AIO                          
*                                                                               
         GOTOR HIGH                RESTORE FILE POINTERS                        
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - ACTVADD'                              
***********************************************************************         
*                                                                     *         
*        ADD ACTIVITY ELEMENT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
ACTVADD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
*        DELETE ALL ACTIVITY ELEMENTS                                           
*                                                                               
         MVI   ELCODE,PATVELQ      SET ACTVITY ELEMENT CODE                     
*                                                                               
         SR    R0,R0               INIT ELEMENT COUNTER                         
         LA    R6,SAVACTV          POINT TO ACTIVITY ELM SAVEAREA               
         XC    0(256,R6),0(R6)     INIT FIRST SAVEAREA                          
*                                                                               
ATVREMLP DS    0H                                                               
*                                                                               
         GOTOR REMELEM             REMOVE A CURRENT ACTIVTY ELEMENT             
*                                                                               
         OC    ELEMENT,ELEMENT     DONE IF NONE FOUND                           
         BZ    ATVREMDN                                                         
*                                                                               
         AHI   R0,1                BUMP ELEMENT COUNTER                         
*                                  REMELEM RETURNS DEL'D ELM IN ELEMENT         
         MVC   0(256,R6),ELEMENT   SAVE DELETED ELEMENT                         
*                                                                               
         LA    R6,256(R6)          POINT TO NEXT SAVEAREA                       
         XC    0(256,R6),0(R6)     INIT NEXT SAVEAREA                           
*                                                                               
ATVREMCN DS    0H                                                               
*                                                                               
         B     ATVREMLP                                                         
*                                                                               
ATVREMDN DS    0H                                                               
*                                                                               
*        FIND PID                                                               
*                                                                               
         XC    SAVPID,SAVPID       PASSWORD ID NUMBER CLEARED                   
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SAVSECAG,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SAVPID,FAPASSWD     SAVE PASSWORD ID NUMBER                      
*                                                                               
*        SEE IF USER MADE PREVIOUS CHANGE TODAY                                 
*                                                                               
         LA    R6,SAVACTV          POINT TO FIRST ACTIVITY ELEMENT              
         USING PATVELD,R6          ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         SR    R5,R5               INIT SEQ SAVEAREA                            
         SR    R3,R3               INIT A(HIGHEST SQN ELM)                      
*                                                                               
ATVCHKLP DS    0H                                                               
*                                                                               
         CLI   PATVELCD,0          DONE IF SLOT EMPTY                           
         BE    ATVCHKDN                                                         
*                                                                               
         CLM   R5,3,PATVSQN        FIND HIGHEST SEQUENCE NUMBER                 
         BNL   *+8                                                              
         ICM   R5,3,PATVSQN                                                     
*                                                                               
         CLC   PATVDTE,BTODAY      MATCH ON DATE                                
         BNE   ATVCHKCN                                                         
*                                                                               
         CLC   PATVPID,SAVPID      MATCH ON PID                                 
         BNE   ATVCHKCN                                                         
*                                                                               
         CLC   PATVUSR,TWAORIG     MATCH ON USERID                              
         BNE   ATVCHKCN                                                         
*                                                                               
         LR    R3,R6               SAVE A(TABLE ENTRY)                          
*                                                                               
ATVCHKCN DS    0H                                                               
         LA    R6,256(R6)          BUMP TO NEXT SAVED ELEMENT                   
         B     ATVCHKLP                                                         
*                                                                               
ATVCHKDN DS    0H                  NO PRIOR ACTIVITY BY USER TODAY              
*                                                                               
         LTR   R3,R3               SKIP IF NO OTHER CHANGES TODAY               
         BZ    ATVCHKD1                                                         
*                                                                               
         CLM   R5,3,PATVSQN-PATVELD(R3) MUST BE LATEST ELM                      
         BNE   *+10                                                             
         LR    R6,R3               POINT TO FOUND ELEMENT                       
         B     ATVCHKFD                                                         
*                                                                               
ATVCHKD1 DS    0H                                                               
*                                                                               
*        BUILD NEW ACTIVITY ELEMENT IN TABLE                                    
*                                                                               
         MVI   PATVELCD,PATVELQ    SET ELEMENT CODE                             
         MVI   PATVLEN,PATVHDRL+2  SET ELEMENT LENGTH                           
*                                                                               
         AHI   R5,1                BUMP HIGHEST SEQUENCE NUMBER                 
         STCM  R5,3,PATVSQN        SET ELM SQN                                  
*                                                                               
         MVC   PATVUSR,TWAORIG     SET USER ID                                  
         MVC   PATVDTE,BTODAY      SET DATE                                     
         MVC   PATVPID,SAVPID      SET PID                                      
*                                                                               
         AHI   R0,1                BUMP NUMBER OF ELEMENTS                      
*                                                                               
ATVCHKFD DS    0H                                                               
*                                                                               
         OC    PATVCH1,ACTVCH1     ADD IN CHANGED DATA BITS                     
         OC    PATVCH2,ACTVCH2                                                  
*                                                                               
*        CLEAR EXTRA ACTIVITY ELEMENTS                                          
*                                                                               
         CHI   R0,MAXACTVQ         OKAY IF UNDER MAX ACTVITY ELEMENTS           
         BNH   ATVCLRDN                                                         
*                                                                               
         LA    R6,SAVACTV          POINT TO TABLE OF ACTIVITY ELMS              
*                                                                               
ATVCLRLP DS    0H                                                               
*                                                                               
         CLI   PATVELCD,0          DONE IF END OF ELEMENTS                      
         BE    ATVCLRDN                                                         
*                                                                               
*                                  SKIP IF ADD OR DELETE OR RESTORE             
         TM    PATVCH1,PATVADDQ+PATVDELQ+PATVRSTQ                               
         BNZ   ATVCLRCN                                                         
*                                                                               
         XC    0(256,R6),0(R6)     CLEAR ENTRY                                  
         SHI   R0,1                DECREMENT NUMBER OF ELEMENTS                 
*                                                                               
         CHI   R0,MAXACTVQ         DONE IF MAX ELMS LEFT                        
         BNH   ATVCLRDN                                                         
*                                                                               
ATVCLRCN DS    0H                                                               
*                                                                               
         LA    R6,256(R6)          NEXT ELEMENT IN TABLE                        
         B     ATVCLRLP                                                         
*                                                                               
ATVCLRDN DS    0H                                                               
*                                                                               
*        ADD ACTIVITY ELEMENTS TO RECORD                                        
*                                                                               
         OI    GENSTAT5,ADDEQCOD   ADD ELMS AT END OF ELEMENT TYPE              
*                                                                               
         LA    R6,SAVACTV          POINT TO SAVED ACTIVITY ELMS                 
*                                                                               
ATVADDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             SKIP IF EMPTY SLOT                           
         BE    ATVADDCN                                                         
*                                                                               
         MVC   ELEMENT,0(R6)       GET NEXT SAVED ELEMENT                       
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
         LA    R6,256(R6)          BUMP TO NEXT ELEMENT                         
*                                                                               
         SHI   R0,1                DECREMENT ELEMENT COUNTER                    
         BZ    ATVADDDN            NONE LEFT                                    
*                                                                               
ATVADDCN DS    0H                                                               
*                                                                               
         B     ATVADDLP            ADD NEXT ACTVITY ELEMENT                     
*                                                                               
ATVADDDN DS    0H                                                               
*                                                                               
ACTVADDX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
DISUSR   NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - DK'                                   
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             POINT TO EXCHANGE RECORD RECORD              
         USING GEXCD,R4            ESTABLISH EXCHANGE RECORD REC                
*                                                                               
         FOUT  SCRCURH,GEKCURF,3   DISPLAY CURRENCY                             
*                                                                               
         MVI   SCRCURH+5,L'GEKCURF  SET INPUT LENGTH                            
*                                                                               
DKCURX   DS    0H                                                               
*                                                                               
DKCUR    DS    0H                                                               
*                                                                               
         XC    SCRCLT,SCRCLT       INIT FIELD                                   
         MVI   SCRCLTH+5,0                                                      
*                                                                               
         CLC   GEKCLI,=X'FFFFFF'   SKIP IF DEFAULT CLIENT                       
         BE    DKCLTX                                                           
*                                                                               
         FOUT  SCRCLTH,GEKCLI,3    DISPLAY CLIENT                               
*                                                                               
         MVI   SCRCLTH+5,3         SET INPUT LENGTH                             
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
DKPER    DS    0H                                                               
*                                                                               
*        DISPLAY PERIOD START                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,GEKPSTA),(17,SCRPER)                              
*                                                                               
         MVI   SCRPER+8,C'-'       SET SEPARATOR                                
*                                                                               
*        DISPLAY END DATE                                                       
*                                                                               
         CLC   GEKPEND,=X'FFFF'    IF UNTIL FURTHER NOTICE                      
         BNE   *+18                                                             
         MVC   SCRPER+9(3),=C'UFN'                                              
         MVI   SCRPERH+5,12        FIELD LENGTH                                 
         B     DKPERX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,GEKPEND),(17,SCRPER+9)                            
*                                                                               
         MVI   SCRPERH+5,17        SET INPUT LENGTH                             
*                                                                               
DKPERX   DS    0H                                                               
*                                                                               
         OI    SCRPERH+6,X'80'     FORCE RE-DISPLAY                             
*                                                                               
DKPUBX   DS    0H                                                               
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - VR'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
*        VALIDATE RATE AS 5 DECIMAL PLACES NUMBER                               
*                                                                               
         LA    R2,SCRFXRTH         POINT TO RATE FIELD                          
*                                                                               
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VRMISS                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SCRFXRTH+5       INPUT LENGTH                                 
         GOTO1 CASHVAL,DMCB,(X'85',SCRFXRT),(R0)                                
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BNE   VREXCNNM                                                         
*                                                                               
         ZAP   DUB,DMCB+4(8)       GET RETURNED VALUE                           
*                                                                               
         CP    DUB,=P'200000'      MAX VALUE OF 2                               
         BH    VREXCNRG                                                         
*                                                                               
         CP    DUB,=P'0'           MUST BE POSITIVE                             
         BNH   VREXCNNP                                                         
*                                                                               
         SRP   DUB,1,0             SHIFT DIGITS 1 NYBBLE LEFT                   
*                                                                               
         L     R4,AIO              ESTABLISH EXCHANGE RECORD                    
         USING GEXCD,R4                                                         
*                                                                               
VRADD    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VRADDX                                                           
*                                                                               
         XC    GEXCD(100),GEXCD    INIT RECORD BUILD AREA                       
         MVC   GEKEY,KEY           SET KEY                                      
*                                                                               
         LA    RF,GEFIRST          MINIMUM LENGTH OF RECORD                     
         LA    RF,GEXELLQ(RF)      ADD ON RATE ELEM LENGTH                      
*                                                                               
         STCM  RF,3,GERLEN         SET RECORD LENGTH                            
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
         MVI   ELCODE,GEXELQ       SET ELEMENT CODE FOR ROUTINES                
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ACTION ADD                           
         BE    VR10                                                             
*                                                                               
         GOTOR REMELEM             CLEAR OUT OLD RATE ELEMENT                   
*                                                                               
VR10     DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT BUILD AREA                      
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
         USING GEXEL,R6            ESTABLISH RATE ELEMENT                       
*                                                                               
         MVI   GEXEL,GEXELQ        SET ELM ID                                   
         MVI   GEXELL,GEXELLQ      SET ELEMENT LENGTH                           
*                                                                               
         MVC   GEXRATE,DUB+2       SET EXCHANGE RATE                            
*                                                                               
         GOTOR ADDELEM             ADD NEW RATE ELEMENT                         
*                                                                               
*        UPDATE RATE INPUT ELEMENT                                              
*                                                                               
         MVI   ELCODE,GIXELQ       SET FOR RATE INPUT ELEMENT                   
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ACTION ADD                           
         BE    VR20                                                             
*                                                                               
         GOTOR REMELEM             CLEAR OUT OLD RATE INPUT ELEMENT             
*                                                                               
VR20     DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH AS RATE INPUT ELEMENT              
         USING GIXEL,R6                                                         
*                                                                               
         MVI   GIXEL,GIXELQ        SET ELEMENT ID                               
         MVI   GIXELL,GIXELLQ      SET ELEMENT LENGTH                           
*                                                                               
         MVC   GIXFROM,DUB+2       SET FROM RATE                                
         MVC   GIXTO,=X'0000100000'  SET TO RATE                                
*                                                                               
         GOTOR ADDELEM             ADD NEW RATE INPUT ELEMENT                   
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         MVI   ACTVCH1,0           INIT ACTION INDICATOR                        
         MVI   ACTVCH2,0           INIT DAT CHANGED INDICATOR                   
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   *+8                                                              
         OI    ACTVCH1,PATVADDQ       SET ACTION                                
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL       OR ACTION SELECT                             
         BNE   *+12                                                             
         OI    ACTVCH1,PATVCHGQ       SET ACTION                                
         OI    ACTVCH2,PEXCRTEQ       SET RATE AS CHANGED                       
*                                                                               
         BRAS  RE,ACTVADD          ADD ACTIVITY ELEMENT                         
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING A RECORD                           
         BNE   *+8                                                              
         BRAS  RE,UFNCHK              CHK IF UFN RECORD NEEDS ALTERING          
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         BRAS  RE,DR                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
VRMISS   LA    RF,PWEEXCRQ         EXCHANGE RATE REQUIRED.                      
         B     VRERR                                                            
*                                                                               
VRLKOUT  LA    RF,PWESECLK         SECURITY LOCKOUT                             
         B     VRERR                                                            
*                                                                               
VREXCNRG LA    RF,PWEEXCRG         OUTSIDE ALLOWED RANGE                        
         B     VRERR                                                            
*                                                                               
VREXCNNP LA    RF,PWEEXCNP         NOT POSITIVE                                 
         B     VRERR                                                            
*                                                                               
VREXCNNM LA    RF,PWEEXCNM         NOT NUMERIC                                  
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - UFNCHK'                               
***********************************************************************         
*                                                                     *         
*        CHECK IF UFN RECORD NEEDS ALTERING                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
UFNCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
         USING GEXCD,R4            ESTABLISH RECORD                             
*                                                                               
         MVC   SAVKEY,GEKEY        SAVE KEY                                     
*                                                                               
         MVC   KEY,SAVKEY          COPY KEY                                     
         LA    R4,KEY              ESTABLISH GEXCD KEY                          
*                                                                               
         MVC   GEKPEND,=X'FFFF'    SET FOR UFN KEYS                             
         XC    GEKPSTA,GEKPSTA     CLEAR PERIOD START                           
*                                                                               
         GOTOR HIGH                READ FOR FIRST KEY                           
*                                                                               
         CLC   KEY(GEKPSTA-GEKEY),KEYSAVE KEYS MUST MATCH TO PEND               
         BNE   UFNCHKX                                                          
*                                                                               
         CLC   GEKPSTA,GEKPSTA-GEKEY+SAVKEY EARLIER START                       
         BNL   UFNCHKX                                                          
*                                                                               
         MVC   AIO,AIO2            USE IOAREA2                                  
*                                                                               
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
*        DELETE OLD UFN RECORD                                                  
*                                                                               
         OI    GEDSTAT,X'80'       DELETE KEY                                   
         GOTOR WRITE                                                            
*                                                                               
         L     R4,AIO2             POINT TO RECORD                              
         OI    GESTAT,X'80'        SET DELETETE FLAG                            
*                                                                               
         GOTOR PUTREC              DELETE RECORD                                
*                                                                               
*        ADD RECORD WHOSE END IS 1 DAY BEFORE NEW START DATE                    
*                                                                               
         LA    R4,KEY              POINT TO DELETED KEY                         
*                                                                               
         GOTOR DATCON,DMCB,(2,GEKPSTA-GEKEY+SAVKEY),(0,WORK)                    
*                                  DAY BEFORE START                             
         GOTOR ADDAY,DMCB,WORK,WORK+16,X'FFFFFFFF'                              
         GOTOR DATCON,DMCB,WORK+16,(2,GEKPEND) NEW END DATE                     
*                                                                               
         NI    GEDSTAT,X'FF'-X'80'  REMOVE DELETE BIT                           
*                                                                               
         L     R4,AIO2             POINT TO DELETED RECORD                      
         MVC   GEKPEND,GEKPEND-GEKEY+KEY  NEW END DATE                          
         NI    GESTAT,X'FF'-X'80'  UNDELETE                                     
*                                                                               
         GOTOR ADDREC              ADD NEW RECORD                               
*                                                                               
UFNCHKX  DS    0H                                                               
*                                                                               
         MVC   KEY,SAVKEY          RESTORE KEY                                  
         MVC   AIO,AIO1            USE IOAREA1                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - DR'                                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO              ESTABLISH READ IN RECORD                     
         USING GEXCD,R4                                                         
*                                                                               
         LA    R6,GEFIRST+GEXCD    POINT TO FIRST ELEMENT IN RECORD             
         USING GEXEL,R6            ESTABLISH RATE ELEMENT                       
*                                                                               
*        DISPLAY EXCHANGE RATE                                                  
*                                                                               
         ZAP   DUB,=P'0'           INIT PACKED WORKAREA                         
         MVC   DUB+2(5),GEXRATE    COPY PWOS RATE                               
         SRP   DUB,64-1,5          SHIFT RIGHT ONE NYBBLE                       
*                                                                               
         LA    R1,WORK+2           INIT FIRST DIGIT POINTER                     
*                                                                               
         MVC   WORK(9),=X'4021204B2020202020' MOVE IN EDIT PATTERN              
         EDMK  WORK(9),DUB+4    DISPLAY EXCHANGE RATE                           
*                                                                               
         LA    RF,WORK+9                                                        
         SR    RF,R1               LENGTH OF NUMBER                             
         BZ    DR10                   NO RATE                                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCRFXRT(0),0(R1)    DISPLAY RATE                                 
*                                                                               
DR10     DS    0H                                                               
*                                                                               
         OI    SCRFXRTH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         FOUT  SCRCUR1H,GEKCURF,3  DISPLAY FROM CURRENCY                        
*                                                                               
         BRAS  RE,ACTVDIS          DISPLAY ACTIVITY                             
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - XR'                                   
***********************************************************************         
*                                                                     *         
*        AFTER ADDING A RECORD - NOTHING FOR NOW                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
XRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - LR'                                   
***********************************************************************         
*                                                                     *         
*        LIST A RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         LA    R4,KEY              ESTABLISH KEY OF RECORD                      
         MVC   AIO,AIO1                                                         
         USING GEXCD,R4                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LRLOOP              NO - KEY IS LAST RECORD READ                 
*                                       SO GO CHECK VS. KEYSAVE                 
*                                                                               
         MVI   GEKREC,GEKRECQ      SET RECORD TYPE                              
         MVC   GEKAGY,AGENCY       SET AGENCY                                   
         MVI   GEKSYS,4            SET TO PRINT SYSTEM                          
         MVC   GEKCURF,SCLCUR      SET FROM CURRENCY                            
         MVC   GEKCURT,=C'USD'     SET TO   CURRENCY TO USD                     
         MVI   GEKCTYP,GEKBOOQ     SET TO BOOKING RATES                         
         MVI   GEKMED,X'FF'        MEDIA    UNUSED                              
         MVC   GEKCLI,QCLT         SET CLIENT                                   
*                                                                               
         CLC   QCLT,SPACES         IF NO CLIENT                                 
         BH    *+10                                                             
         XC    GEKCLI,GEKCLI          SET FOR ALL                               
*                                                                               
         MVI   GEKPRO,X'FF'        PRODUCT  UNUSED                              
         MVI   GEKCAM,X'FF'        CAMPAIGN UNUSED                              
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD ON FILE                    
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH KEY OF RECORD                      
*                                                                               
         CLC   GEKEY(GEKKEY-GEKEY),KEYSAVE TEST FOR ALL DONE                    
         BNE   LRDONE                                                           
*                                                                               
         CLC   QCLT,SPACES         IF CLIENT ENTERED                            
         BNH   *+14                                                             
         CLC   GEKCLI,QCLT            MATCH ON CLIENT                           
         BNE   LRCONT                                                           
*                                                                               
         CLC   GEKPEND,BCSTART     SKIP IF RECORD DOESN'T                       
         BL    LRCONT                                                           
         CLC   GEKPSTA,BCEND                                                    
         BH    LRCONT                                                           
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,GEFIRST+GEXCD    POINT TO FIRST ELEMENT                       
         MVI   ELCODE,GEXELQ       FIND RATE ELEMENT                            
*                                                                               
         CLI   0(R6),GEXELQ        SKIP IF ELEMENT FOUND                        
         BE    *+12                                                             
         BRAS  RE,NEXTEL                                                        
         BNE   LRCONT              NONE FOUND                                   
*                                                                               
         USING GEXEL,R6            ESTABLISH RATE ELEMENT                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                                                               
         MVC   LISTCUR,GEKCURF     DISPLAY FROM CURRENCY                        
*                                                                               
LRCLT    DS    0H                                                               
*                                                                               
         CLC   GEKCLI,=X'FFFFFF'   IF DEFAULT CLIENT                            
         BNE   *+14                                                             
         MVC   LISTCLTN,=CL20'DEFAULT'    DISPLAY DEFAULT INDICATOR             
         B     LRCLTX                                                           
*                                                                               
         MVC   LISTCLT,GEKCLI      DISPLAY CLIENT                               
*                                                                               
         MVC   FLD(3),GEKCLI          MOVE TO WORK SCREEN FIELD                 
         LA    R2,FLDH                                                          
         MVI   FLDH+5,3            SET INPUT LENGTH                             
*                                                                               
         MVC   SAVCLT,QCLT         SAVE FILTERING CLIENT                        
         MVC   SAVDSKAD,DMDSKADD   SAVE DISK ADDRESS                            
*                                                                               
         BRAS  RE,VALCLT           VALIDATE CLIENT                              
*                                                                               
         MVC   QCLT,SAVCLT         RESTORE FILTERING CLIENT                     
         MVC   DMDSKADD,SAVDSKAD   RESTORE DISK ADDRESS                         
*                                                                               
         MVC   LISTCLTN,CLTNM      DISPLAY CLIENT NAME                          
*                                                                               
         MVC   KEY,GEKEY           RESTORE FILE POINTERS                        
*                                                                               
         GOTOR HIGH                                                             
*                                                                               
LRCLTX   DS    0H                                                               
*                                                                               
*        DISPLAY EXCHANGE RATE                                                  
*                                                                               
         ZAP   DUB,=P'0'           INIT PACKED WORKAREA                         
         MVC   DUB+2(5),GEXRATE    COPY PWOS RATE                               
         SRP   DUB,64-1,5          SHIFT RIGHT ONE NYBBLE                       
*                                                                               
         MVC   LISTRATE(9),=X'4021204B2020202020'  MOVE IN EDIT PATTERN         
         ED    LISTRATE(9),DUB+4   DISPLAY EXCHANGE RATE                        
*                                                                               
*        DISPLAY PERIOD START                                                   
*                                                                               
LRPER    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,GEKPSTA),(17,LISTPER)                             
*                                                                               
         MVI   LISTPER+8,C'-'      SET SEPARATOR                                
*                                                                               
*        DISPLAY END DATE                                                       
*                                                                               
         CLC   GEKPEND,=X'FFFF'    IF UNTIL FURTHER NOTICE                      
         BNE   *+14                                                             
         MVC   LISTPER+9(3),=C'UFN'                                             
         B     LRPERX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,GEKPEND),(17,LISTPER+9)                           
*                                                                               
LRPERX   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LISTING RECORDS                           
         BNE   LR900                                                            
*                                                                               
         GOTO1 LISTMON                LIST LINE                                 
*                                                                               
         B     LRCONT                                                           
*                                                                               
LR900    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTREP       IF REPORTING RECORDS                         
         BNE   LR950                                                            
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)        PRINT LINE                                
*                                                                               
LR950    DS    0H                                                               
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD ON FILE                    
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             TELL GENCON WE'RE DONE                       
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C38  EXCHANGE RECORD - HOOK'                                 
***********************************************************************         
*                                                                     *         
*        HEADLINE HOOK FOR REPORT                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HOOK     NTR1  BASE=*,LABEL=*      HEADLINE ROUTINES                            
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
HOOKX    XIT1                                                                   
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME4D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVCODE   DS    CL12                                                             
X        DS    XL100                                                            
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
         DS    A                   SPARE                                        
*                                                                               
SCANBLK  DS    XL70                WORKAREA FOR SCANNER                         
         DS    XL2                 SPARE                                        
*                                                                               
BCSTART  DS    XL2                 COMPRESSED START DATE                        
BCEND    DS    XL2                 COMPRESSED END   DATE                        
*                                                                               
         DS    0D                                                               
SAVKEY   DS    XL48                KEY    SAVE AREA                             
SAVAIO   DS    A                   AIO SAVE AREA                                
SAVDSKAD DS    XL4                 DISK ADDRESS SAVE AREA                       
SAVCLT   DS    CL3                 CLIENT SAVE AREA                             
         DS    XL1                 SPARE                                        
SAVPID   DS    XL2                 PID                                          
SAVSECAG DS    CL2                 SECURITY AGENCY                              
*                                                                               
*        DATA FOR ACTTVITY ELEMENT                                              
*              SEE PACTVELM FOR DETAILS                                         
*                                                                               
ACTVCH1  DS    XL1                 CURRENT ACTION                               
ACTVCH2  DS    XL1                 DATA CHANGED                                 
*                                                                               
MAXACTVQ EQU   5                   MAX 5 ACTVITY ELMS KEPT                      
*                                                                               
         DS    0D                  ALIGNMENT                                    
SAVACTV  DS    10XL256             SAVE AREA FOR ACTIVITY ELEMENTS              
*                                                                               
         DS    0D                  END OF WORKING STORAGE                       
         EJECT                                                                  
*                                                                               
* ***************************                                                   
* ON-SCREEN ACTIVITY DISPLAY                                                    
* ***************************                                                   
DSPACTVD DSECT                                                                  
DSPUSR   DS    CL10                USERID                                       
         DS    CL1                                                              
DSPPID   DS    CL30                PERSON                                       
         DS    CL1                                                              
DSPDTE   DS    CL8                 DATE                                         
         DS    CL1                                                              
DSPDATA  DS    CL27                DATA CHANGED                                 
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LISTCUR  DS    CL3                                                              
         DS    CL4                                                              
LISTCLT  DS    CL3                                                              
         DS    CL1                                                              
LISTCLTN DS    CL20                                                             
         DS    CL1                                                              
LISTRATE DS    CL15                                                             
         DS    CL4                                                              
LISTPER  DS    CL17                                                             
         DS    CL1                                                              
         EJECT                                                                  
* PCLTREC                                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
* PPGENACTV                                                                     
       ++INCLUDE PPGENACTV                                                      
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
* GEGENCUR                                                                      
       ++INCLUDE GEGENCUR                                                       
* GEGENEXC                                                                      
       ++INCLUDE GEGENEXC                                                       
* PAGYREC                                                                       
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE PRWRIEQUS                                                      
       ++INCLUDE PPERREQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PRSFM38   07/17/18'                                      
         END                                                                    
