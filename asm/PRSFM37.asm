*          DATA SET PRSFM37    AT LEVEL 050 AS OF 08/06/07                      
*PHASE T41C37A               * * * * * * * * NOTE "C" PHASE                     
*INCLUDE PUBEDIT                                                                
*INCLUDE SRCHCALL                                                               
*        TITLE 'T41C37  CENTRAL PUB FILE'                                       
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE'                                       
**** CHANGE LOG                                                                 
*                                                                               
* BOBY   03/07  BIG BANG                                                        
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T41C37 - CENTRAL PUB FILE MAINT/LIST/REPORT           *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN T41CDF (MAINTENANCE)                           *         
*               SCREEN T41CEF (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED CENTRAL PUB FILE                              *         
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
         TITLE 'T41C37  CENTRAL PUB FILE'                                       
T41C37   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C37,RR=RE                                                   
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
         OI    GENSTAT3,MULTFILS   DEALING WITH MULTIPLE FILES                  
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - ANAL'                                
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
*                                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         BNE   *+12                                                             
         NOP   DR                                                               
         B     ANALX                                                            
*                                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         BNE   *+12                                                             
         NOP   DR                                                               
         B     ANALX                                                            
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
         TITLE 'T41C37  CENTRAL PUB FILE - VK'                                  
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
         TITLE 'T41C37  CENTRAL PUB FILE - VKMED'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIA                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKMED    DS    0H                                                               
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
         CLI   ACTNUM,ACTLIST      SKIP IF LISTING OR                           
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       REPORTING                                    
         BE    VKL                                                              
*                                                                               
         MVC   SCRMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES        INIT                                         
*                                                                               
         OI    SCRMEDNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         LA    R2,SCRMEDH         VALIDATE MEDIA                                
*                                                                               
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
         GOTO1 VALIMED             VALIDATER MEDIA                              
*                                                                               
         MVI   USEIONUM,0          RESET                                        
*                                                                               
         MVC   SCRMEDN(L'MEDNM),MEDNM  DISPLAY MEDIA NAME                       
*                                                                               
         L     R4,AIO              POINT TO AGENCY RECORD                       
         USING PAGYREC,R4          ESTABLISH AGENCY RECORD                      
*                                                                               
****     MVC   SVSCHMCD,PAGYSCHM   SAVE AGENCY'S SCHEME CODE                    
****     MVC   SVMSTRYN,PAGYMSTR   SAVE AGECNY'S MASTER STATUS                  
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
         MVC   AIO,AIO1            USE AIO1 FOR I/O                             
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKSCHM'                              
***********************************************************************         
*                                                                     *         
*        VALIDATE SCHEME CODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKSCHM   DS    0H                                                               
*                                                                               
         LA    R2,SCRCODEH         POINT TO SCHEME INPUT                        
*                                                                               
         GOTOR VGETFLD             READ IN SCHEME CODE                          
*                                                                               
         CLI   FLDH+5,0            REQUIRED FIELD                               
         BL    VKMISSER                                                         
*                                                                               
         CLI   FLDH+5,2            MAX 2 CH                                     
         BH    VKSCHNV                                                          
*                                                                               
         CLC   SVSCHMCD,FLD        MUST MATCH AGENCY'S SCHEME                   
         BNE   VKLKOUT                                                          
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKPUB'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB CODE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,SCRPUBH          POINT TO PUB FIELD                           
         XC    BPUB(6),BPUB                                                     
*                                                                               
         GOTOR VGETFLD             READ IN PUB FIELD                            
*                                                                               
         ZIC   R0,5(R2)            GET INPUT LENGTH                             
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VKPUBNV                                                          
*                                                                               
VKPUBX   DS    0H                                                               
*                                                                               
         B     VKBLD                                                            
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKL'                                 
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY FIELDS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
VKLMED   DS    0H                  VALIDATE MEDIA                               
*                                                                               
         MVC   SCLMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES        INIT                                         
         OI    SCLMEDNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         LA    R2,SCLMEDH          VALIDATE MEDIA                               
*                                                                               
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         MVI   USEIONUM,0          CLEAR INDICATOR                              
*                                                                               
         MVC   SCLMEDN(L'MEDNM),MEDNM  DISPLAY MEDIA NAME                       
*                                                                               
         L     R4,AIO              POINT TO AGENCY RECORD                       
         USING PAGYREC,R4          ESTABLISH AGENCY RECORD                      
*                                                                               
*****    MVC   SVSCHMCD,PAGYSCHM   SAVE AGENCY'S SCHEME CODE                    
*****    MVC   SVMSTRYN,PAGYMSTR   SAVE AGECNY'S MASTER STATUS                  
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
         MVC   AIO,AIO1            USE AIO1 FOR I/O                             
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKLSCHM'                             
***********************************************************************         
*                                                                     *         
*        VALIDATE SCHEME CODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLSCHM  DS    0H                                                               
*                                                                               
         LA    R2,SCLCODEH         POINT TO SCHEME INPUT                        
*                                                                               
         GOTOR VGETFLD             READ IN SCHEME CODE                          
*                                                                               
         CLI   FLDH+5,0            REQUIRED FIELD                               
         BL    VKMISSER                                                         
*                                                                               
         CLI   FLDH+5,2            MAX 2 CH                                     
         BH    VKSCHNV                                                          
*                                                                               
         CLC   SVSCHMCD,FLD        MUST MATCH AGENCY'S SCHEME                   
         BNE   VKLKOUT                                                          
*                                                                               
VKLSCHMX DS    0H                                                               
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKLPUB'                              
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB CODE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLPUB   DS    0H                                                               
*                                                                               
         LA    R2,SCLPUBH          POINT TO PUB FIELD                           
         XC    BPUB(6),BPUB                                                     
*                                                                               
         CLI   5(R2),0             NO ENTRY OKAY                                
         BE    VKLPUBX                                                          
*                                                                               
         ZIC   R0,5(R2)            GET INPUT LENGTH                             
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VKPUBNV                                                          
*                                                                               
VKLPUBX  DS    0H                                                               
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - VKBLD'                               
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
         USING GPUBKEY,R4          ESTABLISH CENTRAL FILE PUB KEY               
*                                                                               
         MVC   GPUBKRCD,=AL3(GPUBKIDQ)                                          
         MVC   GPUBKSC1,SVSCHMCD   SET SCHEME CODE - FIRST TIME                 
         MVC   GPUBKMED,QMED       SET MEDIA CODE                               
         MVC   GPUBKPUB,BPUB       SET PUB CODE                                 
         MVC   GPUBKSCH,SVSCHMCD   SET SCHEME CODE                              
         MVI   GPUBKCOD,X'81'      SET PUB RECORD ID                            
*                                                                               
         MVC   SVGPUBKY,GPUBKEY    SAVE PUB KEY                                 
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKPUBNV  DS    0H                  INVALID PUB CODE                             
         LA    RF,INVPUB                                                        
         B     VKERR                                                            
*                                                                               
VKMISSER LA    RF,MISSING          REQUIRED FIELD                               
         B     VKERR                                                            
*                                                                               
VKLKOUT  LA    RF,SECLOCK          SECURITY LOCKOUT                             
         B     VKERR                                                            
*                                                                               
VKSCHNV  LA    RF,INVALID          INVALID FIELD                                
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
         DROP                                                                   
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - MYPUBVAL'                            
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB FIELD                                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
MYPUBVAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         LR    R4,R2               COPY INPUT FIELD POINTER                     
         XC    BPUB(6),BPUB                                                     
*                                                                               
         MVI   ERROR,MISSING                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BNE   MYPUBV5                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      OK IF LISTING                                
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       OR IF REPORTING                              
         BE    MYPUBVX                                                          
*                                                                               
         B     MYPUBVER            IF NOT THEN MUST HAVE PUB                    
*                                                                               
MYPUBV5  DS    0H                                                               
*                                                                               
         B     MYPUBSRX            SKIP SRCH CHECKING                           
*                                                                               
         CLI   8(R2),C'='          IF INPUT STARTS WITH '='                     
         BNE   MYPUBSRX                                                         
*                                                                               
* NEED TO DO NAME SEARCHING DISPLACEMENT OF FIELD INTO TWA                      
*                                                                               
         S     R2,ATWA                                                          
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',ATWA),ACOMFACS,       +        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
*                                                                               
         A     R2,ATWA             RESTORE FIELD POINTER                        
*                                                                               
         DROP  R3                                                               
*                                                                               
MYPUBSRX DS    0H                                                               
*                                                                               
         ZIC   R0,5(R2)            GET INPUT LENGTH                             
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),BPUB                                   
*                                                                               
         MVI   ERROR,INVPUB                                                     
         CLI   0(R1),X'FF'                                                      
         BE    MYPUBVER                                                         
*                                                                               
*        READ IN PUB RECORD                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPUBRECD,R4         ESTABLISH PUB KEY                            
*                                                                               
         MVC   GPUBKRCD,=AL3(GPUBKIDQ)  RECORD ID                               
         MVC   GPUBKSC1,SVSCHMCD   SET SCHEME CODE - FIRST TIME                 
         MVC   GPUBKMED,QMED       MEDIA                                        
         MVC   GPUBKPUB(6),BPUB    MOVE PUB/ZONE/EDTN                           
         MVC   GPUBKSCH,SVSCHMCD   SCHEME CODE                                  
         MVI   GPUBKCOD,X'81'      RECORD CODE                                  
*                                                                               
MYPUBVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
MYPUBVER DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - DK'                                  
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
         L     R4,AIO1             POINT TO CENTRAL PUB FILE RECORD             
         USING GPUBRECD,R4         ESTABLISH CENTRAL PUB FILE REC               
*                                                                               
         FOUT  SCRMEDH,GPUBKMED,1  DISPLAY MEDIA                                
*                                                                               
         FOUT  SCRCODEH,GPUBKSCH,2 DISPLAY SCHEME CODE                          
*                                                                               
*        DISPLAY PUB CODE                                                       
*                                                                               
         XC    SCRPUB,SCRPUB                                                    
         OI    SCRPUBH+6,X'80'     TRANSMIT                                     
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,GPUBKPUB,(C'S',SCRPUB),RR=RELO                  
*                                                                               
         LA    R0,L'SCRPUB         MAX LENGTH OF PUB NUMBER                     
         LA    RF,SCRPUB+L'SCRPUB-1 LAST OF PUB FIELD                           
*                                                                               
         CLI   0(RF),C' '          FIND LAST OF PUB ID                          
         BH    *+14                                                             
         SHI   RF,1                BACK UP A BYTE                               
         BCT   R0,*-12                                                          
         DC    H'0'                NO PUB NUMBER                                
*                                                                               
         STC   R0,SCRPUBH+5        SET INPUT LENGTH                             
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
         TITLE 'T41C37  CENTRAL PUB FILE - VR'                                  
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
         MVC   SVGPUBKY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         L     R4,AIO              ESTABLISH RECORD                             
         USING GPUBRECD,R4                                                      
*                                                                               
         CLI   SVMSTRYN,C'Y'      MUST BE MASTER AGENCY                         
         BNE   VRLKOUT                                                          
*                                                                               
         CLI   ACTNUM,ACTADD      SEE IF ADDING                                 
         BNE   VR05                                                             
*                                                                               
         XC    GPUBRECD(100),GPUBRECD   INIT RECORD BUILD AREA                  
         MVC   GPUBKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,GPUBFRST-GPUBKEY MINIMUM LENGTH OF RECORD                     
         LA    RF,PUBNMELE(RF)     ADD ON DESCRIPTION ELEM LENGTH               
*                                                                               
         STCM  RF,3,GPUBRLEN       SET RECORD LENGTH                            
*                                                                               
VR05     DS    0H                                                               
*                                                                               
         LA    R6,GPUBFRST         POINT TO FIRST ELEMENT                       
         USING PUBNAMED,R6         ESTABLISH PUB NAME ELEMENT                   
*                                                                               
         MVC   PUBNAMEL(2),=X'10C4'  SET ELM ID AND LENGTH                      
*                                                                               
         MVC   PUBNAME,SPACES      INIT PUB NAME                                
         LA    R2,SCRPUBNH                                                      
*                                                                               
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VRMISS                                                           
*                                                                               
         MVC   PUBNAME,SCRPUBN     SET PUB NAME                                 
         OC    PUBNAME,SPACES                                                   
*                                                                               
         MVC   PUBZNAME,SPACES     INIT PUB ZONE NAME                           
         LA    R2,SCRPUBZH                                                      
*                                                                               
         MVC   PUBZNAME,SCRPUBZ    SET PUB ZONE NAME                            
         OC    PUBZNAME,SPACES                                                  
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         BRAS  RE,DR                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
VRMISS   LA    RF,MISSING          NO ENTRY                                     
         B     VRERR                                                            
*                                                                               
VRLKOUT  LA    RF,SECLOCK          SECURITY LOCKOUT                             
         B     VRERR                                                            
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
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
         TITLE 'T41C37  CENTRAL PUB FILE - DR'                                  
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
         USING GPUBRECD,R4                                                      
*                                                                               
         LA    R6,GPUBFRST         POINT TO FIRST ELEMENT IN RECORD             
         USING PUBNAMED,R6         ESTABLISH PUB NAME ELEMENT                   
*                                                                               
         FOUT  SCRPUBNH,PUBNAME,20   DISPLAY PUB NAME                           
*                                                                               
         FOUT  SCRPUBZH,PUBZNAME,20  DISPLAY ZONE NAME                          
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C37  CENTRAL PUB FILE - XR'                                  
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
         TITLE 'T41C37  CENTRAL PUB FILE - LR'                                  
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
         USING GPUBRECD,R4                                                      
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LRLOOP              NO - KEY IS LAST RECORD READ                 
*                                       SO GO CHECK VS. KEYSAVE                 
*                                                                               
         MVC   GPUBKRCD,=AL3(GPUBKIDQ)        -RECORD CODE                      
         MVC   GPUBKSC1,SVSCHMCD   SET SCHEME CODE - FIRST TIME                 
         MVC   GPUBKMED,QMED       CREATE KEY -MEDIA                            
         MVC   GPUBKPUB,BPUB       PUB CODE                                     
         MVI   GPUBKCOD,X'81'      SET PUB RECORD ID                            
         MVC   GPUBKSCH,SVSCHMCD   SET SCHEME CODE                              
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD ON FILE                    
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH KEY OF RECORD                      
*                                                                               
         CLC   GPUBKEY(GPUBKPUB-GPUBKEY),KEYSAVE TEST FOR ALL DONE              
         BNE   LRDONE                                                           
*                                                                               
         CLI   GPUBKCOD,X'81'      SKIP IF NOT PUB MASTER RECORD                
         BNE   LRCONT                                                           
*                                                                               
         CLC   GPUBKSCH,SVSCHMCD   SKIP IF NOT FOR THIS SCHEME                  
         BNE   LRCONT                                                           
*                                                                               
         GOTO1 GETREC              GET THE PUB RECORD                           
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,GPUBFRST-GPUBKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         USING PUBNAMED,R6         ESTABLISH PUB DESC ELM                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,GPUBKPUB,(C'S',LISTPUB),RR=RELO                 
*                                                                               
         MVC   LISTPUBN,PUBNAME    PUB NAME                                     
*                                                                               
         MVC   LISTPUBZ,PUBZNAME  PUB ZONE NAME                                 
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
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
HOOK     NTR1  BASE=*,LABEL=*      HEADLINE ROUTINES                            
*                                                                               
HOOKX    XIT1                                                                   
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H1,1,C'     '                                                    
         SSPEC H1,42,C'PRINT CUSTOM COLUMN REPORT'                              
         SSPEC H2,42,C'--------------------------'                              
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,RUN                                                        
         SSPEC H4,95,REPORT                                                     
         SSPEC H5,95,PAGE                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H7,1,C'CODE          DESCRIPTION'                                
         SSPEC H8,1,C'----          -----------'                                
         SSPEC H7,41,C'HEADER 1      HEADER 2       TYP  TOT   MEDIA'           
         SSPEC H8,41,C'--------      --------       ---  ---   -----'           
         DC    X'00'                                                            
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMDFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMEFD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVCODE   DS    CL12                                                             
X        DS    XL100                                                            
SVSCHMCD DS    XL2                 AGENCY SCHEME CODE                           
SVMSTRYN DS    CL1                 AGENCY MASTER STATUS                         
SVGPUBKY DS    XL32                PUB RECORD KEY                               
SVMED    DS    CL1                 MEDIA                                        
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
SCANBLK  DS    XL70                WORKARE FOR SANNER                           
PUBZNM   DS    CL20                PUB ZONE NAME                                
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LISTPUB  DS    CL17                                                             
         DS    CL1                                                              
LISTPUBN DS    CL20                                                             
         DS    CL2                                                              
LISTPUBZ DS    CL20                                                             
         DS    CL1                                                              
         EJECT                                                                  
* GPUBREC                                                                       
                                                                                
       ++INCLUDE GEGENPUB                                                       
* PUBNAMEL                                                                      
PUBNAMED DSECT                                                                  
       ++INCLUDE PUBNAMEL                                                       
* PAGYREC                                                                       
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE PRWRIEQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PRSFM37   08/06/07'                                      
         END                                                                    
